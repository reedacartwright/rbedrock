#define R_NO_REMAP

#include "nbt.h"
#include "support.h"

static SEXP g_palette_symbol = NULL;
static SEXP g_offset_symbol = NULL;

void rbedrock_init_blocks() {
    g_palette_symbol = Rf_install("palette");
    g_offset_symbol = Rf_install("offset");
}

#define return_subchunk_error() error_return("Malformed subchunk data.")

// https://gist.github.com/Tomcc/a96af509e275b1af483b25c543cfbf37
// [version:byte][num_storages:byte][block storage1]...[blockStorageN]

SEXP read_subchunk_palette_ids(const unsigned char **buffer, const unsigned char *end,
    bool *is_persistent, int *palette_size) {
    const unsigned char *p = *buffer;
    if(end-p < 1) {
        return_subchunk_error();
    }
    int flags = p[0];
    ++p;

    // Check for special flag that signals an empty subchunk
    if(flags == 255) {
        *palette_size = 0;
        *buffer = p;
        return R_NilValue;
    }
    *is_persistent = ((flags & 1) == 0);

    SEXP r_blocks = PROTECT(Rf_alloc3DArray(INTSXP, 16,16,16));
    // calculate storage structure.
    int bits_per_block = flags >> 1;
    if(bits_per_block > 0) {
        int blocks_per_word = 32 / bits_per_block; // floor using integer math
        int word_count = (4095 / blocks_per_word)+1; // ceiling using integer math
        int mask = (1 << bits_per_block)-1;
        if(end-p < 4*word_count) {
            return_subchunk_error();
        }
        // read palette ids
        unsigned int u = 0;
        int *v = INTEGER(r_blocks);
        for(int j = 0; j < word_count; ++j) {
            // read current word and parse
            unsigned int temp = 0;
            memcpy(&temp, p, 4);
            p += 4;
            for(int k = 0; k < blocks_per_word && u < 4096; ++k) {
                // calculate position as if we did aperm(v, c(3,1,2))
                unsigned int x = (u >> 8) & 0xf;
                unsigned int y = u & 0xf;
                unsigned int z = (u >> 4) & 0xf;
                unsigned int pos = x + 16*y + 256*z;
                // store block id
                v[pos] = (temp & mask) + 1;
                temp = temp >> bits_per_block;
                u += 1;
            }
        }
        // read NBT palette data
        if(end-p < 4) {
            return_subchunk_error();
        }
        memcpy(palette_size, p, 4);
        p += 4;
    } else {
        // Handle special case of bits_per_block == 0
        int *v = INTEGER(r_blocks);
        for(int u=0; u < 4096; ++u) {
            v[u] = 1;
        }
        *palette_size = 1;
    }
    UNPROTECT(1);
    *buffer = p;
    return r_blocks;
}

static int calc_bits_per_block(int sz) {
    int p[8] = {1,2,3,4,5,6,8,16};
    int z[8] = {2,4,8,16,32,64,256,65536};

    int i;
    for(i = 0; i < 7 && z[i] < sz; ++i) {
        /*noop*/;
    }
    return p[i];
}

SEXP write_subchunk_palette_ids(SEXP r_values, bool is_persistent, R_xlen_t palette_size) {
    // Check Data
    if(!Rf_isInteger(r_values)) {
        return_subchunk_error();
    }
    SEXP r_ret;
    // Handle special case of palette_size == 0
    if(palette_size == 0) {
        r_ret = PROTECT(Rf_allocVector(RAWSXP, 1));
        unsigned char *buffer = RAW(r_ret);
        *buffer = 255;
        UNPROTECT(1);
        return r_ret;
    }
    // Check Length
    if(XLENGTH(r_values) != 4096) {
        return_subchunk_error();
    }  
    // Handle special case of palette_size == 1
    if(palette_size == 1) {
        r_ret = PROTECT(Rf_allocVector(RAWSXP, 1));
        unsigned char *buffer = RAW(r_ret);
        *buffer = (is_persistent ? 0 : 1);
        UNPROTECT(1);
        return r_ret;
    }

    // Calculate which palette type we need to use
    int bits_per_block = calc_bits_per_block(palette_size);
    int blocks_per_word = 32 / bits_per_block; // floor using integer math
    int word_count = (4095 / blocks_per_word)+1; // ceiling using integer math
    int mask = (1 << bits_per_block)-1;

    // Allocate space
    r_ret = PROTECT(Rf_allocVector(RAWSXP, 5+word_count*4));
    unsigned char *buffer = RAW(r_ret);
    *buffer = (bits_per_block << 1) | (is_persistent ? 0 : 1);
    buffer += 1;

    // write palette ids
    unsigned int u = 0;
    int *v = INTEGER(r_values);

    for(int j = 0; j < word_count; ++j) {
        // read current word and parse
        unsigned int temp = 0;
        for(int k = 0; k < blocks_per_word && u < 4096; ++k) {
            // calculate position as if we did aperm(v, c(3,1,2))
            unsigned int x = (u >> 8) & 0xf;
            unsigned int y = u & 0xf;
            unsigned int z = (u >> 4) & 0xf;
            unsigned int pos = x + 16*y + 256*z;
            // store block id
            unsigned int id = v[pos]-1;
            temp |= (id & mask) << k*bits_per_block;
            u += 1;
        }
        memcpy(buffer, &temp, 4);
        buffer += 4;
    }
    // copy palette size
    memcpy(buffer, &palette_size, 4);
    UNPROTECT(1);
    return r_ret;
}

SEXP read_subchunk_blocks(SEXP r_value) {
    if(Rf_isNull(r_value)) {
        return R_NilValue;
    }
    if(TYPEOF(r_value) != RAWSXP) {
        error_return("Argument is not a raw type or NULL.");
    }

    size_t len = XLENGTH(r_value);
    const unsigned char *buffer = RAW(r_value);
    const unsigned char *p = buffer;
    const unsigned char *end = buffer+len;

    if(end-p < 3) {
        return_subchunk_error();
    }
    int version = p[0];
    if(version < 8 || version > 9) {
        Rf_error("Subchunk data version '%d' is not supported.", version);
        return R_NilValue;        
    }
    int num_layers = p[1];
    p += 2;
    int subchunk_offset = NA_INTEGER;
    if(version >= 9) {
        subchunk_offset = (signed char)p[0];
        p += 1;
    }
    SEXP r_ret = PROTECT(Rf_allocVector(VECSXP, num_layers));
    // store offset as an attribute
    Rf_setAttrib(r_ret, g_offset_symbol, Rf_ScalarInteger(subchunk_offset));
    // read each layer of subchunk block storage
    for(int i = 0; i < num_layers; ++i) {
        bool is_persistent;
        int palette_size;
        SEXP r_blocks = PROTECT(read_subchunk_palette_ids(&p, end, &is_persistent, &palette_size));
        if(is_persistent == false) {
            // Chunk storage is runtime
            error_return("Subchunk does not have Persistent IDs.");
        }
        SEXP r_palette = PROTECT(Rf_allocVector(VECSXP, palette_size));
        SEXP r_val;

        for(int i = 0; i < palette_size; ++i) {
            if(p >= end) {
                return_subchunk_error();
            }
            r_val = PROTECT(read_nbt_value(&p, end));
            if(Rf_isNull(r_val)) {
                // We should not encounter a 0 tag in this context
                return_nbt_error_tag(0);
            }
            SET_VECTOR_ELT(r_palette, i, r_val);
            UNPROTECT(1);
        }      
        // construct a list to hold this layer
        const char *names[] = {"values", "palette", ""};
        SEXP r_layer = PROTECT(Rf_mkNamed(VECSXP, names));
        SET_VECTOR_ELT(r_layer, 0, r_blocks);
        SET_VECTOR_ELT(r_layer, 1, r_palette);
        SET_VECTOR_ELT(r_ret, i, r_layer);
        UNPROTECT(3);
    }
    if(p != end) {
        Rf_error("Malformed NBT data: %d bytes were read out of %d bytes total", (int)(end-p), (int)len);
        return R_NilValue;
    }
    UNPROTECT(1);
    return r_ret;
}

SEXP write_subchunk_blocks(SEXP r_values, SEXP r_palettes, SEXP r_version, SEXP r_offset) {
    R_xlen_t num_layers = XLENGTH(r_values);
    if(XLENGTH(r_palettes) != num_layers) {
        return_subchunk_error();
    }
    SEXP r_retv = PROTECT(Rf_allocVector(VECSXP, 2*num_layers));
    for(R_xlen_t i=0; i < num_layers; ++i) {
        SEXP r_layer = VECTOR_ELT(r_values, i);
        SEXP r_pal = VECTOR_ELT(r_palettes, i);
        if(!Rf_isInteger(r_layer) || XLENGTH(r_layer) != 4096 || Rf_isNull(r_pal)) {
            return_subchunk_error();
        }
        // Write the palette ids using persistent storage
        SET_VECTOR_ELT(r_retv, 2*i, write_subchunk_palette_ids(r_layer, true, XLENGTH(r_pal)));
        // write palette
        SET_VECTOR_ELT(r_retv, 2*i+1, write_nbt(r_pal));
    }

    int version = Rf_asInteger(r_version);
    int offset = Rf_asInteger(r_offset);
    // Measure total length
    R_xlen_t len = 2;
    if(version >= 9) {
        len += 1;
    }
    for(R_xlen_t i = 0; i < XLENGTH(r_retv); ++i) {
        len += XLENGTH(VECTOR_ELT(r_retv, i));
    }
    SEXP r_ret = PROTECT(Rf_allocVector(RAWSXP, len));
    unsigned char *p = RAW(r_ret);
    p[0] = version;
    p[1] = num_layers;
    p += 2;
    if(version >= 9) {
        p[0] = (signed char)(offset);
        p += 1;
    }
    for(R_xlen_t i = 0; i < XLENGTH(r_retv); ++i) {
        SEXP r = VECTOR_ELT(r_retv, i);
        memcpy(p, RAW(r), XLENGTH(r));
        p += XLENGTH(r);
    }
    UNPROTECT(2);
    return r_ret;
}

SEXP read_chunk_biomes(SEXP r_value) {
    if(Rf_isNull(r_value)) {
        return R_NilValue;
    }
    if(TYPEOF(r_value) != RAWSXP) {
        error_return("Argument is not a raw type or NULL.");
    }
    SEXP r_ret = PROTECT(create_stretchy_list());

    size_t len = XLENGTH(r_value);
    const unsigned char *buffer = RAW(r_value);
    const unsigned char *p = buffer;
    const unsigned char *end = buffer+len;
    while(p < end) {
        bool is_persistent;
        int palette_size;
        // construct a list to hold this subchunk
        const char *names[] = {"values", "palette", ""};
        SEXP r_val = PROTECT(Rf_mkNamed(VECSXP, names));
        SEXP r_values = PROTECT(read_subchunk_palette_ids(&p, end, &is_persistent, &palette_size));
        SET_VECTOR_ELT(r_val, 0, r_values);
        UNPROTECT(1);
        if(palette_size > 0) {
            if(is_persistent == true) {
                // Biomes are stored as runtime IDs. Toss error if they aren't.
                error_return("Subchunk has Persistent IDs.");
            }
            if(end - p < palette_size*sizeof(int)) {
                return_subchunk_error();
            }
            SEXP r_palette = PROTECT(Rf_allocVector(INTSXP, palette_size));
            memcpy(INTEGER(r_palette), p, palette_size*sizeof(int));
            p += palette_size*sizeof(int);
            SET_VECTOR_ELT(r_val, 1, r_palette);
            UNPROTECT(1);
        } else {
            SET_VECTOR_ELT(r_val, 1, R_NilValue);
        }
        // add to stretchy list
        grow_stretchy_list(r_ret, r_val);
        UNPROTECT(1);
    }
    UNPROTECT(1);
    return Rf_PairToVectorList(CDR(r_ret));
}

SEXP write_chunk_biomes(SEXP r_values, SEXP r_palettes) {
    R_xlen_t num_subchunks = XLENGTH(r_values);
    if(XLENGTH(r_palettes) != num_subchunks) {
        return_subchunk_error();
    }
    SEXP r_retv = PROTECT(Rf_allocVector(VECSXP, 2*num_subchunks));
    for(R_xlen_t i=0; i < num_subchunks; ++i) {
        SEXP r_val = VECTOR_ELT(r_values, i);
        SEXP r_pal = VECTOR_ELT(r_palettes, i);
        // Write the palette ids using runtime storage
        SET_VECTOR_ELT(r_retv, 2*i, write_subchunk_palette_ids(r_val, false, XLENGTH(r_pal)));
        // write palette
        if(!Rf_isInteger(r_pal)) {
            return_subchunk_error();
        }
        SEXP r_buffer = PROTECT(Rf_allocVector(RAWSXP, sizeof(int)*XLENGTH(r_pal)));
        memcpy(RAW(r_buffer), INTEGER(r_pal), sizeof(int)*XLENGTH(r_pal));
        UNPROTECT(1);
        SET_VECTOR_ELT(r_retv, 2*i+1, r_buffer);
    }

    // Calculate total size of our return vector
    R_xlen_t total_size = 0;
    for(R_xlen_t i=0; i < XLENGTH(r_retv); ++i) {
        total_size += XLENGTH(VECTOR_ELT(r_retv, i));
    }
    // Concatenate everything together and return
    SEXP r_ret = PROTECT(Rf_allocVector(RAWSXP, total_size));
    unsigned char *buffer = RAW(r_ret);
    for(R_xlen_t i=0; i < XLENGTH(r_retv); ++i) {
        SEXP r_val = VECTOR_ELT(r_retv, i);
        memcpy(buffer, RAW(r_val), XLENGTH(r_val));
        buffer += XLENGTH(r_val);
    }
    UNPROTECT(2);
    return r_ret;
}
