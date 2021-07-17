#define R_NO_REMAP

#include "nbt.h"

static SEXP g_palette_symbol = NULL;

void rbedrock_init_blocks() {
    g_palette_symbol = Rf_install("palette");
}

#define return_block_error() error_return("Malformed subchunk data.")

// https://gist.github.com/Tomcc/a96af509e275b1af483b25c543cfbf37
// [version:byte][num_storages:byte][block storage1]...[blockStorageN]

SEXP read_subchunk(SEXP r_value) {
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

    if(end-p < 2) {
        return_block_error();
    }
    int version = p[0];
    int num_layers = p[1];
    p += 2;
    if(version != 8) {
        Rf_error("Subchunk data version '%d' is not supported.", version);
        return R_NilValue;
    }
    SEXP r_ret = PROTECT(Rf_allocVector(VECSXP, num_layers));
    // read each layer of subchunk block storage
    for(int i = 0; i < num_layers; ++i) {
        if(end-p < 1) {
            return_block_error();
        }
        int flags = p[0];
        ++p;
        if((flags & 1)) {
            // Chunk storage is runtime
            error_return("Subchunk does not have Persistent IDs.");
        }
        SEXP r_blocks = PROTECT(Rf_alloc3DArray(INTSXP, 16,16,16));
        // calculate storage structure.
        int bits_per_block = flags >> 1;
        int blocks_per_word = 32 / bits_per_block; // floor using integer math
        int word_count = (4095 / blocks_per_word)+1; // ceiling using integer math
        int mask = (1 << bits_per_block)-1;
        if(end-p < 4*word_count) {
            return_block_error();
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
            return_block_error();
        }
        int palette_size;
        memcpy(&palette_size, p, 4);
        p += 4;
        SEXP r_palette = PROTECT(read_nbt_values(&p, end, palette_size, false));
        if(XLENGTH(r_palette) != palette_size) {
            return_block_error();
        }
        // store palette as an attribute
        Rf_setAttrib(r_blocks, g_palette_symbol, r_palette);
        SET_VECTOR_ELT(r_ret, i, r_blocks);
        UNPROTECT(2);
    }
    if(p != end) {
        Rf_error("Malformed NBT data: %d bytes were read out of %d bytes total", (int)(end-p), (int)len);
        return R_NilValue;
    }
    UNPROTECT(1);
    return r_ret;
}

static int calc_bits_per_block(int sz) {
    int p[8] = {1,2,3,4,5,6,8,16};
    int z[8] = {2,4,8,16,32,64,256,65536};

    int i;
    for(i=0;i<7 && z[i] < sz; ++i) {
        /*noop*/;        
    }
    return p[i];
}

SEXP write_subchunk(SEXP r_value) {
    if(Rf_isNull(r_value)) {
        return R_NilValue;
    }
    if(TYPEOF(r_value) != VECSXP) {
        error_return("Argument is not a list.");
    }
    R_xlen_t num_layers = XLENGTH(r_value);
    SEXP r_retv = PROTECT(Rf_allocVector(VECSXP, 2*num_layers));
    for(R_xlen_t i=0; i < num_layers; ++i) {
        SEXP r_layer = VECTOR_ELT(r_value, i);
        SEXP r_pal = PROTECT(Rf_getAttrib(r_layer, g_palette_symbol));
        if(!Rf_isInteger(r_layer) || XLENGTH(r_layer) != 4096 || Rf_isNull(r_pal)) {
            return_block_error();
        }
        // Calculate which palette type we need to use
        R_xlen_t palette_size = XLENGTH(r_pal);
        int bits_per_block = calc_bits_per_block(palette_size);
        int blocks_per_word = 32 / bits_per_block; // floor using integer math
        int word_count = (4095 / blocks_per_word)+1; // ceiling using integer math
        int mask = (1 << bits_per_block)-1;

        // Allocate space
        SET_VECTOR_ELT(r_retv, 2*i, Rf_allocVector(RAWSXP, 5+word_count*4));
        unsigned char *buffer = RAW(VECTOR_ELT(r_retv, 2*i));
        *buffer = bits_per_block << 1;
        buffer += 1;

        // write palette ids
        unsigned int u = 0;
        int *v = INTEGER(r_layer);

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
        // write palette
        SET_VECTOR_ELT(r_retv, 2*i+1, write_nbt(r_pal));
        UNPROTECT(1);
    }
    // Measure total length
    R_xlen_t len = 2;
    for(R_xlen_t i = 0; i < XLENGTH(r_retv); ++i) {
        len += XLENGTH(VECTOR_ELT(r_retv, i));
    }
    SEXP r_ret = PROTECT(Rf_allocVector(RAWSXP, len));
    unsigned char *p = RAW(r_ret);
    p[0] = 8;
    p[1] = num_layers;
    p += 2;
    for(R_xlen_t i = 0; i < XLENGTH(r_retv); ++i) {
        SEXP r = VECTOR_ELT(r_retv, i);
        memcpy(p, RAW(r), XLENGTH(r));
        p += XLENGTH(r);
    }
    UNPROTECT(2);
    return r_ret;
}
