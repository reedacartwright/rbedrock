#define R_NO_REMAP

#include "nbt.h"

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
    int num_storages = p[1];
    p += 2;
    if(version != 8) {
        Rf_error("Subchunk data version '%d' is not supported.", version);
        return R_NilValue;
    }
    SEXP r_ret = PROTECT(Rf_allocVector(VECSXP, num_storages));
    // read each layer of subchunk block storage
    for(int i = 0; i < num_storages; ++i) {
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
            int temp;
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
        SEXP r_palette = PROTECT(read_nbt_compound_payload(&p, end, palette_size));
        if(XLENGTH(r_palette) != palette_size) {
            return_block_error();
        }
        // store palette as an attribute
        Rf_setAttrib(r_blocks, Rf_install("palette"), r_palette);
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