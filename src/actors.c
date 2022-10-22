/*
# Copyright (c) 2021 Reed A. Cartwright <reed@cartwright.ht>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
*/

#define R_NO_REMAP

#include "actors.h"
#include "stdint.h"

SEXP rbedrock_actor_make_uniqueids(SEXP low_counter, SEXP high_counter) {
    if(XLENGTH(low_counter) != XLENGTH(high_counter)) {
        Rf_error("arguments do not have the same length");
    }
    R_xlen_t len = XLENGTH(low_counter);
    low_counter = PROTECT(Rf_coerceVector(low_counter, INTSXP));
    high_counter = PROTECT(Rf_coerceVector(high_counter, INTSXP));

    SEXP result = PROTECT(Rf_allocVector(REALSXP, len));
    unsigned char temp[8];
    // a unique id has 64-bits
    for(R_xlen_t i = 0; i < len; ++i) {
        memcpy(temp+0, &(INTEGER(low_counter)[i]), 4);
        memcpy(temp+4, &(INTEGER(high_counter)[i]), 4);
        memcpy(&REAL(result)[i], temp, 8);
    }
    Rf_setAttrib(result, R_ClassSymbol, Rf_mkString("integer64"));
    UNPROTECT(3);
    return result;
}

SEXP rbedrock_actor_make_storagekeys(SEXP ids) {
    if(TYPEOF(ids) != REALSXP) {
        Rf_error("argument is not an integer64");
    }
    R_xlen_t len = XLENGTH(ids);
    SEXP result = PROTECT(Rf_allocVector(VECSXP, len));
    for(R_xlen_t i = 0; i < len; ++i) {
        uint64_t u;
        memcpy(&u, &REAL(ids)[i], 8);
        uint64_t lo = (uint32_t)u;
        u = 2*lo - u; // negate the upper 32-bit of u while keeping the lower bits unchanged
        u = __builtin_bswap64(u); // probably not 100% portable

        // store result
        SET_VECTOR_ELT(result, i, Rf_allocVector(RAWSXP, 8));
        SEXP elt = VECTOR_ELT(result, i);
        memcpy(RAW(elt), &u, 8);
    }

    UNPROTECT(1);
    return result;
}

/*
# NOTES for later work:
#
# Actor storage keys are 64-bits (8 bytes long). Below, I will describe how the
# game calculates them, which will be useful for anyone injecting custom mobs
# into the game with external tools.
#
# UniqueIDs in the game are 64-bits long and are composed by two parts: the
# upper 32-bits is the "worldStartCount" and the lower 32-bits is a counter
# that increases by 1 every time a UniqueID is requested. The "worldStartCount"
# is stored in level.dat, is a negative number, and decreases by 1 every time
# the game is closed. (Don't ask me why it is negative. Mojang reasons.)
#
# Each actor has a UniqueID, and the ActorStorageKey is based on the UniqueID.
# The high 32-bits are the negative of  UniqueID's "worldStartCount" (so a
# positive number) and the low 32-bits are the low 32bits of the UniqueID.
#
# So if you are injecting mobs into a game save, you need to mimic this UniqueID
# behavior:
#  - load the level.dat
#  - read worldStartCount
#  - decrease worldStartCount by 1
#  - save level.dat
#
# Then use the worldStartCount that you read to create UniqueIDs and
# corresponding ActorStorageKeys for your mobs.
*/
