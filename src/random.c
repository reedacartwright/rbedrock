/* 
 * The Mersenne Twister pseudo-random number generator (PRNG)
 *
 * This is an implementation of fast PRNG called MT19937, meaning it has a
 * period of 2^19937-1, which is a Mersenne prime.
 *
 * Written by Christian Stigen Larsen
 * Distributed under the modified BSD license.
 * 2015-02-17, 2017-12-06
 * Source: https://github.com/cslarsen/mersenne-twister
 */


#include "random.h"


#include <stdio.h>

static void mcpe_random_seed_impl(uint32_t value);

/*
 * We have an array of 624 32-bit values, and there are 31 unused bits, so we
 * have a seed value of 624*32-31 = 19937 bits.
 */
#define SIZE 624
#define PERIOD 397
#define DIFF  (SIZE - PERIOD)

static const uint32_t MAGIC = 0x9908b0df;

// State for a singleton Mersenne Twister. If you want to make this into a
// class, these are what you need to isolate.
struct mcpe_random_ {
    uint32_t mt[SIZE];
    uint32_t mt_tempered[SIZE];
    uint32_t index;
};

typedef struct mcpe_random_ mcpe_random_t;

static mcpe_random_t g_state;


void rbedrock_init_random(void) {
    mcpe_random_seed_impl(5489u);
}

#define M32(x) (0x80000000 & x) // 32nd MSB
#define L31(x) (0x7FFFFFFF & x) // 31 LSBs

#define UNROLL(expr) \
    y = M32(g_state.mt[i]) | L31(g_state.mt[i+1]); \
    g_state.mt[i] = g_state.mt[expr] ^ (y >> 1) ^ ((((int32_t)((y) << 31)) >> 31) & MAGIC); \
    ++i;

static void mcpe_random_update_state(void) {
    size_t i = 0;
    uint32_t y;

    // i = [0 ... 226]
    while ( i < DIFF ) {
        UNROLL(i+PERIOD);
    }

    // i = [227 ... 622]
    while ( i < SIZE -1 ) {
        UNROLL(i-DIFF);
    }

    {
    // i = 623, last step rolls over
        y = M32(g_state.mt[SIZE-1]) | L31(g_state.mt[0]);
        g_state.mt[SIZE-1] = g_state.mt[PERIOD-1] ^ (y >> 1) ^ ((((int32_t)((y) << 31)) >>
            31) & MAGIC);
    }

    // Temper all numbers in a batch
    for (size_t j = 0; j < SIZE; ++j) {
        y = g_state.mt[j];
        y ^= y >> 11;
        y ^= y << 7  & 0x9d2c5680;
        y ^= y << 15 & 0xefc60000;
        y ^= y >> 18;
        g_state.mt_tempered[j] = y;
    }

    g_state.index = 0;
}

static uint32_t mcpe_random_next(void) {
    if ( g_state.index == SIZE ) {
        mcpe_random_update_state();
        g_state.index = 0;
    }
    return g_state.mt_tempered[g_state.index++];
}

static void mcpe_random_seed_impl(uint32_t value) {
    g_state.mt[0] = value;
    g_state.index = SIZE;

    for ( uint_fast32_t i=1; i<SIZE; ++i ) {
        g_state.mt[i] = 0x6c078965*(g_state.mt[i-1] ^ g_state.mt[i-1]>>30) + i;
    }  
}

SEXP mcpe_random_seed(SEXP r_seed) {
    mcpe_random_seed_impl(Rf_asInteger(r_seed));
    return R_NilValue;
}

// returns g_state as a raw vector
// can set it as well
SEXP mcpe_random_state(SEXP r_state) {
    SEXP ret = PROTECT(Rf_allocVector(RAWSXP, sizeof(g_state)));
    memcpy(RAW(ret), &g_state, sizeof(g_state));
    if(!Rf_isNull(r_state)) {
        if((TYPEOF(r_state) != RAWSXP) || XLENGTH(r_state) != sizeof(g_state)) {
            Rf_error("mcpe_random_state: value 'state' is not a raw vector of length %zd.", sizeof(g_state));
            return R_NilValue;
        }
        memcpy(&g_state, RAW(r_state), sizeof(g_state));
    }
    UNPROTECT(1);
    return ret;
}

// fill a numeric vector with unsigned integers
SEXP mcpe_random_get_uint(SEXP r_n, SEXP r_max) {
    size_t num = Rf_asInteger(r_n);
    SEXP ret = PROTECT(Rf_allocVector(REALSXP, num));
    double *p = REAL(ret);
    if(Rf_isNull(r_max)) {
        for(size_t i=0; i < num; ++i) {
            p[i] = (double)mcpe_random_next();

        }
    } else {
        uint32_t maxval = Rf_asInteger(r_max);
        for(size_t i=0; i < num; ++i) {
            uint32_t u = mcpe_random_next() % maxval;
            p[i] = (double)u;
        }
    }
    UNPROTECT(1);
    return ret;
}


// fill a numeric vector with integers
SEXP mcpe_random_get_int(SEXP r_n, SEXP r_min, SEXP r_max) {
    size_t num = Rf_asInteger(r_n);
    SEXP ret = PROTECT(Rf_allocVector(INTSXP, num));
    int *p = INTEGER(ret);
    if(!Rf_isNull(r_max) && !Rf_isNull(r_min)) {
        int32_t maxval = Rf_asInteger(r_max);
        int32_t minval = Rf_asInteger(r_min);
        uint32_t width = (uint32_t)(maxval-minval);
        for(size_t i=0; i < num; ++i) {
            p[i] = minval;
            if(minval < maxval) {
                int val = (int)(mcpe_random_next() % width);
                p[i] +=  val;
            }
        }
    } else if(!Rf_isNull(r_max)) {
        uint32_t width = (uint32_t)Rf_asInteger(r_max);
        for(size_t i=0; i < num; ++i) {       
            if(width == 0) {
                p[i] = 0;
            } else {
                int val = (int)(mcpe_random_next() % width);
                p[i] = val;
            }
        }
    } else {
         for(size_t i=0; i < num; ++i) {
            int val = (int)(mcpe_random_next() >> 1);
            p[i] = val;
        }       
    }
    UNPROTECT(1);
    return ret;
}

SEXP mcpe_random_get_double(SEXP r_n) {
    size_t num = Rf_asInteger(r_n);
    SEXP ret = PROTECT(Rf_allocVector(REALSXP, num));
    double *p = REAL(ret);
    for(size_t i=0; i < num; ++i) {
        p[i] = mcpe_random_next()/4294967296.0;
    }
    UNPROTECT(1);
    return ret;    
}

// fill a numeric vector with floats
SEXP mcpe_random_get_float(SEXP r_n, SEXP r_min, SEXP r_max) {
    size_t num = Rf_asInteger(r_n);
    SEXP ret = PROTECT(Rf_allocVector(REALSXP, num));
    double *p = REAL(ret);
    for(size_t i=0; i < num; ++i) {
        p[i] = (float)(mcpe_random_next()/4294967296.0);
    }

    if(!Rf_isNull(r_max) && !Rf_isNull(r_min)) {
        float maxval = Rf_asReal(r_max);
        float minval = Rf_asReal(r_min);
        float width = maxval-minval;
        for(size_t i=0; i < num; ++i) {
            p[i] = (float)(minval + ((float)p[i])*width);
        }
    } else if(!Rf_isNull(r_max)) {
        float width = Rf_asReal(r_max);
        for(size_t i=0; i < num; ++i) {       
            p[i] = (float)(((float)p[i])*width);
        }
    }
    UNPROTECT(1);
    return ret;
}

SEXP mcpe_random_create_seed(SEXP r_x, SEXP r_z, SEXP r_a, SEXP r_b, SEXP r_salt, SEXP r_type) {
    unsigned int x = Rf_asInteger(r_x);
    unsigned int z = Rf_asInteger(r_z);
    unsigned int a = Rf_asInteger(r_a);
    unsigned int b = Rf_asInteger(r_b);
    unsigned int salt = Rf_asInteger(r_salt);
    int type = Rf_asInteger(r_type);
    unsigned int seed = 0;

    switch(type) {
     case 1:
        seed = (x * a) ^ (z * b) ^ salt;
        break;
     case 2:
        seed = (x * a) + (z * b) + salt;
        break;
     case 3:
        seed = ((x * a) + (z * b)) ^ salt;
        break;
    }
    return Rf_ScalarInteger(seed);
}
