/*
# Copyright (c) 2020 Reed A. Cartwright <reed@cartwright.ht>
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

#include "strkey.h"

#include <string.h>
#include <stdio.h>

bool is_chunk_key(const char *key, size_t key_len) {
    char tag;
    switch(key_len) {
     case 9:
     case 10:
        tag = key[8];
        break;
     case 13:
     case 14:
        tag = key[12];
     default:
        return false;
    }
    if(45 <= tag && tag <= 58) {
        return true;
    }
    if(tag == 118) {
        return true;
    }
    return false;
}

// maximum size 45 characters
bool make_strkey(const char *key, size_t key_len, char *out, size_t *out_len) {
    int dimension = 0;
    int x;
    int z;
    char tag;
    char subtag = -1;
    
    switch(key_len) {
     case 10:
        subtag = key[9];
     case 9:
        tag = key[8];
        memcpy(&x, key+0, 4);
        memcpy(&z, key+4, 4);        
        break;
     case 14:
        subtag = key[13];
     case 13:
        tag = key[12];
        memcpy(&x, key+0, 4);
        memcpy(&z, key+4, 4);
        memcpy(&dimension, key+8, 4);
        break;
     default:
        return false;
    }
    if(45 <= tag && tag <= 58) {
        /* noop */;
    } else if(tag == 118) {
        /* noop */;
    } else {
        return false;
    }
    if(subtag != -1) {
        *out_len = snprintf(out, *out_len, "@%i:%i:%i:%u-%u",
            x, z, dimension, (unsigned int)tag, (unsigned int)subtag);
    } else {
        *out_len = snprintf(out, *out_len, "@%i:%i:%i:%u",
            x, z, dimension, (unsigned int)tag);
    }
    return true;
}
