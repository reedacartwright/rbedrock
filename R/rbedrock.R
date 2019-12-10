is_chunk_key <- function(raw_key) {
    tag = 0
    if(length(raw_key) == 9 || length(raw_key) == 10) {
        tag = as.integer(raw_key[9])
    } else if(length(raw_key) == 13 || length(raw_key) == 14) {
        tag = as.integer(raw_key[13])
    }
    return((45 <= tag && tag <= 58) || tag == 118)
}

data_keys <- function(db) {
    ks <- db$keys(as_raw=TRUE)
    ks <- Filter(Negate(is_chunk_key),ks)
    return(sapply(ks, rawToChar))
}

chunk_keys <- function(db) {
    ks <- db$keys(as_raw=TRUE)
    ks <- Filter(is_chunk_key,ks)

    out <- sapply(ks, function(k) {
        len <- length(k)
        x <- NA
        z <- NA
        d <- NA
        tag <- NA
        subtag <- NA
        if(len == 9 || len == 10) {
            xz <- readBin(k,'integer',n=2L,endian="little")
            x <- xz[1]
            z <- xz[2]
            d <- 0
            tag <- as.integer(k[9])
            subtag <- NA
            if(len == 10) {
                subtag <- as.integer(k[10])
            }
        } else if(len == 13 || len == 14) {
            xz <- readBin(k,'integer',n=3L,endian="little")
            x <- xz[1]
            z <- xz[2]
            d <- xz[3]
            tag <- as.integer(k[13])
            subtag <- NA
            if(len == 14) {
                subtag <- as.integer(k[14])
            }
        }
        c(x,z,d,tag,subtag)
    })
    subtag <- ifelse(is.na(out[5,]), "", sprintf("-%d",out[5,]))
    out <- sprintf("@%d:%d:%d:%d%s",out[1,],out[2,],out[3,],out[4,],subtag)

    return(out)
}

chunk_keys_table <- function(keys) {
    keys <- str_subset(keys,"^@[^:]+:[^:]+:[^:]+:[^:]+$")
    trimmed <- str_replace(keys, "^@","")
    frags <- str_split(trimmed, ":", n=4,simplify=TRUE)
    tags <- str_split(frags[,4],"-",n=2,simplify=TRUE)
    
    data.frame(
        key = keys,
        x = as.integer(frags[,1]),
        z = as.integer(frags[,2]),
        dimension = as.integer(frags[,3]),
        tag = as.integer(tags[,1]),
        subtag = as.integer(tags[,2]),
        stringsAsFactors = FALSE
    )
}

chunk_keys_to_raw <- function(keys) {
    tab <- chunk_keys_table(keys)

    out <- list()
    for(i in 1:nrow(tab)) {

        r <- writeBin(c(tab$x[i],tab$z[i]), raw(), size=4, endian="little")
        if(tab$dimension[i] > 0) {
            r <- c(r, writeBin(tab$dimension[i], raw(), size=4, endian="little"))
        }
        r <- c(r, writeBin(tab$tag[i], raw(), size=1, endian="little"))
        if(!is.na(tab$subtag[i])) {
            r <- c(r, writeBin(tab$subtag[i], raw(), size=1, endian="little"))
        }
        out[[i]] <- r
    }
    out
}

readHSA <- function(val) {
    len = readBin(val[1:4],integer(),n=1,size=4)
    out = c()
    for(i in 1:len) {
        pos = 4+(i-1)*25+1
        aabb = readBin(val[seq.int(pos,pos+23)],integer(),n=6,size=4)
        tag = readBin(val[pos+24],integer(),1,1)
        out = rbind(out,c(aabb,tag))
    }
    colnames(out) = c("x1","y1","z1","x2","y2","z2","tag")
    out = cbind(out, xspot = out[,"x1"] + (out[,"x2"]-out[,"x1"]+1) %/% 2)
    out = cbind(out, yspot = pmin.int(out[,"y1"],out[,"y2"]))
    out = cbind(out, zspot = out[,"z1"] + (out[,"z2"]-out[,"z1"]+1) %/% 2)
    out
}
