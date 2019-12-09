listkeys <- function(db) {
    ks <- db$keys(as_raw=TRUE)
    sapply(ks, function(k) {
        len <- length(k)
        x = NA
        z = NA
        d = NA
        tag = NA
        subtag = NA
        if(len == 9 || len == 10) {
            xz = readBin(k,'integer',2)
            x = xz[1]
            z = xz[2]
            d = 0
            tag = as.integer(k[9])
            subtag = NA
            if(len == 10) {
                subtag = as.integer(k[10])
            }
        } else if(len == 13 || len == 14) {
            xz = readBin(k,'integer',3)
            x = xz[1]
            z = xz[2]
            d = xz[3]
            tag = as.integer(k[13])
            subtag = NA
            if(len == 14) {
                subtag = as.integer(k[14])
            }
        }
        c(x,z,d,tag,subtag)
    })
}
