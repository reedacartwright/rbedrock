
#' @export
read_3dmaps_value <- function(rawdata) {
    if(is.null(rawdata)) {
        return(NULL)
    }
    vec_assert(rawdata, raw())    
    h <- readBin(rawdata[1:512], integer(), n=256L, size=2L, endian="little", signed = TRUE)
    dim(h) <- c(16L,16L)
    b <- .Call(Cread_chunk_biomes, rawdata[-(1:512)])
    
    list(height_map = h, biome_map = b)
}
