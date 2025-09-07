## code to prepare `biome_df` dataset goes here

# Locations of various data
# "https://raw.githubusercontent.com/pmmp/BedrockData/refs/heads/master/biome_id_map.json" # nolint

# Use PyMCTranslate to get information for the latest version of bedrock.
# Identify latest version
url_list <- "https://api.github.com/repos/gentlegiantJGC/PyMCTranslate/contents/PyMCTranslate/json/versions" # nolint
res <- jsonlite::fromJSON(url_list)
n <- grep("^bedrock_", res$name, value = TRUE)
v <- sub("^bedrock_", "", n)
v <- numeric_version(gsub("_+", ".", v))
bedrock_latest <- n[which(v == max(v))]

n <- grep("^java_", res$name, value = TRUE)
v <- sub("^java_", "", n)
v <- numeric_version(gsub("_+", ".", v))
java_latest <- n[which(v == max(v))]

# Download data from latest version
url <- sprintf(
  "https://raw.githubusercontent.com/gentlegiantJGC/PyMCTranslate/refs/heads/main/PyMCTranslate/json/versions/%s/__biome_data__.json", # nolint
  c(bedrock_latest, java_latest)
)
biome_data <- jsonlite::fromJSON(url[[1]])
biome_name <- sub("^minecraft:", "", names(biome_data$int_map))
biome_id <- unlist(biome_data$int_map, use.names = FALSE)
univ_name <- unlist(biome_data$version2universal, use.names = FALSE)

java_data <- jsonlite::fromJSON(url[[2]])
java_name <- unname(unlist(java_data$universal2version)[univ_name])
java_name <- sub("^minecraft:", "", java_name)

univ_name <- sub("^universal_minecraft:", "", univ_name)

# Download color values from cubiomes
# https://github.com/Cubitect/cubiomes/blob/master/util.c
url <- "https://raw.githubusercontent.com/Cubitect/cubiomes/refs/heads/master/util.c" # nolint
color_file <- readLines(url)
s <- grep("setColor\\(colors, ", color_file, value = TRUE)
s <- sub("\\s+setColor\\(colors, ", "", s)

cu_biomes <- sub(",.+", "", s)
cu_colors <- regmatches(s, m = regexpr("0x[0-9A-fa-f]+", s))
cu_colors <- sub("0x", "#", cu_colors)

m1 <- match(univ_name, cu_biomes)
m2 <- match(java_name, cu_biomes)
m <- ifelse(is.na(m1), m2, m1)

biome_df <- tibble::tibble(
  bedrock_id = biome_id,
  bedrock_name = biome_name,
  java_name = java_name,
  universal_name = univ_name,
  color = cu_colors[m]
)

biome_df <- biome_df[order(biome_df$bedrock_id), ]

usethis::use_data(biome_df, overwrite = TRUE)
