pokemon_generation <- data.table::fread("data_raw/pokemon_generation.csv", data.table = FALSE)
moves <- data.table::fread("data_raw/moves.csv", data.table = FALSE)
moves_changelog <- data.table::fread("data_raw/moves_changelog.csv", data.table = FALSE)

usethis::use_data(pokemon_generation, moves, moves_changelog, internal = TRUE, overwrite = TRUE)
