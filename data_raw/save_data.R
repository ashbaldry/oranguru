pokemon_generation <- data.table::fread("data_raw/pokemon_generation.csv", data.table = FALSE)
moves <- data.table::fread("data_raw/moves.csv", data.table = FALSE)
moves_meta <- data.table::fread("data_raw/move_meta.csv", data.table = FALSE)
moves_changelog <- data.table::fread("data_raw/moves_changelog.csv", data.table = FALSE)

type_efficacy <- data.table::fread("data_raw/type_efficacy.csv", data.table = FALSE)
type_efficacy_past <- data.table::fread("data_raw/type_efficacy_past.csv", data.table = FALSE)

usethis::use_data(
  pokemon_generation,
  moves,
  moves_meta,
  moves_changelog,
  type_efficacy,
  type_efficacy_past,
  internal = TRUE,
  overwrite = TRUE
)
