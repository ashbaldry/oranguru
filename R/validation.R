#' @noRd
check_level <- function(level, error_on_fail = TRUE) {
  valid_level <- is.numeric(level) && level >= 1L && level <= 100L && as.integer(level) == level
  if (error_on_fail && !valid_level) {
    stop("Level is invalid. Must be an integer between 1 and 100")
  }
  valid_level
}

#' @noRd
check_generation <- function(gen, error_on_fail = TRUE) {
  valid_gen <- is.numeric(gen) && gen >= 1L && gen <= 8L && as.integer(gen) == gen
  if (error_on_fail && !valid_gen) {
    stop("Generation is invalid. Must be an integer between 1 and 8")
  }
  valid_gen
}

#' @noRd
check_iv <- function(iv, gen = 8L, error_on_fail = TRUE) {
  if (gen >=  3L) {
    max_iv <- 31L
  } else {
    max_iv <- 15L
  }

  valid_iv <- is.numeric(iv) && iv >= 0L && iv <= max_iv && as.integer(iv) == iv
  if (error_on_fail && !valid_iv) {
    stop("IV value is invalid. Must be an integer between 0 and ", max_iv)
  }
  valid_iv
}

#' @noRd
find_stat_name <- function(name) {
  clean_name <- tolower(name) |>
    sub(pattern = "( |\\.)+", replacement = "-") |>
    sub(pattern = "^sp-", replacement = "special-")

  if (clean_name %in% STAT_NAMES) {
    clean_name
  } else {
    stop("Invalid stat attribute. Must be one of ", toString(STAT_NAMES))
  }
}

STAT_NAMES <- c("attack", "defense", "special-attack", "special-defense", "speed")

#' @noRd
find_nature <- function(nature) {
  if (is.null(nature)) {
    clean_nature <- sample(NATURES, 1L)
  } else {
    clean_nature <- tolower(nature)
  }

  if (clean_nature %in% NATURES) {
    clean_nature
  } else {
    stop("Invalid nature. Must be one of ", toString(NATURES))
  }
}

NATURES <- c(
  "hardy", "lonely", "brave", "adamant", "naughty",
  "bold", "docile", "relaxed", "impish", "lax",
  "timid", "hasty", "serious", "jolly", "naive",
  "modest", "mild", "quiet", "bashful", "rash",
  "calm", "gentle", "sassy", "careful", "quirky"
)
