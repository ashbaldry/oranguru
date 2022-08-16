#' @noRd
check_level <- function(level) {
  if (!is.numeric(level) || level < 1 || level > 100 || as.integer(level) != level) {
    stop("Level is invalid. Must be an integer between 1 and 100")
  }
}

#' @noRd
check_generation <- function(generation) {
  if (!is.numeric(generation) || generation < 1 || generation > 100 || as.integer(generation) != generation) {
    stop("Generation is invalid. Must be an integer between 1 and 8")
  }
}

#' @noRd
check_iv <- function(iv, generation = 8) {
  if (generation >=  3) {
    max_iv <- 31
  } else {
    max_iv <- 15
  }

  if (!is.numeric(iv) || iv < 0 || iv > max_iv || as.integer(iv) != iv) {
    stop("IV value is invalid. Must be an integer between 0 and ", max_iv)
  }
}

#' @noRd
check_stat_name <- function(name) {
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
check_nature <- function(nature) {
  if (is.null(nature)) {
    clean_nature <- sample(NATURES, 1)
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
