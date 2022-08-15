#' Calculating Pokémon Stats
#'
#' @description
#' Find the attack, defense, special attack, special defense and speed
#' of a Pokémon given its base stat value, along with other
#' information such as its level, IVs and EVs, and the generation.
#'
#' @param base_stat The stat a Pokémon has
#' @param stat_name The name of the stat i.e attack. Ignored for earlier
#' generations.
#' @param nature Nature of the Pokémon. Ignored for earlier generations.
#' @param level The level of the Pokémon
#' @param iv Individual Value. A random number given to a Pokémon
#' between 1-15 (Generation 1-2)  or 1-31 (Generation 3+)
#' @param ev Effort Value.
#' @param generation Generation to look at. Calculations differ in generations
#' 1 and 2 to subsequent generations
#'
#' @examples
#' TODO with real pokemon stats
#'
#' @seealso \code{\link{calculate_hp}}
#'
#' @encoding UTF-8
#' @export
calculate_stat <- function(base_stat, stat_name, nature, level = 50, iv = 0, ev = 0, generation = 8) {
  check_level(level)
  check_iv(iv)

  if (generation < 3) {
    calculate_stat_v1(base_stat, level, dv = iv, ev = ev)
  } else {
    stat_name <- check_stat_name(stat_name)
    nature <- check_nature(nature)
    calculate_stat_v2(base_stat, stat_name, nature, level, iv = iv, ev = ev)
  }
}

#' @description
#' `calculate_hp_range` gives a potential range of the HP stat, though
#' this is assuming that no EVs have been added to the Pokémon.
#'
#' @rdname calculate_stat
#' @export
calculate_stat_range <- function(base_hp, stat_name, nature, level = 50, generation = 8) {
  if (generation < 3) {
    min_hp <- calculate_stat_v1(base_hp, level, dv = 0)
    max_hp <- calculate_stat_v1(base_hp, level, dv = 15)
  } else {
    stat_name <- check_stat_name(stat_name)
    nature <- check_nature(nature)

    min_hp <- calculate_stat_v2(base_hp, stat_name, nature, level, iv = 0)
    max_hp <- calculate_stat_v2(base_hp, stat_name, nature, level, iv = 31)
  }

  if (missing(stat_name)) {
    cat("Stat Range:", min_hp, "-", max_hp, "\n")
  } else {
    cat(tools::toTitleCase(sub("-", " ", stat_name)), "Range:", min_hp, "-", max_hp, "\n")
  }

  invisible(c(min_hp, max_hp))
}

calculate_stat_v1 <- function(base_hp, level = 50, dv = 0, ev = 0) {
  float_hp <- (((base_hp + dv) * 2 + sqrt(ev) / 4) * level) / 100 + 5
  as.integer(float_hp)
}

calculate_stat_v2 <- function(base_hp, stat_name, nature, level = 50, iv = 0, ev = 0) {
  nature_stat <- get_nature_factor(nature, stat_name)
  float_hp <- (((2 * base_hp + iv + ev / 4) * level) / 100 + 5) * nature_stat
  as.integer(float_hp)
}

get_nature_factor <- function(nature, stat_name) {
  if (paste(stat_name, nature) %in% INCREASE_NATURE_STATS) {
    1.1
  } else if (paste(stat_name, nature) %in% DECREASE_NATURE_STATS) {
    0.9
  } else {
    1
  }
}

INCREASE_NATURE_STATS <- c(
  paste("attack", c("lonely", "brave", "adamant", "naughty")),
  paste("defense", c("bold", "relaxed", "impish", "lax")),
  paste("speed", c("timid", "hasty", "jolly", "naive")),
  paste("special-attack", c("modest", "mild", "quiet", "rash")),
  paste("special-defense", c("calm", "gentle", "sassy", "careful"))
)

DECREASE_NATURE_STATS <- c(
  paste("attack", c("bold", "timid", "modest", "calm")),
  paste("defense", c("lonely", "hasty", "mild", "gentle")),
  paste("speed", c("brave", "relaxed", "quiet", "sassy")),
  paste("special-attack", c("adamant", "impish", "jolly", "careful")),
  paste("special-defense", c("naughty", "lax", "naive", "rash"))
)
