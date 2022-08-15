#' Calculating Pokémon Hit Points
#'
#' @description
#' Find the HP of a Pokémon given its base HP stat, along with other
#' information such as its level, IVs and EVs, and the generation.
#'
#' @param base_hp The base hit point stat a Pokémon has
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
#' @seealso \code{\link{calculate_stat}}
#'
#' @encoding UTF-8
#' @export
calculate_hp <- function(base_hp, level = 50, iv = 0, ev = 0, generation = 8) {
  check_level(level)
  check_iv(iv)

  if (generation < 3) {
    calculate_hp_v1(base_hp, level, dv = iv, ev = ev)
  } else {
    calculate_hp_v2(base_hp, level, iv = iv, ev = ev)
  }
}

#' @description
#' `calculate_hp_range` gives a potential range of the HP stat, though
#' this is assuming that no EVs have been added to the Pokémon.
#'
#' @rdname calculate_hp
#' @export
calculate_hp_range <- function(base_hp, level = 50, generation = 8) {
  if (generation < 3) {
    min_hp <- calculate_hp_v1(base_hp, level, dv = 0)
    max_hp <- calculate_hp_v1(base_hp, level, dv = 15)
  } else {
    min_hp <- calculate_hp_v2(base_hp, level, iv = 0)
    max_hp <- calculate_hp_v2(base_hp, level, iv = 31)
  }

  cat("HP Range:", min_hp, "-", max_hp, "\n")
  invisible(c(min_hp, max_hp))
}

calculate_hp_v1 <- function(base_hp, level = 50, dv = 0, ev = 0) {
  float_hp <- (((base_hp + dv) * 2 + sqrt(ev) / 4) * level) / 100 + level + 10
  as.integer(float_hp)
}

calculate_hp_v2 <- function(base_hp, level = 50, iv = 0, ev = 0) {
  float_hp <- ((2 * base_hp + iv + ev / 4) * level) / 100 + level + 10
  as.integer(float_hp)
}
