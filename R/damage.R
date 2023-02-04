#' Calculate Attack Damage
#'
#' @export
calculate_damage <- function(power, p1_attack, p2_defense, p1_stab, p2_types, ...,
                             level = 50L, critical = FALSE, generation = 1L) {
  check_level(level)

  if (generation == 1L) {
    calculate_damage_v1(power, p1_attack, p2_defense, p1_stab, p2_types, level, critical)
  } else {
    stop("Damage has not been calculated yet for this generation")
  }
}

calculate_damage_v1 <- function(power, p1_attack, p2_defense, p1_stab, p2_types,
                                level = 50, critical = FALSE) {
  if (any(p1_attack, p2_defense) > 255L) {
    p1_attack <- floor(p1_attack / 4L)
    p2_defense <- floor(p2_defense / 4L)
  }

  (((2 * level * (critical + 1) / 5) + 2) * power * p1_attack / p2_defense / 50 + 2)
}
