#' @noRd
calculate_damage <- function(power, p1_attack, p2_defense, p1_stab, p2_types, ...,
                             level = 50L, critical = FALSE, generation = 1L) {
  check_level(level)

  if (generation == 1L) {
    calculate_damage_v1(power, p1_attack, p2_defense, p1_stab, p2_types, level, critical)
  } else {
    stop("Damage has not been calculated yet for this generation")
  }
}
