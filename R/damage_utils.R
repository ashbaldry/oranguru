#' Critical Hit Multiplier
#'
#' @description
#' Calculation on what the damage multiplier will be depending on whether or not a critical
#' hit lands on the opposing Pokémon
#'
#' @param critical_chance The probability of a critical hit landing
#' @param generation The generation of the move
#'
#' @noRd
include_crit_multipler <- function(critical_chance, generation = 1L) {
  if (is_critical_hit(critical_chance)) {
    2
  } else {
    1
  }
}

#' Critical Hit Landed
#'
#' @description
#' A simple check on whether or not the Pokémon lands a critical hit
#'
#' @param chance The probability of a critical hit landing
#'
#' @return
#' A logical value whether or not the Pokémon lands a critical hit
#'
#' @noRd
is_critical_hit <- function(chance) {
  critical <- min(chance, 1L)
  sample(c(FALSE, TRUE), size = 1L, prob = c(1 - chance, chance))
}

#' Random Factor
#'
#' @description
#' A random factor added to an attack, to allow for variability in the damage dealt
#'
#' @noRd
include_random_factor <- function() {
  runif(1L, 217L, 255L) / 255L
}

#' Type Multiplier
#'
#' @description
#' Finding the multiplier of damage dealt to the defending Pokémon depending on the typing
#' of the move and the defending types
#'
#' @param move_type The ID of type of the move used
#' @param pokemon_types A vector of the IDs of types of the defending Pokémon
#' @param generation The generation of the move
#'
#' @noRd
include_type_multiplier <- function(move_type, pokemon_types, generation = 1L) {
  if (generation <= 6L) {
    move_efficacy <- rbind(
      subset(type_efficacy_past, generation_id >= generation, select = names(type_efficacy)),
      type_efficacy
    )
    move_efficacy <- move_efficacy[!duplicated(move_efficacy[, c("damage_type_id", "target_type_id")]), ]
  } else {
    move_efficacy <- type_efficacy
  }

  multiplier <- subset(
    move_efficacy,
    damage_type_id == move_type & target_type_id %in% pokemon_types,
    select = "damage_factor",
    drop = TRUE
  )

  prod(multiplier / 100)
}

#' STAB Multiplier
#'
#' @description
#' Finding out whether the type of the moved used matches the attacking Pokémon type.
#' If so then a multiplier of 1.5 is given, otherwise it is 1.
#'
#' @param move_type The ID of type of the move used
#' @param pokemon_types A vector of the IDs of types of the attacking Pokémon
include_stab_multiplier <- function(move_type, pokemon_types) {
  if (move_type %in% pokemon_types) 1.5 else 1
}
