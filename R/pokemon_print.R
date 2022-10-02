#' Print Current Pokémon Status
#'
#' @description
#' A way to show the current stats and status of a selected Pokémon.
#'
#' Includes:
#'
#' - Pokémon base stats and health
#' - Pokemon move set and PP
#'
#' @include pokemon.R team.R
#'
#' @rdname status
#' @encoding UTF-8
#' @export
status <- R7::new_generic("status", "x", function(x) R7::R7_dispatch())

R7::method(status, pokemon) <- function(x) {
  cat(
    "Pokémon: ", x@name, "\n",
    "Type", if (length(x@type) > 1) "s" else "", ": ", toString(x@type), "\n",
    "\n",
    "Stats\n",
    "HP: ", x@current_hp, " / ", x@hp, "\n",
    "Attack: ", x@attack, "\n",
    "Defense: ", x@defense, "\n",
    "Sp. Attack: ", x@sp_attack, "\n",
    "Sp. Defense: ", x@sp_defense, "\n",
    "Speed: ", x@speed, "\n",
    "\n",
    "Moves (Curent/Max PP) \n",
    x@move_1, " (", x@move_1_pp, " / ", x@move_1_current_pp, ")\n",
    if (!is.na(x@move_2)) paste0(x@move_2, " (", x@move_2_pp, " / ", x@move_2_current_pp, ")\n"),
    if (!is.na(x@move_3)) paste0(x@move_3, " (", x@move_3_pp, " / ", x@move_3_current_pp, ")\n"),
    if (!is.na(x@move_4)) paste0(x@move_4, " (", x@move_4_pp, " / ", x@move_4_current_pp, ")\n"),
    sep = ""
  )
}

R7::method(status, team) <- function(x) {
  cat(
    "Pokémon Team:\n",
    x@pokemon_1@name, " (", x@pokemon_1@hp, " / ", x@pokemon_1@current_hp, ") \n",
    x@pokemon_2@name, " (", x@pokemon_2@hp, " / ", x@pokemon_2@current_hp, ") \n",
    x@pokemon_3@name, " (", x@pokemon_3@hp, " / ", x@pokemon_3@current_hp, ") \n",
    x@pokemon_4@name, " (", x@pokemon_4@hp, " / ", x@pokemon_4@current_hp, ") \n",
    x@pokemon_5@name, " (", x@pokemon_5@hp, " / ", x@pokemon_5@current_hp, ") \n",
    x@pokemon_6@name, " (", x@pokemon_6@hp, " / ", x@pokemon_6@current_hp, ") \n",
    sep = ""
  )
}

R7::method(status, battle) <- function(x) {
  cat("Player 1:\n")
  status(x@team_1)
  cat("\nPlayer 2:\n")
  status(x@team_2)
  cat("\n")
}
