#' Print Current Pokémon Status
#'
#' @include pokemon.R team.R
#' @export
print <- R7::new_generic("print", "x")

R7::method(print, pokemon) <- function(x) {
  cat(
    "Pokémon: ", x@name, "\n",
    "Type", if (length(x@type) > 1) "s" else "", ": ", toString(x@type), "\n",
    "\n",
    "Stats\n",
    "HP: ", x@hp, " / ", x@current_hp, "\n",
    "Attack: ", x@attack, "\n",
    "Defense: ", x@defense, "\n",
    "Sp. Attack: ", x@sp_attack, "\n",
    "Sp. Defense: ", x@sp_defense, "\n",
    "Speed: ", x@speed, "\n",
    "\n",
    "Moves (PP) \n",
    x@move_1, " (", x@move_1_pp, " / ", x@move_1_current_pp, ")\n",
    if (!is.na(x@move_2)) paste0(x@move_2, " (", x@move_2_pp, " / ", x@move_2_current_pp, ")\n"),
    if (!is.na(x@move_3)) paste0(x@move_3, " (", x@move_3_pp, " / ", x@move_3_current_pp, ")\n"),
    if (!is.na(x@move_4)) paste0(x@move_4, " (", x@move_4_pp, " / ", x@move_4_current_pp, ")\n"),
    sep = ""
  )
}

R7::method(print, team) <- function(x) {
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
