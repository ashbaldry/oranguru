#' Print Current Pokémon Status
#'
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
    x@move_2, " (", x@move_2_pp, " / ", x@move_2_current_pp, ")\n",
    x@move_3, " (", x@move_3_pp, " / ", x@move_3_current_pp, ")\n",
    x@move_4, " (", x@move_4_pp, " / ", x@move_4_current_pp, ")\n",
    sep = ""
  )
}
