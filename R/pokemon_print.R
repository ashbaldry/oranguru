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
#' @rdname status
#' @encoding UTF-8
showStatus <- function(x, simple = FALSE) {
  if (simple) {
    showSimpleStatus(x)
  } else {
    showFullStatus(x)
  }
}

showSimpleStatus <- function(x) {
  cat(x$name, " (", x$hp, " / ", x$current_hp, ") \n", sep = "")
}

showFullStatus <- function(x) {
  cat(
    "Pokémon: ", x$name, "\n",
    "Type", if (length(x$type) > 1) "s" else "", ": ", toString(x$type), "\n",
    "\n",
    "Stats\n",
    "HP: ", x$current_hp, " / ", x$hp, "\n",
    "Attack: ", x$attack, "\n",
    "Defense: ", x$defense, "\n",
    "Sp. Attack: ", x$sp_attack, "\n",
    "Sp. Defense: ", x$sp_defense, "\n",
    "Speed: ", x$speed, "\n",
    "\n",
    "Moves (Curent/Max PP) \n",
    x$move_1, " (", x$move_1_pp, " / ", x$move_1_current_pp, ")\n",
    if (!is.na(x$move_2)) paste0(x$move_2, " (", x$move_2_pp, " / ", x$move_2_current_pp, ")\n"),
    if (!is.na(x$move_3)) paste0(x$move_3, " (", x$move_3_pp, " / ", x$move_3_current_pp, ")\n"),
    if (!is.na(x$move_4)) paste0(x$move_4, " (", x$move_4_pp, " / ", x$move_4_current_pp, ")\n"),
    sep = ""
  )
}
