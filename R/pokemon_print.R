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
#' @param x The private fields of a Pokémon
#' @param simple Logical, should a simplified print be used
#' @param console Logical, should the status be printed or returned as a string?
#'
#' @encoding UTF-8
#' @rdname status
show_status <- function(x, simple = FALSE, console = TRUE) {
  func <- if (console) cat else paste
  if (simple) {
    show_simple_status(x, func = func)
  } else {
    show_full_status(x, func = func)
  }
}

show_simple_status <- function(x, func = cat) {
  func(x$name, " (", x$hp, " / ", x$current_hp, ") \n", sep = "")
}

show_full_status <- function(x, func = cat) {
  func(
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
