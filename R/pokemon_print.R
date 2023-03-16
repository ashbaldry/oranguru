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
  pkmn_status <- intersect(x$ailment, seq(5L))
  if (length(pkmn_status) == 1L) {
    ailment <- paste0(" ", names(AILMENT_STATUS)[match(pkmn_status, AILMENT_STATUS)])
  } else {
    ailment <- ""
  }

  func(x$name, ailment, " (", x$current_hp, " / ", x$hp, ") \n", sep = "")
}

show_full_status <- function(x, func = cat) {
  known_moves <- c(
    x$move_1$get_stat("name"),
    if (is.null(x$move_2)) NULL else x$move_2$get_stat("name"),
    if (is.null(x$move_3)) NULL else x$move_3$get_stat("name"),
    if (is.null(x$move_4)) NULL else x$move_4$get_stat("name")
  )

  move_max_nchar <- max(c(9, nchar(known_moves)))
  move_spaces <- pmax((2 + move_max_nchar - nchar(known_moves)) / 2, 1)
  moves_padded <- sprintf("%-*s%s%*s|", move_spaces, "", known_moves, ceiling(move_spaces), "")

  known_pp <- c(
    x$move_1$get_stat("pp"),
    if (is.null(x$move_2)) NULL else x$move_2$get_stat("pp"),
    if (is.null(x$move_3)) NULL else x$move_3$get_stat("pp"),
    if (is.null(x$move_4)) NULL else x$move_4$get_stat("pp")
  )

  known_curr_pp <- c(
    x$move_1$get_stat("curr_pp"),
    if (is.null(x$move_2)) NULL else x$move_2$get_stat("curr_pp"),
    if (is.null(x$move_3)) NULL else x$move_3$get_stat("curr_pp"),
    if (is.null(x$move_4)) NULL else x$move_4$get_stat("curr_pp")
  )

  pp_spaces <- (move_max_nchar - 5) / 2
  pp_padded <- sprintf("%-*s%2d / %2d%*s|", pp_spaces, "", known_curr_pp, known_pp, ceiling(pp_spaces), "")

  func(
    "Pokémon: ", x$name, "\n",
    "Type", if (length(x$type) > 1) "s" else "", ": ", toString(x$type), "\n",
    "\n",
    "|     HP    | Attack | Defense | Sp. Atk | Sp. Def |  Speed  |\n",
    "| ", sprintf("%3d", x$current_hp), " / ", sprintf("%-3d", x$hp), " ",
    "|  ", sprintf("%3d", x$attack), "   ",
    "|   ", sprintf("%3d", x$defense), "   ",
    "|   ", sprintf("%3d", x$sp_attack), "   ",
    "|   ", sprintf("%3d", x$sp_defense), "   ",
    "|   ", sprintf("%3d", x$speed), "   |\n",
    "\n",
    "|", paste0(moves_padded, collapse = ""), "\n",
    "|", paste0(pp_padded, collapse = ""), "\n",
    sep = ""
  )
}
