#' Calculate Stat Multiplier
#'
#' @description
#' Given a stat and the amount increased/decreased, a multiplier can be applied to that stat
#'
#' @param stat Name of the stat
#' @param stage Number of stages the stat has increased/decreased
#' @param generation Generation of the batlle
#'
#' @seealso \url{https://bulbapedia.bulbagarden.net/wiki/Stat_modifier#In-battle_modification}
#' @export
calculate_stat_multiplier <- function(stat, stage = 0L, generation = 1L) {
  if (stat %in% c("accuracy", "evasion")) {
    calculate_accuracy_multiplier(stage = stage, generation = generation)
  } else {
    calculate_base_stat_multiplier(stage = stage, generation = generation)
  }
}

calculate_accuracy_multiplier <- function(stage = 0L, generation = 1L) {
  switch(
    as.character(generation),
    "1" = c(0.25, 0.28, 0.33, 0.4, 0.5, 0.66, 1, 1.5, 2, 2.5, 3, 3.5, 4)[stage + 7L],
    "2" = c(0.33, 0.36, 0.43, 0.5, 0.66, 0.75, 1, 1.33, 1.66, 2, 2.33, 2.66, 3)[stage + 7L],
    "3" = , "4" = c(0.33, 0.36, 0.43, 0.5, 0.66, 0.75, 1, 1.33, 1.66, 2, 2.5, 2.66, 3)[stage + 7L],
    c(3 / seq(9, 4), 1, seq(4, 9) / 3)[stage + 7L]
  )
}

calculate_base_stat_multiplier <- function(stage = 0L, generation = 1L) {
  switch(
    as.character(generation),
    "1" = , "2" = c(0.25, 0.28, 0.33, 0.4, 0.5, 0.66, 1, 1.5, 2, 2.5, 3, 3.5, 4)[stage + 7L],
    c(2 / seq(8, 3), 1, seq(3, 8) / 2)[stage + 7L]
  )
}
