#' Pokémon Move
#'
#' @description
#' R6 object containing required information about a Pokémon move.
#'
#' @encoding UTF-8
#' @export
Move <- R6::R6Class(
  classname = "move",
  public = list(
    #' @description
    #' Create a Move
    #'
    #' @param move_name The hyphen cased name of the move e.g. `"razor-leaf"`
    #'
    #' @return
    #' A Move
    #'
    #' @encoding UTF-8
    initialize = function(move_name) {
      private$name <- move_name

      pp <- get_move_info(move_name, "pp")
      private$pp <- pp
      private$curr_pp <- pp

      private$type_id <- get_move_info(move_name, "type_id")
      private$power <- get_move_info(move_name, "power")
      private$accuracy <- get_move_info(move_name, "accuracy")
      private$priority <- get_move_info(move_name, "priority")
      private$target_id <- get_move_info(move_name, "target_id")
      private$damage_class_id <- get_move_info(move_name, "damage_class_id")
      private$effect_id <- get_move_info(move_name, "effect_id")
      private$effect_chance <- get_move_info(move_name, "effect_chance")
    },

    #' @description
    #' Get the stat of the move
    #'
    #' @param stat The private field of the move
    #'
    #' @encoding UTF-8
    get_stat = function(stat) {
      if (stat %nin% names(private)) {
        stop(stat, " not available for Move")
      }
      private[[stat]]
    }
  ),
  private = list(
    name = NULL,

    pp = NULL,
    curr_pp = NULL,

    type_id = NULL,
    power = NULL,
    accuracy = NULL,
    priority = NULL,
    target_id = NULL,
    damage_class_id = NULL,
    effect_id = NULL,
    effect_chance = NULL
  )
)
