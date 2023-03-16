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

      move_info <- subset(moves, identifier == move_name)
      move_meta <- subset(moves_meta, move_id == move_info$id)

      private$pp <- move_info$pp
      private$curr_pp <- move_info$pp
      private$type_id <- move_info$type_id
      private$power <- move_info$power
      private$accuracy <- move_info$accuracy
      private$priority <- move_info$priority
      private$target_id <- move_info$target_id
      private$damage_class_id <- move_info$damage_class_id
      private$effect_id <- move_info$effect_id
      private$effect_chance <- move_info$effect_chance

      private$meta_category_id <- move_meta$meta_category_id
      private$meta_ailment_id <- move_meta$meta_ailment_id
      private$min_hits <- move_meta$min_hits
      private$max_hits <- move_meta$max_hits
      private$min_turns <- move_meta$min_turns
      private$max_turns <- move_meta$max_turns
      private$drain <- move_meta$drain
      private$healing <- move_meta$healing
      private$crit_rate <- move_meta$crit_rate
      private$ailment_chance <- move_meta$ailment_chance
      private$flinch_chance <- move_meta$flinch_chance
      private$stat_chance <- move_meta$stat_chance
    },

    #' @description
    #' Get current PP of the move
    #'
    #' @encoding UTF-8
    get_pp_status = function() {
      paste0(private$name, " (", private$curr_pp, " / ", private$pp, ")")
    },

    #' @description
    #' Get the stat of the move
    #'
    #' @param stat The private field of the move
    #'
    #' @encoding UTF-8
    get_stat = function(stat) {
      if (stat %nin% names(private)) {
        stop(stat, " not available for Move object")
      }
      private[[stat]]
    },

    #' @description
    #' Use the Move
    #'
    #' @return
    #' Logical as to whether or not the move has been used or not
    use_move = function() {
      if (private$curr_pp == 0L) {
        FALSE
      } else {
        private$curr_pp <- private$curr_pp - 1L
        TRUE
      }
    }
  ),
  private = list(
    name = NULL,
    full_name = NULL,

    pp = NULL,
    curr_pp = NULL,

    type_id = NULL,
    power = NULL,
    accuracy = NULL,
    priority = NULL,
    target_id = NULL,
    damage_class_id = NULL,
    effect_id = NULL,
    effect_chance = NULL,

    meta_category_id = NULL,
    meta_ailment_id = NULL,
    min_hits = NULL,
    max_hits = NULL,
    min_turns = NULL,
    max_turns = NULL,
    drain = NULL,
    healing = NULL,
    crit_rate = NULL,
    ailment_chance = NULL,
    flinch_chance = NULL,
    stat_chance = NULL
  )
)
