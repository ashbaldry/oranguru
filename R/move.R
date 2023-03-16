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
    name = character(0L),
    full_name = character(0L),

    pp = integer(0L),
    curr_pp = integer(0L),

    type_id = integer(0L),
    power = integer(0L),
    accuracy = integer(0L),
    priority = integer(0L),
    target_id = integer(0L),
    damage_class_id = integer(0L),
    effect_id = integer(0L),
    effect_chance = integer(0L),

    meta_category_id = integer(0L),
    meta_ailment_id = integer(0L),
    min_hits = integer(0L),
    max_hits = integer(0L),
    min_turns = integer(0L),
    max_turns = integer(0L),
    drain = integer(0L),
    healing = integer(0L),
    crit_rate = integer(0L),
    ailment_chance = integer(0L),
    flinch_chance = integer(0L),
    stat_chance = integer(0L)
  )
)
