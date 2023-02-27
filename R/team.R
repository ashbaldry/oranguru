#' Pokémon Team
#'
#' @description
#' R6 object containing required information about your Pokémon team.
#'
#' @encoding UTF-8
#' @export
PokemonTeam <- R6::R6Class(
  classname = "pokemon_team",

  public = list(
    #' @description
    #' Create a new Pokémon team
    #'
    #' @param pokemon_1,pokemon_2,pokemon_3,pokemon_4,pokemon_5,pokemon_6 Pre-created Pokémon
    #' @param random Logical, should the team be randomised? Default set to `TRUE`
    #' @param level If random, the level the Pokémon should be
    #' @param generation The generation that the Pokémon comes from
    #'
    #' @return
    #' A Team of 6 Pokémon
    initialize = function(pokemon_1, pokemon_2, pokemon_3, pokemon_4, pokemon_5, pokemon_6,
                          random = FALSE, level = 50L, generation = 1L) {
      if (random) {
        pokemon_ids <- get_random_pokemon_id(generation, n = 6L)

        private$pokemon_1 <- Pokemon$new(pokemon = pokemon_ids[1L], level = level, generation = generation)
        private$pokemon_2 <- Pokemon$new(pokemon = pokemon_ids[2L], level = level, generation = generation)
        private$pokemon_3 <- Pokemon$new(pokemon = pokemon_ids[3L], level = level, generation = generation)
        private$pokemon_4 <- Pokemon$new(pokemon = pokemon_ids[4L], level = level, generation = generation)
        private$pokemon_5 <- Pokemon$new(pokemon = pokemon_ids[5L], level = level, generation = generation)
        private$pokemon_6 <- Pokemon$new(pokemon = pokemon_ids[6L], level = level, generation = generation)
      } else if (missing(pokemon_1) || missing(pokemon_2) || missing(pokemon_3) ||
                 missing(pokemon_4) || missing(pokemon_5) || missing(pokemon_6)) {
        stop("Must have a full set of 6 Pokémon to create a team")
      } else {
        private$pokemon_1 <- pokemon_1
        private$pokemon_2 <- pokemon_2
        private$pokemon_3 <- pokemon_3
        private$pokemon_4 <- pokemon_4
        private$pokemon_5 <- pokemon_5
        private$pokemon_6 <- pokemon_6
      }

      private$team_names <- c(
        private$pokemon_1$get_stat("name"),
        private$pokemon_2$get_stat("name"),
        private$pokemon_3$get_stat("name"),
        private$pokemon_4$get_stat("name"),
        private$pokemon_5$get_stat("name"),
        private$pokemon_6$get_stat("name")
      )
    },

    #' @description
    #' Show current status of Pokémon Team
    #'
    #' @param simple Logical, do you just want the simple status (name + HP) printed?
    #'
    #' @encoding UTF-8
    status = function(simple = TRUE) {
      cat("Pokémon Team:\n")
      private$pokemon_1$status(simple = simple)
      private$pokemon_2$status(simple = simple)
      private$pokemon_3$status(simple = simple)
      private$pokemon_4$status(simple = simple)
      private$pokemon_5$status(simple = simple)
      private$pokemon_6$status(simple = simple)
    },

    #' @description
    #' Get the selected Pokémon
    #'
    #' @param slot The number of the position of the Pokémon
    #'
    #' @return
    #' The selected Pokémon
    #'
    #' @encoding UTF-8
    get_pokemon = function(slot) {
      stopifnot(slot %in% seq(6L))
      private[[paste0("pokemon_", slot)]]
    },

    #' @description
    #' Check for healthy Pokémon within the team
    #'
    #' @return
    #' A numeric vector of the positions of the Pokémon that have non-zero HP
    #'
    #' @encoding UTF-8
    healthy_pokemon = function() {
      stats::setNames(
        which(
          c(
            private$pokemon_1$get_stat("current_hp") > 0,
            private$pokemon_2$get_stat("current_hp") > 0,
            private$pokemon_3$get_stat("current_hp") > 0,
            private$pokemon_4$get_stat("current_hp") > 0,
            private$pokemon_5$get_stat("current_hp") > 0,
            private$pokemon_6$get_stat("current_hp") > 0
          )
        ),
        private$team_names
      )
    },

    #' @description
    #' Check whether any Pokémon are healthy to continue battling
    #'
    #' @return
    #' A logical value that says if at least one Pokémon hasn't fainted
    #'
    #' @encoding UTF-8
    able_to_battle = function() {
      private$pokemon_1$get_stat("current_hp") > 0 ||
        private$pokemon_2$get_stat("current_hp") > 0 ||
        private$pokemon_3$get_stat("current_hp") > 0 ||
        private$pokemon_4$get_stat("current_hp") > 0 ||
        private$pokemon_5$get_stat("current_hp") > 0 ||
        private$pokemon_6$get_stat("current_hp") > 0
    },

    #' @description
    #' Get a stat from each Pokémon in team
    #'
    #' @param stat The selected stat to extract
    #'
    #' @return
    #' A vector of length 6, getting the stat of each Pokémon
    get_team_stat = function(stat) {
      c(
        private$pokemon_1$get_stat(stat),
        private$pokemon_2$get_stat(stat),
        private$pokemon_3$get_stat(stat),
        private$pokemon_4$get_stat(stat),
        private$pokemon_5$get_stat(stat),
        private$pokemon_6$get_stat(stat)
      )

    }
  ),

  private = list(
    pokemon_1 = NULL,
    pokemon_2 = NULL,
    pokemon_3 = NULL,
    pokemon_4 = NULL,
    pokemon_5 = NULL,
    pokemon_6 = NULL,

    team_names = NULL
  )
)
