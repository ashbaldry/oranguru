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
    #' @param generation The generation that the Pokémon comes from
    #'
    #' @return
    #' A Team of 6 Pokémon
    initialize = function(pokemon_1, pokemon_2, pokemon_3, pokemon_4, pokemon_5, pokemon_6,
                          random = FALSE, generation = 1L) {
      if (random) {
        pokemon_ids <- get_random_pokemon_id(generation, n = 6L)

        private$pokemon_1 <- Pokemon$new(pokemon = pokemon_ids[1L], generation = generation)
        private$pokemon_2 <- Pokemon$new(pokemon = pokemon_ids[2L], generation = generation)
        private$pokemon_3 <- Pokemon$new(pokemon = pokemon_ids[3L], generation = generation)
        private$pokemon_4 <- Pokemon$new(pokemon = pokemon_ids[4L], generation = generation)
        private$pokemon_5 <- Pokemon$new(pokemon = pokemon_ids[5L], generation = generation)
        private$pokemon_6 <- Pokemon$new(pokemon = pokemon_ids[6L], generation = generation)
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
    #' Checks whether any Pokémon are healthy to continue battling
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
    }
  ),

  private = list(
    pokemon_1 = NULL,
    pokemon_2 = NULL,
    pokemon_3 = NULL,
    pokemon_4 = NULL,
    pokemon_5 = NULL,
    pokemon_6 = NULL
  )
)
