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
                          random = TRUE, generation = 8L) {
      if (random) {
        pokemon_ids <- get_random_pokemon_id(generation, n = 6)

        private$pokemon_1 <- Pokemon$new(pokemon = pokemon_ids[1], generation = generation)
        private$pokemon_2 <- Pokemon$new(pokemon = pokemon_ids[2], generation = generation)
        private$pokemon_3 <- Pokemon$new(pokemon = pokemon_ids[3], generation = generation)
        private$pokemon_4 <- Pokemon$new(pokemon = pokemon_ids[4], generation = generation)
        private$pokemon_5 <- Pokemon$new(pokemon = pokemon_ids[5], generation = generation)
        private$pokemon_6 <- Pokemon$new(pokemon = pokemon_ids[6], generation = generation)
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
    status = function() {
      cat("Pokémon Team:\n")
      private$pokemon_1$status(simple = TRUE)
      private$pokemon_2$status(simple = TRUE)
      private$pokemon_3$status(simple = TRUE)
      private$pokemon_4$status(simple = TRUE)
      private$pokemon_5$status(simple = TRUE)
      private$pokemon_6$status(simple = TRUE)
    }
  ),

  private = list(
    active = 1L,
    pokemon_1 = NULL,
    pokemon_2 = NULL,
    pokemon_3 = NULL,
    pokemon_4 = NULL,
    pokemon_5 = NULL,
    pokemon_6 = NULL
  )
)
