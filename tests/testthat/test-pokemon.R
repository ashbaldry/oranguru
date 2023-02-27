test_that("New Pokemon can be generated using PokeAPI data", {
  bulbasaur_data <- readRDS("data/bulbasaur.rds")
  bulbasaur <- Pokemon$new(api_data = bulbasaur_data, generation = 1L)

  expect_identical(class(bulbasaur), c("pokemon", "R6"))
  expect_identical(bulbasaur$get_stat("name"), "Bulbasaur")
  expect_length(bulbasaur$get_moves(), 4L)

  bulbasaur2 <- Pokemon$new(api_data = bulbasaur_data, generation = 8L)
  expect_identical(class(bulbasaur2), c("pokemon", "R6"))
  expect_identical(bulbasaur2$get_stat("name"), "Bulbasaur")
})

set.seed(2312L)
bulbasaur_data <- readRDS("data/bulbasaur.rds")
# Moves: "double-edge" "razor-leaf"  "solar-beam"  "tackle"
bulbasaur <- Pokemon$new(api_data = bulbasaur_data, generation = 1L)

test_that("Pokemon prints status successfully", {
  expect_output(bulbasaur$status(), "Pokémon: Bulbasaur\n")
  expect_output(bulbasaur$status(simple = TRUE), r"(Bulbasaur \(\d+ / \d+\))")
})

test_that("Possible to get private Pokemon stats", {
  expect_identical(bulbasaur$get_stat("attack"), bulbasaur$.__enclos_env__$private$attack)
  expect_error(bulbasaur$get_stat("not_a_stat"))
})

test_that("Critical hit chance extractable from Pokemon attack", {
  crit_chance <- bulbasaur$get_crit_chance("double-edge")
  expect_type(crit_chance, "double")
  expect_gte(crit_chance, 0L)
  expect_lte(crit_chance, 1L)

  expect_error(bulbasaur$get_crit_chance("not-an-attack"))
})

test_that("Pokemon is able to learn a new valid move", {
  bulbasaur$change_move("body-slam", "tackle")
  on.exit({
    bulbasaur$change_move("tackle", 4L)
    expect_identical(bulbasaur$get_stat("move_4_pp"), 35L)
    expect_identical(bulbasaur$get_stat("move_4_current_pp"), 35L)
  })

  expect_identical(bulbasaur$get_stat("move_4"), "body-slam")
  expect_identical(bulbasaur$get_stat("move_4_pp"), 15L)
  expect_identical(bulbasaur$get_stat("move_4_current_pp"), 15L)
})

test_that("Pokemon is unable to learn invalid move", {
  expect_error(bulbasaur$change_move("not-a-move", "tackle"))
})
