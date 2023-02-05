test_that("New Pokemon can be generated using PokeAPI data", {
  bulbasaur_data <- readRDS("data/bulbasaur.rds")
  bulbasaur <- Pokemon$new(api_data = bulbasaur_data, generation = 1L)

  expect_identical(class(bulbasaur), c("pokemon", "R6"))
  expect_identical(bulbasaur$get_stat("name"), "Bulbasaur")
  expect_length(bulbasaur$get_moves(), 4L)

  crit_chance <- bulbasaur$get_crit_chance(bulbasaur$get_stat("move_1"))
  expect_type(crit_chance, "double")
  expect_gte(crit_chance, 0L)
  expect_lte(crit_chance, 1L)

  bulbasaur2 <- Pokemon$new(api_data = bulbasaur_data, generation = 8L)
  expect_identical(class(bulbasaur2), c("pokemon", "R6"))
  expect_identical(bulbasaur2$get_stat("name"), "Bulbasaur")
})
