test_that("New Pokemon can be generated using PokeAPI data", {
  bulbasaur_data <- readRDS("data/bulbasaur.rds")
  bulbasaur <- Pokemon$new(bulbasaur_data)

  expect_identical(class(bulbasaur), c("pokemon", "R6"))
  expect_identical(bulbasaur$.__enclos_env__$private$name, "Bulbasaur")

  bulbasaur2 <- Pokemon$new(bulbasaur_data, generation = 1L)
  expect_identical(class(bulbasaur2), c("pokemon", "R6"))
  expect_identical(bulbasaur2$.__enclos_env__$private$name, "Bulbasaur")
})
