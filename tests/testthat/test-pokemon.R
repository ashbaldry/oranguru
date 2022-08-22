test_that("New Pokemon can be generated using PokeAPI data", {
  bulbasaur_data <- readRDS("data/bulbasaur.rds")
  bulbasaur <- pokemon(bulbasaur_data)

  expect_identical(class(bulbasaur), c("pokemon::Pokemon", "R7_object"))
  expect_identical(bulbasaur@name, "Bulbasaur")

  bulbasaur2 <- pokemon(bulbasaur_data, generation = 1L)
  expect_identical(class(bulbasaur2), c("pokemon::Pokemon", "R7_object"))
  expect_identical(bulbasaur2@name, "Bulbasaur")
})
