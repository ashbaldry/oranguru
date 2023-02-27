test_that("get random ID chooses a number correctly", {
  expect_lte(get_random_pokemon_id(generation = 1L, n = 1L), 151L)
  expect_lte(get_random_pokemon_id(generation = 2L, n = 1L), 251L)
})

test_that("remove_alternate_forms correctly keeps standard Pokemon", {
  expect_lte(max(remove_alternate_forms(pokemon_generation$pokemon_id)), 10000L)
})
