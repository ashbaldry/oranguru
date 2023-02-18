# Type to type IDs: `https://github.com/PokeAPI/pokeapi/blob/master/data/v2/csv/types.csv`
test_that("include_type_multiplier works for constant type matches", {
  # Normal: 1
  expect_equal(include_type_multiplier(move_type = 1L, pokemon_types = 1L), 1L)

  # Grass: 12, Water: 11
  expect_equal(include_type_multiplier(move_type = 12L, pokemon_types = 11L), 2L)
  expect_equal(include_type_multiplier(move_type = 11L, pokemon_types = 12L), 0.5)

  # Normal: 1, Ghost: 8
  expect_equal(include_type_multiplier(move_type = 1L, pokemon_types = 8L), 0L)
  expect_equal(include_type_multiplier(move_type = 8L, pokemon_types = 1L), 0L)
})

test_that("include_type_multiplier works for dual types matches", {
  # Fire: 10, Ice: 15, Grass: 12
  expect_equal(include_type_multiplier(move_type = 10L, pokemon_types = c(12L, 15L)), 4L)

  # Fighting: 2, Normal: 1, Ghost: 8
  expect_equal(include_type_multiplier(move_type = 2L, pokemon_types = c(1L, 8L)), 0L)

  # Grass: 12, Fire: 10, Water: 11
  expect_equal(include_type_multiplier(move_type = 12L, pokemon_types = c(10L, 11L)), 1L)
})

test_that("include_type_multiplier adapts for generation changes", {
  # Ice: 15, Fire: 10, Grass: 12
  expect_equal(include_type_multiplier(move_type = 15L, pokemon_types = c(12L, 10L)), 2L)
  expect_equal(include_type_multiplier(move_type = 15L, pokemon_types = c(12L, 10L), generation = 8L), 1L)
})
