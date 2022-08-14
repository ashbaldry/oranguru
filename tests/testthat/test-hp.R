test_that("Hit Point calculations are correct", {
  # Using results from https://bulbapedia.bulbagarden.net/wiki/Stat
  hp <- calculate_hp_v1(35, 81, 7, 22850)
  expect_identical(hp, 189L)

  hp <- calculate_hp_v2(108, 78, 24, 74)
  expect_identical(hp, 289L)
})
