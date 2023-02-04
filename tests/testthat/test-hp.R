test_that("Hit Point calculations are correct", {
  # Using results from https://bulbapedia.bulbagarden.net/wiki/Stat
  hp <- calculate_hp_v1(base_hp = 35, level = 81, dv = 7, ev = 22850)
  expect_identical(hp, 189L)
  hp <- calculate_hp(base_hp = 35, level = 81, iv = 7, ev = 22850, generation = 1)
  expect_identical(hp, 189L)

  hp <- calculate_hp_v2(base_hp = 108, level = 78, iv = 24, ev = 74)
  expect_identical(hp, 289L)
  hp <- calculate_hp(base_hp = 108, level = 78, iv = 24, ev = 74, generation = 8)
  expect_identical(hp, 289L)
})
