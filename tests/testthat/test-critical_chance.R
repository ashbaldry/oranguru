test_that("calculate_critical_chance calculates the chance a critical hit lands", {
  # Using formula from https://bulbapedia.bulbagarden.net/wiki/Critical_hit
  tackle <- Move$new("tackle")
  basic_attack <- calculate_critical_chance(tackle, speed = 50L, high_crit = 0L, generation = 1L)
  expect_identical(basic_attack, 25L / 256L)

  razor_leaf <- Move$new("razor-leaf")
  high_crit_attack <- calculate_critical_chance(razor_leaf, speed = 50L, high_crit = 0L, generation = 1L)
  expect_identical(high_crit_attack, 200L / 256L)
})

test_that("calculate_critical_chance is only available for generation 1", {
  tackle <- Move$new("tackle")
  expect_error(calculate_critical_chance(tackle, speed = 50L, high_crit = 0L, generation = 2L))
})
