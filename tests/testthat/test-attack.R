test_that("calculate_attack is only available for generation 1", {
  # Using formula from https://bulbapedia.bulbagarden.net/wiki/Critical_hit
  basic_attack <- calculate_critical_chance("tackle", speed = 50L, high_crit = 0L, generation = 1L)
  expect_identical(basic_attack, 25L / 256L)

  expect_error(calculate_critical_chance("tackle", speed = 50L, high_crit = 0L, generation = 2L))
})
