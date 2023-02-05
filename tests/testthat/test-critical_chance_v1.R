test_that("calculate_critical_chance_v1 is calculating critical chance correctly", {
  # Using formula from https://bulbapedia.bulbagarden.net/wiki/Critical_hit

  basic_attack <- calculate_critical_chance_v1("tackle", speed = 50L, high_crit = 0L)
  expect_identical(basic_attack, 25L / 256L)

  basic_attack_high_crit <- calculate_critical_chance_v1("tackle", speed = 50L, high_crit = 1L)
  expect_identical(basic_attack_high_crit, 6L / 256L)

  high_crit_attack <- calculate_critical_chance_v1("razor-leaf", speed = 50L, high_crit = 0L)
  expect_identical(high_crit_attack, 200L / 256L)

  high_crit_attack_high_crit <- calculate_critical_chance_v1("razor-leaf", speed = 50L, high_crit = 1L)
  expect_identical(high_crit_attack_high_crit, 48L / 256L)
})
