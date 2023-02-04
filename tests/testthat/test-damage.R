test_that("Damage calculations are correct", {
  # Using results from https://bulbapedia.bulbagarden.net/wiki/Damage
  damage <- calculate_damage_v1(
    power, p1_attack, p2_defense, p1_stab, p2_types,
    level = 50L, critical = FALSE
  )
  expect_identical(damage, 189L)
  damage <- calculate_damage(
    power, p1_attack, p2_defense, p1_stab, p2_types,
    level = 50L, critical = FALSE, generation = 1L
  )
  expect_identical(damage, 189L)
})
