test_that("the healing method of the Pokemon class only heals when not at full health", {
  pikachu <- Pokemon$new(api_data = readRDS("data/pikachu.rds"))

  expect_output(
    pikachu$heal(perc = 10L),
    "Pikachu is currently at full health!"
  )

  expect_output(pikachu$take_damage(20L))
  expect_output(
    pikachu$heal(n = 20L),
    "Pikachu has recovered 20 HP"
  )

  expect_output(pikachu$take_damage(20L))
  expect_output(
    pikachu$heal(perc = 20L),
    "Pikachu has recovered 19 HP"
  )
})

test_that("the healing method of the Pokemon class does not work if fainted", {
  pikachu <- Pokemon$new(api_data = readRDS("data/pikachu.rds"))

  expect_output(pikachu$take_damage(95L))
  expect_output(
    pikachu$heal(perc = 20L),
    "Pikachu is fainted, cannot recover health"
  )
})

test_that("healing moves work (V1)", {
  recover <- Move$new("recover")
  pikachu <- Pokemon$new(api_data = readRDS("data/pikachu.rds"))

  expect_output(
    heal_pokemon_v1(recover, pikachu, pikachu),
    "Pikachu is currently at full health!"
  )

  expect_output(pikachu$take_damage(80L))
  expect_output(
    heal_pokemon_v1(recover, pikachu, pikachu),
    "Pikachu has recovered 47 HP"
  )

  pikachu_gen_7 <- Pokemon$new(api_data = readRDS("data/pikachu.rds"), generation = 7L)
  expect_output(pikachu_gen_7$take_damage(80L))
  expect_output(
    heal_pokemon_v1(recover, pikachu_gen_7, pikachu_gen_7),
    "Pikachu has recovered 48 HP"
  )
})
