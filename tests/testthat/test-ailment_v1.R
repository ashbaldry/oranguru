test_that("Poison can be successfully applied (V1)", {
  bulbasaur <- Pokemon$new(api_data = readRDS("data/bulbasaur.rds"))
  charmander <- Pokemon$new(api_data = readRDS("data/charmander.rds"))
  poison_powder <- Move$new("poison-powder")

  expect_output(
    cause_ailment_v1(poison_powder, bulbasaur, charmander),
    "Charmander has been poisoned"
  )
  expect_output(
    charmander$status(),
    "Status: PSN"
  )

  expect_output(
    cause_ailment_v1(poison_powder, bulbasaur, charmander),
    "Charmander is already poisoned, they cannot be poisoned"
  )
})

test_that("Paralysis can be successfully applied (V1)", {
  bulbasaur <- Pokemon$new(api_data = readRDS("data/bulbasaur.rds"))
  charmander <- Pokemon$new(api_data = readRDS("data/charmander.rds"))
  stun_spore <- Move$new("stun-spore")

  expect_output(
    cause_ailment_v1(stun_spore, bulbasaur, charmander),
    "Charmander has been paralysed"
  )
  expect_output(
    charmander$status(),
    "Status: PRZ"
  )

  expect_output(
    cause_ailment_v1(stun_spore, bulbasaur, charmander),
    "Charmander is already paralysed, they cannot be paralysed"
  )
})

test_that("Sleep can be successfully applied (V1)", {
  bulbasaur <- Pokemon$new(api_data = readRDS("data/bulbasaur.rds"))
  charmander <- Pokemon$new(api_data = readRDS("data/charmander.rds"))
  sleep_powder <- Move$new("sleep-powder")

  expect_output(
    cause_ailment_v1(sleep_powder, bulbasaur, charmander),
    "Charmander has been put to sleep"
  )
  expect_output(
    charmander$status(),
    "Status: SLP"
  )

  expect_output(
    cause_ailment_v1(sleep_powder, bulbasaur, charmander),
    "Charmander is already put to sleep, they cannot be put to sleep"
  )
})

test_that("Freeze can be successfully applied (V1)", {
  squirtle <- Pokemon$new(api_data = readRDS("data/squirtle.rds"))
  bulbasaur <- Pokemon$new(api_data = readRDS("data/bulbasaur.rds"))
  ice_beam <- Move$new("ice-beam")

  expect_output(
    cause_ailment_v1(ice_beam, squirtle, bulbasaur),
    "Bulbasaur has been frozen"
  )
  expect_output(
    bulbasaur$status(),
    "Status: FRZ"
  )

  expect_output(
    cause_ailment_v1(ice_beam, squirtle, bulbasaur),
    "Bulbasaur is already frozen, they cannot be frozen"
  )
})

test_that("Burn can be successfully applied (V1)", {
  bulbasaur <- Pokemon$new(api_data = readRDS("data/bulbasaur.rds"))
  charmander <- Pokemon$new(api_data = readRDS("data/charmander.rds"))
  ember <- Move$new("ember")

  expect_output(
    cause_ailment_v1(ember, charmander, bulbasaur),
    "Bulbasaur has been burnt"
  )
  expect_output(
    bulbasaur$status(),
    "Status: BRN"
  )

  expect_output(
    cause_ailment_v1(ember, charmander, bulbasaur),
    "Bulbasaur is already burnt, they cannot be burnt"
  )
})

test_that("Cannot apply two non-volitile stauses", {
  bulbasaur <- Pokemon$new(api_data = readRDS("data/bulbasaur.rds"))
  charmander <- Pokemon$new(api_data = readRDS("data/charmander.rds"))
  poison_powder <- Move$new("poison-powder")
  sleep_powder <- Move$new("sleep-powder")

  expect_output(
    cause_ailment_v1(poison_powder, bulbasaur, charmander),
    "Charmander has been poisoned"
  )
  expect_output(
    cause_ailment_v1(sleep_powder, bulbasaur, charmander),
    "Charmander is already poisoned, they cannot be put to sleep"
  )
})

test_that("Can apply two volitile stauses", {
  bulbasaur <- Pokemon$new(api_data = readRDS("data/bulbasaur.rds"))
  charmander <- Pokemon$new(api_data = readRDS("data/charmander.rds"))
  leech_seed <- Move$new("leech-seed")
  confuse_ray <- Move$new("confuse-ray")

  expect_output(
    cause_ailment_v1(leech_seed, bulbasaur, charmander),
    "Charmander has been leech seeded"
  )
  expect_output(
    cause_ailment_v1(confuse_ray, bulbasaur, charmander),
    "Charmander has been confused"
  )

  expect_output(
    charmander$status(),
    "Status: N/A"
  )
  expect_identical(charmander$get_stat("ailment"), c(18, 6))
})
