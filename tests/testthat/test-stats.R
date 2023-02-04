test_that("Base Stat calculations are correct", {
  # Using results from https://bulbapedia.bulbagarden.net/wiki/Stat
  # Gen 1-2
  attack <- calculate_stat_v1(base_stat = 55, level = 81, dv = 8, ev = 23140)
  expect_identical(attack, 137L)
  defense <- calculate_stat_v1(base_stat = 30, level = 81, dv = 13, ev = 17280)
  expect_identical(defense, 101L)
  sp_attack <- calculate_stat_v1(base_stat = 50, level = 81, dv = 9, ev = 19625)
  expect_identical(sp_attack, 128L)
  sp_defense <- calculate_stat_v1(base_stat = 40, level = 81, dv = 9, ev = 19625)
  expect_identical(sp_defense, 112L)
  speed <- calculate_stat_v1(base_stat = 90, level = 81, dv = 5, ev = 24795)
  expect_identical(speed, 190L)

  attack <- calculate_stat(base_stat = 55, level = 81, iv = 8, ev = 23140, generation = 1)
  expect_identical(attack, 137L)
  defense <- calculate_stat(base_stat = 30, level = 81, iv = 13, ev = 17280, generation = 1)
  expect_identical(defense, 101L)
  sp_attack <- calculate_stat(base_stat = 50, level = 81, iv = 9, ev = 19625, generation = 1)
  expect_identical(sp_attack, 128L)
  sp_defense <- calculate_stat(base_stat = 40, level = 81, iv = 9, ev = 19625, generation = 1)
  expect_identical(sp_defense, 112L)
  speed <- calculate_stat(base_stat = 90, level = 81, iv = 5, ev = 24795, generation = 1)
  expect_identical(speed, 190L)

  # Gen 3+
  attack <- calculate_stat_v2(
    base_stat = 130, stat_name = "attack", nature = "adamant",
    level = 78, iv = 12, ev = 190
  )
  expect_identical(attack, 278L)
  defense <- calculate_stat_v2(
    base_stat = 95, stat_name = "defense", nature = "adamant",
    level = 78, iv = 30, ev = 91
  )
  expect_identical(defense, 193L)
  sp_attack <- calculate_stat_v2(
    base_stat = 80, stat_name = "special-attack", nature = "adamant",
    level = 78, iv = 16, ev = 48
  )
  expect_identical(sp_attack, 135L)
  sp_defense <- calculate_stat_v2(
    base_stat = 85, stat_name = "special-defense", nature = "adamant",
    level = 78, iv = 23, ev = 84
  )
  expect_identical(sp_defense, 171L)
  speed <- calculate_stat_v2(
    base_stat = 102, stat_name = "speed", nature = "adamant",
    level = 78, iv = 5, ev = 23
  )
  expect_identical(speed, 171L)

  attack <- calculate_stat(
    base_stat = 130, stat_name = "attack", nature = "adamant",
    level = 78, iv = 12, ev = 190, generation = 8
  )
  expect_identical(attack, 278L)
  defense <- calculate_stat(
    base_stat = 95, stat_name = "defense", nature = "adamant",
    level = 78, iv = 30, ev = 91, generation = 8
  )
  expect_identical(defense, 193L)
  sp_attack <- calculate_stat(
    base_stat = 80, stat_name = "special-attack", nature = "adamant",
    level = 78, iv = 16, ev = 48, generation = 8
  )
  expect_identical(sp_attack, 135L)
  sp_defense <- calculate_stat(
    base_stat = 85, stat_name = "special-defense", nature = "adamant",
    level = 78, iv = 23, ev = 84, generation = 8
  )
  expect_identical(sp_defense, 171L)
  speed <- calculate_stat(
    base_stat = 102, stat_name = "speed", nature = "adamant",
    level = 78, iv = 5, ev = 23, generation = 8
  )
  expect_identical(speed, 171L)
})
