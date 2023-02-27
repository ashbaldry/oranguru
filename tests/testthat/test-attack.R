test_that("use_attack is only available for generation 1", {
  # Bulbasaur moves: "double-edge" "razor-leaf"  "solar-beam"  "tackle"
  set.seed(2312L)
  move <- "tackle"

  team <- PokemonTeam$new(
    pokemon_1 = Pokemon$new(api_data = readRDS("data/bulbasaur.rds")),
    pokemon_2 = Pokemon$new(api_data = readRDS("data/charmander.rds")),
    pokemon_3 = Pokemon$new(api_data = readRDS("data/gastly.rds")),
    pokemon_4 = Pokemon$new(api_data = readRDS("data/pikachu.rds")),
    pokemon_5 = Pokemon$new(api_data = readRDS("data/sandshrew.rds")),
    pokemon_6 = Pokemon$new(api_data = readRDS("data/squirtle.rds")),
    level = 50L,
    generation = 1L
  )

  battle <- PokemonBattle$new(team_1 = team, team_2 = team, level = 50L, generation = 1L)
  expect_type(use_attack(move, team$get_pokemon(1L), team$get_pokemon(1L), battle, generation = 1L), "double")

  generation <- 2L
  team <- PokemonTeam$new(
    pokemon_1 = Pokemon$new(api_data = readRDS("data/bulbasaur.rds")),
    pokemon_2 = Pokemon$new(api_data = readRDS("data/charmander.rds")),
    pokemon_3 = Pokemon$new(api_data = readRDS("data/gastly.rds")),
    pokemon_4 = Pokemon$new(api_data = readRDS("data/pikachu.rds")),
    pokemon_5 = Pokemon$new(api_data = readRDS("data/sandshrew.rds")),
    pokemon_6 = Pokemon$new(api_data = readRDS("data/squirtle.rds")),
    level = 50L,
    generation = generation
  )

  battle <- PokemonBattle$new(team_1 = team, team_2 = team, level = 50L, generation = generation)
  expect_error(use_attack(move, team$get_pokemon(1L), team$get_pokemon(1L), battle, generation = 2L))
})
