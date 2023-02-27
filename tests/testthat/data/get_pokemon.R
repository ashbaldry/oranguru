bulbasaur <- pokeapi::get_pokemon(1L)
saveRDS(bulbasaur, "data/bulbasaur.rds")

charmander <- pokeapi::get_pokemon(4L)
saveRDS(charmander, "data/charmander.rds")

squirtle <- pokeapi::get_pokemon(7L)
saveRDS(squirtle, "data/squirtle.rds")

pikachu <- pokeapi::get_pokemon(25L)
saveRDS(pikachu, "data/pikachu.rds")

sandshrew <- pokeapi::get_pokemon(27L)
saveRDS(sandshrew, "data/sandshrew.rds")

gastly <- pokeapi::get_pokemon(92L)
saveRDS(gastly, "data/gastly.rds")
