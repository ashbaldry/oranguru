# `{oranguru}`

The aim of the `{oranguru}` package is to use the data from PokéAPI ([API](https://pokeapi.co/), [R Package](https://github.com/ashbaldry/pokeapi)) and be able to create a team of Pokémon and battle against random teams, a friend or run damage calculations.

## Installation

```r
remotes::install_github("ashbaldry/oranguru")
```

## Pokémon Battle

To start a Pokémon match in the R console, run `PokemonBattle$new()$start()`. This will auto-generate two teams of 6 Pokémon, and begin a menu option to either attack or switch out your Pokémon with one of your others.
