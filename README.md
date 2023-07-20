# `{oranguru}`

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ashbaldry/oranguru/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ashbaldry/oranguru/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The aim of the `{oranguru}` package is to use the data from PokéAPI ([API](https://pokeapi.co/), [R Package](https://github.com/ashbaldry/pokeapi)) and be able to create a team of Pokémon and battle against random teams, a friend or run damage calculations.

## Installation

```r
remotes::install_github("ashbaldry/oranguru")
```

## Pokémon Battle

To start a Pokémon match in the R console, run `PokemonBattle$new()`. This will auto-generate two teams of 6 Pokémon, and begin a menu option to either attack or switch out your Pokémon with one of your others.
