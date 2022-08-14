calculate_hp <- function(base_hp, level = 50, iv = 0, ev = 0, generation = 8) {
  if (generation < 3) {
    calculate_hp_v1(base_hp, level, dv = iv, ev = ev)
  } else {
    calculate_hp_v2(base_hp, level, iv = iv, ev = ev)
  }
}

calculate_hp_v1 <- function(base_hp, level = 50, dv = 0, ev = 0) {
  float_hp <- (((base_hp + dv) * 2 + sqrt(ev) / 4) * level) / 100 + level + 10
  as.integer(float_hp)
}

calculate_hp_v2 <- function(base_hp, level = 50, iv = 0, ev = 0) {
  float_hp <- ((2 * base_hp + iv + ev / 4) * level) / 100 + level + 10
  as.integer(float_hp)
}
