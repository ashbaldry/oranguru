test_that("include_stab_multiplier multiplies correctly", {
  expect_equal(include_stab_multiplier(1L, 1L), 1.5)
  expect_equal(include_stab_multiplier(1L, c(1L, 2L)), 1.5)

  expect_equal(include_stab_multiplier(1L, 2L), 1)
  expect_equal(include_stab_multiplier(1L, c(2L, 3L)), 1)
})
