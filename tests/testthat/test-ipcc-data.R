test_that("IPCC NCV values are exposed as a SIEC-ready table", {
  ncvs <- get_ipcc_ncv()

  expect_named(ncvs, c("siec", "fuel", "ncv_kjkg"))
  expect_true(is.numeric(ncvs$ncv_kjkg))
  expect_equal(anyDuplicated(ncvs$siec), 0L)
  expect_equal(ncvs$ncv_kjkg[ncvs$siec == SIEC_NATURAL_GAS], 48000)
})

test_that("CO2 emission factors have unique SIEC keys", {
  factors <- get_ipcc_emission_factors()

  expect_named(factors, c("siec", "co2_factor_t_per_TJ"))
  expect_true(is.numeric(factors$co2_factor_t_per_TJ))
  expect_equal(anyDuplicated(factors$siec), 0L)
  expect_equal(factors$co2_factor_t_per_TJ[factors$siec == SIEC_HARD_COAL], 92.8)
})

test_that("IPCC values join without changing input rows", {
  input <- tibble::tibble(
    siec = c(SIEC_HARD_COAL, SIEC_NATURAL_GAS),
    value = c(1, 2)
  )

  with_ncvs <- add_ncv_ipcc(input)
  with_factors <- add_emission_factor(input)

  expect_equal(nrow(with_ncvs), nrow(input))
  expect_equal(nrow(with_factors), nrow(input))
  expect_false(anyNA(with_ncvs$ncv_kjkg))
  expect_false(anyNA(with_factors$co2_factor_t_per_TJ))
})
