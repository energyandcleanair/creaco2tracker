# Tests for split_gas_to_elec_others and split_gas_to_elec_all

context("split_gas_to_elec_others and split_gas_to_elec_all")

# Helper for easy comparison
expect_gas_sectors <- function(df, date, expected_sectors, expected_values) {
  res <- df %>% filter(date == as.Date(date)) %>% arrange(sector)
  expect_equal(res$sector, sort(expected_sectors))
  expect_equal(res$value, expected_values)
}

# Helper to compare data frames ignoring column order
expect_equal_ignore_order <- function(actual, expected) {
  expect_equal(actual %>% arrange(across(everything())), 
               expected %>% arrange(across(everything())))
}

# 1. Only "all" and "electricity" present (should infer "others")
test_that("split_gas_to_elec_others infers 'others' from 'all' and 'electricity'", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-01-01"),
    fuel = "gas", sector = c("all", "electricity"), value = c(100, 40), estimate = "central"
  )
  result <- split_gas_to_elec_others(test_data)
  expect_gas_sectors(result, "2023-01-01", c("electricity", "others"), c(40, 60))
})

# 2. Only "all" and "others" present (should infer "electricity")
test_that("split_gas_to_elec_others infers 'electricity' from 'all' and 'others'", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-01-02"),
    fuel = "gas", sector = c("all", "others"), value = c(90, 30), estimate = "central"
  )
  result <- split_gas_to_elec_others(test_data)
  expect_gas_sectors(result, "2023-01-02", c("electricity", "others"), c(60, 30))
})

# 3. Only "electricity" and "others" present (should return unchanged)
test_that("split_gas_to_elec_others returns unchanged if already split", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-01-03"),
    fuel = "gas", sector = c("electricity", "others"), value = c(35, 55), estimate = "central"
  )
  result <- split_gas_to_elec_others(test_data)
  expect_gas_sectors(result, "2023-01-03", c("electricity", "others"), c(35, 55))
})

# 4. All three present (should return unchanged)
test_that("split_gas_to_elec_others returns unchanged if all sectors present", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-01-04"),
    fuel = "gas", sector = c("all", "electricity", "others"), value = c(100, 40, 60), estimate = "central"
  )
  result <- split_gas_to_elec_others(test_data)
  expect_gas_sectors(result, "2023-01-04", c("electricity", "others"), c(40, 60))
})

# 5. Only "all" present (should infer both as NA)
test_that("split_gas_to_elec_others infers both as NA if only 'all' present", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-01-05"),
    fuel = "gas", sector = c("all"), value = c(100), estimate = "central"
  )
  result <- split_gas_to_elec_others(test_data)
  res <- result %>% filter(date == as.Date("2023-01-05"))
  expect_true(all(res$sector %in% c("electricity", "others")))
  expect_true(all(is.na(res$value)))
})

# 6. Only "electricity" present (should infer "others" as NA)
test_that("split_gas_to_elec_others infers 'others' as NA if only 'electricity' present", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-01-06"),
    fuel = "gas", sector = c("electricity"), value = c(40), estimate = "central"
  )
  result <- split_gas_to_elec_others(test_data)
  res <- result %>% filter(date == as.Date("2023-01-06"))
  expect_true(all(res$sector %in% c("electricity", "others")))
  expect_equal(res$value[res$sector == "electricity"], 40)
  expect_true(is.na(res$value[res$sector == "others"]))
})

# 7. Only "others" present (should infer "electricity" as NA)
test_that("split_gas_to_elec_others infers 'electricity' as NA if only 'others' present", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-01-07"),
    fuel = "gas", sector = c("others"), value = c(60), estimate = "central"
  )
  result <- split_gas_to_elec_others(test_data)
  res <- result %>% filter(date == as.Date("2023-01-07"))
  expect_true(all(res$sector %in% c("electricity", "others")))
  expect_equal(res$value[res$sector == "others"], 60)
  expect_true(is.na(res$value[res$sector == "electricity"]))
})

# 8. Non-gas fuel should be unchanged
test_that("split_gas_to_elec_others leaves non-gas data unchanged", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-01-08"),
    fuel = "oil", sector = c("all", "electricity"), value = c(100, 40), estimate = "central"
  )
  result <- split_gas_to_elec_others(test_data)
  # Compare ignoring column order
  expect_equal_ignore_order(result, test_data)
})

# 9. Duplicates should cause an error (data quality check)
test_that("split_gas_to_elec_others fails on duplicate data", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-01-09"),
    fuel = "gas", sector = c("all", "electricity"), value = c(100, 40), estimate = "central"
  )
  # Add a duplicate row
  test_data <- bind_rows(test_data, test_data[1, ])
  # Should fail due to duplicate data causing list columns
  expect_error(split_gas_to_elec_others(test_data), "non-numeric argument to binary operator")
})

# 10. Missing values (should handle NA correctly)
test_that("split_gas_to_elec_others handles NA values", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-01-10"),
    fuel = "gas", sector = c("all", "electricity"), value = c(NA, 40), estimate = "central"
  )
  result <- split_gas_to_elec_others(test_data)
  res <- result %>% filter(date == as.Date("2023-01-10"))
  expect_true(all(res$sector %in% c("electricity", "others")))
  expect_equal(res$value[res$sector == "electricity"], 40)
  # others should be NA
  expect_true(is.na(res$value[res$sector == "others"]))
})

# Tests for split_gas_to_elec_all

test_that("split_gas_to_elec_all infers 'all' from 'electricity' and 'others'", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-02-01"),
    fuel = "gas", sector = c("electricity", "others"), value = c(40, 60), estimate = "central"
  )
  result <- split_gas_to_elec_all(test_data)
  res <- result %>% filter(date == as.Date("2023-02-01")) %>% arrange(sector)
  expect_equal(res$sector, c("all", "electricity"))
  expect_equal(res$value, c(100, 40))
})

test_that("split_gas_to_elec_all returns unchanged if already combined", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-02-02"),
    fuel = "gas", sector = c("all", "electricity"), value = c(100, 40), estimate = "central"
  )
  result <- split_gas_to_elec_all(test_data)
  res <- result %>% filter(date == as.Date("2023-02-02")) %>% arrange(sector)
  expect_equal(res$sector, c("all", "electricity"))
  expect_equal(res$value, c(100, 40))
})

test_that("split_gas_to_elec_all handles only 'all' present", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-02-03"),
    fuel = "gas", sector = c("all"), value = c(100), estimate = "central"
  )
  result <- split_gas_to_elec_all(test_data)
  res <- result %>% filter(date == as.Date("2023-02-03"))
  expect_true(all(res$sector %in% c("all", "electricity")))
  expect_equal(res$value[res$sector == "all"], 100)
  expect_true(is.na(res$value[res$sector == "electricity"]))
})

test_that("split_gas_to_elec_all leaves non-gas data unchanged", {
  test_data <- tibble(
    iso2 = "DE", geo = "DE", unit = "t", date = as.Date("2023-02-04"),
    fuel = "oil", sector = c("all", "electricity"), value = c(100, 40), estimate = "central"
  )
  result <- split_gas_to_elec_all(test_data)
  # Compare ignoring column order
  expect_equal_ignore_order(result, test_data)
})
