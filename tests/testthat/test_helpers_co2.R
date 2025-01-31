test_that("split_gas_to_elec_others correctly splits gas between electricity and others", {

  # Test data with multiple dates and varying sector combinations
    test_data <- tibble(
      iso2 = rep("DE", 6),
      geo = "DE",
      unit = "t",
      date = as.Date(c(
        rep("2023-01-01", 2),
        rep("2023-01-02", 2),
        rep("2023-01-03", 2)
      )),
      fuel = rep("gas", 6),
      sector = c(
        "all", "electricity",  # day 1
        "electricity", "all",   # day 2
        "electricity", "others" # day 3 - already split
      ),
      value = c(100, 40, 30, 90, 35, 55),
      estimate = "central"
    )

  result <- split_gas_to_elec_others(test_data)


  # Check day 1: sum of electricity and others equals original all
  day1_sum <- result %>%
    filter(date == as.Date("2023-01-01")) %>%
    pull(value) %>%
    sum()
  expect_equal(day1_sum, 100)  # original "all" value

  # Check day 2: sum of electricity and others equals original all
  day2_sum <- result %>%
    filter(date == as.Date("2023-01-02")) %>%
    pull(value) %>%
    sum()
  expect_equal(day2_sum, 90)  # original "all" value

  # Check day 3: values remain unchanged when already split
  day3_result <- result %>%
    filter(date == as.Date("2023-01-03")) %>%
    arrange(sector)
  expect_equal(day3_result$value, c(35, 55))  # electricity and others values should remain the same
  expect_equal(day3_result$sector, c("electricity", "others"))
})

test_that("split_gas_to_elec_all correctly combines electricity and others into all", {
  # Test data with multiple dates and varying sector combinations
  test_data <- tibble(
    iso2 = rep("DE", 6),
    geo = "DE",
    date = as.Date(c(
      rep("2023-01-01", 2),
      rep("2023-01-02", 2),
      rep("2023-01-03", 2)
    )),
    fuel = rep("gas", 6),
    sector = c(
      "others", "electricity",  # day 1
      "others", "electricity",  # day 2
      "all", "electricity"             # day 3 - already all
    ),
    value = c(60, 40, 25, 35, 100, 20),
    estimate = "central",
    unit = "t"
  )

  result <- split_gas_to_elec_all(test_data)

  # Check day 1: should have gas combined
  day1_result <- result %>%
    filter(date == as.Date("2023-01-01")) %>%
    arrange(sector)
  expect_equal(nrow(day1_result), 2)
  expect_equal(day1_result$value, c(100, 40))
  expect_equal(day1_result$sector, c("all", "electricity"))


  # Check day 2: should have gas combined
  day2_result <- result %>%
    filter(date == as.Date("2023-01-02")) %>%
    arrange(sector)
  expect_equal(nrow(day2_result), 2)
  expect_equal(day2_result$value, c(60, 35))  # values should be combined
  expect_equal(day2_result$sector, c("all", "electricity"))

  # Check day 3: values should remain unchanged
  day3_result <- result %>%
    filter(date == as.Date("2023-01-03")) %>%
    arrange(sector)
  expect_equal(nrow(day3_result), 2)
  expect_equal(day3_result$value, c(100, 20))  # values should remain the same
  expect_equal(day3_result$sector, c("all", "electricity"))
})
