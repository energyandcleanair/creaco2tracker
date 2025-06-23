test_that("fill_eu_from_countries_sum works correctly", {
  # Create test data using tribble for better readability
  test_data <- tribble(
    ~iso2, ~sector, ~time, ~values,
    # Sector A - good correlation
    "EU", "A", "2020-01-01", 100,
    "EU", "A", "2020-02-01", 110,
    "EU", "A", "2020-03-01", 120,
    "EU", "A", "2020-04-01", NA,  # Missing EU value to be filled
    "EU", "A", "2020-05-01", NA,  # Missing EU value to be filled
    "DE", "A", "2020-01-01", 40,
    "DE", "A", "2020-02-01", 44,
    "DE", "A", "2020-03-01", 48,
    "DE", "A", "2020-04-01", 52,  # Available
    "DE", "A", "2020-05-01", NA,  # Missing
    "FR", "A", "2020-01-01", 30,
    "FR", "A", "2020-02-01", 33,
    "FR", "A", "2020-03-01", 36,
    "FR", "A", "2020-04-01", 39,  # Available
    "FR", "A", "2020-05-01", 42,  # Available
    "IT", "A", "2020-01-01", 20,
    "IT", "A", "2020-02-01", 22,
    "IT", "A", "2020-03-01", 24,
    "IT", "A", "2020-04-01", 26,  # Available
    "IT", "A", "2020-05-01", 28,  # Available
    "ES", "A", "2020-01-01", 10,
    "ES", "A", "2020-02-01", 11,
    "ES", "A", "2020-03-01", 12,
    "ES", "A", "2020-04-01", 13,  # Available
    "ES", "A", "2020-05-01", 14,  # Available
    # Sector B - poor correlation
    "EU", "B", "2020-01-01", 200,
    "EU", "B", "2020-02-01", NA,
    "EU", "B", "2020-03-01", 220,
    "EU", "B", "2020-04-01", NA,  # Missing EU value to be filled
    "EU", "B", "2020-05-01", NA,  # Missing EU value to be filled
    "DE", "B", "2020-01-01", 80,
    "DE", "B", "2020-02-01", 100,
    "DE", "B", "2020-03-01", 120,
    "DE", "B", "2020-04-01", 140,  # Available
    "DE", "B", "2020-05-01", 160,  # Available
    "FR", "B", "2020-01-01", 70,
    "FR", "B", "2020-02-01", 90,
    "FR", "B", "2020-03-01", 110,
    "FR", "B", "2020-04-01", 130,  # Available
    "FR", "B", "2020-05-01", 150,  # Available
    "IT", "B", "2020-01-01", 50,
    "IT", "B", "2020-02-01", 70,
    "IT", "B", "2020-03-01", 90,
    "IT", "B", "2020-04-01", 110,  # Available
    "IT", "B", "2020-05-01", 130,  # Available
    "ES", "B", "2020-01-01", 30,
    "ES", "B", "2020-02-01", 50,
    "ES", "B", "2020-03-01", 70,
    "ES", "B", "2020-04-01", 90,  # Available
    "ES", "B", "2020-05-01", 110  # Available
  ) %>%
    mutate(time = as.Date(time))

  # Test the function
  result <- fill_eu_from_countries_sum(
    test_data,
    group_cols = c("time", "sector"),
    min_countries = 3,
    max_rel_diff = 0.05,
    min_points = 2
  ) %>% arrange(sector, desc(time))

  # Check that sector A April 2020 was filled (all countries available)
  expect_false(is.na(result$values[result$iso2 == "EU" &
                                  result$sector == "A" &
                                  result$time == as.Date("2020-04-01")]))
  expect_equal(result$values[result$iso2 == "EU" &
                           result$sector == "A" &
                           result$time == as.Date("2020-04-01")], 130)

  # Check that sector A May 2020 was not filled (DE missing)
  expect_true(is.na(result$values[result$iso2 == "EU" &
                                 result$sector == "A" &
                                 result$time == as.Date("2020-05-01")]))

  # Check that all sector B values match original data (none should be filled)
  expect_equal(
    result %>% filter(iso2 == "EU", sector == "B") %>% arrange(time) %>% pull(values),
    test_data %>% filter(iso2 == "EU", sector == "B") %>% arrange(time) %>% pull(values)
  )

  # Check that all non-EU values match original data using join
  non_eu_data <- test_data %>% filter(iso2 != "EU")
  result_non_eu <- result %>% filter(iso2 != "EU") %>% rename(values_new=values)
  joined <- non_eu_data %>%
    inner_join(result_non_eu, by = c("iso2", "sector", "time")) %>%
    select(iso2, sector, time, values, values_new)
  expect_equal(joined$values, joined$values_new)
})
