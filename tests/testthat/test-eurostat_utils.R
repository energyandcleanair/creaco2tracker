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

test_that("eurostat_split_elec_others splits electricity and others correctly", {

  # Both sectors present
  df <- tibble(
    iso2 = "DE", time = as.Date("2023-01-01"), unit = "TWh", siec_code = "C0100", fuel = "solid",
    sector = c(SECTOR_ELEC, SECTOR_OTHERS), values = c(10, 90)
  )
  result <- eurostat_split_elec_others(df)
  expect_equal(sort(result$sector), sort(c(SECTOR_ELEC, SECTOR_OTHERS)))
  expect_equal(sum(result$values), 100)
  expect_equal(result$values[result$sector == SECTOR_ELEC], 10)
  expect_equal(result$values[result$sector == SECTOR_OTHERS], 90)

  # Main case: "all" and "electricity" -> split into "electricity" and "others"
  df_main <- tibble(
    iso2 = "DE", time = as.Date("2023-02-01"), unit = "TWh", siec_code = "C0100", fuel = "solid",
    sector = c(SECTOR_ALL, SECTOR_ELEC), values = c(100, 30)
  )
  result_main <- eurostat_split_elec_others(df_main)
  expect_equal(sort(result_main$sector), sort(c(SECTOR_ELEC, SECTOR_OTHERS)))
  expect_equal(result_main$values[result_main$sector == SECTOR_ELEC], 30)
  expect_equal(result_main$values[result_main$sector == SECTOR_OTHERS], 70)  # 100 - 30
  expect_equal(sum(result_main$values), 100)

  # Only electricity present
  df2 <- tibble(
    iso2 = "DE", time = as.Date("2023-03-01"), unit = "TWh", siec_code = "C0100", fuel = "solid",
    sector = c(SECTOR_ELEC), values = c(20)
  )
  result2 <- eurostat_split_elec_others(df2)
  expect_true(SECTOR_OTHERS %in% result2$sector)
  expect_equal(result2$values[result2$sector == SECTOR_ELEC], 20)

  # Only others present
  df3 <- tibble(
    iso2 = "DE", time = as.Date("2023-04-01"), unit = "TWh", siec_code = "C0100", fuel = "solid",
    sector = c(SECTOR_OTHERS), values = c(30)
  )
  result3 <- eurostat_split_elec_others(df3)
  expect_true(SECTOR_OTHERS %in% result3$sector)
  expect_equal(result3$values[result3$sector == SECTOR_OTHERS], 30)

  # Sum preservation with both sectors
  df4 <- tibble(
    iso2 = "DE", time = as.Date("2023-05-01"), unit = "TWh", siec_code = "C0100", fuel = "solid",
    sector = c(SECTOR_ELEC, SECTOR_OTHERS), values = c(15, 85)
  )
  result4 <- eurostat_split_elec_others(df4)
  expect_equal(sum(result4$values), 100)
})

test_that("fix_eu_when_important_countries_missing works correctly", {
  # Create test data with various scenarios
  test_data <- tribble(
    ~iso2, ~nrg_bal_code, ~time, ~values,
    # Scenario 1: All important countries present and non-NA - EU should be kept
    "EU", "GID_OBS", "2020-01-01", 100,
    "DE", "GID_OBS", "2020-01-01", 40,
    "FR", "GID_OBS", "2020-01-01", 30,
    "IT", "GID_OBS", "2020-01-01", 20,
    "ES", "GID_OBS", "2020-01-01", 10,
    
    # Scenario 2: One important country missing (DE) - EU should be removed
    "EU", "GID_OBS", "2020-02-01", 100,
    "FR", "GID_OBS", "2020-02-01", 30,
    "IT", "GID_OBS", "2020-02-01", 20,
    "ES", "GID_OBS", "2020-02-01", 10,
    
    # Scenario 3: One important country has NA value (FR) - EU should be removed
    "EU", "GID_OBS", "2020-03-01", 100,
    "DE", "GID_OBS", "2020-03-01", 40,
    "FR", "GID_OBS", "2020-03-01", NA,
    "IT", "GID_OBS", "2020-03-01", 20,
    "ES", "GID_OBS", "2020-03-01", 10,
    
    # Scenario 4: Different nrg_bal_code - all present, EU should be kept
    "EU", "GID_NE", "2020-01-01", 50,
    "DE", "GID_NE", "2020-01-01", 20,
    "FR", "GID_NE", "2020-01-01", 15,
    "IT", "GID_NE", "2020-01-01", 10,
    "ES", "GID_NE", "2020-01-01", 5,
    
    # Scenario 5: Different nrg_bal_code - one missing, EU should be removed
    "EU", "GID_NE", "2020-02-01", 50,
    "DE", "GID_NE", "2020-02-01", 20,
    "FR", "GID_NE", "2020-02-01", 15,
    "IT", "GID_NE", "2020-02-01", 10,
    
    # Scenario 6: Non-important country missing - EU should be kept
    "EU", "GID_OBS", "2020-04-01", 100,
    "DE", "GID_OBS", "2020-04-01", 40,
    "FR", "GID_OBS", "2020-04-01", 30,
    "IT", "GID_OBS", "2020-04-01", 20,
    "ES", "GID_OBS", "2020-04-01", 10,
    "NL", "GID_OBS", "2020-04-01", NA,  # Non-important country missing
    
    # Scenario 7: EU has NA value - should be kept as is
    "EU", "GID_OBS", "2020-05-01", NA,
    "DE", "GID_OBS", "2020-05-01", 40,
    "FR", "GID_OBS", "2020-05-01", 30,
    "IT", "GID_OBS", "2020-05-01", 20,
    "ES", "GID_OBS", "2020-05-01", 10,
    
    # Scenario 8: Non-EU country data - should be unchanged
    "NL", "GID_OBS", "2020-01-01", 5,
    "BE", "GID_OBS", "2020-01-01", 3
  ) %>%
    mutate(time = as.Date(time))

  # Test the function
  result <- fix_eu_when_important_countries_missing(test_data)

  # Scenario 1: All important countries present - EU should be kept
  expect_false(is.na(result$values[result$iso2 == "EU" & 
                                  result$nrg_bal_code == "GID_OBS" & 
                                  result$time == as.Date("2020-01-01")]))
  expect_equal(result$values[result$iso2 == "EU" & 
                           result$nrg_bal_code == "GID_OBS" & 
                           result$time == as.Date("2020-01-01")], 100)

  # Scenario 2: DE missing - EU should be removed
  expect_true(nrow(result[result$iso2 == "EU" & 
                         result$nrg_bal_code == "GID_OBS" & 
                         result$time == as.Date("2020-02-01"), ]) == 0)

  # Scenario 3: FR has NA - EU should be removed
  expect_true(nrow(result[result$iso2 == "EU" & 
                         result$nrg_bal_code == "GID_OBS" & 
                         result$time == as.Date("2020-03-01"), ]) == 0)

  # Scenario 4: Different nrg_bal_code, all present - EU should be kept
  expect_false(is.na(result$values[result$iso2 == "EU" & 
                                  result$nrg_bal_code == "GID_NE" & 
                                  result$time == as.Date("2020-01-01")]))
  expect_equal(result$values[result$iso2 == "EU" & 
                           result$nrg_bal_code == "GID_NE" & 
                           result$time == as.Date("2020-01-01")], 50)

  # Scenario 5: Different nrg_bal_code, one missing - EU should be removed
  expect_true(nrow(result[result$iso2 == "EU" & 
                         result$nrg_bal_code == "GID_NE" & 
                         result$time == as.Date("2020-02-01"), ]) == 0)

  # Scenario 6: Non-important country missing - EU should be kept
  expect_false(is.na(result$values[result$iso2 == "EU" & 
                                  result$nrg_bal_code == "GID_OBS" & 
                                  result$time == as.Date("2020-04-01")]))
  expect_equal(result$values[result$iso2 == "EU" & 
                           result$nrg_bal_code == "GID_OBS" & 
                           result$time == as.Date("2020-04-01")], 100)

  # Scenario 7: EU has NA - should be kept as is
  expect_true(is.na(result$values[result$iso2 == "EU" & 
                                 result$nrg_bal_code == "GID_OBS" & 
                                 result$time == as.Date("2020-05-01")]))

  # Scenario 8: Non-EU countries should be unchanged
  expect_equal(result$values[result$iso2 == "NL" & 
                           result$nrg_bal_code == "GID_OBS" & 
                           result$time == as.Date("2020-01-01")], 5)
  expect_equal(result$values[result$iso2 == "BE" & 
                           result$nrg_bal_code == "GID_OBS" & 
                           result$time == as.Date("2020-01-01")], 3)

  # Test with custom important countries
  test_data_custom <- tribble(
    ~iso2, ~nrg_bal_code, ~time, ~values,
    "EU", "GID_OBS", "2020-01-01", 100,
    "DE", "GID_OBS", "2020-01-01", 40,
    "FR", "GID_OBS", "2020-01-01", 30,
    "IT", "GID_OBS", "2020-01-01", 20,
    "ES", "GID_OBS", "2020-01-01", 10,
    
    "EU", "GID_OBS", "2020-02-01", 100,
    "DE", "GID_OBS", "2020-02-01", 40,
    "FR", "GID_OBS", "2020-02-01", 30,
    # IT and ES missing
  ) %>%
    mutate(time = as.Date(time))

  result_custom <- fix_eu_when_important_countries_missing(test_data_custom, important_iso2s = c("DE", "FR", "IT", "ES"))

  # With custom important countries, first scenario should be kept, second should be removed
  expect_false(is.na(result_custom$values[result_custom$iso2 == "EU" & 
                                         result_custom$nrg_bal_code == "GID_OBS" & 
                                         result_custom$time == as.Date("2020-01-01")]))
  expect_true(nrow(result_custom[result_custom$iso2 == "EU" & 
                                result_custom$nrg_bal_code == "GID_OBS" & 
                                result_custom$time == as.Date("2020-02-01"), ]) == 0)
})
