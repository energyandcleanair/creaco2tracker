test_that("fill_eu_from_countries_sum comprehensive test", {
  # Comprehensive test for fill_eu_from_countries_sum covering all scenarios
  # Uses one dataset with FR, DE, IT, SMALL countries
  
  # Create comprehensive test data
  test_data <- tribble(
    ~iso2, ~sector, ~siec_code, ~nrg_bal_code, ~unit, ~time, ~values,
    # Historical data (2023) - for correlation
    "FR", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-01-01", 100,
    "DE", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-01-01", 200,
    "IT", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-01-01", 150,
    "SMALL", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-01-01", 10,
    "EU", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-01-01", 450,  # 100 + 200 + 150
    
    "FR", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-02-01", 50,
    "DE", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-02-01", 180,
    "IT", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-02-01", 120,
    "SMALL", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-02-01", 8,
    "EU", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-02-01", 350,  # 50 + 180 + 120
    
    "FR", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-03-01", 0,
    "DE", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-03-01", 160,
    "IT", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-03-01", 100,
    "SMALL", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-03-01", 5,
    "EU", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2023-03-01", 260,  # 0 + 160 + 100
    
    # Recent data (2024) - to be filled
    # 2024-01: All three major countries available
    "FR", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-01-01", 100,
    "DE", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-01-01", 200,
    "IT", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-01-01", 150,
    "SMALL", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-01-01", 10,
    "EU", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-01-01", NA,   # Should be filled with 450
    
    # 2024-02: Only FR and DE available (IT missing)
    "FR", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-02-01", 50,
    "DE", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-02-01", 180,
    "SMALL", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-02-01", 8,
    "EU", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-02-01", NA,   # Should NOT be filled (only 2 countries)
    
    # 2024-03: All three major countries available again
    "FR", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-03-01", 0,
    "DE", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-03-01", 160,
    "IT", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-03-01", 100,
    "SMALL", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-03-01", 5,
    "EU", "electricity", "C0100", "TI_EHG_MAP", "Thousand tonnes", "2024-03-01", NA,   # Should be filled with 260
  ) %>%
    mutate(time = as.Date(time))

  # Count original data
  original_country_data <- test_data %>% filter(iso2 != "EU")
  original_country_rows <- nrow(original_country_data)
  original_eu_data <- test_data %>% filter(iso2 == "EU")
  original_eu_rows <- nrow(original_eu_data)

  # Test 1: Proper filling with all group_cols including iso2 and time
  result1 <- fill_eu_from_countries_sum(
    data = test_data,
    group_cols = c("iso2", "sector", "siec_code", "nrg_bal_code", "unit", "time"),
    min_countries = 3,  # Require 3 countries for filling
    min_points = 2,
    max_rel_diff = 1
  )

  # Check that country data is preserved
  result1_country_data <- result1 %>% filter(iso2 != "EU")
  expect_equal(nrow(result1_country_data), original_country_rows,
               info = "Test 1: All country data should be preserved")

  # Check EU filling results
  result1_eu <- result1 %>% filter(iso2 == "EU") %>% arrange(time)
  
  # Historical values should remain unchanged
  expect_equal(result1_eu$values[result1_eu$time == as.Date("2023-01-01")], 450)
  expect_equal(result1_eu$values[result1_eu$time == as.Date("2023-02-01")], 350)
  expect_equal(result1_eu$values[result1_eu$time == as.Date("2023-03-01")], 260)
  
  # 2024-01: Should be filled (all 3 countries available)
  expect_equal(result1_eu$values[result1_eu$time == as.Date("2024-01-01")], 450)
  
  # 2024-02: Should NOT be filled (only 2 countries available)
  expect_true(is.na(result1_eu$values[result1_eu$time == as.Date("2024-02-01")]))
  
  # 2024-03: Should be filled (all 3 countries available)
  expect_equal(result1_eu$values[result1_eu$time == as.Date("2024-03-01")], 260)

  # Test 2: Resilience to missing iso2 in group_cols (should still work but preserve data)
  result2 <- fill_eu_from_countries_sum(
    data = test_data,
    group_cols = c("sector", "siec_code", "nrg_bal_code", "unit", "time"),  # iso2 missing!
    min_countries = 3,
    min_points = 2,
    max_rel_diff = 1
  )

  # Check that country data is preserved (this is the critical test for the anti_join bug)
  result2_country_data <- result2 %>% filter(iso2 != "EU")
  expect_equal(nrow(result2_country_data), original_country_rows,
               info = "Test 2: Country data should be preserved even when iso2 is missing from group_cols")

  # Test 3: Resilience to missing time in group_cols
  result3 <- fill_eu_from_countries_sum(
    data = test_data,
    group_cols = c("sector", "siec_code", "nrg_bal_code", "unit"),  # time missing!
    min_countries = 3,
    min_points = 2,
    max_rel_diff = 1
  )

  # Check that country data is preserved
  result3_country_data <- result3 %>% filter(iso2 != "EU")
  expect_equal(nrow(result3_country_data), original_country_rows,
               info = "Test 3: Country data should be preserved even when time is missing from group_cols")

  # Test 4: Extreme parameters should not remove country data
  result4 <- fill_eu_from_countries_sum(
    data = test_data,
    group_cols = c("sector", "siec_code", "nrg_bal_code", "unit", "time"),  # iso2 missing!
    min_countries = 100,  # Extremely high threshold
    max_rel_diff = 0.01   # Very strict correlation requirement
  )

  # Check that country data is preserved even with extreme parameters
  result4_country_data <- result4 %>% filter(iso2 != "EU")
  expect_equal(nrow(result4_country_data), original_country_rows,
               info = "Test 4: Country data should be preserved even with extreme parameters")

  # Test 5: Verify each individual country record is preserved
  for (i in 1:nrow(original_country_data)) {
    original_row <- original_country_data[i, ]
    matching_rows <- result2_country_data %>%
      filter(iso2 == original_row$iso2,
             sector == original_row$sector,
             siec_code == original_row$siec_code,
             nrg_bal_code == original_row$nrg_bal_code,
             unit == original_row$unit,
             time == original_row$time,
             values == original_row$values)
    
    expect_equal(nrow(matching_rows), 1,
                 info = paste("Test 5: Country record should be preserved:", 
                             original_row$iso2, original_row$time))
  }
})

test_that("eurostat_split_elec_others preserves data when only 'all' sector is available", {
  # Create test data with only "all" sector (like coke data)
  test_data <- tibble(
    iso2 = "DE",
    time = as.Date("2024-01-01"),
    unit = "Thousand tonnes",
    siec_code = "C0311",  # Coke oven coke
    fuel = "coke",
    sector = "all",
    values = 500
  )
  
  # Run the function
  result <- eurostat_split_elec_others(test_data)
  
  # Test that data is preserved
  expect_equal(nrow(result), 2, info = "Should return 2 rows (electricity and others)")
  expect_equal(sum(result$values, na.rm = TRUE), 500, info = "Total value should be preserved")
  expect_true(all(result$sector %in% c("electricity", "others")), info = "Should have electricity and others sectors")
  expect_equal(result$values[result$sector == "others"], 500, info = "All value should go to others when no electricity data")
  expect_equal(result$values[result$sector == "electricity"], 0, info = "Electricity should be 0 when no electricity data")
})
