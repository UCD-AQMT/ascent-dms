# all invalid scans emit a warning

    Code
      smps_l2_from_files(l1b_path, qc_path)
    Condition
      Warning in `smps_l2_from_files()`:
      No valid data in time period!
      Warning:
      There was 1 warning in `summarise()`.
      i In argument: `qc_outcome = max(qc_outcome)`.
      Caused by warning in `max()`:
      ! no non-missing arguments to max; returning -Inf
    Output
      # A tibble: 1 x 18
        site_number site_code sample_datetime_UTC        stp_factor qc_outcome flag 
              <dbl> <chr>     <dttm>                          <dbl>      <dbl> <chr>
      1           1 TestSite  2023-01-01 01:00:00.000000        1.1          4 659  
      # i 12 more variables: comment <chr>, sample_count <dbl>,
      #   total_concentration_1_cm3 <dbl>, volume_concentration_um3_cm3 <dbl>,
      #   mean_nm <dbl>, geo_mean_nm <dbl>, median_nm <dbl>, mode_nm <dbl>,
      #   geo_std_dev <dbl>, number_concentration_stp_1_cm3 <dbl>,
      #   volume_concentration_stp_um3_cm3 <dbl>, concentration_json <chr>

