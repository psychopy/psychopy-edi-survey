## Analysis of Data from the PsychoPy GitHub Repository

This analysis was conducted to complement a survey exploring the PsychoPy community. 

### Running the Analysis

1. Use the `fetch_github_data_2.R` script to fetch pull requests (PRs) from the PsychoPy repository.  
2. Add your GitHub access token to the script.  
3. Set the `overall_start` and `overall_end` variables to define the time window for data extraction.  

These time windows are used throughout the analysis.

# PR Window 1: 28 May 2022 – 27 Nov 2022
#start_date <- ymd_hms("2022-05-28 00:00:00", tz = "UTC")
#end_date   <- ymd_hms("2022-11-27 23:59:59", tz = "UTC")

# PR Window 2: 28 May 2024 – 27 Nov 2024
#start_date <- ymd_hms("2024-05-28 00:00:00", tz = "UTC")
#end_date   <- ymd_hms("2024-11-27 23:59:59", tz = "UTC")

### Output and Further Analysis

- The script will output the extracted data to the `data` directory.  
  - A separate CSV file is created for each month.  
  - A `_combined` CSV file is generated for the full time window.  
  - This approach helps prevent hitting the GitHub API rate limit.

- After fetching the data, run `summarise_github_data2.qmd` to generate plots and results:  
  - Plots are saved in the `github_plots` directory.  
  - The script also produces the summary files and an HTML report of the results.