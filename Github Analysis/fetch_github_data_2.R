# comment/uncomment the below to sample an appropriate window
# PR Window 1: 28 May 2022 – 27 Nov 2022
#start_date <- ymd_hms("2022-05-28 00:00:00", tz = "UTC")
#end_date   <- ymd_hms("2022-11-27 23:59:59", tz = "UTC")

# PR Window 2: 28 May 2024 – 27 Nov 2024
#start_date <- ymd_hms("2024-05-28 00:00:00", tz = "UTC")
#end_date   <- ymd_hms("2024-11-27 23:59:59", tz = "UTC")

#small window for testing
#start_date <- ymd_hms("2024-09-28 00:00:00", tz = "UTC")
#end_date   <- ymd_hms("2024-11-27 23:59:59", tz = "UTC")
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(rstudioapi)

# --- Set working directory if in RStudio ---
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# --- CONFIG ---
owner <- "psychopy"
repo <- "psychopy"

# Overall PR window (large range)
overall_start <- ymd_hms("2024-05-28 00:00:00", tz = "UTC")
overall_end   <- ymd_hms("2024-11-27 23:59:59", tz = "UTC")

# GitHub token
token <- "YOUR_TOKEN"  # replace with your GitHub token


headers <- c(
  Authorization = paste("token", token),
  Accept = "application/vnd.github.v3+json"
)

# --- Generate monthly windows ---
monthly_starts <- seq(floor_date(overall_start, "month"), ceiling_date(overall_end, "month") - days(1), by = "month")
monthly_ends <- c(monthly_starts[-1] - days(1), overall_end)

# Dataframe to store all months
all_pr_data <- data.frame()

# --- Loop over each monthly window ---
for (i in seq_along(monthly_starts)) {
  
  start_date <- monthly_starts[i]
  end_date <- monthly_ends[i]
  
  message(sprintf("\n=== Fetching PRs from %s to %s ===", start_date, end_date))
  
  # --- Filename for this month ---
  filename <- paste0("data/", repo, "_pull_requests_", 
                     format(start_date, "%Y-%m-%d"), "_to_", 
                     format(end_date, "%Y-%m-%d"), ".csv")
  
  # --- Load existing data for this month ---
  if (file.exists(filename)) {
    existing_df <- read_csv(filename)
    fetched_prs <- existing_df$`PR Number`
    message(sprintf("Found %d existing PRs for this month, will append new ones.", nrow(existing_df)))
  } else {
    existing_df <- data.frame()
    fetched_prs <- c()
    message("No existing file found for this month, creating a new one.")
  }
  
  # --- SEARCH API query ---
  base_url <- "https://api.github.com/search/issues"
  query <- paste0(
    "repo:", owner, "/", repo,
    " is:pr created:", format(start_date, "%Y-%m-%d"), "..", format(end_date, "%Y-%m-%d")
  )
  
  params <- list(q = query, per_page = 100, page = 1)
  pr_data <- data.frame()
  
  repeat {
    message(sprintf("\nFetching page %d of search results...", params$page))
    res <- GET(base_url, add_headers(.headers = headers), query = params)
    
    # --- Debug: Status & Rate Limit ---
    message(sprintf("Status code: %d", status_code(res)))
    message(sprintf("Rate limit remaining: %s", headers(res)$`x-ratelimit-remaining`))
    
    if (status_code(res) == 403 && headers(res)$`x-ratelimit-remaining` == "0") {
      reset_time <- as.numeric(headers(res)$`x-ratelimit-reset`)
      sleep_for <- reset_time - as.numeric(Sys.time()) + 5
      message(sprintf("Rate limit hit. Sleeping for %d seconds...", sleep_for))
      Sys.sleep(max(sleep_for, 1))
      next
    }
    
    res_content <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
    items <- res_content$items
    message(sprintf("Number of PRs returned on this page: %d", length(items)))
    if (length(items) == 0) break
    
    prs_df <- items %>% filter(!number %in% fetched_prs)
    
    if (nrow(prs_df) == 0) {
      message("No new PRs on this page.")
    } else {
      prs_df <- prs_df %>%
        mutate(
          created_at = ymd_hms(created_at, tz = "UTC"),
          closed_at = ymd_hms(closed_at, tz = "UTC")
        ) %>%
        filter(created_at >= start_date & created_at <= end_date)
      
      message(sprintf("Fetching details for PR numbers: %s", paste(prs_df$number, collapse = ", ")))
      
      # --- Fetch PR details (fork-safe) ---
      pr_details <- lapply(prs_df$number, function(pr_num) {
        pr_url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/pulls/", pr_num)
        pr_res <- GET(pr_url, add_headers(.headers = headers))
        
        if (status_code(pr_res) != 200) {
          message(sprintf("PR %d returned status %d; setting lines changed to 0", pr_num, status_code(pr_res)))
          return(data.frame(
            PR_Number = pr_num,
            Merged = NA,
            Additions = 0,
            Deletions = 0,
            Changed_Files = 0,
            stringsAsFactors = FALSE
          ))
        }
        
        pr_info <- fromJSON(content(pr_res, "text", encoding = "UTF-8"), flatten = TRUE)
        
        data.frame(
          PR_Number = pr_num,
          Merged = !is.null(pr_info$merged_at),
          Additions = if(!is.null(pr_info$additions)) pr_info$additions else 0,
          Deletions = if(!is.null(pr_info$deletions)) pr_info$deletions else 0,
          Changed_Files = if(!is.null(pr_info$changed_files)) pr_info$changed_files else 0,
          stringsAsFactors = FALSE
        )
      })
      
      pr_details_df <- do.call(rbind, pr_details)
      
      if (is.null(pr_details_df) || nrow(pr_details_df) == 0) {
        pr_details_df <- data.frame(
          PR_Number = integer(0),
          Merged = logical(0),
          Additions = integer(0),
          Deletions = integer(0),
          Changed_Files = integer(0)
        )
      }
      
      # --- Merge metadata with PR details ---
      prs_filtered <- prs_df %>%
        left_join(pr_details_df, by = c("number" = "PR_Number")) %>%
        transmute(
          `PR Number` = number,
          Title = title,
          Author = user.login,
          State = state,
          `Created At` = created_at,
          `Closed At` = closed_at,
          Merged = ifelse(is.na(Merged), FALSE, Merged),
          Additions = Additions,
          Deletions = Deletions,
          `Changed Files` = Changed_Files,
          `Total Lines Changed` = Additions + Deletions
        )
      
      pr_data <- bind_rows(pr_data, prs_filtered)
      message(sprintf("Fetched %d new PRs on this page.", nrow(prs_filtered)))
    }
    
    # Pagination
    links <- headers(res)$link
    if (!is.null(links) && grepl('rel="next"', links)) {
      params$page <- params$page + 1
    } else {
      break
    }
  }
  
  # --- Save monthly CSV ---
  if (nrow(pr_data) > 0) {
    df <- if (nrow(existing_df) > 0) bind_rows(existing_df, pr_data) else pr_data
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    write_csv(df, filename)
    message(sprintf("Saved %d PRs for this month to %s", nrow(df), filename))
    
    # Append to overall dataframe
    all_pr_data <- bind_rows(all_pr_data, df)
  } else {
    message("No new PRs to add for this month.")
  }
}

# --- Optionally save combined CSV ---
combined_file <- paste0("data/", repo, "_pull_requests_", 
                        format(overall_start, "%Y-%m-%d"), "_to_", 
                        format(overall_end, "%Y-%m-%d"), "_combined.csv")
write_csv(all_pr_data, combined_file)
message(sprintf("Saved combined CSV with %d total PRs to %s", nrow(all_pr_data), combined_file))
