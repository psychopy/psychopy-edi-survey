# create a table function to summarize Likert responses
tableSum <- function(.x) {
  p <- 
    pivot_longer(
      .x,
      cols = everything(), 
      names_to = "gname", 
      values_to = "gval") |>
    group_by(gname) |>
    summarise(
      mean = mean(gval, na.rm = T),
      median = median(gval, na.rm = T),
      range = range(gval, na.rm = T),
      count = n(gval, na.rm = T)
    ) |>
    ungroup(.data)
  print(p)
}

# another version
summaryStatsLikert <- function(.x) {
  p <-
    tbl_summary(
      .x,
      by = NULL,
      type = everything() ~ "continuous2",
      statistic = all_continuous() ~ c(
        "{mean}",
        "{sd}",
        "{median}",
        "{p25}, {p75}",
        "{min}, {max}"
      ),
      digits = list(everything() ~ c(1,2,1,1,1,0,0)),
      missing = "no"
    ) |>
    add_n(
      statistic = "{N_nonmiss}",
      col_label = "N",
      footnote = FALSE,
      last = FALSE) |>
    modify_header(label = "Question", stat_0 = "")
  print(p)
}

# using dplyr only

dat2 <- visMain |>
  select(genderNegPerc:genderOthersLikeMe)

dat3 <- dat2 |>  
  pivot_longer(
    cols = everything(),
    names_to = "Statement",
    values_to = "Response")

dat4 <- dat3 |>
  group_by(Statement) |>
  reframe(
    mean = mean(Response, na.rm = T),
    sd = sd(Response, na.rm = T)
  )