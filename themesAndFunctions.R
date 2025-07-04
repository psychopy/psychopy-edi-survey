# install and/or attach packages

# packagesRequired <-
#   c(
#     "codebook",
#     "colorblindcheck",
#     "dplyr",
#     "DT",
#     "flextable",
#     "forcats",
#     "gdata",
#     "ggplot2",
#     "ggtext",
#     "grateful",
#     "gtsummary",
#     "knitr",
#     "magrittr",
#     "purrr",
#     "readr",
#     "rmarkdown",
#     "showtext",
#     "sjlabelled",
#     "sjmisc",
#     "sjPlot",
#     "stringr",
#     "sysfonts",
#     "systemfonts",
#     "summarytools",
#     "tibble",
#     "tidyr",
#     "tidyselect",
#     "xfun"
#   )

# Un-comment the code below to install any packages you're missing from the 
# list in packagesRequired

# packagesInstalled <- installed.packages()[,"Package"]
# packagesMissing <- packagesRequired[!(packagesRequired %in% packagesInstalled)]
# if(length(packagesMissing)) install.packages(packagesMissing, repos = getCRANmirrors()[1,"URL"])

# attach the packages
# invisible(lapply(packagesRequired, require, character.only=T))

# get the data

# visMain <- readRDS("data/tidyLabeledSurveyData.rds")

# themes

## font

# sysfonts::font_add_google("Raleway", "raleway")
# showtext::showtext_auto()

## set a general theme for sjPlot

sjPlot::set_theme(
  geom.label.size = 5,
  axis.angle.x = 10, 
  base = theme_blank(base_family = "raleway"),
  plot.backcol = "grey95",
  panel.gridcol = "grey95",
  plot.bordercol = "grey95",
  axis.linecolor = "grey95",
  geom.outline.size = 0
)

## for the flextable outputs from gtsummary
flextable::set_flextable_defaults(
  font.family = "raleway",
  font.size = 12,
  arraystretch = 1,
  theme_fun = "theme_zebra"
)

## create an OST pallete for Likert scales with 5 items

ostBlue   <- "#26DCF0"
ostOrange <- "#EB692A"
ostYellow <- "#F7F11C"

ostColours2 <- 
  c(
    "#26DCF0",
    "#EB692A"
  )

ostColours2rev <- 
  c(
    "#EB692A",
    "#26DCF0"
  )

ostColours3 <- 
  c(
    "#26DCF0",
    "#EB692A",
    "#F7F11C"
  )

ostColours4na <- 
  c(
    "#26DCF0",
    "#EB692A",
    "#F7F11C",
    "darkgrey"
  )

ostColours4 <-
  c(
    "#EB692A",
    "#f3a57f",
    "#7deaf6",
    "#26DCF0"
  )

ostColours5 <- 
  c(
    "#EB692A",
    "#f3a57f",
    "#F7F11C",
    "#7deaf6",
    "#26DCF0"
  )

ostColours6 <- 
  c(
    "#EB692A",
    "#f3a57f",
    "#fbe1d4",
    "#d4f8fc",
    "#7deaf6",
    "#26DCF0"
  )

ostColours7 <- 
  c(
    "#EB692A",
    "#f3a57f",
    "#fbe1d4",
    "#F7F11C",
    "#d4f8fc",
    "#7deaf6", 
    "#26DCF0"
  )

# Functions

## Plots

### function for frequency plots 
freqPlot <- 
  function(
    .x, 
    ostColour, 
    sorting
    ) {
  p <- 
    sjPlot::plot_frq(
      .x,
      title = get_label(.x),
      sort.frq = sorting,
      wrap.title = 70,
      type = "bar",
      axis.title = "response",
      expand.grid = T,
      geom.colors = ostColour, 
      show.values = T,
      drop.empty = F
    )
  .GlobalEnv$p <- p
  }

### function for stacked Likert plot
stackedLikertGroups <- function(.x, likertTitle) {
  p <-
    sjPlot::plot_likert(
      .x,
      title = likertTitle,
      legend.title = "response",
      groups = c(1,1,1,1,1,2,2,3),
      groups.titles = c("Treatment by others", "Self-regard", "Isolation"),
      catcount = 6,
      legend.pos = "bottom",
      reverse.scale = T,
      sort.frq = "pos.asc",
      values = "sum.inside",
      cat.neutral = 4,
      cat.neutral.color = "#F7F11C",
      grid.range = c(1.1,1.1),
      group.legend.options = list(nrow = 1),
      wrap.labels = 40,
      geom.colors = ostColours6,
      geom.size = .75,
      digits = 0,
      show.prc.sign = T,
      rel_heights = c(5,2,2)
    )
  .GlobalEnv$p <- p
}

### function for stacked frequencies
stackedFrequencies <- 
  function(
    .x,
    colours,
    ...,
    figTitle) {
    p <- 
      sjPlot::plot_stackfrq(
        .x,
        geom.colors = colours,
        title = figTitle,
        wrap.title = 80,
        digits = 0,
        sort.frq = "last.desc",
        geom.size = .75,
        show.prc = F,
        show.n = T
      )
    .GlobalEnv$p <- p
  }

### function for  plot contingency
contingencyPlot <- 
  function(
    df, 
    contingX, 
    contingGrp, 
    colours
    ) {
  X <<- dplyr::pull(df, contingX)
  Grp <<- dplyr::pull(df, contingGrp)
  sjPlot::set_theme(geom.outline.size = 0)
  p <- sjPlot::plot_xtab(
    x = X, 
    grp = Grp,
    margin = "row",
    bar.pos = "stack",
    show.prc = F,
    geom.colors = colours,
    vjust = "inward",
    expand.grid = T
    )
  .GlobalEnv$p <- p
  }

### function for plotting wrapped frequencies
wrappedFreqPlot <- function(
    .x,
    xVar,
    fillVar,
    wrapVar,
    ostColours
) {
  xVar <- enquo(xVar)
  wrapVar <- enquo(wrapVar)
  p <- ggplot(
    .x,
    aes(
      x=!!xVar,
      fill=!!xVar
      )
    ) +
    geom_bar(stat = "count") +
    geom_text(
      aes(label = after_stat(count)),
      stat = "count",
      hjust = "top",
      nudge_y = 20
      ) +
    scale_fill_manual(values=ostColours) +
    facet_wrap(wrapVar, ncol = 2)
  .GlobalEnv$p <- p
}

### function for plotting wrapped frequency interactions
wrappedStackedFreqPlot <- function(
    .x,
    xVar,
    fillVar,
    ostColours,
    figCaption,
    fillLabel,
    figTitle,
    ...,
    wrapVar,
    titleAdjust
    ) {
  xVar <- enquo(xVar)
  fillVar <- enquo(fillVar)
  wrapVar <- enquo(wrapVar)
  p <- ggplot(
    .x,
    aes(
      x=!!xVar,
      fill=!!fillVar
      )
    ) +
  geom_bar(
    position = "fill",
    stat = "count"
  ) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count", 
    position = position_fill(vjust = .5)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 25,
      vjust = 1,
      hjust = 1
      ),
    plot.title = element_text(vjust = titleAdjust)
    ) +
  scale_fill_manual(values=ostColours) +
    facet_wrap(
      wrapVar, 
      ncol = 2,
      labeller = label_wrap_gen(width=70)
    ) +
    labs(
      title = figTitle,
      x = "",
      y = "",
      fill = fillLabel,
      caption = figCaption
    )
  .GlobalEnv$p <- p
}


### function for boxplots with interactions
interactionBoxplot <- function(
    .x,
    xVar,
    yVar,
    ostColours,
    fillLabel,
    xLabel,
    figCaption,
    upperBound,
    lowerBound,
    ...,
    tickLabels,
    wrapVar
) {
  xVar <- enquo(xVar)
  yVar <- enquo(yVar)
  wrapVar <- enquo(wrapVar)
  
  p <- ggplot(
    .x,
    aes(
      x=!!xVar,
      y=!!yVar,
      fill=!!yVar
    )
  ) +
    geom_boxplot() +
    geom_jitter(
      alpha=.5,
      height = .25,
      width = .3,
      na.rm = T,
      fill = "grey",
      colour = "black",
      shape = 21
    ) +
    stat_summary(
      fun = "mean", 
      geom = "point", 
      shape = 23, 
      size = upperBound, 
      color = "black", 
      fill = "grey",
      alpha = .75,
      na.rm = T
      ) +
    scale_fill_manual(
      values=ostColours,
      limits = ) +
    theme(plot.caption = element_text(hjust = 0)) +
    labs(
      title = xLabel,
      x = "",
      y = "",
      fill = fillLabel,
      caption = figCaption
    ) +
    scale_x_continuous(
      limits = c(lowerBound,upperBound),
      breaks = waiver(),
      n.breaks = upperBound
      ) +
    facet_wrap(
      wrapVar, 
      ncol = 2,
      labeller = label_wrap_gen(width=75)
      )
  .GlobalEnv$p <- p
}

## Tables

### Likert table

likertTable <- function(
    .data,
    lowerBound,
    upperBound,
    byVar = NULL,
    spanVar
) {
  # If byVar is provided, capture and process it
  if (!is.null(byVar)) {
    byVar_quo <- rlang::enquo(byVar)
    byVar_name <- rlang::as_name(byVar_quo)
    
    label_list <- setNames(
      list(sjlabelled::get_label(.data[[byVar_name]])),
      byVar_name
    )
    
    p <- 
      gtsummary::tbl_summary(
        .data,
        by = !!byVar_quo,
        type = everything() ~ "continuous2",
        statistic = all_continuous() ~ c(
          "{mean}",
          "{sd}",
          "{median}",
          "{p25}, {p75}",
          "{min}, {max}"
        ),
        digits = list(everything() ~ c(1,2,1,1,1,0,0)),
        missing = "no",
        label = label_list
      )
  } else {
    # No grouping variable
    p <- 
      gtsummary::tbl_summary(
        .data,
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
      )
  }
  
  # Continue with formatting
  p <- p %>%
    gtsummary::modify_header(
      label = paste0("**Statement** (", lowerBound, "; ", upperBound, ")")
    ) %>%
    gtsummary::modify_spanning_header(all_stat_cols() ~ deparse(substitute(spanVar))) %>%
    gtsummary::add_stat_label() %>%
    gtsummary::as_flex_table()
  
  flextable::flextable_to_rmd(p)
}


### contingency tables for interactions
contingencyTable <- function(
    .data,
    contingX,
    contingGrp
    ) {
p <-  gtsummary::tbl_cross(
    .data,
    contingX,
    contingGrp,
    percent = "cell",
    missing = "always",
    missing_text = "NA"
  ) |>
    gtsummary::as_flex_table()
flextable::flextable_to_rmd(p)
}


# This stratifies the contingency table by one more variable
stratifiedContingencyTable <- function(
  .data,
  strataVar,
  byVar
  ) {
  p <- gtsummary::tbl_strata(
    .data,
    strata = strataVar,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(
        by = byVar, 
        missing = "no"
        ),
    .header = "**{strata}**"
    ) |>
    gtsummary::as_flex_table()
flextable::flextable_to_rmd(p)
}