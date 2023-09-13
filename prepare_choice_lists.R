shinydata <- readRDS(file = "shinydata.RDS")
shinydata$full_data$`no line` <- ""
## Setup different choices lists
plot_types <- c(
  "Pairwise - Interactive" = "Composites",
  "7D - Static" = "8D"
)

## Prepare sdg lists
sdg_list <- shinydata$SDGNames$Var
names(sdg_list) <- shinydata$SDGNames$Name

sdg_indicator_list <- shinydata$codes$IndCode
names(sdg_indicator_list) <- shinydata$codes$name

## Main Choices list
choices_list <- c(
  "None" = "none",
  sdg_list,
  "Region" = "Region",
  "Sub-region" = "sub.region",
  "Income Group" = "Income.Group",
  "SDG Index Score" = "SDG.Index.Score",
  "Population" = "population",
  sdg_indicator_list
)

## For Axes
choices_list_x <- c(
  "Year" = "year",
  sdg_list,
  "SDG Index Score" = "SDG.Index.Score",
  "Population" = "population",
  sdg_indicator_list
  )

choices_list_y <-c(
  sdg_list,
  "SDG Index Score" = "SDG.Index.Score",
  "Population" = "population",
  sdg_indicator_list
)


choices_list_line <- c(
  "Static Colour" = "none",
  "Remove Line" = "no line",
  sdg_list,
  "Region" = "Region",
  "Sub-region" = "sub.region",
  "Income Group" = "Income.Group",
  "SDG Index Score" = "SDG.Index.Score",
  "Population" = "population",
  sdg_indicator_list
)
