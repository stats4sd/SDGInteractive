library(tidyverse)
library(shiny)
library(plotly)
library(ggnewscale)
library(ggrepel)
library(shinyWidgets)
library(RColorBrewer)

shinydata <- readRDS(file = "shinydata.RDS")
shinydata$full_data$`no line` <- ""

source("prepare_choice_lists.R")
source("sidebar_panel.R")
source("main_content.R")


ui <- htmlTemplate("template.html",
  sidebar = sidebar,
  timeline = timeline,
  plot_plotly = plot_plotly,
  plot_interactive = plot_interactive,
  bottom_text = htmlOutput("bottom_text")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## Text for "variables included"
  output$bottom_text <- renderText({
    footnote <- ""
    if (input$x %in% shinydata$SDGNames$Var) {
      footnote <- paste(
        footnote,
        paste(
          names(choices_list)[choices_list == input$x],
          "variables included:",
          paste(
            shinydata$codes$Indicator[shinydata$codes$SDG == as.numeric(str_split_fixed(input$x, pattern = fixed("."), 3)[2])],
            collapse = "; "
          )
        ),
        "<br>",
        sep = "<br>"
      )
    }

    if (input$y %in% shinydata$SDGNames$Var) {
      footnote <- paste(
        footnote,
        paste(
          names(choices_list)[choices_list == input$y],
          "variables included:",
          paste(
            shinydata$codes$Indicator[shinydata$codes$SDG == as.numeric(str_split_fixed(input$y, pattern = fixed("."), 3)[2])],
            collapse = "; ")
          ),
        "<br>",
        sep = "<br>"
      )
    }

    if (input$x %in% shinydata$codes$IndCode) {
      footnote <-
        paste0(
          footnote,
          paste(
            shinydata$codes$name[shinydata$codes$IndCode == input$x],
            ":",
            shinydata$codes$Description[shinydata$codes$IndCode == input$x],
            ". Source: ",
            shinydata$codes$Source[shinydata$codes$IndCode == input$x],
            ". Reference: ",
            shinydata$codes$Reference[shinydata$codes$IndCode == input$x],
            "<br>"
          )
        )
    }

    if (input$y %in% shinydata$codes$IndCode) {
      footnote <-
        paste0(
          footnote,
          paste(
            shinydata$codes$name[shinydata$codes$IndCode == input$y],
            ":",
            shinydata$codes$Description[shinydata$codes$IndCode == input$y],
            ". Source: ",
            shinydata$codes$Source[shinydata$codes$IndCode == input$y],
            ". Reference: ",
            shinydata$codes$Reference[shinydata$codes$IndCode == input$y],
            "<br>"
          )
        )
    }

    if (input$colour1 %in% shinydata$SDGNames$Var) {
      footnote <- paste(
        footnote,
        paste0(
          names(choices_list)[choices_list == input$colour1],
          " variables included: ",
          paste(
            shinydata$codes$Indicator[shinydata$codes$SDG == as.numeric(str_split_fixed(input$colour1, pattern = fixed("."), 3)[2])],
            collapse = "; "
          )
        ),
        "<br>",
        sep = "<br>"
      )
    }

    if (input$colour2 %in% shinydata$SDGNames$Var) {
      footnote <- paste(footnote,
        paste0(
          names(choices_list)[choices_list == input$colour2],
          " variables included: ",
          paste(
            shinydata$codes$Indicator[shinydata$codes$SDG == as.numeric(str_split_fixed(input$colour2, pattern = fixed("."), 3)[2])],
            collapse = "; "
          )
        ),
        "<br>",
        sep = "<br>"
      )
    }

    if (input$colour1 %in% shinydata$codes$IndCode) {
      footnote <- paste0(
        footnote,
        paste(
          shinydata$codes$name[shinydata$codes$IndCode == input$colour1],
          ":",
          shinydata$codes$Description[shinydata$codes$IndCode == input$colour1],
          ". Source: ",
          shinydata$codes$Source[shinydata$codes$IndCode == input$colour1],
          ". Reference: ",
          shinydata$codes$Reference[shinydata$codes$IndCode == input$colour1],
          "<br>"
        )
      )
    }

    if (input$colour2 %in% shinydata$codes$IndCode) {
      footnote <-
        paste0(
          footnote,
          paste(
            shinydata$codes$name[shinydata$codes$IndCode == input$colour2],
            ":",
            shinydata$codes$Description[shinydata$codes$IndCode == input$colour2],
            ". Source: ",
            shinydata$codes$Source[shinydata$codes$IndCode == input$colour2],
            ". Reference: ",
            shinydata$codes$Reference[shinydata$codes$IndCode == input$colour2],
            "<br>"
          )
        )
    }

    if (input$colour_line %in% shinydata$SDGNames$Var) {
      footnote <- paste(footnote,
        paste0(
          names(choices_list)[choices_list == input$colour_line],
          " variables included: ",
          paste(
            shinydata$codes$Indicator[shinydata$codes$SDG == as.numeric(str_split_fixed(input$colour_line, pattern = fixed("."), 3)[2])],
            collapse = "; "
          )
        ),
        "<br>",
        sep = "<br>"
      )
    }

    if (input$colour_line %in% shinydata$codes$IndCode) {
      footnote <- paste0(
        footnote,
        paste(
          shinydata$codes$name[shinydata$codes$IndCode == input$colour_line],
          ":",
          shinydata$codes$Description[shinydata$codes$IndCode == input$colour_line],
          ". Source: ",
          shinydata$codes$Source[shinydata$codes$IndCode == input$colour_line],
          ". Reference: ",
          shinydata$codes$Reference[shinydata$codes$IndCode == input$colour_line],
          "<br>"
        )
      )
    }

    if (input$shape %in% shinydata$SDGNames$Var) {
      footnote <- paste(footnote,
        paste0(
          names(choices_list)[choices_list == input$shape],
          " variables included: ",
          paste(
            shinydata$codes$Indicator[shinydata$codes$SDG == as.numeric(str_split_fixed(input$shape, pattern = fixed("."), 3)[2])],
            collapse = "; "
          )
        ),
        "<br>",
        sep = "<br>"
      )
    }

    if (input$shape %in% shinydata$codes$IndCode) {
      footnote <- paste0(
        footnote,
        paste(
          shinydata$codes$name[shinydata$codes$IndCode == input$shape],
          ":",
          shinydata$codes$Description[shinydata$codes$IndCode == input$shape],
          ". Source: ",
          shinydata$codes$Source[shinydata$codes$IndCode == input$shape],
          ". Reference: ",
          shinydata$codes$Reference[shinydata$codes$IndCode == input$shape],
          "<br>"
        )
      )
    }

    if (input$size %in% shinydata$SDGNames$Var) {
      footnote <- paste(footnote,
        paste0(
          names(choices_list)[choices_list == input$size],
          " variables included: ",
          paste(
            shinydata$codes$Indicator[shinydata$codes$SDG == as.numeric(str_split_fixed(input$size, pattern = fixed("."), 3)[2])],
            collapse = "; "
          )
        ),
        "<br>",
        sep = "<br>"
      )
    }

    if (input$size %in% shinydata$codes$IndCode) {
      footnote <- paste0(
        footnote,
        paste(
          shinydata$codes$name[shinydata$codes$IndCode == input$size],
          ":",
          shinydata$codes$Description[shinydata$codes$IndCode == input$size],
          ". Source: ",
          shinydata$codes$Source[shinydata$codes$IndCode == input$size],
          ". Reference: ",
          shinydata$codes$Reference[shinydata$codes$IndCode == input$size],
          "<br>"
        )
      )
    }

    HTML(footnote)
  })

  ## Data organisation
  data_full <- reactive({
    d <- shinydata$full_data %>% mutate(none = 1)
    if ("Country" %in% input$method) {
      d <- d %>% filter(Country.x %in% input$countries)
    }
    if ("Region" %in% input$method) {
      d <- d %>% filter(Region %in% input$regions)
    }
    if ("Income" %in% input$method) {
      d <- d %>% filter(Income.Group %in% input$income)
    }
    if ("Sub-region" %in% input$method) {
      d <- d %>% filter(sub.region %in% input$sub)
    }

    d %>%
      mutate(
        x = get(input$x),
        y = get(input$y),
        colour1 = get(input$colour1),
        colour2 = get(input$colour2),
        shape = get(input$shape),
        colour_line = get(input$colour_line),
        size = get(input$size)
      ) %>%
      select(
        year,
        Country = Country.x,
        country.3,
        x,
        y,
        colour1,
        colour2,
        shape,
        colour_line,
        size
      ) %>%
      na.omit()
  })

  dataset <- reactive({
    d <- data_full()
    if (input$delta_x == TRUE | input$delta_y == TRUE) {
      d <- d %>%
        group_by(Country) %>%
        mutate(baseline_year = ifelse(
          input$baseline_year == "first",
          min(year),
          as.numeric(input$baseline_year)
        ))

      if (input$delta_x == TRUE) {
        d <- d %>%
          mutate(baseline_x = ifelse(year == baseline_year, x, NA)) %>%
          mutate(x = x - mean(baseline_x, na.rm = T)) %>%
          select(-baseline_x)
      }

      if (input$delta_y == TRUE) {
        d <- d %>%
          mutate(baseline_y = ifelse(year == baseline_year, y, NA)) %>%
          mutate(y = y - mean(baseline_y, na.rm = T)) %>%
          select(-baseline_y)
      }
    }
    d %>%
      ungroup()
  })

  ## Observers for "Select Points By" filters
  observeEvent(input$all_country, {
    updateMultiInput(
      session = session,
      inputId = "countries",
      selected = unique(shinydata$full_data$Country.x)
    )
  })

  observeEvent(input$no_country, {
    updateMultiInput(session = session,
                     inputId = "countries",
                     selected = "")
  })

  observeEvent(input$all_regions, {
    updateMultiInput(
      session = session,
      inputId = "regions",
      selected = unique(shinydata$full_data$Region)
    )
  })

  observeEvent(input$no_regions, {
    updateMultiInput(session = session,
                     inputId = "regions",
                     selected = "")
  })

  observeEvent(input$all_sregion, {
    updateMultiInput(
      session = session,
      inputId = "sub",
      selected = unique(shinydata$full_data$sub.region)
    )
  })

  observeEvent(input$no_sregion, {
    updateMultiInput(session = session,
                     inputId = "sub",
                     selected = "")
  })

  observeEvent(input$all_income, {
    updateMultiInput(
      session = session,
      inputId = "income",
      selected = unique(shinydata$full_data$Income.Group)
    )
  })

  observeEvent(input$no_income, {
    updateMultiInput(session = session,
                     inputId = "income",
                     selected = "")
  })

  ## Play through the years
  timelineFlow <- reactiveValues(
    playing = FALSE,
    current_year = NULL,
  )

  observeEvent(input$year, {
    timelineFlow$current_year = input$year
  })


  observeEvent(input$play, {
    timelineFlow$playing <- TRUE
  })

  observeEvent(input$restart, {
    timelineFlow$playing <- FALSE
    # Reset - to prevent clashes if the timeline is still playing
    updateSliderInput(inputId = "year", value = min(data_full()$year))

  })

  observe({
    ## If max year is reached, end play
    if(timelineFlow$current_year == max(data_full()$year)) {
      timelineFlow$playing <- FALSE
    }

    if(timelineFlow$playing == TRUE) {
      updateSliderInput(inputId = "year", value = timelineFlow$current_year + 1)
    }

  })

  ## Render Plots
  output$plot_plotly <- renderPlotly({
    if (input$type == "Composites" & nrow(dataset() > 0)) {
      #    if(as.numeric(input$year)<min(data_full()$year) | as.numeric(input$year)>max(data_full()$year)){
      updateSliderInput(
        inputId = "year",
        min = min(data_full()$year),
        max = max(data_full()$year)
      )
      #   }
      #  if(as.numeric(input$baseline_year)<min(data_full()$year) | as.numeric(input$baseline_year)>max(data_full()$year)){
      updateSliderInput(
        inputId = "baseline_year",
        min = min(data_full()$year),
        max = max(data_full()$year)
      )
      # }

      xlab1 <- ifelse(
        input$x == "year",
        "Year",
        ifelse(
          input$delta_x == 1,
          paste(
            "Change in",
            names(choices_list_line)[choices_list_line == input$x],
            input$baseline_year,
            "to",
            input$year
          ),
          names(choices_list_line)[choices_list_line == input$x]
        )
      )

      ylab1 <- ifelse(
        input$delta_y == 1,
        paste(
          "Change in",
          names(choices_list_line)[choices_list_line == input$y],
          input$baseline_year,
          "to",
          input$year
        ),
        names(choices_list_line)[choices_list_line == input$y]
      )

      p1 <- (
        dataset() %>%
          filter(year <= as.numeric(input$year)) %>%

          ggplot(
            aes(
              y = y,
              x = x,
              col = colour1,
              size = size,
              Country = Country,
              year = year
            )
          ) +
          geom_point(data = filter(
            dataset(), year == as.numeric(input$year)
          )) +
          geom_line(
            show.legend = FALSE,
            aes(group = Country),
            alpha = 0.2,
            size = 0.5
          ) +
          xlab(str_wrap(xlab1, 90)) +
          ylab(str_wrap(ylab1, 90)) +
          xlim(c(min(dataset()$x), max(dataset()$x))) +
          ylim(c(min(dataset()$y), max(dataset()$y))) +
          theme_light() +
          theme(
            legend.position = "bottom",
            title = element_text(size = 8),
            legend.text = element_text(size = 6)
          ) +
          labs(col = names(choices_list_line)[choices_list_line == input$colour1],
               size = "") +
          ggtitle(str_wrap(paste(
            xlab1, "vs", ylab1
          ), 90), subtitle = input$year)
      )

      if (input$colour1 %in% c("Region", "sub.region", "Income.Group")) {
        p1 <- p1 + scale_color_manual(values = shinydata$manual_colours)
      }

      p1 %>% ggplotly()
    }
  })

  output$plot_interactive <- renderPlot(res = 130, {
    if (input$type == "8D" & nrow(dataset() > 0)) {
      #    if(as.numeric(input$year)<min(data_full()$year) | as.numeric(input$year)>max(data_full()$year)){
      updateSliderInput(
        inputId = "year",
        min = min(data_full()$year),
        max = max(data_full()$year)
      )
      #   }
      #  if(as.numeric(input$baseline_year)<min(data_full()$year) | as.numeric(input$baseline_year)>max(data_full()$year)){
      updateSliderInput(
        inputId = "baseline_year",
        min = min(data_full()$year),
        max = max(data_full()$year)
      )
      # }

      xlab1 <- ifelse(
        input$x == "year",
        "Year",
        ifelse(
          input$delta_x == 1,
          paste(
            "Change in",
            names(choices_list_line)[choices_list_line == input$x],
            input$baseline_year,
            "to",
            input$year
          ),
          names(choices_list_line)[choices_list_line == input$x]
        )
      )

      ylab1 <- ifelse(
        input$delta_y == 1,
        paste(
          "Change in",
          names(choices_list_line)[choices_list_line == input$y],
          input$baseline_year,
          "to",
          input$year
        ),
        names(choices_list_line)[choices_list_line == input$y]
      )

      p1 <- (
        dataset() %>%
          filter(year <= as.numeric(input$year)) %>%
          ggplot(aes(
            y = y,
            x = x, year = year
          )) +
          theme_light() +
          theme(
            legend.position = "bottom",
            legend.title = element_text(size = 8),
            plot.title = element_text(size = 8),
            axis.title = element_text(size = 8),
            axis.text = element_text(size = 6),
            legend.text = element_text(size = 6)
          ) +
          xlim(c(min(dataset()$x), max(dataset()$x))) +
          ylim(c(min(dataset()$y), max(dataset()$y))) +
          ggtitle(paste(xlab1, "vs", ylab1), subtitle = input$year) +
          xlab(xlab1) +
          ylab(ylab1)
      )

      if (input$colour_line != "none" &
          input$colour_line != "no line") {
        if (class(dataset()$colour_line) == "numeric" |
            class(dataset()$colour_line) == "integer" |
            class(dataset()$colour_line) == "double") {
          c1 <- scale_color_fermenter(palette = "RdYlGn")
        } else {
          # if(input$colour_line%in%c("Region","sub.region","Income.Group")){
          # c1<-scale_color_manual(values=shinydata$manual_colours)
          #  }
          # else{
          c1 <- scale_color_brewer(palette = "Greens")
          #}
        }

        p1 <- p1 +
        geom_line(
          aes(group = Country, colour = colour_line),
          alpha = 0.5
        ) +
        c1 +
        labs(colour = names(choices_list_line)[choices_list_line ==   input$colour_line]) +
        new_scale_colour()
      }

      if (input$colour_line == "none") {
        p1 <- p1 + geom_line(
          aes(group = Country),
          col = "gray50",
          alpha = 0.2
        )
      }

      if (input$colour1 != "none") {
        if (class(dataset()$colour1) == "numeric" |
            class(dataset()$colour1) == "integer" |
            class(dataset()$colour1) == "double") {
          c2 <- scale_fill_fermenter(palette = "Reds", direction = 1)

        } else {
          if (input$colour1 %in% c("Region", "sub.region", "Income.Group")) {
            c2 <- scale_fill_manual(values = shinydata$manual_colours)
          } else {
            c2 <- scale_fill_brewer(palette = "Reds", direction = 1)
          }
        }

        p1 <- p1 +
          geom_point(
            data = filter(dataset(), year == as.numeric(input$year)),
            aes(fill = colour1, size = size),
            shape = 21,
            alpha = 0.8,
            stroke = 0.2,
            col = "black"
          ) +
          c2 +
          labs(fill = names(choices_list_line)[choices_list_line == input$colour1])
      }

      if (input$colour2 != "none") {
        if (class(dataset()$colour2) == "numeric" |
            class(dataset()$colour2) == "integer" |
            class(dataset()$colour2) == "double") {
          c3 <- scale_colour_fermenter(palette = "Blues", direction = 1)

        } else{
          if (input$colour2 %in% c("Region", "sub.region", "Income.Group")) {
            c3 <- scale_color_manual(values = shinydata$manual_colours)
          }
          else{
            c3 <- scale_fill_brewer(palette = "Blues", direction = 1)
          }
        }

        p1 <- p1 +
          geom_point(
            data = filter(dataset(), year == as.numeric(input$year)),
            aes(col = colour2, size = size),
            shape = 21,
            alpha = 0.8,
            stroke = 3
          ) +
          geom_point(
            data = filter(dataset(), year == as.numeric(input$year)),
            aes(size = size),
            shape = 1,
            alpha = 0.5,
            col = "black",
            stroke = 0.25
          ) +
          geom_point(
            data = filter(dataset(), year == as.numeric(input$year)),
            aes(size = size),
            shape = 1,
            alpha = 0.5,
            col = "black",
            stroke = 0.25
          ) +
          c3 +
          labs(colour = names(choices_list_line)[choices_list_line == input$colour2])
      }


      if (input$shape != "none") {
        p1 <- p1 +
          geom_point(
            data = filter(dataset(), year == as.numeric(input$year)),
            aes(shape = shape),
            col = alpha("gray30", 0.5),
            size = 2
          ) +
          scale_shape_binned(limits = c(0, 100),
                             breaks = seq(0, 100, by = 20)) +
          labs(shape = names(choices_list_line)[choices_list_line == input$shape])
      }
      if (input$size != "none") {
        p1 <- p1 +
          labs(size = names(choices_list_line)[choices_list_line == input$size])
      }

      if (input$shownames == TRUE) {
        p1 <- p1 +
          geom_text_repel(
            data = filter(dataset(), year == as.numeric(input$year)),
            aes(label = country.3),
            size = 2,
            col = "black"
          )
      }
      p1 +
        theme(
          legend.text = element_text(size = 5),
          legend.title = element_text(size = 6),
          plot.subtitle = element_text(size = 6),
          legend.box = "vertical",
          legend.margin = margin(t = 0, unit = 'cm'),
          legend.key.size = unit(0.6, "lines")
        )
    }
  })

}
# Run the application
shinyApp(ui = ui, server = server)
