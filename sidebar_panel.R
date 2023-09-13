sidebar <- sidebarPanel(
    width = 12,

    selectInput(
        "type",
        "Plot Type",
        choices = plot_types
    ),

    selectInput(
        "x",
        "Variable on X axis",
        choices = choices_list_x,
        selected = shinydata$SDGNames$Var[1]
    ),

    checkboxInput("delta_x", "Plot change in x?", value = FALSE),

    selectInput(
        "y",
        "Variable on Y axis",
        choices = choices_list[!names(choices_list) %in% c("None", "Region", "Sub-region", "Income Group")],
        selected = shinydata$SDGNames$Var[2]
    ),

    checkboxInput("delta_y", "Plot change in y?", value = FALSE),

    conditionalPanel(
        "input.delta_x == '1' || input.delta_y == '1'",
        sliderInput(
            "baseline_year",
            "Year to set as baseline for change",
            min = 2000,
            max = 2022,
            step = 1,
            value = 2000,
            sep = ""
        )
    ),

    selectInput(
        "size",
        "Variable for size",
        choices = choices_list,
        selected = "population"
    ),

    selectInput(
        "colour1",
        "Variable for point colour",
        choices = choices_list,
        selected = "Region"
    ),

    # Extra Pairwise options
    conditionalPanel(
        "input.type == '8D'",

        selectInput(
            "colour2",
            "Variable for outer colour",
            choices = choices_list,
            selected = "none"
        ),

        selectInput(
            "shape",
            "Variable for shape",
            choices = choices_list,
            selected = "none"
        ),

        selectInput(
            "colour_line",
            "Variable for line colour",
            choices = choices_list_line,
            selected = "none"
        ),
    ),

    ## Select Points by
    selectInput(
        "method",
        "Select Points by:",
        choices = c("Country", "Region", "Sub-region", "Income"),
        multiple = TRUE
    ),

    conditionalPanel(
        "input.method.indexOf('Country') > -1",

        multiInput(
            "countries",
            "Select countries",
            choices = as.character(unique(shinydata$full_data$Country.x)),
            selected = as.character(unique(shinydata$full_data$Country.x)),
            options = list(
                enable_search = TRUE,
                non_selected_header = "Excluded",
                selected_header = "Included"
            )
        ),

        tags$div(
            class = "d-flex justify-content-around",
            actionButton("all_country", class = "btn-select mx-2 w-100", label = "Select All Countries"),
            actionButton("no_country", class = "btn-select mx-2 w-100", label = "Deselect All Countries")
        )
    ),

    conditionalPanel(
        "input.method.indexOf('Region') > -1",

        multiInput(
            "regions",
            "Select regions",
            choices = as.character(unique(shinydata$full_data$Region)),
            selected = "Sub-Saharan Africa",
            options = list(
                enable_search = FALSE,
                non_selected_header = "Excluded",
                selected_header = "Included"
            )
        ),

        tags$div(
            class = "d-flex justify-content-around",
            actionButton("all_regions", class = "btn-select mx-2 w-100", label = "Select All Region"),
            actionButton("no_regions", class = "btn-select mx-2 w-100", label = "Deselect All Regions")
        )
    ),

    conditionalPanel(
        "input.method.indexOf('Sub-region') > -1",

        multiInput(
            "sub",
            "Select sub-regions",
            choices = as.character(unique(shinydata$full_data$sub.region)),
            selected = as.character(unique(shinydata$full_data$sub.region)),
            options = list(
                enable_search = FALSE,
                non_selected_header = "Excluded",
                selected_header = "Included"
            )
        ),

        tags$div(
            class = "d-flex justify-content-around",
            actionButton("all_sregion", class = "btn-select mx-2 w-100", label = "Select All Sub-Regions"),
            actionButton("no_sregion", class = "btn-select mx-2 w-100", label = "Deselect All Sub-Regions")
        )
    ),
    conditionalPanel(
        "input.method.indexOf('Income') > -1",

        multiInput(
            "income",
            "Select income groupings",
            choices = as.character(unique(shinydata$full_data$Income.Group)),
            selected = as.character(unique(shinydata$full_data$Income.Group)),
            options = list(
                enable_search = FALSE,
                non_selected_header = "Excluded",
                selected_header = "Included"
            )
        ),

        tags$div(
            class = "d-flex justify-content-around",
            actionButton("all_income", class = "btn-select mx-2 w-100", label = "Select All Income Groups"),
            actionButton("no_income", class = "btn-select mx-2 w-100", label = "Deselect All Income Groups")
        )
    )
)
