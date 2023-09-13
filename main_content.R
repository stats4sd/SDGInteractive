timeline <- fluidRow(
    class = "d-flex justify-content-start align-items-center",
    tags$div(
        class = "col-4 ms-4 ps-4 year-input",
        sliderInput(
            "year",
            "Year",
            min = 2000,
            max = 2022,
            step = 1,
            value = 2000,
            sep = ""
        ),
    ),
    tags$div(
        class = "col-4 offset-1 d-flex justify-content-start",
        actionButton("play", "Play", class = "me-3"),
        actionButton("restart", "Reset")
    )
)

plot_plotly <- conditionalPanel(
    "input.type != '8D' ",
    plotlyOutput(
        "plot_plotly",
        width = "100%",
        height = "50vh",
        )
)

plot_interactive <- conditionalPanel(
    "input.type == '8D' ",
    plotOutput(
        "plot_interactive",
        width = "100%",
        height = "50vh"
    ),
    checkboxInput(
        "shownames",
        "Show Country Names",
        value = TRUE
    )
)
