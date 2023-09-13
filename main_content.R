timeline <- fluidRow(
    column(
        sliderInput(
            "year",
            "Year",
            min = 2000,
            max = 2022,
            step = 1,
            value = 2000,
            sep = ""
        ),
        width = 5
    ),
    column(actionButton("play", "Play"), width = 1),
    column(actionButton("restart", "Reset"), width = 1)
)

plot_plotly <- conditionalPanel(
    "input.type != '8D' ",

    plotlyOutput("plot_plotly")
)

plot_interactive <- conditionalPanel(
    "input.type == '8D' ",

    plotOutput(
        "plot_interactive",
        width = "100%",
        height = "620px"
    ),

    checkboxInput(
        "shownames",
        "Show Country Names",
        value = TRUE
    )
)