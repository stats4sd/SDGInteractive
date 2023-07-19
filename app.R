library(tidyverse)
library(shiny)
library(plotly)

SDGs<-read.csv("SDGSeries.csv")
Fulldata<-read.csv("SDGData.csv")
Countries<-read.csv("SDGCountry.csv")
Countries<-filter(Countries,Region=="Sub-Saharan Africa")

ui <- fluidPage(
  titlePanel("Pairwise SDG Plot"),
 
  sidebarLayout(
    sidebarPanel(
      selectInput("x","Variable on X axis",choices=SDGs$Indicator.Name,selected = SDGs$Indicator.Name[382]),
      selectInput("y","Variable on Y axis",choices=SDGs$Indicator.Name,selected = SDGs$Indicator.Name[3]),
      selectInput("countries","Select countries",choices=Countries$Short.Name,selected = Countries$Short.Name,multiple = TRUE)),
        # Show a plot of the generated distribution
    mainPanel(
      sliderInput("year","Year",min=2000,max=2020,step=1,value=2020,sep = ""),
      plotlyOutput("plot")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {

  dataset<-reactive(
    Fulldata %>% filter(Country.Name%in%input$countries & Indicator.Name%in%c(input$x,input$y)) %>%
      gather(year,value,X1990:X2020) %>%
        mutate(year=as.numeric(substr(year,2,5))) %>%
      select(Indicator.Code,value,country=Country.Name,Country.Code,year) %>%
          spread(Indicator.Code,value)
    
    )

  output$plot <- renderPlotly({
    print(dataset())
    (dataset() %>%
       filter(year<=as.numeric(input$year)) %>%
        ggplot(aes_string(y=SDGs$Series.Code[SDGs$Indicator.Name==input$y],
                      x=SDGs$Series.Code[SDGs$Indicator.Name==input$x],
                      col="country",year="year"))+
        geom_line(show.legend=FALSE,aes(group=country),alpha=0.2)+
        geom_point(data=filter(dataset(),year==as.numeric(input$year)),shape=1,size=4,alpha=1)+
          xlab(input$x)+
        ylab(input$y)+
        ggtitle(input$year)) %>% ggplotly()
  })
}
# Run the application 
shinyApp(ui = ui, server =server)
