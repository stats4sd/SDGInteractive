library(tidyverse)
library(shiny)
library(plotly)
library(readxl)

SDGs<-read.csv("SDGSeries.csv")
Fulldata<-read.csv("SDGData.csv")
Countries<-read.csv("SDGCountry.csv")
Countries<-filter(Countries,Region=="Sub-Saharan Africa")

all<-read_excel("SDR2023-data.xlsx", "Backdated SDG Index", .name_repair = "universal") %>% 
  select(1:22) %>% 
  rename(country.3=Country.Code.ISO3)
##read country codes with regions
country<-read_csv("iso3 country codes.csv", name_repair = "universal") %>% 
  rename(country.3=alpha.3)

##filter to give data for sub-saharan africa
africa<- all %>% left_join(country, by="country.3") %>% 
  filter(sub.region=="Sub-Saharan Africa")




ui <- fluidPage(
  titlePanel("Pairwise SDG Plot"),
 
  sidebarLayout(
    sidebarPanel(
      selectInput("type","Plot Type",choices=c("Composites","Underlying Indicators")),
      conditionalPanel("input.type == 'Composites'",
                       selectInput("x1","Variable on X axis",choices=colnames(africa)[c(6:22,5,4)],
                                   selected = colnames(africa)[6]),
                       selectInput("y1","Variable on Y axis",choices=colnames(africa)[c(6:22,5,4)],
                                   selected = colnames(africa)[7]),
                       selectInput("countries1","Select countries",choices=unique(africa$Country),
                                   selected = unique(africa$Country),multiple = TRUE))
      ,
    conditionalPanel("input.type == 'Underlying Indicators'",
      selectInput("x","Variable on X axis",choices=SDGs$Indicator.Name,selected = SDGs$Indicator.Name[382]),
      selectInput("y","Variable on Y axis",choices=SDGs$Indicator.Name,selected = SDGs$Indicator.Name[3]),
      selectInput("countries","Select countries",choices=Countries$Short.Name,
                  selected = Countries$Short.Name,multiple = TRUE))
    ),
        # Show a plot of the generated distribution
    mainPanel(
      sliderInput("year","Year",min=2000,max=2022,step=1,value=2000,sep = ""),
      plotlyOutput("plot")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {

  dataset<-reactive({
    if(input$type=="Underlying Indicators"){
    d<-Fulldata %>% filter(Country.Name%in%input$countries & Indicator.Name%in%c(input$x,input$y)) %>%
      gather(year,value,X1990:X2020) %>%
        mutate(year=as.numeric(substr(year,2,5))) %>%
      select(Indicator.Code,value,country=Country.Name,Country.Code,year) %>%
          spread(Indicator.Code,value)
    }
    if(input$type=="Composites"){
 d<-   africa %>% filter(Country%in%input$countries1)
    }
    d
    })

  output$plot <- renderPlotly({
    if(input$type=="Underlying Indicators"){
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
        ggtitle(input$year))->p1
      }
    if(input$type=="Composites"){
      print(dataset())
      (dataset() %>%
          filter(year<=as.numeric(input$year)) %>%
          ggplot(aes_string(y=input$y1,
                            x=input$x1,
                            col="Country",year="year"))+
          geom_line(show.legend=FALSE,aes(group=Country),alpha=0.2)+
          geom_point(data=filter(dataset(),year==as.numeric(input$year)),shape=1,size=4,alpha=1)+
          xlab(input$x1)+
          ylab(input$y1)+
          ggtitle(input$year))->p1
    }
    
    p1 %>% ggplotly()
  })
}
# Run the application 
shinyApp(ui = ui, server =server)
