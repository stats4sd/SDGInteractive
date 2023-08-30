library(tidyverse)
library(shiny)
library(plotly)
library(ggnewscale)
library(ggrepel)
library(shinyWidgets)

shinydata<-readRDS(file="shinydata.RDS")

choices_list<-c("none",shinydata$SDGNames$Var,
                 "region","sub.region","Income.Group",
                 "SDG.Index.Score","population",
                 shinydata$codes$IndCode)

names(choices_list)<-c("None", shinydata$SDGNames$Name,"Region","Sub-region","Income Group",
                       "SDG Index Score","Population",
                        shinydata$codes$name)

choices_list2<-c("none","no line",shinydata$SDGNames$Var,
          "region","sub.region","Income.Group",
          "SDG.Index.Score","population",
          shinydata$codes$IndCode)

names(choices_list2)<-c("Static Colour","Remove Line",  shinydata$SDGNames$Name,
                        "Region","Sub-region","Income Group",
                     "SDG Index Score","Population",
                       shinydata$codes$name)

  
ui <- fluidPage(
  tags$head(tags$style(HTML("
        .selectize-input, .selectize-dropdown {
          font-size: 75%;
        }
        "))),
  
  titlePanel("Pairwise SDG Plot"),
 
  sidebarLayout(
    sidebarPanel( width = 3,
      selectInput("type","Plot Type",choices=c("Pairwise - Interactive"="Composites","7D - Static"="8D")),
      selectInput("x","Variable on X axis",choices=choices_list[!names(choices_list)%in%c("None","Region","Sub-region","Income Group")],
                  selected = shinydata$SDGNames$Var[1]),
      selectInput("y","Variable on Y axis",choices=choices_list[!names(choices_list)%in%c("None","Region","Sub-region","Income Group")],
                                 selected = shinydata$SDGNames$Var[2]),
      selectInput("size","Variable for size",choices=choices_list,
                  selected = "population"),
      selectInput("colour1","Variable for point colour",choices=choices_list,
                           selected = "region"),

    conditionalPanel("input.type == '8D'",
                     selectInput("colour2","Variable for outer colour",choices=choices_list,
                                          selected = "none"),
                     selectInput("shape","Variable for shape",choices=choices_list,
                                          selected = "none"),
                                
                     selectInput("colour_line","Variable for line colour",choices=choices_list2,
                                          selected = "none"),
                     ),
    selectInput("method","Select Points by:",choices=c("Country","Region","Sub-region","Income"),multiple = TRUE),
    conditionalPanel("input.method.indexOf('Country') > -1",
                     multiInput("countries","Select countries",choices=unique(shinydata$full_data$Country.x),
                                 selected = unique(shinydata$full_data$Country.x),options = list(
                                   enable_search = TRUE,
                                   non_selected_header = "Excluded",
                                   selected_header = "Included"
                                 )) ),
    conditionalPanel("input.method.indexOf('Region') > -1",
                     multiInput("regions","Select regions",choices=unique(shinydata$full_data$Region),
                                 selected = unique(shinydata$full_data$Region),options = list(
                                   enable_search = FALSE,
                                   non_selected_header = "Excluded",
                                   selected_header = "Included"
                                 ))),
    conditionalPanel("input.method.indexOf('Sub-region') > -1",
                     multiInput("sub","Select sub-regions",choices=unique(shinydata$full_data$sub.region),
                                 selected = unique(shinydata$full_data$sub.region),options = list(
                                   enable_search = FALSE,
                                   non_selected_header = "Excluded",
                                   selected_header = "Included"
                                 ))),
    conditionalPanel("input.method.indexOf('Income') > -1",
                     multiInput("income","Select income groupings",choices=unique(shinydata$full_data$Income.Group),
                                 selected = unique(shinydata$full_data$Income.Group),options = list(
                                   enable_search = FALSE,
                                   non_selected_header = "Excluded",
                                   selected_header = "Included"
                                 )))
    ),  

        # Show a plot of the generated distribution
    mainPanel( width = 9,
      sliderInput("year","Year",min=2000,max=2022,step=1,value=2000,sep = ""),
      conditionalPanel("input.type != '8D' ",
                       plotlyOutput("plot")),
      conditionalPanel("input.type == '8D' ",
                       plotOutput("plot2"),
                       checkboxInput("shownames","Show Country Names",value=TRUE)),
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {

  dataset<-reactive({
    d<-   shinydata$full_data %>% mutate(none=1)
      if("Country" %in% input$method){
        d<-    d  %>% filter(Country.x%in%input$countries )
      }
      if("Region" %in% input$method){
        d<-    d  %>%  filter(Region%in%input$regions )
      }
      if("Income" %in% input$method){
        d<-   d %>% filter(Income.Group%in%input$income )
      }
    if("Sub-region" %in% input$method){
      d<-   d %>% filter(sub.region%in%input$sub )
    }
    d %>%
    select(year,Country.x,country.3,input$x,input$y,input$colour1,input$colour2,input$shape,input$colour_line,input$size) %>%
      na.omit() 
    })


  
  output$plot <- renderPlotly({
    updateSliderInput(inputId="year",min=min(dataset()$year),max=max(dataset()$year))
  if(input$type=="Composites"){

     (dataset() %>%
       filter(year<=as.numeric(input$year)) %>%
        ggplot(aes_string(y=input$y,
                      x=input$x,
                      col=input$colour1,
                      size=input$size,
                      Country="Country.x",
                      year="year"))+
        geom_line(show.legend=FALSE,aes(group=Country.x),alpha=0.2,size=0.5)+
        geom_point(data=filter(dataset(),year==as.numeric(input$year)),alpha=1)+
          xlab(input$x)+
        ylab(input$y)+
        theme_light()+
        theme(legend.position = "bottom")+
        ggtitle(input$year))->p1

    p1 %>% ggplotly()
    }
  })
  
  output$plot2 <- renderPlot({
    updateSliderInput(inputId="year",min=min(dataset()$year),max=max(dataset()$year))
    if(input$type=="8D"){
      
     (dataset() %>%
               filter(year<=as.numeric(input$year)) %>%
                 ggplot(aes_string(y=input$y,
                            x=input$x,year="year"))+
         theme_light()+
         theme(legend.position = "bottom")+
         ggtitle(input$year)+
         xlab(input$x)+
         ylab(input$y))->p1
      
      if(input$colour_line!="none" & input$colour_line!="no line"){
        
        if(class(dataset()[[input$colour_line]])=="numeric"|class(dataset()[[input$colour_line]])=="integer"|
           class(dataset()[[input$colour_line]])=="double"){
          c1<-scale_color_fermenter(palette="RdYlGn")
        }else{
          c1<-scale_color_brewer(palette="Greens")
        }
        
         p1<-p1+ geom_line(aes_string(group="Country.x",colour=input$colour_line),alpha=0.5)+
          c1+
          labs(colour=input$colour_line)+new_scale_colour()}
      
      if(input$colour_line=="none"){
        p1<-p1+ geom_line(aes_string(group="Country.x"),col="gray50",alpha=0.2)
        }
         
      
      if(input$colour1!="none"){
        if(class(dataset()[[input$colour1]])=="numeric"|class(dataset()[[input$colour1]])=="integer"|
           class(dataset()[[input$colour1]])=="double"){
          c2<-scale_fill_fermenter(palette = "Reds",direction = 1)
    
        }else{
          c2<-scale_fill_brewer(palette = "Reds",direction = 1)
        }
        
        
        p1<-p1+
          geom_point(data=filter(dataset(),year==as.numeric(input$year)),
                     aes_string(fill=input$colour1,size=input$size),
                     shape=21,alpha=0.8,stroke=0.2,col="black")+
          c2
      }
      
      if(input$colour2!="none"){
        
            if(class(dataset()[[input$colour2]])=="numeric"|class(dataset()[[input$colour2]])=="integer"|
             class(dataset()[[input$colour2]])=="double"){
            c3<-scale_colour_fermenter(palette = "Blues",direction = 1)
            
          }else{
            c3<-scale_fill_brewer(palette="Blues",direction = 1)
          }
        
        p1<-p1+
          geom_point(data=filter(dataset(),year==as.numeric(input$year)),
                     aes_string(col=input$colour2,size=input$size),
                     shape=21,alpha=0.8,stroke=3)+
          geom_point(data=filter(dataset(),year==as.numeric(input$year)),
                     aes_string(size=input$size),
                     shape=1,alpha=0.5,col="black",stroke=0.25)+
          geom_point(data=filter(dataset(),year==as.numeric(input$year)),
                     aes_string(size=input$size),
                     shape=1,alpha=0.5,col="black",stroke=0.25)+
          c3+
          labs(colour=input$colour2)
        }
      

      if(input$shape!="none"){
        p1<-p1+
          geom_point(data=filter(dataset(),year==as.numeric(input$year)),
                     aes_string(shape=input$shape),col="gray30",size=2)+
          scale_shape_binned(limits=c(0,100),breaks=seq(0,100,by=20))
      }
    if(input$shownames==TRUE){
    p1<- p1+
       geom_text_repel(data=filter(dataset(),year==as.numeric(input$year)),
                       aes(label=country.3),size=3,col="black")
      }
     p1
    }
    
 
  })
  
}
# Run the application 
shinyApp(ui = ui, server =server)
