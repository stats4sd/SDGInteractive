library(tidyverse)
library(shiny)
library(plotly)
library(ggnewscale)
library(ggrepel)
library(shinyWidgets)

shinydata<-readRDS(file="shinydata.RDS")


choices_list<-c("none",shinydata$SDGNames$Var,
                "Region","sub.region","Income.Group",
                "SDG.Index.Score","population",
                shinydata$codes$IndCode)

names(choices_list)<-c("None", shinydata$SDGNames$Name,"Region","Sub-region","Income Group",
                       "SDG Index Score","Population",
                       shinydata$codes$name)

choices_list2<-c("none","no line",shinydata$SDGNames$Var,
                 "Region","sub.region","Income.Group",
                 "SDG.Index.Score","population",
                 shinydata$codes$IndCode)

names(choices_list2)<-c("Static Colour","Remove Line",  shinydata$SDGNames$Name,
                        "Region","Sub-region","Income Group",
                        "SDG Index Score","Population",
                        shinydata$codes$name)


ui <- fluidPage(
  tags$head(tags$style(HTML("
        .selectize-input, .selectize-dropdown {
          font-size: 80%;
        }
        "))),
  
  titlePanel("Pairwise SDG Plot"),
  
  sidebarLayout(
    sidebarPanel( width = 3,
                  selectInput("type","Plot Type",choices=c("Pairwise - Interactive"="Composites","7D - Static"="8D")),
                  selectInput("x","Variable on X axis",choices=choices_list[!names(choices_list)%in%c("None","Region","Sub-region","Income Group")],
                              selected = shinydata$SDGNames$Var[1]),
                  checkboxInput("delta_x","Plot change in x?",value=FALSE),
                  selectInput("y","Variable on Y axis",choices=choices_list[!names(choices_list)%in%c("None","Region","Sub-region","Income Group")],
                              selected = shinydata$SDGNames$Var[2]),
                  checkboxInput("delta_y","Plot change in y?",value=FALSE),
                  conditionalPanel("input.delta_x == '1' || input.delta_y == '1'",
                                   sliderInput("baseline_year","Year to set as baseline for change",
                                               min=2000,max=2022,step=1,value=2000,sep = "")
                  ),
                  selectInput("size","Variable for size",choices=choices_list,
                              selected = "population"),
                  selectInput("colour1","Variable for point colour",choices=choices_list,
                              selected = "Region"),
                  
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
                                              )) ,
                                   actionButton("all_country",label = "Select All Countries"),
                                   actionButton("no_country",label = "Deselect All Countries")),
                  conditionalPanel("input.method.indexOf('Region') > -1",
                                   multiInput("regions","Select regions",choices=unique(shinydata$full_data$Region),
                                              selected = "Sub-Saharan Africa",options = list(
                                                enable_search = FALSE,
                                                non_selected_header = "Excluded",
                                                selected_header = "Included"
                                              )),
                                   actionButton("all_regions",label = "Select All Region"),
                                   actionButton("no_regions",label = "Deselect All Regions")),
                  conditionalPanel("input.method.indexOf('Sub-region') > -1",
                                       multiInput("sub","Select sub-regions",choices=unique(shinydata$full_data$sub.region),
                                              selected = unique(shinydata$full_data$sub.region),options = list(
                                                enable_search = FALSE,
                                                non_selected_header = "Excluded",
                                                selected_header = "Included"
                                              )),
                                   actionButton("all_sregion",label = "Select All Sub-Regions"),
                                   actionButton("no_sregion",label = "Deselect All Sub-Regions")),
                  conditionalPanel("input.method.indexOf('Income') > -1",
                                   multiInput("income","Select income groupings",choices=unique(shinydata$full_data$Income.Group),
                                              selected = unique(shinydata$full_data$Income.Group),options = list(
                                                enable_search = FALSE,
                                                non_selected_header = "Excluded",
                                                selected_header = "Included"
                                              )),
                                   actionButton("all_income",label = "Select All Income Groups"),
                                   actionButton("no_income",label = "Deselect All Income Groups")
    )),  
    
    # Show a plot of the generated distribution
    mainPanel( width = 9,
               sliderInput("year","Year",min=2000,max=2022,step=1,value=2000,sep = ""),
               checkboxInput("play","Play"),
               conditionalPanel("input.type != '8D' ",
                                plotlyOutput("plot")),
               conditionalPanel("input.type == '8D' ",
                                plotOutput("plot2",width = "100%",height="600px"),
                                checkboxInput("shownames","Show Country Names",value=TRUE)),
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  data_full<-reactive({
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
      mutate(x=get(input$x),
             y=get(input$y),
             colour1=get(input$colour1),
             colour2=get(input$colour2),
             shape=get(input$shape),colour_line=get(input$colour_line),size=get(input$size)) %>%
      #      select(year,Country=Country.x,country.3,x=input$x,y=input$y,input$colour1,input$colour2,input$shape,input$colour_line,input$size) %>%
      select(year,Country=Country.x,country.3,x,y,colour1,colour2,shape,colour_line,size) %>%
      na.omit()
    
  })
  
  dataset<-reactive({
    print(data_full())
    d<-data_full()
    if(input$delta_x==TRUE|input$delta_y==TRUE){
      
      d %>%
        filter(year<=as.numeric(input$year)) %>%
        group_by(Country) %>%
        mutate(baseline_year=ifelse(input$baseline_year=="first",min(year),as.numeric(input$baseline_year))) ->d
      
      if(input$delta_x==TRUE){
        d<- d %>%
          mutate(baseline_x=ifelse(year==baseline_year,x,NA)) %>%
          mutate(x=x-mean(baseline_x,na.rm=T)) %>%
          select(-baseline_x)
      }
      if(input$delta_y==TRUE){
        
        d<- d %>%  
          mutate(baseline_y=ifelse(year==baseline_year,y,NA))%>%
          mutate(y=y-mean(baseline_y,na.rm=T)) %>%
          select(-baseline_y)
      } 
      
    }
    d %>%
      ungroup()
  })
  
  observeEvent(input$all_country,{
    updateMultiInput(session=session,inputId="countries",selected=unique(shinydata$full_data$Country.x))
  })
  observeEvent(input$no_country,{
    updateMultiInput(session=session,inputId="countries",selected="")
  })
  
  observeEvent(input$all_region2,{
    updateMultiInput(session=session,inputId="regions",selected=unique(shinydata$full_data$Region))
  })
  observeEvent(input$no_region2,{
    updateMultiInput(session=session,inputId="regions",selected="")
  })
  
  observeEvent(input$all_sregion,{
    updateMultiInput(session=session,inputId="sub",selected=unique(shinydata$full_data$sub.region))
  })
  observeEvent(input$no_sregion,{
    updateMultiInput(session=session,inputId="sub",selected="")
  })
  
  observeEvent(input$all_income,{
    updateMultiInput(session=session,inputId="income",selected=unique(shinydata$full_data$Income.Group))
  })
  observeEvent(input$no_income,{
    updateMultiInput(session=session,inputId="income",selected="")
  })
  
  
  output$plot <- renderPlotly({
    updateSliderInput(inputId="year",min=min(data_full()$year),max=max(data_full()$year))
    updateSliderInput(inputId="baseline_year",min=min(data_full()$year),max=max(data_full()$year))
    if(input$type=="Composites"& nrow(dataset()>0)){
      
      xlab1<-ifelse(input$delta_x==1,paste("Change in",names(choices_list2)[choices_list2==input$x],input$baseline_year,"to",input$year),
                    names(choices_list2)[choices_list2==input$x])
      ylab1<-ifelse(input$delta_y==1,paste("Change in",names(choices_list2)[choices_list2==input$y],input$baseline_year,"to",input$year),
                    names(choices_list2)[choices_list2==input$y])
      
      (dataset() %>%
          filter(year<=as.numeric(input$year)) %>%
          
          ggplot(aes(y=y,
                            x=x,
                            col=colour1,
                            size=size,
                            Country=Country,
                            year=year))+
          geom_line(show.legend=FALSE,aes(group=Country),alpha=0.2,size=0.5)+
          geom_point(data=filter(dataset(),year==as.numeric(input$year)),alpha=1)+
          xlab(xlab1)+
          ylab(ylab1)+
          theme_light()+
          theme(legend.position = "bottom")+
          labs(col=names(choices_list2)[choices_list2==input$colour1],
               size=names(choices_list2)[choices_list2==input$size])+
          ggtitle(paste(xlab1,"vs",ylab1),subtitle=input$year))->p1
      
      p1 %>% ggplotly()
    }
  })
  
  observe({
    current_year<-as.numeric(input$year)
    if(input$play==TRUE&!current_year==max(dataset()$year)){

      updateSliderInput(inputId="year",value=current_year+1)
    }
  
  })
  
  output$plot2 <- renderPlot(res=144,{
    updateSliderInput(inputId="year",min=min(data_full()$year),max=max(data_full()$year))
    updateSliderInput(inputId="baseline_year",min=min(data_full()$year),max=max(data_full()$year))
    if(input$type=="8D" & nrow(dataset()>0)){
      xlab1<-ifelse(input$delta_x==1,paste("Change in",names(choices_list2)[choices_list2==input$x],input$baseline_year,"to",input$year),
                    names(choices_list2)[choices_list2==input$x])
      ylab1<-ifelse(input$delta_y==1,paste("Change in",names(choices_list2)[choices_list2==input$y],input$baseline_year,"to",input$year),
                    names(choices_list2)[choices_list2==input$y])
      
      
      
      
      
      
      (dataset() %>%
          filter(year<=as.numeric(input$year)) %>%
          ggplot(aes(y=y,
                            x=x,year=year))+
          theme_light()+
          theme(legend.position = "bottom",
                legend.title = element_text(size=8),
                plot.title = element_text(size=8),
                axis.title = element_text(size=8),
                axis.text = element_text(size=6),
                legend.text= element_text(size=6))+
          ggtitle(paste(xlab1,"vs",ylab1),subtitle=input$year)+
          xlab(xlab1)+
          ylab(ylab1))->p1
      
      if(input$colour_line!="none" & input$colour_line!="no line"){
        
        if(class(dataset()$colour_line)=="numeric"|class(dataset()$colour_line)=="integer"|
           class(dataset()$colour_line)=="double"){
          c1<-scale_color_fermenter(palette="RdYlGn")
        }else{
          c1<-scale_color_brewer(palette="Greens")
        }
        
        
        
        p1<-p1+ geom_line(aes(group=Country.x,colour=colour_line),alpha=0.5)+
          c1+
          labs(colour=names(choices_list2)[choices_list2==input$colour_line])+new_scale_colour()}
      
      if(input$colour_line=="none"){
        p1<-p1+ geom_line(aes(group=Country),col="gray50",alpha=0.2)
      }
      
      
      if(input$colour1!="none"){
        if(class(dataset()$colour1)=="numeric"|class(dataset()$colour1)=="integer"|
           class(dataset()$colour1)=="double"){
          c2<-scale_fill_fermenter(palette = "Reds",direction = 1)
          
        }else{
          c2<-scale_fill_brewer(palette = "Reds",direction = 1)
        }
        
        
        p1<-p1+
          geom_point(data=filter(dataset(),year==as.numeric(input$year)),
                     aes(fill=colour1,size=size),
                     shape=21,alpha=0.8,stroke=0.2,col="black")+
          c2+
          labs(fill=names(choices_list2)[choices_list2==input$colour1])
      }
      
      if(input$colour2!="none"){
        
        if(class(dataset()$colour2)=="numeric"|class(dataset()$colour2)=="integer"|
           class(dataset()$colour2)=="double"){
          c3<-scale_colour_fermenter(palette = "Blues",direction = 1)
          
        }else{
          c3<-scale_fill_brewer(palette="Blues",direction = 1)
        }
        
        p1<-p1+
          geom_point(data=filter(dataset(),year==as.numeric(input$year)),
                     aes(col=colour2,size=size),
                     shape=21,alpha=0.8,stroke=3)+
          geom_point(data=filter(dataset(),year==as.numeric(input$year)),
                     aes(size=size),
                     shape=1,alpha=0.5,col="black",stroke=0.25)+
          geom_point(data=filter(dataset(),year==as.numeric(input$year)),
                     aes(size=size),
                     shape=1,alpha=0.5,col="black",stroke=0.25)+
          c3+
          labs(colour=names(choices_list2)[choices_list2==input$colour2])
      }
      
      
      if(input$shape!="none"){
        p1<-p1+
          geom_point(data=filter(dataset(),year==as.numeric(input$year)),
                     aes(shape=shape),col="gray30",size=2)+
          scale_shape_binned(limits=c(0,100),breaks=seq(0,100,by=20))+
          labs(shape=names(choices_list2)[choices_list2==input$shape])
      }
      if(input$size!="none"){
        p1<-p1+
          labs(size=names(choices_list2)[choices_list2==input$size])
      }
      
      if(input$shownames==TRUE){
        p1<- p1+
          geom_text_repel(data=filter(dataset(),year==as.numeric(input$year)),
                          aes(label=country.3),size=2,col="black"
          )
      }
      p1
    }
    
    
  })
  
}
# Run the application 
shinyApp(ui = ui, server =server)
