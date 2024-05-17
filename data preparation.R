library(tidyverse)
library(readxl)


#Data preparation

Countries<-read.csv("SDGCountry.csv")

all<-read_excel("SDR2023-data.xlsx", "Backdated SDG Index", .name_repair = "universal") %>% 
  select(1:22) %>% 
  rename(country.3=Country.Code.ISO3)

indicies<-read_excel("SDR2023-data.xlsx", "Raw data (time series)", .name_repair = "universal")
codes<-read_excel("SDR2023-data.xlsx", "Codebook", .name_repair = "universal") %>%
  mutate(name=paste("SDG",SDG,":",Indicator)) %>% 
  filter(!is.na(SDG))

SDGNames<-data.frame(Name=c("SDG1: No Poverty",
                            "SDG2: No Hunger",
                            "SDG3: Good Health and Well-Being",
                            "SDG4: Quality Education",
                            "SDG5: Gender Equality",
                            "SDG6: Clean Water and Sanitation",
                            "SDG7: Affordable and Clean Energy",
                            "SDG8: Decent Work and Economic Growth",
                            "SDG9: Industry, Innovation and Infrastructure",
                            "SDG10: Reduced Inequalities",
                            "SDG11: Sustainable Cities and Communities",
                            "SDG12: Responsible Consumption and Production",
                            "SDG13: Climate Action",
                            "SDG14: Life Below Water",
                            "SDG15: Life on Land",
                            "SDG16: Peace, Justice and Strong Institutions",
                            "SDG17: Partnerships for the Goals"),Var=paste("Goal",1:17,"Score",sep="."))


##read country codes with regions
country<-read_csv("iso3 country codes.csv", name_repair = "universal") %>% 
  rename(country.3=alpha.3)

##filter to give data for sub-saharan africa
full_data<- all %>% left_join(country, by="country.3") %>% inner_join(Countries,by=c("country.3"="Country.Code")) %>%
  filter(substr(country.3,1,1)!="_") %>%
  inner_join(indicies,by=c("country.3"="id","year"))



full_data$Income.Group[full_data$country.3=="VEN"]<-"Lower middle income"

manual_colours<-c(brewer.pal(7,"Set2"),brewer.pal(7,"Dark2"),brewer.pal(8,"Pastel2"),brewer.pal(5,"RdYlGn")[-3])

full_data$Region<-factor(full_data$Region,levels=sort(unique(full_data$Region)))
full_data$sub.region<-factor(full_data$sub.region,levels=
                               c("Australia and New Zealand",
                                 "Eastern Asia",
                                 "Melanesia",
                                 "South-eastern Asia",
                                 "Central Asia",
                                 "Eastern Europe",
                                 "Northern Europe",
                                 "Southern Europe",
                                 "Western Europe",
                                 "Latin America and the Caribbean",
                                 "Northern Africa",
                                 "Western Asia",
                                 "Northern America",
                                 "Southern Asia",
                                 "Sub-Saharan Africa"
                                 )
                               )


full_data$Income.Group<-factor(full_data$Income.Group,levels=c("Low income", "Lower middle income",
                                                         "Upper middle income","High income"  ))

names(manual_colours)<-c(levels(full_data$Region),
                         levels(full_data$sub.region),
                         levels(full_data$Income.Group))

full_data<-droplevels(full_data)

saveRDS(list(full_data=full_data,SDGNames=SDGNames,codes=codes,manual_colours=manual_colours),file="shinydata.RDS")