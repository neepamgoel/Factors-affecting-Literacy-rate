install.packages("devtools")
install.packages("GGally")
install.packages("gapminder")
new_wdi_cache <- WDIcache()
devtools::install_github("ricardo-bion/ggradar",
                         dependencies = TRUE, force=TRUE)
library(GGally)
library(WDI)
library(tidyverse)
library(dplyr)
library(scales)
library(devtools)
library(ggradar)
library(plotly)

#Comparing Literacy rates with Life expectancy over the years around the world

indicator_data <-WDI(country= "all", indicator =c("SE.ADT.LITR.ZS","SE.ADT.LITR.MA.ZS",   #Literacy rate(total,male,female)
                                "SE.ADT.LITR.FE.ZS","SP.DYN.LE00.IN",
                                "SL.UEM.TOTL.NE.ZS","NY.GDP.PCAP.CD","SE.XPD.TOTL.GD.ZS","SP.RUR.TOTL.ZS"),         #life expectancy,unemployment,PGDP,% of GDP on education,Rural population
                   start = 1980, end = 2019,
                   extra=TRUE,
                   cache = new_wdi_cache)

o<-  ggplot(indicator_data,aes(year,SE.ADT.LITR.ZS )) +
  geom_point(aes(colour =SP.DYN.LE00.IN,size=1)) +
  scale_colour_gradient(low="#90e8e8", high="red")+
  labs(x="Year", y="Literacy rate",
       colour="Life\nexpectancy" ,title = "Does high Literacy rate means high Life expectancy?",
       caption="World Bank Data")
ggplotly(o)
indicator_data <- indicator_data[!is.na(indicator_data$income) & indicator_data$income!='Aggregates' & indicator_data$income!='Not classified', ]

#Boxplot for Literacy rate in different income categories calculated via GNI

w <- indicator_data %>% ggplot(aes( x=fct_reorder(income,SE.ADT.LITR.ZS),y=SE.ADT.LITR.ZS)) +
  geom_boxplot(varwidth=TRUE, fill="plum") +
  labs(title="Distribution of Literacy Rate according to income level of countries ",
       caption="World bank data",
       x="Income levels",
       y="Literacy rate")
ggplotly(w)
indicator_data=na.omit(indicator_data)

indicator_data$literacytype[indicator_data$SE.ADT.LITR.ZS<=70] <-"Low"
indicator_data$literacytype[indicator_data$SE.ADT.LITR.ZS>70 & indicator_data$SE.ADT.LITR.ZS<90] <-"Average"
indicator_data$literacytype[indicator_data$SE.ADT.LITR.ZS>=90] <-"High"
new_indicator_data <- indicator_data %>% select("country","income","SE.XPD.TOTL.GD.ZS","literacytype","SP.RUR.TOTL.ZS","SP.DYN.LE00.IN",'NY.GDP.PCAP.CD',"SE.ADT.LITR.FE.ZS","SE.ADT.LITR.MA.ZS")


#Spider chart for comparing relationship between different variables

q<- new_indicator_data %>%
  mutate_if(is.numeric, rescale) %>%
  group_by(literacytype) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(axis.label.size = 4,legend.text.size = 10,
          axis.labels = c("GDP spent on education","Rural population","Life expectancy","GDP per Capita",
                          "Female literacy rate","Male literacy rate"),
          background.circle.colour = "white",gridline.mid.colour = "gray",legend.position = "bottom",
          legend.title = "Literacy levels") 
ggplotly(q)

#World map showing life expectancy

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")+
  theme(panel.background = element_blank())+
  labs(title = "World map", caption="maps package, R")
countryDataWDI<-WDI(indicator=c("SE.ADT.LITR.ZS",
                                "NY.GDP.PCAP.KD.ZG","SP.DYN.LE00.IN"
                                ),
                    start=2017,
                    end=2017,
                    extra= TRUE,
                    cache=new_wdi_cache)
countryDataWDI <- countryDataWDI[!is.na(countryDataWDI$income) & countryDataWDI$income!='Aggregates', ]


countryDataWDI <- countryDataWDI %>%
  mutate(country=recode(str_trim(country),"United States"="USA",
                        "United Kingdom"="UK" ,"Russian Federation"="Russia",
                        "Iran, Islamic Rep." = "Iran", "Egypt, Arab Rep." = "Egypt",
                        "Kyrgyz Republic" = "Kyrgyzstan", "Lao PDR" ="Laos" ,"Korea, Rep."="North Korea",
                        "Korea, Dem. People's Rep."="South Korea" ,"Slovak Republic" ="Slovakia",
                        "Syrian Arab Republic" ="Syria", "Yemen, Rep."="Yemen",
                        "Congo, Dem. Rep." ="Democratic Republic of the Congo" ,"Venezuela, RB"="Venezuela",
                        "Congo, Rep."="Republic of Congo" ,"Cote d'Ivoire"="Ivory Coast"
                        ))
countryDataWDIMap <- left_join(world_map,countryDataWDI,by = c("region"="country"))
p <-ggplot(countryDataWDIMap,aes(long,lat,group=group))+
  geom_polygon(aes(fill=SP.DYN.LE00.IN),colour="white")+
  scale_fill_viridis_c()+
  theme_void()+
  labs(fill="Life Expectancy",
       title="World Heatmap",subtitle = "(Life expectancy)",
       caption="Data source: World Development Indicators")         
ggplotly(p)

