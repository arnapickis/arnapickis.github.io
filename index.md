---
title: "Latvia and Lithuania Economics by region"
author: "Arnas Barišauskas"
date: '2020-05-27'
output: html_document
---
```{r include=FALSE}
library(maps)
library(ggplot2)
library(wbstats)
library(dplyr)
library(plotly)
library(sp)
library(rmarkdown)
```
```{r echo=FALSE, message=FALSE}
gadmLV <- readRDS("C:/Users/Vartotojas/Desktop/projektas/gadm36_LVA_1_sp.rds")
LV <- fortify(gadmLV)
LV <- mutate(LV, 
             id = case_when(
               id == "1" ~ "Kurzeme",
               id == "2" ~ "Latgale",
               id == "3" ~ "Pieriga",
               id == "4" ~ "Vidzeme",
               id == "5" ~ "Zemgale"
             )
)
LV <- rename(LV, Region = id)

nedarbas_LV <-read.csv("C:/Users/Vartotojas/Desktop/projektas/LV nedarbas.csv",
              header=TRUE, sep="\t")
nedarbas_LV <-  dplyr::select(nedarbas_LV, Region, X2019)
nedarbas_LV <- rename(nedarbas_LV, nedarbo_lygis=X2019)
nedarbas_LV <- mutate(nedarbas_LV, 
                 Region = case_when(
                   Region == "Kurzeme region" ~ "Kurzeme",
                   Region == "Latgale region" ~ "Latgale",
                   Region == "Pierīga region" ~ "Pieriga",
                   Region == "Vidzeme region" ~ "Vidzeme",
                   Region == "Zemgale region" ~ "Zemgale"
                 )
)

BVP_LV <- read.csv("C:/Users/Vartotojas/Desktop/projektas/LV BVP.csv", sep=";")
BVP_LV <- rename(BVP_LV, Region=Territorial.unit, BVP_dalis=X2017.Total..share....)
BVP_LV <- mutate(BVP_LV, 
                      Region = case_when(
                        Region == "Kurzeme region" ~ "Kurzeme",
                        Region == "Latgale region" ~ "Latgale",
                        Region == "Pierīga region" ~ "Pieriga",
                        Region == "Vidzeme region" ~ "Vidzeme",
                        Region == "Zemgale region" ~ "Zemgale"
                      )
)


LV_rod <- left_join(nedarbas_LV, BVP_LV, by="Region")


LV_map <- left_join(LV, LV_rod, by="Region",copy=TRUE)



LV_map$HOVER <- with(LV_map, paste(Region, "region,<br>Unemployment rate:",
                                   nedarbo_lygis,"%<br>GDP percentage: ", BVP_dalis,"%"))

LAV<- ggplot(LV_map, aes(x = long, y = lat, group=group, text = HOVER)) +
  geom_polygon(aes(fill = nedarbo_lygis), color = "white", size=.1)+
  labs(title=" ",
       fill = "Unemployment, %")+
  scale_fill_gradient(
        low = "#FFCC66",
         high = "#FF3300",
        space = "Lab",
        na.value = "grey50",
        guide = "colourbar",
        aesthetics = "fill"
      )+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()
        )
```
<br>
<h1>Unemployment and contribution to GDP by region in Latvia, 2017</h1> 
<br>
```{r echo=FALSE, message=FALSE}
ggplotly(LAV, tooltip = "text")
 
#Lietuva
gadmLT <- readRDS("C:/Users/Vartotojas/Desktop/projektas/gadm36_LTU_1_sp.rds")

LT <- fortify(gadmLT)

LT <- mutate(LT, 
                   id = case_when(
                     id == "1" ~ "Alytus",
                     id == "2" ~ "Vilnius",
                     id == "3" ~ "Kaunas",
                     id == "4" ~ "Klaipeda",
                     id == "5" ~ "Marijampole",
                     id == "6" ~ "Panevezys",
                     id == "7" ~ "Siauliai",
                     id == "8" ~ "Taurage",
                     id == "9" ~ "Telsiai",
                     id == "10" ~ "Utena"
                   )
)
LT <- rename(LT, Apskritis = id)

nedarbasLT <- read.csv("C:/Users/Vartotojas/Desktop/projektas/LT nedarbas.csv", header=TRUE)

nedarbasLT <- nedarbasLT %>% 
  rename(nedarbo_lygis=ReikÅ.mÄ.) %>%
                select(Apskritys, nedarbo_lygis)

BVP_LT <- read.csv("C:/Users/Vartotojas/Desktop/projektas/LT BVP.csv", sep=",")

BVP_LT <- BVP_LT %>%
    rename(BVP_dalis=ReikÅ.mÄ.) %>%
      select(Apskritys, BVP_dalis)

LT_rod <- left_join(nedarbasLT, BVP_LT, by="Apskritys")

Apskritis <- c("Lietuva","Sostine","Vilnius","Vidurio","Alytus","Kaunas","Klaipeda",
           "Marijampole","Panevezys","Siauliai","Taurage","Telsiai","Utena")

LT_rod$Apskritys <- Apskritis

LT_rod <- filter(LT_rod, Apskritys != "Lietuva" & Apskritys != "Sostine" &
         Apskritys != "Vidurio")
LT_rod <- rename(LT_rod, Apskritis=Apskritys)


LT_map <- left_join(LT, LT_rod, by="Apskritis",copy=TRUE)

LT_map$HOVER <- with(LT_map, paste(Apskritis, "county,<br>Unemplyoment rate:",
                                   nedarbo_lygis, "%<br>GDP percentage: ", BVP_dalis,"%"))

LTU <- ggplot(LT_map, aes(x = long, y = lat, group=group, text=HOVER)) +
  geom_polygon(aes(fill = nedarbo_lygis ), color = "white", size=.1)+
  labs(title=" ",
       fill = "Unemployment, %")+
  scale_fill_gradient(
    low = "#FFCC66",
    high = "#FF3300",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  )+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        
  )

```
<br>
<h1>Unemployment and contribution to GDP by region in Lithuania, 2017</h1> 
<br>
```{r echo=FALSE}

ggplotly(LTU, tooltip = "text")

```
<br>
<h1>Number of business companies in largest cities (thousands)</h1> 
<br>
```{r echo=FALSE}
miestai <- read.csv("C:/Users/Vartotojas/Desktop/projektas/miestai.csv", sep=",")

miestai <- miestai %>% 
  select(CITIES, Value) %>%
    filter(CITIES!="Belgium" & CITIES!="Panevezys" & CITIES!="Alytus" &
             CITIES!="Lithuania" & CITIES!="Latvia")

miestai$salis <- rep(c("Latvia","Lithuania"), each=4)



miestai %>%
  arrange(Value) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(CITIES=factor(CITIES, levels=CITIES)) %>%   # This trick update the factor levels
  ggplot( aes(x=CITIES, y=Value, color=salis)) +
  geom_segment( aes(xend=CITIES, yend=0)) +
  geom_point( size=4, alpha=.8)+
  labs(title=" ",
       subtitle = "", color=NULL)+
  coord_flip() +
  scale_color_manual(values=c("#999999", "#E69F00"))+
  theme_bw() +
  xlab("")+
  ylab("")
```
