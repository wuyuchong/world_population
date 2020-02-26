## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(magrittr)
library(knitr)
library(leaflet)


## ----------------------------------------------------------------------------------------------------------------------
dat = read.csv("Population+Surface+Area+and+Density.csv", header = TRUE)
dat = dat[900:nrow(dat),]
dat = dat %>% 
  select(X, Year, Series, Value) 
coordinate = read.csv("coordinate.csv", header = TRUE)
# Definition
CAGR_formula = function(FV, PV, yrs) 
{
  values = ((FV/PV)^(1/yrs)-1)
  return(values)
}


## ----------------------------------------------------------------------------------------------------------------------
dat_2019 = dat %>% 
  filter(Year == 2019, Series == 	"Population density") %>% 
  arrange(Value)

name_under = dat_2019$X[1:23] %>% 
  as.vector()
name_over = dat_2019$X[nrow(dat_2019):(nrow(dat_2019) - 22)] %>% 
  as.vector()

dat_density = dat %>% 
  filter(Series == "Population density")


## ----------------------------------------------------------------------------------------------------------------------
dat_under = NULL
for(i in name_under)
{
  table = filter(dat_density, X == i, Year >= 2009)
  CAGR = CAGR_formula(table$Value[nrow(table)], table$Value[1], table$Year[nrow(table)] - table$Year[1]) * 100
  density = table$Value[nrow(table)]
  note = paste("人口密度为：", round(density, 2), "，年复合增长率为：", round(CAGR, 2), "%。")
  row = data.frame(country = i, density = density, CAGR = CAGR, note = note)
  dat_under = rbind(dat_under, row)
}

dat_over = NULL
for(i in name_over)
{
  table = filter(dat_density, X == i, Year >= 2009)
  CAGR = CAGR_formula(table$Value[nrow(table)], table$Value[1], table$Year[nrow(table)] - table$Year[1]) * 100
  density = table$Value[nrow(table)]
  note = paste("人口密度为：", round(density, 2), "，年复合增长率为：", round(CAGR, 2), "%。")
  row = data.frame(country = i, density = density, CAGR = CAGR, note = note)
  dat_over = rbind(dat_over, row)
}


## ----warning=FALSE-----------------------------------------------------------------------------------------------------
dat_over = left_join(dat_over, coordinate, by = "country")
color = NULL
for(i in 1:nrow(dat_over))
{
  if(dat_over$CAGR[i] <= 0)
  {
    color = c(color, "blue")
  } else
  {
    color = c(color, "red")
  }
}
dat_over = cbind(dat_over, color)


## ----fig.cap="悬浮查看国家名，点击查看密度和年复合增长率（蓝色为负增长）"----------------------------------------------
m = dat_over %>% 
  filter(!is.na(latitude)) %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, popup = ~as.character(note), label = ~as.character(country), color = ~color, radius = ~density/1000)
m


## ----fig.cap="世界人口密度90%分位点以上的国家（颜色为年复合增长率）"---------------------------------------------------
dat_over %>% 
  ggplot() +
  scale_fill_gradient(low="white", high="red") +
  geom_col(aes(x = reorder(country, density), y = density, fill = CAGR)) +
  theme_minimal() +
  coord_flip() +
  labs(x = "Country", y = "Density", fill = "growth rate (%)")


## ----warning=FALSE-----------------------------------------------------------------------------------------------------
dat_under = left_join(dat_under, coordinate, by = "country")
color = NULL
for(i in 1:nrow(dat_under))
{
  if(dat_under$CAGR[i] <= 0)
  {
    color = c(color, "blue")
  } else
  {
    color = c(color, "red")
  }
}
dat_under = cbind(dat_under, color)


## ----fig.cap="悬浮查看国家名，点击查看密度和年复合增长率（蓝色为负增长）"----------------------------------------------
m = dat_under %>% 
  filter(!is.na(latitude)) %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, popup = ~as.character(note), label = ~as.character(country), color = ~color, radius = ~density/2)
m


## ----fig.cap="世界人口密度10%分位点以下的国家（颜色为年复合增长率）"---------------------------------------------------
dat_under %>% 
  ggplot() +
  scale_fill_gradient(low="white", high="red") +
  geom_col(aes(x = reorder(country, -density), y = density, fill = CAGR)) +
  theme_minimal() +
  coord_flip() +
  labs(x = "Country", y = "Density", fill = "growth rate (%)")


## ----------------------------------------------------------------------------------------------------------------------
dat_2019 = dat %>% 
  filter(Year == 2019, Series == "Population mid-year estimates (millions)") %>% 
  arrange(Value)

name_under = dat_2019$X[1:23] %>% 
  as.vector()
name_over = dat_2019$X[nrow(dat_2019):(nrow(dat_2019) - 22)] %>% 
  as.vector()

dat_population = dat %>% 
  filter(Series == "Population mid-year estimates (millions)")


## ----------------------------------------------------------------------------------------------------------------------
dat_under = NULL
for(i in name_under)
{
  table = filter(dat_population, X == i, Year >= 2009)
  CAGR = CAGR_formula(table$Value[nrow(table)], table$Value[1], table$Year[nrow(table)] - table$Year[1])
  row = data.frame(country = i, population = table$Value[nrow(table)], CAGR = CAGR * 100)
  dat_under = rbind(dat_under, row)
}

dat_over = NULL
for(i in name_over)
{
  table = filter(dat_population, X == i, Year >= 2009)
  CAGR = CAGR_formula(table$Value[nrow(table)], table$Value[1], table$Year[nrow(table)] - table$Year[1])
  row = data.frame(country = i, population = table$Value[nrow(table)], CAGR = CAGR * 100)
  dat_over = rbind(dat_over, row)
}


## ----------------------------------------------------------------------------------------------------------------------
names(dat_over) = c("国家(地区)", "人口(2019)", "年复合增长率(%)")
dat_over %>% 
  kable(caption = "2019年人口较多的国家人口规模情况", digits = 2)


## ----------------------------------------------------------------------------------------------------------------------
names(dat_under) = c("国家(地区)", "人口(2019)", "年复合增长率(%)")
dat_under %>% 
  kable(caption = "2019年人口较少的国家人口规模情况", digits = 2)

