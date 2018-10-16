library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(countrycode)
library(googleVis)
merch.u = read.csv("merchu.csv",header = T, stringsAsFactors = T)
merch.u1 = merch.u %>%
  tbl_df(.) %>%
  select(.,country.code,cc3,country,flow,primary,value) %>%
  mutate(.,value = value *1000000) %>%
  group_by(.,country.code,cc3,country,flow,primary) %>%
  summarise(.,usd = sum(value)) %>%
  arrange(.,country) %>%
  ungroup(.) %>%
  group_by(.,country.code,cc3,country) %>%
  mutate(.,totalpct = usd/sum(usd)*100) %>%
  group_by(.,country.code,cc3,country,flow) %>%
  mutate(.,flowpct = usd/sum(usd)*100) %>%
  ungroup(.)

merch.u2 = merch.u1 %>%
  select(.,country,flow,primary,usd,totalpct,flowpct)

WORLD = merch.u1 %>%
  group_by(.,flow,primary) %>%
  summarise(.,usd = median(usd),totalpct = median(totalpct),flowpct = median(flowpct)) %>%
  mutate(.,country = "World") %>%
  select(.,country,flow,primary,usd,totalpct,flowpct) %>%
  arrange(.,country,flow,primary) %>%
  ungroup(.)
