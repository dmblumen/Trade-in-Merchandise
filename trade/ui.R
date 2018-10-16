library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(countrycode)
library(googleVis)

dashboardPage(
  dashboardHeader(title = "WTO Merchandise"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("World Map",
               tabName = "worldmap",
               icon = icon("map")),
      menuItem("Country A",
               tabName = "countryA",
               icon = icon("bar-chart")),
      menuItem("Country B",
               tabName = "countryB",
               icon = icon("bar-chart")),
      menuItem("Comparisons",
               tabName = "compare",
               icon = icon("balance-scale")
      )
    ),
    selectInput("type",
                label = "USD or Percentage of Trade",
                choices = c("Percentage of Trade", "USD")
    ),
    selectInput("flow",
                label = "Flow of Trade",
                choices = c("Exports", "Imports","All")
    ),
    selectInput("cA",
                label = "Country A",
                choices = unique(merch.u2$country)),
    selectInput("cB",
                label = "Country B",
                choices = unique(merch.u2$country)),
    checkboxGroupInput(
      "merchandise",
      "Merchandise Categories",
      choices = c("Agricultural Products" = "Agricultural Products",
                  "Chemicals" = "Chemicals",
                  "Clothing" = "Clothing",
                  "Fuels and mining products" = "Fuels and mining products",
                  "Iron and steel" = "Iron and steel",
                  "Machinery and transport equipment" = "Machinery and transport equipment",
                  "Other manufactures" = "Other manufactures",
                  "Other semi-manufactures" = "Other semi-manufactures",
                  "Textiles" = "Textiles"),
      selected = sort(unique(merch.u2$primary))
    )
  ),
  dashboardBody(tabItems(
    tabItem(
      "worldmap",
      box(htmlOutput("world"), width = 12, title = "World Map, Trade in Merchandise")
    ),
    tabItem("countryA",
            fluidRow(box(
              plotlyOutput("countryA"), width = 12, height = 450
            )),
            fluidRow(box(
              plotlyOutput("cvwA"), width = 12, height = 650
            ))),
    tabItem("countryB",
            fluidRow(box(
              plotlyOutput("countryB"), width = 12, height = 450
            )),
            fluidRow(box(
              plotlyOutput("cvwB"), width = 12, height = 650
            ))),
    tabItem("compare",fluidRow(box(plotlyOutput("compare1"),width = 12, height = 450)),
            fluidRow(box(plotlyOutput("compare2"),width = 12,height = 650)))
  ))
)