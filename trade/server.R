library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(countrycode)
library(googleVis)
agg = merch.u2 %>%
  filter(.,flow == "Exports") %>%
  group_by(., country) %>%
  summarise(., total = sum(usd))
shinyServer(function(input, output) {
  react1 = reactive({
    switch(input$type,
           "USD" = starts_with("usd"),
           "Percentage of Trade" = starts_with("totalpct")
           )
  })
  react2 = reactive({
    switch(input$type,
           "USD" = starts_with("usd"),
           "Percentage of Trade" = starts_with("flowpct")
    )
  })
  react.merc = reactive({
    switch(input$merc,
           "All" = sort(unique(merch.u2$primary)),
           "Agricultural Products" = "Agricultural Products",
           "Chemicals" = "Chemicals",
           "Clothing" = "Clothing",
           "Fuels and mining products" = "Fuels and mining products",
           "Iron and steel" = "Iron and steel",
           "Machinery and transport equipment" = "Machinery and transport equipment",
           "Other manufactures" = "Other manufactures",
           "Other semi-manufactures" = "Other semi-manufactures",
           "Textiles" = "Textiles"
    )
  })
  react.flow = reactive({
    switch(input$flow,
           "All" = c("Exports","Imports"),
           "Exports" = "Exports",
           "Imports" = "Imports"
    )
  })
  reactw = reactive({
    merch.u2 %>%
      select(.,country,flow,primary,value = react1()) %>%
      filter(.,flow %in% react.flow(),primary %in% input$merchandise) %>%
      group_by(.,country) %>%
      summarise(.,total = sum(value))
  })
  output$world = renderGvis({
    gvisGeoChart(
      data = reactw(),
      locationvar = "country",
      colorvar = "total",
      options = list(
        title = "Woot!",
        height = 550,
        width = 1100
      )
    )
  })
  output$countryA = renderPlotly({
    ggplotly(
      merch.u2 %>%
        filter(., country == input$cA,primary %in% input$merchandise,flow %in% react.flow()) %>%
        select(., country, flow, primary, value = react1()) %>%
        group_by(., flow, primary) %>%
        summarise(., total = sum(value)) %>%
        ggplot(., aes(
          x = reorder(primary, total), y = total
        )) +
        geom_col(aes(fill = flow), position = "dodge") +
        coord_flip() + 
        theme(
          plot.subtitle = element_text(vjust = 1),
          plot.caption = element_text(vjust = 1),
          axis.text.x = element_text(angle = 0)
        ) + labs(fill = "Trade Flow") + labs(title = paste("A View of",input$cA,"and Their Trade of Merchandise"), x = "Primary Merchandise", y = input$type,fill = "Trade Flow"))
  })
  output$cvwA = renderPlotly({
    ggplotly(merch.u2 %>%
               filter(.,country == input$cA,primary %in% input$merchandise,flow %in% react.flow()) %>%
               rbind(.,filter(WORLD,primary %in% input$merchandise,flow %in% react.flow())) %>%
               select(.,country, flow, primary, value = react1()) %>%
               group_by(.,country,flow, primary) %>%
               summarise(., total = sum(value)) %>%
               ggplot(.,aes(x = country, y = total)) + geom_col(aes(fill = flow),position = "dodge") +
               facet_wrap(. ~ primary) +
               theme(plot.subtitle = element_text(vjust = 1), 
                     plot.caption = element_text(vjust = 1), 
                     axis.text.x = element_text(angle = 0)) +labs(title = paste("Comparison of",input$cA,"Vs. World Median"), 
                                                                  x = "Primary Merchandise", y = input$type, 
                                                                  fill = "Location"),height = 600)
  })
  output$countryB = renderPlotly({
    ggplotly(
      merch.u2 %>%
        filter(., country == input$cB,primary %in% input$merchandise,flow %in% react.flow()) %>%
        select(., country, flow, primary, value = react1()) %>%
        group_by(., flow, primary) %>%
        summarise(., total = sum(value)) %>%
        ggplot(., aes(
          x = reorder(primary, total), y = total
        )) +
        geom_col(aes(fill = flow), position = "dodge") +
        coord_flip() + 
        theme(
          plot.subtitle = element_text(vjust = 1),
          plot.caption = element_text(vjust = 1),
          axis.text.x = element_text(angle = 0)
        ) + labs(fill = "Trade Flow") + labs(title = paste("A View of",input$cB,"and Their Trade of Merchandise"), x = "Primary Merchandise", y = input$type,fill = "Trade Flow"))
  })
  output$cvwB = renderPlotly({
    ggplotly(merch.u2 %>%
               filter(.,country == input$cB,primary %in% input$merchandise,flow %in% react.flow()) %>%
               rbind(.,filter(WORLD,primary %in% input$merchandise,flow %in% react.flow())) %>%
               select(.,country, flow, primary, value = react1()) %>%
               group_by(.,country,flow, primary) %>%
               summarise(., total = sum(value)) %>%
               ggplot(.,aes(x = country, y = total)) + geom_col(aes(fill = flow),position = "dodge") +
               facet_wrap(. ~ primary) +
               theme(plot.subtitle = element_text(vjust = 1), 
                     plot.caption = element_text(vjust = 1), 
                     axis.text.x = element_text(angle = 0)) +labs(title = paste("Comparison of",input$cB,"Vs. World Median"), 
                                                                    x = "Primary Merchandise", y = input$type, 
                                                                    fill = "Location"),height = 600)
  })
  output$compare1 = renderPlotly({
    ggplotly(merch.u2 %>%
               filter(.,country == input$cA | country == input$cB,primary %in% input$merchandise,flow %in% react.flow()) %>%
               select(.,country,flow,primary,value = react1()) %>%
               group_by(.,country,flow) %>%
               summarise(.,total = sum(value)) %>%
               ggplot(.,aes(x = country, y = total)) +
               geom_col(aes(fill = flow),position = "dodge") +
               theme(plot.subtitle = element_text(vjust = 1), 
                     plot.caption = element_text(vjust = 1)) +labs(title = paste("Aggregated Comparison of",input$cA,"vs.",input$cB), x = "Country", y = input$type, 
                                                                   fill = "Trade Flow"))
  }) 
  output$compare2 = renderPlotly({ggplotly(merch.u2 %>%
    filter(.,country == input$cA | country == input$cB,primary %in% input$merchandise,flow %in% react.flow()) %>%
    select(.,country,flow,primary,value = react1()) %>%
    group_by(.,country,flow,primary) %>%
    summarise(.,total = sum(value)) %>%
    ggplot(.,aes(x = country, y = total)) +
    geom_col(aes(fill = flow),position = "dodge") +
    facet_wrap(. ~ primary)+ theme(plot.subtitle = element_text(vjust = 1), 
                                   plot.caption = element_text(vjust = 1)) +labs(title = paste("Comparison of",input$cA,"to",input$cB,"by Merchandise Type"), x = "Country", y = input$type, 
                                                                                 fill = "Trade Flow"),height = 600)
    })
})
