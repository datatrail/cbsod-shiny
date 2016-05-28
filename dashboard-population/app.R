## app.R ##
load(file = "appdata.Rdata")
library(shinydashboard)
library(highcharter)
library(dplyr)
library(reshape2)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "PEARL 2014-2040"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("regio", label = "Regio",choices = setNames(regions$Key,regions$Title)),
      menuItem("Bevolking - groei", tabName = "tab1", icon = icon("th")),
      menuItem("Bevolking - geslacht & leeftijd ", tabName = "tab2", icon = icon("th")),
      menuItem("Info", tabName = "tab3", icon = icon("th"))
    )    
  ),
  dashboardBody(
    tabItems(
      # Bevolking - groei (tab 1)
      tabItem(
        tabName = "tab1",
        fluidRow(
          column(
            width = 6,
            box(title = "Bevolkingsgroei, grafiek", highchartOutput("tab1_chart1", height = 350), status = "primary",  height = 415, width = NULL),
            box(title = "Bevolkingsgroei, tabel", dataTableOutput("tab1_table1"),status = "primary",  height = 400, width = NULL)
          ),
          column(
            width = 6,
            box(title = "Bevolkingsgroei, kaart",
                highchartOutput("tab1_map1", height = 680), 
                sliderInput("tab1_map1_year","Select year:",min = 2014, max = 2040, value = 2040, animate=animationOptions(interval=2500, loop=FALSE),width = NULL),
                status = "primary",
                width = NULL,
                height = 835
            )
          )
        )
      ),

      # Bevolking - geslacht & leeftijd (tab 2)
      tabItem(tabName = "tab2",
        fluidRow(
          column(
            width = 6,
            box(title = "Bevolking naar geslacht", status = "primary", highchartOutput("tab2_chart1", height = 345), height = 410, width = NULL),
            box(title = "Bevolking naar leeftijd",status = "primary",  highchartOutput("tab2_chart2", height = 345), height = 410, width = NULL)
          ),
          column(
            width = 6,            
            box(
              title = "Bevolkingspiramide", 
              status = "primary", 
              highchartOutput("tab2_chart3", height = 680), 
              sliderInput("activeyear","Select year:",min = 2014, max = 2040, value = 2014, animate=animationOptions(interval=500, loop=FALSE)),
              height = 840, 
              width = NULL
            )
          )
        )
      ),        
      
      # Info (tab 3)
      tabItem(tabName = "tab3",
        fluidRow(
          box(
            title = "Info", 
            a("Data: Regionale prognose 2014-2040; kerncijfers", href="http://opendata.cbs.nl/dataportaal/portal.html?_catalog=CBS&_la=nl&tableId=82220NED&_theme=426"),
            br(),
            a("Data: Regionale prognose 2014-2040; bevolking", href="http://opendata.cbs.nl/dataportaal/portal.html?_la=nl&_catalog=CBS&tableId=82172NED&_theme=426"), 
            width = 12
          )
        )
      )
    )
  )
)


server <- function(input, output) {

  output$tab1_chart1 <- renderHighchart({
    tmpdata <-  cbs82220NED.data[cbs82220NED.data$RegioSSituatie2013 == input$regio, ]
    hc <- highchart()
    hc <- hc_xAxis(hc, categories = tmpdata$Perioden_lbl)    
    hc <- hc_yAxis(hc, title = list(text = input$regios))
    hc <- hc_add_serie(hc, name = "bevolkingsgroei", data = tmpdata$bevolkingsgroei)
    hc <- hc_add_serie(hc, name = "geboorteoverschot", data = tmpdata$geboorteoverschot)
    hc <- hc_add_serie(hc, name = "vestigingsoverschot", data = tmpdata$vestigingsoverschot)
    hc <- hc_add_theme(hc, hc_theme_google())
    hc  
  })

  output$tab1_table1 <- renderDataTable({
    tmpdata <- cbs82220NED.data[cbs82220NED.data$RegioSSituatie2013 == input$regio, c('Perioden_lbl', 'bevolkingsgroei', 'geboorteoverschot', 'vestigingsoverschot')]
    colnames(tmpdata) <- c('jaar', 'bevolkingsgroei', 'geboorteoverschot', 'vestigingsoverschot')
    tmpdata$bevolkingsgroei <- format(round(tmpdata$bevolkingsgroei, 2), nsmall = 2)
    tmpdata$geboorteoverschot <- format(round(tmpdata$geboorteoverschot, 2), nsmall = 2)
    tmpdata$vestigingsoverschot <- format(round(tmpdata$vestigingsoverschot, 2), nsmall = 2)
    return(datatable(tmpdata, options = list(info = FALSE, lengthChange = FALSE, searching = FALSE, paging = FALSE, filter = TRUE, scrollY = 290)))
  })
    
  output$tab1_map1 <- renderHighchart({
    tmpdata <- cbs82220NED.data[substr(cbs82220NED.data$RegioSSituatie2013, 1, 2) == 'CR', c('TotaleBevolking_1','Perioden_lbl', 'RegioSSituatie2013')]
    tmpdata$jaar <- as.numeric(tmpdata$Perioden_lbl)
    pop.1988 <- unlist(tmpdata[tmpdata$jaar==2014, c('TotaleBevolking_1')])
    pop.yyyy <-unlist(tmpdata[tmpdata$jaar==input$tab1_map1_year, c('TotaleBevolking_1')])
    pop.growth <- as.integer(pop.yyyy/pop.1988*100) 
    data <- data.frame(REGNR = sapply(map$features, function(f) f$properties$REGNR), REGNM = sapply(map$features, function(f) f$properties$REGNM), value = pop.growth)
    highchart() %>% 
      hc_chart(backgroundColor = "#FFFFFF") %>% 
      hc_title(text = paste("Groei van de bevolking in ", input$tab1_map1_year, "t.o.v. 2014", sep = " "), align = "center") %>% 
      hc_add_series_map(map = map, df = data, name = "Index", 
                        value = "value", joinBy = c("REGNR", "REGNR"), 
                        color = "#7f7f7f",
                        tooltip = list(pointFormat = "{point.properties.REGNR}-{point.properties.REGNM}:<br>{point.value}"),
                        dataLabels = list(enabled = FALSE, pointFormat = "{point.properties.REGNR}-{point.properties.REGNM}:<br>{point.value}")
      ) %>%
      hc_colorAxis(dataClassColor = 'category', dataClasses = list(list(to=90,color="#C32117"),list(from=90,to=97.5,color="#E5B196"), list(from=97.5, to=102.5, color="#fbefcb"), list(from=102.5, to=110, color="#B2D8F6"), list(from=110, color="#009CDF")))   %>%
      hc_legend(title = list(text="Index:"), valueDecimals = 0, layout = "vertical", reversed = FALSE, floating = FALSE, align="right", verticalAlign = "bottom", itemDistance = 0, padding = 1) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_mapNavigation(enabled = FALSE)
  })
  
  output$tab2_chart1 <- renderHighchart({
    tmpdata <- cbs82220NED.data[cbs82220NED.data$RegioSSituatie2013 == input$regio,]
    max_data <- max(tmpdata$TotaleBevolking_1)
    hc <- highchart()
    hc <- hc_xAxis(hc, categories = tmpdata$Periode)    
    hc <- hc_yAxis(hc, min = 0, max = max_data, title = list(text = "Aantal (x 1000)"))
    hc <- hc_add_serie(hc, name = "Totaal", data = tmpdata$TotaleBevolking_1, color =  "#E6AD1F")
    hc <- hc_add_serie(hc, name = "Man", data = tmpdata$Mannen_5, color =  "#009CDF")
    hc <- hc_add_serie(hc, name = "Vrouw", data = tmpdata$Vrouwen_6, color =  "#BF0069")
    hc  
  })
  
  output$tab2_chart2 <- renderHighchart({
    tmpdata <- cbs82220NED.data[cbs82220NED.data$RegioSSituatie2013==input$regio,]
    max_data <- max(tmpdata$TotaleBevolking_1)
    hc <- highchart()
    hc <- hc_chart(hc, type = 'line')
    hc <- hc_xAxis(hc, categories = tmpdata$Perioden_lbl)
    hc <- hc_yAxis(hc, min = 0, max = max_data, title = list(text = "Aantal (x 1000)"))
    hc <- hc_add_serie(hc, name = "0  - 20 jaar", data = tmpdata$k_0Tot20Jaar_2, color = "#8D911F")
    hc <- hc_add_serie(hc, name = "20 - 65 jaar", data = tmpdata$k_20Tot65Jaar_3, color = "#C32117")
    hc <- hc_add_serie(hc, name = "65 jaar en ouder", data = tmpdata$k_65JaarOfOuder_4, color = "#808080")
    hc  
  })
  
  output$tab2_chart3 <- renderHighchart({
    year <- paste(input$activeyear)
    tmpdata <- subset(cbs82172NED.data, Geslacht_lbl %in% c("Mannen", "Vrouwen") & RegioSSituatie2013==input$regio) %>%     
      dcast(Perioden_lbl + Leeftijd_lbl ~ Geslacht_lbl, value.var = "TotaleBevolking_1")
    max_data <- max(tmpdata$Mannen,tmpdata$Vrouwen)
    tmpdata <- tmpdata[tmpdata$Perioden==year,]
    hc <- highchart()
    hc <- hc_chart(hc, type = 'bar')
    hc <- hc_title(hc, text = paste("Bevolking naar leeftijd en geslacht", year), align = "center")
    hc <- hc_xAxis(hc, categories = tmpdata$Leeftijd, reversed = FALSE)
    hc <- hc_yAxis(hc, labels = list(formatter =htmlwidgets::JS("function () {return Math.abs(this.value)}")), min = max_data*-1, max = max_data) #min = max_data*-1, max = max_data
    hc <- hc_add_serie(hc, name = "Man", data = tmpdata$Mannen*-1, color =  "#009CDF")
    hc <- hc_add_serie(hc, name = "Vrouw", data = tmpdata$Vrouwen, color =  "#BF0069")
    hc <- hc_plotOptions(hc, series = list(stacking = 'normal',  pointWidth = 20, pointPadding = 0, animation = FALSE))
    hc  
  })
}

shinyApp(ui, server)