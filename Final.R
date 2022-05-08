library(shinydashboard)
library(fpp3)
library(readr)
library(shinyWidgets)
library(shiny)
library(ggplot2)
library(ggeasy)
library(plotly)
library(tidyverse)
library(patchwork)
library(forecast)
library(tidyr)
library(lubridate)
library(zoo)

# shinyWidgetsGallery()


ARRIVE <- aus_arrivals

##########################################################################
#         UI
##########################################################################

#Side Bar Tabs
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Introduction", tabName = "introduction"),
  menuItem("Time Series", tabName = "timeseries"),
  menuItem("User Selection", tabName = "userselection"),
  menuItem("Simple Models", tabName = "simplemodels"),
  menuItem("Exponential Smoothing Models", tabName = "es"),
  menuItem("Manual ARIMA Model", tabName = "MANUALarima"),
  menuItem("Auto ARIMA Model", tabName = "AUTOarima")
))



#####################################
# UI Instructions Tab
#####################################
body <- dashboardBody(tabItems(
  tabItem(
    tabName = "introduction",
    div(
      img(src = "https://haslam.utk.edu/wp-content/uploads/2021/09/logosArtboard-31.jpg", width =
            300),
      style = "text-align: center;"
    ),
    h2(strong("Creator:"), "Campbell Jessop", align = "center"),
    br(),
    tabBox(
      title = "Information Tab",
      side = "left",
      selected = "Purpose",
      id = "introtabset",
      height = NULL,
      width = NULL,
      tabPanel(
        "Purpose",
        h4(strong("Purpose:"), align = "center"),
        h4(
          "This app was designed to allow users to analyze and forecast trends for quarterly international arrivals to Australia from Japan, New Zealand, United Kingdom, and the United States between 1981 and 2012."
        )
      ),
      tabPanel(
        "User Guide",
        h4(strong("Time Series Sub Menu:"), align = "center"),
        h4(
          "This sub menu allows the users to navigate through two seperate tab boxes. The top box has four different tabs displaying the full series, arrivals from Japan, arrivals from New Zealand, arrivals from the United Kingdom, and arrivals from the United States. The box below the graph tab box has four different tabs displaying the summary text output for the full series, Japan, New Zealand, United Kingdom, and the United States."
        ),
        hr(),
        h4(strong("User Selection Sub Menu:"), align =
             "center"),
        h4(
          "The user selection menu allows users to interact with three inputs to display the origin, graph, and date range of their choosing. Users have the option to select from any of the four origins (Japan, New Zealand, United Kingdom, and the United States). Also, the user can select any of the six different options of graphs to be displayed for their specified origin. The date range selection box is also available if the user would like to specify a particular date range when viewing the regular time series graphs. Once the origin, specific time series graph, and date range have been selected, the graph will populate above along with an interpretation to help the user understand noteworthy aspects."
        ),
        hr(),
        h4(strong("Simple Models Sub Menu:"), align =
             "center"),
        h4(
          "The simple models sub menu contains four different simple forecasting models to forecast the time series trends within the dataset. These four models include the naive, mean, and drift models. Users also have the option to specify which origin they would like to see the forecasts for (either from Japan, New Zealand, United Kingdom, or the United States)."
        ),
        hr(),
        h4(strong("Exponential Smoothing Models Sub Menu:"), align =
             "center"),
        h4(
          "The exponential smoothing models sub menu contains two different exponential smoothing models to forecast the time series trends within the dataset. The two models are the Holt's and Holt's/Winter's. Users also have the option to specify which origin they would like to see the forecasts for (either from Japan, New Zealand, United Kingdom, or the United States)."
        ),
        hr(),
        h4(strong("Manual ARIMA Model Sub Menu:"), align =
             "center"),
        h4(
          "The manual ARIMA model sub menu contains multiple tuning parameters and user selection options needed to produce the desired ARIMA model. Users can select origin, type of ARIMA (seasonal or non-seasonal), and the tuning parameters for each respective type of ARIMA. Users can also tune on p (order of the autoregressive part), d (degree of first differencing involved), or q (order of the moving average part) for both seasonal and non-seasonal components of the ARIMA model."
        ),
        hr(),
        h4(strong("Auto ARIMA Model Sub Menu:"), align =
             "center"),
        h4(
          "The auto ARIMA model sub menu uses a function that automatically finds the ARIMA parameters that produce the best-fitting forecast. Users have the option to specify which origin they would like to see the forecasts for (either from Japan, New Zealand, United Kingdom, or the United States)."
        )
      ),
      
      
      tabPanel(
        "Features",
        h4(strong("Time Series Sub Menu:"), align = "center"),
        h4(
          strong("Graph Tab Box:"),
          "The graph tab box allows users to select between five different tabs showing graphs of the complete time series. The first tab shows a graph of the complete time series of the arrival from all four origins at once while the other four tabs shows graphs of each individual origin."
        ),
        br(),
        h4(
          strong("Summary Tab Box:"),
          em("(Additional Feature) "),
          "The summary tab box allows users to select between five different tabs showing summary text output of the complete time series. The first tab shows the total arrivals from each of the four origins. The other four tabs show the fewest and most arrivals from each respective origin as well as the year/quarter each occurred."
        ),
        hr(),
        h4(strong("User Selection Sub Menu:"), align =
             "center"),
        h4(
          strong("Origin Selection Box:"),
          "The origin selection box lets the user choose which origin they would like to analyze."
        ),
        br(),
        h4(
          strong("Graph Selection Box:"),
          "The origin selection box lets the user choose which origin they would like to analyze."
        ),
        br(),
        h4(
          strong("Date Range Selection Box:"),
          em("(Additional Feature) "),
          "The date range selection box lets the user specify which specific date range they would like to analyze when viewing a regular time series graph."
        ),
        br(),
        h4(
          strong("Regular Time Series Selection:"),
          "This selection will display the full time series for the user's specified origin."
        ),
        br(),
        h4(
          strong("Seasonality Selection:"),
          "This selection will display a seasonality graph over the four quarters for the user's specified origin."
        ),
        br(),
        h4(
          strong("Autocorrelation Selection:"),
          "This selection will display an ACF bar chart for the user's specified origin. This bar chart shows how the present value of a given time series is correlated with the past."
        ),
        br(),
        h4(
          strong("Differenced Autocorrelation Selection:"),
          em("(Additional Feature) "),
          "This selection will display a differenced ACF bar chart for the user's specified origin. This is different from the autocorrelation selection since the data is now de-trended."
        ),
        br(),
        h4(
          strong("Additive Decomposition Selection:"),
          "This selection will display an additive decomposition graph for the user's selected origin."
        ),
        br(),
        h4(
          strong("Multiplicative Decomposition Selection:"),
          em("(Additional Feature) "),
          "This selection will display an multiplicative decomposition graph for the user's selected origin."
        ),
        br(),
        h4(
          strong("Interpretation Summary Box:"),
          "This text output box will display an interpretation for each respective graph after the user has specified the origin and graph type."
        ),
        hr(),
        h4(strong("Simple Models Sub Menu:"), align =
             "center"),
        h4(
          strong("Origin Selection Box:"),
          "The origin selection box lets the user choose which origin they would like to analyze."
        ),
        br(),
        h4(
          strong("Naive forecast model:"),
          "All forecasts are set to the value of the last observation. "
        ),
        br(),
        h4(
          strong("Seasonal naive forecast model:"),
          "Each forecast is equal to the last observed value from that season."
        ),
        br(),
        h4(
          strong("Mean forecast model:"),
          "Each forecast is set to the mean of the historical data."
        ),
        br(),
        h4(
          strong("Drift forecast model:"),
          "The amount of change in the forecasts is set to the average change seen in the historical data."
        ),
        hr(),
        h4(strong("Exponential Smoothing Models Sub Menu:"), align =
             "center"),
        h4(
          strong("Origin Selection Box:"),
          "The origin selection box lets the user choose which origin they would like to analyze."
        ),
        br(),
        h4(
          strong("Holt's forecast model:"),
          "An exponential smoothing model with a trend component."
        ),
        br(),
        h4(
          strong("Holt's/Winter's forecast model:"),
          "An exponential smoothing model with a trend component and a seasonal component."
        ),
        hr(),
        h4(strong("Manual ARIMA Model Sub Menu:"), align =
             "center"),
        h4(
          strong("Origin Selection Box:"),
          "The origin selection box lets the user choose which origin they would like to analyze."
        ),
        br(),
        h4(
          strong("Selected Parameters Text Box:"),
          "This text box displays the user's selected model (either seasonal or non-seasonal)."
        ),
        br(),
        h4(
          strong("Model Type Selection Box:"),
          "The model type selection box allows users to choose either a seasonal or non-seasonal ARIMA model."
        ),
        br(),
        h4(
          strong("Tuning Grid #1 (Non-Seasonal Parameters) Box:"),
          "This tuning box allows users to tune the non-seasonal p, d, and q parameters."
        ),
        br(),
        h4(
          strong("Tuning Grid #2 (Seasonal Parameters) Box:"),
          "This tuning box allows users to tune the seasonal p, d, and q parameters."
        ),
        hr(),
        h4(strong("Auto ARIMA Model Sub Menu:"), align =
             "center"),
        h4(
          strong("Origin Selection Box:"),
          "The origin selection box lets the user choose which origin they would like to analyze."
        ),
        br(),
        h4(
          strong("Selected Parameters Text Box:"),
          "This text box displays the parameters selected by the auto-ARIMA function."
        )
      )
    )
  ),
  
  
  #####################################
  # UI Time Series Tab
  #####################################
  tabItem(tabName = "timeseries",
          fluidRow(
            tabBox(
              title = "Graph",
              side = "left",
              selected = "Full Series",
              id = "tabset1",
              height = NULL,
              width = NULL,
              tabPanel("Full Series",
                       plotOutput("fullSeries")),
              tabPanel("Japan",
                       plotOutput("JapanSeries")),
              tabPanel("NZ",
                       plotOutput("NZSeries")),
              tabPanel("UK",
                       plotOutput("UKSeries")),
              tabPanel("US",
                       plotOutput("USSeries"))
            ),
            tabBox(
              title = "Summary",
              side = "left",
              selected = "Full Series",
              id = "tabset2",
              height = NULL,
              width = NULL,
              tabPanel("Full Series",
                       verbatimTextOutput("FullSummary")),
              tabPanel("Japan",
                       verbatimTextOutput("JapanSummary")),
              tabPanel("NZ",
                       verbatimTextOutput("NZSummary")),
              tabPanel("UK",
                       verbatimTextOutput("UKSummary")),
              tabPanel("US",
                       verbatimTextOutput("USSummary"))
            )
          )),
  
  
  #####################################
  # UI User Selection Tab
  #####################################
  tabItem(tabName = "userselection",
          fluidRow(
            box(
              status = "warning",
              solidHeader = FALSE,
              collapsible = TRUE,
              height = NULL,
              width = NULL,
              plotOutput("userselect")
            ),
            box(
              title = "Origin",
              status = "info",
              solidHeader = TRUE,
              width = 2,
              awesomeRadio(
                inputId = "originchoice",
                label = "Pick an origin:",
                choices = c("Japan", "NZ", "UK", "US"),
                selected = "Japan"
              )
            ),
            box(
              title = "Plot Choice",
              status = "danger",
              solidHeader = TRUE,
              width = 3,
              awesomeRadio(
                inputId = "plotchoice",
                label = "Select which plot to view:",
                choices = c(
                  "Regular Time Series",
                  "Seasonality",
                  "Autocorrelation",
                  "Differenced Autocorrelation",
                  "Additive Decomposition",
                  "Multiplicative Decomposition"
                ),
                selected = "Regular Time Series"
              )
            ),
            box(
              title = "Interpretation:",
              status = "success",
              solidHeader = TRUE,
              width = 7,
              height = NULL,
              htmlOutput("uitext")
            ),
            box(
              title = "Date Range (Applies to regular time series plots)",
              status = "primary",
              solidHeader = TRUE,
              width = 7,
              height = NULL,
              sliderTextInput(
                inputId = "Id096",
                label = "Choose a range:",
                choices = 1981:2012,
                selected = c(1981, 2012)
              )
            )
          )),
  
  #####################################
  # Simple Models Tab
  #####################################
  tabItem(tabName = "simplemodels",
          fluidRow(
            tabBox(
              title = "Simple Models",
              side = "left",
              selected = "Naive",
              id = "tabset2",
              height = NULL,
              width = NULL,
              tabPanel("Naive",
                       plotOutput("naive")),
              tabPanel("Seasonal Naive",
                       plotOutput("seasonalnaive")),
              tabPanel("Mean",
                       plotOutput("mean")),
              tabPanel("Drift",
                       plotOutput("drift"))
            ),
            box(
              title = "Origin",
              status = "primary",
              solidHeader = TRUE,
              width = 2,
              awesomeRadio(
                inputId = "originchoice2",
                label = "Pick an origin:",
                choices = c("Japan", "NZ", "UK", "US"),
                selected = "Japan"
              )
            )
          )),
  #####################################
  # Exponential Smoothing Models Tab
  #####################################
  tabItem(tabName = "es",
          fluidRow(
            tabBox(
              title = "Exponential Smoothing Models",
              side = "left",
              selected = "Holt's",
              id = "tabset3",
              height = NULL,
              width = NULL,
              tabPanel("Holt's",
                       plotOutput("holt")),
              tabPanel("Holt's/Winter's",
                       plotOutput("holtwinter"))
            ),
            box(
              title = "Origin",
              status = "primary",
              solidHeader = TRUE,
              width = 2,
              awesomeRadio(
                inputId = "originchoice3",
                label = "Pick an origin:",
                choices = c("Japan", "NZ", "UK", "US"),
                selected = "Japan"
              )
            )
          )),
  #####################################
  # Manual ARIMA Tab
  #####################################
  tabItem(tabName = "MANUALarima",
          fluidRow(
            box(
              title = "Selected Parameters:",
              status = "success",
              solidHeader = TRUE,
              width = NULL,
              height = NULL,
              htmlOutput("manualarimatext")
            ),
            box(
              title = "Manual ARIMA",
              side = "left",
              height = NULL,
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              plotOutput("manualarima")
            ),
            box(
              title = "Instruction:",
              status = "info",
              solidHeader = TRUE,
              width = 7,
              height = NULL,
              h4("If seasonal is selected in the 'Type' box, complete both tuning grids. If non-seasonal is selected, only complete tuning grid #1")
            ),
            box(
              title = "Origin",
              status = "danger",
              solidHeader = TRUE,
              width = 2,
              awesomeRadio(
                inputId = "originchoice4",
                status="primary",
                label = "Pick an origin:",
                choices = c("Japan", "NZ", "UK", "US"),
                selected = "Japan"
              )
            ),
            box(
              title = "Type",
              status = "primary",
              solidHeader = TRUE,
              width = 3,
              awesomeRadio(
                inputId = "typechoice",
                label = "Seasonal or Non-Seasonal:",
                choices = c("Seasonal", "Non-Seasonal"),
                selected = "Non-Seasonal"
              )
            ),
            box(
              title = "Tuning Grid #1 (Non-Seasonal Parameters)",
              status = "warning",
              solidHeader = TRUE,
              width = 5,
              height = NULL,
              sliderTextInput(
                inputId = "pchoice",
                label = "Select p value:", 
                choices = c(0,1),
                grid = TRUE
              ),
              sliderTextInput(
                inputId = "dchoice",
                label = "Select d value:", 
                choices = c(0,1),
                grid = TRUE
              ),
              sliderTextInput(
                inputId = "qchoice",
                label = "Select q value:", 
                choices = c(0,1),
                grid = TRUE
              ),
            ),
            box(
              title = "Tuning Grid #2 (Seasonal Parameters)",
              status = "warning",
              solidHeader = TRUE,
              width = 5,
              height = NULL,
              sliderTextInput(
                inputId = "pchoice2",
                label = "Select p value:", 
                choices = c(0,1),
                grid = TRUE
              ),
              sliderTextInput(
                inputId = "dchoice2",
                label = "Select d value", 
                choices = c(0,1),
                grid = TRUE
              ),
              sliderTextInput(
                inputId = "qchoice2",
                label = "Select q value", 
                choices = c(0,1),
                grid = TRUE
              )
            )
          )),
  #####################################
  # Auto ARIMA Tab
  #####################################
  tabItem(tabName = "AUTOarima",
          fluidRow(
            box(
              title = "Auto ARIMA",
              side = "left",
              height = NULL,
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              plotOutput("autoarima")
            ),
            box(
              title = "Origin",
              status = "primary",
              solidHeader = TRUE,
              width = 2,
              awesomeRadio(
                inputId = "originchoice5",
                label = "Pick an origin:",
                choices = c("Japan", "NZ", "UK", "US"),
                selected = "Japan"
              )
            ),
            box(
              title = "Warning:",
              status = "danger",
              solidHeader = TRUE,
              width = 7,
              height = NULL,
              h4("Auto ARIMA model may take up to 60 seconds to load after origin is selected.")
            ),
            box(
              title = "Auto Selected Parameters:",
              status = "warning",
              solidHeader = TRUE,
              width = 7,
              height = NULL,
              htmlOutput("autoarimatext")
            )
          ))
))

# Put them together into a dashboardPage
ui <- dashboardPage(dashboardHeader(),
                    sidebar,
                    body, skin = "yellow")


##########################################################################
#         Server
##########################################################################
server <- function(input, output) {
  ########################
  #         Graph Tab Box
  ########################
  output$fullSeries <- renderPlot({
    # draw the time plot with selected symbols and y var
    ARRIVE %>%
      ggplot(aes(Quarter, Arrivals, color = Origin)) +
      geom_line() +
      theme_bw(base_size = 20)
  })
  output$JapanSeries <- renderPlot({
    # draw the time plot with selected symbols and y var
    ARRIVE %>%
      filter(Origin %in% c("Japan")) %>%
      ggplot(aes(x = Quarter, y = Arrivals, color = "red")) +
      geom_line(show.legend = FALSE) +
      labs(title = "Australian arrivals from Japan", x = "Quarter", y = "Arrivals") +
      theme_bw(base_size = 20) +
      easy_center_title()
  })
  output$NZSeries <- renderPlot({
    # draw the time plot with selected symbols and y var
    ARRIVE %>%
      filter(Origin %in% c("NZ")) %>%
      ggplot(aes(x = Quarter, y = Arrivals)) +
      geom_line(color = "darkgreen", show.legend = FALSE) +
      labs(title = "Australian arrivals from NZ", x = "Quarter", y = "Arrivals") +
      theme_bw(base_size = 20) +
      easy_center_title()
  })
  output$UKSeries <- renderPlot({
    # draw the time plot with selected symbols and y var
    ARRIVE %>%
      filter(Origin %in% c("UK")) %>%
      ggplot(aes(x = Quarter, y = Arrivals)) +
      geom_line(color = "blue", show.legend = FALSE) +
      labs(title = "Australian arrivals from the UK", x = "Quarter", y = "Arrivals") +
      theme_bw(base_size = 20) +
      easy_center_title()
  })
  output$USSeries <- renderPlot({
    # draw the time plot with selected symbols and y var
    ARRIVE %>%
      filter(Origin %in% c("US")) %>%
      ggplot(aes(x = Quarter, y = Arrivals)) +
      geom_line(color = "purple", show.legend = FALSE) +
      labs(title = "Australian arrivals from the US", x = "Quarter", y = "Arrivals") +
      theme_bw(base_size = 20) +
      easy_center_title()
  })
  
  ########################
  #         Text Tab Box
  ########################
  output$FullSummary <- renderPrint({
    JapanSUB <- subset(ARRIVE, Origin == "Japan")
    NZSUB <- subset(ARRIVE, Origin == "NZ")
    UKSUB <- subset(ARRIVE, Origin == "UK")
    USSUB <- subset(ARRIVE, Origin == "US")
    cat(
      "Total Japanese arrivals: ",
      sum(JapanSUB$Arrivals),
      "\nTotal NZ arrivals: ",
      sum(NZSUB$Arrivals),
      "\nTotal UK arrivals: ",
      sum(UKSUB$Arrivals),
      "\nTotal US arrivals: ",
      sum(USSUB$Arrivals)
    )
  })
  output$JapanSummary <- renderPrint({
    JapanSUB <- subset(ARRIVE, Origin == "Japan")
    cat(
      "Fewest arrivals from Japan: ",
      min(JapanSUB$Arrivals),
      '\tYear/Quarter of occurrence: ',
      paste(JapanSUB$Quarter[which(JapanSUB$Arrivals == min(JapanSUB$Arrivals))])
    )
    cat(
      "\nMost arrivals from Japan: ",
      max(JapanSUB$Arrivals),
      '\tYear/Quarter of occurrence: ',
      paste(JapanSUB$Quarter[which(JapanSUB$Arrivals == max(JapanSUB$Arrivals))])
    )
  })
  output$NZSummary <- renderPrint({
    NZSUB <- subset(ARRIVE, Origin == "NZ")
    cat(
      "Fewest arrivals from NZ: ",
      min(NZSUB$Arrivals),
      '\tYear/Quarter of occurrence: ',
      paste(NZSUB$Quarter[which(NZSUB$Arrivals == min(NZSUB$Arrivals))])
    )
    cat(
      "\nMost arrivals from NZ: ",
      max(NZSUB$Arrivals),
      '\t\tYear/Quarter of occurrence: ',
      paste(NZSUB$Quarter[which(NZSUB$Arrivals == max(NZSUB$Arrivals))])
    )
  })
  output$UKSummary <- renderPrint({
    UKSUB <- subset(ARRIVE, Origin == "UK")
    cat(
      "Fewest arrivals from UK: ",
      min(UKSUB$Arrivals),
      '\tYear/Quarter of occurrence: ',
      paste(UKSUB$Quarter[which(UKSUB$Arrivals == min(UKSUB$Arrivals))])
    )
    cat(
      "\nMost arrivals from UK: ",
      max(UKSUB$Arrivals),
      '\t\tYear/Quarter of occurrence: ',
      paste(UKSUB$Quarter[which(UKSUB$Arrivals == max(UKSUB$Arrivals))])
    )
  })
  output$USSummary <- renderPrint({
    USSUB <- subset(ARRIVE, Origin == "US")
    cat(
      "Fewest arrivals from US: ",
      min(USSUB$Arrivals),
      '\tYear/Quarter of occurrence: ',
      paste(USSUB$Quarter[which(USSUB$Arrivals == min(USSUB$Arrivals))])
    )
    cat(
      "\nMost arrivals from US: ",
      max(USSUB$Arrivals),
      '\t\tYear/Quarter of occurrence: ',
      paste(USSUB$Quarter[which(USSUB$Arrivals == max(USSUB$Arrivals))])
    )
  })
  
  ########################
  #    User Selection Tab
  ########################
  output$userselect <- renderPlot({
    if (input$plotchoice == "Regular Time Series") {
      selectedOrigin <- input$originchoice
      ARRIVE %>%
        filter(Origin %in% selectedOrigin) %>%
        filter(Quarter >= yearquarter(as.character(input$Id096[1]))) %>%
        filter(Quarter <= yearquarter(as.character(input$Id096[2]))) %>%
        ggplot(aes(Quarter, Arrivals)) +
        geom_line(color = "darkgreen") +
        labs(title = "Regular Time Series Plot of Arrivals",
             y = "Arrivals",
             x = "Quarter") +
        theme_bw(base_size = 20) +
        easy_center_title()
    }
    
    else if (input$plotchoice == "Seasonality") {
      selectedOrigin <- input$originchoice
      ARRIVE %>%
        filter(Origin %in% selectedOrigin) %>%
        gg_season(Arrivals, labels = "both") +
        labs(title = "Seasonal Plot of Arrivals",
             y = "Arrivals",
             x = "Quarter") +
        theme_bw(base_size = 20) +
        easy_center_title()
    }
    
    else if (input$plotchoice == "Autocorrelation") {
      selectedOrigin <- input$originchoice
      ARRIVE %>%
        filter(Origin %in% selectedOrigin) %>%
        ACF(Arrivals, lag_max = 20) %>%
        ggplot(mapping = aes(x = lag, y = acf)) +
        geom_bar(stat = "identity",
                 position = "identity",
                 fill = "grey27") +
        labs(title = "ACF Plot of Arrivals",
             y = "ACF",
             x = "Lag") +
        theme_bw(base_size = 20) +
        easy_center_title() +
        geom_abline(
          slope = 0,
          intercept = 0.15,
          col = "red",
          lty = 2
        ) +
        geom_abline(
          slope = 0,
          intercept = -0.15,
          col = "red",
          lty = 2
        )
    }
    else if (input$plotchoice == "Differenced Autocorrelation") {
      selectedOrigin <- input$originchoice
      ARRIVE %>%
        filter(Origin %in% selectedOrigin) %>%
        ACF(diff(Arrivals, 1), lag_max = 20) %>%
        ggplot(mapping = aes(x = lag, y = acf)) +
        geom_bar(stat = "identity",
                 position = "identity",
                 fill = "grey27") +
        labs(title = "Differenced ACF Plot of Arrivals",
             y = "ACF",
             x = "Lag") +
        theme_bw(base_size = 20) +
        easy_center_title() +
        geom_abline(
          slope = 0,
          intercept = 0.15,
          col = "red",
          lty = 2
        ) +
        geom_abline(
          slope = 0,
          intercept = -0.15,
          col = "red",
          lty = 2
        )
    }
    else if (input$plotchoice == "Additive Decomposition") {
      selectedOrigin <- input$originchoice
      ARRIVE %>%
        filter(Origin %in% selectedOrigin) %>%
        model(classical_decomposition(Arrivals, type = "additive")) %>%
        components() %>%
        autoplot()
    }
    else if (input$plotchoice == "Multiplicative Decomposition") {
      selectedOrigin <- input$originchoice
      ARRIVE %>%
        filter(Origin %in% selectedOrigin) %>%
        model(classical_decomposition(Arrivals, type = "multiplicative")) %>%
        components() %>%
        autoplot()
    }
    
  })
  output$uitext <- renderPrint({
    if ((input$originchoice == "Japan") &
        (input$plotchoice == "Additive Decomposition")) {
      cat(
        "<b>Trend: </b>The trend plays the largest role of the three components that make up arrivals from Japan. It increases, levels out, then begins slightly decreasing."
      )
      cat(
        "<br><br><b>Seasonality: </b>The seasonality plays the smallest role of the three components. It is homoscedastic, which means that the ups and downs remain constant throughout the series."
      )
      cat(
        "<br><br><b>Remainder: </b>The remainder looks to be random (white noise with no signal), and it plays the 2nd largest role of the three components."
      )
    }
    else if ((input$originchoice == "Japan") &
             (input$plotchoice == "Multiplicative Decomposition")) {
      cat(
        "<b>Trend: </b>The trend plays the largest role by far of the three components that make up arrivals from Japan. Similar to the additive decomposition, it increases, levels out, then begins slightly decreasing."
      )
      cat(
        "<br><br><b>Seasonality: </b>The seasonality plays the smallest role of the three components. It is homoscedastic, which means that the ups and downs remain constant throughout the series."
      )
      cat(
        "<br><br><b>Remainder: </b>The remainder looks to be random (white noise with no signal), and it plays the 2nd largest role of the three components."
      )
    }
    else if ((input$originchoice == "NZ") &
             (input$plotchoice == "Additive Decomposition")) {
      cat(
        "<b>Trend: </b>The trend plays the largest role of the three components that make up arrivals from NZ The trend is steadily increasing throughout the series."
      )
      cat(
        "<br><br><b>Seasonality: </b>The seasonality plays the 2nd largest role of the three components. It is homoscedastic, which means that the ups and downs remain constant throughout the series."
      )
      cat(
        "<br><br><b>Remainder: </b>Aside from the slight signal in the remainder (non-randomness), it looks to be mostly random (white noise). The remainder plays the smallest role of the three components."
      )
    }
    else if ((input$originchoice == "NZ") &
             (input$plotchoice == "Multiplicative Decomposition")) {
      cat(
        "<b>Trend: </b>The trend plays the largest role by far of the three components that make up arrivals from NZ Similar to the additive decomposition, the trend is steadily increasing throughout the series."
      )
      cat(
        "<br><br><b>Seasonality: </b>The seasonality is a fairly weak component of the decomposition, but still plays the 2nd largest role. It is homoscedastic, which means that the ups and downs remain constant throughout the series."
      )
      cat(
        "<br><br><b>Remainder: </b>The remainder is also a fairly weak component of the series as it plays the smallest role of the three components and it looks to be mostly random (white noise)."
      )
    }
    else if ((input$originchoice == "UK") &
             (input$plotchoice == "Additive Decomposition")) {
      cat(
        "<b>Trend: </b>The trend plays the largest role of the three components that make up arrivals from the UK. The trend is steadily increasing throughout the series before leveling out at the end."
      )
      cat(
        "<br><br><b>Seasonality: </b>The seasonality plays the 3rd largest role of the three components. It is slightly heteroscedastic, which means that the ups and downs get more drastic as the series progresses."
      )
      cat(
        "<br><br><b>Remainder: </b>The remainder plays the 2nd largest role of the three components, and there is indication of a signal (non-randomness) within the white noise (randomness)."
      )
    }
    else if ((input$originchoice == "UK") &
             (input$plotchoice == "Multiplicative Decomposition")) {
      cat(
        "<b>Trend: </b>The trend plays the largest role of the three components that make up arrivals from the UK. Similar to the additive decomposition, the trend steadily increases throughout the series before leveling out at the end."
      )
      cat(
        "<br><br><b>Seasonality: </b>The seasonality plays the 2nd largest role of the three components. It is slightly heteroscedastic, which means that the ups and downs get more drastic as the series progresses."
      )
      cat(
        "<br><br><b>Remainder: </b>The remainder plays the 3rd largest role of the three components, and there is no indication of any signal (randomness) within the white noise (non-randomness)."
      )
    }
    else if ((input$originchoice == "US") &
             (input$plotchoice == "Additive Decomposition")) {
      cat(
        "<b>Trend: </b>The trend plays the largest roleby far of the three components that make up arrivals from the US. The trend is steadily increasing throughout the series."
      )
      cat(
        "<br><br><b>Seasonality: </b>The seasonality plays the 3rd largest role of the three components. It is homoscedastic, which means that the ups and downs remain constant throughout the series."
      )
      cat(
        "<br><br><b>Remainder: </b>The remainder plays the 2nd largest role of the three components, and there is indication of a signal (non-randomness) within the white noise (randomness)."
      )
    }
    else if ((input$originchoice == "US") &
             (input$plotchoice == "Multiplicative Decomposition")) {
      cat(
        "<b>Trend: </b>The trend plays the largest role by far of the three components that make up arrivals from the US. Similar to the additive decomposition, the trend steadily increases throughout the series before leveling out at the end."
      )
      cat(
        "<br><br><b>Seasonality: </b>The seasonality plays the 3rd largest role of the three components. It is homoscedastic, which means that the ups and downs remain constant throughout the series."
      )
      cat(
        "<br><br><b>Remainder: </b>The remainder plays the 2nd largest role of the three components, and there is indication of a signal (non-randomness) within the white noise (randomness)."
      )
    }
    else if ((input$originchoice == "Japan") &
             (input$plotchoice == "Autocorrelation")) {
      cat("<b><center>*Must Difference*</b></center>")
      cat(
        "This data is highly autocorrelated due to the presence of a trend. Please select the Differenced Autocorrelation button to view full interpretation."
      )
    }
    else if ((input$originchoice == "NZ") &
             (input$plotchoice == "Autocorrelation")) {
      cat("<b><center>*Must Difference*</b></center>")
      cat(
        "This data is highly autocorrelated due to the presence of a trend. Please select the Differenced Autocorrelation button to view full interpretation."
      )
    }
    else if ((input$originchoice == "UK") &
             (input$plotchoice == "Autocorrelation")) {
      cat("<b><center>*Must Difference*</b></center>")
      cat(
        "This data is highly autocorrelated due to the presence of a trend. Please select the Differenced Autocorrelation button to view full interpretation."
      )
    }
    else if ((input$originchoice == "US") &
             (input$plotchoice == "Autocorrelation")) {
      cat("<b><center>*Must Difference*</b></center>")
      cat(
        "This data is highly autocorrelated due to the presence of a trend. Please select the Differenced Autocorrelation button to view full interpretation."
      )
    }
    else if ((input$originchoice == "Japan") &
             (input$plotchoice == "Differenced Autocorrelation")) {
      cat(
        "The inverse pattern indicates that there is a strong negative correlation with Q1 and Q2 arrivals. In other words, where we see low arrivals during the months of Q1, we will see high arrivals during the months of Q2. The same pattern also applies to Q3 and Q4."
      )
    }
    else if ((input$originchoice == "NZ") &
             (input$plotchoice == "Differenced Autocorrelation")) {
      cat(
        "The inverse pattern indicates that there is a strong negative correlation with Q2 and Q4 arrivals. In other words, where we see low arrivals during the months of Q2, we will see high arrivals during the months of Q4."
      )
    }
    else if ((input$originchoice == "UK") &
             (input$plotchoice == "Differenced Autocorrelation")) {
      cat(
        "Similar to arrivals from NZ, the inverse pattern indicates that there is a strong negative correlation with Q2 and Q4 arrivals. In other words, where we see low arrivals during the months of Q2, we will see high arrivals during the months of Q4."
      )
    }
    else if ((input$originchoice == "US") &
             (input$plotchoice == "Differenced Autocorrelation")) {
      cat(
        "Similar to arrivals from the UK and NZ, the inverse pattern indicates that there is a strong negative correlation with Q2 and Q4 arrivals. In other words, where we see low arrivals during the months of Q2, we will see high arrivals during the months of Q4."
      )
    }
    else if ((input$originchoice == "Japan") &
             (input$plotchoice == "Seasonality")) {
      cat(
        "The plot indicates that there is a general increase of arrivals from Japan during Q1 and Q3."
      )
    }
    else if ((input$originchoice == "NZ") &
             (input$plotchoice == "Seasonality")) {
      cat(
        "The plot indicates that the fewest arrivals from NZ occur during Q1, and then arrivals steadily increase from Q2 to Q4. "
      )
    }
    else if ((input$originchoice == "UK") &
             (input$plotchoice == "Seasonality")) {
      cat(
        "The plot shows strong seasonality where peak arrivals from the UK occur during Q4 and Q1 generally followed by a dramatic decrease in arrivals during Q2 and Q3."
      )
    }
    else if ((input$originchoice == "US") &
             (input$plotchoice == "Seasonality")) {
      cat(
        "The plot shows somewhat of a weak seasonality where arrivals from the US peak in Q4 and Q1 followed by a slight decrease in arrivals in Q2 and Q3."
      )
    }
    else if ((input$originchoice == "Japan") &
             (input$plotchoice == "Regular Time Series")) {
      cat(
        "Arrivals from Japan had an increasing trend from 1981 to 1997. After 1997, arrivals from Japan went on a downward trend up until 2012. Also, the seasonality is largely homoscedastic, which means that the ups and downs remain constant throughout the series."
      )
    }
    else if ((input$originchoice == "NZ") &
             (input$plotchoice == "Regular Time Series")) {
      cat(
        "Arrivals from NZ had an increasing trend from 1981 to 2012. The seasonality is homoscedastic, which means that the ups and down remain constant throughout the series."
      )
    }
    else if ((input$originchoice == "UK") &
             (input$plotchoice == "Regular Time Series")) {
      cat(
        "Arrivals from the UK had an increasing trend from 1981 to 2006. After 2006, arrivals from the UK began to level out. The seasonality is heteroscedastic, which means that the ups and downs get more drastic as the series progresses."
      )
    }
    else if ((input$originchoice == "US") &
             (input$plotchoice == "Regular Time Series")) {
      cat(
        "Arrivals from the US had an increasing trend from 1981 to 2012.The seasonality is homoscedastic, which means that the ups and down remain constant throughout the series."
      )
    }
  })
  
  ########################
  #         Simple Models
  ########################
  output$naive <- renderPlot({
    # draw the time plot with selected symbols and y var
    fit_credit <- ARRIVE %>%
      model(naive = NAIVE(Arrivals))
    fc_credit <- forecast(fit_credit)
    fc_credit %>%
      filter(Origin %in% input$originchoice2) %>%  
      autoplot(ARRIVE) +
      labs(title = "Australian Arrivals Naive Model",
           y = "Arrivals")
  })
  output$seasonalnaive <- renderPlot({
    # draw the time plot with selected symbols and y var
    fit_credit <- ARRIVE %>%
      model(seasonalnaive=SNAIVE(Arrivals ~ lag("year")))
    fc_credit <- forecast(fit_credit)
    fc_credit %>%
      filter(Origin %in% input$originchoice2) %>%
      autoplot(ARRIVE) +
      labs(title = "Australian Arrivals Seasonal Naive Model",
           y = "Arrivals")
  })
  output$drift <- renderPlot({
    # draw the time plot with selected symbols and y var
    fit_credit <- ARRIVE %>%
      model(drift=RW(Arrivals ~ drift()))
    fc_credit <- forecast(fit_credit)
    fc_credit %>%
      filter(Origin %in% input$originchoice2) %>%
      autoplot(ARRIVE) +
      labs(title = "Australian Arrivals Drift Model",
           y = "Arrivals")
  })
  output$mean <- renderPlot({
    # draw the time plot with selected symbols and y var
    fit_credit <- ARRIVE %>%
      model(mean=MEAN(Arrivals))
    fc_credit <- forecast(fit_credit)
    fc_credit %>%
      filter(Origin %in% input$originchoice2) %>%
      autoplot(ARRIVE) +
      labs(title = "Australian Arrivals Mean Model",
           y = "Arrivals")
  })

  
  ########################
  # Exponential Smoothing Models
  ########################  
  output$holt <- renderPlot({
    # draw the time plot with selected symbols and y var
    fit_credit <- ARRIVE %>%
      model(holt= ETS(Arrivals ~ error("A") + trend("A") + season("N")))
    fc_credit <- forecast(fit_credit)
    fc_credit %>%
      filter(Origin %in% input$originchoice3) %>%
      autoplot(ARRIVE) +
      labs(title = "Australian Arrivals Holt's Model",
           y = "Arrivals")
  }) 
  output$holtwinter <- renderPlot({
    # draw the time plot with selected symbols and y var
    fit_credit <- ARRIVE %>%
      model(additive = ETS(Arrivals ~ error("A") + trend("A") +season("A")),
            multiplicative = ETS(Arrivals ~ error("M") + trend("A") +season("M")))
    fc_credit <- forecast(fit_credit)
    fc_credit %>%
      filter(Origin %in% input$originchoice3) %>%
      autoplot(ARRIVE) +
      labs(title = "Australian Arrivals Holt's/Winter's Model",
           y = "Arrivals")
  }) 
  ########################
  # Auto ARIMA Models
  ########################   
  output$autoarima <- renderPlot({
    # draw the time plot with selected symbols and y var
    fit_credit <- ARRIVE %>%
      model(  auto = ARIMA(Arrivals, stepwise = FALSE, approx = FALSE))
    fc_credit <- forecast(fit_credit)
    fc_credit %>%
      filter(Origin %in% input$originchoice5) %>%
      autoplot(ARRIVE) +
      labs(title = "Australian Arrivals Auto ARIMA Model",
           y = "Arrivals")
  }) 
  
  output$autoarimatext <- renderPrint({
    if (input$originchoice5 == "Japan") {
      cat("<b>Japan: </b>ARIMA(5,1,0)(0,1,1)[4]")}
    else if (input$originchoice5 == "NZ") {
      cat("<b>NZ: </b>ARIMA(2,0,0)(1,1,2)[4] w/ drift")}
    else if (input$originchoice5 == "UK") {
      cat("<b>UK: </b>ARIMA(5,0,0)(0,1,1)[4]")}
    else if (input$originchoice5 == "US") {
      cat("<b>US: </b>ARIMA(3,0,0)(1,1,1)[4] w/ drift")}
     })
  
  
  output$manualarima <- renderPlot({
    if (input$typechoice == "Non-Seasonal") {
      p <- input$pchoice
      d <- input$dchoice
      q <- input$qchoice
      fit_credit <- ARRIVE %>%
        model(  arima = ARIMA(Arrivals ~ pdq(p,d,q)))
      fc_credit <- forecast(fit_credit)
      fc_credit %>%
        filter(Origin %in% input$originchoice4) %>%
        autoplot(ARRIVE) +
        labs(title = "Australian Arrivals Manual Non-Seasonal ARIMA Model",
             y = "Arrivals")
    }
    else if(input$typechoice == "Seasonal"){
      p <- input$pchoice
      d <- input$dchoice
      q <- input$qchoice
      P<- input$pchoice2
      D<- input$dchoice2
      Q<- input$qchoice2
      fit_credit <- ARRIVE %>%
        model(  arima = ARIMA(Arrivals ~ pdq(p,d,q)+PDQ(P,D,Q)))
      fc_credit <- forecast(fit_credit)
      fc_credit %>%
        filter(Origin %in% input$originchoice4) %>%
        autoplot(ARRIVE) +
        labs(title = "Australian Arrivals Manual Seasonal ARIMA Model",
             y = "Arrivals")
      }
  })
  
  output$manualarimatext <- renderPrint({
    if (input$typechoice == "Non-Seasonal") {
      cat("<b>Non-Seasonal Model Selection: </b>ARIMA(",input$pchoice,",",input$dchoice,",",input$qchoice,")")}
    else if(input$typechoice == "Seasonal"){
      cat("<b>Seasonal Model Selection: </b>ARIMA(",input$pchoice,",",input$dchoice,",",input$qchoice,")","(",input$pchoice2,",",input$dchoice2,",",input$qchoice2,")")
    }
  })
}
##########################################################################
#         Run App
##########################################################################
shinyApp(ui = ui, server = server)
