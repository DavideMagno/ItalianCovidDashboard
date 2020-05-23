library(shiny)
library(shinyjs)
library(billboarder)
library(shinycssloaders)

# Define UI for application that draws a histogram
navbarPage("Italy Covid", id="nav",
           tabPanel("Interactive Map", 
                    fluidRow(
                      column(4,
                             dateInput("date", "Select the date for the map", 
                                       last.date, 
                                       min = as.Date("2020-02-24"),
                                       max = last.date),
                             align="center"
                             ),
                      column(4,
                             radioButtons("type", "Type of data", 
                                          choices = c("Region", "Province"),
                                          selected = "Region"),
                             align="center"
                             ),
                      column(4,
                             conditionalPanel(condition = "input.type == 'Region'",
                                              selectInput("field", "Field for the map", 
                                                          choices = c("Hospitalised", "In ICU", "Home Isolation",
                                                                      "Dead", "Healed", "Total"),
                                                          selected = "Total")),
                             align="center")
                    ),
                    fluidRow(
                      column(12,
                             includeCSS(here::here("styles.css")),
                        leaflet::leafletOutput("map", height = 710),
                        absolutePanel(id = "controls", class = "panel panel-default", 
                                      fixed = FALSE, top = 15, draggable = TRUE, 
                                      left = "auto", right = 60, bottom = "auto",
                                      width = 400, height = 400, cursor = "default",
                                      p(), p(),
                                      plotly::plotlyOutput("dynamic", height = "300px"),
                                      checkboxInput("log", "Log scale", value = TRUE)
                        ),
                        absolutePanel(id = "bestworst", class = "panel panel-default", 
                                      fixed = FALSE, top = 15, draggable = TRUE, 
                                      left = 100, width = 400, cursor = "default",
                                      p(), p(),
                                      plotly::plotlyOutput("best.worst.plot", height = "450px")
                        )
                      )
                    )
           ),
           tabPanel("Data Explorer",
                    fluidRow(
                      column(3,
                             selectInput("regions", "Select the Regions of analysis", 
                                         c("Italy"= "", 
                                           unique(Data$covid.regions$Region)),
                                         multiple=TRUE),
                             align="center"),
                      column(3,
                             selectInput("provinces", "Select the Provinces of analysis", 
                                         c("All provinces"= ""), 
                                         multiple=TRUE),
                             align="center"
                      ),
                      column(3,
                             dateRangeInput("date.range", "Select the date range of analysis", 
                                            start = as.Date("2020-02-24"), 
                                            end = last.date,
                                            min = as.Date("2020-02-24"),
                                            max = last.date),
                             align="center"
                      ),
                      column(3,
                             selectInput("data.field", "Select the fields to analyse",
                                         c("Total" = ""),
                                         multiple=TRUE, selected = "Total"),
                             align="center"
                      )
                    ),
                    fluidRow(
                      column(1, 
                             p(strong("Absolute figures"))
                      ), 
                      column(1,
                             checkboxInput("data_increments", "Show daily increments", value = TRUE)
                      ),
                      column(1,
                             conditionalPanel(condition = "!input.data_increments",
                                              checkboxInput("data.log", "log scale", value = TRUE))
                      )
                    ),
                    fluidRow(
                      uiOutput("plots")
                    ),
                    fluidRow(
                      column(1, 
                             p(strong("Relative increments"))
                      )
                    ),
                    fluidRow(
                      uiOutput("plots.ratio")
                    ),
                    
                    fluidRow(
                      column(2, 
                             conditionalPanel(condition = "output.provinces == true",
                                              p(strong("Increments compared to Tests"))
                             )
                      ),
                      column(3, 
                             conditionalPanel(condition = "output.provinces == true",
                                              p(paste("Number of tests used is the",
                                                      "linearly decreasing weighted",
                                                      "average over previous 7 days"))
                             )
                      )
                    ),
                    fluidRow(
                      uiOutput("tests.ratio")
                    ),
                    fluidRow(
                      hr(),
                      DT::dataTableOutput("analysis_table")
                    )
           ),
           tabPanel("Coefficient of Reproduction",
                    sidebarLayout(
                      sidebarPanel(
                        p(paste0("Latest estimates of the number of confirmed cases by date of infection, ",
                                 "the expected change in daily confirmed cases, the effective reproduction ",
                                 "number, tthe doubling time (when negative this corresponds to the halving time), ",
                                 "and the adjusted R-squared of the exponential fit. The mean an 90% credible ",
                                 "interval is shown for any numeric estimate")),
                        tableOutput("Summary"),
                        p(strong(paste0("The estimates of the coefficient of reproduction shown here have been done ",
                                        "by a team based at London School of Hygiene and Tropical Medicine who ",
                                        "is specialised on real-time modelling and forecasting of infectious disease outbreaks."))),
                        p(strong(paste0("All rights are reserved to them, who have shared codes, data and result with MIT ",
                                        "license at "), a("this Github repository.", href="https://github.com/epiforecasts"),
                                 paste0("Additional methodological info and the estimates for other countries are available ",
                                        "at"), a("this webpage", href="https://epiforecasts.io/")))),
                      mainPanel(
                        p(paste0("Note that it takes time for infection to cause symptoms, to get tested for ",
                                 "SARS-CoV-2 infection, for a positive test to return and ultimately to enter ",
                                 "the case data presented here. In other words, today's case data are only informative ",
                                 "of new infections about two weeks ago. This is reflected in the plots below, which ",
                                 "are by date of infection")),
                        plotOutput("R0XRegion", height = "850px"),
                        p(paste0("Confirmed cases and the time-varying estimate of the ",
                                 "effective reproduction number (light bar = 90% credible interval; dark bar = the 50% credible interval.). ",
                                 "Regions are ordered by the number of expected daily confirmed cases and shaded based on the expected change",
                                 "in daily confirmedcases. The horizontal dotted line indicates the target value of 1 for the effective reproduction ",
                                 "no. required for control and a single case required for elimination.")),
                        hr(),
                        selectInput("region.r0", strong("Analyse the evolution at national or at region level"), 
                                    c("Italy", 
                                      setdiff(unique(Data$covid.regions$Region),
                                              c("Basilicata", "Molise"))),
                                    multiple=FALSE, selected = "Italy"),
                        plotly::plotlyOutput("Cases.Graph"),
                        p(paste0("Confirmed cases by date of report (bars) and their estimated date of infection")),
                        plotly::plotlyOutput("R0.Graph"),
                        p(paste0("Time-varying estimate of the effective reproduction number. Light ribbon",
                                 " = 90% credible interval; dark ribbon = the 50% credible interval. These",
                                 "should be considered indicative only")))
                    ) 
           )
)