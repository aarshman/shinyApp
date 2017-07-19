

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(scales)
library(lubridate)
library(googleVis)
library(dygraphs)
library(DT)
library(shinydashboard)


shinyUI(
  dashboardPage(skin = "blue",
                dashboardHeader(title = "EIA Data Visualization", titleWidth = 300),
                dashboardSidebar(label = h3("Shiny Project at \nNYDSA"), 
                                 sidebarMenu(
                                   #dateRangeInput("date", label = h3("Select Time Period"),start = "2016-01-01",
                                   #               end = "2016-01-31",min = "2012-01-01",max = "2016-12-31"),
                                  menuItem("Inventory", tabName = "stocks", icon = icon("database")),
                                  menuItem('Drilling Productivity',icon=icon('bar-chart-o'),tabName = 'dpr'),
                                  menuItem("Markets", icon = icon("line-chart"),tabName = "spot"),
                                  menuItem('Trade',tabName='trade',icon=icon('money'))
                                 )
                ),
                dashboardBody(
                  tabItems(
                    # map content
                  #  tabItem(tabName = "",
                   #         fluidRow(
                    #          valueBoxOutput("NewWellsDrilledDA"),
                    #          valueBoxOutput("ContractorsActiveDA"),
                    #          valueBoxOutput("LicenceesActiveDA")
                    #        ),
                  #          fluidRow(box(h4("Select time period from the menu on left. Map shows approximate locations of 
                   #                         wells drilled in Alberta during the period."),
                   #                      leafletOutput("mapDrillingdf",width = "100%",height = 700),
                    #                     width = 12, height = 755)
                    #        )
                  #  ),
                    
                    # chart content
                    tabItem(tabName = "stocks",
                            fluidRow(
                              #valueBoxOutput("TopReasonDA"),
                              #valueBoxOutput("TopDrillerDA"),
                              #valueBoxOutput("TopLicenceeDA")
                            ),
                            fluidRow(
                              box(h4("These charts display inventory levels of Crude and Petroleum based Products."), hr(),
                                    checkboxGroupInput("stocks_selected",
                                                     "Select Item to Display",
                                                     selected = 'Crude_and_Products',
                                                     colnames(stocks)[-1]),width = 3,height = 450),
                              box(
                                dygraphOutput("stocksGraph"),width = 9, height = 450)
                              )
                    ),
                    
                    # summary tab content
                    #tabItem(tabName = "summaryDrilling",
                    #        h2('Summary of Drilling Actvities'),
                     #       dataTableOutput('summaryDrillingdf')
                    #),
                    
                    # Spot Prices chart tab content
                    tabItem(tabName = "spot",
                            fluidRow(
                              #valueBoxOutput("TopLicenceeLic"),
                              #valueBoxOutput("TopSubstanceLic"),
                              #valueBoxOutput("TopTypeLic")
                            ),
                            fluidRow(
                              box(h4("These charts display Spot Prices"), hr(),
                                  checkboxGroupInput('spot_selected',
                                                     'Select Item to Display',
                                                     selected = 'WTI',
                                                     colnames(spot_prices)[-1]),width = 3,height = 450),
                              box(dygraphOutput("spotGraph"),width = 9, height = 450)
                              )
                    ),
                    tabItem(tabName = "dpr",
                            fluidRow(
                              
                            ),
                            fluidRow(
                              box(h4('These charts display data from Drilling Productivity Reports'), hr(),
                                  radioButtons('dprProduct',h3('Select Hydrocarbon Type'),
                                               choices=c('Oil','Natural Gas'),selected='Oil'),width=3,height=450),
                              tabBox(
                                title = 'Drilling Productivity Report',
                                id = 'dprTabs', height='250px',
                                selected = 'Bakken',
                                tabPanel('Bakken',dygraphOutput("bakkenDPR")),
                                tabPanel('Eagle Ford',dygraphOutput("eaglefordDPR")),
                                tabPanel('Haynesville',dygraphOutput("haynesvilleDPR")),
                                tabPanel('Marcellus',dygraphOutput("marcellusDPR")),
                                tabPanel('Niobara',dygraphOutput("niobaraDPR")),
                                tabPanel('Permian',dygraphOutput("permianDPR")),
                                tabPanel('Utica',dygraphOutput("uticaDPR")),width=9)
                            )
                    ),
                    tabItem(tabName = 'trade',
                            fluidRow(
                              
                            ),
                            fluidRow(
                              box(h4('These charts display trade data for Crude and Crude Products'),hr(),
                                  selectInput('imports','Import Product',choices=colnames(imports)[-1],selected='Crude_Imports'),
                                  selectInput('exports','Export Product',choices=colnames(exports)[-1],selected='Crude_Exports'),
                                  selectInput('priceTrade','Product Spot Price',choices=colnames(spot_prices)[-1],selected='WTI'),width=3,height=450),
                              box(dygraphOutput('tradeGraph'),width=9,height=450)
                                  )
                      )
                      )
                    )
        )
)

                



