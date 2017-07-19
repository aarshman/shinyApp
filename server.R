#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#





library(shiny)
library(xts)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$stocksGraph <- renderDygraph({
    
    stocksData = stocks[complete.cases(stocks[input$stocks_selected]),]
    stocksData[,2:ncol(stocksData)] = stocksData[,2:ncol(stocksData)]/1000
    stocksData = as.xts(stocksData,order.by=stocksData[,'Date'])
    
    stocksDataGraph = dygraph(stocksData[,input$stocks_selected], main='Total Stocks') %>%
      dyAxis('y',label='Stocks (Millions of Barrels)') %>%
      dyAxis('x',label='Date')
    
    stocksDataGraph %>% dyRangeSelector(dateWindow = c(input$date[1],input$date[2]),height=40)

  })
  
  output$spotGraph <- renderDygraph({
    
    spotData = spot_prices[complete.cases(spot_prices[input$spot_selected]),]
    spotData = as.xts(spotData,order.by=spotData[,'Date'])
    
    spotDataGraph = dygraph(spotData[,input$spot_selected], main='Spot Prices (Close)') %>%
      dyAxis('y',label='Dollars per Barrel') %>%
      dyAxis('x',label='Date')
    
    spotDataGraph %>% dyRangeSelector(dateWindow = c(input$date[1],input$date[2]),height=40)

  })
  output$bakkenDPR <- renderDygraph({
    if(input$dprProduct == 'Oil'){
      dframe = 1
    }else{
      dframe = 2
    }
    dprdata = as.xts(bakken_dpr[[dframe]][,c(-1,-4)],order.by=bakken_dpr[[dframe]][,'Month'])
    dygraph(dprdata,main = 'Drilling Productivity Report')%>%
      dyAxis('y',label='Mbbl/d or MMcf/d') %>%
      dyAxis('y2',label='Number of Rigs') %>%
      dySeries('Rig count',axis=('y2'))
  })
  output$eaglefordDPR <- renderDygraph({
    if(input$dprProduct == 'Oil'){
      dframe = 1
    }else{
      dframe = 2
    }
    dprdata = as.xts(eagleford_dpr[[dframe]][,c(-1,-4)],order.by=eagleford_dpr[[dframe]][,'Month'])
    dygraph(dprdata,main = 'Drilling Productivity Report')%>%
      dyAxis('y',label='Mbbl/d or MMcf/d') %>%
      dyAxis('y2',label='Number of Rigs') %>%
      dySeries('Rig count',axis=('y2'))
  })
  output$haynesvilleDPR <- renderDygraph({
    if(input$dprProduct == 'Oil'){
      dframe = 1
    }else{
      dframe = 2
    }
    dprdata = as.xts(haynesville_dpr[[dframe]][,c(-1,-4)],order.by=haynesville_dpr[[dframe]][,'Month'])
    dygraph(dprdata,main = 'Drilling Productivity Report')%>%
      dyAxis('y',label='Mbbl/d or MMcf/d') %>%
      dyAxis('y2',label='Number of Rigs') %>%
      dySeries('Rig count',axis=('y2'))
  })
  output$marcellusDPR <- renderDygraph({
    if(input$dprProduct == 'Oil'){
      dframe = 1
    }else{
      dframe = 2
    }
    dprdata = as.xts(marcellus_dpr[[dframe]][,c(-1,-4)],order.by=marcellus_dpr[[dframe]][,'Month'])
    dygraph(dprdata,main = 'Drilling Productivity Report')%>%
      dyAxis('y',label='Mbbl/d or MMcf/d') %>%
      dyAxis('y2',label='Number of Rigs') %>%
      dySeries('Rig count',axis=('y2'))
  })
  output$niobaraDPR <- renderDygraph({
    if(input$dprProduct == 'Oil'){
      dframe = 1
    }else{
      dframe = 2
    }
    dprdata = as.xts(niobara_dpr[[dframe]][,c(-1,-4)],order.by=niobara_dpr[[dframe]][,'Month'])
    dygraph(dprdata,main = 'Drilling Productivity Report')%>%
      dyAxis('y',label='Mbbl/d or MMcf/d') %>%
      dyAxis('y2',label='Number of Rigs') %>%
      dySeries('Rig count',axis=('y2'))
  })
  output$permianDPR <- renderDygraph({
    if(input$dprProduct == 'Oil'){
      dframe = 1
    }else{
      dframe = 2
    }
    dprdata = as.xts(permian_dpr[[dframe]][,c(-1,-4)],order.by=permian_dpr[[dframe]][,'Month'])
    dygraph(dprdata,main = 'Drilling Productivity Report')%>%
      dyAxis('y',label='Mbbl/d or MMcf/d') %>%
      dyAxis('y2',label='Number of Rigs') %>%
      dySeries('Rig count',axis=('y2'))
  })
  output$uticaDPR <- renderDygraph({
    if(input$dprProduct == 'Oil'){
      dframe = 1
    }else{
      dframe = 2
    }
    dprdata = as.xts(utica_dpr[[dframe]][,c(-1,-4)],order.by=utica_dpr[[dframe]][,'Month'])
    dygraph(dprdata,main = 'Drilling Productivity Report') %>%
      dyAxis('y',label='Mbbl/d or MMcf/d') %>%
      dyAxis('y2',label='Number of Rigs') %>%
      dySeries('Rig count',axis=('y2'))
  })
  output$tradeGraph = renderDygraph({
    data = imports[,c('Date',input$imports)] %>%
      join(.,exports[,c('Date',input$exports)],by='Date') %>%
      join(.,spot_prices[,c('Date',input$priceTrade)],by='Date')
    
    data = data[complete.cases(data),]
    
    dygraph(as.xts(data,order.by=data[,'Date'])[,c(-1)],main='Graphing Trade Balances') %>%
      dyAxis('y',label = 'Thousand Barrels per Day') %>%
      dyAxis('y2',label = 'USD per Contract') %>%
      dySeries(input$priceTrade,axis=('y2'))
    
  })
})


