

library(shiny)
library(dplyr)
library(plyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(scales)
library(lubridate)

total_production = read.csv('data/crude_production_weekly_us.csv',stringsAsFactors = FALSE)
colnames(total_production) = as.character(as.vector(total_production[2,]))
total_production = total_production[-c(1:2,nrow(total_production)),-c(ncol(total_production))]
total_production[,2:ncol(total_production)] = as.numeric(total_production[,2:ncol(total_production)])
total_production$Date = as.Date(total_production$Date,format='%b %d,%Y')
rownames(total_production) = total_production$Date
colnames(total_production) = c('Date','Production_Tbpd')

refiners_input = read.csv('data/crude_refiner_input_util_weekly_us.csv',stringsAsFactors = FALSE)
colnames(refiners_input) = as.character(as.vector(refiners_input[2,]))
refiners_input = refiners_input[-c(1:2,nrow(refiners_input)),-c(ncol(refiners_input))]
refiners_input[,2:ncol(refiners_input)] = lapply(refiners_input[,2:ncol(refiners_input)],as.numeric)
refiners_input$Date = as.Date(refiners_input$Date,format='%b %d,%Y')
rownames(refiners_input) = refiners_input$Date
colnames(refiners_input) = c('Date','Net_Input_Crude_Tbpd','Gross_Input_Tbpd','Crude_Capacity','Capacity_Utilization_Rate')

exports = read.csv('data/exports_weekly_us.csv',stringsAsFactors = FALSE)
colnames(exports) = as.character(as.vector(exports[2,]))
exports = exports[-c(1:2,nrow(exports)),-c(ncol(exports))]
exports[,2:ncol(exports)] = lapply(exports[,2:ncol(exports)],as.numeric)
exports$Date = as.Date(exports$Date,format='%b %d,%Y')
rownames(exports) = exports$Date
colnames(exports) = c('Date','Crude_and_Products_Exports','Crude_Exports','Products_Exports','Gas_Exports','Jet_Fuel_Exports','Distillate_Exports','Fuel_Oil_Exports','Propane_Exports','Other_Exports')

imports = read.csv('data/imports_weekly_us.csv',stringsAsFactors = FALSE)
colnames(imports) = as.character(as.vector(imports[2,]))
imports = imports[-c(1:2,nrow(imports)),-c(ncol(imports))]
imports[,2:ncol(imports)] = lapply(imports[,2:ncol(imports)],as.numeric)
imports$Date = as.Date(imports$Date,format='%b %d,%Y')
rownames(imports) = imports$Date
imports = select(imports,c(1,2,3,7,8,25,26,27,32,33,34,35))
colnames(imports) = c('Date','Crude_and_Products_Imports','Crude_Imports','Products_Imports','Gasoline_Imports','Fuel_Ethanol_Imports','Jet_Fuel_Imports','Distillate_Fuel_Oil_Imports','Residual_Fuel_Oil_Imports','Propane_Imports','Other_Imports','Kerosene_Imports')

net_imports = read.csv('data/net_imports_weekly_us.csv',stringsAsFactors = FALSE)
colnames(net_imports) = as.character(as.vector(net_imports[2,]))
net_imports = net_imports[-c(1:2,nrow(net_imports)),-c(ncol(net_imports))]
net_imports[,2:ncol(net_imports)] = lapply(net_imports[,2:ncol(net_imports)],as.numeric)
net_imports$Date = as.Date(net_imports$Date,format='%b %d,%Y')
rownames(net_imports) = net_imports$Date
colnames(net_imports) = c('Date','Crude_and_Products_Net_Imports','Crude_Net_Imports','Products_Net_Imports')

proved_crude_reserves = read.csv('data/proved_reserves_yearly.csv',stringsAsFactors = FALSE)
colnames(proved_crude_reserves) = as.character(as.vector(proved_crude_reserves[2,]))
proved_crude_reserves = proved_crude_reserves[-c(1:2,nrow(proved_crude_reserves)),-c(ncol(proved_crude_reserves))]
proved_crude_reserves[,2:ncol(proved_crude_reserves)] = lapply(proved_crude_reserves[,2:ncol(proved_crude_reserves)],as.numeric)
proved_crude_reserves$Date = as.Date(paste(proved_crude_reserves$Date,12,31,sep='-'),format='%Y-%m-%d')
rownames(proved_crude_reserves) = proved_crude_reserves$Date
proved_crude_reserves = select(proved_crude_reserves,c(1:4,8:11,16:22,26:30,33:37,51:54))
colnames(proved_crude_reserves) = c('Date','US','Lower_48','Federal_Offshore','Alaska','Alabama','Arkansas','California','Colorado','Florida','Illinois','Indiana','Kansas','Kentucky','Louisiana','Michigan','Mississippi','Montana','Nebraska','New Mexico','North Dakota','Ohio','Oklahoma','Pennsylvania','Texas','Utah','West_Virginia','Wyoming','Miscellaneous')

refiner_net_production = read.csv('data/refiner_net_production_weekly_us.csv',stringsAsFactors = FALSE)
colnames(refiner_net_production) = as.character(as.vector(refiner_net_production[2,]))
refiner_net_production = refiner_net_production[-c(1:2,nrow(refiner_net_production)),-c(ncol(refiner_net_production))]
refiner_net_production[,2:ncol(refiner_net_production)] = lapply(refiner_net_production[,2:ncol(refiner_net_production)],as.numeric)
refiner_net_production$Date = as.Date(refiner_net_production$Date,format='%b %d,%Y')
rownames(refiner_net_production) = refiner_net_production$Date
refiner_net_production = select(refiner_net_production,c(1:2,13,16,20:21))
colnames(refiner_net_production) = c('Date','Motor_Gasoline','Kerosene_Jet_Fuel','Distillate_Fuel_Oil','Residual_Fuel_Oil','Propane')

stocks = read.csv('data/stocks_weekly_us.csv',stringsAsFactors = FALSE)
colnames(stocks) = as.character(as.vector(stocks[2,]))
stocks = stocks[-c(1:2,nrow(stocks)),-c(ncol(stocks))]
stocks[,2:ncol(stocks)] = lapply(stocks[,2:ncol(stocks)],as.numeric)
stocks$Date = as.Date(stocks$Date,format='%d-%b-%y')
rownames(stocks) = stocks$Date
stocks = select(stocks,c(1:2,4,9,26:28,32:33,35))
colnames(stocks) = c('Date','Crude_and_Products','Crude','Gasoline','Fuel_Ethanol','Kerosene_Jet_Fuel','Distillate_Fuel_Oil','Residual_Fuel_Oil','Propane','Other_Oils')

rigs_by_site = read.csv('data/rigs_by_site.csv',stringsAsFactors = FALSE)
colnames(rigs_by_site) = as.character(as.vector(rigs_by_site[2,]))
rigs_by_site = rigs_by_site[-c(1:2,nrow(rigs_by_site)),-c(ncol(rigs_by_site))]
rigs_by_site[,2:ncol(rigs_by_site)] = lapply(rigs_by_site[,2:ncol(rigs_by_site)],as.numeric)
rigs_by_site$Date = as.Date(paste(rigs_by_site$Date,15,sep='-'),format='%b-%Y-%d')
rownames(rigs_by_site) = rigs_by_site$Date
colnames(rigs_by_site) = c('Date','Onshore','Offshore')

rigs_by_type = read.csv('data/rigs_by_type.csv',stringsAsFactors = FALSE)
colnames(rigs_by_type) = as.character(as.vector(rigs_by_type[2,]))
rigs_by_type = rigs_by_type[-c(1:2,nrow(rigs_by_type)),-c(ncol(rigs_by_type))]
rigs_by_type[,2:ncol(rigs_by_type)] = lapply(rigs_by_type[,2:ncol(rigs_by_type)],as.numeric)
rigs_by_type$Date = as.Date(paste(rigs_by_type$Date,15,sep='-'),format='%b-%Y-%d')
rownames(rigs_by_type) = rigs_by_type$Date
colnames(rigs_by_type) = c('Date','Crude','Natural_Gas')

rigs_total = read.csv('data/rigs_total.csv',stringsAsFactors = FALSE)
colnames(rigs_total) = as.character(as.vector(rigs_total[2,]))
rigs_total = rigs_total[-c(1:2,nrow(rigs_total)),-c(ncol(rigs_total))]
rigs_total[,2] = as.numeric(rigs_total[,2])
rigs_total$Date = as.Date(paste(rigs_total$Date,15,sep='-'),format='%b-%Y-%d')
rownames(rigs_total) = rigs_total$Date
colnames(rigs_total) = c('Date','Rig_Count')

well_service_rigs_by_type = read.csv('data/well_service_rigs_by_type.csv',stringsAsFactors = FALSE)
colnames(well_service_rigs_by_type) = as.character(as.vector(well_service_rigs_by_type[2,]))
well_service_rigs_by_type = well_service_rigs_by_type[-c(1:2,nrow(well_service_rigs_by_type)),-c(ncol(well_service_rigs_by_type))]
well_service_rigs_by_type[,2:ncol(well_service_rigs_by_type)] = lapply(well_service_rigs_by_type[,2:ncol(well_service_rigs_by_type)],as.numeric)
well_service_rigs_by_type$Date = as.Date(paste(well_service_rigs_by_type$Date,15,sep='-'),format='%b-%Y-%d')
rownames(well_service_rigs_by_type) = well_service_rigs_by_type$Date
colnames(well_service_rigs_by_type) = c('Date','Crude','Natural_Gas')

crude_spot = read.csv('data/crude_spot.csv',stringsAsFactors = FALSE)
colnames(crude_spot) = as.character(as.vector(crude_spot[2,]))
crude_spot = crude_spot[-c(1:2,nrow(crude_spot)),-c(ncol(crude_spot))]
crude_spot[,2:ncol(crude_spot)] = lapply(crude_spot[,2:ncol(crude_spot)],as.numeric)
crude_spot$Date = as.Date(crude_spot$Date,format='%b %d,%Y')
rownames(crude_spot) = crude_spot$Date
colnames(crude_spot) = c('Date','WTI','Brent')

heating_oil_spot = read.csv('data/heating_oil_spot.csv',stringsAsFactors = FALSE)
colnames(heating_oil_spot) = as.character(as.vector(heating_oil_spot[2,]))
heating_oil_spot = heating_oil_spot[-c(1:2,nrow(heating_oil_spot)),-c(ncol(heating_oil_spot))]
heating_oil_spot[,2:ncol(heating_oil_spot)] = as.numeric(heating_oil_spot[,2:ncol(heating_oil_spot)])
heating_oil_spot$Date = as.Date(heating_oil_spot$Date,format='%b %d,%Y')
rownames(heating_oil_spot) = heating_oil_spot$Date
colnames(heating_oil_spot) = c('Date','Heating_Oil')

kerosene_jet_fuel_spot = read.csv('data/kerosene_jet_fuel_spot.csv',stringsAsFactors = FALSE)
colnames(kerosene_jet_fuel_spot) = as.character(as.vector(kerosene_jet_fuel_spot[2,]))
kerosene_jet_fuel_spot = kerosene_jet_fuel_spot[-c(1:2,nrow(kerosene_jet_fuel_spot)),-c(ncol(kerosene_jet_fuel_spot))]
kerosene_jet_fuel_spot[,2] = as.numeric(kerosene_jet_fuel_spot[,2])
kerosene_jet_fuel_spot$Date = as.Date(kerosene_jet_fuel_spot$Date,format='%b %d,%Y')
rownames(kerosene_jet_fuel_spot) = kerosene_jet_fuel_spot$Date
colnames(kerosene_jet_fuel_spot) = c('Date','Kerosene_Jet_Fuel')

conventional_gasoline_spot = read.csv('data/conventional_gasoline_spot.csv',stringsAsFactors = FALSE)
colnames(conventional_gasoline_spot) = as.character(as.vector(conventional_gasoline_spot[2,]))
conventional_gasoline_spot = conventional_gasoline_spot[-c(1:2,nrow(conventional_gasoline_spot)),-c(ncol(conventional_gasoline_spot))]
conventional_gasoline_spot[,2:ncol(conventional_gasoline_spot)] = lapply(conventional_gasoline_spot[,2:ncol(conventional_gasoline_spot)],as.numeric)
conventional_gasoline_spot$Date = as.Date(conventional_gasoline_spot$Date,format='%b %d,%Y')
rownames(conventional_gasoline_spot) = conventional_gasoline_spot$Date
colnames(conventional_gasoline_spot) = c('Date','Conventional_Gasoline_NY_Harbor','Conventional_Gasoline_Gulf_Coast')

rbob_gasoline_spot = read.csv('data/rbob_gasoline_spot.csv',stringsAsFactors = FALSE)
colnames(rbob_gasoline_spot) = as.character(as.vector(rbob_gasoline_spot[2,]))
rbob_gasoline_spot = rbob_gasoline_spot[-c(1:2,nrow(rbob_gasoline_spot)),-c(ncol(rbob_gasoline_spot))]
rbob_gasoline_spot[,2] = as.numeric(rbob_gasoline_spot[,2])
rbob_gasoline_spot$Date = as.Date(rbob_gasoline_spot$Date,format='%b %d,%Y')
rownames(rbob_gasoline_spot) = rbob_gasoline_spot$Date
colnames(rbob_gasoline_spot) = c('Date','RBOB_Gasoline')

propane_spot = read.csv('data/propane_spot.csv',stringsAsFactors = FALSE)
colnames(propane_spot) = as.character(as.vector(propane_spot[2,]))
propane_spot = propane_spot[-c(1:2,nrow(propane_spot)),-c(ncol(propane_spot))]
propane_spot[,2] = as.numeric(propane_spot[,2])
propane_spot$Date = as.Date(propane_spot$Date,format='%b %d,%Y')
rownames(propane_spot) = propane_spot$Date
colnames(propane_spot) = c('Date','Propane')

spot_prices = crude_spot %>%
  join(.,heating_oil_spot,type='full') %>%
  join(.,kerosene_jet_fuel_spot,type='full') %>%
  join(.,conventional_gasoline_spot,type='full') %>%
  join(.,rbob_gasoline_spot,type='full') %>%
  join(.,propane_spot,type='full')

bakken_oil_dpr = read.csv('data/bakken_dpr.csv',stringsAsFactors = FALSE)
colnames(bakken_oil_dpr) = as.character(as.vector(bakken_oil_dpr[1,]))
bakken_oil_dpr = bakken_oil_dpr[-1,]
bakken_oil_dpr$Month = as.Date(paste(bakken_oil_dpr$Month,15,sep='-'),format = '%b-%y-%d')
rownames(bakken_oil_dpr) = bakken_oil_dpr$Month
bakken_oil_dpr[,2:ncol(bakken_oil_dpr)] = apply(bakken_oil_dpr[,2:ncol(bakken_oil_dpr)],2,function(y) as.numeric(gsub('\\(','-',gsub('\\)','',gsub(',','',y)))))
bakken_oil_dpr[,c(5,8)] = lapply(bakken_oil_dpr[,c(5,8)],function(y) y/1000)
bakken_natgas_dpr = bakken_oil_dpr[-1,c(1:2,6:8)]
bakken_oil_dpr = bakken_oil_dpr[-1,1:5]
bakken_dpr = list(bakken_oil_dpr,bakken_natgas_dpr)

eagleford_oil_dpr = read.csv('data/eagleford_dpr.csv',stringsAsFactors = FALSE)
colnames(eagleford_oil_dpr) = as.character(as.vector(eagleford_oil_dpr[1,]))
eagleford_oil_dpr = eagleford_oil_dpr[-1,]
eagleford_oil_dpr$Month = as.Date(paste(eagleford_oil_dpr$Month,15,sep='-'),format = '%b-%y-%d')
rownames(eagleford_oil_dpr) = eagleford_oil_dpr$Month
eagleford_oil_dpr[,2:ncol(eagleford_oil_dpr)] = apply(eagleford_oil_dpr[,2:ncol(eagleford_oil_dpr)],2,function(y) as.numeric(gsub('\\(','-',gsub('\\)','',gsub(',','',y)))))
eagleford_oil_dpr[,c(5,8)] = lapply(eagleford_oil_dpr[,c(5,8)],function(y) y/1000)
eagleford_natgas_dpr = eagleford_oil_dpr[-1,c(1:2,6:8)]
eagleford_oil_dpr = eagleford_oil_dpr[-1,1:5]
eagleford_dpr = list(eagleford_oil_dpr,eagleford_natgas_dpr)

haynesville_oil_dpr = read.csv('data/haynesville_dpr.csv',stringsAsFactors = FALSE)
colnames(haynesville_oil_dpr) = as.character(as.vector(haynesville_oil_dpr[1,]))
haynesville_oil_dpr = haynesville_oil_dpr[-1,]
haynesville_oil_dpr$Month = as.Date(paste(haynesville_oil_dpr$Month,15,sep='-'),format = '%b-%y-%d')
rownames(haynesville_oil_dpr) = haynesville_oil_dpr$Month
haynesville_oil_dpr[,2:ncol(haynesville_oil_dpr)] = apply(haynesville_oil_dpr[,2:ncol(haynesville_oil_dpr)],2,function(y) as.numeric(gsub('\\(','-',gsub('\\)','',gsub(',','',y)))))
haynesville_oil_dpr[,c(5,8)] = lapply(haynesville_oil_dpr[,c(5,8)],function(y) y/1000)
haynesville_natgas_dpr = haynesville_oil_dpr[-1,c(1:2,6:8)]
haynesville_oil_dpr = haynesville_oil_dpr[-1,1:5]
haynesville_dpr = list(haynesville_oil_dpr,haynesville_natgas_dpr)

marcellus_oil_dpr = read.csv('data/marcellus_dpr.csv',stringsAsFactors = FALSE)
colnames(marcellus_oil_dpr) = as.character(as.vector(marcellus_oil_dpr[1,]))
marcellus_oil_dpr = marcellus_oil_dpr[-1,]
marcellus_oil_dpr$Month = as.Date(paste(marcellus_oil_dpr$Month,15,sep='-'),format = '%b-%y-%d')
rownames(marcellus_oil_dpr) = marcellus_oil_dpr$Month
marcellus_oil_dpr[,2:ncol(marcellus_oil_dpr)] = apply(marcellus_oil_dpr[,2:ncol(marcellus_oil_dpr)],2,function(y) as.numeric(gsub('\\(','-',gsub('\\)','',gsub(',','',y)))))
marcellus_oil_dpr[,c(5,8)] = lapply(marcellus_oil_dpr[,c(5,8)],function(y) y/1000)
marcellus_natgas_dpr = marcellus_oil_dpr[-1,c(1:2,6:8)]
marcellus_oil_dpr = marcellus_oil_dpr[-1,1:5]
marcellus_dpr = list(marcellus_oil_dpr,marcellus_natgas_dpr)

niobara_oil_dpr = read.csv('data/niobara_dpr.csv',stringsAsFactors = FALSE)
colnames(niobara_oil_dpr) = as.character(as.vector(niobara_oil_dpr[1,]))
niobara_oil_dpr = niobara_oil_dpr[-1,]
niobara_oil_dpr$Month = as.Date(paste(niobara_oil_dpr$Month,15,sep='-'),format = '%b-%y-%d')
rownames(niobara_oil_dpr) = niobara_oil_dpr$Month
niobara_oil_dpr[,2:ncol(niobara_oil_dpr)] = apply(niobara_oil_dpr[,2:ncol(niobara_oil_dpr)],2,function(y) as.numeric(gsub('\\(','-',gsub('\\)','',gsub(',','',y)))))
niobara_oil_dpr[,c(5,8)] = lapply(niobara_oil_dpr[,c(5,8)],function(y) y/1000)
niobara_natgas_dpr = niobara_oil_dpr[-1,c(1:2,6:8)]
niobara_oil_dpr = niobara_oil_dpr[-1,1:5]
niobara_dpr = list(niobara_oil_dpr,niobara_natgas_dpr)

permian_oil_dpr = read.csv('data/permian_dpr.csv',stringsAsFactors = FALSE)
colnames(permian_oil_dpr) = as.character(as.vector(permian_oil_dpr[1,]))
permian_oil_dpr = permian_oil_dpr[-1,]
permian_oil_dpr$Month = as.Date(paste(permian_oil_dpr$Month,15,sep='-'),format = '%b-%y-%d')
rownames(permian_oil_dpr) = permian_oil_dpr$Month
permian_oil_dpr[,2:ncol(permian_oil_dpr)] = apply(permian_oil_dpr[,2:ncol(permian_oil_dpr)],2,function(y) as.numeric(gsub('\\(','-',gsub('\\)','',gsub(',','',y)))))
permian_oil_dpr[,c(5,8)] = lapply(permian_oil_dpr[,c(5,8)],function(y) y/1000)
permian_natgas_dpr = permian_oil_dpr[-1,c(1:2,6:8)]
permian_oil_dpr = permian_oil_dpr[-1,1:5]
permian_dpr = list(permian_oil_dpr,permian_natgas_dpr)

utica_oil_dpr = read.csv('data/utica_dpr.csv',stringsAsFactors = FALSE)
colnames(utica_oil_dpr) = as.character(as.vector(utica_oil_dpr[1,]))
utica_oil_dpr = utica_oil_dpr[-1,]
utica_oil_dpr$Month = as.Date(paste(utica_oil_dpr$Month,15,sep='-'),format = '%b-%y-%d')
rownames(utica_oil_dpr) = utica_oil_dpr$Month
utica_oil_dpr[,2:ncol(utica_oil_dpr)] = apply(utica_oil_dpr[,2:ncol(utica_oil_dpr)],2,function(y) as.numeric(gsub('\\(','-',gsub('\\)','',gsub(',','',y)))))
utica_oil_dpr[,c(5,8)] = lapply(utica_oil_dpr[,c(5,8)],function(y) y/1000)
utica_natgas_dpr = utica_oil_dpr[-1,c(1:2,6:8)]
utica_oil_dpr = utica_oil_dpr[-1,1:5]
utica_dpr = list(utica_oil_dpr,utica_natgas_dpr)




