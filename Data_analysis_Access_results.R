library(RODBC)

# function to read i-Tree result in MS Access 
# input the name of Access with quotation mark
func_access_df <- function(var_mdb, var_table){
  driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/kangj/Documents/R/KES/i_Tree_results/"
  my_channel <- odbcDriverConnect(paste0(driver, var_mdb))
  for (i in sqlTables(my_channel, tableType = "TABLE")$TABLE_NAME) {
    df_mdb <- sqlQuery(my_channel, paste0("select * from [", i, "]"))
    var_name <- paste0("db_", gsub(".mdb", "", var_mdb), "_", i)
    assign(var_name, df_mdb, envir=.GlobalEnv)
  }
  close(my_channel)
}

# plot valuation of air removal by category of pollutant 
func_access_df("BenMAP.mdb")
par(mfrow = c(1,1))
barplot(db_BenMAP_BenMAPResult$`Removal Value ($/yr/m2)`, names.arg = db_BenMAP_BenMAPResult$Pollutant)

# plot monthly change of pollutant flux
func_access_df("DryDeposition.mdb")
par(mfrow = c(1,2))
plot(subset(db_DryDeposition_01_SiteMonthlySums, Pollutant == "NO2")$Month, 
  subset(db_DryDeposition_01_SiteMonthlySums, Pollutant == "NO2")$`CumFlux (g/m2)`)
plot(subset(db_DryDeposition_03_SiteMonthlyMeans, Pollutant == "NO2")$Month, 
  subset(db_DryDeposition_03_SiteMonthlyMeans, Pollutant == "NO2")$`MeanFlux (g/m2)`)
# and yearly flux by pollutant
library(ggplot2)
ggplot(db_DryDeposition_02_SiteYearlySums, aes(Pollutant, `CumFlux (g/m2)`)) + geom_point()
ggplot(db_DryDeposition_04_SiteYearlyMeans, aes(Pollutant, `MeanFlux (g/m2)`)) + geom_point()

# plot hourly LAI change across the year
func_access_df("LAI.mdb")
par(mfrow = c(1,1))
plot(db_LAI_LAI$LAI)
