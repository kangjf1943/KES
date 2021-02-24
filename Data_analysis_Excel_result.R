library(xlsx)
library(ggplot2)
library(dplyr)

all_result <- read.xlsx("Tan_dum_result.xlsx", sheetName = "Trees")

# compare difference of plant structure across land use types
# for exmaple, LAI
ggplot(all_result, aes(Land.Use, LEAF.AREA.INDEX)) + geom_boxplot()

# compare ecosystem services across land use types 
# either with physical amount ... 
ggplot(all_result, aes(Land.Use, Avoided.Runoff..m3.)) + geom_boxplot()
# or valuation
ggplot(all_result, aes(Land.Use, NO2.Value....)) + geom_boxplot()

# compare the ecosystem services provided by different species
top_water_species <- all_result %>% group_by(Species) %>% 
  summarise(Avoided_runoff = sum(Avoided.Runoff..m3.)) %>% 
  arrange(desc(Avoided_runoff)) %>% head(6)
top_water_species$Species <- c("Ginkgo", "Zelkova","Acer", "Prunus", "Tulip", "Platanus")
barplot(top_water_species$Avoided_runoff, names.arg = top_water_species$Species)
