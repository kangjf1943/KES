library(xlsx)
library(ggplot2)

all_result <- read.xlsx("Tan_dum_result.xlsx", sheetName = "Trees")

# compare difference of plant structure across land use types
# for exmaple, LAI
ggplot(all_result, aes(Land.Use, LEAF.AREA.INDEX)) + geom_boxplot()

# compare ecosystem services across land use types 
# either with physical amount ... 
ggplot(all_result, aes(Land.Use, Avoided.Runoff..m3.)) + geom_boxplot()
# or valuation
ggplot(all_result, aes(Land.Use, NO2.Value....)) + geom_boxplot()
