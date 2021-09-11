# comparison of diff ES - value and physical amount
# comparison of diff sub-ES - value and physical amount 
# comparison of ES across diff land use

# On the MS Excel results
library(xlsx)

tree_fia_1 <- read.csv("INVENT_FIA.csv")
tree_fia_2 <- read.xlsx("INVENT_FIA.xlsx", sheetName = "INVENT_FIA")
# tree_fia_2多两个列和两个行，且“evg.LA..m2.”和“LEAF.AREA..M2.”要么相等，要么为0
names(tree_fia_2)[names(tree_fia_2) %in% names(tree_fia_1) == FALSE]
# 多的两列则是总计计算的列
tree_fia_2[tree_fia_2$NUMBER %in% tree_fia_1$NUMBER == FALSE, ]
# 可见除去这两行两列的话，发现两者其实是一样的
nrow(tree_fia_2)
tree_fia_3 <- tree_fia_2[-c(1241,1242), -c(7,10)]
j <- 0
for (i in colnames(tree_fia_1)) {
  j <- sum(tree_fia_1[, i] %in% tree_fia_3[ ,i] == FALSE)+j
  print(j)
}

# 值得注意的变量
tree_fia_1$CARBON.STORAGE..KG.
tree_fia_1$GROSS.CARBON.SEQ...KG.YR.
tree_fia_1$TREE.VALUE....
tree_fia_1$BIOMASS.ADJUSTMENT


# On the MS Access results
library(RODBC)
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             DBQ=C:/Users/kangj/Documents/R/KES/Tree/IndividualTree.mdb")
tables <- sqlTables(channel, tableType = "TABLE")$TABLE_NAME
target_tables <- c(grep("Individual", tables, value = TRUE), 
                   "Inventory", "Trees", 
                   "EcoServiceValueSummary", "SpeciesSummary")
for (var_table in target_tables) {
  df_mdb <- sqlQuery(channel, paste0("select * from [", var_table, "]"))
  name_df_mdb <- paste0(var_table)
  assign(name_df_mdb, df_mdb)
}
close(channel)
sum(IndividualTreeAvRoSumNoAdj$`Avoided Runoff (m3)`/
      IndividualTreeAvRoSumAdjusted$`Avoided Runoff (m3)` < 1)
plot(IndividualTreeAvRoSumNoAdj$`Avoided Runoff (m3)`/
           IndividualTreeAvRoSumAdjusted$`Avoided Runoff (m3)`)
# 可见没调整的值比较大，而且是等比例缩放，未调整/调整后 = 1.04

assign("avo_runoff_adj", IndividualTreeAvRoSumAdjusted)
rm(IndividualTreeAvRoSumAdjusted)
assign("avo_runoff", IndividualTreeAvRoSumNoAdj)
rm(IndividualTreeAvRoSumNoAdj)
assign("health_adj", IndividualTreeHlthEfSumAdjusted)
rm(IndividualTreeHlthEfSumAdjusted)
assign("health", IndividualTreeHlthEfSumNoAdj)
rm(IndividualTreeHlthEfSumNoAdj)
assign("pollutant_adj", IndividualTreePollRemSumAdjusted)
rm(IndividualTreePollRemSumAdjusted)
assign("pollutant", IndividualTreePollRemSumNoAdj)
rm(IndividualTreePollRemSumNoAdj)

# 发现Inventory其实包含了六个Individual对象的列名
obeject_list <- list(avo_runoff, avo_runoff_adj, 
                     health, health_adj, 
                     pollutant, pollutant_adj)
for (i in c(1:6)) {
  print(setdiff(names(obeject_list[[i]]), names(Trees)))
}
# 而且Trees则包含了Inventory的所有列名
setdiff(names(Inventory), names(Trees))

# 如果有不一样，就把列名打印出来
func_df_diff <- function(var_small, var_big){
  for (i in colnames(var_small)) {
    j <- sum(var_big[,i] %in% var_small[,i] == FALSE)
    if (j > 0) {print(i)}
  }
}

# 确认Trees和Inventory的值是否相等：相等
func_df_diff(Inventory, Trees)

# 可见Trees中的数据是调整后的结果
func_df_diff(avo_runoff_adj, Trees)
func_df_diff(avo_runoff, Trees)
func_df_diff(avo_runoff_adj, avo_runoff)

func_df_diff(health_adj, Trees)
func_df_diff(health, Trees)
func_df_diff(health_adj, health)

func_df_diff(pollutant_adj, Trees)
func_df_diff(pollutant, Trees)

# Trees和tree_fia是部分重叠的
names(tree_fia_1)[names(tree_fia_1) %in% gsub("[ ()]", "\\.", names(Trees)) == FALSE]
names(Trees)[gsub("[ ()]", "\\.", names(Trees)) %in% names(tree_fia_1) == FALSE]

# all the ES value in Trees
esvalue <- Trees[grep("\\$", names(Trees), value = TRUE)]

library(ggplot2)
EcoServiceValueSummary <- EcoServiceValueSummary[-nrow(EcoServiceValueSummary),]
colnames(EcoServiceValueSummary) <- 
  c("species", "number_of_trees",
    "carbon_storage_sum_yen", "carbon_storage_mean_yen", 
    "carbon_seq_sum_yen", "carbon_seq_mean_yen", 
    "pollutant_sum_yen", "pollutant_mean_yen", 
    "avo_runoff_sum_yen", "avo_runoff_mean_yen")
ggplot(EcoServiceValueSummary, aes(x = c(1:nrow(EcoServiceValueSummary)))) + 
  geom_line(aes(y = carbon_storage_sum_yen), color = "red") + 
  geom_line(aes(y = carbon_seq_sum_yen)) + 
  geom_line(aes(y = pollutant_sum_yen)) + 
  geom_line(aes(y = avo_runoff_sum_yen))
ggplot(head(EcoServiceValueSummary, 50), 
       aes(x = c(1:50))) + 
  geom_line(aes(y = carbon_seq_sum_yen, color = "carbon")) + 
  geom_line(aes(y = pollutant_sum_yen, color = "pollutant")) + 
  geom_line(aes(y = avo_runoff_sum_yen, color = "runoff")) +
  scale_color_manual("", values = c(
    "carbon" = "red", "pollutant" = "darkgreen", "runoff" = "blue")) + 
  ylab("sum_yen")

ggplot(head(EcoServiceValueSummary, 118), 
       aes(x = c(1:118))) + 
  geom_line(aes(y = carbon_seq_mean_yen, color = "carbon")) + 
  geom_line(aes(y = pollutant_mean_yen, color = "pollutant")) + 
  geom_line(aes(y = avo_runoff_mean_yen, color = "runoff")) +
  scale_color_manual("", values = c(
    "carbon" = "red", "pollutant" = "darkgreen", "runoff" = "blue")) + 
  ylab("mean_yen") 
# seems pollutant removal means are generally higher?

# comparion of sum of tree values
sum(Trees$`TREE VALUE ($)`)/10^6*105.37496
sum(EcoServiceValueSummary[3:10])/10^6
sum(EcoServiceValueSummary$carbon_storage_sum_yen)/10^6
# why tree value in Trees is much higher than that in EcoServiceValueSummary?

sum(SpeciesSummary$`樹木補償額合計 (円)`)/10^6
sum(SpeciesSummary$`樹木補償額平均 (円)`)/10^6
# the former is more close to sum(Trees$`TREE VALUE ($)`)

# what about the air pollutant value sums?
sum(Trees[grep("Value", names(Trees), value = TRUE)])*105.37496/10^4
sum(EcoServiceValueSummary$pollutant_sum_yen)/10^4
# they are almost the same


# whatever... compare sums of different ecosystem values
barplot(c(sum(EcoServiceValueSummary$carbon_storage_sum_yen), 
          sum(EcoServiceValueSummary$carbon_seq_sum_yen), 
          sum(EcoServiceValueSummary$pollutant_sum_yen), 
          sum(EcoServiceValueSummary$avo_runoff_sum_yen)))
barplot(c(sum(EcoServiceValueSummary$carbon_seq_sum_yen), 
          sum(EcoServiceValueSummary$pollutant_sum_yen), 
          sum(EcoServiceValueSummary$avo_runoff_sum_yen)))
barplot(c(sum(EcoServiceValueSummary$carbon_storage_mean_yen), 
          sum(EcoServiceValueSummary$carbon_seq_mean_yen), 
          sum(EcoServiceValueSummary$pollutant_mean_yen), 
          sum(EcoServiceValueSummary$avo_runoff_mean_yen)))
barplot(c(sum(EcoServiceValueSummary$carbon_seq_mean_yen), 
          sum(EcoServiceValueSummary$pollutant_mean_yen), 
          sum(EcoServiceValueSummary$avo_runoff_mean_yen)))

# compare sums of ecosystem services by species
EcoServiceValueSummary$sum_yen <- 
  EcoServiceValueSummary$carbon_seq_sum_yen + 
  EcoServiceValueSummary$pollutant_sum_yen +
  EcoServiceValueSummary$avo_runoff_sum_yen

ggplot(EcoServiceValueSummary) + geom_bar(aes(x = species, y = pollutant_sum_yen), stat = "identity") + geom_bar(aes(x = species, y = avo_runoff_sum_yen), stat = "identity", color = "red")

summary(lm(EcoServiceValueSummary$pollutant_sum_yen ~ 
             EcoServiceValueSummary$number_of_trees))
plot(EcoServiceValueSummary$number_of_trees, 
     EcoServiceValueSummary$pollutant_sum_yen)

library(tidyr)
EcoServiceValueSummaryLong <- pivot_longer(
  EcoServiceValueSummary, 
  cols = c("carbon_seq_sum_yen", "pollutant_sum_yen", "avo_runoff_sum_yen"), 
  names_to = "services", 
  values_to = "service_sum_value"
)
ggplot(EcoServiceValueSummaryLong, 
       aes(species, service_sum_value, fill = services)) + 
  geom_bar(stat = "identity", position = "fill")

ggplot(mpg, aes(fl, fill = drv))+  geom_bar(stat = "identity", position = "stack")




