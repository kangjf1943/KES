# make a comprihensive datafram of each tree first

plant_data <- read.csv("In_plant_info.csv")
subset(plant_data, Width_EW == 150 & Width_NS == 180)$DBH.1

sum(plant_data$HT/100 != Trees[order(Trees$TreeID),]$`HEIGHT (M)`)
# 其中一个不符合？看看是哪个：
plant_data[plant_data$HT/100 != Trees[order(Trees$TreeID),]$`HEIGHT (M)`,]
Trees[plant_data$HT/100 != Trees[order(Trees$TreeID),]$`HEIGHT (M)`,]

plot(plant_data$Width_NS/100 / Trees[order(Trees$TreeID),]$CrownWidthNS)
sum(plant_data$Width_EW/100 != Trees[order(Trees$TreeID),]$CrownWidthEW)

input_tree_data <- read.csv("In_input_tree_data.csv")
plot(input_tree_data$TreeId)
sum(input_tree_data$TreeHeightLiveTop == Trees[order(Trees$TreeID),]$`HEIGHT (M)`)
sum(input_tree_data$CrownWidth1 != Trees[order(Trees$TreeID),]$CrownWidthEW)

newid <- 
  Trees$CrownWidthEW * Trees$CrownWidthNS + 
  Trees$`HEIGHT (M)` + 
  Trees$`DBH (CM)`^5 + 
  Trees$BaseHt^5
length(unique(newid))

# 思路：一层层筛选
# 先把能通过唯一值筛选出来的行挑出来
length(names(table(Trees$CrownWidthEW))[table(Trees$CrownWidthEW) == 1])
length(unique(Trees$CrownWidthEW))


sum(signif(as.numeric(names(table(Trees$CrownWidthEW))[table(Trees$CrownWidthEW) == 1]), 2) %in%
      signif(as.numeric(names(table(input_tree_data$CrownWidth2))[table(input_tree_data$CrownWidth2) == 1]), 2) == FALSE)

sum(signif(as.numeric(names(table(Trees$CrownWidthEW))[table(Trees$CrownWidthEW) == 1]), 2) %in%
      signif(as.numeric(names(table(input_tree_data$CrownWidth1))[table(input_tree_data$CrownWidth1) == 1]), 2) == FALSE)
# 可见Trees中的CrownWidthEW唯一值对应着input_tree_data中的CrownWidth1唯一值，这个列名就可以作为这些行的唯一识别符


