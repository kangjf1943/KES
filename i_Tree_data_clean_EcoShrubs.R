indata <- read.csv("./In_EcoShrubs.csv")
names(indata) <- c("id", "area_prop", "species", "ht", "missing", "re_ht", "re_missing")
nrow(indata)

# 第一层：基于re_ht和re_missing等级对数据分类汇总
indata_agg1_1 <- aggregate(cbind(ht, missing) ~ id + species + re_ht + re_missing, data = indata, FUN = mean)
nrow(indata_agg1_1)
indata_agg1_2 <- aggregate(area_prop ~ id + species + re_ht + re_missing, data = indata, FUN = sum)
nrow(indata_agg1_2)
indata_agg1 <- cbind(indata_agg1_1, indata_agg1_2$area_prop)
names(indata_agg1)[7] <- c("area_prop")
nrow(indata_agg1)
head(indata_agg1)

# 第二层：取出面积占比大于0.5%的数据先放一边，对剩下的再基于样地和物种分类汇总
indata_agg1_part1 <- subset(indata_agg1, area_prop > 0.5)
nrow(indata_agg1_part1)
target1 <- indata_agg1_part1
indata_agg1_part2 <- subset(indata_agg1, area_prop <= 0.5)
nrow(indata_agg1_part2)
head(indata_agg1_part2)

indata_agg2_1 <- aggregate(cbind(ht, missing) ~ id +species, indata_agg1_part2, mean)
nrow(indata_agg2_1)
indata_agg2_2 <- aggregate(area_prop ~ id +species, indata_agg1_part2, sum)
nrow(indata_agg2_2)
indata_agg2 <- cbind(indata_agg2_1, indata_agg2_2$area_prop)
names(indata_agg2)[5] <- c("area_prop")
head(indata_agg2)

# 第三层：同样取出面积占比大于0.5%的数据先放一边，对剩下的再基于样地分类汇总，并且将其物种值全改为硬木
indata_agg2_part1 <- subset(indata_agg2, area_prop > 0.5)
nrow(indata_agg2_part1)
target2 <- indata_agg2_part1
indata_agg2_part2 <- subset(indata_agg2, area_prop <= 0.5)
nrow(indata_agg2_part2)
head(indata_agg2_part2)
plot(sort(indata_agg2_part2$area_prop))

indata_agg3_1 <- aggregate(cbind(ht, missing) ~ id, indata_agg2_part2, mean)
indata_agg3_2 <- aggregate(area_prop ~ id, indata_agg2_part2, sum)
nrow(indata_agg3_1)
nrow(indata_agg3_2)
indata_agg3 <- cbind(indata_agg3_1, indata_agg3_2$area_prop)
names(indata_agg3)[4] <- c("area_prop")
nrow(indata_agg3)
head(indata_agg3)
plot(sort(indata_agg3$area_prop))
del_data <- target3 <- subset(indata_agg3, area_prop <= 0.5)
target3 <- subset(indata_agg3, area_prop > 0.5)
nrow(target3)
target3$species <- c("MACLASS")

# 对目标数据作一些更改让它们格式一致
target1$re_ht <- NULL
target1$re_missing <- NULL

dim(target1)
dim(target2)
dim(target3)
sum(nrow(target1), nrow(target2), nrow(target3))

names(target1)
names(target2)
names(target3)

target <- rbind(target1, target2, target3)
target <- target[order(target$id, target$species),]
sum(target$area_prop, del_data$area_prop)
sum(target$area_prop)

write.csv(target, "In_EcoShrubs_output.csv")

In_EcoPlot <- aggregate(area_prop ~ id, target, sum)
nrow(In_EcoPlot)
head(In_EcoPlot)

write.csv(In_EcoPlot, "In_EcoPlot_output.csv")
