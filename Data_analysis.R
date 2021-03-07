## questions: 
# 四类绿地空间分布#对应我论文的土地覆盖分布？#
# 四类绿地结构差异：外推的树和树种数量，密度，多样性如richness，年龄DBH结构，叶面积密度，叶生物量分布等
# 碳储存和碳捕捉：总量，各类绿地对比#做总储存量和捕获效率的差异对比？
# 空气净化：各类污染物去除总量对比-趋势和叶面积类似#结果之间的联系#，各类对比-去除总量和去除效率排序有差异
# 径流减少：各类绿地雨水拦截总量、个体树拦截效率对比

library(car)
library(gplots)
library(ggplot2)
library(dplyr)
library(RODBC)

## import the data
# In_land_use.csv is a file copied from KUP_Plots_info.xlsx, including the plot land use class information 
info_plot <- read.csv("In_land_use.csv") %>% 
  select(KES_plot_id, Landuse_class) %>% 
  rename(plot_id = KES_plot_id, 
         landuse_class = Landuse_class) %>% 
  subset(plot_id != "#N/A") %>% 
  mutate(plot_id = as.numeric(plot_id))

# In_TreesWithID.csv is a file shared by Dr. Hirabayashi
itree_input <- read.csv("In_TreesWithID.csv") %>% 
  select(ID, PlotId, TreeId, FieldLandUse, TreeStatus, 
         Species, TreeHeightTotal, CrownWidth1, CrownWidth2, CrownLightExposure,
         PercentCrownMissing, PercentImperviousBelow, PercentShrubBelow) %>% 
  rename(res_tree_id = ID, 
         plot_id = PlotId, 
         in_tree_id = TreeId, 
         land_cover = FieldLandUse, 
         spo_pla = TreeStatus, 
         species_code = Species, 
         height = TreeHeightTotal, 
         crown_width_ew = CrownWidth1, 
         crown_width_ns = CrownWidth2, 
         light_expo = CrownLightExposure, 
         per_crow_mis = PercentCrownMissing, 
         per_impervious_below = PercentImperviousBelow, 
         per_shrub_below = PercentShrubBelow
         ) %>% 
  mutate(plot_id = as.factor(plot_id)) %>% 
  left_join(info_plot, by = "plot_id")

# data from Access database
my_channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
        DBQ=C:/Users/kangj/Documents/OneDrive/KES/Tree/IndividualTree.mdb")
tree_ind_es <- sqlQuery(my_channel, "select * from [Trees]") %>% 
  select(TreeID, `DBH (CM)`, `LEAF AREA INDEX`, 
         `CARBON STORAGE (KG)`, `GROSS CARBON SEQ (KG/YR)`, `BIOMASS ADJUSTMENT`, 
         grep("\\(g\\)", colnames(.)), grep("\\$", colnames(.)), 
         `Avoided Runoff (m3)`) %>%
  rename(res_tree_id = TreeID, 
         dbh = `DBH (CM)`, 
         lai = `LEAF AREA INDEX`, 
         carbon_storage = `CARBON STORAGE (KG)`, 
         carbon_seq = `GROSS CARBON SEQ (KG/YR)`, 
         biomass = `BIOMASS ADJUSTMENT`,
         co_removal = "CO Removal (g)",
         no2_removal = "NO2 Removal (g)", 
         o3_removal = "O3 Removal (g)", 
         pm25_removal = "PM25 Removal (g)", 
         so2_removal = "SO2 Removal (g)", 
         total_value = "TREE VALUE ($)",          
         no2_value = "NO2 Value ($)", 
         o3_value = "O3 Value ($)", 
         pm25_value = "PM25 Value ($)",          
         so2_value = "SO2 Value ($)", 
         avo_runoff = "Avoided Runoff (m3)") %>% 
  left_join(itree_input, by = "res_tree_id") %>% 
  mutate(carbon_storage_value = 51.2/10000*carbon_storage, 
         carbon_seq_value = 51.2/1000*carbon_seq, 
         avo_runoff_value = 2.36*avo_runoff) %>% 
  group_by(res_tree_id) %>% 
  mutate(es_value = sum(carbon_storage_value, carbon_seq_value, 
                        no2_value, o3_value, pm25_value, so2_value,  
                        avo_runoff_value)) %>% 
  ungroup() %>% 
  select(res_tree_id, plot_id, in_tree_id, species_code, 
         spo_pla, dbh, height, crown_width_ew, crown_width_ns, per_crow_mis, 
         light_expo, per_shrub_below, per_impervious_below, 
         landuse_class, land_cover, 
         lai, biomass, 
         carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff, 
         carbon_storage_value, carbon_seq_value, 
         no2_value, o3_value, pm25_value, so2_value,  
         avo_runoff_value, 
         es_value, 
         total_value) 
species_summary <- sqlQuery(my_channel, "select * from [SpeciesSummary]")
close(my_channel)
Sys.setlocale(category = "LC_ALL", locale = "Japanese")
sum(species_summary$`樹木補償額合計 (円)`)
# 5,705,830,792
# 1 dollar = 105.37496 yen in input i-Tree data
sum(tree_ind_es$total_value)*105.37496
# 5,465,924,468
# 1 dollar = 108.40 yen on Mar. 7, 2021
sum(tree_ind_es$total_value)*108.40
# 5,622,836,890
sum(tree_ind_es$es_value)*105.37496
# 373,879.9
sum(tree_ind_es$es_value)*108.40
# 384,613

# ES of each plot
tree_plot_es <- tree_ind_es %>% 
  select(plot_id, lai, biomass, 
         carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff, 
         carbon_storage_value, carbon_seq_value, 
         no2_value, o3_value, pm25_value, so2_value,  
         avo_runoff_value, 
         es_value, 
         total_value) %>% 
  group_by(plot_id) %>% 
  summarise(across(!starts_with("plot_id"), sum)) %>% 
  ungroup() %>% 
  as.data.frame() %>%
  mutate(plot_id = as.numeric(plot_id)) %>% 
  left_join(info_plot, by = "plot_id")






