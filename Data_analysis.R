library(car)
library(gplots)
library(ggplot2)
library(dplyr)
library(RODBC)
library(tidyr)
library(dunn.test)
library(openxlsx)

# Function ----
# function for data summary
func_datasummary <- function(oridata, land_class) {
  # mean of individual tree ES
  ind_data_mean <- oridata %>% 
    select({{land_class}}, carbon_storage, carbon_seq, 
           no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
           avo_runoff) %>% 
    group_by({{land_class}}) %>% 
    summarise(across(all_of(es_annual), mean)) %>% 
    pivot_longer(cols = all_of(es_annual), names_to = "ES", values_to = "mean")
  # se of individual ES
  ind_data_se <- oridata %>% 
    select({{land_class}}, carbon_storage, carbon_seq, 
           no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
           avo_runoff) %>% 
    group_by({{land_class}}) %>% 
    summarise(n = n(), across(all_of(es_annual), list(function(x) {sd(x)/sqrt(n)}))) %>% 
    mutate(n = NULL) %>% 
    rename(carbon_seq = carbon_seq_1, 
           no2_removal = no2_removal_1, 
           o3_removal = o3_removal_1, 
           pm25_removal = pm25_removal_1, 
           so2_removal = so2_removal_1, 
           avo_runoff = avo_runoff_1) %>% 
    pivot_longer(cols = all_of(es_annual), names_to = "ES", values_to = "se")
  # join the data
  ind_data_summary <- ind_data_mean %>%
    left_join(ind_data_se)
  ind_data_summary$ES <- factor(ind_data_summary$ES, levels = es_annual)
  ind_data_summary
}

# parameter method ANOVA
func_es_para <- function(var_es, name_depend_var, name_independ_var) {
  if (length(unique(var_es$species)) == 1) {
    var_species_name <- var_es$species[1]
    print(var_species_name)
  } else {
    var_species_name <- ""}
  par(mfrow = c(floor(sqrt(length(name_depend_var))),
                ceiling(sqrt(length(name_depend_var)))))
  for (var_loop_colname in name_depend_var) {
    var_loop_fit <- aov(var_es[, var_loop_colname] ~ 
                          var_es[, name_independ_var])
    var_loop_aov_pvalue <- summary(var_loop_fit)[[1]]$`Pr(>F)`[1]
    print(var_loop_colname)
    cat("anova p-value:", var_loop_aov_pvalue, "\n")
    if (var_loop_aov_pvalue < 0.05) {
      var_loop_tukey <- TukeyHSD(var_loop_fit)
      cat("Tukey result: \n")
      print(subset(as.data.frame(var_loop_tukey[[1]]), `p adj` < 0.05))
      cat("\n")
      plotmeans(var_es[, var_loop_colname] ~ var_es[, name_independ_var], 
                ylab = var_loop_colname, 
                main = var_species_name, 
                xlab = paste0("anova p-value = ", round(var_loop_aov_pvalue,2)))
    } else {
      plotmeans(var_es[, var_loop_colname] ~ var_es[, name_independ_var], 
                ylab = var_loop_colname, 
                main = var_species_name, 
                xlab = paste0("anova p-value = ", round(var_loop_aov_pvalue,2)),
                col = "grey")
    }
  }
  cat("\n\n")
  par(mfrow = c(1,1))
}

# function for non-parameter method: Kruskal test
func_es_nonpara <- function(var_es, name_depend_var, name_independ_var) {
  if (length(unique(var_es$species)) == 1) {
    var_species_name <- var_es$species[1]
    print(var_species_name)
  } else {
    var_species_name <- ""}
  par(mfrow = c(floor(sqrt(length(name_depend_var))),
                ceiling(sqrt(length(name_depend_var)))))
  for (var_loop_colname in name_depend_var) {
    var_loop_kruskal <- kruskal.test(var_es[, var_loop_colname] ~ 
                                       var_es[, name_independ_var])
    cat(var_loop_colname, ": ", var_loop_kruskal$p.value, "\n")
    if (var_loop_kruskal$p.value < 0.05) {
      capture.output(var_loop_dunn <- dunn.test(var_es[, var_loop_colname], 
                                                var_es[, name_independ_var], table = FALSE))
      print(var_loop_dunn$comparisons[var_loop_dunn$P.adjusted < 0.05])
      boxplot(var_es[, var_loop_colname] ~ var_es[, name_independ_var], 
              ylab = var_loop_colname, xlab = "", 
              main = var_species_name, 
              sub = paste0("p-value = ", round(var_loop_kruskal$p.value, 2)))
      cat("\n")
    } else {
      boxplot(var_es[, var_loop_colname] ~ var_es[, name_independ_var], 
              ylab = var_loop_colname, xlab = "", 
              main = var_species_name, 
              sub = paste0("p-value = ", round(var_loop_kruskal$p.value, 2)), 
              border = "grey")
      cat("\n")
    }
  }
}

# function of ANOVA with interaction effect
func_es_inter <- function(var_es, name_dep, 
                          name_indep1, name_indep2) {
  par(mfrow = c(floor(sqrt(length(name_dep))),
                ceiling(sqrt(length(name_dep)))))
  var_pvalue_ls <- vector("list", 6)
  var_pvalue_ls_i <- 0
  for (name_loop_dep in name_dep) {
    var_loop_dep <- var_es[, name_loop_dep]
    var_loop_indep1 <- var_es[, name_indep1]
    var_loop_indep2 <- var_es[, name_indep2]
    var_loop_fit <- aov(var_loop_dep ~ 
                          var_loop_indep1*var_loop_indep2)
    cat(name_loop_dep, "\n")
    var_loop_pvalue <- summary(var_loop_fit)[[1]]
    print(var_loop_pvalue)
    var_pvalue_ls_i <- var_pvalue_ls_i + 1
    var_pvalue_ls[[var_pvalue_ls_i]] <- var_loop_pvalue
    cat("\n\n")
    interaction.plot(var_loop_indep1, 
                     var_loop_indep2,
                     var_loop_dep,
                     xlab = "", 
                     ylab = name_loop_dep,
                     type = "b", 
                     col = c("black", "red", "violet", "orange", 
                             "green", "blue", "lightblue"))
  }
  par(mfrow = c(1,1))
  writexl::write_xlsx(var_pvalue_ls, "Out_func_es_inter.xlsx")
}

# Settings ----
es_annual <- c("carbon_seq", 
               "no2_removal", "o3_removal", "pm25_removal", "so2_removal",
               "avo_runoff")
es_annual_value <- c("carbon_seq_value", es_annual)

# Data read ----
# code book of land cover
info_abb_land_cover <- read.csv("In_abb_land_cover.csv")
names(info_abb_land_cover) <- c("land_cover_abb", "description", "land_cover")
info_abb_land_cover <- info_abb_land_cover %>% 
  select(land_cover_abb, land_cover)

# land use of quadrats, source from KUP program
info_plot <- read.csv("In_land_use.csv") %>% 
  select(KES_qua_id, Landuse_class) %>% 
  rename(qua_id = KES_qua_id, 
         land_use = Landuse_class) %>% 
  subset(qua_id != "#N/A") %>% 
  mutate(qua_id = as.numeric(qua_id))

# tree cover of each quadrat: source from KUP GIS data
info_treecover <- read.xlsx(xlsxFile = "In_GIS_Kyoto_Biodiversity_Tree_buff.xlsx")
info_treecover <- info_treecover[c("KES_qua_id", "Shape_Area")]
names(info_treecover) <- c("qua_id", "treecover")
# 问题：因为将样地边界内外的植物冠层都计算在内，所以有个样地树冠覆盖面积超过了400m2
info_treecover$treecover[info_treecover$treecover > 400] <- 400

# tree species list: downloaded from web and parsed by R
info_species_code <- read.csv("In_itree_species_list.csv") %>% 
  mutate(species = paste(Genus, Species.Name)) %>% 
  rename(species_code = SppCode, 
         genus = Genus, 
         species_name = Species.Name, 
         common_name = Common.Name) %>%
  select(species_code, species, common_name) %>% 
  subset(!duplicated(species_code)) %>% 
  subset(!duplicated(species)) %>% 
  subset(!duplicated(common_name))
info_species_code$species[info_species_code$species == " "] <- NA

# i-Tree input data: from Dr. Hirabayashi
itree_input <- read.csv("In_TreesWithID.csv") %>% 
  select(ID, PlotId, TreeId, FieldLandUse, TreeStatus, 
         Species, TreeHeightTotal, CrownWidth1, CrownWidth2, CrownLightExposure,
         PercentCrownMissing, PercentImperviousBelow, PercentShrubBelow) %>% 
  rename(res_tree_id = ID, 
         qua_id = PlotId, 
         in_tree_id = TreeId, 
         land_cover_abb = FieldLandUse, 
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
  mutate(qua_id = as.numeric(qua_id)) %>% 
  left_join(info_plot, by = "qua_id") %>% 
  left_join(info_abb_land_cover, by = "land_cover_abb") %>% 
  left_join(info_species_code, by = "species_code")

# individual ES data from Access database
my_channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
        DBQ=C:/Users/kangj/Documents/R/KES/ODS/i_Tree_results/IndividualTree.mdb")
ind_data <- sqlQuery(my_channel, "select * from [Trees]") %>% 
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
  mutate(es_annual_value = sum(carbon_seq_value, 
                        no2_value, o3_value, pm25_value, so2_value,  
                        avo_runoff_value)) %>% 
  ungroup() %>% 
  select(res_tree_id, qua_id, in_tree_id, species_code, species, common_name, 
         spo_pla, dbh, height, crown_width_ew, crown_width_ns, per_crow_mis, 
         light_expo, per_shrub_below, per_impervious_below, 
         land_use, land_cover, 
         lai, biomass, 
         carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff, 
         carbon_storage_value, carbon_seq_value, 
         no2_value, o3_value, pm25_value, so2_value,  
         avo_runoff_value, 
         es_annual_value, 
         total_value) %>% 
  as.data.frame() %>% 
  mutate(dbh_class = case_when(
    dbh <= 5 ~ "(00,05]", 
    dbh <= 10 ~ "(05,10]", 
    dbh <= 15 ~ "(10,15]", 
    dbh <= 20 ~ "(15,20]", 
    dbh <= 25 ~ "(20,25]", 
    dbh <= 30 ~ "(25,30]", 
    dbh > 30 ~ "(30,  )"
  ))
close(my_channel)
rm(my_channel)

# Data summary ----
## Individual data summary ----
inddata_summary <- vector("list", 2)
inddata_summary[[1]] <- func_datasummary(ind_data, land_use)
inddata_summary[[2]] <- func_datasummary(ind_data, land_cover)

## Quadrat data ----
# basic info: qua id, land use, tree number, tree cover
# ESs and ES values
# ESs per tree
qua_data <- ind_data %>% 
  select(qua_id, lai, biomass, 
         carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff, 
         carbon_storage_value, carbon_seq_value, 
         no2_value, o3_value, pm25_value, so2_value, 
         avo_runoff_value, 
         es_annual_value, 
         total_value) %>% 
  group_by(qua_id) %>% 
  summarise(across(!starts_with("qua_id"), sum), 
            treenum = n()) %>% 
  ungroup() %>% 
  as.data.frame() %>%
  mutate(qua_id = as.numeric(qua_id)) %>% 
  left_join(info_plot, by = "qua_id") %>% 
  left_join(info_treecover, by = "qua_id")
# 计算各个样地平均每个个体的ES
qua_esspertree <- 
  aggregate(ind_data[es_annual], by = list(ind_data$qua_id), mean)
names(qua_esspertree) <- c("qua_id", paste(es_annual, "pertree", sep = "_"))
# 合并数据
qua_data <- merge(qua_data, qua_esspertree, by = "qua_id")

# Quadrat data summary ----
quadata_summary <- func_datasummary(qua_data, land_use)


# Analysis begins ----
## DBH structure ----
# structure across land use types: calculatd based on land use scale
ggplot(ind_data) + 
  geom_bar(aes(land_use, fill = dbh_class), color = "grey", position = "fill")

## Sum of ESs value ~ land use ----
qua_data_long <- pivot_longer(
  qua_data, cols = c(grep("value", colnames(qua_data), value = TRUE)), 
  names_to = "es", values_to = "es_annual_value")
# values of sum, mean, and se
subset(qua_data_long, 
       es %in% c("es_annual_value", "total_value", "carbon_storage_value")) %>% 
  group_by(es) %>% 
  summarise(sum = sum(es_annual_value), 
            mean = mean(es_annual_value), 
            sd = sd(es_annual_value))
# sum of ESs ~ land use
ggplot(subset(qua_data_long, es %in% 
                c("es_annual_value", "total_value", "carbon_storage_value") == FALSE)) +
  geom_bar(aes(land_use, es_annual_value, fill = es), 
           stat = "identity", position = "stack")
# proportions of ESs ~ land use
ggplot(subset(qua_data_long, es %in% 
                c("es_annual_value", "total_value", "carbon_storage_value") == FALSE)) +
  geom_bar(aes(land_use, es_annual_value, fill = es), 
           stat = "identity", position = "fill")
# relationship between carbon seq ~ carbon storage
ggplot(qua_data) + geom_point(aes(carbon_storage, carbon_seq), alpha = 0.5)
summary(lm(carbon_seq ~ carbon_storage, data = qua_data))

# Non-species-specific analysis ---- 
# test the assumptions for statistical analysis
for (name_roop_dep in es_annual) {
  loop_var <- shapiro.test(qua_data[, name_roop_dep])
  cat(name_roop_dep, loop_var$p.value > 0.05, "\n")
  loop_var <- shapiro.test(ind_data[, name_roop_dep])
  cat(name_roop_dep, loop_var$p.value > 0.05, "\n")
}

# quadrat ES ~ land use
func_es_para(qua_data, es_annual, "land_use")
func_es_nonpara(qua_data, es_annual, "land_use")

# individual ES ~ land use
func_es_para(ind_data, es_annual, "land_use")
func_es_nonpara(ind_data, es_annual, "land_use")

# individual ES ~ onsite land cover
tar_land_cover_sgl <- table(ind_data$land_cover) %>% 
  as.data.frame() %>% 
  group_by(Var1) %>% 
  summarise(num = sum(Freq > 3)) %>% 
  ungroup() %>% 
  subset(num == 1) %>% 
  .$Var1 %>% 
  as.character()
func_es_para(subset(ind_data, land_cover %in% tar_land_cover_sgl), 
             es_annual, "land_cover")
func_es_nonpara(subset(ind_data, land_cover %in% tar_land_cover_sgl), 
                es_annual, "land_cover")

# individual ES ~ land use * land cover
# target land cover: wide-spread over land use types and with trees >= 3
func_var_sub <- function(var_es, name_gp, name_subgp, num_sample, num_subgp) {
  # each pair with sample size larger than or equal to 2
  var_gp_subgp_ct <- table(var_es[,name_gp], var_es[,name_subgp]) %>% 
    as.data.frame() %>% 
    subset(Freq >= num_sample)
  # each group with larger than or equal to 3 subgroups
  var_gp_ct <- var_gp_subgp_ct %>% group_by(Var1) %>% summarise(n = n()) %>% 
    subset(n >= num_subgp)
  # knock out pairs of group-subgroup with not enough sample size  
  var_gp_subgp_tar <- subset(var_gp_subgp_ct, Var1 %in% var_gp_ct$Var1)
  # get required subset from original var_es data frame
  subset(var_es, paste0(var_es[, name_gp], var_es[, name_subgp]) %in% 
           paste0(var_gp_subgp_tar$Var1, var_gp_subgp_tar$Var2))
}

func_var_sub(ind_data, "land_cover", "land_use", 3, 3) %>% 
  func_es_inter(es_annual, "land_use", "land_cover")

# Species-specific anlysis ----
# individual ES ~ land use 
func_var_sub(ind_data, "species", "land_use", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_para, es_annual, "land_use")
func_var_sub(ind_data, "species", "land_use", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_nonpara, es_annual, "land_use")

# individual ES ~ land cover 
func_var_sub(ind_data, "species", "land_cover", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_para, es_annual, "land_cover")
func_var_sub(ind_data, "species", "land_cover", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_nonpara, es_annual, "land_cover")

