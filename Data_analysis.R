library(car)
library(gplots)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(RODBC)
library(tidyr)
library(openxlsx)

# Data ----
# factor level of annual ES
es_annual <- c("carbon_seq", 
               "no2_removal", "o3_removal", "pm25_removal", "so2_removal",
               "avo_runoff")

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
  mutate(qua_id = as.numeric(qua_id)) %>% 
  mutate(
    land_use = case_when(
      land_use == "Com" ~ "Com", 
      land_use == "Com-neigh" ~ "ComNbr", 
      land_use == "Ind" ~ "Ind", 
      land_use == "R-high" ~ "ResHigh", 
      land_use == "R-low" ~ "ResLow", 
      land_use == "R-other" ~ "ResOther"
    ), 
    land_use = factor(
      land_use, 
      levels = c("ResLow", "ResHigh", "ResOther", "Ind", "ComNbr", "Com")))

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

# individual ES data from Excel file
# The Excel file is from Access data
inddata <- read.xlsx("Trees.xlsx", sheet = "Trees") %>% 
  rename(res_tree_id = "TreeID", 
         dbh = "DBH.(CM)", 
         lai = "LEAF.AREA.INDEX", 
         carbon_storage = "CARBON.STORAGE.(KG)", 
         carbon_seq = `GROSS.CARBON.SEQ.(KG/YR)`, 
         biomass = `BIOMASS.ADJUSTMENT`,
         co_removal = "CO.Removal.(g)",
         no2_removal = "NO2.Removal.(g)", 
         o3_removal = "O3.Removal.(g)", 
         pm25_removal = "PM25.Removal.(g)", 
         so2_removal = "SO2.Removal.(g)", 
         compensatory_value = "TREE.VALUE.(Yen)",          
         no2_value = "NO2.Value.($)", 
         o3_value = "O3.Value.($)", 
         pm25_value = "PM25.Value.($)",          
         so2_value = "SO2.Value.($)", 
         avo_runoff = "Avoided.Runoff.(m3)") %>% 
  left_join(itree_input, by = "res_tree_id") %>% 
  mutate(compensatory_value = compensatory_value/100, 
         carbon_storage_value = 188/1000*carbon_storage, 
         carbon_seq_value = 188/1000*carbon_seq, 
         avo_runoff_value = 2.36*avo_runoff, 
         land_use = factor(
           land_use, 
           levels = c("ResLow", "ResHigh", "ResOther", "Ind", "ComNbr", "Com")), 
         land_cover = factor(
           land_cover, 
           levels = c("ComInd", "ComNeiBld", "Trans", "Insti", 
                      "MulFamiRes", "LowResBld", "Park", "TemShr", 
                      "Golf", "Vacant", "Water", "Agri", "Cemetery"))) %>% 
  group_by(res_tree_id) %>% 
  mutate(es_annual_value = sum(carbon_seq_value, 
                        no2_value, o3_value, pm25_value, so2_value,  
                        avo_runoff_value)) %>% 
  ungroup() %>% 
  mutate(
    dbh_class = case_when(
      dbh <= 15 ~ "0 < DBH ≤ 15", 
      dbh <= 30 ~ "15 < DBH ≤ 30", 
      dbh > 30 ~ "DBH > 30"
    ), 
    lai_class = case_when(
      lai <= 3 ~ "0 < LAI ≤ 3", 
      lai <= 6 ~ "3 < LAI ≤ 6", 
      lai <= 9 ~ "6 < LAI ≤ 9", 
      lai > 9 ~ "LAI > 9" 
    )
  ) %>% 
  select(res_tree_id, qua_id, in_tree_id, species_code, species, common_name, 
         spo_pla, dbh, dbh_class, height, crown_width_ew, crown_width_ns,
         per_crow_mis, light_expo, per_shrub_below, per_impervious_below, 
         lai, lai_class, biomass, 
         land_use, land_cover, 
         carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff, 
         compensatory_value, 
         carbon_storage_value, carbon_seq_value, 
         no2_value, o3_value, pm25_value, so2_value,  
         avo_runoff_value, 
         es_annual_value)

# Quadrat data
quadata <- inddata %>% 
  select(qua_id, dbh, lai, biomass, 
         carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff, 
         compensatory_value, 
         carbon_storage_value, carbon_seq_value, 
         no2_value, o3_value, pm25_value, so2_value, 
         avo_runoff_value, 
         es_annual_value) %>% 
  group_by(qua_id) %>% 
  summarise(across(!starts_with("qua_id"), sum), 
            treenum = n()) %>% 
  ungroup() %>% 
  mutate(qua_id = as.numeric(qua_id)) %>% 
  left_join(info_plot, by = "qua_id") %>% 
  left_join(info_treecover, by = "qua_id")


# Analysis ----
## DBH structure ----
# structure across land use types: calculatd based on land use scale
ggplot(inddata) + 
  geom_bar(aes(land_use, fill = dbh_class), color = "grey", position = "fill") + 
  labs(x = "Land use class", y = "Proportion", fill = "DBH class") + 
  theme_bw()

## LAI structure ----
# structure across land use types: calculatd based on land use scale
ggplot(inddata) + 
  geom_bar(aes(land_use, fill = lai_class), color = "grey", position = "fill") + 
  labs(x = "Land use class", y = "Proportion", fill = "LAI class") + 
  theme_bw()

## Qua avg ESs value ~ land use ----
quavalue_summary <- quadata %>% 
  select(land_use, carbon_seq_value, 
         no2_value, o3_value, pm25_value, so2_value, 
         avo_runoff_value) %>% 
  pivot_longer(cols = c(carbon_seq_value, 
                        no2_value, o3_value, pm25_value, so2_value, 
                        avo_runoff_value), 
               names_to = "es", values_to = "es_value") %>%
  group_by(land_use, es) %>% 
  summarise(es_value = mean(es_value)) %>% 
  ungroup() %>%
  mutate(es = factor(
    es, levels = c("carbon_seq_value", 
                   "no2_value", "o3_value", "pm25_value", "so2_value", 
                   "avo_runoff_value")))
ggplot(quavalue_summary, aes(land_use, es_value)) + 
  geom_bar(aes(fill = es), stat = "identity", position = "stack") + 
  scale_fill_discrete(
    limits = c("carbon_seq_value", "no2_value", 
               "o3_value", "pm25_value", 
               "so2_value", "avo_runoff_value"), 
    labels = c("Carbon sequestration", "NO2 removal", 
               "O3 removal", "PM2.5 removal", 
               "SO2 removal", "Avoided runoff")) + 
  labs(x = "Land use", y = "Quadrat ecosystem service values (dollars / year)", 
       fill = "Ecosystem service") + 
  theme_bw()
ggplot(quavalue_summary, aes(land_use, es_value)) + 
  geom_bar(aes(fill = es), stat = "identity", position = "fill")

## Quadrat and individual ESs ~ land use ---- 
# test the assumptions for statistical analysis
apply(as.data.frame(quadata[es_annual]), 2, 
      function(x) {shapiro.test(x)$p.value > 0.05})
apply(as.data.frame(inddata[es_annual]), 2, 
      function(x) {shapiro.test(x)$p.value > 0.05})

# quadrat structure indexes ~ land use
TukeyHSD(aov(quadata$dbh ~ quadata$land_use)) %>% 
  .$`quadata$land_use` %>% 
  as.data.frame() %>% 
  .[which(.$`p adj` < 0.05), ]

TukeyHSD(aov(quadata$lai ~ quadata$land_use)) %>% 
  .$`quadata$land_use` %>% 
  as.data.frame() %>% 
  .[which(.$`p adj` < 0.05), ]

TukeyHSD(aov(quadata$treenum ~ quadata$land_use)) %>% 
  .$`quadata$land_use` %>% 
  as.data.frame() %>% 
  .[which(.$`p adj` < 0.05), ]

# parameter method ANOVA
func_es_para <- function(var_es, name_depend_var, name_independ_var) {
  var_es <- as.data.frame(var_es)
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
                ylab = var_loop_colname, xlab = "", las = 2, 
                main = paste0(var_species_name, "\n", 
                              "anova p-value = ", round(var_loop_aov_pvalue,2)))
    } else {
      plotmeans(var_es[, var_loop_colname] ~ var_es[, name_independ_var], 
                ylab = var_loop_colname, xlab = "", las = 2, 
                main = paste0(var_species_name, "\n", 
                              "anova p-value = ", round(var_loop_aov_pvalue,2)),
                col = "grey")
    }
  }
  cat("\n\n")
  par(mfrow = c(1,1))
}

# quadrat ES ~ land use
func_es_para(quadata, es_annual, "land_use")

# individual structure indexes ~ land use
TukeyHSD(aov(inddata$dbh ~ inddata$land_use)) %>% 
  .$`inddata$land_use` %>% 
  as.data.frame() %>% 
  .[which(.$`p adj` < 0.05), ]

TukeyHSD(aov(inddata$lai ~ inddata$land_use)) %>% 
  .$`inddata$land_use` %>% 
  as.data.frame() %>% 
  .[which(.$`p adj` < 0.05), ]

TukeyHSD(aov(inddata$biomass ~ inddata$land_use)) %>% 
  .$`inddata$land_use` %>% 
  as.data.frame() %>% 
  .[which(.$`p adj` < 0.05), ]

# individual ES ~ land use
func_es_para(inddata, es_annual, "land_use")

# Visualization
# function to get group labels for individual ES ANOVA
exfunc_label <- function(mydata, name_es, name_group){
  # ANOVA
  HSD <- 
    TukeyHSD(aov(mydata[[name_es]] ~ mydata[[name_group]]), ordered = FALSE)
  # extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- HSD$`mydata[[name_group]]`[,4]
  Tukey.labels <- multcompView::multcompLetters(Tukey.levels)['Letters']
  plot.labels <- names(Tukey.labels[['Letters']])
  
  # data.frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
                            stringsAsFactors = FALSE)
  rownames(plot.levels) <- NULL
  
  # add ES item and rename the columns
  names(plot.levels) <- c(name_group, "label")
  plot.levels$ES <- name_es
  
  return(plot.levels)
}

# function for data summary
func_essummary <- function(oridata) {
  # mean of individual tree ES
  data_mean <- oridata %>% 
    select(land_use, carbon_storage, all_of(es_annual)) %>% 
    pivot_longer(cols = c(carbon_storage, all_of(es_annual)), 
                 names_to = "ES", values_to = "value") %>% 
    group_by(land_use, ES) %>% 
    summarise(mean = mean(value), .groups = "keep") %>% 
    ungroup()
  
  # se of individual ES
  data_se <- oridata %>% 
    select(land_use, carbon_storage, all_of(es_annual)) %>% 
    pivot_longer(cols = all_of(c("carbon_storage", es_annual)), 
                 names_to = "ES", values_to = "value") %>% 
    group_by(land_use, ES) %>% 
    summarise(n = n(), se = sd(value)/sqrt(n), .groups = "keep") %>% 
    ungroup() %>% 
    mutate(n = NULL)
  
  # join the data
  data_summary <- left_join(data_mean, data_se)
  data_summary$ES <- factor(data_summary$ES, 
                            levels = c("carbon_storage", es_annual))
  
  # add TukeyHSD group labels
  data_summary <- 
    merge(data_summary, 
          rbind(exfunc_label(oridata, "carbon_storage", "land_use"),
                exfunc_label(oridata, "carbon_seq", "land_use"), 
                exfunc_label(oridata, "no2_removal", "land_use"), 
                exfunc_label(oridata, "o3_removal", "land_use"), 
                exfunc_label(oridata, "pm25_removal", "land_use"), 
                exfunc_label(oridata, "so2_removal", "land_use"), 
                exfunc_label(oridata, "avo_runoff", "land_use")))
  data_summary
}

indes_summary <- func_essummary(inddata)
quaes_summary <- func_essummary(quadata)

chart_lables <- c(
  carbon_storage = "Carbon \n storage \n (kg)",
  carbon_seq = "Carbon \n sequestration \n (kg)",
  no2_removal = "NO2 \n removal \n (g)",
  o3_removal = "O3 \n removal \n (g)", 
  pm25_removal = "PM2.5 \n removal \n (g)", 
  so2_removal = "SO2 \n removal \n (g)", 
  avo_runoff = "Runoff \n reduction \n (m3)"
)
func_temp <- function(x) {
  ggplot(x, aes(x = land_use, y = mean)) + 
    geom_bar(stat = "identity") + 
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3) + 
    geom_text(aes(x = land_use, y = Inf, label = label), 
              vjust = 1.1, size = 3) + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.4))) + 
    facet_wrap(~ ES, scales = "free_y", ncol = 1, strip.position = "left", 
               labeller = labeller(ES = chart_lables)) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
}
ggarrange(plotlist = list(
  func_temp(quaes_summary) + 
    labs(x = "Land use", y = "Quadrat ecosystem services", title = "(a)"), 
  func_temp(indes_summary) + 
    labs(x = "Land use", y = "Single-tree ecosystem services", title = "(b)")
), ncol = 2)

## Carbon seq ~ carbon storage ----
# at quadrat scale 
ggplot(quadata) + 
  geom_point(aes(carbon_storage, carbon_seq, color = land_use), alpha = 0.5) 
summary(lm(carbon_seq ~ carbon_storage, data = quadata))

# at individual scale
ggplot(inddata) + 
  geom_point(aes(carbon_storage, carbon_seq, color = species), alpha = 0.5) + 
  guides(color = "none")
summary(lm(carbon_seq ~ carbon_storage, data = inddata))

# Species-specific analysis ----
# function to select target species and land use 
func_var_sub <- function(var_es, name_gp, name_subgp, num_sample, num_subgp) {
  var_es <- as.data.frame(var_es)
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
# individual ES ~ land use 
func_var_sub(inddata, "species", "land_use", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_para, es_annual, "land_use")

