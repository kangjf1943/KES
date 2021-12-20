library(car)
library(gplots)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(RODBC)
library(tidyr)
library(openxlsx)
library(dunn.test)

# Data ----
# factor level of annual ES
es_annual <- c("carbon_seq", 
               "no2_removal", "o3_removal", "pm25_removal", "so2_removal",
               "avo_runoff")

# land use of quadrats, source from KUP program
info_plot <- read.csv("Land_use.csv") %>% 
  select(KES_qua_id, Landuse_class) %>% 
  rename(qua_id = KES_qua_id, 
         land_use = Landuse_class) %>% 
  subset(qua_id != "#N/A") %>% 
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

# tree species list: downloaded from web and parsed by R
info_species_code <- read.csv("iTree_species_list.csv") %>% 
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
itree_input <- read.csv("iTree_input.csv") %>% 
  select(ID, PlotId, TreeId, FieldLandUse, TreeStatus, 
         Species, TreeHeightTotal, CrownWidth1, CrownWidth2, CrownLightExposure,
         PercentCrownMissing, PercentImperviousBelow, PercentShrubBelow) %>% 
  rename(res_tree_id = ID, 
         qua_id = PlotId, 
         in_tree_id = TreeId, 
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
  mutate(qua_id = as.factor(qua_id)) %>% 
  left_join(info_plot, by = "qua_id") %>% 
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
           levels = c("ResLow", "ResHigh", "ResOther", "Ind", "ComNbr", "Com"))
         ) %>% 
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
         land_use, 
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
  left_join(info_plot, by = "qua_id")


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

# function for non-parametric statistical analysis 
fun_comparison <- function(var_es, name_depend_var, name_independ_var) {
  var_es <- as.data.frame(var_es)
  if (length(unique(var_es$species)) == 1) {
    var_species_name <- var_es$species[1]
    print(var_species_name)
  } else {
    var_species_name <- ""}
  
  # store the statistics: chi-square and p value
  chi <- vector("numeric")
  pvalue <- vector("numeric")
  
  # prepare the canvas for boxplots 
  par(mfrow = c(floor(sqrt(length(name_depend_var))),
                ceiling(sqrt(length(name_depend_var)))))
  
  # get statistics, post hoc comparison, and plots 
  for (var_loop_colname in name_depend_var) {
    print(var_loop_colname)
    
    var_loop_fit <- 
      kruskal.test(var_es[, var_loop_colname] ~ var_es[, name_independ_var])
    chi <- c(chi, var_loop_fit$statistic)
    var_loop_aov_pvalue <- var_loop_fit$p.value
    pvalue <- c(pvalue, var_loop_aov_pvalue)
    
    if (var_loop_aov_pvalue < 0.05) {
      var_loop_tukey <- 
        dunn.test(var_es[, var_loop_colname], var_es[, name_independ_var], 
                  method = "Bonferroni")
      cat("Tukey result: \n")
      # print(subset(as.data.frame(var_loop_tukey[[1]]), `p adj` < 0.05))
      cat("\n")
      boxplot(var_es[, var_loop_colname] ~ var_es[, name_independ_var], 
                ylab = var_loop_colname, xlab = "", las = 2, 
                main = paste0(var_species_name, "\n", 
                              "anova p-value = ", round(var_loop_aov_pvalue,2)))
    } else {
      boxplot(var_es[, var_loop_colname] ~ var_es[, name_independ_var], 
                ylab = var_loop_colname, xlab = "", las = 2, 
                main = paste0(var_species_name, "\n", 
                              "anova p-value = ", round(var_loop_aov_pvalue,2)),
                border = "grey")
    }
  }
  cat("\n\n")
  par(mfrow = c(1,1))
  
  # data frame of the statistics 
  output <- data.frame(chi = chi, pvalue = pvalue)
  output$mark <- ""
  output$mark[which(output$pvalue < 0.05)] <- "*"
  output$mark[which(output$pvalue < 0.01)] <- "**"
  output$mark[which(output$pvalue < 0.001)] <- "***"
  
  return(output)
}

# quadrat ES ~ land use
fun_comparison(quadata, es_annual, "land_use")
# individual ES ~ land use
fun_comparison(inddata, es_annual, "land_use")


## Quadrat structure indexes ~ land use ----
quastr_summary <- func_summary(quadata, c("dbh", "lai", "treenum"))
quastr_summary <- 
  merge(quastr_summary,
        rbind(exfunc_label(quadata, "dbh", "land_use"),
              exfunc_label(quadata, "lai", "land_use"), 
              exfunc_label(quadata, "treenum", "land_use")))
indstr_summary <- func_summary(inddata, c("dbh", "lai", "biomass"))
indstr_summary <- 
  merge(indstr_summary,
        rbind(exfunc_label(inddata, "dbh", "land_use"),
              exfunc_label(inddata, "lai", "land_use"), 
              exfunc_label(inddata, "o3_removal", "land_use")))

# quadrat structure indexes ~ land use 
chart_lables <- c(
  dbh = "DBH (cm)",
  lai = "LAI", 
  treenum = "Number of trees"
)
func_compplot(quastr_summary) + 
  labs(x = "Land use", y = "")

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

# individual structure indexes ~ land use 
chart_lables <- c(
  dbh = "DBH (cm)",
  lai = "LAI"
)
func_compplot(indstr_summary) + 
  labs(x = "Land use", y = "")

TukeyHSD(aov(inddata$dbh ~ inddata$land_use)) %>% 
  .$`inddata$land_use` %>% 
  as.data.frame() %>% 
  .[which(.$`p adj` < 0.05), ]
TukeyHSD(aov(inddata$lai ~ inddata$land_use)) %>% 
  .$`inddata$land_use` %>% 
  as.data.frame() %>% 
  .[which(.$`p adj` < 0.05), ]


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
  lapply(fun_comparison, es_annual, "land_use") %>% 
  write.xlsx("Output_Species-specific_comparison.xlsx")

