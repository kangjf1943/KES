library(car)
library(gplots)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(RODBC)
library(tidyr)
library(dunn.test)
library(openxlsx)

# Function ----
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
func_datasummary <- function(oridata, land_class, name_land_class) {
  # mean of individual tree ES
  inddata_mean <- oridata %>% 
    select({{land_class}}, carbon_storage, carbon_seq, 
           no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
           avo_runoff) %>% 
    group_by({{land_class}}) %>% 
    summarise(across(all_of(es_annual), mean)) %>% 
    pivot_longer(cols = all_of(es_annual), names_to = "ES", values_to = "mean")
  # se of individual ES
  inddata_se <- oridata %>% 
    select({{land_class}}, carbon_storage, carbon_seq, 
           no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
           avo_runoff) %>% 
    group_by({{land_class}}) %>% 
    summarise(n = n(), across(all_of(es_annual), 
                              list(function(x) {sd(x)/sqrt(n)}))) %>% 
    mutate(n = NULL) %>% 
    rename(carbon_seq = carbon_seq_1, 
           no2_removal = no2_removal_1, 
           o3_removal = o3_removal_1, 
           pm25_removal = pm25_removal_1, 
           so2_removal = so2_removal_1, 
           avo_runoff = avo_runoff_1) %>% 
    pivot_longer(cols = all_of(es_annual), names_to = "ES", values_to = "se")
  # join the data
  data_summary <- inddata_mean %>%
    left_join(inddata_se)
  data_summary$ES <- factor(data_summary$ES, levels = es_annual)
  
  # add TukeyHSD group labels
  data_summary <- 
    merge(data_summary, 
          rbind(exfunc_label(oridata, "carbon_seq", name_land_class), 
                exfunc_label(oridata, "no2_removal", name_land_class), 
                exfunc_label(oridata, "o3_removal", name_land_class), 
                exfunc_label(oridata, "pm25_removal", name_land_class), 
                exfunc_label(oridata, "so2_removal", name_land_class), 
                exfunc_label(oridata, "avo_runoff", name_land_class)))
  
  data_summary
}

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
  var_es <- as.data.frame(var_es)
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
func_es_inter <- function(var_es, name_dep, name_indep1, name_indep2) {
  var_es <- as.data.frame(var_es)
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
    var_loop_pvalue <- summary(var_loop_fit)[[1]]
    var_pvalue_ls_i <- var_pvalue_ls_i + 1
    var_pvalue_ls[[var_pvalue_ls_i]] <- as.data.frame(var_loop_pvalue)
    # add ES and factor information
    var_pvalue_ls[[var_pvalue_ls_i]]$"Ecosystem Service" <- 
      c(name_loop_dep, rep(" ", 3))
    var_pvalue_ls[[var_pvalue_ls_i]]$"Factor" <- 
      c(name_indep1, name_indep2, paste(name_indep1, name_indep2, sep = ":"), 
        "Residuals")
    # reorder the columns
    rownames(var_pvalue_ls[[var_pvalue_ls_i]]) <- NULL
    var_pvalue_ls[[var_pvalue_ls_i]] <- 
      var_pvalue_ls[[var_pvalue_ls_i]][c(
        "Ecosystem Service", "Factor", 
        names(var_pvalue_ls[[var_pvalue_ls_i]])[1:5])]
    # interaction plot
    interaction.plot(var_loop_indep1, var_loop_indep2, var_loop_dep,
                     xlab = "", ylab = name_loop_dep,
                     type = "b", 
                     col = c("black", "red", "violet", "orange", 
                             "green", "blue", "lightblue"))
  }
  par(mfrow = c(1,1))
  pvalue_df <- Reduce(rbind, var_pvalue_ls)
  pvalue_df
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
  mutate(qua_id = as.numeric(qua_id)) %>% 
  mutate(
    land_use = case_when(
      land_use == "Com" ~ "Com", 
      land_use == "Com-neigh" ~ "ComNeigh", 
      land_use == "Ind" ~ "Ind", 
      land_use == "R-high" ~ "ResHigh", 
      land_use == "R-low" ~ "ResLow", 
      land_use == "R-other" ~ "ResOther"
    ), 
    land_use = factor(
      land_use, 
      levels = c("Com", "ComNeigh", "Ind", "ResOther", "ResHigh", "ResLow")))

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
        DBQ=C:/Users/kangj/Documents/R/KES/ODS/i_Tree_result/IndividualTree.mdb")
inddata <- sqlQuery(my_channel, "select * from [Trees]") %>% 
  select(TreeID, `DBH (CM)`, `LEAF AREA INDEX`, 
         `CARBON STORAGE (KG)`, `GROSS CARBON SEQ (KG/YR)`, `BIOMASS ADJUSTMENT`, 
         grep("\\(g\\)", colnames(.)), `TREE VALUE (Yen)`, grep("\\$", colnames(.)), 
         `Avoided Runoff (m3)`) %>%
  rename(res_tree_id = "TreeID", 
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
         compensatory_value = "TREE VALUE (Yen)",          
         no2_value = "NO2 Value ($)", 
         o3_value = "O3 Value ($)", 
         pm25_value = "PM25 Value ($)",          
         so2_value = "SO2 Value ($)", 
         avo_runoff = "Avoided Runoff (m3)") %>% 
  left_join(itree_input, by = "res_tree_id") %>% 
  mutate(compensatory_value = compensatory_value/100, 
         carbon_storage_value = 188/1000*carbon_storage, 
         carbon_seq_value = 188/1000*carbon_seq, 
         avo_runoff_value = 2.36*avo_runoff, 
         land_use = factor(
           land_use, 
           levels = c("Com", "ComNeigh", "Ind", "ResOther", "ResHigh", "ResLow")), 
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
      dbh <= 15 ~ "(00,15]", 
      dbh <= 30 ~ "(15,30]", 
      dbh > 30 ~ "(30,  )"
    ), 
    lai_class = case_when(
      lai <= 3 ~ "(0, 3]", 
      lai <= 6 ~ "(3, 6]", 
      lai <= 9 ~ "(6, 9]", 
      lai > 9 ~ "(9, )" 
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
close(my_channel)
rm(my_channel)

# Data summary ----
## Individual data summary ----
inddata_summary <- vector("list", 2)
names(inddata_summary) <- c("by_land_use", "by_land_cover")
inddata_summary[[1]] <- func_datasummary(inddata, land_use, "land_use")
inddata_summary[[2]] <- func_datasummary(inddata, land_cover, "land_cover")

## Quadrat data ----
quadata <- inddata %>% 
  select(qua_id, lai, biomass, 
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

## Quadrat data summary ----
quadata_summary <- func_datasummary(quadata, land_use, "land_use")


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

## Carbon seq ~ carbon storage ----
# at individual scale
ggplot(inddata) + 
  geom_point(aes(carbon_storage, carbon_seq, color = species), alpha = 0.5) + 
  guides(color = "none")
summary(lm(carbon_seq ~ carbon_storage, data = inddata))
# at quadrat scale 
ggplot(quadata) + 
  geom_point(aes(carbon_storage, carbon_seq, color = land_use), alpha = 0.5) 
summary(lm(carbon_seq ~ carbon_storage, data = quadata))

## Non-species-specific analysis ---- 
# test the assumptions for statistical analysis
apply(as.data.frame(quadata[es_annual]), 2, 
      function(x) {shapiro.test(x)$p.value > 0.05})
apply(as.data.frame(inddata[es_annual]), 2, 
      function(x) {shapiro.test(x)$p.value > 0.05})

# quadrat ES ~ land use
func_es_para(quadata, es_annual, "land_use")
func_es_nonpara(quadata, es_annual, "land_use")

# individual ES ~ land use
func_es_para(inddata, es_annual, "land_use")
func_es_nonpara(inddata, es_annual, "land_use")

# individual ES ~ onsite land cover
tar_land_cover_sgl <- table(inddata$land_cover) %>% 
  as.data.frame() %>% 
  group_by(Var1) %>% 
  summarise(num = sum(Freq > 3)) %>% 
  ungroup() %>% 
  subset(num == 1) %>% 
  .$Var1 %>% 
  as.character()
func_es_para(subset(inddata, land_cover %in% tar_land_cover_sgl), 
             es_annual, "land_cover")
func_es_nonpara(subset(inddata, land_cover %in% tar_land_cover_sgl), 
                es_annual, "land_cover")

# conclusion graph
ggarrange(plotlist = list(
  ggarrange(plotlist = list(
    ggplot(quadata_summary, aes(x = land_use, y = mean)) + 
      geom_bar(stat = "identity") + 
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3) + 
      geom_text(aes(x = land_use, y = Inf, label = label), 
                vjust = 0.9, size = 3) + 
      scale_y_continuous(expand = expansion(mult = c(0, 0.3))) + 
      facet_wrap(~ ES, scales = "free_y", nco = 1, strip.position = "left", 
                 labeller = labeller(ES = qua_lables)) + 
      labs(x = "Land use", y = "Quadrat ecosystem services") + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(title = "(a)"), 
    ggplot(inddata_summary[[1]], aes(x = land_use, y = mean)) + 
      geom_bar(stat = "identity") + 
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3) + 
      geom_text(aes(x = land_use, y = Inf, label = label), 
                vjust = 0.9, size = 3) + 
      scale_y_continuous(expand = expansion(mult = c(0, 0.3))) + 
      facet_wrap(~ ES, scales = "free_y", ncol = 1, strip.position = "left", 
                 labeller = labeller(ES = ind_labels)) + 
      labs(x = "Land use", y = "Individual ecosystem services") +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(title = "(b)")
  ), ncol = 2),  
  ggplot(inddata_summary[[2]], aes(x = land_cover, y = mean)) + 
    geom_bar(stat = "identity") + 
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3) + 
    geom_text(aes(x = land_cover, y = Inf, label = label), 
              vjust = 0.9, size = 3) + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.3))) + 
    facet_wrap(~ ES, scales = "free_y", ncol = 1, strip.position = "left", 
               labeller = labeller(ES = ind_labels)) + 
    labs(x = "Onsite land cover", y = "Individual ecosystem services") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) + 
    labs(title = "(c)") 
), ncol = 2, common.legend = TRUE)

# individual ES ~ land use * land cover
# target land cover: wide-spread over land use types and with trees >= 3
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

func_var_sub(inddata, "land_cover", "land_use", 3, 3) %>% 
  func_es_inter(es_annual, "land_use", "land_cover")

# Species-specific anlysis ----
# individual ES ~ land use 
func_var_sub(inddata, "species", "land_use", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_para, es_annual, "land_use")
func_var_sub(inddata, "species", "land_use", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_nonpara, es_annual, "land_use")

# individual ES ~ land cover 
func_var_sub(inddata, "species", "land_cover", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_para, es_annual, "land_cover")
func_var_sub(inddata, "species", "land_cover", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_nonpara, es_annual, "land_cover")

