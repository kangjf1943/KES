library(car)
library(gplots)
library(ggplot2)
library(dplyr)
library(RODBC)
library(tidyr)
library(dunn.test)

## default settings
es_annual <- c("carbon_seq", 
               "no2_removal", "o3_removal", "pm25_removal", "so2_removal",
               "avo_runoff")
es_annual_value <- c("carbon_seq_value", 
                     "no2_value", "o3_value", "pm25_value", "so2_value", 
                     "avo_runoff_value")

## data import 
# In_land_use.csv is copied from KUP_Plots_info.xlsx, including the plot land use class information 
info_abb_land_cover <- read.csv("In_abb_land_cover.csv")
names(info_abb_land_cover) <- c("land_cover_abb", "description", "land_cover")
info_abb_land_cover <- info_abb_land_cover %>% 
  select(land_cover_abb, land_cover)
info_plot <- read.csv("In_land_use.csv") %>% 
  select(KES_plot_id, Landuse_class) %>% 
  rename(plot_id = KES_plot_id, 
         land_use = Landuse_class) %>% 
  subset(plot_id != "#N/A") %>% 
  mutate(plot_id = as.numeric(plot_id))

# In_itree_species_list.csv is downloaded from web, parsed by R
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

# In_TreesWithID.csv is from Dr. Hirabayashi - i-Tree input data
itree_input <- read.csv("In_TreesWithID.csv") %>% 
  select(ID, PlotId, TreeId, FieldLandUse, TreeStatus, 
         Species, TreeHeightTotal, CrownWidth1, CrownWidth2, CrownLightExposure,
         PercentCrownMissing, PercentImperviousBelow, PercentShrubBelow) %>% 
  rename(res_tree_id = ID, 
         plot_id = PlotId, 
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
  mutate(plot_id = as.numeric(plot_id)) %>% 
  left_join(info_plot, by = "plot_id") %>% 
  left_join(info_abb_land_cover, by = "land_cover_abb") %>% 
  left_join(info_species_code, by = "species_code")

# individual ES data from Access database
my_channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
        DBQ=C:/Users/kangj/Documents/OneDrive/KES/ODS/Tree/IndividualTree.mdb")
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
  mutate(es_annual_value = sum(carbon_seq_value, 
                        no2_value, o3_value, pm25_value, so2_value,  
                        avo_runoff_value)) %>% 
  ungroup() %>% 
  select(res_tree_id, plot_id, in_tree_id, species_code, species, common_name, 
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
  as.data.frame()
tree_ind_es$dbh_class <- "(50,  )"
tree_ind_es$dbh_class[tree_ind_es$dbh <= 50] <- "(45,50]"
tree_ind_es$dbh_class[tree_ind_es$dbh <= 45] <- "(40,45]"
tree_ind_es$dbh_class[tree_ind_es$dbh <= 40] <- "(35,40]"
tree_ind_es$dbh_class[tree_ind_es$dbh <= 35] <- "(30,35]"
tree_ind_es$dbh_class[tree_ind_es$dbh <= 30] <- "(25,30]"
tree_ind_es$dbh_class[tree_ind_es$dbh <= 25] <- "(20,25]"
tree_ind_es$dbh_class[tree_ind_es$dbh <= 20] <- "(15,20]"
tree_ind_es$dbh_class[tree_ind_es$dbh <= 15] <- "(10,15]"
tree_ind_es$dbh_class[tree_ind_es$dbh <= 10] <- "(05,10]"
tree_ind_es$dbh_class[tree_ind_es$dbh <= 5] <- "(00,05]"
close(my_channel)

# ES of each plot
tree_plot_es <- tree_ind_es %>% 
  select(plot_id, lai, biomass, 
         carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff, 
         carbon_storage_value, carbon_seq_value, 
         no2_value, o3_value, pm25_value, so2_value,  
         avo_runoff_value, 
         es_annual_value, 
         total_value) %>% 
  group_by(plot_id) %>% 
  summarise(across(!starts_with("plot_id"), sum)) %>% 
  ungroup() %>% 
  as.data.frame() %>%
  mutate(plot_id = as.numeric(plot_id)) %>% 
  left_join(info_plot, by = "plot_id")


## analysis begins
## general description

## structure across land use types
plot_dbh <- ggplot(tree_ind_es) + geom_bar(aes(land_use, fill = dbh_class), 
                               position = "fill")
plot_dbh
tree_ind_landuse <- tree_ind_es %>% group_by(land_use) %>% 
  summarise(num_ind = n())
tree_dbh_str <- tree_ind_es %>% 
  group_by(land_use) %>% 
  summarise("n(00,05]" = sum(dbh_class == "(00,05]"), 
            "n(05,10]" = sum(dbh_class == "(05,10]"), 
            "n(10,15]" = sum(dbh_class == "(10,15]"), 
            "n(15,20]" = sum(dbh_class == "(15,20]"), 
            "n(20,25]" = sum(dbh_class == "(20,25]"), 
            "n(25,30]" = sum(dbh_class == "(25,30]"), 
            "n(30,35]" = sum(dbh_class == "(30,35]"), 
            "n(35,40]" = sum(dbh_class == "(35,40]"), 
            "n(40,45]" = sum(dbh_class == "(40,45]"), 
            "n(45,50]" = sum(dbh_class == "(45,50]"), 
            "n(50,  )" = sum(dbh_class == "(50,  )")) %>% 
  ungroup() %>% 
  left_join(tree_ind_landuse, by = "land_use")
tree_dbh_str <- cbind(tree_dbh_str$land_use, 
  round(tree_dbh_str[, grep("n\\(", colnames(tree_dbh_str))]/tree_dbh_str$num_ind, 2))
tree_dbh_str

## compare sum of each ES value
tree_plot_es_long <- pivot_longer(
  tree_plot_es, cols = c(grep("value", colnames(tree_plot_es), value = TRUE)), 
  names_to = "es", values_to = "es_annual_value")

subset(tree_plot_es_long, 
       es %in% c("es_annual_value", "total_value", "carbon_storage_value")) %>% 
  group_by(es) %>% 
  summarise(sum = sum(es_annual_value), 
            mean = mean(es_annual_value), 
            sd = sd(es_annual_value))

plot_es_annual <- ggplot(subset(tree_plot_es_long, 
                                es %in% c("es_annual_value", "total_value",
                                          "carbon_storage_value") == FALSE)) +
  geom_bar(aes(land_use, es_annual_value, fill = es), 
           stat = "identity", position = "stack")
plot_es_annual
plot_es_annual_prop <- ggplot(subset(tree_plot_es_long, 
                                es %in% c("es_annual_value", "total_value",
                                          "carbon_storage_value") == FALSE)) +
  geom_bar(aes(land_use, es_annual_value, fill = es), 
           stat = "identity", position = "fill")
plot_es_annual_prop
ggplot(tree_plot_es) + geom_line(aes(plot_id, carbon_seq)) +
  geom_line(aes(plot_id, carbon_storage), color = "red")


## non-species-specific analysis 
## individual ES and plot ES across land use or land use cover
# functions for test
# function for parameter method - ANOVA
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
# function for non-parameter method - Kruskal test
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
  for (name_loop_dep in name_dep) {
    var_loop_dep <- var_es[, name_loop_dep]
    var_loop_indep1 <- var_es[, name_indep1]
    var_loop_indep2 <- var_es[, name_indep2]
    var_loop_fit <- aov(var_loop_dep ~ 
                          var_loop_indep1*var_loop_indep2)
    cat(name_loop_dep, "\n")
    var_loop_pvalue <- summary(var_loop_fit) %>% print()
    cat("\n\n")
    interaction.plot(var_loop_indep1, 
                     var_loop_indep2,
                     var_loop_dep,
                     xlab = "", ylab = name_loop_dep,
                     type = "b", 
                     col = c("black", "red", "violet", "orange", 
                             "green", "blue", "lightblue"))
  }
  par(mfrow = c(1,1))
}

# plot ES ~ land use
func_es_para(tree_plot_es, es_annual, "land_use")
func_es_nonpara(tree_plot_es, es_annual, "land_use")

# individual ES ~ land use
func_es_para(tree_ind_es, es_annual, "land_use")
func_es_nonpara(tree_ind_es, es_annual, "land_use")

# individual ES ~ land cover
tar_land_cover_sgl <- table(tree_ind_es$land_cover) %>% 
  as.data.frame() %>% 
  group_by(Var1) %>% 
  summarise(num = sum(Freq > 3)) %>% 
  ungroup() %>% 
  subset(num == 1) %>% 
  .$Var1 %>% 
  as.character()
func_es_para(subset(tree_ind_es, land_cover %in% tar_land_cover_sgl), 
             es_annual, "land_cover")
func_es_nonpara(subset(tree_ind_es, land_cover %in% tar_land_cover_sgl), 
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

func_var_sub(tree_ind_es, "land_cover", "land_use", 3, 3) %>% 
  func_es_inter(es_annual, "land_use", "land_cover")

## species-specific analysis
# individual ES ~ land use 
func_var_sub(tree_ind_es, "species", "land_use", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_para, es_annual, "land_use")
func_var_sub(tree_ind_es, "species", "land_use", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_nonpara, es_annual, "land_use")

# individual ES ~ land cover 
func_var_sub(tree_ind_es, "species", "land_cover", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_para, es_annual, "land_cover")
func_var_sub(tree_ind_es, "species", "land_cover", 3, 4) %>% 
  split(.$species) %>% 
  lapply(func_es_nonpara, es_annual, "land_cover")


