# code for exporting figures for article submission 

png(filename = "Manuscript/Figures/dbh.png", res = 300, 
    width = 1600, height = 1000, type = "cairo")
ggplot(inddata) + 
  geom_bar(aes(land_use, fill = dbh_class), color = "grey", position = "fill") + 
  labs(x = "Land use class", y = "Proportion", fill = "DBH class") + 
  theme_bw()
dev.off()

png(filename = "Manuscript/Figures/lai.png", res = 300, 
    width = 1600, height = 1000, type = "cairo")
ggplot(inddata) + 
  geom_bar(aes(land_use, fill = lai_class), color = "grey", position = "fill") + 
  labs(x = "Land use class", y = "Proportion", fill = "LAI class") + 
  theme_bw()
dev.off()

png(filename = "Manuscript/Figures/quadrat_valuation.png", res = 300, 
   width = 1800, height = 1200, type = "cairo")
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
dev.off()

png(filename = "Manuscript/Figures/comparison.png", res = 300, 
    width = 2000, height = 2200, type = "cairo")
ggarrange(plotlist = list(
  func_temp(quaes_summary) + 
    labs(x = "Land use", y = "Quadrat ecosystem services", title = "(a)"), 
  func_temp(indes_summary) + 
    labs(x = "Land use", y = "Single-tree ecosystem services", title = "(b)")
), ncol = 2)
dev.off()


# Appendix ---- 
png(filename = "Manuscript/Figures/quastr_comparison.png", res = 300, 
    width = 1000, height = 1200, type = "cairo")
chart_lables <- c(
  dbh = "DBH (cm)",
  lai = "LAI", 
  treenum = "Number of trees"
)
func_compplot(quastr_summary) + 
  labs(x = "Land use", y = "")
dev.off()

png(filename = "Manuscript/Figures/indstr_comparison.png", res = 300, 
    width = 1000, height = 900, type = "cairo")
chart_lables <- c(
  dbh = "DBH (cm)",
  lai = "LAI"
)
func_compplot(indstr_summary) + 
  labs(x = "Land use", y = "")
dev.off()




