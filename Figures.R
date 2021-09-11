png(filename = "anova_es_landuse.png", res = 300, 
    width = 2500, height = 2000, type = "cairo")
ggarrange(plotlist = list(
  func_temp(quaes_summary) + 
    labs(x = "Land use", y = "Quadrat ecosystem services", title = "(a)"), 
  func_temp(indes_summary) + 
    labs(x = "Land use", y = "Single-tree ecosystem services", title = "(b)")
), ncol = 2)
dev.off()

