ggplot(subset(qua_data_long, 
              es %in% c("es_annual_value", "total_value",
                        "carbon_storage_value") == FALSE)) +
  geom_bar(aes(land_use, es_annual_value, fill = es), 
           stat = "identity", position = "stack") + 
  labs(x = "", y = "Annual value (dolars/quadrat)")

ggplot(qua_data_summary, aes(x = land_use, y = mean)) + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3) + 
  facet_wrap(~ ES, scales = "free_y") + 
  labs(x = "Land use", y = "Quadrat ES") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(ind_data_summary, aes(x = land_use, y = mean)) + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3) + 
  facet_wrap(~ ES, scales = "free_y") + 
  labs(x = "Land use", y = "Individual ES") +
  theme(axis.text.x = element_text(angle = 90))


# ind data summary for land cover
# mean of individual tree ES
ind_data_mean <- ind_data %>% 
  select(land_cover, carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff) %>% 
  group_by(land_cover) %>% 
  summarise(across(all_of(es_annual), mean)) %>% 
  pivot_longer(cols = all_of(es_annual), names_to = "ES", values_to = "mean")
# se of individual ES
ind_data_se <- ind_data %>% 
  select(land_cover, carbon_storage, carbon_seq, 
         no2_removal, o3_removal, pm25_removal, so2_removal, co_removal, 
         avo_runoff) %>% 
  group_by(land_cover) %>% 
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
  left_join(ind_data_se, by = c("land_cover" = "land_cover", "ES" = "ES"))



ggplot(ind_data_summary, aes(x = land_cover, y = mean)) + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3) + 
  facet_wrap(~ ES, scales = "free_y") + 
  labs(x = "Onsite land cover", y = "Individual ES") +
  theme(axis.text.x = element_text(angle = 90))
