rm(list=ls())
##--## Compare satelitte and logger time series ##--##

#### Setup ####

library(tidyverse)

# Painful data cleaning!

surface <- readxl::read_excel("IUI_vs_NOAA_temp_comp_2.xlsx", n_max = 3988) %>% 
  select(1:7) %>% 
  rename(Date = Date...1, `SST_IUI Avg` = Avg...4, `SST_NOAA Avg` = Avg...7)

deepest1 <- readxl::read_excel("2018_Jul-Sep_temp_60m_SST_10min_interval.xlsx", 
                               col_types = c("skip", "date", "skip", "numeric", "skip"), 
                               n_max = 192, col_names = c("Date", "Temperature"), skip = 1) %>% 
  mutate(Date = as.character(Date) %>% as.Date(format = "%Y-%d-%m"))

deepest <- readxl::read_excel("2018_Jul-Sep_temp_60m_SST_10min_interval.xlsx", 
                              col_types = c("skip", "text", "skip", "numeric", "skip"), 
                              skip = 193, col_names = colnames(deepest1)) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>% 
  bind_rows(deepest1) %>% 
  mutate(Depth = "60m")
  
deep <- readxl::read_excel("IUI_vs_NOAA_temp_comp_2.xlsx") 

deep2 <- map_dfr(list(c(9,10), c(12,13), c(15,16)), ~{
    select(deep, .x) %>%
    mutate(Depth = str_remove(colnames(.)[1], "Temperature_")) %>% 
    rename(Temperature = 1, Date = 2) %>% 
    drop_na(Date) }) %>%
  bind_rows(deepest) %>% 
  group_by(Date, Depth) %>% 
  summarise(IUI_min = min(Temperature, na.rm = T),
            IUI_max = max(Temperature, na.rm = T),
            IUI_avg = mean(Temperature, na.rm = T))
  
        
ggplot(surface) +
  geom_path(aes(x = Date, y = `SST_IUI min`))

ggplot(deep2) +
  geom_point(aes(x = Date, y = IUI_avg, colour = Depth))

#### Compare surface max to satellite ####

deep3 <- pivot_wider(deep2, id_cols = Date, names_from = Depth, values_from = c(IUI_min, IUI_max, IUI_avg)) %>% 
  left_join(surface)

coef2 <- ccf(deep3$`IUI_max_2m`, deep3$`SST_NOAA max`, na.action = na.pass)
max(coef2$acf)                              # Whats the best correlation?
best2 <- which(coef2$acf == max(coef2$acf))   # Where is it?
coef2$lag[best2]                             # How much of a lag is that?

coef10 <- ccf(deep3$`IUI_max_10m`, deep3$`SST_NOAA max`, na.action = na.pass)
max(coef10$acf)                              # Whats the best correlation?
best10 <- which(coef10$acf == max(coef10$acf))   # Where is it?
coef10$lag[best10]                             # How much of a lag is that?

coef45 <- ccf(deep3$`IUI_max_45m`, deep3$`SST_NOAA max`, na.action = na.pass)
max(coef45$acf)                              # Whats the best correlation?
best45 <- which(coef45$acf == max(coef45$acf))   # Where is it?
coef45$lag[best45]                             # How much of a lag is that?

coef60 <- ccf(deep3$`IUI_max_60m`, deep3$`SST_NOAA max`, na.action = na.pass)
max(coef60$acf)                                  # Whats the best correlation?
best60 <- which(coef60$acf == max(coef60$acf))   # Where is it?
coef60$lag[best60]                               # How much of a lag is that?


deep4 <- mutate(deep3, Diff_max_2 = `SST_NOAA max` - `IUI_max_2m`,
                Diff_max_10 = `SST_NOAA max` - `IUI_max_10m`,
                Diff_max_45 = `SST_NOAA max` - `IUI_max_45m`,
                Diff_max_60 = `SST_NOAA max` - `IUI_max_60m`) %>% 
  select(starts_with("Diff_max")) %>% 
  pivot_longer(starts_with("Diff_max"), names_to = "logger", values_to = "error") %>% 
  mutate(logger = case_when(str_detect(logger, "2") ~ "2 m",
                            str_detect(logger, "10") ~ "10 m",
                            str_detect(logger, "45") ~ "45 m",
                            str_detect(logger, "60") ~ "60 m",
                            T  ~ "SST"),
         Correlation = case_when(str_detect(logger, "2") ~ max(coef2$acf),
                                 str_detect(logger, "10") ~ max(coef10$acf),
                                 str_detect(logger, "45") ~ max(coef45$acf),
                                 str_detect(logger, "60") ~ max(coef60$acf),
                                 T  ~ max(look$acf))) %>% 
  group_by(logger) %>% 
  mutate(days = sum(is.finite(error))) %>% 
  ungroup() %>% 
  mutate(logger = factor(logger, levels = c("SST", "2 m", "10 m", "45 m", "60 m")),
         direction = ifelse(error < 0, "Too cold", "Too hot"))

ggplot(deep4) +
  geom_histogram(aes(x = error, fill = direction)) +
  facet_grid(rows = vars(logger)) +
  geom_text(aes(x = -1.5, y = 125, label = str_glue("CCF = {round(Correlation, 3)}
                                                  n = {days}")), size = 3, hjust = 0) +
  geom_vline(aes(xintercept = 0)) +
  labs(x = "Satellite error (°C)", y = "Days", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("cadetblue3", "firebrick3")) +
  NULL

ggsave("satellite_max.png", width = 18, height = 10, units = "cm")

## Check there isn't a seasonal component ##

deep5 <- mutate(deep4, month = lubridate::month(Date))

ggplot(filter(deep5, month %in% 7:8)) +
  geom_histogram(aes(x = error, fill = as.factor(month))) +
  #facet_grid(rows = vars(logger), cols = vars(month)) +
  facet_grid(rows = vars(logger)) +
  geom_text(aes(x = -1.5, y = 125, label = str_glue("CCF = {round(Correlation, 3)}
                                                  n = {days}")), size = 3, hjust = 0) +
  geom_vline(aes(xintercept = 0)) +
  labs(x = "Satellite error (°C)", y = "Days") +
  theme_minimal() +
  theme(legend.position = "top") +
 # scale_fill_manual(values = c("cadetblue3", "firebrick3")) +
  NULL

#### Compare averages ####

coef2 <- ccf(deep3$`IUI_avg_2m`, deep3$`SST_NOAA Avg`, na.action = na.pass)
max(coef2$acf)                              # Whats the best correlation?
best2 <- which(coef2$acf == max(coef2$acf))   # Where is it?
coef2$lag[best2]                             # How much of a lag is that?

coef10 <- ccf(deep3$`IUI_avg_10m`, deep3$`SST_NOAA Avg`, na.action = na.pass)
max(coef10$acf)                              # Whats the best correlation?
best10 <- which(coef10$acf == max(coef10$acf))   # Where is it?
coef10$lag[best10]                             # How much of a lag is that?

coef45 <- ccf(deep3$`IUI_avg_45m`, deep3$`SST_NOAA Avg`, na.action = na.pass)
max(coef45$acf)                              # Whats the best correlation?
best45 <- which(coef45$acf == max(coef45$acf))   # Where is it?
coef45$lag[best45]                             # How much of a lag is that?

coef60 <- ccf(deep3$`IUI_avg_60m`, deep3$`SST_NOAA Avg`, na.action = na.pass)
max(coef60$acf)                                  # Whats the best correlation?
best60 <- which(coef60$acf == max(coef60$acf))   # Where is it?
coef60$lag[best60]                               # How much of a lag is that?


deep4 <- mutate(deep3, Diff_avg_2 = `SST_NOAA Avg` - `IUI_avg_2m`,
                Diff_avg_10 = `SST_NOAA Avg` - `IUI_avg_10m`,
                Diff_avg_45 = `SST_NOAA Avg` - `IUI_avg_45m`,
                Diff_avg_60 = `SST_NOAA Avg` - `IUI_avg_60m`) %>% 
  select(starts_with("Diff_avg")) %>% 
  pivot_longer(starts_with("Diff_avg"), names_to = "logger", values_to = "error") %>% 
  mutate(logger = case_when(str_detect(logger, "2") ~ "2 m",
                            str_detect(logger, "10") ~ "10 m",
                            str_detect(logger, "45") ~ "45 m",
                            str_detect(logger, "60") ~ "60 m",
                            T  ~ "SST"),
         Correlation = case_when(str_detect(logger, "2") ~ max(coef2$acf),
                                 str_detect(logger, "10") ~ max(coef10$acf),
                                 str_detect(logger, "45") ~ max(coef45$acf),
                                 str_detect(logger, "60") ~ max(coef60$acf),
                                 T  ~ max(look$acf))) %>% 
  group_by(logger) %>% 
  mutate(days = sum(is.finite(error))) %>% 
  ungroup() %>% 
  mutate(logger = factor(logger, levels = c("SST", "2 m", "10 m", "45 m", "60 m")),
         direction = ifelse(error < 0, "Too cold", "Too hot"))

ggplot(deep4) +
  geom_histogram(aes(x = error, fill = direction)) +
  facet_grid(rows = vars(logger)) +
  geom_text(aes(x = -1.5, y = 125, label = str_glue("CCF = {round(Correlation, 3)}
                                                  n = {days}")), size = 3, hjust = 0) +
  geom_vline(aes(xintercept = 0)) +
  labs(x = "Satellite error (°C)", y = "Days", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("cadetblue3", "firebrick3")) +
  NULL

ggsave("satellite.png", width = 18, height = 10, units = "cm")


#### Compare min ####

coef2 <- ccf(deep3$`IUI_min_2m`, deep3$`SST_NOAA min`, na.action = na.pass)
max(coef2$acf)                              # Whats the best correlation?
best2 <- which(coef2$acf == max(coef2$acf))   # Where is it?
coef2$lag[best2]                             # How much of a lag is that?

coef10 <- ccf(deep3$`IUI_min_10m`, deep3$`SST_NOAA min`, na.action = na.pass)
max(coef10$acf)                              # Whats the best correlation?
best10 <- which(coef10$acf == max(coef10$acf))   # Where is it?
coef10$lag[best10]                             # How much of a lag is that?

coef45 <- ccf(deep3$`IUI_min_45m`, deep3$`SST_NOAA min`, na.action = na.pass)
max(coef45$acf)                              # Whats the best correlation?
best45 <- which(coef45$acf == max(coef45$acf))   # Where is it?
coef45$lag[best45]                             # How much of a lag is that?

coef60 <- ccf(deep3$`IUI_min_60m`, deep3$`SST_NOAA min`, na.action = na.pass)
max(coef60$acf)                                  # Whats the best correlation?
best60 <- which(coef60$acf == max(coef60$acf))   # Where is it?
coef60$lag[best60]                               # How much of a lag is that?


deep4 <- mutate(deep3, Diff_min_2 = `SST_NOAA min` - `IUI_min_2m`,
                Diff_min_10 = `SST_NOAA min` - `IUI_min_10m`,
                Diff_min_45 = `SST_NOAA min` - `IUI_min_45m`,
                Diff_min_60 = `SST_NOAA min` - `IUI_min_60m`) %>% 
  select(starts_with("Diff_min")) %>% 
  pivot_longer(starts_with("Diff_min"), names_to = "logger", values_to = "error") %>% 
  mutate(logger = case_when(str_detect(logger, "2") ~ "2 m",
                            str_detect(logger, "10") ~ "10 m",
                            str_detect(logger, "45") ~ "45 m",
                            str_detect(logger, "60") ~ "60 m",
                            T  ~ "SST"),
         Correlation = case_when(str_detect(logger, "2") ~ max(coef2$acf),
                                 str_detect(logger, "10") ~ max(coef10$acf),
                                 str_detect(logger, "45") ~ max(coef45$acf),
                                 str_detect(logger, "60") ~ max(coef60$acf),
                                 T  ~ max(look$acf))) %>% 
  group_by(logger) %>% 
  mutate(days = sum(is.finite(error))) %>% 
  ungroup() %>% 
  mutate(logger = factor(logger, levels = c("SST", "2 m", "10 m", "45 m", "60 m")),
         direction = ifelse(error < 0, "Too cold", "Too hot"))

ggplot(deep4) +
  geom_histogram(aes(x = error, fill = direction)) +
  facet_grid(rows = vars(logger)) +
  geom_text(aes(x = -1.5, y = 125, label = str_glue("CCF = {round(Correlation, 3)}
                                                  n = {days}")), size = 3, hjust = 0) +
  geom_vline(aes(xintercept = 0)) +
  labs(x = "Satellite error (°C)", y = "Days", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("cadetblue3", "firebrick3")) +
  NULL

ggsave("satellite_min.png", width = 18, height = 10, units = "cm")
