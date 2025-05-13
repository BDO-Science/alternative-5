library(tidyverse)
library(janitor)
library(busdater)
library(here)

path <- here('steelhead-spring-run-jpe','sh-input-data')

wDay <- readRDS(here(path, 'waterDay.rds'))
old_hatchery <- read_csv(here(path, 'CV_Steelhead_Hatchery_Release_Database.csv')) %>%
  clean_names()

hatchery_releases <- old_hatchery %>% group_by(water_year_wy, hatchery_name) %>%
  summarize(number_released = sum(total_number_released)) %>%
  ungroup() %>%
  mutate(hatchery_name = case_when(grepl('Coleman', hatchery_name, ignore.case = TRUE) ~ 'CNFH',
                                   grepl('Feather', hatchery_name, ignore.case = TRUE) ~ 'FRFH',
                                   grepl('Nimbus', hatchery_name, ignore.case = TRUE) ~ 'NMFH',
                                   grepl('Mok', hatchery_name, ignore.case = TRUE) ~ 'MKFH')) %>%
  select(hatchery = 2, wy = 1, 3) %>%
  mutate(wy = as.numeric(wy)) %>%
  bind_rows(read.csv(here(path, 'hatchery_release.csv')))

loss_import <- read_csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year=all&species=2%3At&dnaOnly=no&age=no') %>%
  clean_names()

hatchery_loss <- loss_import %>%
  mutate(wy = get_fy(as.Date(sample_time), opt_fy_start = '10-01')) %>%
  mutate(wday = wDay(as.Date(sample_time))) %>%
  arrange(wy, wday) %>%
  group_by(wy) %>%
  mutate(cumul = cumsum(loss)) %>%
  ungroup() %>%
  filter(wy > 2008 & wy < 2025)

wy_types <- read_csv('https://www.cbr.washington.edu/sacramento/data/php/rpt/hci.php?sc=1&outputFormat=csv&classification=Official') %>%
  clean_names() %>%
  filter(basin == 'Sacramento Valley') %>%
  select(1,4) %>%
  mutate(wy = as.numeric(wy))

survival <- read_csv(here(path,'survival.csv'))

jpe_raw <- filter(hatchery_releases, wy > 2008) %>%
  left_join(wy_types, by = 'wy') %>%
  left_join(survival , by = c('code' = 'wy_type', 'hatchery')) %>%
  mutate(jpe = number_released*survival) 

jpe_hatch <- jpe_raw %>%
  select(2,1,6) %>%
  group_by(wy, hatchery) %>%
  summarize(jpe = sum(jpe, na.rm = TRUE)) %>%
  pivot_wider(names_from = 'hatchery', values_from = 'jpe') %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  mutate(TOTAL = rowSums(across(c(2,3,4,5))))

jpe <- jpe_raw %>%
  group_by(wy) %>%
  summarize(jpe = sum(jpe))

loss_table <- hatchery_loss %>% 
  left_join(jpe, by = 'wy') %>%
  mutate(perc_jpe = cumul/jpe) %>%
  mutate(trigger_50 = if_else(perc_jpe >= 0.005, 1, 0),
         trigger_75 = if_else(perc_jpe >= 0.0075, 1, 0),
         trigger_100 = if_else(perc_jpe >= 0.01, 1, 0))

trigger_dates_50 <- loss_table %>%
  filter(trigger_50 == 1) %>%
  group_by(wy) %>%
  summarize(trigger_50_date = min(as.Date(sample_time)))
trigger_dates_75 <- loss_table %>%
  filter(trigger_75 == 1) %>%
  group_by(wy) %>%
  summarize(trigger_75_date = min(as.Date(sample_time)))
trigger_dates_100 <- loss_table %>%
  filter(trigger_100 == 1) %>%
  group_by(wy) %>%
  summarize(trigger_100_date = min(as.Date(sample_time)))

summary <- loss_table %>%
  group_by(wy) %>%
  summarize(
    loss = max(cumul),
    jpe = round(mean(jpe),0),
    trigger_50 = sum(trigger_50),
    trigger_75 = sum(trigger_75),
    trigger_100 = sum(trigger_100),
  ) %>%
  left_join(trigger_dates_50, by = 'wy') %>%
  left_join(trigger_dates_75, by = 'wy') %>%
  left_join(trigger_dates_100, by = 'wy') %>%
  mutate(across(4:6, ~ if_else(. > 0, "YES", "-"))) %>%
  mutate(across(7:9, ~ if_else(is.na(.), "-", format(., '%b %d')))) %>%
  mutate(proportion = round((loss/jpe)*100,2)) %>%
  select('Water Year' = 1, 'JPE' = 3, 'Total Loss' = 2, 'Proportion Loss' = 10,
         '50% Trigger' = 7, '75% Trigger' = 8, '100% Trigger' = 9)

jpe_graph <- jpe %>%
  mutate(perc_1 = jpe *.01,
         perc_75 = jpe *.0075,
         perc_50 = jpe *.0050) %>%
  pivot_longer(names_to = 'name', values_to = 'threshold', 3:5) %>%
  mutate(name = factor(name, levels = c('perc_1', 'perc_75', 'perc_50'),
                       labels = c('100%', '75%', '50%')))

loss_graph <- ggplot(loss_table, aes(x = wday, y = cumul)) +
  geom_line(linewidth = 1, color = 'steelblue3') +
  geom_hline(jpe_graph, mapping = aes(yintercept = threshold, linetype = name), 
             linewidth = 1, alpha = 0.75,
             color = 'grey') +
  labs(x = 'Date', y = 'Cumulative Loss', linetype = 'Thresholds') +
  scale_y_continuous(n.breaks = 4) +
  scale_x_continuous(breaks = c(65, 155, 245), labels = c('Dec', 'Mar', 'Jun')) +
  facet_wrap(~wy) +
  theme_bw() +
  theme(legend.position = 'bottom',
        plot.margin = margin(0.2, 0.5, 0.2, 0.2, unit = 'cm'),
        axis.title.y = element_text(margin = margin(r = 15), size = 15),
        axis.title.x = element_text(margin = margin(t = 15), size = 15),
        strip.text = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))
loss_graph

