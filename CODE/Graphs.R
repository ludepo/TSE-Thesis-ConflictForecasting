# Created by: Luca Poll
# Created on: 27.07.2021

# ------------------------------------------------------------------------------
# Part O: setup
# ------------------------------------------------------------------------------

# libraries used
library(tidyverse)
library(ggsci)
library(sf)
library(zoo)
library(colorspace)
library(maps)
library(ggmap)
library(viridis)
library(circlize)


# color palette
colpal <- pal_nejm("default")(8)

theme_set(theme_minimal())
theme_minimal <- theme_update(text = element_text(family="LM Roman 10"))

# set working directory
setwd("C:/Users/Luca Poll/Google Drive/TSE/Thesis/Code/Model")

# load data
# master dataset
load("DATA/master_data.RData")
load("DATA/cnmh_data_aggregated.RData")
load("DATA/cnmh_data_clean.RData")
load("DATA/icews_data_aggregated.RData")

# convert time in master data to yearqtr format
master_data <- master_data %>% mutate(time = as.yearqtr(time))



# ------------------------------------------------------------------------------
# Part 1: Data section
# ------------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1) proportion of municipios with violence
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
master_data %>%
  count(time, y_violence_F1) %>%
  pivot_wider(names_from = y_violence_F1, values_from = n) %>%
  select(-`NA`) %>% arrange(time) %>%
  mutate(no = lag(no, order_by = time), yes = lag(yes, order_by = time),
         violent = yes/(no + yes)) %>%
  filter(time < as.yearqtr(as.Date("2015-01-01"))) %>%
  ggplot() +
    geom_line(aes(x = time, y = violent, group = 1), color = colpal[2], size = 0.75) +
    geom_point(aes(x = time, y = violent), color = colpal[2], size = 2, shape = 15) +
    scale_y_continuous(limits = c(0,0.3)) +
    scale_x_yearqtr(breaks = seq(min(master_data$time), max(master_data$time), by = 1),format = "%YQ%q") +
    ylab("Share of municipalities with at least one violent event\n") + xlab("")

ggsave("OUTPUT/Graphs/violence_prop.pdf", width = 9, height = 4, device = cairo_pdf)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2) descriptive stats sentiment
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
master_data %>%
  select(time, rhs_tv_sent_av_gov_public, rhs_tv_sent_av_gov_rebels,
         rhs_tv_sent_av_public_gov, rhs_tv_sent_av_public_rebels,
         rhs_tv_sent_av_rebels_gov, rhs_tv_sent_av_rebels_public) %>%
  pivot_longer(!time, names_to = "rel", values_to = "sent") %>%
  filter(sent != 0) %>%
  mutate(rel = str_sub(rel, 16)) %>%
  ggplot() +
    geom_hline(aes(yintercept = 0), linetype = "dashed", alpha = 0.5, color = "grey50") +
    geom_boxplot(aes(rel, sent, color = rel,
                     fill = after_scale(desaturate(lighten(color, .5), .25)))) +
    scale_color_manual(values = colpal[1:6], name = "",
                       labels = c("gov -> public", "gov -> rebels",
                                  "public -> gov", "public -> rebels",
                                  "rebels -> gov", "rebels -> public")) +
    scale_y_continuous(limits = c(-12, 12)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
  labs(y = "CAMEO intensity score")
ggsave("OUTPUT/Graphs/sentiment_boxplot.pdf", width = 9, height = 4, device = cairo_pdf)



master_data %>% select(time, municipio,
                       rhs_tv_sent_sum_gov_public, rhs_tv_sent_sum_gov_rebels,
                       rhs_tv_sent_sum_public_gov, rhs_tv_sent_sum_public_rebels,
                       rhs_tv_sent_sum_rebels_gov, rhs_tv_sent_sum_rebels_public) %>%
  pivot_longer(!c(time, municipio), names_to = "rel", values_to = "sent") %>%
  group_by(municipio) %>%
  mutate(total_sent = sum(sent)) %>%
  filter(total_sent != 0) %>%
  mutate(sent = as.double(if_else(sent == 0, "NA", as.character(sent)))) %>%
  group_by(time, rel) %>%
  summarise(sent = mean(sent, na.rm = TRUE)) %>%
  ggplot() +
    geom_hline(aes(yintercept = 0), linetype = "dashed", alpha = 0.5, color = "grey50") +
    geom_line(aes(x = time, y = sent, color = rel), size = 1, alpha = 0.8) +
    scale_color_manual(values = (colpal[1:6]),
                       labels = c("gov -> public", "gov -> rebels",
                                  "public -> gov", "public -> rebels",
                                  "rebels -> gov", "rebels -> public"),
                       name = "") +
    scale_y_continuous(limits = c(-40, 10)) +
    labs(y = "Average CAMEO intensity score\n", x = "") +
    scale_x_yearqtr(breaks = seq(min(master_data$time), max(master_data$time), by = 1),format = "%YQ%q")

ggsave("OUTPUT/Graphs/sentiment_time.pdf", width = 12, height = 2.75, device = cairo_pdf)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3) descriptive stats structural (drop)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
structural_desc <- master_data %>% select(municipio, structural_selected) %>% group_by(municipio) %>%
  summarise(across(everything(), mean))

describe(structural_desc[-1])







# ------------------------------------------------------------------------------
# Part 2: Appendix
# ------------------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1) map of violence by type
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

departments <- st_read("DATA/Shapefiles/Departments/MGN_DPTO_POLITICO.shp") %>%
  filter(DPTO_CNMBR != "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA")
municipios <- st_read("DATA/Shapefiles/Municipalities/MGN_MPIO_POLITICO.shp") %>%
  rename(municipio = MPIO_CDPMP) %>%
  mutate(municipio = as.character(as.double(municipio))) %>%
  filter(DPTO_CNMBR != "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA")

load("DATA/large_mun.RData")

violence_plot <- cnmh_quarterly %>%
  select(time, municipio, dv_violent_events_F1) %>%
  group_by(municipio) %>%
  mutate(violence = lag(dv_violent_events_F1, order_by = time)) %>%
  ungroup() %>%
  filter(time > as.Date("2000-12-31") & time < as.Date("2015-01-01")) %>%
  select(-time) %>%
  group_by(municipio) %>%
  summarise(violence = sum(violence)) %>%
  filter(! municipio %in% large_mun) %>%
  left_join(municipios, by = "municipio") %>%
  st_as_sf() %>%
  filter(! MPIO_CNMBR %in% c("PROVIDENCIA")) %>%
  mutate(violence = violence + 1)

ggplot() +
  geom_sf(data = violence_plot, aes(fill = log(violence)), color = "grey90", size = 0.00001) +
  scale_fill_viridis(option = "inferno", begin = 0.9, end = 0.4, name = "Log(violent events)") +
  geom_sf(data = departments, fill = "white", color = "grey50", alpha = 0.1, size = 0.0001) +
  theme_nothing(legend = TRUE) +
  theme(text = element_text(family="LM Roman 10"))

ggsave("OUTPUT/Graphs/violence_count_map.pdf", width = 10, height = 8, device = cairo_pdf)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2) total violent events by quarter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cnmh_data %>%
  rename(municipio = "Código DANE de Municipio") %>%
  select(date, type_of_violence) %>%
  mutate(time = as.yearqtr(date)) %>%
  filter(time > as.yearqtr(as.Date("2000-12-31")) &
           time < as.yearqtr(as.Date("2015-01-01"))) %>%
  count(time, type_of_violence) %>%
  mutate(type_of_violence = factor(recode(type_of_violence, ambush = "ambush/harassment", harassment = "ambush/harassment",
                           attack_military_base = "ambush/harassment",
                    military_operation = "military operation/other",
  air_attack = "military operation/other", other = "military operation/other"),
                                   levels = (c("combat", "ambush/harassment","military operation/other")))) %>%
  group_by(time) %>%
  ggplot() +
    geom_bar(aes(x = time, y = n, fill = type_of_violence),
             stat = "identity", position = "dodge", alpha = 0.8) +
    scale_y_continuous(limits = c(0,600)) +
    scale_fill_manual(values = (c(colpal[2], colpal[3], colpal[1])), name = "Violence type") +
    scale_x_yearqtr(breaks = seq(min(master_data$time), max(master_data$time), by = 1),format = "%YQ%q") +
    labs(x = "", y = "Total violent events\n")
ggsave("OUTPUT/Graphs/violence_count.pdf", width = 10, height = 3, device = cairo_pdf)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3) ICEWS event counts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
icews_data %>%
  mutate(time = as.yearqtr(event_date)) %>%
  filter(time > as.yearqtr(as.Date("2000-12-31")) &
           time < as.yearqtr(as.Date("2015-01-01"))) %>%
  filter(longitude > -80) %>% drop_na(latitude) %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = st_crs(municipios)) %>%
  ungroup() %>%
  st_join(., municipios) %>%
  filter(! municipio %in% large_mun) %>%
  as.data.frame() %>%
  mutate(relation = case_when(ssector == "gov" & tsector == "public" ~"gov-public",
                              tsector == "gov" & ssector == "public" ~"gov-public",
                              ssector == "rebels" & tsector == "public" ~"rebels-public",
                              tsector == "rebels" & ssector == "public" ~"rebels-public",
                              ssector == "gov" & tsector == "rebels" ~"gov-rebels",
                              tsector == "gov" & ssector == "rebels" ~"gov-rebels")) %>%
  select(time, cameo_code, relation) %>%
  left_join(cameo_codes, by = "cameo_code") %>%
  count(time, quad_category, relation) %>%
  mutate(quad_category = factor(quad_category, levels = (c("material conflict", "verbal conflict",
                                                              "verbal cooperation", "material cooperation")))) %>%
  ggplot() +
    geom_bar(aes(x = time, y = n, fill = quad_category), stat = "identity",
           position = "stack", alpha = 0.8, width = 0.25001) +
    scale_fill_manual(values = rev(c(colpal[4], colpal[2], colpal[3], colpal[1])),
                      name = "Quad category") +
    facet_wrap(~relation, nrow=3, scales = "free_y") +
    scale_x_yearqtr(breaks = seq(min(master_data$time), max(master_data$time), by = 1),format = "%YQ%q") +
    labs(x = "", y = "Total violent events\n")

ggsave("OUTPUT/Graphs/icews_count.pdf", width = 10, height = 8, device = cairo_pdf)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4) ICEWS event map
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
icews_rel <- icews_data %>%
  mutate(time = as.yearqtr(event_date)) %>%
  filter(time > as.yearqtr(as.Date("2000-12-31")) &
           time < as.yearqtr(as.Date("2015-01-01"))) %>%
  filter(longitude > -80) %>% drop_na(latitude) %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = st_crs(municipios)) %>%
  ungroup() %>%
  st_join(., municipios) %>%
  filter(! municipio %in% large_mun)

ggplot() +
  geom_sf(data = departments, fill = "white", color = "grey50") +
  geom_sf(data = icews_rel, alpha = 0.7, color = colpal[2]) +
  theme_nothing()

ggsave("OUTPUT/Graphs/icews_map.pdf", width = 10, height = 8, device = cairo_pdf)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5) ICEWS chord diagram
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
chord_data <- data.frame(table(icews_rel$ssector, icews_rel$tsector)) %>%
  rename(rowname = Var1, key = Var2, value = Freq)

# parameters
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# Base plot
chordDiagram(chord_data, grid.col = colpal[c(1,2,3)], transparency = 0.5,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight  = -0.04,
  annotationTrack = c("name","grid"),
  annotationTrackHeight = c(0.1, 0.05),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 6) map of municipios in sample
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sample_mun <- unique(master_data$municipio)
mun_aggreg <- read_csv2("DATA/municipio_aggregation.csv",
                        col_types = list(col_character(), col_character())) %>%
  rename(municipio = DANE)
mun_aggreg <- mun_aggreg$municipio

municipios <- municipios %>% mutate(samp = if_else(municipio %in% large_mun, "Large municipio",
                                                   if_else(municipio %in% mun_aggreg, "Aggregated",
                                                           if_else(municipio %in% sample_mun, "In sample",
                                                                   "Missing"))))

ggplot() +
  geom_sf(data = municipios,
          aes(color = samp,
              fill = after_scale(desaturate(lighten(color, .5), .25))),
              size = 0.00001) +
  geom_sf(data = departments, fill = "white", alpha = 0.1, color = "grey50", size = 0.0001) +
  scale_color_manual(values = c(colpal[2], colpal[6], colpal[1], colpal[4]), name = "") +
  theme_nothing(legend = TRUE) +
  theme(text = element_text(family="LM Roman 10"))

ggsave("OUTPUT/Graphs/sample_map.pdf", width = 10, height = 8, device = cairo_pdf)







# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# x) additionals
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# does violence occure where it occured before?
master_data %>% select(time, municipio, rhs_tv_viol_violent_events_L0) %>%
  mutate(violent = if_else(rhs_tv_viol_violent_events_L0 > 0, 1, 0)) %>%
  group_by(municipio) %>%
  summarise(violent = mean(violent)) %>%
  ggplot() +
    geom_histogram(aes(violent), binwidth = 0.02, color = colpal[2], fill = after_scale(desaturate(lighten(colpal[2], .25), .1)), alpha = 0.8) +
    scale_x_continuous(breaks = seq(0,1,by=0.1)) +
  scale_y_continuous(limits = c(0,400)) +
  labs(y = "Number of municipalities\n", x = "\n Proportion of quarters with at least one violent event")

ggsave("OUTPUT/Graphs/violence_time_prop.pdf", width = 10, height = 4, device = cairo_pdf)

# is violence persistent?
master_data %>% select(time, municipio, rhs_tv_viol_violent_events_L0) %>%
  mutate(violent = if_else(rhs_tv_viol_violent_events_L0 > 0, 1, 0)) %>%
  group_by(municipio) %>% arrange(time) %>%
  mutate(two_periods =   ifelse(violent == 1 & lag(violent) == 1, 1, 0),
         three_periods = ifelse(violent == 1 & lag(violent) == 1 & lag(violent, n=2) == 1, 1, 0),
         four_periods =  ifelse(violent == 1 & lag(violent) == 1 & lag(violent, n=2) == 1 & lag(violent, n=3) == 1, 1, 0),
         five_periods =  ifelse(violent == 1 & lag(violent) == 1 & lag(violent, n=2) == 1 & lag(violent, n=3) == 1 & lag(violent, n=4) == 1, 1, 0),
         six_periods =   ifelse(violent == 1 & lag(violent) == 1 & lag(violent, n=2) == 1 & lag(violent, n=3) == 1 & lag(violent, n=4) == 1 & lag(violent, n=5) == 1, 1, 0),
         seven_periods = ifelse(violent == 1 & lag(violent) == 1 & lag(violent, n=2) == 1 & lag(violent, n=3) == 1 & lag(violent, n=4) == 1 & lag(violent, n=5) == 1 & lag(violent, n=6), 1, 0),
         eight_periods = ifelse(violent == 1 & lag(violent) == 1 & lag(violent, n=2) == 1 & lag(violent, n=3) == 1 & lag(violent, n=4) == 1 & lag(violent, n=5) == 1 & lag(violent, n=6) & lag(violent, n=7) == 1, 1, 0),
         nine_periods =  ifelse(violent == 1 & lag(violent) == 1 & lag(violent, n=2) == 1 & lag(violent, n=3) == 1 & lag(violent, n=4) == 1 & lag(violent, n=5) == 1 & lag(violent, n=6) & lag(violent, n=7) == 1 & lag(violent, n=8) == 1, 1, 0),
         ten_periods =   ifelse(violent == 1 & lag(violent) == 1 & lag(violent, n=2) == 1 & lag(violent, n=3) == 1 & lag(violent, n=4) == 1 & lag(violent, n=5) == 1 & lag(violent, n=6) & lag(violent, n=7) == 1 & lag(violent, n=8) == 1 & lag(violent, n=9) == 1, 1, 0)) %>%
  drop_na() %>% pivot_longer(!c(time, municipio, rhs_tv_viol_violent_events_L0),
                              names_to = "length", values_to = "value") %>%
  ungroup() %>% count(length, value)  %>%
  ggplot() +
    geom_bar(aes(x = reorder(length, n), y = n, fill = as.character(value)), stat = "identity", position = "stack")





