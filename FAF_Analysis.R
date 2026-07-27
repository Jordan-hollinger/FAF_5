# data/FAF5.7.1.csv is not tracked in this repo (too large for GitHub).
# To obtain it:
#   1. Go to https://www.bts.gov/faf
#   2. Download the FAF5.7.1 regional database (CSV format)
#   3. Unzip and place FAF5.7.1.csv in the data/ folder

library(tidyverse)
library(readxl)
library(ggthemes)
library(writexl)
library(ggrepel)

faf_5_regions <- read_csv("data/CFS-area-code-FAF5-zone-id.csv") |> janitor::clean_names()
faf_5_sctg2 <- read_excel("data/FAF5_metadata.xlsx", sheet = "Commodity (SCTG2)") |> janitor::clean_names()
faf_5_mode <- read_excel("data/FAF5_metadata.xlsx", sheet = "Mode") |> janitor::clean_names() |>
  mutate(numeric_label = as.numeric(numeric_label))

#zone codes (e.g. "011") are zero-padded strings matching dms_orig/dms_dest - keep numeric_label as character
faf_5_zones <- bind_rows(
  read_excel("data/FAF5_metadata.xlsx", sheet = "FAF Zone (Domestic)") |>
    janitor::clean_names() |>
    rename(description = short_description) |>
    select(numeric_label, description),
  read_excel("data/FAF5_metadata.xlsx", sheet = "FAF Zone (Foreign)") |>
    janitor::clean_names() |>
    select(numeric_label, description)
)

#FAF 5 Boston Worcester Providence Region/zone = 251
#value in millions of dollars (constant 2017$) - divide by 1000 to get billions
#weight in thousand tons
#annual estimate years: 2017 (base) through 2024

faf_5 <- read_csv("data/FAF5.7.1.csv") |>
  filter(dms_orig == 251 | dms_dest == 251) |>
  select(-matches("^(tons|value|current_value)_20(30|35|40|45|50)$")) |>
  select(-starts_with(c("tmiles"))) |> 
  pivot_longer(
    cols = matches("^(tons|value|current_value)_20(1[7-9]|2[0-4])$"),
    names_to = c(".value", "year"),
    names_pattern = "(tons|value|current_value)_(\\d{4})"
  ) |>
  mutate(year = as.integer(year)) |>
  rename(value_2017_dollars = value) |>
  mutate(current_value = if_else(year == 2017, value_2017_dollars, current_value))

####Totals####
exports <- faf_5 |>
  filter(dms_orig == 251 & dms_dest != 251) |>
  group_by(year) |>
  summarize(
    total_export_value_2017_dollars = sum(value_2017_dollars)/1000,
    total_export_value_current = sum(current_value)/1000
  )

imports <- faf_5 |>
  filter(dms_dest == 251 & dms_orig != 251) |>
  group_by(year) |>
  summarize(
    total_import_value_2017_dollars = sum(value_2017_dollars)/1000,
    total_import_value_current = sum(current_value)/1000
  )

intra_flows <- faf_5 |>
  filter(dms_dest == 251 & dms_orig == 251) |>
  group_by(year) |>
  summarize(
    total_intra_value_2017_dollars = sum(value_2017_dollars)/1000,
    total_intra_value_current = sum(current_value)/1000
  )

totals <- exports |>
  left_join(imports, by = "year") |>
  left_join(intra_flows, by = "year") |>
  mutate(
    total_value_2017_dollars = total_export_value_2017_dollars + total_import_value_2017_dollars + total_intra_value_2017_dollars,
    total_value_current = total_export_value_current + total_import_value_current + total_intra_value_current
  )

#total non foreign originating intra region flows
faf_5 |> filter(dms_orig == 251 & dms_dest == 251 & is.na(fr_orig) & is.na(fr_dest)) |>
  group_by(year) |>
  summarise(
    total_value_2017_dollars = sum(value_2017_dollars)/1000,
    total_value_current = sum(current_value)/1000
  )

####Top Ten Export Locations####
top_10_dest <- faf_5 |>
  filter(dms_orig == 251) |>
  group_by(dms_dest) |>
  summarize(total_value = sum(value_2017_dollars)) |>
  arrange(desc(total_value)) |>
  slice(1:10) |>
  pull(dms_dest)

#destinations in descending rank order, used to fix both the legend order
#and the color assignment so they match across the 2017$ and current$ charts
top_10_dest_names <- faf_5_zones$description[match(top_10_dest, faf_5_zones$numeric_label)]

#muted earth-tone palette, spread across lightness (not just hue) so the
#10 destinations stay distinguishable under red-green color vision deficiency
earth_tone_palette_10 <- c(
  "#001219", "#005f73", "#0a9396", "#94d2bd", "#e9d8a6",
  "#ee9b00", "#ca6702", "#bb3e03", "#ae2012", "#9b2226"
)
names(earth_tone_palette_10) <- top_10_dest_names

earth_tone_theme <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

top_10_dest_values <- faf_5 |>
  filter(dms_orig == 251 & dms_dest %in% top_10_dest) |>
  group_by(dms_dest, year) |>
  summarize(
    total_value_2017_dollars = sum(value_2017_dollars)/1000,
    total_value_current = sum(current_value)/1000
  ) |>
  left_join(faf_5_zones, join_by(dms_dest == numeric_label)) |>
  mutate(description = factor(description, levels = top_10_dest_names))

top_10_dest_values |>
  ggplot(aes(x = year, y = total_value_2017_dollars, color = description)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_text(
    data = filter(top_10_dest_values, year == 2017 & dms_dest %in% c("251","331","363","230","061")),
    aes(label = round(total_value_2017_dollars, 1)),
    hjust = 1.25, size = 3, show.legend = FALSE
  ) +
  geom_text(
    data = filter(top_10_dest_values, year == 2024 & dms_dest %in% c("251","331","363","230","061")),
    aes(label = round(total_value_2017_dollars, 1)),
    hjust = -0.25, size = 3, show.legend = FALSE
  ) +
  scale_color_manual(values = earth_tone_palette_10) +
  scale_x_continuous(breaks = 2017:2024, expand = expansion(mult = 0.1)) +
  xlab("Year") +
  ylab("Total Export/Intra-Zone Value, 2017$ (Billions)") +
  labs(color = "Destination") +
  earth_tone_theme


top_10_dest_values |>
  ggplot(aes(x = year, y = total_value_current, color = description)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_text(
    data = filter(top_10_dest_values, year == 2017),
    aes(label = round(total_value_current, 1)),
    hjust = 1.25, size = 3, show.legend = FALSE
  ) +
  geom_text(
    data = filter(top_10_dest_values, year == 2024),
    aes(label = round(total_value_current, 1)),
    hjust = -0.25, size = 3, show.legend = FALSE
  ) +
  scale_color_manual(values = earth_tone_palette_10) +
  scale_x_continuous(breaks = 2017:2024, expand = expansion(mult = 0.1)) +
  xlab("Year") +
  ylab("Total Export Value, Current $ (Billions)") +
  labs(color = "Destination") +
  earth_tone_theme


year_palette_8 <- c(
  "#001219", "#005f73", "#0a9396", "#94d2bd", "#e9d8a6",
  "#ee9b00", "#ca6702", "#bb3e03"
)
names(year_palette_8) <- 2017:2024

top_10_dest_values |>
  ggplot(aes(x = description, y = total_value_current, fill = factor(year))) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_manual(values = year_palette_8) +
  ylab("Total Export Value, Current $ (Billions)") +
  labs(fill = "Year")+
  theme_minimal()+
  xlab("")

ggsave(filename = "top_10_dest.png")



####Top Ten Export Products by Weight and value####

faf_5 <- faf_5 |> mutate(intra_region = ifelse(dms_orig == 251 & dms_dest == 251, "intra", "not_intra"))

#Includes exports to other states/zones and intra-zone shipments
#rank products by total value summed across all years (2017-2024)
top_10_val_sctg2 <- faf_5 |>
  filter(dms_orig == 251) |>
  group_by(sctg2) |>
  summarize(total_value = sum(value_2017_dollars)/1000) |>
  arrange(desc(total_value)) |>
  slice(1:10) |>
  pull(sctg2)

top_ten_exports_value <- faf_5 |>
  filter(dms_orig == 251 & sctg2 %in% top_10_val_sctg2) |>
  group_by(sctg2, year) |>
  summarize(
    total_value_2017_dollars = sum(value_2017_dollars)/1000,
    total_value_current = sum(current_value)/1000
  ) |>
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |>
  arrange(desc(total_value_2017_dollars), year)

write_xlsx(top_ten_exports_value, "data/top_ten_exp_val.xlsx")

#long form by value and whether internal or external shipments 
top_ten_exports_val_intra_long <- top_ten_exports_val_intra |> 
  select(sctg2, year, description, total_value_current_not_intra, total_value_current_intra) |> 
  pivot_longer(cols = total_value_current_not_intra:total_value_current_intra,
               names_to = "intra",
               values_to = "value")

#weight
top_10_wt_sctg2 <- faf_5 |>
  filter(dms_orig == 251) |>
  group_by(sctg2) |>
  summarize(total_weight = sum(tons)) |>
  arrange(desc(total_weight)) |>
  slice(1:10) |>
  pull(sctg2)

top_ten_exports_wt <- faf_5 |>
  filter(dms_orig == 251 & sctg2 %in% top_10_wt_sctg2) |>
  group_by(sctg2, year) |>
  summarize(total_weight = sum(tons)) |>
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |>
  arrange(desc(total_weight), year)

#top exports by weight and whether internal or external shipments
top_ten_exports_wt_intra_long <- faf_5 |>
  filter(dms_orig == 251 & sctg2 %in% top_10_wt_sctg2) |>
  group_by(sctg2, year, intra_region) |>
  summarize(total_weight = sum(tons)) |>
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |>
  ungroup()

top_ten_exports_wt_intra <- top_ten_exports_wt_intra_long |>
  pivot_wider(names_from = intra_region, values_from = total_weight, values_fill = 0) |>
  mutate(total_weight = intra + not_intra) |>
  arrange(desc(total_weight), year)

write_xlsx(top_ten_exports_wt_intra, "data/top_ten_exp_wt_intra.xlsx")

#top exports by value and whether internal or external shipments
top_ten_exports_val_intra <- faf_5 |>
  filter(dms_orig == 251 & sctg2 %in% top_10_val_sctg2) |>
  group_by(sctg2, year, intra_region) |>
  summarize(
    total_value_2017_dollars = sum(value_2017_dollars)/1000,
    total_value_current = sum(current_value)/1000
  ) |>
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |>
  ungroup() |>
  pivot_wider(
    names_from = intra_region,
    values_from = c(total_value_2017_dollars, total_value_current),
    values_fill = 0
  ) |>
  mutate(
    total_value_2017_dollars = total_value_2017_dollars_intra + total_value_2017_dollars_not_intra,
    total_value_current = total_value_current_intra + total_value_current_not_intra
  ) |>
  arrange(desc(total_value_2017_dollars), year)

write_xlsx(top_ten_exports_val_intra, "data/top_ten_exp_val_intra.xlsx")


#actual value figure
year_palette_8 <- c(
  "#001219", "#005f73", "#0a9396", "#94d2bd", "#e9d8a6",
  "#ee9b00", "#ca6702", "#bb3e03"
)
names(year_palette_8) <- 2017:2024

top_ten_exports_val_intra_long |>
  mutate(description = fct_reorder(description, value, .fun = sum, .desc = TRUE)) |>
  ggplot(aes(x = year, y = value, fill = intra)) +
  geom_col() +
  facet_wrap(~description, nrow = 2, ncol = 5,
             labeller = labeller(description = label_wrap_gen(width = 20))) +
  scale_fill_manual(
    values = c("total_value_current_intra" = "#bb3e03", "total_value_current_not_intra" = "#ca6702"),
    labels = c("total_value_current_intra" = "Within Zone", "total_value_current_not_intra" = "Out of Zone")
  ) +
  xlab("") +
  ylab("Total Export Value, Current $ (Billions)") +
  labs(fill = "Flow Type")+
  theme_minimal()

ggsave(filename = "top_10_goods_value_facet.png")


#weight figure
top_ten_exports_wt_intra_long |>
  mutate(description = fct_reorder(description, total_weight, .fun = sum, .desc = TRUE)) |>
  ggplot(aes(x = year, y = total_weight, fill = intra_region)) +
  geom_col() +
  facet_wrap(~description, nrow = 2, ncol = 5,
             labeller = labeller(description = label_wrap_gen(width = 20))) +
  scale_fill_manual(
    values = c("intra" = "#005f73", "not_intra" = "#0a9396"),
    labels = c("intra" = "Within Zone", "not_intra" = "Out of Zone")
  ) +
  xlab("") +
  ylab("Total Weight (Thousand Tons)") +
  labs(fill = "Flow Type")+
  theme_minimal()

ggsave(filename = "top_10_goods_wt_facet.png")



####Products by Weight, value and mode####

#weight by mode

year_palette_2 <- c(
  "#001219", "#005f73", "#0a9396", "#94d2bd", "#e9d8a6",
  "#ee9b00", "#ca6702", "#bb3e03"
)
names(year_palette_2) <- 2017:2024

wt_mode_year <- faf_5 |>
  filter(dms_orig == 251) |>
  group_by(dms_mode, year) |>
  summarize(total_weight = sum(tons, na.rm = TRUE), .groups = "drop") |> 
  left_join(faf_5_mode, join_by(dms_mode == numeric_label)) |>
  mutate(description = fct_reorder(description, total_weight, .fun = sum, .desc = TRUE),
         total_weight_millions = total_weight / 1000) |> 
  filter(description != "No domestic mode")

wt_mode_year |>
  ggplot(aes(description, total_weight_millions, fill = factor(year))) +
  geom_col(position = position_dodge2()) +
  geom_text(
    data = wt_mode_year |> filter(description %in% c("Multiple modes & mail", "Rail", "Other and unknown", "Air (include truck-air)", "Water"), year %in% c(2017, 2024)),
    aes(label = round(total_weight_millions, 2)),
    position = position_dodge2(width = 1),
    vjust = -0.9,
    size = 3
  ) +
  scale_fill_manual(values = year_palette_2) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 25, 50, 75, 100, 125)) +
  ylab("Total Weight (Million Tons)") +
  xlab("") +
  labs(fill = "Year") +
  theme_minimal()

ggsave("wt_by_mode_year.png")


#value by mode
mode_value_by_year <- faf_5 |>
  filter(dms_orig == 251) |>
  group_by(dms_mode, year) |>
  summarize(
    total_value_2017_dollars = sum(value_2017_dollars)/1000,
    total_value_current = sum(current_value)/1000,
    .groups = "drop") |> 
  left_join(faf_5_mode, join_by(dms_mode == numeric_label)) |> 
  mutate(description = fct_reorder(description, total_value_current, .fun = sum, .desc = TRUE)) |> 
  filter(description != "No domestic mode")

mode_value_by_year |>
  ggplot(aes(description, total_value_current, fill = factor(year))) +
  geom_col(position = position_dodge2()) +
  geom_text(
    data = mode_value_by_year |> filter(description %in% c("Pipeline", "Rail", "Other and unknown", "Air (include truck-air)", "Water"), year %in% c(2017, 2024)),
    aes(label = round(total_value_current, 2)),
    position = position_dodge2(width = 1),
    vjust = -0.9,
    size = 3
  ) +
  scale_fill_manual(values = year_palette_2) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250)) +
  ylab("Total Value $ (Billions)") +
  xlab("") +
  labs(fill = "Year") +
  theme_minimal()

ggsave("mode_value_by_year.png")



####Imports####
#does not include the intra-zone movement of goods. This is exclusively imports from elsewhere.

#Total weight and value by year

total_imports_wt_val <- faf_5 |> 
  filter(dms_dest == 251 & dms_orig != 251) |>
  group_by(year) |>
  summarise(
    total_value_current = sum(current_value) / 1000,
    total_weight = sum(tons) / 1000,
    .groups = "drop"
  )

scale_factor <- max(total_imports_wt_val$total_value_current) / max(total_imports_wt_val$total_weight)

total_imports_wt_val <- total_imports_wt_val |>
  mutate(year_pos = as.numeric(factor(year)) * 1.5)

total_imports_wt_val |>
  ggplot() +
  geom_col(aes(x = year_pos - 0.32, y = total_value_current), 
           width = 0.6, fill = "#005f73") +
  geom_text(aes(x = year_pos - 0.32, y = total_value_current, 
                label = paste0("$", round(total_value_current, 0))),
            vjust = -0.5, size = 3) +
  geom_col(aes(x = year_pos + 0.32, y = total_weight * scale_factor), 
           width = 0.6, fill = "#94d2bd") +
  geom_text(aes(x = year_pos + 0.32, y = total_weight * scale_factor, 
                label = round(total_weight, 0)),
            vjust = -0.5, size = 3) +
  scale_y_continuous(
    name = "Total Value $(Billion)",
    labels = scales::comma,
    sec.axis = sec_axis(~ . / scale_factor, name = "Total Weight (Million Tons)", labels = scales::comma, breaks = c(10, 20, 30, 40, 50, 60))
  ) +
  scale_x_continuous(breaks = unique(total_imports_wt_val$year_pos), labels = unique(total_imports_wt_val$year)) +
  xlab("") +
  theme_minimal()

ggsave("total_import_val_wt.png")

#Top 10 Imports by weight and value

top_10_imp_val_sctg2 <- faf_5 |>
  filter(dms_dest == 251 & dms_orig != 251) |>
  group_by(sctg2) |>
  summarize(total_value = sum(value_2017_dollars)/1000) |>
  arrange(desc(total_value)) |>
  slice(1:10) |>
  pull(sctg2)

top_ten_imports_value <- faf_5 |>
  filter(dms_dest == 251 & dms_orig != 251 & sctg2 %in% top_10_imp_val_sctg2) |>
  group_by(sctg2, year) |>
  summarize(
    total_value_2017_dollars = sum(value_2017_dollars)/1000,
    total_value_current = sum(current_value)/1000,
    .groups = "drop"
  ) |>
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |>
  arrange(desc(total_value_2017_dollars), year)

write_xlsx(top_ten_imports_value, "data/top_ten_imp_val.xlsx")

faf_5 |> filter(dms_dest == 251 & dms_orig != 251) |>
  group_by(year) |>
  summarise(total_weight = sum(tons))

top_10_imp_wt_sctg2 <- faf_5 |>
  filter(dms_dest == 251 & dms_orig != 251) |>
  group_by(sctg2) |>
  summarize(total_weight = sum(tons)) |>
  arrange(desc(total_weight)) |>
  slice(1:10) |>
  pull(sctg2)

top_ten_imports_wt <- faf_5 |>
  filter(dms_dest == 251 & dms_orig != 251 & sctg2 %in% top_10_imp_wt_sctg2) |>
  group_by(sctg2, year) |>
  summarize(total_weight = sum(tons)/1000,
            .groups = "drop") |>
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |>
  arrange(desc(total_weight), year)

write_xlsx(top_ten_imports_wt, "data/top_ten_imp_wt.xlsx")

#Imports by value
top_ten_imports_value |>
  mutate(description = fct_reorder(description, total_value_current, .fun = sum, .desc = TRUE)) |>
  ggplot(aes(description, total_value_current, fill = factor(year))) +
  geom_col(position = position_dodge2()) +
  scale_fill_manual(values = year_palette_2) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  ylab("Total Value $ (Billions)") +
  xlab("") +
  labs(fill = "Year") +
  theme_minimal()

ggsave("imports_value_by_year.png")


top_ten_imports_wt |>
  mutate(description = fct_reorder(description, total_weight, .fun = sum, .desc = TRUE)) |>
  ggplot(aes(description, total_weight, fill = factor(year))) +
  geom_col(position = position_dodge2()) +
  scale_fill_manual(values = year_palette_2) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20)) +
  ylab("Total Weight (Million Tons)") +
  xlab("") +
  labs(fill = "Year") +
  theme_minimal()

ggsave("imports_wt_by_year.png")






                                                              
