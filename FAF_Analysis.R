# data/FAF5.7.1.csv is not tracked in this repo (too large for GitHub).
# To obtain it:
#   1. Go to https://www.bts.gov/faf
#   2. Download the FAF5.7.1 regional database (CSV format)
#   3. Unzip and place FAF5.7.1.csv in the data/ folder

library(tidyverse)
library(readxl)
library(ggthemes)
library(writexl)

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

exports |>
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

top_10_dest_values <- faf_5 |>
  filter(dms_orig == 251 & dms_dest %in% top_10_dest) |>
  group_by(dms_dest, year) |>
  summarize(
    total_value_2017_dollars = sum(value_2017_dollars)/1000,
    total_value_current = sum(current_value)/1000
  ) |>
  left_join(faf_5_zones, join_by(dms_dest == numeric_label))

top_10_dest_values |>
  ggplot(aes(x = year, y = total_value_2017_dollars, color = description)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  xlab("Year") +
  ylab("Total Export Value, 2017$ (Billions)") +
  labs(color = "Destination")

top_10_dest_values |>
  ggplot(aes(x = year, y = total_value_current, color = description)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  xlab("Year") +
  ylab("Total Export Value, Current $ (Billions)") +
  labs(color = "Destination")

####Top Ten Export Products####

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


top_ten_exports_value |>
  ggplot(aes(x = year, y = total_value_2017_dollars, color = description)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  xlab("Year") +
  ylab("Total Export Value, 2017$ (Billions)") +
  labs(color = "Product")

top_ten_exports_value |>
  ggplot(aes(x = year, y = total_value_current, color = description)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  xlab("Year") +
  ylab("Total Export Value, Current $ (Billions)") +
  labs(color = "Product")

top_ten_exports_wt |>
  ggplot(aes(x = year, y = total_weight, color = description)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  xlab("Year") +
  ylab("Total Export Weight (Thousand Tons)") +
  labs(color = "Product")

top_ten_exports_wt_intra_long |>
  ggplot(aes(x = year, y = total_weight, fill = intra_region)) +
  geom_col() +
  facet_wrap(~description) +
  xlab("Year") +
  ylab("Total Weight (Thousand Tons)") +
  labs(fill = "Flow Type")

#weight by mode
faf_5 |>
  filter(dms_orig == 251) |>
  group_by(dms_mode, year, intra_region) |>
  summarize(total_weight = sum(tons)) |>
  pivot_wider(names_from = intra_region, values_from = total_weight, values_fill = 0) |>
  mutate(total_weight = intra + not_intra) |>
  left_join(faf_5_mode, join_by(dms_mode == numeric_label)) |>
  select(description, year, intra, not_intra, total_weight) |>
  arrange(desc(total_weight), year) |>
  write_xlsx("data/mode_wt.xlsx")

#value by mode
mode_value_by_year <- faf_5 |>
  filter(dms_orig == 251) |>
  group_by(dms_mode, year, intra_region) |>
  summarize(
    total_value_2017_dollars = sum(value_2017_dollars)/1000,
    total_value_current = sum(current_value)/1000
  ) |>
  pivot_wider(
    names_from = intra_region,
    values_from = c(total_value_2017_dollars, total_value_current),
    values_fill = 0
  ) |>
  mutate(
    total_value_2017_dollars = total_value_2017_dollars_intra + total_value_2017_dollars_not_intra,
    total_value_current = total_value_current_intra + total_value_current_not_intra
  ) |>
  left_join(faf_5_mode, join_by(dms_mode == numeric_label))

mode_value_by_year |>
  select(description, year,
         intra_2017_dollars = total_value_2017_dollars_intra,
         not_intra_2017_dollars = total_value_2017_dollars_not_intra,
         total_value_2017_dollars,
         intra_current = total_value_current_intra,
         not_intra_current = total_value_current_not_intra,
         total_value_current) |>
  arrange(desc(total_value_2017_dollars), year) |>
  write_xlsx("data/mode_val.xlsx")

#value share by mode, computed within each year
mode_value_by_year |>
  select(description, year, total_value_2017_dollars, total_value_current) |>
  group_by(year) |>
  mutate(
    share_of_value_2017_dollars = total_value_2017_dollars/sum(total_value_2017_dollars),
    share_of_value_current = total_value_current/sum(total_value_current)
  ) |>
  arrange(year, desc(share_of_value_2017_dollars)) |>
  ungroup()



####Imports####
#does not include the intra-zone movement of goods. This is exclusively imports from elsewhere.

faf_5 |> filter(dms_dest == 251 & dms_orig != 251) |>
  group_by(year) |>
  summarise(
    total_value_2017_dollars = sum(value_2017_dollars)/1000,
    total_value_current = sum(current_value)/1000
  )

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
    total_value_current = sum(current_value)/1000
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
  summarize(total_weight = sum(tons)) |>
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |>
  arrange(desc(total_weight), year)

write_xlsx(top_ten_imports_wt, "data/top_ten_imp_wt.xlsx")

top_ten_imports_value |>
  ggplot(aes(x = year, y = total_value_2017_dollars, color = description)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  xlab("Year") +
  ylab("Total Import Value, 2017$ (Billions)") +
  labs(color = "Product")

top_ten_imports_value |>
  ggplot(aes(x = year, y = total_value_current, color = description)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  xlab("Year") +
  ylab("Total Import Value, Current $ (Billions)") +
  labs(color = "Product")

top_ten_imports_wt |>
  ggplot(aes(x = year, y = total_weight, color = description)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  xlab("Year") +
  ylab("Total Import Weight (Thousand Tons)") +
  labs(color = "Product")






                                                              
