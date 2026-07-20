# data/FAF5.7.1.csv is not tracked in this repo (too large for GitHub).
# To obtain it:
#   1. Go to https://www.bts.gov/faf
#   2. Download the FAF5.7.1 regional database (CSV format)
#   3. Unzip and place FAF5.7.1.csv in the data/ folder

library(tidyverse)
library(readxl)
library(ggthemes)
library(writexl)

faf_5 <- read_csv("data/FAF5.7.1.csv")
faf_5_regions <- read_csv("data/CFS-area-code-FAF5-zone-id.csv") |> janitor::clean_names()
faf_5_sctg2 <- read_excel("data/FAF5_metadata.xlsx", sheet = "Commodity (SCTG2)") |> janitor::clean_names()
faf_5_mode <- read_excel("data/FAF5_metadata.xlsx", sheet = "Mode") |> janitor::clean_names()

#FAF 5 Boston Region = 251
#value in millions of dollars (all 2017) - divide by 1000 to get billions
#weight in thousand tons

####Totals####
exports <- faf_5 |> 
  filter(dms_orig == 251 & dms_dest != 251) |> 
  summarize(total_export_value = sum(value_2017)/1000)

imports <- faf_5 |> 
  filter(dms_dest == 251 & dms_orig != 251) |> 
  summarize(total_import_value = sum(value_2017)/1000)

intra_flows <- faf_5 |> 
  filter(dms_dest == 251 & dms_orig == 251) |> 
  summarize(total_import_value = sum(value_2017)/1000)

exports + imports + intra_flows

#total non foreign originating intra region flows
faf_5 |> filter(dms_orig == 251 & dms_dest == 251 & is.na(fr_orig) & is.na(fr_dest)) |> 
  summarise(total_value = sum(value_2017)/1000)

####Top Ten Export Locations####
top_10_dest <- faf_5 |> 
  filter(dms_orig == 251) |> 
  group_by(dms_dest) |> 
  summarize(total_value = sum(value_2017)) |> 
  arrange(desc(total_value)) |> 
  select(dms_dest) |> 
  slice(1:10)


faf_5 |> filter(dms_orig == 251 & dms_dest %in% top_10_dest$dms_dest) |> 
  group_by(dms_dest) |> 
  summarize(value_2017 = sum(value_2017)) |> 
  ggplot(aes(x = reorder(dms_dest, -value_2017), value_2017))+
  geom_col()

####Top Ten Export Products####

faf_5 <- faf_5 |> mutate(intra_region = ifelse(dms_orig == 251 & dms_dest == 251, "intra", "not_intra"))

#Includes exports to other states/zones and intr-azone shipments
top_ten_exports_value <- faf_5 |> 
  filter(dms_orig == 251) |>
  group_by(sctg2) |> 
  summarize(
    total_value = sum(value_2017)/1000,
  ) |> 
  arrange(desc(total_value)) |> 
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |> 
  slice(1:10)

write_xlsx(top_ten_exports_value, "data/top_ten_exp_val.xlsx")

top_ten_exports_wt <- faf_5 |> 
  filter(dms_orig == 251) |>
  group_by(sctg2) |> 
  summarize(total_weight = sum(tons_2017)) |> 
  arrange(desc(total_weight)) |> 
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |> 
  slice(1:10)

top_ten_wt <- top_ten_exports_wt$description

#top exports by weight and whether internal or external shipments 
top_ten_exports_wt_intra <- faf_5 |> 
  filter(dms_orig == 251) |>
  group_by(sctg2, intra_region) |> 
  summarize(total_weight = sum(tons_2017)) |> 
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |> 
  filter(description %in% top_ten_wt) |> 
  pivot_wider(names_from = intra_region, values_from = total_weight) |> 
  mutate(total_weight = intra + not_intra) |> 
  arrange(desc(total_weight)) 

write_xlsx(top_ten_exports_wt_intra, "data/top_ten_exp_wt_intra.xlsx")

#top exports by value and whether internal or external shipments 
top_ten_exports_val_intra <- faf_5 |> 
  filter(dms_orig == 251) |>
  group_by(sctg2, intra_region) |> 
  summarize(total_value = sum(value_2017)/1000) |> 
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |> 
  ungroup() |> 
  pivot_wider(names_from = intra_region, values_from = total_value) |> 
  mutate(total_value = intra + not_intra) |> 
  arrange(desc(total_value)) |> 
  slice(1:10)

write_xlsx(top_ten_exports_val_intra, "data/top_ten_exp_val_intra.xlsx")


#top_ten_exports_fig <- 
top_ten_exports |> ggplot(aes(x = reorder(description, -total_value), total_value))+
  geom_col(fill = "skyblue4")+
  xlab("Standard Classification of Transported Goods (SCTG)")+
  ylab("Total Value ($Billions)")+
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, size = 12),
        axis.text.y = element_blank())+
  geom_text(aes(label = round(total_value, digits = 1)), vjust = 2, color = "white")

top_ten_exports |> ggplot(aes(x = reorder(description, -total_weight), total_weight))+
  geom_col(fill = "skyblue4")+
  xlab("Standard Classification of Transported Goods (SCTG)")+
  ylab("Total Value ($Billions)")+
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, size = 12),
        axis.text.y = element_blank())+
  geom_text(aes(label = round(total_weight, digits = 1)), vjust = 2, color = "white")

#ggsave("top_ten_exp.png", plot = top_ten_exports_fig, dpi = 300)

ggplot(top_ten_exports_wt_intra, aes(x = description, y = total_weight, fill = intra_region))+
  geom_col()

#weight
faf_5 |> 
  filter(dms_orig == 251) |> 
  group_by(dms_mode, intra_region) |> 
  summarize(
    total_weight = sum(tons_2017)
  ) |> 
  pivot_wider(names_from = intra_region, values_from = total_weight) |> 
  mutate(
    intra = ifelse(is.na(intra), 0, intra),
    not_intra = ifelse(is.na(not_intra), 0, not_intra),
    total_weight = intra + not_intra
  ) |> 
  left_join(faf_5_modes, join_by(dms_mode == numeric_label)) |> 
  select(description, intra, not_intra, total_weight)
write_xlsx("data/mode_wt.xlsx")

#value  
faf_5 |> 
  filter(dms_orig == 251) |> 
  group_by(dms_mode, intra_region) |> 
  summarize(
    total_value = sum(value_2017)/1000
  ) |> 
  pivot_wider(names_from = intra_region, values_from = total_value) |> 
  mutate(
    intra = ifelse(is.na(intra), 0, intra),
    not_intra = ifelse(is.na(not_intra), 0, not_intra),
    total_value = intra + not_intra
  ) |> 
  left_join(faf_5_modes, join_by(dms_mode == numeric_label)) |> 
  select(description, intra, not_intra, total_value) |> 
  arrange(desc(total_value)) |> 
  write_xlsx("data/mode_val.xlsx")

faf_5 |> 
  filter(dms_orig == 251) |> 
  group_by(dms_mode, intra_region) |> 
  summarize(
    total_value = sum(value_2017)/1000
  ) |> 
  pivot_wider(names_from = intra_region, values_from = total_value) |> 
  mutate(
    intra = ifelse(is.na(intra), 0, intra),
    not_intra = ifelse(is.na(not_intra), 0, not_intra),
    total_value = intra + not_intra
  ) |> 
  left_join(faf_5_modes, join_by(dms_mode == numeric_label)) |> 
  select(description, intra, not_intra, total_value) |> 
  arrange(desc(total_value)) |>
  ungroup() |> 
  summarise(total_value/sum(total_value))



####Imports####
#does not include the intra-zone movement of goods. This is exclusively imports from elsewhere.

faf_5 |> filter(dms_dest == 251 & dms_orig != 251) |> 
  summarise(total_value = sum(value_2017)/1000)

faf_5 |> 
  filter(dms_dest == 251 & dms_orig != 251) |>
  group_by(sctg2) |> 
  summarize(total_value = sum(value_2017)/1000) |> 
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |> 
  ungroup() |> 
  arrange(desc(total_value)) |> 
  slice(1:10) |> 
  write_xlsx("data/top_ten_imp_val.xlsx")

faf_5 |> filter(dms_dest == 251 & dms_orig != 251) |> 
  summarise(total_value = sum(tons_2017))

faf_5 |> 
  filter(dms_dest == 251 & dms_orig != 251) |>
  group_by(sctg2) |> 
  summarize(total_weight = sum(tons_2017)) |> 
  left_join(faf_5_sctg2, join_by(sctg2 == numeric_label)) |> 
  ungroup() |> 
  arrange(desc(total_weight)) |> 
  slice(1:10) |> 
  write_xlsx("data/top_ten_imp_wt.xlsx")






                                                              
