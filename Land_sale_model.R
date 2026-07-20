library(tidyverse)
library(tidymodels)

land_sales <- read_csv("data/land_sales.csv") |> janitor::clean_names()

ggplot(land_sales, aes(land_area_acres, land_sale_price_per_acre_no_discounts))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

land_sales <- land_sales |> rename(price_per_acre = land_sale_price_per_acre_no_discounts)

land_sales_model <- linear_reg() |> 
  set_engine("lm") |> 
  fit(price_per_acre ~ land_area_acres, data = land_sales)


tidy(land_sales_model)

summary(land_sales_model$fit)

glance(land_sales_model)
tidy(land_sales_model)
