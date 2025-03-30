library(dplyr)
library(ggplot2)


sales_data <- data.frame(
  date = as.Date(c("2025-01-01", "2025-01-02", "2025-01-03", "2025-01-04", "2025-01-05",
                  "2025-01-06", "2025-01-07", "2025-01-08", "2025-01-09", "2025-01-10",
                  "2025-01-11", "2025-01-12", "2025-01-13", "2025-01-14", "2025-01-15")),
  product = c("Milk", "Eggs", "Butter", "Milk", "Eggs", "Butter", "Milk", "Eggs", "Butter", "Milk", "Eggs", "Butter",
              "Milk", "Eggs", "Butter"),
  region = c("ATB", "ATB", "ATB", "Silpo", "Silpo", "Silpo", "ATB", "ATB",
             "Silpo", "Silpo", "ATB", "Silpo", "ATB", "Silpo", "ATB"),
  units_sold = c(120, 85, 100, 75, 93, 110, 115, 80, 105, 70, 90, 115, 125, 88, 95),
  price_per_unit = c(10, 15, 12, 10, 15, 12, 10, 15, 12, 10, 15, 12, 10, 15, 12)
)


sales_data <- sales_data %>%
  mutate(total_value = units_sold * price_per_unit)


region_sales <- sales_data %>%
  group_by(region) %>%
  summarise(
    total_sales = sum(total_value),
    avg_units = mean(units_sold),
    count = n()
  ) %>%
  arrange(desc(total_sales))
print(region_sales)

product_sales <- sales_data %>%
  group_by(product) %>%
  summarise(
    total_sales = sum(total_value),
    avg_units = mean(units_sold),
    count = n()
  ) %>%
  arrange(desc(total_sales))
print(product_sales)

top_days <- sales_data %>%
  group_by(date) %>%
  summarise(daily_sales = sum(total_value)) %>%
  arrange(desc(daily_sales)) %>%
  head(5)
print(top_days)

high_value_sales <- sales_data %>%
  filter(total_value > 1000) %>%
  select(date, product, region, total_value) %>%
  arrange(desc(total_value))
print(high_value_sales)


ggplot(region_sales, aes(x = region, y = total_sales, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Total sales by region",
       x = "Region",
       y = "Total sales (UAH)") +
  theme_minimal()

ggplot(product_sales, aes(x = "", y = total_sales, fill = product)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Sales Distribution by Product",
       fill = "Product") +
  theme_void() +
  theme(legend.position = "right")

daily_sales <- sales_data %>%
  group_by(date) %>%
  summarise(daily_sales = sum(total_value))

ggplot(daily_sales, aes(x = date, y = daily_sales)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  labs(title = "Daily Sales",
       x = "Date",
       y = "Sales (UAH)") +
  theme_minimal()

ggplot(sales_data, aes(x = product, y = units_sold, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Units Sold by Product and Region",
       x = "Product",
       y = "Units Sold",
       fill = "Region") +
  theme_minimal()
