# * = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# * OpenFinancialData
# * https://github.com/VictorBenitoGR/OpenFInancialData
# * = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# *** PACKAGES *** ------------------------------------------------------------

# install.packages("PackageName")

library("openxlsx") # Reading/writing/editing excel files
library("dplyr") # Data manipulation/transformation
library("ggplot2") # Data visualization
library("maps") # Map data visualization

# *** FILE *** ----------------------------------------------------------------

food_waste <- read.csv("data/FAOSTAT_FoodWaste.csv")


# *** COLUMNS *** -------------------------------------------------------------

# * Select useful columns
food_waste <- food_waste %>%
  select(
    country, region, commodity, year, loss_percentage, loss_percentage_original,
    loss_quantity, activity, food_supply_stage, treatment, cause_of_loss
  )

# *** FILTER *** --------------------------------------------------------------

# Filter by country
# food_waste <- food_waste %>%
#   filter(country %in% c("Mexico", "United States of America"))

# *** VISUALIZATION *** -------------------------------------------------------


# Filter by country (Mexico and United States)
filtered_food_waste <- food_waste %>%
  filter(country %in% c("Mexico", "United States of America"))

# Calculate average loss_percentage by country and year
avg_loss_pct <- filtered_food_waste %>%
  group_by(country, year) %>%
  summarize(avg_loss_percentage = mean(loss_percentage))

# Set a custom theme for the plot
theme_set(theme_bw())

# Graph the average loss_percentage by country and year with customized aesthetics
avg_loss_pct_plot <- ggplot(avg_loss_pct, aes(x = year, y = avg_loss_percentage, color = country)) +
  geom_line(size = 1.5) +
  labs(
    title = "Average Loss Percentage by Country over the Years",
    x = "Year",
    y = "Average Loss Percentage",
    color = "Country"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

# Save the plot as an image with higher resolution
ggsave("./assets/avg_loss_pct_plot.png",
  plot = avg_loss_pct_plot,
  width = 10, height = 6, dpi = 600
)



# # Get the map data for countries
# world_map <- map_data("world")

# # Merge the food_waste data with the map data
# food_waste_map <- merge(world_map, food_waste, by.x = "region", by.y = "country", all.x = TRUE)

# # Create the heat map
# food_waste_heatmap <- ggplot(food_waste_map, aes(x = long, y = lat, group = group, fill = loss_percentage)) +
#   geom_tile() +
#   scale_fill_gradient(low = "green", high = "red", na.value = "white") +
#   labs(
#     title = "Food Waste: Loss Percentage by Country",
#     fill = "Loss Percentage"
#   ) +
#   theme_void()

# # Save the heat map as an image
# ggsave("./assets/food_waste_heatmap.png",
#   plot = food_waste_heatmap,
#   width = 10, height = 6, dpi = 300
# )
