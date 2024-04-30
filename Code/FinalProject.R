library(dplyr)
library(ggplot2)
library(gridExtra)

selected_data <- select(full.data, 3:22, 26, 27)

head(selected_data)

mean_values <- selected_data %>%
  group_by(Genotype, Year) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

genotypes_to_remove <- c("IL14H", "HP301", "P39")
final_values <- mean_values %>% 
  filter(!Genotype %in% genotypes_to_remove)

elemental_columns <- colnames(final_values)[3:length(final_values)]

element_year_dfs <- list()

for (element in elemental_columns) {
  # Data frame for 2010 for the current element
  element_2010 <- final_values[final_values$Year == 2010, c("Genotype", element)]
  
  # Data frame for 2011 for the current element
  element_2011 <- final_values[final_values$Year == 2011, c("Genotype", element)]
  
  # Merge the data frames by Genotype
  merged_df <- merge(element_2010, element_2011, by = "Genotype", suffixes = c("_2010", "_2011"))
  
  # Store the merged data frame in the list
  element_year_dfs[[element]] <- merged_df
}

element_df <- element_year_dfs[["Mg25"]]  # Replace "B11" with the desired element name

# Create scatter plot
Mg25_scatter_plot <- ggplot(element_df, aes(x = Mg25_2010, y = Mg25_2011, color = Genotype)) +
  geom_point() +
  labs(x = "2010", y = "2011", color = "Genotype") +
  ggtitle("Scatter Plot of Mg25 (2010) vs Mg25 (2011) by Genotype")

element_df <- element_year_dfs[["Mg25"]]  # Replace "B11" with the desired element name

# Create scatter plot
Mg25_scatter_plot <- ggplot(element_df, aes(x = Mg25_2010, y = Mg25_2011)) +
  geom_point(size = 3) +
  labs(x = "2010", y = "2011") +
  ggtitle("Mg25") +
  theme(plot.title = element_text(hjust = 0.5))
print(Mg25_scatter_plot)
# Save the scatter plot to a file (e.g., "scatter_plot.png")
ggsave("Mg25_scatter_plot.png", plot = Mg25_scatter_plot, width = 8, height = 6, units = "in", dpi = 300)


element_df <- element_year_dfs[["Cd111"]]  # Replace "B11" with the desired element name

# Create scatter plot
ggplot(element_df, aes(x = Cd111_2010, y = Cd111_2011, color = Genotype)) +
  geom_point() +
  labs(x = "2010", y = "2011", color = "Genotype") +
  ggtitle("Scatter Plot of Cd111 (2010) vs Cd111 (2011) by Genotype")

Mg25_df1 <- element_year_dfs[["Mg25"]]
Cd111_df2 <- element_year_dfs[["Cd111"]]  # Adjust to the desired element name

names(Mg25_df1) <- c("Genotype", "X2010", "X2011")
names(Cd111_df2) <- c("Genotype", "X2010", "X2011")

# Add a column to each data frame to indicate the element
Mg25_df1$Element <- "Mg25"
Cd111_df2$Element <- "Cd111"

# Combine the data frames
combined_df <- rbind(Mg25_df1, Cd111_df2)

# Create scatter plot with facets
scatter_plot <- ggplot(combined_df, aes(x = X2010, y = X2011, color = Genotype)) +
  geom_point() +
  labs(x = "2010", y = "2011", color = "Genotype") +
  facet_wrap(~ Element, scales = "free") +
  ggtitle("Scatter Plot of 2010 vs 2011 by Element and Genotype")


print(scatter_plot)

for (i in names(element_year_dfs)) {
  i_df <- element_year_dfs[[i]] 
}

element_df <- element_year_dfs[["Mg25"]] 
Mg25_plot <- ggplot(element_df, aes(x = Mg25_2010, y = Mg25_2011)) +
  geom_point(size = 3) +
  labs(x = "2010", y = "2011") +
  ggtitle("Mg25") +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white"), panel.border = element_rect(color = "black", fill = NA, linewidth = 1), axis.text = element_text(color = "black", size = 12), axis.title = element_text(color = "black", size = 14))
element_df <- element_year_dfs[["Cd111"]]
Cd111_plot <- ggplot(element_df, aes(x = Cd111_2010, y = Cd111_2011)) +
  geom_point(size = 3) +
  labs(x = "2010", y = "2011") +
  ggtitle("Cd111") +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white"), panel.border = element_rect(color = "black", fill = NA, linewidth = 1), axis.text = element_text(color = "black", size = 12), axis.title = element_text(color = "black", size = 14))
Mg25_Cd111_combined_plot <- grid.arrange(Mg25_plot, Cd111_plot, ncol = 2)



scatter_plots <- list()

# Loop through each element data frame and create a scatter plot
for (name in names(element_year_dfs)) {
  element_df <- element_year_dfs[[name]]
}
  
  # Create scatter plot for the current element
  scatter_plot <- ggplot(element_df, aes(x = paste0(element_name, "_2010"), y = paste0(element_name, "_2011"))) +
    geom_point(size = 3) +
    labs(x = paste0(element_name, " (2010)"), y = paste0(element_name, " (2011)")) +
    ggtitle(paste("Scatter Plot of", element_name)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14))
  
  # Store the scatter plot in the list
  scatter_plots[[element_name]] <- scatter_plot
}

# Arrange all scatter plots using grid.arrange
scatter_plots_grob <- lapply(scatter_plots, ggplotGrob)
grid.arrange(grobs = scatter_plots_grob, ncol = 4)

scatter_plots <- list()

# Loop through each element in the data frame
for (element_name in names(element_year_dfs)) {
  # Get the data frame for the current element
  element_df <- element_year_dfs[[element_name]]
  
  # Extract column names for 2010 and 2011
  col_2010 <- paste0(element_name, "_2010")
  col_2011 <- paste0(element_name, "_2011")
  
  # Create a scatter plot for the current element
  plot <- ggplot(element_df, aes(x = !!sym(col_2010), y = !!sym(col_2011))) +
    geom_point(size = 1) +
    labs(x = "2010", y = "2011") +
    ggtitle(element_name) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 14))
  
  # Store the plot in the list
  scatter_plots[[element_name]] <- plot
}

# Convert the list of plots to a list of grobs (graphical objects)
plots_grobs <- lapply(scatter_plots, ggplotGrob)

# Arrange all plots in a matrix using grid.arrange
all_elements_plot <- grid.arrange(grobs = plots_grobs, ncol = 4)
ggsave("all_elements_plot.png", plot = all_elements_plot, width = 12, height = 10, units = "in", dpi = 300)
