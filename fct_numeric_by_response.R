library(ggplot2)
library(scales)
library(gridExtra)

numeric_by_response <- function(.variable, .data = retail_transform){
  grid.arrange(
    .data |> 
      ggplot(aes(x = .data[[.variable]], fill = response)) + 
      geom_density(alpha = 0.5) + 
      theme_minimal() +
      theme(
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        legend.position = "top"
      ),
    .data |> 
      ggplot(aes(x = .data[[.variable]], fill = response)) + 
      geom_boxplot(alpha = 0.5) + 
      scale_x_continuous(labels = number_format(big.mark = ",")) + 
      theme_minimal() +
      theme(
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        legend.position = "none"
      ),
    ncol = 1, 
    heights = c(0.7, 0.3)
  )
}