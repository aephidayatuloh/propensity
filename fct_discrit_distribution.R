library(ggplot2)
library(scales)
library(gridExtra)

discrit_distribution <- function(.variable, data = retail_transform){
  grid.arrange(
    data |> 
      ggplot(aes(x = .data[[.variable]])) + 
      geom_bar(fill = "skyblue") + 
      theme(
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank()
      ),
    data |> 
      ggplot(aes(x = .data[[.variable]])) + 
      geom_boxplot() + 
      scale_x_continuous(labels = number_format(big.mark = ",")) + 
      theme(
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank()
      ),
    ncol = 1, 
    heights = c(0.7, 0.3)
  )
}