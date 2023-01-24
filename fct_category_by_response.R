library(ggplot2)
library(scales)

category_by_response <- function(.variable, data = retail_transform){
  p.value <- fisher.test(x = data[[.variable]], 
                         y = data[["response"]]) |> 
    pluck("p.value")
  
  data |> 
    ggplot(aes(x = .data[[.variable]], fill = response)) + 
    geom_bar(position = position_fill()) + 
    scale_y_continuous(labels = percent_format()) + 
    labs(y = "Proportion", 
         subtitle = paste("Fisher's Extact Test p-value =", 
                          round(p.value, 5))
    ) + 
    theme_light() + 
    theme(panel.grid = element_blank())
}
