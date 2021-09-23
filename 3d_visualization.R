library(plotly)
library(reshape2)
my_df <- iris

petal_lm <- lm(Petal.Length ~ Sepal.Length, data = my_df)

graph_reso <- 0.05

#Setup Axis
axis_x <- seq(min(my_df$Sepal.Length), max(my_df$Sepal.Length), by = graph_reso)
axis_y <- seq(min(my_df$Sepal.Width), max(my_df$Sepal.Width), by = graph_reso)

#Sample points
petal_lm_surface <- expand.grid(Sepal.Length = axis_x, 
                                Sepal.Width = axis_y, 
                                KEEP.OUT.ATTRS = F)

petal_lm_surface$Petal.Length <- predict.lm(petal_lm, newdata = petal_lm_surface)

petal_lm_surface <- acast(petal_lm_surface, 
                          Sepal.Width ~ Sepal.Length, 
                          value.var = "Petal.Length")

my_df_2 <- my_df %>% 
  mutate(Petal.Length = predict(petal_lm, newdata = ),
         predict = 1)

my_df <- my_df %>% 
  mutate(predict = 0)

my_df <- rbind(my_df, my_df_2) %>% 
  mutate(predict = as.numeric(predict))

iris_plot <- plot_ly(my_df, 
                     x = ~Sepal.Length, 
                     y = ~Sepal.Width, 
                     z = ~Petal.Length,
                     frame = ~predict) %>% 
  animation_opts(1000, transition = 500, easing = "elastic", redraw = T)

fig <- iris_plot %>% 
  add_markers(size = 5) %>% 
  add_surface(z = petal_lm_surface,
              x = axis_x,
              y = axis_y,
              opacity = 0.5)
fig

