pacman::p_load(tidyverse, data.table)
df_all =   data.table::fread("input/04 geological/df_all_geo.csv")
## Check Health of Attributes via plot

files = "input/04 geological/features" %>%
  list.files(pattern = "csv", full.names = T)

files = files[1:4]

df = files %>%
  map(fread) %>%
  map(~pivot_longer(., cols = -c(lon,lat),
                        names_to = "attribute",
                        values_to = "value") %>%
           na.omit() %>%
        mutate(attribute = attribute %>% str_remove("_UR|_URI")) %>%
        separate(col = attribute,
                 into = c("title","attribute"),
                 sep = "_"))



plotter = function(x){
p = x %>%
  ggplot(aes(x = lon,
             y = lat,
             fill = attribute)) +
  geom_raster() +
  scale_fill_viridis_d(x$title[1]) +
  coord_equal() +
  labs(title = x$title[1])+
  hrbrthemes::theme_ipsum_rc() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    legend.box = "vertical",
    text = element_text(colour = "white"),
    axis.text = element_text(colour = "white"),
    panel.grid.major.x = element_line(colour = rgb(40, 40, 40, maxColorValue = 255)),
    panel.grid.major.y = element_line(colour = rgb(40, 40, 40, maxColorValue = 255))
  ) +
  ggthemr::no_minor_gridlines() +
  ggthemr::legend_bottom()+
  guides(fill = guide_legend(ncol = 2,
                             label.theme = element_text(size = 6,
                                                        colour = "white")))+
  ggthemr::no_legend_title()


  ggsave(plot = p,
    filename = glue::glue("plot/geological/{x$title[1]}.png"),
    width = 5,
    height = 7,
    units = "cm",
    dpi = 320,
    scale = 3,
    limitsize = F
  )

  }

df %>% walk(plotter)
