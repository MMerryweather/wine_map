pacman::p_load(tidyverse, sf, glue)
#
root = "input/08 wine maps"

zip_url = "https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_AUS_shp.zip"
zip_path = glue("{root}/gadm36_AUS_shp.zip")
curl::curl_download(zip_url, zip_path)
zip::unzip(zip_path, exdir = glue("{root}/gadm36_AUS_shp/"))

aus = glue("{root}/gadm36_AUS_shp/gadm36_AUS_0.shp") %>% st_read()

intersection = glue("{root}/local government areas/intersection.csv") %>%
  data.table::fread() %>%
  transmute(Name = name, region)

wine_summary = glue("{root}/wine report/regional_summary.csv") %>%
  data.table::fread() %>%
  mutate(region = str_remove_all(region, " zone other"))

wine_stats = glue("{root}/wine report/wine_stats.csv") %>% data.table::fread() %>%
  mutate_all(str_trim) %>%
  mutate(varietal = str_replace_all(varietal,c("(Ã¨| Ã |Ã)" ="A",
                                               "\\/" = "_",
                                               "\\(|\\)" = "",
                                               "A¼" = "u",
                                               "total" = "Total")))

wine_stats_wide = wine_stats %>%
  filter(category == "total Crushed" & !is.na(varietal) & varietal != "") %>%
  pivot_wider(names_from = "varietal",
              values_from = numeric,
              names_repair = "minimal",
              values_fn = list(numeric = max)) %>%
  mutate_at(vars(`Cabernet Sauvignon`:Doradillo), as.numeric) %>%
  mutate_at(vars(`Cabernet Sauvignon`:Doradillo), replace_na, replace = 0) %>%
  group_by(region) %>%
  select(-colour, -category, -subcategory) %>%
  summarise_all(max, na.rm = T) %>%
  mutate(present = "1") %>%
  left_join(intersection, by = "region") %>%
  select(region, Name, present, everything()) %>%
  unique() %>%
  mutate(Name = as.character(Name) %>% str_replace_all(c("Pericoota" = "Perricoota",
                                                         "^Australia$" = "Tasmania")))

shp = glue("{root}/shp") %>%
  list.files("shp$", full.names = T) %>%
  st_read() %>%
  transmute(Name = as.character(Name) %>% str_replace_all(c("Pericoota" = "Perricoota",
                                                            "Area" = "Gippsland",
                                                            "^Australia$" = "Tasmania"))) %>%
  left_join(wine_summary, by = c("Name" = "region")) %>%
  select(Name, everything()) %>%
  mutate(area = st_area(.),
         tonnes_per_area = total_collected_tonnes / as.numeric(area,"km^2")) %>%
  left_join(wine_stats_wide, by = c("Name" = "Name"))

shp %>%
  ggplot(aes(fill = `Red Total` / `Grand Total`))+
  geom_sf()+
  scale_fill_distiller(palette = "Reds", type = "seq", direction = 1, labels = scales::percent)+
  labs(fill = "Proportion of Red Wine")

p = aus %>%
  st_crop(xmin = 110, xmax = 154, ymin = -44, ymax = -10) %>%
  ggplot()+
  geom_sf(fill = "grey10", colour = "grey20")+
  hrbrthemes::theme_ipsum_rc() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    legend.box = "vertical",
    legend.text = element_text(colour = "white"),
    text = element_text(colour = "white"),
    axis.text = element_text(colour = "grey20"),
    panel.grid.major = element_line(colour = "grey10")) +
  ggthemr::no_minor_gridlines() +
  ggthemr::no_legend_title() +
  geom_sf(data = shp, aes(
  fill = `Grand Total`),
  colour = "grey20") +
  scale_fill_viridis_c(labels = scales::comma)+
  labs(title = "Wine Production in Australia",
       subtitle = "Tonnes crushed in 2019",
       caption = "National Vintage Report 2019 - Appendix")

ggsave(plot = p,
       filename = glue::glue("plot/varietals/annual_production.png"),
       width = 5,
       height = 5,
       units = "cm",
       dpi = 320,
       scale = 3,
       limitsize = F
)

q = aus %>%
  st_crop(xmin = 110, xmax = 154, ymin = -44, ymax = -10) %>%
  ggplot()+
  geom_sf(fill = "grey10", colour = "grey20")+
  hrbrthemes::theme_ipsum_rc() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    legend.box = "vertical",
    legend.text = element_text(colour = "white"),
    text = element_text(colour = "white"),
    axis.text = element_text(colour = "grey20"),
    panel.grid.major = element_line(colour = "grey10")) +
  ggthemr::no_minor_gridlines() +
  ggthemr::no_legend_title() +
  geom_sf(data = shp, aes(
    fill = Riesling),
    colour = "grey20") +
  scale_fill_viridis_c(labels = scales::comma)+
  labs(title = "Riesling Production in Australia",
       subtitle = "Tonnes crushed in 2019",
       caption = "National Vintage Report 2019 - Appendix")

ggsave(plot = q,
       filename = glue::glue("plot/varietals/riesling_production.png"),
       width = 5,
       height = 5,
       units = "cm",
       dpi = 320,
       scale = 3,
       limitsize = F
)

r = aus %>%
  st_crop(xmin = 110, xmax = 154, ymin = -44, ymax = -10) %>%
  ggplot()+
  geom_sf(fill = "grey10", colour = "grey20")+
  hrbrthemes::theme_ipsum_rc() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    legend.box = "vertical",
    legend.text = element_text(colour = "white"),
    text = element_text(colour = "white"),
    axis.text = element_text(colour = "grey20"),
    panel.grid.major = element_line(colour = "grey10")) +
  ggthemr::no_minor_gridlines() +
  ggthemr::no_legend_title() +
  geom_sf(data = shp, aes(
    fill = `Red Total` / `Grand Total`),
    colour = "grey20") +
  scale_fill_viridis_c(labels = scales::percent)+
  labs(title = "Red Wine Split",
       subtitle = "Red varietals as a fraction of total production",
       caption = "National Vintage Report 2019 - Appendix")

ggsave(plot = r,
       filename = glue::glue("plot/varietals/red_white_split.png"),
       width = 5,
       height = 5,
       units = "cm",
       dpi = 320,
       scale = 3,
       limitsize = F
)

