pacman::p_load(tidyverse, data.table)
# TODO
# think about multiple years?

files = "input/02 aus csv" %>%
  list.files(full.names = T, pattern = ".csv") %>%
  here::here()

extract_common = function(){
 df_common = "input/02 aus csv" %>%
    list.files(full.names = T, pattern = ".csv") %>%
    here::here() %>%
    magrittr::extract2(1) %>%
    fread() %>%
    select(lon, lat) %>%
   unique()

 err = min(df_common$lon) - 112

 df_common %>%
   mutate(lat = lat - err,
          lon = lon - err) %>%
   arrange(desc(lat), lon) %>%
   filter(between(lon, 112, 154) & between(lat, -44, -10))
}

extract_attribute = function(file){
  all_data = file %>% fread()
  attribute = all_data$attribute[[1]]

  err = min(all_data$lon) - 112

  df = all_data %>%
    mutate(lat = lat - err,
           lon = lon - err) %>%
    transmute(lat, lon, month, !! attribute := value) %>%
    pivot_wider(names_from = month,
                names_prefix = attribute,
                values_from = !! attribute) %>%
    arrange(desc(lat), lon) %>%
    filter(between(lon, 112, 154) & between(lat, -44, -10)) %>%
    select(-lat, -lon)

  rm(all_data)
  gc()
  df
}

df_common = extract_common()
df_attributes = files %>% map_dfc(extract_attribute)

df = df_common %>% bind_cols(df_attributes)
df %>% fwrite("input/03 wide csv/wide.csv")

attributes = df %>% select(-c(year:lat)) %>% colnames()

## Check Health of Attributes via plot
return_column_plot = function(col, df){
  # https://edwinth.github.io/blog/dplyr-recipes/
  col_enq = rlang::sym(col)

  title = fread("input/00 terraclimate codes/codes.csv") %>%
    filter(str_trim(code) == col) %>% pluck("description")

  df %>%
    dplyr::transmute(lon, lat, value = !!col_enq) %>%
    na.omit() %>%
    ggplot(aes(x = lon,
               y = lat,
               fill = value)) +
    geom_raster() +
    scale_fill_viridis_c() +
    coord_equal() +
    labs(title = title) +
    hrbrthemes::theme_ipsum_rc() +
    theme(
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black"),
      legend.background = element_rect(fill = "black"),
      text = element_text(colour = "white"),
      axis.text = element_text(colour = "white"),
      panel.grid.major.x = element_line(colour = rgb(40, 40, 40, maxColorValue = 255)),
      panel.grid.major.y = element_line(colour = rgb(40, 40, 40, maxColorValue = 255))
    ) +
    ggthemr::no_minor_gridlines()

  ggsave(
    glue::glue("plot/attributes/{col}.png"),
    width = 5,
    height = 5,
    units = "cm",
    dpi = 320,
    scale = 3
  )
}

attributes %>% walk(return_column_plot, df = df %>%
                      filter(year == max(year)) %>%
                      mutate_at(vars(aet:ws), as.numeric))



