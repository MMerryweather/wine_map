pacman::p_load(tidyverse, sf, glue)

root = "input/08 wine maps"

shp = readRDS(glue::glue("{root}/aus_wine_production.rds"))

blank_raster = raster::raster(ncol = 1008,
                              nrow = 816,
                              crs = 4283,
                              xmn = 112,
                              xmx = 154,
                              ymn = -44,
                              ymx = -10,
                              vals = 0)

per_varietal = function(varietal){
  sym_varietal = rlang::sym(varietal)

  folder_path = here::here(glue("{root}/varietals"))
  if(!dir.exists(folder_path)){
    dir.create(folder_path, showWarnings = F, recursive = T)
  }
  message(glue("starting: {varietal}"))
  #Iterate Per Level

  file_name = glue::glue("{folder_path}/{varietal}.csv")

  shp_subset = shp %>% select(!! sym_varietal)
  clean_name = varietal %>%
    stringi::stri_trans_general("latin-ascii") %>%
    str_replace_all(c(" +" = "_",
                      "'" = "")) %>%
    str_to_lower()

  ## .@ subsets into S4 objects, could user pipeR but not a fan
  df_raster  = fasterize::fasterize(shp_subset, blank_raster, field = varietal) %>%
    .@data %>%
    .@values %>%
    as.data.frame(row.names = row.names(.)) %>%
    data.table::setDT() %>%
    set_names(str_to_lower(names(.))) %>%
    set_names(nm = clean_name)

  #df_raster %>% data.table::fwrite(file_name)
  df_raster

}

res = 360 / 8640
df_points = list(
  lon = seq(from  = 112,
            to    = 154 - res,
            by    = res),
  lat = seq(from  = -44,
            to    = -10 - res,
            by    = res)) %>%
  cross_df() %>%
  arrange(desc(lat), lon)

df_all_varietals = shp %>%
  select(winery_grown_fruit ,`Cabernet Sauvignon`:Doradillo) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  colnames() %>%
  as.list() %>%
  map_dfc(per_varietal)

df_points %>%
  bind_cols(df_all_varietals) %>%
  data.table::fwrite(glue("{root}/df_all_varietals.csv"))

