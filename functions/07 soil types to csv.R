pacman::p_load(tidyverse, sf, glue)

root = "input/07 soil type"
file_name = "soe2016lan117lan16"
zip_url = "https://data.gov.au/dataset/116eb634-fc0b-42d8-ae27-b876a12c4f6a/resource/8b82738a-2e59-41b8-8080-3d7b4014e648/download/soe2016lan117lan16.zip"
zip_path = glue("{root}/{file_name}.zip") %>% here::here()

if(!dir.exists(glue("{root}/soils"))){
  dir.create(glue("{root}/soils"), showWarnings = F, recursive = T)
}
if(!file.exists(zip_path)){
curl::curl_download(zip_url, zip_path)
}
zip::unzip(zip_path, exdir = glue("{root}/soils"))

shp = glue("{root}/soils") %>% list.files(pattern = ".shp$", full.names = T)

shp_geo = sf::st_read(shp) %>%
    st_set_crs(NA) %>%
    st_set_crs(4283)

  per_level = function(level, category){
    message(glue("     {str_pad(category,10,'left')} - {level}"))
    sym_level = rlang::sym(level)
    sym_category = rlang::sym(category)
    folder_path = here::here(glue("{root}/features/{category}"))
    file_name = glue::glue("{folder_path}/{category}_{sym_level}.csv")

    shp_subset = shp_geo %>%
      filter(!!sym_category == sym_level) %>%
      mutate(present = 1)

    blank_raster = raster::raster(ncol = 1008,
                                  nrow = 816,
                                  crs = 4283,
                                  xmn = 112,
                                  xmx = 154,
                                  ymn = -44,
                                  ymx = -10,
                                  vals = 0)
    ## .@ subsets into S4 objects, could user pipeR but not a fan
    df_raster  = fasterize::fasterize(shp_subset, blank_raster) %>%
      .@data %>%
      .@values %>%
      as.data.frame(row.names = row.names(.)) %>%
      data.table::setDT() %>%
      set_names(str_to_lower(names(.))) %>%
      set_names(nm = glue::glue("{category}_{sym_level}")) %>%
      mutate_all(as.numeric)

    df_raster %>% data.table::fwrite(file_name)
    df_raster
  }
  per_category = function(category){
    folder_path = here::here(glue("{root}/features/{category}"))
    if(!dir.exists(folder_path)){
      dir.create(folder_path, showWarnings = F, recursive = T)
    }
    message(glue("starting: {category}"))
    #Iterate Per Level
    df_category = shp_geo %>%
      pluck(category) %>%
      unique() %>%
      sort() %>%
      as.character() %>%
      map_dfc(per_level, category)

    df_points %>%
      bind_cols(df_category) %>%
      data.table::fwrite(glue::glue("{root}/features/{category}.csv"))
    df_category
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

  df_soils = shp_geo %>%
    as.data.frame() %>%
    select(soil_order) %>%
    colnames() %>%
    map_dfc(per_category)

  df_points %>%
    bind_cols(df_soils) %>%
    mutate_all(as.numeric) %>%
    data.table::fwrite(glue("{root}/df_all_soils.csv"))

