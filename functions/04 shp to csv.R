# Data from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/73140

pacman::p_load(tidyverse, curl, sf, glue, fasterize)
zip_url = "https://d28rz98at9flks.cloudfront.net/73140/73140_2_5M_shapefiles.zip"
zip_path = "input/04 geological/classification/73140_2_5M_shapefiles.zip"

curl_download(zip_url, zip_path)

# Unzip all of the 2.5M GeologicUnitPolygon files
zip_files = zip::zip_list(zip_path) %>%
  data.frame() %>%
  filter(str_detect(filename, "GeologicUnitPolygons2_5M")) %>%
  pluck("filename")

zip::unzip(zip_path,
           files = zip_files,
           exdir = "input/04 geological/classification")

# Extract the SHP Paths
shp = "input/04 geological/classification/shapefiles" %>%
  list.files(pattern = "shp$",
             full.names = TRUE)

# https://epsg.io/4283
shp_geo = sf::st_read(shp,
                      query = "SELECT * FROM \"GeologicUnitPolygons2_5M\"") %>%
  st_set_crs(NA) %>%
  st_set_crs(4283) %>%
  transmute(FEATUREID,
            STRATNO,
            MAPSYMBOL,
            PLOTSYMBOL,
            NAME = str_remove_all(NAME, " \\d+"),
            DESCR = str_extract(DESCR, "^.*?(?=(;|\\.|\\n))") %>% str_remove_all("(,|\\/)"),
            REPAGE_URI = str_remove(REPAGE_URI, "http://resource.geosciml.org/classifier/ics/ischart/"),
            YNGAGE_URI = str_remove(YNGAGE_URI, "http://resource.geosciml.org/classifier/ics/ischart/"),
            LITHOLOGY = str_replace(LITHOLOGY,";"," and"),
            REPLITH_UR = str_remove(REPLITH_UR,"http://resource.geosciml.org/classifier/cgi/lithology/")
  )

per_level = function(level, category){
  message(glue("     {str_pad(category,10,'left')} - {level}"))
  sym_level = rlang::sym(level)
  sym_category = rlang::sym(category)
  folder_path = here::here(glue("input/04 geological/features/{category}"))
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
    set_names(nm = glue::glue("{category}_{sym_level}"))

  df_raster %>% data.table::fwrite(file_name)
  df_raster
}
per_category = function(category){
  folder_path = here::here(glue("input/04 geological/features/{category}"))
  if(!dir.exists(folder_path)){
    dir.create(folder_path, showWarnings = F, recursive = T)
  }
  message(glue("starting: {category}"))
  #Iterate Per Level
  df_category = shp_geo %>%
    pluck(category) %>%
    unique() %>%
    sort() %>%
    map_dfc(per_level, category)

  df_points %>%
    bind_cols(df_category) %>%
    data.table::fwrite(glue::glue("input/04 geological/features/{category}.csv"))
  df_category
}

column_levels = df_geo_join %>% select(-(FEATUREID:NAME)) %>%
  as.list() %>%
  map(unique)

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

df_all_geo = df_geo_join %>%
  select(-(FEATUREID:NAME)) %>%
  colnames() %>%
  map_dfc(per_category)

df_points %>%
  bind_cols(df_all_geo) %>%
  data.table::fwrite("input/04 geological/df_all_geo.csv")
