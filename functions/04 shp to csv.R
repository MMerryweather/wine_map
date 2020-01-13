# Data from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/73140

pacman::p_load(tidyverse, curl, sf, glue)
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

# Extract the DBF and SHP Paths
dbf = "input/04 geological/classification/shapefiles" %>%
  list.files(pattern = "dbf",
             full.names = TRUE)
shp = "input/04 geological/classification/shapefiles" %>%
  list.files(pattern = "shp$",
             full.names = TRUE)

df_geo = foreign::read.dbf(dbf)
df_geo_join = df_geo %>%
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

# df_geo_join %>% data.table::fwrite("input/04 geological/df_geo_join.csv")
# https://epsg.io/4283
shp_geo = sf::st_read(shp,
                      query = "SELECT FEATUREID FROM \"GeologicUnitPolygons2_5M\"") %>%
  st_set_crs(NA) %>%
  st_set_crs(4283) %>%
  inner_join(df_geo_join, by = "FEATUREID")

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

per_category = function(category){
  folder_path = here::here(glue("input/04 geological/features/{category}"))
  if(!dir.exists(folder_path)){
    dir.create(folder_path, showWarnings = F, recursive = T)
  }

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
per_level = function(level, category){
  sym_level = rlang::sym(level)
  sym_category = rlang::sym(category)
  file_name = glue::glue("{folder_path}/{category}_{sym_level}.csv")

  shp_subset = shp_geo %>%
    filter(!!sym_category == sym_level) %>%
    mutate(present = 1)

  ## Convert to a SpatialPolygonsDataFrame for rasterizing
  spatial_shp = sf:::as_Spatial(shp_subset$geom)
  blank_raster = raster::raster(ncol = 1008,
                                nrow = 816,
                                crs = 4283,
                                xmn = 112,
                                xmx = 154,
                                ymn = -44,
                                ymx = -10,
                                vals = 0)

  df_raster  = raster::rasterize(spatial_shp, blank_raster, field = 1) %>%
    brick() %>%
    as.data.frame(row.names = row.names(.)) %>%
    data.table::setDT() %>%
    set_names(str_to_lower(names(.))) %>%
    set_names(nm = glue::glue("{category}_{sym_level}"))

  df_raster %>% data.table::fwrite(file_name)
  df_raster
}

df_all_geo = df_geo_join %>%
  select(-(FEATUREID:NAME)) %>%
  colnames() %>%
  map_dfc(per_category)

df_points %>%
  bind_cols(df_all_geo) %>%
  data.table::fwrite("input/04 geological/df_all_geo.csv")




# Importing SHP File
# Enabling processing toolbox
# Toggle Editing Mode
# Processing Toolbox > Fix Geometries
# Write df_geo_join
# JOIN in
# Processing Toolbox > Split Vector Layer



