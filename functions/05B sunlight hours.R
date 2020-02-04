pacman::p_load(tidyverse, curl, sf, glue, fasterize)
root = "input/09 climate grids"
#http://www.bom.gov.au/jsp/ncc/climate_averages/climate-classifications/index.jsp?maptype=tmp_zones#maps

climate_files = c("http://www.bom.gov.au/web01/ncc/www/climatology/climate-classification/seasgrpb.zip",
          "http://www.bom.gov.au/web01/ncc/www/climatology/climate-classification/seasb.zip",
          "http://www.bom.gov.au/web01/ncc/www/climatology/climate-classification/kpngrp.zip",
          "http://www.bom.gov.au/web01/ncc/www/climatology/climate-classification/kpn.zip",
          "http://www.bom.gov.au/web01/ncc/www/climatology/climate-classification/tmp_zones.zip")

sun_files = month.abb %>%
  str_to_lower() %>%
  paste0("http://www.bom.gov.au/web01/ncc/www/climatology/sunshine/sun",.,".zip")

frost_files = c('http://www.bom.gov.au/web01/ncc/www/climatology/frost/frost-ltm5deg-7605-an.zip',
                'http://www.bom.gov.au/web01/ncc/www/climatology/frost/frost-ltm2deg-7605-an.zip',
                'http://www.bom.gov.au/web01/ncc/www/climatology/frost/frost-lt0deg-7605-an.zip',
                'http://www.bom.gov.au/web01/ncc/www/climatology/frost/frost-lt2deg-7605-an.zip')

zip_files = c(climate_files,sun_files,frost_files)

download_zip_and_unpack = function(zip){
  file_name = zip %>% str_extract("[^\\/]+(?=\\.zip)")
  zip_path = glue("{root}/zip/{file_name}.zip")
  if(!dir.exists(glue("{root}/zip"))){dir.create(glue("{root}/zip"), F, T)}
  if(!dir.exists(glue("{root}/grids"))){dir.create(glue("{root}/grids"), F, T)}
  if(!file.exists(zip_path)){curl_download(zip, zip_path)}
  folder_name = case_when(str_detect(zip,"sunshine") ~ "sun",
                          str_detect(zip, "frost") ~ "frost",
                          TRUE ~ file_name)
  zip::unzip(zip_path, exdir = glue("{root}/grids/{folder_name}"))
  unzipped_files = list.files(path = glue("{root}/grids/{folder_name}"), full.names = T)
  ifelse(str_detect(zip,"(sunshine|frost)"),
          glue("{root}/grids/{folder_name}/{file_name}.txt"),
          unzipped_files[!str_detect(unzipped_files,"(readme|tif)")])
  }

returned_files = zip_files %>% map_chr(download_zip_and_unpack)

all_files = list.files(glue("{root}/grids"),
                       recursive = T,
                       pattern = ".txt",
                       full.names = T) %>%
  data.frame(file = .) %>%
  filter(!str_detect(file, "readme")) %>%
  pluck("file") %>%
  as.character()

singleband_raster_to_dataframe = function(img){
  img_brick = img %>%
    raster::raster() %>%
    raster::brick()

  col_name = img_brick@data@names[1]

  img_brick@data@values %>%
    as.data.frame() %>%
    set_names(nm = str_to_lower(col_name))
}
raster_preprocessing = function(file){
  res = 360 / 8640
  raw = raster(file)
  raster::crs(raw) = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
  tif_path = str_replace_all(file,".txt$",".tif")
  raster::writeRaster(raw, filename = tif_path, overwrite = TRUE)
  sf::gdal_utils("translate",
                 source = tif_path,
                 destination = str_replace(tif_path, "\\.tif","\\_temp\\.tif"),
                 options = c("-tr", res, res),
                 )
  # gdal_translate -projwin ulx uly lrx lry inraster.tif outraster.tif`
  sf::gdal_utils("translate",
                 source = str_replace(tif_path, "\\.tif","\\_temp\\.tif"),
                 destination = tif_path,
                 options = c("-projwin",
                             112,
                             -10,
                             154,
                             -44))
  file.remove(str_replace(tif_path, "\\.tif","\\_temp\\.tif"))
  tif_path
}

df = all_files %>%
  map_chr(raster_preprocessing) %>%
  map_dfc(singleband_raster_to_dataframe)

df = df %>% select(-seasrain1)

category_map = glue("{root}/grids/category_map.csv") %>% data.table::fread()

categorical_column_to_wide = function(df, col){
  col_enq = enquo(col)

  vector_levels = df %>%
    select(!!col_enq) %>%
    set_names("column") %>%
    mutate(n = row_number(),
           value = 1,
           category = col) %>%
    left_join(category_map,
              by = c("column" = "from", "category" = "category")) %>%
    pivot_wider(names_from = "to",
                values_from = value,
                values_fill = list(value = 0)) %>%
    select(-n, -category, -column)
  if ("NA" %in% names(vector_levels)) {
    vector_levels = vector_levels %>%  select(-`NA`)
  }
  df %>% select(-!!col_enq) %>% bind_cols(vector_levels)
  }

df_wide = df %>%
  categorical_column_to_wide("clim.zones") %>%
  categorical_column_to_wide("seasrain") %>%
  categorical_column_to_wide("kpngrp") %>%
  categorical_column_to_wide("kpnall") %>%
  set_names(names(.) %>% str_replace_all(c("fltm" = "frost_days_less_than_minus_",
                                           "\\.ann" = "",
                                           "flt"  = "frost_days_less_than_",
                                           "frostann" = "frost_days_per_annum",
                                           "sun" = "sunshine_hours_in_")))

df_wide %>%
  bind_cols(data.table::fread("input/df_points.csv")) %>%
  data.table::fwrite(glue("{root}/df_climate.csv"))
