pacman::p_load(tidyverse, curl, sf, glue)

#https://globalsolaratlas.info/download/australia
root = "input/06 wms layers to csv"
input_folder = root

folder_of_tifs_to_df = function(input_folder, output_folder){
  # Remove the TEMP Variable
  input_folder %>% list.files(full.names = T, pattern = "TEMP") %>% file.remove()

  files = input_folder  %>%
    list.files(pattern = "tif$",
               full.names = T) %>%
    sort()


  if(!dir.exists(output_folder)){
    dir.create(output_folder, showWarnings = F, recursive = T)
  }

  dest_files = input_folder %>%
    list.files(pattern = "tif$") %>%
    sort() %>%
    str_c(output_folder,"/",.)
  res = 360 / 8640

  gdal_resize = function(source, destination, res){
    # https://gdal.org/programs/gdal_translate.html
    # gdal_translate -tr 0.1 0.1 "C:\dem.tif" "C:\dem_0.1.tif"
    sf::gdal_utils("translate",
                   source = source,
                   destination = str_replace(destination, "\\.tif","\\_temp\\.tif"),
                   options = c("-tr", res, res))
    # gdal_translate -projwin ulx uly lrx lry inraster.tif outraster.tif`
    sf::gdal_utils("translate",
                   source = str_replace(destination, "\\.tif","\\_temp\\.tif"),
                   destination = destination,
                   options = c("-projwin",
                               112,
                               -10,
                               154,
                               -44))
    file.remove(str_replace(destination, "\\.tif","\\_temp\\.tif"))

    destination
  }
  singleband_raster_to_dataframe = function(img){
    img_brick = img %>%
      raster::raster() %>%
      raster::brick()

    col_name = img_brick@data@names[1]

    img_brick@data@values %>%
      as.data.frame() %>%
      set_names(nm = str_to_lower(col_name))
  }

  df = files %>%
    map2_chr(.y = dest_files,
             .f = gdal_resize,
             res = res) %>%
    map_dfc(singleband_raster_to_dataframe)
  df
}

input_folder = glue("{root}/input")
output_folder = glue("{root}/output")

df = folder_of_tifs_to_df(input_folder, output_folder)

df_points = list(
  lon = seq(from  = 112,
            to    = 154 - res,
            by    = res),
  lat = seq(from  = -44,
            to    = -10 - res,
            by    = res)) %>%
  cross_df() %>%
  arrange(desc(lat), lon)

df_all = df_points %>% bind_cols(df)
df_all %>%
  select(-matches("(lat|lon)\\d")) %>%
  data.table::fwrite(glue("{root}/df_wms.csv"))
