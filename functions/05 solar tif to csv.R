pacman::p_load(tidyverse, curl, sf, glue)

#https://globalsolaratlas.info/download/australia
root = "input/05 solar"
file_name = "Australia_GISdata_LTAy_YearlyMonthlyTotals_GlobalSolarAtlas-v2_GEOTIFF"

zip_url = glue("https://api.globalsolaratlas.info/download/Australia/{file_name}.zip")
zip_path = glue("{root}/{file_name}.zip")

curl_download(zip_url, zip_path)
zip::unzip(zip_path, exdir = glue("{root}"))

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

expand_names = c("PVOUT" = "photovoltaic_power_potential",
                 "GHI" = "global_horizontal_irradiation",
                 "DIF" = "diffuse_horizontal_irradiation",
                 "GTI" = "global_irradiation_for_optimally_tilted_surface",
                 "DNI" = "direct_normal_irradiation",
                 "OPTA" = "optimum_tilt_to_maximize_yearly_yield")

dest_files = input_folder %>%
  list.files(pattern = "tif$") %>%
  sort() %>%
  str_c(output_folder,"/",.) %>%
  str_replace_all(expand_names)

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

annual_input = glue("{root}/{file_name}")
annual_output = glue("{root}/scaled/annual")
monthly_input = glue("{root}/{file_name}/monthly")
monthly_output = glue("{root}/scaled/monthly")

df_monthly = folder_of_tifs_to_df(monthly_input, monthly_output)
df_annual  = folder_of_tifs_to_df(annual_input, annual_output)

df_points = list(
  lon = seq(from  = 112,
            to    = 154 - res,
            by    = res),
  lat = seq(from  = -44,
            to    = -10 - res,
            by    = res)) %>%
  cross_df() %>%
  arrange(desc(lat), lon)

df = df_points %>% bind_cols(df_annual, df_monthly)
df %>% data.table::fwrite(glue("{root}/df_solar.csv"))

plotter = function(fill, ...){
  fill_enq = enquo(fill)
p = df %>%
  na.omit() %>%
  ggplot(aes(x = lon,
             y = lat,
             fill = !!fill_enq)) +
  geom_raster() +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(...) +
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
  ggthemr::no_legend_title()


ggsave(plot = p,
       filename = glue::glue("plot/solar/{quo_name(fill_enq)}.png"),
       width = 5,
       height = 5,
       units = "cm",
       dpi = 320,
       scale = 3,
       limitsize = F
)}
plotter(dif,
        title = "Diffuse Horizontal Irradiation",
        subtitle = expression( frac(kWh, m^2 )),
        caption = "Source: https://globalsolaratlas.info/download/australia"
        )

plotter(dni,
        title = "Direct Normal Irradiation",
        subtitle = expression( frac(kWh, m^2 )),
        caption = "Source: https://globalsolaratlas.info/download/australia"
)
