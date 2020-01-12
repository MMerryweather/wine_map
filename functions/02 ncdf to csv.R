pacman::p_load(tidyverse, ncdf4, data.table)

files = "input/01 climate ncdf" %>%
  list.files(full.names = T, pattern = ".nc") %>%
  here::here()

# Australian bounding box
lat_range = c(-44, -10)
lon_range = c(112, 154)
res = 360 / 8640

per_file = function(file){

  year = str_extract(file,"[0-9]{4}") %>% as.numeric()
  message(glue::glue("opening {file}"))
  data = nc_open(file)
  attr_names = attributes(data$var)$names[1]
  dim_names = attributes(data$dim)$names
  message(glue::glue("    processing {str_pad(attr_names, 4, side = 'right')} - {year}"))

  df = ncvar_get(data, attr_names)
  lats = ncvar_get(data, dim_names[1])
  lons = ncvar_get(data, dim_names[2])

  # Assign names to dimensions so we can pivot long later on
  dimnames(df) = list(lon = lons, lat = lats)

  per_month = function(selected_month, df, year){
    message(glue::glue("        {year} - {selected_month}"))

    df_monthly = df[, , selected_month] %>%
      as.data.frame(row.names = row.names(.)) %>%
      mutate(lon = row.names(.),
             attribute = attr_names,
             year = year,
             month = selected_month) %>%
      select(attribute, year, month, lon, everything()) %>%
      pivot_longer(cols = -(attribute:lon),
                   names_to = "lat",
                   values_to = "value") %>%
      mutate_at(vars(month:value), as.numeric) %>%
      filter(between(lat, -44, -10) & between(lon, 112, 154))

    data.table::setDT(df_monthly)
    gc()
    df_monthly
    }

  tidy_data = seq_len(dim(df)[3]) %>%
    map(per_month, df = df, year = year) %>%
    rbindlist() %>%
    arrange(month, lat, lon, month)
  fwrite(tidy_data, glue::glue("input/02 aus csv/{year}_{attr_names}_aus.csv"))
  message(glue::glue("    finished {str_pad(attr_names, 4, side = 'right')} - {year}"))

}
files %>% walk(per_file)
