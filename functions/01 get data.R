pacman::p_load(tidyverse, curl)
codes = read_csv("input/00 terraclimate codes/codes.csv", col_types = c("cc"))
year = 2018

url_builder = function(code, year){
  glue::glue("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_{str_to_upper(code)}_{year}.nc")
}

df = codes %>%
  mutate(url = map_chr(code, url_builder, year = year),
         destfile = glue::glue("input/01 climate ncdf/{year}_{code}.nc"))

df %>% select(url, destfile) %>% pwalk(curl_download)
