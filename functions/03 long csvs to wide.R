pacman::p_load(tidyverse, ncdf4, data.table)
# TODO
# think about multiple years?

files = "input/02 aus csv" %>%
  list.files(full.names = T, pattern = ".csv") %>%
  here::here()

extract_common = function(){
  "input/02 aus csv" %>%
    list.files(full.names = T, pattern = ".csv") %>%
    here::here() %>%
    magrittr::extract2(1) %>%
    fread() %>%
    select(year, month, lon, lat)
}

extract_attribute = function(file){
  all_data = file %>% fread()
  attribute = all_data$attribute[[1]]
  df = all_data %>% transmute(!! attribute := value)

  rm(all_data)
  gc()
  df
}

df_common = extract_common()
df_attributes = files %>% map_dfc(extract_attribute)

df = df_common %>% bind_cols(df_attributes)
df %>% fwrite("input/03 wide csv/wide.csv")
