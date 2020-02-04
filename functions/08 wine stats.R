pacman::p_load(tidyverse, tidyxl, unpivotr, data.table)

root = "input/08 wine maps"
xl_path = glue("{root}/wine report/") %>%
  list.files("xlsx$", full.names = T) %>%
  sort() %>%
  rev() %>%
  .[1]
csv_map = glue("{root}/wine report/column_map.csv") %>% fread()
wine_colour = glue("{root}/wine report/wine_colour.csv") %>% fread()

xl = xlsx_cells(xl_path,
                include_blank_cells = TRUE,
                sheets = "Sheet1")

formats = xlsx_formats(xl_path)
coloured_text = which(!is.na(formats$local$font$color$rgb))
corners = xl %>%
  dplyr::filter(local_format_id %in% coloured_text & col == 1) %>%
  select(row, col)

grand_totals = xl %>%
  filter(col == 1 & str_detect(str_to_lower(character),"grand total")) %>%
  transmute(grand_total_row = row)

partitions = xl %>%
  select(row, col, data_type, character, numeric, local_format_id) %>%
  partition(corners) %>%
  bind_cols(grand_totals) %>%
  select(corner_row, corner_col, grand_total_row, cells)

test = filter(partitions, corner_row == 1408)
x = test$cells[[1]]

regions = function(x, corner_row, grand_total_row){
x %>%
  filter(row <= grand_total_row) %>%
  behead("NNW", region) %>%
  behead("NNW", delete_1) %>%
  behead("NNW", delete_2) %>%
  behead("W", varietal) %>%
  filter(row != min(row)) %>%
  inner_join(csv_map, by = c("col")) %>%
  select(region, varietal, category, subcategory, numeric)
  }

df_tidy = partitions %>%
  mutate(cells = pmap(list(cells, corner_row, grand_total_row), regions)) %>%
  unnest() %>%
  select(-corner_row, -corner_col, -grand_total_row) %>%
  left_join(wine_colour, by = "varietal")

df_tidy %>% fwrite(glue("{root}/wine report/wine_stats.csv"))
