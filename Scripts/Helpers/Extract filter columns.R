lines <- readLines(here("Data", "columns_per_year.txt"))

data_spec <- tibble(line = lines) %>%
  mutate(
    dataset = str_extract(line, "^[^_]+"),
    year = str_extract(line, "\\d{4}"),
    cols = str_extract(line, "(?<=select\\().*(?=\\))")
  )
