scopus_cleaned <- 
  scopus |> 
  select(SR, CR) |> 
  na.omit()  |> 
  separate_rows(CR, sep = "; ") |> 
  mutate(PY = str_extract(CR, "\\([0-9]{4}\\)"),
         PY = str_remove_all(PY, "[\\(\\)]")) |> 
  na.omit() |> 
  mutate(AU = str_extract(CR, ".*\\([0-9]{4}\\)"),
         AU = str_extract(AU, ".*\\.,"),
         AU = gsub("([^,]+,[^,]+),", "\\1;", AU),
         AU = str_sub(AU, 1, nchar(AU)-1),
         AU = str_replace_all(AU,
                              pattern = "; ",
                              replacement = ";"),
         AU = str_remove_all(AU, pattern = "\\."),
         AU = str_remove_all(AU, pattern = ",")) |>
  mutate(type_ref = if_else(str_detect(CR,
                                       "\\., \\("), 2, # books
                            if_else(str_detect(CR,
                                               "^\\([0-9]{4}\\)"), 3,
                                    if_else(str_detect(CR, 
                                                       " \\([0-9]{4}\\), "), 4,
                                                    1)))) |> # papers
  mutate(TI = if_else(type_ref == 1,
                      str_extract(CR,
                                  ".*\\([0-9]{4}\\)"),
                      CR)) |>
  mutate(TI = if_else(type_ref == 1,
                      str_remove(TI, "\\([0-9]{4}\\)"),
                      TI)) |>
  mutate(TI = if_else(type_ref == 1,
                      str_remove(TI, ".*\\., "),
                      TI)) |>
  mutate(TI = if_else(type_ref == 1,
                      str_trim(TI),
                      TI)) |>
  mutate(TI = if_else(type_ref == 2, 
                      str_extract(CR, 
                                  "\\([0-9]{4}\\).*"), 
                      TI)) |> 
  mutate(TI = if_else(type_ref == 2, 
                      str_remove(TI, "\\([0-9]{4}\\)"), 
                      TI)) |> 
  mutate(TI = if_else(type_ref == 2, 
                      str_remove(TI, ", [0-9].*"), 
                      TI)) |> 
  mutate(TI = if_else(type_ref == 2, 
                      str_trim(TI), 
                      TI)) |> 
  mutate(TI = if_else(type_ref == 3, 
                      str_remove(CR, "\\([0-9]{4}\\)"), 
                      TI)) |> 
  mutate(TI = if_else(type_ref == 3, 
                      str_remove(TI, ", ,.*"), 
                      TI)) |> 
  mutate(TI = if_else(type_ref == 3, 
                      str_trim(TI), 
                      TI)) |> 
  mutate(TI = if_else(type_ref == 4, 
                      str_extract(CR, 
                                  ".* \\([0-9]{4}\\) "), 
                      TI)) |> 
  mutate(TI = if_else(type_ref == 4, 
                      str_remove(TI, "\\([0-9]{4}\\)"), 
                      TI)) |> 
  mutate(TI = if_else(type_ref == 4, 
                      str_remove(TI, ".*\\., "), 
                      TI)) |> 
  mutate(TI = if_else(type_ref == 4, 
                      str_trim(TI), 
                      TI)) |> 
  mutate(JI = if_else(type_ref == 1, 
                      str_remove(CR, ".*\\([0-9]{4}\\)"), 
                      CR)) |> 
  mutate(JI = if_else(type_ref == 1, 
                      str_remove(JI, ", .*"), 
                      JI)) |> 
  mutate(JI = if_else(type_ref == 1, 
                      str_trim(JI), 
                      JI)) |> 
  filter(type_ref == 1) |> 
  filter(JI != "") |> 
  mutate(JI = str_remove_all(JI, "\\.")) |> 
  mutate(SR_ref = gsub("^(.*?);.*", "\\1", AU),
         SR_ref = str_c(SR_ref, ", ", PY, ", ", JI, sep = ""))
