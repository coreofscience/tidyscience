get_references <- function(data) {

  references_df <-
    data |>
    select(SR, CR) |>
    na.omit()  |>
    separate_rows(CR, sep = "; ") |>
    mutate(PY = str_extract(CR, "\\([0-9]{4}\\)"),
           PY = str_remove_all(PY, "[\\(\\)]"),
           PY = as.numeric(PY)) |>
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
    mutate(TI = if_else(type_ref == 1,
                        str_remove(TI, "\""),
                        TI)) |>
    mutate(TI = if_else(type_ref == 1,
                        str_remove(TI, "\"$"),
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
    filter(JI != "") |>
    mutate(JI = str_remove_all(JI, "\\.")) |>
    mutate(SR_ref = gsub("^(.*?);.*", "\\1", AU),
           SR_ref = str_c(SR_ref, ", ", PY, ", ", JI, sep = "")) |>
    select(SR, SR_ref, TI, AU, JI, PY,
           ref_type = type_ref, CR_ref = CR)

  references_df_1 <- tibble()

  list_ref_TI <-
    references_df |>
    filter(ref_type == 1) |>
    select(TI) |>
    dplyr::distinct()

  for (i in list_ref_TI$TI) {

    df_1 <-
      scopus_df |>
      filter(TI == i)

    if (length(df_1$TI) != 0) {


      references_df$SR_ref[references_df$TI == df_1$TI] <- df_1$SR

      references_df_1 <-
        references_df |>
        mutate(SR_ref = replace(SR_ref,
                                TI == i,
                                df_1 |>
                                  filter(TI == i) |>
                                  select(SR) |>
                                  pull()))
    }

  }
    # add_column(DE = NA, .after = "AU",
    #            ID = NA,
    #            C1 = NA,
    #            AB = NA,
    #            PA = NA,
    #            AR = NA,
    #            chemicals_cas = NA,
    #            coden = NA,
    #            RP = NA,
    #            DT = NA,
    #            DI = NA,
    #            BE = NA,
    #            FU = NA,
    #            BN = NA,
    #            SN = NA,
    #            SO = NA,
    #            LA = NA,
    #            TC = NA,
    #            PN = NA,
    #            page_count = NA,
    #            PP = NA,
    #            PU = NA,
    #            PM = NA,
    #            DB = NA,
    #            sponsors = NA,
    #            url = NA,
    #            VL = NA,
    #            FX = NA,
    #            AU_UN = NA,
    #            AU1_UN = NA,
    #            AU_UN_NR = NA,
    #            SR_FULL = NA) |>
    # select(AU, DE, ID, C1, CR, JI, AB, PA, AR, chemicals_cas,
    #        coden, RP, DT, DI, BE, FU, BN, SN, SO,
    #        LA, TC, PN, page_count, PP, PU, PM, DB, sponsors, url,
    #        VL, PY, FX, AU_UN, AU1_UN, AU_UN_NR, SR_FULL, SR)

  # preprossed_data <-
  #   data |>
  #   bind_rows(scopus_cleaned )

  return(references_df = references_df_1)
}


get_asn <- function(data) {

  data_tidied_scopus_ref <-
    data |>
    dplyr::select(SR, CR) |>
    separate_rows(CR, sep = "; ") |>
    na.omit() |>
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
    na.omit()|>
    rename(main_ref = SR,
           id_ref = CR)

  dummy <-
    data_tidied_scopus_ref |>
    dplyr::select(AU, PY) |>
    mutate(PY = as.numeric(PY)) |>
    bind_rows(data |>
                dplyr::select(AU, PY)) |>
    unique()

  asn <-
    biblioNetwork(M = data.frame(dummy),
                  analysis = "collaboration",
                  network = "authors") |>
    graph_from_adjacency_matrix(mode = "undirected",
                                weighted = TRUE) |>
    simplify() |>
    as_tbl_graph() |>
    activate(nodes) |>
    mutate(communities = group_components(type = "weak")) |>
    filter(communities == 1)

  return(asn)
}
