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
           ref_type = type_ref, CR_ref = CR) |>
    filter(!is.na(SR_ref))
  # Finding right SR_ref in main dataset
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

  # Finding duplicate titles in references with different SR_ref
  TI_ref_duplicates <-
    references_df_1 |>
    filter(ref_type == 1) |>
    group_by(TI, SR_ref) |>
    count(TI, sort = TRUE) |>
    ungroup() |>
    select(TI) |>
    group_by(TI) |>
    count(TI) |>
    filter(n > 1) |>
    select(TI)

  # Create the

  df_1 <- references_df

  for (i in TI_ref_duplicates$TI) {

    SR_ref_1 <-
      references_df_1 |>
      filter(TI == i) |>
      select(SR_ref) |>
      group_by(SR_ref) |>
      count(SR_ref, sort = TRUE) |>
      ungroup() |>
      slice(1) |>
      select(SR_ref)

    df_1 <-
      df_1 |>
      mutate(SR_ref = if_else(TI == i, SR_ref_1$SR_ref,
                              SR_ref))

  }

  return(references_df = df_1)
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

get_citation_network <- function(scopus_df, references_df) {

  sca_citation_network <-
    references_df |>
    filter(ref_type == 1) |>
    select(SR, SR_ref) |>
    na.omit() |>
    as_tbl_graph()

  sca_data_cleaned_1 <-
    scopus_df |>
    select(SR, TI, PY) |>
    bind_rows(references_df |>
                select(SR = SR_ref,
                       TI,
                       PY)) |>
    dplyr::distinct() |>
    dplyr::rename(name = SR) |>
    filter(!duplicated(name))

  sca_citation_network_1 <-
    sca_citation_network |>
    activate(nodes) |>
    left_join(sca_data_cleaned_1, by = "name")

}

get_citation_network_tos <- function(citation_network) {

  citation_network_gc <-
    citation_network |>
    activate(nodes) |>
    mutate(component = group_components(type = "weak")) |>
    filter(component == 1)

  subfields <-
    citation_network_gc |>
    tidygraph::to_undirected() |>
    activate(nodes) |>
    mutate(subfield = tidygraph::group_louvain()) |>
    as_tibble() |>
    select(name, subfield)

  citation_network_subfield <-
    citation_network_gc |>
    activate(nodes) |>
    left_join(subfields, by = "name")

  df_tos = tibble()

  for (i in unique(subfields$subfield) ) {

    df_1 <-
      citation_network_subfield |>
      activate(nodes) |>
      filter(subfield == i) |>
      mutate(in_degree = centrality_degree(mode = "out"),
             out_degree = centrality_degree(mode = "in"),
             bet = centrality_betweenness()) |>
      as_tibble() |>
      select(name,
             in_degree,
             out_degree,
             bet)

    df_tos <-
      df_tos |>
      bind_rows(df_1)

  }

  citation_network_tos <-
    citation_network_subfield |>
    activate(nodes) |>
    left_join(df_tos, by = "name") |>
    select(-component)

}

get_sap <- function(citation_network) {

}
