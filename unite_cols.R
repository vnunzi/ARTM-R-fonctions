library(dplyr)
library(tidyr)
library(stringr)


unite_cols <- function(
    data,
    col,
    vars,
    sep = "_",
    condition = TRUE,
    name_start =  1,
    name_end   = -1,
    remove = TRUE
){
  
  ## création d'un identifiant unique des éléments de data :
  data_copy <- data %>%
    dplyr::mutate(uid = 1:nrow(data))
  
  ## pivot longueur selon les colonnes à unir :
  df <- data_copy %>% tidyr::pivot_longer(
    cols = {{vars}},
    names_to  = "NAMES",
    values_to = "VALUES"
  )
  
  ## filtre selon la condition :
  if (is.na(condition)) {
    df <- df %>% dplyr::filter(is.na(VALUES))
  }
  
  else{
    df <- df %>% dplyr::filter(VALUES == condition)
  }
  
  ## formattage des champs et summarise :
  df <- df %>%
    mutate(
      NAMES = str_sub(NAMES, name_start, name_end)
    ) %>%
    group_by(uid) %>%
    summarise(
      !!col := paste(NAMES, collapse = sep)
    )
  
  ## fusion avec le dataframe d'origine :
  output <- left_join(data_copy, df[c("uid", col)],
                      by = "uid") %>%
    select(-uid) %>%
    relocate({{col}}, .after = {{vars}})
  
  ## rejet (ou non) des colonnes d'entrée :
  if (remove){
    output <- output %>% dplyr::select(-{{vars}})
  }
  
  return(output)
}