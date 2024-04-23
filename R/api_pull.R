library(tidyverse)

pull_redcap_data <- function(token, url){

  ### Pulling raw data
  require(tidyverse)
  formData_raw <- list("token"=token,
                       content='record',
                       action='export',
                       format='csv',
                       type='flat',
                       csvDelimiter='',
                       rawOrLabel='raw',
                       rawOrLabelHeaders='raw',
                       exportCheckboxLabel='false',
                       exportSurveyFields='false',
                       exportDataAccessGroups='false',
                       returnFormat='csv'
  )
  response_raw <- httr::POST(url, body = formData_raw, encode = "form")
  result_raw <- httr::content(response_raw)

  ### Pulling labelled data
  formData_label <- list("token"=token,
                         content='record',
                         action='export',
                         format='csv',
                         type='flat',
                         csvDelimiter='',
                         rawOrLabel='label',
                         rawOrLabelHeaders='raw',
                         exportCheckboxLabel='true',
                         exportSurveyFields='false',
                         exportDataAccessGroups='false',
                         returnFormat='csv'
  )
  response_label <- httr::POST(url, body = formData_label, encode = "form")
  result_label <- httr::content(response_label)

  ###Pulling data dictionary
  formData_dict <- list("token"=token,
                        content='metadata',
                        format='csv',
                        returnFormat='csv'
  )
  response_dict <- httr::POST(url, body = formData_dict, encode = "form")
  data_dict <- httr::content(response_dict)

  ### Get radio and dropdown column names
  fact_cols <- data_dict %>%
    filter(field_type %in% c("radio", "dropdown")) %>%
    pull(field_name)

  ### Get checkbox column names
  chec_box_cols <- data_dict %>%
    filter(field_type %in% c("checkbox")) %>%
    pull(field_name)

  ### Select those columns from labelled data and rename each column
  ### by adding .factor at end
  fact_df <- result_label %>%
    select(all_of(fact_cols), all_of(starts_with(chec_box_cols))) %>%
    rename_with( ~ paste0(., ".factor"), everything())

  ### Combine the factored columns with raw data
  df <- result_raw %>%
    bind_cols(fact_df)

  return(df)
}









