#' Title
#'
#' @param org_id
#' @param df_file
#' @param org_col
#' @param label
#' @param primary_only
#' @param sub_label_pre
#' @param sub_label_post
#' @param primary_percent
#'
#' @return
#' @export
#'
#' @examples
ppm_helper_single <- function(org_id
                               ,df_file = "chron_assigned_to_scheduled.rds"
                               ,org_col = "id_provider_dim_pcv"
                               ,label = NA
                               ,units = NA
                               ,primary_only = FALSE
                               ,sub_label_pre = ''
                               ,sub_label_post = ''
                               ,primary_percent = FALSE
                              ) {

  if (primary_only) {
    measure_extract_single(df_file = df_file
                                              ,measure_col = "measure_value"
                                              ,org_col = org_col
                                              ,org_id = org_id
                                              ,round_plc = 2
                                              ,primary_label = label
                                              ,secondary_label = sub_label_pre
                                              ,tertiary_label = sub_label_post
                                              ,units = units) %>%
      mutate(label = primary_label
             ,threshold = NA
             ,label = primary_label
             ,sublabel = ifelse(any(all(is.na(secondary_label), is.na(secondary_label))
                                    ,all(secondary_label == '', secondary_label == ''))
                                ,NA
                                ,paste0(sub_label_pre
                                ,sub_label_post))
             ,value = ifelse(primary_percent, scales::percent(value), value)
             ) %>%
      select(threshold
             ,value
             ,label
             ,sublabel)

  } else {
    measure_primary <- measure_extract_single(df_file = df_file
                                              ,measure_col = "measure_value"
                                              ,org_col = org_col
                                              ,org_id = org_id
                                              ,round_plc = 1
                                              ,primary_label = label
                                              ,secondary_label = sub_label_pre
                                              ,tertiary_label = sub_label_post
                                              ,units = units)

    measure_secondary <- measure_extract_single(df_file = df_file
                                                ,measure_col = "fl_met_goal"
                                                ,org_col = org_col
                                                ,org_id = org_id
                                                ,round_plc = 2
                                                ,primary_label = label
                                                ,secondary_label = sub_label_pre
                                                ,tertiary_label = sub_label_post
                                                ,units = units)



    measure_primary %>%
      mutate(label = primary_label
             ,threshold = NA
             ,label = primary_label
             ,value = ifelse(primary_percent, scales::percent(value), value)
             ,sublabel = paste0(sub_label_pre
                                ,measure_secondary$value*100
                                ,sub_label_post)) %>%
      select(threshold
             ,value
             ,label
             ,sublabel)
  }

}
