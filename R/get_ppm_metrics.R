#' Title
#'
#' @param org_id
#'
#' @return
#' @export
#'
#' @examples
get_ppm_metrics <- function(org_id) {

x <- data_frame(id = org_id)

x$acceptance_to_schedule = ppm_helper_single(org_id
                                              ,df_file = "chron_assigned_to_agreed.rds"
                                              ,org_col = "id_provider_dim_pcv"
                                              ,label = "Days Until Visit is Scheduled"
                                              ,sub_label_pre = ''
                                              ,sub_label_post = '% Scheduled within 3 Days')

x$acceptance_to_first_visit = ppm_helper_single(org_id = org_id
                                                  ,df_file = "chron_assigned_to_scheduled.rds"
                                                  ,org_col = "id_provider_dim_pcv"
                                                  ,label = "Days Until First Visit, as Planned"
                                                  ,sub_label_pre = ''
                                                  ,sub_label_post = '% Planned within 7 Days')

x$child_count_value = ppm_helper_single(org_id = org_id
                                                ,df_file = "count_child_count.rds"
                                                ,org_col = "id_provider_dim_pcv"
                                                ,label = "Children per Referral"
                                                ,primary_only = TRUE)

x$attendance_per_scheduled_visit = ppm_helper_single(org_id = org_id
                                        ,df_file = "prop_missed_visit_by_pvd.rds"
                                        ,org_col = "id_provider_dim_pcv"
                                        ,label = "Rate of Provider Cancellations"
                                        ,primary_percent = TRUE
                                        ,primary_only = TRUE
                                        ,sub_label_pre = "Among 24-Hour Cancellations"
                                        )


x

}
