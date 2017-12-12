#' Title
#'
#' @param org_id
#'
#' @return
#' @export
#'
#' @examples

get_ppm_metrics <- function(org_id) {

  file_path <- paste0(system.file('extdata', package = 'oliveR2'),'/')

  df_file <- "provider_performance_metrics.rds"

  dat <- readr::read_rds(paste0(file_path, df_file))

  dat <- dat[dat$org_id == org_id,]

  x <- data_frame(id = org_id)

  if(NROW(dat) == 0){
    return(NA)
  } else {

  x$acceptance_to_schedule <- tibble(threshold = NA
                                     , value = dat$avg_days_to_scheduled
                                     , label = "Days Until Visit is Scheduled"
                                     , sublabel = ifelse(!is.na(dat$percent_agreed_in_3)
                                                         , paste0(dat$percent_agreed_in_3 * 100, "% Scheduled within 3 Days")
                                                         , NA))

  x$acceptance_to_first_visit = tibble(threshold = NA
                                       , value = dat$avg_days_to_agreed
                                       , label = "Days Until First Visit, as Planned"
                                       , sublabel = ifelse(!is.na(dat$percent_scheduled_in_7)
                                                           , paste0(dat$percent_scheduled_in_7 * 100, "% Planned within 7 Days")
                                                           , NA))

  x$child_count_value = tibble(threshold = NA
                               , value = dat$avg_num_children
                               , label = "Children per Referral"
                               , sublabel = NA)

  x$attendance_per_scheduled_visit = tibble(threshold = NA
                                            , value = dat$percent_provider_caused
                                            , label = "Rate of Provider Cancellations"
                                            , sublabel = "Among 24-Hour Cancellations")

  }
  x

  }












