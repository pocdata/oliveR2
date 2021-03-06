#' Title
#'
#' @param org_id
#'
#' @return
#' @export
#'
#' @examples

get_ppm_metrics <- function(org_id
                            , table_name = Sys.getenv("OLIVER_REPLICA_PPM_TABLE")){

  file_path <- paste0(system.file('extdata', package = 'oliveR2'),'/')

  df_file <- paste0(table_name, ".rds")

  dat <- readr::read_rds(paste0(file_path, df_file))

  dat <- dat[dat$org_id == org_id,]

  x <- data_frame(id = org_id)

  if(NROW(dat) == 0){

    dat_names <- names(dat)
    dat <- data.frame(matrix(NA, 1, NCOL(dat)))
    names(dat) <- dat_names

  }

  x$acceptance_to_schedule <- tibble(threshold = NA
                                     , value = dat$median_days_to_agreed
                                     , label = "Days Until Visit is Scheduled"
                                     , sublabel = ifelse(!is.na(dat$percent_agreed_in_3)
                                                         , paste0(dat$percent_agreed_in_3 * 100, "% Scheduled within 3 Days")
                                                         , NA))

  x$acceptance_to_first_visit = tibble(threshold = NA
                                       , value = dat$median_days_to_scheduled
                                       , label = "Days Until First Visit, as Planned"
                                       , sublabel = ifelse(!is.na(dat$percent_scheduled_in_7)
                                                           , paste0(dat$percent_scheduled_in_7 * 100, "% Planned within 7 Days")
                                                           , NA))

  x$attendence_rate = tibble(threshold = NA
                             , value = ifelse(!is.na(dat$percent_attended)
                                              , paste0(dat$percent_attended * 100, "%")
                                              , NA)
                             , label = "Attendance Rate"
                             , sublabel = "Among Scheduled Visits")

  x$attendance_per_scheduled_visit = tibble(threshold = NA
                                            , value = ifelse(!is.na(dat$percent_provider_caused)
                                                                    , paste0(dat$percent_provider_caused * 100, "%")
                                                                    , NA)
                                            , label = "Rate of Provider Cancellations"
                                            , sublabel = "Among 24-Hour Cancellations")


  x

}
