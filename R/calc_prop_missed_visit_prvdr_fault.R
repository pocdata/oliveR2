#' Title
#'
#' @param contract_network
#' @param measurement_name
#' @param obs_window_start
#' @param obs_window_stop
#' @param bld_sch_name
#' @param establish_con
#' @param tz
#'
#' @return
#' @export
#'
#' @examples
calc_prop_missed_visit_prvdr_fault <- function(contract_network = 2
                                               ,tz = 'America/Los_Angeles'
                            ,measurement_name = "missed_visit_by_pvd"
                            ,obs_window_start = as.Date(ifelse(ymd(20170920) + days(180) > rollback(today())
                                                               ,ymd(20170920), rollback(today()) - days(180))
                                                        ,origin = "1970-01-01")
                            ,obs_window_stop = rollback(today())
                            ,bld_sch_name = "independent"
                            ,establish_con = TRUE){

  message(paste0("* begin build procedure for "
                 ,measurement_name
                 ," metric *")
  )

  if (establish_con) {
    message("set connection to build schema... ", appendLF = FALSE)

    suppressMessages(establish_con_olvr_rplc(bld_sch_name ))

    message("done")
  }

  message("Getting data from SQL... ", appendLF = FALSE)

  dat <- DBI::dbGetQuery(con, "SELECT org_id
                                      , provider_caused
                                      , cancelled_visits
                               FROM independent.assigned_agreed_scheduled aas
                               JOIN staging.\"ServiceReferrals\" sr
                                  ON aas.ref_id = sr.id
                               LEFT OUTER JOIN
                               (SELECT \"serviceReferralId\" ref_id
                                  , count(1) FILTER (WHERE \"cancellationType\" = 'Cancelled (less than 24 hour notice)' AND \"causedBy\" = 'Provider') provider_caused
                                  , count(1) FILTER (WHERE \"cancellationType\" = 'Cancelled (less than 24 hour notice)') cancelled_visits
                                FROM staging.\"VisitReports\"
                                WHERE \"deletedAt\" IS NULL
                                  AND \"isCurrentVersion\" = true
                                GROUP BY
                                  \"serviceReferralId\") vr
                                USING(ref_id)
                                WHERE NOT agreed_date IS NULL
                                  AND NOT scheduled_date IS NULL
                                  AND \"deletedAt\" IS NULL
                                  AND \"isCurrentVersion\" = true")

  message("Calculate missed visits... ", appendLF = FALSE)

  missed_visists <-
    group_by(dat, org_id) %>%
    summarize(measure_value = sum(provider_caused, na.rm = TRUE) / sum(cancelled_visits, na.rm = TRUE)) %>%
    rename(id_provider_dim_pcv = org_id)

  missed_visists$measurement_name <- 'missed_visit'
  missed_visists$table_build_date <- today()
  missed_visists$obs_window_start <- NA
  missed_visists$obs_window_stop <- NA

  dat_measurement <- select(missed_visists
                            , id_provider_dim_pcv
                            , measurement_name
                            , table_build_date
                            , obs_window_start
                            , obs_window_stop
                            , measure_value)

  file_path_short1 <- paste0(system.file('extdata'
                                         ,package = 'oliveR2')
                             ,'/'
                             ,paste0("prop_", measurement_name, ".rds")
  )

  readr::write_rds(dat_measurement, file_path_short1)

  message("done")

  message(")xxxxx[;;;;;;;;;> kill connections...", appendLF = FALSE)

  kill_pg_connections()

  message("done")

}
