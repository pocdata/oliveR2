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
calc_child_count_referral <- function(contract_network = 2
                            ,measurement_name = "child_count"
                            ,tz = 'America/Los_Angeles'
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

    suppressMessages(establish_con_olvr_rplc(set_schema = bld_sch_name))

    message("done")
  }

  dat <- DBI::dbGetQuery(con, "SELECT org_id AS id_provider_dim_pcv
                                      , round(avg((SELECT count(1) FROM json_array_elements(\"childDetails\"))), 1) measure_value
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
                                  AND \"isCurrentVersion\" = true
                                GROUP BY org_id")

  message("Writing data to file... ", appendLF = FALSE)

  child_count <- dat

  child_count$measurement_name <- "child_count"
  child_count$table_build_date <- today()
  child_count$obs_window_start <- NA
  child_count$obs_window_stop <- NA

  file_path_long <- paste0(system.file('extdata'
                                       ,package = 'oliveR2')
                           ,'/'
                           ,paste0("count_", measurement_name, ".rds")
  )

  readr::write_rds(child_count, file_path_long)

  message("done")

  message(")xxxxx[;;;;;;;;;> kill connections...", appendLF = FALSE)

  kill_pg_connections()

  message("done")

}
