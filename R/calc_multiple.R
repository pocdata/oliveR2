#' Title
#'
#' @param obs_window_start
#' @param obs_window_stop
#'
#' @return
#' @export
#'
#' @examples

calc_multiple <- function(bld_sch_name = "independent"){

  message("set connection to build schema... ", appendLF = FALSE)

  suppressMessages(establish_con_olvr_rplc(set_schema = bld_sch_name))

  message("done")

  message("Querying data... ", appendLF = FALSE)

  dat <- DBI::dbGetQuery(con, "SELECT org_id AS id_provider_dim_pcv
                         , ref_id
                         , assigned_date
                         , agreed_date
                         , scheduled_date
                         , work_days_elapsed(assigned_date, agreed_date) AS pm1
                         , work_days_elapsed(assigned_date, scheduled_date) AS pm2
                         , CASE WHEN independent.work_days_elapsed(assigned_date, agreed_date) <= 3 THEN 1 ELSE 0 END AS pm1_goal
                         , CASE WHEN independent.work_days_elapsed(assigned_date, scheduled_date) <= 7 THEN 1 ELSE 0 END AS pm2_goal
                         FROM assigned_agreed_scheduled;")

  message("done")

  message("* CALCULATE METRIC TABLES *")

  calc_function_list <- tribble(
    ~f,                                          ~params,
    ## Average Number of Days from Accepted by Provider to Scheduled (PM #1) X --Scheduling DONE
    ## % of Referrals Scheduled within 3 Days from date Accepted by provider (PM #1) X --Scheduling DONE
    "calc_pm",                        list(data = dat
                                            , start = "assigned"
                                            , stop = "agreed"
                                            , metric = "pm1"
                                            , target = 3),

    ## Average Days from Accepted by Provider to First Scheduled Visit (PM #2)
    ## % of Referrals with a Scheduled Visit within 7 Days of provider Accepting referral (PM #2)
    "calc_pm",                        list(data = dat
                                           , start = "assigned"
                                           , stop = "scheduled"
                                           , metric = 'pm2'
                                           , target = 7),
    "calc_prop_missed_visit_prvdr_fault",            list(),
    "calc_child_count_referral",                     list()
  )

  out <- purrr::invoke_map(calc_function_list$f
                           ,calc_function_list$params)

  # Removing data object from memory

  # rm(dat)

}


