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

  message("* CALCULATE METRIC TABLES *")

  calc_function_list <- tribble(
    ~f,                                          ~params,
    ## Average Number of Days from Accepted by Provider to Scheduled (PM #1) X --Scheduling DONE
    ## % of Referrals Scheduled within 3 Days from date Accepted by provider (PM #1) X --Scheduling DONE
    "calc_pm",                        list(start = "assigned"
                                            , stop = "agreed"
                                            , metric = "pm1"
                                            , target = 3),

    ## Average Days from Accepted by Provider to First Scheduled Visit (PM #2)
    ## % of Referrals with a Scheduled Visit within 7 Days of provider Accepting referral (PM #2)
    "calc_pm",                        list(start = "assigned"
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


