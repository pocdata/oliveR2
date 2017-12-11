#' Title
#'
#'
#' @return
#' @export
#'
#' @examples

calc_pm <- function(data
                    , metric
                    , target
                    , start
                    , stop
                    , obs_window_start = as.Date(ifelse(ymd(20170920) + days(180) > rollback(today())
                                                        ,ymd(20170920), rollback(today()) - days(180))
                                                 ,origin = "1970-01-01")
                    , obs_window_stop = today()
                    , bld_sch_name = "independent"
){

  message("set connection to build schema... ", appendLF = FALSE)

  suppressMessages(establish_con_olvr_rplc(set_schema = bld_sch_name))

  message("done")

  message("Querying data for PPM1 and PPM2... ", appendLF = FALSE)

  dat <- DBI::dbGetQuery(con, "SELECT org_id AS id_provider_dim_pcv
                                      , ref_id
                                      , assigned_date
                                      , agreed_date
                                      , scheduled_date
                                      , work_days_elapsed(assigned_date, agreed_date) AS pm1
                                      , work_days_elapsed(assigned_date, scheduled_date) AS pm2
                                      , CASE WHEN independent.work_days_elapsed(assigned_date, agreed_date) <= 3 THEN 1 ELSE 0 END AS pm1_goal
                                      , CASE WHEN independent.work_days_elapsed(assigned_date, scheduled_date) <= 7 THEN 1 ELSE 0 END AS pm2_goal
                                FROM assigned_agreed_scheduled
                                WHERE agreed_date IS NOT NULL
                                  AND scheduled_date IS NOT NULL
                                ;")
  message("done")

  message(paste("Calculating", metric, "... "), appendLF = FALSE)

  metric_target <- paste0(metric, '_taget')

  stop_date <- names(dat)[str_detect(names(dat), stop)]

  # pm_dat <- dat[!is.na(dat[names(dat) == stop_date]),]

  pm_dat <- dat %>%
    select(id_provider_dim_pcv, contains(metric)) %>%
    group_by(id_provider_dim_pcv) %>%
    summarise_all(mean)

  names(pm_dat) <- str_replace(names(pm_dat), paste0(metric, '_goal'), 'fl_met_goal')
  names(pm_dat) <- str_replace(names(pm_dat), metric, 'measure_value')

  # adding extra data

  pm_dat$starting_state <- start
  pm_dat$stopping_state <- stop
  pm_dat$target <- target
  pm_dat$table_build_date <- today()
  pm_dat$obs_window_start <- obs_window_start
  pm_dat$obs_window_stop <- obs_window_stop

  file_path_short1 <- paste0(system.file('extdata'
                                         ,package = 'oliveR2')
                             ,'/'
                             ,paste0("chron_", start, "_to_", stop, ".rds")
  )

  readr::write_rds(pm_dat, file_path_short1)

  message(paste(metric, 'done'))

  message(")xxxxx[;;;;;;;;;> kill connections...", appendLF = FALSE)

  kill_pg_connections()

  message(paste(metric, 'done'))

}
