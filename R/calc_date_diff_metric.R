#' Title
#'
#' @param start
#' @param stop
#' @param contract_network
#' @param obs_window_start
#' @param obs_window_stop
#' @param bld_sch_name
#' @param establish_con
#' @param exclude_negative
#' @param target
#' @param weekend_holiday_credit
#' @param jitter
#'
#' @return
#' @export
#'
#' @examples
calc_date_diff_metric <- function(start = "assigned"
                                ,stop = "agreed"
                                ,contract_network = 2
                                ,obs_window_start = as.Date(ifelse(ymd(20170920) + days(180) > rollback(today())
                                                                   ,ymd(20170920), rollback(today()) - days(180))
                                                            ,origin = "1970-01-01")
                                ,obs_window_stop = today()
                                ,bld_sch_name = "independent"
                                ,establish_con = TRUE
                                ,exclude_negative = TRUE
                                ,target = 7
                                ,weekend_holiday_credit = TRUE
                                ,jitter = FALSE){

  message(paste0("* begin build procedure for "
                 ,start
                 ," to "
                 ,stop
                 ," metric *")
  )

  if (establish_con) {
    message("set connection to build schema... ", appendLF = FALSE)

    suppressMessages(establish_con_olvr_rplc(set_schema = bld_sch_name))

    message("done")
  }

  message("get table build date... ", appendLF = FALSE)
  table_build_date <- tbl(con, "visitation_referral_fact") %>%
    select(id_calendar_dim_table_update) %>%
    as_data_frame() %>%
    distinct() %>%
    .$id_calendar_dim_table_update
  table_build_date <- ymd(table_build_date)
  message("done")

  if (!is.na(contract_network)) {
    message("gather network contracts... ", appendLF = FALSE)

    DBI::dbSendQuery(con, dbplyr::build_sql("SET search_path TO ", 'staging'))

    tbl_network_contracts <- tbl(con, "OrganizationContracts") %>%
      dplyr::filter(contractOwnerId == contract_network
             ,is.na(deletedAt)) %>%
      arrange(desc(updatedAt)) %>%
      distinct(contractOwnerId
               ,contractedOrganizationId) %>%
      select(id_organization = contractedOrganizationId) %>%
      as_data_frame()

    message("done")
  }

  DBI::dbSendQuery(con, dbplyr::build_sql("SET search_path TO ", bld_sch_name))

  message("set state version limit... ", appendLF = FALSE)

  v1_limit <- tbl(con, "calendar_dim") %>%
    dplyr::filter(fl_pre_timeline) %>%
    summarise(limit = max(id_calendar_dim)) %>%
    pull(limit)

  message("done")

  message("get calendar_dim... ", appendLF = FALSE)
  calendar_dim <- tbl(con, "calendar_dim") %>% as_data_frame()
  message("done")

  message("format start times... ", appendLF = FALSE)
  tbl_start <- tbl(con, "visitation_referral_fact") %>%
    select(contains(start)
           ,id_visitation_referral_fact) %>%
    as_data_frame()

  tbl_start_cols <- colnames(tbl_start)

  var_rename_start <- function(var) {
    var <- rep("start_combo", length(var))
    return(var)
  }

  var_rename_stop <- function(var) {
    var <- rep("stop_combo", length(var))
    return(var)
  }

  if(length(tbl_start_cols) > 2){

    tbl_start <- tbl_start %>%
      select(id_visitation_referral_fact, contains('v2')) %>%
      mutate_at(vars(ends_with("v2")), funs(ifelse(. > v1_limit, ., NA)))

    names(tbl_start) <- c('id_visitation_referral_fact', 'start_combo')

  } else{
    tbl_start <- rename_at(.tbl = tbl_start
                           ,.vars = vars(contains(start))
                           ,.funs = var_rename_start)
  }
  message("done")

  message("format stop times... ", appendLF = FALSE)
  tbl_stop <- tbl(con, "visitation_referral_fact") %>%
    select(contains(stop)
           ,id_visitation_referral_fact) %>%
    as_data_frame()

  tbl_stop_cols <- colnames(tbl_stop)

  if(length(tbl_stop_cols) > 2){

    tbl_stop <- tbl_stop %>%
      select(id_visitation_referral_fact, contains('v2')) %>%
      mutate_at(vars(ends_with("v2")), funs(ifelse(. > v1_limit, ., NA)))

    names(tbl_stop) <- c('id_visitation_referral_fact', 'stop_combo')

  }  else{
    tbl_stop <- rename_at(.tbl = tbl_stop
                           ,.vars = vars(contains(stop))
                           ,.funs = var_rename_stop)
  }
  message("done")

  message("format recepit times... ", appendLF = FALSE)

  tbl_receipts <- tbl(con, "visitation_referral_fact") %>%
    select(id_calendar_dim_received_v2
           ,id_visitation_referral_fact) %>%
    as_data_frame()

  tbl_receipts <- tbl_receipts %>%
    select(id_visitation_referral_fact, contains('v2')) %>%
    mutate_at(vars(ends_with("v2")), funs(ifelse(. > v1_limit, ., NA)))

  names(tbl_receipts) <- c('id_visitation_referral_fact', 'date_marker')

  message("done")

  message("join start and stop dates... ", appendLF = FALSE)

  dat_start_stop <- inner_join(tbl_start, tbl_stop, by = "id_visitation_referral_fact")

  message("done")

  message("join relevant variables together... ", appendLF = FALSE)

  visitation_referral_providers <- tbl(con, "visitation_referral_fact") %>%
    select(id_visitation_referral_fact
           ,id_provider_dim_pcv) %>%
    as_data_frame()

  dat_start_stop <- inner_join(dat_start_stop
                               ,visitation_referral_providers
                               ,by = "id_visitation_referral_fact")

  dat_start_stop <- left_join(dat_start_stop
                              ,tbl_receipts
                              ,by = "id_visitation_referral_fact")

  message("done")

  message("calculate durations... ", appendLF = FALSE)

  dat_start_stop <- dat_start_stop %>%
    mutate(interval = interval(ymd(start_combo), ymd(stop_combo))
           ,period = as.period(interval, "days")
           ,period = ifelse(exclude_negative &
                              start_combo > stop_combo
                            ,NA
                            ,as.numeric(period, "days"))
           ,period = ifelse(jitter & !is.na(period)
                            ,ceiling(jitter(period))
                            ,period)
           ,measure_value = ddays(period)
           ,measure_value = as.numeric(measure_value, "days")
           ) %>%
    select(-period, -interval)

  message("done")

  if (weekend_holiday_credit){

    message("calculate weekend or holiday overlaps... ", appendLF = FALSE)
    weekend_or_holiday <- calendar_dim %>%
      mutate(weekend_or_holiday = coalesce(calendar_date_weekend, calendar_date_holiday)) %>%
      dplyr::filter(calendar_date <= now()
             ,!is.na(weekend_or_holiday)) %>%
      select(id_calendar_dim, calendar_date)

    weekend_holiday_overlap <- fuzzyjoin::fuzzy_left_join(dat_start_stop
                                                          ,weekend_or_holiday
                                                          ,by = c("start_combo" = "id_calendar_dim"
                                                                  ,"stop_combo" = "id_calendar_dim")
                                                          ,match_fun = list(`<=`, `>=`)
    ) %>% select(id_visitation_referral_fact, start_combo, stop_combo, id_calendar_dim) %>%
      dplyr::filter(!is.na(id_calendar_dim)) %>%
      group_by(id_visitation_referral_fact) %>%
      summarise(weekend_holiday_overlap = n()) %>%
      mutate(weekend_holiday_overlap = ifelse(is.na(weekend_holiday_overlap)
                                              ,0
                                              ,weekend_holiday_overlap))

    dat_start_stop <- left_join(dat_start_stop
                                ,weekend_holiday_overlap
                                ,by = "id_visitation_referral_fact") %>%
      mutate(measure_value = measure_value - weekend_holiday_overlap
             ,measure_value = ifelse(measure_value < 0, 0, measure_value)
             ,fl_met_goal = (measure_value <= target)
      )

    message("done")

  }

  if (!is.na(contract_network)) {
    message("restrict to network contracts... ", appendLF = FALSE)

    dat_start_stop <- dat_start_stop %>%
      dplyr::filter(id_provider_dim_pcv %in% tbl_network_contracts$id_organization)

    message("done")
  }

  file_path_long <- paste0(system.file('extdata'
                                  ,package = 'oliveR2')
                      ,'/'
                      ,paste0("chron_long_", start, "_to_", stop, ".rds")
  )

  readr::write_rds(dat_start_stop, file_path_long)

  if (!is.na(obs_window_start)) {
    message("applying observation window filter... ", appendLF = FALSE)

    dat_start_stop <- dat_start_stop %>%
      dplyr::filter(ymd(date_marker) >= obs_window_start)

    message("done")
  }

  if (!is.na(obs_window_stop)) {
    message("apply observation window filter... ", appendLF = FALSE)

    dat_start_stop <- dat_start_stop %>%
      dplyr::filter(ymd(date_marker) <= obs_window_stop)

    message("done")
  }

  message(")xxxxx[;;;;;;;;;> kill connections...", appendLF = FALSE)

  kill_pg_connections()

  message("aggregate measurements for providers... ", appendLF = FALSE)

  dat_measurement <- dat_start_stop %>%
    select(id_provider_dim_pcv, measure_value, fl_met_goal) %>%
    group_by(id_provider_dim_pcv) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    mutate_all(funs(ifelse(is.nan(.), NA, .))) %>%
    mutate(starting_state = start
           ,stopping_state = stop
           ,target = target
           ,table_build_date = table_build_date
           ,obs_window_start = obs_window_start
           ,obs_window_stop = obs_window_stop)

  file_path_short1 <- paste0(system.file('extdata'
                                       ,package = 'oliveR2')
                           ,'/'
                           ,paste0("chron_", start, "_to_", stop, ".rds")
  )

  readr::write_rds(dat_measurement, file_path_short1)

  message("done")

  message("aggregate measurements for network... ", appendLF = FALSE)

  dat_measurement_all <- dat_start_stop %>%
    select(id_provider_dim_pcv, measure_value, fl_met_goal) %>%
    select(measure_value, fl_met_goal) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    mutate_all(funs(ifelse(is.nan(.), NA, .))) %>%
    mutate(starting_state = start
           ,stopping_state = stop
           ,target = target
           ,table_build_date = table_build_date
           ,obs_window_start = obs_window_start
           ,obs_window_stop = obs_window_stop)

  file_path_short2 <- paste0(system.file('extdata'
                                        ,package = 'oliveR2')
                            ,'/'
                            ,paste0("chron_network_", start, "_to_", stop, ".rds")
  )

  readr::write_rds(dat_measurement_all, file_path_short2)

  message("done")

}
