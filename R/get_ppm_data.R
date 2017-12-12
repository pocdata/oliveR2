#' Title
#'
#' @param org_id
#'
#' @return
#' @export
#'
#' @examples
#'

get_ppm_data <- function(bld_sch_name = "independent"
                         , establish_con = TRUE
                         , table_name = "provider_performance_metrics"){

  if (establish_con) {
    message("establishing connection to database... ", appendLF = FALSE)

    suppressMessages(establish_con_olvr_rplc(bld_sch_name))

    message("done")
  }

  message("querying ppm data... ", appendLF = FALSE)

  # ppm_data <-
  #   tbl(con, "provider_performance_metrics") %>%
  #   rename(id_provider_dim_pcv = org_id)

  ppm_data <-
    DBI::dbGetQuery(con, "SELECT * FROM provider_performance_metrics")

  message("done")

  message(")xxxxx[;;;;;;;;;> kill connections...", appendLF = FALSE)

  kill_pg_connections()

  message("done")

  message("saving ppm data... ", appendLF = FALSE)

  file_path <- paste0(system.file('extdata',package = 'oliveR2'), '/', paste0(table_name, ".rds"))

  readr::write_rds(x = ppm_data, path = file_path)

  message("done")

}
