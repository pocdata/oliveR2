#' Title
#'
#' @param bld_sch_name
#' @param wrt_sch_name
#' @param establish_con
#'
#' @return
#' @export
#'
#' @examples
build_visitation_referral_fact <- function(bld_sch_name = NA
                                           ,wrt_sch_name = NA
                                           ,establish_con = FALSE) {

  message("* begin visitation_referral_fact build procedure *")

  if (establish_con) {
    message("setting connection to build schema... ", appendLF = FALSE)

    suppressMessages(establish_con_olvr_rplc(set_schema = bld_sch_name))

    message("done")
  }

  # Single query from ServiceReferrals with all the fields used for the tables built off it

  service_referrals <- DBI::dbGetQuery(con, "SELECT id
                                              , \"referralId\"
	                                            , \"caseNumber\"
	                                            , \"organizationId\"
                                            	, \"requestDate\"
                                              , \"createdAt\"
	                                            , \"updatedAt\"
	                                            , \"isCurrentVersion\"
                                            FROM staging.\"ServiceReferrals\"
                                            WHERE \"deletedAt\" IS NULL
                                              AND \"isCurrentVersion\" = TRUE") %>%
    dplyr::as_data_frame()

  message("building organization table... ", appendLF = FALSE)

  tbl_visit_referral_organizations <- dplyr::arrange(service_referrals
                                              , id
                                              , organizationId
                                              , desc(updatedAt)) %>%
    dplyr::distinct(organizationId, id) %>%
    dplyr::select(id_referral_visit = id
           , id_organization = organizationId)

  message("done")

  message("building case id table... ", appendLF = FALSE)

  tbl_visit_referral_case <- dplyr::arrange(service_referrals
                                     , id
                                     , caseNumber
                                     , desc(updatedAt)
                                     ) %>%
    dplyr::distinct(caseNumber, id) %>%
    dplyr::select(id_referral_visit = id
           , id_case = caseNumber)

  message("done")

  message("building referral creation table... ", appendLF = FALSE)

  tbl_visit_referral_created <- dplyr::select(service_referrals
                                       , requestDate
                                       , id
                                       , referralId
                                       , createdAt
                                       ) %>%
    dplyr::mutate(id_referral_visit = id
           , dt_referral_created = if_else(referralId == '(copy)'
                                           , createdAt
                                           , as.POSIXct(requestDate))
           , dt_referral_created = if_else(is.na(dt_referral_created)
                                           , createdAt
                                           , dt_referral_created)
           ) %>%
    dplyr::select(id_referral_visit
             , dt_referral_created)

  message("done")

  # this is essentially the old version of the received date
  # it does restrict on the basis of the psql function date_trunc
  # and does not require the existence of a referral id

  message("building receipt tables... ", appendLF = FALSE)

  tbl_visit_referrals_received_v1 <- DBI::dbGetQuery(con, "SELECT id AS id_referral_visit
	                                                           , min(\"updatedAt\") AS dt_referral_received_v1
                                                           FROM staging.\"ServiceReferrals\"
                                                           WHERE \"deletedAt\" IS NULL
                                                           GROUP BY id")


  tbl_visit_referrals_received_v2 <- DBI::dbGetQuery(con, "
          WITH tbl_visit_referrals_received_v2 AS

          (
          SELECT \"ServiceReferralId\"
	          , date
	          , rank() OVER (PARTITION BY \"ServiceReferralId\", date ORDER BY srtls.\"updatedAt\" DESC) AS rnk
          FROM staging.\"ServiceReferralTimelineStages\" AS srtls
          LEFT JOIN staging.\"StageTypes\" AS st ON srtls.\"StageTypeId\" = st.id
          WHERE name = 'Received'
          )

          SELECT \"ServiceReferralId\" AS id_referral_visit
	          , date AS dt_referral_received_v2
          FROM tbl_visit_referrals_received_v2
          WHERE rnk = 1")

  suppressMessages(
    tbl_visit_referral_received <-
      dplyr::full_join(
        tbl_visit_referrals_received_v1
        ,
        tbl_visit_referrals_received_v2
      )
  )
  message("done")

  message("building assignment tables... ", appendLF = FALSE)

  suppressWarnings(
    tbl_visit_referrals_assigned_v1 <- DBI::dbGetQuery(
      con
      ,
      "SELECT sr_1.id AS id_referral_visit,
      sr_1.\"updatedAt\"::date AS dt_referral_assigned_v1
      FROM staging.\"ServiceReferrals\" sr_1
      JOIN ( SELECT \"ServiceReferrals\".id AS id_referral_visit,
      min(\"ServiceReferrals\".\"versionId\") AS id_version_min
      FROM staging.\"ServiceReferrals\"
      LEFT JOIN ( SELECT \"ServiceReferrals_1\".id,
      max(\"ServiceReferrals_1\".\"versionId\") AS maxreqvid
      FROM staging.\"ServiceReferrals\" \"ServiceReferrals_1\"
      WHERE \"ServiceReferrals_1\".\"requestDate\" IS NOT NULL AND \"ServiceReferrals_1\".\"referralState\"::text = 'Requested'::text
      GROUP BY \"ServiceReferrals_1\".id) mreq ON mreq.id = \"ServiceReferrals\".id
      WHERE \"ServiceReferrals\".\"referralState\"::text = 'Accepted'::text AND \"ServiceReferrals\".\"requestDate\" IS NOT NULL AND \"ServiceReferrals\".\"versionId\" > COALESCE(mreq.maxreqvid, 1)
      GROUP BY \"ServiceReferrals\".id) armv ON sr_1.id = armv.id_referral_visit AND sr_1.\"versionId\" = armv.id_version_min"
      )
      )

  tbl_visit_referrals_assigned_v2 <- DBI::dbGetQuery(
    con
    ,"WITH tbl_visit_referrals_assigned_v2 AS

          (
          SELECT \"ServiceReferralId\"
	          , date
	          , rank() OVER (PARTITION BY \"ServiceReferralId\", date ORDER BY srtls.\"updatedAt\" DESC) AS rnk
          FROM staging.\"ServiceReferralTimelineStages\" AS srtls
          LEFT JOIN staging.\"StageTypes\" AS st ON srtls.\"StageTypeId\" = st.id
          WHERE name = 'Assigned'
          )

          SELECT DISTINCT \"ServiceReferralId\" AS id_referral_visit
	          , date AS dt_referral_assigned_v2
          FROM tbl_visit_referrals_assigned_v2
          WHERE rnk = 1")

  suppressMessages(
    tbl_visit_referral_assigned <-
      dplyr::full_join(
        tbl_visit_referrals_assigned_v1
        ,
        tbl_visit_referrals_assigned_v2
      )
  )

  message("done")

  message("building agreement tables... ", appendLF = FALSE)

  suppressWarnings(
    tbl_visit_referrals_agreed_v1 <- DBI::dbGetQuery(
      con
      ,
               "SELECT sr_1.id AS id_referral_visit,
            sr_1.\"requestDate\" AS dt_referral_agreed_v1
      FROM staging.\"ServiceReferrals\" sr_1
      JOIN ( SELECT \"ServiceReferrals\".id AS id_referral_visit,
      min(\"ServiceReferrals\".\"versionId\") AS id_version_min
      FROM staging.\"ServiceReferrals\"
      WHERE \"ServiceReferrals\".\"referralState\"::text = 'Scheduled'::text AND \"ServiceReferrals\".\"requestDate\" IS NOT NULL
      GROUP BY \"ServiceReferrals\".id) armv ON sr_1.id = armv.id_referral_visit AND sr_1.\"versionId\" = armv.id_version_min"
      )
      )
  tbl_visit_referrals_agreed_v2 <- DBI::dbGetQuery(
    con
    ,"WITH tbl_visit_referrals_agreed_v2 AS

        (
        SELECT \"ServiceReferralId\"
	        , date
	        , rank() OVER (PARTITION BY \"ServiceReferralId\", date ORDER BY srtls.\"updatedAt\" DESC) AS rnk
        FROM staging.\"ServiceReferralTimelineStages\" AS srtls
        LEFT JOIN staging.\"StageTypes\" AS st ON srtls.\"StageTypeId\" = st.id
        WHERE name = 'Agreed'
        )

        SELECT \"ServiceReferralId\" AS id_referral_visit
	        , date AS dt_referral_agreed_v2
        FROM tbl_visit_referrals_agreed_v2
        WHERE rnk = 1"
  )
  suppressMessages(
    tbl_visit_referral_agreed <-
      dplyr::full_join(
        tbl_visit_referrals_agreed_v1
        ,
        tbl_visit_referrals_agreed_v2
      )
  )

  message("done")

  message("building scheduled tables... ", appendLF = FALSE)

  suppressWarnings(
    tbl_visit_referrals_scheduled_v1 <- DBI::dbGetQuery(
      con
      ,
      "SELECT x.internalsrid id_referral_visit,
      min(x.visitdate) AS dt_referral_scheduled_v1
      FROM ( SELECT srf.id AS internalsrid,
      (json_array_elements(srf.\"visitSchedule\") ->> 'visitStartDate'::text)::date AS visitdate
      FROM staging.\"ServiceReferrals\" srf
      WHERE srf.\"isCurrentVersion\" = true AND srf.\"deletedAt\" IS NULL) x
      GROUP BY x.internalsrid"
      )
      )
    tbl_visit_referrals_scheduled_v2 <- DBI::dbGetQuery(
        con
        ,"WITH tbl_visit_referrals_scheduled_v2 AS

        (
        SELECT \"ServiceReferralId\"
	        , date
	        , rank() OVER (PARTITION BY \"ServiceReferralId\", date ORDER BY srtls.\"updatedAt\" DESC) AS rnk
        FROM staging.\"ServiceReferralTimelineStages\" AS srtls
        LEFT JOIN staging.\"StageTypes\" AS st ON srtls.\"StageTypeId\" = st.id
        WHERE name = 'Scheduled'
        )

        SELECT \"ServiceReferralId\" AS id_referral_visit
	        , date AS dt_referral_scheduled_v2
        FROM tbl_visit_referrals_scheduled_v2
        WHERE rnk = 1"
      )
  suppressMessages(
    tbl_visit_referral_scheduled <-
      dplyr::full_join(
        tbl_visit_referrals_scheduled_v1
        ,
        tbl_visit_referrals_scheduled_v2
      )
  )

  message("done")

  message("building opd table... ", appendLF = FALSE)

  tbl_visit_referral_opd <- DBI::dbGetQuery(
    con
    ,
    "SELECT
    \"ServiceReferrals\".id id_referral_visit
    ,json_array_elements(\"ServiceReferrals\".\"childDetails\") ->> 'childOpd'::text AS dt_opd
    FROM staging.\"ServiceReferrals\"
    WHERE \"ServiceReferrals\".\"isCurrentVersion\" = true AND \"ServiceReferrals\".\"deletedAt\" IS NULL"
    )

  message("done")

  message("building in-progress tables... ", appendLF = FALSE)

  tbl_visit_referral_inprogress <- DBI::dbGetQuery(con, "SELECT \"serviceReferralId\" AS id_referral_visit
                                                          ,date AS dt_referral_inprogress
                                                         FROM staging.\"VisitReports\"
                                                         WHERE \"isCurrentVersion\" = TRUE
	                                                       AND \"deletedAt\" IS NULL
	                                                       AND \"approvedAt\" IS NOT NULL")

  message("done")

  message("building child table for counting... ", appendLF = FALSE)

  suppressWarnings(
  tbl_referral_children <- DBI::dbGetQuery(con
                                           ,"SELECT
                                           \"ServiceReferrals\".id,
                                           json_array_elements(\"ServiceReferrals\".\"childDetails\") ->> 'childFamlinkPersonID'::text AS child_id
                                           FROM staging.\"ServiceReferrals\"
                                           WHERE \"ServiceReferrals\".\"isCurrentVersion\" = true AND \"ServiceReferrals\".\"deletedAt\" IS NULL") %>%
    dplyr::mutate(id_referral_visit = id
           ,id_prsn_child = ifelse(is.na(child_id), 0
                                   ,as.integer(child_id))
    ) %>%
    dplyr::select(id_referral_visit, id_prsn_child) %>%
    dplyr::as_data_frame()
  )

  tbl_referral_children <- group_by(tbl_referral_children, id_referral_visit) %>%
    dplyr::summarise(qt_child_count = n())

  message("done")

  message("combining all tables... ", appendLF = FALSE)

  suppressMessages(
    visitation_referral_fact <- list(
      tbl_visit_referral_received,
      tbl_visit_referral_assigned,
      tbl_visit_referral_agreed,
      tbl_visit_referral_scheduled,
      tbl_visit_referral_inprogress,
      tbl_visit_referral_created,
      tbl_visit_referral_opd,
      tbl_visit_referral_organizations,
      tbl_visit_referral_case,
      tbl_referral_children
    ) %>%
      Reduce(function(dtf1, dtf2)
        dplyr::left_join(dtf1, dtf2, by = "id_referral_visit"), .) %>%
      dplyr::mutate(
        id_visitation_referral_fact = id_referral_visit,
        id_calendar_dim_opd = as.integer(format(mdy(dt_opd), "%Y%m%d")),
        id_provider_dim_pcv = id_organization,
        id_case = id_case,
        id_calendar_dim_created = as.integer(format(dt_referral_created, "%Y%m%d")),
        id_calendar_dim_received_v1 = as.integer(format(dt_referral_received_v1, "%Y%m%d")),
        id_calendar_dim_received_v2 = as.integer(format(dt_referral_received_v2, "%Y%m%d")),
        id_calendar_dim_assigned_v1 = as.integer(format(dt_referral_assigned_v1, "%Y%m%d")),
        id_calendar_dim_assigned_v2 = as.integer(format(dt_referral_assigned_v2, "%Y%m%d")),
        id_calendar_dim_agreed_v1 = as.integer(format(dt_referral_agreed_v1, "%Y%m%d")),
        id_calendar_dim_agreed_v2 = as.integer(format(dt_referral_agreed_v2, "%Y%m%d")),
        id_calendar_dim_scheduled_v1 = as.integer(format(dt_referral_scheduled_v1, "%Y%m%d")),
        id_calendar_dim_scheduled_v2 = as.integer(format(dt_referral_scheduled_v2, "%Y%m%d")),
        id_calendar_dim_inprogress = as.integer(format(dt_referral_inprogress, "%Y%m%d"))
      ) %>%
      dplyr::group_by(id_visitation_referral_fact, id_provider_dim_pcv) %>%
      dplyr::summarise(
        id_calendar_dim_opd = ifelse(is.infinite(max(
          id_calendar_dim_opd, na.rm = TRUE))
          ,NA
          ,max(id_calendar_dim_opd, na.rm = TRUE))
        ,id_calendar_dim_created = ifelse(
          is.infinite(max(id_calendar_dim_created, na.rm = TRUE))
          ,NA
          ,max(id_calendar_dim_created, na.rm = TRUE)
        )
        ,id_calendar_dim_received_v1 = ifelse(
          is.infinite(max(id_calendar_dim_received_v1
                          , na.rm = TRUE))
          ,NA
          ,max(id_calendar_dim_received_v1, na.rm = TRUE)
        )
        ,id_calendar_dim_received_v2 = ifelse(
          is.infinite(max(id_calendar_dim_received_v2
                          , na.rm = TRUE))
          ,NA
          ,max(id_calendar_dim_received_v2, na.rm = TRUE)
        )
        ,id_calendar_dim_assigned_v1 = ifelse(
          is.infinite(max(id_calendar_dim_assigned_v1
                          , na.rm = TRUE))
          ,NA
          ,max(id_calendar_dim_assigned_v1, na.rm = TRUE)
        )
        ,id_calendar_dim_assigned_v2 = ifelse(
          is.infinite(max(id_calendar_dim_assigned_v2
                          , na.rm = TRUE))
          ,NA
          ,max(id_calendar_dim_assigned_v2, na.rm = TRUE)
        )
        ,id_calendar_dim_agreed_v1 = ifelse(
          is.infinite(max(id_calendar_dim_agreed_v1
                          , na.rm = TRUE))
          ,NA
          ,max(id_calendar_dim_agreed_v1, na.rm = TRUE)
        )
        ,id_calendar_dim_agreed_v2 = ifelse(
          is.infinite(max(id_calendar_dim_agreed_v2
                          , na.rm = TRUE))
          ,NA
          ,max(id_calendar_dim_agreed_v2, na.rm = TRUE)
        )
        ,id_calendar_dim_scheduled_v1 = ifelse(
          is.infinite(max(id_calendar_dim_scheduled_v1
                          , na.rm = TRUE))
          ,NA
          ,max(id_calendar_dim_scheduled_v1, na.rm = TRUE)
        )
        ,id_calendar_dim_scheduled_v2 = ifelse(
          is.infinite(max(id_calendar_dim_scheduled_v2
                          , na.rm = TRUE))
          ,NA
          ,max(id_calendar_dim_scheduled_v2, na.rm = TRUE)
        )
        ,id_calendar_dim_inprogress = ifelse(
          is.infinite(max(id_calendar_dim_inprogress, na.rm = TRUE))
          ,NA
          ,max(id_calendar_dim_inprogress, na.rm = TRUE)
        )
        # this may cause us problems at some point. should eventually look for
        # "most recent" case & child count
        ,id_case = max(id_case)
        ,qt_child_count = max(qt_child_count)
      ) %>%
      dplyr::select(
        id_visitation_referral_fact,
        id_case,
        id_provider_dim_pcv,
        id_calendar_dim_opd,
        id_calendar_dim_created,
        id_calendar_dim_received_v1,
        id_calendar_dim_received_v2,
        id_calendar_dim_assigned_v1,
        id_calendar_dim_assigned_v2,
        id_calendar_dim_agreed_v1,
        id_calendar_dim_agreed_v2,
        id_calendar_dim_scheduled_v1,
        id_calendar_dim_scheduled_v2,
        id_calendar_dim_inprogress,
        qt_child_count
      ) %>%
      dplyr::inner_join(visitation_referral_attribute_fact_and_dim
                 , by = "id_visitation_referral_fact") %>%
      dplyr::distinct()
  )
  message("done")

  if(establish_con){
    message("switching to write schema... ", appendLF = FALSE)

    suppressMessages(DBI::dbSendQuery(con, dbplyr::build_sql("SET search_path TO ", wrt_sch_name)))

    message("done")
  }

  message("send visitation_referral_fact to db... ", appendLF = FALSE)

  visitation_referral_fact <- visitation_referral_fact %>%
    dplyr::mutate(id_calendar_dim_table_update = as.integer(format(now(), "%Y%m%d")))

  job_status <- DBI::dbWriteTable(
    conn = con
    ,
    name = "visitation_referral_fact"
    ,
    value = visitation_referral_fact
    ,
    overwrite = TRUE
    ,
    row.names = FALSE
  )

  message("done")

  message("altering table ownership to report_developer... ", appendLF = FALSE)

  DBI::dbGetQuery(con, "ALTER TABLE independent.visitation_referral_fact
  OWNER TO report_developer;")

  message("done")

  # TODO: document the table as was done for calendar_dim

  # col_desc <- c(
  #   id_visitation_referral_fact = "PK"
  #   ,id_provider_dim_pcv = "test"
  #   ,id_calendar_dim_opd = "test"
  #   ,id_calendar_dim_created = "test"
  #   ,id_calendar_dim_received = "test"
  #   ,id_calendar_dim_assigned = "test"
  #   ,id_calendar_dim_scheduled = "test"
  #   ,id_calendar_dim_inprogress = "test"
  #   ,id_visitation_referral_attribute_dim = "test"
  # )
  #
  # for (i in colnames(visitation_referral_fact)){
  #   col_comment_olvr_rplc(sch_name = "independent"
  #                         ,tbl_name = "visitation_referral_fact"
  #                         ,col_name = i
  #                         ,col_comment = as.character(col_desc[i]))
  # }
  #
  # meta_visitation_referral_fact <- dplyr::as_data_frame(dplyr::data_frame(col_desc, col = names(col_desc)))

  # save(meta_visitation_referral_fact
  #      ,file = "data/meta_visitation_referral_fact.rda")

  if (job_status) {
    message("done")
  } else {
    message("unable to build and write table")
  }
}

