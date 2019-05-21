
#' A table of active foster homes
#'
#' @description Outputs a data frame where each record represents an active
#' foster home (relative or non-relative), including Resource ID,
#' Resource Name, Resource Service, Service Start, Service End, License Start,
#' License End, License Status, Home Status, responsible Region,
#' and responsible County. Active homes are homes that have a License Status
#' and Home Status of 'ACTIV'. Because this is not tracked historically in the
#' database, this is the status as of the data run time.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
active_foster_homes <- function(from = Sys.Date(), to = Sys.Date()) {
  resource_services(from, to) %>%
    dplyr::filter(
      `License Status` == 'ACTIV',
      `Home Status` == 'ACTIV',
      `Resource Service` %in% c(
        'Foster Home',
        'Relative Foster Home'
      ),
      `Licensing Type` != 'Umbrella'
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

}

#' A table of Custody Children with TPR Dates
#'
#' @description Outputs a data frame where each record represents a custody
#' child, including responsible Region, responsible County, Child ID,
#' Custody Start date, Custody End date, Custody Type,
#' Custody Sequence Number, and Legally Freed Date. For this table, the
#' Legally Freed date is derived from either the entered Legally Freed
#' date or the Termination Date, which is calculated from the TPR dates
#' of both a male and female parent.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
adoptable_children <- function(from = Sys.Date(), to = Sys.Date()) {
  dplyr::left_join(custody_children(from, to), tpr_info()) %>%
    dplyr::filter(
      (`Legally Freed Date` <= to | `Termination Date` <= to)
    ) %>%
    dplyr::mutate(
      `Legally Freed Date` = dplyr::if_else(
        is.na(`Legally Freed Date`),
        `Termination Date`,
        `Legally Freed Date`
      )
    ) %>%
    dplyr::select(-`Termination Date`) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

}

#' A table of child fatalities
#'
#' @description Outputs a data frame where each record represents an investigation
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
fatality_counts <- function(from = Sys.Date(), to = Sys.Date()) {
  allegations(from, to) %>%
    dplyr::filter(
      `Findings Approved Date` <= to,
      `Findings Approved Date` >= from
    ) %>%
    dplyr::group_by(Region, County, `Intake ID`, `Victim ID`) %>%
    dplyr::summarise(
      Fatality = any(`Victim Fatality`)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

}

#' A table of CFSR Round 3 Permanency Outcomes (Entry Cohort)
#'
#' @description  Outputs a data frame where each record represents a child who
#' entered custody in the 12 months prior to the first_day (Entry Cohort),
#' including responsible Region, responsible County, Child ID,
#' Custody Start Date, Custody End date, Custody Sequence Number,
#' Date of Birth, Service Outcome, Placement Start date (for final placement),
#' Final Placement Type, Adjusted Custody End date (for THV adjustment),
#' Days in Care (adjusted), Months in Care (adjusted),
#' Achieved Permanency indicator, Discharged in 12 Months indicator,
#' and Permanency in 12 Months indicator. For more information about the
#' THV adjustment, exclusions, and definition of Permanency in 12 Months,
#' please see the CFSR Round 3 documentation.
#'
#' A first_day of 01/01/2017, for example, will yield custody episodes starting
#' from 01/01/2016 through 12/31/2016.
#'
#' @param first_day  the first day of the year-long measurement period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom lubridate "%m+%"
#' @importFrom lubridate "%m-%"
cfsr3_perm1 <- function(first_day = Sys.Date() %m-% lubridate::years(1)) {
  cfsr3_permanency_data(
      first_day,
      first_day %m+% lubridate::years(1) %m-% lubridate::days(1)
    ) %>%
    dplyr::mutate(
      `Discharged in 12 Months` = (
        `Adjusted Custody End` <= `Custody Start` %m+% lubridate::years(1)
      ),
      `Permanency in 12 Months` = (
        `Discharged in 12 Months` & `Achieved Permanency`
      )
    ) %>%
    dplyr::filter(
      `Custody Start` < first_day,
      `Custody Start` >= (first_day %m-% lubridate::years(1))
    ) %>%
    dplyr::select(
      Region:`Placement Start`, `Final Placement Type`, `Adjusted Custody End`,
      `Days in Care`:`Permanency in 12 Months`
    ) %>%
    dplyr::ungroup()
}

#' A table of CFSR Round 3 Permanency Outcomes (12-23 Month Cohort)
#'
#' @description  Outputs a data frame where each record represents a child who
#' had been in custody 12-23 months prior on the first_day (12-23 Month Cohort),
#' including responsible Region, responsible County, Child ID,
#' Custody Start Date, Custody End date, Custody Sequence Number,
#' Date of Birth, Service Outcome, Placement Start date (for final placement),
#' Final Placement Type, Adjusted Custody End date (for THV adjustment),
#' Days in Care (adjusted), Months in Care (adjusted),
#' Achieved Permanency indicator, Discharged in 12 Months indicator,
#' and Permanency in 12 Months indicator. For more information about the
#' THV adjustment, exclusions, and definition of Permanency in 12 Months,
#' please see the CFSR Round 3 documentation.
#'
#' @param first_day  the first day of the year-long measurement period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom lubridate "%m+%"
#' @importFrom lubridate "%m-%"
cfsr3_perm2 <- function(first_day = Sys.Date() %m-% lubridate::years(1)) {
  cfsr3_permanency_data(
      first_day,
      first_day + lubridate::years(1) %m-% lubridate::days(1)
    ) %>%
    dplyr::mutate(
      `Discharged in 12 Months` = (
        `Adjusted Custody End` <= first_day %m+% lubridate::years(1)
      ),
      `Permanency in 12 Months` = (
        `Discharged in 12 Months` & `Achieved Permanency`
      )
    ) %>%
    dplyr::filter(
      `Custody Start` < (first_day %m-% lubridate::years(1)) ,
      `Custody Start` >= (first_day %m-% lubridate::years(2)),
      `Adjusted Custody End` >= first_day
    ) %>%
    dplyr::select(
      Region:`Placement Start`, `Final Placement Type`, `Adjusted Custody End`,
      `Days in Care`:`Permanency in 12 Months`
    ) %>%
    dplyr::ungroup()
}

#' A table of CFSR Round 3 Permanency Outcomes (24+ Month Cohort)
#'
#' @description  Outputs a data frame where each record represents a child who
#' had been in custody 24+ months prior on the first_day (24+ Month Cohort),
#' including responsible Region, responsible County, Child ID,
#' Custody Start Date, Custody End date, Custody Sequence Number,
#' Date of Birth, Service Outcome, Placement Start date (for final placement),
#' Final Placement Type, Adjusted Custody End date (for THV adjustment),
#' Days in Care (adjusted), Months in Care (adjusted),
#' Achieved Permanency indicator, Discharged in 12 Months indicator,
#' and Permanency in 12 Months indicator. For more information about the
#' THV adjustment, exclusions, and definition of Permanency in 12 Months,
#' please see the CFSR Round 3 documentation.
#'
#' @param first_day  the first day of the year-long measurement period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom lubridate "%m+%"
#' @importFrom lubridate "%m-%"
cfsr3_perm3 <- function(first_day = Sys.Date() %m-% lubridate::years(1)) {
  cfsr3_permanency_data(
      first_day,
      first_day + lubridate::years(1) %m-% lubridate::days(1)
    ) %>%
    dplyr::mutate(
      `Discharged in 12 Months` = (
        `Adjusted Custody End` <= first_day %m+% lubridate::years(1)
      ),
      `Permanency in 12 Months` = (
        `Discharged in 12 Months` & `Achieved Permanency`
      )
    ) %>%
    dplyr::filter(
      `Custody Start` < (first_day %m-% lubridate::years(2)) ,
      `Adjusted Custody End` >= first_day
    ) %>%
    dplyr::select(
      Region:`Placement Start`, `Final Placement Type`, `Adjusted Custody End`,
      `Days in Care`:`Permanency in 12 Months`
    ) %>%
    dplyr::ungroup()
}

#' A table of CFSR Round 3 Permanency Outcomes (Reentry)
#'
#' @description Outputs a data frame where each record represents a child who
#' entered custody in the 12 months prior to the first_day (Entry Cohort),
#' and who exited  from custody within 12 months to reunification,
#' guardianship, or living with another relative, then re-entered custody within
#' the following 12 months, including responsible Region,
#' responsible County, Child ID, Custody Start date, Custody End date,
#' Custody Sequence Number, Date of Birth, Service Outcome,
#' Placement Start date (final placement), Final Placement Type,
#' Adjusted Custody End date (for THV adjustment),
#' Subsequent Custody Start date, and Re-Entered in 12 Months indicator. This
#' data frame retains only the first custody episode for each child initated
#' within the 12 months prior to the first_day, in cases where the child enters
#' custody multiple times during the entry cohort period, only the child's
#' first custody episode will be included.
#'
#' A first_day of 01/01/2017, for example, will yield custody episodes starting
#' from 01/01/2016 through 12/31/2016.
#'
#' @param first_day the first day of the year-long measurement period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom lubridate "%m+%"
#' @importFrom lubridate "%m-%"
cfsr3_perm4 <- function(first_day = Sys.Date() %m-% lubridate::years(2)) {
  cfsr3_perm1(first_day) %>%
    dplyr::filter(
      `Service Outcome` %in% c(
        'Guardianship',
        'Reunify with parents/caretaker',
        'Living with other relatives'
      ),
      `Discharged in 12 Months`
    ) %>%
    dplyr::left_join(
      custody_children(first_day) %>%
        dplyr::select(
          `Child ID`,
          `Custody Sequence Number`,
          `Custody Start`
        ) %>%
        dplyr::mutate(
          `Re-Entered` = TRUE,
          `Custody Sequence Number` = `Custody Sequence Number` - 1
        ) %>%
        dplyr::rename(`Subsequent Custody Start` = `Custody Start`) %>%
        dplyr::filter(`Custody Sequence Number` > 0),
      by = c(
        'Child ID' = 'Child ID',
        'Custody Sequence Number' = 'Custody Sequence Number'
      )
    ) %>%
    dplyr::arrange(Region, County, `Child ID`, `Custody Sequence Number`) %>%
    dplyr::group_by(`Child ID`) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::mutate(
      `Re-Entered in 12 Months` = `Re-Entered` &
        `Subsequent Custody Start` <= `Adjusted Custody End` %m+% lubridate::years(1),
      `Re-Entered in 12 Months` = dplyr::if_else(
        is.na(`Re-Entered in 12 Months`),
        FALSE,
        `Re-Entered in 12 Months`
      )
    ) %>%
    dplyr::select(
      Region:`Adjusted Custody End`, `Subsequent Custody Start`,
      `Re-Entered in 12 Months`
    ) %>%
    dplyr::ungroup()
}

#' A table of congregate care placements and associated indicators
#'
#' @description Outputs a data frame where each record represents a placement
#' into a congregate care setting, including responsible Region, responsible
#' County, Child ID, Custody Start date, Custody End date, Custody Type,
#' Custody Sequence Number, Person Name, Date of Birth, Resource ID, Status,
#' Placement Start date, Placement End date, Placement Timestamp, Rate,
#' Placement Change Reason, Facility Type, Approval for Congregate Care,
#' Reason(s) Approved for Cong Care, Age (at placement start). Date filters
#' include placements that began during the reporting period.
#'
#' @param date_filter whether the result should be filtered
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
congregate_care_placements <- function(
  date_filter = F,
  from = Sys.Date(),
  to = SysDate()
) {
  custody_child_demographics(date_filter, from, to) %>%
    dplyr::left_join(
      custody_placements(date_filter, from, to) %>%
        dplyr::select(-county)
    ) %>%
    (function(data){
      if (date_filter) {
        data %>%
          dplyr::filter(
            `placement_start` <= to,
            `placement_start` >= from
          )
      } else {
        data
      }
    }) %>%
    dplyr::select(
      region:`date_of_birth`,
      `resource_id`:`facility_type`,
      `placement_id`,
      `approval_for_congregate_care`,
      `cong_care_reasons`,
      `rd_approval_date`
    ) %>%
    dplyr::filter(
      `facility_type` %in% c(
        # 'Residential Treatment',
        'Nursing Home',
        'Chemically Dependent Group',
        # 'Adoption Unit Foster Home',
        'Specialized Residential School',
        # 'Medical Treatment Group Home',
        'Contract Facility - Non MDHS',
        'Group Home',
        'Therapeutic Group Home',
        'Licensed Facility',
        'CO NonLicensed Det/Trng School',
        'Residential Child Caring Facil',
        'Institution',
        'Emergency Shelter'
      )
    ) %>%
    dplyr::mutate(
      Age = lubridate::as.period(
        lubridate::interval(
          `date_of_birth`,
          `placement_start`
        )
      )$year
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

}

#' A table of emergency placments with indicators for RD approval
#'
#' @description Outputs a data frame where each record represents a placement
#' into an emergency shelter for a custody child, including Placement ID,
#' Child ID, Custody Sequence Number, Placement Sequence per Episode,
#' Resource ID, County, Status, Placement Start date,
#' Placement End date, Placement Timestamp, Worker ID,
#' Approved for Emergency Placement indicator,
#' Reason(s) Approved for Emerg Placement, and Emergency Placement Sequence.
#'
#' @param date_filter whether the result should be filtered
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
emergency_placement_approvals <- function(
  date_filter = F,
  from = Sys.Date(),
  to = SysDate()
) {
  x <- custody_child_demographics(date_filter, from, to) %>%
    dplyr::left_join(
      custody_placements(date_filter, from, to) %>%
        dplyr::select(-County)
    ) %>%
    dplyr::filter(`Facility Type` == 'Emergency Shelter') %>%
    dplyr::arrange(`Placement Sequence Per Episode`) %>%
    dplyr::group_by(`Child ID`, `Custody Sequence Number`) %>%
    dplyr::mutate(`Emergency Placement Sequence` = row_number()) %>%
    dplyr::select(
      Region:`Custody End`,
      `Placement ID`:`Placement Timestamp`,
      `Worker ID`,
      `Approved for Emergency Placement`,
      `Reason(s) Approved for Emerg Placement`,
      `Emergency Placement Sequence`
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

}

#' A table of investigations with indicators for approval timeliness
#'
#' @description Outputs a data frame where each record represents an
#' investigation, including intake Region, intake County,
#' Investigation Worker ID, Intake ID, Report Level, SIU Worker,
#' Incident Date, Intake DateTime, Intake Status, Screening DateTime,
#' Findings Submitted Date, Findings Approved Date,
#' Approval Deadline, Approval Timely indicator
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
investigation_approval_timeliness <- function(
  from = Sys.Date(),
  to = Sys.Date()
) {
  investigations(from, to) %>%
    dplyr::mutate(
      `Approval Deadline` = lubridate::floor_date(
        `Intake DateTime` + lubridate::days(30),
        unit = 'days'
      ),
      `Approval Timely` = `Findings Approved Date` <= `Approval Deadline` &
        !is.na(`Findings Approved Date`)
    ) %>%
    dplyr::filter(
      `Approval Deadline` <= to,
      `Approval Deadline` >= from
    )
}

#' A table of investigations with indicators for initation timeliness
#'
#' @description Outputs a data frame where each record represents an
#' investigation, including intake Region, intake County,
#' Investigation Worker ID, Intake ID, Report Level, SIU Worker,
#' Incident Date, Intake DateTime, Intake Status, Screening DateTime,
#' Findings Submitted Date, Findings Approved Date, Initiation DateTime,
#' Initiation Deadline, Period End, Initiation Timely indicator
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
investigation_initiation_timeliness <- function(
  from = Sys.Date(),
  to = Sys.Date()
) {
  investigations(from, to) %>%
    dplyr::left_join(investigation_initiations()) %>%
    dplyr::mutate(
      `initiation_deadline` = dplyr::if_else(
        `report_level` == 3,
        `intake_datetime` + lubridate::hours(24),
        `intake_datetime` + lubridate::hours(72)
      ),
      `period_end` = lubridate::ymd_hms(
        to + lubridate::days(1) - lubridate::seconds(1)
      ),
      `initiation_timely` = `initiation_datetime` <= `initiation_deadline` &
        !is.na(`initiation_datetime`)
    ) %>%
    dplyr::filter(
      `initiation_deadline` <= `period_end`,
      `initiation_deadline` >= from,
      `intake_status`  %in% c('35INV', '40INV')
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

}

#' A table of contacts by months
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
monthly_contacts <- function(from = Sys.Date(), to = Sys.Date()) {
  case_narratives(from, to) %>%
    tidyr::crossing(month_frame(from, to)) %>%
    dplyr::filter(
      as.Date(`narrative_datetime`) <= `month_end`,
      `narrative_datetime` >= `month_start`
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

}

#' A table with all foster care reviews for chgildren in custody for a period
#'
#' @param from start date fo reporting period
#' @param to end date of reproting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
#'
all_fcr_children <- function(from = Sys.Date(), to = Sys.Date()) {
  result <- custody_children(from, to) %>%
    dplyr::left_join(
      foster_care_review(), by = c("child_id" = "person_id")
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

  result
}

#' A table of contacts with children in THV placements
#'
#' @description Outputs a data frame where each record represents a contact
#' with a child in a trial home visit, including responsible Region,
#' responsible County, Child ID, Custody Start date, Custody End date,
#' Custody Type, Custody Sequence Number, Placement ID,
#' Placement Sequence Per Episode, Resource ID, Status, Placement Start date,
#' Placement End date, Placement Timestamp, Rate, Placement Change Reason,
#' Facility Type, Contact Month, Narrative ID, Narrative Owner, Owner Job Code,
#' Owner Job Title, Narrative DateTime, Narrative Timestamp,
#' Narrative Location, Contact Method, and Contact Type. Contacts are recorded
#' for children during any month in which the child spent the entire month in
#' the same THV placement. Partial months are not considered. Only the first 90
#' days of the THV are considered.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"

thv_placement_contacts <- function(from = Sys.Date(), to = Sys.Date()) {
  x <- custody_children(from, to) %>%
    dplyr::mutate(custody_sequence_number = as.integer(custody_sequence_number)) %>%
   dplyr::inner_join(
      custody_placements(T, from, to) %>%
        dplyr::select(-county) %>%
        dplyr::mutate(
          placement_end = pmin(
            placement_end,
            placement_start + lubridate::days(90),
            na.rm = T
          ),
          custody_sequence_number = as.integer(custody_sequence_number)
        ) %>%
        dplyr::filter(
          placement_end >= from,
          placement_start <= to,
          stringr::str_detect(facility_type, 'Own Home')
        )
    ) %>%
    tidyr::crossing(month_frame(from, to)) %>%
    dplyr::filter(
      placement_start <= month_start,
      placement_end >= month_end
    ) %>%
    dplyr::left_join(
      custody_child_contacts(from, to) %>%
        dplyr::mutate(custody_sequence_number = as.integer(custody_sequence_number))
    ) %>%
    dplyr::select(
      region:facility_type,
      contact_month,
      narrative_id:contact_type
    )
}

#Age Function

#' Calculate age based on a DOB and another date
#' @param dob dAte of birth
#' @param age.day day to use to calulate dob; defaults to today()
#' @return age in years
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
