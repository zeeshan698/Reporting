# Standard vectors ------------------------------------------------------------

# This file contains base functions for
# pulling data files from the database

#' Standard vector for valid contact job codes
#'
#' @return a vector of valid contact job codes
#' @export
valid_contact_job_codes <- function() {
  c('3745', '4554', '4555', '4556', '4557', '4558', '5574','5574','5575', '5576', '5577', '5567', '5567', '5568',
    '5569', '5570', '5571','5572','5573','5579','5579','5580','5581','5582')
}

#' Standard vector for regions
#'
#' @return a vector of Region names in order
#' @export
region_vector <- function() {
  c(
    'I-N',   'I-S',   'II-E', 'II-W',
    'III-N', 'III-S', 'IV-N', 'IV-S',
    'V-E',   'V-W',   'VI',   'VII-C',
    'VII-E', 'VII-W'
  )
}

eastwestsouth <-data.frame(
  "EastWestSouth" = c('East','East','East','East','East','East','East','East','East','East','East','East','East',
                      'East','East','East','East','East','East','East','East','East','East','East','East','East',
                      'East','East','East','East','East','East','East','East','East','East','East','East','East',
                      'East','East',
                      'South','South','South','South','South','South','South','South','South','South',
                      'West','West','West','West','West','West','West','West','West','West','West','West','West',
                      'West','West','West','West','West','West','West','West','West','West','West','West','West',
                      'West','West','West','West','West','West','West','SIU'),
"region"= c('I-N','I-N','I-N','I-N','I-N','I-N','I-S',
'I-S','I-S','I-S','I-S','I-S','II-E','II-E','II-E','II-E','II-E','II-E','II-E','II-E','IV-N','IV-N','IV-N','IV-N',
'IV-N','IV-N','IV-N','IV-N','IV-N','IV-N','IV-N','IV-S','IV-S','IV-S','IV-S','IV-S','IV-S','IV-S','IV-S',
'IV-S','IV-S','VI','VI','VI','VI','VII-C','VII-E','VII-E','VII-E','VII-W','VII-W','II-W','II-W','II-W','II-W',
'II-W','II-W','II-W','II-W','II-W','II-W','II-W','II-W','III-N','III-N','III-N','III-S','V-E','V-E','V-E',
'V-E','V-E','V-E','V-E','V-E','V-W','V-W','V-W','V-W','V-W','V-W','V-W','V-W','V-W','SIU'),
"county"= c('Alcorn','Benton','Marshall','Prentiss','Tippah','Tishomingo','Itawamba','Lafayette','Lee',
'Monroe','Pontotoc','Union','DeSoto','Grenada','Panola','Quitman','Tallahatchie','Tate','Tunica','Yalobusha','Attala',
'Calhoun','Choctaw','Clay','EastChickasaw','Lowndes','Noxubee','Oktibbeha','Webster','WestChickasaw','Winston',
'Clarke','Jasper','Jones','Kemper','Lauderdale','Leake','Neshoba','Newton','Scott','Wayne','Forrest','Lamar',
'Perry','Stone','Harrison','George','Greene','Jackson','Hancock','PearlRiver',
'Carroll','Coahoma','EastBolivar','Holmes','Humphreys','Issaquena','Leflore','Montgomery','Sharkey','Sunflower','Washington',
'WestBolivar','Madison','Rankin','Yazoo','Hinds','Copiah','Covington','JeffersonDavis','Lawrence','Lincoln','Marion',
'Simpson','Smith','Adams','Amite','Claiborne','Franklin','Jefferson','Pike','Walthall','Warren','Wilkinson',''
)
)
#' Standard vector for allegations
#'
#' @return a vector of allegation names
#' @export
#' This function is creating unique list of potential allegations names reported
potential_allegations <- function() {
  c(
    'Sexual Abuse', 'Physical Abuse', 'Physical Neglect',
    'Medical Neglect', 'Emotional Abuse/Neglect', 'Exploitation',
    'Abandonment'
  )
}

#' Standard vector for custody outcomes
#'
#' @return a vector of custody outcome names
#' @export
#' This function is creating unique list of custody outcomereported
potential_custody_outcomes <- function() {
  c(
    'Adoption', 'Death of a child', 'Emancipation', 'Guardianship',
    'Living with other relatives',  'Reunify with parents/caretaker',
    'Runaway', 'Transfer to another agency'
  )
}

#' Provides a template for a data frame for monthly periods
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#'
month_frame <- function(from = Sys.Date(), to = Sys.Date()) {
  data.frame(
    'contact_month' = format.Date(seq.Date(from, to, by='months'), '%B %Y'),
    'month_start' = seq.Date(from, to, by='months'),
    'month_end' = lubridate::ceiling_date(
      seq.Date(from, to, by='months')
      , unit = 'months'
    ) - lubridate::days(1),
    check.names = F
  ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

}

# Tools -----------------------------------------------------------------------



#' Update package from GitLab
#'
#' @description Updates the installed package from GitLab
#'
#' @param gitlab_url url to git repo
#' @param username GitLab username
#'
#' @export
update_from_gitlab <- function(
  gitlab_url = "https://gitlab.com/ericwburden/mdcpsde2.git",
  username = "ericwburden"
) {
  devtools::install_git(
    gitlab_url,
    credentials = git2r::cred_user_pass(
      username,
      getPass::getPass()
    )
  )
}

# Base Tables -----------------------------------------------------------------

#' A table of adoption worker assignments
#'
#' @description Outputs a data frame where each record represents the most
#' recent adoption worker assignment per Adoption Permanency Plan that was
#' active during the period, including including responsible Region,
#' responsible County, responsible Supervisor, responsible Worker, Case ID,
#' Child ID, Child Name, Child DOB, Custody Start, Custody End,
#' Custody Sequence Number, Plan ID, Plan Start date, Plan End date,
#' Adoption Supervisor ID, Adoption Worker ID, Adoption Worker Start datetime,
#' and Adoption Worker End datetime.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
adoption_assignments <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  paste(
    'exec mdcpsde_gen_adoption_assignments',
    paste0('\'', gsub('-', '', from), '\','),
    paste0('\'', gsub('-', '', to), '\'')
  ) %>%
    DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_adoption_assignments') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Period Start` = as.Date(`Period Start`),
      `Period End` = as.Date(`Period End`),
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Child DOB` = as.Date(`Child DOB`),
      `Custody Start` = as.Date(`Custody Start`),
      `Custody End` = as.Date(`Custody End`),
      `Plan Start` = as.Date(`Plan Start`),
      `Plan End` = as.Date(`Plan End`)
    )

  result
}

#' A table of ANE allegations
#'
#' @description Outputs a data frame where each record represents an allegation
#' of abuse or neglect, including Intake ID, Perpetrator ID, Victim ID,
#' Relationship, Allegation, Finding, Serious Injury indicator,
#' Victim Fatality indicator.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
allegations <- function(from = Sys.Date(), to = Sys.Date())
  {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_inv_allegations') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      Relationship = factor(Relationship),
      Allegation = factor(Allegation),
      Finding = factor(Finding),
      `Serious Injury` = as.logical(as.integer(`Serious Injury`)),
      `Victim Fatality` = as.logical(as.integer(`Victim Fatality`))
    )  %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower) %>%
    dplyr::right_join(investigations(from, to)) %>%
    dplyr::ungroup()

  result
}

#' A base table for case narratives
#'
#' @description Outputs a data frame where each recrod represents a contact
#' between a case worker and a case participant, including Case ID,
#' Narrative ID, Participant ID, Narrative Owner (case worker id),
#' Owner Job Code, Narrative DateTime, Narrative Timestamp, Narrative Location,
#' Contact Method, and Contact Type. The most recent narrative will be dated
#' 01/01/2017, the beginning of the STRO period.
#'
#' @param from the start date of the measurement period
#' @param to the end date of the measurement period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
case_narratives <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_case_narratives_subset') %>%
    dplyr::filter(
      `Narrative DateTime` >= from,
      `Narrative DateTime` <= to
    ) %>%
    dplyr::collect() %>%
    dplyr::select(-`Run Date`) %>%
    dplyr::mutate(
      `Narrative DateTime` = lubridate::ymd_hms(`Narrative DateTime`),
      `Narrative Timestamp` = lubridate::ymd_hms(`Narrative Timestamp`),
      `Narrative Location` = factor(`Narrative Location`),
      `Contact Method` = factor(`Contact Method`),
      `Contact Type` = factor(`Contact Type`)
    ) %>%
    unique() %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

  result
}

#' A base table for CFSR Round 3 Permanency measures
#'
#' @description Outputs a data frame where each record represents a child
#' in custody from one year prior to the measurement period start date through
#' the measurement period end date, including responsible Region, responsible
#' County, Child ID, Custody Start date, Custody End date,
#' Custody Sequence Number, Date of Birth, Service Outcome, Placement Start,
#' Placement End, Final Placement Type, Trial Home Visit indicator,
#' Adjusted Custody End date (see below), Seventeenth Birthday,
#' Eighteenth Birthday, Entered at Seventeen indicator,
#' Turned Eighteen in Care indicator, Days in Care, Months in Care,
#' and Achieved Permanency indicator.
#'
#' Adjusted Custody End Date applies the Trial Home Visit adjustment. For
#' description of this adjustment and exclusion criteria, see CFSR Round 3
#' Procedures Manual.
#'
#' @param from the start date of the measurement period
#' @param to the end date of the measurement period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
cfsr3_permanency_data <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_cfsr3_permanency_data')%>%
    dplyr::collect() %>%
    dplyr::mutate(
      Region = factor(Region),
      County = factor(County),
      `Custody Start` = lubridate::ymd(`Custody Start`),
      `Custody End` = lubridate::ymd(`Custody End`),
      `Date of Birth` = lubridate::ymd(`Date of Birth`),
      `Service Outcome` = factor(`Service Outcome`),
      `Placement Start` = lubridate::ymd(`Placement Start`),
      `Placement End` = lubridate::ymd(`Placement End`),
      `Final Placement Type` = factor(`Final Placement Type`),
      `Trial Home Visit` = as.logical(`Trial Home Visit`),
      `Adjusted Custody End` = dplyr::if_else(
        `Trial Home Visit`,
        `Placement Start` + lubridate::days(30),
        `Custody End`
      ), # Adjusts for THV
      `Adjusted Custody End` = dplyr::if_else(
        `Adjusted Custody End` > `Custody End`,
        `Custody End`,
        `Adjusted Custody End`
      ), # Corrects for THV less than 30 days
      `Seventeenth Birthday` = `Date of Birth` + lubridate::years(17),
      `Eighteenth Birthday` = `Date of Birth` + lubridate::years(18),
      `Entered at Seventeen` = `Custody Start` >= `Seventeenth Birthday` &
        `Custody Start` <= `Eighteenth Birthday`,
      `Turned Eighteen in Care` = `Eighteenth Birthday` >= `Custody Start` &
        `Eighteenth Birthday` <= `Custody End`,
      `Days in Care` = as.integer(difftime(
        `Adjusted Custody End`,
        `Custody Start`,
        units = 'days'
      )),
      `Months in Care` = lubridate::as.period(lubridate::interval(
        `Custody Start`,
        `Adjusted Custody End`
      )) %/% months(1),
      `Achieved Permanency` = `Service Outcome` %in% c(
        'Adoption',
        'Guardianship',
        'Reunify with parents/caretaker'
      ),
      Include = `Days in Care` >= 8 &
        `Eighteenth Birthday` >= `Custody Start` &
        !(`Entered at Seventeen` & `Turned Eighteen in Care`)
    ) %>%
    dplyr::filter(
      Include,
      `Custody Start` <= to,
      `Custody End` >= (from - lubridate::years(1))
    ) %>%
    dplyr::select(-Include) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

  result
}

#' A table of custody child case/custody episodes
#'
#' @description Outputs a data frame where each record represents a unique
#' case/custody episode for a foster child, including responsible Region,
#' responsible County, Supervisor, Worker, Case ID, Child ID, Child Name,
#' Child DOB, Custody Start date, Custody End date, Custody Sequence Number
#'
#' @param from the start date of the measurement period
#' @param to the end date of the measurement period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
child_case_crosswalk <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  paste(
      'exec mdcpsde_gen_child_case_crosswalk',
      paste0('\'', gsub('-', '', from), '\','),
      paste0('\'', gsub('-', '', to), '\'')
    ) %>%
      DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_child_case_crosswalk') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Period Start` = as.Date(`Period Start`),
      `Period End` = as.Date(`Period End`),
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Case ID` = as.integer(`Case ID`),
      `Case Start` = as.Date(`Case Start`),
      `Case End` = as.Date(`Case End`),
      `Child DOB` = as.Date(`Child DOB`),
      `Custody Start` = as.Date(`Custody Start`),
      `Custody End` = as.Date(`Custody End`)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

  result
}

#' A table of court events
#'
#' @description Outputs a data frame where each record represents a court
#' action, including Court Event ID, Child ID, Court Tracking Number,
#' Event Type, Hearing Start date, Hearing End date, Court Order Requested date,
#' Court Order Received date, Order Effective date, Court Recommendation,
#' Judge, THV Approval indicator
#'
#' @param from the start date of the measurement period
#' @param to the end date of the measurement period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
court_events <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  paste(
    'exec mdcpsde_gen_court_events',
    paste0('\'', gsub('-', '', from), '\','),
    paste0('\'', gsub('-', '', to), '\'')
  ) %>%
    DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_court_events') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Period Start` = as.Date(`Period Start`),
      `Period End` = as.Date(`Period End`),
      `Event Type` = factor(`Event Type`),
      `Hearing Start` = as.Date(`Hearing Start`),
      `Hearing End` = as.Date(`Hearing End`),
      `Court Order Requested` = as.Date(`Court Order Requested`),
      `Court Order Received` = as.Date(`Court Order Received`),
      `Order Effective` = as.Date(`Order Effective`),
      `Court Recommendation` = factor(`Court Recommendation`),
      `THV Approval` = factor(`THV Approval`)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of custody case members
#'
#' @description Outputs a data frame where each record represents a member of
#' a custody case, including responsible Region, responsible County,
#' Case ID, Open Period Sequence, Case Start date, Case Closed date, Person ID,
#' Person Status, Person Type, Case Member Start date, Case Member End date,
#' and Case Change Reason.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
custody_case_members <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcspde_custody_case_members') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Case Start` = lubridate::ymd(`Case Start`),
      `Case Closed` = lubridate::ymd(`Case Closed`),
      `Person Status` = factor(`Person Status`),
      `Person Type` = factor(`Person Type`),
      `Case Member Start` = lubridate::ymd(`Case Member Start`),
      `Case Member End` = lubridate::ymd(`Case Member End`),
      `Case Change Reason` = factor(`Case Change Reason`)
    ) %>%
    dplyr::filter(
      `Case Start` <= to,
      (is.na(`Case Closed`) | `Case Closed` >= from)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of custody case member contacts
#'
#' @description Outputs a data frame where each record represents a contact
#' with a child in custody, including responsible Region, responsible
#' County, Case ID, Child ID, Custody Start date, Custody End date,
#' Custody Sequence Number, Custody Type, Case Member Start date,
#' Case Member End date, Case Member Type, Narrative ID, Narrative Owner,
#' Owner Job Code, Owner Job Title, Narrative DateTime, Narrative Timestamp,
#' Narrative Location, Contact Method, Contact Type, and Contact Month. The
#' most recent narrative will be dated 01/01/2017, the beginning of the STRO
#' period.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
custody_child_contacts <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  DBI::dbExecute(db_con, 'exec mdcpsde_gen_custody_child_contacts')
  result <- dplyr::tbl(db_con, 'mdcpsde_custody_child_contacts') %>%
    dplyr::filter(
      `Narrative DateTime` >= from,
      `Narrative DateTime` <= to
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Custody Start` = lubridate::ymd(`Custody Start`),
      `Custody End` = lubridate::ymd(`Custody End`),
      `Custody Type` = factor(`Custody Type`),
      `Case Member Start` = lubridate::ymd(`Case Member Start`),
      `Case Member End` = lubridate::ymd(`Case Member End`),
      `Case Member Type` = factor(`Case Member Type`),
      `Owner Job Code` = factor(`Owner Job Code`),
      `Owner Job Title` = factor(`Owner Job Title`),
      `Narrative DateTime` = lubridate::ymd_hms(`Narrative DateTime`),
      `Narrative Date` = lubridate::ymd(`Narrative DateTime`),
      `Narrative Timestamp` = lubridate::ymd_hms(`Narrative Timestamp`),
      `Narrative Location` = factor(`Narrative Location`),
      `Contact Method` = factor(`Contact Method`),
      `Contact Type` = factor(`Contact Type`),
      `Contact Month` = factor(`Contact Month`)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of custody children
#'
#' @description Outputs a data frame where each record represents a custody
#' episode, including responsible Region, responsible County, custody start
#' and end dates, custody type, child id, and custody sequence number. The
#' source View combines custody change events into continuous custody
#' episodes.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
custody_children <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_custody_episodes') %>%
    dplyr::filter(
      `Custody Start` <= to,
      (is.na(`Custody End`) | `Custody End` >= from)
    ) %>%
    dplyr::collect()  %>%
    dplyr::mutate(
      `Custody Sequence Number` = as.integer(`Custody Sequence Number`),
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Custody Start` = lubridate::ymd(`Custody Start`),
      `Custody End` = lubridate::ymd(`Custody End`),
      `Custody Type` = factor(`Custody Type`)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

    result
}

#' A table of all foster care reviews
#'
#' @return all foster care reviews completed
#' @export
#' @importFrom magrittr "%>%"
foster_care_review <- function() {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_foster_care_review') %>%
    dplyr::collect() %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

    result
}


#' A table of custody child demographics
#'
#' @description Outputs a data frame where each record represents a custody
#' episode, including responsible Region, responsible County, custody start
#' and end dates, custody type, child id, custody sequence number, person name,
#' Date of Birth, Gender, Ethnicity, Medicaid Number and all applicable races.
#'
#' @param date_filter whether the result should be filtered
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
custody_child_demographics <- function(
  date_filter = F,
  from = Sys.Date(),
  to = Sys.Date()
) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_custody_episodes') %>%
    (function(data){
      if (date_filter) {
        data %>%
          dplyr::filter(
            `Custody Start` <= to,
            (is.na(`Custody End`) | `Custody End` >= from)
          )
      } else {
        data
      }
    }) %>%
    dplyr::left_join(
      dplyr::tbl(db_con, 'mdcpsde_demographics'),
      by = c('Child ID' = 'Person ID')
    ) %>%
    dplyr::collect()  %>%
    dplyr::mutate(
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Custody Start` = lubridate::ymd(`Custody Start`),
      `Custody End` = lubridate::ymd(`Custody End`),
      `Custody Type` = factor(`Custody Type`),
      `Date of Birth` = lubridate::ymd(`Date of Birth`),
      Gender = factor(Gender),
      Ethnicity = factor(Ethnicity)
    ) %>%
    dplyr::mutate(`Custody Sequence Number` = as.integer(`Custody Sequence Number`)) %>%
    dplyr::group_by(
      ISN, Region, County, `Child ID`, `Custody Start`, `Custody End`,
      `Custody Type`, `Custody Sequence Number`, `Person Name`,
      `Date of Birth`, Gender, Ethnicity, `Medicaid Number`
    ) %>%
    # dplyr::summarise(
    #   Race = list(unique(Race))
    # ) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

  result
}

#' A table of custody child permanency outcomes and AFCARS removal reasons
#'
#' @description Ported over from mdcpsde, used in original STRO reporting.
#' Outputs a data frame where each record represents a custody episode,
#' including responsible Region, responsible County, responsible Supervisor,
#' responsible Worker, Child ID, Child Name, Child DOB, Custody Type,
#' Custody Start date, Custody End date, Subsequent Custody Start date,
#' Custody Sequence Number, Permanency Plan, Permanency Plan Start date,
#' Concurrent Plan, Concurrent Plan Start date, Service Type,
#' Service Outcome, Most Recent Placement Type, Placement Start date,
#' Adoption Finalization Date, Legally Free Date, Physical Abuse indicator,
#' Sexual Abuse indicator, Neglect indicator, Alcohol Abuse - Parent indicator,
#' Alcohol Abuse - Child indicator, Drug Abuse - Parent indicator,
#' Drug Abuse - Child indicator, Child Disability indicator,
#' Child Behavior Problems indicator, Death of Parent(s) indicator,
#' Incarceration of Parent indicator, Caretaker Inability to Cope indicator,
#' Abandonment indicator, Relinquishment indicator,
#' Inadequate Housing indicator
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
custody_child_rr_outcomes <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  paste(
      'exec mdcpsde_gen_custody_child_rr_outcomes_legacy',
      paste0('\'', gsub('-', '', from), '\','),
      paste0('\'', gsub('-', '', to), '\'')
    ) %>%
    DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_custody_child_rr_outcomes_legacy') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Period Start` = as.Date(`Period Start`),
      `Period End` = as.Date(`Period End`),
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Child DOB` = as.Date(`Child DOB`),
      `Custody Type` = factor(`Custody Type`),
      `Custody Start` = as.Date(`Custody Start`),
      `Custody End` = as.Date(`Custody End`),
      `Subsequent Custody Start` = as.Date(`Subsequent Custody Start`),
      `Permanency Plan Start Date` = as.Date(`Permanency Plan Start Date`),
      `Concurrent Plan Start Date` = as.Date(`Concurrent Plan Start Date`),
      `Service Type` = factor(`Service Type`),
      `Service Outcome` = dplyr::if_else(
        is.na(`Service Outcome`),
        dplyr::if_else(
          is.na(`Custody End`),
          'Remaining in Care',
          'Unknown'
        ),
        `Service Outcome`
      ) %>% factor(),
      `Most Recent Placement Type` = factor(`Most Recent Placement Type`),
      `Placement Start` = as.Date(`Placement Start`),
      `Adoption Finalization Date` = as.Date(`Adoption Finalization Date`),
      `Legally Free Date` = as.Date(`Legally Free Date`),
      `Physical Abuse` = as.logical(as.integer(`Physical Abuse`)),
      `Sexual Abuse` = as.logical(as.integer(`Sexual Abuse`)),
      `Neglect` = as.logical(as.integer(`Neglect`)),
      `Alcohol Abuse - Parent` = as.logical(as.integer(
        `Alcohol Abuse - Parent`
      )),
      `Alcohol Abuse - Child` = as.logical(as.integer(
        `Alcohol Abuse - Child`
      )),
      `Drug Abuse - Parent` = as.logical(as.integer(`Drug Abuse - Parent`)),
      `Drug Abuse - Child` = as.logical(as.integer(`Drug Abuse - Child`)),
      `Child Disability` = as.logical(as.integer(`Child Disability`)),
      `Child Behavior Problem` = as.logical(as.integer(
        `Child Behavior Problem`
      )),
      `Death of Parent(s)` = as.logical(as.integer(`Death of Parent(s)`)),
      `Incarceration of Parent` = as.logical(as.integer(
        `Incarceration of Parent`
      )),
      `Caretaker Inability to Cope` = as.logical(as.integer(
        `Caretaker Inability to Cope`
      )),
      `Abandonment` = as.logical(as.integer(`Abandonment`)),
      `Relinquishment` = as.logical(as.integer(`Relinquishment`)),
      `Inadequate Housing` = as.logical(as.integer(`Inadequate Housing`))
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of Custody Outcomes
#'
#' @description Outputs a data frame where each record represents an outcome
#' of a custody episode, including Child ID, Custody Sequence Number,
#' Custody Start date, Custody End date, Service Type, and
#' Service Outcome.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
custody_outcomes <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_custody_outcomes') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Custody Sequence Number` = as.integer(`Custody Sequence Number`),
      `Custody Start` = lubridate::ymd(`Custody Start`),
      `Custody End` = lubridate::ymd(`Custody End`),
      `Service Type` = factor(`Service Type`),
      `Service Outcome` = factor(`Service Outcome`)
    ) %>%
    dplyr::filter(
      `Custody End` <= to,
      `Custody End` >= from
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of custody parent contacts, by month
#'
#' @description Outputs a data frame where each record represents a parent of
#' a child in custody, per month in the period, including Run Date,
#' Period Start date, Period End date, responsible Region, responsible County,
#' Supervisor name, Worker name, Month Name, Month Start date, Month End date,
#' Case ID, Child ID, Child Name, Custody Start date, Custody End date,
#' Parent ID, Reunification indicator, Narrative DateTime
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
custody_parent_monthly_contacts <- function(
  from = Sys.Date(),
  to = Sys.Date()
) {
  db_con <- con
  paste(
      'exec mdcpsde_gen_cust_parent_monthly_contacts',
      paste0('\'', gsub('-', '', from), '\','),
      paste0('\'', gsub('-', '', to), '\'')
    ) %>%
    DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_cust_parent_monthly_contacts') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Period Start` = as.Date(`Period Start`),
      `Period End` = as.Date(`Period End`),
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Month Start` = as.Date(`Month Start`),
      `Month End` = as.Date(`Month End`),
      `Custody Start` = as.Date(`Custody Start`),
      `Custody End` = as.Date(`Custody End`)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A Table of Closing Summary Narratives
#'
#' @return a data frame
#' @export
#'
#' @importFrom magrittr "%>%"
closing_summary_narratives <- function()
  {
    db_con <- con
    result <- dplyr::tbl(db_con, 'mdcpsde_case_narratives_closing_summary') %>%
      dplyr::collect() %>%
      dplyr::mutate(
        `Run Date` = lubridate::ymd_hms(`Run Date`),
        `Narrative DateTime` = lubridate::ymd_hms(`Narrative DateTime`),
        `Narrative Timestamp` = lubridate::ymd_hms(`Narrative Timestamp`)
         ) %>%
      dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
      dplyr::select_all(tolower)


      result
}
#' A table of Custody Child contacts
#'
#' @description Outputs a data frame where each record represents a period
#' of placement into a resource of a custody child, including Placement ID,
#' Child ID, Custody Sequence Number, Placement Sequence Per Episode,
#' Resource ID, County, Status, Placement Start date, Placement End date,
#' Placement Timestamp datetime, Rate, Placement Change Reason,
#' Facility Type, Worker ID, Child Placed in Proximity? indicator,
#' Reason Not Placed in Proximity, Placed Together indicator,
#' Reason Not Placed Together, Approval for Congregate Care indicator,
#' Approved for Emergency Placement indicator,
#' Reason(s) Approved for Cong Care, Reason(s) Approved for Emerg Placement
#'
#' @param date_filter whether the result should be filtered
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
custody_placements <- function(
  date_filter = T,
  from = Sys.Date(),
  to = Sys.Date()
) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_placements') %>%
    (function(data){
      if(date_filter) {
        data %>% dplyr::filter(
          `Placement Start` <= to,
          (`Placement End` >= from | is.na(`Placement End`))
        )
      } else {
        data
      }
    }) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      County = factor(County),
      Status = factor(Status),
      `Placement Start` = lubridate::ymd(`Placement Start`),
      `Placement End` = lubridate::ymd(`Placement End`),
      `Placement Timestamp` = lubridate::ymd_hms(`Placement Timestamp`),
      Rate = factor(Rate),
      `Placement Change Reason` = factor(`Placement Change Reason`),
      `Facility Type` = factor(`Facility Type`),
      `Child Placed in Proximity?` = factor(`Child Placed in Proximity?`),
      `Reason Not Placed in Proximity` = factor(`Reason Not Placed in Proximity`),
      `Placed Together` = factor(`Placed Together`),
      `Reason Not Placed Together` = factor(`Reason Not Placed Together`),
      `Approval for Congregate Care` = factor(`Approval for Congregate Care`),
      `Approved for Emergency Placement` = factor(`Approved for Emergency Placement`),
      `RD Approval Date` = lubridate::ymd_hms(`RD Approval Date`),
      `cong_care_reasons` = factor(`Reason Approved for Cong Care`),
      `emer_plac_reasons` = factor(`Reason Approved for Emerg Placement`),
      `Custody Sequence Number` = as.integer(`Custody Sequence Number`)

    #       ) %>%
    # dplyr::group_by(
    #   `Placement ID`, `Child ID`, `Custody Sequence Number`,
    #   `Placement Sequence Per Episode`, `Resource ID`, `County`, Status,
    #   `Placement Start`, `Placement End`, `Placement Timestamp`, Rate,
    #   `Placement Change Reason`, `Facility Type`, `Worker ID`,
    #   `Child Placed in Proximity?`, `Reason Not Placed in Proximity`,
    #   `Placed Together`, `Reason Not Placed Together`,
    #   `Approval for Congregate Care`, `Approved for Emergency Placement`, `RD Approval Date`
    # ) %>%
    # dplyr::summarise(
    #   `Reason(s) Approved for Cong Care` = list(`Reason Approved for Cong Care`),
    #   `Reason(s) Approved for Emerg Placement` = list(`Reason Approved for Emerg Placement`)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Status %in% c("A", "C")) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of Custody Child contacts
#'
#' @description Outputs a data frame where each record represents a period
#' of placement into a resource of a custody child, including Placement ID,
#' Child ID, Custody Sequence Number, Placement Sequence Per Episode,
#' Resource ID, County, Status, Placement Start date, Placement End date,
#' Placement Timestamp datetime, Rate, Placement Change Reason,
#' Facility Type, Worker ID, Child Placed in Proximity? indicator,
#' Reason Not Placed in Proximity, Placed Together indicator,
#' Reason Not Placed Together, Approval for Congregate Care indicator,
#' Approved for Emergency Placement indicator,
#' Reason(s) Approved for Cong Care, Reason(s) Approved for Emerg Placement
#'
#' @param date_filter whether the result should be filtered
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
custody_placements_withpending <- function(
  date_filter = T,
  from = Sys.Date(),
  to = Sys.Date()
) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_placements') %>%
    (function(data){
      if(date_filter) {
        data %>% dplyr::filter(
          `Placement Start` <= to,
          (`Placement End` >= from | is.na(`Placement End`))
        )
      } else {
        data
      }
    }) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      County = factor(County),
      Status = factor(Status),
      `Placement Start` = lubridate::ymd(`Placement Start`),
      `Placement End` = lubridate::ymd(`Placement End`),
      `Placement Timestamp` = lubridate::ymd_hms(`Placement Timestamp`),
      Rate = factor(Rate),
      `Placement Change Reason` = factor(`Placement Change Reason`),
      `Facility Type` = factor(`Facility Type`),
      `Child Placed in Proximity?` = factor(`Child Placed in Proximity?`),
      `Reason Not Placed in Proximity` = factor(`Reason Not Placed in Proximity`),
      `Placed Together` = factor(`Placed Together`),
      `Reason Not Placed Together` = factor(`Reason Not Placed Together`),
      `Approval for Congregate Care` = factor(`Approval for Congregate Care`),
      `Approved for Emergency Placement` = factor(`Approved for Emergency Placement`),
      `RD Approval Date` = lubridate::ymd_hms(`RD Approval Date`),
      `cong_care_reasons` = factor(`Reason Approved for Cong Care`),
      `emer_plac_reasons` = factor(`Reason Approved for Emerg Placement`),
      `Custody Sequence Number` = as.integer(`Custody Sequence Number`)

      #       ) %>%
      # dplyr::group_by(
      #   `Placement ID`, `Child ID`, `Custody Sequence Number`,
      #   `Placement Sequence Per Episode`, `Resource ID`, `County`, Status,
      #   `Placement Start`, `Placement End`, `Placement Timestamp`, Rate,
      #   `Placement Change Reason`, `Facility Type`, `Worker ID`,
      #   `Child Placed in Proximity?`, `Reason Not Placed in Proximity`,
      #   `Placed Together`, `Reason Not Placed Together`,
      #   `Approval for Congregate Care`, `Approved for Emergency Placement`, `RD Approval Date`
      # ) %>%
      # dplyr::summarise(
      #   `Reason(s) Approved for Cong Care` = list(`Reason Approved for Cong Care`),
      #   `Reason(s) Approved for Emerg Placement` = list(`Reason Approved for Emerg Placement`)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Status %in% c("A", "C", "P")) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}


#' A table of placement change events
#'
#' @description Outputs a data frame where each record represents a placement
#' change event, i.e. the movement of a custody child from one placement to
#' another, including Child ID, Custody Sequence Number,
#' Placement Sequence Per Episode, Placement ID for the originating
#' placement (OP), OP Resource ID, OP County, OP Facility Type,
#' OP Placement Start date, OP Placement End Date, OP Rate,
#' Placement Change Reason, Placement ID for the subsequent placement (SP),
#' SP Resource ID, SP County, SP Facility Type, SP Rate,
#' Placement Count value (whether and how much the OP contributes to the
#' child's total placement count), Placement Disrupted indicator.
#'
#' @param date_filter whether the result should be filtered
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
custody_placement_changes <- function(
  date_filter = F,
  from = Sys.Date(),
  to = Sys.Date()
) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_placement_changes') %>%
    (function(data) {
      if(date_filter) {
        data %>%
          dplyr::filter(
            `Placement Start` <= to,
            is.na(`Placement End`)|`Placement End` >= from
          )
      } else {
        data
      }
    }) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      County = factor(County, levels = unique(sort(County))),
      `Facility Type` = factor(`Facility Type`),
      `Placement Start` = lubridate::ymd(`Placement Start`),
      `Placement End` = lubridate::ymd(`Placement End`),
      Rate = factor(Rate),
      `Placement Change Reason` = factor(`Placement Change Reason`),
      `Subsequent County` = factor(
        `Subsequent County`,
        levels = unique(sort(`Subsequent County`))
      ),
      `Subsequent Facility Type` = factor(`Subsequent Facility Type`),
      `Subsequent Rate` = factor(`Subsequent Rate`),
      `Placement Disrupted` = as.logical(`Placement Disrupted`)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A Table of family service plans
#'
#'  @description outputs a table of family service plans for custody children
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#'
#' @importFrom magrittr "%>%"
family_service_plans <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con

  paste(
    'exec mdcpsde_gen_fsp',
    paste0('\'', gsub('-', '', from), '\','),
    paste0('\'', gsub('-', '', to), '\'')
  ) %>%
    DBI::dbExecute(db_con, .)
  results <- dplyr::tbl(db_con, 'mdcpsde_fsp') %>%
    dplyr::collect() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower) %>%
    dplyr::mutate(
      run_date = lubridate::ymd_hms(run_date),
      period_start = as.Date(period_start),
      period_end = as.Date(period_end),
      region  = factor(region, levels = region_vector()),
      county = factor(county, levels = unique(sort(county))),
      child_dob = as.Date(child_dob),
      custody_start = as.Date(custody_start),
      custody_end = as.Date(custody_end),
      fsp_type = factor(fsp_type),
      fsp_submitted = as.Date(fsp_submitted),
      fsp_approved = as.Date(fsp_approved),
      fsp_end = lubridate::ymd(fsp_end),
      fsp_timestamp = lubridate::ymd_hms(fsp_timestamp),
      ftm_indicator = factor(ftm_indicator),
      member_type = factor(member_type)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  results
}




#' A table of foster care review results
#'
#' @description Outputs a data frame where each record represents an FCR review
#' for a custody child case, including responsible Region, responsible County,
#' responsible Supervisor, responsible Worker, Case ID, Child ID, Child Name,
#' Child DOB, Custody Start date, Custody End date, Custody Sequence Number,
#' Custody Tracking Number, Review ID, Review Sequence, Review Date,
#' Sent to County date, Child Support Ordered indicator,
#' Foster Parent Training Required indicator, Parent Convicted indicator,
#' Parent Previous TPR indicator, Reviewed Risk Levelindicator,
#' Reviewed Safety indicator, Reviewed Well-Being indicator,
#' Risk Abandonment indicator, Risk No Contact indicator,
#' Risk Abusive Parent indicator, Risk No Visitation indicator,
#' Risk Parent Not Compliant indicator, Risk Parent Unchanged Behavior indicator,
#' Risk Animosity indicator, Risk Reun Not in Best Interest indicator,
#' Aggravated Circumstances indicator, Recommend TPR indicator,
#' ASFA Cared for by a Relative indicator, ASFA Child Best Interest indicator,
#' ASFA Services Not Provided indicator, ASFA Child 14+ Objects indicator,
#' ASFA Parents Maintain Visits indicator
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
foster_care_reviews <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  paste(
      'exec mdcpsde_gen_fcr_reviews',
      paste0('\'', gsub('-', '', from), '\','),
      paste0('\'', gsub('-', '', to), '\'')
    ) %>%
      DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_fcr_reviews') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Period Start` = as.Date(`Period Start`),
      `Period End` = as.Date(`Period End`),
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Child DOB` = as.Date(`Child DOB`),
      `Custody Start` = lubridate::ymd(`Custody Start`),
      `Custody End` = lubridate::ymd(`Custody End`),
      `Review Date` = as.Date(`Review Date`),
      `Sent to County` = as.Date(`Sent to County`),
      `Child Support Ordered` = factor(`Child Support Ordered`),
      `FP Training Req` = factor(`FP Training Req`),
      `Parent Convicted` = factor(`Parent Convicted`),
      `Parent Previous TPR` = factor(`Parent Previous TPR`),
      `Reviewed Risk Level` = factor(`Reviewed Risk Level`),
      `Reviewed Safety` = factor(`Reviewed Safety`),
      `Reviewed Well-Being` = factor(`Reviewed Well-Being`),
      `Risk Abandoment` = !is.na(`Risk Abandoment`),
      `Risk No Contact` = !is.na(`Risk No Contact`),
      `Risk Abusive Parent` = !is.na(`Risk Abusive Parent`),
      `Risk No Visitation` = !is.na(`Risk No Visitation`),
      `Risk Parent Not Compliant` = !is.na(`Risk Parent Not Compliant`),
      `Risk Parent Unchanged Behavior` = !is.na(`Risk Parent Unchanged Behavior`),
      `Risk Animosity` = !is.na(`Risk Animosity`),
      `Risk Reun Not in Best Interest` = !is.na(`Risk Reun Not in Best Interest`),
      `Aggravated Circumstances` = !is.na(`Aggravated Circumstances`),
      `Recommend TPR` = !is.na(`Recommend TPR`),
      `ASFA Cared for by a Relative` = !is.na(`ASFA Cared for by a Relative`),
      `ASFA Child Best Interest` = !is.na(`ASFA Child Best Interest`),
      `ASFA Services Not Provided` = !is.na(`ASFA Services Not Provided`),
      `ASFA Child 14+ Objects` = !is.na(`ASFA Child 14+ Objects`),
      `ASFA Parents Maintain Visits` = !is.na(`ASFA Parents Maintain Visits`)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of recommended surveys for foster parents
#'
#' @description Outputs a data frame where each record represents a foster home
#' or prospective foster home (Inquiry initated after **pit** date), including
#' Resource ID, Inquiry Start Date, Training Complete Date,
#' Completed Training indicator, Home Study Required indicator, Inquiry End date,
#' Converted to Home Study indicator, Home Study Worker Rec indicator,
#' Home Study Worker Date, Worker Approved Home Study indicator,
#' Home Study Super Rec indicator, Home Study Super Date (Supervisor Approval),
#' ASWS Approved Home Study indicator, License Start Date,
#' Initial License Granted indicator, Placement Start Date,
#' First Child Placed indicator, First Child Removed Date,
#' First Child Removed indicator, First Child Adopted Date,
#' First Child Adopted indicator, Closed Date, Final Disposition Date,
#' Recommended Survey, Email Address, Email Address Source
#'
#' @param pit start date for inquiries to be included
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
foster_parent_survey_pop <- function(pit = as.Date('2018-04-01')) {
  db_con <- con
  paste(
    'exec mdcpsde_gen_foster_parent_survey_pop_backup',
    paste0('\'', gsub('-', '', pit), '\'')
  ) %>%
    DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_foster_parent_survey_pop_backup') %>%
    dplyr::collect() %>%
    dplyr::mutate_at(
      c(2, 6, 7, 10, 13, 16, 18, 20, 22, 24, 26, 27),
      as.Date
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of the current day's workload numbers
#'
#' @description Outputs a data frame where each records represents a service
#' provided by a worker, including Run Date, COR Region, COR County,
#' Supervisor Region (employed), Supervisor County (employed), Supervisor Unit,
#' Supervisor Job Title, Supervisor Name, Worker Region (employed),
#' Worker County (employed), Worker Unit, Worker Job Title, Worker Name,
#' Assignment Type, Assignment Description, Case ID Type, Case ID,
#' Person ID Type, Person ID, Person Name, Weight (case weight of service)
#'
#' @return a data
#' @export
#' @importFrom magrittr "%>%"
daily_workload <- function() {
  db_con <- con
  DBI::dbExecute(db_con, 'exec mdcpsde_gen_workload_table_NoPK')
  result <- dplyr::tbl(db_con, 'mdcpsde_workload_table_nopk') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Run Date` = lubridate::ymd_hms(`Run Date`),
      `COR Region` = factor(`COR Region`, levels = region_vector()),
      `COR County` = factor(`COR County`, levels = unique(sort(`COR County`))),
      `Supervisor Region` = factor(`Supervisor Region`, levels = region_vector()),
      `Supervisor County` = factor(
        `Supervisor County`, levels = unique(sort(`Supervisor County`))
      ),
      `Supervisor Unit` = factor(
        `Supervisor Unit`,
        levels = unique(sort(`Supervisor Unit`))
      ),
      `Supervisor Job Title` = factor(
        `Supervisor Job Title`,
        levels = unique(sort(`Supervisor Job Title`))
      ),
      `Worker Region` = factor(`Worker Region`, levels = region_vector()),
      `Worker County` = factor(
        `Worker County`,
        levels = unique(sort(`Worker County`))
      ),
      `Worker Unit` = factor(
        `Worker Unit`,
        levels = unique(sort(`Worker Unit`))
      ),
      `Worker Job Title` = factor(
        `Worker Job Title`,
        levels = unique(sort(`Worker Job Title`))
      ),
      `Assignment Type` = factor(
        `Assignment Type`,
        levels = unique(sort(`Assignment Type`))
      ),
      `Assignment Description` = factor(
        `Assignment Description`,
        levels = unique(sort(`Assignment Description`))
      ),
      `Case ID Type` = factor(
        `Case ID Type`,
        levels = unique(sort(`Case ID Type`))
      ),
      `Person ID Type` = factor(
        `Person ID Type`,
        levels = unique(sort(`Person ID Type`))
      ),
      `Assignment Region` = dplyr::if_else(
        stringr::str_detect(`Assignment Description`, 'COS'),
        `Worker Region`,
        `COR Region`
      ),
      `Assignment County` = dplyr::if_else(
        stringr::str_detect(`Assignment Description`, 'COS'),
        as.character(`Worker County`),
        as.character(`COR County`)
      ),
      `Assignment County` = factor(
        `Assignment County`,
        levels = sort(unique(`Assignment County`))
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of inhome case members
#'
#' @description Outputs a data frame where each record represents a member of
#' an in-home services case, including responsible Region, responsible County,
#' Case ID, Open Period Sequence, Case Start date, Case Closed date, Person ID,
#' Person Status, Person Type, Case Member Start date, Case Member End date,
#' and Case Change Reason.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
inhome_case_members <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcspde_inhome_case_members') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Case Start` = lubridate::ymd(`Case Start`),
      `Case Closed` = lubridate::ymd(`Case Closed`),
      `Person Status` = factor(`Person Status`),
      `Person Type` = factor(`Person Type`),
      `Case Member Start` = lubridate::ymd(`Case Member Start`),
      `Case Member End` = lubridate::ymd(`Case Member End`),
      `Case Change Reason` = factor(`Case Change Reason`)
    ) %>%
    dplyr::filter(
      `Case Start` <= to,
      (is.na(`Case Member End`) | `Case Member End` >= from)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of foster parent relationships
#'
#' @description Outputs a data frame where each record represents the
#' relationship between a custody child and foster parent, including
#' Resource ID, Foster Parent ID, Foster Parent Role, Foster Parent Start date,
#' Foster Parent End date, Foster Parent Sequence, Child ID, Placement ID,
#' Placement Start date, Placement End date
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
foster_parent_history <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  DBI::dbExecute(db_con, 'exec mdcpsde_gen_foster_parents_history')
  result <- dplyr::tbl(db_con, 'mdcpsde_foster_parents_history') %>%
    dplyr::filter(
      `Placement Start` <= to,
      (`Placement End` >= from | is.na(`Placement End`))
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Foster Parent Role` = factor(`Foster Parent Role`),
      `Foster Parent Start` = as.Date(`Foster Parent Start`),
      `Foster Parent End` = as.Date(`Foster Parent End`),
      `Placement Start` = as.Date(`Placement Start`),
      `Placement End` = as.Date(`Placement End`)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of ANE investigations
#'
#' @description Outputs a data frame where each record represents an
#' investigation, including intake Region, intake County, Investigation
#' Worker ID, Intake ID, Investigation HH ID, Report Level,
#' SIU Worker indicator, Incident Date, Intake DateTime, Intake Status,
#' Screening DateTime, Findings Submitted Date, and Findings Approved Date.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
investigations <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_investigations_q3andlater') %>%
    dplyr::filter(
      `Intake DateTime` <= to,
      (`Findings Approved Date` >= from | is.na(`Findings Approved Date`)),
      (`Intake Status` %in% c('35INV', '40INV') | `Screening DateTime` >= from)
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `INTS_SIU_ANE_IND` = factor(`INTS_SIU_ANE_IND`),
      `INTS_SIU_ANE_OWNER` = factor(`INTS_SIU_ANE_OWNER`),
      `INTI_SIU_ANE` = factor(`INTI_SIU_ANE`),
      `INTI_SIU_OWNER` = factor(`INTI_SIU_OWNER`),
      `Incident Date` = lubridate::ymd(`Incident Date`),
      `Intake DateTime` = lubridate::ymd_hms(`Intake DateTime`),
      `Intake Status` = factor(`Intake Status`),
      `Screening DateTime` = lubridate::ymd_hms(`Screening DateTime`),
      `Findings Submitted Date` = lubridate::ymd(`Findings Submitted Date`),
      `Findings Approved Date` = lubridate::ymd(`Findings Approved Date`)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of ANE investigations
#'
#' @description Outputs a data frame where each record represents a report of ane
#' , including intake Region, intake County, Investigation
#' Worker ID, Intake ID, Investigation HH ID, Report Level,
#' SIU Worker indicator, Incident Date, Intake DateTime, Intake Status,
#' Screening DateTime, Findings Submitted Date, and Findings Approved Date.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
anereports <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_investigations_q3andlater') %>%
    dplyr::filter(
      `Intake DateTime` <= to,
      `Intake DateTime` >= from,
      (`Findings Approved Date` >= from | is.na(`Findings Approved Date`))) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `SIU Worker` = factor(`SIU Worker`),
      `Incident Date` = lubridate::ymd(`Incident Date`),
      `Intake DateTime` = lubridate::ymd_hms(`Intake DateTime`),
      `Intake Status` = factor(`Intake Status`),
      `Screening DateTime` = lubridate::ymd_hms(`Screening DateTime`),
      `Findings Submitted Date` = lubridate::ymd(`Findings Submitted Date`),
      `Findings Approved Date` = lubridate::ymd(`Findings Approved Date`)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of investigation initiations
#'
#' @description Outputs a data frame where each record represents an
#' investigation initiation, including Intake ID and Initiation DateTime.
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
investigation_initiations <- function(){
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_inv_initiations') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Initiation DateTime` = lubridate::ymd_hms(`Initiation DateTime`)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}


#' A table of all medical services
#'
#' @description Ouputs a dataframe of all medicals for custody children in a given data range
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"

medicals_custody_child <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  paste(
    'exec mdcpsde_gen_child_medical_services',
    paste0('\'', gsub('-', '', from), '\','),
    paste0('\'', gsub('-', '', to), '\'')
  ) %>%
    DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_child_medical_services') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Period Start` = as.Date(`Period Start`),
      `Period End` = as.Date(`Period End`),
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Child DOB` = as.Date(`Child DOB`),
      `Custody Start` = as.Date(`Custody Start`),
      `Custody End` = as.Date(`Custody End`),
      `Custody Sequence Number` = as.integer(`Custody Sequence Number`),
      `Exam Type` = as.factor(`Exam Type`),
      `Exam Date` = as.Date(`Exam Date`)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)

    result
}


#' A table of medical service information
#'
#' @description  Outputs a data frame where each record represents medical
#' services provided to a child during a case/custody episode
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
medical_services_table <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  paste(
    'exec mdcpsde_gen_medical_services_root_table',
    paste0('\'', gsub('-', '', from), '\','),
    paste0('\'', gsub('-', '', to), '\'')
  ) %>%
    DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_medical_services_root_table') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Period Start` = as.Date(`Period Start`),
      `Period End` = as.Date(`Period End`),
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Child DOB` = as.Date(`Child DOB`),
      `Custody Start` = as.Date(`Custody Start`),
      `Custody End` = as.Date(`Custody End`),
      `Initial Medical Due Date` = as.Date(`Initial Medical Due Date`),
      `Initial Medical Exam Date` = as.Date(`Initial Medical Exam Date`),
      `First Comp Medical Due Date` = as.Date(`First Comp Medical Due Date`),
      `First Comp Medical Exam Date` = as.Date(`First Comp Medical Exam Date`),
      `EPSDT 1 Due Date` = as.Date(`EPSDT 1 Due Date`),
      `EPSDT 1 Exam Date` = as.Date(`EPSDT 1 Exam Date`),
      `EPSDT 2 Due Date` = as.Date(`EPSDT 2 Due Date`),
      `EPSDT 2 Exam Date` = as.Date(`EPSDT 2 Exam Date`),
      `EPSDT 3 Due Date` = as.Date(`EPSDT 3 Due Date`),
      `EPSDT 3 Exam Date` = as.Date(`EPSDT 3 Exam Date`),
      `EPSDT 4 Due Date` = as.Date(`EPSDT 4 Due Date`),
      `EPSDT 4 Exam Date` = as.Date(`EPSDT 4 Exam Date`),
      `EPSDT 5 Due Date` = as.Date(`EPSDT 5 Due Date`),
      `EPSDT 5 Exam Date` = as.Date(`EPSDT 5 Exam Date`),
      `EPSDT 6 Due Date` = as.Date(`EPSDT 6 Due Date`),
      `EPSDT 6 Exam Date` = as.Date(`EPSDT 6 Exam Date`),
      `EPSDT 7 Due Date` = as.Date(`EPSDT 7 Due Date`),
      `EPSDT 7 Exam Date` = as.Date(`EPSDT 7 Exam Date`),
      `EPSDT 8 Due Date` = as.Date(`EPSDT 8 Due Date`),
      `EPSDT 8 Exam Date` = as.Date(`EPSDT 8 Exam Date`),
      `EPSDT 9 Due Date` = as.Date(`EPSDT 9 Due Date`),
      `EPSDT 9 Exam Date` = as.Date(`EPSDT 9 Exam Date`),
      `EPSDT 10 Due Date` = as.Date(`EPSDT 10 Due Date`),
      `EPSDT 10 Exam Date` = as.Date(`EPSDT 10 Exam Date`),
      `EPSDT 11 Due Date` = as.Date(`EPSDT 11 Due Date`),
      `EPSDT 11 Exam Date` = as.Date(`EPSDT 11 Exam Date`),
      `Last Yearly Medical Due Date` = as.Date(`Last Yearly Medical Due Date`),
      `Last Yearly Medical Exam Date` = as.Date(`Last Yearly Medical Exam Date`),
      `Second to Last Yearly Medical Due Date` = as.Date(
        `Second to Last Yearly Medical Due Date`
      ),
      `Second to Last Yearly Medical Exam Date` = as.Date(
        `Second to Last Yearly Medical Exam Date`
      ),
      `Most Recent Medical Exam` = as.Date(`Most Recent Medical Exam`),
      `Initial Dental Due Date` = as.Date(`Initial Dental Due Date`),
      `Initial Dental Exam Date` = as.Date(`Initial Dental Exam Date`),
      `Last Dental Exam Due` = as.Date(`Last Dental Exam Due`),
      `Last Dental Exam Date` = as.Date(`Last Dental Exam Date`),
      `Second to Last Dental Exam Due` = as.Date(
        `Second to Last Dental Exam Due`
      ),
      `Second to Last Dental Exam Date` = as.Date(
        `Second to Last Dental Exam Date`
      ),
      `Most Recent Dental Date` = as.Date(`Most Recent Dental Date`),
      `Most Recent Placement Change` = as.Date(`Most Recent Placement Change`)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of all permamanency plans for kids in custody during a period
#'
#' @description Oputputs a data frame where each record represents a permanency or concurrent plan for a custody child,
#' where the child was in custody sometime during the period selected.  The data fram eincludeds the follwoing columns:
#' Region, Supervisor, Worker, Case ID, Child ID, Child Name, Chidl Dob, Cusotdy Start, Custody End, PlanID, FSPID, Plan Type,
#' Plan Code, PLan Start, Plan End, plan Achievement, PLan Status
#'
#' @param from start date of the period
#' @param to end date of the period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
expanded_perm_plan <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  paste(
    ' exec spExpandedPermPlanByDates',
    paste0('\'',gsub('-','',from), '\','),
    paste0('\'', gsub('-', '', to), '\'')
    ) %>%
    DBI::dbGetQuery(db_con, .)

}

#' A table of permanency plans
#'
#' @description  Outputs a data frame where each record represents a permanency
#' (or concurrent) plan for a custody child for an FSP  active in the defined
#' period, including responsible Region, responsible County,
#' responsible Supervisor, responsible Worker, Case ID, Child ID, Child Name,
#' Child DOB, Custody Start, Custody End, Custody Sequence Number, FSP ID,
#' FSP Type, FSP Approved date, FSP End date, FSP Timestamp, Member Type,
#' Plan ID, Plan Type, Plan Name, Plan Status, and Plan Timestamp
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
permanency_plans <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
 paste(
    'exec mdcpsde_gen_perm_plans',
    paste0('\'', gsub('-', '', from), '\','),
    paste0('\'', gsub('-', '', to), '\'')
  ) %>%
  DBI::dbExecute(db_con, .)
 result <- dplyr::tbl(db_con, 'mdcpsde_perm_plans') %>%
   dplyr::collect() %>%
     dplyr::mutate(
      `Period Start` = as.Date(`Period Start`),
      `Period End` = as.Date(`Period End`),
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Custody Start` = as.Date(`Custody Start`),
      `Custody End` = as.Date(`Custody End`),
      `FSP Submitted` = as.Date(`FSP Submitted`),
      `FSP Approved` = as.Date(`FSP Approved`),
      `Plan Type` = factor(`Plan Type`),
      `Plan Name` = factor(`Plan Name`),
      `Plan Status` = factor(`Plan Status`),
      `Plan Start` = as.Date(`Plan Start`),
      `Plan End` = as.Date(`Plan End`)
        ) %>%
   dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
   dplyr::select_all(tolower)


 result
  }

#' A table of case member relationships
#'
#' @description  Outputs a data frame where each record represents a
#' relationship between two individuals, including Person ID, Relative ID,
#' Relationship, Relationship Start date, Relationship End date,
#' Relationship End Reason
#'
#' @param date_filter whether the result should be filtered
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
relationships <- function(
  date_filter = T,
  from = Sys.Date(),
  to = Sys.Date()
) {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_relationships') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      Relationship = factor(Relationship),
      `Relationship Start` = lubridate::ymd(`Relationship Start`),
      `Relationship End` = lubridate::ymd(`Relationship End`),
      `Relationship End Reason` = factor(`Relationship End Reason`)
    )

  if(date_filter) {
    result <- result %>% dplyr::filter(
      `Relationship Start` <= to,
      (`Relationship End` >= from | is.na(`Relationship End`))
    ) %>%
      dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
      dplyr::select_all(tolower)

  }


  result
}

#' A table of resource homes with indicators for placement limits
#'
#' @description Outputs a data frame where each record represents a licensed
#' resource home with children placed, including Region, County,
#' Resource Supervisor, Resource Worker, Resource ID, Resource Name,
#' Total Children in Home, Custody Children Placed, Total Children Under 2,
#' Therapeutic Children Placed, Siblings Only, and Meets Standards indicator.
#'
#' @param pit point in time date of the report
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
resource_home_limits <- function(pit = Sys.Date()) {
  db_con <- con
  paste(
      'exec mdcpsde_gen_resource_home_limits',
      paste0('\'', gsub('-', '', pit), '\'')
    ) %>%
    DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_resource_home_limits') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Data Date` = as.Date(`Data Date`),
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County)))
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of resource home services
#'
#' @description Outputs a data frame where each record represents a placement
#' resource service provided, including Resource ID, Resource Name,
#' Resource Service, Service Start date, Service End date, License Start date,
#' License End date, License Status, Home Status, responsible Region, and
#' Responsible County.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
resource_services <- function(from = Sys.Date(), to = Sys.Date()) {
   db_con <- con
   result <- dplyr::tbl(db_con, 'mdcpsde_resource_services') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Resource Service` = factor(`Resource Service`),
      `Service Start` = as.Date(`Service Start`),
      `Service End` = as.Date(`Service End`),
      `License Start` = as.Date(`License Start`),
      `License End` = as.Date(`License End`),
      `License Status` = factor(`License Status`),
      `Home Status` = factor(`Home Status`),
      `Resource Service` = trimws(`Resource Service`)
    ) %>%
    dplyr::filter(
      `Service Start` <= to,
      (`Service End` >= from | is.na(`Service End`)),
      `License Start` <= to,
      (`License End` >= from | is.na(`License End`))
    ) %>%
     dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
     dplyr::select_all(tolower)


   result
}


#' A table of records indicating worker contacts with children following MIC
#'
#' @description Outputs a data frame where each record represents a child who
#' was listed as a victim in an Investigation that was approved and
#' substantiated within the reporting period and which was the result of a
#' maltreatment in care allegation, after which the child remained in the
#' same out of home placement in which the incident occurred, including
#' responsible Region, responsible County, ASWS Name, Worker Name, Case ID,
#' Child ID, Child Name, Intake ID, Incident Date, Intake DateTime,
#' Findings Approved Date, Resource ID, Resource Name, Placement End,
#' First Week Start date, Week 1 Contacts indicator, Second Week Start date,
#' Week 2 Contacts indicator, Third Week Start date, Week 3 Contacts indicator,
#' Fourth Week Start date, Week 4 Contacts indicator,
#' Weekly Contacts Made indicator, First Month Start date,
#' Month 1 Contacts indicator, Second Month Start date,
#' Month 2 Contacts indicator, Third Month Start date,
#' Month 3 Contacts indicator, Fourth Month Start date,
#' Monthly Contacts Made indicator, Required Contacts Made indicator
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
sub_mic_child_contacts <- function(
  from = lubridate::floor_date(Sys.Date() - lubridate::years(1), 'month'),
  to = lubridate::floor_date(Sys.Date(), 'month') - lubridate::days(1)
) {
  db_con <- con
  paste(
      'exec mdcpsde_gen_sub_mic_child_contacts',
      paste0('\'', gsub('-', '', from), '\','),
      paste0('\'', gsub('-', '', to), '\'')
    ) %>%
    DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_sub_mic_child_contacts') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Period Start` = as.Date(`Period Start`),
      `Period End` = as.Date(`Period End`),
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Incident Date` = as.Date(`Incident Date`),
      `Findings Approved Date` = as.Date(`Findings Approved Date`),
      `Resource Name` = factor(`Resource Name`),
      `Placement End` = as.Date(`Placement End`),
      `Weekly Contacts Made` = as.logical(`Weekly Contacts Made`),
      `First Month Start` = as.Date(`First Month Start`),
      `Second Month Start` = as.Date(`Second Month Start`),
      `Third Month Start` = as.Date(`Third Month Start`),
      `Monthly Contacts Made` = as.logical(`Monthly Contacts Made`),
      `Required Contacts Made` = `Weekly Contacts Made`
        & (`Monthly Contacts Made` | is.na(`Monthly Contacts Made`))
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}


#' A table of records indicating sibling contemporaneous initial placements
#'
#' @description Outputs a data frame where each record represents a custody
#' child's initial placement per custody episode, including responsible Region,
#' responsible County, Child ID, Custody Start date, Placed Together indicator
#' (from MACWIS), Reason not Placed Together, Sibling IDs (for siblings that
#' entered custody within 30 days of the child's entry date), count of Siblings
#' that entered custody within 30 days of the child's entry date, and count
#' of Siblings Placed Contemporaneously (in the same resource as the target
#' child, placed within 30 days of the child's inital placement date)
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
siblings_initally_placed_together <- function(
  from = Sys.Date(),
  to = Sys.Date()
) {
  db_con <- con
  paste(
      'exec mdcpsde_gen_sibling_initial_placements',
      paste0('\'', gsub('-', '', from), '\','),
      paste0('\'', gsub('-', '', to), '\'')
    ) %>%
    DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_sibling_initial_placements') %>%
    dplyr::collect() %>%
    dplyr::group_by(
      Region, County, `Child ID`, `Case ID`,
      `Custody Sequence Number`, `Custody Start`,
      `Placed Together`,
      `Reason Not Placed Together`
    ) %>%
    dplyr::summarise(
      `Sibling IDs` = list(`Sibling ID`),
      `Siblings` = n(),
      `Siblings Placed Contemporaneously` = sum(`Placed with Sibling`, na.rm = T)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of TPR dates
#'
#' @description Outputs a data frame where each record represents a custody
#' episode, including Child ID, Custody Start date, Custody End date,
#' entered Legally Freed Date, and Termination Date (derived from the
#' individual TPR dates of a male and female parent).
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
tpr_info <- function() {
  db_con <- con
  result <- dplyr::tbl(db_con, 'mdcpsde_tpr_table') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Custody Sequence Number` = as.integer(`Custody Sequence Number`),
      `Custody Start` = lubridate::ymd(`Custody Start`),
      `Custody End` = lubridate::ymd(`Custody End`),
      `Legally Freed Date` = lubridate::ymd(`Legally Freed Date`),
      `Termination Date` = lubridate::ymd(`Termination Date`)
    ) %>%
    unique() %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of TPR Timeframes for youth entering custody in last 22 months
#'
#' @description Outputs a data frame where each record represents a child
#' who entered custody at least one time in the past 22 months, including
#' responsible Region, responsible County, Child ID, Custody Start date,
#' Custody End date, Child Remains in Custody indicator,
#' In Custody 13/22 Months indicator, Thirteenth Month in Custody,
#' In Custody 15/22 Months indicator, Fifteenth Month in Custody,
#' In Custody 17/22 Months indicator, Seventeenth Month in Custody,
#' ASFA Cared for by a Relative indicator, ASFA Child's Best Interest indicator,
#' ASFA Services Not Provided indicator, ASFA Child 14+ Objects indicator,
#' ASFA Parents Maintain Visits indicator, TPR Submitted by Worker date,
#' TPR Submitted with 17 Months indicator, TPR Referred to AG date,
#' TPR Referred to AG within 17 Months indicator
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
tpr_timeframes <- function(to = Sys.Date()) {
  db_con <- con
  paste(
      'exec mdcpsde_gen_tpr_timeframes',
      paste0('\'', gsub('-', '', to), '\'')
    ) %>%
      DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_tpr_timeframes') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Custody Start` = as.Date(`Custody Start`),
      `Custody End` = as.Date(`Custody End`),
      `TPR Submitted by Worker` = as.Date(`TPR Submitted by Worker`),
      `TPR Referred to AG` = as.Date(`TPR Referred to AG`)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of youth court approvals for trial home visits
#'
#' @description Outputs a data frame where each record represents the final
#' placement of a custody child with a custody outcome of reunification,
#' including responsible Region, responsible County, Child ID,
#' Custody Sequence Number, Custody Start date, Custody End date,
#' Days in Custody, Case ID, Permanency Plan Name, Plan Start date,
#' Plan End date, Court (Hearing) Type, Court Order Date,
#' THV Approved by YC indicator, Resource ID, Placement Start date,
#' Placement End date, and Facility Type.
#'
#' @param date_filter whether the result should be filtered
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
thv_yc_approval <- function(
  from = Sys.Date(),
  to = Sys.Date()
) {
  db_con <- con
  paste(
    'exec mdcpsde_gen_thv_yc_approvals',
    paste0('\'', gsub('-', '', from), '\','),
    paste0('\'', gsub('-', '', to), '\'')
  ) %>%
    DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_thv_yc_approvals') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Run Date` = lubridate::ymd_hms(`Run Date`),
      `Period Start` = as.Date(`Period Start`),
      `Period End` = as.Date(`Period End`),
      Region = factor(Region, levels = region_vector()),
      County = factor(County, levels = unique(sort(County))),
      `Custody Start` = lubridate::ymd(`Custody Start`),
      `Custody End` = lubridate::ymd(`Custody End`),
      `Plan Name` = factor(`Plan Name`),
      `Plan Start` = lubridate::ymd(`Plan Start`),
      `Plan End` = lubridate::ymd(`Plan End`),
      # `Court Type` = factor(`Court Type`),
      # `Court Order Date` = lubridate::ymd(`Court Order Date`),
      # `THV Approved by YC` = factor(`THV Approved by YC`),
      `Placement Start` = lubridate::ymd(`Placement Start`),
      `Placement End` = lubridate::ymd(`Placement End`),
      `Facility Type` = factor(`Facility Type`)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of visitation plans
#'
#' @description Outputs a data frame where each record represents a plan of
#' visitation between a custody child and a family member, including Case ID,
#' Child ID, FSP ID, FSP Approved Date, FSP Sequence, Visitor ID,
#' Restricted indicator, Supervised indicator, Visitation Frequence, and
#' Frequency Type.
#'
#' @param from start date of the reporting period
#' @param to end date of the reporting period
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
visitation_plans <- function(from = Sys.Date(), to = Sys.Date()) {
  db_con <- con
  paste(
    'exec mdcpsde_gen_visitation_plans',
    paste0('\'', gsub('-', '', from), '\','),
    paste0('\'', gsub('-', '', to), '\'')
  ) %>%
    DBI::dbExecute(db_con, .)
  result <- dplyr::tbl(db_con, 'mdcpsde_visitation_plans') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Period Start` = as.Date(`Period Start`),
      `Period End` = as.Date(`Period End`),
      `Custody Start` = as.Date(`Custody Start`),
      `Custody End` = as.Date(`Custody End`),
      `FSP Approved Date` = as.Date(`FSP Approved Date`),
      Restricted = factor(Restricted),
      Supervised = factor(Supervised),
      `Frequency Type` = factor(`Frequency Type`)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}

#' A table of caseworker assignments
#'
#' @description Outputs a data frame where each record represents an assignment
#' of a worker to a case, including Case ID, Assignment Region,
#' Assignment County, County Type, Supervisor ID, Worker ID,
#' Worker Start datetime, Worker End datetime, Assignment Timestamp,
#' Worker PIN, worker Job Title, and worker Unit Title.
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%>%"
worker_assignments <- function() {
  db_con <- con
  DBI::dbExecute(db_con, 'exec mdcpsde_gen_worker_assignments')
  result <- dplyr::tbl(db_con, 'mdcpsde_worker_assignments') %>%
    dplyr::collect() %>%
    dplyr::mutate(
      `Assignment Region` = factor(`Assignment Region`, levels = region_vector()),
      `Assignment County` = factor(
        `Assignment County`,
        levels = unique(sort(`Assignment County`))
      ),
      `County Type` = factor(`County Type`),
      `Job Title` = factor(`Job Title`),
      `Unit Title` = factor(`Unit Title`)
    ) %>%
    dplyr::select_all(~gsub("\\s+|\\.", "_", .)) %>%
    dplyr::select_all(tolower)


  result
}
