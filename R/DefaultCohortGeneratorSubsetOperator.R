#' Default subset operators
#'
#' Make a default set of sub-operators
#' @param minAge (optional, set to 0) The min age on first drug exposure. Default is 0.
#' @param maxAge (optional, set to 90) The max age on first drug exposure. Default is 90.
#' @param genderSubset (Optional) by default set to  c(8532, 8507)
#' @param race (Optional) concept id for race
#' @param ethnicity (Optional) concept id for ethnicity
#' @param restrictToEarliestOccurrence do you want to restrict to first.
#' @param calendarStartDate (optional) The earliest date from which to consider drug exposures. Used to left-censor data.
#' @param calendarEndDate (optional) The latest date up to which to consider drug exposures. Used to right-censor data.
#' @param priorTime (optional, default 365) The number of days of prior observation.
#' @param followUpTime (optional, default 365) The number of days of follow-up observation.
#' @param limitTo (optional, default 'firstEver') Restrict to first observation period or all observation periods with drug exposure.
#'
#' @export
defaultCohortGeneratorSubsetOperator <- function(minAge = 0,
                                                 maxAge = 90,
                                                 genderSubset = c(8532, 8507),
                                                 restrictToEarliestOccurrence = TRUE,
                                                 race = NULL,
                                                 calendarStartDate = "2016-01-01",
                                                 calendarEndDate = Sys.Date() - 365,
                                                 priorTime = 365,
                                                 followUpTime = 365,
                                                 ethnicity = NULL,
                                                 limitTo = 'firstEver') {
  subsetOperators <- list()
  subsetOperators$limitValidGenderAgeMin0 <-
    CohortGenerator::createDemographicSubset(
      name = "",
      gender = c(8532, 8507),
      ageMin = minAge,
      ageMax = maxAge,
      race = race,
      ethnicity = ethnicity
    )
  
  subsetOperators$limitSubset <- CohortGenerator::createLimitSubset(
    name = "",
    priorTime = priorTime,
    followUpTime = followUpTime,
    limitTo = limitTo
  )
  return(subsetOperators)
}