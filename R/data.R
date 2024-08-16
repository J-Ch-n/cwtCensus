#' Release information
#'
#' @format ## `release`
#'
#'  \describe{
#'    \item{"brood_year"}{Integer. The birth year of each CWT batch.}
#'    \item{"total_release"}{Double. The total number of releases for each batch of CWT.}
#'    \item{"prod_exp"}{Double. The ratio between the number of tagged fish and the total number \cr
#'                      of fish in each batch, i.e., number of tagged fish / batch size.}
#'    \item{"tag_code"}{Any type. The identifier for each batch of CWT release.}
#' }
#'
#' @description Nimbus Fish Hatchery release information 2007 - 2009.
#' @source Regional Mark Processing Center (RMPC), Pacific States Marine Fisheries Commission (2024) \cr
#' Regional Mark Information System Database. www.rmpc.org (accessed 31 Jan 2024)
#'
"release"

#' Recovery information
#'
#' @format ## `recovery`
#'
#'  \describe{
#'    \item{"run_year"}{Integer. The year of each CWT recovery.}
#'    \item{"month"}{Integer. The month of each CWT recovery.}
#'    \item{"length"}{Double. The length in inches of each recovered fish.}
#'    \item{"tag_code"}{Any type. The identifier for each recovered tag.}
#'    \item{"fishery"}{Double. The identifier for the fishery from which the tag is recovered. \cr
#'                      The fisheries include river harvest, spawning ground escapement, hatchery \cr
#'                      escapement, ocean recreational fishing, ocean commercial fishing.}
#'    \item{"location"}{Character. The identifier for the location at which the tag is recovered.}
#'    \item{"size_limit"}{Double. The minimum harvest limit (inclusive) in inches for the recovered \cr
#'                        tag's region and date.}
#'    \item{"est_num"}{Double. The estimated number of fish each tag represents.}
#'    \item{"sex"}{Character. The biological sex of the recovered fish.}
#' }
#'
#' @description Nimbus Fish Hatchery recovery information 2007 - 2009.
#' @source Regional Mark Processing Center (RMPC), Pacific States Marine Fisheries Commission (2024) \cr
#' Regional Mark Information System Database. www.rmpc.org (accessed 31 Jan 2024)
#'
"recovery"

#' Age-specific survival information.
#'
#' @format ## `survival_default`
#'
#'  \describe{
#'    \item{age}{Integer. The age of fish in which the CWT is recovered}
#'    \item{month}{Integer. The month in which the CWTs are released}
#'    \item{rate}{Double. The natural mortality rate for the specified age and month}
#'  }
#' @description Age-specific survival rate described in \insertCite{age_struct}{cwtCensus}.
#' @references
#' \insertAllCited{}
"survival_default"

#' Length-at-age information
#'
#' @format ## `length_at_age`
#'
#'  \describe{
#'    \item{"age"}{Integer. The fishing age of the cohort.}
#'    \item{"month"}{Integer. The month in numeric form. For instance, `3L` is March.}
#'    \item{"mean"}{Double. The mean total body length in inches.}
#'    \item{"sd"}{Double. The standard deviation of total body length in inches.}
#' }
#'
#' @description length-at-age estimated in \insertCite{age_struct}{cwtCensus}. Estimated \cr
#' using the Basic Model described in \insertCite{bm}{cwtCensus} from hatchery fish from \cr
#' 2000-2020 originating from Coleman National Fish Hatchery, Feather River Hatchery, and \cr
#' Nimbus Fish Hatchery.
#'
#' @references
#' \insertAllCited{}
#'
"length_at_age"

#' Release mortality information
#'
#' @format ## `release_mort`
#'
#'  \describe{
#'    \item{"run_year"}{Integer. The year of each CWT recovery.}
#'    \item{"fishery"}{Any type. The identifier for each type of fishery.}
#'    \item{"location"}{Any type. The identifier for location.}
#'    \item{"month"}{Integer. The month of each CWT recovery.}
#'    \item{"rate"}{Double. The release mortality rate for each timestep.}
#'  }
#'
#' @description hook-and-release mortality described in \insertCite{age_struct}{cwtCensus}.
#' @references
#' \insertAllCited{}
"release_mort"




