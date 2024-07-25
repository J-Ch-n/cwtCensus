#' Release information
#'
#' @format ## 'release'
#'
#' \describe{
#'  \item{release_month}{The month in which the CWTs are released}
#'  \item{brood_year}{The birth year of the tagged fish}
#'  \item{tag_code}{The unique identification for each batch of CWT.}
#'  \item{prod_exp}{The production expansion rate for each batch of CWT.}
#'  \item{total_release}{The total number of fish released under a specific CWT ID}
#' }
#'
"release"

#' Recovery information
#'
#' @format ## 'release'
#'
#' \describe{
#' \item{run_year}{The year in which the CWT is recovered}
#' \item{recovery_id}{The recovery ID of the CWT}
#' \item{fishery}{The index of the fishery from which the CWT is recovered}
#' \item{tag_code}{The unique identification for each batch of CWT}
#' \item{length}{The length of tagged fish in inches}
#' \item{sex}{The biological sex of the tagged fish}
#' \item{month}{The month in which the CWT is recovered}
#' \item{location}{The code of the location from which the CWT is recovered}
#' \item{size_limit}{The minimum size limit for fish in inches at the time of recovery}
#' \item{est_num}{The estimated number of fish that each CWT represents.}
#' }
#'
"recovery"

#' Natural mortality information
#'
#' @format ## 'release'
#'
#' \describe{
#' \item{age}{The age of fish in which the CWT is recovered}
#' \item{month}{The month in which the CWTs are released}
#' \item{rate}{The natural mortality rate for the specified age and month}
#' }
#'
"nat_mort_default"

#' Length at age, or key length age, information
#'
#' @format ## 'release'
#'
#' \describe{
#' \item{age}{The age of fish in which the CWT is recovered}
#' \item{month}{The month in which the CWT is recovered}
#' \item{mean}{The mean legnth of fish in inches for the specified age and month}
#' \item{sd}{The standard deviation of the fish length in inches for the specified age and month}
#' }
#'
"length_at_age"

#' Release mortality information
#'
#' @format ## 'release'
#'
#' \describe{
#' \item{run_year}{The year in which the CWT is recovered}
#' \item{fishery}{The index of the fishery from which the CWT is recovered}
#' \item{location}{The code of the location from which the CWT is recovered}
#' \item{month}{The month in which the CWT is recovered}
#' \item{rate}{The release mortality rate for the specified location and time}
#' }
#'
"release_mort"




