#' Define an Oliver subdomain to serve as a base url
#'
#' @param oliver_env A character string representing the desired API environment: 'production', 'usertest', or 'staging'.
#'
#' @return
#' @export
#'
#' @examples
return_subdomain <- function(oliver_env) {
  if (oliver_env == "production") {
    base_url <- "https://www.oliverservices.org/"
  } else if (oliver_env == "staging") {
    base_url <- "https://staging.oliverservices.org/"
  } else if (oliver_env == "usertest") {
    base_url <- "https://usertest.oliverservices.org/"
  } else {
    stop("Please specify an environment variable of 'production', 'usertest', or 'staging'",
         call. = FALSE)
  }
  base_url
}
