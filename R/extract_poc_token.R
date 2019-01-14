#' Extract token from Oliver sign-in GET response
#'
#' @param response A response from a GET to one of the oliver Sign-in Domains containing a "poc.t" cookie.
#'
#' @return
#' @export
#'
#' @examples
extract_poc_token <- function(response) {

    dat_cookies <- httr::cookies(response)

    dat_cookies[dat_cookies$name == "poc.t","value"]

}
