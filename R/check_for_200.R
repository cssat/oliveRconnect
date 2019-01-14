#' Check for error messages on non-200 responses
#'
#' @param response A response from a POST call to one of the oliver Sign-in Domains.
#'
#' @return
#' @export
#'
#' @examples
check_for_200 <- function(response) {

  if (httr::status_code(response) != 200) {
    stop(
      sprintf(
        "Oliver API request failed [%s]"
        ,httr::status_code(response)
      ),
      call. = FALSE
    )
  }
}
