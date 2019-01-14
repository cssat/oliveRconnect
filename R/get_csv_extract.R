#' GET Oliver csv extracts via authentication object
#'
#' This is a function that will pull a specified PPM extract from the Oliver API
#' for authorized users. The \code{extract_name} parameter must be either
#' \code{fss_raw.csv}, \code{fss_extract.csv}, \code{notes_extract.csv}, or \code{grassroots_diff.csv}.
#'
#' @param extract_name A valid extract name available to the specified user.
#'
#' @return
#' @export
#'
#' @examples

get_csv_extract <- function(extract_name = NA){

  `%not_in%` <- purrr::negate(`%in%`)

  if (!exists("authentication_objects")) {
    stop(
      "Cannot find authentication_objects. Run post_auth_to_oliver()?"
    )
  }

possible_extracts <- c("fss_raw.csv"
                        ,"fss_extract.csv"
                        ,"notes_extract.csv"
                        ,"grassroots_diff.csv"
  )

if (is.na(extract_name) | extract_name %not_in% possible_extracts) {
  stop(
    "Unknown extract_name specified."
  )
}


# Get csv extract
path <- paste0("api/ppm/", extract_name)
api_url <- httr::modify_url(authentication_objects$base
                            ,path = path)

httr::GET(api_url) %>%
  httr::content()

}
