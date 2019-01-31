#' GET Oliver organization information
#'
#' This is a function that will hit the Oliver organization API and return information
#' related to that organization. At this time, the function only returns the organization
#' name.
#'
#' @param org_ids A valid organization id or vector of ids. Expects integer vectors.
#'
#' @return
#' @export
#'
#' @examples

get_org_info <- function(org_ids = NA){

  get_id_and_name <- function(url) {
    server_response <- httr::GET(url)
    if (server_response %>% httr::status_code() == 200) {
      server_response %>%
        httr::content() %>%
        .[c("id", "name")] %>%
        dplyr::as_data_frame()
    } else {
      dplyr::data_frame(id = NA, name = "Org Not Available")
    }

  }

  if (!exists("authentication_objects")) {
    stop(
      "Cannot find authentication_objects. Run post_auth_to_oliver()?"
    )
  }

  session_info <- authentication_objects$session %>% httr::content()

  org_ids <- ifelse(is.na(org_ids), session_info$Organization$id, org_ids)

  # Get csv extract
  paths <- paste0(authentication_objects$base, "api/organizations/", org_ids)

  org_df <- lapply(paths, get_id_and_name) %>%
    dplyr::bind_rows()

  setNames(org_df$id, org_df$name)

}
