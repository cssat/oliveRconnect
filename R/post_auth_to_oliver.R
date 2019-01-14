#' Authenticate to the Oliver API
#'
#' @param oliver_email An email registered with the Oliver application.
#' @param oliver_password A password associated with `oliver_email`.
#' @param oliver_env The desired API environment.
#'
#' @return
#' @export
#'
#' @examples
post_auth_to_oliver <- function(oliver_email, oliver_password, oliver_env) {

  base_url <- return_subdomain(oliver_env)

  if (is.null(oliver_email) | is.null(oliver_password)) {
    stop("Please specify an email and password registered with www.oliverservices.org",
         call. = FALSE)
  }

  info_url <- httr::modify_url(base_url
                               ,path = "sign-in")

  session_url <- httr::modify_url(base_url
                                  ,path = "api/sessions")

  info_res <- httr::GET(info_url)

  check_for_200(info_res)

  poc_t <- extract_poc_token(info_res)

  session_res <- httr::POST(session_url
                            ,encode = "json"
                            ,query = list(email=oliver_email
                                          ,password=oliver_password)
                            ,httr::add_headers("x-csrf-token" = poc_t))

  check_for_200(session_res)

  authentication_objects <- list(info = info_res
                                 ,session = session_res
                                 ,base = base_url)

  assign("authentication_objects", authentication_objects, envir = .GlobalEnv)

  message("oliver API authentication established")

}
