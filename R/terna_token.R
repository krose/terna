
#' Get a token to download data.
#'
#' @param client_id Client id. Defaults to NULL.
#' @param client_secret Client secret. Defaults to NULL.
#'
#' @export
#'
#'
terna_get_access_token <- function(client_id = NULL, client_secret = NULL) {

  if(is.null(client_id)) client_id = Sys.getenv("TERNA_CLIENT_ID")
  if(is.null(client_secret)) client_secret = Sys.getenv("TERNA_CLIENT_SECRET")

  url <- "https://api.terna.it/public-api/access-token"

  response <- httr2::request(url) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers('Content-Type' = 'application/x-www-form-urlencoded') %>%
    httr2::req_body_form(
      grant_type = "client_credentials",
      client_id = client_id,
      client_secret = client_secret
    ) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  if (!is.null(response$error)) {
    stop("Error in getting access token: ", response$error)
  }

  Sys.sleep(1)

  return(response$access_token)
}
