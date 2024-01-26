

#' Get total load from the terna API.
#'
#' @param year Year
#' @param type type.
#' @param token token
#'
#' @export
#'
#'
terna_installed_capacity <- function(year,
                                 type =c('Thermal', 'Wind', 'Geothermal', 'Photovoltaic', 'Self-consumption', 'Hydro'),
                                 token = NULL) {

  if(is.null(token)) token <- terna_get_access_token()

  url <- paste0("https://api.terna.it/transparency/v1.0/getinstalledcapacity?",
                paste0("&year=",paste(year,
                                      collapse = "&year=")),
                paste0("&type=",paste(type,
                                      collapse = "&type=")))

  response <- httr2::request(url) %>%
    httr2::req_headers(Authorization = paste("Bearer", token)) %>%
    httr2::req_perform()

  content <-
    response %>%
    httr2::resp_body_string()

  cont_df <- jsonlite::fromJSON(content)

  cont_df <- tibble::as_tibble( cont_df$installedCapacity )

  names(cont_df) <- tolower(names(cont_df))

  cont_df$installed_capacity_gwh <- as.numeric(cont_df$installed_capacity_gwh)

  cont_df
}


