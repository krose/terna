

#' Get total load from the terna API.
#'
#' @param date_from Date from
#' @param date_to Date to
#' @param type type.
#' @param token token
#'
#' @export
#'
#'
terna_actual_generation <- function(date_from, date_to,
                                    type =c('Thermal', 'Wind', 'Geothermal', 'Photovoltaic', 'Self-consumption', 'Hydro'),
                                    token = NULL) {

  if(is.null(token)) token <- terna_get_access_token()

  date_from <- strftime(date_from, format = "%d/%m/%Y")
  date_to <- strftime(date_to, format = "%d/%m/%Y")

  url <- paste0("https://api.terna.it/transparency/v1.0/getactualgeneration?dateFrom=", date_from, "&dateTo=", date_to,
                paste0("&type=",paste(type,
                                             collapse = "&type=")))

  response <- httr2::request(url) %>%
    httr2::req_headers(Authorization = paste("Bearer", token)) %>%
    httr2::req_perform()

  content <-
    response %>%
    httr2::resp_body_string()

  cont_df <- jsonlite::fromJSON(content)

  cont_df <- tibble::as_tibble( cont_df$actualGeneration )

  names(cont_df) <- tolower(names(cont_df))
  names(cont_df)[1] <- "dt"

  cont_df$dt <- lubridate::ymd_hms(cont_df$dt, tz = "Europe/Rome")
  cont_df$actual_generation_gwh <- as.numeric(cont_df$actual_generation_gwh)

  cont_df
}


