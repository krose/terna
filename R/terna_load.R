

#' Get total load from the terna API.
#'
#' @param date_from Date from
#' @param date_to Date to
#' @param bidding_zone Bidding zone.
#' @param token token
#'
#' @export
#'
#'
terna_load <- function(date_from, date_to, bidding_zone =c("Centre-North", "Centre-South", "North", "Sardinia", "Sicily", "South", "Calabria", "Italy"), token = NULL) {

  if(is.null(token)) token <- terna_get_access_token()

  date_from <- strftime(date_from, format = "%d/%m/%Y")
  date_to <- strftime(date_to, format = "%d/%m/%Y")

  url <- paste0("https://api.terna.it/transparency/v1.0/gettotalload?dateFrom=", date_from, "&dateTo=", date_to,
                paste0("&biddingZone=",paste(bidding_zone, collapse = "&biddingZone=")))

  response <- httr2::request(url) %>%
    httr2::req_headers(Authorization = paste("Bearer", token)) %>%
    httr2::req_perform()

  content <-
    response %>%
    httr2::resp_body_string()

  cont_df <- jsonlite::fromJSON(content)

  cont_df <- tibble::as_tibble( cont_df$totalLoad )

  names(cont_df) <- tolower(names(cont_df))
  names(cont_df)[1] <- "dt"

  cont_df$dt <- lubridate::ymd_hms(cont_df$dt, tz = "Europe/Rome")
  cont_df$total_load_mw <- as.numeric(cont_df$total_load_mw)
  cont_df$forecast_total_load_mw <- as.numeric(cont_df$forecast_total_load_mw)

  cont_df
}


