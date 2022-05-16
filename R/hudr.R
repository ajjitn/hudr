


#' check_args
#' @keywords internal
#' @import data.table
check_args <- function(pass, argument, label) {
  if (any(!is.character(argument))) {
    pass[] <- paste0(label, ": Must be a character.")
  } else {
    if (any(is.na(argument))) {
      pass[] <- paste0(label, ": Must be a character string specifying an ", label, ".")
    } else {
      if (length(argument) != 1) {
        pass[] <- paste0(label, ": Must be of length 1.")
      }
    }
  }
  return(pass)
}

#' get_fmrdata_info
#' @keywords internal
#' @import data.table
get_fmrdata_info <- function(X) {
  lst <- list("data_info" = NULL, "basicdata" = NULL)
  lst[["data_info"]] <- data.table("county_name" = X$county_name,
                                   "counties_msa" = X$counties_msa,
                                   "town_name" = X$town_name,
                                   "metro_status" = X$metro_status,
                                   "metro_name" = X$metro_name,
                                   "area_name" = X$area_name,
                                   "smallarea_status" = X$smallarea_status)
  lst[["basicdata"]] <- X[["basicdata"]]
  return(lst)
}

#' Get HUD Fair Market Rent data at the County or MSA Level from HUD.
#'
#' Provides fair market rent data at the County and MSA level from HUD.
#' @author Paul Richardson
#' @export
#' @import httr magrittr data.table janitor
#' @name get_hud_fmr_data
#' @param entityid Character string containing the 'fips_code' from get_hud_fmr_listcounties(...) or 'cbsa_code' from get_hud_fmr_listmetros(...).
#' @param yr Character string indicating the year.
#' @param hud_key Character string indicating your API key from HUD.
#' @details Provides fair market rent data at the County and MSA level from HUD.
#' @return A list of data tables containing fair market rent data from HUD.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_key <- Sys.getenv("HUD_API_KEY")
#'
#' cbsa_codes <- get_hud_fmr_listmetros(hud_key = hud_key)
#' cnty_codes <- get_hud_fmr_listcounties(hud_key = hud_key)
#'
#' fmr_msa_dt <- get_hud_fmr_data(entityid = cbsa_codes$cbsa_code[1],
#'                                yr = "2020",
#'                                hud_key = hud_key)
#' fmr_cnty_dt <- get_hud_fmr_data(entityid = cnty_codes$fips_code[1],
#'                                 yr = "2020",
#'                                 hud_key = hud_key)
#' }
get_hud_fmr_data <- function(entityid = "METRO10180M10180", yr, hud_key) {
  pass <- "PASS"
  pass[] <- check_args(pass = pass, argument = entityid, label = "entityid")
  pass[] <- check_args(pass = pass, argument = yr, label = "yr")
  if (pass != "PASS") {
    stop(pass)
  }
  dta <- tryCatch(expr = {
    url <- paste0("https://www.huduser.gov/hudapi/public/fmr/data/", entityid, "?year=", yr)
    get_response <- httr::GET(url = url, config = add_headers("Authorization" = paste0("Bearer ", hud_key)))
    get_content <- httr::content(x = get_response, simplifyVector = TRUE)[["data"]] %>% get_fmrdata_info() %>%
      lapply(FUN = function(X, entityid, yr) {expra <- parse(text = paste0("c('entityid', 'year') := list('", entityid, "', '", yr, "')")); dta <- as.data.table(x = X)[, eval(expra)]; dta}, entityid = entityid, yr = yr) %>%
      lapply(FUN = janitor::clean_names)

    if (nrow(get_content$basicdata) == 0) {
      stop("")
    }
    get_content
  },
  error = function(e) {c("Error in get_hud_fmr_data(...): No data returned. check arguments.")})
  return(dta)
}

#' Get HUD Fair Market Rent data at the State Level from HUD.
#'
#' Provides fair market rent data at the State level from HUD.
#' @author Paul Richardson
#' @export
#' @import httr magrittr data.table janitor
#' @name get_hud_fmr_statedata
#' @param entityid Character string containing the 'state_code' from get_hud_fmr_liststates(...).
#' @param yr Character string indicating the year.
#' @param hud_key Character string indicating your API key from HUD.
#' @details Provides fair market rent data at the State level from HUD.
#' @return A list of data tables containing fair market rent data from HUD.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_key <- Sys.getenv("HUD_API_KEY")
#'
#' state_codes <- get_hud_fmr_liststates(hud_key = hud_key)
#'
#' fmr_state_dt <- get_hud_fmr_statedata(entityid = state_codes$state_code[1],
#'                                       yr = "2020",
#'                                       hud_key = hud_key)
#' }
get_hud_fmr_statedata <- function(entityid = "AL", yr, hud_key) {
  pass <- "PASS"
  pass[] <- check_args(pass = pass, argument = entityid, label = "entityid")
  pass[] <- check_args(pass = pass, argument = yr, label = "year")
  if (pass != "PASS") {
    stop(pass)
  }
  dta <- tryCatch(expr = {
    url <- paste0("https://www.huduser.gov/hudapi/public/fmr/statedata/", entityid, "?year=", yr)
    get_response <- httr::GET(url = url, config = add_headers("Authorization" = paste0("Bearer ", hud_key)))
    get_content <- httr::content(x = get_response, simplifyVector = TRUE)[["data"]][c("metroareas", "counties")] %>%
      lapply(FUN = function(X, entityid, yr) {expra <- parse(text = paste0("c('state_code', 'year') := list('", entityid, "', '", yr, "')")); dta <- as.data.table(x = X)[, eval(expra)]; dta}, entityid = entityid, yr = yr) %>%
      lapply(FUN = janitor::clean_names)
    if (nrow(get_content$metroareas) == 0) {
      stop("")
    }
    get_content
  },
  error = function(e) {c("Error in get_hud_fmr_data(...): No data returned. check arguments.")})
  return(dta)
}

#' get_ildata_info
#' @keywords internal
#' @import data.table
get_ildata_info <- function(X) {
  lst <- list("data_info" = NULL, "low" = NULL, "very_low" = NULL, "extremely_low" = NULL)
  lst[["data_info"]] <- data.table("county_name" = X$county_name,
                                   "counties_msa" = X$counties_msa,
                                   "town_name" = X$town_name,
                                   "metro_status" = X$metro_status,
                                   "metro_name" = X$metro_name,
                                   "area_name" = X$area_name,
                                   "year" = X$year,
                                   "median_income" = X$median_income)
  lst[c("low", "very_low", "extremely_low")] <- X[c("low", "very_low", "extremely_low")]
  return(lst)
}

#' Get HUD Income Limit data at the County or MSA Level from HUD.
#'
#' Provides income limit data at the County and MSA level from HUD.
#' @author Paul Richardson
#' @export
#' @import httr magrittr data.table janitor
#' @name get_hud_il_data
#' @param entityid Character string containing the 'fips_code' from get_hud_fmr_listcounties(...) or 'cbsa_code' from get_hud_fmr_listmetros(...).
#' @param yr Character string indicating the year.
#' @param hud_key Character string indicating your API key from HUD.
#' @details Provides income limit data at the County and MSA level from HUD.
#' @return A list of data tables containing income limit data from HUD.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_key <- Sys.getenv("HUD_API_KEY")
#'
#' cbsa_codes <- get_hud_fmr_listmetros(hud_key = hud_key)
#' cnty_codes <- get_hud_fmr_listcounties(hud_key = hud_key)
#'
#' il_msa_dt <- get_hud_il_data(entityid = cbsa_codes$cbsa_code[1],
#'                              yr = "2020",
#'                              hud_key = hud_key)
#' il_cnty_dt <- get_hud_il_data(entityid = cnty_codes$fips_code[1],
#'                               yr = "2020",
#'                               hud_key = hud_key)
#' }
get_hud_il_data <- function(entityid = "0100199999", yr, hud_key) {
  pass <- "PASS"
  pass[] <- check_args(pass = pass, argument = entityid, label = "entityid")
  pass[] <- check_args(pass = pass, argument = yr, label = "year")
  if (pass != "PASS") {
    stop(pass)
  }
  dta <- tryCatch(expr = {
    url <- paste0("https://www.huduser.gov/hudapi/public/il/data/", entityid, "?year=", yr)
    get_response <- httr::GET(url = url, config = add_headers("Authorization" = paste0("Bearer ", hud_key)))
    get_content <- httr::content(x = get_response, simplifyVector = TRUE)[["data"]] %>% get_ildata_info() %>%
      lapply(FUN = function(X, entityid, yr) {expra <- parse(text = paste0("c('entityid', 'year') := list('", entityid, "', '", yr, "')")); dta <- as.data.table(x = X)[, eval(expra)]; dta}, entityid = entityid, yr = yr) %>%
      lapply(FUN = janitor::clean_names)
    if (nrow(get_content$low) == 0) {
      stop("")
    }
    get_content
  },
  error = function(e) {c("Error in get_hud_fmr_data(...): No data returned. check arguments.")})
  return(dta)
}

#' get_ilstatedata_info
#' @keywords internal
#' @import data.table
get_ilstatedata_info <- function(X) {
  X[["data_info"]] <- data.table("year" = X$year,
                                 "statecode" = X$statecode,
                                 "stateid" = X$stateID,
                                 "median_income" = X$median_income)
  return(X[c("data_info", "low", "very_low", "extremely_low")])
}

#' Get HUD Income Limit data at the State Level from HUD.
#'
#' Provides income limit data at the State level from HUD.
#' @author Paul Richardson
#' @export
#' @import httr magrittr data.table janitor
#' @name get_hud_il_statedata
#' @param entityid Character string containing the 'state_code' from get_hud_fmr_liststates(...).
#' @param yr Character string indicating the year.
#' @param hud_key Character string indicating your API key from HUD.
#' @details Provides income limit data at the State level from HUD.
#' @return A list of data tables containing income limit data from HUD.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_key <- Sys.getenv("HUD_API_KEY")
#'
#' state_codes <- get_hud_fmr_liststates(hud_key = hud_key)
#'
#' il_state_dt <- get_hud_il_statedata(entityid = state_codes$state_code[1],
#'                                     yr = "2020",
#'                                     hud_key = hud_key)
#' }
get_hud_il_statedata <- function(entityid = "AL", yr, hud_key) {
  pass <- "PASS"
  pass[] <- check_args(pass = pass, argument = entityid, label = "entityid")
  pass[] <- check_args(pass = pass, argument = yr, label = "year")
  if (pass != "PASS") {
    stop(pass)
  }
  dta <- tryCatch(expr = {
    url <- paste0("https://www.huduser.gov/hudapi/public/il/statedata/", entityid, "?year=", yr)
    get_response <- httr::GET(url = url, config = add_headers("Authorization" = paste0("Bearer ", hud_key)))
    get_content <- httr::content(x = get_response, simplifyVector = TRUE)[["data"]] %>% get_ilstatedata_info() %>%
      lapply(FUN = function(X, entityid, yr) {expra <- parse(text = paste0("c('state_code', 'year') := list('", entityid, "', '", yr, "')")); dta <- as.data.table(x = X)[, eval(expra)]; dta}, entityid = entityid, yr = yr) %>%
      lapply(FUN = janitor::clean_names)
    if (nrow(get_content$low) == 0) {
      stop("")
    }
    get_content
  },
  error = function(e) {c("Error in get_hud_fmr_data(...): No data returned. check arguments.")})
  return(dta)
}


#' Get HUD List of County Codes data.
#'
#' Provides list of county codes data from HUD.
#' @author Paul Richardson
#' @export
#' @import httr magrittr data.table janitor
#' @name get_hud_fmr_listcounties
#' @param stateid Character string indicating the state.
#' @param hud_key Character string indicating your API key from HUD.
#' @details Provides list of county codes data from HUD.
#' @return A data table containing a list of county codes from HUD.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_key <- Sys.getenv("HUD_API_KEY")
#'
#' state_codes <- get_hud_fmr_listcounties(hud_key = hud_key)
#' }
get_hud_fmr_listcounties <- function(stateid = "AL", hud_key) {
  pass <- "PASS"
  pass[] <- check_args(pass = pass, argument = stateid, label = "stateid")
  if (pass != "PASS") {
    stop(pass)
  }
  dta <- tryCatch(expr = {
    get_content <- httr::GET(url = paste0("https://www.huduser.gov/hudapi/public/fmr/listCounties/", stateid),
                              config = add_headers("Authorization" = paste0("Bearer ", hud_key))) %>%
      httr::content(simplifyVector = TRUE) %>% data.table::as.data.table()
    get_content
  },
  error = function(e) {c("Error in get_hud_fmr_listcounties(...): No data returned. check arguments.")})
  return(dta)
}



#' Get HUD List of State Codes data.
#'
#' Provides list of state codes data from HUD.
#' @author Paul Richardson
#' @export
#' @import httr magrittr data.table janitor
#' @name get_hud_fmr_liststates
#' @param hud_key Character string indicating your API key from HUD.
#' @details Provides list of state codes data from HUD.
#' @return A data table containing a list of state codes from HUD.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_key <- Sys.getenv("HUD_API_KEY")
#'
#' state_codes <- get_hud_fmr_liststates(hud_key = hud_key)
#' }
get_hud_fmr_liststates <- function(hud_key) {
  dta <- tryCatch(expr = {
    get_content <- httr::GET(url = "https://www.huduser.gov/hudapi/public/fmr/listStates",
                             config = add_headers("Authorization" = paste0("Bearer ", hud_key))) %>%
      httr::content(simplifyVector = TRUE) %>% data.table::as.data.table()
    get_content
  },
  error = function(e) {c("Error in get_hud_fmr_listcounties(...): No data returned. check arguments.")})
  return(dta)
}

#' Get HUD List of MSA Codes data.
#'
#' Provides list of MSA codes data from HUD.
#' @author Paul Richardson
#' @export
#' @import httr magrittr data.table janitor
#' @name get_hud_fmr_listmetros
#' @param hud_key Character string indicating your API key from HUD.
#' @details Provides list of MSA codes from HUD.
#' @return A data table containing a list of MSA codes from HUD.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_key <- Sys.getenv("HUD_API_KEY")
#'
#' cbsa_codes <- get_hud_fmr_listmetros(hud_key = hud_key)
#' }
get_hud_fmr_listmetros <- function(hud_key) {
  dta <- tryCatch(expr = {
    get_content <- httr::GET(url = "https://www.huduser.gov/hudapi/public/fmr/listMetroAreas",
                             config = add_headers("Authorization" = paste0("Bearer ", hud_key))) %>%
      httr::content(simplifyVector = TRUE) %>% data.table::as.data.table()
    get_content
  },
  error = function(e) {c("Error in get_hud_fmr_listcounties(...): No data returned. check arguments.")})
  return(dta)
}

