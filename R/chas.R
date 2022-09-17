

utils::globalVariables(names = c(".", ":="))


#' Get HUD Comprehensive Housing Affordability Strategy (CHAS) data at the Place, MCD, County, State, National Level from HUD.
#'
#' Provides a data.table containing the entityIds for all of the counties within a given state for the Comprehensive Housing Affordability Strategy (CHAS) data from HUD.
#' @author Paul Richardson
#' @export
#' @import httr magrittr data.table janitor
#' @name get_hud_chas_entityid_list
#' @param stateid Character string containing the 'fips_code' for the State.
#' @param geo_lvl Character string indicating either "county", "MDC", or "city".
#' @param hud_key Character string indicating your API key from HUD.
#' @details Provides fair market rent data at the County and MSA level from HUD.
#' @return A list of data tables containing fair market rent data from HUD.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_key <- Sys.getenv("HUD_API_KEY")
#'
#' chas_cnty_lst_dt <- get_hud_chas_entityid_list(stateid = "53",
#'                                                geo_lvl = "county",
#'                                                hud_key = hud_key)
#' }
get_hud_chas_entityid_list <- function(stateid, geo_lvl, hud_key = Sys.getenv("HUD_API_KEY")) {
  pass <- as.list(environment()) %>%
    lapply(FUN = check_args)
  if (any(pass != "PASS")) {
    stop(paste0(names(pass)[which(pass != "PASS")], ": ", pass[which(pass != "PASS")]))
  }

  dta <- tryCatch(
    expr = {
      endpoint <- switch(EXPR = geo_lvl,
                         "county" = paste0("chas/listCounties/", stateid),
                         "MCD" = paste0("chas/listMCDs/", stateid),
                         "city" = paste0("chas/listCities/", stateid))
      url <- paste0("https://www.huduser.gov/hudapi/public/", endpoint)
      get_response <- httr::GET(url = url, config = httr::add_headers("Authorization" = paste0("Bearer ", hud_key)))
      get_content <- httr::content(x = get_response, simplifyVector = TRUE) %>% data.table::as.data.table() %>%
        .[, c("statecode", "entityId", "countyname") := lapply(X = .SD, FUN = as.character), .SDcols = c("statecode", "entityId", "countyname")] %>%
        .[, c("statecode", "entityId", "countyname") := lapply(X = .SD, FUN = trimws), .SDcols = c("statecode", "entityId", "countyname")] %>%
        janitor::clean_names()

      stopifnot(nrow(get_content) != 0)

      get_content
    },
    error = function(e) {c("Error in get_hud_chas_counties_list(...): No data returned. Check arguments.")})
  return(dta)
}


#' Get HUD Comprehensive Housing Affordability Strategy (CHAS) data at the Place, MCD, County, State, National Level from HUD.
#'
#' Provides Comprehensive Housing Affordability Strategy (CHAS) data at the Place, MCD, County, State, National Level from HUD.
#' @author Paul Richardson
#' @export
#' @import httr magrittr data.table janitor
#' @name get_hud_chas_data
#' @param entityid Character string containing the 'fips_code' from get_hud_fmr_listcounties(...) or 'cbsa_code' from get_hud_fmr_listmetros(...).
#' @param stateid Character string containing the 'fips_code' for the State.
#' @param type Character string indicating the summary level. Option include 1-5. 1 - Nation, 2 - State, 3 - County, 4 - MCD, 5 - Place.
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
#' chas_cnty_dt <- get_hud_chas_data(entityid = "033", stateid = "53",
#'                                   type = "3", yr = "2014-2018",
#'                                   hud_key = hud_key)
#' }
get_hud_chas_data <- function(entityid, stateid, type, yr, hud_key = Sys.getenv("HUD_API_KEY")) {
  pass <- as.list(environment()) %>%
    lapply(FUN = check_args)
  if (any(pass != "PASS")) {
    stop(paste0(names(pass)[which(pass != "PASS")], ": ", pass[which(pass != "PASS")]))
  }

  dta <- tryCatch(expr = {
    url <- paste0("https://www.huduser.gov/hudapi/public/chas?type=", type, "&year=", yr, "&stateId=", as.integer(stateid),"&entityId=", as.integer(entityid))
    get_response <- httr::GET(url = url, config = httr::add_headers("Authorization" = paste0("Bearer ", hud_key)))
    get_content <- httr::content(x = get_response, simplifyVector = TRUE) %>% data.table::as.data.table() %>%
      .[, c("geoname", "sumlevel", "year") := lapply(X = .SD, FUN = as.character), .SDcols = c("geoname", "sumlevel", "year")] %>%
      .[, c("geoname", "sumlevel", "year") := lapply(X = .SD, FUN = trimws), .SDcols = c("geoname", "sumlevel", "year")] %>%
      .[, c(grep(pattern = "geoname|sumlevel|year", x = colnames(x = .), value = TRUE, invert = TRUE)) := lapply(X = .SD, FUN = as.double), .SDcols = grep(pattern = "geoname|sumlevel|year", x = colnames(x = .), value = TRUE, invert = TRUE)] %>%
      data.table::melt.data.table(id.vars = c("geoname", "sumlevel", "year"), variable.name = "variable", variable.factor = FALSE, value.name = "value", value.factor = FALSE) %>%
      janitor::clean_names()

    stopifnot(nrow(get_content) != 0)

    get_content
  },
  error = function(e) {c("Error in get_hud_chas_data(...): No data returned. Check arguments.")})
  return(dta)
}

