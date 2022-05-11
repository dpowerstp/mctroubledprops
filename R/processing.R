
#' Read and process MC Troubled Properties Data
#'
#' Read and process data from Montgomery County's Open data portal on inspections of troubled properties, fixing some addresses that are improperly geocoded. Data from here: https://data.montgomerycountymd.gov/Consumer-Housing/Troubled-Properties-Analysis/bw2r-araf
#'
#' @param apptoken App token from Montgomery County's open-data developer portal to download troubled property data
#'
#' @importFrom magrittr %>%
#'
#' @return Latest-version of Montgomery-County troubled properties dataset, with broken addresses fixed
#' @export
#'
#' @examples
read_fix_mc <- function(apptoken){
  mc_troubledprops <- sf::st_read(glue::glue("https://data.montgomerycountymd.gov/resource/bw2r-araf.geojson?$limit=2000&$offset=0&$$app_token={apptoken}")) %>%
    sf::st_transform(4326) %>%
    dplyr::mutate(inspectedtimely = dplyr::case_when(compliantind == 1 ~ "Yes",
                                       compliantind == 0 ~ "No"),
                  city = gsub("Silever", "Silver", city, ignore.case = T),
           rating = stringr::str_to_title(rating),
           rating = factor(rating, c("Compliant", "At-Risk", "Troubled", "Tbd")))  %>%
    dplyr::mutate(
      state = "MD",
      fulladdress = glue::glue("{streetaddress}, {city}, MD  {zipcode}"),
      geomet_char = as.character(geometry),
      severityindex = as.numeric(severityindex),
      unitcount = as.numeric(unitcount),
      sizedist = dplyr::percent_rank(unitcount))

  # lots addresses coordinaters 0, 0
  mc_troubledprops_nothomes <- mc_troubledprops %>%
    # and problem with 4800 auburn ave.
    dplyr::filter(geomet_char != "c(0, 0)" & !grepl("4800 auburn ave.", fulladdress, ignore.case = T))

  mc_troubledprops_homes <- mc_troubledprops %>%
    # filter(grepl("Homes on quaker", communityname, ignore.case = T))
    dplyr::filter(geomet_char == "c(0, 0)" | grepl("4800 auburn ave.", fulladdress, ignore.case = T))

  homesaddress <- tidygeocoder::geo(mc_troubledprops_homes$fulladdress %>% unique())

  still_missing <- homesaddress %>%
    dplyr::filter(is.na(lat))

  still_missing_regeocode <- tidygeocoder::geo(still_missing$address, method = "census")

  missing_regeocode <- still_missing_regeocode %>%
    dplyr::filter(is.na(lat))

  not_missing_regeocode <- still_missing_regeocode %>%
    dplyr::filter(!is.na(lat))

  not_missing_origgeocode <- homesaddress%>%
    dplyr::filter(!is.na(lat))

  not_missing <- rbind(not_missing_regeocode, not_missing_origgeocode)

  address_notmissing <- sf::st_drop_geometry(mc_troubledprops_homes) %>%
    dplyr::right_join(not_missing, by = c("fulladdress" = "address")) %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

  # sf::st_drop_geometry(mc_troubledprops_homes) %>%
  #   dplyr::anti_join(not_missing, by = c("fulladdress" = "address"))

  # final address - correctly-geocoded joined to ungeocoded
  mc_troubledprops <- mc_troubledprops_nothomes %>%
    sf::st_transform(4326) %>%
    rbind(address_notmissing)

  mc_troubledprops

}
