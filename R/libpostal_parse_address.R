#' Parse Address with libpostal
#' @description parse an address with libpostal. This depends on a running instance of libpostal with a REST API. Using a Docker image of libpostal_rest is recommended. Make sure that your Docker VM has at least 2GB of RAM available.
#'
#' @param serveraddress  the address of a libpostal_rest server parser, by default 192.168.99.100:8080/parser
#' @param address a character vector of addresses to parse. Each address should be a single item
#' @param collapsestreet a logical value indicating whether house number and road name should be collapsed to addr1
#' @param simplify a logical value indicating, if TRUE, that simple character vector should be returned, or if FALSE, a data frame should be returned
#'
#' @details Use `check_libpostal_docker()` to ensure that your docker image is running. If not, you can start it with `start_libpostal_docker()`.
#'
#' @return if simplify = TRUE, a character vector of addresses parsed by libpostal. If simplify = F, a data frame with up to 8 columns including:
#'   * house_number - usually refers to the external (street-facing) building number.
#'   * unit - apartment or suite number
#'   * po_box - post office box: typically found in non-physical (mail-only) addresses
#'   * road - street name(s)
#'   * city - name of the city
#'   * st - state abbreviation
#'   * zip - postal code
#'
#' @export
#'
parse_address  <- function(serveraddress = '192.168.99.100:8080/parser', address, collapsestreet = T, simplify = T) {



  namekey  <- c(
      house_number = "house_number"
    , unit = "unit"
    , po_box = "po_box"
    , road = "road"
    , city = "city"
    , state = 'st'
    , postcode = 'zip'
    , country = "country"
  )



  results  <- prep_query(address) %>%
    map_dfr(~
      httr::POST(serveraddress, body = .x ) %>%
      httr::content("text") %>%
      jsonlite::fromJSON() %>%
      tidyr::spread(label, value)
    )

  # rename columns based on key
  names(results)  <- namekey[names(results)]
  # reorder columns based on key
  renamed  <- results[,namekey[namekey %in% names(results)]]

  if(collapsestreet) {
    streetfix  <- renamed %>%
      mutate(addr1 = paste(house_number, road)) %>%
      select(-house_number, -road) %>%
      select(addr1, everything())
  } else {
    streetfix  <- renamed
  }


  if(simplify) {

    fulladdr  <- streetfix %>%
      tidyr::unite(fulladdress, colnames(streetfix), sep = " ")
    gsub("\\s*NA\\s*" , "", fulladdr$fulladdress)

  } else {
    streetfix
  }



}
