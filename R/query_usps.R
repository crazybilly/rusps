#' Query USPS API
#'
#' @param queryurl a semi-human readable query URL to the USPS API
#'
#' @import xml2
#' @return a data frame with the columns
#'
query_usps  <- function(queryurl) {

  queryurl  <- gsub("#", "", queryurl)


  encodedurl   <- URLencode(queryurl)

  queryresults  <- xml2::read_xml(encodedurl) %>%
    xml2::xml_children() %>%
    purrr::map_dfr(
      ~xml_children(.x) %>%
        purrr::map_dfc(~  data_frame(xml_text(.x)) %>% setNames(xml_name(.x)) , .null = data_frame(address1 = NA))
    )

  queryresults %>%
    setNames(tolower(names(queryresults))) %>%
    dplyr::rename_at( dplyr::vars(contains("address2")),  dplyr::funs(sub("address2", "addr1", .)) ) %>%
    dplyr::rename_at( dplyr::vars(contains("address1")),  dplyr::funs(sub("address2", "addr2", .)) ) %>%
    dplyr::rename_at( dplyr::vars(contains("zip5"    )),  dplyr::funs(sub("zip5"    , "zip"  , .)) )




}
