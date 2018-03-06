
#' Validate a mailing address
#'
#' @description validate a mailing address against the USPS database. Not yet vectorized. Needs to support returning a single character string (or some such)
#'
#' @param username your USPS username. Sign up at https://registration.shippingapis.com/
#' @param aptnum optional apartment number, ie "Apt 38"
#' @param street required street number and name, eg. "123 Easy St"
#' @param city required city name, eg. "Decatur"
#' @param st  required state name, eg. "IL"
#' @param zip optional 5 digit zip code, eg "12345"
#' @param zip4 optional 4 digit zip code addition, eg. "1234"
#' @param simplify a logical value determining whether the results should be returned as a simple character vector or, if FALSE, a multicolum data frame
#'
#' @return a data frame with friendly column names, including addr1 (street address), city, st, zip and zip4
#' @export
#'
usps_validate  <- function(username, aptnum = NULL, street, city, st, zip = NULL, zip4 = NULL, simplify = T) {

   rowid       <- 1:max(c(length(aptnum), length(street), length(city), length(st), length(zip), length(zip4)) )
   batchnumber <- floor((rowid-1)/5)
   seqnumber   <- (rowid -1) %% 5

   addr1  <- paste0("<Address1>", aptnum, "</Address1>")
   street <- paste0("<Address2>", street, "</Address2>")
   city   <- paste0("<City>"    , city  , "</City>")
   st     <- paste0("<State>"   , st    , "</State>")
   zip    <- paste0("<Zip5>"    , zip    ,"</Zip5>")
   zip4   <- paste0("<Zip4>"    , zip4   ,"</Zip4>")

   thexml <- paste0(
    '<Address ID="', seqnumber, '">'
           , addr1
           , street
           , city
           , st
           , zip
           , zip4
       , '</Address>'
   )




   results  <- dplyr::bind_rows(
     lapply(unique(batchnumber), function(batch){

       i <- batchnumber == batch

       queryurl  <- paste0(
         'http://production.shippingapis.com/ShippingAPITest.dll?API=Verify&XML=<AddressValidateRequest USERID="'
         , username, '">'
         , paste0(thexml[i], collapse = "")
         , '</AddressValidateRequest>'
       )

       message(queryurl, "\n")

       # you're already URLencoding it in query_usps, shouldn't you take this out of here?
       results  <- query_usps(URLencode(queryurl))

       message(results)

       results

      })
     )

   if( simplify ) {

     fulladdr  <-  results %>% tidyr::unite("fulladdress", colnames(results), sep = " ")
     gsub("\\s*NA\\s*" , "", fulladdr$fulladdress)

   } else {
     results
   }


}


