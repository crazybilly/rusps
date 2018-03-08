
#' Prep REST query
#'
#' @description prep a character vector to be a query for a REST API.
#'
#' @param x a character vector
#'
#' @return a vector of queries passable to a libpostal_rest api via POST in the form of `{\"query\": \"123 Easy St, Buffalo NY\"}`
#'
prep_query  <- function(x) {

  paste0('{"query": "', x , '"}')

}





#' Test If Libpostal Is Running
#'
#' @description test to see if libpostal-rest docker image is running on the current machine.
#'
#' @param imagename a regular expression to search for in the image name
#'
#' @details currently Windows only
#' @return a logical value indicating whether libpostal is running or not
#'
#' @export
#'
check_libpostal_docker  <- function(image = "libpostal") {

  dockerlist  <- shell("docker-machine env --shell=powershell | Invoke-Expression; docker container ls", shell = 'powershell', intern = T)
  any(grepl(image, dockerlist))

}



#' Start Libpostal Docker Instance
#'
#' @description starts a docker instance on the current machine. Currently Windows only.
#'
#' @param dockername the name of a libpostal-rest docker instance
#'
#' @return
#' @export
#'
start_libpostal_docker  <- function(dockername = 'amazing_curran') {

  command  <- paste("docker-machine env --shell=powershell | Invoke-Expression; docker start", dockername)

  shell(command, shell = 'powershell')

}
