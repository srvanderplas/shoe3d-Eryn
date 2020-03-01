#' @name shoe_grabber
#' @export
#'
#' @title  Single Shoe Extractor for any stl
#'
#' @description This fuction creates a mesh object of a specified shoe from the data.raw folder
#'
#' @param filepath the file path to the shoe
#' @param shoeid the shoe id in the form of number followed by R or L to signify the shoe in question, when imputting this make sure to include ""
#' @param scandate the date at which the shoe was scanned in the form of yearmonthday, when imputting this make sure to include ""
#'
#' @return a triangle mesh object ( a large mesh3d )
#'
#'
#' @importFrom Rvcg vcgImport
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom Morpho berycenter
#' @import assertthat


shoe_grabber <- function(shoeid, scandate, filepath) {
  stl_files <- list.files(filepath, pattern = ".stl", full.names = T)
  # checking to see if the folder containing the data is not empty
  assertthat::not_empty(stl_files)
  ##
  stl_file_path <- stl_new[str_detect(stl_files, shoeid)]
  shoePath <- stl_file_path[stringr::str_detect(stl_file_path, scandate)]
  #checking to make sure the shoe exists
  assertthat::see_if(file.exists(shoePath))
  ###
  shoe <- Rvcg::vcgImport(shoePath, clean = T)
  #checking to make sure the shoe is a mesh object
  assertthat::assert_that(class(shoe) == "mesh3d")

  ##
  centering<-Morpho::barycenter(shoe)%>%colMeans()
  meshshoe<-translate3d(shoe, -centering[1], -centering[2], -centering[3])




  return(meshshoe)
}
