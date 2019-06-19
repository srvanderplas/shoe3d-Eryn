#' @name shoe_grab
#' @export mesh3d
#'
#' @title  Single Shoe Extractor
#'
#' @description This fuction creates a mesh object of a specified shoe from the data.raw folder
#'
#' @param id the shoe id in the form of number followed by R or L to signify the shoe in question, when imputting this make sure to include ""
#' @param scandate the date at which the shoe was scanned in the form of yearmonthday, when imputting this make sure to include ""
#'
#' @return a triangle mesh object ( a large mesh3d )
#'
#'
#' @importFrom Rvcg
#' @importFrom tidyverse






shoe_grab<- function(shoeid,scandate){
  #function input checks


  #setting up the file path to the data. This assumes that the file you want is in the folder labled data-raw
  #this will only work on study shoes
stl_files <- list.files("./data-raw", pattern = ".stl", full.names = T)

  #grabbing a single shoe
shoe_paths <- stl_files[str_detect(stl_files, shoeid)]
shoe_path<-shoe_paths[str_detect(shoe_paths, "3_1_1")]
shoe<-shoe_path[str_detect(shoe_path, scandate)]

#turning it into a mesh object
shoe_mesh <- vcgImport(shoe, clean = T)


return(shoe_mesh)

}
