#' @name shoe_grab
#' @export mesh3d
#'
#' @title  Single Shoe Extractor
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
#' @importFrom Rvcg
#' @importFrom stringr
#' @import assertthat




shoe_grab<- function(shoeid,scandate=NULL, filepath){
  #function input checks


  #setting up the file path to the data.
stl_files <- list.files( filepath, pattern = ".stl", full.names = T)
        #checking to see if the folder containing the data is not empty
          assertthat::not_empty(stl_files)


  #grabbing a single shoe
shoe_paths <- stl_files[str_detect(stl_files, shoeid)]
shoePath<-shoe_paths[str_detect(shoe_paths, "3_1_1")]
shoe<-shoePath[str_detect(shoePath, scandate)]

        #checking to see if the file exists
          assertthat::see_if(file.exists(shoe))


#turning it into a mesh object
shoe_mesh <- Rvcg::vcgImport(shoe, clean = T)
        #Checking that it is a mesh3d object
          assertthat::assert_that(class(shoe_mesh)=="mesh3d")


#returning a mesh3d object
return(shoe_mesh)

}