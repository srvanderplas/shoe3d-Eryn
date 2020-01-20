#' @name shoe_coord
#' @export
#'
#' @title  Single Shoe coordinates of vertext
#'
#' @description This fuction turns a mesh object into a matrix of coordinates
#'
#' @param shoe the mesh object of a shoe from the data set
#' @param verts the number of edges to each vertex (if na does full shoe) to filter out of the shoe
#'
#'
#' @return data.frame
#'
#'
#' @importFrom geometry convhulln
#' @importFrom Arothron trasf.mesh bary.mesh
#' @import dplyr
#' @importFrom tidyr gather spread
#' @importFrom tibble tibble as_tibble
#' @importFrom magrittr extract2 set_colnames
#' @import assertthat

shoe_coord <- function(shoe, verts = NULL) {
  # check imputs
  assertthat::not_empty(shoe)
  assertthat::assert_that(class(shoe) == "mesh3d")
  assertthat::is.number(verts)





  if (!is.null(verts)) {
    vert <- as.data.frame(t(shoe$it)) %>%
      mutate(triangle_id = 1:n()) %>%
      tidyr::gather(-triangle_id, key = "vertex", value = idx) %>%
      group_by(idx) %>%
      dplyr::filter(n() <= verts) %>%
      ungroup(idx) %>%
      tidyr::spread(key = "vertex", value = idx) %>%
      dplyr::filter(!is.na(V1) & !is.na(V2) & !is.na(V3))
  }


  if (is.null(verts)) {
    vert <- as.data.frame(t(shoe$it)) %>%
      mutate(triangle_id = 1:n()) %>%
      tidyr::gather(-triangle_id, key = "vertex", value = idx) %>%
      group_by(idx) %>%
      ungroup(idx) %>%
      tidyr::spread(key = "vertex", value = idx) %>%
      dplyr::filter(!is.na(V1) & !is.na(V2) & !is.na(V3))
  }
  # checking
  assertthat::not_empty(verts)


  vert_long <- vert %>%
    select(-triangle_id) %>%
    tidyr::gather(key = V, value = idx) %>%
    select(-V) %>%
    unique()
  vert_coords <- shoe %>%
    magrittr::extract2("vb") %>%
    t() %>%
    magrittr::set_colnames(c("x", "y", "z", "idk")) %>%
    as_tibble() %>%
    mutate(idx = 1:n())


  x <- left_join(vert_long, vert_coords) %>%
    select(-idk) %>%
    select(-idx)

  hul <- geometry::convhulln(x,
    output.options = c("p", "Fx"),
    return.non.triangulated.facets = FALSE
  )



  hull <- as.vector(hul) %>%
    unique() %>%
    as.data.frame()
  colnames(hull) <- "idx"

  edge_coords <- vert_coords %>%
    dplyr::filter(idx %in% hull$idx) %>%
    select(-idk) %>%
    select(-idx) %>%
    as.matrix()

  assertthat::assert_that(class(edge_coords) == "matrix")

  return(edge_coords)
}
