#' @name shoe_coord
#' @export dataframe
#'
#' @title  Single Shoe coordinates of vertext
#'
#' @description This fuction turns a mesh object into a dataframe of coordinates
#'
#' @param shoe the mesh object of a shoe from the data set
#' @param verts the number of edges to each vertex (if na does full shoe)
#'
#'
#' @return dataframe
#'
#'
#' @importFrom Rvcg
#' @importFrom
#' @import

shoe_coord<-function(shoe, verts=NULL){

centeredshoe<-Arothron::trasf.mesh(shoe,barycenter = Arothron::bary.mesh(shoe))

#try angles and allign it
if(!is.null(verts)){
vert<-as.data.frame(t(centeredshoe$it)) %>%
  mutate(triangle_id = 1:n()) %>%
  tidyr::gather(-triangle_id, key = "vertex", value = idx) %>%
  group_by(idx)  %>%
  dplyr::filter(n() <= verts)%>%
  ungroup(idx)%>%
  tidyr::spread(key="vertex", value=idx) %>%
  dplyr::filter(!is.na(V1) & !is.na(V2) & !is.na(V3))}

if(is.null(verts))  {
  vert<-as.data.frame(t(centeredshoe$it)) %>%
    mutate(triangle_id = 1:n()) %>%
    tidyr::gather(-triangle_id, key = "vertex", value = idx) %>%
    group_by(idx)  %>%
    ungroup(idx)%>%
    tidyr::spread(key="vertex", value=idx) %>%
    dplyr::filter(!is.na(V1) & !is.na(V2) & !is.na(V3))}

vert_long <- vert %>%
  select(-triangle_id) %>%
  tidyr::gather(key = V, value = idx) %>%
  select(-V) %>%
  unique()
vert_coords <- centeredshoe %>%
  magrittr::extract2("vb") %>%
  t() %>%
  magrittr::set_colnames(c("x", "y", "z", "idk")) %>%
  as_tibble() %>%
  mutate(idx = 1:n())
x <- left_join(vert_long, vert_coords)%>% select(-idk)%>% select(-idx)

hul<-geometry::convhulln(x, output.options = c("p", "Fx"),
                         return.non.triangulated.facets = FALSE)
#flatten shoe here with old code from origional

hull<-as.vector(hul)%>%unique()%>%as.data.frame()
colnames(hull) <- "idx"

edge_coords<-vert_coords%>%
  dplyr::filter(idx %in% hull$idx)%>%
  select(-idk)%>%select(-idx)%>%as.matrix()

return(edge_coords)

}
