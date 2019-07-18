







vert<-as.data.frame(t(shoe05_1$it)) %>%
  mutate(triangle_id = 1:n()) %>%
  tidyr::gather(-triangle_id, key = "vertex", value = idx) %>%
  group_by(idx)  %>%
  dplyr::filter(n() <= 6)%>%
  ungroup(idx)%>%
  tidyr::spread(key="vertex", value=idx) %>%
  dplyr::filter(!is.na(V1) & !is.na(V2) & !is.na(V3))

# get vertices in a long df
vert_long <- vert %>%
  select(-triangle_id) %>%
  tidyr::gather(key = V, value = idx) %>%
  select(-V) %>%
  unique()
vert_coords <- shoe05_1 %>%
  magrittr::extract2("vb") %>%
  t() %>%
  magrittr::set_colnames(c("x", "y", "z", "idk")) %>%
  as_tibble() %>%
  mutate(idx = 1:n())
x <- left_join(vert_long, vert_coords)%>% select(-idk)%>% select(-idx)

hul<-geometry::convhulln(x, output.options = c("p", "Fx"),
                          return.non.triangulated.facets = FALSE)
#plotting
shoe_flip_angle <- structure(
  c(-0.999475359916687, -0.0319919027388096, -0.0030277690384537,
    0, -0.0320942476391792, 0.99845552444458, 0.0450587198138237,
    0, 0.0015826690942049, 0.0451280698180199, -0.998970746994019,
    0, 0, 0, 0, 1), .Dim = c(4L, 4L))
rgl::open3d()
rgl::points3d(hul, aspect = "iso", col = "red")
rgl::view3d(userMatrix = shoe_flip_angle, zoom = .5)

#just for fun
rgl::open3d()
rgl::triangle3d(hul, aspect = "iso", col = "red")
rgl::view3d(userMatrix = shoe_flip_angle, zoom = .5)

#pulling out indicies

hull<-as.vector(hul)%>%unique()%>%as.data.frame()
colnames(hull) <- "idx"

#joining this with the coordinates
edge_coords<-vert_coords%>%
  dplyr::filter(idx %in% hull$idx)%>%
  select(-idk)%>%select(-idx)

#plotting
plotly::plot_ly(edge_coords, x = ~x, y = ~y, z = ~z)









