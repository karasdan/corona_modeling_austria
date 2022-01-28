library(osmdata)
library(tidyverse)
library(sf)

q <- opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = 'highway', value = 'motorway')

head (available_features ())

bb <- getbb("aut", format_out = "data.frame")
bb <- getbb ('london uk', format_out = 'data.frame')


test <- opq(bbox = "Melk, Austria") %>%
  add_osm_feature(key = "highway")

melk <- osmdata_sf(test)
melk_xlm <- osmdata_xml(test)
melk_sc <- osmdata_sc(test)


test2 <- melk$osm_lines

ggplot(test2)+
  geom_sf()+
  theme_void()

ggplot(coordinates_mumicipality) +
  geom_polygon(aes(X,Y, fill = name), show.legend = FALSE) +
  geom_sf(data = test2)
  #geom_line(data = melk_sc$vertex, aes(x_,y_))


ggplot(melk_sc$vertex) +
  aes(x_, y_) +
  geom_point()

# Test getting data from district melk
test <- opq_osm_id(id = 114629, type = "way")

melk_sc <- osmdata_sc(test)


# Find Boundariesnumber for austria (Gemeindeebene): https://osm-boundaries.com/Map -> 8 is Gemeinde ebene,
# 6 ist Bezirksebene

melk_bbox <- getbb("Bezirk Melk, austria", format_out = "polygon")

melk_boundary <- opq(melk_bbox) %>% 
  add_osm_feature(key = "admin_level", value = "8") %>% 
  osmdata_sf()

melk_polys <- melk_boundary$osm_multipolygons

melk_polys <- melk_polys %>%
  filter(grepl("315", ref.at.gkz) == TRUE)

#remove digits from any distirct name 
melk_polys$name <- gsub('[[:digit:]]+', '', melk_polys$name)
#remove . from any district name
melk_polys$name <- gsub("[.]", '', melk_polys$name)
#trim whitespace
melk_polys$name  <- trimws(melk_polys$name, "both")
#factorize 
melk_polys$name <- as.factor(melk_polys$name)

melk_polys <- melk_polys %>%
  select(osm_id, name, ref.at.gkz, type, geometry)

melk_polys$poly_area <- st_area(melk_polys)

melk_road <- opq(melk_bbox) %>%
  add_osm_features (features = c ("\"highway\"=\"motorway\"",
                                  #"\"highway\"=\"trunk\"",
                                  "\"highway\"=\"primary\"",
                                  "\"highway\"=\"secondary\"",
                                  "\"highway\"=\"tertiary\"",
                                  "\"highway\"=\"unclassified\"")) %>%
  osmdata_sf()

melk_lines <- melk_road$osm_lines

melk_lines <- melk_lines %>%
  select(osm_id, highway, geometry)

ggplot() +
  geom_sf(data = melk_polys) +
  geom_sf(data = melk_lines, aes(color = highway))

# Filter nach jene Straßen die innerhalb sind
test <- st_join(melk_lines, melk_polys, join = st_within)

test <- test %>%
  filter(! is.na(osm_id.y))

ggplot() +
  geom_sf(data = melk_polys) +
  geom_sf(data = test, aes(color = name), show.legend = FALSE)

#-------------------------------------------------------------------
# Test finding shortest driving distance between two municipality
melk_bbox <- getbb("Bezirk Melk, austria", format_out = "polygon")

melk_boundary <- opq(melk_bbox) %>% 
  add_osm_feature(key = "admin_level", value = "8") %>% 
  osmdata_sf()

melk_polys <- melk_boundary$osm_multipolygons

melk_polys <- melk_polys %>%
  filter(grepl("315", ref.at.gkz) == TRUE)

#remove digits from any distirct name 
melk_polys$name <- gsub('[[:digit:]]+', '', melk_polys$name)
#remove . from any district name
melk_polys$name <- gsub("[.]", '', melk_polys$name)
#trim whitespace
melk_polys$name  <- trimws(melk_polys$name, "both")
#factorize 
melk_polys$name <- as.factor(melk_polys$name)

melk_polys <- melk_polys %>%
  select(osm_id, name, ref.at.gkz, type, geometry)

melk_polys$poly_area <- st_area(melk_polys)

centre <- st_centroid(melk_polys)

# ggplot() +
#   geom_sf(data = melk_polys) +
#   geom_sf(data = centre)

melk_road <- opq(melk_bbox) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

melk_lines <- melk_road$osm_lines

melk_lines <- melk_lines %>%
  select(osm_id, highway, geometry)

# ggplot() +
#   geom_sf(data = melk_polys, aes(color = name), show.legend = FALSE) +
#   geom_sf(data = melk_lines)

# Filter nach jene Straßen die innerhalb sind
melk_lines <- st_join(melk_lines, melk_polys, join = st_within)

melk_lines <- melk_lines %>%
  filter(! is.na(osm_id.y))

# ggplot() +
#   geom_sf(data = melk_polys) +
#   geom_sf(data = melk_lines, aes(color = name), show.legend = FALSE) +
#   geom_sf(data = centre)

g <- dodgr::weight_streetnet(melk_lines, wt_profile = "motorcar")

from <- st_coordinates(centre)
to <- st_coordinates(centre)
d <- dodgr::dodgr_dists(graph = g, from = from, to = to)

df <- as.data.frame(d)

