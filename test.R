osmFile <- "/home/sapi/planet_osm/poland-latest.osm.pbf"
water <- "data/water-polygons-split-4326/water_polygons.shp"

a <- osmextract::oe_read(
  file_path = osmFile, layer = "multipolygons",
  # extra_tags = c("teryt:terc", "wikipedia", "wikidata"),
  query = "SELECT * FROM multipolygons WHERE admin_level = '4' AND teryt_terc <> ''"
)

xmin <- floor(sf::st_bbox(a)[1])
xmax <- ceiling(sf::st_bbox(a)[3])
ymin <- floor(sf::st_bbox(a)[2])
ymax <- ceiling(sf::st_bbox(a)[4])

bbbox <- sf::st_bbox(c(xmin, xmax, ymin, ymax), crs = 4326)

abbox <- sf::st_bbox(a) |>
  sf::st_as_sfc() |>
  sf::st_as_sf()

abbox |>
  sf::st_geometry() |>
  plot()

a |>
  sf::st_geometry() |>
  plot(add = TRUE)



b <- sf::st_read(water, 
  query = paste("SELECT * FROM water_polygons WHERE X between", xmin, "AND", xmax, "AND Y between", ymin, "AND", ymax))

b |>
  sf::st_union() |>
  sf::st_intersection(abbox) |>
  sf::st_geometry() |>
  plot(add = TRUE, col = "steelblue")

bbox <- sf::st_bbox(a) |>
  sf::st_as_sfc()
# osmextract::oe_vectortranslate(file_path = osmFile, layer = "multilinestrings", extra_tags = "natural")
# https://stackoverflow.com/questions/72532869/sf-package-close-a-polygon-fom-complex-shape

coastline <- osmextract::oe_read(
  file_path = osmFile, layer = "lines", 
#   extra_tags = "natural",
  query = "SELECT osm_id, geometry FROM multilinestrings WHERE natural = 'coastline' \
        UNION \
        SELECT osm_id, geometry FROM lines WHERE natural = 'coastline'"
)

c <- coastline |>
  st_crop(y = bbox)

cbox <- sf::st_bbox(c) |>
  sf::st_as_sfc()

polygon <- cbox |>
  lwgeom::st_split(coastline) |>
  sf::st_collection_extract("POLYGON")  

polygon[3] |>
  plot(add = TRUE, col = 'steelblue')

c <- osmextract::oe_read(
  file_path = osmFile, layer = "lines", extra_tags = "ref",
  query = "SELECT ref, geometry FROM 'lines' WHERE highway = 'motorway'"
) 

c <- c |>
  dplyr::mutate(ref = gsub(" ", "", ref)) |>
  dplyr::group_by(ref) |>
  dplyr::summarize()
  
  gsub()

?stringi::stri_replace_all(" ", "", fixed)
  
plot(c$geometry, add = TRUE)

dlns <- a |>
  subset(name == "województwo dolnośląskie")
c |>  
  subset(apply(sf::st_intersects(c, dlns, sparse = FALSE), 1, any)) |>
  plot(add = TRUE, col = c("red", "blue", "green"))
# BDL API GUSu
# Client registered: 09896470-60f5-4801-2b62-08da9adf9202
# Client registered: dc247540-e756-44cf-2b63-08da9adf9202

mydat <- data.frame(
  "name" = c("bob", "bob", "alice", "beth", "patty", "patty", "patty"),
  "fav_color" = c("green", "blue", "red", "orange", "green", "red", "pink")
)
mydat %>%
  group_by(name) %>%
  filter(row_number() <= 2) %>%
  ungroup() %>%
  arrange(name)


?png()


# https://stackoverflow.com/questions/34393884/how-to-get-image-url-property-from-wikidata-item-by-api
# json
# P94 - coats of arms
# P41 - flaga
coa <- jsonlite::fromJSON("https://www.wikidata.org/w/api.php?action=wbgetclaims&property=P41&entity=Q54188&format=json")
img <- coa$claims$P41$mainsnak$datavalue$value

magick::image_read_svg(
  URLencode(paste0("https://commons.wikimedia.org/w/index.php?title=Special:Redirect/file/", img))
)

# where img_name.ext is the name of the image you are looking for.
# 
# The final image URL will be: https://upload.wikimedia.org/wikipedia/commons/a/ab/img_name.ext, where a and b are the first and the second chars of MD5 hashsum of the img_name.ext (with all whitespaces replaced by _).
# 
# Example: For item jaguar (Q35694) the API will returns image name "Junior-Jaguar-Belize-Zoo.jpg", which has MD5 hashsum("Junior-Jaguar-Belize-Zoo.jpg") = 213b31ec141dafebe457e49bcd7f9329, so a=2 and b=1, or the final image URL will be: https://upload.wikimedia.org/wikipedia/commons/2/21/Junior-Jaguar-Belize-Zoo.jpg
# 
# Note: The MD5 hashsum is for the name of the image file, not the P18[0].mainsnak.hash property included in the JSON body.

# przykład mapy
# https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Murrell.pdf


?sf::st_read()


#############
# https://stackoverflow.com/questions/73942768/is-it-possible-to-measure-distances-between-each-and-every-sampling-point-220-l/73948330#73948330

occ <- rgbif::occ_data(
  scientificName = "Calystegia pulchra", 
  country = "GB", 
  hasCoordinate = TRUE
)

occ <- head(occ$data, 220) |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  subset(select = c("key", "scientificName"))

m <- sf::st_distance(occ)
m[1:4, 1:4]

how_much <- function(matrix = m, row = 1, distance = 100000) {
  length(which({{matrix}}[{{row}},] > units::as_units({{distance}}, "m")))
}

how_much(m, 2, 100000)

occ |>
  dplyr::mutate(row_number = dplyr::row_number()) |>
  dplyr::rowwise() |>
  dplyr::mutate(dist_200000 = how_much(m, row_number, 200000))



