library(lidR)
library(osmdata)

dane <- data.frame(godlo = character(),
                   akt_rok = integer(),
                   format = character(),
                   charPrzest = character(),
                   bladSrWys = numeric(),
                   ukladXY = character(),
                   #modulArch = character(),
                   ukladH = character(),
                   #nrZglosz = character(),
                   czy_ark_wypelniony = character(),
                   #daneAktualne = integer(),
                   #lok_nmt = character(),
                   url_do_pobrania = character(),
                   nazwa_pliku = character(),
                   #idNmt = integer(),
                   idSerie = integer(),
                   sha1 = character(),
                   asortyment = character()
)


bb <- osmdata::getbb("gmina Milicz")
gr_adm <- osmdata::opq(bb, timeout = 120) |>
  osmdata::add_osm_features(features = c("\"boundary\"= \"administrative\"")) |>
  osmdata::osmdata_sf()

gr_gminy <- gr_adm$osm_multipolygons |>
  subset(admin_level == "7" & name == "gmina Milicz") |>
  subset(select = "name")

gr_wsi <- gr_adm$osm_multipolygons |>
  subset(admin_level == "8" & name %in% c("Ruda Milicka", "Nowy Zamek", "Grabownica")) |>
  sf::st_join(gr_gminy, join = sf::st_within, left = FALSE) |>
  subset(select = c("name.x"))

for (j in 1:nrow(gr_wsi)) {
  print(gr_wsi[j,])
  dane <- rgugik::DEM_request(gr_wsi[j,]) |>
    subset(product == "PointCloud" & format == "LAZ") |>
    rbind(dane)
}

dane <- unique(dane)
dane
if(!dir.exists("data")) {dir.create("data")}

rgugik::tile_download(dane, outdir = "/home/sapi/projekty/quarto/posts/from_las_to_dtm/data/", method = "wget", extra = "-c --progress=bar:force")


# las catalog ---------------------------------------------------------------------------------


library(lidR)

myPath <- "/home/sapi/projekty/quarto/posts/from_las_to_dtm/data/"
ctg <- readLAScatalog(myPath)
ctg
crs(ctg) <- "EPSG:2180"

gr_wsi |>
  sf::st_transform(crs = "EPSG:2180") |>
  sf::st_geometry() |>
  plot()

plot(ctg, add = TRUE)

ctg@output_options$drivers$SpatRaster$param$overwrite <- TRUE
opt_output_files(ctg) <- paste0(myPath, "/dtm/*")
# opt_chunk_size(ctg) <- 500
# opt_chunk_buffer(ctg) <- 800
# opt_filter(ctg) <- "-keep_class 2 9"
summary(ctg)
rt <- rasterize_terrain(ctg, 1, tin())
rt <- terra::rast("/home/sapi/projekty/quarto/posts/from_las_to_dtm/data/dtm/rasterize_terrain.vrt")

terra::plot(rt)

osmr <- maptiles::get_tiles(gr_wsi[2,], zoom = 16)  |>
  terra::project("EPSG:2180") |>
  terra::plot(xlim = c(388000, 390000), ylim = c(407400, 409400))

terra::plot(rt,
            xlim = c(388000, 390000), 
            ylim = c(407400, 409400), 
            na.color = "transparent",
            add = TRUE)



# nowy terrain z bufforem 800 -----------------------------------------------------------------

# konwertujemy do las, filtrujemy

convertLAZ <- function(lazfile) {
  print(lazfile)
  las <- lidR::readLAS(files = paste0(myPath, "/", {{lazfile}}), filter = "-keep_class 2 9")
  writeLAS(las, file = paste0(myPath, "/las/", stringi::stri_replace_all_fixed({{lazfile}}, "laz", "las")), index = TRUE)
}
f <- list.files(myPath, pattern = "*.laz")
lapply(f, convertLAZ)

myPath <- "/home/sapi/projekty/quarto/posts/from_las_to_dtm/data"
ctg <- readLAScatalog(paste0(myPath, "/las/"))
ctg$filename
crs(ctg) <- "EPSG:2180"

plot(ctg)

ctg@output_options$drivers$SpatRaster$param$overwrite <- TRUE
opt_output_files(ctg) <- paste0(myPath, "/dtm/*")
# opt_chunk_size(ctg) <- 500
opt_chunk_buffer(ctg) <- 30
# opt_filter(ctg) <- "-keep_class 2 9"
summary(ctg)
rt <- rasterize_terrain(ctg, 1, tin())
terra::plot(rt)


opt_output_files(ctg) <- paste0(tempdir(), "/{XCENTER}_{YCENTER}_{ID}")
rois <- clip_circle(ctg, x = 389000, y = 408000, r = 2000)

rois$filename
rt <- rasterize_terrain(rois, 1, tin())
terra::plot(rt)
# for( i in 1:nrow(dane)) {
#   from_file <- paste0("/home/sapi/projekty/stforek/gruszeczka/data/las/", dane[i, "filename"], ".laz")
#   to_file <- paste0("/home/sapi/projekty/quarto/posts/from_las_to_dtm/data/", dane[i, "filename"], ".laz")
#   if(file.exists(from_file)) {
#     print(paste(i, "::", dane[i, "filename"]))
#     file.copy(from = from_file, to = to_file, overwrite = TRUE)
#   }
# }
  