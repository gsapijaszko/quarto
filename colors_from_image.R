
# creates the palete of colors taking sample from image
#
i <- imager::load.image("https://upload.wikimedia.org/wikipedia/commons/a/ad/Kaiserstuhl_-_Herbst_-_Rebblatt_im_Gegenlicht.jpg")
# i <- imager::as.cimg(rgb2ban(i, 255))
a <- as.data.frame(i, wide = "c") |>
  subset(select = c("c.1", "c.2", "c.3")) |>
  unique() |>
  dplyr::mutate(color = rgb(c.1, c.2, c.3))
LUV <- colorspace::coords(as(colorspace::sRGB(a$c.1, a$c.2, a$c.3), "LUV"))

a <- a |>
  dplyr::mutate(
    L = LUV[, "L"],
    U = LUV[, "U"],
    V = LUV[, "V"],
    hue2 = atan2(V, U)
  )

# sample length = 15
s <- sample(nrow(a),15)

a <- a[s,] |>
#   dplyr::arrange(dplyr::desc(hue2))
  dplyr::arrange(hue2)

plot(i, xlim = c(-250, imager::width(i)+50))
ggplot2::ggplot(i)
sqh <- floor(imager::height(i) / nrow(a))
for (j in 1:nrow(a)) {
  rect(-220,(j-1)*sqh,-20,j*sqh, col = a[j,"color"][[1]])
}
colorblindcheck::palette_check(a$color, plot = TRUE)
g <- grid::rasterGrob(i)
colorBlindness::cvdPlot(g)

# rgb2ban for image posterization
# from https://statistic-on-air.blogspot.com/2010/11/r-is-cool-image-editor.html
# adopted to imager

rgb2ban <- function(img, n){
  iRed <- img[,,1]*255
  iGreen <- img[,,2]*255
  iBlue <- img[,,3]*255
  
  band_size <- trunc(255/n)
  
  oRed <- band_size * trunc(iRed / band_size)
  oGreen <- band_size * trunc(iGreen / band_size)
  oBlue <- band_size * trunc(iBlue / band_size)
  
  qw <- array( c(oRed/255, oGreen/255, oBlue/255), c(dim(iRed)[1],dim(iRed)[2],3))
  
  suppressWarnings(imager::as.cimg(qw))
}

dd <- imager::as.cimg(rgb2ban(i, 3))
plot(dd, axes = FALSE)
