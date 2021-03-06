context("autoimage tests")

test <- FALSE

if (test) {
  setwd("~")
  pdf("test-autoimage.pdf")
  data(narccap)
  
  # restructure data for 2 images
  tasmax2 <- tasmax[, , 1:2]
  
  # plot irregularly gridded images with separate legends and usa border
  reset.par()
  autoimage(lon, lat, tasmax2, common.legend = FALSE, map = "usa", main = "usa, h leg")
  
  # plot irregularly gridded images with common legend and world lines
  # customize world lines add and customize title
  reset.par()
  autoimage(lon, lat, tasmax2, map = "world", lines.args = list(col = "white", 
    lwd = 2), outer.title = "with world white lines, v leg, big blue title", 
    mtext.args = list(col = "blue", cex = 2), legend = "v")
  
  # plot irregularly-spaced responses as images with separate legends and
  # county borders.  Add observed data locations with custom point
  # options
  reset.par()
  data(co, package = "gear")
  autoimage(co$lon, co$lat, co[, c("Al", "Ca")], common.legend = FALSE, 
    map = "county", main = c("Aluminum", "Cadmium"), 
    points = list(x = co$lon, y = co$lat), 
    points.args = list(pch = 20, col = "white"), 
    outer.title = "co with points titles")
  
  # plot irregularly-spaced responsed as images with separate legends and
  # county borders.  Add observed data locations with custom point
  # options, and text
  reset.par()
  data(co, package = "gear")
  autoimage(co$lon, co$lat, co[, c("Al", "Ca")], common.legend = FALSE, 
            map = "county", main = c("Aluminum", "Cadmium"), 
            points = list(x = co$lon, y = co$lat), 
            points.args = list(pch = ".", col = "white"), 
            text = list(x = co$lon, y = co$lat),
            text.args = list(col = "orange"),
            outer.title = "co with points, orange text titles")
  
  # customize margins and lratio for large plot also use projection
  # specify manual lines (though in this case it is the same as using map
  # = 'world')
  reset.par()
  data(worldMapEnv, package = "maps")
  worldpoly <- maps::map("world", plot = FALSE)
  par(mar = c(1.1, 4.1, 2.1, 1.1))
  autoimage(lon, lat, tasmax, lines = worldpoly, proj = "bonne", parameters = 40, 
    main = c("day 1", "day 2", "day 3", "day 4", "day 5"), ylab = "", 
    axes = FALSE, lratio = 0.5, outer.title = "multiday, title, w/ proj, no axes, big legend")
  
  reset.par()
  data(worldMapEnv, package = "maps")
  worldpoly <- maps::map("world", plot = FALSE)
  autoimage(lon, lat, tasmax, outer.title = "custom legend, color", col = fields::tim.colors(64), 
    legend.axis.args = list(col.axis = "blue", las = 2), legend = "v", 
    lratio = 0.3)
  
  reset.par()
  ctasmax = tasmax[,,1] - mean(tasmax[,,1])
  zmin = min(ctasmax, na.rm = TRUE)
  zmax = max(ctasmax, na.rm = TRUE)

  nHalf = 10
  Thresh = 0
  
  rc1 = colorRampPalette(colors = c("red", "white"), space="Lab")(nHalf)    
  rc2 = colorRampPalette(colors = c("white", "blue"), space="Lab")(nHalf)
  rampcols = c(rc1, rc2)
  rampcols[c(nHalf, nHalf+1)] = rgb(t(col2rgb("white")), maxColorValue=256)
  rampcols <- rev(rampcols)
  
  rb1 = seq(zmin, Thresh, length.out=nHalf+1)
  rb2 = seq(Thresh, zmax, length.out=nHalf+1)[-1]
  rampbreaks = c(rb1, rb2)
  
  autoimage(lon, lat, ctasmax, col = rampcols,
            main = c("blue/white/red colors, white at zero"))
  reset.par()

  dev.off()
}
