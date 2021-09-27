
#################################################################################
############ HELPERS ##########################################################
############################################################################

# Links
getlinks <- function(x, df, xid, dfid, dfvar){
  dots <- sf::st_centroid(x = sf::st_geometry(x),of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON")))
  x2 <- data.frame(id = x[[xid]],
                   sf::st_coordinates(sf::st_centroid(x = sf::st_geometry(x),
                                                      of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON")))))
  df <- df[, c(dfid,dfvar)]
  colnames(df) <- c("i","j","fij")
  df <- df[!df$i == df$j,]
  
  link <- merge(df, x2, by.x = dfid[2], by.y = "id", all.x = TRUE)
  link <- merge(link, x2, by.x = dfid[1], by.y = "id", all.x = TRUE)
  names(link)[4:7] <- c("xj", "yj", "xi", "yi")
  link <- link[link$i != link$j,]
  link$ang <- atan2(link$yj - link$yi, link$xj - link$xi) * 180 / pi
  link$dist <- sqrt ((link$xj - link$xi) ^ 2 + (link$yj - link$yi) ^ 2)
  stringo <- paste0("LINESTRING(", link$xi, " ", link$yi, ", ",link$xj, " ", link$yj, ")")
  link <- sf::st_sf(link, geometry = sf::st_as_sfc(stringo, crs = sf::st_crs(x)))
  link <- link[,c(1:3,8:10)]
  ctr <- st_coordinates(st_centroid(st_geometry(link)))
  link$ctrx <- ctr[,1]
  link$ctry <- ctr[,2]
  link <- link[,c("i","j","fij","ang","dist","ctrx","ctry","geometry")]
  return(link)
}

# Rotate

rotate <- function(x){
  ctr = st_as_sf(x[,c("ctrx","ctry")] %>% st_drop_geometry(), coords = c("ctrx", "ctry"), crs = st_crs(x))
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  n <- nrow(x)
  for (i in 1:n){
    st_geometry(x[i,]) <- ( st_geometry(x)[i] - st_geometry(ctr)[i]) * rot(x$ang[i] * pi/180) + st_geometry(ctr)[i]
  }
  return (x)
}

# Circles

getcircles <- function(x, xid, df, dfid, dfvar, k){
  
  if (is.null(k)){
    m <- max(df[,dfvar])
    bb <- st_bbox(x)
    k <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin)/ (m * 700)
  }
  
  dots <- sf::st_centroid(x = sf::st_geometry(x),of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON")))
  dots <- st_sf(x[,xid] %>% st_drop_geometry(), dots)
  dots <- merge(dots, df, by.x = xid, by.y = dfid)
  dots[,"r"] <- dots[,dfvar] %>% st_drop_geometry() * k
  circles <- st_buffer(dots, dots$nb * k)
  circles <- circles[,c(xid,dfvar,"r","geometry")]
  return(circles)
}

# Dots

getdots <- function(x, xid, k = NULL){
  if (is.null(k)){
    bb <- st_bbox(x)
    k <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin)/300
  }

  dots <- sf::st_centroid(x = sf::st_geometry(x),of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON")))
  dots <- st_sf(x[,xid] %>% st_drop_geometry(), geometry = dots)
  dots[,"r"] <- k
  circles <- st_buffer(dots, k)
  circles = circles[,c("id","r","geometry")]
  return(circles)
}

# Link to Flows

linktoflows <- function(link, size, k, dfvar, type, decreasing){
  
  crs <- st_crs(link)
  link$shift <- 0
  link$ij <- paste0(link$i,"_",link$j)
  link$ji <- paste0(link$j,"_",link$i)
  nb <- nrow(link)
  for(i in 1:nb) {
    if(link$ij[i] %in% link$ji) {link$shift[i] <- 1}
  }
  
  bb <- st_bbox(link)
  hmax <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin)/25
  delta <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin) / 300
  delta2 <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin) / 750
  
  if(!"delta_i" %in% names(link)){
    link$delta_i <- 0
    link$delta_j <- 0
    delta <- 0
  }
  
  
  if (size == "area"){
    if(is.null(k)){ k <- hmax / max(link[[dfvar]] / link$dist)}
    link$height <- link[[dfvar]] / link$dist * k
    link$area <- link$height * link$dist
    
  } else {
    if(is.null(k)){ k <- hmax / max(link[[dfvar]])}
    link$height <- link[[dfvar]] * k
  }
  
  link <- rotate(link)
  l <- st_geometry(link)
  d <- st_coordinates(l)
  n <- nrow(d)
  r <- data.frame(d[seq(1, n, 2),] ,d[seq(2, n, 2),] )
  rownames(r) <- r$L1
  r <- r[,c("X","Y","X.1","Y.1")]
  colnames(r) <- c("x1","y1","x2","y2")
  r$height <- link$height
  
  if (type == "arrows"){
    r$x1 <- r$x1 + link$delta_j + delta
    r$x2 <- r$x2 - link$delta_i - delta
    r$y1 <- r$y1 - (r[["height"]]/2 + delta2) * link$shift
    r$y2 <- r$y2 - (r[["height"]]/2 + delta2) * link$shift
    r$p1x <- r$x1
    r$p1y <- r$y1 - r[["height"]]/2
    r$p2x <- r$x2 - r[["height"]]/2
    r$p2y <- r$y2 - r[["height"]]/2
    r$p3x <- r$x2
    r$p3y <- r$y2
    r$p4x <- r$p2x
    r$p4y <- r$y2 + r[["height"]]/2
    r$p5x <- r$x1
    r$p5y <- r$y1 + r[["height"]]/2
    
    
    st_geometry(link) <- st_as_sfc(paste0("POLYGON((",r$p1x," ",r$p1y,", ",r$p2x," ",r$p2y,", ",r$p3x," ",r$p3y,", ",r$p4x," ",r$p4y,", ",r$p5x," ",r$p5y,", ",r$p1x," ",r$p1y,"))"))
  }
  
  
  if (type == "rect"){
    r$p1x <- r$x1 + link$delta_j + delta
    r$p2x <- r$x2 - link$delta_i - delta
    r$p1y <- r$y1 - (r[["height"]]/2 + delta2) * link$shift
    r$p2y <- r$y2 - (r[["height"]]/2 + delta2) * link$shift
    r$p3x <- r$p2x
    r$p3y <- r$p2y + r[["height"]]/2
    r$p4x <- r$p1x
    r$p4y <- r$p1y + r[["height"]]/2
    r$p1y <- r$p1y - r[["height"]]/2
    r$p2y <- r$p2y - r[["height"]]/2
    st_geometry(link) <- st_as_sfc(paste0("POLYGON((",r$p1x," ",r$p1y,", ",r$p2x," ",r$p2y,", ",r$p3x," ",r$p3y,", ",r$p4x," ",r$p4y,", ",r$p1x," ",r$p1y,"))"))
  }
  
  link$ang <- - link$ang
  link <- rotate(link)
  link <- link[order(link$height, decreasing = decreasing),]
  st_crs(link) <- crs
  return(link)
}


#################################################################################
############ flowmapper ##########################################################
############################################################################

ttt_flowmapper <- function(
  x,
  xid = "id",
  df,
  dfid = c("i","j"),
  dfvar = "fij",
  size = "thickness",
  type = "arrows",
  decreasing = FALSE,
  add = FALSE,
  lwd = 1,
  col = "#FF000099",
  border = "#4a0c25",
  k = NULL,
  df2 = NULL,
  df2id = "id",
  df2var,
  k2 = NULL,
  col2 = "white",
  border2 = "black",
  lwd2 = 2,
  plot = TRUE
){
  
  
  # x = subregions
  # xid = "id"
  # df = migr
  # dfid = c("i","j")
  # dfvar = "fij"
  # size = "thickness"
  # type = "arrows"
  # decreasing = FALSE
  # add = FALSE
  # lwd = 1
  # col = "#FF000099"
  # border = "#4a0c25"
  # k = NULL
  # df2 = NULL
  # df2id = "id"
  # df2var
  # k2 = NULL
  # col2 = "white"
  # border2 = "black"
  # lwd2 = 2
  # plot = TRUE
  # 
  
  
  links <- getlinks(x, df, xid, dfid, dfvar)
  
  if(!is.null(df2)){
    c <- getcircles(x = x, xid = xid, df = df2, dfid = df2id, dfvar = df2var, k = k2)
    r <- c[,c(df2id,"r")] %>% st_drop_geometry()
  } else {
    c <- getdots(x = x, xid = xid, k = k2)
    r <- c[,c(xid,"r")] %>% st_drop_geometry()
  }
  
  links <- merge(links,r, by.x = "j", by.y = "id", all.x = TRUE)
  links <- merge(links,r, by.x = "i", by.y = "id", all.x = TRUE)
  colnames(links) <- c("i", "j", "fij", "ang", "dist", "ctrx", "ctry", "delta_i", "delta_j", "geometry")
  links$delta_i[is.na(links$delta_i)] <- 0
  links$delta_j[is.na(links$delta_j)] <- 0
  flows <- linktoflows(links, size, k = k, dfvar, type, decreasing)


  if (plot == TRUE){
    plot(st_geometry(flows), col = col, border = border, lwd = lwd, add = add)
    plot(st_geometry(c), col = col2, border = border2, lwd = lwd2, add = TRUE)
  }
  
  output <- list("links" = links, "circles" = c, "flows" = flows)
  
  return(output)
  
}


########################################################################################################################################
###### LEGEND #######################################################################################################################
##################################################################################################################################

ttt_flowmapperlegend <- function(x, pos = NULL, title = "Title 1", title2 = NULL) {
  # Vars
  
  col = "white"
  border = "black"
  lwd = 1
  values.cex = 0.6
  values.round = 0
  lty = 3
  nb.circles = 4
  title.cex = 0.8
  title.font = 2
  
  
  # 1 - circles
  
  # Radii & Values
  v <- x[[2]]
  st_geometry(v) <- NULL
  v <- v[,2]
  r <- sqrt(as.numeric(st_area(x[[2]]))/pi)
  radii <- seq(from = max(r), to = min(r), length.out = nb.circles)
  sle <- radii * radii * pi
  values <- sle * max(v) / sle[1]
  
  # Positions
  
  delta <- (par()$usr[2] - par()$usr[1]) / 50
  if(length(pos) != 2){
    pos <- c(par()$usr[1] + radii[1] + delta,par()$usr[3] + delta)
  }
  
  # Circles
  
  if (!is.null(title2)){
    
    for(i in 1:nb.circles){
      # circles
      posx <- pos[1]
      posy <- pos[2] + radii[i]
      p <- st_sfc(st_point(c(posx,posy)))
      circle <- st_buffer(st_as_sf(p), dist = radii[i])
      plot(circle, col = col, border = border, lwd=lwd, add=T)
      # lines
      segments(posx, posy + radii[i], posx + radii[1] + radii[1]/10, col = border, lwd=lwd, lty = lty)
      # texts
      text(x = posx + radii[1] + radii[1]/5, y = posy + radii[i],
           labels = formatC(round(values[i],values.round), big.mark = " ", format = "fg", digits = values.round), adj = c(0,0.5), cex = values.cex)
    }
    
    # Title
    text(x = posx - radii[1] ,y = posy + radii[1]*2 + radii[1]/3, title2,
         adj = c(0,0), cex = title.cex, font = title.font)
  }
  
  # flows (thickness)
  
  if(is.null(x[[3]]$area)){
    
    
    hmax <- max(x[[3]]$height)
    hmin <- min(x[[3]]$height)
    bb <- st_bbox(x[[3]])
    width <- (bb$xmax - bb$xmin) / 15
    deltax <- (bb$xmax - bb$xmin) / 10
    rectmax <- st_as_sfc(paste0("POLYGON((",0," ",0,", ",width," ",0,", ",width," ",hmax,", ",0," ",hmax,", ",0," ",0,"))")) + pos + c(max(radii) + deltax,0)
    rectmin <- st_as_sfc(paste0("POLYGON((",0," ",0,", ",width," ",0,", ",width," ",hmin,", ",0," ",hmin,", ",0," ",0,"))")) + pos + c(max(radii) + deltax,0)
    segments(pos[1] + max(radii) + width + deltax, pos[2] + hmin, pos[1] + max(radii) + width + deltax + width/4, pos[2] + hmin, col = border, lwd=lwd, lty = lty)
    segments(pos[1] + max(radii) + width + deltax, pos[2] + hmax, pos[1] + max(radii) + width + deltax + width/4, pos[2] + hmax, col = border, lwd=lwd, lty = lty)
    plot(rectmax, add= TRUE)
    plot(rectmin, add= TRUE)
    vals <- flows[[2]][,2] %>% st_drop_geometry()
    text(x = pos[1] + max(radii) + width + deltax + width/4 + width/8 , y = pos[2] + hmin,
         labels = formatC(round(min(vals),values.round), big.mark = " ", format = "fg", digits = values.round), adj = c(0,0.5), cex = values.cex)
    
    text(x = pos[1] + max(radii) + width + deltax + width/4 + width/8, y = pos[2] + hmax,
         labels = formatC(round(max(vals),values.round), big.mark = " ", format = "fg", digits = values.round), adj = c(0,0.5), cex = values.cex)
    
    text(x = pos[1] + max(radii) + width/2 + deltax, y = pos[2] + (hmax - hmin)/2,
         labels = "(thickness)", adj = c(0.5,0), cex = 0.6)
    
    # Title
    text(x = pos[1] + max(radii) + deltax ,y = pos[2] + hmax  + radii[1]/3, title,
         adj = c(0,0), cex = title.cex, font = title.font)
    
  }
  
  # flows (area)
  
  
  if(!is.null(x[[3]]$area)){
    
    hmax <- sqrt(max(x[[3]]$area))
    hmin <- sqrt(min(x[[3]]$area))
    
    bb <- st_bbox(x[[3]])
    width <- (bb$xmax - bb$xmin) / 15
    deltax <- (bb$xmax - bb$xmin) / 10
    rectmax <- st_as_sfc(paste0("POLYGON((",0," ",0,", ",hmax," ",0,", ",hmax," ",hmax,", ",0," ",hmax,", ",0," ",0,"))")) + pos + c(max(radii) + deltax,0)
    rectmin <- st_as_sfc(paste0("POLYGON((",0," ",0,", ",hmin," ",0,", ",hmin," ",hmin,", ",0," ",hmin,", ",0," ",0,"))")) + pos + c(max(radii) + deltax + hmax - hmin,0)
    segments(pos[1] + max(radii) + hmax + deltax, pos[2] + hmin, pos[1] + max(radii) + hmax + deltax + width/4, pos[2] + hmin, col = border, lwd=lwd, lty = lty)
    segments(pos[1] + max(radii) + hmax + deltax, pos[2] + hmax, pos[1] + max(radii) + hmax + deltax + width/4, pos[2] + hmax, col = border, lwd=lwd, lty = lty)
    plot(rectmax, add= TRUE)
    plot(rectmin, add= TRUE)
    vals <- flows[[2]][,2] %>% st_drop_geometry()
    text(x = pos[1] + max(radii) + hmax + deltax + width/4 + width/8 , y = pos[2] + hmin,
         labels = formatC(round(min(vals),values.round), big.mark = " ", format = "fg", digits = values.round), adj = c(0,0.5), cex = values.cex)
    
    text(x = pos[1] + max(radii) + hmax + deltax + width/4 + width/8, y = pos[2] + hmax,
         labels = formatC(round(max(vals),values.round), big.mark = " ", format = "fg", digits = values.round), adj = c(0,0.5), cex = values.cex)
    
    text(x = pos[1] + max(radii) + hmax/2 + deltax, y = pos[2] + hmax/2,
         labels = "(area)", adj = c(0.5,0), cex = 0.6)
    
    # Title
    text(x = pos[1] + max(radii) + deltax ,y = pos[2] + hmax  + radii[1]/3, title,
         adj = c(0,0), cex = title.cex, font = title.font)
    
    
    
  }
}

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################

library(sf)
library(mapsf)

##### DATA

crs <- "+proj=aeqd +lat_0=90 +lon_0=50 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
subregions <- st_read(system.file("subregions.gpkg", package="flowmapper")) %>% st_transform(crs)
migr <- read.csv("data/world/fij/migr2019_T.csv")
subregions <- st_read("data/world/geom/subregions.gpkg")
#migr <- read.csv(system.file("migrantstocks2019.csv", package="flowmapper"))
migr <- read.csv("data/world/fij/migr2019_T.csv")
subregions <- st_transform(x = subregions, crs = crs)
#threshold <- 1500
#migr <- migr[migr$fij >= threshold,]


mf_map(subregions, col = "#6b274f", border = "white")

flows <- ttt_flowmapper(
  x = subregions,
  xid = "id",
  df = migr,
  dfid = c("i","j"),
  dfvar = "fij",
  add = TRUE)


getlinks(x=subregions, df = migr, xid = "id", dfid = c("i","j"), dfvar = "fij")

ttt_flowmapperlegend(x = flows, title = "inter", title2 = "intra")

