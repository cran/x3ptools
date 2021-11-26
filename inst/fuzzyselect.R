library(x3ptools)
# needs the newest version of x3ptools from my github repo 
# remotes::install_github("heike/x3ptools")

x3p_fuzzyselect <- function(x3p, col="#FF0000", mad=5, type="plane", update=TRUE) {
  ids <- rgl::ids3d()
  if (nrow(ids) == 0) {
    size <- dim(tm$surface.matrix)
    size[2] <- round(500/size[1]*size[2])
    size[1] <- 500
    x3p %>% x3p_image(size = size, zoom=0.8)
  }
  rgl::rgl.bringtotop()
  
  stop <- FALSE
  cat("select rectangular area or click to stop\n")
  
  while(!stop) {
    tab1 <- table(as.vector(x3p$mask))
    x3p <- x3p %>% x3p_select(col=col)
    
    tab2 <- table(as.vector(x3p$mask))
    stop <- identical(tab1, tab2)
    
    if (stop) {
      cat("exiting selection.")
      
      return(x3p)
    }
    
    x3p_df <- x3p %>% x3p_to_df()
    x3p_df %>% count(mask)
    
    if (type== "plane") {
      m1 <- x3p_df %>% dplyr::filter(mask == col) %>% MASS::rlm(value ~ x + y, data = .)
      
      
      cat("... adding similar cases \n")
      
      x3p_df$p1 <- predict(m1, newdata = x3p_df)
      x3p_df$r1 <- abs(x3p_df$value - x3p_df$p1) 
      
      #x3p_df <- x3p_df %>% filter(r1 < 1*max(abs(m1$residuals)))
      
      x3p_df$mask[which(x3p_df$r1 < mad*mad(m1$residuals))] = col
    }
    if (type == "distance") {
      # idea: 
    }
    # convert back to x3p
    x3p <- x3p_df %>% df_to_x3p()
    
    if (update) {
      rgl::pop3d()
      #   browser()
      surface <- x3p$surface.matrix
      z <- 5 * surface
      yidx <- ncol(z):1
      y <- x3p$header.info$incrementY * yidx
      x <- x3p$header.info$incrementX * (1:nrow(x3p$surface.matrix))
      rgl::surface3d(x, y, z, x3p_df$mask, back = "fill")
    }
    cat("select a rectangular area on the active rgl device or click on white space to stop ...\n")
    
  }
  
  cat("exiting selection.")
  x3p
}


f11m <- read_x3p("~/Documents/Teaching/DS 401/fadul1-1-mask-2.x3p")

f11m <- f11m %>% x3p_add_mask()

f11m %>% image_x3p(size=c(500,500), zoom = 0.8)

f11m <- f11m %>% x3p_fuzzyselect()



tenth <- f11m %>% x3p_sample(m = 8)
tenth %>% image_x3p(size=c(500,500), zoom = 0.8)
tenth_db <- tenth %>% x3p_to_df()

dist <- dist(tenth_db[,1:3])
cl <- hclust(dist, method="complete")
plot(cl)
tenth_db$cl <- cutree(cl, k=5)
tenth_db$mask <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF")[tenth_db$cl]
tenth <- tenth_db %>% df_to_x3p()
tenth %>% image_x3p(size=c(500,500), zoom = 0.8)
count(tenth_db, mask)

f11m <- f11m %>% x3p_select(col="#FFFFFF")

f11m %>% image_x3p(size=c(500,500), zoom = 0.8)



x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
p <- plot3d(x, y, z, type = 's')
ids3d()
lines3d(x, y, z)
ids3d()
if (interactive() && !rgl.useNULL()) {
  readline("Hit enter to change spheres")
  pop3d(id = p["data"])
  spheres3d(x, y, z, col = "green", radius = 1/2)
  box3d()
}