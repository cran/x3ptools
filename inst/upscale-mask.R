
x3p <- read_x3p(file.choose())

sub <- x3p %>% x3p_sample(m=20)
sub <- sub %>% x3p_add_mask()

sub %>% x3p_image(size=c(500,500), zoom=0.8)
sub %>% x3p_select()

sub %>% x3p_fuzzyselect()


sub <- sub %>% x3p_circle_select()

sub <- sub %>% x3p_circle_select(col="#cd7f32")

# mask now looks good
x3p <- x3p %>% x3p_scale_mask(sub)
x3p %>% x3p_image(size=c(500,500), zoom=0.8)

x3p_scale_mask <- function(x3p, sub) {
  stopifnot(!is.null(sub$mask)) 
    
  scale <- ceiling(dim(t(x3p$surface.matrix))/dim(sub$mask))
  
  x3p_scale <- x3p %>% x3p_get_scale()
  x3p_df <- x3p %>% x3p_to_df() %>% 
    mutate(
      yindex = round(y / x3p_scale, 0),
      xindex = round(x / x3p_scale, 0),
      xdiv = xindex %/% scale[1],
      ydiv = yindex %/% scale[2]
    )

  sub_scale <- sub %>% x3p_get_scale()
  sub_df <- sub %>% x3p_to_df() %>% 
    mutate(
      yindex = round(y / sub_scale,0),
      xindex = round(x / sub_scale,0)
    )
  
  if (!is.null(x3p$mask)) x3p_df <- x3p_df %>% select(-mask)
  merged_df <- x3p_df  %>% left_join(sub_df %>% select(xindex, yindex, mask), by=c("xdiv"="xindex", "ydiv"="yindex"))
  merged <- merged_df %>% df_to_x3p()
  
  merged
}
################

x3p1 <- read_x3p(file.choose())
x3p2 <- read_x3p(file.choose())
x3p3 <- read_x3p(file.choose())
