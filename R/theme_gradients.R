theme_gradients <- function(gradient) {
  theme <- list(
    scale_colour_continuous = function() { }, 
    scale_color_continuous  = function() { }, 
    scale_fill_continuous   = function() { })
  
  colour_f <- attr(gradient, "colour")
  fill_f   <- attr(gradient, "fill")
  
  formals(theme$scale_colour_continuous) <- formals(colour_f)
  formals(theme$scale_colour_continuous)[names(gradient)] <- gradient
  body(theme$scale_colour_continuous) <- theme_gradients_fbody(gradient, 'colour')
  environment(theme$scale_colour_continuous) <- getNamespace("ggplot2")
  
  formals(theme$scale_fill_continuous) <- formals(fill_f)
  formals(theme$scale_fill_continuous)[names(gradient)] <- gradient
  body(theme$scale_fill_continuous) <- theme_gradients_fbody(gradient, 'fill')
  environment(theme$scale_fill_continuous) <- getNamespace("ggplot2")
  
  theme$scale_color_continuous <- theme$scale_colour_continuous
  theme$scale_colour_gradient  <- theme$scale_colour_continuous
  theme$scale_color_gradient   <- theme$scale_colour_continuous
  theme$scale_fill_gradient    <- theme$scale_fill_continuous
  
  theme
}

theme_gradients_fbody <- function(gradient, aesthetic) {
  UseMethod("theme_gradients_fbody")
}

theme_gradients_fbody.ggthemr_gradientn <- function(gradient, aesthetic) {
  bquote({
    colours <- if (!missing(colors)) colors else colours
    continuous_scale(.(aesthetic), 'ggthemr', 
        scales::gradient_n_pal(colours, values, space), guide = guide, ...)
  })
}

theme_gradients_fbody.ggthemr_gradient2 <- function(gradient, aesthetic) {
  bquote({
    continuous_scale(.(aesthetic), 'ggthemr', 
      scales::div_gradient_pal(low, mid, high, space), guide = guide, 
      rescaler = ggplot2:::mid_rescaler(mid = midpoint), ...)
  })
}

theme_gradients_fbody.ggthemr_gradient <- function(gradient, aesthetic) {
  bquote({
    continuous_scale(.(aesthetic), 'ggthemr', 
      scales::seq_gradient_pal(low, high, space), guide = guide, ...)
  })
}