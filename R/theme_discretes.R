theme_discretes <- function(colours) {
  theme <- list(
    scale_colour_discrete = function() { }, 
    scale_color_discrete  = function() { }, 
    scale_fill_discrete   = function() { })
  
  theme$scale_colour_discrete <- function(..., aesthetics = 'colour') { }
  body(theme$scale_colour_discrete) <- bquote({
    discrete_scale(aesthetics, 'ggthemr', .(discrete_colours(colours)), na.value = na.value, ...)
  })
  formals(theme$scale_colour_discrete)$na.value <- formals(getExportedValue("ggplot2", "scale_color_discrete"))$na.value
  environment(theme$scale_colour_discrete) <- getNamespace("ggplot2")
  
  theme$scale_fill_discrete = function(..., aesthetics = 'fill') { }
  body(theme$scale_fill_discrete) <- bquote({
    discrete_scale(aesthetics, 'ggthemr', .(discrete_colours(colours)), na.value = na.value, ...)
  })
  formals(theme$scale_fill_discrete)$na.value <- formals(getExportedValue("ggplot2", "scale_fill_discrete"))$na.value
  environment(theme$scale_fill_discrete) <- getNamespace("ggplot2")
  
  theme$scale_color_discrete <- theme$scale_colour_discrete
  theme
}
