theme_discretes <- function(gradient) {
  theme <- list(
    scale_colour_discrete = function() { }, 
    scale_color_discrete  = function() { }, 
    scale_fill_discrete   = function() { })
  
  theme$scale_colour_discrete <- function() {
    discrete_scale('colour', 'ggthemr', discrete_colours(colours), na.value = na.value, ...)
  }
  formals(theme$scale_colour_discrete) <- formals(ggplot2::scale_color_discrete)
  environment(theme$scale_colour_discrete) <- getNamespace("ggplot2")
  
  theme$scale_fill_discrete = function() {
    discrete_scale('fill', 'ggthemr', discrete_colours(colours), na.value = na.value, ...)
  }
  formals(theme$scale_fill_discrete) <- formals(ggplot2::scale_fill_discrete)
  environment(theme$scale_fill_discrete) <- getNamespace("ggplot2")
  
  theme$scale_color_discrete <- theme$scale_colour_discrete
  theme
}
