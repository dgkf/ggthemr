#' Create a ggthemr_gradient object
#' 
#' Given a vectors of colors, checks will be carried out to make sure colours
#' are not duplicated and are valid, and creates fashions values into arguments
#' to a known ggplot2 gradient layer See \code{\link{define_palette}} for more
#' details.
#' 
#' @param x Vector of colours
#' 
#' @return Object of type ggthemr_gradient
#' 
define_gradient <- function(colours, ..., low, mid, high, colors) {
  colours <- if (!missing(colors)) colors else colours

  # handle situation where an already defined gradient is passed 
  if (!missing(colours) && inherits(colours, "ggthemr_gradient")) 
    return(colours)
  
  # create scale_*_gradient* functions, handling specified arguments  
  if (!missing(colours) && all(is_colour(colours))) {
    return(structure(
      list(colours = colours, ...), 
      colour = ggplot2::scale_colour_gradientn,
      fill = ggplot2::scale_fill_gradientn,
      class = c("ggthemr_gradientn", "ggthemr_gradient")))
  } else if (!any(missing(low), missing(mid), missing(high)) && 
      all(is_colour(c(low, mid, high)))) {
    return(structure(
      list(low = low, mid = mid, high = high, ...), 
      colour = ggplot2::scale_colour_gradient2,
      fill = ggplot2::scale_fill_gradient2,
      class = c("ggthemr_gradient2", "ggthemr_gradient")))
  } else if (!any(missing(low), missing(high)) && all(is_colour(c(low, high)))) {
    return(structure(
      list(low = low, high = high, ...), 
      colour = ggplot2::scale_colour_gradient,
      fill = ggplot2::scale_fill_gradient,
      class = c("ggthemr_gradient2", "ggthemr_gradient")))
  }
  
  stop("insufficient arguments provided to define_gradient. expected one of: \n",
       "  arbitrary gradient: 'colours' or 'colors'\n",
       "  divergent gradient: 'low', 'mid' and 'high'\n",
       "   two-tone gradient: 'low' and 'high'")
}