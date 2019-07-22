#' @importFrom scales seq_gradient_pal
theme_scales <- function(palette) {
  discretes <- theme_discretes(palette$swatch[-1])
  gradients <- theme_gradients(palette$gradient)
  append(discretes, gradients)
}

theme_geoms <- function(palette, line_weight) {
  colours <- palette$swatch[-1]
  
  geom_names <- grep("^geom_", getNamespaceExports("ggplot2"), value = TRUE)
  
  Filter(Negate(is.null), Map(n = geom_names, function(n) {
    obj <- layer_call_formal(getExportedValue("ggplot2", n))
    if (is.null(obj)) return(NULL)
    
    new_aes <- obj$default_aes
    
    if (!is.null(new_aes$colour) && !is.na(new_aes$colour))
      new_aes$colour <- colours[1]
    else if (!is.null(new_aes$fill) && !is.na(new_aes$fill))
      new_aes$fill <- colours[1]
    if (all(c("size", "linetype") %in% names(new_aes)))
      new_aes$size <- line_weight
    
    list(geom = obj, new = new_aes)
  }))
}

layer_call_formal <- function(geom_f, formal = c("geom", "stat")) {
  formal <- match.arg(formal, c("geom", "stat"))
  layer_call <- Filter(function(i) is.call(i) && i[[1]] == "layer", body(geom_f))
  if (!length(layer_call)) return(NULL)
  
  layer_fml  <- as.character(as.list(layer_call)[[1]][formal])
  if (!layer_fml %in% getNamespaceExports("ggplot2")) return(NULL)
  getExportedValue("ggplot2", layer_fml)
}
