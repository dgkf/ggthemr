#' create object for ggplot2 backups
.ggplot2 <- list()



#' cache ggplot2 objects into ggthemr, allowing them to be modified directly
#' within ggplot2 and restored as needed
#' 
#' this doesn't feel ideal, but prevents flooding of a the global namespace with
#' masked ggplot2 functions
#' 
#' @import ggplot2
#' 
.onAttach <- function(libname, pkgname) {
  ggplot2_exports <- as.list(getNamespace("ggplot2"))[getNamespaceExports("ggplot2")]
  envir <- getNamespace("ggthemr")
  unlockBinding(".ggplot2", envir)
  assign(".ggplot2", ggplot2_exports, envir = envir)
  lockBinding(".ggplot2", envir)
  setHook("detach", restore_ggplot2_binding)
}



#' restore a single binding within ggplot2 namespace back to original value
restore_ggplot2_binding <- function(x = getNamespaceExports("ggplot2")) {
  for (xi in x) modify_ggplot2_binding(xi, .ggplot2[xi])
}



#' modify a locked binding within an environment (the ggplot2 namespace by default)
modify_ggplot2_binding <- function(x, value, ..., envir = getNamespace("ggplot2")) {
  # ignore ggplot2 reexports
  binding_env <- environment(get(x, envir))
  if (is.null(binding_env) || !isNamespace(binding_env)) return()
  binding_ns  <- getNamespaceName(binding_env)
  if (binding_ns != "ggplot2") return()
  
  unlockBinding(x, envir)
  assign(x, value, ..., envir = envir)
  lockBinding(x, envir)
}
