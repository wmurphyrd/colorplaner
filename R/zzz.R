.onLoad <- function(libname, pkgname) {
  # add new aesthetics to ggplot2
  assignInternal <- function(sym, val, env) {
    unlockBinding(sym, env)
    assign(sym, val, env)
    lockBinding(sym, env)
  }
  assignInternal(".all_aesthetics",
                 c(ggplot2:::.all_aesthetics, "colour2", "color2", "fill2"),
                 asNamespace("ggplot2"))
  assignInternal(".base_to_ggplot",
                 c(ggplot2:::.base_to_ggplot, "color2" = "colour2"),
                 asNamespace("ggplot2"))
}
