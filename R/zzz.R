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

`%||%` <- function (a, b)
{
  if (!is.null(a))
    a
  else b
}

# this version of as.trans will work without importing all *_trans functions
# from scales (unlike scales::as.trans)
as.trans <- function(x) {
  if (scales::is.trans(x)) return(x)
  get(paste0(x, "_trans"), asNamespace("scales"), mode = "function")()
}

