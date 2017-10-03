lagpad <- function(x, k) {
  if(is.data.frame(x)){
    lagged = lapply(x, FUN = function(xx) {
      c(rep(NA,k), xx)
    })
    lagged  = as.data.table(lagged)
  } else {
    lagged = c(rep(NA, k), x)
  }
  return(lagged)
}
