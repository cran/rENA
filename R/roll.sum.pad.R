roll.sum.pad <- function(x,k) {
  if(is.data.frame(x)) {
    rolled = (
      x[,lapply(.SD, FUN= function(xx) {
        yy = roll_sum(lagpad(xx, k-1), n=k, align="right", fill = NA, na.rm=T)[(k):(length(xx)+(k-1))]
        return(yy);
      })]);
    return(rolled);
  } else {
    y = (roll_sum(lagpad(x, k-1), n=k, align="right", fill = NA, na.rm=T)[k:(length(x)+(k-1))])
    return(y)
  }
}
