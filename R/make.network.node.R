
make.network.node <- function(vals, types=c("A","B") ) {
  if(length(vals) == 1) {
    if(vals[1] == 0) {
      list(size=0,type=NA)
    } else {
      list(size=vals[1], type=types[1])
    }
  } else if (vals[1] > vals[2]) {
    list(size=vals[1]-vals[2], type=types[1])
  } else if (vals[1] < vals[2]) {
    list(size=vals[2]-vals[1], type=types[2])
  } else {
    list(size=0, type=NA)
  }
};
