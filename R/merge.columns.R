###
# Merge data.table columns
#
# @param x data.table
# @param from.cols vector of column names to merge
# @param new.col optional name for appending the new column in-place
# @param sep character used in joining column values, default is "."
#
# @return if new.col is NULL, returns vector of the same length as x,
#         else the values are appended to x in a column named `new.col`
###
merge.columns <- function(x, from.cols, new.col = NULL, sep = ".") {
  combine <- function(v){paste(trimws(v),collapse=sep)};

  new.col.vector = x[,{apply(.SD,1,combine)},with=T,.SDcols=from.cols];
  if(!is.null(new.col)) {
    x[[new.col]] = new.col.vector;
    return(x);
  } else {
    return(new.col.vector);
  }
}
