###
# @title Split code columns
#
# @description This function will split single code columns into binary columns.
#
# @param data.file The csv file location or data frame which has the columns to be split
# @param split.columns The specific column in the The csv file or data frame that will be split
# @param split.columns.by Delimits columns in The csv file or data frame that will be split
# @param code.names The name for the codes resulting from the split columns in the The csv file or data.frame
#
# @keywords columns, split
#
# @seealso \code{\link{ena.accumulate.data}}, \code{\link{ena.make.set}}
#
# @examples
# \dontrun{
# #Given a csv file location
# ena.split.codes(data = .csv)
#
# #Given a data frame
# ena.split.codes(data = data.frame)
# }
# @return Data frame containing the split code columns
#
###

ena.split.codes <- function(
  data.file,
  split.columns = NULL,
  split.columns.by = ",",
  code.names = NULL
) {
  data.file.raw = data.file;

  if(is.character(data.file)) {
    data.file = read.csv(data.file);
  }
  else if(!is.data.frame(data.file)) {
    data.file = data.frame(data.file.raw);
  }

  ## Use `split.columns.by` to split values in `split.columns`
  if( !is.null(split.columns) ) {
    data.file.dt = data.table::data.table(data.file);

    for(col in split.columns) {
      found.codes = sort(unique(unlist(mapply(function(x) {strsplit(x, split.columns.by) }, as.character(data.table::data.table(data.file)[[col]])))));

      found.codes.dt = data.table::data.table(matrix(ncol=length(found.codes), nrow=nrow(data.file), 0));
      colnames(found.codes.dt) = found.codes
      for(i in 1:nrow(data.file)) {
        found.codes.dt[i,unlist(strsplit(as.character(as.matrix(data.file.dt[i, col, with=F])), split.columns.by))] = 1
      }
      data.file = cbind(data.file,found.codes.dt);
    }
  }

  return(data.file);
}
