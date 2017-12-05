##
#' @title Find conversations by unit
#'
#' @description Find rows of conversations by unit
#'
#' @details [TBD]
#'
#' @param data [TBD]
#' @param units [TBD]
#' @param units.by [TBD]
#' @param conversation.by [TBD]
#' @param window [TBD]
#' @param codes [TBD]
#'
#' @export
#' @return list containing the accumulation and set
##
ena.conversation = function(data, units, units.by, conversation.by, window, codes=NULL) {
  # browser()
  if(!is(data, "data.table")){
    data = data.table::as.data.table(data);
  }
  if(!any(colnames(data) == "ENA_UNIT")) {
    data$ENA_UNIT = merge_columns_c(data, units.by)
  }
  codedUnitRows = c()
  conversation.by = make.names(conversation.by)
  units.by = make.names(units.by)
  if(!is.null(codes)) codes = make.names(codes)
  by.unit <- function(cols, I) {
    # browser()
    whichRows = which(cols$ENA_UNIT %in% units);
    unitRows = I[whichRows];
    codedRows = rowSums(data[unitRows,c(codes),with=F]) > 0;
    unitRows = unitRows[codedRows];

    winRows = unique(unlist(lapply(unitRows, function(x) {
      begin = ifelse(x - window < 1, 1, x - window + 1);
      range = c(begin:x)
      codedCols = which(colSums(data[range,c(codes),with=F]) > 0)

      if(length(codedCols) > 1) {
        # browser()
        if(is.null(codedUnitRows)) {
          codedUnitRows <<- c(x);
        } else {
          codedUnitRows[length(codedUnitRows)+1] <<- x;
        }

        range
      } else {
        NULL
      }
    })));

    return(paste(winRows, collapse=","));
  }

  rowsByConversation = data[, by.unit(.SD, .I), by=conversation.by, .SDcols=c("ENA_UNIT", conversation.by, codes)];
  numericRows = as.numeric(unlist(strsplit(paste(rowsByConversation$V1, collapse=","),",")));
  numericRows = numericRows[!is.na(numericRows)]
  # browser()
  return(list(
    allRows = numericRows[!is.na(numericRows)],
    unitRows = codedUnitRows,
    referantRows = codedUnitRows == numericRows
  ));
}
# convs = ena.conversation(rsdt, units, c("Condition","UserName"), c('Condition','ActivityNumber'), window, c("Data","Performance.Parameters"))
# print(convs)
