##
# @title Find conversations by unit
#
# @description Find rows of conversations by unit
#
# @details [TBD]
#
# @param data [TBD]
# @param units [TBD]
# @param units.by [TBD]
# @param conversation.by [TBD]
# @param window [TBD]
# @param codes [TBD]
#
# @export
# @return list containing the accumulation and set
##
ena.conversation = function(data, units, units.by, conversation.by, window, codes=NULL, conversation.exclude = c()) {
  if(!is(data, "data.table")){
    data = data.table::as.data.table(data);
  }
  units.by = make.names(units.by)
  conversation.by = make.names(conversation.by)

  if(!any(colnames(data) == "ENA_UNIT")) {
    data$ENA_UNIT = merge_columns_c(data, make.names(units.by))
  }

  codedUnitRows = c()
  if(!is.null(codes)) codes = make.names(codes)
  by.unit <- function(cols, I) {
    whichRows = which(cols$ENA_UNIT %in% units);
    unitRows = I[whichRows];
    codedRows = rowSums(data[unitRows,c(codes),with=F]) > 0;
    unitRows = unitRows[codedRows];

    winRows = unique(unlist(lapply(unitRows, function(x) {
      indexI = which(I == x)
      rangeI = (indexI - window + 1):indexI
      rowsI = I[rangeI[rangeI > 0]]
      range = rowsI;
      codedCols = which(colSums(data[range,c(codes),with=F]) > 0)

      if(length(codedCols) > 1) {
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
  if(!any(colnames(data) == "ENA_CONV")) {
    rowsByConversation$ENA_CONV = merge_columns_c(rowsByConversation, conversation.by)
  }
  rowsByConversation = rowsByConversation[! ENA_CONV %in% conversation.exclude,]
  numericRows = as.numeric(unlist(strsplit(paste(rowsByConversation$V1, collapse=","),",")));
  numericRows = numericRows[!is.na(numericRows)]
  # browser()
  return(list(
    allRows = numericRows[!is.na(numericRows)],
    unitRows = codedUnitRows,
    referantRows = numericRows %in% codedUnitRows #codedUnitRows == numericRows
  ));
}
# convs = ena.conversation(rsdt, units, c("Condition","UserName"), c('Condition','ActivityNumber'), window, c("Data","Performance.Parameters"))
# print(convs)
