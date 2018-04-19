##
#' @title Find conversations by unit
#'
#' @description Find rows of conversations by unit
#'
#' @details [TBD]
#'
#' @param set [TBD]
#' @param units [TBD]
#' @param units.by [TBD]
#' @param codes [TBD]
#' @param conversation.exclude [TBD]
#'
#' @examples
#' data(RS.data)
#'
#' codeNames = c('Data','Technical.Constraints','Performance.Parameters',
#'               'Client.and.Consultant.Requests','Design.Reasoning',
#'               'Collaboration');
#'
#' accum = rENA::ena.accumulate.data(
#'   units = RS.data[,c("Condition","UserName")],
#'   conversation = RS.data[,c("Condition","GroupName")],
#'   metadata = RS.data[,c("CONFIDENCE.Change","CONFIDENCE.Pre",
#'                         "CONFIDENCE.Post","C.Change")],
#'   codes = RS.data[,codeNames],
#'   model = "EndPoint",
#'   window.size.back = 4
#' );
#' set = ena.make.set(
#'   enadata = accum,
#'   rotation.by = ena.rotate.by.mean,
#'   rotation.params = list(accum$metadata$Condition=="FirstGame",
#'                          accum$metadata$Condition=="SecondGame")
#' );
#' ena.conversations(set, accum$unit.names[2], codes = set$enadata$codes)
#'
#' @export
#' @return list containing row indices representing conversations
##
ena.conversations = function(set, units, units.by=NULL, codes=NULL, conversation.exclude = c()) {
  # rawData = data.table::copy(set$enadata$raw);
  if(is.null(units.by)) {
    units.by = set$enadata$function.params$units.by;
  }
  conversation.by = set$enadata$function.params$conversations.by;
  window = set$enadata$function.params$window.size.back;

  rawAcc = data.table::copy(set$enadata$accumulated.adjacency.vectors);
  rawAcc$KEYCOL = merge_columns_c(rawAcc, conversation.by)

  conversationsTable = rawAcc[, paste(.I, collapse = ","), by = c(conversation.by)]
  rows = sapply(conversationsTable$V1, function(x) as.numeric(unlist(strsplit(x, split=","))),USE.NAMES = T)
  names(rows) = merge_columns_c(conversationsTable,conversation.by); #unique(rawAcc[,KEYCOL])

  adjCol = set$enadata$adjacency.matrix[1,] %in%  codes[1] & set$enadata$adjacency.matrix[2,] %in% codes[2]
  adjColName = paste("adjacency.code.", which(adjCol), sep = "")

  unitRows = merge_columns_c(rawAcc[,c(units.by),with=F], units.by)
  codedUnitRows = which(unitRows %in% units & rawAcc[[adjColName]] == 1)
  codedUnitRowConvs = rawAcc[codedUnitRows,KEYCOL];
  codedUnitRowConvsAll = NULL;
  if(length(codedUnitRows) > 0) {
    codedUnitRowConvsAll = unique(unlist(sapply(X = 1:length(codedUnitRows), simplify = F, FUN = function(x) {
      thisConvRows = rows[[codedUnitRowConvs[x]]]
      thisRowInConv = which(thisConvRows == codedUnitRows[x])
      thisRowAndWindow = rep(thisRowInConv,window) - (window-1):0;
      thisConvRows[thisRowAndWindow[thisRowAndWindow > 0]]
    })))
  }
  return(list(
    conversations = rows,
    unitConvs = unique(rawAcc[codedUnitRows,KEYCOL]),
    allRows = codedUnitRowConvsAll,
    unitRows = codedUnitRows
  ));
}
