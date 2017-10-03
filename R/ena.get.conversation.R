##
# @title Gets a conversation from an ENA set
#
# @description Gets a conversation from an ENA set from a given ENA data object.
#
# @param data.file data.frame or file path to CSV
# @param conversations Vector of conversations
#@param filter.by
#@param code.cols Columns of codes
#@param referants.only Only return rows that are coded for a column in code.cols or are contained in a window
#@param metadata.cols Vector of additional metadata columns
#
#@keywords ENA, get, conversation
#
#@seealso \code{\link{ena.make.set}}
#
# @examples
# \dontrun{
# #Given an ENA set
# ena.get.conversation(\code{\link{ENAset}})
# }
#
# @return Conversation from desired \code{\link{ENAset}}
##
ena.get.conversation <- function(
  data.file,
  conversations
) {
  root.node = "Conversations"
  path.sep = "/"
  data.file.raw = data.file;

  if(is.character(data.file)) {
    data.file = read.csv(data.file);
  }
  else if(!is.data.frame(data.file)) {
    data.file = data.frame(data.file.raw);
  }
  df.dt = data.table::data.table(data.file);

  # browser()
  sub = df.dt[,.SD ,.SDcols=conversations,with=T];
  sub$pathString = apply(data.frame(root.node,sub[,c(conversations),with=F]), 1, paste, collapse=path.sep);
  sub.nodes = as.list(data.tree::as.Node(sub));

  sub.nodes[[1]] = NULL;

  rapply(sub.nodes, function(x) x, how = "replace")
}
