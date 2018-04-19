##
#' @title Accumulate data from a data frame into a set of adjacency (co-occurrence) vectors
#'
#' @description This function initializes an ENAdata object, processing conversations from coded data to generate adjacency (co-occurrence) vectors
#'
#' @details ENAData objects are created using this function. This accumulation receives
#' separate data frames for units, codes, conversation, and optionally, metadata. It
#' iterates through the data to create an adjacency (co-occurrence) vector corresponding
#' to each unit - or in a trajectory model multiple adjacency (co-occurrence) vectors for
#' each unit.
#'
#' In the default MovingStanzaWindow model, co-occurrences between codes are
#' calculated for each line k in the data between line k and the window.size.back-1 previous
#' lines and window.size.forward-1 subsequent lines in the same conversation as line k.
#'
#' In the Conversation model, co-occurrences between codes are calculated across all lines in
#' each conversation. Adjacency (co-occurrence) vectors are constructed for each unit u by
#' summing the co-occurrences for the lines that correspond to u.
#'
#' Options for how the data is accumulated are endpoint, which produces one adjacency (co-occurrence)
#' vector for each until summing the co-occurrences for all lines, and two trajectory models:
#' AccumulatedTrajectory and SeparateTrajectory. Trajectory models produce an adjacency
#' (co-occurrence) model for each conversation for each unit. In a SeparateTrajectory model,
#' each conversation is modeled as a separate network. In an AccumulatedTrajectory model, the
#' adjacency (co-occurrence) vector for the current conversation includes the co-occurrences
#' from all previous conversations in the data.
#'
#' @export
#'
#' @param units A data frame where the columns are the properties by which units will be identified
#' @param conversation A data frame where the columns are the properties by which conversations will be identified
#' @param codes A data frame where the columns are the codes used to create adjacency (co-occurrence) vectors
#' @param metadata (optional) A data frame with additional columns of metadata to be associated with each unit in the data
#' @param model A character, choices: EndPoint (or E), AccumulatedTrajectory (or A), or SeparateTrajectory (or S); default: EndPoint. Determines the ENA model to be constructed
#' @param weight.by (optional) A function to apply to values after accumulation
#' @param mask (optional) A binary matrix of size ncol(codes) x ncol(codes). 0s in the mask matrix row i column j indicates that co-occurrence will not be modeled between code i and code j
#' @param window A character, choices are Conversation (or C), MovingStanzaWindow (MSW, MS); default MovingStanzaWindow. Determines how stanzas are constructed, which defines how co-occurrences are modeled
#' @param window.size.back A positive integer, Inf, or character (INF or Infinite), default: 1. Determines, for each line in the data frame, the number of previous lines in a conversation to include in the stanza window, which defines how co-occurrences are modeled
#' @param window.size.forward (optional) A positive integer, Inf, or character (INF or Infinite), default: 0. Determines, for each line in the data frame, the number of subsequent lines in a conversation to include in the stanza window, which defines how co-occurrences are modeled
#' @param ... additional parameters addressed in inner function
#'
#' @keywords data, accumulate
#'
#' @seealso \code{\link{ENAdata}}, \code{\link{ena.make.set}}
#'
#' @examples
#' data(RS.data)
#'
#' codeNames = c('Data','Technical.Constraints','Performance.Parameters',
#'   'Client.and.Consultant.Requests','Design.Reasoning','Collaboration');
#'
#' accum = ena.accumulate.data(
#'   units = RS.data[,c("UserName","Condition")],
#'   conversation = RS.data[,c("Condition","GroupName")],
#'   metadata = RS.data[,c("CONFIDENCE.Change","CONFIDENCE.Pre","CONFIDENCE.Post")],
#'   codes = RS.data[,codeNames],
#'   window.size.back = 4
#' )
#'
#' @return \code{\link{ENAdata}} object with data [adjacency (co-occurrence) vectors] accumulated from the provided data frames.
#'
##

ena.accumulate.data <- function(

  ##### NOTE: units, conversations, codes, and metadata must be data frames with the same number of rows
  units = NULL,   # data frame containing units
  conversation = NULL,    # df containing conversation lines
  codes = NULL,   # df containing codes
  metadata = NULL,   #optional - df containing metadata
  model = c("EndPoint", "AccumulatedTrajectory", "SeparateTrajectory"),   #use match arg and list?
  weight.by = "binary",
  window = c("MovingStanzaWindow", "Conversation"),
  window.size.back = 1,
  window.size.forward = 0,
  mask = NULL, #matrix (default - upper triangle of 1's)

  ### PARAMS NOT IN SPECS
  # output = c("class","json"),    #keep for now
  # output.fields = NULL,       #keep for now
  ...
) {

  if(is.null(units) || is.null(conversation) || is.null(codes)) {
    print("ACCUMULATION FROM DATA FRAMES REQUIRES: units, conversation, and codes");
  }
  if(nrow(units) != nrow(conversation) || nrow(conversation) != nrow(codes)) {
    print("Data Frames do not have the same number of rows!");
    ### throw error
  }

  ## DOOPT - inefficient cbinds
  df <- cbind(units, conversation);
  df <- cbind(df, codes);

  metadata = data.table::as.data.table(metadata)
  if(!is.null(metadata) && nrow(metadata) == nrow(df)) {
    df <- cbind(df, metadata);
  }

  model = match.arg(model)
  window = match.arg(window)

  units.by = colnames(units);   #accumulating by all unit columns provided in units df
  conversations.by = colnames(conversation); #accumulating by all columns provided in conversation df
  if(identical(window, "Conversation")) {
    conversations.by = c(conversations.by, units.by);
    window.size.back = window;
  } else if (identical(window, "MovingStanzaWindow")) {
    # infCheck = c("Inf", "Infinite")
    # if(any(window.size.back %in% infCheck)) {
    if(grepl(pattern = "inf",x = window.size.back, ignore.case=T)) {
      window.size.back = Inf
    }
    # if(any(window.size.forward %in% infCheck)) {
    if(grepl(pattern = "inf",x = window.size.forward, ignore.case=T)) {
      window.size.forward = Inf
    }
  }

  units.used = NULL;   # when accumulating from data frames, all units are used

  data = ENAdata$new(
    file = df,

    units = units,    #data frame of unit columns (including values)
    units.used = units.used,

    units.by = units.by,    # KEEP- automatically uses all units for accumulation from separate data frames
    conversations.by = conversations.by,    #column names of conversation df, automatically accumulating by all cols for accum from dfs
    codes = codes,

    window.size.back = window.size.back,
    window.size.forward = window.size.forward,

    weight.by = weight.by,

    model = model,
    mask = mask,
    ...
  );

  data$function.call = sys.call();

  # output = match.arg(output);
  # if(output == "json") {
  #   output.class = get(class(data))
  #
  #   if(is.null(output.fields)) {
  #     output.fields = names(output.class$public_fields)
  #   }
  #
  #   r6.to.json(data, o.class = output.class, o.fields = output.fields)
  # }
  #else
  data
}

