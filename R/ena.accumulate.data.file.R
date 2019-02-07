##
# @title Accumulate Data from csv
#
# @description This function accumulates rows of data.
#
# @details [TBD]
#
#@export
#
# @param file The csv file location or data.frame for the function
# @param units.used Delimits columns based on the units (which specific units to use)
# @param units.by unit columns to accumulate by
# @param conversations.by Columns used in the conversation
# @param codes Columns used based on codes
# @param window.size.back Number of lines back to include window in stanza
# @param window.size.forward Number of lines forward in stanza window
# @param binary [TBD]
# @param model [TBD]
# @param window [TBD]
# @param weight.by [TBD]
# @param binary.stanzas [TBD]
# @param mask [TBD]
# @param ... additional parameters addressed in inner function
#
# @keywords data, accumulate
#
# @seealso \code{\link{ena.make.set}}
#
# @examples
# \dontrun{
# codeNames = c(
#   "E.data","S.data","E.design","S.design","S.professional","E.client",
#   "V.client","E.consultant","V.consultant","S.collaboration","I.engineer",
#   "I.intern","K.actuator","K.rom","K.materials","K.power"
# )
#
# df.file <- system.file("extdata", "rs.data.csv", package="rENA")
#
# # Given a csv file location
# ena.accumulate.data(
#   df.file, units.by = c("UserName","Condition"),
#   conversations.by = c("ActivityNumber","GroupName"),
#   codes = codeNames
# )
# }
# @return \code{\link{ENAdata}} class object with accumulated data
#
##
ena.accumulate.data.file <- function(
  file,
  units.used = NULL,   #subset of actual unit values to use for accumulation - all used if not specified
  conversations.used = NULL,
  units.by,    #unit columns to merge on to create ENA_UNIT --- MUST BE SUPPLIED
  conversations.by,    #conversation columns to accumulate by --- MUST BE SUPPLIED
  codes = NULL,
  model = c("EndPoint", "AccumulatedTrajectory", "SeparateTrajectory"),
  window = c("Moving Stanza", "Conversation"),
  window.size.back = 1,
  window.size.forward = 0,
  weight.by = "binary",
  binary.stanzas = F,
  mask = NULL,
  include.meta = T,
  ...
) {
  #print(file);
  if(is.null(file) || is.null(units.by) || is.null(conversations.by) || is.null(codes)) {
    print("ACCUMULATION FROM FILE REQUIRES: file, units.by, conversations.by, and codes");
  }

  units = NULL;    #will be populated once csv is read

  model = match.arg(model);
  window = match.arg(window);
  if(identical(window, "Conversation")) {
    conversations.by = c(conversations.by, units.by);
    window.size.back = window;
  }
  data = ENAdata$new(
    file = file,
    units = units,
    units.used = units.used,
    units.by = units.by,
    conversations.by = conversations.by,
    codes = codes,
    window.size.back = window.size.back,
    window.size.forward = window.size.forward,
    weight.by = weight.by,
    model = model,
    mask = mask,
    include.meta = include.meta,
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
