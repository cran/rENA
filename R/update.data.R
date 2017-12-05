# ##
# # @title Update data file in S3
# #
# # @description Update data files
# #
# # @details [TBD]
# #
# # @param file [TBD]
# # @param moveTo
# # @export
# # @import httr
# # @return list containing the accumulation and set
# ##
# updateData <- function(file, moveTo) {
#   if(!is(file, "data.frame")) {
#     query=list();
#     if(!is.null(moveTo$VersionId)){
#       query["versionId"] = moveTo$VersionId
#     }
#     s3.raw = s3HTTP(verb="GET", bucket=moveTo$Bucket, path=moveTo$Key, query=query);
#     s3.df = read.csv(textConnection(rawToChar(httr::content(s3.raw,as="raw"))));
#     if(!is.null(s3.df) && is.data.frame(s3.df)) {
#       file = s3.df
#     } else {
#       return("No file found in S3")
#     }
#   }
#
#   if(is(file,"data.frame")){
#     oldata = data.table::as.data.table(file)
#     oldata$MERGECOL = merge_columns_c(oldata,moveTo$columns)
#     rowsToChange = which(oldata$MERGECOL==paste(moveTo$from,collapse="."))
#     oldata[rowsToChange, (moveTo$columns) := moveTo$to]
#     oldata = oldata[,-c("MERGECOL")]
#     fileName = tools::file_path_sans_ext(moveTo$Key)
#     fileNameParts = strsplit(fileName, split="/")[[1]];
#     env = environment()
#     assign(x = fileName, value = oldata, envir = env);
#     env[[fileName]] = get(x = fileName, envir = env)
#     tmp <- tempfile(
#       pattern = fileNameParts[length(fileNameParts)],
#       fileext = paste(".",tools::file_ext(moveTo$Key),sep="")
#     );
#     on.exit(unlink(tmp))
#     write.csv(x = env[[fileName]], file = tmp, row.names = F)
#     updatedFile = put.s3.object(file = tmp, bucket = moveTo$Bucket, object = moveTo$Key);
#
#     return(list(
#       VersionId = attr(updatedFile, "x-amz-version-id"),
#       Key = moveTo$Key,
#       Bucket = moveTo$Bucket
#     ))
#   } else {
#     return("Unable to create data.frame from file provided.")
#   }
# }
#
# # moveTo = list(
# #   VersionId = 'FpQ23VlPL5EdisfUwnEqpWmsKm4x_Lv2',
# #   Bucket = 'rena-data-files',
# #   Key = 'google:115244317706686998972/Default/CleanRSdata_w_names.csv',
# #   columns=c("Condition","UserName"),
# #   from=list(Condition="New Group 13", UserName="brandon f"),
# #   to=list(Condition="New Group 14", UserName="brandon f")
# # )
# # moveTo = list(
# #   VersionId = 'FpQ23VlPL5EdisfUwnEqpWmsKm4x_Lv2',
# #   Bucket = 'rena-data-files',
# #   Key = 'google:115244317706686998972/Default/CleanRSdata_w_names.csv',
# #   columns=c("Condition","userName"),
# #   from=list(Condition="FirstGame", userName="Justin Kim"),
# #   to=list(Condition="New Group 1", userName="Justin Kim")
# # )
# # update.data(RS.data, moveTo)
