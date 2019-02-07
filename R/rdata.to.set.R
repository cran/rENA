# rdata.to.set <- function(file, name, bucket, folder) {
#   readin = load(file)
#   # readin2= source(tmp)
#
#   # Pull CSV data out and store in AWS S3
#   csv = set$enadata$raw
#   colnames(csv) = make.names(colnames(csv), unique=T)
#   aws.signature::locate_credentials(key = "AKIAJ5IBR7IRVAYQX5MQ", secret = "1IDb2Osnovnez3QneoAj8HaZ1s2")
#   tmp <- tempfile(fileext = ".csv")
#   on.exit(unlink(tmp))
#   utils::write.csv(csv, file = tmp, fileEncoding = "UTF-8")
#   res = rENA:::put.s3.object(file = tmp, object = paste0("/",name), bucket = bucket) #paste0(bucket,"/",folder))
#
#   return(list(
#     status = 'success',
#     location = paste0("https://",bucket,".us-west-2.amazonaws.com/",name),
#     raw = list(
#       Bucket = bucket,
#       ETag = attr(res, "etag"),
#       Key = name,
#       Location = paste0("https://",bucket,".us-west-2.amazonaws.com/",name),
#       VersionId = attr(res, "x-amz-version-id"),
#       key = name
#     ),
#     selections = list(
#       units = colnames(set$enadata$function.params$units),
#       conversations = set$enadata$function.params$conversations.by,
#       codes = colnames(set$enadata$function.params$codes),
#       model = set$enadata$function.params$model,
#       weightBy = set$enadata$function.params$weight.by,
#       windowSize = set$enadata$function.params$window.size.back
#     ),
#     name = name
#   ))
# }
