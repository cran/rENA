#' #' @rdname put_object
#' #' @title Put object
#' #'
#' #' @description Puts an object into an S3 bucket
#' #'
#' #' @param file A character string containing the filename (or full path) of the file you want to upload to S3. Alternatively, an raw vector containing the file can be passed directly, in which case \code{object} needs to be specified explicitly.
#' #' @param object A character string containing the name the object should have in S3 (i.e., its "object key"). If missing, the filename is used.
#' #' @param bucket [TBD]
#' #' @param multipart A logical indicating whether to use multipart uploads. See \url{http://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html}. If \code{file} is less than 100 MB, this is ignored.
#' #' @param acl [TBD]
#' #' @param headers List of request headers for the REST call.
#' #' @param dots [TBD]
#' #'
#' #' @import aws.s3
#' #' @details This provide a generic interface for sending files (or serialized, in-memory representations thereof) to S3. Some convenience wrappers are provided for common tasks: e.g., s3save and s3saveRDS.
#' #'
#' #' Note that S3 is a flat file store. So there is no folder hierarchy as in a traditional hard drive. However, S3 allows users to create pseudo-folders by prepending object keys with \code{foldername/}. The \code{put_folder} function is provided as a high-level convenience function for creating folders. This is not actually necessary as objects with slashes in their key will be displayed in the S3 web console as if they were in folders, but it may be useful for creating an empty directory (which is possible in the web console).
#' #'
#' #' @return If successful, \code{TRUE}.
#' #'
#' #' @examples
#' #' \dontrun{
#' #'   library("datasets")
#' #'
#' #'   # write file to S3
#' #'   tmp <- tempfile()
#' #'   on.exit(unlink(tmp))
#' #'   utils::write.csv(mtcars, file = tmp)
#' #'   put_object(tmp, object = "mtcars.csv", bucket = "myexamplebucket")
#' #'
#' #'   # create a "folder" in a bucket
#' #'   put_folder("example", bucket = "myexamplebucket")
#' #'   ## write object to the "folder"
#' #'   put_object(tmp, object = "example/mtcars.csv", bucket = "myexamplebucket")
#' #'
#' #'   # write serialized, in-memory object to S3
#' #'   x <- rawConnection(raw(0), "w")
#' #'   utils::write.csv(mtcars, x)
#' #'   put_object(rawConnectionValue(x), object = "mtcars.csv", bucket = "myexamplebucketname")
#' #'
#' #'   # use `headers` for server-side encryption
#' #'   ## require appropriate bucket policy
#' #'   put_object(file = tmp, object = "mtcars.csv", bucket = "myexamplebucket",
#' #'              headers = c('x-amz-server-side-encryption' = 'AES256'))
#' #'
#' #'   # alternative "S3 URI" syntax:
#' #'   put_object(rawConnectionValue(x), object = "s3://myexamplebucketname/mtcars.csv")
#' #'   close(x)
#' #'
#' #'   # read the object back from S3
#' #'   read.csv(text = rawToChar(get_object(object = "s3://myexamplebucketname/mtcars.csv")))
#' #' }
#' #' @references \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPUT.html}{API Documentation}
#' #' @seealso put_bucket, get_object, delete_object
#' #' @importFrom utils head
#' #' @export
#' put.s3.object <- function(file, object,bucket, multipart = FALSE,
#'   acl = c("private", "public-read", "public-read-write", "aws-exec-read", "authenticated-read",
#'           "bucket-owner-read", "bucket-owner-full-control"), headers = list(),
#'   return = c("raw","logical"),
#'   ...)
#' {
#'   if (missing(object) && is.character(file)) {
#'     object <- basename(file)
#'   } else {
#'     if (missing(bucket)) {
#'       bucket <- aws.s3:::get_bucketname(object)
#'     }
#'     object <- aws.s3:::get_objectkey.character(object)
#'   }
#'   return <- match.arg(return);
#'   acl <- match.arg(acl);
#'   headers <- c(list('x-amz-acl' = acl), headers);
#'
#'   # function to call abort if any part fails
#'   abort <- function(id) aws.s3::delete_object(object = object, bucket = bucket, query = list(uploadId = id), ...)
#'   put <- function(bucket, path, headers, request_body, ...) {
#'     return(aws.s3::s3HTTP(
#'       verb="PUT", bucket=bucket,
#'       path=paste0('/', object),
#'       headers=c(
#'         headers, list(
#'           'Content-Length' = ifelse(is.character(file) && file.exists(file),file.size(file), length(file))
#'         )
#'       ),
#'       request_body=file,
#'       ...
#'     ))
#'   }
#'
#'   if (isTRUE(multipart)) {
#'     if (is.character(file) && file.exists(file)) {
#'       file <- readBin(file, what = "raw")
#'     }
#'     size <- length(file)
#'     partsize <- 1e8 # 100 MB
#'     nparts <- ceiling(size/partsize)
#'
#'     # if file is small, there is no need for multipart upload
#'     if (size < partsize) {
#'       r = put(file = file, object = object, bucket = bucket, headers = headers, multipart = FALSE)
#'       # aws.s3::put_object(file = file, object = object, bucket = bucket, multipart = FALSE, headers = headers, ...)
#'       if(return == "logical") return(TRUE)
#'       else return(r)
#'     }
#'
#'
#'     # split object into parts
#'     seqparts <- seq_len(partsize)
#'     parts <- list()
#'     for (i in seq_len(nparts)) {
#'       parts[[i]] <- head(file, partsize)
#'       if (i < nparts) {
#'         file <- file[-seqparts]
#'       }
#'     }
#'
#'     # initialize the upload
#'     initialize <- aws.s3::post_object(file = NULL, object = object, bucket = bucket, query = list(uploads = ""), headers = headers, ...)
#'     id <- initialize[["UploadId"]]
#'
#'     # loop over parts
#'     partlist <- list(Number = character(length(parts)), ETag = character(length(parts)))
#'     for (i in seq_along(parts)) {
#'       query <- list(partNumber = i, uploadId = id)
#'       r <- try(aws.s3::put_object(
#'             file = parts[[i]], object = object, bucket = bucket,
#'             multipart = FALSE, headers = headers, query = query),
#'             silent = FALSE)
#'       if (inherits(r, "try-error")) {
#'         abort(id)
#'         stop("Multipart upload failed.")
#'       } else {
#'         partlist[["Number"]][i] <- i
#'         partlist[["ETag"]][i] <- attributes(r)[["ETag"]]
#'       }
#'     }
#'
#'     # complete
#'     r <- aws.s3:::complete_parts(object = object, bucket = bucket, id = id, parts = partlist, ...)
#'     if(return == "logical") return(TRUE)
#'     else return(r)
#'   } else {
#'     r <- put(bucket, path, headers, request_body, ...);
#'     # r <- s3HTTP(verb="PUT",bucket=bucket,path=paste0('/', object),headers=c(headers, list(
#'     #       'Content-Length' = ifelse(is.character(file) && file.exists(file),file.size(file), length(file))
#'     #     )),request_body=file,...)
#'
#'     if(return == "logical") return(TRUE)
#'     else return(r)
#'   }
#' }
