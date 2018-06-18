#' #' @export
#' put.s3.object <- function(file, object,bucket, multipart = FALSE,
#'  acl = c("private", "public-read", "public-read-write", "aws-exec-read", "authenticated-read",
#'          "bucket-owner-read", "bucket-owner-full-control"), headers = list(),
#'  return = c("raw","logical"),
#'  ...)
#' {
#'  if (missing(object) && is.character(file)) {
#'    object <- basename(file)
#'  } else {
#'    if (missing(bucket)) {
#'      bucket <- aws.s3:::get_bucketname(object)
#'    }
#'    object <- aws.s3:::get_objectkey.character(object)
#'  }
#'  return <- match.arg(return);
#'  acl <- match.arg(acl);
#'  headers <- c(list('x-amz-acl' = acl), headers);
#'  # function to call abort if any part fails
#'  abort <- function(id) aws.s3::delete_object(object = object, bucket = bucket, query = list(uploadId = id), ...)
#'  put <- function(bucket, path, headers, request_body, ...) {
#'    return(aws.s3::s3HTTP(
#'      verb="PUT", bucket=bucket,
#'      path=paste0('/', object),
#'      headers=c(
#'        headers, list(
#'          'Content-Length' = ifelse(is.character(file) && file.exists(file),file.size(file), length(file))
#'        )
#'      ),
#'      request_body=file,
#'      ...
#'    ))
#'  }
#'  if (isTRUE(multipart)) {
#'    if (is.character(file) && file.exists(file)) {
#'      file <- readBin(file, what = "raw")
#'    }
#'    size <- length(file)
#'    partsize <- 1e8 # 100 MB
#'    nparts <- ceiling(size/partsize)
#'    # if file is small, there is no need for multipart upload
#'    if (size < partsize) {
#'      r = put(file = file, object = object, bucket = bucket, headers = headers, multipart = FALSE)
#'      # aws.s3::put_object(file = file, object = object, bucket = bucket, multipart = FALSE, headers = headers, ...)
#'      if(return == "logical") return(TRUE)
#'      else return(r)
#'    }
#'    # split object into parts
#'    seqparts <- seq_len(partsize)
#'    parts <- list()
#'    for (i in seq_len(nparts)) {
#'      parts[[i]] <- head(file, partsize)
#'      if (i < nparts) {
#'        file <- file[-seqparts]
#'      }
#'    }
#'    # initialize the upload
#'    initialize <- aws.s3::post_object(file = NULL, object = object, bucket = bucket, query = list(uploads = ""), headers = headers, ...)
#'    id <- initialize[["UploadId"]]
#'    # loop over parts
#'    partlist <- list(Number = character(length(parts)), ETag = character(length(parts)))
#'    for (i in seq_along(parts)) {
#'      query <- list(partNumber = i, uploadId = id)
#'      r <- try(aws.s3::put_object(
#'            file = parts[[i]], object = object, bucket = bucket,
#'            multipart = FALSE, headers = headers, query = query),
#'            silent = FALSE)
#'      if (inherits(r, "try-error")) {
#'        abort(id)
#'        stop("Multipart upload failed.")
#'      } else {
#'        partlist[["Number"]][i] <- i
#'        partlist[["ETag"]][i] <- attributes(r)[["ETag"]]
#'      }
#'    }
#'    # complete
#'    r <- aws.s3:::complete_parts(object = object, bucket = bucket, id = id, parts = partlist, ...)
#'    if(return == "logical") return(TRUE)
#'    else return(r)
#'  } else {
#'    r <- put(bucket, path, headers, request_body, ...);
#'    # r <- s3HTTP(verb="PUT",bucket=bucket,path=paste0('/', object),headers=c(headers, list(
#'    #       'Content-Length' = ifelse(is.character(file) && file.exists(file),file.size(file), length(file))
#'    #     )),request_body=file,...)
#'    if(return == "logical") return(TRUE)
#'    else return(r)
#'  }
#' }
