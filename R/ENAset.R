####
#' ENAset R6class
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @export

#' @field enadata An \code{\link{ENAdata}} object originally used to create the set
#' @field points.raw A data frame containing accumulated adjacency (co-occurrence) vectors per unit
#' @field points.normed.centered A data frame of centered normed accumulated adjacency (co-occurrence) vectors for each unit
#' @field points.rotated A data frame of point positions for number of dimensions specified in ena.make.set (i.e., the centered, normed, and rotated data)
#' @field line.weights A data frame of connections strengths per unit (Data frame of normed accumu- lated adjacency (co-occurrence) vectors for each unit)
#' @field node.positions - A data frame of positions for each code
#' @field codes - A vector of code names
#' @field rotation.set - An \code{\link{ENARotationSet}} object
#' @field variance - A vector of variance accounted for by each dimension specified
#' @field centroids - A matrix of the calculated centroid positions
#' @field function.call - The string representation of function called
#' @field function.params - A list of all parameters sent to function call
#' @field rotation_dists TBD
#' @field points.rotated.scaled TBD
#' @field points.rotated.non.zero TBD
#' @field line.weights.unrotated TBD
#' @field line.weights.non.zero TBD
#' @field correlations A data frame of spearman and pearson correlations for each dimension specified
#' @field center.align.to.origin - align point and centroid centers to origin
####
ENAset = R6::R6Class("ENAset",
  public = list(


  ## Public Functions ----
    #' Create ENAset
    #'
    #' @param enadata TBD
    #' @param dimensions TBD
    #' @param norm.by TBD
    #' @param rotation.by TBD
    #' @param rotation.params TBD
    #' @param rotation.set TBD
    #' @param node.position.method TBD
    #' @param endpoints.only TBD
    #' @param center.align.to.origin TBD
    #' @param ... TBD
    #'
    #' @return ENAset
    initialize = function(
        enadata,
        dimensions = 2,

        norm.by = fun_sphere_norm,

        rotation.by = ena.svd.R6,
        rotation.params = NULL,
        rotation.set = NULL,

        #center.data = center_data_c,    ### made local to run
        node.position.method = lws.positions.sq.R6,
        endpoints.only = TRUE,
        center.align.to.origin = TRUE,
        ...
    ) {
       self$enadata <- enadata;

       private$dimensions <- dimensions;

       self$codes <- enadata$codes;

       self$function.call <- sys.call(-1);

       self$function.params$norm.by <- norm.by;    #was sphere_norm
       #self$function.params$center.data <- center.data;
       self$function.params$node.position.method <- node.position.method;    #was position.method
       self$function.params$rotation.by <- rotation.by;
       self$function.params$rotation.params <- rotation.params;
       self$function.params$rotation.set <- rotation.set;
       self$function.params$endpoints.only <- endpoints.only;
       self$function.params$center.align.to.origin <- center.align.to.origin;
       private$args <- list(...);
     },


    #' Process ENAset
    #'
    #' @return ENASet
    process = function() {
      return(private$run())
    },

    #' Get property from object
    #'
    #' @param x character key to retrieve from object
    #' @return value from object at x
    get = function(x = "enadata") {
      return(private[[x]])
    },

  ## Public Properties ----
    rotation_dists = NULL,  #leave for now - to be removed for a temp variable
    enadata = NULL,
    points.raw = NULL,    #was data$raw
    points.normed.centered = NULL,    #was data$centered$normed
    points.rotated = NULL,    #was data$centered$rotated
    points.rotated.scaled = NULL,
    points.rotated.non.zero = NULL,
    line.weights = NULL,   #was data$normed
    line.weights.non.zero = NULL,
    line.weights.unrotated = NULL,
    node.positions = NULL,  #was nodes$positions$scaled
    codes = NULL,
    rotation.set = NULL,   ## new - ENARotation object
    correlations = NULL,   #not formerly listed, comes from optimized node positions in egr.positions
    variance = NULL,     #was self$data$centered$latent
    centroids = NULL,
    center.align.to.origin = TRUE,
    function.call = NULL,     #new - string reping function call
    function.params = list(   #list containing parameters function was called with
      norm.by = NULL,
      node.position.method = NULL,
      rotation.by = NULL,
      rotation.params = NULL,
      endpoints.only = NULL,
      center.align.to.origin = TRUE
    )
  ),

  private = list(

     ## Private Properties ----
     args = NULL,
     data.original = NULL,
     optim = NULL,

     #moved from public
     dimensions = 2,

     ## Private Functions ----
     run = function() {
       df = self$enadata$adjacency.vectors;

       # Backup of ENA data, this is not touched again.
       #private$data.original = df[,grep("adjacency.code", colnames(df)), with=F];
       private$data.original = df;

       # carry this out for node positioning
       self$function.params$center.align.to.origin = self$center.align.to.origin;

       # Copy of the original data, this is used for all
       # further operations. Unlike, `data.original`, this
       # is likely to be overwritten.
       self$points.raw = data.table::copy(private$data.original);

       ###
       # Normalize the raw data using self$function.params$norm.by,
       # which defaults to calling rENA::.sphere_norm
       ###
       self$line.weights = self$function.params$norm.by(self$points.raw);

       ###
       # Convert the string vector of code names to their corresponding
       # co-occurence names and set as colnames for the self$line.weights
       ##
       codeNames_tri = svector_to_ut(self$enadata$codes);

       colnames(self$line.weights) = codeNames_tri;
       # set the rownames to that of the original ENAdata file object
       rownames(self$line.weights) = rownames(df);

       attr(self$line.weights, opts$UNIT_NAMES) = attr(df, opts$UNIT_NAMES) #df[, .SD, with=T, .SDcols=self$enadata$get("unitsBy")];
       ###


       ###
       # Center the normed data
       # FIX - store as $data$centered
       ###
       #### ISSUE
       if (self$center.align.to.origin) {
         # only centers non-zero networks
         self$points.normed.centered = self$line.weights;

         non_zero_rows <- rowSums(as.matrix(self$line.weights)) != 0;
         self$points.normed.centered[non_zero_rows,] = center_data_c(self$line.weights[non_zero_rows,]);
       }
       else {
        self$points.normed.centered = center_data_c(self$line.weights);
       }
       colnames(self$points.normed.centered) = codeNames_tri;
       rownames(self$points.normed.centered) = rownames(df);
       attr(self$points.normed.centered, opts$UNIT_NAMES) = attr(self$enadata$adjacency.vectors.raw, opts$UNIT_NAMES)

       ###

       ###
       # Generate and Assign the rotation set
       ###
        if(is.function(self$function.params$rotation.by) && is.null(self$function.params$rotation.set)) {
          self$rotation.set = do.call(self$function.params$rotation.by, list(self, self$function.params$rotation.params));
        }
        else if (!is.null(self$function.params$rotation.set)) {
          if(is(self$function.params$rotation.set, "ENARotationSet")) {
            print("Using custom rotation.set.")

            self$rotation.set = self$function.params$rotation.set;
          } else {
            stop("Supplied rotation.set is not an instance of ENARotationSet")
          }
        }
        else {
          stop("Unable to find or create a rotation set")
        }
       ###

       ###
       # Generated the rotated points
       ###
        self$points.rotated = self$points.normed.centered %*% self$rotation.set$rotation;
        private$dimensions = min(private$dimensions, ncol(self$points.rotated))
        attr(self$points.rotated, opts$UNIT_NAMES) = attr(self$points.normed.centered, opts$UNIT_NAMES);
       ###

       ###
       # Calculate node positions
       #  - The supplied methoed is responsible is expected to return a list
       #    with two keys, "node.positions" and "centroids"
       ###
        if(!is.null(self$rotation.set) && is.null(self$function.params$rotation.set)) {
          positions = self$function.params$node.position.method(self);
          if(all(names(positions) %in% c("node.positions","centroids"))) {
            self$node.positions = positions$node.positions
            self$centroids = positions$centroids

            self$rotation.set$node.positions = positions$node.positions
          }
          else {
            stop(paste(
                "The node position method didn't return back the expected objects:",
                "\tExpected: c('node.positions','centroids')",
                paste("\tReceived: ", names(positions), sep=""),
                sep = "\n"
            ));
          }
        }
        else {
          if (!is.null(self$function.params$rotation.set) && !is.null(self$function.params$rotation.set$node.positions)) {
            self$node.positions = self$function.params$rotation.set$node.positions
          }
          else {
            stop("Unable to determine the node positions either by calculating
                  them using `node.position.method` or using a supplied
                  `rotation.set`");
          }
        }
       ###

       ###
       # Variance
       ###
       variance.of.rotated.data = var(self$points.rotated)
       diagonal.of.variance.of.rotated.data = as.vector(diag(variance.of.rotated.data))
       self$variance = diagonal.of.variance.of.rotated.data/sum(diagonal.of.variance.of.rotated.data)

       return(self);
     }
   )
)
