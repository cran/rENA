####
#' ENAdata R6class
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @export
#'
#' @field raw A data frame constructed from the unit, convo, code, and metadata parameters of ena.accumulate.data
#' @field adjacency.vectors A data frame of adjacency (co-occurrence) vectors by row
#' @field accumulated.adjacency.vectors A data frame of adjacency (co-occurrence) vectors accumulated per unit
#' @field model The type of ENA model: EndPoint, Accumulated Trajectory, or Separate Trajectory
#' @field units A data frame of columns that were combined to make the unique units. Includes column for trajectory selections. (unique)
#' @field unit.names A vector of unique unit values
#' @field metadata A data frame of unique metadata for each unit
#' @field trajectories A list: units - data frame, for a given row tells which trajectory it's a part; step - data frame, where along the trajectory a row sits
#'
#' @field codes A vector of code names
#' @field function.call The string representation of function called and parameters provided
#' @field function.params A list of all parameters sent to function call
####
ENAdata = R6::R6Class("ENAdata", public = list(

  ####
  ## Constructor - documented in main class declaration
  ####
  initialize = function(
    file,    #csv or data frame containing units, codes, conversations, and metadata
    units = NULL,     #data frame of unit columns and values
    units.used = NULL,    #vector of unit values to include (a subset of rows of the units df)
    units.by = NULL,  # unit col names that will be grouped by to determine ENA_UNIT
    conversations.by = NULL,   # conversation col names that will be grouped by to determine conversations
    codes = NULL,  #vector of code column names to use in accumulation
    model = NULL,
    weight.by = "binary",
    window.size.back = 1,
    window.size.forward = 0,
    # units.selected = NULL,
    # units.exclude = c(),
    mask = NULL,
    ...
  ) {
    self$function.call <- sys.call(-1);
    private$file <- file;
    self$units <- units;
    private$units.used <- units.used;
    private$units.by <- units.by
    private$conversations.by <- conversations.by;
    self$codes <- codes;

    if(is.data.frame(self$codes)) self$codes <- colnames(self$codes);

    private$weight.by <- weight.by;
    private$window.size <- list(
      "back" = window.size.back,
      "forward" = window.size.forward
    );
    #private$units.exclude <- units.exclude;
    self$model <- model;

    private$mask <- mask;
    # private$trajectory.by <- conversations.by;
    private$loadFile();

    self
  },

    ####
    ## Public Properties
    ####
      model = NULL,
      raw = NULL,
      adjacency.vectors = NULL,
      adjacency.matrix = NULL,
      accumulated.adjacency.vectors = NULL,
      adjacency.vectors.raw = NULL,
      units = NULL,
      unit.names = NULL,
      metadata = NULL,
      trajectories = list(
        units = NULL,
        step = NULL
      ),
      codes = NULL,
      function.call = NULL,
      function.params = NULL,
    ####
    ## END: Public Properties
    ####

    ####
    ## Public Functions
    ####

      ####
      # \code{get()} - Return a read-only property
      # \preformatted{  Example:
      #     get( x = 'file' )}
      # \preformatted{  Parameters:
      #      x - Property to return. Defaults to 'file', returning the original data}
      ####
      get = function(x = "data") {
        return(private[[x]])
      },

      ####
      # \code{read()} - Return the accumulated data
      # \preformatted{  Example:
      #     get( colnames = T, sep = " & " )}
      # \preformatted{  Parameters:
      #      colnames - Logical, whether to replace colnames with their names values from the adjacency (co-occurrence)
      #      sep - String to use as a seperator in the updated column names. Ignored if colnames == F}
      ####
      read = function(colnames = T, sep = " & ") {
        namedData = data.table::copy(self$adjacency.vectors);
        if(colnames == T) {
          namedRows = attr(namedData, "adjacency.matrix");
          colnames(namedData)[grep("adjacency.code",colnames(namedData))] = apply(namedRows, 2, function(x) paste(x[1], x[2], sep=sep))
        }
        namedData
      },

      ####
      # \code{update()} - Change any of the allowed properties then reprocess the ENAdata.
      # \preformatted{  Example:
      #     update(
      #       file = private$file,
      #       codes = self$codes,
      #       conversations.by = private$conversations.by,
      #       units = self$units,
      #       unitsSelected = private$unitsSelected,
      #       windowSize = private$windowSize,
      #       reload = FALSE
      #       ...
      #     )}
      #
      # \preformatted{  Parameters:
      #     file - The original data to accumulate, as a data.frame or data.table
      #     codes - String vector of column names to use as codes
      #     conversations.by - String vector of column names to create the conversations
      #     units - String vector of which units to include in the ENAset
      #      windowSize - Integer used to select the size of each stanza window within a conversation
      #     reload - Logical, force reloading of the ENAdata object}
      ####
      update = function(
        file = private$file,
        codes = self$codes,
        conversations.by = private$conversations.by,
        units = self$units,
        #units.exclude = private$units.exclude,
        windowSize = private$window.size,
        reload = F
      ) {
        if(all.equal.raw(file, private$file) == FALSE) {
          private$file <- file; reload = T;
        }
        if( identical(codes, self$codes) == F ) {
          self$codes <- codes; reload = T;
        }
        if( is.null(units) || !all(units == self$units) ) {
          self$units <- units; reload = T;
        }
        # if( identical(units.exclude, private$units.exclude) == F ) {
        #   private$units.exclude <- units.exclude; reload = T;
        # }
        if( is.null(conversations.by) || !all(conversations.by == private$conversations.by) ) {
          private$conversations.by <- conversations.by; reload = T;
        }
        if( identical(windowSize, private$window.size) == F) {
          private$window.size = windowSize; reload = T;
        }

        if(reload == T) self$data <- private$loadFile();

        return(self);
      },

      add.metadata = function(merge = F) {
        ### get columns which arent in codes, units.by, or conversations.by
        metaAvail=colnames(self$raw)[-which(colnames(self$raw) %in% c(self$codes, private$units.by, private$conversations.by))];
        ### delimit possible metadata to only columns with one value per unit
        dfDT.meta.poss = self$raw[, { nc = lapply(.SD, function(x) length(unique(x))); }, by=c(private$units.by), .SDcols=c(metaAvail)][,,.SDcols=metaAvail];
        metaAvail = colnames(dfDT.meta.poss)[rapply(dfDT.meta.poss, function(x) all(x == 1))]
        metaAvail = metaAvail[metaAvail != "ENA_UNIT"];

        raw.meta = self$raw[!duplicated(ENA_UNIT)][ENA_UNIT %in% unique(self$accumulated.adjacency.vectors$ENA_UNIT),c("ENA_UNIT",private$units.by,private$conversations.by, metaAvail),,with=F];

        df.to.return = NULL;
        if(merge == T) {
          df.to.return = merge(self$adjacency.vectors, raw.meta[,unique(colnames(raw.meta)),with=F], by=c("ENA_UNIT"), suffixes=c("",".y"), sort=F)
        } else {
          df.to.return = raw.meta; #merge(self$adjacency.vectors[,c("ENA_UNIT", private$trajectory.by),with=F],raw.meta,by=c("ENA_UNIT"), suffixes=c("","y"))
        }

        #attr(df.to.return, opts$UNIT_NAMES) = df.to.return[,  .SD ,with=T,.SDcols=c(private$units.by,private$trajectory.by)];
        #self$adjacency.vectors[,  .SD ,with=T,.SDcols=c(private$units.by,private$trajectory.by)]

        df.to.return
      },
      print = function(...) {
        args = list(...);
        fields = NULL;
        to.print = list();
        if(is.null(args$fields)) {
          fields = names(get(class(self))$public_fields)
        } else {
          fields = args$fields
        }
        for(f in fields) {
          to.print[[f]] = self[[f]]
        }
        return(to.print);
      }
    ####
    ## END: Public Functions
    ####
  ),

  ####
  ### Private
  ####
  private = list(

    ####
    ## Private Properties
    ####
      file = NULL,
      window.size = NULL,
      units.used = NULL,
      units.by = NULL,
      conversations.by = NULL,
      weight.by = NULL,
      #units.exclude = NULL,
      trajectory.by = NULL,
      mask = NULL,
    ####
    ## END: Private Properties
    ####

    ####
    ## Private Functions
    ####
    loadFile = function() {
      if(any(class(private$file) == "data.table")) {
        df_DT = private$file;
      } else {
        if(any(class(private$file) == "data.frame")) {
          df = private$file;
        } else {
          df = read.csv(private$file);
        }
        df_DT = data.table::as.data.table(df);
      }

      self$raw = df_DT;
      self$raw$ENA_UNIT = merge_columns_c(self$raw,private$units.by);

      self = accumulate.data(self);

      self$units = self$adjacency.vectors[,private$units.by, with=F];

      if(!self$model %in% c("AccumulatedTrajectory","SeparateTrajectory")) {
        self$unit.names <- self$adjacency.vectors$ENA_UNIT;
      } else {
        self$trajectories$units <- self$units;
        conversation = self$adjacency.vectors[,private$conversations.by, with=F];
        self$trajectories$step <- conversation;
        self$units <- cbind(self$units, conversation);

        self$unit.names <- paste(self$adjacency.vectors$ENA_UNIT, self$adjacency.vectors$TRAJ_UNIT, sep = ".");

      }

      # save raw adjacency vectors prior to corrections
      self$adjacency.vectors.raw = self$adjacency.vectors;

      adjCols = colnames(self$adjacency.vectors)[grep("adjacency.code", colnames(self$adjacency.vectors))];
      if(is.null(private$mask)) {
        private$mask = matrix(1, nrow=length(self$codes), ncol=length(self$codes), dimnames=list(self$codes,self$codes))
      }
      self$adjacency.vectors[,c(adjCols)] =
        self$adjacency.vectors[,c(adjCols),with=F] *
        rep(private$mask[upper.tri(private$mask)], rep(nrow(self$adjacency.vectors),length(adjCols)))

      if(is.function(private$weight.by)) {
        cols = colnames(self$adjacency.vectors)[grep("adjacency.code", colnames(self$adjacency.vectors))];
        self$adjacency.vectors[, (cols) := lapply(.SD, private$weight.by), .SDcols = cols];
      }

      self$metadata = self$add.metadata(merge = F);

      ### remove non-adjacency vector columns from adj.vecs
      ####### idea: if those cols are needed later - use adjacency.vectors.raw
      self$adjacency.vectors = self$adjacency.vectors[,grep("adjacency.code", colnames(self$adjacency.vectors)), with=F]

      return(self);
    }
  )
)
