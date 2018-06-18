# # ##
# # # @title Accumulate and Generate
# # #
# # # @description Accumulate and Generate
# # #
# # # @details [TBD]
# # #
# # # @param file [TBD]
# # # @param window.size.back [TBD]
# # # @param units.by [TBD]
# # # @param conversations.by [TBD]
# # # @param code [TBD]
# # # @param units.used [TBD]
# # # @export
# # # @return list containing the accumulation and set
# # ##
# # ena.generate <- function(
# #   file,
# #   window.size.back,
# #   units.by,
# #   conversations.by,
# #   code,
# #   scale.nodes = T,
# #   units.used = NULL,
# #   ...
# # ) {
# #   args = list(...);
# #   conversations.used = NULL;
# #   weight.by = "binary";
# #   if(!is.null(args$conversations.used)) {
# #     conversations.used = args$conversations.used
# #     file$KEYCOL = merge_columns_c(file,conversations.by)
# #     file = file[file$KEYCOL %in% conversations.used,]
# #   }
# #   if(!is.null(args$weight.by)) {
# #     weight.by = args$weight.by
# #   }
# #
# #   accum = ena.accumulate.data.file(
# #     file = file,
# #     window.size.back = window.size.back,
# #     units.by = make.names(units.by),
# #     units.used = units.used,
# #     model = "EndPoint",
# #     conversations.by = make.names(conversations.by),
# #     codes = make.names(code),
# #     ...
# #   )
# #
# #   rotate.groups = NULL
# #   if(!is.null(args$rotate.by)) {
# #     rotate.meta = accum$metadata[accum$metadata$ENA_UNIT %in% accum$unit.names,]
# #     rotate.col = accum$metadata[accum$metadata$ENA_UNIT %in% accum$unit.names,][[names(args$rotate.by)[1]]]
# #     rotate.groups = list(
# #       rotate.col == args$rotate.by[[1]][1],
# #       rotate.col == args$rotate.by[[1]][2]
# #     )
# #   }
# #   set = ena.make.set(
# #     enadata = accum,
# #     norm.by = ifelse((is.null(args$sphere.norm) || args$sphere.norm==T),sphere_norm_c,dont_sphere_norm_c),
# #     rotation.by = if(is.null(rotate.groups)) ena.svd else ena.rotate.by.mean, #ifelse(is.null(rotate.groups), NULL, ena.rotate.by.mean),
# #     rotation.params = rotate.groups,
# #     ...
# #   )
# #
# # @param file [TBD]
# # @param window.size.back [TBD]
# # @param units.by [TBD]
# # @param conversations.by [TBD]
# # @param code [TBD]
# # @param units.used [TBD]
# # @export
# # @return list containing the accumulation and set
# ##
# ena.generate <- function(
#   file,
#   window.size.back,
#   units.by,
#   conversations.by,
#   code,
#   scale.nodes = T,
#   units.used = NULL,
#   dimensions = 6,
#   ...
# ) {
#   args = list(...);
#   unit.groups = NULL;
#   conversations.used = NULL;
#   weight.by = "binary";
#   if(!is.null(args$conversations.used)) {
#     conversations.used = args$conversations.used
#     file$KEYCOL = rENA:::merge_columns_c(file, make.names(conversations.by))
#     file = file[file$KEYCOL %in% conversations.used,]
#   }
#   if(!is.null(args$weight.by)) {
#     weight.by = args$weight.by;
#   }
#   if(!is.null(args$unit.groups)){
#     unit.groups = list();
#     group.json = jsonlite::fromJSON(args$unit.groups)
#     for(grp in 1:length(group.json$name)) {
#       unit.groups[group.json$name[grp]] = group.json$units[grp];
#     }
#   }
#   accum = rENA:::ena.accumulate.data.file(
#     file = file,
#     window.size.back = window.size.back,
#     units.by = make.names(units.by),
#     units.used = units.used,
#     model = "EndPoint",
#     conversations.by = make.names(conversations.by),
#     codes = make.names(code),
#     ...
#   )
#
#   rotate.groups = NULL
#   if(!is.null(args$rotate.by)) {
#     # rotate.meta = accum$metadata[accum$metadata$ENA_UNIT %in% accum$unit.names,]
#     # rotate.col = accum$metadata[accum$metadata$ENA_UNIT %in% accum$unit.names,][[make.names(names(args$rotate.by)[1])]]
#     # rotate.groups = list(
#     #   rotate.col == args$rotate.by[[1]][1],
#     #   rotate.col == args$rotate.by[[1]][2]
#     # )
#     rotate.groups = lapply(args$rotate.by, function(x) accum$unit.names %in% x )
#   }
#
#   use.to.norm = rENA:::dont_sphere_norm_c;
#   if(is.null(args$sphere.norm) || args$sphere.norm == T) {
#     use.to.norm = rENA:::sphere_norm_c
#   }
#   rotation.set = NULL
#   if(!is.null(args$rotation.matrix)) {
#     rotation.set = ENARotationSet$new(
#       rotation = args$rotation.matrix$rotation$rotation,
#       node.positions = args$rotation.matrix$rotation$node.positions,
#       codes = args$rotation.matrix$codes
#     );
#   }
#   set = rENA::ena.make.set(
#     enadata = accum,
#     norm.by = use.to.norm,
#     rotation.by = if(is.null(rotate.groups)) rENA:::ena.svd else rENA:::ena.rotate.by.mean,
#     rotation.params = rotate.groups,
#     rotation.set = rotation.set,
#     dimensions = dimensions,
#     ...
#   )
#
#   tryCatch(set$correlations <- ena.correlations(set, dims=c(1:2)));
#
#   group.names = NULL;
#   if(length(units.by)>1) {
#     group.names = unique(set$enadata$units[[make.names(units.by)[[1]]]])
#   } else {
#     group.names = units.by
#   }
#   group.cnt = length(group.names);
#   conf.ints = list();
#   outlier.ints = matrix(0, nrow=(group.cnt), ncol=(2));
#
#   set$points.rotated.scaled = set$points.rotated;
#   scaleFactor = 1.0
#   if(scale.nodes == T) {
#     np.min.x = min(set$node.positions[,1])
#     np.min.y = min(set$node.positions[,2])
#     rp.min.x = min(set$points.rotated[,1])
#     rp.min.y = min(set$points.rotated[,2])
#     maxMin = abs(max(np.min.x / rp.min.x, np.min.y / rp.min.y))
#
#     np.max.x = max(set$node.positions[,1])
#     np.max.y = max(set$node.positions[,2])
#     rp.max.x = max(set$points.rotated[,1])
#     rp.max.y = max(set$points.rotated[,2])
#     maxMax = abs(max(np.max.x / rp.max.x, np.max.y / rp.max.y))
#     scaleFactor = min(maxMin, maxMax)
#     # set$points.rotated = set$points.rotated * scaleFactor;
#     set$points.rotated.scaled = set$points.rotated * scaleFactor;
#   }
#
#   # groups = NULL
#   group.method = "mean"
#   if(!is.null(args$weight.network.by) && (args$weight.network.by %in% c("mean","sum"))) {
#     group.method = args$weight.network.by;
#   }
#
#   groups = list()
#   # group.by = NULL;
#   # if(length(units.by)>1) {
#   #   group.by = as.vector(set$enadata$units[[make.names(units.by)[[1]]]]);
#   #   grps = as.character(unique(set$enadata$units[[make.names(units.by[[1]])]]))
#   #   groups = lapply(grps, function(x) { ena.unit.group(set, set$enadata$unit.names[group.by == x], name = x, method = group.method, scaleFactor = scaleFactor) })
#   # #   groups = ena.group(set, group.by, method = "mean") #group.method)
#   # #   groups$points = as.matrix(groups$points[, colnames(groups$points) != "ENA_GROUP_NAME"][as.character(groups$names),])
#   # #   groups$line.weights = as.matrix(groups$line.weights[,colnames(groups$line.weights) != "ENA_GROUP_NAME"][as.character(group.names),]);
#   # } else {
#   #   # group.by = as.vector(rep(T, length(units.by)));
#   #   groups = list(ena.unit.group(set, set$enadata$unit.names, name = units.by[[1]], method = group.method, scaleFactor = scaleFactor));
#   # #   groups = ena.group(set, group.by, method = "mean"); #group.method);
#   # #   groups$names = units.by;
#   # #   groups$points = matrix(as.numeric(groups$points),nrow=1);
#   # #   groups$line.weights = matrix(as.numeric(groups$line.weights),nrow=1);
#   # }
#
#   # rle = rle(as.vector(group.by));
#   # groups$rle = list( lengths = rle$lengths, values = rle$values );
#   # if(group.method == "sum") {
#   #   groups$line.weights = groups$line.weights * rle$lengths;
#   #   groups$line.weights = scales::rescale(groups$line.weights, c(0.1,1));
#   # }
#   # cis = lapply(as.character(unique(set$enadata$units[[make.names(units.by[[1]])]])), function(x) {
#   #   pntRows = as.matrix(rep(T, nrow(set$points.rotated)))
#   #   if(length(units.by)>1) {
#   #     pntRows = as.data.frame(set$enadata$units[[make.names(units.by[[1]])]]) == x;
#   #   }
#   #   pnts = as.matrix(set$points.rotated[pntRows,])
#   #   dim(pnts) = c(length(which(pntRows)),ncol(set$points.rotated))
#   #   ci = matrix(NA, ncol=2,nrow=2)
#   #   oi = rep(NA, 2)
#   #   if(nrow(pnts) > 1) {
#   #     ci = t(matrix(c(
#   #         tryCatch(t.test(pnts[, 1], conf.level = 0.95), error = function(e) list(conf.int = c(NA,NA)))$conf.int,
#   #         tryCatch(t.test(pnts[, 2], conf.level = 0.95), error = function(e) list(conf.int = c(NA,NA)))$conf.int
#   #       ), nrow=2));
#   #     oi = c(IQR(pnts[,1]), IQR(pnts[,2])) * 1.5
#   #   }
#   #   list(ci = ci, oi = oi)
#   # });
#   # for(n in 1:length(group.names)) {
#   #   conf.ints[[n]] = cis[[n]]$ci
#   #   outlier.ints[n, ] = cis[[n]]$oi
#   # }
#   # groups$line.weights = as.matrix(groups$line.weights)
#   # groups$edge.saturation = scales::rescale(groups$line.weights, c(0.25,1));
#   # groups$edge.opacity = scales::rescale(groups$line.weights, c(0.3,1));
#   # colnames(groups$line.weights) = NULL
#   # groups$conf.ints = conf.ints;
#   # groups$outlier.ints = outlier.ints;
#
#   if(!is.null(unit.groups) && length(unit.groups) > 0){
#     for(i in 1:length(names(unit.groups))) {
#       groups[[length(groups)+1]] = ena.unit.group(set, set$enadata$unit.names[set$enadata$unit.names %in% unit.groups[[i]]], name = names(unit.groups)[i], method = group.method, scaleFactor = scaleFactor)
#     }
#   }
#   if(
#     !is.null(args$output) && args$output == "save" &&
#     !is.null(args$output.to)
#   ) {
#     setName = tools::file_path_sans_ext(basename(args$output.to))
#     env = environment()
#     assign(x = setName, value = set, envir = env);
#     env[[setName]] = get(x = setName, envir = env)
#
#     tmp <- tempfile(fileext = ".rdata")
#     on.exit(unlink(tmp))
#     save(list = c(setName), file = tmp, envir = env)
#     bucket <- aws.s3::get_bucketname(args$output.to)
#     object <- aws.s3:::get_objectkey.character(args$output.to)
#     return(aws.s3::put_object(file = tmp, bucket = bucket, object = object));
#   }
#   else {
#     nodes = data.frame(set$node.positions);
#     nodes$weight = rep(0, nrow(nodes))
#     node.rows = rownames(set$node.positions);
#     estimate.over.units = (!(set$enadata$unit.names %in% args$units.exclude))
#     weights = matrix(0, ncol=nrow(set$node.positions), nrow=length(which(estimate.over.units)));
#
#     colnames(weights) = node.rows
#     network.scaled = set$line.weights[estimate.over.units,];
#     # if(!is.null(scale.weights) && scale.weights == T) {
#     #   network.scaled = network.scaled * (1 / max(abs(network.scaled)));
#     # }
#
#     mat = set$enadata$adjacency.matrix;
#     for (x in 1:nrow(network.scaled)) {
#       network.thickness = network.scaled[x,] #scales::rescale(abs(network.scaled[x,]), thickness);
#       for (i in 1:ncol(mat)) {
#         weights[x,node.rows==mat[1,i]] = weights[x,node.rows==mat[1,i]] + network.thickness[i];
#       }
#     }
#
#     # #weights = t(apply(weights, 1, scales::rescale, c(1,ncol(weights))));
#     weights = scales::rescale(weights, c(1,ncol(weights)));
#
#     set$line.weights[estimate.over.units,] = set$line.weights[estimate.over.units,];
#
#     # If not included, remove the weights as to not effect the scaling
#     set$line.weights[!estimate.over.units,] = 0
#     scaleRange = c(min(set$line.weights[estimate.over.units,]) ,1);
#     if(scaleRange[1] < 0.1 && min(set$line.weights)>0) {
#       scaleRange[1] = 0.1;
#     }
#
#     set$line.weights = scales::rescale(set$line.weights, to=scaleRange, from=range(set$line.weights, na.rm = T, finite = T))
#     # adjRows = triIndices(length(code)) + 1
#     # codedRow1 = code[adjRows[1,]];
#     # codedRow2 = code[adjRows[2,]];
#
#     tmp = getwd();
#     sess = regexec("temp/(x[^/]*)/workspace", tmp)[[1]]
#     assign("set", set, envir = parent.frame())
#
#     dimension.names = paste("SVD",1:ncol(set$points.rotated), sep="")
#     if(length(set$function.params$rotation.params) == 2) dimension.names[1] = "MR1"
#     # if(length(args$plotted.nodes) == 2) {
#     #   methods = ena.methods(enaset = set, tool = "webENA", tool.version = "0.1.0", comparison = "parametric", comparison.groups = args$plotted.nodes)
#     # } else {
#     #   methods = ena.methods(enaset = set, tool = "webENA", tool.version = "0.1.0")
#     # }
#     return(list(
#       codes = make.names(code),
#       adjacency.matrix = mat, #rbind(codedRow1, codedRow2),
#       set = set,
#       # methods = readChar(methods, file.info(methods)$size),
#       custom.rotation = if(!is.null(rotation.set)) T else F,
#       custom.rotation.set = rotation.set,
#       dimensions = dimension.names, #colnames(set$points.rotated),
#       session = substr(tmp, start=sess[2], stop=sess[2]+attr(sess, "match.length")[2]-1),
#       # groups = groups,
#       groups = groups,
#       scaled = scale.nodes,
#       node.sizes = weights,
#       esitmated.over = args$units.exclude,
#       edge.saturation = scales::rescale(set$line.weights, c(0.25,1)),
#       edge.opacity = scales::rescale(set$line.weights, c(0.3,1))
#     ));
#   }
# }
