# ##
# # @title Accumulate and Generate
# #
# # @description Accumulate and Generate
# #
# # @details [TBD]
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
#   ...
# ) {
#   args = list(...);
#   conversations.used = NULL;
#   weight.by = "binary";
#   if(!is.null(args$conversations.used)) {
#     conversations.used = args$conversations.used
#     file$KEYCOL = merge_columns_c(file,conversations.by)
#     file = file[file$KEYCOL %in% conversations.used,]
#   }
#   if(!is.null(args$weight.by)) {
#     weight.by = args$weight.by
#   }
#
#   accum = ena.accumulate.data.file(
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
#     rotate.meta = accum$metadata[accum$metadata$ENA_UNIT %in% accum$unit.names,]
#     rotate.col = accum$metadata[accum$metadata$ENA_UNIT %in% accum$unit.names,][[names(args$rotate.by)[1]]]
#     rotate.groups = list(
#       rotate.col == args$rotate.by[[1]][1],
#       rotate.col == args$rotate.by[[1]][2]
#     )
#   }
#   set = ena.make.set(
#     enadata = accum,
#     norm.by = ifelse((is.null(args$sphere.norm) || args$sphere.norm==T),sphere_norm_c,dont_sphere_norm_c),
#     rotation.by = if(is.null(rotate.groups)) ena.svd else ena.rotate.by.mean, #ifelse(is.null(rotate.groups), NULL, ena.rotate.by.mean),
#     rotation.params = rotate.groups,
#     ...
#   )
#
#   group.names = NULL;
#   if(length(units.by)>1) {
#     group.names = unique(set$enadata$units[[units.by[[1]]]])
#   } else {
#     group.names = units.by
#   }
#   group.cnt = length(group.names);
#   conf.ints = list();
#   outlier.ints = matrix(0, nrow=(group.cnt), ncol=(2));
#
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
#     set$points.rotated = set$points.rotated * scaleFactor;
#   }
#
#   groups = NULL
#   group.method = "mean"
#   if(!is.null(args$weight.network.by) && (args$weight.network.by %in% c("mean","sum"))) {
#     group.method = args$weight.network.by;
#   }
#   group.by = NULL;
#   if(length(units.by)>1) {
#     group.by = set$enadata$units[[units.by[[1]]]];
#     groups = ena.group(set, group.by, method = "mean") #group.method)
#   } else {
#     group.by = rep(T, length(units.by));
#     groups = ena.group(set, group.by, method = "mean"); #group.method);
#     groups$names = units.by;
#   }
#   rle = rle(as.vector(group.by));
#   groups$rle = list( lengths = rle$lengths, values = rle$values );
#   groups$line.weights = as.matrix(groups$line.weights)
#   if(group.method == "sum") {
#     groups$line.weights = groups$line.weights * rle$lengths;
#     groups$line.weights = scales::rescale(groups$line.weights, c(0,1));
#   }
#
#   cis = lapply(as.character(unique(set$enadata$units[[units.by[[1]]]])), function(x) {
#     pntRows = as.matrix(rep(T, nrow(set$points.rotated)))
#     if(length(units.by)>1) {
#       pntRows = as.data.frame(set$enadata$units[[units.by[[1]]]]) == x;
#     }
#     pnts = as.matrix(set$points.rotated[pntRows,])
#     dim(pnts) = c(length(which(pntRows)),2)
#     ci = t(matrix(c(
#         tryCatch(t.test(pnts[, 1], conf.level = 0.95), error = function(e) list(conf.int = c(NA,NA)))$conf.int,
#         tryCatch(t.test(pnts[, 2], conf.level = 0.95), error = function(e) list(conf.int = c(NA,NA)))$conf.int
#         # as.numeric(t.test(pnts[,1], conf.level = 0.95)$conf.int),
#         # as.numeric(t.test(pnts[,2], conf.level = 0.95)$conf.int)
#       ), nrow=2));
#     oi = c(IQR(pnts[,1]), IQR(pnts[,2])) * 1.5
#     list(ci = ci, oi = oi)
#   });
#   for(n in 1:length(group.names)) {
#     conf.ints[[n]] = cis[[n]]$ci
#     outlier.ints[n, ] = cis[[n]]$oi
#   }
#
#   groups$line.weights = as.matrix(groups$line.weights)
#
#   groups$edge.saturation = scales::rescale(groups$line.weights, c(0.25,1));
#   groups$edge.opacity = scales::rescale(groups$line.weights, c(0.3,1));
#
#   colnames(groups$line.weights) = NULL
#   groups$conf.ints = conf.ints;
#   groups$outlier.ints = outlier.ints;
#
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
#
#     # browser()
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
#         weights[x,node.rows==mat[2,i]] = weights[x,node.rows==mat[2,i]] + network.thickness[i];
#       }
#     }
#
#     #weights = t(apply(weights, 1, scales::rescale, c(1,ncol(weights))));
#     weights = scales::rescale(weights, c(1,ncol(weights)));
#
#     set$line.weights[estimate.over.units,] = set$line.weights[estimate.over.units,];
#     set$line.weights[!estimate.over.units,] = 0
#     scaleRange = c(min(set$line.weights[estimate.over.units,]) ,1);
#     set$line.weights = scales::rescale(set$line.weights, to=scaleRange, from=range(set$line.weights, na.rm = T, finite = T))
#
#     codedRow1 = code[triIndices(length(code), 0)[,1]+1];
#     codedRow2 = code[triIndices(length(code), 1)[,1]+1];
#     return(list(
#       codes = code,
#       adjacency.matrix = rbind(codedRow1, codedRow2),
#       set = set,
#       groups = groups,
#       scaled = scale.nodes,
#       node.sizes = weights,
#       esitmated.over = args$units.exclude,
#       edge.saturation = scales::rescale(set$line.weights, c(0.25,1)),
#       edge.opacity = scales::rescale(set$line.weights, c(0.3,1))
#     ));
#   }
# }
