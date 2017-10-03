#Perform LDA,SVM, and mean rotation Plus SVD on an ENA set given a set of groups
#Input
# enaSet       - An ENA set
# groups       - A 3 by k matrix which defines the grouping.
#                The first element in a row defines the column from the
#                enaSet$set.information$metadata matrix to use for grouping.
#                The second and third elements define the groups.
# rotationList - a vector of length k that specifies the type of rotations
#                e. g. c('lda','svm','mean')
# meta_vec - vector of length K giving the group membership of the rows of data
#Output
# Q - A square, orthogonal matrix whose first k columns are the LDA,SVM,
#      and mean vectors. The remaining columns are the SVD vectors.
#
# Examples
#   meta =
#     user_id condition
#           1         A
#           2         B
#
#   meta_vec = c("A", "B")
#   groups =
#     NA
#      A
#      B

rotationPlusSVDWrapper = function(X, meta_vec, groups = NA, rotationList = 'mean') {
  if (all(is.na(groups))) {
	groups =
	  unique(meta_vec)
	if (length(groups) != 2)
	  stop('if not giving groups, must be 2 unique vals in meta_vec')
  }

  if (length(unique(meta_vec)) > 2 & !( class(groups) %in% c('data.frame', 'matrix'))) {
	  X = X[meta_vec %in% groups, ]
	  meta_vec = meta_vec[meta_vec %in% groups]
  }

  groupingMat = groups
  # :::::: argument validation :::::: #
  if(class(groups) != 'data.frame' & class(groups) != 'matrix')
		 groupingMat = matrix(c(NA, groupingMat[1], groupingMat[2]), ncol = 1)

  if(class(meta_vec)!='matrix'& class(meta_vec)!="data.frame")
	  meta_vec = matrix( meta_vec, ncol=1)

  if (length(rotationList) != ncol(groupingMat))
    stop(paste('Matrix of grouping information has', as.character(ncol(groupingMat)),
           'columns, but the rotation list only has length ', as.character(length(rotationList))))

  # use the centered X matrix
  X = scale(X, scale=F, center=T)
  # get one row from the source X for each unit
  # rawData = enaSet$set.information$metaX[rownames(enaSet$meta),]
  # get the list of grouping information

  #get weight vectors
  weights = matrix(0, nrow = ncol(X), ncol = ncol(meta_vec))

  for (i in 1:ncol(meta_vec)) {
    group = matrix(meta_vec[,i],ncol=1)
    selectedRows = !is.na(group)  # get the units that are assigned to a group
    group = group[selectedRows,]
    Y = X[selectedRows,]
    # select the method for this iteration
    if (rotationList[i] == 'lda') {
      weights[,i] = ulda(Y,group)
    } else if (rotationList[i] == 'svm') {
      #weights[,i] = linearSvm(Y,group)
    } else if (rotationList[i] == 'mean') {
      weights[,i] = meanRotation(Y,group)
    }
    # deflate the last weight vector from the X matrix
    X = X - (X %*% weights[,i])%*%t(weights[,i])
  }

  # fill in the rotation matrix with singular vectors orthogonal to the
  # weight vectors
  W = orthogonalSVD(X,weights)
  colnames(W) = c(paste(rotationList,as.character(1:ncol(weights)), sep=' '),
                  paste('svd',as.character(1:(ncol(W)-ncol(weights))), sep=' '))
  set = list( w = X, Q = W, mean.rot = NULL)

  total.var = sum( apply(set$w, 2, var))

  set$w.c = scale(set$w, scale = F, center = T)

  set$t = set$w.c %*% set$Q

  set$percent = apply(set$t, 2, var) / total.var * 100

  return (set)
}

#Create a list of vectors that define groups for LDA,SVM, and mean rotations
#Input
# rawData - enaSet$set.information$metadata matrix
# groups - A 3 by k matrix which defines the grouping.
#          The first element in a row defines the column from the
#          enaSet$set.information$metadata matrix to use for grouping.
#          The second and third elements define the groups.
#Output
# grouping - a list of vectors that define groups
getGroupList = function(rawData, groups) {
  grouping = list()
  for (i in 1:ncol(groups)) {
    groupName = groups[1,i]
    #Check which rows are included in the current group
    inGroup = is.element(rawData[,groupName], groups[2:3,i])
    # NA signifies units that are not in a group
    grouping[[i]] = rep(NA, length(inGroup))
    grouping[[i]][inGroup] = rawData[inGroup,groupName]
  }
  return (grouping)
}

# This function returns right singular vectors of the transformed data
# matrix which are orthogonal to the columns of W
#Input
# data - the data matrix
# W    - matrix used to transform the data matrix
#Output
# V    - right singular vectors of the transformed data matrix
orthogonalSVD = function(data,W) {
  if(class(data) != "matrix"){
    message("orthogonalSVD:  converting data to matrix")
    data = as.matrix(data)
  }
  #Find the orthogonal transformation that includes W
  Q = qrOrtho(W)
  X.bar = data%*%Q[,(ncol(W)+1):ncol(Q)]
  V = prcomp(X.bar, scale.=F)$rotation
  if (class(V)=="numeric") {
    V = matrix(V,nrow=length(V))
  }
  return (cbind(Q[,1:ncol(W)],Q[,(ncol(W)+1):ncol(Q)]%*%V))
}

# Extend a set of vectors of length m to form a basis of R^m
# by computing the complete QR factorization of the input matrix.
#Input
# A   - A matrix whose columns represent a set of vectors
#Output
# out - The orthogonal matrix Q from the QR factorization of A
qrOrtho = function(A) {
  return(qr.Q(qr(A),complete=T))
}

#@article{Ye:2006:CTA:1248547.1248590,
# author = {Ye, Jieping and Xiong, Tao},
# title = {Computational and Theoretical Analysis of Null Space and Orthogonal Linear Discriminant Analysis},
# journal = {J. Mach. Learn. Res.},
# issue_date = {12/1/2006},
# volume = {7},
# month = dec,
# year = {2006},
# issn = {1532-4435},
# pages = {1183--1204},
# numpages = {22},
# url = {http://dl.acm.org/citation.cfm?id=1248547.1248590},
# acmid = {1248590},
# publisher = {JMLR.org},
#}

# This function performs uncorrelated linear discriminant analysis on an data matrix with
# m samples in n variables and k groups. For m > n, k-1 vectors are returned.
# For m < n, 2(k-1) vectors are returned.
#Input
# data     - m x n data matrix with m samples of n variables
# grouping - column vector of length m with group information
# tol      - Singular values with absolute values less than tol are
#            considered zero
#Output
# out - a matrix containing the discriminant vectors
ulda = function (data,grouping,tol=1e-12) {
  #get groups
  groups = levels(factor(grouping))
  #get global centroid
  mu = colMeans(data)
  #hold within group scatter
  Hw = matrix(NA,nrow=dim(data)[2],ncol=0)
  #hold between group scatter
  Hb = matrix(NA,nrow=dim(data)[2],ncol=0)
  for (i in 1:length(groups)) {
    leveli.rows = grouping == groups[i]
    data.leveli = data[leveli.rows,]
    #get within group centroid
    mui = colMeans(data.leveli)
    Hb = cbind(Hb,sqrt(dim(data.leveli)[1])*(mui-mu))
    wi = t(data.leveli)-mui
    Hw = cbind(Hw,wi)
  }
  #total scatter
  Ht = t(data) - mu
  #Compute the pseudo-inverse of Ht
  Ht.svd = svd(Ht)
  Ht.svd.nullvalues = abs(Ht.svd$d) < tol
  #get singular vectors with non-zero singular values
  U1 = Ht.svd$u[,!Ht.svd.nullvalues]
  Sigma1 = diag(1/Ht.svd$d[!Ht.svd.nullvalues])
  #Multiply Hb by the pseudo-inverse of Ht
  B = Sigma1%*%t(U1)%*%Hb
  #the singular vectors of B are the LDA vectors
  B.svd = svd(B)
  #transform back to data space
  X = U1%*%Sigma1%*%B.svd$u
  X = X[,1:(length(groups)-1)]
  if (class(X)=="numeric") {
    X = matrix(X,nrow=length(X))
  }
  #normalize
  for (i in 1:ncol(X)) {
    X[,i] = X[,i]/sqrt(sum(X[,i]^2))
  }
  return (X)
}

# This function computes the vector normal to the hyperplane that best
# separates the data with the given grouping information using linear
# support vector machines
#Input
# data     - m x n data matrix with m samples of n variables
# grouping - column vector of length m with group information
#Output
# out - a vector containing the weights
linearSvm = function(data,grouping) {
  #require('kernlab')
  ##Compute the linear, unscaled, uncentered support vector machine model
  #svm.model = ksvm(data, grouping,kernel="vanilladot",scale=F,center=F)
  ##
  #weights = t(matrix(unlist(coef(svm.model)),nrow=1)%*%data[SVindex(svm.model),])
  #return (weights/sqrt(sum(weights^2)))
}

# This function computes the vector that connects the mean
# of group 1 to mean the of group 2
#Input
# data     - m x n data matrix with m samples of n variables
# grouping - column vector of length m with group information
#Output
# out - a vector connecting the means
meanRotation = function(data,grouping) {
  groups = unique(grouping)
  #compute group 1 mean
  group1 = data[grouping==groups[1],]
  mean1 = colMeans(group1)
  #compute group 2 mean
  group2 = data[grouping==groups[2],]
  mean2 = colMeans(group2)
  meanDiff = mean1-mean2
  return (meanDiff/sqrt(sum(meanDiff^2)))
}
