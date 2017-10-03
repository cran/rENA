
adj.mat.to.dir <- function(mat, binary = T) {
  mat.refs = mat[1:nrow(mat)-1,];
  mat.refs.sums = colSums(mat.refs);
  mat.refs.adj = tcrossprod(mat.refs.sums);

  mat.utt = mat[nrow(mat),];
  mat.utt.adj = tcrossprod(mat.utt);

  mat.win.senders = mat.refs.adj %*% mat.utt.adj;

  mat.all.conns = mat.win.senders + mat.utt.adj;
  diag(mat.all.conns) = 0;

  if(binary == T) {
    mat.all.conns[mat.all.conns > 1] = 1;
  }
  rownames(mat.all.conns) = colnames(mat.all.conns) = colnames(mat)
  mat.all.conns
}

adj.mat.to.vec <- function(mat, upper = T, diag = F) {
  if(upper == T) {
    mat[upper.tri(mat)]
  } else {
    nms = colnames(mat);
    matrix(
      c(mat[upper.tri(mat)], mat[lower.tri(mat)]),
      nrow=1,
      dimnames=  list(
        NULL,
        c(
          svector_to_ut(nms[1:length(nms)]),
          rev(svector_to_ut(nms[length(nms):1]))
        )
      )
    )
  }
}
