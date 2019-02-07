// // [[Rcpp::depends(RcppArmadillo)]]
// // [[Rcpp::depends(RcppParallel)]]
//
// #include <RcppArmadillo.h>
// using namespace Rcpp;
// using namespace arma;
// #include <RcppParallel.h>
// using namespace RcppParallel;
//
//
// arma::rowvec codes_to_occurrences(arma::mat v) {
//   int vL = v.size();
//   int vS = ( (vL * (vL + 1)) / 2) - vL ;
//   // arma::vec vR( vS, fill::zeros );
//   arma::rowvec vR2( vS, fill::zeros );
//   int s = 0;
//   for( int i = 2; i <= vL; i++ ) {
//     for (int j = 0; j < i-1; j++ ) {
//       // vR[s] = v[j] * v[i-1];
//       vR2[s] = v[j] * v[i-1];
//       s++;
//     }
//   }
//   return vR2;
// }
// struct WindowWorker : public RcppParallel::Worker {
//   // const RMatrix<double> input;
//   const arma::mat input;
//
//   size_t window;
//   bool binary;
//
//   // destination matrix
//   RMatrix<double> output;
//   // arma::mat output;
//
//   // WindowWorker(const NumericMatrix input, int window, bool binary, NumericMatrix output)
//   WindowWorker(const arma::mat input, int window, bool binary, NumericMatrix output)
//     : input(input), window(window), binary(binary), output(output) {}
//
//   void operator()(std::size_t begin, std::size_t end) {
//     // Rcpp::Rcout << "Accum beginning at: " << begin << std::endl;
//
//     int win = window;
//     if(win > 0) {
//       win = window - 1;
//     }
//
//     // RMatrix<double>::Row thisRow = input.row(begin);
//     // RMatrix<double>::Row prevRow = input.row(begin);
//     size_t earliestRow;
//     arma::rowvec full_win_sum;
//     arma::rowvec ref_win_sum;
//     arma::rowvec full_win_ut;
//     arma::rowvec ref_win_ut;
//     arma::rowvec win_subbed;
//
//     // NumericVector sums2;
//     // NumericVector sums3;
//     // NumericVector sums2_ut;
//     // NumericVector sums3_ut;
//     // NumericVector sums_subbed;
//     for (std::size_t j = begin; j < end; j++) {
//       // Rcpp::Rcout << "Row: " << j << " of " << input.n_rows << std::endl;
//       // RMatrix<double>::Row thisRow = input.row(j);
//       arma::rowvec thisRow = input.row(j);
//
//       // sums2 = rep_len(NumericVector::create(0), thisRow.size());
//       full_win_sum = zeros<arma::rowvec>(thisRow.size());
//
//       // std::transform(sums2.begin(), sums2.end(), thisRow.begin(), sums2.begin(), std::plus<double>());
//       // Rcpp::Rcout << "Full win: " << full_win_sum << std::endl;
//       // Rcpp::Rcout << "Adding: " << thisRow << std::endl;
//       full_win_sum = full_win_sum + thisRow;
//
//       if (win == 0) {
//         earliestRow = j;
//       } else if (j < win) {
//         earliestRow = 0;
//       } else {
//         earliestRow = j - win;
//       }
//
//       // NumericVector ssss = ;
//       // Rcpp::print(ssss);
//       // NumericVector sums3(thisRow.size(), 0);
//       // sums3 = rep_len(NumericVector::create(0), thisRow.size());
//       ref_win_sum = zeros<arma::rowvec>(thisRow.size());
//       if (earliestRow != j) {
//         // int maxBack = 0;
//         for(std::size_t k = 1; k <= j - earliestRow; k++) {
//           // RMatrix<double>::Row prevRow = input.row(j-k);
//
//           // std::transform(sums2.begin(), sums2.end(), prevRow.begin(), sums2.begin(), std::plus<double>());
//           // std::transform(sums3.begin(), sums3.end(), prevRow.begin(), sums3.begin(), std::plus<double>());
//           arma::rowvec prevRow = input.row(j-k);
//           full_win_sum = full_win_sum + prevRow;
//           ref_win_sum = ref_win_sum + prevRow;
//           // Rcpp::Rcout << "Full win: " << full_win_sum << std::endl;
//           // Rcpp::Rcout << "Ref win: " << ref_win_sum << std::endl;
//           // Rcpp::Rcout << "Adding: " << prevRow << std::endl;
//         }
//       }
//
//       // sums2_ut = vector_to_ut2(sums2);
//       // sums3_ut = vector_to_ut2(sums3);
//       // sums_subbed = sums2_ut - sums3_ut;
//       // for(std::size_t l = 0; l < sums2_ut.size(); l++) {
//       //   output(j, l) = sums_subbed[l]; //sums2_ut[l] - sums3_ut[l];
//       // }
//
//       full_win_ut = codes_to_occurrences(full_win_sum);
//       ref_win_ut = codes_to_occurrences(ref_win_sum);
//       win_subbed = full_win_ut - ref_win_ut;
//       // output.row(j) = win_subbed;
//       for(std::size_t l = 0; l < win_subbed.size(); l++) {
//         // Rcpp::Rcout << "Putting: " << win_subbed[l] << std::endl;
//         output(j, l) = win_subbed[l]; //sums2_ut[l] - sums3_ut[l];
//       }
//     }
//   }
// };
//
//
// // [[Rcpp::export]]
// DataFrame try_one(DataFrame df, size_t window, bool binary = true, int grainSize = 10) {
//   int dfRows = df.nrows();
//   int dfCols = df.size();
//   int numCoOccurences = ( (dfCols * (dfCols + 1)) / 2) - dfCols;
//
//   arma::mat df_AsMatrix(dfRows, dfCols, fill::zeros);
//   for (int i=0; i<dfCols;i++) {
//     df_AsMatrix.col(i) = Rcpp::as<arma::vec>(df[i]);
//   }
//
//   NumericMatrix output(dfRows, numCoOccurences);
//
//   WindowWorker worker(df_AsMatrix, window, binary, output);
//   parallelFor(0, df_AsMatrix.n_rows, worker, grainSize);
//
//   if(binary == true) {
//     //This conversion could be really slow, need more testing, or move
//     //the binary check to each
//     arma::mat out_bin = Rcpp::as<arma::mat>(output);
//     out_bin.elem( find(out_bin > 0) ).ones();
//     return(out_bin);
//   } else {
//     return(output);
//   }
// }
