// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/rENA.h"
#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// rowSums_c
NumericVector rowSums_c(NumericMatrix x);
RcppExport SEXP _rENA_rowSums_c(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rowSums_c(x));
    return rcpp_result_gen;
END_RCPP
}
// c_cor
NumericMatrix c_cor(NumericMatrix mat);
RcppExport SEXP _rENA_c_cor(SEXP matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    rcpp_result_gen = Rcpp::wrap(c_cor(mat));
    return rcpp_result_gen;
END_RCPP
}
// sphere_norm_c
NumericMatrix sphere_norm_c(DataFrame dfM);
RcppExport SEXP _rENA_sphere_norm_c(SEXP dfMSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type dfM(dfMSEXP);
    rcpp_result_gen = Rcpp::wrap(sphere_norm_c(dfM));
    return rcpp_result_gen;
END_RCPP
}
// dont_sphere_norm_c
NumericMatrix dont_sphere_norm_c(DataFrame dfM);
RcppExport SEXP _rENA_dont_sphere_norm_c(SEXP dfMSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type dfM(dfMSEXP);
    rcpp_result_gen = Rcpp::wrap(dont_sphere_norm_c(dfM));
    return rcpp_result_gen;
END_RCPP
}
// pca_c
List pca_c(arma::mat m, int dims);
RcppExport SEXP _rENA_pca_c(SEXP mSEXP, SEXP dimsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type dims(dimsSEXP);
    rcpp_result_gen = Rcpp::wrap(pca_c(m, dims));
    return rcpp_result_gen;
END_RCPP
}
// center_data_c
Rcpp::NumericMatrix center_data_c(arma::mat values);
RcppExport SEXP _rENA_center_data_c(SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type values(valuesSEXP);
    rcpp_result_gen = Rcpp::wrap(center_data_c(values));
    return rcpp_result_gen;
END_RCPP
}
// triIndices
arma::uvec triIndices(int len, int row);
RcppExport SEXP _rENA_triIndices(SEXP lenSEXP, SEXP rowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type len(lenSEXP);
    Rcpp::traits::input_parameter< int >::type row(rowSEXP);
    rcpp_result_gen = Rcpp::wrap(triIndices(len, row));
    return rcpp_result_gen;
END_RCPP
}
// getcor
double getcor(arma::mat dists, arma::mat normed, arma::mat x, arma::uvec NtriOne, arma::uvec NtriTwo, arma::uvec KtriOne, arma::uvec KtriTwo, int dim);
RcppExport SEXP _rENA_getcor(SEXP distsSEXP, SEXP normedSEXP, SEXP xSEXP, SEXP NtriOneSEXP, SEXP NtriTwoSEXP, SEXP KtriOneSEXP, SEXP KtriTwoSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type dists(distsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type normed(normedSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type NtriOne(NtriOneSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type NtriTwo(NtriTwoSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type KtriOne(KtriOneSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type KtriTwo(KtriTwoSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(getcor(dists, normed, x, NtriOne, NtriTwo, KtriOne, KtriTwo, dim));
    return rcpp_result_gen;
END_RCPP
}
// getN
int getN(arma::mat normed);
RcppExport SEXP _rENA_getN(SEXP normedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type normed(normedSEXP);
    rcpp_result_gen = Rcpp::wrap(getN(normed));
    return rcpp_result_gen;
END_RCPP
}
// getK
int getK(arma::mat normed);
RcppExport SEXP _rENA_getK(SEXP normedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type normed(normedSEXP);
    rcpp_result_gen = Rcpp::wrap(getK(normed));
    return rcpp_result_gen;
END_RCPP
}
// getRotationDistances_c
arma::mat getRotationDistances_c(arma::mat rotated);
RcppExport SEXP _rENA_getRotationDistances_c(SEXP rotatedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type rotated(rotatedSEXP);
    rcpp_result_gen = Rcpp::wrap(getRotationDistances_c(rotated));
    return rcpp_result_gen;
END_RCPP
}
// get_optimized_node_pos_c
Rcpp::List get_optimized_node_pos_c(arma::mat normedFiltered, NumericMatrix opted, int num_dims, int num_samples, int max_iter, bool return_all);
RcppExport SEXP _rENA_get_optimized_node_pos_c(SEXP normedFilteredSEXP, SEXP optedSEXP, SEXP num_dimsSEXP, SEXP num_samplesSEXP, SEXP max_iterSEXP, SEXP return_allSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type normedFiltered(normedFilteredSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type opted(optedSEXP);
    Rcpp::traits::input_parameter< int >::type num_dims(num_dimsSEXP);
    Rcpp::traits::input_parameter< int >::type num_samples(num_samplesSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< bool >::type return_all(return_allSEXP);
    rcpp_result_gen = Rcpp::wrap(get_optimized_node_pos_c(normedFiltered, opted, num_dims, num_samples, max_iter, return_all));
    return rcpp_result_gen;
END_RCPP
}
// lm_
List lm_(NumericMatrix x);
RcppExport SEXP _rENA_lm_(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(lm_(x));
    return rcpp_result_gen;
END_RCPP
}
// full_opt_c
Rcpp::List full_opt_c(arma::mat normed, arma::mat rotated, Rcpp::List optim_nodes, int dims, int num_samples, bool checkUnique);
RcppExport SEXP _rENA_full_opt_c(SEXP normedSEXP, SEXP rotatedSEXP, SEXP optim_nodesSEXP, SEXP dimsSEXP, SEXP num_samplesSEXP, SEXP checkUniqueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type normed(normedSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type rotated(rotatedSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type optim_nodes(optim_nodesSEXP);
    Rcpp::traits::input_parameter< int >::type dims(dimsSEXP);
    Rcpp::traits::input_parameter< int >::type num_samples(num_samplesSEXP);
    Rcpp::traits::input_parameter< bool >::type checkUnique(checkUniqueSEXP);
    rcpp_result_gen = Rcpp::wrap(full_opt_c(normed, rotated, optim_nodes, dims, num_samples, checkUnique));
    return rcpp_result_gen;
END_RCPP
}
// calc_cor
double calc_cor(arma::vec x, List set, int dim);
RcppExport SEXP _rENA_calc_cor(SEXP xSEXP, SEXP setSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< List >::type set(setSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_cor(x, set, dim));
    return rcpp_result_gen;
END_RCPP
}
// soln_MPS
arma::vec soln_MPS(arma::mat x);
RcppExport SEXP _rENA_soln_MPS(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(soln_MPS(x));
    return rcpp_result_gen;
END_RCPP
}
// soln_calc_c
double soln_calc_c(arma::vec coeff, arma::vec xi, arma::vec ti, arma::mat w, int dim);
RcppExport SEXP _rENA_soln_calc_c(SEXP coeffSEXP, SEXP xiSEXP, SEXP tiSEXP, SEXP wSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type coeff(coeffSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type xi(xiSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type ti(tiSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type w(wSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(soln_calc_c(coeff, xi, ti, w, dim));
    return rcpp_result_gen;
END_RCPP
}
// remove_zero_rows_c
arma::mat remove_zero_rows_c(arma::mat toFilter);
RcppExport SEXP _rENA_remove_zero_rows_c(SEXP toFilterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type toFilter(toFilterSEXP);
    rcpp_result_gen = Rcpp::wrap(remove_zero_rows_c(toFilter));
    return rcpp_result_gen;
END_RCPP
}
// remove_zero_rows_by_c
arma::mat remove_zero_rows_by_c(arma::mat toFilter, arma::mat indices);
RcppExport SEXP _rENA_remove_zero_rows_by_c(SEXP toFilterSEXP, SEXP indicesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type toFilter(toFilterSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type indices(indicesSEXP);
    rcpp_result_gen = Rcpp::wrap(remove_zero_rows_by_c(toFilter, indices));
    return rcpp_result_gen;
END_RCPP
}
// linderoth_pos
Rcpp::List linderoth_pos(Eigen::MatrixXd adjMats, Eigen::MatrixXd t);
RcppExport SEXP _rENA_linderoth_pos(SEXP adjMatsSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type adjMats(adjMatsSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(linderoth_pos(adjMats, t));
    return rcpp_result_gen;
END_RCPP
}
// linderoth_pos_es
Rcpp::List linderoth_pos_es(Eigen::MatrixXd adjMats, Eigen::MatrixXd t);
RcppExport SEXP _rENA_linderoth_pos_es(SEXP adjMatsSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type adjMats(adjMatsSEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(linderoth_pos_es(adjMats, t));
    return rcpp_result_gen;
END_RCPP
}
// merge_columns_c
std::vector<std::string> merge_columns_c(DataFrame df, CharacterVector cols, std::string sep);
static SEXP _rENA_merge_columns_c_try(SEXP dfSEXP, SEXP colsSEXP, SEXP sepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< std::string >::type sep(sepSEXP);
    rcpp_result_gen = Rcpp::wrap(merge_columns_c(df, cols, sep));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_merge_columns_c(SEXP dfSEXP, SEXP colsSEXP, SEXP sepSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_merge_columns_c_try(dfSEXP, colsSEXP, sepSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// rows_to_co_occurrences
arma::mat rows_to_co_occurrences(DataFrame df, bool binary);
static SEXP _rENA_rows_to_co_occurrences_try(SEXP dfSEXP, SEXP binarySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< bool >::type binary(binarySEXP);
    rcpp_result_gen = Rcpp::wrap(rows_to_co_occurrences(df, binary));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_rows_to_co_occurrences(SEXP dfSEXP, SEXP binarySEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_rows_to_co_occurrences_try(dfSEXP, binarySEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// ref_window_df
DataFrame ref_window_df(DataFrame df, float windowSize, float windowForward, bool binary, bool binaryStanzas);
static SEXP _rENA_ref_window_df_try(SEXP dfSEXP, SEXP windowSizeSEXP, SEXP windowForwardSEXP, SEXP binarySEXP, SEXP binaryStanzasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< float >::type windowSize(windowSizeSEXP);
    Rcpp::traits::input_parameter< float >::type windowForward(windowForwardSEXP);
    Rcpp::traits::input_parameter< bool >::type binary(binarySEXP);
    Rcpp::traits::input_parameter< bool >::type binaryStanzas(binaryStanzasSEXP);
    rcpp_result_gen = Rcpp::wrap(ref_window_df(df, windowSize, windowForward, binary, binaryStanzas));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_ref_window_df(SEXP dfSEXP, SEXP windowSizeSEXP, SEXP windowForwardSEXP, SEXP binarySEXP, SEXP binaryStanzasSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_ref_window_df_try(dfSEXP, windowSizeSEXP, windowForwardSEXP, binarySEXP, binaryStanzasSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// ref_window_lag
DataFrame ref_window_lag(DataFrame df, int windowSize, bool binary);
static SEXP _rENA_ref_window_lag_try(SEXP dfSEXP, SEXP windowSizeSEXP, SEXP binarySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< int >::type windowSize(windowSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type binary(binarySEXP);
    rcpp_result_gen = Rcpp::wrap(ref_window_lag(df, windowSize, binary));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_ref_window_lag(SEXP dfSEXP, SEXP windowSizeSEXP, SEXP binarySEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_ref_window_lag_try(dfSEXP, windowSizeSEXP, binarySEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// ref_window_sum
DataFrame ref_window_sum(DataFrame df, bool binary);
RcppExport SEXP _rENA_ref_window_sum(SEXP dfSEXP, SEXP binarySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< bool >::type binary(binarySEXP);
    rcpp_result_gen = Rcpp::wrap(ref_window_sum(df, binary));
    return rcpp_result_gen;
END_RCPP
}
// svector_to_ut
std::vector<std::string> svector_to_ut(std::vector<std::string> v);
RcppExport SEXP _rENA_svector_to_ut(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(svector_to_ut(v));
    return rcpp_result_gen;
END_RCPP
}

// validate (ensure exported C++ functions exist before calling them)
static int _rENA_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("std::vector<std::string>(*merge_columns_c)(DataFrame,CharacterVector,std::string)");
        signatures.insert("arma::mat(*rows_to_co_occurrences)(DataFrame,bool)");
        signatures.insert("DataFrame(*ref_window_df)(DataFrame,float,float,bool,bool)");
        signatures.insert("DataFrame(*ref_window_lag)(DataFrame,int,bool)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _rENA_RcppExport_registerCCallable() { 
    R_RegisterCCallable("rENA", "_rENA_merge_columns_c", (DL_FUNC)_rENA_merge_columns_c_try);
    R_RegisterCCallable("rENA", "_rENA_rows_to_co_occurrences", (DL_FUNC)_rENA_rows_to_co_occurrences_try);
    R_RegisterCCallable("rENA", "_rENA_ref_window_df", (DL_FUNC)_rENA_ref_window_df_try);
    R_RegisterCCallable("rENA", "_rENA_ref_window_lag", (DL_FUNC)_rENA_ref_window_lag_try);
    R_RegisterCCallable("rENA", "_rENA_RcppExport_validate", (DL_FUNC)_rENA_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_rENA_rowSums_c", (DL_FUNC) &_rENA_rowSums_c, 1},
    {"_rENA_c_cor", (DL_FUNC) &_rENA_c_cor, 1},
    {"_rENA_sphere_norm_c", (DL_FUNC) &_rENA_sphere_norm_c, 1},
    {"_rENA_dont_sphere_norm_c", (DL_FUNC) &_rENA_dont_sphere_norm_c, 1},
    {"_rENA_pca_c", (DL_FUNC) &_rENA_pca_c, 2},
    {"_rENA_center_data_c", (DL_FUNC) &_rENA_center_data_c, 1},
    {"_rENA_triIndices", (DL_FUNC) &_rENA_triIndices, 2},
    {"_rENA_getcor", (DL_FUNC) &_rENA_getcor, 8},
    {"_rENA_getN", (DL_FUNC) &_rENA_getN, 1},
    {"_rENA_getK", (DL_FUNC) &_rENA_getK, 1},
    {"_rENA_getRotationDistances_c", (DL_FUNC) &_rENA_getRotationDistances_c, 1},
    {"_rENA_get_optimized_node_pos_c", (DL_FUNC) &_rENA_get_optimized_node_pos_c, 6},
    {"_rENA_lm_", (DL_FUNC) &_rENA_lm_, 1},
    {"_rENA_full_opt_c", (DL_FUNC) &_rENA_full_opt_c, 6},
    {"_rENA_calc_cor", (DL_FUNC) &_rENA_calc_cor, 3},
    {"_rENA_soln_MPS", (DL_FUNC) &_rENA_soln_MPS, 1},
    {"_rENA_soln_calc_c", (DL_FUNC) &_rENA_soln_calc_c, 5},
    {"_rENA_remove_zero_rows_c", (DL_FUNC) &_rENA_remove_zero_rows_c, 1},
    {"_rENA_remove_zero_rows_by_c", (DL_FUNC) &_rENA_remove_zero_rows_by_c, 2},
    {"_rENA_linderoth_pos", (DL_FUNC) &_rENA_linderoth_pos, 2},
    {"_rENA_linderoth_pos_es", (DL_FUNC) &_rENA_linderoth_pos_es, 2},
    {"_rENA_merge_columns_c", (DL_FUNC) &_rENA_merge_columns_c, 3},
    {"_rENA_rows_to_co_occurrences", (DL_FUNC) &_rENA_rows_to_co_occurrences, 2},
    {"_rENA_ref_window_df", (DL_FUNC) &_rENA_ref_window_df, 5},
    {"_rENA_ref_window_lag", (DL_FUNC) &_rENA_ref_window_lag, 3},
    {"_rENA_ref_window_sum", (DL_FUNC) &_rENA_ref_window_sum, 2},
    {"_rENA_svector_to_ut", (DL_FUNC) &_rENA_svector_to_ut, 1},
    {"_rENA_RcppExport_registerCCallable", (DL_FUNC) &_rENA_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_rENA(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
