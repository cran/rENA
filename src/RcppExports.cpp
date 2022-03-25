// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/rENA.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// combn_c2
arma::umat combn_c2(double n);
static SEXP _rENA_combn_c2_try(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(combn_c2(n));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_combn_c2(SEXP nSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_combn_c2_try(nSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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
// ena_correlation
arma::mat ena_correlation(arma::mat points, arma::mat centroids, double conf_level);
static SEXP _rENA_ena_correlation_try(SEXP pointsSEXP, SEXP centroidsSEXP, SEXP conf_levelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::mat >::type points(pointsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type centroids(centroidsSEXP);
    Rcpp::traits::input_parameter< double >::type conf_level(conf_levelSEXP);
    rcpp_result_gen = Rcpp::wrap(ena_correlation(points, centroids, conf_level));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_ena_correlation(SEXP pointsSEXP, SEXP centroidsSEXP, SEXP conf_levelSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_ena_correlation_try(pointsSEXP, centroidsSEXP, conf_levelSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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
// vector_to_ut
arma::rowvec vector_to_ut(arma::mat v);
static SEXP _rENA_vector_to_ut_try(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::mat >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(vector_to_ut(v));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_vector_to_ut(SEXP vSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_vector_to_ut_try(vSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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
// svector_to_ut
std::vector<std::string> svector_to_ut(std::vector<std::string> v);
static SEXP _rENA_svector_to_ut_try(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(svector_to_ut(v));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_svector_to_ut(SEXP vSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_svector_to_ut_try(vSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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
DataFrame ref_window_df(DataFrame df, float windowSize, float windowForward, bool binary);
static SEXP _rENA_ref_window_df_try(SEXP dfSEXP, SEXP windowSizeSEXP, SEXP windowForwardSEXP, SEXP binarySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< float >::type windowSize(windowSizeSEXP);
    Rcpp::traits::input_parameter< float >::type windowForward(windowForwardSEXP);
    Rcpp::traits::input_parameter< bool >::type binary(binarySEXP);
    rcpp_result_gen = Rcpp::wrap(ref_window_df(df, windowSize, windowForward, binary));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_ref_window_df(SEXP dfSEXP, SEXP windowSizeSEXP, SEXP windowForwardSEXP, SEXP binarySEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_ref_window_df_try(dfSEXP, windowSizeSEXP, windowForwardSEXP, binarySEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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
// fun_sphere_norm
NumericMatrix fun_sphere_norm(DataFrame dfM);
static SEXP _rENA_fun_sphere_norm_try(SEXP dfMSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< DataFrame >::type dfM(dfMSEXP);
    rcpp_result_gen = Rcpp::wrap(fun_sphere_norm(dfM));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_fun_sphere_norm(SEXP dfMSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_fun_sphere_norm_try(dfMSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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
// fun_skip_sphere_norm
NumericMatrix fun_skip_sphere_norm(DataFrame dfM);
static SEXP _rENA_fun_skip_sphere_norm_try(SEXP dfMSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< DataFrame >::type dfM(dfMSEXP);
    rcpp_result_gen = Rcpp::wrap(fun_skip_sphere_norm(dfM));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_fun_skip_sphere_norm(SEXP dfMSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_fun_skip_sphere_norm_try(dfMSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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
// center_data_c
Rcpp::NumericMatrix center_data_c(arma::mat values);
static SEXP _rENA_center_data_c_try(SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::mat >::type values(valuesSEXP);
    rcpp_result_gen = Rcpp::wrap(center_data_c(values));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_center_data_c(SEXP valuesSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_center_data_c_try(valuesSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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
// triIndices
arma::umat triIndices(int len, int row);
static SEXP _rENA_triIndices_try(SEXP lenSEXP, SEXP rowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< int >::type len(lenSEXP);
    Rcpp::traits::input_parameter< int >::type row(rowSEXP);
    rcpp_result_gen = Rcpp::wrap(triIndices(len, row));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_triIndices(SEXP lenSEXP, SEXP rowSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_triIndices_try(lenSEXP, rowSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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
// lws_lsq_positions
Rcpp::List lws_lsq_positions(arma::mat adjMats, arma::mat t, int numDims);
static SEXP _rENA_lws_lsq_positions_try(SEXP adjMatsSEXP, SEXP tSEXP, SEXP numDimsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::mat >::type adjMats(adjMatsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type t(tSEXP);
    Rcpp::traits::input_parameter< int >::type numDims(numDimsSEXP);
    rcpp_result_gen = Rcpp::wrap(lws_lsq_positions(adjMats, t, numDims));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _rENA_lws_lsq_positions(SEXP adjMatsSEXP, SEXP tSEXP, SEXP numDimsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_rENA_lws_lsq_positions_try(adjMatsSEXP, tSEXP, numDimsSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
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

// validate (ensure exported C++ functions exist before calling them)
static int _rENA_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("arma::umat(*combn_c2)(double)");
        signatures.insert("arma::mat(*ena_correlation)(arma::mat,arma::mat,double)");
        signatures.insert("std::vector<std::string>(*merge_columns_c)(DataFrame,CharacterVector,std::string)");
        signatures.insert("arma::rowvec(*vector_to_ut)(arma::mat)");
        signatures.insert("std::vector<std::string>(*svector_to_ut)(std::vector<std::string>)");
        signatures.insert("arma::mat(*rows_to_co_occurrences)(DataFrame,bool)");
        signatures.insert("DataFrame(*ref_window_df)(DataFrame,float,float,bool)");
        signatures.insert("DataFrame(*ref_window_lag)(DataFrame,int,bool)");
        signatures.insert("NumericMatrix(*fun_sphere_norm)(DataFrame)");
        signatures.insert("NumericMatrix(*fun_skip_sphere_norm)(DataFrame)");
        signatures.insert("Rcpp::NumericMatrix(*center_data_c)(arma::mat)");
        signatures.insert("arma::umat(*triIndices)(int,int)");
        signatures.insert("Rcpp::List(*lws_lsq_positions)(arma::mat,arma::mat,int)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _rENA_RcppExport_registerCCallable() { 
    R_RegisterCCallable("rENA", "_rENA_combn_c2", (DL_FUNC)_rENA_combn_c2_try);
    R_RegisterCCallable("rENA", "_rENA_ena_correlation", (DL_FUNC)_rENA_ena_correlation_try);
    R_RegisterCCallable("rENA", "_rENA_merge_columns_c", (DL_FUNC)_rENA_merge_columns_c_try);
    R_RegisterCCallable("rENA", "_rENA_vector_to_ut", (DL_FUNC)_rENA_vector_to_ut_try);
    R_RegisterCCallable("rENA", "_rENA_svector_to_ut", (DL_FUNC)_rENA_svector_to_ut_try);
    R_RegisterCCallable("rENA", "_rENA_rows_to_co_occurrences", (DL_FUNC)_rENA_rows_to_co_occurrences_try);
    R_RegisterCCallable("rENA", "_rENA_ref_window_df", (DL_FUNC)_rENA_ref_window_df_try);
    R_RegisterCCallable("rENA", "_rENA_ref_window_lag", (DL_FUNC)_rENA_ref_window_lag_try);
    R_RegisterCCallable("rENA", "_rENA_fun_sphere_norm", (DL_FUNC)_rENA_fun_sphere_norm_try);
    R_RegisterCCallable("rENA", "_rENA_fun_skip_sphere_norm", (DL_FUNC)_rENA_fun_skip_sphere_norm_try);
    R_RegisterCCallable("rENA", "_rENA_center_data_c", (DL_FUNC)_rENA_center_data_c_try);
    R_RegisterCCallable("rENA", "_rENA_triIndices", (DL_FUNC)_rENA_triIndices_try);
    R_RegisterCCallable("rENA", "_rENA_lws_lsq_positions", (DL_FUNC)_rENA_lws_lsq_positions_try);
    R_RegisterCCallable("rENA", "_rENA_RcppExport_validate", (DL_FUNC)_rENA_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_rENA_combn_c2", (DL_FUNC) &_rENA_combn_c2, 1},
    {"_rENA_ena_correlation", (DL_FUNC) &_rENA_ena_correlation, 3},
    {"_rENA_merge_columns_c", (DL_FUNC) &_rENA_merge_columns_c, 3},
    {"_rENA_vector_to_ut", (DL_FUNC) &_rENA_vector_to_ut, 1},
    {"_rENA_svector_to_ut", (DL_FUNC) &_rENA_svector_to_ut, 1},
    {"_rENA_rows_to_co_occurrences", (DL_FUNC) &_rENA_rows_to_co_occurrences, 2},
    {"_rENA_ref_window_df", (DL_FUNC) &_rENA_ref_window_df, 4},
    {"_rENA_ref_window_lag", (DL_FUNC) &_rENA_ref_window_lag, 3},
    {"_rENA_fun_sphere_norm", (DL_FUNC) &_rENA_fun_sphere_norm, 1},
    {"_rENA_fun_skip_sphere_norm", (DL_FUNC) &_rENA_fun_skip_sphere_norm, 1},
    {"_rENA_center_data_c", (DL_FUNC) &_rENA_center_data_c, 1},
    {"_rENA_triIndices", (DL_FUNC) &_rENA_triIndices, 2},
    {"_rENA_lws_lsq_positions", (DL_FUNC) &_rENA_lws_lsq_positions, 3},
    {"_rENA_RcppExport_registerCCallable", (DL_FUNC) &_rENA_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_rENA(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
