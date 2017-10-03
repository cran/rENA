// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_rENA_RCPPEXPORTS_H_GEN_
#define RCPP_rENA_RCPPEXPORTS_H_GEN_

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <Rcpp.h>

namespace rENA {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("rENA", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("rENA", "_rENA_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in rENA");
            }
        }
    }

    inline std::vector<std::string> merge_columns_c(DataFrame df, CharacterVector cols, std::string sep = ".") {
        typedef SEXP(*Ptr_merge_columns_c)(SEXP,SEXP,SEXP);
        static Ptr_merge_columns_c p_merge_columns_c = NULL;
        if (p_merge_columns_c == NULL) {
            validateSignature("std::vector<std::string>(*merge_columns_c)(DataFrame,CharacterVector,std::string)");
            p_merge_columns_c = (Ptr_merge_columns_c)R_GetCCallable("rENA", "_rENA_merge_columns_c");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_merge_columns_c(Shield<SEXP>(Rcpp::wrap(df)), Shield<SEXP>(Rcpp::wrap(cols)), Shield<SEXP>(Rcpp::wrap(sep)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<std::vector<std::string> >(rcpp_result_gen);
    }

    inline arma::mat rows_to_co_occurrences(DataFrame df, bool binary = true) {
        typedef SEXP(*Ptr_rows_to_co_occurrences)(SEXP,SEXP);
        static Ptr_rows_to_co_occurrences p_rows_to_co_occurrences = NULL;
        if (p_rows_to_co_occurrences == NULL) {
            validateSignature("arma::mat(*rows_to_co_occurrences)(DataFrame,bool)");
            p_rows_to_co_occurrences = (Ptr_rows_to_co_occurrences)R_GetCCallable("rENA", "_rENA_rows_to_co_occurrences");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_rows_to_co_occurrences(Shield<SEXP>(Rcpp::wrap(df)), Shield<SEXP>(Rcpp::wrap(binary)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::mat >(rcpp_result_gen);
    }

    inline DataFrame ref_window_df(DataFrame df, float windowSize = 1, float windowForward = 0, bool binary = true, bool binaryStanzas = false) {
        typedef SEXP(*Ptr_ref_window_df)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_ref_window_df p_ref_window_df = NULL;
        if (p_ref_window_df == NULL) {
            validateSignature("DataFrame(*ref_window_df)(DataFrame,float,float,bool,bool)");
            p_ref_window_df = (Ptr_ref_window_df)R_GetCCallable("rENA", "_rENA_ref_window_df");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ref_window_df(Shield<SEXP>(Rcpp::wrap(df)), Shield<SEXP>(Rcpp::wrap(windowSize)), Shield<SEXP>(Rcpp::wrap(windowForward)), Shield<SEXP>(Rcpp::wrap(binary)), Shield<SEXP>(Rcpp::wrap(binaryStanzas)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<DataFrame >(rcpp_result_gen);
    }

    inline DataFrame ref_window_lag(DataFrame df, int windowSize = 0, bool binary = true) {
        typedef SEXP(*Ptr_ref_window_lag)(SEXP,SEXP,SEXP);
        static Ptr_ref_window_lag p_ref_window_lag = NULL;
        if (p_ref_window_lag == NULL) {
            validateSignature("DataFrame(*ref_window_lag)(DataFrame,int,bool)");
            p_ref_window_lag = (Ptr_ref_window_lag)R_GetCCallable("rENA", "_rENA_ref_window_lag");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ref_window_lag(Shield<SEXP>(Rcpp::wrap(df)), Shield<SEXP>(Rcpp::wrap(windowSize)), Shield<SEXP>(Rcpp::wrap(binary)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<DataFrame >(rcpp_result_gen);
    }

}

#endif // RCPP_rENA_RCPPEXPORTS_H_GEN_
