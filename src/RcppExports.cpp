// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/tandoori.h"
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// find_effort
Rcpp::NumericVector find_effort(simple_array_2D n_after_move, simple_array_2D m, simple_array_2D waa, simple_array_2D selq, double effort_mult_initial, Rcpp::NumericVector target, Rcpp::IntegerVector target_type, Rcpp::IntegerVector fishery_area);
RcppExport SEXP _tandoori_find_effort(SEXP n_after_moveSEXP, SEXP mSEXP, SEXP waaSEXP, SEXP selqSEXP, SEXP effort_mult_initialSEXP, SEXP targetSEXP, SEXP target_typeSEXP, SEXP fishery_areaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< simple_array_2D >::type n_after_move(n_after_moveSEXP);
    Rcpp::traits::input_parameter< simple_array_2D >::type m(mSEXP);
    Rcpp::traits::input_parameter< simple_array_2D >::type waa(waaSEXP);
    Rcpp::traits::input_parameter< simple_array_2D >::type selq(selqSEXP);
    Rcpp::traits::input_parameter< double >::type effort_mult_initial(effort_mult_initialSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type target(targetSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type target_type(target_typeSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type fishery_area(fishery_areaSEXP);
    rcpp_result_gen = Rcpp::wrap(find_effort(n_after_move, m, waa, selq, effort_mult_initial, target, target_type, fishery_area));
    return rcpp_result_gen;
END_RCPP
}
// int_test
int int_test(int dummy);
RcppExport SEXP _tandoori_int_test(SEXP dummySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type dummy(dummySEXP);
    rcpp_result_gen = Rcpp::wrap(int_test(dummy));
    return rcpp_result_gen;
END_RCPP
}
// simple_array_2D_constructor
simple_array_2D simple_array_2D_constructor(unsigned int x, unsigned int y, double fill);
RcppExport SEXP _tandoori_simple_array_2D_constructor(SEXP xSEXP, SEXP ySEXP, SEXP fillSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned int >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type fill(fillSEXP);
    rcpp_result_gen = Rcpp::wrap(simple_array_2D_constructor(x, y, fill));
    return rcpp_result_gen;
END_RCPP
}
// simple_array_2D_as_wrap_test
simple_array_2D simple_array_2D_as_wrap_test(simple_array_2D sa2d);
RcppExport SEXP _tandoori_simple_array_2D_as_wrap_test(SEXP sa2dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< simple_array_2D >::type sa2d(sa2dSEXP);
    rcpp_result_gen = Rcpp::wrap(simple_array_2D_as_wrap_test(sa2d));
    return rcpp_result_gen;
END_RCPP
}
// simple_array_2D_get_accessor_test
double simple_array_2D_get_accessor_test(simple_array_2D sa2d, const int x, const int y);
RcppExport SEXP _tandoori_simple_array_2D_get_accessor_test(SEXP sa2dSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< simple_array_2D >::type sa2d(sa2dSEXP);
    Rcpp::traits::input_parameter< const int >::type x(xSEXP);
    Rcpp::traits::input_parameter< const int >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(simple_array_2D_get_accessor_test(sa2d, x, y));
    return rcpp_result_gen;
END_RCPP
}
// get_catch_demo
simple_array_2D get_catch_demo(simple_array_2D f, simple_array_2D m, simple_array_2D n, Rcpp::IntegerVector fishery_area);
RcppExport SEXP _tandoori_get_catch_demo(SEXP fSEXP, SEXP mSEXP, SEXP nSEXP, SEXP fishery_areaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< simple_array_2D >::type f(fSEXP);
    Rcpp::traits::input_parameter< simple_array_2D >::type m(mSEXP);
    Rcpp::traits::input_parameter< simple_array_2D >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type fishery_area(fishery_areaSEXP);
    rcpp_result_gen = Rcpp::wrap(get_catch_demo(f, m, n, fishery_area));
    return rcpp_result_gen;
END_RCPP
}
// test_2D_iterator
simple_array_2D test_2D_iterator(simple_array_2D sa2d);
RcppExport SEXP _tandoori_test_2D_iterator(SEXP sa2dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< simple_array_2D >::type sa2d(sa2dSEXP);
    rcpp_result_gen = Rcpp::wrap(test_2D_iterator(sa2d));
    return rcpp_result_gen;
END_RCPP
}
// test_catch_weight
Rcpp::NumericVector test_catch_weight(Rcpp::NumericVector effort_in, simple_array_2D n_after_move, simple_array_2D m, simple_array_2D waa, simple_array_2D selq, Rcpp::IntegerVector fishery_area);
RcppExport SEXP _tandoori_test_catch_weight(SEXP effort_inSEXP, SEXP n_after_moveSEXP, SEXP mSEXP, SEXP waaSEXP, SEXP selqSEXP, SEXP fishery_areaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type effort_in(effort_inSEXP);
    Rcpp::traits::input_parameter< simple_array_2D >::type n_after_move(n_after_moveSEXP);
    Rcpp::traits::input_parameter< simple_array_2D >::type m(mSEXP);
    Rcpp::traits::input_parameter< simple_array_2D >::type waa(waaSEXP);
    Rcpp::traits::input_parameter< simple_array_2D >::type selq(selqSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type fishery_area(fishery_areaSEXP);
    rcpp_result_gen = Rcpp::wrap(test_catch_weight(effort_in, n_after_move, m, waa, selq, fishery_area));
    return rcpp_result_gen;
END_RCPP
}
// simple_array_3D_as_wrap_test
simple_array_3D simple_array_3D_as_wrap_test(simple_array_3D sa3d);
RcppExport SEXP _tandoori_simple_array_3D_as_wrap_test(SEXP sa3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< simple_array_3D >::type sa3d(sa3dSEXP);
    rcpp_result_gen = Rcpp::wrap(simple_array_3D_as_wrap_test(sa3d));
    return rcpp_result_gen;
END_RCPP
}
// simple_array_3D_get_accessor_test
double simple_array_3D_get_accessor_test(simple_array_3D sa3d, const int x, const int y, const int z);
RcppExport SEXP _tandoori_simple_array_3D_get_accessor_test(SEXP sa3dSEXP, SEXP xSEXP, SEXP ySEXP, SEXP zSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< simple_array_3D >::type sa3d(sa3dSEXP);
    Rcpp::traits::input_parameter< const int >::type x(xSEXP);
    Rcpp::traits::input_parameter< const int >::type y(ySEXP);
    Rcpp::traits::input_parameter< const int >::type z(zSEXP);
    rcpp_result_gen = Rcpp::wrap(simple_array_3D_get_accessor_test(sa3d, x, y, z));
    return rcpp_result_gen;
END_RCPP
}
// simple_array_3D_constructor
simple_array_3D simple_array_3D_constructor(unsigned int x, unsigned int y, unsigned int z, double fill);
RcppExport SEXP _tandoori_simple_array_3D_constructor(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP, SEXP fillSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned int >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type y(ySEXP);
    Rcpp::traits::input_parameter< unsigned int >::type z(zSEXP);
    Rcpp::traits::input_parameter< double >::type fill(fillSEXP);
    rcpp_result_gen = Rcpp::wrap(simple_array_3D_constructor(x, y, z, fill));
    return rcpp_result_gen;
END_RCPP
}
// test_3D_iterator
simple_array_3D test_3D_iterator(simple_array_3D sa3d);
RcppExport SEXP _tandoori_test_3D_iterator(SEXP sa3dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< simple_array_3D >::type sa3d(sa3dSEXP);
    rcpp_result_gen = Rcpp::wrap(test_3D_iterator(sa3d));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tandoori_find_effort", (DL_FUNC) &_tandoori_find_effort, 8},
    {"_tandoori_int_test", (DL_FUNC) &_tandoori_int_test, 1},
    {"_tandoori_simple_array_2D_constructor", (DL_FUNC) &_tandoori_simple_array_2D_constructor, 3},
    {"_tandoori_simple_array_2D_as_wrap_test", (DL_FUNC) &_tandoori_simple_array_2D_as_wrap_test, 1},
    {"_tandoori_simple_array_2D_get_accessor_test", (DL_FUNC) &_tandoori_simple_array_2D_get_accessor_test, 3},
    {"_tandoori_get_catch_demo", (DL_FUNC) &_tandoori_get_catch_demo, 4},
    {"_tandoori_test_2D_iterator", (DL_FUNC) &_tandoori_test_2D_iterator, 1},
    {"_tandoori_test_catch_weight", (DL_FUNC) &_tandoori_test_catch_weight, 6},
    {"_tandoori_simple_array_3D_as_wrap_test", (DL_FUNC) &_tandoori_simple_array_3D_as_wrap_test, 1},
    {"_tandoori_simple_array_3D_get_accessor_test", (DL_FUNC) &_tandoori_simple_array_3D_get_accessor_test, 4},
    {"_tandoori_simple_array_3D_constructor", (DL_FUNC) &_tandoori_simple_array_3D_constructor, 4},
    {"_tandoori_test_3D_iterator", (DL_FUNC) &_tandoori_test_3D_iterator, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_tandoori(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
