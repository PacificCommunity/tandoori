/* 
 * Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott
 */

#ifndef _simple_array_
#define _simple_array_
#include "simple_array.h"
#endif

#include "solver.h"

std::vector<adouble> get_catch_wt(std::vector<adouble>& effort, simple_array_2D& n_after_move, simple_array_2D& m, simple_array_2D& waa, simple_array_2D& selq, Rcpp::IntegerVector& fishery_map);

Rcpp::NumericVector run(simple_array_2D n_after_move, simple_array_2D m, simple_array_2D waa, simple_array_2D selq, double effort_mult_initial, Rcpp::NumericVector catch_target, Rcpp::IntegerVector fishery_map);
