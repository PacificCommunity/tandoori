/* 
 * Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott
 */

#include <cppad/cppad.hpp> // CppAD package http://www.coin-or.org/CppAD/

#include <Rcpp.h>

double euclid_norm(std::vector<double> x);

// A Newton Raphson solver for a function that has already been taped.
// Pass in the independent variables, tape no. and control parameters
int newton_raphson(std::vector<double>& indep, CppAD::ADFun<double>& fun, std::vector<double> max_indep, const unsigned int max_iters= 50, const double tolerance = 1.5e-8);
