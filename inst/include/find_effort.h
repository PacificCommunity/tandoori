/* 
 * Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott
 */

#ifndef _simple_array_
#define _simple_array_
#include "simple_array.h"
#endif

#ifndef _simple_array_3D_
#define _simple_array_3D_
#include "simple_array_3D.h"
#endif

#include "projection_functions.h"

#include <RcppEigen.h>
#include "LBFGSpp/LBFGSB.h"  // Header file for bounded LBFGSB


//using Eigen::VectorXd;
//using namespace LBFGSpp;


adouble get_error(std::vector<adouble>& log_effort_mult, simple_array_2D& n_after_move, simple_array_2D& m, simple_array_2D& waa, simple_array_2D& selq, Rcpp::NumericVector& target, Rcpp::IntegerVector& target_type, Rcpp::IntegerVector& fishery_map);

// Define EffortFun class
// The object that holds the function to be minimised and associated params 
class EffortFun{
private:
  int nfisheries;
  CppAD::ADFun<double> fun;
  public:
    // Constructor taking n
    // Pass in by reference?
    EffortFun(int nfisheries_, simple_array_2D& n_after_move, simple_array_2D& m, simple_array_2D& waa, simple_array_2D& selq, Rcpp::NumericVector& target, Rcpp::IntegerVector& target_type, Rcpp::IntegerVector& fishery_map);    
    // Essential method that evals the function to be minimised,
    // returns the results and places the gradient into the grad object
    double operator()(const Eigen::VectorXd& x, Eigen::VectorXd& grad);
};



Rcpp::List solve_effort(simple_array_2D n_pre_move, simple_array_2D m, simple_array_2D waa, simple_array_3D movement, simple_array_2D selq, double effort_mult_initial, Rcpp::NumericVector target, Rcpp::IntegerVector target_type, Rcpp::IntegerVector fishery_map, Rcpp::NumericVector max_effort, const unsigned int max_solver_iters);
