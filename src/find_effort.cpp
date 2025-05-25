/* 
 * Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott
 */

#include "../inst/include/find_effort.h"


// Main function that calcs the error given the log_effort_mult
adouble get_error(std::vector<adouble> log_effort_mult, simple_array_2D n_after_move, simple_array_2D m, simple_array_2D waa, simple_array_2D selq, Rcpp::NumericVector target, Rcpp::IntegerVector target_type, Rcpp::IntegerVector fishery_map){
  bool verbose = false;
  // Make effort vector based on new log_effort_mult
  //  if(verbose){Rprintf("\nUpdating effort with multipler\n");}
  //  // new effort = initial effort * mult
  auto nfisheries = log_effort_mult.size();
  std::vector<adouble> effort(nfisheries);
  double effort_initial = 1.0;
  for (int fcount=0; fcount < nfisheries; fcount++){
    effort[fcount] = effort_initial * exp(log_effort_mult[fcount]);
  }
  
  // Get catch weight per fishery with that effort
  if(verbose){Rprintf("\nGetting catch target hat\n");}
  std::vector<adouble> total_catch_weight = get_catch_wt(effort, n_after_move, m, waa, selq, fishery_map);
  
  // Calculate error
  std::vector<adouble> error(nfisheries);
  if(verbose){Rprintf("\nCalculating error\n");}
  // Go fishery by fishery
  // If target_type == 0, then catch target
  // If target_type == 1, then effort target
  // Get error as appropriate
  std::vector<adouble> hat(nfisheries);
  for (int fcount = 0; fcount < nfisheries; fcount++){
    if(target_type[fcount] == 0){
      hat[fcount] = total_catch_weight[fcount];
    } else if(target_type[fcount] == 1){
      hat[fcount] = effort[fcount];
    } else {
      Rcpp::stop("Unrecognised target type for fishery %i.", fcount);
    }
    // Get error
    error[fcount] = log(target[fcount]) - log(hat[fcount]);
  }
  // Square, sum and return
  // Force type of initial value to be adouble
  adouble out = std::inner_product(error.begin(), error.end(), error.begin(), adouble{0.0});
  out = sqrt(out);
  return out;
}

EffortFun::EffortFun(int nfisheries_, simple_array_2D n_after_move, simple_array_2D m, simple_array_2D waa, simple_array_2D selq, Rcpp::NumericVector target, Rcpp::IntegerVector target_type, Rcpp::IntegerVector fishery_map) {
  nfisheries = nfisheries_;
  // Tape the function at some initial value
  // What are we finding? Log effort mult again? Initialise
  std::vector<adouble> log_effort_mult(nfisheries, 0.0);
  CppAD::Independent(log_effort_mult);
  // Output needs to be a vector for tape to work even though we only have scalar back
  std::vector<adouble> fx(1);
  // Eval the error function
  // How do we pass in the other params - I guess as part of the constructor?
  fx[0] = get_error(log_effort_mult, n_after_move, m, waa, selq, target, target_type, fishery_map);
  CppAD::ADFun<double> newfun(log_effort_mult, fx);
  fun = newfun;
}

double EffortFun::operator()(const Eigen::VectorXd& x, Eigen::VectorXd& grad)  {
  // Get fx and grad by evaluating existing tape
  // Bit annoying that output has to be vector
  //Rprintf("Getting new fx and grad\n");
  Eigen::VectorXd fx(1);
  fx = fun.Forward(0, x); 
  grad  = fun.RevOne(x, 0);
  
  //Rprintf("\nNext fx: %f\n", fx[0]);
  //Rprintf("Next grad: ");
  //for (int i=0; i < grad.size(); i++){
  //  Rprintf("%f ", grad[i]);
  //}
  //Rprintf("\n");
  
  return fx[0];
}





Rcpp::List run(simple_array_2D n_pre_move, simple_array_2D m, simple_array_2D waa, simple_array_3D movement, simple_array_2D selq, double effort_mult_initial, Rcpp::NumericVector target, Rcpp::IntegerVector target_type, Rcpp::IntegerVector fishery_map, Rcpp::NumericVector max_effort, const unsigned int max_solver_iters){
  
  bool verbose = false;
  auto nfisheries = selq.get_dim()[1];
  if(verbose){Rprintf("Number of fisheries to solve effort for: %i\n", nfisheries);}
  
  // Get n after movement - not dependent on effort so don't include in tape section
  simple_array_2D n_after_move = get_n_after_movement(n_pre_move, movement);

  // Bounded - needs different header file
  LBFGSpp::LBFGSBParam<double> param;  // New parameter class
  param.epsilon = 1e-9;
  param.epsilon_rel = 1e-9;
  param.max_iterations = max_solver_iters;
  // Bounds
  Eigen::VectorXd lb = Eigen::VectorXd::Constant(nfisheries, -21.0);
  // Upper bound set by max effort
  Eigen::VectorXd ub = Eigen::VectorXd(nfisheries);
  for(int fcount=0; fcount < nfisheries; fcount++){
    ub[fcount] = log(max_effort[fcount]);
  }
  // Create solver object
  LBFGSpp::LBFGSBSolver<double> solver(param);  // New solver class
									   
  // Initialise function to be solved - do taping in constructor
  EffortFun effort_fun(nfisheries, n_after_move, m, waa, selq, target, target_type, fishery_map);
  // Initial guess
  Eigen::VectorXd x = Eigen::VectorXd::Zero(nfisheries);
  double fx;
  int niter = solver.minimize(effort_fun, x, fx, lb, ub);
  
  //Rprintf("niter: %i\n", niter);
  //Rprintf("x[0]: %f\n", x[0]);
  //Rprintf("f(x): %f\n", fx);
  
  std::vector<double> final_effort(nfisheries, 0.0);
  // Assuming initial effort is 1.0 !
  for(int fcount=0; fcount<nfisheries; fcount++){
    final_effort[fcount] = exp(x[fcount]);
  }
  
  return Rcpp::List::create(
    Rcpp::Named("effort", final_effort),
    Rcpp::Named("solver_iters", niter),
    Rcpp::Named("final_value", fx));
}


// Just function exposed to R
// [[Rcpp::export]]
Rcpp::List find_effort(simple_array_2D n_pre_move, simple_array_2D m, simple_array_2D waa, simple_array_3D movement, simple_array_2D selq, double effort_mult_initial, Rcpp::NumericVector target, Rcpp::IntegerVector target_type, Rcpp::IntegerVector fishery_area, Rcpp::NumericVector max_effort, const unsigned int max_solver_iters){
  
  Rcpp::List out = run(n_pre_move, m, waa, movement, selq, effort_mult_initial, target, target_type, fishery_area, max_effort, max_solver_iters);
  
  return out;
}
