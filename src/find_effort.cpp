/*
 * Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott
 */

#include "../inst/include/find_effort.h"

//' @name get_error
//' @title Defines f(x) for effort minimisation.
//'
//' @description For catch fisheries:
//' \deqn{g(x) = \sum [log(C_{target}) - log(C(x))]^2}
//' For effort fisheries:
//' \deqn{h(x) = \sum [log(E_{target}) - log(x)]^2}
//' Error function is then \eqn{f(x) = \sqrt{g(x) + h(x)}}.
//'
//' @param nfisheries_ an integer number of fisheries
//' @param n_after_move a 2D array of numbers-at-age by area
//' @param m natural a 2D array of natural mortality-at-age by area
//' @param waa a 2D array of weight-at-age by fishery
//' @param selq a 2D array of selectivity-at-age by fishery
//' @param target numeric vector of targets by fishery
//' @param target_type integer vector of target types by fishery (0 = catch, 1 = effort)
//' @param fishery_map integer vector indicating which area each fishery operates in
//' @return f(x) evaluated at x.
adouble get_error(std::vector<adouble>& log_effort_mult, simple_array_2D& n_after_move, simple_array_2D& m, simple_array_2D& waa, simple_array_2D& selq, Rcpp::NumericVector& target, Rcpp::IntegerVector& target_type, Rcpp::IntegerVector& fishery_map){
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
    error[fcount] = log(target[fcount] / hat[fcount]); // I think this is where it breaks because target is larger than hat
  }
  // Square, sum and return
  // Force type of initial value to be adouble
  adouble out = std::inner_product(error.begin(), error.end(), error.begin(), adouble{0.0});
  out = sqrt(out);
  return out;
}

EffortFun::EffortFun(int nfisheries_, simple_array_2D& n_after_move, simple_array_2D& m, simple_array_2D& waa, simple_array_2D& selq, Rcpp::NumericVector& target, Rcpp::IntegerVector& target_type, Rcpp::IntegerVector& fishery_map) {
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

//' Finds the effort that achieves the supplied catch target.
//'
//' Originally, solve_effort() was not exposed to R, but now it is exported to help with documentation.
//' Uses the LBFGSBSolver to perform minimisation. The minimised function, f(x), is of type EffortFun. See ?get_error.
//'
//' @param n_pre_move a 2D array of numbers-at-age by area
//' @param n0_pre_move a 2D array of unfished numbers-at-age by area
//' @param m a 2D array of natural mortality-at-age by area
//' @param waa a 2D array of weight-at-age by fishery
//' @param movement a 3D array of movement-at-age
//' @param selq a 2D array of selectivity-at-age by fishery
//' @param target numeric vector of targets by fishery
//' @param target_type integer vector of target types by fishery (0 = catch, 1 = effort)
//' @param fishery_area integer vector indicating which area each fishery operates in
//' @param max_effort numeric vector of maxmimum effort by fishery
//' @param max_solver_iters integer maximum number of solver iterations
//' @return list of effort, solver info and updated population quantities
// [[Rcpp::export]]
Rcpp::List solve_effort(simple_array_2D n_pre_move, simple_array_2D m, simple_array_2D waa, simple_array_3D movement, simple_array_2D selq, Rcpp::NumericVector target, Rcpp::IntegerVector target_type, Rcpp::IntegerVector fishery_map, Rcpp::NumericVector max_effort, const unsigned int max_solver_iters){

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

  EffortFun effort_fun(nfisheries, n_after_move, m, waa, selq, target, target_type, fishery_map);
  Eigen::VectorXd x = Eigen::VectorXd::Zero(nfisheries);
  double fx = 0;

  int ncatchfisheries = std::count(target_type.begin(), target_type.end(), 0);
  std::vector<double> final_effort(nfisheries, 0.0);
  int niter = 0;
  Rcpp::String message("NULL");

  // Only run solver if there are catch-based fisheries
  if(ncatchfisheries>0){
    niter = solver.minimize(effort_fun, x, fx, lb, ub);
    // Assuming initial effort is 1.0 !
    for(int fcount=0; fcount<nfisheries; fcount++){
      final_effort[fcount] = exp(x[fcount]);
    }
    message = "Solver successful";
  } else {
    // if all fisheries are effort-based, then the target is achieved
    for (int i = 0; i < nfisheries; i++) {
      final_effort[i] = target[i];
    }
    message = "No catch fisheries: skipped solver";
  }

  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("effort", final_effort),
    Rcpp::Named("solver_iters", niter),
    Rcpp::Named("final_value", fx),
    Rcpp::Named("message") = message
  );


  return out;
}


//' Finds the effort that achieves the supplied catch target.
//'
//' find_effort() is a wrapper for solve_effort() that is exposed to R.
//'
//' @param n_pre_move a 2D array of numbers-at-age by area
//' @param n0_pre_move a 2D array of unfished numbers-at-age by area
//' @param m natural a 2D array of natural mortality-at-age by area
//' @param waa a 2D array of weight-at-age by fishery
//' @param movement a 3D array of movement-at-age
//' @param selq a 2D array of selectivity-at-age by fishery
//' @param target numeric vector of targets by fishery
//' @param target_type integer vector of target types by fishery (0 = catch, 1 = effort)
//' @param fishery_area integer vector indicating which area each fishery operates in
//' @param max_effort numeric vector of maxmimum effort by fishery
//' @param max_solver_iters integer maximum number of solver iterations
//' @return list of solve_effort().
// [[Rcpp::export]]
Rcpp::List find_effort(simple_array_2D n_pre_move, simple_array_2D m, simple_array_2D waa, simple_array_3D movement, simple_array_2D selq, Rcpp::NumericVector target, Rcpp::IntegerVector target_type, Rcpp::IntegerVector fishery_area, Rcpp::NumericVector max_effort, const unsigned int max_solver_iters){

  Rcpp::List out = solve_effort(n_pre_move, m, waa, movement, selq, target, target_type, fishery_area, max_effort, max_solver_iters);

  return out;
}

//' Projects fishery with catch or effort targets.
//'
//' project() is mostly a wrapper for solve_effort() but also applies movement to the numbers-at-age arrays and calculates survivors and catch. Fisheries with catch targets require optimisation of effort to acheive target catch. solve_effort() is an internal function that can be accessed through the exposed function find_effort().
//'
//' @param n_pre_move a 2D array of numbers-at-age by area
//' @param n0_pre_move a 2D array of unfished numbers-at-age by area
//' @param m natural a 2D array of natural mortality-at-age by area
//' @param waa a 2D array of weight-at-age by fishery
//' @param movement a 3D array of movement-at-age
//' @param selq a 2D array of selectivity-at-age by fishery
//' @param target vector of targets by fishery
//' @param target_type vector of target types by fishery (0 = catch, 1 = effort)
//' @param fishery_area integer vector indicating which area each fishery operates in
//' @param max_effort numeric vector of maxmimum effort by fishery
//' @param max_solver_iters integer maximum number of solver iterations
//' @return list of effort, solver info and updated population quantities
// [[Rcpp::export]]
Rcpp::List project(simple_array_2D n_pre_move, simple_array_2D n0_pre_move, simple_array_2D m, simple_array_2D waa, simple_array_3D movement, simple_array_2D selq, Rcpp::NumericVector target, Rcpp::IntegerVector target_type, Rcpp::IntegerVector fishery_area, Rcpp::NumericVector max_effort, const unsigned int max_solver_iters){

  Rcpp::List solve_out = solve_effort(n_pre_move, m, waa, movement, selq, target, target_type, fishery_area, max_effort, max_solver_iters);

  std::vector<double> effort = solve_out["effort"];

  simple_array_2D n_after_move = get_n_after_movement(n_pre_move, movement);
  simple_array_2D n0_after_move = get_n_after_movement(n0_pre_move, movement);

  // Get and survivors and catch_n with new effort
  Rcpp::List pout = get_survivors_and_catch(effort, n_after_move, n0_after_move, m, waa, selq, fishery_area);


  //return pout;
  return Rcpp::List::create(
    Rcpp::Named("effort", solve_out["effort"]),
    Rcpp::Named("solver_iters", solve_out["solver_iters"]),
    Rcpp::Named("final_value", solve_out["final_value"]),
    Rcpp::Named("catch_n", pout["catch_n"]),
    Rcpp::Named("survivors", pout["survivors"]),
    Rcpp::Named("survivors0", pout["survivors0"])
  );
}
