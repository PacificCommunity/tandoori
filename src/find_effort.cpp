/* 
 * Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott
 */

#include "../inst/include/find_effort.h"

// Dims of objects going in:
// n_after_move: nage x narea
// m: nage x narea
// waa: nage x narea
// selq: nage x nunit
// initial_effort_mult: nunit
// fishery_map: nunit

// Given the fishing effort, return the catch wt given all the other information
std::vector<adouble> get_catch_wt(std::vector<adouble>& effort, simple_array_2D& n_after_move, simple_array_2D& m, simple_array_2D& waa, simple_array_2D& selq, Rcpp::IntegerVector& fishery_map){
  // Chatty mode
  bool verbose = false;
  if(verbose){Rprintf("\nIn get_catch_wt()\n");}
  auto nages = selq.get_dim()[0];
  auto nareas = n_after_move.get_dim()[1];
  auto nfisheries = selq.get_dim()[1];
  std::vector<adouble> total_catch_wt(nfisheries, 0.0);
  
  // Get f from each fishery.
  // Get total Z in each area, i.e. F from each fishery in that area, plus m
  // Calc (f / Z) * (1-exp(-Z)) * N
  for (int age_count = 0; age_count < nages; age_count++){
    std::vector<adouble> total_z(nareas,0.0);
    std::vector<adouble> f_fishery(nfisheries,0.0);
    for (int fishery_count = 0; fishery_count < nfisheries; fishery_count++){
      // Get F by fishery
      auto area_index = fishery_map[fishery_count] - 1;
      f_fishery[fishery_count] = effort[fishery_count] * selq(age_count, fishery_count);
      // Add all individual Fs to total Z
      total_z[area_index] += f_fishery[fishery_count];
    }
    // Add m onto total Z
    for (int area_count = 0; area_count < nareas; area_count++){
      total_z[area_count] += m(age_count, area_count);
    }
    // Now get catch by fishery
    for (int fishery_count = 0; fishery_count < nfisheries; fishery_count++){
      auto area_index = fishery_map[fishery_count] - 1;
      adouble catch_n_temp = (f_fishery[fishery_count] / total_z[area_index]) * (1 - exp(-1 * total_z[area_index])) * n_after_move(age_count, area_index);
      // Multiply CN at age by Waa and sum into total catch wt
      total_catch_wt[fishery_count] += catch_n_temp * waa(age_count, area_index);
    }
   //if(age_count == 5){return(total_z);} // Just testing if total Z is coming out OK
  }
  return total_catch_wt;
}



Rcpp::NumericVector run(simple_array_2D n_after_move, simple_array_2D m, simple_array_2D waa, simple_array_2D selq, double effort_mult_initial, Rcpp::NumericVector catch_target, Rcpp::IntegerVector fishery_map){
  // Chatty mode
  bool verbose = false;
  if(verbose){Rprintf("\nIn run()\n");}
  // Effort multiplier is the independent value. There is one independent values for each effort, i.e. for each fishery
  auto nages = selq.get_dim()[0];
  auto nareas = n_after_move.get_dim()[1];
  auto nfisheries = selq.get_dim()[1];
  if(verbose){Rprintf("Number of fisheries to solve effort for: %i\n", nfisheries);}
  
  // Make an adouble vector version of the initial mult so every fishery has an initial effort mult
  // This will be the independent variable 
  std::vector<adouble> effort_mult_ad(nfisheries, effort_mult_initial);
  //std::fill(effort_mult_ad.begin(), effort_mult_ad.end(), effort_mult_initial); // Do we need this fill?
  if(verbose){Rprintf("Effort_mult_ad: %f\n", Value(effort_mult_ad[0]));}
  
  // Make initial effort vector
  double effort_initial = 1.0;
  std::vector<adouble> effort_ad(nfisheries, effort_initial);
  
  // Don't print any Value() things when inside the tape - it bombs!!!
  /* ***************************************************************** */
  // Tape has effort_mult_ad as the independent variable
  if(verbose){Rprintf("\nTurning on tape\n");}
  CppAD::Independent(effort_mult_ad);
  
  if(verbose){Rprintf("\nUpdating effort with multipler\n");}
  // new effort = initial effort * mult
  std::transform(effort_ad.begin(), effort_ad.end(), effort_mult_ad.begin(), effort_ad.begin(), std::multiplies<adouble>());
  
  // Get catch weight per fishery with that effort
  if(verbose){Rprintf("\nGetting catch target hat\n");}
  std::vector<adouble> total_catch_weight_ad = get_catch_wt(effort_ad, n_after_move, m, waa, selq, fishery_map);
  
  // Calculate error
  std::vector<adouble> error(nfisheries);
  if(verbose){Rprintf("\nCalculating error\n");}
  std::transform(catch_target.begin(), catch_target.end(), total_catch_weight_ad.begin(), error.begin(),
      [](double x, adouble y){return y - x;});
  
  // Turn off tape
  if(verbose){Rprintf("\nTurning off tape and linking effort to error\n");}
  CppAD::ADFun<double> fun(effort_mult_ad, error);
  
  /* ***************************************************************** */
  // Take a look at stuff - just for sanity checking
  if(verbose){
    Rprintf("target_catch 1: %f catch_hat 1: %f error 1: %f\n", catch_target[0], Value(total_catch_weight_ad[0]), Value(error[0]));
    Rprintf("\nCatch hat[i]: ");
    for(int icount=0; icount<nfisheries; icount++){
      Rprintf(" %f", Value(total_catch_weight_ad[icount]));
    }
    Rprintf("\n");
    Rprintf("\nError[i]: ");
    for(int icount=0; icount<nfisheries; icount++){
      Rprintf(" %f", Value(error[icount]));
    }
    Rprintf("\n");
  }
  
  /* ***************************************************************** */
  
  //// Solve
  //// Testing CppAD bits
  //std::vector<double> y(nfisheries, 1000.0);
  //std::vector<double> delta_indep(nfisheries, 0.0); // For updating indep in final step
  //std::vector<double> jac(nfisheries * nfisheries);
  //std::vector<double> effort_mult(nfisheries, effort_mult_initial);
  //for(int iter=0; iter<10; iter++){
  //  Rprintf("\niter: %i\n", iter);
  //
  //// Eval the taped function that returns ERROR given effort mult
  //y = fun.Forward(0, effort_mult); 
  //Rprintf("\nEval function. y[i]: ");
  //for(int icount=0; icount<nfisheries; icount++){
  //  Rprintf(" %f", y[icount]);
  //}
  //Rprintf("\n");
  //// Get Jacobian
  //jac = fun.Jacobian(effort_mult);
  ////Rprintf("\nJacobian[i][j]: ");
  ////for(int jcount=0; jcount<nfisheries; jcount++){
  ////  for(int icount=0; icount<nfisheries; icount++){
  ////    Rprintf(" %f", jac[(jcount * nfisheries) + icount]);
  ////  }
  ////  Rprintf("\n");
  ////}
  ////Rprintf("\n");
  //// Solve J x = y
  //double logdet = 0.0; // Used in the CppAD LUsolve function, after solving has log of the determinant
  //CppAD::LuSolve(nfisheries, 1, jac, y, delta_indep, logdet); 
  //Rprintf("\nLU Solve. logdet: %f: \n", logdet);
  //// determinant of jac = signdet * exp(logdet), so if 0 it is a problem
  //Rprintf("LU Solve. delta_indep[i]: ");
  //for(int icount=0; icount<nfisheries; icount++){
  //  Rprintf(" %f", delta_indep[icount]);
  //}
  //Rprintf("\n");
  //// Update effort mult with new effort - but what to do if negative effort - not allowed...
  //std::transform(effort_mult.begin(), effort_mult.end(), delta_indep.begin(), effort_mult.begin(),std::minus<double>());
  //Rprintf("\nNew indep[i]: ");
  //for(int icount=0; icount<nfisheries; icount++){
  //  Rprintf(" %f", effort_mult[icount]);
  //}
  //Rprintf("\n");
  //}
  
  // Is jac being solved OK?
  // Output jac and indep to R and run solve() on them
  // How to prevent negative effort multiplier? Log?
  
  
  
  
  
  if(verbose){Rprintf("Calling solver\n");}
  int solver_code = 0;
  // Fucking linking error when I include this
  std::vector<double> effort_mult(nfisheries, effort_mult_initial);
  solver_code = newton_raphson(effort_mult, fun, 50, 1.5e-8);
  if(verbose){Rprintf("Done solving\n");}
  if(verbose){Rprintf("solver_code: %i\n", solver_code);}
  
  // Calc effort and return it
  //std::vector<double> effort(nfisheries, effort_initial);
  Rcpp::NumericVector effort(nfisheries, effort_initial);
  std::transform(effort.begin(), effort.end(), effort_mult.begin(), effort.begin(), std::multiplies<double>());
  
  return effort;
}


