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
// fishery_area: nunit


simple_array_2D get_n_after_movement(simple_array_2D n_pre_move, simple_array_3D movement){
	// Make a new simple_array_2D that is filled with 0s
	std::vector<unsigned int> ndim = n_pre_move.get_dim();
	simple_array_2D n_after_move(ndim[0], ndim[1], 0.0);
	// for loop over age
	// movement at age %*% n at age
	// [narea x narea] %*% n[narea]
	auto nages = n_pre_move.get_dim()[0];
	auto nareas = n_after_move.get_dim()[1];
	for (int age_count = 0; age_count < nages; age_count++){
		for(int i = 0; i < nareas; i++){
			for(int j = 0; j < nareas; j++){
				n_after_move(age_count, i) += movement(i,j,age_count) * n_pre_move(age_count, j);
			}
		}
	}
	return n_after_move;
}

// Given the fishing effort, return the catch wt given all the other information
std::vector<adouble> get_catch_wt(std::vector<adouble>& effort, simple_array_2D& n_after_move, simple_array_2D& m, simple_array_2D& waa, simple_array_2D& selq, Rcpp::IntegerVector& fishery_area){
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
      auto area_index = fishery_area[fishery_count] - 1;
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
      auto area_index = fishery_area[fishery_count] - 1;
      adouble catch_n_temp = (f_fishery[fishery_count] / total_z[area_index]) * (1 - exp(-1 * total_z[area_index])) * n_after_move(age_count, area_index);
      // Multiply CN at age by Waa and sum into total catch wt
      total_catch_wt[fishery_count] += catch_n_temp * waa(age_count, area_index);
    }
   //if(age_count == 5){return(total_z);} // Just testing if total Z is coming out OK
  }
  return total_catch_wt;
}


// target_type: 0 = catch, 1 = effort
Rcpp::List run(simple_array_2D n_pre_move, simple_array_2D m, simple_array_2D waa, simple_array_3D movement, simple_array_2D selq, double effort_mult_initial, Rcpp::NumericVector target, Rcpp::IntegerVector target_type, Rcpp::IntegerVector fishery_map){
  // Chatty mode
  bool verbose = false;
  if(verbose){Rprintf("\nIn run()\n");}
  
  // Effort multiplier is the independent value. There is one independent value for each effort, i.e. for each fishery
  // Change log effort multiplier, to prevent -ve effort values
  auto nages = selq.get_dim()[0];
  auto nareas = n_pre_move.get_dim()[1];
  auto nfisheries = selq.get_dim()[1];
  if(verbose){Rprintf("Number of fisheries to solve effort for: %i\n", nfisheries);}
  
  // Make initial effort vector
  double effort_initial = 1.0;
  Rcpp::NumericVector effort(nfisheries, effort_initial);
  std::vector<adouble> effort_ad(nfisheries, effort_initial);
  
  // Special case. If target type is all effort, don't do any solving. Just return target effort.
  bool all_effort_targets = true;
  for(int fishery_count = 0; fishery_count < nfisheries; fishery_count++){
    // Any target type is catch, then not all effort targets
    if(target_type[fishery_count] == 0){
      all_effort_targets = false;
    }
  }
  if(all_effort_targets){
    if(verbose){Rprintf("All effort targets. Loading return effort with target and returning.\n");}
    // Load output effort with target and exit
    effort = target;
    return Rcpp::List::create(
      Rcpp::Named("effort", effort),
      Rcpp::Named("solver_code", 1000));
    
    
  }
  
  // Make an adouble vector version of the initial mult so every fishery has an initial effort mult
  // This will be the independent variable 
  std::vector<adouble> effort_mult_ad(nfisheries, effort_mult_initial);
  std::vector<adouble> log_effort_mult_ad(nfisheries, log(effort_mult_initial)); // Want this to be indep. variable
  if(verbose){Rprintf("Effort_mult_ad: %f\n", Value(effort_mult_ad[0]));}
  if(verbose){Rprintf("log_effort_mult_ad: %f\n", Value(log_effort_mult_ad[0]));}


  // Get n after movement - not dependent on effort so don't include in tape section
  simple_array_2D n_after_move = get_n_after_movement(n_pre_move, movement);
  
  // Don't print any Value() things when inside the tape - it bombs!!!
  /* ***************************************************************** */
  // Tape has effort_mult_ad as the independent variable
  if(verbose){Rprintf("\nTurning on tape\n");}
  //CppAD::Independent(effort_mult_ad);
  CppAD::Independent(log_effort_mult_ad);
  
  if(verbose){Rprintf("\nUpdating effort with multipler\n");}
  // new effort = initial effort * mult
  
  // Put exponent of log_effort_multiplier into effort multiplier
  std::transform(log_effort_mult_ad.begin(), log_effort_mult_ad.end(), effort_mult_ad.begin(), [](adouble x) {return exp(x);});
  // And multiply to get effort
  std::transform(effort_ad.begin(), effort_ad.end(), effort_mult_ad.begin(), effort_ad.begin(), std::multiplies<adouble>());
  
  
  //// Conditional to set max effort - not sure it helps
  //adouble max_effort = 10.0;
  //for (int fishery_count= 0; fishery_count < nfisheries; fishery_count++){
  //  // If effort >  10, then effort = 10
  //  effort_ad[fishery_count] = CppAD::CondExpGt(effort_ad[fishery_count], max_effort, max_effort, effort_ad[fishery_count]);
  //}
  
  
  // Get catch weight per fishery with that effort
  if(verbose){Rprintf("\nGetting catch target hat\n");}
  std::vector<adouble> total_catch_weight_ad = get_catch_wt(effort_ad, n_after_move, m, waa, selq, fishery_map);
  
  // Calculate error
  std::vector<adouble> error(nfisheries);
  if(verbose){Rprintf("\nCalculating error\n");}
  // Go fishery by fishery
  // If target_type == 0, then catch target
  // If target_type == 1, then effort target
  // Get error as appropriate
  for (int fishery_count = 0; fishery_count < nfisheries; fishery_count++){
    // Catch target
    // Changed error to be log(target) - log(target_hat) - requires fewer solver iterations - 22/04/2025
    if(target_type[fishery_count] == 0){
      error[fishery_count] = log(target[fishery_count]) - log(total_catch_weight_ad[fishery_count]);
      // Forgot - you can't print things out when being taped
    } else if(target_type[fishery_count] == 1){
      error[fishery_count] = log(target[fishery_count]) - log(effort_ad[fishery_count]);
      //Rprintf("fishery: %i Effort target. target: %f  effort_ad: %f error: %f\n", fishery_count, target[fishery_count], Value(effort_ad[fishery_count]), Value(error[fishery_count]));
    } else {
      Rcpp::stop("Unrecognised target type for fishery %i.", fishery_count);
    }
  }
  
  // Turn off tape
  if(verbose){Rprintf("\nTurning off tape and linking effort to error\n");}
  //CppAD::ADFun<double> fun(effort_mult_ad, error);
  CppAD::ADFun<double> fun(log_effort_mult_ad, error);
  
  // ------------------------------------------------
  
  for (int fishery_count = 0; fishery_count < nfisheries; fishery_count++){
    if(target_type[fishery_count] == 0){
      if(verbose){Rprintf("fishery: %i Catch target. target: %f  catch_hat: %f error: %f\n", fishery_count, target[fishery_count], Value(total_catch_weight_ad[fishery_count]), Value(error[fishery_count]));}
    } else if(target_type[fishery_count] == 1){
      if(verbose){Rprintf("fishery: %i Effort target. target: %f  effort_ad: %f error: %f\n", fishery_count, target[fishery_count], Value(effort_ad[fishery_count]), Value(error[fishery_count]));}
    } else {
      Rcpp::stop("Unrecognised target type for fishery %i.", fishery_count);
    }
  }
  
  /* ***************************************************************** */
  // Take a look at error
  if(verbose){
    Rprintf("\nError[i]: ");
    for(int icount=0; icount<nfisheries; icount++){
      Rprintf(" %f", Value(error[icount]));
    }
    Rprintf("\n");
  }
  
  /* ***************************************************************** */
  
  // Testing CppAD bits
  //std::vector<double> y(nfisheries, 1000.0);
  //std::vector<double> delta_indep(nfisheries, 0.0); // For updating indep in final step
  //std::vector<double> jac(nfisheries * nfisheries);
  //std::vector<double> effort_mult_temp(nfisheries, effort_mult_initial);
  //std::vector<double> log_effort_mult_temp(nfisheries, log(effort_mult_initial));
  //// First iter looks OK - then bombs
  //for(int iter=0; iter<2; iter++){
  //  Rprintf("\niter: %i\n", iter);
  //
  //  // Eval the taped function that returns ERROR given effort mult
  //  y = fun.Forward(0, log_effort_mult_temp); 
  //  Rprintf("\nEval function. y[i]: ");
  //  for(int icount=0; icount<nfisheries; icount++){
  //    Rprintf(" %f", y[icount]);
  //  }
  //  Rprintf("\n");
  //  // Get Jacobian
  //  jac = fun.Jacobian(log_effort_mult_temp);
  //  //Rprintf("\nJacobian[i][j]: ");
  //  //for(int jcount=0; jcount<nfisheries; jcount++){
  //  //  for(int icount=0; icount<nfisheries; icount++){
  //  //    Rprintf(" %f", jac[(jcount * nfisheries) + icount]);
  //  //  }
  //  //  Rprintf("\n");
  //  //}
  //  //Rprintf("\n");
  //  // Solve J x = y
  //  double logdet = 0.0; // Used in the CppAD LUsolve function, after solving has log of the determinant
  //  CppAD::LuSolve(nfisheries, 1, jac, y, delta_indep, logdet); 
  //  Rprintf("\nLU Solve. logdet: %f: \n", logdet);
  //  // determinant of jac = signdet * exp(logdet), so if 0 it is a problem
  //  Rprintf("LU Solve. delta_indep[i]: ");
  //  //for(int icount=0; icount<nfisheries; icount++){
  //  //  Rprintf(" %f", delta_indep[icount]);
  //  //}
  //  Rprintf("\n");
  //  // Update log effort mult with new log effort
  //  std::transform(log_effort_mult_temp.begin(), log_effort_mult_temp.end(), delta_indep.begin(), log_effort_mult_temp.begin(),std::minus<double>());
  //  Rprintf("\nNew indep[i]: ");
  //  for(int i (default position, if it hasn't stopped for any other reason then it's because the iterations have maxed out)count=0; icount<nfisheries; icount++){
  //    Rprintf(" %f", log_effort_mult_temp[icount]);
  //  }
  //  Rprintf("\n");
  //}
  
  // Is jac being solved OK?
  // Output jac and indep to R and run solve() on them
  // How to prevent negative effort multiplier? Log?
  
  
  /* ***************************************************************** */
  // Solve and exit
  
  std::vector<double> effort_mult(nfisheries, effort_mult_initial);
  std::vector<double> log_effort_mult(nfisheries, log(effort_mult_initial));
  if(verbose){Rprintf("Calling solver\n");}
  int solver_code = 0;
  solver_code = newton_raphson(log_effort_mult, fun, 50, 1e-9);
  if(verbose){Rprintf("Done solving\n");}
  if(verbose){Rprintf("solver_code: %i\n", solver_code);}
  
  // Apply new effort multiplier to effort and return
  //std::transform(log_effort_mult.begin(), log_effort_mult.end(), effort_mult.begin(), std::exp<double>());
  std::transform(log_effort_mult.begin(), log_effort_mult.end(), effort_mult.begin(), [](double x) {return exp(x);});
  std::transform(effort.begin(), effort.end(), effort_mult.begin(), effort.begin(), std::multiplies<double>());
  
  return Rcpp::List::create(
    Rcpp::Named("effort", effort),
    Rcpp::Named("solver_code", solver_code));
  
  
  //return effort;
}


// Function exposed to R
// [[Rcpp::export]]
Rcpp::List find_effort(simple_array_2D n_pre_move, simple_array_2D m, simple_array_2D waa, simple_array_3D movement, simple_array_2D selq, double effort_mult_initial, Rcpp::NumericVector target, Rcpp::IntegerVector target_type, Rcpp::IntegerVector fishery_area){
  
  Rcpp::List out = run(n_pre_move, m, waa, movement, selq, effort_mult_initial, target, target_type, fishery_area);
  
  return out;
}

