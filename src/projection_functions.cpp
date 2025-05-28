/* 
 * Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott
 */

#include "../inst/include/projection_functions.h"

simple_array_2D get_n_after_movement(simple_array_2D& n_pre_move, simple_array_3D& movement){
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


Rcpp::List get_survivors_and_catch(std::vector<double>& effort, simple_array_2D& n_after_move, simple_array_2D& n0_after_move, simple_array_2D& m, simple_array_2D& waa, simple_array_2D& selq, Rcpp::IntegerVector& fishery_area){
  // Chatty mode
  bool verbose = false;
  if(verbose){Rprintf("\nIn get_survivors_and_catch()\n");}
  auto nages = selq.get_dim()[0];
  auto nareas = n_after_move.get_dim()[1];
  auto nfisheries = selq.get_dim()[1];
  
  // Set up output objects
  simple_array_2D catch_n(nages, nfisheries, 0.0);
  simple_array_2D survivors(nages, nareas, 0.0);
  simple_array_2D survivors0(nages, nareas, 0.0); // Dims?

  // Get f from each fishery.
  // Get total Z in each area, i.e. F from each fishery in that area, plus m
  // Calc (f / Z) * (1-exp(-Z)) * N
  for (int age_count = 0; age_count < nages; age_count++){
    std::vector<double> total_z(nareas,0.0);
    std::vector<double> f_fishery(nfisheries,0.0);
    for (int fishery_count = 0; fishery_count < nfisheries; fishery_count++){
      // Get F by fishery
      auto area_index = fishery_area[fishery_count] - 1;
      f_fishery[fishery_count] = effort[fishery_count] * selq(age_count, fishery_count);
      // Add all individual Fs to total Z
      total_z[area_index] += f_fishery[fishery_count];
    }
    // Add m onto total Z
    // And calculate survivors
    for (int area_count = 0; area_count < nareas; area_count++){
      total_z[area_count] += m(age_count, area_count);
      survivors(age_count, area_count) = n_after_move(age_count, area_count) * exp(-total_z[area_count]);
      survivors0(age_count, area_count) = n0_after_move(age_count, area_count) * exp(-m(age_count, area_count));
    }
    // Now get catch by fishery
    for (int fishery_count = 0; fishery_count < nfisheries; fishery_count++){
      auto area_index = fishery_area[fishery_count] - 1;
      catch_n(age_count, fishery_count) = (f_fishery[fishery_count] / total_z[area_index]) * (1 - exp(-1 * total_z[area_index])) * n_after_move(age_count, area_index);
    }
  }
  // Plus group shenanigans
  for(int area_count = 0; area_count < nareas; area_count++){
    survivors(nages-1, area_count) = survivors(nages-1, area_count) + survivors(nages-2, area_count);
    survivors0(nages-1, area_count) = survivors0(nages-1, area_count) + survivors0(nages-2, area_count);
    for (int age_count = 2; age_count < nages; age_count++){
      survivors(nages - age_count, area_count) = survivors(nages - age_count - 1, area_count);
      survivors0(nages - age_count, area_count) = survivors0(nages - age_count - 1, area_count);
    }
    survivors(0, area_count) = 0;
    survivors0(0, area_count) = 0;
  }

  return Rcpp::List::create(
    Rcpp::Named("catch_n", catch_n),
    Rcpp::Named("survivors", survivors),
    Rcpp::Named("survivors0", survivors0));
}