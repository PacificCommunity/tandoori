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

