/* 
 * Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott
 */

#include "../inst/include/tandoori.h"

// A do nothing sort of test just to show that Rcpp and compilation is working OK
// [[Rcpp::export]]
int int_test(int dummy){
	int dummy2 = dummy * 2;
	return dummy2;
}

// Make and initialise a 2D array
// [[Rcpp::export]]
simple_array_2D simple_array_2D_constructor(unsigned int x, unsigned int y, double fill){
    simple_array_2D sa2d(x,y,fill);
	return sa2d;
}

// Pass in and return 2D array as <double>
// [[Rcpp::export]]
simple_array_2D simple_array_2D_as_wrap_test(simple_array_2D sa2d){
    return sa2d;
}

// Access element test
// [[Rcpp::export]]
double simple_array_2D_get_accessor_test(simple_array_2D sa2d, const int x, const int y){
    return sa2d(x,y);
}


// [[Rcpp::export]]
simple_array_2D get_catch_demo(simple_array_2D f, simple_array_2D m, simple_array_2D n, Rcpp::IntegerVector fishery_area){
	// implement:
	// (f / f + m) * (1 - exp(-f -m)) * n
	// Looking for catch by fishery
	const int nfisheries = f.get_dim()[1];
	const int nages = f.get_dim()[0];
	// Where to put result - into array same size as f
	// Should probably set to 0 or NA
	// Maybe add a constructor based on dims?
	simple_array_2D catch_out(f);
	// Loop over fishery
	for (int fcount = 0; fcount < nfisheries; fcount++){
		// Get area that fishery is operating in - need fishery map - vector of length nfisheries
		const int area_index = fishery_area[fcount] - 1;
		for(int acount = 0; acount < nages; acount++){
			double z = f(acount, fcount) + m(acount, area_index);
			catch_out(acount, fcount) = (f(acount, fcount) / z) * (1 - exp(-1 * z)) * n(acount, area_index); 
		}
	}
	// No point writing sweep methods or anything like that - just keep it simple
	// Multiply by weights at age and sum
	// waa by area? Sure - keep biol bits seperate
	return catch_out;
}


// [[Rcpp::export]]
simple_array_2D test_2D_iterator(simple_array_2D sa2d){
	std::fill(sa2d.begin(), sa2d.end(), 0.0);
	return sa2d;
}


// [[Rcpp::export]]
Rcpp::NumericVector test_catch_weight(Rcpp::NumericVector effort_in, simple_array_2D n_after_move, simple_array_2D m, simple_array_2D waa, simple_array_2D selq, Rcpp::IntegerVector fishery_area){
  
  std::vector<adouble> effort(selq.get_dim()[1], 0.0);
  std::transform(effort_in.begin(), effort_in.end(), effort.begin(),
                 [](double x) { return x; } );

  
  std::vector<adouble> cwt = get_catch_wt(effort, n_after_move, m, waa, selq, fishery_area);
    
  Rcpp::NumericVector out(cwt.size());
  std::transform(cwt.begin(), cwt.end(), out.begin(),
                 [](adouble x) { return Value(x); } );
    
  
  return out;
}

// Pass in and return 3D array as <double>
// [[Rcpp::export]]
simple_array_3D simple_array_3D_as_wrap_test(simple_array_3D sa3d){
    return sa3d;
}

// Access element test
// [[Rcpp::export]]
double simple_array_3D_get_accessor_test(simple_array_3D sa3d, const int x, const int y, const int z){
    return sa3d(x,y,z);
}

// Make and initialise a 3D array
// [[Rcpp::export]]
simple_array_3D simple_array_3D_constructor(unsigned int x, unsigned int y, unsigned int z, double fill){
    simple_array_3D sa3d(x,y,z,fill);
	return sa3d;
}


// [[Rcpp::export]]
simple_array_3D test_3D_iterator(simple_array_3D sa3d){
	std::fill(sa3d.begin(), sa3d.end(), 0.0);
	return sa3d;
}

// [[Rcpp::export]]
simple_array_2D movement_test(simple_array_2D n_pre_move, simple_array_3D movement){
	simple_array_2D n_after_move = get_n_after_movement(n_pre_move, movement);

	//// Make a new simple_array_2D that is filled with 0s
	//std::vector<unsigned int> ndim = n_pre_move.get_dim();
	//simple_array_2D n_after_move(ndim[0], ndim[1], 0.0);
	//// for loop over age
	//// movement at age %*% n at age
	//// [narea x narea] %*% n[narea]
	//auto nages = n_pre_move.get_dim()[0];
	//auto nareas = n_after_move.get_dim()[1];
	//for (int age_count = 0; age_count < nages; age_count++){
	//	for(int i = 0; i < nareas; i++){
	//		for(int j = 0; j < nareas; j++){
	//			n_after_move(age_count, i) += movement(i,j,age_count) * n_pre_move(age_count, j);
	//		}
	//	}
	//}
	return n_after_move;
}




