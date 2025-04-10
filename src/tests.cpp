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




