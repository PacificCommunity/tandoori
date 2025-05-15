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

simple_array_2D get_n_after_movement(simple_array_2D n_pre_move, simple_array_3D movement);

std::vector<adouble> get_catch_wt(std::vector<adouble>& effort, simple_array_2D& n_after_move, simple_array_2D& m, simple_array_2D& waa, simple_array_2D& selq, Rcpp::IntegerVector& fishery_map);
