/* 
 * Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott
 */


#include "../inst/include/solver.h"

double euclid_norm(std::vector<double> x){
    double xsum = std::inner_product(x.begin(), x.end(), x.begin(), 0.0);
    xsum = sqrt(xsum);
    return xsum;
}

// We should offer the option of doing iterations in chunks - i.e. if 5000 iters, do 1000 at a time else Jacobian becomes massive and we hit memory problems
// And we need to make sure that solving for multiple targets (e.g. if two fleets and we have two fmults) works
// So we pass in the number of iterations we want to solve, and how many simultaneous targets there in that iteration, i.e. the dimension of the problem
// As each iteration is indpendent, we solve each iteration of simultaneous targets separately
// find x: f(x) = 0
// x1 = x0 - f(x0) / f'(x0)
// w = f(x0) / f'(x0)
// We want w.
// Rearrange w:
// f'(x0) w = f(x0)
// We use LU solve, give it f'(x0) and f(x0) to get w
// x1 = x0 - w
// Need to add max_limit

/*! \brief A simple Newton-Raphson optimiser
 *
 * The Newton-Raphons optimiser uses the Jacobian matrix calculated by CppAD.
 * Limits are applied to the minimum and maximum value of indep. These limits are applied while solving to prevent the solver going to strange places.
 * \param indep The initial values of the independent values.
 * \param fun The CppAD function object.
 * \param indep_min The minimum value of the independent variable (default is 0).
 * \param indep_max The maximum value of the independent variable (default is 1000).
 * \param max_iters The maximum number of solver iterations (not FLR iterations).
 * \param tolerance The tolerance of the solutions.
 */


int newton_raphson(std::vector<double>& indep, CppAD::ADFun<double>& fun, const unsigned int max_iters, const double tolerance){
    unsigned int nindep = indep.size();
    bool verbose = false;
    if(verbose){
      Rprintf("\nIn Newton Raphson\n");
      Rprintf("indep.size(): %i\n", nindep);
    }
    
    double logdet = 0.0; // Used in the CppAD LUsolve function, after solving has log of the determinant
    std::vector<double> y(nindep, 1000.0);
    std::vector<double> delta_indep(nindep, 0.0); // For updating indep in final step
    std::vector<double> jac(nindep * nindep);
    // Reasons for stopping
    //  1 - Solved within tolerance
    // -1 - Iteration limit reached (default position, if it hasn't stopped for any other reason then it's because the iterations have maxed out)
    // -2 - Min limit reached
    // -3 - Max limit reached
    int success_code = -1; 
    unsigned int nr_count = 0;
    // Keep looping until target has been solved, or number of iterations has been hit
    while(nr_count < max_iters){ 
        ++nr_count;
        if(verbose){Rprintf("Iter count: %i\n", nr_count);}
    
        // Get f(x0). Eval function at current independent value (current effort mult)
        y = fun.Forward(0, indep); 
        if(verbose){
          Rprintf("Eval function. y[i]: ");
          for(int icount=0; icount<nindep; icount++){
            Rprintf(" %f", y[icount]);
          }
          Rprintf("\n");
        }
    
        // Did we hit tolerance?
        if (euclid_norm(y) < tolerance){
          if(verbose){Rprintf("Solved within tolerance!\n");}
          success_code = 1;
          break;
        }
    
        // Get f'(x0) -  gets Jacobian for all simultaneous targets
        // Sparse or no?
        jac = fun.Jacobian(indep);
        //Rprintf("\nJacobian[i][j]: ");
        //for(int jcount=0; jcount<nindep; jcount++){
        //  for(int icount=0; icount<nindep; icount++){
        //    Rprintf(" %f", jac[(jcount * nindep) + icount]);
        //  }
        //  Rprintf("\n");
        //}
        //Rprintf("\n");
        
        // Solve to get w = f(x0) / f'(x0)
        // Jacobian must be square - i.e no of indeps must equal no. deps
        CppAD::LuSolve(nindep, 1, jac, y, delta_indep, logdet); 
        if(logdet == 0.0){
          Rcpp::stop("In solver. LUSolve returned logdet == 0 which means it is computationally singular. This Probably means an infinite number of solutions to the linear system Jac * x = y. This probably means that any value of effort mult will hit your catch target, and this probably means that your selq is 0 for all ages for at least one fishery.\n");
        }
        
        if(verbose){
          Rprintf("\nLU Solve. delta_indep[i]: ");
          for(int icount=0; icount<nindep; icount++){
            Rprintf(" %f", delta_indep[icount]);
          }
          Rprintf("\n");
        }
        
        // Update x = x - w
        // Ideally should only update the iterations that have not hit the tolerance
        std::transform(indep.begin(), indep.end(), delta_indep.begin(), indep.begin(),std::minus<double>());
        if(verbose){
          Rprintf("\nNew indep[i]: ");
          for(int icount=0; icount<nindep; icount++){
            Rprintf(" %f", indep[icount]);
          }
          Rprintf("\n");
        }
    //    
    //    // Bluntly enforce limits
    //    // indep cannot be less than minimum value or greater than maximum value
    //    // Limit during solving loop to prevent the solver going off to weird places? Yes
    //    // Or just ID the breached iters at the end and correct them (even though solver may go outside limit on way to solution within limit)
    //    // Should each indep value have it's own limit? - maybe later
    //    //for (unsigned int minmax_counter = 0; minmax_counter < indep.size(); ++minmax_counter){
    //    //    // Have we breached min limit?
    //    //    if (indep[minmax_counter] <= indep_min){
    //    //        indep[minmax_counter] = indep_min;
    //    //        success_code[minmax_counter / nsim_targets] = -2;
    //    //    }
    //    //    // Have we breached max limit?
    //    //    if (indep[minmax_counter] >= indep_max){
    //    //        indep[minmax_counter] = indep_max;
    //    //        success_code[minmax_counter / nsim_targets] = -3;
    //    //    }
    //    //} 
    }
    //if(verbose){Rprintf("\nLeaving solver after %i iterations.\n\n", nr_count);}
    return success_code;
}



//int newton_raphson(std::vector<double>& indep, CppAD::ADFun<double>& fun, const double indep_min, const double indep_max, const unsigned int max_iters, const double tolerance){
//  int fuckoff = 0;
//  
//  
//  
//  
//  
//  
//  
//  
//  
//  return fuckoff;
//}
