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
    // -1 - Iteration limit reached
    int success_code = -1; 
    unsigned int nr_count = 0;
    // Keep looping until target has been solved, or number of iterations has been hit
    while(nr_count < max_iters){ 
    //while(nr_count < 20){ 
        ++nr_count;
        if(verbose){Rprintf("\nIter count: %i\n", nr_count);}
    
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
        if(verbose){
          Rprintf("logdet: %f\n", logdet);
        }
        // This error message is not quite right.
        // If all effort projection, then jacobian is a diagonal matrix which also sets logdet to 0.
        // However, this is not a problem, and the sovled system is OK.
        // The problem is when the target for a fishery is 0, and selq is 0, meaning that any effort mult is a solution.
        // This results in an infinite number of solutions and a problem.
        // Anyway to detect this special case?
        // Two situations where logdet == 0.
        // Catch target of 0 for a fishery with selq = 0 (any effort mult = catch of 0 - infinite solutions) - LUSolve gives bad solution
        // All effort targets - LUSolve is OK but doesn't need solving anyway
        // Reinstate error message and don't call solver if all effort targets
        
        if(logdet == 0.0){
          Rcpp::stop("In solver. LUSolve returned logdet == 0.\nThis potentially means that the system is computationally singular.\i.e. there are an infinite number of solutions to the linear system Jac * x = y.\nAnd this means that any value of effort mult will hit your target.\nAnd this probably means that your selq is 0 for all ages for at least one fishery.\n");
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
        
        // Bluntly enforce limits - horrible mathematically but might be enough to stop solver going to a bad place
        // while it trundles around.
        for (int indep_count = 0; indep_count < nindep; indep_count ++){
          // Too restrictive with 'real' effort and a start effort of 1
          //if (indep[indep_count] >= log(10.0)){
          //  //if(verbose){Rprintf("Fishery %i hit indep limit\n", indep_count + 1);}
          //  indep[indep_count] = log(10.0);
          //}
          //if (indep[indep_count] >= 10.0){
          //  if(verbose){Rprintf("Fishery %i hit indep max limit\n", indep_count + 1);}
          //  indep[indep_count] = 10.0;
          //}
          // Just eats up all iters as trying to get to 0
          if (indep[indep_count] <= log(1e-12)){
            //if(verbose){Rprintf("Fishery %i hit indep min limit\n", indep_count + 1);}
            indep[indep_count] = log(1e-12);
          }
        }
        
    }
    if(verbose){Rprintf("\nLeaving solver after %i iterations.\n\n", nr_count);}
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
