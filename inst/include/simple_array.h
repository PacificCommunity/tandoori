/* 
 * Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott
 */

#include "cppad/cppad.hpp" // CppAD package http://www.coin-or.org/CppAD/

#include <Rcpp.h>

#define _simple_array_

typedef CppAD::AD<double> adouble;

// Don't know if we need all these methods - looks a bit overkill
template <typename T>
class simple_array_2D_base {
	public:
        /* Constructors */
		simple_array_2D_base();
		simple_array_2D_base(SEXP array_sexp); // Used as intrusive 'as'
        operator SEXP() const; // Used as intrusive 'wrap' - just implemented double version. Could implement adouble if needed.
		simple_array_2D_base(const simple_array_2D_base& simple_array_2D_base_source); // copy constructor to ensure that copies (i.e. when passing to functions) are deep
		simple_array_2D_base& operator = (const simple_array_2D_base& simple_array_2D_source); // Assignment operator for a deep copy
        ////simple_array_2D_base(const unsigned int nquant, const unsigned int nyear, const unsigned int nunit, const unsigned int nseason, const unsigned int narea, const unsigned int niter, const T value=0.0); // Make an empty simple_array_2D
        ////simple_array_2D_base(const std::vector<unsigned int> dims, const T value=0.0);

        //// Specialised constructor to make an simple_array_2DAD from an simple_array_2D
        //template <typename T2>
		//simple_array_2D_base(const simple_array_2D_base<T2>& simple_array_2D_source); 

		///* Get accessors */
        //std::vector<T> get_data() const;
        std::vector<unsigned int> get_dim() const;

		///* Set accessors */
		//void set_data(const std::vector<T>& data_in);

        ///* () get accessors - just get so const reinforced */
		unsigned int get_data_element(const unsigned int x, const unsigned int y) const;
        ///* Get single values */
		//T operator () (const unsigned int element) const; 
		T operator () (const unsigned int x, const unsigned int y) const; 

        ///* () get and set accessors - const not reinforced */
		//T& operator () (const unsigned int element); 
		T& operator () (const unsigned int x, const unsigned int y);

    protected:
        std::vector<T> data;
        std::vector<unsigned int> dim;
};

typedef simple_array_2D_base<double> simple_array_2D;
typedef simple_array_2D_base<adouble> simple_array_2DAD;






