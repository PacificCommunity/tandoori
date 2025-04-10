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
		simple_array_2D_base(const unsigned int x, const unsigned int y, const T fill=0.0);
		simple_array_2D_base(SEXP array_sexp); // Used as intrusive 'as'
        operator SEXP() const; // Used as intrusive 'wrap' - just implemented double version. Could implement adouble if needed.
		simple_array_2D_base(const simple_array_2D_base& simple_array_2D_base_source); // copy constructor to ensure that copies (i.e. when passing to functions) are deep
		simple_array_2D_base& operator = (const simple_array_2D_base& simple_array_2D_source); // Assignment operator for a deep copy

		///* Get accessors */
        std::vector<unsigned int> get_dim() const;

        ///* () get accessors - just get so const reinforced */
		unsigned int get_data_element(const unsigned int x, const unsigned int y) const;
        ///* Get single values */
		//T operator () (const unsigned int element) const; 
		T operator () (const unsigned int x, const unsigned int y) const; 

        ///* () get and set accessors - const not reinforced */
		//T& operator () (const unsigned int element); 
		T& operator () (const unsigned int x, const unsigned int y);

        /* begin and end and const versions for iterators */
        typedef typename std::vector<T>::iterator iterator;
        iterator begin();
        iterator end();
        typedef typename std::vector<T>::const_iterator const_iterator;
        const_iterator begin() const;
        const_iterator end() const;

    protected:
        std::vector<T> data;
        std::vector<unsigned int> dim;
};

typedef simple_array_2D_base<double> simple_array_2D;
typedef simple_array_2D_base<adouble> simple_array_2DAD;






