/* 
 * Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott
 */

#include "cppad/cppad.hpp" // CppAD package http://www.coin-or.org/CppAD/

#include <Rcpp.h>

#define _simple_array_3D_

typedef CppAD::AD<double> adouble;

template <typename T>
class simple_array_3D_base {
	public:
        /* Constructors */
		simple_array_3D_base();
		simple_array_3D_base(SEXP array_sexp); // Used as intrusive 'as'
        operator SEXP() const; // Used as intrusive 'wrap' - just implemented double version. Could implement adouble if needed.
		simple_array_3D_base(const simple_array_3D_base& simple_array_3D_base_source); // copy constructor to ensure that copies (i.e. when passing to functions) are deep
		simple_array_3D_base& operator = (const simple_array_3D_base& simple_array_3D_source); // Assignment operator for a deep copy

		///* Get accessors */
        std::vector<unsigned int> get_dim() const;

        ///* () get accessors - just get so const reinforced */
		unsigned int get_data_element(const unsigned int x, const unsigned int y, const unsigned int z) const;
        ///* Get single values */
		//T operator () (const unsigned int element) const; 
		T operator () (const unsigned int x, const unsigned int y, const unsigned int z) const; 

        ///* () get and set accessors - const not reinforced */
		//T& operator () (const unsigned int element); 
		T& operator () (const unsigned int x, const unsigned int y, const unsigned int z);

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

typedef simple_array_3D_base<double> simple_array_3D;
typedef simple_array_3D_base<adouble> simple_array_3DAD;






