/* 
 * Copyright 2025 Finlay Scott. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott
 */

#include "../inst/include/simple_array.h"

/*! \brief Default constructor 
 *
 * Creates an empty 2D array with no dims 
 */
template<typename T>
simple_array_2D_base<T>::simple_array_2D_base(){
    data = std::vector<T>();
    dim = std::vector<unsigned int>();
}

/*! \brief Constructor 
 *
 * Creates a 2D array of dim x,y filled with whatever
 */
template<typename T>
simple_array_2D_base<T>::simple_array_2D_base(const unsigned int x, const unsigned int y, const T fill){
    data = std::vector<T>(x*y, fill);
	dim = {x,y};
}

/*! \brief Generic SEXP constructor used as intrusive as
 *
 * \param array_sexp an array from R
 */
template<typename T>
simple_array_2D_base<T>::simple_array_2D_base(SEXP array_sexp){
    // We need it to be a NumericVector (so we can get dim)
	Rcpp::NumericVector data_nv = array_sexp;
    dim = Rcpp::as<std::vector<unsigned int>>(data_nv.attr("dim"));
	// If array dim > 2
	if (dim.size() > 2){
		Rcpp::stop("Expecting 2D array.\n");
	}
    // Sort out data - need to copy across as maybe AD
    // Initialise data to the correct size
    data.reserve(std::accumulate(dim.begin(), dim.end(), 1, std::multiplies<unsigned int>()));
    data.insert(data.begin(), data_nv.begin(), data_nv.end());
}

// Specialise the wrap for a simple_array_2D_base<double>
/*! \brief Specialised intrusive wrap for simple_array_2d_base<double>
 */
template<>
simple_array_2D_base<double>::operator SEXP() const{
    Rcpp::NumericVector data_nv(data.size());
    // Filling this up takes a long time
    // No NV.insert but can use transform and a lambda (sort of)
    std::transform(data.begin(), data.end(), data_nv.begin(),
        [](double x) { return x; } );
    // Apply dims and dimnames
	data_nv.attr("dim") = dim;
    return Rcpp::wrap(data_nv);
}


// Copy constructor - else 'data' can be pointed at by multiple instances
template<typename T>
simple_array_2D_base<T>::simple_array_2D_base(const simple_array_2D_base<T>& simple_array_2D_source){
	data  = simple_array_2D_source.data; // std::vector always does deep copy
	dim  = simple_array_2D_source.dim; // std::vector always does deep copy
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
template<typename T>
simple_array_2D_base<T>& simple_array_2D_base<T>::operator = (const simple_array_2D_base<T>& simple_array_2D_source){
	if (this != &simple_array_2D_source){
        data  = simple_array_2D_source.data; // std::vector always does deep copy
        dim = simple_array_2D_source.dim; // std::string always does deep copy
	}
	return *this;
}

template <typename T>
std::vector<unsigned int> simple_array_2D_base<T>::get_dim() const{
    return dim;
}

// Note that elements start at 0 = C++ style
template <typename T>
unsigned int simple_array_2D_base<T>::get_data_element(const unsigned int x, const unsigned int y) const{
    // Check that quant etc >= 0
    if ((x < 0) || (y < 0)){
            Rcpp::stop("In simple_array_2D accessor. x and y must be >= 0\n");
    }
    //std::vector<unsigned int> dim = get_dim();
    if ((x >= dim[0]) || (y >= dim[1])){
            Rcpp::stop("Trying to access element outside of x or y dim range.");
    }
	unsigned int element = (dim[0] * y) + x; 
	return element;
}


// Get only data accessor - all dims
template <typename T>
T simple_array_2D_base<T>::operator () (const unsigned int x, const unsigned int y) const{
	unsigned int element = get_data_element(x, y);
	return data[element];
}

// Data accessor - all dims
template <typename T>
T& simple_array_2D_base<T>::operator () (const unsigned int x, const unsigned int y){
	unsigned int element = get_data_element(x, y);
	return data[element];
}

//------------------ begin and end ---------------------------------
// These are used for for_range and iterators

template <typename T>
typename simple_array_2D_base<T>::iterator simple_array_2D_base<T>::begin(){
    return data.begin();
}

template <typename T>
typename simple_array_2D_base<T>::iterator simple_array_2D_base<T>::end(){
    return data.end();
}

template <typename T>
typename simple_array_2D_base<T>::const_iterator simple_array_2D_base<T>::begin() const {
    return data.begin();
}

template <typename T>
typename simple_array_2D_base<T>::const_iterator simple_array_2D_base<T>::end() const {
    return data.end();
}

/*----------------------------------------------------*/
/* Explicit instantiations - alternatively put all the definitions into the header file
 * This way we have more control over what types the functions work with
 */
// Explicit instantiation of class
template class simple_array_2D_base<double>;
template class simple_array_2D_base<adouble>;



