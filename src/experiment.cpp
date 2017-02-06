#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector callFunction(CharacterVector x, Function f) {
    NumericVector res = f(x);
    return res;
}

// [[Rcpp::export]]
CharacterVector callFunction2(CharacterVector x, CharacterVector y, Function f) {
    CharacterVector res = f(x, y);
    return res;
}

/*** R
callFunction(letters, stringi::stri_width)
callFunction2(letters, "[a-h]", stringi::stri_subset_regex)

*/
