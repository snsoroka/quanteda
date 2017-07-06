//#include "dev.h"
#include "quanteda.h"
#include "recompile.h"
using namespace quanteda;

// [[Rcpp::export]]
XPtr<Texts> qatd_cpp_tokens(List &texts_){
    
    Texts texts = Rcpp::as< Texts >(texts_);
    
    Texts* texts_pt = new Texts(texts_.size());
    for (std::size_t i = 0; i < texts_.size(); i++) {
        texts_pt->push_back(Rcpp::as< Text >(texts_[i]));
    }
    Rcpp::XPtr< Texts > texts_pt_(texts_pt, true);
    return(texts_pt_);
}

// [[Rcpp::export]]
IntegerVector qatd_cpp_tokens_unlist(XPtr<Texts> texts_pt_)
{
    //XPtr<Texts> texts_pt(texts_pt_);
    Texts texts = *texts_pt_;
    Rcout << texts.size() << "\n";
    
    Text texts_flat;
    //tokens_flat.reserve(count_match);
    for (auto &text: texts) {
        texts_flat.insert(text.end(), text.begin(), text.end());
    }
    return texts_flat;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R


pt <- qatd_cpp_tokens(toks)
attr(pt, 'types') <- attr(toks, 'types')
str(pt)


*/
