#include "quanteda.h"
#include "dev.h"
#include "recompile.h"
#include <algorithm>
using namespace quanteda;


// [[Rcpp::export]]
XPtr<Texts> qatd_cpp_xpointer(List &texts_) {
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Texts* texts_pt = new Texts;
    for (std::size_t i = 0; i < texts_.size(); i++) {
        Text text = texts[i];
        (*texts_pt).push_back(text);
    }
    
    // Texts* texts_pt = new Texts(texts_.size());
    // for (std::size_t i = 0; i < texts_.size(); i++) {
    //     Text text = texts[i];
    //     if (text.size() > 0) {
    //     (*texts_pt)[i] = text;
    //     }
    // }
    Rcpp::XPtr<Texts> texts_pt_(texts_pt, true);
    return(texts_pt_);
}

// [[Rcpp::export]]
List qatd_cpp_tokens(XPtr<Texts> texts_pt_, CharacterVector types_) {
    
    Texts texts = *texts_pt_;
    Types types = Rcpp::as<Types>(types_);
    return(recompile(texts, types));
}

// [[Rcpp::export]]
IntegerVector qatd_cpp_xpointer_unlist(XPtr<Texts> texts_pt_) {
    //XPtr<Texts> texts_pt(texts_pt_);
    Texts texts = *texts_pt_;
    //Rcout << texts.size() << "\n";
    
    std::vector<int> texts_flat;
    std::vector<int> pos_start;
    // //tokens_flat.reserve(count_match);
    //for (auto text: texts) {
    int pos = 1;
    for (std::size_t i = 0; i < texts.size(); i++) {
        //texts_flat.insert(texts_flat.end(), text.begin(), text.end());
        Text text = texts[i];
        texts_flat.insert(texts_flat.end(), text.begin(), text.end());
        pos_start.push_back(pos + (int)text.size());
    }
    //Rcout << texts_flat.size() << "\n";
    IntegerVector texts_flat_ = Rcpp::wrap(texts_flat);
    IntegerVector pos_start_ = Rcpp::wrap(pos_start);
    texts_flat_.attr("position") = pos_start_;
    return texts_flat_;
}

// [[Rcpp::export]]
XPtr<Texts> qatd_cpp_xpointer_subset(XPtr<Texts> texts_pt_, IntegerVector index_) {
    Texts texts = *texts_pt_;
    std::vector<int> index = Rcpp::as< std::vector<int> >(index_);
    if (*std::max_element(index.begin(), index.end()) - 1 > (int)texts.size()) {
        throw std::range_error("Invalid index");
    }
    Texts* texts_sub_pt = new Texts(index.size());
    for (int i : index) {
        (*texts_sub_pt)[i - 1] = texts[i - 1];
    }
    Rcpp::XPtr< Texts > texts_sub_pt_(texts_sub_pt);
    return(texts_sub_pt_);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
require(quanteda)
#load("/home/kohei/Documents/Brexit/Analysis/data_tokens_guardian.RData")
# toks <- tokens(rep(c("You can include R code blocks in C++ files processed with sourceCpp",
#                  "The R code will be automatically run after the compilation."), 3))
toks <- tokens(data_corpus_irishbudget2010)
#toks <- data_tokens_guardian
pt <- qatd_cpp_xpointer(toks)
#attr(pt, 'types') <- attr(toks, 'types')

microbenchmark::microbenchmark(
    qatd_cpp_xpointer(toks)
)

str(pt)
microbenchmark::microbenchmark(
    qatd_cpp_xpointer_unlist(pt),
    unlist(toks, use.names = FALSE)
)

pt_sub <- qatd_cpp_xpointer_subset(pt, 1:10)
qatd_cpp_tokens(pt_sub, attr(toks, 'types'))

*/
