#include <Rcpp.h>
#include "dev.h"
#include "quanteda.h"

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppParallel;
using namespace quanteda;
using namespace ngrams;



// [[Rcpp::export]]
List qatd_cpp_tokens_dummy(const List &texts_, 
                            int mode){
    
    Texts input = Rcpp::as<Texts>(texts_);
    Texts output = input;
    
    dev::Timer timer;
    dev::start_timer("Convert", timer);
    //ListOf<IntegerVector> texts_list;
    //if (mode == 1) {
    ListOf<IntegerVector> texts_list = Rcpp::wrap(output);
    //} else if (mode == 2) {
    //    texts_list = as_list(input);
    //}
    dev::stop_timer("Convert", timer);
    return texts_list;
    
}

/***R
#library(quanteda)
#load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
#toks <- tokens(data_corpus_guardian)

qatd_cpp_tokens_dummy(list(1:100, vector()), 2)

microbenchmark::microbenchmark(
    quanteda:::qatd_cpp_tokens_dummy(toks, 1),
    #qatd_cpp_tokens_dummy(toks, 2),
    times = 10
)




*/
