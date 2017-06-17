#include "quanteda.h"
#include "dev.h"
using namespace quanteda;


#if QUANTEDA_USE_TBB
typedef tbb::concurrent_unordered_map<unsigned int, unsigned int> MapUnigrams;
typedef tbb::concurrent_unordered_map<Ngram, MapUnigrams, hash_ngram, equal_ngram> TreeNgrams;
typedef tbb::concurrent_unordered_map<Ngram, double, hash_ngram, equal_ngram> DoubleNgrams;
#else
typedef std::unordered_map<unsigned int, unsigned int> MapUnigrams;
typedef std::unordered_map<Ngram, MapUnigrams, hash_ngram, equal_ngram> TreeNgrams;
typedef std::unordered_map<Ngram, double, hash_ngram, equal_ngram> DoubleNgrams;
#endif 

double lambda(Ngram &ngram,
              unsigned int &count_ngram,
              IntParam &count_unigram,
              MapUnigrams &counts_unigram,
              TreeNgrams &counts_trail){

    if (ngram.size() < 2) return 0;
    Ngram ngram_sub(ngram.begin(), ngram.end() - 1);
    unsigned int unigram_sub = ngram.back();
    double l_trail = (double)counts_trail[ngram_sub][unigram_sub] / (double)count_ngram;
    double l_all = (double)counts_unigram[unigram_sub] / (double)count_unigram;
    //Rcout << "ngram ";
    //dev::print_ngram(ngram);
    //Rcout << "ngram_sub ";
    //dev::print_ngram(ngram_sub);
    //Rcout << "uningram_sub " << unigram_sub << "\n";
    //Rcout << "trail " << l_trail << " all " << l_all << "\n";
    return log(l_trail / l_all);
}

void estimate(std::size_t i,
              VecNgrams &ngrams,
              IntParam &count_unigram,
              MapUnigrams &counts_unigram,
              MapNgrams &counts_ngram,
              TreeNgrams &counts_trail,
              DoubleNgrams &lambdas_cache,
              DoubleParams &ls,
              IntParams &cs,
              IntParams &ns,
              const unsigned int count_min
              ){
    
    if (cs[i] < count_min) {
        //Rcout << "Skip " << i << "\n";
        ls[i] = 0;
        return;
    }
    // Rcout << "Estimate " << i << "\n";
    Ngram ngram = ngrams[i];
    unsigned int count_ngram = counts_ngram[ngram];
    for (unsigned int j = 2; j <= ngram.size(); j++){
        Ngram ngram_sub(ngram.begin(), ngram.begin() + j);
        
        double &lambda_temp = lambdas_cache[ngram_sub];
        if (!lambda_temp){
            lambda_temp = lambda(ngram_sub, count_ngram, count_unigram, counts_unigram, counts_trail);
        }
        //Rcout << "lambda_temp " << lambda_temp << "\n";
        if (lambda_temp < 1.0) {
            ls[i] = 0;
            return;
        }
        ls[i] *= lambda_temp;
    }
}

void counts(Text text, 
            IntParam &count_unigram,
             MapUnigrams &counts_unigram,
             MapNgrams &counts_ngram,
             TreeNgrams &counts_trail,
             const std::vector<unsigned int> &sizes){
    
    if (text.size() == 0) return; // do nothing with empty text
    text.push_back(0); // add padding to include last words
    
    std::size_t len_text = text.size();
    for (std::size_t i = 0; i <= len_text; i++) {
        if (text[i] > 0) {
            counts_unigram[text[i]]++;
            count_unigram++;
        }
        for (std::size_t size : sizes) {
            //Rcout << "Size" << size << "\n";
            if (size > 1 &&  i + size < len_text) {
                if (std::find(text.begin() + i, text.begin() + i + size, 0) == text.begin() + i + size) {
                    Text ngram(text.begin() + i, text.begin() + i + size);
                    Text ngram_sub(text.begin() + i, text.begin() + i + size - 1);
                    
                    //dev::print_ngram(ngram);
                    //Rcout << "back " << ngram.back() << "\n";
                    counts_ngram[ngram]++;
                    counts_trail[ngram_sub][ngram.back()]++;
                    //Rcout << "counts_trail " << counts_trail[ngram_sub][ngram.back()] << "\n";;
                }
            }
        }
    }
}

// [[Rcpp::export]]
DataFrame qatd_cpp_sequences_tree(const List &texts_,
                                  const CharacterVector &types_,
                                  const unsigned int count_min,
                                  const IntegerVector sizes_){
    
    Texts texts = as< Texts >(texts_);
    std::vector<unsigned int> sizes = as< std::vector<unsigned int> >(sizes_);
    IntParam count_unigram(0);
    MapUnigrams counts_unigram;
    MapNgrams counts_ngram;
    TreeNgrams counts_trail;
    
    for (std::size_t h = 0; h < texts.size(); h++) {
        counts(texts[h], count_unigram, counts_unigram, counts_ngram, counts_trail, sizes);
    }
    
    std::size_t len = counts_ngram.size();
    VecNgrams ngrams;
    DoubleParams ls;
    IntParams cs, ns;
    ngrams.reserve(len);
    ls.reserve(len);
    cs.reserve(len);
    ns.reserve(len);
    
    for (auto it = counts_ngram.begin(); it != counts_ngram.end(); ++it) {
        ngrams.push_back(it->first);
        ls.push_back(1.0);
        cs.push_back(it->second);
        ns.push_back(it->first.size());
    }
    
    DoubleNgrams lambdas_cache;
    for (size_t i = 0; i < ngrams.size(); i ++) {
        estimate(i, ngrams, count_unigram, counts_unigram, counts_ngram, counts_trail, lambdas_cache, 
                 ls, cs, ns, count_min);
        R_CheckUserInterrupt();
    }

    CharacterVector ngrams_(ngrams.size());
    for (std::size_t i = 0; i < ngrams.size(); i++) {
        ngrams_[i] = join(ngrams[i], types_, " ");
    }
    
    DataFrame output_ = DataFrame::create(_["collocation"] = ngrams_,
                                          _["count"] = as<IntegerVector>(wrap(cs)),
                                          _["length"] = as<IntegerVector>(wrap(ns)),
                                          _["lambda"] = as<NumericVector>(wrap(ls)),
                                          _["stringsAsFactors"] = false);
    return output_;
}

/***R
# 
toks <- tokens(data_corpus_inaugural)
toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
toks <- tokens_select(toks, '[\\p{P}]', valuetype = 'regex', "remove", padding = TRUE)
types<- attr(toks, 'types')
out <- qatd_cpp_sequences_tree(toks, types, 2, 1:10)
out <- out[order(out$lambda),]
tail(out, 30)

# qatd_cpp_sequences_tree(toks, types, 2, 4)

# toks2 <- tokens('capital other capital gains other capital word2 other gains capital')
# types2 <- attr(toks2, 'types')
# 
# out <- qatd_cpp_sequences_tree(toks2, types2, 1, 2:4)
# out[order(out$lambda),]


load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
corp <- corpus_subset(data_corpus_guardian, format(docvars(data_corpus_guardian, 'date'), '%Y') == '2016')
toks <- tokens(corp)
toks <- tokens_select(toks, stopwords("english"), "remove", padding = TRUE)
toks <- tokens_select(toks, '^[A-Z][A-Za-z]', valuetype = 'regex', padding = TRUE, case_insensitive = FALSE)
toks <- tokens_select(toks, '[\\p{P}]', valuetype = 'regex', "remove", padding = TRUE)
types<- attr(toks, 'types')
out <- qatd_cpp_sequences_tree(toks, types, 50, 1:10)
out <- out[order(out$lambda),]
tail(out, 50)

*/
