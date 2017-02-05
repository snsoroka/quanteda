#include <Rcpp.h>
//#include "dev.h"
#include "quanteda.h"

// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using namespace RcppParallel;
using namespace quanteda;
using namespace ngrams;

#if RCPP_PARALLEL_USE_TBB
typedef tbb::concurrent_unordered_map<unsigned int, unsigned int> MapIds;
typedef tbb::concurrent_unordered_set<std::string> SetTypes;
#else
typedef std::unordered_map<unsigned int, unsigned int> MapIds;
typedef std::unordered_set<std::string> SetTypes;
#endif

struct compile_mt : public Worker{
    
    Texts &texts;
    MapIds &ids_map;
    
    compile_mt(Texts &texts_, MapIds &ids_map_):
               texts(texts_), ids_map(ids_map_) {}
    
    void operator()(std::size_t begin, std::size_t end){
        for (std::size_t h = begin; h < end; h++) {
            for (std::size_t i = 0; i < texts[h].size(); i++) {
                texts[h][i] = ids_map[texts[h][i]];
            }
        }
    }
};


/* 
* This funciton recompiles tokens object.
* @used tokens_lookup()
* @creator Kohei Watanabe
* @param texts_ tokens ojbect
* @param types_ types in tokens
*/


// [[Rcpp::export]]
List qatd_cpp_recompile(const List &texts_, 
                        const CharacterVector types_){
    
    Texts texts = Rcpp::as<Texts>(texts_);
    Types types = Rcpp::as< Types >(types_);

    // Get unique IDs
    MapIds ids_map;
    ids_map[0] = 0; // Reserve zero for padding
    bool padding = false; // Check use of padding
    unsigned int id_last = 0;
    for (std::size_t h = 0; h < texts.size(); h++) {
        for (std::size_t i = 0; i < texts[h].size(); i++) {
            unsigned int id = texts[h][i];
            if (id == 0) {
                padding = true;
                continue;
            }
            unsigned int &id_new = ids_map[id];
            if (!id_new) {
                id_new = ++id_last;
            }
        }
    }

    // Get unique types
    SetTypes types_set;
    for (std::size_t j = 0; j < types.size(); j++) {
        if (types[j] == "") continue;
        types_set.insert(types[j]);
    }
    
    // Check gaps and duplicates
    if (ids_map.size() == types_set.size() && types.size() == types_set.size()) return texts_;

    // Convert old IDs to new IDs
    #if RCPP_PARALLEL_USE_TBB
    compile_mt compile_mt(texts, ids_map);
    parallelFor(0, texts.size(), compile_mt);
    #else
    for (std::size_t h = 0; h < texts.size(); h++) {
        for (std::size_t i = 0; i < texts[h].size(); i++) {
            texts[h][i] = ids_map[texts[h][i]];
        }
    }
    #endif
    std::vector<std::string> types_new(ids_map.size() - 1);
    for (const auto& it : ids_map) {
         if (it.first == 0 || it.second == 0) continue; // padding is not in types
         types_new[it.second - 1] = types[it.first - 1];
    }
            
    // dev::stop_timer("Dictionary lookup", timer);
    ListOf<IntegerVector> texts_list = Rcpp::wrap(texts);
    texts_list.attr("padding") = padding;
    texts_list.attr("types") = types_new;
    return texts_list;
}

/***R

toks <- list(rep(0:10, 1), rep(5:15, 1))
qatd_cpp_recompile(toks, letters)



*/
