toks <- tokens(inaugCorpus)
load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
toks <- tokens(data_corpus_guardian)

attr_org <- attributes(toks)
toks <- lapply(unclass(toks), function(x) x[(x %% 2) == 0])
toks <- as.tokens(toks)
attributes(toks) <- attr_org

microbenchmark::microbenchmark(
    r=quanteda:::tokens_hashed_recompile(toks),
    cpp=quanteda:::qatd_cpp_recompile(toks, attr(toks, "types")),
    times = 1
)

r <- quanteda:::tokens_hashed_recompile(toks)
cpp <- quanteda:::qatd_cpp_recompile(toks, attr(toks, "types"))
class(cpp) <- 'tokens'

any(unlist(as.list(r)) != unlist(as.list(cpp)))
