toks <- tokens(inaugCorpus)
load("/home/kohei/Documents/Brexit/Analysis/data_corpus_guardian.RData")
toks <- tokens(data_corpus_guardian)

attr_org <- attributes(toks)
toks2 <- lapply(unclass(toks), function(x) x[(x %% 2) == 0])
toks2 <- as.tokens(toks2)
attributes(toks2) <- attr_org

microbenchmark::microbenchmark(
    r=quanteda:::tokens_hashed_recompile(toks2),
    cpp=quanteda:::qatd_cpp_recompile(toks2, attr(toks2, "types")),
    times = 5
)

r <- quanteda:::tokens_hashed_recompile(toks2)
cpp <- quanteda:::qatd_cpp_recompile(toks, attr(toks2, "types"))
class(cpp) <- 'tokens'

#any(unlist(as.list(r)) != unlist(as.list(cpp)))
