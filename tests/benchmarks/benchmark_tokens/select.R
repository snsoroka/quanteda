library(quanteda)

load("/home/kohei/Documents/Brexit/Analysis/data_tokens_guardian.RData")
toks <- data_tokens_guardian

#toks <- tokens(corpus_subset(data_corpus_guardian, year == 2012))
toks[[1]]

toks2 <- tokens_select(toks, stopwords(), valuetype='fixed')
toks2[[1]]

toks3 <- tokens_select(toks, stopwords(), valuetype='fixed', padding = TRUE)
toks3[[1]]

system.time(tokens_select(toks, '*'))
system.time(tokens_select(toks, list(c('President', '*'))))
system.time(tokens_remove(toks, list(c('President', '*'))))

microbenchmark::microbenchmark(
    dfm(tokens_remove(toks, stopwords(), valuetype='fixed')),
    dfm_remove(dfm(toks), stopwords(), valuetype='fixed'),
    times=1
)

profvis::profvis(tokens_select(toks, stopwords(), valuetype='fixed'))
profvis::profvis(tokens_select(toks, stopwords(), valuetype='glob'))

# XPointer -----------------------------------------------

toks <- data_tokens_guardian
xpt <- as.xtokens(toks)

dict <- 1:100
toks2 <- quanteda:::qatd_cpp_tokens_select(toks, attr(toks, 'types'), dict, 1, TRUE)
xpt2 <- quanteda:::qatd_cpp_xpointer_select(xpt, attr(toks, 'types'), dict, 1, TRUE)
toks3 <- quanteda:::qatd_cpp_tokens(xpt2, attr(toks, 'types'))
identical(toks2, toks3)

microbenchmark::microbenchmark(
    quanteda:::qatd_cpp_xpointer_select(xpt, attr(toks, 'types'), dict, 1, TRUE),
    times = 10
)

microbenchmark::microbenchmark(
    quanteda:::qatd_cpp_tokens_select(toks, attr(toks, 'types'), dict, 1, TRUE),
    quanteda:::qatd_cpp_xpointer_select(xpt, attr(toks, 'types'), dict, 1, TRUE),
    times = 10
)


