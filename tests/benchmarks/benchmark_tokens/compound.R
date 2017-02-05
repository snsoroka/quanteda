toks <- tokens(inaugCorpus)
load("/home/kohei/Documents/Brexit/Analysis/data_tokens_guardian.RData")
toks <- data_tokens_guardian

seqs <- list(c('united', 'states'))
microbenchmark::microbenchmark(
    tokens_compound(toks, seqs, valuetype='fixed'),
    times=1
)

seqs_not <- list(c('not', '*'))
microbenchmark::microbenchmark(
    tokens_compound(toks, seqs_not, valuetype='glob'),
    times=1
)

seqs_will <- list(c('will', '*'))
microbenchmark::microbenchmark(
    tokens_compound(toks, seqs_will, valuetype='glob'),
    times=1
)

dict_lex <- dictionary(file='/home/kohei/Documents/Dictionary/Lexicoder/LSDaug2015/LSD2015_NEG.lc3')
seq_lex <- quanteda:::sequence2list(unlist(dict_lex, use.names = FALSE))
length(seq_lex)

out <- tokens_compound(toks, seq_lex, valuetype='glob', join=TRUE)
out <- tokens_compound(toks, c('not *'), valuetype='glob', join=TRUE)

profvis::profvis(tokens_compound(tokens(inaugCorpus), seq_lex, valuetype='glob', join=TRUE))
profvis::profvis(tokens_compound(toks, seq_lex, valuetype='glob', join=FALSE))
profvis::profvis(tokens_compound(toks, seq_lex, valuetype='glob', join=TRUE))

            