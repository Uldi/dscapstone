# loadCorpusInMem <- function(dir) {
#     corpus(readtext(paste0(dir, "/*")))
# }

loadTMCorpusInMem <- function(dir) {
    textCorpus <- VCorpus(DirSource(dir),
                          readerControl = list(reader = readPlain,
                                               language = "en_US",
                                               load = TRUE))
    textCorpus
}


persistCorpus <- function(tokens, corpName) {
    saveRDS(tokens,  file=paste0("data/corpus/", corpName, ".rds"))
}

loadPersistedCorpus <- function(corpName) {
    readRDS(file=paste0("data/corpus/", corpName, ".rds"))
}


#a 3 document corpus
prepareCorpus <- function(dir="data/split/train", filterStopwords=TRUE) {
    flog.trace("prepareCorpus - laodData from dir=%s", dir)
    c <- loadTMCorpusInMem(dir)
    flog.trace("prepareCorpus - remove stopwords")
    c <- tm_map(c, FUN = content_transformer(tolower))
    c <- tm_map(c, FUN = removeWords, myStopwords(filterStopwords))
    cm <- VCorpus(VectorSource(c(c[[1]]$content, c[[2]]$content, c[[3]]$content)))
    corp <- corpus(cm)
    flog.trace("prepareCorpus - quanteda in action")
    corp <- corpus_reshape(corp, to = c("sentences"), use_docvars = FALSE)
    corp <- corpus(texts(corp, groups = rep(1, ndoc(corp))))
    tokens <- tokens(corp, what = "sentence")
    as.character(tokens)
}

#just a single doc corpus
prepareTestCorpus <- function(dir="data/test", filterStopwords=TRUE) {
    flog.trace("prepareTestCorpus on dir=%s", dir)
    c <- loadTMCorpusInMem(dir)
    c <- tm_map(c, FUN = content_transformer(tolower))
    c <- tm_map(c, FUN = removeWords, myStopwords(filterStopwords))
    cm <- VCorpus(VectorSource(c(c[[1]]$content)))
    corp <- corpus(cm)
    corp <- corpus_reshape(corp, to = c("sentences"), use_docvars = FALSE)
    corp <- corpus(texts(corp, groups = rep(1, ndoc(corp))))
    tokens <- tokens(corp, what = "sentence")
    as.character(tokens)
}

myStopwords <- function(filterStopwords=TRUE) {
    if (filterStopwords) 
        c(getProfanityWords(),stopwords("en"))
    else 
        getProfanityWords()
}

removeStopwords <- function(sentence, filterStopwords=TRUE) {
    tok <- tokens(sentence, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=FALSE, remove_twitter=TRUE, remove_url=TRUE)
    tok <- tokens_tolower(tok, keep_acronyms = TRUE)
    tok <- tokens_remove(tok, myStopwords(filterStopwords))
    as.character(tok)
}

getDocFeatureMatrix <- function(tokens, n) {
    dfm <- dfm(tokens, what="word", remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=FALSE, remove_twitter=TRUE, remove_url=TRUE, ngram=n, concatenator = " ")
}

getTermCountDF <- function(dfm, minFreq=10) {
    dfmt <- dfm_trim(dfm, min_termfreq = minFreq)
    tf <- topfeatures(dfmt, Inf)
    df <- data.frame(ngram=names(tf), count=tf, stringsAsFactors = FALSE, row.names = NULL)
    flog.trace(paste("getTermCountDF - count: ", length(tf)))
    df
}

# listMostFrequentTerms <- function(tdm, numTerms=10, minFreq=100, ngram="n") {
#     ft <- (tdm[findFreqTerms(tdm,minFreq),] %>%
#                as.matrix() %>%
#                rowSums() %>% sort(decreasing = TRUE)) [1:numTerms]
#     as.matrix(ft)
# } 
# 
# analyseFrequentTerms <- function(tdm, numTerms=10, minFreq=100, ngram="n") {
#     ft <- (tdm[findFreqTerms(tdm,minFreq),] %>%
#                 as.matrix() %>%
#                 rowSums() %>% sort(decreasing = TRUE)) [1:numTerms]
#     barplot(ft, las=2, cex.names = 1.0, main= paste("top", numTerms, ngram, "-grams", sep = " "))
# }
# 
# plotTopFrequentTerms <- function(tdm, frequency=100, ngram="n") {
#     ft <- (tdm[findFreqTerms(tdm,10),] %>%
#                as.matrix() %>%
#                rowSums() %>% sort(decreasing = TRUE)) [1:100]
#     plot(ft, main=paste("top", frequency, "frequent", ngram, "-grams"), ylab="frequency", xlab="terms with decreasing frequency")
# }

maxChars <- function(textDocument) {
    l <- lapply(textDocument$content, nchar)
    i <- which.max(l)
    l[i]
}


