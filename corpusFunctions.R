loadCorpusInMem <- function(dir) {
    corpus(readtext(paste0(dir, "/*")))
}

loadTMCorpusInMem <- function(dir) {
    textCorpus <- VCorpus(DirSource(dir),
                          readerControl = list(reader = readPlain,
                                               language = "en_US",
                                               load = TRUE))
    textCorpus
}

loadTestCorpus <- function() {
    loadTMCorpusInMem("data/test")
}

loadSampleCorpus <- function() {
    loadTMCorpusInMem("data/samples")
}

loadFullCorpus <- function() {
    loadTMCorpusInMem("data/final/en_US")
}

persistCorpus <- function(tokens, corpName) {
    saveRDS(tokens,  file=paste0("data/corpus/", corpName, ".rds"))
}

loadPersistedCorpus <- function(corpName) {
    readRDS(file=paste0("data/corpus/", corpName, ".rds"))
}


maxChars <- function(textDocument) {
    l <- lapply(textDocument$content, nchar)
    i <- which.max(l)
    l[i]
}

prepareSampleCorpus <- function() {
    flog.trace("prepareSampleCorpus")
    primPrepareCorpus(loadSampleCorpus)
}

prepareFullCorpus <- function() {
    flog.trace("prepareFullCorpus")
    primPrepareCorpus(loadFullCorpus)
}

prepareTestCorpus <- function() {
    flog.trace("prepareTestCorpus")
    primPrepareTestCorpus()
}

primPrepareCorpus <- function(dataLoadFunction) {
    flog.trace("primPrepareCorpus - laodData")
    c <- dataLoadFunction()
    flog.trace("primPrepareCorpus - remove stopwords")
    c <- tm_map(c, FUN = removeWords, myStopwords())
    cm <- VCorpus(VectorSource(c(c[[1]]$content, c[[2]]$content, c[[3]]$content)))
    corp <- corpus(cm)
    #corp <- dataLoadFunction()
    flog.trace("primPrepareCorpus - quanteda in action")
    corp <- corpus_reshape(corp, to = c("sentences"), use_docvars = FALSE)
    corp <- corpus(texts(corp, groups = rep(1, ndoc(corp))))
    tokens <- tokens(corp, what = "sentence")
    as.character(tokens)
}

primPrepareTestCorpus <- function() {
    
    c <- loadTestCorpus()
    c <- tm_map(c, FUN = removeWords, myStopwords())
    cm <- VCorpus(VectorSource(c(c[[1]]$content)))
    corp <- corpus(cm)
    #corp <- dataLoadFunction()
    corp <- corpus_reshape(corp, to = c("sentences"), use_docvars = FALSE)
    corp <- corpus(texts(corp, groups = rep(1, ndoc(corp))))
    tokens <- tokens(corp, what = "sentence")
    as.character(tokens)
}

# primPrepareCorpus <- function(dataLoadFunction) {
#     corp <- dataLoadFunction()
#     corp <- corpus_reshape(corp, to = c("sentences"), use_docvars = FALSE)
#     corp <- corpus(texts(corp, groups = rep(1, ndoc(corp))))
# 
#     tokens <- tokens(corp, what = "sentence")
#     
#     #das bringt hier nichts, da ich noch Sätze habe!
#     # tokens <- tokens_remove(tokens, stopwords("en"))
#     # tokens <- tokens_remove(tokens, getProfanityWords())
#     as.character(tokens)
# }

# primPrepareCorpus2 <- function(dataLoadFunction) {
#     corp <- dataLoadFunction()
#     corp <- corpus_reshape(corp, to = c("sentences"), use_docvars = FALSE)
#     corp <- corpus(texts(corp, groups = rep(1, ndoc(corp))))
#     
#     tokens <- tokens(corp, what = "sentence")
#     
#     tokens <- tokens(tokens, remove_numbers=TRUE, remove_symbols=TRUE, remove_hyphens=FALSE, what=c("fasterword"))
#     tokens <- tokens_remove(tokens, myStopwords())
#     
#     
#     
#     #das bringt hier nichts, da ich noch Sätze habe!
#     # tokens <- tokens_remove(tokens, stopwords("en"))
#     # tokens <- tokens_remove(tokens, getProfanityWords())
#     as.character(tokens)
# }


# primPrepareCorpus <- function(dataLoadFunction) {
#     corp <- dataLoadFunction()
#     corp <- corpus_reshape(corp, to = c("sentences"), use_docvars = FALSE)
#     corp <- corpus(texts(corp, groups = rep(1, ndoc(corp))))
#     tok <- tokens(corp, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=FALSE, remove_twitter=TRUE, what=c("fasterword"))
#     tok <- tokens_tolower(tok, keep_acronyms = TRUE)
#     tok <- tokens_remove(tok, getProfanityWords())
#     #tok <- tokens_remove(tok, pattern="^[^a-zA-Z]|[^a-zA-Z]$", valuetype="regex", padding=TRUE)
#     tok
# }

myStopwords <- function() {
    # c(getProfanityWords(),stopwords("en"))
    getProfanityWords()
}

removeStopwords <- function(sentence) {
    tok <- tokens(sentence, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=FALSE, remove_twitter=TRUE)
    tok <- tokens_tolower(tok, keep_acronyms = TRUE)
    tok <- tokens_remove(tok, myStopwords())
    as.character(tok)
}

# getDocFeatureMatrix <- function(tokens, n) {
#     tokens %>%
#         tokens(remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=FALSE, remove_twitter=TRUE) %>%
#         tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
#         tokens_remove(myStopwords(), padding  = TRUE) %>%
#         tokens_ngrams(n = n, concatenator = " ") %>%
#         dfm()
# }

getDocFeatureMatrix <- function(tokens, n) {
    dfm <- dfm(tokens, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=FALSE, remove_twitter=TRUE, ngram=n, concatenator = " ")
    
    # feat <- featnames(dfm)
    # feat_split <- stringi::stri_split_fixed(feat, " ")
    # feat_stop <- feat[sapply(feat_split, function(x) any(x %in% myStopwords()))]
    # 
    # dfmClean <- dfm_remove(dfm, feat_stop)
    # dfmClean 

}

# getDocFeatureMatrix <- function(tokens, ngram) {
#     dfm(tokens_ngrams(tokens, ngram, concatenator = " "))
#     
# }

getTermCountDF <- function(dfm, minFreq=10) {
    dfmt <- dfm_trim(dfm, min_termfreq = minFreq)
    tf <- topfeatures(dfmt, Inf)
    df <- data.frame(ngram=names(tf), count=tf, stringsAsFactors = FALSE, row.names = NULL)
    flog.trace(paste("getTermCountDF - count: ", length(tf)))
    df
}

# getTermCountDF <- function(dfm, minFreq=10) {
#     dfmt <- dfm_trim(dfm, min_termfreq = minFreq)
#     dfmt <- dfm_sort(dfmt, margin=c("features"))
#     tripList <- convert(dfmt, to = "tripletlist")
#     df <- data.frame(ngram=tripList$feature, count=tripList$frequency, stringsAsFactors = FALSE)
#     df
# }

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



