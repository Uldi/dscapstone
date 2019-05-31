test <- function(sboTables, nTests=0) {
    test4grams <- readRDS(file="data/testing/test4grams.rds")
    successCount <-0
    if(nTests == 0) nTests <- nrow(test4grams)

    flog.info("testing %i samples", nTests)
    for(i in 1:nTests) {
        row <- test4grams[i,]
        ngram <- paste(as.character(row$ngram_1), as.character(row$nextWord))
        predicted4gram <- predictSBONLP(sboTables, as.character(row$ngram_1))
        if(predicted4gram == ngram) successCount <- successCount + 1
    }
    flog.info("test result: success count out of %i test: %i", i, successCount)
}

createTestSamples <- function(sboTables, nTests=1000) {
    samples <- sample_n(sboTables[[4]], nTests)
    saveRDS(samples, file="data/testing/test4grams.rds")
}