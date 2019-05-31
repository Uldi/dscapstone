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

testQuiz2 <- function(sboTables){
    sentence <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", "You’re the reason why I smile everyday. Can you follow me please? It would mean the", "Hey sunshine, can you follow me and make me the", "Very early observations on the Bills game: Offence still struggling but the", "Go on a romantic date at the", "Well I’m pretty sure my granny has some old bagpipes in her garage I’ll dust them on and be on my", "Ohhhhh #PointBreak is on tomorrow. Love that film and haven’t seen it in quite some", "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", "Be grateful for the good times and keep the faith during the", "If this isn’t the cutest thing you’ve ever seen, then you must be")
    choices <- c(c("prezels", "soda", "beer", "cheese"), c("world", "best", "most", "universe"), c("bluest", "smelliest", "saddest", "happiest"), c("crowd", "defense", "referees", "players"), c("mall", "grocery", "movies", "beach"), c("way", "horse", "motorcycle", "phone"), c("thing", "weeks", "time", "years"), c("fingers", "eyes", "ears", "toes"), c("worse", "bad", "hard", "sad"), c("asleep", "insensitive", "callous", "insane"))
    solution <- c("beer", "world", "happiest", "defense", "beach", "way", "time", "fingers", "bad", "insane")
    
    sucCount <- 0
    for(i in 1:length(sentences)) {
        if (primQuizTest(sboTables, sentence[i], choices[i], solution[i])) sucCount <- sucCount + 1
    }
    flog.info("Quiz 2: success count out of %i tests: %i", i, sucCount)
}

primQuizTest <- function(sboTables, sentence, choices, solution) {
    predictedWord <- primPredictSBONLP(sboTables, sentence)
    flog.info("Quiz Test - sentence %s", sentence)
    success <- predictedWord == solution
    if (success) {
        flog.info("*** Quiz Test - direct prediction successful - prediction: %s, solution: %s", predictedWord, solution)
    } else {
        flog.info("*** Quiz Test - direct prediction not successful - prediction: %s, solution: %s", predictedWord, solution)
    
        #indirect 4gram test
        words <- removeStopwords(sentence)
        numWords <- length(words)
        
        ngram3 <- paste(words[(numWords-2): numWords], collapse = " ")
        flog.info("Quiz Test - doing indirect 4gram prediction with: %s", ngram3)
        r <- lookupNGram(sboTables[[4]], ngram3)
        rows <- r %>% filter(nextWord %in% choices) %>% arrange(desc(mle))
        print(r)
        print(rows)
        if(nrow(rows) > 0) {
            predictedWord <- rows[1,2]
            success <- predictedWord == solution
            if (success) {
                flog.info("*** Quiz Test - indirect 4gram prediction successful - prediction: %s, solution: %s", predictedWord, solution)
            } else {
                flog.info("*** Quiz Test - indirect 4gram prediction not successful - prediction: %s, solution: %s", predictedWord, solution)
            }
        }
        if (!success) {
            ngram2 <- paste(words[(numWords-1): numWords], collapse = " ")
            flog.info("Quiz Test - doing indirect 3gram prediction with: %s", ngram2)
            r <- lookupNGram(sboTables[[3]], ngram2)
            rows <- r %>% filter(nextWord %in% choices) %>% arrange(desc(mle))
            print(r)
            print(rows)
            
            if(nrow(rows) > 0) {
                predictedWord <- rows[1,2]
                success <- predictedWord == solution
                if (success) {
                    flog.info("*** Quiz Test - indirect 3gram prediction successful - prediction: %s, solution: %s", predictedWord, solution)
                } else {
                    flog.info("*** Quiz Test - indirect 3gram prediction not successful - prediction: %s, solution: %s", predictedWord, solution)
                }
            }
        }
    }
    success
    
}

createTestSamples <- function(sboTables, nTests=1000) {
    samples <- sample_n(sboTables[[4]], nTests)
    saveRDS(samples, file="data/testing/test4grams.rds")
}