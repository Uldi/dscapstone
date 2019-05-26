initLibraries <- function() {
    library(tm)
    library(dplyr)
    library(tokenizers)
    library(quanteda)
    library(readtext)
    library(futile.logger)
    flog.threshold(TRACE)
}

setupTestNLP <- function() {
    initLibraries()
    tokens <- prepareTestCorpus()
    persistCorpus(tokens, "test1")
#    Rprof("rprof.out", append=FALSE)
    primSetupNLP(tokens)
#    Rprof(NULL)
}

setupSampleNLP <- function() {
    initLibraries()
    flog.info("setupSampleNLP: start")
    tokens <- prepareSampleCorpus()
    flog.trace("setupSampleNLP: corpus prepared")
    Rprof("rprof.out", append=FALSE)
    sbo <- primSetupNLP(tokens, 5)
    Rprof(NULL)
    flog.info("setupSampleNLP: end")
    sbo
}

setupFullNLP <- function() {
    initLibraries()
    flog.info("setupFullNLP: start")
    tokens <- prepareFullCorpus()
    persistCorpus(tokens, "full10")
    flog.trace("setupFullNLP: corpus prepared")
    sbo <- primSetupNLP(tokens, 10)
    flog.info("setupFullNLP: end")
    sbo
}

primSetupNLP <- function(tokens, minFreq=1) {
    #bei getTermDocMatrix lohnt sich die Parallelisierung nicht, da immer auf cm zugegriffen wird!
    flog.trace("primSetupNLP: getDocFeatureMatrix 1")
    dfm1 <- getDocFeatureMatrix(tokens, 1)
    flog.trace("primSetupNLP: getDocFeatureMatrix 2")
    dfm2 <- getDocFeatureMatrix(tokens, 2)
    flog.trace("primSetupNLP: getDocFeatureMatrix 3")
    dfm3 <- getDocFeatureMatrix(tokens, 3)
    flog.trace("primSetupNLP: getDocFeatureMatrix 4")
    dfm4 <- getDocFeatureMatrix(tokens, 4)
    
    flog.trace("primSetupNLP: getSBOTables")
    getSBOTables(dfm1, dfm2, dfm3, dfm4, minFreq)
}


persistSBOModel <- function(sboTables, modelName="model") {
    write.csv(sboTables[[4]], file=paste0("data/model/", modelName, "SBO4.csv"), row.names = FALSE)
    write.csv(sboTables[[3]], file=paste0("data/model/", modelName, "SBO3.csv"), row.names = FALSE)
    write.csv(sboTables[[2]], file=paste0("data/model/", modelName, "SBO2.csv"), row.names = FALSE)
    write.csv(sboTables[[1]], file=paste0("data/model/", modelName, "SBO1.csv"), row.names = FALSE)
}

loadSBOModel <- function(modelName="model") {
    library(tokenizers)
    sbo4 <- read.csv(file=paste0("data/model/", modelName, "SBO4.csv"))
    sbo3 <- read.csv(file=paste0("data/model/", modelName, "SBO3.csv"))
    sbo2 <- read.csv(file=paste0("data/model/", modelName, "SBO2.csv"))
    sbo1 <- read.csv(file=paste0("data/model/", modelName, "SBO1.csv"))
    list(sbo1, sbo2, sbo3, sbo4)
}


# source: https://gigadom.in/tag/katz-backoff/
# source: https://en.wikipedia.org/wiki/Katz%27s_back-off_model
# https://rpubs.com/mszczepaniak/predictkbo3model


# offene Frage: berechne ich die p für alle möglichen Kombinationen im Voraus?
# sprich ich wende für p(c|ab) alle terms aus 1-ngrams an? ansonsten macht das
# smoothing ja nicht soviel sinn...
# Katz-backoff probabilities P(ngram) = 
#    d * mle für non zero count ngrams 
#    a * p (ngram_1) für zero count ngrams
#
#  d via Good-Turing  d = C* / C, wobei C* estimate für C durch Good-Turing
#
#  a 
#
# ToDo's
# for a ngram eine lookup table mit ngram_1 und smoothed mle für ngram

######
# SBO: Stupid Backoff Modell: https://www.aclweb.org/anthology/D07-1090.pdf
#
# a = 0.4 (heuristik)
# k = 0
# P(C|AB) = count(ABC)/count(AB) (wenn count(ABC)  > k), a * P(B|A) sonst
# P(B|A) = count(AB)/count(A) wenn count(AB) > 0 und sonst a * P(A)
# P(A) = count(A)/V (Anzahl unigrams)
#
# algorithmus bei einem alle ngrams, die mit ngram_1 beginnen P berechnen und 
# dasjenige Wort vorschlagen mit grösstem P
#
# dazu benötige ich für ein ngram eine tabelle mit ngram_1, ngram Wort (ohne ngram_1), berechnetes P
######

#predict using SBO approach
predictSBONLP <- function(ngramSBOTables, text) {
    
    #ToDo
    #idee wäre hier mit dem tokenizer package den Input zu zerstückeln...
    words <- c()
    if (nchar(trimws(text)) > 0) {
        inputSubSentences <- unlist(text %>% tokenize_sentences(simplify = TRUE) %>% tokenize_regex(", "))
        subSentence <- last(inputSubSentences)
        #ToDo: Achtung ein neuer Satz muss mit Grossbuchstaben oder Zahl beginnen!
        print(subSentence)
        words <- subSentence %>% tokenize_words(simplify = TRUE)
    }
    
    #Algorithmus:
    #0-gram --> 1-grams mit höchster P (mle)
    #1-gram --> 2-grams mit hächster P (mle) für 1-gram
    #2-gram --> 3-grams mit hächster P (mle) für 2-gram
    #3-gram --> 4-grams mit hächster P (mle) für 3-gram
    
    #words <- unlist(strsplit(subSentence, " "))
    
    ngramWord <- words[length(words)]
    ngram_1 <- paste(words[-length(words)], collapse = " ")
    nextWord <- NA
    numWords <- length(words)
    
    # a = 0.4 (heuristik), k = 0
    # P(C|AB) = count(ABC)/count(AB) (wenn count(ABC)  > k), a * P(B|A) sonst
    # P(B|A) = count(AB)/count(A) wenn count(AB) > 0 und sonst a * P(A)
    # P(A) = count(A)/V (Anzahl unigrams)
    #Algorithmus bei 4-grams beginnen und dann runter iterieren
    #4-gram bedingt ein geschriebenes 3-gram
    #Assumption: k = 0!
    if (numWords >= 3) {
        last3Words <- paste(words[(numWords-2): numWords], collapse = " ")
        printMsg("4-gram", last3Words)
        
        rows <- lookupNGram(ngramSBOTables[[4]], last3Words)
        if (length(rows) > 0) {
            nextWord <- rows[1,2]
            printMsg("4-gram found", nextWord)
        }
    }
    if (is.na(nextWord) && (numWords >= 2)) {
        last2Words <- paste(words[(numWords-1): numWords], collapse = " ")
        printMsg("3-gram", last2Words)
        
        rows <- lookupNGram(ngramSBOTables[[3]], last2Words)
        if(length(rows) > 0) {
            nextWord <- rows[1,2]
            printMsg("3-gram found", nextWord)
        }
    }
    if (is.na(nextWord) && (numWords >= 1)) {
        lastWord <- words[numWords]
        printMsg("2-gram", lastWord)
        
        rows <- lookupNGram(ngramSBOTables[[2]], lastWord)
        if(length(rows) > 0) {
            nextWord <- rows[1,2]
            printMsg("2-gram found", nextWord)
        } 
    } 
    if (is.na(nextWord)) {
        print("1-gram")
        rows <- ngramSBOTables[[1]]
        nextWord <- rows[1,1]
        printMsg("1-gram found", nextWord)
    }
    
    #return orginial text appended with next word
    trimws(paste(trimws(text), nextWord,sep = " "))
}

lookupNGram <- function(sboTable, ngram) {
    rows <- sboTable %>% filter(ngram_1 == ngram)
    #TODO: hier könnte ich evtl. nur die erste row zurückgeben...
    rows
}


getSBOTables <-  function(dfm1, dfm2, dfm3, dfm4, minFreq=5) {
    flog.trace("getSBOTables: getTermCountDF 4")
    df_4 <- getTermCountDF(dfm4, minFreq)
    flog.trace("getSBOTables: getTermCountDF 3")
    df_3 <- getTermCountDF(dfm3, minFreq)
    flog.trace("getSBOTables: getTermCountDF 2")
    df_2 <- getTermCountDF(dfm2, minFreq)
    flog.trace("getSBOTables: getTermCountDF 1")
    df_1 <- getTermCountDF(dfm1, minFreq)

    flog.trace("getSBOTables: primCalculateSBOTables 4")
    sdf_4 <- primCalculateSBOTables(df_4, df_3)
    flog.trace("getSBOTables: primCalculateSBOTables 3")
    sdf_3 <- primCalculateSBOTables(df_3, df_2)
    flog.trace("getSBOTables: primCalculateSBOTables 2")
    sdf_2 <- primCalculateSBOTables(df_2, df_1)
    flog.trace("getSBOTables: primCalculateSBO_1Table 1")
    sdf_1 <- primCalculateSBO_1Table(df_1)
    list(sdf_1, sdf_2, sdf_3, sdf_4)
}


primCalculateSBOTables <- function(df_n, df_n_1) {
    V <- as.integer(count(df_n))
    mle <- numeric(V)
    nextWord <- character(V)
    ngram_1 <- character(V)
    for (i in 1:V) {
        i_ngram <- df_n$ngram[i]
        words <- unlist(strsplit(i_ngram, " "))
        i_nextWord <- words[length(words)]
        i_ngram_1 <- paste(words[-length(words)], collapse = " ")
        
        c_n <- df_n$count[i]
        c_n_1 <- df_n_1$count[df_n_1$ngram==i_ngram_1]
        mle[i] <- c_n / c_n_1
        nextWord[i] <- i_nextWord
        ngram_1[i] <- i_ngram_1
    }
    df <- data.frame(ngram_1, nextWord, mle)
    #keep for sbo only the ngrams with the highest probability per ngram_1
    #don't filter for the quiz 3 as i need the other probabilities
    df %>% group_by(ngram_1) #%>% filter(mle==max(mle))
}

#
primCalculateSBO_1Table <- function(df1) {
    V <- as.integer(count(df1))
    mle <- numeric(V)
    nextWord <- character(V)
    for (i in 1:V) {
        mle[i] <- df1$count[i] / V
        nextWord[i] <- df1$ngram[i]
    }
    df <- data.frame(nextWord, mle, stringsAsFactors=FALSE)
    #keep for sbo only tghe ngrams with the highest probability
    #todo: man könnte noch optimieren und nur im das ngram mit dem höchsten MLE sammeln...
    #don't filter for the quiz 3 as i need the other probabilities
    #df %>% filter(mle==max(mle))
}


# just calc the mle for known ngrams, no smoothing...
# momentan nicht genutzt
# calcMLE <- function(ngram, df_x, df_x_1) {
#     #print(ngram)
#     # helper calculations..
#     words <- unlist(strsplit(ngram, " "))
#     nextWord <- words[length(words)]
#     ngram_1 <- paste(words[-length(words)], collapse = " ")
#     c_x <- df_x$count[df_x$ngram==ngram]
#     c_x_1 <- df_x_1$count[df_x_1$ngram==ngram_1]
#     mle <- c_x / c_x_1
#     data.frame(ngram_1, nextWord, mle)
# }

# just calc the mle for known 1-grams, no smoothing...
# momentan nicht genutzt...
# calcMLE_1 <- function(nextWord, df_1, V) {
#     #print(ngram)
#     c_x <- df_1$count[df_1$ngram==nextWord]
#     mle <- c_x / V
#     data.frame(nextWord, mle)
# }


#
# ----------------- La Place Smoothing ---------
#

### approach
## 1. Use Markov chains to calculate the Maximum Likelihood estimate 
##    P(C|AB) = count(ABC)/count(AB)
##
## 2. For previous terms whose count is 0, perform Laplace Add - 1 smoothing 
##    Padd-1(C|AB) = (count(ABC) + 1)/(count(AB) + V)
##
## V is the number of unique n-1 grams you have in the corpus
## MLE = (Count(n grams) + 1)/ (Count(n-1 grams) + V)
###

calculateMLE <- function(tdm_n, tdm_n_1, minFreq=5 ) {
    df_n <- getTermCountDF(tdm_n, minFreq)
    df_n_1 <- getTermCountDF(tdm_n_1, minFreq)
    primCalculateMLE(df_n, df_n_1)
}

tbd_primCalculateMLE <- function(df_n, df_n_1) {
    ngram <- df_n[1,]$ngram
    ngram_1 <- paste(unlist(strsplit(ngram, " "))[-1], collapse = " ")
    V <- as.integer(count(df_n_1))
    
    mle <- (df_n$count[df_n$ngram==ngram] + 1) / (df_n_1$count[df_n_1$ngram==ngram_1] + V)
    mle
    lapply(df_n, calcMLE)
    
}

testCalcMLE <- function(df_n, df_n_1) {
    V <- as.integer(count(df_n_1))
    p <- lapply(df_n$ngram, calcMLE, df_n, df_n_1,V)
    df_n$p = unlist(p)
    df_n
}

#calc mle and use laplace smoothing...
calcMLELaPlace <- function(ngram, df_x, df_x_1, V) {
    print(ngram)
    # helper calculations..
    ngram_1 <- paste(unlist(strsplit(ngram, " "))[-1], collapse = " ")
    c_x <- df_x$count[df_x$ngram==ngram]
    c_x_1 <- df_x_1$count[df_x_1$ngram==ngram_1]
    mle <- 0
    if (c_x != 0) {
        # calculate Maximum Likelihood estimate P(C|AB) = count(ABC)/count(AB)
        print("mle")
        mle <- c_x / c_x_1
    } else {
        # perform Laplace Add - 1 smoothing Padd-1(C|AB) = (count(ABC) + 1)/(count(AB) + V)
        print("laplace")
        mle <- (c_x + 1) / (c_x_1 + V)
    }
    print(mle)
    mle
}

test_primCalculateMLE <- function(df_n, df_n_1) {
    ngram <- df_n[1,]$ngram
    ngram_1 <- paste(unlist(strsplit(ngram, " "))[-1], collapse = " ")
    V <- as.integer(count(df_n_1))
    
    mle <- (df_n$count[df_n$ngram==ngram] + 1) / (df_n_1$count[df_n_1$ngram==ngram_1] + V)
    mle
}



# ---------
# Helper functions

printMsg <- function(text, msg) {
    print(paste(text, msg, sep = ": "))
}