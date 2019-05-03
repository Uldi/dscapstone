setupNLP <- function() {
    cm <- allStepsWithTestData()
    tdm1 <- getTermDocMatrix(cm, 1)
    tdm2 <- getTermDocMatrix(cm, 2)
    tdm3 <- getTermDocMatrix(cm, 3)
    tdm4 <- getTermDocMatrix(cm, 4)
    getSBOTables(tdm4, tdm3, tdm2, tdm1, 1)
}

predictNLP <- function(ngramSBOTables, text) {
    paste(text, "answer",sep = " ")
}

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
getSBOTables <-  function(tdm_4, tdm_3, tdm_2, tdm_1, minFreq=5) {
    df_4 <- getTermCountDF(tdm_4, minFreq)
    df_3 <- getTermCountDF(tdm_3, minFreq)
    df_2 <- getTermCountDF(tdm_2, minFreq)
    df_1 <- getTermCountDF(tdm_1, minFreq)

    sdf_4 <- primCalculateSBOTables(df_4, df_3)
    sdf_3 <- primCalculateSBOTables(df_3, df_2)
    sdf_2 <- primCalculateSBOTables(df_2, df_1)
    sdf_1 <- primCalculateSBO_1Table(df_1)
    list(sdf_1, sdf_2, sdf_3, sdf_4)
}

primCalculateSBOTables <- function(df_n, df_n_1) {
    l  <- lapply(df_n$ngram, calcMLE, df_n, df_n_1)
    sdf <- do.call(rbind, l)
    sdf
}

#tbd
primCalculateSBO_1Table <- function(df_1) {
    V <- as.integer(count(df_1))
    l  <- lapply(df_1$ngram, calcMLE_1, df_1, V)
    sdf <- do.call(rbind, l)
    sdf
}

#just calc the mle for known ngrams, no smoothing...
calcMLE <- function(ngram, df_x, df_x_1) {
    print(ngram)
    # helper calculations..
    words <- unlist(strsplit(ngram, " "))
    ngramWord <- words[length(words)]
    ngram_1 <- paste(words[-length(words)], collapse = " ")
    c_x <- df_x$count[df_x$ngram==ngram]
    c_x_1 <- df_x_1$count[df_x_1$ngram==ngram_1]
    mle <- c_x / c_x_1
    l <- list(ngram_1 = ngram_1, ngramWord = ngramWord, mle=mle)
    print(l)
    l
}

#just calc the mle for known 1-grams, no smoothing...
calcMLE_1 <- function(ngram, df_1, V) {
    print(ngram)
    c_x <- df_1$count[df_1$ngram==ngram]
    mle <- c_x / V
    l <- list(ngramWord = ngram, mle=mle)
    print(l)
    l
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