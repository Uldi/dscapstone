

generateSampleFiles <- function(p=0.01) {
    fileNames <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
    for(fileName in fileNames) {
        generateSampleFile(fileName, p)
    }
}

generateSampleFile <- function(fileName, p=0.01) {
    print(fileName)
    set.seed(12345)
    numLines <- 10000
    inFile <- file(paste("data/final/en_US", fileName, sep="/"), "r")
    outFile <- file(paste("data/samples", fileName, sep="/"), "w")
    txtLines <- readLines(inFile, numLines)
    while (length(txtLines)) {
        sample <- rbinom(length(txtLines),1,p)
        writeLines(txtLines[sample==1],outFile)
        txtLines <- readLines(inFile,numLines)
    }
    close(inFile)
    close(outFile)
}

getProfanityWords <- function() {
    pFile <- file("data/Google-profanity-words-master/list.txt", "r")
    profanityWords <- readLines(pFile)
    close(pFile)
    profanityWords
}