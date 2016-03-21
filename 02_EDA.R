########################################################################
##  Data Science Capstone Project
##  3/12/16
##  Exploratory data analysis
########################################################################

# Set the working directory
setwd("/users/Richard/Documents/Coursera Data Science Track/Data Science Capstone")
getwd()

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(ggplot2)
library(scales)
library(gridExtra)


options(mc.cores=1)

# set the random number seed
set.seed(2838)

# set the documents together
all_text_vector <- c(twitter_data,news_data,blogs_data)
rm(list = c("twitter_data","news_data","blogs_data"))

# take a 1% sample of the data to write and test the code
text_sample <- sample(all_text_vector, length(all_text_vector)*0.01, replace = FALSE)

# number of words after the sample
nwords <- function(string, pseudo = F) {
  ifelse(pseudo,
         pattern <- "\\S+",
         pattern <- "[[:alpha:]]+")
  str_count(string, pattern)
}

total_word_count <- data.frame(nwords(text_sample,pseudo=T))
sum(total_word_count)

# make all text lower case
all_text_vector <- tolower(all_text_vector)

# remove the vector of all documents to free up memory
#rm(all_text_vector)

# save the vector
save(text_sample, file="text_sample.RData")
#load("text_sample.RData")

# create the corpus
corpus_sample_text <- Corpus(VectorSource(text_sample),
                      readerControl = list(reader = readPlain,
                                           language = "en_US",
                                           load = TRUE))

test <- termFreq(corpus_sample_text)

# view some sample records
writeLines(as.character(corpus_sample_text[[30]]))
writeLines(as.character(corpus_sample_text[[4600]]))
writeLines(as.character(corpus_sample_text[[3000]]))

# remove colons and hypens
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
corpus_sample_text <- tm_map(corpus_sample_text, toSpace, "-")
corpus_sample_text <- tm_map(corpus_sample_text, toSpace, ":")

# remove all other punctuation
corpus_sample_text <- tm_map(corpus_sample_text, removePunctuation)
corpus_sample_text <- tm_map(corpus_sample_text, toSpace, "’")
corpus_sample_text <- tm_map(corpus_sample_text, toSpace, "‘")
corpus_sample_text <- tm_map(corpus_sample_text, toSpace, " -")

# remove numbers
corpus_sample_text <- tm_map(corpus_sample_text, removeNumbers)

# remove profanity:  NOTE:  List taken from http://fffff.at/googles-official-list-of-bad-words/

profanity <- c("4r5e","5h1t","5hit","a55","anal","anus","ar5e","arrse","arse","ass","ass-fucker","asses",
               "assfucker","assfukka","asshole","assholes","asswhole","a_s_s","b!tch","b00bs","b17ch","b1tch",
                "ballbag","balls","ballsack","bastard","beastial","beastiality","bellend","bestial","bestiality",
                 "bi+ch","biatch","bitch","bitcher","bitchers","bitches","bitchin","bitching","bloody","blow job",
                 "blowjob","blowjobs","boiolas","bollock","bollok","boner","boob","boobs","booobs","boooobs","booooobs",
                 "booooooobs","buceta","bugger","bum","bunny fucker","butt","butthole","buttmuch","buttplug","c0ck",
                 "c0cksucker","carpet muncher","cawk","chink","cipa","cl1t","clit","clitoris","clits","cnut",
                 "cock","cock-sucker","cockface","cockhead","cockmunch","cockmuncher","cocks","cocksuck","cocksucked",
                 "cocksucker","cocksucking","cocksucks","cocksuka","cocksukka","cok","cokmuncher","coksucka",
                 "coon","cox","crap","cum","cummer","cumming","cums","cumshot","cunilingus","cunillingus","cunnilingus",
                 "cunt","cuntlick","cuntlicker","cuntlicking","cunts","cyalis","cyberfuc","cyberfuck","cyberfucked",
                 "cyberfucker","cyberfuckers","cyberfucking","d1ck","damn","dick","dickhead","dildo","dildos",
                 "dink","dinks","dirsa","dlck","dog-fucker","doggin","dogging","donkeyribber","doosh","duche",
                 "dyke","ejaculate","ejaculated","ejaculates","ejaculating","ejaculatings","ejaculation","ejakulate",
                 "f u c k","f u c k e r","f4nny","fag","fagging","faggitt","faggot","faggs","fagot","fagots",
                 "fags","fanny","fannyflaps","fannyfucker","fanyy","fatass","fcuk","fcuker","fcuking","feck","fecker",
                 "felching","fellate","fellatio","fingerfuck","fingerfucked","fingerfucker","fingerfuckers","fingerfucking",
                 "fingerfucks","fistfuck","fistfucked","fistfucker","fistfuckers","fistfucking","fistfuckings","fistfucks",
                 "flange","fook","fooker","fuck","fucka","fucked","fucker","fuckers","fuckhead","fuckheads","fuckin",
                 "fucking","fuckings","fuckingshitmotherfucker","fuckme","fucks","fuckwhit","fuckwit","fudge packer",
                 "fudgepacker","fuk","fuker","fukker","fukkin","fuks","fukwhit","fukwit","fux","fux0r","f_u_c_k",
                 "gangbang","gangbanged","gangbangs","gaylord","gaysex","goatse","god-dam","god-damned","goddamn",
                 "goddamned","hardcoresex","hell","heshe","hoar","hoare","hoer","homo","hore","horniest","horny",
                 "hotsex","jack-off","jackoff","jap","jerk-off","jism","jiz","jizm","jizz","kawk","knob","knobead",
                 "knobed","knobend","knobhead","knobjocky","knobjokey","kock","kondum","kondums","kum","kummer",
                 "kumming","kums","kunilingus","l3i+ch","l3itch","labia","lmfao","lust","lusting","m0f0","m0fo",
                 "m45terbate","ma5terb8","ma5terbate","masochist","master-bate","masterb8","masterbat*","masterbat3",
                 "masterbate","masterbation","masterbations","masturbate","mo-fo","mof0","mofo","mothafuck","mothafucka",
                 "mothafuckas","mothafuckaz","mothafucked","mothafucker","mothafuckers","mothafuckin","mothafucking",
                 "mothafuckings","mothafucks","mother fucker","motherfuck","motherfucked","motherfucker","motherfuckers",
                 "motherfuckin","motherfucking","motherfuckings","motherfuckka","motherfucks","muff","mutha","muthafecker",
                 "muthafuckker","muther","mutherfucker","n1gga","n1gger","nazi","nigg3r","nigg4h","nigga","niggah",
                 "niggas","niggaz","nigger","niggers","nob","nob jokey","nobhead","nobjocky","nobjokey","numbnuts",
                 "nutsack","orgasim","orgasims","orgasm","orgasms","p0rn","pawn","pecker","penis","penisfucker","phonesex",
                 "phuck","phuk","phuked","phuking","phukked","phukking","phuks","phuq","pigfucker","pimpis","piss",
                 "pissed","pisser","pissers","pisses","pissflaps","pissin","pissing","pissoff","poop","porn",
                 "porno","pornography","pornos","prick","pricks","pron","pube","pusse","pussi","pussies","pussy",
                 "pussys","rectum","retard","rimjaw","rimming","s hit","s.o.b.","sadist","schlong","screwing","scroat",
                 "scrote","scrotum","semen","sex","sh!+","sh!t","sh1t","shag","shagger","shaggin","shagging",
                 "shemale","shi+","shit","shitdick","shite","shited","shitey","shitfuck","shitfull","shithead","shiting",
                 "shitings","shits","shitted","shitter","shitters","shitting","shittings","shitty","skank","slut",
                 "sluts","smegma","smut","snatch","son-of-a-bitch","spac","spunk","s_h_i_t","t1tt1e5","t1tties",
                 "teets","teez","testical","testicle","tit","titfuck","tits","titt","tittie5","tittiefucker",
                 "titties","tittyfuck","tittywank","titwank","tosser","turd","tw4t","twat","twathead","twatty",
                 "twunt","twunter","v14gra","v1gra","vagina","viagra","vulva","w00se","wang","wank","wanker",
                 "wanky","whoar","whore","willies","willy","xrated","xxx")

corpus_sample_text <- tm_map(corpus_sample_text, removeWords, profanity)

# strip white space
corpus_sample_text <- tm_map(corpus_sample_text, stripWhitespace)

# create the document term matrix - limit the maximum word length to 40 and remove very sparse terms
sample_dtm <- removeSparseTerms(DocumentTermMatrix(corpus_sample_text, 
                                                   control = list(wordLengths = c(1,40))),0.9999)


# look at some examples
inspect(sample_dtm[1:2,1:10])

# now get the counts of the terms
term_counts <- as.matrix(sample_dtm)
term_counts[1:10,1:20]
term_counts <- colSums(term_counts)
term_counts <- as.data.frame(term_counts)
term_counts$term <- rownames(term_counts)
names(term_counts)[names(term_counts)=="term_counts"] <- "term_frequency"
term_counts <- term_counts[order(-term_counts$term_frequency),]
sum(term_counts$term_frequency)

quantile(term_counts$term_frequency,prob = 0.5,0.9)

term_counts$cumperc <- cumsum(term_counts$term_frequency)/sum(term_counts$term_frequency)
median <- nrow(subset(term_counts,term_counts$cumperc <= 0.5))
percentile_90 <- nrow(subset(term_counts,term_counts$cumperc <= 0.9))


wordcloud(corpus_sample_text, 
          max.words = 400, 
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"),
          scale=c(5,0.5),
          rot.per=0.35)


##### Now count bi-grams and tri-grams #####

## Bigrams

BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

sample_text_bigram <- TermDocumentMatrix(corpus_sample_text, control = list(tokenize = BigramTokenizer))
inspect(removeSparseTerms(sample_text_bigram[, 1:10], 0.99))
sample_text_bigram <- removeSparseTerms(sample_text_bigram, 0.99)

# now get the counts of the terms
bigram_counts <- as.matrix(sample_text_bigram)
bigram_counts[1:10,1:20]
counts <- as.data.frame(rowSums(bigram_counts))
bigram_counts <- as.data.frame(bigram_counts)
bigram_counts$bigram_term <- rownames(bigram_counts)
bigram_counts <- cbind(bigram_counts$bigram_term,counts)
names(bigram_counts)[names(bigram_counts)=="rowSums(bigram_counts)"] <- "term_frequency"
names(bigram_counts)[names(bigram_counts)=="bigram_counts$bigram_term"] <- "bigram_term"
bigram_counts <- bigram_counts[order(-bigram_counts$term_frequency),]

wordcloud(bigram_counts$bigram_term,
          freq = bigram_counts$term_frequency,
          max.words = 400, 
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"),
          scale=c(5,0.5),
          rot.per=0.35)

## Trigrams

TrigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

sample_text_trigram <- TermDocumentMatrix(corpus_sample_text, control = list(tokenize = TrigramTokenizer))
inspect(removeSparseTerms(sample_text_trigram[, 1:10], 0.999))
sample_text_trigram <- removeSparseTerms(sample_text_trigram, 0.999)

# now get the counts of the terms
trigram_counts <- as.matrix(sample_text_trigram)
trigram_counts[1:10,1:20]
counts <- as.data.frame(rowSums(trigram_counts))
trigram_counts <- as.data.frame(trigram_counts)
trigram_counts$trigram_term <- rownames(trigram_counts)
trigram_counts <- cbind(trigram_counts$trigram_term,counts)
names(trigram_counts)[names(trigram_counts)=="rowSums(trigram_counts)"] <- "term_frequency"
names(trigram_counts)[names(trigram_counts)=="trigram_counts$trigram_term"] <- "trigram_term"
trigram_counts <- trigram_counts[order(-trigram_counts$term_frequency),]

wordcloud(trigram_counts$trigram_term,
          freq = trigram_counts$term_frequency,
          max.words = 400, 
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"),
          scale=c(5,0.5),
          rot.per=0.35)

