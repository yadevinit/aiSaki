# was: aiSaki.R
# TBD other install functions
# refer aiSaki-flowConversation.pdf.

options(java.parameters="- Xmx256m") # was: "- Xmx1g" but shinyapps.io defaults to 512MB RAM
suppressMessages(require(rJava))
  # "1000m" or "1g" both denote 1000MB or 1GB RAM for Java heap space. This has to be within your computer memory.
  # ref https://stackoverflow.com/questions/41417961/running-out-of-memory-with-pos-tagging-in-r.
  # https://stackoverflow.com/questions/21937640/handling-java-lang-outofmemoryerror-when-writing-to-excel-from-r says:
  #   "further improvement can be obtained by requesting a garbage collection in each iteration of the loop."
  # gc() might be slow but avoids "java.lang.OutOfMemoryError: GC overhead limit exceeded".
  # Alternatively, prefer C/C++ code.
jgc <- function(){
  gc()
  .jcall("java/lang/System", method = "gc")
}

installGoogleAPI <- function(){
  require(devtools)
  devtools::install_github("ropensci/cld2") # it builds too. R Wrapper for Google's Compact Language Detector 2,
    # which uses a Bayesian model, whereas cld3 uses Neural Networks
  return()
}
suppressMessages(require("cld2"))

suppressMessages(require("NLP"))
suppressMessages(require("openNLP"))
# install.packages("openNLPmodels.en", repos="http://datacube.wu.ac.at", type="source")
  # first time only for MS-Windows. For Named Entity Recognition (NER) and Chunk annotation
  # Chunking needs word-token annotations with POS tags

# devtools::install_github("cran/openNLPmodels.en") # BEWARE this is for now since pkg not on CRAN
  # github pkg version is of 2009, whereas sourceforge or the datacube repos has recent models
suppressMessages(require("openNLPmodels.en"))
  # BEWARE commented coz uninstallable in shinyapps.io:
  # with Shiny, date is explicitly ensured to be valid. NER of person's name is temporarily
  # not called for coz not good enough for Indian names; needed though

# download models such as en-ner-date.bin from http://opennlp.sourceforge.net/models-1.5/
# then copy into <R library install path>/openNLPdata/models, e.g.,
#   C:/Users/SONY/Documents/R/win-library/3.4/openNLPdata/models
# then launch R afresh so that it finds the present models

# py -2.7 -m pip install -U <pkg>
#   ensures compatible python version's environment is used. else earlier in PATH or default might get chosen
#   --user to install only in user space. safer since it requires lesser permissions than sudo
installPattern <- function(){
  require(devtools)
  # after installing compatible Python2.5+, not Python3+.  and after including in PATH
  devtools::install_github("clips/pattern") # this is not an R pkg, likely Python. Has pattern.db module etc.
    # BSD license
  devtools::install_github("bnosac/pattern.nlp", args="--no-multiarch")
    # as per https://api.github.com/repos/bnosac/pattern.nlp, which requires compatible Python. It says:
    # Make sure your when you run the R version (64/32 bit) it is the same as the Python version
    # you installed (64/32 bit). Advise: don't use RStudio, but just plain R when executing the
    # code. Mark that the pattern.nlp package is released under the AGPL-3 license.
    # shinyapps.io presently supports a minimal Python 2.7
  library(pattern.nlp)
  return()
}
# library(findpython)
# can_find_python_cmd(required_modules = "pattern.db")
# (require("pattern.nlp"))
# suppressMessages(require("pattern.nlp"))


cPOS <- "POS"
cPOS.VerbPastTense <- "VBD"
cPOS.Adjective <- c("JJ", "JJR", "JJS") # including comparative & superlative
cPOS.Verb <- "VB"; cPOS.NounProperSingular <- "NNP"
cNER <- "kind"; cNERtype <- "entity"
cNER.person <- "person"; cNER.date <- "date"
cNERkinds <- c(cNER.person, cNER.date) # , "location", "money", "organization", "time", "percentage")
cLoops <- 2 # attempts to get human being's expression
cPattern.English <- "english"
cPattern.subjThreshold <- 0.1 # subjectivity [0,1] is flagged when >= threshold
cPattern.polarThreshold <- 0.3 # polarity [-1,1] is flagged when its abs() >= threshold
cLangReliabilityCutoff <- 0.95 # for language detection
c.env.PC <- 1; c.env.RmarkdownShiny <- 2 # TBD interactive() instead?

g.debug <- FALSE
g.dbreak.s <- NULL # input string from user
g.dbreak.annPTdoc.sents <- NULL; g.dbreak.annPTdoc.words <- NULL # AnnotatedPlainTextDocument
g.dbreak.name <- "Leader"; g.dbreak.date <- date() # was: as.character(Sys.Date(), format="%d-%b-%Y")
g.dbreak.future <- NULL; g.dbreak.area <- NULL
g.dbreak.reality <- NULL
g.dbreak.defaultFuture <- g.dbreak.wantFuture <- NULL
g.dbreak.action <- NULL
# g.dbreak.wantFuture.ann and other global variables are generated within functions
g.env <- NULL


myWordListDocument <- function(encoding = "unknown", meta = list()){
  # adapted from NLP::WordListDocument()
  # <environment: namespace:NLP>
  # TBD ns("NLP")
  if(g.env == c.env.RmarkdownShiny){
    words <- TBD
  } else {
    words <- readLines(stdin(), encoding = encoding, warn = FALSE)
  }
  doc <- list(content = words, meta = meta)
  class(doc) <- c("WordListDocument", "TextDocument")
  doc
}
getParas <- function(){
  ans <- myWordListDocument(encoding="unknown", meta=list())
  return(ans)
}
myCat <- function(myEnv=c.env.RmarkdownShiny, s, ...){
  # return()

  if(interactive()){ # (g.env == c.env.RmarkdownShiny){
    ({print(paste(s, ...))}) # convey to renderPrint(). return a reactive fn that renders printed output
  } else {
    cat(s, ...)
  }
  # auto return the last evaluated expression
}
getPastConversations.df <- function(){
  
setwd("F:/aiSaki/")
  
mySent <- c("data-whatSo-declareBreakdown.txt") # alt: metadata "What's So" across .pdf
  
colClasses <- c("factor", "character", "integer")

  myDfSubjectivity <- read.table(mySent, sep="\t", colClasses=colClasses, header=TRUE)
  # 
str(myDfSubjectivity)
  return(myDfSubjectivity)
}
entities <- function(doc, kind) { # adapted from RPubs.  Extract entities from an AnnotatedPlainTextDocument
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, cNER) # "kind"
    s[a[k == kind]]
  } else {
    s[a[a$type == cNERtype]]
  }
}
myDebug <- function(...){
  if(g.debug){ myCat(myEnv=c.env.RmarkdownShiny, "<debug:\n", ..., ">\n") } # else continue
  return()
}
myFail <- function(mys=""){
  myCat(myEnv=c.env.RmarkdownShiny, mys)
  stop("\nI stop now. This is beyond my coaching ability. Please contact a human coach")
  # return()
}
isLangSupported <- function(mys){
  langMix <- cld2::detect_language_mixed(mys, plain_text=TRUE) # set plain_text=FALSE if input contains HTML
  myDebug(unlist(langMix))
  langMix.top <- (langMix[[1]])[1,]
  ans <- (max(langMix.top$proportion) >= cLangReliabilityCutoff)
  if(ans){
    myCat(myEnv=c.env.RmarkdownShiny,
      paste(langMix.top$language, "code -", langMix.top$code, "is what I see you using\n"))
  } else {
    myCat(myEnv=c.env.RmarkdownShiny,
      "unsupported language mix\n"); myFail(unlist(langMix))
  }
  return(ans)
}
myString <- function(aList){
  ans <- as.String(paste0(aList, collapse=" "))
  return(ans)
}
myAnnotate <- function(mys, forOnlyPOS=TRUE){
  g.dbreak.s <<- as.String(paste(unlist(mys), collapse=" "))
  # tree parsing and other extensions are possible towards context-sensitive grammars
  sent.ant <- Maxent_Sent_Token_Annotator()
  word.ant <- Maxent_Word_Token_Annotator()
  pos.ant <- Maxent_POS_Tag_Annotator() # probs=TRUE
  if(forOnlyPOS){
    pipeline.ant <- list(sent.ant, word.ant, pos.ant) # first sentence then word annotation
  } else { # annotate further, e.g., for NER
    pipeline.ant <- list(sent.ant, word.ant, pos.ant,
      Maxent_Entity_Annotator(kind=cNER.person), # doesn't mark names such as (Indian) Navneet
      Maxent_Entity_Annotator(kind=cNER.date)) # doesn't mark dates such as 30-Nov-2017
    # earlier, named-entity recognition for date did not work coz pipelined with already-listed pipeline
  }

  ans <- annotate(g.dbreak.s, pipeline.ant)
  # a5; mys[entity_annotator(mys, a3)] # fails saying invalid subscript type list
  ans.doc <- AnnotatedPlainTextDocument(g.dbreak.s, ans)
  g.dbreak.annPTdoc.sents <<- sents(ans.doc); g.dbreak.annPTdoc.words <<- words(ans.doc) # AnnotatedPlainTextDocument
  myDebug("sentences:", myString(g.dbreak.annPTdoc.sents)); myDebug("words:", g.dbreak.annPTdoc.words)
  if(! forOnlyPOS){
    myDebug(cNER.person, entities(ans.doc, kind=cNER.person)); myDebug(cNER.date, entities(ans.doc, kind=cNER.date))
  } # else continue
  return(ans)
}
isFeaturePresent <- function(chkFeature, a3, theFeatures=cPOS){
  stopifnot(theFeatures == cPOS || theFeatures == cNER) # assert
  a3.featExtract <- subset(a3, type==ifelse(theFeatures==cPOS, "word", cNERtype))
  a3.featExtract.tags <- sapply(a3.featExtract$features, `[[`, theFeatures)
  # a3.featExtract.tags.tab <- table(a3.featExtract.tags) # tabulate frequencies for efficiency  
  return(chkFeature %in% a3.featExtract.tags)
}
isNERpresent <- function(chkNER, a4, theFeatures=cNER){
  return(isFeaturePresent(chkNER, a4, theFeatures))
}
getPolaritySubjectivity <- function(mys, lang=cPattern.English){
  ans <- pattern_sentiment(mys, lang)
  return(ans)
}
isPolar <- function(patt.senti, thresh=cPattern.polarThreshold){
  ans <- (abs(patt.senti$polarity) > thresh)
  if(ans){
    myDebug(paste("is polar:", patt.senti$polarity, ":", patt.senti$id))
  } # else continue
  return(ans)
}
isSubjective <- function(patt.senti, thresh=cPattern.subjThreshold){
  ans <- (patt.senti$subjectivity > thresh) # subjectivity range is [0,1]. so, no abs() needed
  if(ans){
    myDebug(paste("is subjective:", patt.senti$subjectivity, ":", patt.senti$id))
  } # else continue
  return(ans)
}
isSentenceSubjectivePolar <- function(wld.sent){
  mys <- paste(unlist(wld.sent), collapse=" ")
  myDebug(paste(wld.sent, "- became -", mys))
  patt.senti <- getPolaritySubjectivity(mys)
  ans <- isSubjective(patt.senti) || isPolar(patt.senti)
  if(ans){
    myCat(myEnv=c.env.RmarkdownShiny,
      "Please say what really happened or existed out there that makes you say this (what I believe is) opinion:\n",
      patt.senti$id, "\n")
  } # else continue
  return(ans)
}
areSentencesSubjectivePolar <- function(annPTdoc.sents){
  ansVec <- sapply(1:length(annPTdoc.sents), # returns vector while lapply() returns list; named ones can complicate
    function(x, y) isSentenceSubjectivePolar(y[x]),
    y=annPTdoc.sents,
    simplify=TRUE) # simplify to vectorize, since we don't need a list
  ans <- TRUE %in% ansVec
  myDebug(paste(annPTdoc.sents, "- mapped to -", ansVec))
  return(ans)
}


isFuture <- function(a3){
  ans <- (! isFeaturePresent(cPOS.VerbPastTense, a3, cPOS))
  myDebug(ans, "TBD certainty =expression validity because", "no", cPOS.VerbPastTense, "\n")
  return(ans)
}
dbreak.wantFuture <- function(loop=cLoops){
  ans <- FALSE
  for(i in 1:loop){
    myCat(myEnv=c.env.RmarkdownShiny,
      "So, what is the Future wanted right now, for yourself or others?\n")
    g.dbreak.wantFuture <<- getParas()
    g.dbreak.wantFuture.ann <<- myAnnotate(g.dbreak.wantFuture, TRUE)
    stopifnot(isLangSupported(g.dbreak.s)) # g.dbreak.s is set as side effect by myAnnotate()
    ans <- isFuture(g.dbreak.wantFuture.ann)
    if(ans){ break } else { myCat(myEnv=c.env.RmarkdownShiny, ifelse(i < loop, "Try again\n", "")); next }
  }
  return(ans)
}
dbreak.defaultFuture <- function(loop=cLoops){
  ans <- FALSE
  for(i in 1:loop){
    myCat(myEnv=c.env.RmarkdownShiny,
      "In that Area if no new Action is taken, what is the Default Future you head into?\n")
    myCat(myEnv=c.env.RmarkdownShiny,
      "What is the Default Future that is going to happen unless something dramatic and unexpected happens?\n")
    myCat(myEnv=c.env.RmarkdownShiny,
      "What will it be like for you and others, near future and long term, and in this and all areas of your life?\n")
    # ref: http://www.sesp.northwestern.edu/docs/publications/103212837744f8415902f1d.pdf
    #   "Emerging from the CAVE: Attributional Style and the Narrative Study of Identity in Midlife Adults"
    #   causal attribution coded along 3 dimensions: internal-external, global-specific, and stable-unstable
    #   Explanatory or Attributional Style can be Personal, Permanent, and Pervasive
    #   Contamination Sequences ~~ Life Events for self-defining life stories: these have greater impact
    #   These relate with role of narrative (identity for a Redemptive-Self life story, as per McAdam) in personality
    #   psychology
    g.dbreak.defaultFuture <<- getParas()
    g.dbreak.defaultFuture.ann <<- myAnnotate(g.dbreak.defaultFuture, TRUE)
    ans <- isFuture(g.dbreak.defaultFuture.ann)
    if(ans){ break } else { myCat(myEnv=c.env.RmarkdownShiny, ifelse(i < loop, "Try again\n", "")); next }
  }
  return(ans)
}
dbreak.act <- function(loop=cLoops){
  ans <- FALSE
  for(i in 1:loop){
    myCat(myEnv=c.env.RmarkdownShiny,
      "For causing that Future, what is the (missing-conversation) Action and with whom? By when will you do it?\n")
    g.dbreak.action <<- getParas()
    g.dbreak.action.ann <<- myAnnotate(g.dbreak.action, FALSE) # FALSE is what's desired for NER annotations too
    ans <- isAct(g.dbreak.action.ann)
    if(ans){ break } else { myCat(myEnv=c.env.RmarkdownShiny, ifelse(i < loop, "Try again\n", "")); next }
  }
  return(ans)
}
want.dbreak <- function(){
  toDbreak <- tolower(readline(
    "Do you want to declare a breakdown and create a new Future and Action? Type Y: "))
  ans <- (toDbreak == "y")
  return(ans)
}
dbreak.okDefaultFuture <- function(loop=cLoops){ # later, relate with Area and re-consider loop
  okDefault <- tolower(readline(
    "Are you really ok with this Default Future? If so, type Y: "))
  ans <- (okDefault == "y")
  return(ans)
}
isArea <- function(anArea){
  myDebug("TBD assuming valid Area\n")
  return(TRUE)
}
dbreak.area <- function(loop=cLoops){
  ans <- FALSE
  for(i in 1:loop){
    myCat(myEnv=c.env.RmarkdownShiny,
      "Life happens in career, relationships, hobbies, society, money, and health Areas.\n")
    myCat(myEnv=c.env.RmarkdownShiny,
      "What is an Area of your life that is not working as well as wanted?\n")
    g.dbreak.area <<- getParas()
    ans <- isArea(g.dbreak.area)
    if(ans){ break } else { myCat(myEnv=c.env.RmarkdownShiny, ifelse(i < loop, "Try expressing again\n", "")); next }
  }
  return(ans)
}
isReality <- function(a3, annPTdoc.sents){
  # ans1 <- (! isFeaturePresent(cPOS.Adjective, a3, cPOS))
  # myDebug("TBD pastness certainty", ans1, "=expression test for", "no", cPOS.Adjective, "\n")
  ans2 <- (! areSentencesSubjectivePolar(annPTdoc.sents))
  # ans <- (ans1 && ans2) # for now. eventually, ans2 might be chosen as superior over ans1
  ans <- ans2
  return(ans)
}
dbreak.reality <- function(loop=cLoops){
  ans <- FALSE
  for(i in 1:loop){
    myCat(myEnv=c.env.RmarkdownShiny,
      "Related to chosen Area, what really happened out there, or did not happen?\n",
      "What exists or does not exist in reality out there?\n",
      "Express those without any subjectivity, opinions, assessments, inner thoughts, or inferences.\n")
    g.dbreak.reality <<- getParas()
    g.dbreak.reality.ann <<- myAnnotate(g.dbreak.reality, TRUE)
    ans <- isReality(g.dbreak.reality.ann, # was: (! isFuture(g.dbreak.reality.ann)) for "pastness"
       g.dbreak.annPTdoc.sents) # g.dbreak.annPTdoc.sents updated temporarily by myAnnotate()
       # using g.dbreak.s loses the sentence boundaries that have already been identified; so, loses precise reporting
       # to user regarding which specific sentence or utterance is subjective or polar.
    if(ans){ break } else { myCat(myEnv=c.env.RmarkdownShiny, ifelse(i < loop, "Try expressing again\n", "")); next }
  }
  return(ans)
}
dbreak <- function(){
  ans <- FALSE
  ans <- (dbreak.area() && dbreak.reality() && dbreak.defaultFuture())
  if(ans){
    myCat(myEnv=c.env.RmarkdownShiny,
      "You expressed an Area, its Realities, and Default Future\n")
    ans <- dbreak.okDefaultFuture() # Being declared whether (s)he is ok with Default Future
    if(ans){
      myCat(myEnv=c.env.RmarkdownShiny,
        "You said you are ok with the Default Future. You are now re-assured\n")
    } else {
      ans <- (dbreak.wantFuture() && dbreak.act())
      myCat(myEnv=c.env.RmarkdownShiny, ifelse(ans,
        "You have declared a breakdown and created a Future that matters\n",
        "You missed something in expressing a Future you want or Action for it\n"))
    }
  } else {
    myCat("You missed something in expressing the Area, its Realities, or Default Future\n")
  }
  return(ans)
}
aiSaki <- function(myEnv=c.env.RmarkdownShiny, toDebug=FALSE){ # re-write this with single return
  g.debug <<- toDebug
  g.env <<- myEnv
  ans <- FALSE
  myCat(myEnv, "Dear ") # , g.dbreak.name, ", now the time is ", g.dbreak.date, ".\n", sep="")
  myCat("Consider that every human Being (or group) has the Power to create a Future right now.\n",
    "The Being can choose to do one missing (conversation) Action for it.\n",
    "For that, the Being must be aware of (i.e., present to) that Action.\n",
    "Thats what this aiSaki coach causes.\n",
    "(Use complete sentences and capitals since I do not have expert comprehension yet,\n",
    "  and type ENTER then CTRL-Z to end input on MS-Windows.)\n",
    "(If you wish to know more of this coaching, refer to http://www.sameerdua.com/declaring-breakdowns/ for a gist.)\n")
  ans <- dbreak.wantFuture() && dbreak.act() # beware: depend on conditional left-to-right eval!
  if(ans){
    myCat("You are present to a Future you want and Action for it. Go ahead.\n")
  } else {
    myCat("You do not seem aware of a Future that matters or Action for it.\n")
    jgc() # request garbage collection, as suggested earlier
    ans <- want.dbreak() && dbreak() # beware: depend on conditional left-to-right eval!
    if(! ans){
      myFail() # gets stopped
    } else {
      # do nothing
    }
  }
  return(ans)
}
do.discoverTopicsAcrossConversations <- function(){ # ref: https://github.com/trinker/topicmodels_learning
  # install.packages('servr')
  require(RTextTools)
  require(topicmodels)
  require("servr")
  
## Install/Load Tools & Data

  # if (!require("pacman")) install.packages("pacman")
  require("pacman")
  
# pacman::p_load_gh("trinker/gofastr")
  
# pacman::p_load(tm, topicmodels, dplyr, tidyr, igraph, devtools, LDAvis, ggplot2)
  require("gofastr"); require(tm); require(topicmodels); require("dplyr"); require("tidyr"); require("igraph");
  require("devtools"); require("LDAvis"); require("ggplot2")
  
## Source topicmodels2LDAvis & optimal_k functions
  
invisible(lapply(

    file.path("https://raw.githubusercontent.com/trinker/topicmodels_learning/master/functions",
    # file.path("F:/www-all/topicmodels_learning-master-githubCom/topicmodels_learning-master/functions",
      c("topicmodels2LDAvis.R", "optimal_k.R")
),

    ## devtools::source_url
 # uses libcurl for URL
    source
))


  myDfSubjectivity <- getPastConversations.df()
  
stops <- c(
tm::stopwords("english"), 
tm::stopwords("SMART")
) %>%

    gofastr::prep_stopwords()
  doc_term_mat <- myDfSubjectivity %>% with(gofastr::q_dtm_stem(paragraph, source)) %>%
    
gofastr::remove_stopwords(stops, stem=TRUE) %>%

    gofastr::filter_tf_idf() %>%
 gofastr::filter_documents()
  
control <- list(burnin=500, iter=1000, keep=100, seed=2500)
  (k <- optimal_k(doc_term_mat, 40, control=control))
  
control[["seed"]] <- 100

  lda_model <- topicmodels::LDA(doc_term_mat, k=as.numeric(k), method="Gibbs",
 control=control)
  
lda_model %>% 
topicmodels2LDAvis() %>% 
LDAvis::serVis()


  ## Plot the Topics
  # topics <- topicmodels::posterior(lda_model, doc_term_mat)[["topics"]]
  # colnames(topic_dat)[-1] <- apply(terms(lda_model, 10), 2, paste, collapse = ", ")
  # tidyr::gather(topic_dat, Topic, Proportion, -c(Person_Time)) %>%
  #   tidyr::separate(Person_Time, c("Person", "Time"), sep = "_") %>%
  #   dplyr::mutate(Person = factor(Person, 
  #     levels = c("OBAMA", "ROMNEY", "LEHRER", "SCHIEFFER", "CROWLEY", "QUESTION" ))
  #   ) %>%
  #   ggplot2::ggplot(ggplot2::aes(weight=Proportion, x=Topic, fill=Topic)) +
  #     ggplot2::geom_bar() +
  #     ggplot2::coord_flip() +
  #     ggplot2::facet_grid(Person~Time) +
  #     ggplot2::guides(fill=FALSE) +
  #     ggplot2::xlab("Proportion")
  ## Plot the Topics Matrix as a Heatmap 
  # heatmap(topics, scale = "none")

  return()
}
discoverTopicsAcrossConversations <- function(){
  if(! g.debug){
    suppressPackageStartupMessages(do.discoverTopicsAcrossConversations())
  } else {
    do.discoverTopicsAcrossConversations()
  }
  return()
}

myDebug("Now as User, type the following to be coached: aiSaki()\n",
  "  then as Administrator, type discoverTopicsAcrossConversations()\n",
  "    to discover topics across coaching conversations\n")
  # was writeLines()
