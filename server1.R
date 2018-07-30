source("server11.R", local=TRUE) # beware scope!! was: "aiSaki.R"
# sentDetect(s, model = system.file("models", "sentdetect", "EnglishSD.bin.gz", package = "openNLPmodels.en"))

cLoops <- 3 # attempts to get human being's expression. Make it a Server session-Specific var

myCat <- function(myEnv=c.env.RmarkdownShiny, s, ...){
  if(interactive()){ # (g.env == c.env.RmarkdownShiny){
    ({print(paste(s, ...))}) # convey to renderPrint(). return a reactive fn that renders printed output
  } else {
    cat(s, ...)
  }
  # auto return the last evaluated expression
}
myFail <- function(mys=""){
  myCat(myEnv=c.env.RmarkdownShiny, mys)
  stop("\nWith your input, I feel like stopping now. This is beyond my coaching ability. Please contact a human coach")
  # return()
}
myWLD <- function(txt, encoding = "unknown", meta = list()){
  # adapted from NLP::WordListDocument()
  # <environment: namespace:NLP>
  # TBD ns("NLP")
  # words <- readLines(stdin(), encoding = encoding, warn = FALSE)
  words <- txt
  doc <- list(content = words, meta = meta)
  class(doc) <- c("WordListDocument", "TextDocument")
  doc
}
myAnnotate <- function(mys, forOnlyPOS=TRUE){
  g.dbreak.s <- as.String(paste(unlist(mys), collapse=" ")) # was <<-
  # tree parsing and other extensions are possible towards context-sensitive grammars
  sent.ant <- Maxent_Sent_Token_Annotator()
  word.ant <- Maxent_Word_Token_Annotator()
  pos.ant <- Maxent_POS_Tag_Annotator() # probs=TRUE
  if(forOnlyPOS){
    pipeline.ant <- list(sent.ant, word.ant, pos.ant) # first sentence then word annotation
  } else { # annotate further, e.g., for NER
    pipeline.ant <- list(sent.ant, word.ant, pos.ant
# BEWARE: presently commenting out following 2 lines coz unsupported by github openNLPmodels.en
#      , Maxent_Entity_Annotator(kind=cNER.person), # doesn't mark names such as (Indian) Navneet
#      Maxent_Entity_Annotator(kind=cNER.date) # doesn't mark dates such as 30-Nov-2017
    )
    # earlier, named-entity recognition for date did not work coz pipelined with already-listed pipeline
  }

  ans <- annotate(g.dbreak.s, pipeline.ant)
  # a5; mys[entity_annotator(mys, a3)] # fails saying invalid subscript type list
  ans.doc <- AnnotatedPlainTextDocument(g.dbreak.s, ans)
  g.dbreak.annPTdoc.sents <- sents(ans.doc) # <<- ; g.dbreak.annPTdoc.words <<- words(ans.doc)
    # was <<- AnnotatedPlainTextDocument
  myDebug("sentences:", myString(g.dbreak.annPTdoc.sents)) # ; myDebug("words:", g.dbreak.annPTdoc.words)
  # if(! forOnlyPOS){
  #   myDebug(cNER.person, entities(ans.doc, kind=cNER.person)); myDebug(cNER.date, entities(ans.doc, kind=cNER.date))
  # } # else continue
  return(list(ans, g.dbreak.annPTdoc.sents)) # was: return(ans)
}
# wantFuture <- function(userText){
#     g.dbreak.wantFuture <- myWLD(userText)
#     g.dbreak.wantFuture.ann <- (myAnnotate(g.dbreak.wantFuture, TRUE))[[1]] # beware had <<- side effects
#     # stopifnot(isLangSupported(g.dbreak.s)) # g.dbreak.s is set as side effect by myAnnotate()
#     ans <- isFuture(g.dbreak.wantFuture.ann) # beware calls cat()
#     # if(ans){ break } else { myCat(ifelse(i < loop, "Try again\n", "")); next }

#   return(ans)
# }
isLangOk <- function(mys){
  s <- as.String(paste(unlist(mys), collapse=" "))
  ans <- isLangSupported(s)
  return(ans)
}
isReality <- function(a3, annPTdoc.sents){
  ans1 <- (! isFeaturePresent(cPOS.Adjective, a3, cPOS))
  # myDebug("TBD pastness certainty", ans1, "=expression test for", "no", cPOS.Adjective, "\n")
#  ans2 <- (! areSentencesSubjectivePolar(annPTdoc.sents))
  # ans <- (ans1 && ans2) # for now. eventually, ans2 might be chosen as superior over ans1
#  ans <- ans2
  ans <- ans1 # BEWARE coz pattern is Python module not installable in shinyapps.io
  return(ans)
}
isAct <- function(a3){
  ans1 <- (isFeaturePresent(cPOS.Verb, a3, cPOS) && isFeaturePresent(cPOS.NounProperSingular, a3, cPOS))
  myDebug(ans1, "=expression validity because", cPOS.Verb, cPOS.NounProperSingular, "\n")
#  ans2 <- isNERpresent(cNER.date, a3, theFeatures=cNER) # && isNERpresent(cNER.person, a3, theFeatures=cNER)
#  myDebug(ans2, "=expression validity because", cNER.date, "\n") # cNER.person failing for Indian names
#  if(! ans2){ myCat(myEnv=c.env.RmarkdownShiny,
#    "You did not promise a date (I believe) by when you will do that Action\n") } # else continue
#  ans <- ans1 && ans2
  # warning("TBD voice support, by future date")
  
  return(ans1) # BEWARE for now, instead of 'ans' coz date is committed surely and separately
}
dbreak.area <- function(txt){ # , loop=cLoops){ # override earlier fn
  ans <- FALSE
  # for(i in 1:loop){
#    myCat(myEnv=c.env.RmarkdownShiny,
#      "Life happens in career, relationships, hobbies, society, money, and health Areas.\n")
#    myCat(myEnv=c.env.RmarkdownShiny,
#      "What is an Area of your life that is not working as well as wanted?\n")
    g.dbreak.area <- myWLD(txt) # <<- getParas()
    ans <- isArea(g.dbreak.area)
    # if(ans){ break } else { myCat(myEnv=c.env.RmarkdownShiny,
    # ifelse(i < loop, "Try expressing again\n", ""); next }
  # }
  return(ans)
}
dbreak.reality <- function(txt){ # , loop=cLoops){ # override earlier fn
  ans <- FALSE
  # for(i in 1:loop){
#    myCat(myEnv=c.env.RmarkdownShiny,
#      "Related to chosen Area, what really happened out there, or did not happen?\n",
#      "What exists or does not exist in reality out there?\n",
#      "Express those without any subjectivity, opinions, assessments, inner thoughts, or inferences.\n")
    g.dbreak.reality <- myWLD(txt) # g.dbreak.reality <<- getParas()
    listAnnSents <- myAnnotate(g.dbreak.reality, TRUE) # was <<-
myDebug(length(listAnnSents)); myDebug(listAnnSents[[1]]); myDebug(listAnnSents[[2]])
    g.dbreak.reality.ann <- listAnnSents[[1]]
    g.dbreak.annPTdoc.sents <- listAnnSents[[2]]
    ans <- isReality(g.dbreak.reality.ann, # was: (! isFuture(g.dbreak.reality.ann)) for "pastness"
       g.dbreak.annPTdoc.sents) # g.dbreak.annPTdoc.sents updated temporarily by myAnnotate()
       # using g.dbreak.s loses the sentence boundaries that have already been identified; so, loses precise reporting
       # to user regarding which specific sentence or utterance is subjective or polar.
    # if(ans){ break } else { myCat(myEnv=c.env.RmarkdownShiny, ifelse(i < loop, "Try expressing again\n", "")); next }
  # }
myDebug(ans); myDebug(unlist(ans))
  ans <- all(unlist(ans))
myDebug(ans); myDebug(length(ans))
  return(ans) # whether all are TRUE (or NULL)
}
dbreak.defaultFuture <- function(txt){ # , loop=cLoops){ # override earlier fn
  ans <- FALSE
  # for(i in 1:loop){
    # ref: http://www.sesp.northwestern.edu/docs/publications/103212837744f8415902f1d.pdf
    #   "Emerging from the CAVE: Attributional Style and the Narrative Study of Identity in Midlife Adults"
    #   causal attribution coded along 3 dimensions: internal-external, global-specific, and stable-unstable
    #   Explanatory or Attributional Style can be Personal, Permanent, and Pervasive
    #   Contamination Sequences ~~ Life Events for self-defining life stories: these have greater impact
    #   These relate with role of narrative (identity for a Redemptive-Self life story, as per McAdam) in personality
    #   psychology
    g.dbreak.defaultFuture <- myWLD(txt) # <<- getParas()
    g.dbreak.defaultFuture.ann <- (myAnnotate(g.dbreak.defaultFuture, TRUE))[[1]] # was <<-
    ans <- isFuture(g.dbreak.defaultFuture.ann)
    # if(ans){ break } else { myCat(myEnv=c.env.RmarkdownShiny, ifelse(i < loop, "Try again\n", "")); next }
  # }
  return(ans)
}
dbreak.wantFuture <- function(txt){ # , loop=cLoops){ # override earlier fn
  return(dbreak.defaultFuture(txt))
}
dbreak.act <- function(userText){
  g.dbreak.action <- myWLD(userText)
  g.dbreak.action.ann <- (myAnnotate(g.dbreak.action, FALSE))[[1]] # FALSE is what's desired for NER annotations too
    # beware had <<- side effects
  ans <- isAct(g.dbreak.action.ann) # beware calls cat()
  return(ans)
}
