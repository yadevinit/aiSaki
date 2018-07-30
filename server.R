source("server1.R", local=TRUE) # beware of scope and extent of vars

booleanToTextResponse <- function(ok){
  return(ifelse(ok, cStepOkResponse, cStepNotOkResponse))
}

# module Server fn
# The input, output, and session objects we're provided with are special, in that they are scoped
# to the specific namespace that matches up with our (name-convention) UI fn
mText <- function(input, output, session,
  stepLabel){ # called per stepLabel
  output$cIdTextOut <- renderPrint({ # as side effect, data is sent to browser (User session)
    tries <- {input$cIdButton} # get enclosing block {} to depend on this reactive expr
    isolate({ # execute following code {block} without taking dependency on reactives within
      if((tries > 0) && (tries <= cLoops)){ # deal with tries
        # and skip running the following code at init when actionButton start value is 0
        txt <- {input$cIdTextIn} # {} isn't essential, but let's treat reactives with respect
        # ignoreNULL=TRUE
        # myWLD() and myAnnotate() not extracted as common code before step-specific code coz differ per step
        if((nchar(txt) > 0)){ # was: && isLangOk(txt) but that's not for steps such as Area of Breakdown
          ans <- switch(which(cStepLabel==stepLabel),
            {okArea <- dbreak.area(txt); booleanToTextResponse(okArea)},
            {# listAnnSents <- myAnnotate(myWLD(txt), TRUE); print(listAnnSents); print(length(listAnnSents))
              # g.dbreak.reality.ann <- listAnnSents[[1]]; g.dbreak.annPTdoc.sents <- listAnnSents[[2]]
              # myCat(myEnv=c.env.RmarkdownShiny,
              #   "Please say what really happened or existed out there that makes you say this (what I believe is) opinion:\n",
              #   patt.senti$id, "\n")

              chk <- dbreak.reality(txt); chktxt <- booleanToTextResponse(chk)
              ifelse(chk, chktxt,
                paste(
                  paste0("Please say what really happened or existed out there that makes you say this (what ",
                    cCoach, " believes is) opinion.\n "),
                  chktxt))},
            {chkbox <- {input$cIdCheckboxInput}; chk <- dbreak.defaultFuture(txt); chktxt <- booleanToTextResponse(chk)
              paste(chktxt,
                ifelse(chkbox && chk, "You said you are ok with the Default Future. You are now re-assured",
                  ""))},
            {booleanToTextResponse(okfut <- dbreak.wantFuture(txt))},
            {dt <- {input$cIdDateInput}; chk <- dbreak.act(txt); chktxt <- booleanToTextResponse(chk)
              paste0("You declared an Action (by ", dt, "). ", chktxt,
                ifelse(chk,
                  paste("You are aware of a Future you want and Action for it. Go ahead with ", cStepLabel[6]),
                    ""))}
          )
        } else {
          ans <- "did not understand your text. Do clarify the text and retry" # or empty text
        }
        ans <- paste0(ans, ". (You can retry ", cLoops - tries, " times more.)") # return last evaluated expr
      } else {
        ans <- ifelse(tries > cLoops,
          "You retried often. Please turn on the all-steps checkbox at the top of this conversation", # if chkbox yes, then insertUI()
          # else tries <= 0
          "After entering text and any selecting, click the button for coach aiSaki's response at this step")
      } # dealt with tries
      print(ans)
    }) # exit isolate()
  }) # return last evaluated expr. exit renderPrint()
  return()
}

#  mStepSpecificServletAtSessionInit(input, output, session, stepLabel)
  # now isolate reactions related to the text box. observeEvent() returns an
  # observer reference class object.
  # eventReactive() returns a reactive-expr object (to encapsulate once and share across observers
  # and other reactive expr objects)
#  myMsg <- eventReactive(eventExpr=input$cIdButton,
    # (quoted/unquoted) expr representing event can even be complex expression inside {}
#    valueExpr={input$cIdTextIn})
#  observeEvent(myMsg(), handlerExpr={
#    renderPrint(print(input$cIdCheckboxInput))
#    mStepSpecificServletAfterSessionInit(input, output, session, stepLabel) #, myMsg)
#      list({input$cIdTextIn}, {input$cIdCheckboxInput}, {input$cIdDateInput})
#    } # expr producing calculated return value of eventReactive that
    # updates on event
    # in this eventReactive(), there's no update (using calculated value upon event)
    # could consider annotating TBD
#  )
#  eventReactive(input$cIdButton, # within corresponding module for step
#    mStepSpecificServletAfterSessionInit(input, output, session, stepLabel, myMsg)
#  )

serverM <- function(input, output, session){ # called once per session to initiate its connection
  # with browser (User) session

  # set up Server for other (unconditional) steps
  sessionServletsModules <- lapply(cStepNumsInit, # cStepNumsAll, # returns a list of same length as X
    function(i){
      callModule(mText, # module Server fn
        id=cStepLabel[i], # considering UI module fn calls mTextInput(id, ...)
        stepLabel=cStepLabel[i] # additional parameters
      )
    }
  )
  # Unlike the module UI fn, the module server fn is not called directly, but via callModule()
  # The callModule fn is responsible for creating the namespaced input, output, and session arguments

  # set up Server for conditional-UI steps
#  output$cIdAllStepsTextOut <- renderPrint({
#    if({input$cIdAllStepsCheckboxInput}){ # register following code's dependency on this reactive expr
  observeEvent(eventExpr=({input$cIdAllStepsCheckboxInput}==TRUE), handlerExpr={
#      isolate({
        cLoops <<- cLoops + 3
          # reset this "global" for this Server but shared across sessions and step modules??
        moreServlets <- lapply(c(1:3), # returns a list of same length as X
          function(i){
            callModule(mText, # module Server fn
              id=cStepLabel[i], # considering UI module fn calls mTextInput(id, ...)
              stepLabel=cStepLabel[i] # additional parameters
            )
            # output$cIdAllStepsTextOut <- renderPrint({paste("done callModule ", i)})
          }
        )
        # "" # done callModule123. now disable this checkbox apparatus")
#      }) # exit isolate()
    }, ignoreNULL=TRUE, suspended=FALSE, autoDestroy=TRUE, ignoreInit=FALSE, once=FALSE
    # suspended=TRUE starts the observer in a suspended state
  )
#    } else { # not all steps; only brief steps
#      "" # disable exec upon init")
#    }
#  }) # end renderPrint()

#  return(sessionServletsModules) # which is also the returned last expr
  return()
}

serverM # return the defined called-per-session-init Server fn
