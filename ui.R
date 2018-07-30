# module UI fn
mTextInput <- function(id){
  # id used is not User id whose namespace uniqueness is ensured via session parameter.
  ns <- NS(id) # (for this module) create a namespace fn using provided id (to suffix a namespace)
  tagList( # instead of fluidPage() coz this is a module fn
    # theme=cMyTheme,
    # column(width) must total 12 for each fluidRow in it; grid system relatively sizes %
    # column(width=6, offset=0, title=cUserArea,
    textAreaInput(inputId=ns(cIdTextIn), # input slot used to access the value.
      # NS() enables reuse of this module (UI and Server) fns for multiple text controls
      # by making these slot names unique to (and hence accessible for) each text-control instance.
      label=h4(id), # display label for the control. was: stepLabel
      value=cDefaultText), # initial value
    # ref: https://shiny.rstudio.com/articles/html-tags.html
    # tags$div(class = "header", id = if (FALSE) 100,
    # if (FALSE) "line 1",
    # "line 2"
    # )
    p(em(cStepPrompts[which(cStepLabel==id)])), # for emphasis (HTML5) tag
    switch(which(cStepLabel==id),
      # alt: shinyJS::show() / hide() but AGPL3 license where commercial use needs ok
      # was: uiOutput(ns("cIdCondUI"))
      # alt: conditionalPanel() from the Server fn could take any R expr (though better to avoid)
      # for which: outputOptions(output, [newOutputName], suspendWhenHidden = FALSE)
      hr(),
      hr(),
      checkboxInput(inputId=ns(cIdCheckboxInput), label=cLabelCheckbox, value=FALSE),
      hr(),
      dateInput(inputId=ns(cIdDateInput), label=cLabelDate,
        min=Sys.Date(), max=(Sys.Date()+cDateRange), format=cDateFormat)
    ),
    actionButton(ns(cIdButton), cActButtonLabel, icon(cActButtonIcon),
      # alt: submitButton() but that alters the behavior of all other inputs on the page
      # ref: https://stackoverflow.com/questions/33620133/change-the-color-of-action-button-in-shiny?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
      style=cActButtonStyle
    ),

    # column(width=6, offset=0, title=cCoachArea,
    fluidRow(column(3, textOutput(ns(cIdTextOut))))
    # was: verbatimTextOutput() that showed unwrapped text with scrollbar unprintably
  )
  # return last expression: tagList which defines the UI
}

# define UI "skeleton" or facade in a way that also allows User to print (scroll) screen or
# print from browser the conversation
uiFluidPage <- fluidPage( # theme=cMyTheme,
  titlePanel(cTitle),
  p(paste("Dear ", g.dbreak.name, ": now the time is ", format(Sys.Date(), "%d-%b-%Y"), ".\n", sep="")),
  p(cTitlePrompt),
  tags$a(href=cURLprompt,
    paste0("To know more of this coaching (e.g., step ", cStepLabel[6], "), click here!")),
  hr(),

  # keep outside ns() as common UI and Server without repeating per module for each step
  checkboxInput(inputId=cIdAllStepsCheckboxInput, label=cLabelAllStepsCheckbox, value=FALSE),
  # fluidRow(column(3, textOutput(cIdAllStepsTextOut))),
  conditionalPanel( # alt: shinyJS::show() / hide() but AGPL3 license where commercial use needs ok
    # ns=NS(NULL) coz not module so default namespace ("global" across R-app's sessions)
    # The JS expression is evaluated once at startup and whenever Shiny detects a relevant change
    # in input/output. This is unlike the usual UI controls where only Server reactives and
    # observers are re-called
    condition=paste("input.", cIdAllStepsCheckboxInput, " == true", sep=""), # JS expr
    lapply(c(1:3),
      function(i){
        mTextInput(id=cStepLabel[i])
      }
    )
  ),

  lapply(cStepNumsInit, # alt: cStepNumsAll. lapply() returns a list of same length as X
    function(i){
      mTextInput(id=cStepLabel[i])
    }
  )
  # return last expr: UI definition
)

uiFluidPage # launched once per-app (-R-process) launch and (via HTML cache) served to
# each User session
