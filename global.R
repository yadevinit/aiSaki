# as per http://shiny.rstudio.com/articles/unicode.html, shinyapps.io expects source files
# and data in UTF-8 encoding; so (re)save with UTF-8 encoding.
# also avoid '.' in names, e.g., of variables, as it might conflict with HTML div commands

# to run app locally:
# library(shiny)
# setwd('f://aisaki')
# runApp(display.mode="showcase") # showcase to show code!

# to deploy app online on shinyapps.io:
# library(rsconnect) # was deprecated: library(shinyapps)
# deployApp()
# rsconnect::showLogs()
# had to > install.packages("digest", "rcmdcheck") # for resolving install_github("devtools", "hadley")
# had to > devtools::install_github("rstudio/rstudioapi") # coz deployApp() was failing

# ref: https://shiny.rstudio.com/articles/modules.html
# If you're using functions to generate UI, and those functions generate inputs and outputs,
# then you need to ensure that none of the IDs collide. So, namespaces via NS().
# Namespace for the module is decided by the caller at the time the module is used,
# not decided by the author at the time the module is written.
# If a module needs to access an input that isn't part of the module, the containing app
# should pass the input value wrapped in a reactive expression (i.e. reactive(...) ):
# callModule(myModule, "myModule1", reactive(input$checkbox1))

# ref: https://shiny.rstudio.com/articles/scoping.html
# Where you define objects will determine where the objects are visible. There are three different
# levels of visibility to be aware of ...
# Some objects are visible within the server code of each user session; other objects are visible
# in the server code across all sessions (multiple users could use a shared variable); and yet
# others are visible in the server and the ui code across all user sessions.
# ... One R process can support multiple Shiny
# sessions. Some hosting platforms (including RStudio Connect, Shiny Server Pro, and shinyapps.io)
# also allow running multiple R processes to handle heavier traffic. Within each R process, the
# scoping works as explained below, but between the R processes, no objects are shared. ...
# ? analogy for this app: R (-process) restaurant, session User/customer, UI waiter, server cook.
# Server function is called once for each session param (when browser connects); its vars are
# created and unique per session. UI fn is created per app.
# Non-reactive (not involving input/output, ie, pure) fns and read-only data vars are visible
# across sessions ("global" objects relatively); <<- has to be used if side effecting. Visible to
# only Server fn if defined outside global.R. Visible to Server and UI fns if defined in global.R.
# code in ui is run once, when the Shiny app is started and it generates an HTML file which is
# cached and sent to each web browser that connects.
# source(local = TRUE) to receive same scope as if you had inline code. local=FALSE is sourced in
# global(.R) env. Efficiency?
# Whenever a reactive value changes, any reactive expressions that depended on it are marked as
# "invalidated" and will automatically re-execute if necessary
# To store expressions for later conversion to reactive, use quote(): lazy/delayed eval
# Within isolate(), use local() to avoid affecting calling environment
# The expression given to isolate() is evaluated in the calling environment
# input$id in server logic can determine which of the current tabs is active

# ref: https://shiny.rstudio.com/articles/understanding-reactivity.html gives 4 reactivity maxims:
# 1. R expressions update themselves, if you ask (coz R uses lazy evaluation).
# 2. Nothing needs to happen instantly.
# 3. The app must do as little as possible:
#   input$a is a reactive values object and print(input$a) is an observer. These two classes
#   behave like regular R values and R expressions with a few exceptions. Whenever an observer
#   uses a reactive value, it registers a reactive context (a callback expr to be run upon change)
#   with the value.
# 4. Think carrier pigeons, not electricity:
#   A context is like a virtual carrier pigeon (carrying a callback msg) that an observer leaves
#   with a reactive value. When it's released, callback msg is delivered to server.
#   In fact, a reactive values object will return an error if an expression tries to access
#   its value without leaving behind a context.
#   If the reactive value object ever changes, it will release all of the contexts ("pigeons")
#   it has collected (a process known as *invalidating* the contexts). When a context is
#   invalidated, it places its callback in the server's queue to be run on the next flush.
#   Then the context ceases to be relevant, just like a pigeon that has delivered its
#   message.

library("shiny")
library("reticulate") # this was logged as a missing error by shinyapps.io. R-Python interface
g.dbreak.name <- "Leader"
cMyTheme <- "bootstrap-337cerulean.css"
# default: drop theme. alt: other CSSs at ./www/
# got from https://bootswatch.com/3/ for Bootstrap-3 compatibility
cCoach <- "aiSaki"
cTitle <- paste0("Declare a Breakdown with coach ", cCoach, ", and create a future that matters")
cTitlePrompt <- paste("Consider that every human Being (or group) has the Power to create a Future right now.\n",
    "The (human) Being can choose to do one missing (conversation) Action for it.\n",
    "For that, the Being must be aware of (i.e., present to) that Action.\n",
    "That is what this coach aiSaki (presently, in beta version) causes.\n", # BEWARE
    "(In the following steps, use complete sentences\n", # and capitals\n",
    "since aiSaki does not have expert comprehension yet,\n", # BEWARE
    # "  and type ENTER then CTRL-Z to end input on MS-Windows.)\n",
    " and click the aiSaki button to end input and get its response for that step.)\n", # BEWARE
    "You are welcome to use an app of your choice to convert speech to English text and vice versa; that would be a voice interface.\n",
    "If you wish to report a problem (with supporting .pdf, .xps, or screenshot saved from Internet-browser\n",
    "session) or contribute, please contact the app author at yadevinit@gmail.com\n",
    sep="")
    # "If you wish to know more of this coaching, refer to:", sep="")
cURLprompt <- "http://www.sameerdua.com/declaring-breakdowns/"
cDateFormat <- "yyyy-M-dd"; cDateRange <- 30*2 # allowable gap before User completes Action
cStepLabel <- c("#1 Area of Breakdown", "#2 What is So?", "#3 Default Future",
  "#4 New Future", "#5 Missing Action", "#6 Execution") # process steps in Declaring Breakdowns
cStepPrompts <- c(
  paste("Life happens in career, relationships, hobbies, society, money, and health Areas.\n",
    "What is an Area of your life that is not working as well as wanted?\n"),
  paste("Related to chosen Area, what really happened out there (or did not happen)?\n",
      "What exists (or does not exist) in reality out there?\n",
      "Express those that exist(ed) without any subjectivity, opinions, assessments, inner thoughts, or inferences.\n"),
  paste("In that Area if no new Action is taken, what is the Default Future you head into?\n",
    "What is the Default Future that is going to happen unless something dramatic and unexpected happens?\n",
    "What will it be like for you and others, near future and long term, and in this and all areas of your life?\n"),
  "So, what is the Future wanted right now, for yourself or others?\n",
  "For causing that Future, what is the (missing-conversation) Action and with whom? By when will you do it?\n")
# cStepNum <- paste("step", cStepNumsAll, sep="")
cStepOkResponse <- "OK. Please go to next step.\n"
cStepNotOkResponse <- "That does not seem OK. Please retry, or turn on all-steps checkbox at the top of this conversation.\n"
cStepNumsAll <- c(1:(length(cStepLabel)-1))
cStepNumsInit <- c(4, 5)

# cUserArea <- "User"; cCoachArea <- "Coach"
# cIdNavbar <- "cIdNavbar"
# cDefaultLabel <- "my default label"
cIdTextIn <- "cIdTextIn"
cIdTextOut <- "cIdTextOut"
cIdButton <- "cIdButton"
cIdAllStepsCheckboxInput <- "cIdAllStepsCheckboxInput"
cIdAllStepsTextOut <- "cIdAllStepsTextOut"
cLabelAllStepsCheckbox <- paste0("Do you want to use all the steps (5 + step ", cStepLabel[6], ")?")
cDefaultText <- "Enter text here for this step" # "Enter text here to say what I will do today"
cIdCheckboxInput <- "cIdCheckboxInput"
cLabelCheckbox <- "Are you ok with this Future?"
cIdDateInput <- "cIdDateInput"
cLabelDate <- "By when will you complete Action?"
cActButtonLabel <- "aiSaki turn"
cActButtonIcon <- "paper-plane" # https://fontawesome.com/icons?d=gallery
cActButtonStyle <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"

# beware: scope would be "global.R" env/namespace
