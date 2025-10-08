
about_you_ques <- p(

  # add any questions of interest using the standard Shiny widgets.
  # see details at https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
  
  strong("1. Do you specialise in any specific type(s) of cancer?"),
  em("Please select the option that best describes your professional focus."), br(),
  radioButtons("about_you_1"," ",c(
    "Yes – I specialise in one or more specific cancer types" = 1,
    "No – I am a generalist (e.g. general oncologist or primary care provider)" = 2), selected=)
  
  
  # strong("2. Please select the setting where you work from below:"),
  # checkboxGroupInput("about_you_2"," ",c(
  #   "Primary care" = 1,
  #   "Secondary care" = 2,
  #   "Social care" = 3,
  #   "Other (please specify below)" = 4), selected=integer(0)),
  # textInput("about_you_other_2","",value=""), br(), br(),
  
)


