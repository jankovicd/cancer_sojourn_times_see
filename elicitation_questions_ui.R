
f_chips_and_bins_1a <- function(cancer_type_now,
                                elici_minis,
                                elici_maxis,
                                chips_nchip,
                                chips_chips,
                                chips_lbins,
                                chips_rbins,
                                show_plot,
                                enter_plot,
                                comment
                                #condition
                                )

(p(
  strong(paste0("Q1a: What is the overall mean sojourn time (OMST) of clinically detected ", cancer_type_labels[cancer_type_now], "s in England?")), br(), br(),
  p("I believe that it's very unlikely that:"),
  tags$li(div(style = "display: inline-block; vertical-align:middle; width: 210px;",HTML(paste0("OMST is less than"))),
          div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("min_1a_",cancer_type_now), NULL, elici_minis, min = 0, max = NA)),
          div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
          div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML("years,"))),
  tags$li(div(style = "display: inline-block; vertical-align:middle; width: 210px;",HTML(paste0("OMST is greater than"))),
          div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("max_1a_",cancer_type_now), NULL, elici_maxis, min = 0, max = NA)),
          div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
          div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML("years."))),
  br(),br(),
  ifelse(show_plot == 0,
         tagList(div(
           fluidRow(
             column(9, p(style="font-size:90%;", "When you are happy with your answers please click on 'Continue'.")),
             column(1,
                    actionButton(paste0("show_plot_1a_", cancer_type_now), "Continue", width = '120px', style = "background-color: lightgrey")), br()
           )
           )),
         tagList(div(
           fluidRow(
             column(9, p(style = "font-size:90%;", "You can update your range by entering new values and clicking 'Update range'.")),
             column(1,
                    actionButton(paste0("show_plot_1a_", cancer_type_now), "Update range", width = '120px', style="background-color: lightgrey"))
             ),
           hr(),br(),
           p("Please add", chips_nchip,"chips to the grid below to express your uncertainty.
      The more chips you place in a particular bin the more certain you are
      that the OMST lies in that bin."),br(), br(),
           "You can use",strong(round(chips_nchip-sum(chips_chips),digits=0)), " more chips.",
           HTML("<div style='height: 350px;'>"),
           plotOutput(paste0("plot_1a_",cancer_type_now), click=paste0("location_1a_",cancer_type_now)),
           HTML("</div>"), br(),
           ifelse(enter_plot == 0,#****
                  tagList(div(
                    fluidRow(
                      column(9, "When you are happy with your answers please click on 'Enter', then scroll down."),
                      column(2, actionButton(paste0("enter_plot_1a_", cancer_type_now), "Enter", width='120px', style="background-color: lightgrey")
                      )), br(), br(),
                    )),
                  tagList(div(
                    hr(), br(),
                    strong("Summary"),br(),br(),
                    "Your answers imply that",br(),br(),
                    div(f_text_fback_chips(chips_chips, chips_lbins, chips_rbins),
                        style='width:700px; padding-left:45px;'), br(),br(),
                    "If these summary statements do not represent your beliefs you can modify the grid.",br(),
                    hr(),
                    p("Please provide rationale for your answers here, and state any additional comments about your answer.",
                      withTags(div(
                        textAreaInput(inputId = paste0("comment_1a_", cancer_type_now),
                                      label = "",
                                      value = comment,
                                      rows=2,
                                      width='800px')
                          )), br(), br(),
                     fluidRow(
                        column(9, p(style="font-size:90%;", "Once you are satisfied that the statements represent your beliefs, click on 'Save', then click on the next tab in the side panel to continue.")),
                        column(1, actionButton(paste0("next_que_1a_", cancer_type_now), "Save", width='120px', style="background-color: lightgrey"))
                        ), br()), br(),
                      "Note that you can edit your answer at any point, but remember to save your new inputs.", br(), br(), br()
                  ))
           )

           ))


         )


)

)

f_chips_and_bins_1b <- function(cancer_type_now,
                                elici_minis,
                                elici_maxis,
                                chips_nchip,
                                chips_chips,
                                chips_lbins,
                                chips_rbins,
                                show_plot,
                                enter_plot,
                                comment
                                #condition
                                )
  
  (p(
    br(), br(),
    strong(paste0("Q1b: What is the overall mean sojourn time (OMST) of ", cancer_type_labels[cancer_type_now], "s in England?")), br(),
    em("This includes clinically and screen-detected cancers."), br(), br(),
    p("I believe that it's very unlikely that:"),
    tags$li(div(style = "display: inline-block; vertical-align:middle; width: 210px;",HTML(paste0("OMST is less than"))),
            div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("min_1b_",cancer_type_now), NULL, elici_minis, min = 0, max = NA)),
            div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
            div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML("years,"))),
    tags$li(div(style = "display: inline-block; vertical-align:middle; width: 210px;",HTML(paste0("OMST is greater than"))),
            div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("max_1b_",cancer_type_now), NULL, elici_maxis, min = 0, max = NA)),
            div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
            div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML("years."))),
    br(),br(),
    ifelse(show_plot == 0,
           tagList(div(
             fluidRow(
               column(9, p(style="font-size:90%;", "When you are happy with your answers please click on 'Continue'.")),
               column(1,
                      actionButton(paste0("show_plot_1b_", cancer_type_now), "Continue", width = '120px', style = "background-color: lightgrey")), br()
             )
           )),
           tagList(div(
             fluidRow(
               column(9, p(style = "font-size:90%;", "You can update your range by entering new values and clicking 'Update range'.")),
               column(1,
                      actionButton(paste0("show_plot_1b_", cancer_type_now), "Update range", width = '120px', style="background-color: lightgrey"))
             ),
             hr(),br(),
             p("Please add", chips_nchip,"chips to the grid below to express your uncertainty.
      The more chips you place in a particular bin the more certain you are
      that the OMST lies in that bin."),br(), br(),
             "You can use",strong(round(chips_nchip-sum(chips_chips),digits=0)), " more chips.",
             HTML("<div style='height: 350px;'>"),
             plotOutput(paste0("plot_1b_",cancer_type_now), click=paste0("location_1b_",cancer_type_now)),
             HTML("</div>"), br(),
             ifelse(enter_plot == 0,#****
                    tagList(div(
                      fluidRow(
                        column(9, "When you are happy with your answers please click on 'Enter', then scroll down."),
                        column(2, actionButton(paste0("enter_plot_1b_", cancer_type_now), "Enter", width='120px', style="background-color: lightgrey")
                        )), br(), br(),
                    )),
                    tagList(div(
                      hr(), br(),
                      strong("Summary"),br(),br(),
                      "Your answers imply that",br(),br(),
                      div(f_text_fback_chips(chips_chips, chips_lbins, chips_rbins),
                          style='width:700px; padding-left:45px;'), br(),br(),
                      "If these summary statements do not represent your beliefs you can modify the grid.",br(),
                      hr(),
                      p("Please provide rationale for your answers here, and state any additional comments about your answer.",
                        withTags(div(
                          textAreaInput(inputId = paste0("comment_1b_", cancer_type_now),
                                        label = "",
                                        value = comment,
                                        rows=2,
                                        width='800px')
                        )), br(), br(),
                        fluidRow(
                          column(9, p(style="font-size:90%;", "Once you are satisfied that the statements represent your beliefs, click on 'Save', then click on the next tab in the side panel to continue.")),
                          column(1, actionButton(paste0("next_que_1b_", cancer_type_now), "Save", width='120px', style="background-color: lightgrey"))
                          ), br()), br(),
                      "Note that you can edit your answer at any point, but remember to save your new inputs.", br(), br(), br()
                    ))
             )
             
           ))
           
           
    )
    
    
  )
  
  )

f_chips_and_bins_3c <- function(cancer_type_now,
                                elici_minis,
                                elici_maxis,
                                chips_nchip,
                                chips_chips,
                                chips_lbins,
                                chips_rbins,
                                show_plot,
                                enter_plot,
                                comment,
                                #condition,
                                mode_omst)
  
  (p(
    br(), br(),
    strong(paste0("If OMST for all ", cancer_type_labels[cancer_type_now], "s (ctDNA and non-ctDNA) is ", mode_omst, " years, what do you believe is the OMST of ctDNA ", cancer_type_labels[cancer_type_now], "s?")), br(),br(),
    p("I believe that it's very unlikely that:"),
    tags$li(div(style = "display: inline-block; vertical-align:middle; width: 210px;",HTML(paste0("OMST is less than"))),
            div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("min_3c_",cancer_type_now), NULL, elici_minis, min = 0, max = NA)),
            div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
            div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML("years,"))),
    tags$li(div(style = "display: inline-block; vertical-align:middle; width: 210px;",HTML(paste0("OMST is greater than"))),
            div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("max_3c_",cancer_type_now), NULL, elici_maxis, min = 0, max = NA)),
            div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
            div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML("years."))),
    br(),br(),
    ifelse(show_plot == 0,
           tagList(div(
             fluidRow(
               column(9, p(style="font-size:90%;", "When you are happy with your answers please click on 'Continue'.")),
               column(1,
                      actionButton(paste0("show_plot_3c_", cancer_type_now), "Continue", width = '120px', style = "background-color: lightgrey")), br()
             )
           )),
           tagList(div(
             fluidRow(
               column(9, p(style = "font-size:90%;", "You can update your range by entering new values and clicking 'Update range'.")),
               column(1,
                      actionButton(paste0("show_plot_3c_", cancer_type_now), "Update range", width = '120px', style="background-color: lightgrey"))
             ),
             hr(),br(),
             p("Please add", chips_nchip,"chips to the grid below to express your uncertainty.
      The more chips you place in a particular bin the more certain you are
      that the OMST lies in that bin."),br(), br(),
             "You can use",strong(round(chips_nchip-sum(chips_chips),digits=0)), " more chips.",
             HTML("<div style='height: 350px;'>"),
             plotOutput(paste0("plot_3c_",cancer_type_now), click=paste0("location_3c_",cancer_type_now)),
             HTML("</div>"), br(),
             ifelse(enter_plot == 0,#****
                    tagList(div(
                      fluidRow(
                        column(9, "When you are happy with your answers please click on 'Enter', then scroll down."),
                        column(2, actionButton(paste0("enter_plot_3c_", cancer_type_now), "Enter", width='120px', style="background-color: lightgrey")
                        )), br(), br(),
                    )),
                    tagList(div(
                      hr(), br(),
                      strong("Summary"),br(),br(),
                      "Your answers imply that",br(),br(),
                      div(f_text_fback_chips(chips_chips, chips_lbins, chips_rbins),
                          style='width:700px; padding-left:45px;'), br(),br(),
                      "If these summary statements do not represent your beliefs you can modify the grid.",br(),
                      hr(),
                      p("Please provide rationale for your answers here, and state any additional comments about your answer.",
                        withTags(div(
                          textAreaInput(inputId = paste0("comment_3c_", cancer_type_now),
                                        label = "",
                                        value = comment,
                                        rows=2,
                                        width='800px')
                        )), br(), br(),
                        fluidRow(
                          column(9, p(style="font-size:90%;", "Once you are satisfied that the statements represent your beliefs, click on 'Save', then click on the next cancer type to continue.")),
                          column(1, actionButton(paste0("next_que_3c_", cancer_type_now), "Save", width='120px', style="background-color: lightgrey"))
                        ), br()), br(),
                      "Note that you can edit your answer at any point, but remember to save your new inputs.", br(), br(), br()
                    ))
             )
             
           ))
           
           
    )
    
    
  )
  
  )

f_chips_and_bins_test <- function(elici_minis,
                                  elici_maxis,
                                  chips_nchip,
                                  chips_chips,
                                  chips_lbins,
                                  chips_rbins,
                                  show_plot,
                                  enter_plot)

(p(
  strong("Practice question: What is the average number of rainy days in York during October?"), br(), br(),
  p("I believe that it's very unlikely that:"),
  tags$li(div(style = "display: inline-block; vertical-align:middle; width: 250px;",HTML(paste0("the number of days is less than"))),
          div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput("min_test_1", NULL, elici_minis, min = 0, max = 31)),
          div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
          div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML(","))),
  tags$li(div(style = "display: inline-block; vertical-align:middle; width: 250px;",HTML(paste0("the number of days is greater than"))),
          div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput("max_test_1", NULL, elici_maxis, min = 0, max = NA)),
          div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
          div(style = "display: inline-block; vertical-align:middle; width: 120px;", HTML("."))),
  br(),br(),
  ifelse(show_plot == 0,
         tagList(div(
           fluidRow(
             column(9, p(style="font-size:90%;", "When you are happy with your answers please click on 'Continue'.")),
             column(1,
                    actionButton("show_plot_test_1", "Continue", width = '120px', style = "background-color: lightgrey")), br()
           )
         )),
         tagList(div(
           fluidRow(
             column(9, p(style = "font-size:90%;", "You can update your range by entering new values and clicking 'Update range'.")),
             column(1,
                    actionButton("show_plot_test_1", "Update range", width = '120px', style="background-color: lightgrey"))
           ),
           hr(),br(),
           p("Please add", chips_nchip,"chips to the grid below to express your uncertainty.
      The more chips you place in a particular bin the more certain you are
      that the OMST lies in that bin."),br(), br(),
           "You can use",strong(round(chips_nchip-sum(chips_chips),digits=0)), " more chips.",
           HTML("<div style='height: 350px;'>"),
           plotOutput("plot_test_1", click="location_test_1"),
           HTML("</div>"), br(),
           ifelse(enter_plot == 0,#****
                  tagList(div(
                    fluidRow(
                      column(9, "When you are happy with your answers please click on 'Enter', then scroll down."),
                      column(2, actionButton("enter_plot_test_1", "Enter", width='120px', style="background-color: lightgrey")
                      )), br(), br(),
                  )),
                  tagList(div(
                    hr(), br(),
                    strong("Summary"),br(),br(),
                    "Your answers imply that",br(),br(),
                    div(f_text_fback_chips(chips_chips, chips_lbins, chips_rbins),
                        style='width:700px; padding-left:45px;'), br(),br(),
                    "If these summary statements do not represent your beliefs you can modify the grid.",br(),
                    hr(), br(),
                    fluidRow(
                        column(9, p(style="font-size:90%;", "Once you are satisfied that the statements represent your beliefs, click on 'Next' to continue.")),
                        column(1, actionButton("next_que_test_1", "next", width='120px', style="background-color: lightgrey"))
                      ), br(), br(), br()
                  ))
           )
           
         ))
         
         
  )
  
  
)

)

f_section_3_quesions <- function(cancer_type_now)
  
  p(
    strong(paste0("Which of the following cancers (that you have already expressed beliefs about) do you believe ",
                  cancer_type_labels[cancer_type_now], " is likely to have the most similar overall mean sojourn time?")), br(),
    em("Note that we are interested in all bladder cancers (ctDNA and non-ctDNA) in England. You can select more than one cancer type."), br(), br(),
    checkboxGroupInput(paste0("section_3_", cancer_type_now)," ",c(
      "Breast" = 1,
      "Lung/Bronchus" = 2,
      "Colon/Rectum" = 3,
      "Ovary" = 4,
      "Prostate" = 5,
      "Liver" = 6,
      "Cervix" = 7,
      "Head and neck" = 8,
      "Lymphoma" = 9,
      "Oesophagus" = 10,
      "Pancreas" = 11), selected = paste0("elici_section_3_",cancer_type_now)), br(), br(),
    fluidRow(
      column(9, p(style="font-size:90%;", "Click on 'Next' to continue.")),
      column(1, actionButton(paste0("next_que_section_3_", cancer_type_now), "Next", width='120px', style="background-color: lightgrey"))
    ), br(), br()
    
  )
