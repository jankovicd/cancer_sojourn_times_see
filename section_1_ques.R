
f_section_1_2_questions <- function(cancer_type_now,
                                    cancer_name,
                                    section_no,
                                    buttons_prognosis,
                                    buttons_omst_graph,
                                    buttons_next_que_1a,
                                    buttons_next_que_1b,
                                    buttons_next_que_1c,
                                    buttons_next_que_2a,
                                    buttons_next_que_2b,
                                    buttons_next_que_3a,
                                    buttons_next_que_3b,
                                    buttons_next_que_3c,
                                    buttons_cancer_types,
                                    buttons_view_ctDNA_evidence,
                                    buttons_validate_que_2_b,
                                    elici_minis_1a,
                                    elici_minis_1b,
                                    elici_minis_3c,
                                    elici_maxis_1a,
                                    elici_maxis_1b,
                                    elici_maxis_3c,
                                    chips_nchip_1a,
                                    chips_nchip_1b,
                                    chips_nchip_3c,
                                    chips_chips_1a,
                                    chips_chips_1b,
                                    chips_chips_3c,
                                    chips_lbins_1a,
                                    chips_lbins_1b,
                                    chips_lbins_3c,
                                    chips_rbins_1a,
                                    chips_rbins_1b,
                                    chips_rbins_3c,
                                    show_plot_1a,
                                    show_plot_1b,
                                    show_plot_3c,
                                    enter_plot_1a,
                                    enter_plot_1b,
                                    enter_plot_3c,
                                    comments_1a,
                                    comments_1b,
                                    comments_3c,
                                    elici_1c,
                                    elici_2a,
                                    elici_2b,
                                    elici_3a,
                                    elici_3b,
                                    mode_omst_all,
                                    omst_late,
                                    lmst,
                                    omst_ctDNA)

  (
  
  p(
  
  # add links to each bit of evidence and question
  
  if(buttons_prognosis == 0){
    
    tagList(div(
      
      fluidRow(
        column(6,
               tagList(div(
                 strong("Five-year survival (bar, left-side axis) and number of cases between 2015 and 2019 (line, right-side axis)"), br(),
                 em("Cancer types ordered by largest to smallest survival."), br(), br(), br(),
                 tags$img(src = paste0("5_year_survival_incidence_", cancer_types[cancer_type_now],".jpeg"), width = "97%", height = "97%")
                 #img(src=paste0("5_year_survival_incidence_", cancer_types[cancer_type_now],".jpeg"), height="97%", width="97%", align = "left")
                 ))),
        column(6,
               tagList(div(
                 strong("Breakdown of early vs. late stage at diagnosis"), br(),
                 em("Cancer types ordered from largest to smallest proportion in early-stage at diagnosis."), br(), br(), br(), br(),
                 tags$img(src = paste0("stages_at_diagnosis_", cancer_types[cancer_type_now],"_early_late.jpeg"), width = "97%", height = "97%")
                 #img(src=paste0("stages_at_diagnosis_", cancer_types[cancer_type_now],"_early_late.jpeg"), height="97%", width="97%", align = "left"), br()
                 )),
               style = 'border-left: 1px')
        ),
      br(), br(),
      hr(), br(),
      fluidRow(
        column(9, p(style = "font-size:90%;", "Please click on 'Next' to continue.")),
        column(1,
               actionButton(paste0("prognosis_", cancer_type_now), "Next", width = '120px', style="background-color: lightgrey"))
      ), br(), br()
      
    ))
    
  } else if(buttons_omst_graph == 0){
    
    tagList(div(strong("OMSTs reported in the literature"), br(),
                em("Studies identified in a literature review of mathematical estimates 
                 derived from the analyses of primary data either from clinical trials of 
                 screening interventions or from routine data from existing screening programmes 
                 (Geurts et al., 2022) that we have updated to 2025."), br(), br(), hr(), br(),
                strong("Summary of datasets"), br(), br(),
                tags$img(src = paste0('datasets_', ifelse (cancer_type_now < 7, cancer_types[cancer_type_now], "all"), ".png"), width = "85%", height = "85%"), br(), br(), hr(), br(),
                #img(src=paste0('datasets_', ifelse (cancer_type_now < 7, cancer_types[cancer_type_now], "all"), ".png"), height="87%", width="87%", align = "left"), br(), br(), br(), br(), br(),
                strong("Summary of OMST estimates"), br(), br(),
                tags$img(src = paste0('all_omsts_', ifelse (cancer_type_now < 7, cancer_types[cancer_type_now], ""), ".png"), width = "100%", height = "100%"), br(), br(), br(),br(),
                #img(src=paste0('all_omsts_', ifelse (cancer_type_now < 7, cancer_types[cancer_type_now], ""), ".png"), height="100%", width="100%", align = "left"), br(), br(), br(),
                fluidRow(
                  column(9, p(style = "font-size:90%;", "Please click on 'Next' to continue to the first question.")),
                  column(1,
                         actionButton(paste0("omst_graph_", cancer_type_now), "Next", width = '120px', style="background-color: lightgrey"))
                ), br(), br(), br()
    ))
    
  } else if(buttons_next_que_1a == 0){
    
    tagList(div(
      f_chips_and_bins_1a(cancer_type_now,
                          elici_minis_1a,
                          elici_maxis_1a,
                          chips_nchip_1a,
                          chips_chips_1a,
                          chips_lbins_1a,
                          chips_rbins_1a,
                          show_plot_1a,
                          enter_plot_1a,
                          comments_1a)
    ))
    
  } else if(cancer_type_now %in% cancer_types_with_screening_programmes & buttons_next_que_1b == 0) {
    
    #que_name <- cancer_types[cancer_type_now]
    
    tagList(div(
      f_chips_and_bins_1b(cancer_type_now,
                          elici_minis_1b,
                          elici_maxis_1b,
                          chips_nchip_1b,
                          chips_chips_1b,
                          chips_lbins_1b,
                          chips_rbins_1b,
                          show_plot_1b,
                          enter_plot_1b,
                          comments_1b)
    ))
    
  } else if((!cancer_type_now %in% cancer_types_with_screening_programmes & buttons_next_que_1c == 0)| cancer_type_now %in% cancer_types_with_screening_programmes & buttons_next_que_1c == 0){
    
    tagList(div(
      "In this question we are interested in how much sojourn times vary between cancers of the same type.", br(), br(),
      strong("Q1c: I believe that, if mean sojourn time of", cancer_type_labels[cancer_type_now],"is", mode_omst_all,
             "years, then 25% of patients with the fastest-progressing cancers (shortest sojourn time) will have a sojourn time of:"), br(),br(),
             tags$p(div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("elicit_1c_",cancer_type_now), NULL, elici_1c, min = 0, max = NA)),
                     div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
                     div(style = "display: inline-block; vertical-align:middle; width: 150px;", HTML("years or shorter.")), br(), br(),
             fluidRow(
               column(9, p(style="font-size:90%;", "Please click on 'Next' to continue.")),
               column(1, actionButton(paste0("next_que_1c_", cancer_type_now), "Next", width='120px', style="background-color: lightgrey"))
             ), br()), br()
      ))
    
  } else if(section_no == 1 & buttons_next_que_2b == 0) {
    
    tagList(div(
      strong("Q2a: What is the OMST for cancers diagnosed in early stages?"), br(),
      em("Note that this is the same as early-stage mean sojourn time (EMST) for cancers diagnosed in early stages."), br(), br(),
      "If OMST for", cancer_type_labels[cancer_type_now], "is", mode_omst_all, "years, and ",
      proportion_diagnosed_in_late_stage[cancer_type_now], "% of cancers will progress to late stage before being diagnosed, what is the OMST in cancers diagnosed in early stage?", br(),br(),
      tags$p(div(style = "display: inline-block; vertical-align:middle; width: 400px;", HTML("I believe that OMST in cancers diagnosed in early stage is")),
              div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("elicit_2a_",cancer_type_now), NULL, elici_2a, min = 0, max = NA)),
              div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
              div(style = "display: inline-block; vertical-align:middle; width: 150px;", HTML("years."))), br(), br(),
      fluidRow(
        column(9, p(style="font-size:90%;", "Please click on 'Next' to continue.")),
        column(1, actionButton(paste0("next_que_2a_", cancer_type_now), "Next", width='120px', style="background-color: lightgrey"))
      ), br(), br(),
      ifelse(buttons_next_que_2a > 0,
             tagList(div(
               paste0("Your answers imply that OMST for cancers diagnosed in late stages is ", omst_late, " years."),  br(), br(),
               "If you do not agree with this statement you can adjust your answers to Q1a and Q2a.", br(), br(), hr(), br(),
               strong("Q2b: What is the EMST for cancers diagnosed in late stages?"),br(), br(),
               "If cancers diagnosed in late stages have an overall sojourn time of", omst_late,
               "years, how much of that time will those cancers have spent in early-stage?", br(), br(),
               tags$p(div(style = "display: inline-block; vertical-align:middle; width: 400px;", HTML("I believe that cancers diagnosed in late stage will have spent")),
                       div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("elicit_2b_", cancer_type_now), NULL, elici_2b, min = 0, max = NA)),
                       div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
                       div(style = "display: inline-block; vertical-align:middle; width: 400px;", HTML("years in early stage before progressing to late stage."))), br(), br(),
                       fluidRow(
                         column(9, p(style="font-size:90%;", "Please click on 'Next' to continue.")),
                         column(1, actionButton(paste0("validate_que_2b_", cancer_type_now), "Next", width='120px', style="background-color: lightgrey"))
                       ), br(), br(),
                       ifelse(buttons_validate_que_2_b == 1,
                              tagList(div(
                                paste0("Your answers imply that late-stage mean sojourn time is ", lmst, " years."),  br(), br(),
                                "If you do not agree with this statement you can adjust your answers to Q2b.", br(), br(), hr(), br(),
                                fluidRow(
                                  column(9, p(style="font-size:90%;", "Please click on 'Next' to continue.")),
                                  column(1, actionButton(paste0("next_que_2b_", cancer_type_now), "Next", width='120px', style="background-color: lightgrey"))
                                ), br(), br()
                              )),
                              tagList(div("")))
               )),
             tagList(div("")))
      ))
    
  # } else if(section_no == 1 & buttons_next_que_2b == 0) {
  #   
  #   tagList(div(
  #     
  #     strong("If cancers diagnosed in late stage have an overall sojourn time of", omst_late,
  #            "years, how much of that time will those cancers have spent in early-stage?"), br(), br(),
  #     tags$li(div(style = "display: inline-block; vertical-align:middle; width: 400px;", HTML("I believe that cancers diagnosed in late stage will spend")),
  #             div(style = "display: inline-block; vertical-align:top; width: 75px;", numericInput(paste0("elicit_2b_", cancer_type_now), NULL, elici_2b, min = 0, max = NA)),
  #             div(style = "display: inline-block; vertical-align:middle; width: 10px;", HTML("")),
  #             div(style = "display: inline-block; vertical-align:middle; width: 400px;", HTML("years in early stage before progressing to late stage.")), br(), br(),
  #             fluidRow(
  #               column(9, p(style="font-size:90%;", "Please click on 'Save' to continue.")),
  #               column(1, actionButton(paste0("next_que_2b_", cancer_type_now), "Save", width='120px', style="background-color: lightgrey"))
  #             ), br()), br()
  #     
  #   ))
    
    } else {
      
        tagList(div(
          
          ifelse(cancer_type_now != buttons_cancer_types & buttons_view_ctDNA_evidence == 0,
                   tagList(div(
                     fluidRow(
                       column(9, p(style = "font-size:90%;", "If you would like to view evidence on on ctDNA cancers again, please click on 'View'.")),
                       column(1,
                              actionButton(paste0("view_ctDNA_evidence_", cancer_type_now), "View", width = '120px', style="background-color: lightgrey"))
                     ), br(), br()
                   )),
                   tagList(div(
                     strong("Survival for cancers detected vs not detected by Galleri"), br(), br(),
                     tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("Participants who developed cancer in the first 3 years of follow-up in CCGA study (Chen et al., 2021; Swanton et al., 2025)."))),
                     tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("Participants' blood samples were tested for cancer signals post-diagnosis."))),
                     tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("Outcome: overall survival for cancers detected vs not detected by Galleri (stratified by stage), compared to SEER expected survival adjusted for cancer type, stage and gender mix."))),
                     tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("Note: the values include all cancer types."))),
                     br(), br(),
                     tags$img(src = "ctDNA_plot_1.jpg", width = "70%", height = "70%"),
                     #tags$img(src = "ctDNA_plot_1.jpg", width = "550px", height = "400px"),
                     #img(src="ctDNA_plot_1.jpg", height="70%", width="70%", align = "left"),
                     br(), br(), hr(), br(),
                     strong("Signal detection in samples prior to cancer diagnosis"), br(), br(),
                     tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("Participants who enrolled in the study healthy and donated blood samples at the start (these were frozen)."))),
                     tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("Samples analysed for participants who developed cancer within 3 years of enrollment."))),
                     tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("Outcome: ctDNA signal detection rate up to 1, 2 or 3 years prior to cancer diagnosis."))),
                     br(), br(),
                     tags$img(src = "ctDNA_plot_2.jpg", width = "100%", height = "100%"),
                     #tags$img(src = "ctDNA_plot_2.jpg", width = "800px", height = "300px"),
                     #img(src="ctDNA_plot_2.jpg", height="100%", width="100%", align = "left"),
                     br(),br(), br(),
                   ))
            ),
            hr(), br(),
          # "buttons_next_que_3a =", buttons_next_que_3a, br(),
          # "elici_3a =", elici_3a, br(),
          # "buttons_next_que_3b =", buttons_next_que_3b,br(),
          # "elici_3b =", elici_3b,br(),
          # "condition 1 = ", ifelse(buttons_next_que_3a == 1 & elici_3a == 0, 1, 0),br(),
          # "condition 2 = ", ifelse(buttons_next_que_3b == 1 & elici_3b == 0, 1, 0),br(),
          # "condition 12 = ", ifelse((buttons_next_que_3a == 1 & elici_3a == 0)|(buttons_next_que_3b == 1 & elici_3b == 0), 1, 0),br(),
            strong(paste0("Do you believe that the ctDNA cancers detected by Galleri have the shortest sojourn time of all ", cancer_type_labels[cancer_type_now], "s?")), br(),
            radioButtons(paste0("elicit_3a_", cancer_type_now), "",
                         choices = c("Yes" = 1, "No" = 0),
                         selected = elici_3a),
            fluidRow(
              column(9, p(style = "font-size:90%;", "Please click on 'Next' to continue.")),
              column(1,
                     actionButton(paste0("next_que_3a_", cancer_type_now), "Next", width = '120px', style="background-color: lightgrey"))
            ), br(), br(),
          ifelse(buttons_next_que_3a == 1 & elici_3a == 1, #if they answered yes to 3a
                 tagList(div(
                   hr(), br(),
                   paste0("Galleri test sensitivity for ", cancer_type_labels[cancer_type_now]," is ", ct_DNA_sensitivity[cancer_type_now,"total"], "%, indicating they are ctDNA positive cancers (",
                          ct_DNA_sensitivity[cancer_type_now,"early"],"% of cancers in early stages, ",
                          ct_DNA_sensitivity[cancer_type_now,"late"] ,"% of cancers 
                          in late stages)."), br(), br(),
                   paste0("The predicted sojourn time for the ", ct_DNA_sensitivity[cancer_type_now,"total"],
                          "% fastest progressing ", cancer_type_labels[cancer_type_now],
                          "s (using a statistical model applied to your answer to question 1c) is ",
                          omst_ctDNA, " years. "), br(), br(),
                   strong("Is this value reflective of your beliefs on the MST of the ctDNA cancers detected by the Galleri test?"),
                   radioButtons(paste0("elicit_3b_", cancer_type_now), "",
                                choices = c("Yes" = 1, "No" = 0),
                                selected = elici_3b),
                   fluidRow(
                     column(9, p(style = "font-size:90%;", "Please click on 'Next' to continue.")),
                     column(1,
                            actionButton(paste0("next_que_3b_", cancer_type_now), "Next", width = '120px', style="background-color: lightgrey"))
                   ), br(), br()
                 )),
                 tagList(div(""))),
          #ifelse((buttons_next_que_3a == 1 & elici_3a == 0) | (buttons_next_que_3b == 1 & elici_3b == 0),
          ifelse((buttons_next_que_3a == 1 & elici_3a == 0) | buttons_next_que_3b == 1,
                 tagList(div(
                   hr(), br(),
                   f_chips_and_bins_3c(cancer_type_now,
                                       elici_minis_3c,
                                       elici_maxis_3c,
                                       chips_nchip_3c,
                                       chips_chips_3c,
                                       chips_lbins_3c,
                                       chips_rbins_3c,
                                       show_plot_3c,
                                       enter_plot_3c,
                                       comments_3c,
                                       mode_omst_all)
                   )),
                 tagList(div(""))
                 )
        ))
      
      }
    
  # } else if(buttons_next_que_3c == 0) {
  #   
  #   tagList(div(
  #     
  #     ifelse(cancer_type_now != buttons_cancer_types & buttons_view_ctDNA_evidence == 0,
  #              tagList(div(
  #                fluidRow(
  #                  column(9, p(style = "font-size:90%;", "If you would like to view evidence on on ctDNA cancers again, please click on 'View'.")),
  #                  column(1,
  #                         actionButton(paste0("view_ctDNA_evidence_", cancer_type_now), "View", width = '120px', style="background-color: lightgrey"))
  #                ), br(), br()
  #              )),
  #              tagList(div(
  #                strong("Survival for cancers detected vs not detected by Galleri"), br(),
  #                "Add description", br(), br(),
  #                img(src="ctDNA_plot_1.jpeg", height="100%", width="100%", align = "left"),
  #                strong("Signal detection in samples prior to cancer diagnosis"), br(),
  #                "Add description", br(),br(),
  #                img(src="ctDNA_plot_2.jpeg", height="100%", width="100%", align = "left"),
  #                br(),br()
  #              ))
  #       ),
  #       
  #       strong(paste0("Do you believe that the ctDNA cancers detected by Galleri are the most progressive of ", cancer_type_labels[cancer_type_now], "s?")), br(), 
  #       radioButtons(paste0("elicit_3a_", cancer_type_now), "",
  #                    choices = c("Yes" = 1, "No" = 0),
  #                    selected = elici_3a),
  #       fluidRow(
  #         column(9, p(style = "font-size:90%;", "Please click on 'Next' to continue.")),
  #         column(1,
  #                actionButton(paste0("next_que_3a_", cancer_type_now), "Next", width = '120px', style="background-color: lightgrey"))
  #       ), br(), br(),
  #     hr(), br(),
  #     
  #     ifelse(buttons_next_que_3a == 1 & elici_3a == 1,
  #            tagList(div(
  #              
  #              paste0("Galleri’s test sensitivity for ", cancer_type_labels[cancer_type_now]," is ", ct_DNA_sensitivity[cancer_type_now,"total"], "% (",
  #                     ct_DNA_sensitivity[cancer_type_now,"early"],"% in early stages, ",
  #                     ct_DNA_sensitivity[cancer_type_now,"late"] ,"% in late stages)."), br(), br(),
  #              paste0("The predicted sojourn time for the ", ct_DNA_sensitivity[cancer_type_now,"total"],
  #                     "% fastest progressing ", cancer_type_labels[cancer_type_now],
  #                     "s (using a statistical model applied to your answer to question 1c) is ",
  #                     omst_ctDNA, " years. "), br(), br(),
  #              strong("Is this value reflective of your beliefs on the MST of the ctDNA cancers detected by the Galleri test?"),
  #              radioButtons(paste0("elicit_3b_", cancer_type_now), "",
  #                           choices = c("Yes" = 1, "No" = 0),
  #                           selected = elici_3b),
  #              fluidRow(
  #                column(9, p(style = "font-size:90%;", "Please click on 'Next' to continue.")),
  #                column(1,
  #                       actionButton(paste0("next_que_3b_", cancer_type_now), "Next", width = '120px', style="background-color: lightgrey"))
  #              ), br(), br(),
  #              hr(), br(),
  #              ifelse(buttons_next_que_3b == 1,
  #                     tagList(div(f_chips_and_bins_3c(cancer_type_now,
  #                                                     elici_minis_3c,
  #                                                     elici_maxis_3c,
  #                                                     chips_nchip_3c,
  #                                                     chips_chips_3c,
  #                                                     chips_lbins_3c,
  #                                                     chips_rbins_3c,
  #                                                     show_plot_3c,
  #                                                     enter_plot_3c,
  #                                                     comments_3c,
  #                                                     mode_omst_all)
  #                                 )),
  #                     tagList(div("")))
  #              )),
  #            tagList(div(""))
  #            )
  #     
  #         )) #end of question 3
  #   
  # 
  # }
  
  )
  
)

