
# add graphs
# git dist to sojourn time 1c
# add save

library(DT)

#source("manual_inputs.R", local = TRUE)
source("functions.R", local = TRUE)
source("elicitation_questions_ui.R", local = TRUE)
source("about_you.R", local = TRUE)
source("section_1_ques.R", local = TRUE) # add 

cancer_type_labels <- c("breast cancer", "lung cancer", "colorectal cancer", "ovarian cancer", "prostate cancer", "liver cancer",
                  "cervical cancer", "head and neck cancer", "lymphoma", "oesophageal cancer", "pancreatic cancer",
                  "anal cancer", "bladder cancer", "gallbladder cancer", "renal cancer", "melanoma", "sarcoma", "stomach cancer", "thyroid cancer", "urothelial tract cancer", "uterus cancer")

cancer_types <- c("breast", "lung", "colorectal", "ovary", "prostate", "liver",
                  "cervix", "head_and_neck", "lymphoma", "oesophagus", "pancreas",
                  "anus", "bladder", "gallbladder", "kidney", "melanoma", "sarcoma", "stomach", "thyroid", "urothelial_tract", "uterus")

cancer_types_with_screening_programmes <- c(1, 2, 3, 7)

all_expert_ids <- c(1111,2222,3333,4444, 0710, 0810, 1610, 1710, 2110, 22101, 22102, 22103, 2310, 27101, 27102, 2810, 2910, 31101, 31102, 07111, 07112)

proportion_diagnosed_in_late_stage <- round(c(0.14632496, 0.71207422, 0.56682874, 0.60693454, 0.45755251, 0.67102804, 0.24155405, 0.62538332,
                                        0.67737544, 0.75448898, 0.77014604, 0.55223881, 0.24892561, 0.67700000, 0.42200536, 0.09929642,
                                        0.45700000, 0.71225806, 0.24466572, 0.62600000, 0.19376330)*100, digits = 0)

ct_DNA_sensitivity <- matrix(NA, 21, 3, dimnames = list(cancer_types, c("early", "late", "total")))
ct_DNA_sensitivity[,1] <- c(20.9, 40.0, 67.1, 60.0, 4.4, 81.3,
                            70.6, 72.2, 45.7, 10.5, 42.4,
                            50.0, 17.6, 20.0, 7.1, 0.0, 75.8, 33.3, 0.0, 0.0, 17.7)

ct_DNA_sensitivity[,2] <- c(87.0,93.2,91.5,90.0,70.3,100.0,
                            100.0, 92.8, 66.3, 97.3, 93.6,
                            100.0, 83.3, 91.7, 10.5, 66.7, 11.1, 94.1, 0.0, 100.0, 77.8)

ct_DNA_sensitivity[,3] <- c(30.5, 74.8, 82, 83.1, 11.2, 93.5,
                            80, 85.7, 56.3, 85, 83.7,
                            81.8, 34.8, 70.6, 18.2, 46.2, 72.3, 66.7, 0, 80, 28)

app_hosting <- "shiny_server" #"shiny.io", "local" or "shiny_server"
outputDir <- ifelse(app_hosting == "shiny_server", "/mnt/shiny/cancer_sojourn_time", paste0(getwd(),"/saved_answers"))


function (input, output, session) {
  
  ##### reactive values ##########
  
  elici_minis_training <- reactiveValues(test1 = integer(0)) # lower limit of expert's plausible range
  elici_maxis_training <- reactiveValues(test1 = integer(0), test2 = integer(0)) # upper limit of expert's plausible range
  chips_width_training <- reactiveValues(test1 = 0, test2 = 0) # bin width
  chips_lower_training <- reactiveValues(test1 = 0, test2 = 0) # lower limit of the plot
  chips_upper_training <- reactiveValues(test1 = 0, test2 = 0) # upper limit of the plot
  chips_nbins_training <- reactiveValues(test1 = 0, test2 = 0) # total number of bins
  chips_nchip_training <- reactiveValues(test1 = 0, test2 = 0) # total number of chips
  chips_nhigh_training <- reactiveValues(test1 = 0, test2 = 0) # height of plot
  chips_lbins_training <- reactiveValues(test1 = 0, test2 = 0) # lower limit of each bin
  chips_rbins_training <- reactiveValues(test1 = 0, test2 = 0) # upper limit of each bin
  chips_value_training <- reactiveValues(test1 = 0, test2 = 0) # bins (equal to rbins)
  chips_chips_training <- reactiveValues(test1 = 0, test2 = 0) # number of chips in each bin
  show_plot_training <- reactiveValues(test1 = 0, test2 = 0) # indicator (>0) that experts have entered (or updated) their plausible range for chips and bins
  enter_plot_training <- reactiveValues(test1 = 0, test2 = 0) #indicator (>0) that experts have used all chips
  comments_training <- reactiveValues(test1 = "Enter text here", test2 = "Enter text here") # expert's comments about elicitation questions
  
  
  elici_minis_1a <- reactiveValues(breast = integer(0)) # lower limit of expert's plausible range
  elici_maxis_1a <- reactiveValues(breast = integer(0)) # upper limit of expert's plausible range
  chips_width_1a <- reactiveValues(breast = 0) # bin width
  chips_lower_1a <- reactiveValues(breast = 0) # lower limit of the plot
  chips_upper_1a <- reactiveValues(breast = 0) # upper limit of the plot
  chips_nbins_1a <- reactiveValues(breast = 0) # total number of bins
  chips_nchip_1a <- reactiveValues(breast = 0) # total number of chips
  chips_nhigh_1a <- reactiveValues(breast = 0) # height of plot
  chips_lbins_1a <- reactiveValues(breast = 0) # lower limit of each bin
  chips_rbins_1a <- reactiveValues(breast = 0) # upper limit of each bin
  chips_value_1a <- reactiveValues(breast = 0) # bins (equal to rbins)
  chips_chips_1a <- reactiveValues(breast = 0) # number of chips in each bin
  show_plot_1a <- reactiveValues(breast = 0) # indicator (>0) that experts have entered (or updated) their plausible range for chips and bins
  enter_plot_1a <- reactiveValues(breast = 0) #indicator (>0) that experts have used all chips
  comments_1a <- reactiveValues(breast = "Enter text here") # expert's comments about elicitation questions

  elici_minis_1b <- reactiveValues(breast = integer(0)) # lower limit of expert's plausible range
  elici_maxis_1b <- reactiveValues(breast = integer(0)) # upper limit of expert's plausible range
  chips_width_1b <- reactiveValues(breast = 0) # bin width
  chips_lower_1b <- reactiveValues(breast = 0) # lower limit of the plot
  chips_upper_1b <- reactiveValues(breast = 0) # upper limit of the plot
  chips_nbins_1b <- reactiveValues(breast = 0) # total number of bins
  chips_nchip_1b <- reactiveValues(breast = 0) # total number of chips
  chips_nhigh_1b <- reactiveValues(breast = 0) # height of plot
  chips_lbins_1b <- reactiveValues(breast = 0) # lower limit of each bin
  chips_rbins_1b <- reactiveValues(breast = 0) # upper limit of each bin
  chips_value_1b <- reactiveValues(breast = 0) # bins (equal to rbins)
  chips_chips_1b <- reactiveValues(breast = 0) # number of chips in each bin
  show_plot_1b <- reactiveValues(breast = 0) # indicator (>0) that experts have entered (or updated) their plausible range for chips and bins
  enter_plot_1b <- reactiveValues(breast = 0) #indicator (>0) that experts have used all chips
  comments_1b <- reactiveValues(breast = "Enter text here") # expert's comments about elicitation questions
  
  elici_minis_3c <- reactiveValues(breast = integer(0)) # lower limit of expert's plausible range
  elici_maxis_3c <- reactiveValues(breast = integer(0)) # upper limit of expert's plausible range
  chips_width_3c <- reactiveValues(breast = 0) # bin width
  chips_lower_3c <- reactiveValues(breast = 0) # lower limit of the plot
  chips_upper_3c <- reactiveValues(breast = 0) # upper limit of the plot
  chips_nbins_3c <- reactiveValues(breast = 0) # total number of bins
  chips_nchip_3c <- reactiveValues(breast = 0) # total number of chips
  chips_nhigh_3c <- reactiveValues(breast = 0) # height of plot
  chips_lbins_3c <- reactiveValues(breast = 0) # lower limit of each bin
  chips_rbins_3c <- reactiveValues(breast = 0) # upper limit of each bin
  chips_value_3c <- reactiveValues(breast = 0) # bins (equal to rbins)
  chips_chips_3c <- reactiveValues(breast = 0) # number of chips in each bin
  show_plot_3c <- reactiveValues(breast = 0) # indicator (>0) that experts have entered (or updated) their plausible range for chips and bins
  enter_plot_3c <- reactiveValues(breast = 0) #indicator (>0) that experts have used all chips
  comments_3c <- reactiveValues(breast = "Enter text here") # expert's comments about elicitation questions
  
  elici_1c <- reactiveValues(breast = integer(0))
  elici_2a <- reactiveValues(breast = integer(0))
  elici_2b <- reactiveValues(breast = integer(0))
  elici_3a <- reactiveValues(breast = integer(0))
  elici_3b <- reactiveValues(breast = integer(0))
  elici_section_3 <- reactiveValues(breast = integer(0))
  mode_omst_all <- reactiveValues(breast = integer(0))
  mode_omst_ctDNA <- reactiveValues(breast = 0)
  omst_late <- reactiveValues(breast = integer(0))
  lmst <- reactiveValues(breast = integer(0))
  omst_ctDNA <- reactiveValues(breast = integer(0))
  
  buttons <- reactiveValues(expert_id = integer(0), # expert's unique code provided by the investigator that distinguished their saved answers from others'
                            enter_unique_id = 0, # indicator (>0) that the expert has entered their unique identifier
                            consent = 0, # indicator (>0) that the expert has consented
                            next_que_test_1 = 0,
                            enter_about_you = 0, # indicator (>0) that the expert has answered "about you" questions
                            cancer_types = 0, # placeholder for all cancer types the expert will comment on
                            cancer_types_section_1 = 0, # placeholder for all cancer types for full elicitation
                            cancer_types_section_2 = 0, # placeholder for all cancer types for partial elicitation
                            cancer_types_section_3 = 0, # placeholder for all cancer types for qualitative elicitation
                            cancer_types_section_1_2 = 0, # placeholder for all cancer types for full or partial elicitation
                            cancer_type_labels = 0, # as cancer_types but the expert-facing name
                            back = 1, # indicator (>0) that expert is on the "summary table" page
                            cancer_type_live = 1, # cancer type on current screen
                            prognosis = rep(0,21), # indicator if the expert has finished viewing the page for the relevant cancer
                            omst_graph = rep(0,21), # indicator if the expert has finished viewing the page for the relevant cancer
                            next_que_1a = rep(0,21), # indicator if the expert has finished viewing the page for the relevant cancer
                            next_que_1b = rep(0,21), # indicator if the expert has finished viewing the page for the relevant cancer 
                            next_que_1c = rep(0,21), # indicator if the expert has finished viewing the page for the relevant cancer
                            next_que_2a = rep(0,21), # indicator if the expert has finished viewing the page for the relevant cancer
                            next_que_2b = rep(0,21), # indicator if the expert has finished viewing the page for the relevant cancer
                            next_que_3a = rep(0,21), # indicator if the expert has finished viewing the page for the relevant cancer
                            next_que_3b = rep(0,21), # indicator if the expert has finished viewing the page for the relevant cancer
                            next_que_3c = rep(0,21), # indicator if the expert has finished viewing the page for the relevant cancer
                            validate_que_2b = rep(0,21),
                            view_ctDNA_evidence = rep(0,21), # indicator if the expert has finished viewing the page for the relevant cancer
                            current_section = 1,
                            section_3 = 0,
                            next_que_section_3 = rep(0,21)
                            #enter_previous_responses = 0,
                            #radio_previous_responses = 0,
                            #get_previous_responses = 0
                            )
  
  about_you <- reactiveValues(que1 = integer(0),
                              que2 = "Enter text",
                              que3 = "Enter text",
                              que4 = "Enter text",
                              que5 = integer(0),
                              que6 = "Enter text",
                              que7 = integer(0))
  
  summary_table <- reactiveValues(d1 = as.data.frame(matrix(rep(0, 4), nrow = 2, ncol = 2)))
  
  save <- reactiveValues(about_you_all = 0, # object for saving "about you" questions
                         about_you_colnames = "expert_id", # column names for "about_you" questions
                         all_answers = 0,
                         all_answers_colnames = 0)
  
  # populate values for other cancer types
  for (i in 1:21){
    
    que_name <- cancer_types[i]
    
    elici_minis_1a[[que_name]] <- integer(0) # lower limit of expert's plausible range
    elici_maxis_1a[[que_name]] <- integer(0) # upper limit of expert's plausible range
    chips_width_1a[[que_name]] <- 0 # bin width
    chips_lower_1a[[que_name]] <- 0 # lower limit of the plot
    chips_upper_1a[[que_name]] <- 0 # upper limit of the plot
    chips_nbins_1a[[que_name]] <- 0 # total number of bins
    chips_nchip_1a[[que_name]] <- 0 # total number of chips
    chips_nhigh_1a[[que_name]] <- 0 # height of plot
    chips_lbins_1a[[que_name]] <- 0 # lower limit of each bin
    chips_rbins_1a[[que_name]] <- 0 # upper limit of each bin
    chips_value_1a[[que_name]] <- 0 # bins (equal to rbins)
    chips_chips_1a[[que_name]] <- 0 # number of chips in each bin
    show_plot_1a[[que_name]] <- 0 # indicator (>0) that experts have entered (or updated) their plausible range for chips and bins
    enter_plot_1a[[que_name]] <- 0 #indicator (>0) that experts have used all chips
    comments_1a[[que_name]] <- "Enter text here" # expert's comments about elicitation questions

    elici_minis_1b[[que_name]] <- integer(0) # lower limit of expert's plausible range
    elici_maxis_1b[[que_name]] <- integer(0) # upper limit of expert's plausible range
    chips_width_1b[[que_name]] <- 0 # bin width
    chips_lower_1b[[que_name]] <- 0 # lower limit of the plot
    chips_upper_1b[[que_name]] <- 0 # upper limit of the plot
    chips_nbins_1b[[que_name]] <- 0 # total number of bins
    chips_nchip_1b[[que_name]] <- 0 # total number of chips
    chips_nhigh_1b[[que_name]] <- 0 # height of plot
    chips_lbins_1b[[que_name]] <- 0 # lower limit of each bin
    chips_rbins_1b[[que_name]] <- 0 # upper limit of each bin
    chips_value_1b[[que_name]] <- 0 # bins (equal to rbins)
    chips_chips_1b[[que_name]] <- 0 # number of chips in each bin
    show_plot_1b[[que_name]] <- 0 # indicator (>0) that experts have entered (or updated) their plausible range for chips and bins
    enter_plot_1b[[que_name]] <- 0 #indicator (>0) that experts have used all chips
    comments_1b[[que_name]] <- "Enter text here" # expert's comments about elicitation questions
    
    elici_minis_3c[[que_name]] <- integer(0) # lower limit of expert's plausible range
    elici_maxis_3c[[que_name]] <- integer(0) # upper limit of expert's plausible range
    chips_width_3c[[que_name]] <- 0 # bin width
    chips_lower_3c[[que_name]] <- 0 # lower limit of the plot
    chips_upper_3c[[que_name]] <- 0 # upper limit of the plot
    chips_nbins_3c[[que_name]] <- 0 # total number of bins
    chips_nchip_3c[[que_name]] <- 0 # total number of chips
    chips_nhigh_3c[[que_name]] <- 0 # height of plot
    chips_lbins_3c[[que_name]] <- 0 # lower limit of each bin
    chips_rbins_3c[[que_name]] <- 0 # upper limit of each bin
    chips_value_3c[[que_name]] <- 0 # bins (equal to rbins)
    chips_chips_3c[[que_name]] <- 0 # number of chips in each bin
    show_plot_3c[[que_name]] <- 0 # indicator (>0) that experts have entered (or updated) their plausible range for chips and bins
    enter_plot_3c[[que_name]] <- 0 #indicator (>0) that experts have used all chips
    comments_3c[[que_name]] <- "Enter text here" # expert's comments about elicitation questions
    
    elici_1c[[que_name]] <- integer(0)
    elici_2a[[que_name]] <- integer(0)
    elici_2b[[que_name]] <- integer(0)
    elici_3a[[que_name]] <- integer(0)
    elici_3b[[que_name]] <- integer(0)
    elici_section_3[[que_name]] <- integer(0)
    mode_omst_all[[que_name]] <- integer(0)
    omst_late[[que_name]] <- integer(0)
    lmst[[que_name]] <- integer(0)
    #omst_25[[que_name]] <- integer(0)
    omst_ctDNA[[que_name]] <- integer(0)
    mode_omst_ctDNA[[que_name]] <- 0
    
    
  }
  
  ##### button clicks ####
  
  
  observeEvent(input$enter_unique_id, {
    
    buttons$expert_id <- input$expert_id
    
    if(buttons$expert_id %in% all_expert_ids){
      
          buttons$enter_unique_id <- 1
          
          # upload previous responses
          
          #if app hosted on shiny.io change the outputDir
          outputDir <- ifelse(app_hosting == "shiny.io", tempdir(), outputDir)
          
          # Read all the files into a list
          files <-list.files(path = outputDir, pattern = as.character(buttons$expert_id), full.names = TRUE)
          
          if(length(files > 0)){
            
            # upload all files
            data <- setNames(lapply(files, read.csv, stringsAsFactors = FALSE), files)
            names(data)<- gsub(paste0(outputDir, "/", buttons$expert_id, "_"), "", names(data))
            # data
            
            if("about_you.csv" %in% names(data)){
              
              temp_about_you <- data[["about_you.csv"]]
              
              about_you[["que1"]] <- temp_about_you["que1"]
              about_you[["que2"]] <- ifelse("que2" %in% colnames(temp_about_you), temp_about_you[,"que2"], "Enter text")
              about_you[["que3"]] <- ifelse("que3" %in% colnames(temp_about_you), temp_about_you[,"que3"], "Enter text")
              about_you[["que4"]] <- ifelse("que4" %in% colnames(temp_about_you), temp_about_you[,"que4"], "Enter text")
              about_you[["que5"]] <- ifelse("que5" %in% colnames(temp_about_you), temp_about_you[,"que5"], integer(0))
              about_you[["que6"]] <- ifelse("que6" %in% colnames(temp_about_you), temp_about_you[,"que6"], "Enter text")
              
              if(temp_about_you[,"que1"] == 1){
                about_you[["que7"]] <- integer(0)
              } else {
                about_you[["que7"]] <- as.vector(unlist(temp_about_you[,grep("que7", colnames(temp_about_you))]))
              }
              
              if(about_you$que1 == 1){
                
                buttons$cancer_types <- 1:21
                buttons$cancer_types_section_1 <-  1: 6
                buttons$cancer_types_section_2 <-  7:11
                buttons$cancer_types_section_3 <- 12:21
                buttons$cancer_types_section_1_2<- 1:11
                buttons$cancer_type_labels <- cancer_type_labels[1:21]
                
              } else {
                
                buttons$cancer_types <- about_you$que7
                buttons$cancer_types_section_1 <- buttons$cancer_types
                buttons$cancer_types_section_2 <- integer(0)
                buttons$cancer_types_section_3 <- integer(0)
                buttons$cancer_types_section_1_2 <- buttons$cancer_types
                
                temp <- rep(NA, 21)
                
                for (i in 1:21){
                  
                  if(i %in% about_you$que7){
                    temp[i] <- cancer_type_labels[i]
                  }
                  
                }
                
                buttons$cancer_type_labels <- temp[which(!is.na(temp))]
                
              }
              
              # create a matrix of reactive values that populate the summary table. These later get updated when experts save each answer.
              temp_nrow <- length(buttons$cancer_types_section_1_2)
              temp_ncol <- 8
              summary_table$d1 <- as.data.frame(matrix(rep("-", temp_ncol * temp_nrow), nrow=temp_nrow, ncol = temp_ncol))
              colnames(summary_table$d1) <- c("Cancer type", "OMST w/o screening", "OMST w/ screening", "Sojourn time in most severe cancers", "OMST/EMST if diagnosed in early stages",
                                              "EMST if diagnosed in late stages", "LMST", "OMST for ctDNA cancers") 
              
              for (i in 1:temp_nrow){
                
                summary_table$d1[i,1] <- buttons$cancer_type_labels[i]
                
                if(!buttons$cancer_types_section_1_2[i] %in% cancer_types_with_screening_programmes){
                  summary_table$d1[i,3] <- "NA"
                }
                if(buttons$cancer_types_section_1_2[i] %in% buttons$cancer_types_section_2){
                  summary_table$d1[i,c(5,6,7)] <- "NA"
                }
                
              }
              
              buttons$consent <- 1
              buttons$next_que_test_1 <- 1
              buttons$enter_about_you <- 1
              
            }
            
            for (i in 1:21){
              
              que_name <- cancer_types[i]
              
              if(paste0("que_1a_", i, ".csv") %in% names(data)){
                
                temp_que_1a <- data[[paste0("que_1a_", i, ".csv")]]
                
                elici_minis_1a[[que_name]] <- temp_que_1a[,"elici_minis"]
                elici_maxis_1a[[que_name]] <- temp_que_1a[,"elici_maxis"]
                chips_width_1a[[que_name]] <- temp_que_1a[,"bin_width"]
                chips_lower_1a[[que_name]] <- f_lower(elici_minis_1a[[que_name]], chips_width_1a[[que_name]], 0)
                chips_upper_1a[[que_name]] <- f_upper(elici_maxis_1a[[que_name]], chips_width_1a[[que_name]], NA)
                chips_nbins_1a[[que_name]] <- f_nbins(chips_lower_1a[[que_name]], chips_upper_1a[[que_name]], chips_width_1a[[que_name]])
                chips_nchip_1a[[que_name]] <- 2 * chips_nbins_1a[[que_name]]
                chips_nhigh_1a[[que_name]] <- 2 * chips_nbins_1a[[que_name]]
                chips_lbins_1a[[que_name]] <- f_lbins(chips_lower_1a[[que_name]], chips_upper_1a[[que_name]], chips_width_1a[[que_name]])
                chips_rbins_1a[[que_name]] <- as.numeric(unlist(temp_que_1a[,grep("rbins", colnames(temp_que_1a))]))
                chips_value_1a[[que_name]] <- chips_rbins_1a[[que_name]]
                chips_chips_1a[[que_name]] <- as.numeric(unlist(temp_que_1a[,grep("chips", colnames(temp_que_1a))]))
                show_plot_1a[[que_name]] <- 1
                enter_plot_1a[[que_name]] <- 1
                comments_1a[[que_name]] <- ifelse(length(grep("comments", colnames(temp_que_1a))) == 0, "Enter text here", temp_que_1a[,"comments"])
                mode_omst_all[[que_name]] <- temp_que_1a[,"mode_omst_all"]
                
                tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
                summary_table$d1[tab_row,2] <- paste0(mode_omst_all[[que_name]], " (", elici_minis_1a[[que_name]], " - ", elici_maxis_1a[[que_name]], ")")
                
                buttons$prognosis[i] <- 1
                buttons$omst_graph[i] <- 1
                buttons$next_que_1a[i] <- 1
                
                
              }
              
              if(paste0("que_1b_", i, ".csv") %in% names(data)){
                
                temp_que_1b <- data[[paste0("que_1b_", i, ".csv")]]
                
                elici_minis_1b[[que_name]] <- temp_que_1b[,"elici_minis"]
                elici_maxis_1b[[que_name]] <- temp_que_1b[,"elici_maxis"]
                chips_width_1b[[que_name]] <- temp_que_1b[,"bin_width"]
                chips_lower_1b[[que_name]] <- f_lower(elici_minis_1b[[que_name]], chips_width_1b[[que_name]], 0)
                chips_upper_1b[[que_name]] <- f_upper(elici_maxis_1b[[que_name]], chips_width_1b[[que_name]], NA)
                chips_nbins_1b[[que_name]] <- f_nbins(chips_lower_1b[[que_name]], chips_upper_1b[[que_name]], chips_width_1b[[que_name]])
                chips_nchip_1b[[que_name]] <- 2 * chips_nbins_1b[[que_name]]
                chips_nhigh_1b[[que_name]] <- 2 * chips_nbins_1b[[que_name]]
                chips_lbins_1b[[que_name]] <- f_lbins(chips_lower_1b[[que_name]], chips_upper_1b[[que_name]], chips_width_1b[[que_name]])
                chips_rbins_1b[[que_name]] <- as.numeric(unlist(temp_que_1b[,grep("rbins", colnames(temp_que_1b))]))
                chips_value_1b[[que_name]] <- chips_rbins_1b[[que_name]]
                chips_chips_1b[[que_name]] <- as.numeric(unlist(temp_que_1b[,grep("chips", colnames(temp_que_1b))]))
                show_plot_1b[[que_name]] <- 1
                enter_plot_1b[[que_name]] <- 1
                comments_1b[[que_name]] <- ifelse(length(grep("comments", colnames(temp_que_1b))) == 0, "Enter text here", temp_que_1b[,"comments"])
                mode_omst_all[[que_name]] <- temp_que_1a[,"mode_omst_all"]
                
                tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
                summary_table$d1[tab_row,3] <- paste0(mode_omst_all[[que_name]], " (", elici_minis_1b[[que_name]], " - ", elici_maxis_1b[[que_name]], ")")
                
                buttons$next_que_1b[i] <- 1
                
              }
              
              if(paste0("que_1c_", i, ".csv") %in% names(data)){
                
                temp_que_1c <- data[[paste0("que_1c_", i, ".csv")]]
                
                elici_1c[[que_name]] <- temp_que_1c[,"elici_1c"]
                omst_ctDNA[[que_name]] <- temp_que_1c[,"omst_ctDNA"]
                
                tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
                summary_table$d1[tab_row,4] <- elici_1c[[que_name]]
                
                buttons$next_que_1c[i] <- 1
                
              }
              
              if(paste0("que_2_", i, ".csv") %in% names(data)){
                
                temp_que_2 <- data[[paste0("que_2_", i, ".csv")]]
                
                elici_2a[[que_name]] <- temp_que_2[,"elici_2a"]
                elici_2b[[que_name]] <- temp_que_2[,"elici_2b"]
                omst_late[[que_name]] <- temp_que_2[,"omst_late"]
                lmst[[que_name]] <- temp_que_2[,"lmst"]
                
                tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
                summary_table$d1[tab_row,5] <- elici_2a[[que_name]]
                summary_table$d1[tab_row,6] <- elici_2b[[que_name]]
                summary_table$d1[tab_row,7] <- lmst[[que_name]]
                
                buttons$next_que_2a[i] <- 1
                buttons$next_que_2b[i] <- 1
                buttons$validate_que_2b[i] <- 1
                
              }
              
              if(paste0("que_3_", i, ".csv") %in% names(data)){
                
                temp_que_3c <- data[[paste0("que_3_", i, ".csv")]]
                
                elici_3a[[que_name]] <- temp_que_3c[,"elici_3a"]
                # elici_3b[[que_name]] <- temp_que_3c[,"elici_3b"]
                elici_3b[[que_name]] <- ifelse(is.na(temp_que_3c[,"elici_3b"]), integer(0), temp_que_3c[,"elici_3b"])
                # omst_ctDNA[[que_name]] <- temp_que_3c[,"omst_ctDNA"]
                
                temp_elici_3b <- ifelse(length(elici_3b[[que_name]]) == 0, 0, elici_3b[[que_name]])
                
                buttons$next_que_3a[i] <- 1
                buttons$next_que_3b[i] <- 1
                
                if(elici_3a[[que_name]] == 0 | temp_elici_3b == 0){
                  
                  #elici_3c[[que_name]] <- 1
                  
                  elici_minis_3c[[que_name]] <- temp_que_3c[,"elici_minis"]
                  elici_maxis_3c[[que_name]] <- temp_que_3c[,"elici_maxis"]
                  chips_width_3c[[que_name]] <- temp_que_3c[,"bin_width"]
                  chips_lower_3c[[que_name]] <- f_lower(elici_minis_3c[[que_name]], chips_width_3c[[que_name]], 0)
                  chips_upper_3c[[que_name]] <- f_upper(elici_maxis_3c[[que_name]], chips_width_3c[[que_name]], NA)
                  chips_nbins_3c[[que_name]] <- f_nbins(chips_lower_3c[[que_name]], chips_upper_3c[[que_name]], chips_width_3c[[que_name]])
                  chips_nchip_3c[[que_name]] <- 2 * chips_nbins_3c[[que_name]]
                  chips_nhigh_3c[[que_name]] <- 2 * chips_nbins_3c[[que_name]]
                  chips_lbins_3c[[que_name]] <- f_lbins(chips_lower_3c[[que_name]], chips_upper_3c[[que_name]], chips_width_3c[[que_name]])
                  chips_rbins_3c[[que_name]] <- as.numeric(unlist(temp_que_3c[,grep("rbins", colnames(temp_que_3c))]))
                  chips_value_3c[[que_name]] <- chips_rbins_3c[[que_name]]
                  chips_chips_3c[[que_name]] <- as.numeric(unlist(temp_que_3c[,grep("chips", colnames(temp_que_3c))]))
                  show_plot_3c[[que_name]] <- 1
                  enter_plot_3c[[que_name]] <- 1
                  comments_3c[[que_name]] <- ifelse(length(grep("comments", colnames(temp_que_3c))) == 0, "Enter text here", temp_que_3c[,"comments"])
                  
                  mode_omst_ctDNA[[que_name]] <- round(f_get_mode_from_histogram(chips_chips_3c[[que_name]], chips_lbins_3c[[que_name]], chips_rbins_3c[[que_name]]), digits = 1)
                  
                  tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
                  summary_table$d1[tab_row,8] <- paste0(mode_omst_ctDNA[[que_name]], " (", elici_minis_3c[[que_name]], " - ", elici_maxis_3c[[que_name]], ")")
                  
                  buttons$next_que_3c[i] <- 1
                  
                } else {
                  
                  tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
                  summary_table$d1[tab_row,8] <- omst_ctDNA[[que_name]]
                  
                }
                
              }
              
            }
            
            for (i in 12:length(cancer_types)){
              
              if(paste0("sec_3_", i, ".csv") %in% names(data)){
                
                que_name <- cancer_types[i]
                
                temp_section_3 <- data[[paste0("sec_3_", i, ".csv")]]
                
                elici_section_3[[que_name]] <- temp_section_3[,grep("cancer_type_",colnames(temp_section_3))]
                
                buttons$next_que_section_3[i] <- 1
                
              }
              
            }
            
            # set "buttons$cancer_type_live"
            
          }
          
          
    } else {
      
      showModal(modalDialog ("Your unique ID has not been recognised. Please make sure you have entered the ID correctly.", size="l"))
      
    }
    
  })
  
  observeEvent(input$enter_consent, {
    
    buttons$consent <- 1
    
  })
  
  observeEvent(input[["show_plot_test_1"]],{
    
      # save inputs as reactive values
      elici_minis_training[["test1"]] <- input[["min_test_1"]]
      elici_maxis_training[["test1"]] <- input[["max_test_1"]]
      
      #check whether plausible range is within parameter limits and elici_minis < elici_maxis
      condition_test <- f_cond_min_max(elici_minis_training[["test1"]], elici_maxis_training[["test1"]], 0, NA) #remove latter two arguments for OMST
      
      if(condition_test==1){
        
        show_plot_training[["test1"]] <- 1 # show plot
        enter_plot_training[["test1"]] <- 0 # do not show feedback on expert's plot
        
        # update plot parameters
        chips_width_training[["test1"]] <- f_width(elici_minis_training[["test1"]], elici_maxis_training[["test1"]], bins[-(1:3)])
        chips_lower_training[["test1"]] <- f_lower(elici_minis_training[["test1"]], chips_width_training[["test1"]], 0) # remove last argument for OMST
        chips_upper_training[["test1"]] <- f_upper(elici_maxis_training[["test1"]], chips_width_training[["test1"]], NA) #remove last argument for OMST
        chips_nbins_training[["test1"]] <- f_nbins(chips_lower_training[["test1"]], chips_upper_training[["test1"]], chips_width_training[["test1"]])
        chips_nchip_training[["test1"]] <- 2 * chips_nbins_training[["test1"]]
        chips_nhigh_training[["test1"]] <- 2 * chips_nbins_training[["test1"]]
        chips_lbins_training[["test1"]] <- f_lbins(chips_lower_training[["test1"]], chips_upper_training[["test1"]], chips_width_training[["test1"]])
        chips_rbins_training[["test1"]] <- f_rbins(chips_lower_training[["test1"]], chips_upper_training[["test1"]], chips_width_training[["test1"]])
        chips_value_training[["test1"]] <- chips_rbins_training[["test1"]]
        chips_chips_training[["test1"]] <- rep(0,chips_nbins_training[["test1"]])
        
      } else {
        
        # if condition == 0, who error message
        showModal(modalDialog (strong("Please make sure you've entered the minimum and the
               maximum, and that the minimum is lower than the maximum."), size="l"))
        
      }
    
  })
  
  observeEvent(input[["enter_plot_test_1"]],{
    
    if(round(chips_nchip_training[["test1"]]-sum(chips_chips_training[["test1"]]),digits=0) > 0 ){
        
        # error mesage if not all chips are used
        showModal(modalDialog (strong("Please make sure you use all the available chips before proceeding."), size="l"))
        
      } else {
        
        enter_plot_training[["test1"]] <- 1
        
      }
    
  })
  
  output[["plot_test_1"]]<- renderPlot({
    
    f_plot_the_plot_test(chips_lower_training[["test1"]], chips_upper_training[["test1"]],
                      chips_nhigh_training[["test1"]], chips_nbins_training[["test1"]],
                      chips_lbins_training[["test1"]], chips_rbins_training[["test1"]],
                      chips_chips_training[["test1"]])
    }, height = 330, width = 400)#530
  
  observeEvent(input[["location_test_1"]], { #add red chips to bins
    
    chips_chips_training[["test1"]] <- f_add_chips(chips_nbins_training[["test1"]],
                                              chips_lbins_training[["test1"]],
                                              chips_rbins_training[["test1"]],
                                              chips_nchip_training[["test1"]],
                                              chips_chips_training[["test1"]],
                                              input[["location_test_1"]]$x,
                                              input[["location_test_1"]]$y)

    
  })
  
  observeEvent(input[["next_que_test_1"]],{
    
    buttons$next_que_test_1 <- 1
    
  })
  
  
  observeEvent(input$about_you_1,{
    about_you[["que1"]] <- input[["about_you_1"]]
  })
  
  observeEvent(input$about_you_2,{
    about_you[["que2"]] <- input[["about_you_2"]]
  })
  
    observeEvent(input$about_you_3,{
    about_you[["que3"]] <- input[["about_you_3"]]
  })

  observeEvent(input$about_you_4,{
    about_you[["que4"]] <- input[["about_you_4"]]
  })

  observeEvent(input$about_you_5,{
    about_you[["que5"]] <- input[["about_you_5"]]
  })
  
  observeEvent(input$about_you_6,{
    about_you[["que6"]] <- input[["about_you_6"]]
  })
  
  observeEvent(input$about_you_7,{
    about_you[["que7"]] <- input[["about_you_7"]]
  })
  
  observeEvent(input$enter_about_you, {
  
    if(length(about_you$que1) > 0){

      if(about_you$que1 == 1 | (about_you$que1 == 2 & length(about_you$que7) > 0)){
        
        buttons$enter_about_you <- 1
        
        # if(length(input[["about_you_2"]]) > 0){
        #   about_you_que2() <- input[["about_you_2"]]
        # }
        # if(length(input[["about_you_3"]]) > 0){
        #   about_you[["que3"]] <- input[["about_you_3"]]
        # }
        # if(length(input[["about_you_4"]]) > 0){
        #   about_you[["que4"]] <- input[["about_you_4"]]
        # }
        # if(length(input[["about_you_5"]]) > 0){
        #   about_you[["que5"]] <- input[["about_you_5"]]
        # }
        # if(length(input[["about_you_6"]]) > 0){
        #   about_you[["que6"]] <- input[["about_you_6"]]
        # }
        
        #save_answers
        
        save_about_you <- c(buttons$expert_id,
                            about_you$que1, about_you$que2, about_you$que3,
                            about_you$que4, about_you$que5, about_you$que6, about_you$que7)
        save_about_you_colnames <- c("expert_id", "que1", "que2", "que3", "que4", "que5", "que6")
        
        #if they don't answer que5, remove it from colnames (it's already integer(0) in save_about_you so doesn't need removing)
        if(length(about_you[["que5"]]) == 0){
          save_about_you_colnames <- save_about_you_colnames[-6]
        }
        
        #if they answer que7, add more colnames)
        if(length(about_you[["que7"]]) > 0){
          save_about_you_colnames <- c(save_about_you_colnames, paste0("que7_", 1:length(about_you[["que7"]])))
        }
        
        f_save(save_about_you, save_about_you_colnames, paste0(buttons$expert_id, "_about_you.csv"))

        
        if(about_you$que1 == 1){
          
          buttons$cancer_types <- 1:21
          buttons$cancer_types_section_1 <-  1: 6
          buttons$cancer_types_section_2 <-  7:11
          buttons$cancer_types_section_3 <- 12:21
          buttons$cancer_types_section_1_2<- 1:11
          buttons$cancer_type_labels <- cancer_type_labels[1:21]
          
          } else {
            
          buttons$cancer_types <- about_you$que7
          buttons$cancer_types_section_1 <- buttons$cancer_types
          buttons$cancer_types_section_2 <- integer(0)
          buttons$cancer_types_section_3 <- integer(0)
          buttons$cancer_types_section_1_2 <- buttons$cancer_types
         
          temp <- rep(NA, 21)
          
          for (i in 1:21){
            
            if(i %in% about_you$que7){
              temp[i] <- cancer_type_labels[i]
            }
            
          }
          
          buttons$cancer_type_labels <- temp[which(!is.na(temp))]
          
          }
        
        # create a matrix of reactive values that populate the summary table. These later get updated when experts save each answer.
        temp_nrow <- length(buttons$cancer_types_section_1_2)
        temp_ncol <- 8
        summary_table$d1 <- as.data.frame(matrix(rep("-", temp_ncol * temp_nrow), nrow=temp_nrow, ncol = temp_ncol))
        colnames(summary_table$d1) <- c("Cancer type", "OMST w/o screening", "OMST w/ screening", "Sojourn time in most severe cancers", "OMST/EMST if diagnosed in early stages",
                                    "EMST if diagnosed in late stages", "LMST", "OMST for ctDNA cancers") 
        
        for (i in 1:temp_nrow){
          
          summary_table$d1[i,1] <- buttons$cancer_type_labels[i]
          
          if(!buttons$cancer_types_section_1_2[i] %in% cancer_types_with_screening_programmes){
            summary_table$d1[i,3] <- "NA"
          }
          if(buttons$cancer_types_section_1_2[i] %in% buttons$cancer_types_section_2){
            summary_table$d1[i,c(5,6,7)] <- "NA"
          }
          
        }

        
      } else {
        
        showModal(modalDialog("Please ensure you have answered all questions.", size = "l"))
        
      }
      
    } else {
      
      showModal(modalDialog("Please ensure you have answered all questions.", size = "l"))
      
    }
    
  })
  
  
  
  observeEvent(input$summary_table_cells_selected, {
    
    buttons$back <- 0
    
    if(!is.na(input$summary_table_cells_selected[1] == 1)){
      
      cell_row <- input$summary_table_cells_selected[1]
      cell_col <- input$summary_table_cells_selected[2]
      
      buttons$cancer_type_live <- buttons$cancer_types_section_1_2[cell_row]
      
      if(cell_col == 1){
        buttons$prognosis[buttons$cancer_type_live] <- 0
        buttons$omst_graph[buttons$cancer_type_live] <- 0
      } else if(cell_col == 2){
        buttons$prognosis[buttons$cancer_type_live] <- 1
        buttons$omst_graph[buttons$cancer_type_live] <- 1
        buttons$next_que_1a[buttons$cancer_type_live] <- 0
      } else if(cell_col == 3){
        buttons$prognosis[buttons$cancer_type_live] <- 1
        buttons$omst_graph[buttons$cancer_type_live] <- 1
        buttons$next_que_1a[buttons$cancer_type_live] <- 1
        buttons$next_que_1b[buttons$cancer_type_live] <- 0
      } else if(cell_col == 4){
        buttons$prognosis[buttons$cancer_type_live] <- 1
        buttons$omst_graph[buttons$cancer_type_live] <- 1
        buttons$next_que_1a[buttons$cancer_type_live] <- 1
        buttons$next_que_1b[buttons$cancer_type_live] <- 1
        buttons$next_que_1c[buttons$cancer_type_live] <- 0
      } else if(cell_col == 5 | cell_col == 6| cell_col == 7){
        buttons$prognosis[buttons$cancer_type_live] <- 1
        buttons$omst_graph[buttons$cancer_type_live] <- 1
        buttons$next_que_1a[buttons$cancer_type_live] <- 1
        buttons$next_que_1b[buttons$cancer_type_live] <- 1
        buttons$next_que_1c[buttons$cancer_type_live] <- 1
        buttons$next_que_2b[buttons$cancer_type_live] <- 0
      } else if(cell_col == 8){
        buttons$prognosis[buttons$cancer_type_live] <- 1
        buttons$omst_graph[buttons$cancer_type_live] <- 1
        buttons$next_que_1a[buttons$cancer_type_live] <- 1
        buttons$next_que_1b[buttons$cancer_type_live] <- 1
        buttons$next_que_1c[buttons$cancer_type_live] <- 1
        #buttons$next_que_2a[buttons$cancer_type_live] <- 1
        buttons$next_que_2b[buttons$cancer_type_live] <- 1
        buttons$next_que_3c[buttons$cancer_type_live] <- 0 
      }
      
      } else {
      
      buttons$back <- 1
      
    }
    
    
  })
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("link_prognosis_", i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        buttons$prognosis[i] <- 0
        buttons$back <- 0
        
        if(i %in% buttons$cancer_types_section_1){
          buttons$current_section <- 1
        } else if(i %in% buttons$cancer_types_section_2){
          buttons$current_section <- 2
        }
        
      }

      
    })
    
    observeEvent(input[[paste0("link_omst_graph_", i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        buttons$prognosis[i] <- 1
        buttons$omst_graph[i] <- 0
        buttons$back <- 0
        
        if(i %in% buttons$cancer_types_section_1){
          buttons$current_section <- 1
        } else if(i %in% buttons$cancer_types_section_2){
          buttons$current_section <- 2
        }
        
      }
      
    })
    
    observeEvent(input[[paste0("link_que_1a_", i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        buttons$prognosis[i] <- 1
        buttons$omst_graph[i] <- 1
        buttons$next_que_1a[i] <- 0
        buttons$back <- 0
        
        if(i %in% buttons$cancer_types_section_1){
          buttons$current_section <- 1
        } else if(i %in% buttons$cancer_types_section_2){
          buttons$current_section <- 2
        }
        
      }
      
    })
    
    observeEvent(input[[paste0("link_que_1b_", i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        if(i %in% cancer_types_with_screening_programmes){
          
          buttons$prognosis[i] <- 1
          buttons$omst_graph[i] <- 1
          buttons$next_que_1a[i] <- 1
          buttons$next_que_1b[i] <- 0
          buttons$back <- 0
          
          if(i %in% buttons$cancer_types_section_1){
            buttons$current_section <- 1
          } else if(i %in% buttons$cancer_types_section_2){
            buttons$current_section <- 2
          }
          
        }
        
      }
      
    })
    
    observeEvent(input[[paste0("link_que_1c_", i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        buttons$prognosis[i] <- 1
        buttons$omst_graph[i] <- 1
        buttons$next_que_1a[i] <- 1
        buttons$next_que_1b[i] <- 1
        buttons$next_que_1c[i] <- 0
        buttons$back <- 0
        
        if(i %in% buttons$cancer_types_section_1){
          buttons$current_section <- 1
        } else if(i %in% buttons$cancer_types_section_2){
          buttons$current_section <- 2
          buttons$next_que_2a[i] <-1
          buttons$next_que_2b[i] <-1
        }
        
      }
      
    })
    
    observeEvent(input[[paste0("link_que_3_", i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        buttons$prognosis[i] <- 1
        buttons$omst_graph[i] <- 1
        buttons$next_que_1a[i] <- 1
        buttons$next_que_1b[i] <- 1
        buttons$next_que_1c[i] <- 1
        #buttons$next_que_2a[i] <- 1
        buttons$next_que_2b[i] <- 1
        #buttons$next_que_3c[i] <- 0
        buttons$back <- 0
        
        if(i %in% buttons$cancer_types_section_1){
          buttons$current_section <- 1
        } else if(i %in% buttons$cancer_types_section_2){
          buttons$current_section <- 2
        }
        
      }
      
    })
    
    observeEvent(input[["link_back"]],{
        
      if(i %in% buttons$cancer_types_section_1_2){
        buttons$back <- 1
      }
      
      })
    
    observeEvent(input[[paste0("prognosis_",i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        buttons$prognosis[i] <- 1
      }
        
      })
    observeEvent(input[[paste0("omst_graph_",i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        buttons$omst_graph[i] <- 1
      }
        
      })
      
    
  })
  
 
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("link_que_2b_", i)]],{
      
      if(i %in% buttons$cancer_types_section_1){
        
        buttons$prognosis[i] <- 1
        buttons$omst_graph[i] <- 1
        buttons$next_que_1a[i] <- 1
        buttons$next_que_1b[i] <- 1
        buttons$next_que_1c[i] <- 1
        buttons$next_que_2b[i] <- 0
        buttons$back <- 0
        
        buttons$current_section <- 1
        
      }
      
    })
    
  })
  
  observeEvent(input$section_3,{
    
    buttons$back <- 0
    buttons$section_3 <- 1
    buttons$current_section  <- 3
    buttons$cancer_type_live <- 12
    
  })
  
  lapply(X = 12:21, FUN = function(i){
    
    observeEvent(input[[paste0("elici_section_3_",i)]],{
      
      que_name <- cancer_types[i]
      elici_section_3[[que_name]] <- input[[paste0("elici_section_3_",i)]]
      
    })
    
    observeEvent(input[[paste0("next_que_section_3_", i)]],{
      
      #if they've answered the questions, save the answer
      if(length(elici_section_3[[que_name]]) > 0){
        save_sec_3 <- c(buttons$expert_id, que_name, elici_section_3[[que_name]])
        save_sec_3_colnames <- c("expert_id", "cancer_type", paste0("cancert_type_", 1:length(elici_section_3[[que_name]])))
        f_save(save_sec_3, paste0(buttons$expert_id, "_sec_3_", i, ".csv"))
      }
      
      if(i<21){
        buttons$cancer_type_live <- i+1
      } else {
        showModal(modalDialog (strong("This is the end of the elicitation. Thank you for taking part!", br(), "You can close the window now."), size="l"))
      }

      
    })
    
  })
  

  
  # when an expert enters or updates their plausible range
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("show_plot_1a_",i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
      
      que_name <- cancer_types[i]
      
      # save inputs as reactive values
      elici_minis_1a[[que_name]] <- input[[paste0("min_1a_",i)]]
      elici_maxis_1a[[que_name]] <- input[[paste0("max_1a_",i)]]
      
      #check whether plausible range is within parameter limits and elici_minis < elici_maxis
      condition_1a <- f_cond_min_max(elici_minis_1a[[que_name]], elici_maxis_1a[[que_name]], 0, NA) #remove latter two arguments for OMST
      
      if(condition_1a == 1){
        
        show_plot_1a[[que_name]] <- 1 # show plot
        enter_plot_1a[[que_name]] <- 0 # do not show feedback on expert's plot
        
        # update plot parameters
        chips_width_1a[[que_name]] <- f_width(elici_minis_1a[[que_name]], elici_maxis_1a[[que_name]], bins)
        chips_lower_1a[[que_name]] <- f_lower(elici_minis_1a[[que_name]], chips_width_1a[[que_name]], 0) # remove last argument for OMST
        chips_upper_1a[[que_name]] <- f_upper(elici_maxis_1a[[que_name]], chips_width_1a[[que_name]], NA) #remove last argument for OMST
        chips_nbins_1a[[que_name]] <- f_nbins(chips_lower_1a[[que_name]], chips_upper_1a[[que_name]], chips_width_1a[[que_name]])
        chips_nchip_1a[[que_name]] <- 2 * chips_nbins_1a[[que_name]]
        chips_nhigh_1a[[que_name]] <- 2 * chips_nbins_1a[[que_name]]
        chips_lbins_1a[[que_name]] <- f_lbins(chips_lower_1a[[que_name]], chips_upper_1a[[que_name]], chips_width_1a[[que_name]])
        chips_rbins_1a[[que_name]] <- f_rbins(chips_lower_1a[[que_name]], chips_upper_1a[[que_name]], chips_width_1a[[que_name]])
        chips_value_1a[[que_name]] <- chips_rbins_1a[[que_name]]
        chips_chips_1a[[que_name]] <- rep(0,chips_nbins_1a[[que_name]])
        
      } else {
        
        # if condition == 0, who error message
        showModal(modalDialog (strong("Please make sure you've entered the minimum and the
               maximum, and that the minimum is lower than the maximum."), size="l"))
        
      }
      
      }
      
    })
    
  }
  )
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("show_plot_1b_",i)]],{
      
      if(i %in% cancer_types_with_screening_programmes){
      
      que_name <- cancer_types[i]
      
      #updateNavlistPanel(session, inputId = "elicitation_questions", selected = "Question 1b: OMST with screening")
      updateTabsetPanel(session, inputId = "elicitation_questions", selected = "Question 1b: OMST with screening")
      
      # save inputs as reactive values
      elici_minis_1b[[que_name]] <- input[[paste0("min_1b_",i)]]
      elici_maxis_1b[[que_name]] <- input[[paste0("max_1b_",i)]]
      
      #check whether plausible range is within parameter limits and elici_minis < elici_maxis
      condition_1b <- f_cond_min_max(elici_minis_1b[[que_name]], elici_maxis_1b[[que_name]], 0, NA) #remove latter two arguments for OMST
      
      if(condition_1b==1){
        
        show_plot_1b[[que_name]] <- 1 # show plot
        enter_plot_1b[[que_name]] <- 0 # do not show feedback on expert's plot
        
        # update plot parameters
        chips_width_1b[[que_name]] <- f_width(elici_minis_1b[[que_name]], elici_maxis_1b[[que_name]], bins)
        chips_lower_1b[[que_name]] <- f_lower(elici_minis_1b[[que_name]], chips_width_1b[[que_name]], 0) # remove last argument for OMST
        chips_upper_1b[[que_name]] <- f_upper(elici_maxis_1b[[que_name]], chips_width_1b[[que_name]], NA) #remove last argument for OMST
        chips_nbins_1b[[que_name]] <- f_nbins(chips_lower_1b[[que_name]], chips_upper_1b[[que_name]], chips_width_1b[[que_name]])
        chips_nchip_1b[[que_name]] <- 2 * chips_nbins_1b[[que_name]]
        chips_nhigh_1b[[que_name]] <- 2 * chips_nbins_1b[[que_name]]
        chips_lbins_1b[[que_name]] <- f_lbins(chips_lower_1b[[que_name]], chips_upper_1b[[que_name]], chips_width_1b[[que_name]])
        chips_rbins_1b[[que_name]] <- f_rbins(chips_lower_1b[[que_name]], chips_upper_1b[[que_name]], chips_width_1b[[que_name]])
        chips_value_1b[[que_name]] <- chips_rbins_1b[[que_name]]
        chips_chips_1b[[que_name]] <- rep(0,chips_nbins_1b[[que_name]])
        
      } else {
        
        # if condition == 0, who error message
        showModal(modalDialog (strong("Please make sure you've entered the minimum and the
               maximum, and that the minimum is lower than the maximum."), size="l"))
        
      }
      
      }
      
    })
    
  })
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("show_plot_3c_",i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        
        #updateTabsetPanel(session, inputId = "elicitation_questions", selected = "Question 3: OMST in ctDNA cancers")
        
        # save inputs as reactive values
        elici_minis_3c[[que_name]] <- input[[paste0("min_3c_",i)]]
        elici_maxis_3c[[que_name]] <- input[[paste0("max_3c_",i)]]
        
        #check whether plausible range is within parameter limits and elici_minis < elici_maxis
        condition_3c <- f_cond_min_max(elici_minis_3c[[que_name]], elici_maxis_3c[[que_name]], 0, NA) #remove latter two arguments for OMST
        
        if(condition_3c==1){
          
          show_plot_3c[[que_name]] <- 1 # show plot
          enter_plot_3c[[que_name]] <- 0 # do not show feedback on expert's plot
          
          # update plot parameters
          chips_width_3c[[que_name]] <- f_width(elici_minis_3c[[que_name]], elici_maxis_3c[[que_name]], bins)
          chips_lower_3c[[que_name]] <- f_lower(elici_minis_3c[[que_name]], chips_width_3c[[que_name]], 0) # remove last argument for OMST
          chips_upper_3c[[que_name]] <- f_upper(elici_maxis_3c[[que_name]], chips_width_3c[[que_name]], NA) #remove last argument for OMST
          chips_nbins_3c[[que_name]] <- f_nbins(chips_lower_3c[[que_name]], chips_upper_3c[[que_name]], chips_width_3c[[que_name]])
          chips_nchip_3c[[que_name]] <- 2 * chips_nbins_3c[[que_name]]
          chips_nhigh_3c[[que_name]] <- 2 * chips_nbins_3c[[que_name]]
          chips_lbins_3c[[que_name]] <- f_lbins(chips_lower_3c[[que_name]], chips_upper_3c[[que_name]], chips_width_3c[[que_name]])
          chips_rbins_3c[[que_name]] <- f_rbins(chips_lower_3c[[que_name]], chips_upper_3c[[que_name]], chips_width_3c[[que_name]])
          chips_value_3c[[que_name]] <- chips_rbins_3c[[que_name]]
          chips_chips_3c[[que_name]] <- rep(0,chips_nbins_3c[[que_name]])
          
        } else {
          
          # if condition == 0, who error message
          showModal(modalDialog (strong("Please make sure you've entered the minimum and the
               maximum, and that the minimum is lower than the maximum."), size="l"))
          
        }
        
      }
      
    })
    
  })
  
  # when an expert enters or updates their plot
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("enter_plot_1a_",i)]],{
      
      if (i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        
        #updateNavlistPanel(session, inputId = "elicitation_questions", selected = "Question 1a: OMST without screening")
        # updateTabsetPanel(session, inputId = "elicitation_questions", selected = "Question 1a: OMST without screening")
        
        if(round(chips_nchip_1a[[que_name]]-sum(chips_chips_1a[[que_name]]),digits=0) > 0 ){
          
          # error mesage if not all chips are used
          showModal(modalDialog (strong("Please make sure you use all the available chips before proceeding."), size="l"))
          
        } else {
          
          enter_plot_1a[[que_name]] <- 1
          
        } 
        
      }
      
    })
    
  })
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("enter_plot_1b_",i)]],{
      
      if (i %in% cancer_types_with_screening_programmes){
        
        que_name <- cancer_types[i]
        
        #updateNavlistPanel(session, inputId = "elicitation_questions", selected = "Question 1b: OMST with screening")
        updateTabsetPanel(session, inputId = "elicitation_questions", selected = "Question 1b: OMST with screening")
        
        if(round(chips_nchip_1b[[que_name]]-sum(chips_chips_1b[[que_name]]),digits=0) > 0 ){
          
          # error mesage if not all chips are used
          showModal(modalDialog (strong("Please make sure you use all the available chips before proceeding."), size="l"))
          
        } else {
          
          enter_plot_1b[[que_name]] <- 1
          
        }
        
      }
        
      })
    
    })
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("enter_plot_3c_",i)]],{
      
      if (i %in% buttons$cancer_types_section_1_2){
      
        que_name <- cancer_types[i]
        
        #updateNavlistPanel(session, inputId = "elicitation_questions", selected = "Question 3: OMST in ctDNA cancers")
        updateTabsetPanel(session, inputId = "elicitation_questions", selected = "Question 3: OMST in ctDNA cancers")
        
        if(round(chips_nchip_3c[[que_name]]-sum(chips_chips_3c[[que_name]]),digits=0) > 0 ){
          
          # error mesage if not all chips are used
          showModal(modalDialog (strong("Please make sure you use all the available chips before proceeding."), size="l"))
          
        } else {
          
          enter_plot_3c[[que_name]] <- 1
          
        }
      
      }
      
    })
    
  })
  
  # conditions_1a <- lapply(X = 1:21, FUN = function(i){
  #   
  #   reactive({
  #     
  #     if (i %in% buttons$cancer_types_section_1_2){
  #       
  #       ifelse(sum(chips_chips_1a[[buttons$cancer_types[i]]]) < chips_nchip_1a[[buttons$cancer_types[i]]],
  #              0,
  #              1)
  #       
  #     }
  #     
  #   })
  #   
  # })
  # 
  # conditions_1b <- lapply(X = 1:21, FUN = function(i){
  #   
  #   reactive({
  #     
  #     if(i %in% cancer_types_with_screening_programmes){
  #       
  #       ifelse(sum(chips_chips_1b[[buttons$cancer_types_section_1[i]]]) < chips_nchip_1b[[buttons$cancer_types_section_1[i]]],
  #              0,
  #              1)
  #       
  #     }
  #     
  #     })
  #   
  #   })
  # 
  # 
  # conditions_3c <- lapply(X = 1:21, FUN = function(i){
  #   
  #   reactive({
  #     
  #     if (i %in% buttons$cancer_types_section_1_2){
  #       
  #       ifelse(sum(chips_chips_3c[[buttons$cancer_types[i]]]) < chips_nchip_3c[[buttons$cancer_types[i]]],
  #              0,
  #              1)
  #       
  #     }
  #     
  #   })
  #   
  # })
  # 
  # names(conditions_1a) <- paste0('que_', 1:length(buttons$cancer_types_section_1_2))
  # names(conditions_1b) <- paste0('que_', which(buttons$cancer_types_section_1 %in% cancer_types_with_screening_programmes))
  # names(conditions_3c) <- paste0('que_', 1:length(buttons$cancer_types_section_1_2))  
  
  # when expert is ready to move onto next question - save answers and change tab
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("next_que_1a_",i)]], {
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        
        if(round(chips_nchip_1a[[que_name]] - sum(chips_chips_1a[[que_name]]), digits=0) > 0){
        #if(sum(chips_chips_1a[[que_name]]) < chips_nchip_1a[[que_name]]){
          
          # show error message if not all chips are used
          showModal(modalDialog(strong("Please make sure you use all the available chips before proceeding."), size="l"))
          
        } else {
          
          tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
          mode_omst_all[[que_name]] <- round(f_get_mode_from_histogram(chips_chips_1a[[que_name]], chips_lbins_1a[[que_name]], chips_rbins_1a[[que_name]]), digits = 1)
          summary_table$d1[tab_row,2] <- paste0(mode_omst_all[[que_name]], " (", elici_minis_1a[[que_name]], " - ", elici_maxis_1a[[que_name]], ")")
          buttons$next_que_1a[i] <- 1
          #save
          save_que_1a <- c(buttons$expert_id, elici_minis_1a[[que_name]], elici_maxis_1a[[que_name]], chips_width_1a[[que_name]], chips_rbins_1a[[que_name]], chips_chips_1a[[que_name]], comments_1a[[que_name]], mode_omst_all[[que_name]])
          save_que_1a_colnames <- c("expert_id", "elici_minis", "elici_maxis", "bin_width", paste0("rbins_", 1:length(chips_rbins_1a[[que_name]])), paste0("chips_", 1:length(chips_chips_1a[[que_name]])), "comments", "mode_omst_all")
          f_save(save_que_1a, save_que_1a_colnames, paste0(buttons$expert_id, "_que_1a_", i, ".csv"))
          
          
        }
        
      }
      
    })
    
  })
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("next_que_1b_",i)]], {
      
      if(i %in% cancer_types_with_screening_programmes){
        
        que_name <- cancer_types[i]
        
        if(round(chips_nchip_1b[[que_name]] - sum(chips_chips_1b[[que_name]]), digits=0) > 0){
        # if(sum(chips_chips_1b[[que_name]]) < chips_nchip_1b[[que_name]]){
          
          # show error message if not all chips are used
          showModal(modalDialog(strong("Please make sure you use all the available chips before proceeding."), size="l"))
          
        } else {
          
          tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
          mode_omst_all[[que_name]] <- round(f_get_mode_from_histogram(chips_chips_1b[[que_name]], chips_lbins_1b[[que_name]], chips_rbins_1b[[que_name]]), digits = 1)
          summary_table$d1[tab_row,3] <- paste0(mode_omst_all[[que_name]], " (", elici_minis_1b[[que_name]], " - ", elici_maxis_1b[[que_name]], ")")
          buttons$next_que_1b[i] <- 1
          #save
          save_que_1b <- c(buttons$expert_id, elici_minis_1b[[que_name]], elici_maxis_1b[[que_name]], chips_width_1b[[que_name]], chips_rbins_1b[[que_name]], chips_chips_1b[[que_name]], comments_1b[[que_name]], mode_omst_all[[que_name]])
          save_que_1b_colnames <- c("expert_id", "elici_minis", "elici_maxis", "bin_width", paste0("rbins_", 1:length(chips_rbins_1b[[que_name]])), paste0("chips_", 1:length(chips_chips_1b[[que_name]])), "comments", "mode_omst_all")
          f_save(save_que_1b, save_que_1b_colnames, paste0(buttons$expert_id, "_que_1b_", i, ".csv"))
          
        }
        
      }
      
      })
    
    })
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("next_que_1c_",i)]], {
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        elici_1c[[que_name]] <- input[[paste0("elicit_1c_",i)]]
        
        if(length(elici_1c[[que_name]]) > 0){
          
          tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
          omst_ctDNA[[que_name]] <- f_derive_omst_ctDNA(mode_omst_all[[que_name]], elici_1c[[que_name]], ct_DNA_sensitivity[i,"total"])
          summary_table$d1[tab_row,4] <- elici_1c[[que_name]]
          buttons$next_que_1c[i] <- 1
          #add save
          save_que_1c <- c(buttons$expert_id, mode_omst_all[[que_name]], elici_1c[[que_name]], omst_ctDNA[[que_name]]) #add which distribution was fitted
          save_que_1c_colnames <- c("expert_id", "mode_omst_all", "elici_1c", "omst_ctDNA")
          f_save(save_que_1c, save_que_1c_colnames, paste0(buttons$expert_id, "_que_1c_", i, ".csv"))
          
          
        } else {
          
          showModal(modalDialog ("Please make sure you have entered a value before proceeding.", size="l"))
          
        }
        
      }
      
      })
    
    })
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("next_que_2a_",i)]], {
      
      if(i %in% buttons$cancer_types_section_1){
        
        que_name <- cancer_types[i]
        elici_2a[[que_name]] <- input[[paste0("elicit_2a_",i)]]
        
        if(length(elici_2a[[que_name]]) > 0){
          
          tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
          omst_late[[que_name]] <- round((mode_omst_all[[que_name]] - (1 - proportion_diagnosed_in_late_stage[i]/100) * elici_2a[[que_name]]) / (proportion_diagnosed_in_late_stage[i]/100), digits = 1)
          summary_table$d1[tab_row,5] <- elici_2a[[que_name]]
          buttons$next_que_2a[i] <- 1
          #add save?
          
        } else {
          
          showModal(modalDialog ("Please make sure you have entered a value before proceeding.", size="l"))
          
        }
        
      }
      
    })
    
  })
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("validate_que_2b_",i)]], {
      
      if(i %in% buttons$cancer_types_section_1){
        
        que_name <- cancer_types[i]
        elici_2b[[que_name]] <- input[[paste0("elicit_2b_",i)]]
        
        if(length(elici_2b[[que_name]]) > 0){
          
          tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
          lmst[[que_name]] <- omst_late[[que_name]] - elici_2b[[que_name]]
          summary_table$d1[tab_row,6] <- elici_2b[[que_name]]
          summary_table$d1[tab_row,7] <- lmst[[que_name]]
          
          buttons$validate_que_2b[i] <- 1
          
        } else {
          
          showModal(modalDialog ("Please make sure you have entered a value before proceeding.", size="l"))
          
        }
        
      }
      
      })
    
    })
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("next_que_2b_",i)]], {
      
      if(i %in% buttons$cancer_types_section_1){
        
        que_name <- cancer_types[i]
        
         buttons$next_que_2b[i] <- 1
         # save
         save_que_2 <- c(buttons$expert_id, mode_omst_all[[que_name]], elici_2a[[que_name]], omst_late[[que_name]], elici_2b[[que_name]], lmst[[que_name]]) #add which distribution was fitted
         save_que_2_colnames <- c("expert_id", "mode_omst_all", "elici_2a", "omst_late", "elici_2b", "lmst")
         f_save(save_que_2, save_que_2_colnames, paste0(buttons$expert_id, "_que_2_", i, ".csv"))
        
      }
      
    })
    
  })
  
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("view_ctDNA_evidence_",i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        buttons$view_ctDNA_evidence[i] <- 1
        #add save
        
      }
      
    })
    
  })
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("elicit_3a_",i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        elici_3a[[que_name]] <- input[[paste0("elicit_3a_",i)]]
        
      }
      
    })
    
    observeEvent(input[[paste0("next_que_3a_",i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        
        if(length(elici_3a[[que_name]]) > 0){
          
          buttons$next_que_3a[i] <- 1
          
          if(elici_3a[[que_name]] == 0){
            
            #buttons$next_que_3b[i] <- 1
            buttons$next_que_3c[i] <- 0
            
          } else {
            
            buttons$next_que_3b[i] <- 0
            buttons$next_que_3c[i] <- 0
            
          }
          
          #add save
          
        } else {
          
          showModal(modalDialog ("Please make sure you have entered a value before proceeding.", size="l"))
          
        }
        
      }
      
    })
    
  })
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("elicit_3b_",i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        elici_3b[[que_name]] <- input[[paste0("elicit_3b_",i)]]
        
      }
      
    })
    
    observeEvent(input[[paste0("next_que_3b_",i)]],{
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        #elici_3b[[que_name]] <- input[[paste0("elicit_3b_",i)]]
        
        if(length(elici_3b[[que_name]]) > 0){
          
          buttons$next_que_3b[i] <- 1
          
          if(elici_3b[[que_name]] == 1){
            
            tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
            summary_table$d1[tab_row,8] <- omst_ctDNA[[que_name]]
            buttons$back <- 1
            
            # save
            save_que_3 <- c(buttons$expert_id, elici_3a[[que_name]], elici_3b[[que_name]], omst_ctDNA[[que_name]]) #add which distribution was fitted
            save_que_3_colnames <- c("expert_id", "elici_3a", "elici_3b", "omst_ctDNA")
            f_save(save_que_3, save_que_3_colnames, paste0(buttons$expert_id, "_que_3_", i, ".csv"))
            
            
            # if last cancer type in section 2, go to section 3
            if(i == tail(buttons$cancer_types_section_1, n = 1)){
              
              buttons$current_section <- 2
              
            } else if (length(buttons$cancer_types_section_2) > 0){
              
              if(i == tail(buttons$cancer_types_section_2, n = 1)){
                buttons$current_section <- 3
              }
              
            }
            
          } else {
            
            buttons$next_que_3c[i] <- 0
            
          }
          #add save
          
        } else {
          
          showModal(modalDialog ("Please make sure you have entered a value before proceeding.", size="l"))
          
        }
        
      }
      
    })
    
  })
  
  lapply(X = 1:21, FUN = function(i){
    
    observeEvent(input[[paste0("next_que_3c_",i)]], {
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        
        if(round(chips_nchip_3c[[que_name]] - sum(chips_chips_3c[[que_name]]), digits=0) > 0){
        # if(sum(chips_chips_3c[[que_name]]) < chips_nchip_3c[[que_name]]){
          
          # show error message if not all chips are used
          showModal(modalDialog (strong("Please make sure you use all the available chips before proceeding."), size="l"))
          
          
        } else {
          
          mode_omst_ctDNA[[que_name]] <- round(f_get_mode_from_histogram(chips_chips_3c[[que_name]], chips_lbins_3c[[que_name]], chips_rbins_3c[[que_name]]), digits = 1)
          
          tab_row <- which(summary_table$d1[,1] == cancer_type_labels[i])
          summary_table$d1[tab_row,8] <- paste0(mode_omst_ctDNA[[que_name]], " (", elici_minis_3c[[que_name]], " - ", elici_maxis_3c[[que_name]], ")")
          buttons$next_que_3c[i] <- 1
          buttons$back <- 1
          
          # save
          save_que_3 <- c(buttons$expert_id, elici_3a[[que_name]], ifelse(length(elici_3b[[que_name]]) > 0, elici_3b[[que_name]], "NA"), omst_ctDNA[[que_name]], elici_minis_3c[[que_name]], elici_maxis_3c[[que_name]], chips_width_3c[[que_name]], chips_rbins_3c[[que_name]], chips_chips_3c[[que_name]], comments_3c[[que_name]])
          save_que_3_colnames <- c("expert_id", "elici_3a", "elici_3b", "omst_ctDNA", "elici_minis", "elici_maxis", "bin_width", paste0("rbins_", 1:length(chips_rbins_3c[[que_name]])), paste0("chips_", 1:length(chips_chips_3c[[que_name]])), "comments")
          f_save(save_que_3, save_que_3_colnames, paste0(buttons$expert_id, "_que_3_", i, ".csv"))
          
          # if last cancer type in section 2, go to section 3
          if(i == tail(buttons$cancer_types_section_1, n = 1)){
            
            buttons$current_section <- 2
            
          } else if(length(buttons$cancer_types_section_2)>0){
            
            if(i == tail(buttons$cancer_types_section_2, n = 1)){
              
              buttons$current_section <- 3
              
            }
            
          }
          #add save
          
        }
        
      }
      
    })
    
  })
  

  
  ##### summary table #####
  
  output$summary_table <- DT::renderDataTable(
    
    if(buttons$enter_about_you == 0){
      
      datatable(as.data.frame(matrix(rep(0,4), nrow = 2, ncol = 2)), options = list(paging = FALSE, dom = 't'), selection = list(target = 'cell'))
       
    } else {
      
      datatable(summary_table$d1, options = list(paging = FALSE, dom = 't'), selection = list(target = 'cell'))
     
    }
    
    )
      

  ##### chips and bins plots #####
  
  lapply(X = 1:21, FUN = function(i){
  
    output[[paste0("plot_1a_",i)]]<- renderPlot({
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        
        f_plot_the_plot(chips_lower_1a[[que_name]], chips_upper_1a[[que_name]],
                      chips_nhigh_1a[[que_name]], chips_nbins_1a[[que_name]],
                      chips_lbins_1a[[que_name]], chips_rbins_1a[[que_name]],
                      chips_chips_1a[[que_name]])
        
      }
      }, height = 330, width = 400)#530
    
    observeEvent(input[[paste0("location_1a_",i)]], { #add red chips to bins
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        
        chips_chips_1a[[que_name]] <- f_add_chips(chips_nbins_1a[[que_name]],
                                                  chips_lbins_1a[[que_name]],
                                                  chips_rbins_1a[[que_name]],
                                                  chips_nchip_1a[[que_name]],
                                                  chips_chips_1a[[que_name]],
                                                  input[[paste0("location_1a_",i)]]$x,
                                                  input[[paste0("location_1a_",i)]]$y)
        
      }
      
    })
    
  })
  
  lapply(X = 1:21, FUN = function(i){
    
    output[[paste0("plot_1b_",i)]]<- renderPlot({
      
      if(i %in% cancer_types_with_screening_programmes){
        
        que_name <- cancer_types[i]
        
        f_plot_the_plot(chips_lower_1b[[que_name]], chips_upper_1b[[que_name]],
                        chips_nhigh_1b[[que_name]], chips_nbins_1b[[que_name]],
                        chips_lbins_1b[[que_name]], chips_rbins_1b[[que_name]],
                        chips_chips_1b[[que_name]])
        
      }
      }, height = 330, width = 400)#530
      
      observeEvent(input[[paste0("location_1b_",i)]], { #add red chips to bins
        
        if(i %in% cancer_types_with_screening_programmes){
          
          que_name <- cancer_types[i]
          
          chips_chips_1b[[que_name]] <- f_add_chips(chips_nbins_1b[[que_name]],
                                                    chips_lbins_1b[[que_name]],
                                                    chips_rbins_1b[[que_name]],
                                                    chips_nchip_1b[[que_name]],
                                                    chips_chips_1b[[que_name]],
                                                    input[[paste0("location_1b_",i)]]$x,
                                                    input[[paste0("location_1b_",i)]]$y)
        }
        
        
      })
    
  })
  
  lapply(X = 1:21, FUN = function(i){
    
    output[[paste0("plot_3c_",i)]]<- renderPlot({
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        
        f_plot_the_plot(chips_lower_3c[[que_name]], chips_upper_3c[[que_name]],
                        chips_nhigh_3c[[que_name]], chips_nbins_3c[[que_name]],
                        chips_lbins_3c[[que_name]], chips_rbins_3c[[que_name]],
                        chips_chips_3c[[que_name]])
        
      }
      
    }, height = 330, width = 400)#530
    
    observeEvent(input[[paste0("location_3c_",i)]], { #add red chips to bins
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        que_name <- cancer_types[i]
        
        chips_chips_3c[[que_name]] <- f_add_chips(chips_nbins_3c[[que_name]],
                                                  chips_lbins_3c[[que_name]],
                                                  chips_rbins_3c[[que_name]],
                                                  chips_nchip_3c[[que_name]],
                                                  chips_chips_3c[[que_name]],
                                                  input[[paste0("location_3c_",i)]]$x,
                                                  input[[paste0("location_3c_",i)]]$y)
        
      }
                                                
    })
    
  })
  
  
  
  #### UI ####
 
  output$about_you_answer_all <- renderUI({
    
      tagList(div("Please ensure you have answered all questions."))
    
  })
  
  output$main_ui <- renderUI({
    
    fluidPage(
      
      mainPanel(
        
        tagList(div(
        
        if(buttons$enter_unique_id == 0){
          
          tagList(div(
            
            strong("Please enter the unique identifier you've been provided, then click on 'Enter'."), br(),
            fluidRow(
              column(3, numericInput("expert_id", "", NULL, min = 0)),br(),
              column(1, offset = 6, actionButton("enter_unique_id", "Enter", width='120px', style="background-color: lightgrey")))
            
          ))
          
        } else if (buttons$consent == 0){
          
          tagList(div(
            #includeHTML("www/text_consent.htm"), br(),
            h2("Your consent"),br(),
            "Please check that you are satisfied with the following statements before proceeding.", br(), br(),
            tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("I have received enough information about the study."))), br(),
            tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("I have had the opportunity to ask questions and discuss this study."))), br(),
            tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("I have received satisfactory answers to all of my questions."))), br(),
            tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("I understand my participation in the study is voluntary and that I am free to withdraw from the study up to 2 weeks post-interview without having to give a reason for withdrawing, and my responses will be deleted."))), br(),
            tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("I understand that any information I provide, including personal data, will be kept confidential, stored securely and only accessed by those carrying out the study."))), br(),
            tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("I understand that any information I provide may be included in published documents but all information will be anonymised."))), br(),
            tags$li(div(style = "display: inline-block; vertical-align:middle; width: 900px;", HTML("I agree to take part in this study."))),br(),br(),
            fluidRow(
              column(9, p(style="font-size:90%;", "Please click on 'Next' to continue")),
              column(1, actionButton("enter_consent", "Next", width='120px', style="background-color: lightgrey"))
            )
          ))
          
        } else if(buttons$next_que_test_1 == 0) {
          
          tagList(div(
            f_chips_and_bins_test(elici_minis_training[["test1"]],
                                elici_maxis_training[["test1"]],
                                chips_nchip_training[["test1"]],
                                chips_chips_training[["test1"]],
                                chips_lbins_training[["test1"]],
                                chips_rbins_training[["test1"]],
                                show_plot_training[["test1"]],
                                enter_plot_training[["test1"]])
            
          ))
          
        } else if(buttons$enter_about_you == 0) {
          
          tagList(div(
            h2("About you"), br(),
            strong("1. Do you specialise in any specific type(s) of cancer?"), br(),
            em("Please select the option that best describes your professional focus."), br(),
            radioButtons("about_you_1", " ", choices = c(
              "Yes – I specialise in one or more specific cancer types" = 2,
              "No – I am a generalist (e.g. general oncologist or primary care provider)" = 1), selected = about_you$que1), br(),br(),
            
            strong("2. What is your current clinical specialism or area of practice?"), br(),
            textInput("about_you_2", label = " ", value = about_you$que2, width = "800px"), br(), br(),
            
            strong("3. What is your current clinical affiliation (e.g. hospital, clinic, institution)?"), br(),
            textInput("about_you_3", label = " ", value = about_you$que3, width = "800px"), br(), br(),
            
            strong("4. How long have you been working in your current specialism?"), br(),
            em("Please enter the number of years (or approximate duration)"), br(),
            textInput("about_you_4", label = " ", value = about_you$que4, width = "800px"), br(), br(),
            
            strong("5. Do you currently hold an academic role?"),br(),
            radioButtons("about_you_5", " ", choices = c(
              "Yes, I hold a formal academic position (e.g. lecturer, professor, research fellow)" = 1,
              "No, I do not currently hold an academic role" = 0), selected = about_you$que5), br(), br(),
            
            strong("6. If applicable, please provide your academic affiliation (e.g. university, research institute):"), br(),
            textInput("about_you_6", label = " ", value = about_you$que6, width = "800px"), br(), br(),
            
            ifelse(input$about_you_1 == 2,
                   tagList(div(
                     strong("7. For which of the following cancer type(s) do you feel confident providing estimates or insights into sojourn times?"),br(),
                     em("Please select all that apply"), br(),
                     checkboxGroupInput("about_you_7"," ",c(
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
                       "Pancreas" = 11, 
                       "Anus" = 12,
                       "Bladder" = 13,
                       "Gallbladder" = 14,
                       "Kidney" = 15,
                       "Melanoma" = 16,
                       "Sarcoma" = 17,
                       "Stomach" = 18,
                       "Thyroid" = 19,
                       "Urothelial tract" = 20,
                       "Uterus" = 21), selected = about_you[["que7"]])
                   )),
                   tagList(div(""))
          ),
          fluidRow(
            column(9, p(style="font-size:90%;", "Please click on 'Next' to continue")),
            column(1, actionButton("enter_about_you", "Next", width='120px', style="background-color: lightgrey"))), br(), br()
          ))
          
        } else if (buttons$back == 1) {
          
          tagList(div(
            DT::dataTableOutput('summary_table'), br(), br(),
            fluidRow(
              column(9, p(style="font-size:90%;", "Please click on 'Section 3' to proceed to the last part of the exercise.")),
              column(1, actionButton("section_3", "Section 3", width='120px', style="background-color: lightgrey"))
            ), br(), br(),
            
            ))

          
        } else if (buttons$current_section < 3){
          
          tagList(div(
            
            uiOutput(paste0("section_1_2_ques_", buttons$cancer_type_live))
            
          ))
          
        } else {
          
          tagList(div(
            
            uiOutput(paste0("section_3_ques_", buttons$cancer_type_live))

          ))
          
        }
        
      )),
      
      width = 12) # close mainPanel
      ) # close fluidPage
    
  })

  lapply(X = 1:21, FUN = function(i){
    
    output[[paste0("section_1_2_ques_",i)]]<-renderUI({
      
      if(i %in% buttons$cancer_types_section_1_2){
        
        tagList(div(
          hr(),
          fluidRow(
            column(1, actionLink(paste0("link_prognosis_", i), "Cancer prognoses")),
            column(1, actionLink(paste0("link_omst_graph_", i), "OMST literature"), style = 'border-left: 1px'),
            column(2, actionLink(paste0("link_que_1a_", i), "Q1a: OMST - clinical detection"), style = 'border-left: 1px'),
            ifelse(i %in% cancer_types_with_screening_programmes,
                   tagList(div(column(2, actionLink(paste0("link_que_1b_", i), "Q1b: OMST - clinical and screen detection"), style = 'border-left: 1px'))),
                   tagList(div(""))),
            column(2, actionLink(paste0("link_que_1c_", i), "Q1c: Most severe cancers"), style = 'border-left: 1px'),
            ifelse(i %in% buttons$cancer_types_section_1,
                   tagList(div(
                     #column(2, actionLink(paste0("link_que_2a_", i), "Q2a: Cancers diagnosed early"), style = 'border-left: 1px'),
                     column(2, actionLink(paste0("link_que_2b_", i), "Q2: Stage-sojourn times"), style = 'border-left: 1px')
                   )),
                   tagList(div(""))),
            column(1, actionLink(paste0("link_que_3_", i), "Q3: ctDNA cancers"), style = 'border-left: 1px'),
            column(1, actionLink("link_back", "Back to summaries"), style = 'border-left: 1px')
            ),
          # tags$p(div(style = "display: inline-block; vertical-align:middle; width: 115px;", actionLink(paste0("link_prognosis_", i), "Cancer prognoses")),
          #         div(style = "display: inline-block; vertical-align:middle; width: 110px;", actionLink(paste0("link_omst_graph_", i), "OMST literature")),
          #         div(style = "display: inline-block; vertical-align:middle; width: 100px;", actionLink(paste0("link_que_1a_", i), "Q1a: OMST")),
          #         div(style = "display: inline-block; vertical-align:middle; width: 130px;", actionLink(paste0("link_que_1b_", i), "Q1b: OMST with screening")),
          #         div(style = "display: inline-block; vertical-align:middle; width: 130px;", actionLink(paste0("link_que_1c_", i), "Q1c: Most severe cancers")),
          #         div(style = "display: inline-block; vertical-align:middle; width: 130px;", actionLink(paste0("link_que_2a_", i), "Q2a: Cancers diagnosed early")),
          #         div(style = "display: inline-block; vertical-align:middle; width: 130px;", actionLink(paste0("link_que_2b_", i), "Q2b: Cancers diagnosed late")),
          #         div(style = "display: inline-block; vertical-align:middle; width: 115px;", actionLink(paste0("link_que_3_", i), "Q3: ctDNA cancers")),
          #         div(style = "display: inline-block; vertical-align:middle; width: 120px;", actionLink(paste0("link_back"), "Back to summaries"))),
          hr(), br(),
                  
          # fluidRow(column(12,
          #                 tagList(div(actionLink(paste0("link_prognosis_", i), "Cancer prognoses"),
          #                             actionLink(paste0("link_omst_graph_", i), "OMST literature"),
          #                             actionLink(paste0("link_que_1a_", i), "Q1a: OMST"),
          #                             ifelse(cancer_types[i] %in% cancer_types_with_screening_programmes,
          #                                    tagList(div(actionLink(paste0("link_que_1b_", i), "Q1b: OMST with screening"))),
          #                                    tagList(div(""))),
          #                             actionLink(paste0("link_que_1c_", i), "Q1c: Most severe cancers"),
          #                             ifelse(i %in% buttons$cancer_types_section_1,
          #                                    tagList(div(
          #                                      actionLink(paste0("link_que_2a_", i), "Q2a"),
          #                                      actionLink(paste0("link_que_2b_", i), "Cancer prognoses"))),
          #                                    tagList(div(""))),
          #                             actionLink(paste0("link_que_3_", i), "Cancer prognoses"),
          #                             actionLink("link_back", "Back to summaries"))))),
          f_section_1_2_questions(i,
                                  cancer_types[i],
                                  buttons$current_section,
                                  buttons$prognosis[i],
                                  buttons$omst_graph[i],
                                  buttons$next_que_1a[i],
                                  buttons$next_que_1b[i],
                                  buttons$next_que_1c[i],
                                  buttons$next_que_2a[i],
                                  buttons$next_que_2b[i],
                                  buttons$next_que_3a[i],
                                  buttons$next_que_3b[i],
                                  buttons$next_que_3c[i],
                                  buttons$cancer_types[1],
                                  buttons$view_ctDNA_evidence[i],
                                  buttons$validate_que_2b[i],
                                  elici_minis_1a[[cancer_types[i]]],
                                  elici_minis_1b[[cancer_types[i]]],
                                  elici_minis_3c[[cancer_types[i]]],
                                  elici_maxis_1a[[cancer_types[i]]],
                                  elici_maxis_1b[[cancer_types[i]]],
                                  elici_maxis_3c[[cancer_types[i]]],
                                  chips_nchip_1a[[cancer_types[i]]],
                                  chips_nchip_1b[[cancer_types[i]]],
                                  chips_nchip_3c[[cancer_types[i]]],
                                  chips_chips_1a[[cancer_types[i]]],
                                  chips_chips_1b[[cancer_types[i]]],
                                  chips_chips_3c[[cancer_types[i]]],
                                  chips_lbins_1a[[cancer_types[i]]],
                                  chips_lbins_1b[[cancer_types[i]]],
                                  chips_lbins_3c[[cancer_types[i]]],
                                  chips_rbins_1a[[cancer_types[i]]],
                                  chips_rbins_1b[[cancer_types[i]]],
                                  chips_rbins_3c[[cancer_types[i]]],
                                  show_plot_1a[[cancer_types[i]]],
                                  show_plot_1b[[cancer_types[i]]],
                                  show_plot_3c[[cancer_types[i]]],
                                  enter_plot_1a[[cancer_types[i]]],
                                  enter_plot_1b[[cancer_types[i]]],
                                  enter_plot_3c[[cancer_types[i]]],
                                  comments_1a[[cancer_types[i]]],
                                  comments_1b[[cancer_types[i]]],
                                  comments_3c[[cancer_types[i]]],
                                  elici_1c[[cancer_types[i]]],
                                  elici_2a[[cancer_types[i]]],
                                  elici_2b[[cancer_types[i]]],
                                  elici_3a[[cancer_types[i]]],
                                  elici_3b[[cancer_types[i]]],
                                  mode_omst_all[[cancer_types[i]]],
                                  omst_late[[cancer_types[i]]],
                                  lmst[[cancer_types[i]]],
                                  omst_ctDNA[[cancer_types[i]]])
        ))
        
      }
    
  })
    
  })
  

    lapply(X = 12:21, FUN = function(i){
      
      output[[paste0("section_3_ques_",i)]]<-renderUI({
        
        tagList(div(
          
          f_section_3_quesions(i)
          
        ))
        
      })
      })
        
  
  
}


