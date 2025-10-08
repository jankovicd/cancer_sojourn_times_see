
# # total number of elicitation questions
# tot_eli_ques <- length (quantity)
# 
# # list of elicitation questions for use in reactive values and output objects
# eli_que_names <- paste0("que_",1:tot_eli_ques)
# 
# # number of "pages" on the Home tab, used in server.R file
# # to determine when to skip to the next tab
# last_home_page <- 1 + include_consent + include_about_you

if(!exists("dummy_app")){ # if dummy_app doesn't exist (e.g. when deploying the app) assume the app is live

  dummy_app <- FALSE

}

save_method <- "local"

########## functions ##########

###### elicitation ######

#### chips and bins plot parameters ####

# options for bin width in Chips and Bins plots
# used by f_width function to creaste chips_width reactive values
bins <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)

# derives the bins width from expert's plausible range (elicit_minis and elici_maxis)
f_width <- function(elici_minis, elici_maxis, bins)
  (bins[which.min(abs((elici_maxis - elici_minis) / bins - 10))])

# derives the lower limit of the plot x-axis
# based on expert's range (elici_minis), bin width (chips_width)
# and the lower limit of the parameter (lower_limit) entered in manual_inputs.R
f_lower <- function(elici_minis, chips_width, lower_limit)
  {min_with_extra_bin <- (floor(elici_minis / chips_width) * chips_width) - chips_width
  if(!is.na(lower_limit)) {
    min_on_plot <- ifelse(min_with_extra_bin >= lower_limit, min_with_extra_bin, lower_limit)
  } else {
    min_on_plot <- min_with_extra_bin
  }
   min_on_plot
}

# derives the upper limit of the plot x-axis
# based on expert's range (elici_maxis), bin width (chips_width)
# and the upper limit of the parameter (upper_limit) entered in manual_inputs.R
f_upper <- function(elici_maxis, chips_width, upper_limit)
  {max_with_extra_bin <- (ceiling(elici_maxis / chips_width) * chips_width) + chips_width
  if(!is.na(upper_limit)) {
    max_on_plot <- ifelse(max_with_extra_bin <= upper_limit, max_with_extra_bin, upper_limit)
  } else {
    max_on_plot <- max_with_extra_bin
  }
  max_on_plot
}

# total number of bins on the plot
f_nbins <- function(chips_lower, chips_upper, chips_width)
  ((chips_upper - chips_lower) / chips_width)

# derives the lower limit of each bin
# based on the the range presented in the plot (chips_lower and chips_upper, derived using f_lower and f_upper)
# and the bin width (chips_width)
f_lbins <- function(chips_lower, chips_upper, chips_width)
  (seq(from = chips_lower, to = chips_upper - chips_width, by = chips_width))

# derives the upper limit of each bin
# based on the the range presented in the plot (chips_lower and chips_upper, derived using f_lower and f_upper)
# and the bin width (chips_width)
f_rbins <- function(chips_lower, chips_upper, chips_width)
  (seq(from = chips_lower + chips_width, to = chips_upper, by = chips_width))

# total number of chips (twice the number of bins)
f_nchip<-function(nbins)
  (nbins * 2)


#### chips and bins plots ####

# function for creating chips and bins plots
f_plot_the_plot <- function(chips_lower, chips_upper, chips_nhigh, chips_nbins, chips_lbins, chips_rbins, chips_chips)

{
  par(ps = 12, mar = c(4, 0, 0, 0))
  plot(c(chips_lower, chips_upper), c(0, 0),
       xlim = c(chips_lower, chips_upper),
       ylim = c(-1, chips_nhigh), type="l",
       ylab = "",
       xlab = "OMST (years)",
       xaxp = c(chips_lower, chips_upper, chips_nbins),
       yaxt="n")

  for(i in 1:chips_nbins){
    lines(c(chips_lbins[i], chips_lbins[i]),
          c(0, chips_nhigh), lty=3, col=8)
  }

  lines(c(chips_rbins[chips_nbins], chips_rbins[chips_nbins]),
        c(0, chips_nhigh), lty=3, col=8)

  for(i in 1:chips_nhigh){
    lines(c(chips_lower, chips_upper), c(i, i), lty=3, col=8)
  }

  for(i in 1:chips_nbins){
    if(chips_chips[i] > 0){
      rect(rep(chips_lbins[i], chips_chips[i]), c(0 : (chips_chips[i] - 1)),
           rep(chips_rbins[i], chips_chips[i]), c(1 : chips_chips[i]), col = 2)
    }
  }

}

# function for adding chips to bins by clicking on the plot
f_add_chips <- function(chips_nbins, chips_lbins, chips_rbins, chips_nchip, chips_chips, coord_x, coord_y)
{
  for (i in 1:chips_nbins){

    if(coord_x > chips_lbins[i] & coord_x < chips_rbins[i]){
      temp <- chips_chips[i]

      if(coord_y > temp & coord_y < chips_nchip & round(chips_nchip-sum(chips_chips), digits=0) > 0){
        chips_chips[i] <- temp+1
      }
      if(coord_y < temp & round(sum(chips_chips), digits = 0) > 0){
        chips_chips[i] <- temp-1
      }

    }
  }

  chips_chips

}

#### chips and bins training ####

# function for creating chips and bins plots
f_plot_the_plot_test <- function(chips_lower, chips_upper, chips_nhigh, chips_nbins, chips_lbins, chips_rbins, chips_chips)
  
{
  par(ps = 12, mar = c(4, 0, 0, 0))
  plot(c(chips_lower, chips_upper), c(0, 0),
       xlim = c(chips_lower, chips_upper),
       ylim = c(-1, chips_nhigh), type="l",
       ylab = "",
       xlab = "Number of rainy days",
       xaxp = c(chips_lower, chips_upper, chips_nbins),
       yaxt="n")
  
  for(i in 1:chips_nbins){
    lines(c(chips_lbins[i], chips_lbins[i]),
          c(0, chips_nhigh), lty=3, col=8)
  }
  
  lines(c(chips_rbins[chips_nbins], chips_rbins[chips_nbins]),
        c(0, chips_nhigh), lty=3, col=8)
  
  for(i in 1:chips_nhigh){
    lines(c(chips_lower, chips_upper), c(i, i), lty=3, col=8)
  }
  
  for(i in 1:chips_nbins){
    if(chips_chips[i] > 0){
      rect(rep(chips_lbins[i], chips_chips[i]), c(0 : (chips_chips[i] - 1)),
           rep(chips_rbins[i], chips_chips[i]), c(1 : chips_chips[i]), col = 2)
    }
  }
  
}

# # function for adding chips to bins by clicking on the plot
# f_add_chips <- function(chips_nbins, chips_lbins, chips_rbins, chips_nchip, chips_chips, coord_x, coord_y)
# {
#   for (i in 1:chips_nbins){
#     
#     if(coord_x > chips_lbins[i] & coord_x < chips_rbins[i]){
#       temp <- chips_chips[i]
#       
#       if(coord_y > temp & coord_y < chips_nchip & round(chips_nchip-sum(chips_chips), digits=0) > 0){
#         chips_chips[i] <- temp+1
#       }
#       if(coord_y < temp & round(sum(chips_chips), digits = 0) > 0){
#         chips_chips[i] <- temp-1
#       }
#       
#     }
#   }
#   
#   chips_chips
#   
# }


#### feedback on chips and bins ####

# phrasing for feedback on experts' plots
# a,b,c and d represent different values on expert's plots,
# depending on the shape of their histogram
# unit is the quantity unit entered in maual_inputs.R

f_text_hist_feedback1 <- function(a, b)
  (paste0("There is a 98% probability that OMST is between ",
         a, " and ", b, " years"))

f_text_hist_feedback2 <- function(a, b, c, d)
  (paste0("OMST is equally likely to be between ",
         a, " and ", b, " years, as it is to be between ", c, " and ",d, " years"))

f_text_hist_feedback3 <- function(a, b, c)
  (paste0("There is a ", a, "% probability that OMST is between ",
         b, " and ", c, " years"))


f_text_fback_chips <- function(chips_chips,chips_lbins,chips_rbins){

  # function for feedback text for experts' plots
  # e.g. if flat prior, returns f_text_hist_feedback1
  # e.g. if the prior has a mode, returns f_text_hist_feedback3 for
  # the mode, and the range on either side of the mode

  valss<-rep(0,7)
  x<-which(chips_chips!=0) # number of bins with at least one chip

  if (length(x)!=0){

    a<-chips_chips[x]
    b<-length(a)
    c<-chips_lbins[x]
    d<-chips_rbins[x]
    
    y<-which(a==max(a))
    z<-which(diff(y)==1)
    mmode<-y[1:ifelse(length(z)==0,1,length(z)+1)]
    l_mode<-c[mmode[1]]
    r_mode<-d[mmode[length(mmode)]]
    p_mode<-sum(a[mmode])/sum(a)
    p_less<-ifelse(mmode[1]>1,sum(a[1:(mmode[1]-1)])/sum(a),0)
    p_more<-ifelse(mmode[length(mmode)]<b,sum(a[-(1:mmode[length(mmode)])])/sum(a),0)

    if(p_mode==1&length(mmode)==1){
      vec<-1
      valss[1]<-l_mode
      valss[2]<-r_mode
      wording<-tags$li(f_text_hist_feedback1(l_mode,r_mode))
    }

    if(p_mode==1&length(mmode)!=1){

      if(is.integer(length(mmode)/2)){
        vec<-2
        t_ratio<-length(mmode)/2
        valss[1]<-sum(a[1:t_ratio])/sum(a)
        valss[2]<-c[1]
        valss[3]<-d[t_ratio]
        valss[4]<-d[b]

        temptext1<-f_text_hist_feedback1(l_mode,r_mode)
        temptext2<-f_text_hist_feedback2(c[1],d[t_ratio],d[t_ratio],d[b])
        wording<-tagList(div(tags$li(temptext1),
                             tags$li(temptext2)))

      }else{
        vec<-3
        t_ratio<-ceiling(length(mmode)/2)
        valss[1]<-round(sum(a[1:t_ratio])/sum(a),digits=2)
        valss[2]<-c[1]
        valss[3]<-d[t_ratio]
        valss[4]<-d[b]

        temptext1<-f_text_hist_feedback1(l_mode,r_mode)
        temptext2<-f_text_hist_feedback3((valss[1]*100),c[1],d[t_ratio])

        wording<-tagList(div(tags$li(temptext1),
                             tags$li(temptext2)))
      }

    }

    if(p_mode!=1&(p_less==0|p_more==0)){

      temptext1<-f_text_hist_feedback3(round(p_mode*100),l_mode,r_mode)

      if(p_less!=0){
        temptext2<-f_text_hist_feedback3(round(p_less*100),c[1],l_mode)
      }else{
        temptext2<-f_text_hist_feedback3(round(p_more*100),r_mode,d[b])
      }

      wording<-tagList(div(tags$li(temptext1),
                           tags$li(temptext2)))

    }

    if(p_less!=0&p_more!=0){

      temptext2<-f_text_hist_feedback3(round(p_mode*100),l_mode,r_mode)

      temptext1<-f_text_hist_feedback3(round(p_less*100),c[1],l_mode)

      temptext3<-f_text_hist_feedback3(round(p_more*100),r_mode,d[b])

      wording<-tagList(div(tags$li(temptext1),
                           tags$li(temptext2),
                           tags$li(temptext3)))

    }

    wording

  }

}


#### conditions ####

f_cond_min_max <- function(min, max, lower_limit, upper_limit){

  # functions checks that expert's plausible range is within parameter limits
  # and the upper limit is higher than the lower limit
  # min, max = expert's minumum and maximum
  # lower_limit and upper_limit = parameter limits set in manual_inputs.R

  if(!is.na(min) & !is.na(max)) {

  if(!is.na(lower_limit)){
    # if the parameter has a lower limit, cond_min = 1 if expert's
    # "min" is within the plausible range of the parameter
    cond_min <- ifelse(min >= lower_limit, 1, 0)
    } else {
      cond_min <- 1
    }

  if(!is.na(upper_limit)){
    # if the parameter has an upper limit, cond_max = 1 if expert's
    # "min" is within the plausible range of the parameter
    cond_max <- ifelse(max <= upper_limit, 1, 0)
    } else {
      cond_max <- 1
    }

  cond <- ifelse(cond_min + cond_max == 2 & min < max, 1, 0)

  } else {

    cond <- 0

}
  cond }

####### ctDNA functions ########

# get mode from a histogram. selects mean of the mode. if bimodal, picks the first mode.
f_get_mode_from_histogram <- function(chips_chips,chips_lbins,chips_rbins){

  valss<-rep(0,7)
  x<-which(chips_chips!=0) # number of bins with at least one chip
  
  if (length(x)!=0){
    
    # eliminate chips, rbins and lbins with no chips
    a<-chips_chips[x]
    b<-length(a)
    c<-chips_lbins[x]
    d<-chips_rbins[x]
    
    y<-which(a==max(a))
    z<-which(diff(y)==1)
    mmode<-y[1:ifelse(length(z)==0,1,length(z)+1)]
    l_mode<-c[mmode[1]]
    r_mode<-d[mmode[length(mmode)]]
    
    mode <- (l_mode + r_mode)/2
    
    return(mode)
    
  }
  
}

# copied from ARC A&E code
f_erf_inv<- function (x) qnorm((1 + x)/2)/sqrt(2)

# derive the exponential distribution from the mean (mode_omst_all) and elicited 25th percentile (elici_1c)
# fit exponential and if the elicited quartile is more than 10% off the exponential, fit lognormal
# use sampling to derive the mean OMST for ct_DNA_sensitivity% of the most severe cancers. (note it is not the quantile but the mean of the tail)
f_derive_omst_ctDNA <- function (mode_omst_all, elici_1c, ct_DNA_sensitivity){
  
  sensitivity <- ct_DNA_sensitivity/100
  rate_omst_all <- 1/mode_omst_all
  omst_exp_25_perc <- qexp(0.25, rate_omst_all)
    
  if(elici_1c/omst_exp_25_perc >= 0.9 | elici_1c/omst_exp_25_perc <= 1.1){ # assume exponential
    
    omst_exp_sensiti <- qexp(sensitivity, rate_omst_all)
    #omst_ctDNA <- qexp(ct_DNA_sensitivity, rate_omst_all)
    temp <- rexp(100000, rate_omst_all)
    omst_ctDNA <- round(mean(temp[which(temp < omst_exp_sensiti)]), digits = 1)
    
  } else { # fit lognormal
    
    mean_log <- log(mode_omst_all)
    x1_log   <- log(elici_1c)
    erf_val<-f_erf_inv(2 * 0.25 - 1) #https://stackoverflow.com/questions/29067916/error-function-erfz
    mean_lnorm<-(2*sqrt(2)*erf_val*mean_log-x1_log)/(2*sqrt(2)*erf_val-1)
    sdev_lnorm<-2*(mean_log - mean_lnorm)
    
    #omst_ctDNA <- qlnorm(ct_DNA_sensitivity, mean_lnorm, sdev_lnorm)
    temp <- rlnorm(100000, mean_lnorm, sdev_lnorm)
    omst_ctDNA <- round(mean(temp[which(temp < qexp(sensitivity, rate_omst_all))]), digits = 1)
    
  }
  
  return(omst_ctDNA)
  
}


####### saving functions (no longer used as Shiny no longer supports dropbox) #######

if(save_method == "dropbox") {

  #required for connection to dropbox
token <- if(file.exists("droptoken.rds")){readRDS("droptoken.rds")}else{NULL}
drop_acc(dtoken = token)
if(file.exists("droptoken.rds")){token$refresh()}else{NULL}

f_save_answers <- function(data,que_colnames,name1) {

  # function for saving in dropbox
  # can be edited to save elsewhere (e.g. locally)
  data <- t(data)
  colnames(data) <- que_colnames
  # Create a unique file name
  fileName <- name1

  if(is.null(token)){
    if (!dir.exists("SEE outputs")){
      dir.create("SEE outputs")
    }
    filePath <- file.path("SEE outputs",fileName)
    write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  } else {
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    write.csv(data, filePath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filePath, path = folder_name, dtoken = token)
  }

}

f_load_answers <- function(unique_id) {

  if(is.null(token)){
    if (!dir.exists("SEE outputs")){
      dir.create("SEE outputs")
    }
    filesInfo <- dir("SEE outputs")
    if(length(filesInfo)==0){
      data <- NA
    } else {
      temp <- filesInfo[grep(unique_id, filesInfo)]
      if(length(temp) == 0){
        data <- NA
      } else {
        filePaths <- paste0("SEE outputs/",filesInfo[grep(unique_id, filesInfo)])
        data <- lapply(filePaths, read.csv, stringsAsFactors = FALSE)
        # Concatenate all data together into one data.frame
        data <- do.call(cbind, data)
      }

    }

    } else {

    filesInfo <- drop_dir(folder_name)
    if(nrow(filesInfo) == 0){
      data <- NA
    } else {
      temp <- filesInfo$path_display
      filePaths <- temp[grep(unique_id, temp)]
      if(length(filePaths) == 0){
        data <- NA
      } else {
        data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
        # Concatenate all data together into one data.frame
        data <- do.call(cbind, data)
      }

    }
    data

    }
}

}






