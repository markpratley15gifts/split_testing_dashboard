#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(stringr)

sig_calc <- function(engaged_uniqs, control_uniqs, engaged_sales, control_sales, null_hypoth, tails){
  
  eng_conv <- engaged_sales / engaged_uniqs
  con_conv <- control_sales / control_uniqs
  
  eng_var <- eng_conv * ( 1 - eng_conv)
  con_var <- con_conv * ( 1 - con_conv)
  
  comb_var <- ( eng_var / engaged_uniqs + con_var/control_uniqs) ^0.5
  
  z_stat <- (eng_conv - con_conv - (null_hypoth /100 * con_conv)) / comb_var
  
  significance <- pnorm(z_stat)
  
  if (tails ==2)  {
    significance <- abs((2 * significance) -1)
  }
  
  else{
    significance <- pnorm(abs(z_stat))
  }
  
  return(significance)
}

uplift <- function (engaged_uniques, control_uniques, engaged_sales, control_sales){
  
  uplift <- 100 * (((engaged_sales / engaged_uniques) - (control_sales / control_uniques))) / (control_sales / control_uniques)
  return(uplift)
}

conf_interval_calc <- function(engaged_uniques, control_uniques, engaged_sales, control_sales, conf_level){
  
  c_conv <- control_sales / control_uniques
  e_conv <- engaged_sales / engaged_uniques
  
  var_conv <- c_conv*(1-c_conv)
  var_eng <- e_conv*(1-e_conv)
  
  uplift <- (e_conv - c_conv) / c_conv
  
  stan_dev <- ( (var_conv/control_uniques + var_eng/engaged_uniques)^ 0.5) * qnorm((conf_level+100)/2/100)
  ninety_five_each_way <- stan_dev/c_conv
  #range up uplift either side of overseved uplift to 95% confidence level
  
  return(data.frame(lower_bound = 100*(uplift - ninety_five_each_way), upper_bound = 100*(uplift + ninety_five_each_way)))
  
}

add_stats <- function(eng_u, con_u, eng_suc, con_suc, null_hypoth, tails, conf_level) {
  
  split_test_df <- data.frame(engaged_uniques = eng_u,
                             control_uniques = con_u,
                             engaged_sucesses = eng_suc,
                             control_sucesses = con_suc)%>%
    mutate(Confidence = sig_calc(engaged_uniques, control_uniques, engaged_sucesses, control_sucesses, null_hypoth, tails),
           Uplift = uplift(engaged_uniques, control_uniques, engaged_sucesses, control_sucesses))%>%
    do(cbind(.,conf_interval_calc(.$engaged_uniques, .$control_uniques, .$engaged_sucesses, .$control_sucesses, conf_level)))
    
  return(split_test_df)
}

add_stats_csv <- function(df, null_hypoth, tails, conf_level) {
  
    split_test_df<-df%>%
    mutate(Confidence = sig_calc(treatment_uniques, control_uniques, treatment_sucesses, control_sucesses, null_hypoth, tails),
           Uplift = uplift(treatment_uniques, control_uniques, treatment_sucesses, control_sucesses))%>%
    do(cbind(.,conf_interval_calc(.$treatment_uniques, .$control_uniques, .$treatment_sucesses, .$control_sucesses, conf_level)))
  
  return(split_test_df)
}

power_calc <- function(days ,av_uniques_day, e_prop, base_conv, true_uplift, null_hypoth, sig_level,tails){
  
  period_uniques <- av_uniques_day * days
  e_uniques <- e_prop * period_uniques
  c_uniques <- (1-e_prop) * period_uniques
  c_conv <- base_conv
  e_conv <- (base_conv * true_uplift + base_conv)
  
  c_var <- c_conv * (1 - c_conv)
  e_var <- e_conv * (1 - e_conv) 
  stan_dev <- (c_var/c_uniques + e_var/e_uniques) ^ 0.5
  
  sig = 1 - sig_level/100
  if (tails == 2){
    sig <- sig/2 + 0.5
  }
  null_hypoth_diff_means <- null_hypoth * base_conv
  true_diff_means  <- base_conv * true_uplift
  
  pass_mark <- qnorm(sig,null_hypoth_diff_means, stan_dev)
  power_value <-  1-pnorm(pass_mark, true_diff_means, stan_dev)
  #probability of significant result given parameters
  return(power_value)
}

pass_mark <- function(sig_level, null_hypoth, tails, cont_u, cont_succ, treat_u, treat_succ){

  sig = 1 - sig_level

  if (tails == 2){

    sig <- sig/2 + 0.5
  }
  
  null_hypoth_diff_means <- null_hypoth * (cont_succ/cont_u)
  c_var <- (cont_succ/cont_u) * (1 - (cont_succ/cont_u))
  e_var <- (treat_succ/treat_u) * (1 - (treat_succ/treat_u))
  stan_dev <- (c_var/cont_u + e_var/treat_u) ^ 0.5
  
  pass_mark_diff_means <-  qnorm(sig,null_hypoth_diff_means, stan_dev)

  pass_mark_uplift <- 100 * pass_mark_diff_means/ (cont_succ/cont_u)

  return(pass_mark_uplift)
  
}
power_with_start_calc <- function(days_from_now, days_to_date, uniqs_to_date_eng, uniqs_to_date_con, sales_to_date_eng, 
                                  sales_to_date_con, true_uplift, null_hypoth, sig_level, tails){
  
  e_uniques_total <- uniqs_to_date_eng / days_to_date * (days_to_date + days_from_now)
  c_uniques_total <- uniqs_to_date_con/ days_to_date * (days_to_date + days_from_now)
  e_uniques_simulated <- uniqs_to_date_eng * days_from_now / days_to_date
  c_uniques_simulated <- uniqs_to_date_con * days_from_now / days_to_date
  
  c_conv_total <- sales_to_date_con/ uniqs_to_date_con
  e_conv_total <- (sales_to_date_eng + c_conv_total * (1 + true_uplift) *e_uniques_simulated)/ e_uniques_total
  
  c_var_total <- c_conv_total * (1 - c_conv_total)
  c_var_simulated <- c_var_total
  e_var_total <- e_conv_total * (1 - e_conv_total) 
  e_var_simulated <- (c_conv_total * (1 + true_uplift)) * (1 - c_conv_total * (1 + true_uplift))
  
  stan_dev_total <- (c_var_total/c_uniques_total + e_var_total/e_uniques_total) ^ 0.5
  stan_dev_sim <- (c_var_simulated/c_uniques_simulated + e_var_simulated/e_uniques_simulated) ^ 0.5
  
  sig = 1 - sig_level/100
  if (tails == 2){
    sig <- sig/2 + 0.5
  }
  
  null_hypoth_diff_conv <- null_hypoth/100 * c_conv_total
  
  diff_conv_pass_mark_total <- qnorm(sig,null_hypoth_diff_conv, stan_dev_total)
  
  required_simulated_e_conv <- (((diff_conv_pass_mark_total + c_conv_total) * e_uniques_total) - sales_to_date_eng) / e_uniques_simulated
  required_simulated_diff_conv <- required_simulated_e_conv - c_conv_total
  
  true_diff_means <- true_uplift * c_conv_total
  power_value <-  1-pnorm(required_simulated_diff_conv, true_diff_means, stan_dev_sim)
  
  return(power_value)
}



# Define server logic 
shinyServer(function(input, output, session) {
  
  output$text1 <- renderText({ 
    if (input$select == 1 ){
      'Overview of current test'
    }
  })
  
  output$testControls <- renderUI({

    if (input$select == 1 & !is.null(input$csv)) {
      if (input$csv == 1 ) {
      tagList(numericInput("treat_u", 
                 label = ("Treatment uniques*"),
                 value = 0),
              numericInput("cont_u", 
                   label = ("Control uniques*"),
                   value = 0),
              numericInput("treat_sucess", 
                 label = ("Treatment sucesses*"),
                 value = 0),
              numericInput("cont_sucess", 
                 label = ("Control sucesses*"),
                 value = 0))
      }

    }  
  })
  
  output$testCsv <- renderUI({
    if (input$select == 1 & !is.null(input$csv)){
      if (input$csv == 2 ){
        fileInput("datafile" ,'')
      }
    }
  })
  output$testCsvText <- renderUI({
    if (input$select == 1 & !is.null(input$csv)){
      if (input$csv == 2 ){
        p('CSV must have the columns treatment_uniques, control_uniques, treatment_sucesses, control_sucesses and be cumulative')
      }
    }
  })
  
  
  output$loadcsv <- renderUI({
    if (input$select ==1 ) {
    selectInput("csv", label = ("Load data from csv"), 
                choices = list("no" = 1, "yes" = 2),
                selected = 1)
    }
  })
  
  output$testOver <- renderUI({
    if (input$select ==1 ) {
      selectInput("testOver", label = ("Has the test finished"), 
                  choices = list("Yes" = 1, "No" = 2),
                  selected = 1)
    }
  })
  
  output$range <- renderUI({
    if (input$select == 1 ) {
      sliderInput('conf', label = ('Range for U/L bound (%)'),
                  min = 80, max = 99, value=95
      )
    }
  })
  

  
  output$testRunning <- renderUI({
    if (!is.null(input$testOver)){
      if (input$testOver == 2 && input$select == 1) {
      numericInput("testDaysRun", label = ("How many days has the test run for*"), 
                  value = 1)
      }
    }

  })
  
  
  output$testSig <- renderUI({
    if (!is.null(input$testOver)){
      if ((input$testOver == 2 || input$csv == 2) && input$select == 1) {
        sliderInput('sig', label = ('Significance level (%)'),
                    min = 1, max = 10, value=5)
      }
    }
  })
  

  output$testSigPlan <- renderUI({
      if (input$select == 2 ) {
        sliderInput('sigPlan', label = ('Significance level (%)'),
                    min = 1, max = 10, value=5)
      }
  })
  
  output$convPlan <- renderUI({
    if (input$select == 2 ) {
     numericInput('convPlan', label = ('base conversion (%)*'),
                   value=0)
    }
  })
  
  output$trafficPlan <- renderUI({
    if (input$select == 2 ) {
      numericInput('trafficPlan', label = ('Total traffic/day in test*'),
                  value=0)
    }
  })

  
  output$testReamaining <- renderUI({
  
  if (!is.null(input$testOver)){
    if (input$testOver == 2 && input$select == 1) {
      numericInput("testDaysLeft", label = ("How many days are left?"), 
                   value = 50)
    }
  }
  })
  
  
  
  output$minUp <- renderUI({
    
    if (!is.null(input$testOver)){
      if (input$testOver == 2 && input$select ==1) {
        sliderInput('minUp', label = ('True uplift estimated range (%)'),
                    min = 1, max = 20, value=c(1,4))
      }
    }
  })
  
  output$minUpPlan <- renderUI({
    
      if (input$select == 2 ) {
        sliderInput('minUpPlan', label = ('True uplift estimated range (%)'),
                    min = 1, max = 20, value=c(1,4))
      }
  })
  
  output$propsPlan <- renderUI({
    
    if (input$select == 2 ) {
      sliderInput('propsPlan', label = ('Proportion given treatment (%)'),
                  min = 10, max = 90, value=50)
    }
  })
  output$daysPlan <- renderUI({
    
    if (input$select == 2 ) {
      numericInput('daysPlan', label = ('Max test length (days)'),
                   value=100)
    }
  })
  
  output$minPowerPlan <- renderUI({
    
    if (input$select == 2 ) {
      sliderInput('powerPlan', label = ('Minimum required power'),
                  min = 50, max = 90, value=75)
    }
  })
  
  output$table <- renderTable({
    if (input$select == 1){
      if (!is.null(input$cont_sucess)){
        if(input$csv == 1){
          df <- add_stats(input$treat_u, input$cont_u,input$treat_sucess,
                        input$cont_sucess,input$null_hypoth,input$tails,input$conf) %>% 
          select(Uplift, Confidence, lower_bound,upper_bound)
          
          names(df)[3] <- 'Lower bound'
          names(df)[4] <- 'Upper bound'
         return(df)
        }
      }

    }
  })
  
  output$tableCsv <- renderTable({
    if (input$select == 1){
        if (!is.null(input$datafile) && input$csv == 2){
          df <- read.csv(input$datafile$datapath) %>% 
            tail(1) %>% 
            add_stats_csv(input$null_hypoth,input$tails,input$conf) %>% 
            select(Uplift, Confidence, lower_bound,upper_bound)
          names(df)[3] <- 'Lower bound'
          names(df)[4] <- 'Upper bound'
          

          return(df)
        }
    }
  })
  
  output$plotCsv <- renderPlot({
    if (input$select == 1){
      if (!is.null(input$datafile)){
        df <- read.csv(input$datafile$datapath) %>% 
          mutate(Uplift = uplift(treatment_uniques, control_uniques, treatment_sucesses, control_sucesses))%>% 
          mutate(row = seq_along(treatment_uniques)) %>%
          mutate(pass_u =  pass_mark(input$sig/100, input$null_hypoth/100, input$tails, control_uniques,
                                         control_sucesses, treatment_uniques, treatment_sucesses))
       
         if (input$tails == 2) {
          df<- df %>% mutate( pass_l = pass_mark((2-input$sig/100), input$null_hypoth/100, input$tails,  control_uniques,
                                          control_sucesses, treatment_uniques, treatment_sucesses))
        }
        
        df<-df %>% 
          select(Uplift,row, pass_u, starts_with('pass')) %>% 
          melt(id.vars= 'row') %>% 
          mutate(color = ifelse(str_detect(variable, 'pass'),'pass mark','uplift'))

         ggplot(df, aes(x=row, y = value, group = variable, color = color)) + geom_line() + xlab("")+
           theme(legend.title=element_blank()) + ylab('%')
      }
    }
  })
      


  getData <- reactive({
    if(is.null(input$datafile)) {
      return(NULL)
    } else{1}
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(getData()))
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$plotSim <- renderPlot({
    
    if (input$select == 1){
      if(!is.null(input$testOver)){
        if (input$testOver == 2){
          if (!is.null(input$testDaysLeft)){
          
            power_df <- expand.grid(days= seq(1, input$testDaysLeft, 1),
                                    true_uplift= seq(input$minUp[1]/100, input$minUp[2]/100, length.out = 5)) %>%
              mutate(power = power_with_start_calc(days, input$testDaysRun, input$treat_u, input$cont_u, input$treat_sucess,
                                                   input$cont_sucess, true_uplift, input$null_hypoth, input$sig, input$tails),
                                    true_uplift = true_uplift * 100)
        
            power_df$true_uplift <- factor(power_df$true_uplift, levels = rev(levels(factor(power_df$true_uplift))))
            return(ggplot(power_df, aes(x=days, y=power, group = true_uplift, colour = true_uplift)) +geom_line()+ 
                     ggtitle("Test Prognosis") + theme(plot.title = element_text(hjust = 0.5)))
          }
        }
      }
    }
  })  
  
  output$plotSimCsv <- renderPlot({
    
    if (input$select == 1){
      if(!is.null(input$testOver)){
        if (input$testOver == 2){
          if (!is.null(input$testDaysLeft)){
            if (!is.null(input$datafile)){
              df <- read.csv(input$datafile$datapath) %>% 
                tail(1)
              
              power_df <- expand.grid(days= seq(1, input$testDaysLeft, 1),
                                      true_uplift= seq(input$minUp[1]/100, input$minUp[2]/100, length.out = 5)) %>%
                mutate(power = power_with_start_calc(days, input$testDaysRun, df$treatment_uniques, df$control_uniques, df$treatment_sucesses,
                                                     df$control_sucesses, true_uplift, input$null_hypoth, input$sig, input$tails),
                       true_uplift = true_uplift * 100)
              
              power_df$true_uplift <- factor(power_df$true_uplift, levels = rev(levels(factor(power_df$true_uplift))))
              return(ggplot(power_df, aes(x=days, y=power, group = true_uplift, colour = true_uplift)) +geom_line()+ 
                       ggtitle("Test Prognosis") + theme(plot.title = element_text(hjust = 0.5)))
              
            }
          }
        }
      }
    }
  }) 
  
  output$testPlan <- renderPlot({
    if (input$select ==2){
      if (!is.null(input$daysPlan)){
        power_df <-   expand.grid(days= seq(1,input$daysPlan, 1),
                              e_prop= input$propsPlan,
                              true_uplift= seq(input$minUpPlan[1]/100, input$minUpPlan[2]/100, length.out = 5)) %>%
          mutate(power = power_calc(days, input$trafficPlan, e_prop/100, input$convPlan/100, true_uplift,input$null_hypoth/100, input$sigPlan, input$tails),
             true_uplift = true_uplift*100)
    
    
        power_df$true_uplift <- factor(power_df$true_uplift, levels = rev(levels(factor(power_df$true_uplift))))

      
        return( ggplot(power_df, aes(x=days, y=power, group = true_uplift, colour = true_uplift)) +geom_line() + 
                  ggtitle("Test Prognosis") + theme(plot.title = element_text(hjust = 0.5)))
      }
    }
  })
  
  output$powerPlan <- renderTable({
    if (input$select ==2){
      if (!is.null(input$daysPlan)){
        power_df <-   expand.grid(days= seq(1,input$daysPlan, 1),
                                  e_prop= input$propsPlan,
                                  true_uplift= seq(input$minUpPlan[1]/100, input$minUpPlan[2]/100, length.out = 5)) %>%
          mutate(power = power_calc(days, input$trafficPlan, e_prop/100, input$convPlan/100, true_uplift,input$null_hypoth/100, input$sigPlan, input$tails),
                 true_uplift = true_uplift*100)%>%
          mutate( Uniques = days * input$trafficPlan)%>%
          group_by(true_uplift) %>% 
          filter(power >= input$powerPlan/100) %>% 
          summarise(min_days = as.integer(min(days)),
                    Uniques = as.integer(min(Uniques))) 
        
        colnames(power_df)[1] = 'True uplift'
        colnames(power_df)[2] = 'Min days'
        colnames(power_df)[3] = 'Min uniques'
        return (power_df)
        
      }
    }
  })
  
})
