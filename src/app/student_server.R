###############################################################################
# CS-6460 Education Technology
#
# @file: student_server.R
#
# @author: Jeffery Painter <jpainter32@gatech.edu>
#
###############################################################################

##############################################################
##  Functions to support the student data investigation
##############################################################

# Reactive values
effect_size <- reactiveVal()
data1 <- reactiveVal()
data2 <- reactiveVal()
mean2 <- reactiveVal()

observeEvent(input$distribution2, {
  # Generate data from selected pre-built distributions
  data2_val <- switch(
    input$distribution2,
    "d2" = rnorm(1000, mean = avgWeight * myMeans[2], sd = 1),
    "d3" = rnorm(1000, mean = avgWeight * myMeans[3], sd = 1),
    "d4" = rnorm(1000, mean = avgWeight * myMeans[4], sd = 1)
    
  )
  
  mean2_val <- switch(
    input$distribution2,
    "d2" = avgWeight * myMeans[2],
    "d3" = avgWeight * myMeans[3],
    "d4" = avgWeight * myMeans[4] )
  
  # Update reactive values
  data2(data2_val)
  mean2(mean2_val)
})

observeEvent(input$distribution2, {
  # Generate data from selected pre-built distributions
  data1_val <- switch(input$distribution1,
                      "d1" = rnorm(1000, mean = avgWeight * myMeans[1], sd = 1),)
  
  data2_val <- switch(
    input$distribution2,
    "d2" = rnorm(1000, mean = avgWeight * myMeans[2], sd = 1),
    "d3" = rnorm(1000, mean = avgWeight * myMeans[3], sd = 1),
    "d4" = rnorm(1000, mean = avgWeight * myMeans[4], sd = 1))
  
  # Update reactive values
  data1(data1_val)
  data2(data2_val)
})

output$distributionPlot <- renderPlot({
  # Validate that different distributions are selected
  validate(
    need(
      input$distribution1 != input$distribution2,
      "Please select different distributions for comparison."
    )
  )
  
  # Plot the distributions
  md1 = min(data1())
  md2 = min(data2())
  min_both = min(md1, md2)
  
  md1 = max(data1())
  md2 = max(data2())
  max_both = max(md1, md2)
  selected_alpha = input$alpha
  alpha_line = qnorm(
    1 - selected_alpha,
    mean = avgWeight, sd = 1)
  
  
  p <- ggplot() +
    
    stat_function(
      fun = dnorm,
      args = list(mean = avgWeight , sd = 1),
      aes(color = "Company A (Normal)")
    ) +
    stat_function(
      fun = dnorm,
      args = list(mean = mean2(), sd = 1),
      aes(color = "Company B (Normal)")
    ) +
    geom_vline(xintercept = alpha_line) +
    geom_vline(xintercept = avgWeight+ input$desired_effect_size) +
    geom_area(aes(x=c(-3,13)) , stat = "function", fun = dnorm, args = list(mean = mean2() , sd = 1),fill = "#00998a", xlim = c(-3,avgWeight+input$desired_effect_size ), alpha = 0.5) +
    geom_area(aes(x=c(-3,13)) , stat = "function", fun = dnorm, args = list(mean = mean2() , sd = 1),fill = "Blue", xlim = c(avgWeight+input$desired_effect_size, 13 ), alpha = 0.5) +
    geom_area(aes(x=c(-3,13)) , stat = "function", fun = dnorm, args = list(mean = avgWeight , sd = 1),fill = "Red", xlim = c(alpha_line, 13 ), alpha = 0.5) +
    labs(title = "Effect Size Visualization",
         x = "Value",
         y = "Density") +
    theme_minimal() +
    #scale_fill_manual(values = c(
    #  "Company A" = "blue", "Company B" = "red"
    #)) + 
    xlim(0,13)
  
  
  # Calculate effect size
  effect_size_val <-
    cohen.d(isolate(data1()), isolate(data2()))$estimate
  effect_size(effect_size_val)  # Update the reactive value
  
  # Print effect size
  effect_size_val <-
    effect_size()  # Retrieve the value from reactiveVal
  #print(paste("Cohen's d Effect Size: ", round(effect_size_val, 2)))
  
  p
})

output$effectSizeOutput <- renderText({
  # Print effect size
  #paste("Cohen's d Effect Size: ", round(effect_size(), 2))
})

#
# Update the power
#
output$powerOutput <- renderText({
  desired_effect_size <- input$desired_effect_size
  beta <- pnorm( avgWeight + desired_effect_size, mean = mean2(), sd = 1 )
  power <- 1 - beta
  paste("Power: ", round(power, 2))
})

#
# Update the sample size
#
output$sampleSizeOutput <- renderText({

  alpha <- input$alpha
  desired_effect_size <- input$desired_effect_size
  beta <- pnorm( avgWeight + desired_effect_size, mean = mean2(), sd = 1 )
  power <- 1 - beta
  sample_size <-
    pwr.t.test(d = desired_effect_size,
               sig.level = alpha,
               power = power)$n
  paste("Required Sample Size: ", round(sample_size))
})

output$typeIErrorOutput <- renderText({
  # Calculate Type I error rate
  alpha <- input$alpha
  paste("Type I Error Rate: ", alpha)
})

output$typeIIErrorOutput <- renderText({
  # Calculate Type II error rate
  desired_effect_size <- input$desired_effect_size
  beta <- pnorm( avgWeight + desired_effect_size, mean = mean2(), sd = 1 )
  paste("Type II Error Rate: ", round(beta, 2))
})


######################################
# Record student answers
######################################
observeEvent(input$btnSubmitAnswers, {
  if (credentials()$user_auth)
  {
    # Get the current logged in student
    user_id <- credentials()$info$user_id
    
    # What course is this student registered in?
    course_id <- getStudentCourse(user_id)
    #print(paste("Course: ", course_id))
    
    # Get the answers from the database
    answers = getAnswerKey()

    # Get the student responses
    a1 = input$q1
    a2 = input$q2
    a3 = input$q3

    # Get the answer key
    a1key = answers[1, "answer"]
    a2key = answers[2, "answer"]
    a3key = answers[3, "answer"]
    
    a1_correct = 1
    a2_correct = 1
    a3_correct = 1
    if ( a1 != a1key ) { a1_correct = 0 }
    if ( a2 != a2key ) { a2_correct = 0 }
    if ( a3 != a3key ) { a3_correct = 0 }
    
    # Compute the students grade
    grade = 100
    if ( a1_correct == 0 ) {
      grade = grade - 100 * (1/3)
    }

    if ( a2_correct == 0 ) {
      grade = grade - 100 * (1/3)
    }

    if ( a3_correct == 0 ) {
      grade = grade - 100 * (1/3)
    }

    # All wrong = 0.0    
    if ( a1_correct == 0 && a2_correct == 0 && a3_correct == 0 ) {
      grade = 0.0
    }
    
    # Store student answers in the database
    assessmentId <- getNextAssessment()
    createNewAssessment( assessmentId, user_id, course_id, grade )
    recordStudentAnswer( assessmentId, 1, a1, a1_correct )
    recordStudentAnswer( assessmentId, 2, a2, a2_correct )
    recordStudentAnswer( assessmentId, 3, a3, a3_correct )
    
    # Compare answers!
    all_correct = TRUE
    if ( a1_correct == 0  ) {
      shinyalert("Oops!",
                 "You did not answer the question 1 correctly. Please check your answer and try again.",
                 type = "error")      
      all_correct = FALSE
    } else {

      if ( a2_correct == 0  ) {
        shinyalert("Oops!",
                   "You did not answer the question 2 correctly. Please check your answer and try again.",
                   type = "error")      
        all_correct = FALSE
      } else {

        if ( a3_correct == 0  ) {
          shinyalert("Oops!",
                     "You did not answer the question 3 correctly. Please check your answer and try again.",
                     type = "error")      
          all_correct = FALSE
        }
      }
    }
    

    if ( all_correct == TRUE ) {
      shinyalert("Success!", "You rock! All of your answers were correct.", type = "success")
    }
  } else {
    shinyalert("Oops!",
               "We could not locate your account. Please log out and back in.",
               type = "error")   
  }
    
})
##############################################################