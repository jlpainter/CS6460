###############################################################################
# CS-6460 Education Technology
#
# @file: studentUI.R
#
# @author: Jeffery Painter <jpainter32@gatech.edu>
#
###############################################################################

# ############################################################################
#  Student content begins here
# ############################################################################

##############################################################
# Define the company names to select from
##############################################################
myChoicesA = c("Old McDonald's Favorite Feed" = "d1")
myChoicesB = c(
  "Premium Brand Chicken Feed"    = "d2",
  "Corn Full of Feed"             = "d3",
  "Sticky and Sweet Chicken Feed" = "d4"
)


##############################################################
# Average weight of a standard chicken
##############################################################
avgWeight = 5.7

# We will use these factors to determine new avg weight
# when chickens are fed a particular feed
myMeans = c(1.0, 1.3, 1.2, 1.5)

##############################################################
# Assessment answer choices
##############################################################
q1_answers = c("Select Answer", "Alpha", "Effect Size", "Power")
q2_answers = c("Select Answer", "Alpha", "Effect Size", "Power")
q3_answers = c("Select Answer",
               "No Change",
               "Increase in sample size",
               "Decrease in sample size")



##############################################################
# Assessment answer choices
##############################################################
q1_answers = c("Select Answer", "Alpha", "Effect Size", "Power")
q2_answers = c("Select Answer", "Alpha", "Effect Size", "Power")
q3_answers = c("Select Answer", "No Change", "Increase in sample size", "Decrease in sample size")



## ###########################################################
## Panel 1
## ###########################################################
student_tab_1 <- tabPanel("Power Exploration",
         
         sidebarLayout(
           sidebarPanel(
             selectInput("distribution1", "Company A", myChoicesA),
             selectInput("distribution2", "Company B", myChoicesB, selected = "d2"),
             sliderInput("alpha", "Significance Level (Alpha)", min = 0.01, max = 0.10, value = 0.05, step = 0.01),
             sliderInput("desired_effect_size", "Desired Effect Size", min = 0.1, max = 2, value = 0.5, step = 0.1)
           ),
           
           mainPanel(
             HTML(
               '<div style="float: right;"><img src="fig_01.png" style="width:250px;"></div>'
             ),

             
             includeHTML("www/task_01.html"),
             
             #
             # The assessment questions will be created in the UI here.
             # In the teacher view, you will see the questions defined
             # with the correct answers in the database. This is required
             # to help align  the student responses and capture what their
             # answers are in the database itself.
             #
             # The questions are not automatically created from the database
             # at this point in time. That may be a future improvement.
             #
             
             plotOutput("distributionPlot"),
             textOutput("effectSizeOutput"),
             textOutput("powerOutput"),
             textOutput("sampleSizeOutput"),
             textOutput("typeIErrorOutput"),
             textOutput("typeIIErrorOutput"),
             
             # Student questions to answer
             h2("Test your understanding"),

             p("Question 1: What parameter do you need to change to minimize your Type I error rate?"),
             selectInput("q1", "Answer", q1_answers ),
             
             p("Question 2: Decreasing which parameter will help to minimize your Type II error rate?"),
             selectInput("q2", "Answer", q2_answers ),
             
             p("Question 3: What effect does increasing the power have on the sample size?"),
             selectInput("q3", "Answer", q3_answers ),
             
             actionButton("btnSubmitAnswers", "Check your answers")

           )
         )
)
