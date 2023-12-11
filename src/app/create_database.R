################################################################################
# Setup our student / instructor database
################################################################################
#
# See help documentation online:
# https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html
#

library(DBI)
library(RSQLite)

reset_database <- function() {

  current_time <- Sys.time()
  
  # This is where we still login information for classes
  current_directory <- getwd()
  db_file = paste0(current_directory, "/data/cs6460.sqlite")
  
  # Remove the old file if it exists
  if ( file.exists((db_file)) ) {
    file.remove(db_file)
  }
  
  # Create the new file
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  
  # Setup initial user table
  user <- data.frame(
    user_id = c(1, 2, 3, 4 ),
    first_name = c('Jeffery', 'Mr', 'Kristi', 'Justin'),
    last_name = c('Painter', 'Lazarski', 'Sargent', 'Post'),
    email= c('jpainter32@gatech.edu', 
             'craig_lazarski@caryacademy.org', 
             'krist_sargent@caryacademy.org', 
             'Justin_Post@ncsu.edu' ),
    username = c('admin', 'craig', 'kristi', 'justin'),
    password = c('admin', 'pass', 'pass', 'pass' ),
    enabled = c(TRUE, TRUE, TRUE, TRUE)
  )
  
  # Capture when a user has logged into the system
  user_login <- data.frame(
    user_id    = c(1,2,3,4),
    num_logins = c(0,0,0,0),
    last_login = c('','','','')
  )
  
  # Create user roles
  roles <- data.frame(
    ref_id = c(1, 2, 3),
    role_name = c('admin', 'teacher', 'student')
  )
  colnames(roles) <- c("ref_id", "role_name")
  
  # Map users to roles
  user_roles <- data.frame(
    user_id = c(1, 2, 3, 4),
    role_id = c(1, 2, 2, 2)
  )
  colnames(user_roles) <- c("user_id", "role_id")
  
  #
  # Course sections contain students and owned by an instructor
  # First course is owned by the admin
  #
  course_section <- data.frame(
    course_id = c(1, 2, 3, 4),
    user_id = c(1, 2, 2, 2),
    year = c (2023, 2023, 2023, 2023),
    semester = c ('Fall', 'Fall', 'Fall', 'Fall'),
    course_name = c('Default Admin Course', 'A Block', 'B Block', 'C Block')
  )
  
  #
  # Link students to courses
  #
  registration <- data.frame( user_id <- c(1),
                              course_id <- c(1) )
  colnames(registration) <- c("user_id", "course_id")
  

  ###############################################################
  # Define the answers to the questions in the assessment here
  ###############################################################
  
  ###############################################################
  # The ref_id matches the question number
  # The answer is for each question id
  ###############################################################
  answer_key <- data.frame(
    ref_id = c(1, 2, 3),
    answer = c("Alpha", "Effect Size", "Increase in sample size")
  )

  ###############################################################
  # When a student starts a new assessment,
  # their answers will be linked here under
  # a single assessment observation
  ###############################################################
  student_assessment <- data.frame (
    ref_id = c( 1 ),
    user_id = c( 1 ),
    course_id = c( 1 ), 
    eval_date = c( '2023-11-12 00:00:00' ),
    grade = c( 100.0 )
  )

  # A students answer to each question is stored here and linked
  # to their assessment table entry above
  student_answer <- data.frame(
    ref_id = c( 1, 2, 3 ),
    assessment_id = c( 1, 1, 1 ),
    question_id = c( 1, 2, 3 ),
    response_id = c( "Alpha", "Effect Size", "Increase in sample size" ),
    correct     = c(1, 1, 1)
  )
  
  # Capture how many times a student played with the simulation
  student_interaction <- data.frame(
    ref_id = c( 1 ),
    user_id = c( 1 ),
    course_id = c( 1 ), 
    interact_date = c( '2023-11-12 00:00:00' )
  )

  #
  # Write tables to our SQLite database
  #
  dbWriteTable(mydb, "user", user)
  dbWriteTable(mydb, "user_login", user_login)
  
  dbWriteTable(mydb, "roles", roles)
  dbWriteTable(mydb, "user_roles", user_roles)
  dbWriteTable(mydb, "course_section", course_section)
  dbWriteTable(mydb, "registration", registration)
  
  dbWriteTable(mydb, "answer_key", answer_key)
  dbWriteTable(mydb, "student_assessment", student_assessment)
  dbWriteTable(mydb, "student_answer", student_answer)
  dbWriteTable(mydb, "student_interaction", student_interaction)

  # Show the tables
  dbListTables(mydb)
  
  
  # Disconnect from the database
  dbDisconnect(mydb)
}


# Create sample data for the demo app
rebuild_demo_data <- function() {
  #
  # Load our dbManager API
  #
  source('dbManager.R', local = TRUE)
  
  
  ################################################################################
  #
  # Sample course for dashboads
  #
  course_id = 2
  num_students = 90
  
  for ( idx in 1:num_students)
  {
    # Update the course ID so each sample course has 30 students
    if ( idx %% 30 == 0 ) {
      course_id = course_id + 1
      #print(course_id)
    }
  
    first_name = paste("Student_", idx)
    last_name = paste("")
    email = paste0("student_", idx, "@myschool.edu")
    username = paste0("student", idx)
    passwd = "pass"
  
    # Create a new student returns the new student ID
    if ( course_id < 5 ) {
      student_id <- addNewStudent( first_name, last_name, email, username, passwd, course_id )
        
      # Store student answers in the database
      a1_correct = "Alpha"
      a1_incorrect = "Effect Size"
      a1 = ""
      a1b = 0
      
      a2_correct = "Effect Size"
      a2_incorrect = "Alpha"
      a2 = ""
      a2b = 0
      
      a3_correct = "Increase in sample size"
      a3_incorrect = "No Change"
      a3 = ""
      a3b = 0
    
      g <- sample(1:10, 1, replace = TRUE )
      if ( g > 3 ) {
        a1 = a1_correct
        a1b = 1
      } else {
        a1 = a1_incorrect
      }
      
      g <- sample(1:10, 1, replace = TRUE )
      if ( g > 2 ) {
        a2 = a2_correct
        a2b = 1
      } else {
        a2 = a2_incorrect
      }
      
      g <- sample(1:10, 1, replace = TRUE )
      if ( g > 3 ) {
        a3 = a3_correct
        a3b = 1
      } else {
        a3 = a3_incorrect
      }
      
    
        grade <- 100.00
        if ( a1b == 0 )
        {
          grade = grade - 33.0
        }
    
        if ( a2b == 0 )
        {
          grade = grade - 33.0
        }
    
        if ( a3b == 0 )
        {
          grade = grade - 33.0
        }
        
      
        # Create new assessment  
        assessmentId <- getNextAssessment()
        createNewAssessment( assessmentId, student_id, course_id, grade )
        recordStudentAnswer( assessmentId, 1, a1, a1b )
        recordStudentAnswer( assessmentId, 2, a2, a2b )
        recordStudentAnswer( assessmentId, 3, a3, a3b )  
    }
  }
}

###############################################################################
#
#                    Database Initialization
#
# 1. Reset the database
# 2. Create a bunch of sample students and assign to the first instructor
#    to demonstrate dashboard capabilities.
#
###############################################################################
reset_database()
rebuild_demo_data()

