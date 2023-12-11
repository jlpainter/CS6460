###############################################################################
# CS-6460 Education Technology
#
# @author: Jeffery Painter <jpainter32@gatech.edu>
#
# Filename: dbManager.R
# Description:
#  This file will contain all of the database operations needed
#  to support running the app for multiple instructors, courses and students
#
#
###############################################################################


# Database stuff
library(DBI)
library(RSQLite)
library(DT)

# This is where we still login information for classes
current_directory <- getwd()
db_file = paste0(current_directory, "/data/cs6460.sqlite")

#u <- setUserBase()

# This will initialize the authorized users to login and
# their credentials (student or teacher)
setUserBase <- function()
{
  # Get all users
  all_users <- getAllUsers()
  
  user_ids <- c()
  usernames <- c()
  passwords <- c()
  perms <- c()
  names <- c()

  for (row in 1:nrow(all_users)) {
    entry = all_users[row,]
    #print(paste("Next user: ", entry$user_id))
    
    usernames <- c( usernames, entry$username )
    passwords <- c( passwords, entry$password )
    
    # Get this user's role
    user_role <- getUserRole(entry$user_id)
    perms     <- c( perms, user_role )
    names     <- c(names, paste( entry$first_name, entry$last_name ) )
    user_ids  <- c( user_ids, entry$user_id )
  }

  # dataframe that holds usernames, passwords and other user data
  user_base <- tibble::tibble(
    user_id = user_ids,
    user = usernames,
    password = sapply(passwords, sodium::password_store),
    permissions = perms,
    name = names
  )

  return(user_base)
}


# Get the user roles
getAllUsers <- function()
{
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  
  df <- dbGetQuery(mydb, 'SELECT * FROM user')
  
  # Disconnect from the database
  dbDisconnect(mydb)
  
  return(df)
}



# Load users belonging to a specific role
getUsersByRole <- function(roleId)
{
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  
  # Don't show the password in the UI
  sql = paste0('SELECT user_id, first_name, last_name, email, username FROM user WHERE user_id in (select user_id from user_roles where role_id = ', roleId, ');')
  df <- dbGetQuery(mydb, sql)
  
  # Disconnect from the database
  dbDisconnect(mydb)
  
  # Convert to a data table for display
  result <- datatable( data = df, extensions = 'Buttons', selection = 'single' )
  
  return(result)
}

# Get a user's role based on their user id
getUserRole  <- function(userId)
{
  #print(paste("Getting user role for user id: " , userId ))
  role_value = ""
  
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  
  sql = paste0('SELECT roles.role_name FROM roles, user_roles WHERE roles.ref_id = user_roles.role_id AND user_roles.user_id = ', userId, ';')
  #print(paste("SQL: ", sql))
  
  df <- dbGetQuery(mydb, sql)
  if ( nrow(df) == 1 ) 
  {
      role_value <- df$role_name
  }
  
  # Disconnect from the database
  dbDisconnect(mydb)
  
  return(role_value)  
}

getCourseSections <- function()
{
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  
  df <- dbGetQuery(mydb, 'SELECT * FROM course_section')
  
  # Disconnect from the database
  dbDisconnect(mydb)
  
  return(df)
}

# Load students
getStudents <- function()
{
  students <- getUsersByRole(3)
  return(students)
}

getRegistration <- function()
{
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  
  df <- dbGetQuery(mydb, 'SELECT * FROM registration')
  colnames(df) <- c("user_id", "course_id")
  # Disconnect from the database
  dbDisconnect(mydb)
  
  return(df)
}

# Allow users to change their password
updatePassword <- function( user_id, passwd ) {
  
  mydb <- dbConnect(RSQLite::SQLite(), db_file)

  # Prepare the update statement
  sql <- paste0("UPDATE user SET password = '", passwd, "' WHERE user_id = ", user_id, ";")
  updated <- dbExecute(mydb, sql)

  # Disconnect from the database
  dbDisconnect(mydb)

  if ( updated == 1 ) {
    # Success!
    return(TRUE)
  } else {
    # Failure
    return(FALSE)
  }
}

getUserLoginHistory <- function( user_id ) {
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  
  sql = paste('SELECT * FROM user_login WHERE user_id = ', user_id, ';')
  df <- dbGetQuery(mydb, sql)

  colnames(df) <- c("user_id", "num_logins", "last_login")
  # Disconnect from the database
  dbDisconnect(mydb)
  
  return(df)
}

# Allow users to change their password
updateUserLogin <- function( user_id ) {
  
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  history <- getUserLoginHistory(user_id)
  num_logins = 1
  if ( nrow(history) == 1 ) 
  {
    num_logins <- history$num_logins + 1
  }  

  # Get the current time
  last_login = Sys.time()  

  sql = paste("UPDATE user_login SET num_logins = ", num_logins, ", last_login = '", last_login, "' WHERE user_id = ", user_id, ";")
  updated <- dbExecute(mydb, sql)
  
  # Disconnect from the database
  dbDisconnect(mydb)
  
  if ( updated == 1 ) {
    # Success!
    return(TRUE)
  } else {
    # Failure
    return(FALSE)
  }
}


# Get all course sections
getAllCourses <- function() {
  
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  
  df <- dbGetQuery(mydb, 'SELECT * FROM course_section')
  colnames(df) <- c("course_id", "user_id", "year", "semester", "course_name")
  
  # Disconnect from the database
  dbDisconnect(mydb)
  
  return(df)
}

getNextCourseId <- function() {
  df <- getAllCourses()
  nextId <- max(df$course_id) + 1
  return(nextId)
}


# Get all course sections
getCourseDetails <- function(course_id) {
  
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  sql = paste0('SELECT * FROM course_section where course_id = ', course_id, ';')
  df <- dbGetQuery(mydb, sql )
  colnames(df) <- c("course_id", "user_id", "year", "semester", "course_name")
  
  # Disconnect from the database
  dbDisconnect(mydb)
  
  return(df)
}


getAllAssessments <- function() {
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  
  df <- dbGetQuery(mydb, 'SELECT * FROM student_assessment')
  colnames(df) <- c("ref_id", "user_id", "course_id", "eval_date", "grade")

  # Disconnect from the database
  dbDisconnect(mydb)
  
  return(df)  
}

getNextAssessment <- function() {
  df <- getAllAssessments()
  nextId <- max(df$ref_id) + 1
  return(nextId)
}

# Store student answers in the database
createNewAssessment <- function(assessmentId, user_id, course_id, grade) {
  
  mydb <- dbConnect(RSQLite::SQLite(), db_file)

  # Prepare the update statement
  sql <- paste0("INSERT INTO student_assessment 
                (ref_id, user_id, course_id, eval_date, grade) 
                VALUES (", assessmentId ,", ", user_id, ",", course_id, ", '", Sys.time() ,"', ",grade, ");")
  updated <- dbExecute(mydb, sql)
  
  # Disconnect from the database
  dbDisconnect(mydb)
  
  if ( updated == 1 ) {
    # Success!
    return(TRUE)
  } else {
    # Failure
    return(FALSE)
  }  
}

getAllStudentAnswers <- function() {
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  
  df <- dbGetQuery(mydb, 'SELECT * FROM student_answer')
  colnames(df) <- c("ref_id", "assessment_id", "question_id", "response_id")
  
  # Disconnect from the database
  dbDisconnect(mydb)
  
  return(df)  
}

getNextStudentAnswerId <- function() {
  df <- getAllStudentAnswers()
  nextId <- max(df$ref_id) + 1
  return(nextId)
}

# Store student answers in the database
recordStudentAnswer <- function(assessmentId, questionId, answer, correct) 
{
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  nextId <- getNextStudentAnswerId()

  # Prepare the update statement
  sql <- paste0("INSERT INTO student_answer 
                (ref_id, assessment_id, question_id, response_id, correct) 
                VALUES (", nextId ,", ", assessmentId, ", ", questionId, ", '", answer, "', ", correct, ");")
  updated <- dbExecute(mydb, sql)
  
  # Disconnect from the database
  dbDisconnect(mydb)
  
  if ( updated == 1 ) {
    # Success!
    return(TRUE)
  } else {
    # Failure
    return(FALSE)
  }    
}


getAssessmentAnswers <- function(assessment_id) {
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  sql <- paste0( 'SELECT question_id, response_id, correct FROM student_answer where assessment_id = ', assessment_id)
  df <- dbGetQuery(mydb, sql)
  colnames(df) <- c("Question Number", "Student Response", "Correct")

  # Disconnect from the database
  dbDisconnect(mydb)
  
  return(df)  
}


# Load the students data into a table
getStudentData <- function(student_id) {
  
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  sql <- paste('SELECT * FROM student_assessment WHERE user_id = ', student_id, ';')
  df <- dbGetQuery(mydb, sql)
  colnames(df) <- c("Assessment ID", "Student ID", "Course", "Evaluation Date", "Grade")
  return(df)  
}

getStudentCourse <- function(user_id) {
  
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  sql <- paste('SELECT * FROM registration WHERE user_id = ', user_id, ';')
  df <- dbGetQuery(mydb, sql)
  colnames(df) <- c("user_id", "course_id")
  course_id = 0
  if ( nrow(df) == 1 )
  {
    course_id = df$course_id
  }

  return(course_id)  

}

# Add a new course section
addNewCourseSection <- function( q_semester, q_year, q_name, user_id )
{
  status <- FALSE
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  nextId = getNextCourseId()

  # Prepare the update statement
  sql <- paste0("INSERT INTO course_section 
                (course_id, user_id, year, semester, course_name  ) 
                VALUES (", nextId ,", ", user_id, ", ",
                as.integer(q_year), ", '", 
                q_semester, "', '", 
                q_name, "' );")
  
  # Step 1: Insert the user
  updated <- dbExecute(mydb, sql)
  
  if ( updated == 1 )
  {
    # Only time status is true is if both items succeed
    status <- TRUE
  }
  
  # Disconnect from the database
  dbDisconnect(mydb)
  return(status)  
}

getNextUserId <- function() {
  df <- getAllUsers()
  nextId <- max(df$user_id) + 1
  return(nextId)
}


# Add a new user
addNewUser <- function(new_user_role, new_user_first_name, new_user_last_name, new_user_email, new_user_username, new_user_password) {

  status <- FALSE
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  nextId = getNextUserId()
  
  # Prepare the update statement
  sql <- paste0("INSERT INTO user 
                (user_id, first_name, last_name, email, username, password, enabled ) 
                VALUES (", nextId ,", '", 
                new_user_first_name, "', '", 
                new_user_last_name, "', '", 
                new_user_email, "', '",
                new_user_username , "', '", 
                new_user_password, "', TRUE );")
  
  # Step 1: Insert the user
  updated <- dbExecute(mydb, sql)

  if ( updated == 1 )
  {
    # Step 2: Insert the role assignment
    sql <- paste0("INSERT INTO user_roles 
                (user_id, role_id) 
                VALUES (", nextId ,", ", new_user_role, " );")

    updated <- dbExecute(mydb, sql)
    if ( updated == 1 ) {
      # Only time status is true is if both items succeed
      status <- TRUE
    }
  }

  # Disconnect from the database
  dbDisconnect(mydb)
  return(status)

}

# Add a new student
addNewStudent <- function(student_first_name, student_last_name, student_email, student_username, student_passwd, course_id) {

  status <- FALSE
  
  # We are adding a new user with the student role
  student_user_role = 3
  
  # This should be the next user id assigned
  userId = getNextUserId()
  #print(paste("Adding new student, userid: ", userId, " Role: ", student_user_role))
  
  # Create the student
  result <- addNewUser(student_user_role, student_first_name, student_last_name, student_email, student_username, student_passwd)  
  #print(paste( "New user added: ", result) )

  # Did we create the student?
  if ( result == TRUE )
  {
    # Assign the student to the section now
    #print(paste( "Assigning new student [", userId , "] to course: ", course_id ))
    result <- assignStudentToSection( userId, course_id )
    
    if ( result == TRUE )
    {
      status = TRUE
    }
  }
  
  return(userId)
  
}
  
# Clear any existing student registrations for this user
removeStudentRegistration <- function(userId) {

  status <- FALSE
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  nextId = getNextUserId()
  
  # Prepare the update statement
  sql <- paste0("DELETE FROM registration WHERE user_id = ", userId, ";") 
  updated <- dbExecute(mydb, sql)
  if ( updated == 1 ) {
    status = TRUE
  }
  
  # Disconnect from the database
  dbDisconnect(mydb)
  return(status)
}

assignStudentToSection <- function(userId, courseId) {
  
  # First delete any existing course section assignments for this student
  status <- FALSE
  mydb <- dbConnect(RSQLite::SQLite(), db_file)

  # Prepare the update statement
  sql <- paste0("INSERT INTO registration (user_id, course_id) VALUES (", 
                as.integer(userId),  ", ", 
                as.integer(courseId), ");") 
  #print(paste("SQL: ", sql))
  
  updated <- dbExecute(mydb, sql)
  if ( updated == 1 ) {
    status = TRUE
  }
  
  # Disconnect from the database
  dbDisconnect(mydb)
  return(status)  
}


#
# Given a course id, find all students taking that course
#
getCourseStudents <- function(course_id)
{
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  sql <- paste('SELECT * FROM user where user_id in (SELECT user_id from registration where course_id = ', course_id, ');')
  df <- dbGetQuery(mydb, sql )
  colnames(df) <- c("user_id", "first_name", "last_name", "email", "username", "password", "enabled")

    # Disconnect from the database
  dbDisconnect(mydb)
  return(df)
}

# Get a data frame that has the performance metrics of a teacher's course section
getStudentLastAssessment <- function(student_id) {
  last_test <- data.frame()
  studentData <- getStudentData(student_id)
    if ( nrow(studentData) >= 1 )
    {
      # The last row is the most current attempt
      last_test <- tail(studentData, n=1)
    }

  return(last_test)
}


#
# Get the performance characteristics for this teacher's courses
#
getTeacherCourseMetrics <- function(teacher_id) {
  
  # Get all of my courses
  myScores <- data.frame()
  
  all_courses <- getTeacherCourses(teacher_id)
  if ( nrow(all_courses) > 0 ) {
    for ( row in 1:nrow(all_courses) )
    {
      course_id <- all_courses[row, "course_id"]
      course_grades <- getCourseAssessments(course_id)
      
      # Get the course information
      course_details <- getCourseDetails(course_id)
      course_year    <- course_details$year
      course_semester <- course_details$semester
      course_name <- course_details$course_name
      
      min_grade = min(course_grades$Grade)
      max_grade = max(course_grades$Grade)
      median_grade = median(course_grades$Grade)
      mean_grade = round( mean(course_grades$Grade), digits = 3 )
      student_count = nrow(course_grades)
      
      new_row <- data.frame( course_id,  course_name, course_year, course_semester, student_count, min_grade, max_grade, median_grade, mean_grade ) 
      colnames(new_row) <- c("course_id", "course_name", "year", "semester", "num_students", "min_grade", "max_grade", "median", "mean" )
      
      myScores <- rbind(myScores, new_row)
      colnames(myScores) <- c("course_id", "course_name", "year", "semester", "num_students", "min_grade", "max_grade", "median", "mean" )
    }
  }
    
  return(myScores)
  
}


# Get a data frame that has the performance metrics of a teacher's course section
getCourseAssessments <- function(course_id)
{
  # Create a data frame to store all of the students last assessment scores
  class_assessments = data.frame()
  first = TRUE
  
  # Get all the students from this section
  all_students <- getCourseStudents(course_id)
  if ( nrow(all_students) >= 1 )
  {
    # For each student, find their last assessment
    for ( row in 1:nrow(all_students) ) {
      student_id <- all_students[row, "user_id"]
      student_assessment <- getStudentLastAssessment(student_id)
      if ( nrow(student_assessment) == 1 )
      {
        if ( first == TRUE ) {
          class_assessments <- student_assessment
          first = FALSE
        } else {
          class_assessments <- rbind(student_assessment, class_assessments)
        }
      }
    }
  }

 return(class_assessments)
  
}

  
# Get the teacher's current course sections
getTeacherCoursesSelect <- function(teacher_id)
{
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  sql <- paste('SELECT course_id, course_name FROM course_section where user_id = ', teacher_id, ';' )
  df <- dbGetQuery(mydb, sql )
  
  # Disconnect from the database
  dbDisconnect(mydb)
  
  if ( nrow(df) > 0 ) {
    # Create a named list to return
    option_values = c()
    option_names = c()
    for ( row in 1:nrow(df) ) {
      
      # Course ID is the value
      cid <- df[row, "course_id"]
      option_values <- c( option_values, cid )
      
      # Course name is the label
      cname <- df[row, "course_name"]
      option_names <- c( option_names, cname )
    }
  
    # Create a named list of options to return
    options <- option_values
    names(options) <- option_names
    return(options)
    
  } else {
    return(NULL)
  }
}

getTeacherCourses <- function(teacher_id)
{
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  sql <- paste('SELECT * FROM course_section where user_id = ', teacher_id, ';' )
  df <- dbGetQuery(mydb, sql )
  
  # Disconnect from the database
  dbDisconnect(mydb)
  
  return(df)  
}

# Get the answer key
getAnswerKey <- function()
{
  # Connect to the database
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  sql <- paste('SELECT * FROM answer_key;')
  df <- dbGetQuery(mydb, sql )
  
  # Disconnect from the database
  dbDisconnect(mydb)
  return(df)  
}


#######################################################################
# Return a dataframe showing the percent correct/incorrect for
# each assessment question for a particular teacher
#######################################################################
getTeacherQuestionMetrics <- function(teacher_id) {

  # How many questions are there?
  numQuestions = nrow(getAnswerKey())
  
  # Get all the answers into one dataframe
  all_answers = data.frame()
  all_courses <- getTeacherCourses(teacher_id)
  if ( nrow(all_courses) > 0 )
  {
    for ( row in 1:nrow(all_courses) )
    {
      course_id <- all_courses[row, "course_id"]
      course_name <- all_courses[row, "course_name"]
      course_grades <- getCourseAssessments(course_id)
      for ( sub_row in 1:nrow(course_grades) )
      {
        assessment_id <- course_grades[sub_row, "Assessment ID"]
        assessmentData <- getAssessmentAnswers(assessment_id)
        assessmentData$CourseName <- course_name
        all_answers <- rbind(all_answers, assessmentData)
      }
    }
    
    # Subset
    cols_to_keep = c("Question Number", "Correct", "CourseName")
    all_answers <- all_answers[cols_to_keep]
    cols_to_keep = c("Question", "Correct", "CourseName")
    colnames(all_answers) <- cols_to_keep
    
    # Compute the percent correct and incorrect for each assessment question
    percents <- data.frame()
    for ( index in 1:numQuestions ) {
      responses <- subset( all_answers, Question == index )  
      correct   <- subset( responses, Correct == 1 )
      percent_correct = round( 100.0 * nrow(correct) / nrow(responses), 2 )
      incorrect = 100 - percent_correct
      
      # Add rows
      new_row <- data.frame( index, "Correct",  percent_correct ) 
      colnames(new_row) <- c("Question", "Correct", "Percent")
      percents <- rbind( percents, new_row )
      
      new_row <- data.frame( index, "Wrong",  incorrect ) 
      colnames(new_row) <- c("Question", "Correct", "Percent")
      percents <- rbind( percents, new_row )
    }
    
    return(percents)
  } else {
    p <- data.frame()
    return(p)
  }
}

