###############################################################################
# CS-6460 Education Technology
#
# @file: teacher_server.R
#
# @author: Jeffery Painter <jpainter32@gatech.edu>
#
###############################################################################

########################################################################
# Dashboard dynamics
########################################################################

# Create a reactive object to store selected data
selectedDashboardCourseSection <- reactiveVal()

########################################################################
# When the teacher selects a course on the dashboard,
# show the student data below
########################################################################
observeEvent(input$teacher_sections_tbl_rows_selected, {
  
  # Get my current user ID
  user_id <- credentials()$info$user_id
  df <- getTeacherCourses(user_id)
  selectedRow <- input$teacher_sections_tbl_rows_selected
  course_id <- df[selectedRow, "course_id"]

  # Load the student data  
  studentData <- getCourseStudents(course_id)
  
  # Update the data table
  output$teacher_students_tbl <- DT::renderDT(studentData, selection = 'single')
  
  # Clear the student answers
  output$teacher_student_detail_tbl <- DT::renderDT(NULL, selection = 'single')
  output$teacher_student_answer_tbl <- DT::renderDT(NULL, selection = 'single')
  
  # Update the reactive variable
  selectedDashboardCourseSection = course_id

})

#####################################################
# Side-by-side box plots displaying performance metrics
# for each of my teacher's individual class sections
#####################################################
output$course_performance_plot <- renderPlot({

  user_id <- credentials()$info$user_id
  allCourses <- getTeacherCourseMetrics(user_id)
  
  ################################################
  # Build ggplot of course performance metrics
  grades_df <- data.frame()
  if ( nrow(allCourses) > 0 ) {
    for ( row in 1:nrow(allCourses) )
    {
      course_id <- allCourses[row, "course_id"]
      course_name <- allCourses[row, "course_name"]
      course_grades <- getCourseAssessments(course_id)
      course_grades$CourseName <- course_name
      
      cols_to_keep <- c("CourseName", "Grade")
      course_grades <- course_grades[cols_to_keep]
      grades_df <- rbind(grades_df , course_grades )
      colnames(grades_df) <- cols_to_keep
    }

    # Generate box plots
    p <- ggplot(grades_df, aes( group=CourseName, y=Grade, fill=CourseName )) +
      geom_boxplot() +
      ggtitle("Course Section Performance")
  
    p

  } else {
    p <- ggplot() +
      ggtitle(paste0("No course sections found"))
    p

  }
})

#####################################################
# Pie chart showing percent of students who got
# each assessment question right or wrong
#####################################################
output$question_performance_plot<- renderPlot({
  
  user_id <- credentials()$info$user_id

  # Get the data
  percents <- getTeacherQuestionMetrics(user_id)
  if ( nrow(percents) > 0 ) {
  
    # Create Data
    data <- subset( percents, Question == 1 )
    
    # Basic piechart
    p1 <- ggplot(data, aes(x="", y=Percent, fill=Correct)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      ggtitle(paste0("Question ", 1, ": Percent Right"))
    
    # Create Data
    data <- subset( percents, Question == 2 )
    
    # Basic piechart
    p2 <- ggplot(data, aes(x="", y=Percent, fill=Correct)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      ggtitle(paste0("Question ", 2, ": Percent Right"))
  
    
    # Create Data
    data <- subset( percents, Question == 3 )
    
    # Basic piechart
    p3 <- ggplot(data, aes(x="", y=Percent, fill=Correct)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      ggtitle(paste0("Question ", 3, ": Percent Right"))
    
    joint_graph <- grid.arrange( p1, p2, p3,
                  nrow = 2,
                  top = "Question Performance" )
  
    joint_graph
  } else {
    p <- ggplot() +
      ggtitle(paste0("No course sections found"))
    p
  }
})

########################################################################
# When the teacher selects a student on the dashboard,
# show the individual student data below
########################################################################
observeEvent(input$teacher_students_tbl_rows_selected, {
  
  # Get my current user ID
  user_id <- credentials()$info$user_id


  # Reload the course id
  df <- getTeacherCourses(user_id)
  selectedRow <- input$teacher_sections_tbl_rows_selected
  course_id <- df[selectedRow, "course_id"]
  
  # Load the student data  
  df <- getCourseStudents(course_id)

  # Get the currently selected student
  selectedRow <- input$teacher_students_tbl_rows_selected
  student_id <- df[selectedRow, "user_id"]
  
  # Debugging code
  #print(paste("Selected student: ", student_id))
  
  # Load the student data  
  studentData <- getStudentData(student_id)
  
  # Update the data table
  output$teacher_student_detail_tbl <- DT::renderDT(studentData, selection = 'single', options = list(searching = FALSE))
  
  # Clear the student answers
  output$teacher_student_answer_tbl <- DT::renderDT(NULL, selection = 'single')
  
})



########################################################################
# Allow the teacher to see the students complete answers
# for a given assessment
########################################################################
observeEvent(input$teacher_student_detail_tbl_rows_selected, {
  
  # Get my current user ID
  user_id <- credentials()$info$user_id
  
  
  # Reload the course id
  df <- getTeacherCourses(user_id)
  selectedRow <- input$teacher_sections_tbl_rows_selected
  course_id <- df[selectedRow, "course_id"]
  
  # Load the student data  
  df <- getCourseStudents(course_id)
  
  # Get the currently selected student
  selectedRow <- input$teacher_students_tbl_rows_selected
  student_id <- df[selectedRow, "user_id"]

  # Load the student data  
  df <- getStudentData(student_id)
  selectedRow <- input$teacher_student_detail_tbl_rows_selected
  assessment_id <-df[selectedRow, "Assessment ID"]

  assessmentData <- getAssessmentAnswers(assessment_id)
  output$teacher_student_answer_tbl <- DT::renderDT(assessmentData, selection = 'single', options = list(searching = FALSE))
  
})

######################################
# Add new student
######################################
observeEvent(input$btnTeacherAddUser, {
  if (credentials()$user_auth)
  {
    user_id <- credentials()$info$user_id
    
    student_first_name = input$add_student_user_first_name
    student_last_name = input$add_student_last_name
    student_email = input$add_student_email
    student_username = input$add_student_username
    student_passwd = input$add_student_password
    student_section = input$add_student_course_id

    # Add the question to the database
    studentId <-
      addNewStudent(student_first_name, student_last_name, student_email, student_username, student_passwd, student_section)
    
    if (studentId > 1) {
      shinyalert("Success!", "New Student Added", type = "success")

    } else {
      shinyalert("Oops!",
                 "The student cound not be added. Please check your input and try again.",
                 type = "error")
    }
  } else {
    shinyalert("Oops!", "User not logged in!", type = "error")
  }
})



######################################
# Add new courses section
######################################
observeEvent(input$btnAddCourseSection, {
  if (credentials()$user_auth)
  {
    user_id <- credentials()$info$user_id
    
    q_semester = input$add_course_semester
    q_year = input$add_course_year
    q_name = input$add_course_name
    
    
    # Add the question to the database
    result <-
      addNewCourseSection(q_semester, q_year, q_name, user_id)
    
    if (result == TRUE) {
      shinyalert("Success!", "New Course Section Added", type = "success")
      
      # Update section tables
      output$teacher_courses_tbl <-
        DT::renderDT(getTeacherCourses(user_id), selection = 'single' )
      output$teacher_sections_tbl <-
        DT::renderDT(getTeacherCourses(user_id), selection = 'single' )

      # Call the database to get current list of courses
      myCourses <- getTeacherCoursesSelect(user_id)
      
      # Update courses based on the teacher who is currently logged in
      updateSelectInput(session, "add_student_course_id", choices = myCourses)
      
    } else {
      shinyalert("Oops!",
                 "The course section was not added. Please check your input and try again.",
                 type = "error")
    }
  } else {
    shinyalert("Oops!", "User not logged in!", type = "error")
  }
})


######################################
# Add new question
######################################
observeEvent(input$btnAddQuestion, {
  if (credentials()$user_auth)
  {
    user_id <- credentials()$info$user_id
    question_group = input$question_group
    question = input$txt_question
    qtype = input$question_type
    answer = input$txt_question_answer
    
    # Add the question to the database
    result <-
      addNewQuestion(question_group, question, qtype, answer)
    
    if (result == TRUE) {
      shinyalert("Success!", "New Question Added", type = "success")
      
      # Update the questions table
      output$questions_tbl <- DT::renderDT(getQuestions())
      
    } else {
      shinyalert("Oops!",
                 "Your question was not added. Please check your input and try again.",
                 type = "error")
    }
  } else {
    shinyalert("Oops!", "User not logged in!", type = "error")
  }
})
