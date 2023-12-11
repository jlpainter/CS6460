###############################################################################
# CS-6460 Education Technology
#
# @file: teacherUI.R
#
# @author: Jeffery Painter <jpainter32@gatech.edu>
#
###############################################################################

# ############################################################################
#  Teacher content
# ############################################################################
teacher_dashboard_tab <- tabPanel(
  title = icon("chart-pie"),
  value = "teacherDashboard",
  
  verbatimTextOutput("selectedRow"),
  
  column(
    width = 8, 
    h1("My Dashboard"),
    
    # Course peformance metrics
    h2("Explore Individual Student Performance"),
    dataTableOutput("teacher_sections_tbl"),
    dataTableOutput("teacher_students_tbl"),
    dataTableOutput("teacher_student_detail_tbl"),
    dataTableOutput("teacher_student_answer_tbl"),
  ),

  column(
    width = 4, 
    h1("Visual Analytics"),
    
    # Course peformance metrics
    h2("Box Plot of Courses"),
    plotOutput("course_performance_plot"),
    
    h2("Individual Question Performance"),
    plotOutput("question_performance_plot")
  )
  
    
)


teacher_courses_tab <- tabPanel(
  title = icon("user"),
  value = "teacherCourseSections",
  column(
    width = 12, 
    h2("Current Course Sections"),
    DT::DTOutput("teacher_courses_tbl"),

    fluidRow(
      column(6, 
        h2("Add a new course section"),
        selectInput("add_course_semester", "Semester:", c("Spring" = 'Spring', "Fall" = 'Fall', "Summer" = 'Summer')),
        textInput("add_course_year", "Course Year", value = "", width = NULL, placeholder = NULL),
        textInput("add_course_name", "Course Name", value = "", width = NULL, placeholder = NULL),
        actionButton("btnAddCourseSection", "Add New Section")
      ),
      
      column(6, 
        h2("Add a new student"),
        
        textInput("add_student_user_first_name", "First Name", value = "", width = NULL, placeholder = NULL),
        textInput("add_student_last_name", "Last Name", value = "", width = NULL, placeholder = NULL),
        textInput("add_student_email", "Email", value = "", width = NULL, placeholder = NULL),
        textInput("add_student_username", "Username", value = "", width = NULL, placeholder = NULL),
        textInput("add_student_password", "Password", value = "", width = NULL, placeholder = NULL),
        selectInput("add_student_course_id", "Course section:", ""),
        actionButton("btnTeacherAddUser", "Add New Student")
      ),
    
    )

  )
)


teacher_question_tab <- tabPanel(
  title = icon("pencil"),
  value = "question",
  column(
    width = 12, 
    h2("Current Assessment Questions"),
    DT::DTOutput("questions_tbl"),
    
    h2("Add a new question"),
    selectInput("add_question_group", "Display Panel:", c("Panel 1" = 1, "Panel 2" = 2, "Panel 3" = 3 )),
    textInput("add_question_label", "New Question", value = "", width = NULL, placeholder = NULL),
    selectInput("add_question_type", "Question Type:", c("Yes or No" = "YesNo", "Numeric" = "number", "Free Response (text)" = "text" )),
    textInput("add_question_answer", "Answer", value = "", width = NULL, placeholder = NULL),
    actionButton("btnAddQuestion", "Add New Question"),
  )
)