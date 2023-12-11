###############################################################################
# CS-6460 Education Technology
#
# @file: common_server.R
#
# @author: Jeffery Painter <jpainter32@gatech.edu>
#
###############################################################################

  #Setup global variable
  myCourses <- reactiveVal(NULL)
  
  ##############################################################
  # Restart each login/logout with a fresh call to the database
  ##############################################################
  user_base <- setUserBase()
  
  # hack to add the logout button to the navbar on app launch 
  insertUI(
    selector = ".navbar .container-fluid .navbar-collapse",
    ui = tags$ul(
      class="nav navbar-nav navbar-right",
      tags$li(
        div(
          style = "padding: 10px; padding-top: 8px; padding-bottom: 0;",
          shinyauthr::logoutUI("logout")
        )
      )
    )
  )
  
  # call the shinyauthr login and logout server modules
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    sodium_hashed = TRUE,
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # Setup the panels
  observeEvent(credentials()$user_auth, {
    # if user logs in successfully
    if (credentials()$user_auth) { 
      # remove the login tab
      removeTab("tabs", "login")
      
      # Extract the logged in user credentials and user id
      user_permission <- credentials()$info$permissions
      user_id <- credentials()$info$user_id
      
      # Update the user login!
      status <- updateUserLogin(user_id)
      #
      # Debug output
      #
      #if ( status == TRUE ) {
        #print("Added login to table.")
        #tmp <- getUserLoginHistory(user_id)
        #print(paste("Num logins: ", tmp$num_logins))
        #print(paste("Last logins: ", tmp$last_login))
      #}
      
      if (user_permission == "admin") {
        
        # add home tab 
        appendTab("tabs", admin_users, select = TRUE)
        
        # Load this teachers current courses
        output$admin_adm_user_table <- DT::renderDT(getUsersByRole(1))
        output$admin_teacher_user_table <- DT::renderDT(getUsersByRole(2))    
        
      } else if ( user_permission == "teacher" ) {
        
        # Custom tabs for teachers
        appendTab("tabs", teacher_dashboard_tab, select = TRUE)
        appendTab("tabs", teacher_courses_tab)
        
        # Load this teachers current courses
        output$questions_tbl <- DT::renderDT(getQuestions())
        
        allCourses <- getTeacherCourseMetrics(user_id)
        output$teacher_sections_tbl <- DT::renderDT(allCourses, selection = 'single')

        # Call the database to get current list of courses
        myCourses <- getTeacherCoursesSelect(user_id)

        # Update courses based on the teacher who is currently logged in
        updateSelectInput(session, "add_student_course_id", choices = myCourses )

      } else if (user_permission == "student") {
        
        # Student tabs        
        appendTab("tabs", student_tab_1, select = TRUE)

      }
      
      # Every user can change their password
      appendTab("tabs", password_tab)
      appendTab("tabs", about_tab)
      
    }
  })

  
  ######################################
  # Change password
  ######################################
  observeEvent(input$btnChangePassword, {
    if (credentials()$user_auth) 
    { 
      user_id <- credentials()$info$user_id
      pw1 = input$userPassword1
      pw2 = input$userPassword2
      if ( pw1 == pw2 ) {
        shinyalert("Success!", "Password changed!", type = "success")
        updatePassword(user_id, pw1)
      } else {
        shinyalert("Oops!", "Passwords do not match, please update and try again.", type = "error")
      }
    } else {
      shinyalert("Oops!", "User not logged in!", type = "error")
    }
  })  
