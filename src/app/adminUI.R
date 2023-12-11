###############################################################################
# CS-6460 Education Technology
#
# @file: adminUI.R
#
# @author: Jeffery Painter <jpainter32@gatech.edu>
#
###############################################################################


# ############################################################################
#  Shared content for all users
# ############################################################################
# login tab ui to be rendered on launch
login_tab <- tabPanel(
  title = icon("lock"), 
  value = "login", 
  shinyauthr::loginUI("login"),
  
  p("Created by: Jeffery Painter - jpainter32@gatech.edu - Fall 2023"),
  
  # For demo purposes, allow the entire database to be reset
  includeHTML("www/default_logins.html"),
  
  h2("Reset database"),  
  actionButton("btnAdminResetDatabase", "Reset Database"),
)

password_tab <- tabPanel(
  title = icon("lock"),
  value = "password",
  column(
    width = 12, 
    tags$h2("Change Password"),
    p("Here you can change your user password. It will be active the next time you login."),
    passwordInput("userPassword1", "New Password", value = "", width = NULL, placeholder = NULL),
    passwordInput("userPassword2", "Confirm Password", value = "", width = NULL, placeholder = NULL),
    actionButton("btnChangePassword", "Change Password")
  )
)

about_tab <- tabPanel("About",
                      includeHTML("www/about.html"))

# additional tabs to be added after login
home_tab <- tabPanel(
  title = icon("user"),
  value = "home",
  column(
    width = 12, 
    tags$h2("User Information"),
    verbatimTextOutput("user_data")
  )
)


# ############################################################################
#  Admin content
# ############################################################################
admin_users <- tabPanel(
  title = icon("user"),
  value = "admin_users",
  column(
    width = 12, 
    h2("Admin Users"),
    DT::DTOutput("admin_adm_user_table"),
    
    h2("Teachers"),
    DT::DTOutput("admin_teacher_user_table"),
    
    h2("Add a new user"),
    selectInput("admin_user_role", "User Role:", c("Admin" = 1, "Teacher" = 2 )),
    textInput("admin_user_first_name", "First Name", value = "", width = NULL, placeholder = NULL),
    textInput("admin_user_last_name", "Last Name", value = "", width = NULL, placeholder = NULL),
    textInput("admin_user_email", "Email", value = "", width = NULL, placeholder = NULL),
    textInput("admin_user_username", "Username", value = "", width = NULL, placeholder = NULL),
    textInput("admin_user_password", "Password", value = "", width = NULL, placeholder = NULL),
    actionButton("btnAdminAddUser", "Add New User"),
    
    # For demo purposes, allow the entire database to be reset
    #h2("Reset database"),
    #p("This will return the database to it's original state for the demo."),
    #actionButton("btnAdminResetDatabase", "Reset Database"),
  )
)
