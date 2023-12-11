###############################################################################
# CS-6460 Education Technology
#
# @file: admin_server.R
#
# @author: Jeffery Painter <jpainter32@gatech.edu>
#
###############################################################################

  ######################################
  # Reset the database
  ######################################
  observeEvent(input$btnAdminResetDatabase, {
    
    # Normally, we would require the user to be logged
    # in as the admin to reset the entire database.
    # But for the demo, we will allow anyone to reset
    # the database.
    
    DEMO_ONLY = TRUE
    if ( DEMO_ONLY == TRUE )
    {
      
      # Source the script and reset the database
      source('create_database.R',   local = TRUE)
      reset_database()
      rebuild_demo_data()
      
      shinyalert("Success!", "The database has been reset", type = "success")
      
    } else {
        # Not in demo mode, allow only the admin to reset the database
        if (credentials()$user_auth) 
        { 
          user_id <- credentials()$info$user_id
          
          # Check permissions
          user_permission <- credentials()$info$permissions
          
          # Only an admin can reset the entire database
          if ( user_permission == "admin" )
          {
            # Source the script and reset the database
            source('create_database.R',   local = TRUE)
            reset_database()
            shinyalert("Success!", "The database has been reset", type = "success")
          } else {
            shinyalert("Oops!", "You do not have permission to reset the database", type = "error")  
          }
        } else {
          shinyalert("Oops!", "User not logged in!", type = "error")
        }
    }
  })

  ######################################
  # Add new user
  ######################################
  observeEvent(input$btnAdminAddUser, {
    if (credentials()$user_auth) 
    { 
      user_id <- credentials()$info$user_id
      
      # Check permissions
      user_permission <- credentials()$info$permissions
      
      # Only an admin can add more admins and teachers
      if ( user_permission == "admin" )
      {
        new_user_role = input$admin_user_role
        new_user_first_name = input$admin_user_first_name
        new_user_last_name = input$admin_user_last_name
        new_user_email = input$admin_user_email
        new_user_username = input$admin_user_username
        new_user_password = input$admin_user_password
        
        result <- addNewUser(new_user_role, new_user_first_name, new_user_last_name, new_user_email, new_user_username, new_user_password)
        if ( result == TRUE )
        {
          shinyalert("Success!", "New User Added", type = "success")
          
          # Update the user tables
          output$admin_adm_user_table <- DT::renderDT(getUsersByRole(1))
          output$admin_teacher_user_table <- DT::renderDT(getUsersByRole(2))
          
        } else {
          shinyalert("Oops!", "An error occurred adding the user, please check your input and submit again.", type = "error")            
        }

      } else {
        shinyalert("Oops!", "You do not have permission to add a user!", type = "error")  
      }
    } else {
      shinyalert("Oops!", "User not logged in!", type = "error")
    }
  })

