###############################################################################
# CS-6460 Education Technology
#
# @author: Jeffery Painter <jpainter32@gatech.edu>
#
###############################################################################

#
# External library imports
#
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

library(reactable)
library(shiny)
library(shinyalert)
library(shinyauthr)
library(effsize)
library(pwr)

# ############################################################################
# Local file imports
# ############################################################################

# Load the database support functions
source('dbManager.R', local = TRUE)

# Content panels are divided by role 
# for easier management of UI content
source('adminUI.R',   local = TRUE)
source('teacherUI.R', local = TRUE)
source('studentUI.R', local = TRUE)

# ############################################################################
#      Initialize the application UI with only the login tab to begin
# ############################################################################
ui <- navbarPage(
  title = "CS-6460 Educational Technology",
  id = "tabs",
  collapsible = TRUE,
  login_tab
)

server <- function(input, output, session) 
{
  # Server functions
  source('common_server.R',   local = TRUE)
  source('admin_server.R', local = TRUE)
  source('teacher_server.R', local = TRUE)
  source('student_server.R', local = TRUE)  
}

shinyApp(ui, server)