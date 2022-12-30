library(DBI)
library(RMariaDB)
library(geosphere)
library(shiny)
library(shinyMobile)
library(tidyverse)

# Define unchanging constants for permanent load
Settings <- fGetSettings()

# Connect to DB
DBCon <- fBuildConnection(Settings)



# Define UI for application that draws a histogram -----
ui <- shinyMobile::f7Page(
  
  title = "Strapping",
  
  options = list(dark = FALSE,
                 color = Settings$ColourTheme),  # TODO: COLOR DESIGN OF TABS
  
  
  # Core app
  shinyMobile::f7TabLayout(
    
    # Navbar
    navbar = shinyMobile::f7Navbar(
      
      title = "UPDATE TITLE BASED ON TAB",  # TODO: update tab title
      rightPanel = TRUE
      
    ),
    
    # User panel
    panels = shinyMobile::f7Panel(
      
      id = "UserPanel",
      side = "right",
      theme = "light",
      effect = "reveal",
      title = shiny::textOutput("UserID"),
      
      # TODO: change icon to user icon. Insert JS to change innerHTML shiny::tags$script(shiny::HTML())
      shinyMobile::f7Padding(shiny::tags$p("Info on user account to go here TODO"), 
                             side = "horizontal")
      
    ),
    
    shinyMobile::f7Tabs(
      id = "tabs",
      
      # First tab - Home -----
      shinyMobile::f7Tab(
        
        tabName = "Home",
        title = "Home",
        icon = shinyMobile::f7Icon("house_fill"),
        active = TRUE,
        
        # Active workouts section
        shiny::tagList(

          shiny::tags$u(shinyMobile::f7Padding(shiny::h2("Active workouts", 
                                                         style = paste0("color: ", Settings$ColourTheme), 
                                                         .noWS = "after"), side = "left")),
          shiny::uiOutput("HomePageActiveWorkoutUI")
        ),
        
        # Previous workouts section
        shiny::tagList(
          
          shiny::tags$u(shinyMobile::f7Padding(shiny::h2("Previous workouts", 
                                                         style = paste0("color: ", Settings$ColourTheme), 
                                                         .noWS = "after"), side = "left")),
          shiny::uiOutput("HomePagePreviousWorkoutUI")
        )
        

        
      ),
      
      # Second tab - Workout -----
      shinyMobile::f7Tab(
        
        tabName = "Workout",
        title = "Workout",
        icon = shinyMobile::f7Icon("bolt_fill"),
        
        # Active workouts section
        shiny::tagList(
          
          shiny::tags$u(shinyMobile::f7Padding(shiny::h2("Active workouts", 
                                                         style = paste0("color: ", Settings$ColourTheme), 
                                                         .noWS = "after"), side = "left")),
          shiny::uiOutput("WorkoutPageUI")
        ),
        
      ),
      
      # Third tab - Progress -----
      shinyMobile::f7Tab(
        
        tabName = "Progress",
        title = "Progress",
        icon = shinyMobile::f7Icon("chart_bar_fill")
        
      )
    )
    
  )
  
)





# Define server logic required to draw a histogram -----
server <- function(input, output, session) {
  
  # User authentication - TODO -----
  ## For now based on GET parameter called 'User'
  output$UserID <- shiny::renderText({
    
    Query <- parseQueryString(session$clientData$url_search)
    if (!is.null(Query[['User']])) {
      UserID <- Query[['User']]
    } else {
      UserID <- "Anonymous"
    }
    
    return(UserID)
    
  })
  
  # Detect active workout - TODO -----
  ## Eventual dynamic list contents, but Status always set
  ActiveWorkout <- list(
    Status = FALSE
  )
  
  # Home page tab output -----
  ## Homepage active workout detection section
  output$HomePageActiveWorkoutUI <- shiny::renderUI({
    
    if (ActiveWorkout$Status) {
      
      # Add purrr for number of workouts active
      shinyMobile::f7Card()
      
    } else {
      # No workouts are active
      ## TODO: can't seem to change tabs with hyperlink - using fab button instead
      shiny::tagList(
        # Option 1: Plain text with icon - hyperlink not working
        ## shinyMobile::f7Padding(
        ##   shiny::a(href="#Workout", class="tab-link",
        ##            shiny::tags$span(shinyMobile::f7Icon("plus_app_fill", color = k$ColourTheme),
        ##                            shiny::tags$b("Start a new workout"), style = paste0("color: ", k$ColourTheme))),
        ##   side = "left")
        # Option2: action button styled to the left
        ## shiny::div(shinyMobile::f7Button(inputId = "StartNewWorkoutButton",
        ##                                  label = "+ Start new workout",
        ##                                  rounded = TRUE),
        ##            style = "width: 60%; position = absolute; left: 0px;",
        ##            class = " padding-left")
        # Option3: actionlink, no f7 component
        shinyMobile::f7Padding(
          shiny::actionLink(inputId = "StartNewWorkoutButton",
                            label = "Start new workout",
                            icon = shinyMobile::f7Icon("plus_app_fill", color = Settings$ColourTheme)),
          side = "left"
        )
      )
    }
  })
  
  ### Homepage no active workout button logic
  shiny::observeEvent(input$StartNewWorkoutButton, {
    shinyMobile::updateF7Tabs(id = "tabs", selected = "Workout")
  })
  
  
  ## Homepage previous workout section
  output$HomePagePreviousWorkoutUI <- shiny::renderUI({
    
    shinyMobile::f7Padding(shiny::span("TODO: add content"))
    
  })
  
  
  # Detect workout types
  ## TODO: complete by SQL query
  WorkoutTypes <- list(
    Legs = list(),
    Back = list(),
    Shoulders = list()
  )
  
  # Workout tab output -----
  ## Workout page UI content - all generated by server
  
  output$WorkoutPageUI <- shiny::renderUI({
    
    # TODO: if selected workout is true, generate ui for output
    
    ## Active workout detection section
    if (ActiveWorkout$Status) {
      
      shiny::tagList(
        
        shiny::tags$u(shinyMobile::f7Padding(shiny::h2("Active workouts", 
                                                       style = paste0("color: ", Settings$ColourTheme), 
                                                       .noWS = "after"), side = "left")),
        # Add purrr for number of workouts active
        shinyMobile::f7Card()
      )
      
      
    } else {
      # No workouts are active
      shiny::tagList(
        shiny::tags$u(shinyMobile::f7Padding(shiny::h2("Choose a workout to start:", 
                                                       style = paste0("color: ", Settings$ColourTheme), 
                                                       .noWS = "after"), side = "left"))
      )
    }
    
    
    if (!purrr::is_empty(WorkoutTypes)) {
      
      shiny::tagList(
        purrr::imap(WorkoutTypes,
                    function(WorkoutData, WorkoutName) {
                      
                      shinyMobile::f7Card(title = WorkoutName,
                                          shiny::span("Fill summary content"),
                                          footer = shiny::tagList(
                                            shinyMobile::f7Padding(
                                              shiny::actionLink(inputId = "ActivateSavedWorkout",
                                                                label = "Start this workout",
                                                                icon = shinyMobile::f7Icon("arrowshape_turn_up_right_circle_fill", 
                                                                                           color = Settings$ColourTheme)),
                                              side = "horizontal"
                                            )
                                          ))
                      
                    }),
        
        shinyMobile::f7Card(title = "Create new workout",
                            shiny::tagList(
                              shinyMobile::f7Padding(
                                shiny::actionLink(inputId = "CreateNewWorkout",
                                                  label = "Build workout",
                                                  icon = shinyMobile::f7Icon("plus_square_fill_on_square_fill", 
                                                                             color = Settings$ColourTheme)),
                                side = "horizontal"
                              )))
        
      )
      
    }
    
  
  
  })
    
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
