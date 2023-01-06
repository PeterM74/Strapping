library(DBI)
library(RMariaDB)
library(geosphere)
library(shiny)
library(shinyMobile)
library(shinyMatrix)
library(tidyverse)

# Load functions
purrr::map(list.files(path = "./R/", pattern = ".R$", recursive = TRUE, full.names = TRUE),
           source) %>%
  invisible()

# Define unchanging constants for permanent load
Settings <- fGetSettings()

# Connect to DB
DBCon <- fBuildDBConnection(Settings)



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
          
          shiny::uiOutput("WorkoutPageUI")
          
        ),
        
      ),
      
      ## Third tab (hidden) - create new workout -----
      shinyMobile::f7Tab(
        
        tabName = "NewWorkout",
        title = "New Workout",
        icon = shinyMobile::f7Icon("star_fill"),
        hidden = TRUE,
        
        
        # Info help page
        shiny::div(shinyMobile::f7Icon("info_circle"),
                   `data-sheet` = "#CreateNewWorkoutInfoSheet",
                   class = "sheet-open",
                   style = "float: right; margin: 2% 2% 0 0;"),
        shinyMobile::f7Sheet(id = "CreateNewWorkoutInfoSheet",
                             label = "Help",
                             orientation = "bottom",
                             swipeToClose = TRUE,
                             swipeToStep = FALSE,
                             shinyMobile::f7Padding(
                               shiny::p("TODO: HELP INFO TO GO HERE")
                             )),
        
        # Title
        shiny::tags$u(shinyMobile::f7Padding(shiny::h2("Create new workout", 
                                                       style = paste0("color: ", Settings$ColourTheme), 
                                                       .noWS = "after"),
                                             side = "horizontal")),
        
        # Workout name
        shinyMobile::f7Padding(shinyMobile::f7Text("NewWorkoutName",
                                                   label = "",
                                                   value = "",
                                                   placeholder = "Name workout"),
                               side = "horizontal"),
        
        # Generate input programmatically
        shiny::uiOutput("NewWorkoutPageUI"),
        
        # Add exercise button
        shiny::div(shinyMobile::f7Button(inputId = "CreateNewWorkoutAddExerciseBtn",
                                         label = "Add new exercise",
                                         color = Settings$ColourTheme,
                                         rounded = TRUE),
                   style = paste0("margin: 0 auto; width: 50%")),
        
        # Save/cancel button
        shinyMobile::f7Segment(
          container = "row",
          shinyMobile::f7Button(inputId = "CreateNewWorkoutSaveBtn",
                                label = "Save",
                                fill = FALSE,
                                color = Settings$ColourTheme,
                                rounded = TRUE),
          shinyMobile::f7Button(inputId = "CreateNewWorkoutCancelBtn",
                                label = "Cancel",
                                fill = FALSE,
                                color = Settings$ColourTheme,
                                rounded = TRUE)
        )
        
      ),
      
      # Fourth tab - Progress -----
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
  UserID <- shiny::reactive({
    
    Query <- parseQueryString(session$clientData$url_search)
    if (!is.null(Query[['User']])) {
      UserID <- Query[['User']]
    } else {
      UserID <- "Anonymous"
    }
    
    return(UserID)
    
  })
    
  output$UserID <- shiny::renderText({UserID()})
  
  
  
  # Detect active workout - TODO -----
  ## Eventual dynamic list contents, but Status always set
  ActiveWorkout <- list()
  ActiveWorkout$Status <- shiny::reactive({
    
    CheckForTabChange <- input$tabs
    fCheckForActiveWorkout(UserID(), DBCon, Settings)
    
  })
  ActiveWorkout$Table <- shiny::reactive({
    
    if (ActiveWorkout$Status()) {
      
      fActiveWorkoutTable(UserID(), DBCon, Settings)
      
    } else {
      
      FALSE
      
    }
    
  })
  
  # Home page tab output -----
  ## Homepage active workout detection section
  output$HomePageActiveWorkoutUI <- shiny::renderUI({
    
    if (ActiveWorkout$Status()) {
      
      # Add purrr for number of workouts active
      shinyMobile::f7Card()
      
    } else {
      # No workouts are active
      shiny::tagList(
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
    
    ## Active workout detection section
    if (ActiveWorkout$Status()) {
      
      ActiveWorkoutSection <- shiny::tagList(
        
        shiny::tags$u(shinyMobile::f7Padding(shiny::h2("Active workouts", 
                                                       style = paste0("color: ", Settings$ColourTheme), 
                                                       .noWS = "after"), side = "left")),
        # Add purrr for number of workouts active
        shinyMobile::f7Card()
      )
      
      
    } else {
      # No workouts are active
      ActiveWorkoutSection <- shiny::tagList(
        shiny::tags$u(shinyMobile::f7Padding(shiny::h2("Choose a workout to start:", 
                                                       style = paste0("color: ", Settings$ColourTheme), 
                                                       .noWS = "after"), side = "left"))
      )
    }
    
    
    if (!purrr::is_empty(WorkoutTypes)) {
      
      ExistingWorkoutListSection <- shiny::tagList(
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
                      
                    })
        
      )
      
    } else {
      
      ExistingWorkoutListSection <- NULL
      
    }
    
    BuildNewWorkoutCard <- shinyMobile::f7Card(title = "Create new workout",
                        shiny::tagList(
                          shinyMobile::f7Padding(
                            shiny::actionLink(inputId = "CreateNewWorkout",
                                              label = "Build workout",
                                              icon = shinyMobile::f7Icon("plus_square_fill_on_square_fill", 
                                                                         color = Settings$ColourTheme)),
                            side = "horizontal"
                          )))
    
    
    # Return workout ui
    shiny::tagList(
      
      ActiveWorkoutSection,
      ExistingWorkoutListSection,
      BuildNewWorkoutCard
      
    )
    
  
  
  })
  
  ### Start new workout link activated
  shiny::observeEvent(input$CreateNewWorkout, {
    shinyMobile::updateF7Tabs(id = "tabs", selected = "NewWorkout")
  })
  
  
  
  # Create new workout page UI -----
  ## Add exercise button logic
  CreateNewWorkoutAddExerciseBtn <- shiny::reactive({
    
    # Put max workout in place
    if (is.na(input$CreateNewWorkoutAddExerciseBtn) | is.null(input$CreateNewWorkoutAddExerciseBtn) | input$CreateNewWorkoutAddExerciseBtn == 0) {
      
      1
      
    } else if (input$CreateNewWorkoutAddExerciseBtn >= 10) {
      
      shinyMobile::f7Toast("Maximum of 10 exercises allowed!",
                           position = "bottom",
                           closeButtonColor = Settings$ColourThemeLight,
                           closeTimeout = 10000)
      10
      
    } else {
      
      input$CreateNewWorkoutAddExerciseBtn + 1
      
    }
    
  })
  
  ## Stored values for exercises
  MatrixDefaultTemplate <- matrix("", nrow = 3, ncol = 2, byrow = TRUE,
                                  dimnames = list(1:3, Settings$TableColNames[1:2]))
  CreateNewWorkoutTempSaveState <- shiny::reactiveValues(
    Exc1 = list("", "", MatrixDefaultTemplate),
    Exc2 = list("", "", MatrixDefaultTemplate),
    Exc3 = list("", "", MatrixDefaultTemplate),
    Exc4 = list("", "", MatrixDefaultTemplate),
    Exc5 = list("", "", MatrixDefaultTemplate),
    Exc6 = list("", "", MatrixDefaultTemplate),
    Exc7 = list("", "", MatrixDefaultTemplate),
    Exc8 = list("", "", MatrixDefaultTemplate),
    Exc9 = list("", "", MatrixDefaultTemplate),
    Exc10 = list("", "", MatrixDefaultTemplate)
  )
  
  ## Ui output
  output$NewWorkoutPageUI <- shiny::renderUI({
    
    # Create inputs
    InputSection <- purrr::map(1:CreateNewWorkoutAddExerciseBtn(),
                               function(ExerciseCounter) {
                                 
       
      shinyMobile::f7Block(inset = TRUE, strong = TRUE,
                           shinyMobile::f7BlockHeader(paste("Exercise", ExerciseCounter)),
                           shinyMobile::f7Text(paste0("ExerciseName", ExerciseCounter),
                                               label = "",
                                               value = CreateNewWorkoutTempSaveState[[paste0("Exc", ExerciseCounter)]][[1]],
                                               placeholder = paste0("Exercise name")),
                           shinyMobile::f7TextArea(paste0("ExerciseInfo", ExerciseCounter),
                                                   label = "",
                                                   value = CreateNewWorkoutTempSaveState[[paste0("Exc", ExerciseCounter)]][[2]],
                                                   resize = TRUE,
                                                   placeholder = paste0("Insert exercise details here")),
                           shinyMatrix::matrixInput(paste0("ExerciseMatrix", ExerciseCounter),
                                                    value = CreateNewWorkoutTempSaveState[[paste0("Exc", ExerciseCounter)]][[3]],
                                                    rows = list(extend = TRUE, delete = TRUE, names = TRUE),
                                                    cols = list(names = TRUE, extend = TRUE, delta = 0)))
                                 
                               })

    
    # Return object
    shiny::tagList(
      
      InputSection
      
    )
    
  })
  
  ## Save exercise data in temp state when inputs updated
  shiny::observe({

    for (i in seq(CreateNewWorkoutAddExerciseBtn())) {
      
      # Exercise name store
      if (is.character(input[[paste0("ExerciseName", i)]])) {
        
        CreateNewWorkoutTempSaveState[[paste0("Exc", i)]][[1]] <- input[[paste0("ExerciseName", i)]]
        
      }
      
      # Exercise info store
      if (is.character(input[[paste0("ExerciseInfo", i)]])) {
        
        CreateNewWorkoutTempSaveState[[paste0("Exc", i)]][[2]] <- input[[paste0("ExerciseInfo", i)]]
        
      }
      
      # Exercise matrix store
      if (is.matrix(input[[paste0("ExerciseMatrix", i)]])) {
        
        CreateNewWorkoutTempSaveState[[paste0("Exc", i)]][[3]] <- input[[paste0("ExerciseMatrix", i)]]
        
      }

    }
    
    print(CreateNewWorkoutTempSaveState$Exc2)

  })
  
  shiny::observeEvent(input$CreateNewWorkoutSaveBtn, {
    
    # Check workout name is unique for user
    UniqueWorkoutNames <- fUniqueWorkoutNames(UserID(), DBCon, Settings) %>%
      dplyr::pull()
    
    if (input$NewWorkoutName %in% UniqueWorkoutNames) {
      
      shinyMobile::f7Dialog(id = "CreateNewWorkoutSameWONameAlert",
                            title = "Workout name not unique",
                            text = paste("Workout name must be unique.",
                                         input$NewWorkoutName,
                                         "is already in use for this user.<br>Please rename."),
                            type = "alert")
      
    } else {
      
      # Store new WorkoutType in DB
      NumberOfExercises <- isolate(CreateNewWorkoutAddExerciseBtn())
      NewWorkoutTable <- fCreateNewWorkoutTableFormat(isolate(input), UserID(), 
                                                      NumberOfExercises, Settings)
      fStoreDataInDB(NewWorkoutTable,
                     DBTable = Settings$DBTableName$Types,
                     DBCon = DBCon,
                     Settings)
      
    }
    
  })
  
  shiny::observeEvent(input$CreateNewWorkoutCancelBtn, {
    
    shinyMobile::updateF7Tabs(id = "tabs", selected = "Workout")
    
  })
    
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
