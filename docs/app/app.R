library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)

# Data
fetch_data <- function() {
  read.csv("https://docs.google.com/spreadsheets/d/1eeOczpiN6QZ2Xq0Dj18do4Wc91U9vzTN_v2glUzesRg/export?format=csv")
}

# Predefined options
materials_options <- c("Lectures", "Seminars", "Assignments", "Active Learning Activities", "Methods Training", "Exams or Quizzes", "Reading List")
tags_options <- c(
  "Activism/social movements", "Advertising", "Agenda setting", "AI and algorithms", "Censorship",
  "Comparative analysis", "Computational methods", "Content analysis", "Deliberation and political conversation",
  "Discourse analysis", "Disinformation conspiracy fact-checks", "Elections and voting behavior", "Emotion",
  "Engagement and participation", "Entertainment popular culture satire", "Equity and diversity",
  "Ethnicity and race", "Ethnography and in-depth interviews", "Experimental research", "Framing",
  "Gender and politics", "Identity", "Ideologies/values", "Incivility negativity", "International politics",
  "Internet/technologies", "Language/symbolic politics", "Media & politics", "Methods", "Network analysis", "News and journalism",
  "Persuasion", "Polarization & partisanship", "Political campaigns", "Political knowledge & sophistication",
  "Political psychology", "Political public relations & marketing", "Populism", "Priming", "Propaganda",
  "Public opinion", "Public sphere", "Qualitative methodology", "Quantitative methodology",
  "Social capital and/or trust", "Social media", "Survey research", "Terrorism", "Textual analysis", "Visual analysis"
)
education_levels <- c("Bachelor´s", "Master´s", "PhD", "Lifelong Learning")
language_options <- c(
  "Arabic", "Cantonese (Chinese)", "Danish", "Dutch", "English", "Finnish", "French", "Hebrew", "Hindi",
  "German", "Indonesian", "Italian", "Japanese", "Korean", "Mandarin (Chinese)", "Norwegian", "Portuguese", "Spanish", "Other"
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "PolComm Teaching DB"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Search Guide", tabName = "guide", icon = icon("search")),   
      menuItem("Teaching Materials", tabName = "courses", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12, title = "Welcome to the Political Communication Teaching Database", solidHeader = TRUE, status = "primary",
                    p("The Political Communication Teaching Database is a collaborative repository supported by the APSA and ICA Political Communication Divisions designed to facilitate the sharing and exploration of teaching materials in political communication. It allows teachers to upload and access syllabi and course materials across a diverse range of subfields."),
                    p("Users can contribute materials via a shared link (e.g., Google Drive, Dropbox), and all entries are organized in an interactive dashboard, enabling easy filtering by subject, language, and instruction level. This database serves as a resource for scholars looking to enhance their political communication courses with high-quality, peer-shared content."),
                    p("To submit materials, click ",
                      a("here", href="https://tally.so/r/mDMGb5", target="_blank"), "."),
                    p("If you have questions,
suggestions, or want to update your upload later, please contact Isabella Gonçalves (igoncalves@uni-mainz.de) or Michael Bossetta (michael.bossetta@iko.ilu.se)"),
                    img(src="https://pbs.twimg.com/profile_images/1530492115247149056/V1z9MLnE_400x400.jpg", height = "100px")
                )
              )
      ),
      tabItem(tabName = "guide",
              fluidRow(
                box(width = 12, title = "🔍 Search Guide 🔍", solidHeader = TRUE, status = "primary",
                    tags$p("Welcome to the PolComm Teaching Database! This document provides a short guide on how to search and navigate the database. There are three main ways to search for materials."),
                    
                    tags$h4("Search Courses (Free Text Search)"),
                    tags$p("The first way to search is the Search Courses feature at the top-left of the page. With this search bar, you can input text or keywords and see which results are generated. The search bar is designed to return results that match the Course Title, Instructor Name, Language, Tagged Keywords, and text that uploaders have written in the Course Description. We recommend trying different combinations of keywords alone, as well as in combination, to explore what types of matches are generated for you."),
                    
                    tags$h4("Filters"),
                    tags$p("The second way to search the database is to use the Filters feature on the left of the page. With these filters, you can select or combine four categories:"),
                    
                    tags$h5("🌍 Language"),
                    tags$p("This includes the top 20 most spoken languages of Political Communication members. If you do not see the language that you need, you can type it into the Search Courses bar instead."),
                    
                    tags$h5("🎓 Education Level"),
                    tags$p("Here you can select the level of the course materials: Bachelor’s, Master’s, PhD, or Lifelong Learning (i.e., public workshops or non-university settings)."),
                    
                    tags$h5("🧩 Materials Included"),
                    tags$p("Each upload is marked by the author for the types of teaching materials that the course contains:"),
                    tags$ul(
                      tags$li("Lectures: Slides, pre-recorded lectures, outlines"),
                      tags$li("Seminars: Small group activities, discussion guides, other in-class seminar tasks"),
                      tags$li("Assignments: Essay prompts, take-home tasks, grading rubrics"),
                      tags$li("Active Learning Activities: Simulations, role-plays, games"),
                      tags$li("Methods Training: Coding tutorials, data files, software demos, interview guides"),
                      tags$li("Exams or Quizzes: Multiple choice test, final exams, grading rubrics"),
                      tags$li("Reading List: Literature list, syllabi, external materials for students to read")
                    ),
                    
                    tags$h5("🏷️ Tags"),
                    tags$p("These are 50 thematic keywords that relate to sub-areas within PolComm (both theory and method). Uploaders can select several when uploading their materials to the database."),
                    
                    tags$h4("Combining Search + Filters"),
                    tags$p("The third way to search the database is to combine the search and filters features. Here, you can set filters first, and then type free-text keywords into the search bar. Results will only be returned that meet the filter criteria."),
                    tags$p("This can be useful if you are looking for something specific, such as a single lecture or assignment. You can set the filters to narrow down your results to education level, language, and type of teaching material (e.g., a lecture or seminar task). Once the filters are set, you can then input your own keywords into the search bar to see what results are generated that meet your search criteria within the set filters."),
                    
                    tags$h4("Get in Touch"),
                    tags$p("We are very happy to hear how your experience went and if there are any ways that we can adjust the navigation features to make them more helpful. Please don’t hesitate to contact Isabella Gonçalves (igoncalves@uni-mainz.de) and Michael Bossetta (michael.bossetta@iko.lu.se) with suggestions or feedback.")
                )
              )
      ),
      tabItem(tabName = "courses",
              fluidRow(
                column(width = 12,
                       textInput("global_search", "Search Courses:", placeholder = "Search by title, description, instructor, tags..."),
                       tags$hr(style = "margin-top: -10px;")
                )
              ),
              fluidRow(
                column(width = 3,
                       box(width = NULL, title = "Filters", solidHeader = TRUE, status = "info",
                           pickerInput("language", "Language:", choices = language_options, multiple = TRUE, options = list(`actions-box` = TRUE)),
                           pickerInput("level", "Education Level:", choices = education_levels, multiple = TRUE, options = list(`actions-box` = TRUE)),
                           pickerInput("materials", "Materials Included:", choices = materials_options, multiple = TRUE, options = list(`actions-box` = TRUE)),
                           pickerInput("tags", "Tags:", choices = tags_options, multiple = TRUE, options = list(`actions-box` = TRUE)),
                           actionButton("refresh", "Refresh Data", class = "btn-primary")
                       )
                ),
                column(width = 9,
                       box(width = NULL, title = "Courses", solidHeader = TRUE, status = "success",
                           DTOutput("table"),
                           uiOutput("no_results")
                       )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data_reactive <- reactive({
    input$refresh
    isolate({
      df <- fetch_data()
      
      if (!is.null(input$language) && length(input$language) > 0) {
        df <- df[df$Language %in% input$language, ]
      }
      
      if (!is.null(input$level) && length(input$level) > 0) {
        df <- df[df$Level %in% input$level, ]
      }
      
      if (!is.null(input$tags) && length(input$tags) > 0) {
        df <- df[sapply(df$Tags, function(tag_str) {
          any(trimws(unlist(strsplit(tag_str, ";|,"))) %in% input$tags)
        }), ]
      }
      
      if (!is.null(input$materials) && length(input$materials) > 0) {
        df <- df[sapply(df$Materials, function(mat_str) {
          any(trimws(unlist(strsplit(mat_str, ";|,"))) %in% input$materials)
        }), ]
      }
      
      return(df)
    })
  })
  
  # NEW REACTIVE for filtered table (global search applied)
  filtered_data <- reactive({
    df <- data_reactive()
    
    # Apply global search
    if (!is.null(input$global_search) && input$global_search != "") {
      search_term <- tolower(input$global_search)
      df <- df[grepl(search_term, tolower(
        paste(
          df$Title,
          df$Instructor,
          df$Language,
          df$Tags,
          df$Description,
          sep = " "
        )
      )), ]
    }
    
    # Add Preview column - USER-FRIENDLY VERSION: always shows 🔍
    df$Preview <- ifelse(
      is.na(df$Description) | trimws(df$Description) == "",
      "",
      ifelse(
        nchar(trimws(df$Description)) > 80,
        paste0(substr(trimws(df$Description), 1, 80), "... 🔍"),
        paste0(trimws(df$Description), " 🔍")
      )
    )
    
    return(df)
  })
  
  # Render Table
  output$table <- renderDT({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    datatable(
      df[, c("Title", "Instructor", "Language", "Preview")],
      colnames = c("Title", "Instructor", "Language", "Description Preview"),
      selection = "single",
      options = list(
        pageLength = 5,
        search = list(regex = TRUE),
        dom = 'lrtip'
      )
    )
  })
  
  # Observe Row Selection
  observeEvent(input$table_rows_selected, {
    selected_row <- input$table_rows_selected
    if (!is.null(selected_row) && length(selected_row) > 0) {
      selected_course <- filtered_data()[selected_row, ]
      showModal(modalDialog(
        title = selected_course$Title,
        strong("Instructor: "), selected_course$Instructor, br(),
        strong("Language: "), selected_course$Language, br(),
        strong("Level: "), selected_course$Level, br(),
        strong("Description: "), selected_course$Description, br(),
        strong("Tags: "), selected_course$Tags, br(),
        strong("Materials Included: "), selected_course$Materials, br(),
        HTML(sprintf('<a href="%s" target="_blank">Download Syllabus</a>', selected_course$Syllabus)), br(),
        HTML(sprintf('<a href="%s" target="_blank">Download Materials</a>', selected_course$Materials_Link)),
        easyClose = TRUE
      ))
    }
  })
}

# Run the app
shiny(ui, server)
