if (!requireNamespace("miniUI", quietly = TRUE)) {
  message("installing miniUI so can run config app")
  install.packages("miniUI")
}
if (!requireNamespace("gert", quietly = TRUE)) {
  message("installing gert so can run config app")
  install.packages("gert")
}
library(shiny)
library(miniUI)
# Define UI for application that draws a histogram
ui <- miniPage(
  
  # Application title
  gadgetTitleBar("System config"),
  miniContentPanel(
    column(6,
           uiOutput("missing_git"),
           textInput("git_user_name", "user.name"),
           textInput("git_user_email", "user.email"),
           actionButton("set_git_config", "Set Config"),
           h4("Current git configurations set:"),
           tableOutput("git_name")
    ),
    column(6,
           actionButton("show_ssh_instructions", "Instructions for ssh"),
           h3("Upload tarball of ssh keys"),
           fileInput("ssh_key", NULL, buttonLabel = "upload..."),
           uiOutput('ssh_result')
    )
  )
)

is_defined_value <- function(.x) {
  if(is.null(.x) || .x == "") {
    return(FALSE)
  }
  return(TRUE)
}
# Define server logic required to draw a histogram
server <- function(input, output) {
  dataModal <- function(failed = FALSE) {
    modalDialog(
      h4("To add the ssh keys from your laptop onto your workflow, run the following commands from your laptop terminal"),
      wellPanel(tags$p('cd ~/.ssh'), tags$p('tar -czvf ~/Desktop/keys.tar.gz id_rsa id_rsa.pub')),
      h4("upload the resulting tarball (keys.tar.gz) available on your desktop"),
      h4('You only need to do this once per disk'),
      footer = modalButton("Cancel")
    )
  }
  
  # Show modal when button is clicked.
  observeEvent(input$show_ssh_instructions, {
    showModal(dataModal())
  })
  
  config_changed <- eventReactive(input$set_git_config, {
    values_set <- c()
    if (is_defined_value(input$git_user_email)) {
      gert::git_config_global_set("user.email", input$git_user_email) 
      values_set <- c(values_set, "user.email")
    }
    if (is_defined_value(input$git_user_name)) {
      gert::git_config_global_set("user.name", input$git_user_name) 
      values_set <- c(values_set, "user.name")
    }
    if (!length(values_set)) {
      return(NULL)
    }
    return(values_set)
  }, ignoreNULL = FALSE)
  
  git_config_vals <- reactive({
    # reactive as want to run this at beginning so can't be eventreactive
    config_changed()
    gert::git_config_global()
  }) 
  
  git_config_suggestions <- reactive({
    current_config <- git_config_vals()
    missings <- c()
    if (!("user.email" %in% current_config$name)) {
      missings <- c(missings, "user.email")
    }
    if (!("user.name" %in% current_config$name)) {
      missings <- c(missings, "user.name")
    }
    if (!length(missings)) {
      return(NULL)
    }
    return(missings)
  })
  output$missing_git <- renderUI({
    req(git_config_suggestions())
    wellPanel(
      h3("please set the following config item(s):"),
      h3(tags$span(style="color:red", paste(git_config_suggestions(), collapse = ", ")))
    )        
  })
  output$git_name <- renderTable({
    git_config_vals()
  })
  
  ssh_copy <- reactive({
    req(input$ssh_key)
    ex_dir <- file.path(tempdir(), "ssh_keys")
    if (fs::dir_exists(ex_dir)) {
      fs::dir_delete(ex_dir)
    }
    fs::dir_create(ex_dir, recurse = TRUE)
    withr::with_dir(ex_dir, {
      untar(input$ssh_key$datapath)
    })
    id_rsa_pub <- file.path(ex_dir, "id_rsa.pub")
    id_rsa <- file.path(ex_dir, "id_rsa")
    copied_files <- c() 
    if (fs::file_exists(id_rsa_pub)) {
      fs::file_copy(id_rsa_pub, "~/.ssh/id_rsa.pub", overwrite = TRUE)
      fs::file_chmod("~/.ssh/id_rsa.pub", "644")
      copied_files <- c(copied_files, "id_rsa.pub")
    }
    if (fs::file_exists(id_rsa)) {
      fs::file_copy(id_rsa, "~/.ssh/id_rsa", overwrite = TRUE)
      fs::file_chmod("~/.ssh/id_rsa", "600")
      copied_files <- c(copied_files, "id_rsa")
    }
    fs::file_chmod("~/.ssh/authorized_keys", "666")
    file.append("~/.ssh/authorized_keys", "~/.ssh/id_rsa.pub")
    fs::file_chmod("~/.ssh/authorized_keys", "600")
    if (!length(copied_files)) {
      return(NULL)
    }
    copied_files
  })
  output$ssh_result <- renderUI({
    req(ssh_copy())
    wellPanel(
      h3("We have set the following key file(s):"),
      h3(tags$span(style="color:red", paste(ssh_copy(), collapse = ", ")))
    ) 
  })
  
  observeEvent(input$done, {
    stopApp()
  })
}

# Run the application 
runGadget(ui, server)