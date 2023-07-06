if (!requireNamespace("shiny", quietly = TRUE)) {
  message("Installing shiny for configinator...")
  install.packages("shiny")
}
if (!requireNamespace("gert", quietly = TRUE)) {
  message("Installing gert for configinator...")
  install.packages("gert")
}

library(shiny)

ui <- fluidPage(
  h1("🐤 Configinator"),
  uiOutput("hostname"),
  fluidRow(
    column(
      6,
      h2("Git"),
      uiOutput("missing_git"),
      textInput("git_user_name", "Full Name"),
      textInput("git_user_email", "Email"),
      actionButton("set_git_config", "Set config"),
      h4("Current git config"),
      tableOutput("git_name")
    ),
    column(
      6,
      h2("SSH"),
      p(
        "If you don't know how to obtain an SSH key, see ",
        a(
          style = "font-weight: bold;",
          "Obtaining an SSH key",
          href = "https://a2-ai.atlassian.net/l/cp/kz10w0fn",
          target = "_blank"
        ),
        " on Confluence."
      ),
      h4("Add an SSH key"),
      textInput(
        "ssh_key",
        label = NULL,
        width = "auto",
        placeholder = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDREhaDeDd4NeG/ClfkByKHUbPgQwuIac5XYwE+R3MDU example-device"
      ),
      actionButton("add_ssh_key", "Add key"),
      uiOutput("ssh_result"),
      h4("Authorized SSH keys"),
      tableOutput("authorized_ssh_keys")
    )
  )
)

is_defined_value <- function(.x) {
  if (is.null(.x) || .x == "") {
    return(FALSE)
  }
  return(TRUE)
}

server <- function(input, output) {
  config_changed <- eventReactive(input$set_git_config,
    {
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
    },
    ignoreNULL = FALSE
  )

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
      h3(tags$span(style = "color:red", paste(git_config_suggestions(), collapse = ", ")))
    )
  })
  output$git_name <- renderTable({
    git_config_vals()
  })
  append_ssh_key <- reactive({
    req(input$add_ssh_key)

    # Append key to authorized keys
    system2("echo", paste0("\"", input$ssh_key, "\" >> ~/.ssh/authorized_keys"))
  })
  output$ssh_result <- renderUI({
    result <- append_ssh_key()
    req(result)

    if (result != 0) {
      p(
        style = "color:red",
        "Error adding SSH key"
      )
    } else {
      p(
        style = "color:green",
        "SSH key added successfully!"
      )
    }
  })
  output$authorized_ssh_keys <- renderTable({
    system2("cat", "~/.ssh/authorized_keys", stdout = TRUE)
  })
  output$hostname <- renderUI(
    p("running at ", system2("hostname", stdout = TRUE), "(make sure this isn't running on your laptop!)")
  )

  observeEvent(input$done, {
    stopApp()
  })
}

# Run the application
app <- shinyApp(ui, server)
runApp(app)
