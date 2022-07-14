pacman::p_load(
  "shiny", "officer", "tidyverse","assertr", "forcats",
  "Cairo", "grid", "gridExtra",
  "rvg", "glue", "lubridate", "here", "janitor", "withr",
  "finalfit", "writexl", "shinyWidgets", "shinydashboard",
  "shinydashboardPlus", "plotly", "shinybusy", "flexdashboard",
  "readxl"
)

# List of inputs used in the app -----------------------------------------------

inputs <- list(

  # folder where data files will be read in from
  input_dir = here("files/input"),

  # list of aesthetic choices
  aesthetics = list(
    dashboard_title = NULL,
    tab_title = "Key Indicator Visuals",
    color = "green"),

  # folder where templates are stored
  template_source = here("files/input/template/"),

  # list of templates
  templates = list(
    template_one = here("files/input/template/Template_SlideCFR.pptx"),
   # template_two = here("files/input/template/Template_SlideRepDeath.pptx"),
   # template_three = here("files/input/template/Template_SlideEpiCurve.pptx")
   ),

  # list of functions used by app
  functions = list(
    clean = here("files/input/functions/clean_jhu_coviddata.R"),
    preview_rmd = here("files/input/preview_CFRs.Rmd"),
    generate_rmd = here("files/input/generate_CFRs.RMD"))
)

# Lists of data files available to use in app ----------------------------------

# Create lists of templates available for data ---------------------------------
template_list <- list.files(inputs$template_source,
  full.names = FALSE,
  pattern = "Template"
)

# Setup user interface for app -------------------------------------------------

ui <- function() {
  ui <- dashboardPage(
    skin = inputs$aesthetics$color,
    # Title
    header = dashboardHeader(
      title = as.character(inputs$aesthetics$dashboard_title),
      titleWidth = 400
    ),
    # Disable ugly sidebar
    sidebar = dashboardSidebar(disable = TRUE),
    # Create body of the page
    body = dashboardBody(
      fluidRow(
        # # progress spinner progress icon
        add_busy_spinner(
          spin = "fading-circle",
          timeout = 100,
          position = "top-left"
        ),
        # Initialize tabs
        # First tab
        tabBox(
          id = "panels",
          tabPanel(
            as.character(inputs$aesthetics$tab_title),
            # select template
            shinyWidgets::pickerInput(
              inputId = "select_template",
              label = "Select a template file:",
              choices = template_list,
              options = list(size = 8)
            ),
            width = 4,
            tags$br(),
            # Figure Preview button
            actionButton(
              inputId = "figure_preview",
              label = "Preview Figure"
            ),
            # Generate slide button
            actionButton(
              inputId = "generate_slide",
              label = "Generate Slide"
            ),
            width = 4,
            tags$br()
          ) # close first tab
        ), # tab box
        box(
          title = "Figure Preview",
          htmlOutput("plot"), width = 12
        )
      ) # fluid row
    ) # dashboard body
  ) # dashboard page
  return(ui)
} # ui function

# Initialize connection to the R Shiny server ----------------------------------
server <- function(input, output, session) {

  # Update on figure preview ---------------------------------------------------

  # if the command is submitted
  # If the user selects to preview a figure
  observeEvent(input$figure_preview, {
    temp <- isolate(input$select_template)

    template_dict <- list(
      # replace with user indicated name of template
      "Template_SlideCFR.pptx" =
        list(
          in_file_p = inputs$functions$preview_rmd,
          out_file = "preview_CFRs.html"
        )
    )

    # Output the plots requested
    output$plot <- renderUI({
      rmarkdown::render(
        input = template_dict[[temp]]$in_file_p,
        params = param_list,
        output_dir = "./www/",
        output_format = "flexdashboard::flex_dashboard",
        quiet = FALSE
      )
      tags$html(
        tags$iframe(
          seamless = "seamless",
          src = template_dict[[temp]]$out_file,
          width = "100%",
          height = "800px",
          id = "reportIframe"
        )
      )
    })
  }) # end slide preview loop

  # If the "Generate Slide button is pushed,
  observeEvent(input$generate_slide, {
    temp <- isolate(input$select_template)

    template_dict <- list(
      # replace with user indicated name of template
      "Template_SlideCFR.pptx" =
        list(in_file_g = inputs$functions$generate_rmd)
    )

    # generate the slides with user-provided parameters
    rmarkdown::render(
      input = template_dict[[temp]]$in_file_g,
      params = param_list,
      quiet = FALSE
    )
  })
} # server
# run complete App -------------------------------------------------------------
shinyApp(ui, server) # Shiny App
