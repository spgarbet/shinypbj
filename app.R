#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(pain21)
library(pbj)
library(papayaWidget)

# Javascript to enable/disable tabs
# https://stackoverflow.com/questions/31703241/activate-tabpanel-from-another-tabpanel/31719425#31719425
jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}
.papaya-toolbar span#File {
  display: none;
}
"

pain_images <- function(patient)
{
    vault  <- pain21()
    fnames <- c(
        vault$template,
        vault$data$images[patient],
        # mask
        vault$data$varimages[patient]

    )
    names(fnames)  = neurobase::nii.stub(fnames, bn = TRUE)
    fnames
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    extendShinyjs(text=jscode),
    inlineCSS(css),
    # Application title
    titlePanel("Shiny PBJ"),
    
    tabsetPanel(
      tabPanel(title="Data",
        fluidPage(
          radioButtons(
            "source",
            "Select Source",
            c("Pain 21 Study", "Upload"),
            "Pain 21 Study"
          ),
          fileInput("studydata", "Study Data"),
          fileInput("varimages", "Var Images"),
          fileInput("template",  "Template Nifti Location"),
          fileInput("mask",      "Mask Nifti Location"),

          hr(),
          selectInput("dataRow", h3("Data Row"), choices=as.list(1:21), selected=1),
          papayaOutput("visualize")
        )
      ),
      tabPanel(title="Model"),
      tabPanel(title="Inference"),
      tabPanel(title="Visualize"),
      tabPanel(title="Back Matter")
    )
)

hideVisualizer <- function()
{
  shinyjs::hide("visualize")
  shinyjs::hide("dataRow")
}
showVisualizer <- function()
{
  shinyjs::show("visualize")
  shinyjs::show("dataRow")
}
hideUpload <- function()
{
  shinyjs::hide("studydata")
  shinyjs::hide("varimages")
  shinyjs::hide("template")
  shinyjs::hide("mask")
}
showUpload <- function()
{
  shinyjs::show("studydata")
  shinyjs::show("varimages")
  shinyjs::show("template")
  shinyjs::show("mask")
}
dataUploaded <- function(input)
{
  !is.null(input$studydata) &&
  !is.null(input$varimages) &&
  !is.null(input$template )
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    js$disableTab("Inference")
    js$disableTab("Visualize")
    hideUpload()

    output$visualize <- renderPapaya({
        fnames <- pain_images(as.numeric(input$dataRow))
        img    <- papaya(fnames)
        img$elementId <- NULL
        img
    })

    observeEvent(input$source, {
      if(input$source == "Upload")
      {
        showUpload()
        if(!dataUploaded(input)) hideVisualizer()
      } else {
        hideUpload()
        showVisualizer()
      }
    })
    
    observeEvent(input$studydata, {
      if(dataUploaded(input)) showVisualizer()
    })
    observeEvent(input$varimages, {
      if(dataUploaded(input)) showVisualizer()
    })
    observeEvent(input$template,  {
      if(dataUploaded(input)) showVisualizer()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
