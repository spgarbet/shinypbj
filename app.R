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

# Allow upload of bigger files
# from http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
# The first number is the number of MB
options(shiny.maxRequestSize=30*1024^2)

# Javascript to enable/disable tabs
# https://stackoverflow.com/questions/31703241/activate-tabpanel-from-another-tabpanel/31719425#31719425
jscode <- "
shinyjs.disableTab = function(name)
{
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e)
  {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name)
{
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}

shinyjs.disableDiv = function(name)
{
  var div = $('#'+name);
  div.addClass('disabled')
}
shinyjs.enableDiv = function(name)
{
  var div = $('#'+name);
  div.removeClass('disabled')
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

div#fileuploadbox
{
  border:  1px solid #333;
  padding: 5px;
}
div.disabled
{
  background-color: #ccc !important;
  color: #aaa !important;
  font-style: oblique;
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
          div(id="fileuploadbox",
            splitLayout(
              fileInput("studydata", "Study Data"),
              fileInput("varimages", "Var Images")
            ),
            splitLayout(
              fileInput("template",  "Template Nifti Location"),
              fileInput("mask",      "Mask Nifti Location")
            )
          ),

          selectInput("dataRow", h3("Data Row"), choices=as.list(1:21), selected=1),
          papayaOutput("visualize")
        )
      ),
      tabPanel(title="Model",
        fluidPage(
          tableOutput("variables")
        )
      ),
      tabPanel(title="Inference"),
      tabPanel(title="Visualize"),
      tabPanel(title="Back Matter")
    )
)

hideVisualizer <- function()
{
  shinyjs::hide("visualize")
  shinyjs::hide("dataRow")
  js$disableTab("Model")
}
showVisualizer <- function()
{
  shinyjs::show("visualize")
  shinyjs::show("dataRow")
  js$enableTab("Model")
}
disableUpload <- function()
{
  shinyjs::disable("studydata")
  shinyjs::disable("varimages")
  shinyjs::disable("template")
  shinyjs::disable("mask")
  js$disableDiv("fileuploadbox")
}
enableUpload <- function()
{
  shinyjs::enable("studydata")
  shinyjs::enable("varimages")
  shinyjs::enable("template")
  shinyjs::enable("mask")
  js$enableDiv("fileuploadbox")
}
dataUploaded <- function(input)
{
  !is.null(input$studydata) &&
  !is.null(input$varimages) &&
  !is.null(input$template )
}


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  studydata <- reactive({
    if(input$source == "Upload") 
    {
      input$studydata
    } else {
      pain21()$data 
    }
  })
  
  varsToModel <- reactive({
    req(studydata())
    
    names(studydata())
  })
    
    js$disableTab("Inference")
    js$disableTab("Visualize")
    disableUpload()

    output$visualize <- renderPapaya({
        fnames <- pain_images(as.numeric(input$dataRow))
        img    <- papaya(fnames)
        img$elementId <- NULL
        img
    })
    
    output$variables <- renderText({
      varsToModel()
    })

    observeEvent(input$source, {
      if(input$source == "Upload")
      {
        enableUpload()
        if(!dataUploaded(input)) hideVisualizer()
      } else {
        disableUpload()
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
