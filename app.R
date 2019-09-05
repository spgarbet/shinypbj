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

# File types to accept for upload
accept <- c("text/csv", "text/comma-separated-values","text/plain", ".csv")

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
div#formulaFullFeedback
{
  vertical-align: middle;
  display: table-cell;
  color: #f00 !important;
  font-style: oblique;
  height: 60px;
}
div#formulaReducedFeedback
{
  vertical-align: middle;
  display: table-cell;
  color: #f00 !important;
  font-style: oblique;
  height: 60px;
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
    titlePanel(title=div(img(src="pbj-transparent.png", style="width: 50px"), "Shiny PBJ")),
    
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
              fileInput("studydata", "Study Data", accept=accept),
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
          hr(),
          tableOutput("variables"),
          hr(),
          splitLayout(
            textInput("formulaFull",    "Formula Full", "~1"),
            textOutput("formulaFullFeedback")
          ),
          splitLayout(
            textInput("formulaReduced",    "Formula Reduced", "~1"),
            textOutput("formulaReducedFeedback")
          ),
          uiOutput("histograms")
        )
      ),
      tabPanel(title="Inference"),
      tabPanel(title="Visualize"),
      tabPanel(title="Back Matter")
    )
)

hideInference <- function()
{
  js$disableTab("Inference") 
}

showInference <- function()
{
  js$enableTab("Inference")
}

hideVisualizer <- function()
{
  shinyjs::hide("visualize")
  shinyjs::hide("dataRow")
  js$disableTab("Model")
}

showVisualizer <- function(output, data)
{
  shinyjs::show("visualize")
  shinyjs::show("dataRow")
  js$enableTab("Model")
  
  output$histograms <- renderUI({
    vars <- Filter(function(i) length(intersect(class(data[[i]]), c("integer", "numeric"))) > 0, names(data))
    lapply(vars, function(i) {
       renderPlot(hist(data[[i]], main=i, xlab=""))
    })
  })
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

validFormula <- function(formula, data)
{
   available <- names(data)
   specified <- all.vars(formula)
   all(sapply(specified, function(x) x %in% available))
}

formulaErrors <- function(formula, data)
{
  tryCatch(
    {
      f <- as.formula(formula)
      available <- names(data)
      specified <- all.vars(f)
      exists    <- sapply(specified, function(x) x %in% available)
      if(all(exists)) NULL else
        paste("Not in data.frame:", paste(specified[!exists], collapse=", "))
    },
    error=function(cond) {cond$message}
  )  
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  studydata <- reactive({
    if(input$source == "Upload") 
    {
      read.csv(input$studydata$datapath, header=TRUE)
    } else {
      pain21()$data 
    }
  })
  
  formulaFull <- reactive({
    e <- formulaErrors(input$formulaFull, studydata())
    need(!is.null(e), e) 
    as.formula(input$formulaFull)
  })
  
  formulaReduced <- reactive({
    e <- formulaErrors(input$formulaReduced, studydata())
    need(!is.null(e), e) 
    as.formula(input$formulaReduced)
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
   paste("Variables Provided:", paste(varsToModel(), collapse=", "))
  })

  observeEvent(input$source, {
    if(input$source == "Upload")
    {
      enableUpload()
      if(!dataUploaded(input)) hideVisualizer()
    } else {
      disableUpload()
      showVisualizer(output, studydata())
    }
  })
    
  observeEvent(input$studydata, {
    if(dataUploaded(input)) showVisualizer(output, studydata())
  })
  observeEvent(input$varimages, {
    if(dataUploaded(input)) showVisualizer(output, studydata())
  })
  observeEvent(input$template,  {
    if(dataUploaded(input)) showVisualizer(output, studydata())
  })
  observeEvent(input$formulaFull, {
    if(is.null(formulaErrors(input$formulaFull, studydata())) &&
       is.null(formulaErrors(input$formulaReduced, studydata())))
      showInference() else
      hideInference()
  })
  observeEvent(input$formulaReduced, {
    if(is.null(formulaErrors(input$formulaFull, studydata())) &&
       is.null(formulaErrors(input$formulaReduced, studydata())))
      showInference() else
      hideInference()
  })

  output$formulaFullFeedback <- renderText({
    formulaErrors(input$formulaFull, studydata())
  })
  output$formulaReducedFeedback <- renderText({
    formulaErrors(input$formulaReduced, studydata())
  })
  
  
  # observeEvent(input$formula_reduced, {
  #   tryCatch(
  #     {
  #       if(validFormula(studydata(), as.formula(input$formula_full)) &&
  #          validFormula(studydata(), as.formula(input$formula_reduced)))
  #         showInference() else
  #         hideInference()
  #     },
  #     error=function(cond) {hideInference()}
  #   )  
  # })

}

# Run the application 
shinyApp(ui = ui, server = server)
