
for (package in c('shiny', 'DT', 'shinythemes', 'htmlwidgets')) 
{
	if (!require(package, character.only=T, quietly=F)) 
	{
    		install.packages(package)
    		library(package, character.only=T)
	}
}
#library("shiny")
#library("DT")
#library("shinythemes")

#shinyUI(pageWithSidebar(

ui <-fluidPage(theme = shinytheme("cerulean"), #superhero
  headerPanel("Dataset Preprocessing"),
  sidebarPanel(fluid = FALSE,
  tags$head(tags$script('var dimension = [0, 0];
                          $(document).on("shiny:connected", function(e) {
                          dimension[0] = window.innerWidth;
                          dimension[1] = window.innerHeight;
                          Shiny.onInputChange("dimension", dimension);
                          });
                          $(window).resize(function(e) {
                          dimension[0] = window.innerWidth;
                          dimension[1] = window.innerHeight;
                          Shiny.onInputChange("dimension", dimension);
                          });
                    ')),               
    fileInput('file1', 'Choose Dataset1',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','.txt')),
    #tags$hr(),
    
    fileInput(inputId = 'file2', 'Choose Dataset2',
             accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','.txt')),
    #tags$hr(),
    
    splitLayout(
      radioButtons(inputId = 'sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   'Comma'),
      radioButtons(inputId = 'quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   'Double Quote')
    ),
    #splitLayout(cellWidths = c("75%", "25%"),
    selectInput(inputId = "normalize",
                label = "Normalization",
                choices = c("Quantile", "Loess", "CyclicLoess","qspline", "RMA", "MASS5", "GCRMA")),
    splitLayout(cellWidths = c("50%", "50%"),
              checkboxInput('header', 'Header', TRUE),
              checkboxInput('header', 'Log2', TRUE)
            ),
  sliderInput("bins","Select the Number of Histogram",min=5,max=25,value=15),
  br(),
  radioButtons("color","Select the color of histogram",choices=c("Green","Red","Blue"),selected="Green"),
  actionButton("preprocess", "Preprocess"),
  actionButton("next", "Next")
  
    
  ),
  mainPanel(
      tabsetPanel(
      tabPanel("Dataset1",  DT::dataTableOutput('contents1')),
      tabPanel("Dataset2",  DT::dataTableOutput('contents2')),
      tabPanel("Output",    DT::dataTableOutput('contents3')),
      tabPanel("Volcano Plot", plotOutput('myhist'))
  )
)
)
#)