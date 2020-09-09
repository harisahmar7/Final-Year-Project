for (package in c('shiny', 'DT', 'shinythemes', 'preprocessCore', 'htmlwidgets')) 
{
	if (!require(package, character.only=T, quietly=F)) 
	{
    		install.packages(package)
    		library(package, character.only=T)
	}
}

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")


if (!require("limma", character.only=T, quietly=F)) {
  BiocManager::install("limma")
}

if (!require("affy", character.only=T, quietly=F)) {
  BiocManager::install("affy")
}


if (!require("NormalyzerDE", character.only=T, quietly=F)) {
  BiocManager::install("NormalyzerDE")
}




library("limma")

library("affy")

library("NormalyzerDE")










#library("shiny")
#library("DT")
#library("preprocessCore")

#shinyServer(function(input, output,session) {
options(shiny.maxRequestSize=30*1024^2)
server <- function(input, output, session) {
  #output$contents <- DT::renderDataTable({
  
  output$contents1 <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    inFile1 <- input$file1
    
    if (is.null(inFile1))
      return(NULL)
    
    #dat1<- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    else{
      
      dat1<- read.table(inFile1$datapath, header=input$header, sep=input$sep, quote=input$quote)
      DT::datatable(dat1, class = 'cell-border stripe', options = list(pageLength = 10, scrollY=FALSE, scrollX=TRUE, paging=TRUE))
    }
    
    
    
    #read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
    #DT::datatable(dat1, options = list(pageLength = 25, paging=FALSE))
    
  })

  
  output$contents2 <- DT::renderDataTable({

    # input$file1 will be NULL initially. After the user selects and uploads a
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
    # columns. The 'datapath' column will contain the local filenames where the
    # data can be found.

    inFile2 <- input$file2

    if (is.null(inFile2))
      return(NULL)

    #dat1<- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    else{

      dat2<- read.table(inFile2$datapath, header=input$header, sep=input$sep, quote=input$quote, row.names = 1)
      DT::datatable(dat2, class = 'cell-border stripe', options = list(pageLength = 10, scrollY=FALSE, scrollX=TRUE, paging=TRUE))
    }



    #read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)

    #DT::datatable(dat1, options = list(pageLength = 25, paging=FALSE))

  })
  #)
  
  
  observeEvent(c(input$preprocess,input$normalize), {
    
    
    inFile1 <- input$file1
    
    if (is.null(inFile1))
      return(NULL)
    
    #dat1<- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    else{
    
    dat1<- read.table(inFile1$datapath, header=input$header,  sep=input$sep, quote=input$quote, row.names = 1)
    
    }
    
    mat1 <- as.matrix(dat1)
    
    res1 <- mat1
    
    if(input$normalize == "Quantile"){
    
        res1<- normalize.quantiles(mat1)
    }
    
    
    
    if(input$normalize == "Loess"){
      
      res1<- normalize.loess(mat1)
    }
    
    if(input$normalize == "CyclicLoess"){
      
      res1<- performCyclicLoessNormalization(mat1)
    }
    
    
    
    #normalizeQuantiles
    output$contents3 <- DT::renderDataTable({
      DT::datatable(res1, class = 'cell-border stripe', options = list(pageLength = 10, scrollY=FALSE, scrollX=TRUE, paging=TRUE)) 
    })
    
    #Volcano plot
    output$myhist = renderPlot({
      
      library(EnhancedVolcano)
      library(airway)
      library(magrittr)
      
      data('airway')
      airway$dex %<>% relevel('untrt')
      library('DESeq2')
      
      dds <- DESeqDataSet(airway, design = ~ cell + dex)
      dds <- DESeq(dds, betaPrior=FALSE)
      res1 <- results(dds,
                      contrast = c('dex','trt','untrt'))
      res1 <- lfcShrink(dds,
                        contrast = c('dex','trt','untrt'), res=res1, type = 'normal')
      res2 <- results(dds,
                      contrast = c('cell', 'N061011', 'N61311'))
      res2 <- lfcShrink(dds,
                        contrast = c('cell', 'N061011', 'N61311'), res=res2, type = 'normal')
      EnhancedVolcano(res1,
                      lab = rownames(res1),
                      x = 'log2FoldChange',
                      y = 'pvalue',
                      xlim = c(-5, 8))
      
    })
    
    
  })
  
}
#)