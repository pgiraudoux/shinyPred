fluidPage(
  
  titlePanel('Voles, predation and trapping'),
  
  sidebarPanel(
    sliderInput('densren','Predator density (ind/km2)', value = 1,min = 0, max = 20, step=0.25),
    sliderInput('eatAT','Maximum number of voles eaten by day by predator', value = 6, min = 0, max = 200, step=1),
    sliderInput('N','Vole density at start (ind./ha)', value = 2,   min = 2, max = 1000,step=1),
    sliderInput('K','K: Carrying capacity (max number of voles, ind./ha)', value = 1000,   min = 10, max = 1000,step=10),
    numericInput('area','Area surveyed and trapped (ha)',min=1,value=1),
    fileInput('file1', 'Trapping time schedule (number of voles trapped in the area)', accept=c('text/tab telimited with header', '.txt')),
  ),

  
  mainPanel(
    tableOutput(outputId = 'table.output'),
    plotOutput('plot1')
    
  )
)

