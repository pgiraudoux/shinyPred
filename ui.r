fluidPage(
  
  titlePanel('Campagnols, prédation et piégeage'),
  
  sidebarPanel(
    sliderInput('densren','Densité de prédateurs (ind/km2)', value = 1,min = 0, max = 20, step=0.25),
    sliderInput('eatAT','Nombre  maximum de campagnols mangés par jour, par prédateur', value = 134, min = 0, max = 300, step=1),
    sliderInput('N','Densité initiale de campagnols (ind./ha)', value = 2,   min = 2, max = 1000,step=1),
    sliderInput('K','K: densité maximale potentielle de campagnols (ind./ha)', value = 1000,   min = 10, max = 1000,step=10),
    numericInput('area','Surface surveillée et piégée (ha)',min=1,value=1),
    fileInput('file1', 'Tableau de piégeage (nombre de campagnols piégés sur la surface)', accept=c('text/tab telimited with header', '.txt')),
  ),

  
  mainPanel(
    tableOutput(outputId = 'table.output'),
    plotOutput('plot1')
    
  )
)

