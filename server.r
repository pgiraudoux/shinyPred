

function(input, output, session) {
  
  ## veille à ce que N ne soit pas supérieur à K
  observe({
    if(input$N>=input$K) updateSliderInput(session,"N",value=input$K)
      })
  
  ## lit le fichier text
  reads<-function(tab){
    inFile <- tab
    if (is.null(inFile))
      return(NULL)
    tbl <- read.csv(inFile$datapath, header=TRUE, sep="\t",  dec =".")
    return(tbl)
  }
  
  ## calcul des cinétiques
    kinetics<-function(densren,eatAT,N,K,area,tpieg){
    if(!is.null(tpieg)){
     # tpieg<-as.data.frame(tpieg)
     tpieg[,2]<-tpieg[,2]/area # nombre d'animaux piégés ramenés à 1 ha
    }
    dren1<-densren/100 # densite de renards par ha
    dayintake<-eatAT/2*dren1 # prelevement journalier de 3 femelles/renard (si 6 campagnols manges)
    Nt<-rep(NA,12*30)
    Ntr1<-rep(NA,12*30)
    Ntr1p<-rep(NA,12*30)
    durerep<-8*30
    durationh<-12*30-durerep
    r0<-0.0165 # 1 femelle donne ~50 jeunes femelles en 8 mois
    Ntr1p[1]<-Ntr1[1]<-Nt[1]<-N/2  # nombre de femelles a t0
    hol2<-function(H, omega=1,D=10) {omega*H/(D+H)} # Holling type II
    K<-K/2 # femelles seulement
    
    # mars - octobre
    for(i in 2:durerep) {
      Nt[i]<-Nt[i-1]+r0*Nt[i-1]*((K-Nt[i-1])/K) # sans les renards avec croissance logistique
      Ntr1[i]<-Ntr1[i-1]+r0*Ntr1[i-1]*((K-Ntr1[i-1])/K)-dayintake*hol2(Ntr1[i-1]) # avec les renards et Holling type II
      Ntr1p[i]<-Ntr1p[i-1]+r0*Ntr1p[i-1]*((K-Ntr1p[i-1])/K)-dayintake*hol2(Ntr1p[i-1]) 
      if(i%in%tpieg[,1]) {
        Ntr1p[i]<-Ntr1p[i]-tpieg[which(tpieg[,1]==i),2]/2 #  femelles piégées
        if (Ntr1p[i]<0) Ntr1p[i]<-0
        }
    }
    
    
    # novembre - fevrier
    deb<-i+1
    for(i in deb:(deb+durationh)) {
      Nt[i]<-Nt[i-1]
      Ntr1[i]<-Ntr1[i-1]-dayintake*hol2(Ntr1[i-1]) # avec les renards
      Ntr1p[i]<-Ntr1p[i-1]-dayintake*hol2(Ntr1p[i-1])
      if(i%in%tpieg[,1]) {
        Ntr1p[i]<-Ntr1p[i]-tpieg[which(tpieg[,1]==i),2]/2 #  femelles piégées
        if (Ntr1p[i]<0) Ntr1p[i]<-0
      }
    }
    data.frame(Nt=Nt,Ntr1=Ntr1,Ntr1p=Ntr1p)
    }
    

  tabpieg<-reactive(reads(input$file1)) # lire le fichier text
  
  dens2<-reactive(kinetics(input$densren,input$eatAT,input$N,input$K,input$area,{tabpieg()})) # calcule les cinétiques

  output$table.output<-renderTable({tabpieg()}) # envoie la table
 
  output$plot1 <- renderPlot({ # envoie le graphe
    par(mar=c(5.1,4.1,4.1,3))
    plot(1:length(dens2()$Nt),dens2()$Nt*2,type="l",las=1,xlab="Jours",ylab="N campagnols/ha")
    lines(1:length(dens2()$Nt),dens2()$Ntr1p*2,col="green")
    lines(1:length(dens2()$Nt),dens2()$Ntr1*2,col="red")
    axis(4,at=dens2()$Nt[length(dens2()$Nt)]*2,labels=round(dens2()$Nt[length(dens2()$Nt)]*2,0),col.axis="black",col.ticks="black",las=1)
    axis(4,at=dens2()$Ntr1p[length(dens2()$Ntr1p)]*2,labels=round(dens2()$Ntr1p[length(dens2()$Ntr1p)]*2,0),col.axis="green",col.ticks="green",las=1)
    axis(4,at=dens2()$Ntr1[length(dens2()$Ntr1)]*2,labels=round(dens2()$Ntr1[length(dens2()$Ntr1)]*2,0),col.axis="red",col.ticks="red",las=1)
    mtext("Tous les modèles sont faux, certains sont utiles",side=1,line=4,adj=0,cex=0.8)
    # legend(list(x=0,y=max(dens2()$Nt)),legend=c(expression("Sans predateur"),bquote(.(input$densren)~ind./km^2)),lty=1,col=c("black","red"),bty="n") # solution Bert Gunter
    legend(list(x=0,y=max(dens2()$Nt)*2),
           legend=as.expression(list(
             "Sans prédateur",
             bquote(.(input$densren) * " ind."/"km"^2),
             "avec piégeage"
           )),
           lty=1,col=c("black","red","green"),bty="n") # solution Peter Dalgaard

  })

}



