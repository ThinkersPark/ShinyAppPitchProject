
## Server file

library(shiny)
library(quantmod)

shinyServer(function(input, output) {
  
  ## Non-reactive part: Stock parameters & charting
  
  getSymbols("WMT"); getSymbols("AAPL"); 
  WMTreturns <- annualReturn(Cl(WMT))
  APPreturns <- annualReturn(Cl(AAPL))
  
  mAPP <- mean(APPreturns)
  mWMT <- mean(WMTreturns)
  sdAPP <- sd(APPreturns)
  sdWMT <- sd(WMTreturns)
  corWMTAPP <- cor(APPreturns,WMTreturns)
  
  w_app <- seq(0,1,0.01)
  w_wmt <- 1-w_app
  eqret <- w_wmt*mWMT + w_app*mAPP
  eqvol <- sqrt((w_wmt*sdWMT)^2 + (w_app*sdAPP)^2 + 2*sdWMT*sdAPP*corWMTAPP[[1]]*w_wmt*w_app)
  
  output$eqplot <- renderPlot({
    myts <- merge(WMTreturns, APPreturns, join = 'inner')
    myts$mWMT <- mWMT
    myts$mAPP <- mAPP
    mar=c(4, 4, 6, 2) + 0.1
    plot(myts[,1:2],col=c("blue","red"), 
         main = "Annual equity returns: WalMart (WMT, blue line) and Apple (AAP, red line)
    WMT average annual return 8.3% (dashed line), volatility 15.3%
    AAP average annual return 35.7% (dashed line), volatility 52.5%")
    lines(myts$mWMT,lty=2, lwd=2, col="blue")
    lines(myts$mAPP,lty=2, lwd=2, col="red")
  })
  
  ## Reactive part: Portfolio weights and efficient frontier/ CML plot
  
  rfrate <- reactive({
    input$num0
  })
  
  ratios <- reactive({
    as.vector((eqret-rfrate())/eqvol) 
  })
  
  sharperatio <- reactive({
    round(max(ratios()),2)
  })
  
  rportfolio <- reactive({ 
    rfrate() + sharperatio()*eqvol
  })
  
  portfolios <- reactive({
    
    ## Tangency portfolio
    
    tp_WMT <- w_wmt[which.max(ratios())]
    tp_APP <- w_app[which.max(ratios())]
    tpret <- eqret[which.max(ratios())]
    tpvol <- eqvol[which.max(ratios())]
    
    ## Minimum variance portfolio
    
    minvar <- eqvol[which.min(eqvol)]
    minvarret <- eqret[which.min(eqvol)]
    minvar_WMT <- w_wmt[which.min(eqvol)]
    minvar_APP <- w_app[which.min(eqvol)]
    
    ## Target return portfolio
    
    tarret <- input$num1
    
    tarretcapm <- tarret
    tarretvolcapm <- (tarretcapm-rfrate())/sharperatio()
    tarretcapm_rf <- 1-tarretvolcapm/eqvol[which.max(ratios())]
    tarretcapm_WMT <- tp_WMT*(1-tarretcapm_rf) 
    tarretcapm_APP <- tp_APP*(1-tarretcapm_rf)
    
    if (tarret <= eqret[which.max(eqret)]){
      tarretvol <- eqvol[which.min(abs(eqret-tarret))]
      tarret_rf <- 0.0
      tarret_WMT <- w_wmt[which.max(ratios()[which.min(abs(eqret-tarret)):length(ratios())])]
      tarret_APP <- w_app[which.max(ratios()[which.min(abs(eqret-tarret)):length(ratios())])]
    }
    else {
      tarretvol <- tarretvolcapm
      tarret_rf <- tarretcapm_rf
      tarret_WMT <- tarretcapm_WMT 
      tarret_APP <- tarretcapm_APP
    }
    
    ## Target volatility portfolio 
    
    tarvol <- input$num2
    
    tarvolcapm <- tarvol
    tarvolretcapm <- rfrate() + sharperatio()*tarvolcapm
    tarvolcapm_rf <- 1-tarvolcapm/eqvol[which.max(ratios())]
    tarvolcapm_WMT <- tp_WMT*(1-tarvolcapm_rf)
    tarvolcapm_APP <- tp_APP*(1-tarvolcapm_rf)
    
    if (tarvol >= minvar){
      tarvolret <- eqret[which.min(abs(eqvol-tarvol))]
      tarvol_rf <- 0.0
      tarvol_WMT <- w_wmt[which.max(ratios()[1:which.min(abs(eqvol-tarvol))])]
      tarvol_APP <- w_app[which.max(ratios()[1:which.min(abs(eqvol-tarvol))])]
    }
    
    else {
      tarvolret <- tarvolretcapm 
      tarvol_rf <- tarvolcapm_rf
      tarvol_WMT <- tarvolcapm_WMT
      tarvol_APP <- tarvolcapm_APP
    }
    
    portfoliotable <- as.data.frame(rbind(c(minvar_WMT,minvar_APP,0,minvarret,minvar),
                                          c(tp_WMT,tp_APP,0,tpret,tpvol),
                                          c(tarret_WMT,tarret_APP,tarret_rf,tarret,tarretvol),
                                          c(tarvol_WMT,tarvol_APP,tarvol_rf,tarvolret,tarvol),
                                          c(tarretcapm_WMT,tarretcapm_APP,tarretcapm_rf,tarretcapm,tarretvolcapm),
                                          c(tarvolcapm_WMT,tarvolcapm_APP,tarvolcapm_rf,tarvolretcapm,tarvolcapm)))
    colnames(portfoliotable) <- c("WMT weight", "AAP weight", "Risk free asset weight", "Expected return", "Volatility")
    rownames(portfoliotable) <- c("Minimum variance portfolio", "Tangency portfolio", 
                                  "Target return portfolio (frontier)", "Target volatility portfolio (frontier)",
                                  "Target return portfolio (CML)", "Target volatility portfolio (CML)")
    
    ## Managing labels for plotting
    
    portfoliotable$labels <- rownames(portfoliotable)
    portfoliotable$colours <- rep(1,6)
    if (tarret > eqret[which.max(eqret)]) {
      portfoliotable$labels[3] <- "Target return portfolio defaults to CML"
      portfoliotable$colours[3] <- NA
    }
    if (tarvol < minvar) {
      portfoliotable$labels[4] <- "Target volatility portfolio defaults to CML"
      portfoliotable$colours[4] <-NA
    }
    
    portfoliotable
    
  })
  
  ## The following outputs are input based
  
  output$mplot <- renderPlot({
    plot(eqvol[which.min(eqvol):length(eqvol)], eqret[which.min(eqvol):length(eqret)], pch=20, col="grey",xlim=c(0.0,max(max(portfolios()[,5])+0.05)),
         ylim=c(0,max(0.35,max(portfolios()[,4])+0.05)),
         main="Markowitz model: Efficient frontier (dotted grey parabola),
          Capital market line/ CML (solid black line), Risk free rate (dashed green line)", 
         ylab="expected return", xlab="volatility")
    lines(c(0,eqvol,portfolios()[5,5]), c(rfrate(),rportfolio(),portfolios()[5,4]), lwd=2)
    abline(h=rfrate(),col="darkgreen", lwd=2, lty=2)
    points(x=portfolios()[1:6,5],y=portfolios()[1:6,4],pch=21, 
           bg=c("magenta","green","blue","red","lightblue","salmon"))
    text(x=c(portfolios()[1,5]+0.03, portfolios()[2,5]+0.02),
         y=c(portfolios()[1,4]-0.007,portfolios()[2,4]-0.007),
         labels=portfolios()[1:2,6])
    
    if(!is.na(portfolios()[3,7])){
      lines(x=c(portfolios()[3,5],portfolios()[5,5]),y=c(portfolios()[3,4],portfolios()[5,4]),lwd=2,col="darkgreen")
      text(x=portfolios()[5,5]-0.03,y=portfolios()[5,4]+0.02,labels="Deposit and reduce volatility")
    }
    
    if(!is.na(portfolios()[4,7])){
      lines(x=c(portfolios()[4,5],portfolios()[6,5]),y=c(portfolios()[4,4],portfolios()[6,4]),lwd=2,col="darkgreen")
      text(x=portfolios()[6,5]-0.03,y=portfolios()[6,4]+0.02,labels="Borrow and increase return")
    }
    
    legend(x = "topleft", inset=0.05, bty="n", legend=portfolios()[3:6,6], 
           pch=16*portfolios()[3:6,7], col=c("blue","red","lightblue","salmon"),
           title="Your portfolios:",title.adj=0)
    
  },height=600)
  
  output$ratioout <- renderText({paste("Sharpe ratio: ",sharperatio())})
  
  output$mtable1 <- renderTable({
    portfolios()[1,1:5]
  },align="c")
  
  output$mtable2 <- renderTable({
    portfolios()[2,1:5]
  },align="c")
  
  output$mtable3 <- renderTable({
    portfolios()[3,1:5]
  },align="c")
  
  output$mtable4 <- renderTable({
    portfolios()[4,1:5]
  },align="c")
  
  output$mtable5 <- renderTable({
    portfolios()[5,1:5]
  },align="c")
  
  output$mtable6 <- renderTable({
    portfolios()[6,1:5]
  },align="c")
  
})
