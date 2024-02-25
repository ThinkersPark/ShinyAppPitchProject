
## UI file

library(shiny)
library(quantmod)

shinyUI(fluidPage(
  
  titlePanel(
    p(
      h1("Markowitz Portfolio Optimisation Demo", align = "center"),
      h2("With two stocks: Walmart (WMT, lower return/ lower volatility), and Apple (AAP, higher 
        return/ higher volatility).", align = "center")
    )),
  
  ## Input and stock chart section row
  
  fluidRow(
    
    column(1), 
    column(4, 
           h5(HTML("<b>Please select parameter values.</b>")),
           h5("All parameters expressed on annual basis/ per annum (p.a)"),
           h5("Assuming annual investment horizon."),
           numericInput("num0", "Reference/ zero rate (p.a.)", 0.02),
           numericInput("num1", "Target return (p.a.)", 0.12),
           h5("If target return is above the frontier maximum, then"),
           h5("the target return portfolio will default to a CML portfolio."),
           numericInput("num2", "Target volatility (p.a.)", 0.35),
           h5("If target volatility is below the frontier minimum, then"),
           h5("the target volatility portfolio will default to a CML portfolio."),
           submitButton("Submit")),
    
    column(6,plotOutput("eqplot")),
    column(1)
  ),
  
  ## Section split row
  
  fluidRow(
    
    column(1,br()),
    column(4,br()),
    column(6,br()),
    column(1,br())),
  
  ## Output row
  
  fluidRow(
    
    column(1), 
    column(4, 
           h5(textOutput("ratioout")),
           h5(HTML("<b>Portfolio characteristics:</b>")),
           h5("Minimum variance portfolio:"),
           tableOutput("mtable1"),
           h5("Tangency portfolio:"),
           tableOutput("mtable2"),
           h5("Target return portfolio (frontier):"),
           tableOutput("mtable3"),
           h5("Target volatility portfolio (frontier):"),
           tableOutput("mtable4"),
           h5("Target return portfolio (CML):"),
           tableOutput("mtable5"),
           h5("Target volatility portfolio (CML):"),
           tableOutput("mtable6")),
    
    column(6,plotOutput("mplot")),
    column(1),
  )
))
