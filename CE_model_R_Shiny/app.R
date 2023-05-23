# Libraries

library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(plotly)

# Define UI for application that draws a histogram
ui <-   fluidPage(
  title = "CE model R Shiny",
  # theme = 
  navbarPage("Cost Effectiveness Model to evaluate Test drug versus gold standard",
             tabPanel("Inputs",
                      h2("General parameters"),
                      fluidRow(
                        column(
                          width = 2,
                          numericInput(inputId = "timeHorizon",
                                       label = "Time horizon",
                                       value = 25,
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          numericInput(inputId = "cohortSize",
                                       label = "Cohort Size",
                                       value = 100,
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          textInput(inputId = "statesName1",
                                       label = "State name 1",
                                       value = "Healthy",
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          textInput(inputId = "statesName2",
                                    label = "State name 2",
                                    value = "Ill",
                                    width = "100%")
                        ),
                        column(
                          width = 2,
                          textInput(inputId = "statesName3",
                                    label = "State name 3",
                                    value = "Very Ill",
                                    width = "100%")
                        ),
                        column(
                          width = 2,
                          textInput(inputId = "statesName4",
                                    label = "State name 4",
                                    value = "Dead",
                                    width = "100%")
                        )
                      ),
                      tags$hr(),
                      h2("Clinical parameters: Transition probabilities (TP) and relative risk (RR)"),
                      fluidRow(
                        column(
                          width = 2,
                          numericInput(inputId = "tpA2A",
                                       label = "TP: A to A",
                                       value = 0.685,
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          numericInput(inputId = "tpA2B",
                                       label = "TP: A to B",
                                       value = 0.152,
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          numericInput(inputId = "tpA2C",
                                       label = "TP: A to C",
                                       value = 0.123,
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          numericInput(inputId = "tpA2D",
                                       label = "TP: A to D",
                                       value = 0.04,
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          numericInput(inputId = "tpB2B",
                                       label = "TP: B to B",
                                       value = 0.596,
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          numericInput(inputId = "tpB2C",
                                       label = "TP: B to C",
                                       value = 0.359,
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          numericInput(inputId = "tpB2D",
                                       label = "TP: B to D",
                                       value = 0.045,
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          numericInput(inputId = "tpC2C",
                                       label = "TP: C to C",
                                       value = 0.762,
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          numericInput(inputId = "tpC2D",
                                       label = "TP: C to D",
                                       value = 0.238,
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          numericInput(inputId = "tpD2D",
                                       label = "TP: D to D",
                                       value = 1,
                                       width = "100%")
                        ),
                        column(
                          width = 2,
                          numericInput(inputId = "RR",
                                       label = "Relative risk",
                                       value = 0.728,
                                       width = "100%")
                        )
                      ),
                      fluidRow(
                        column(
                          width = 6,
                          h4("Transition probability for Gold Standard"),
                          dataTableOutput("transMatGold")
                        ),
                        column(
                          width = 6,
                          h4("Transition probability for Test Drug"),
                          dataTableOutput("transMatTest")
                        )
                      ),
                      tags$hr(),
                      h2("Cost parameters"),
                      fluidRow(
                        column(
                          width = 12,
                          h4("Drug costs"),
                          fluidRow(
                            column(
                              width = 3,
                              numericInput(inputId = "drugCostTest",
                                           label = "Test drug",
                                           value = 4500,
                                           width = "100%")
                                          ),
                            column(
                              width = 3,
                              numericInput(inputId = "drugCostGold",
                                           label = "Gold standard drug",
                                           value = 2000,
                                           width = "100%")
                            ),
                            column(
                              width = 3,
                              numericInput(inputId = "adminCostTest",
                                           label = "Test drug admin",
                                           value = 850,
                                           width = "100%")
                            ),
                            column(
                              width = 3,
                              numericInput(inputId = "adminCostGold",
                                           label = "Gold standard drug admin",
                                           value = 450,
                                           width = "100%")
                            )
                            )
                          ),
                        column(
                          width = 12,
                          h4("Health state management costs"),
                          fluidRow(
                            column(
                              width = 4,
                              numericInput(inputId = "hsmCostHealthy",
                                           label = "Healthy state",
                                           value = 5000,
                                           width = "100%")
                            ),
                            column(
                              width = 4,
                              numericInput(inputId = "hsmCostIll",
                                           label = "Ill state",
                                           value = 7000,
                                           width = "100%")
                            ),
                            column(
                              width = 4,
                              numericInput(inputId = "hsmCostVeryIll",
                                           label = "Very Ill state",
                                           value = 12000,
                                           width = "100%")
                            )
                          )
                        ),
                        column(
                          width = 12,
                          h4("health states specific costs"),
                          fluidRow(
                            column(
                              width = 4,
                              numericInput(inputId = "otherCostHealthy",
                                           label = "Healthy state",
                                           value = 3000,
                                           width = "100%")
                            ),
                            column(
                              width = 4,
                              numericInput(inputId = "otherCostIll",
                                           label = "Ill state",
                                           value = 6000,
                                           width = "100%")
                            ),
                            column(
                              width = 4,
                              numericInput(inputId = "otherCostVeryIll",
                                           label = "Very Ill state",
                                           value = 9000,
                                           width = "100%")
                            )
                          )
                        ),
                        column(
                          width = 12,
                          h4("Death cost"),
                          fluidRow(
                            column(
                              width = 12,
                              numericInput(inputId = "deathCost",
                                           label = "Death related cost",
                                           value = 10000,
                                           width = "100%")
                            )
                          )
                        )
                      ),
                      tags$hr(),
                      h2("Utility parameters"),
                      fluidRow(
                        column(
                          width = 4,
                          numericInput(inputId = "hsuvHealthy",
                                       label = "Healthy state",
                                       value = 0.9,
                                       width = "100%")
                        ),
                        column(
                          width = 4,
                          numericInput(inputId = "hsuvIll",
                                       label = "Ill state",
                                       value = 0.65,
                                       width = "100%")
                        ),
                        column(
                          width = 4,
                          numericInput(inputId = "hsuvVeryIll",
                                       label = "Very Ill state",
                                       value = 0.2,
                                       width = "100%")
                        )
                      ),
                      tags$br(),
                      tags$br()
                      ),
             tabPanel("Results",
                      fluidRow(
                        column(
                          offset = 3,
                          width = 6,
                          h4("Summary"),
                          dataTableOutput("resdf",width = "100%")
                        )
                      ),
                      tags$hr(),
                      fluidRow(
                        h4("Markov Trace"),
                        column(width = 6,
                               plotlyOutput("markovGoldP",width = "100%"),
                               tags$br(),
                               dataTableOutput("markovGold",width = "100%")
                               ),
                        column(width = 6,
                               plotlyOutput("markovTestP",width = "100%"),
                               tags$br(),
                               dataTableOutput("markovTest",width = "100%"))
                      )
                      
                      )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$transMatGold = renderDataTable({
      statesName = c(input$statesName1,input$statesName2, input$statesName3, input$statesName4)
      transMatGold = matrix(c(input$tpA2A,input$tpA2B,input$tpA2C,input$tpA2D,
                              0  ,input$tpB2B,input$tpB2C,input$tpB2D,
                              0  ,  0  ,input$tpC2C,input$tpC2D,
                              0  ,  0  ,  0  ,input$tpD2D
      ),
      nrow = length(statesName),
      byrow = T,
      dimnames = list(from = statesName, to = statesName)
      )
      transMatGold = as.data.frame(transMatGold) %>% 
        mutate(CHECK = rowSums(.))
      datatable(
        transMatGold,
        options = list(
          fixedHeader = TRUE,
          searching = FALSE,
          paging = F,
          scrollX = F,
          dom = 't',
          ordering = FALSE
        )
      )
    })
    output$transMatTest = renderDataTable({
      statesName = c(input$statesName1,input$statesName2, input$statesName3, input$statesName4)
      transMatTest = matrix(c(1-(input$tpA2B+input$tpA2C+input$tpA2D)*input$RR, input$tpA2B*input$RR           ,   input$tpA2C*input$RR    , input$tpA2D*input$RR,
                              0               , 1-(input$tpB2C+input$tpB2D)*input$RR ,   input$tpB2C*input$RR    , input$tpB2D*input$RR,
                              0               ,         0          , 1-(input$tpC2D*input$RR)  , input$tpC2D*input$RR,
                              0               ,         0          ,     0         ,   input$tpD2D
      ),
      nrow = length(statesName),
      byrow = T,
      dimnames = list(from = statesName, to = statesName)
      )
      transMatTest = as.data.frame(transMatTest) %>% 
        mutate(CHECK = rowSums(.))
      datatable(
        transMatTest,
        options = list(
          fixedHeader = TRUE,
          searching = FALSE,
          paging = F,
          scrollX = F,
          dom = 't',
          ordering = FALSE
        )
      )
    })
    
    
    
observe({
  statesName = c(input$statesName1,input$statesName2, input$statesName3, input$statesName4)
  nStates = 4
  transMatGold = matrix(c(input$tpA2A,input$tpA2B,input$tpA2C,input$tpA2D,
                          0  ,input$tpB2B,input$tpB2C,input$tpB2D,
                          0  ,  0  ,input$tpC2C,input$tpC2D,
                          0  ,  0  ,  0  ,input$tpD2D
  ),
  nrow = length(statesName),
  byrow = T,
  dimnames = list(from = statesName, to = statesName)
  )
  transMatTest = matrix(c(1-(input$tpA2B+input$tpA2C+input$tpA2D)*input$RR, input$tpA2B*input$RR           ,   input$tpA2C*input$RR    , input$tpA2D*input$RR,
                          0               , 1-(input$tpB2C+input$tpB2D)*input$RR ,   input$tpB2C*input$RR    , input$tpB2D*input$RR,
                          0               ,         0          , 1-(input$tpC2D*input$RR)  , input$tpC2D*input$RR,
                          0               ,         0          ,     0         ,   input$tpD2D
  ),
  nrow = length(statesName),
  byrow = T,
  dimnames = list(from = statesName, to = statesName)
  )
  
  # State membership Calculation --------------------------------------------
  
  stateMembershipGold = array(NA_real_, dim = c(input$timeHorizon+1,nStates), dimnames = list(cycle = 0:(input$timeHorizon), state = statesName))
  stateMembershipGold[1, ] <- c(input$cohortSize,0,0,0)
  for (i in 2:(input$timeHorizon+1)) {
    stateMembershipGold[i, ]<- stateMembershipGold[i-1, ]%*%transMatGold
  }
  
  stateMembershipTest = array(NA_real_, dim = c(input$timeHorizon+1,nStates), dimnames = list(cycle = 0:(input$timeHorizon), state = statesName))
  stateMembershipTest[1, ] <- c(input$cohortSize,0,0,0)
  for (i in 2:(input$timeHorizon+1)) {
    stateMembershipTest[i, ]<- stateMembershipTest[i-1, ]%*%transMatTest
  }
 
  
  
  # QALYs Calculations ------------------------------------------------------
  
  # Creation of utility values matrix
  QALYMat = matrix(c(input$hsuvHealthy, input$hsuvIll, input$hsuvVeryIll, 0), 
                   nrow = 4, 
                   ncol = 1, 
                   byrow = FALSE, 
                   dimnames = list(state = statesName, payoff = c("QALYS")))
  
  stateMembershipQALYGold = stateMembershipGold %*% QALYMat # Creation of utility values matrix for Gold standard drug
  stateMembershipQALYTest = stateMembershipTest %*% QALYMat # Creation of utility values matrix for Test drug
  
  
  # Cost calculations -------------------------------------------------------
  
  # Creation of Gold standard drug cost matrix 
  costMatGold = matrix(c(input$drugCostGold + input$adminCostGold + input$hsmCostHealthy + input$otherCostHealthy, # Healthy state costs
                         input$drugCostGold + input$adminCostGold + input$hsmCostIll + input$otherCostIll,         # Ill state costs
                         input$drugCostGold + input$adminCostGold + input$hsmCostVeryIll + input$otherCostVeryIll, # Very Ill state costs
                         input$deathCost), # Death state costs
                       nrow = 4, 
                       ncol = 1, 
                       byrow = FALSE, 
                       dimnames = list(state = statesName, payoff = c("Costs")))
  
  # Creation of Test drug cost matrix 
  costMatTest = matrix(c(input$drugCostTest + input$adminCostTest + input$hsmCostHealthy + input$otherCostHealthy, # Healthy state costs
                         input$drugCostTest + input$adminCostTest + input$hsmCostIll + input$otherCostIll,         # Ill state costs
                         input$drugCostTest + input$adminCostTest + input$hsmCostVeryIll + input$otherCostVeryIll, # Very Ill state costs
                         input$deathCost), # Death state costs
                       nrow = 4, 
                       ncol = 1, 
                       byrow = FALSE, 
                       dimnames = list(state = statesName, payoff = c("Costs")))
  
  stateMembershipCostGold = stateMembershipGold %*% costMatGold # Creation of cost matrix for Gold standard drug
  stateMembershipCostTest = stateMembershipTest %*% costMatTest # Creation of cost matrix for Test drug
  
  
  # ICER calculation --------------------------------------------------------
  
  totalCostTest = colSums(stateMembershipCostTest)/input$cohortSize # Total Test drug costs
  totalCostGold = colSums(stateMembershipCostGold)/input$cohortSize # Total gold standard drug costs
  
  totalQALYTest = colSums(stateMembershipQALYTest)/input$cohortSize # Total Test drug QALYs
  totalQALYGold = colSums(stateMembershipQALYGold)/input$cohortSize # Total gold standard drug QALYs
  
  incrementalCost = totalCostTest - totalCostGold # Incremental cost
  incrementalQALY = totalQALYTest - totalQALYGold # Incremental QALYs
  
  ICER = incrementalCost / incrementalQALY # Incremental cost-effectiveness ratio

  
  output$resdf = renderDataTable({
    x = data.frame(
      "Total Costs" = round(c(totalCostTest,totalCostGold,incrementalCost),0),
      "Total QALYs (in Years)" = round(c(totalQALYTest,totalQALYGold,incrementalQALY),2),
      "Incremental Cost effectiveness ratio (ICER)" = c(NA, NA, round(ICER,0)),
      check.names = F
    )
    rownames(x) = c("Test Drug"," Gold standard","Incremental")
    datatable(
      x,extensions = c('Buttons'),
      options = list(
        fixedHeader = TRUE,
        searching = FALSE,
        paging = F,
        scrollX = F,
        dom = 'Bt',
        ordering = FALSE,
        buttons = list(list(
          extend = 'collection',
          buttons = c('csv', 'pdf'),
          text = 'Download'
        ))
      )
    ) %>% 
      formatCurrency(c(1,3),digits = 0)
  })
  
  output$markovGold = renderDataTable({
    x = cbind(
      "Cycle" = 0:(input$timeHorizon),
      as.data.frame(stateMembershipGold) %>% mutate(CHECK = rowSums(.)),
      as.data.frame(stateMembershipQALYGold),
      as.data.frame(stateMembershipCostGold)
    )
    datatable(
      x,rownames = F,extensions = c('Buttons'),
      options = list(
        fixedHeader = TRUE,
        searching = FALSE,
        paging = F,
        scrollX = F,
        dom = 'Bt',
        ordering = FALSE,
        buttons = list(list(
          extend = 'collection',
          buttons = c('csv', 'pdf'),
          text = 'Download'
        ))
      )
    ) %>% 
      formatRound(c(2:5),digits = 4)%>% 
      formatRound(7,digits = 2) %>% 
      formatCurrency(8,digits = 0)
  })
  
  output$markovTest = renderDataTable({
    x = cbind(
      "Cycle" = 0:(input$timeHorizon),
      as.data.frame(stateMembershipTest) %>% mutate(CHECK = rowSums(.)),
      as.data.frame(stateMembershipQALYTest),
      as.data.frame(stateMembershipCostTest)
    )
    datatable(
      x,rownames = F,extensions = c('Buttons'),
      options = list(
        fixedHeader = TRUE,
        searching = FALSE,
        paging = F,
        scrollX = F,
        dom = 'Bt',
        ordering = FALSE,
        buttons = list(list(
          extend = 'collection',
          buttons = c('csv', 'pdf'),
          text = 'Download'
        ))
      )
    ) %>% 
      formatRound(c(2:5),digits = 4)%>% 
      formatRound(7,digits = 2) %>% 
      formatCurrency(8,digits = 0)
  })
   

output$markovGoldP = renderPlotly({
  x = cbind(
    "Cycle" = 0:(input$timeHorizon),
    as.data.frame(stateMembershipGold) %>% round(4)
  )
  fig <- plot_ly(x, x = x$Cycle, y = x$Healthy, name = 'Healthy', type = 'scatter', mode = 'lines') 
  fig <- fig %>% add_trace(y = x$Ill, name = 'Ill', mode = 'lines') 
  fig <- fig %>% add_trace(y = x$`Very Ill`, name = 'Very Ill', mode = 'lines')
  fig <- fig %>% add_trace(y = x$Dead, name = 'Dead', mode = 'lines')
  fig <- fig %>% layout(title = "Gold Standard",
                        xaxis = list(title = "Years"),
                        yaxis = list (title = "Number of Patients"),
                        hovermode = 'x') %>% 
                  config(displayModeBar = FALSE)
  
  fig
})

output$markovTestP = renderPlotly({
  x = cbind(
    "Cycle" = 0:(input$timeHorizon),
    as.data.frame(stateMembershipTest) %>% round(4)
  )
  fig <- plot_ly(x, x = x$Cycle, y = x$Healthy, name = 'Healthy', type = 'scatter', mode = 'lines') 
  fig <- fig %>% add_trace(y = x$Ill, name = 'Ill', mode = 'lines') 
  fig <- fig %>% add_trace(y = x$`Very Ill`, name = 'Very Ill', mode = 'lines')
  fig <- fig %>% add_trace(y = x$Dead, name = 'Dead', mode = 'lines')
  fig <- fig %>% layout(title = "Test Drug",
                        xaxis = list(title = "Years"),
                        yaxis = list (title = "Number of Patients"),
                        hovermode = 'x') %>% 
    config(displayModeBar = FALSE)
  fig
})
}) 
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
