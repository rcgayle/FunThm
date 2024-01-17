library(shiny)
source('calcfun2.r')
source('calcfun2qs.r')
ui<-fluidPage(
tags$style(type = "text/css",
             "h1 {color: darkblue; font-weight: bold; font-size: 22px;}",
             "h4 {color: darkblue; font-weight: bold; font-size: 16px;}",
             "h2 {color: darkblue; font-weight: bold; font-size: 14px;}",
             "body {background-color: darkturquoise;}",
             "label {font-size: 16px; color: darkblue;
                   font-weight: bold}"),
tags$head(tags$style(HTML(
            "#text3 {font-size: 20px;color: red; font-weight: bold;}"))),
    mainPanel(tags$h1('Fun with the Second Fundamental Theorem'),
    tabsetPanel(
      tabPanel(tags$h4('Fun Theorem I'),
        column(5, 
          hr(),
          radioButtons('F', "F(x) is:",
                          choices=list(
                             'non-negative'=1, 'negative'=-1)
                        ),
            hr(),
            radioButtons('dF', "F'(x) is:",
                          choices=list(
                             'non-negative'=1, 'negative'=-1)
                        ),
            hr(),
            radioButtons('d2F', "F''(x) is:",
                          choices=list(
                             'non-negative'=1, 'negative'=-1)
                        ),
            hr(),
            fluidRow(
            actionButton(inputId='getG1', 
              label='Graph/New Graph',
              style="color: darkblue; font-size: 14px; font-weight: bold; 
                   background-color: white;"),
            actionButton(inputId='saveVals1', 
              label='Save Values',
              style="color: darkblue; font-size: 14px; font-weight: bold; 
                   background-color: white;")
                   ),
            hr(),
            fluidRow(
            actionButton(inputId='chkSgns1', label='Check Signs',
              style="color: darkblue; font-size: 14px; font-weight: bold; 
                   background-color: white;"
                        ),
            actionButton(inputId='newSel1', label='Retry',
              style="color: darkblue; font-size: 14px; font-weight: bold; 
                   background-color: white;"
                        )
                     )
              ),
       column(7,
          plotOutput('plot1', click='plot_click1',height='450px', width='100%')
               )
               
            ), 
             

       tabPanel(tags$h4('Fun Theorem II'),
       fluidRow(
        column(1, ''),
        column(11,
          plotOutput('plot2', click='plot_click2',height='450px', width='100%'))
               ),
         hr(),
         fluidRow(
           column(5,
             actionButton(inputId='getQ', 
              label='Create Graph/Question',
              style="color: darkblue; font-size: 14px; font-weight: bold; 
                   background-color: white;")
                  ),
           column(3,
              actionButton(inputId='chkPt2', label='Check Point',
              style="color: darkblue; font-size: 14px; font-weight: bold; 
                   background-color: white;")
                 ),
           column(4,
              actionButton(inputId='newPt2', label='Retry',
              style="color: darkblue; font-size: 14px; font-weight: bold; 
                   background-color: white;")
                   )   
                 )
               ),
               
        tabPanel(tags$h4('Fun Theorem III'),
        column(1, ''),
        column(11,
          fluidRow(
            plotOutput('plot3', click='plot_click3',
                    height='450px', width='100%')
                  ),
          hr(), 
          fluidRow(
            actionButton(inputId='getG2', 
              label='Create Graph',
              style="color: darkblue; font-size: 14px; font-weight: bold; 
                   background-color: white;") 
                  )
               )
                 
                 )
                 
                 )))
              
                     
                   
server<-function(input, output){

v1<-reactiveValues(VAL=FALSE, a=c(), x=c(), G=NULL, count=0,
                    x1=1, x2=1, x3=1, V=c(1,1,1), GNP1=TRUE, CK=FALSE, 
                       T=0, Temp=0)
                    
observeEvent(input$getG1,{v1$G<-selFun()
                          v1$a<-selA(v1$G)
                          v1$x<-quesPt1(v1$G, v1$a)
                          v1$T<-sample(c(0,1), 1)
                          if (v1$T==1){
                               v1$Temp<-v1$a
                               v1$a<-v1$x
                               v1$x<-v1$Temp}
                          else {}
                          v1$GNP1<-FALSE
                          v1$CK<-FALSE
                          v1$count<-0
                                  }
            )
            
            

observeEvent(input$saveVals1,if (v1$CK==FALSE)
                            {v1$x1<-as.numeric(input$F)
                            v1$x2<-as.numeric(input$dF)
                            v1$x3<-as.numeric(input$d2F)
                            v1$V<-c(v1$x1,v1$x2,v1$x3)
                            v1$VAL<-checkVal1(v1$G, v1$a, v1$x, v1$V)
                              }
                            else {}
            )
            
observeEvent(input$chkSgns1,{v1$CK<-TRUE}
            )

observeEvent(input$newSel1, if(v1$VAL==FALSE )
                              {v1$CK<-FALSE
                               v1$count<-v1$count+1
                              }
                            else {}
            )
                    

output$plot1<-renderPlot(
                    if (v1$GNP1==TRUE) 
                           {GetPlotE2()}
                    else if (v1$GNP1==FALSE & v1$CK==FALSE & v1$count<3)
                           {quesPlot1(v1$G,v1$a, v1$x, v1$count)}
                    else if (v1$GNP1==FALSE & v1$CK==TRUE & v1$count<3)
                           {corPlot1(v1$G,v1$a, v1$x, v1$V, v1$count)}
                    else if (v1$count==3 )
                           {revPlot1(v1$G,v1$a, v1$x)}
                        )


v2<-reactiveValues(G=NULL, Q=NULL, a=c(), count=0, P=NULL, SP=NULL,
                      GNP2=TRUE, CK=FALSE, VAL=FALSE)

observeEvent(input$getQ,{v2$G<-selFun()
                         v2$a<-selA(v2$G)
                         v2$Q<-quesFun(v2$G, v2$a)
                         while (is.null(corPoint(v2$G, v2$a, v2$Q)))
                              {v2$a<-selA(v2$G)}
                         if(abs(v2$a)<1.5)
                               {v2$SP<-list(v2$a+sample(c(1,-1),1)*0.5,0)}
                         else if (v2$a>=1.5)
                               {v2$SP<-list(v2$a-0.5,0)}
                         else {v2$SP<-list(v2$a+0.5,0)}
                         v2$count<-0
                         v2$GNP2<-FALSE
                         v2$CK<-FALSE
                         v2$VAL<-FALSE
                                  }
              )                    
                                  
observeEvent(input$plot_click2,
               if (abs(input$plot_click2[[2]])<0.75 & v2$VAL==FALSE
                            & v2$count<3)
                    {v2$P<-input$plot_click2}
               else {}
                ) 
                
observeEvent(input$chkPt2,
             {if (!is.null(v2$P))
               {v2$CK<-TRUE
               v2$VAL<-checkVal2(v2$G, v2$a, (v2$P)[[1]],(v2$Q)[[1]])
                }
              else{v2$CK<-FALSE}
              }
            )
                                   
                                  
observeEvent(input$newPt2, {if (v2$VAL==FALSE & v2$CK==TRUE & !is.null(v2$P))
                               {v2$count<-v2$count+1
                                v2$CK<-FALSE
                                v2$P<-NULL}
                            else {}
                            }                          
            )
                                            

output$plot2<-renderPlot(if (v2$GNP2==TRUE)
                            {GetPlotE1()}
                         else if (v2$GNP2==FALSE & v2$CK==FALSE
                           & is.null(v2$P) & v2$count<3)
                            {quesPlot2(v2$G,v2$a,v2$SP,v2$Q,v2$count)}
                         else if (v2$GNP2==FALSE & v2$CK==FALSE
                           & !is.null(v2$P) & v2$count<3)
                            {quesPlot2(v2$G,v2$a,v2$P,v2$Q,v2$count)}
                         else if (v2$GNP2==FALSE & v2$CK==TRUE & v2$count<3
                            & !is.null(v2$P))
                            {corPlot2(v2$G,v2$a,v2$P,v2$Q,v2$count)}
                         else if (v2$count==3)
                            {revPlot2(v2$G, v2$a, v2$Q)}
                         )

v3<-reactiveValues(P=NULL,a=c(), GNP3=TRUE, G=NULL) 

observeEvent(input$getG2, {v3$G<-selFun()
                           v3$a<-selA(v3$G)
                           v3$P<-list(quesPt1(v3$G, v3$a),0)
                           v3$GNP3<-FALSE}
            )

observeEvent(input$plot_click3,
               if (abs(input$plot_click3[[2]])<0.75)
                    {v3$P<-input$plot_click3}
               else {}
                )

output$plot3<-renderPlot(if (v3$GNP3==TRUE)
                                  {GetPlotE2()}
                         else {quesPlot3(v3$G, v3$a, v3$P)}
                        )


}



                                      
shinyApp(ui=ui, server=server)