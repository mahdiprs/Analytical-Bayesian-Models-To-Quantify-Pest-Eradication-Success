#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(devtools)
library(ggplot2)
library(dplyr)
library(sqldf)
library(reshape)
library(data.table)
library(DT)
library(viridis)
library(hrbrthemes)

# load the data
load("../data/allCases1506.rdata")

# adjust cell
widthCell =150
widthbar =270
steps = 6
colsize =3
maxgrowth =3

## cuarate the data
case41 = subset(case41, time<=6 & time > 0)
case41$rho = round(case41$rho, digits = 1)
case41$delta = round(case41$delta, digits = 2)
case41$lambda = round(case41$lambda, digits = 1)

case42 = subset(case42, time<=6 & time > 0)
case42$rho = round(case42$rho, digits = 1)
case42$delta = round(case42$delta, digits = 2)
case42$lambda = round(case42$lambda, digits = 1)

case43 = subset(case43, time<=6 & time > 0)
case43$rho = round(case43$rho, digits = 1)
case43$delta = round(case43$delta, digits = 2)
case43$lambda = round(case43$lambda, digits = 1)

case44 = subset(case44, time<=6 & time > 0)
case44$rho = round(case44$rho, digits = 1)
case44$delta = round(case44$delta, digits = 2)
case44$lambda = round(case44$lambda, digits = 1)

textInputRow<-function (inputId, label, value = "")
{
    div(style="display:inline-block",
        tags$label(label, `for` = inputId),
        tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

# server code
ui = navbarPage("Eradication Inference Tool",
                tabPanel("Inferred population mean", fluidRow( div(style="display:inline-block"), responsive=TRUE,br(),
        column(colsize,h5("First Model"),
               HTML("<ul><li>Without the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
               plotOutput("plotM43", height=400, width = 400), br(),br(),
               sliderInput("timeM43", "Number of negative surveys",
                           min = 1, max = steps, value = 0,width = widthbar)),
        column(colsize, h5("Second Model"),
               HTML("<ul><li>With the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
               plotOutput("plotM44", height=400, width = 400), br(),sliderInput("timeM44", "Number of negative surveys",
                                                                      min = 1, max = steps, value = 0,width = widthbar, step =1),br(),
               sliderInput("growthM44", "Population growth rate",
                           min = 1, max = maxgrowth, step = .5, value = 0,width = widthbar)))
),
tabPanel("Inferred population standard deviation", fluidRow( div(style="display:inline-block"), responsive=TRUE,br(),
                                          column(colsize,h5("First Model"),
                                                 HTML("<ul><li>Without the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                 plotOutput("plotv43", height=400), br(),br(),
                                                 sliderInput("timev43", "Number of negative surveys",
                                                             min = 1, max = steps, value = 0,width = widthbar)),
                                          column(colsize, h5("Second Model"),
                                                 HTML("<ul><li>With the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                 plotOutput("plotv44", height=400), br(),sliderInput("timev44", "Number of negative surveys",
                                                                                                     min = 1, max = steps, value = 0,width = widthbar, step =1),br(),
                                                 sliderInput("growthv44", "Population growth rate",
                                                             min = 1, max = maxgrowth, step = .5, value = 0,width = widthbar)))
),
tabPanel("Inferred probability of absence", fluidRow( div(style="display:inline-block"), responsive=TRUE, br(),
                                                       column(colsize,h5("First Model"),
                                                       HTML("<ul><li>Without the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                       plotOutput("plotEX43", height=400), br(),br(),
                                                       sliderInput("timeEX43", "Number of negative surveys",
                                                                   min = 1, max = steps, value = 0,width = widthbar)),
                                                column(colsize, h5("Second Model"),
                                                       HTML("<ul><li>With the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                       plotOutput("plotEX44", height=400), br(),sliderInput("timeEX44", "Number of negative surveys",
                                                                                                           min = 1, max = steps, value = 0,width = widthbar, step =1),br(),
                                                       sliderInput("growthEX44", "Population growth rate",
                                                                   min = 1, max = maxgrowth, step = .5, value = 0,width = widthbar)))),
tabPanel("Inferred probability of none detected", fluidRow( div(style="display:inline-block"), responsive=TRUE,br(),
                                                  column(colsize,h5("First Model"),
                                                         HTML("<ul><li>Without the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                         plotOutput("plotNODCT43", height=400), br(),br(),
                                                         sliderInput("timeNODTC43", "Number of negative surveys",
                                                                     min = 1, max = steps, value = 0,width = widthbar)),
                                                  column(colsize, h5("Second Model"),
                                                         HTML("<ul><li>With the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                         plotOutput("plotNODTC44", height=400), br(),sliderInput("timeNODT44", "Number of negative surveys",
                                                                                                              min = 1, max = steps, value = 0,width = widthbar),br(),
                                                         sliderInput("growthNODTC44", "Population growth rate",
                                                                     min = 1, max = maxgrowth, step = .5, value = 0,width = widthbar)))),
tabPanel("Inferred probability of first detection", fluidRow( div(style="display:inline-block"), responsive=TRUE,br(),
                                                      column(colsize,h5("First Model"),
                                                            HTML("<ul><li>Without the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                            plotOutput("plotFDTC43", height=400), br(),br(),
                                                            sliderInput("timeFDTC43", "Number of negative surveys",
                                                                        min = 1, max = steps, value = 0,width = widthbar)),
                                                     column(colsize, h5("Second Model"),
                                                            HTML("<ul><li>With the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                            plotOutput("plotFDTC44", height=400), br(),sliderInput("timeFDTC44", "Number of negative surveys",
                                                                                                                       min = 1, max = steps, value = 0,width = widthbar),br(),
                                                            sliderInput("growthFDTC44", "Population growth rate",
                                                                        min = 1, max = maxgrowth, step = .5, value = 0,width = widthbar)))),
tabPanel("Inferred probability of none detected given population is present", fluidRow( div(style="display:inline-block"), responsive=TRUE, br(),
                                                                                    column(colsize,h5("First Model"),
                                                                                           HTML("<ul><li>Without the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                                                           plotOutput("plotNODCTPN043", height=400), br(),br(),
                                                                                           sliderInput("timeNODTCPN043", "Number of negative surveys",
                                                                                                       min = 1, max = steps, value = 0,width = widthbar)),
                                                                                    column(colsize, h5("Second Model"),
                                                                                           HTML("<ul><li>With the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                                                           plotOutput("plotNODTCPN044", height=400), br(),sliderInput("timeNODTPN044", "Number of negative surveys",
                                                                                                                                                      min = 1, max = steps, value = 0,width = widthbar),br(),
                                                                                           sliderInput("growthNODTCPN044", "Population growth rate",
                                                                                                       min = 1, max = maxgrowth, step = .5, value = 0,width = widthbar))))
)

server <- function(input, output,session) {

    output$plotM41 <- renderPlot({
        df = case41
        ggplot(df, aes(rho,rho)) +
            geom_line(colour = "red")+
            labs(x ="Mean initial population size", y ="Mean population size")+ ggtitle("Mean population size") +
            theme(plot.title = element_text(hjust = 0.5))+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotM42 <- renderPlot({
        df = subset(case42, time ==input$timeM42 )
        ggplot(df, aes(rho,lambda, z =mean)) +
            geom_raster(aes(fill = mean), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case42$mean))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Population  growth rate", fill= "Mean Population size")+ ggtitle("Mean population size") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotM43 <- renderPlot({
        df = subset(case43, time ==input$timeM43 )
        ggplot(df, aes(rho,delta,z =mean)) +
            geom_raster(aes(fill = mean), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Mean Population size")+ ggtitle("Mean population size") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotM44 <- renderPlot({
        df = subset(case44, time ==input$timeM44 &lambda == input$growthM44 )
        ggplot(df, aes(rho,delta,z =mean)) +
            geom_raster(aes(fill = mean), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case44$mean))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Mean Population size")+ ggtitle("Mean population size") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank()) +
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })
    ## standard deviation
    output$plotv41 <- renderPlot({
        df = case41
        ggplot(df, aes(rho,sqrt(var))) +
            geom_line(colour = "red")+
            labs(x ="Mean initial population size", y ="Population standard deviation")+ ggtitle("Population standard deviation") +
            theme(plot.title = element_text(hjust = 0.5))+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotv42 <- renderPlot({
        df = subset(case42, time ==input$timev42 )
        ggplot(df, aes(rho,lambda, z= sqrt(var))) +
            geom_raster(aes(fill = sqrt(var)), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(sqrt(case42$var)))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Population  growth rate", fill= "Population standard deviation")+ ggtitle("Population standard deviation") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotv43 <- renderPlot({
        df = subset(case43, time ==input$timev43 )
        ggplot(df, aes(rho,delta, z= sqrt(var))) +
            geom_raster(aes(fill = sqrt(var)), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"), limit = range(sqrt(case43$var)))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Population standard deviation")+ ggtitle("Population standard deviation") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotv44 <- renderPlot({
        df = subset(case44, time ==input$timev44 &lambda == input$growthv44 )
        ggplot(df, aes(rho,delta, z= sqrt(var))) +
            geom_raster(aes(fill = sqrt(var)), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"), limit = range(sqrt(case44$var)))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Population standard deviation")+ ggtitle("Population standard deviation") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })
## PROBABILITY of absence
##
    output$plotEX41 <- renderPlot({
        df = case41
        ggplot(df, aes(rho,extinct)) +
            geom_line(colour = "red")+
            labs(x ="Mean initial population size", y ="Probability of absence")+ ggtitle("Probability of absence") +
            theme(plot.title = element_text(hjust = 0.5))+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotEX42 <- renderPlot({
        df = subset(case42, time ==input$timeEX42 & !is.na(extinct) )
        ggplot(df, aes(rho,lambda, z =extinct)) +
            geom_raster(aes(fill = extinct), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case42$extinct))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Population  growth rate", fill= "Probability of absence")+ ggtitle("Probability of absence") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank()) +geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotEX43 <- renderPlot({
        df = subset(case43, time ==input$timeEX43 & !is.na(extinct))
        ggplot(df, aes(rho,delta, z =extinct)) +
            geom_raster(aes(fill = extinct), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case43$extinct))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Probability of absence")+ ggtitle("Probability of absence") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank()) +geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotEX44 <- renderPlot({
        df = subset(case44, time ==input$timeEX44 &lambda == input$growthEX44 & !is.na(extinct) )
        ggplot(df, aes(rho,delta, z =extinct)) +
            geom_raster(aes(fill = extinct), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case44$extinct)
            )+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Probability of absence")+ ggtitle("Probability of absence") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank()) + geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })
## PROBABILITY none detected
##
    output$plotNODCT41 <- renderPlot({
        df = subset(case41, time ==input$timeNODT41 & !is.na(notDCT))
        ggplot(df, aes(rho,delta, z = notDCT)) +
            geom_raster(aes(fill = notDCT), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case41$notDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "The probability of none detected")+ ggtitle("The probability of none detected") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotNODCT42 <- renderPlot({
        df = subset(case42, time ==input$timeNODTC42 & lambda == input$growthNODTC42 & !is.na(notDCT) )
        ggplot(df, aes(rho,delta,z = notDCT)) +
            geom_raster(aes(fill = notDCT), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case42$notDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "The probability of none detected")+ ggtitle("The probability of none detected") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotNODCT43 <- renderPlot({
        df = subset(case43, time ==input$timeNODTC43 & !is.na(notDCT))
        ggplot(df, aes(rho,delta,z = notDCT)) +
            geom_raster(aes(fill = notDCT), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case43$notDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "The probability of none detected")+ ggtitle("The probability of none detected") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotNODTC44 <- renderPlot({
        df = subset(case44, time ==input$timeNODT44 &lambda == input$growthNODTC44 & !is.na(notDCT) )
        ggplot(df, aes(rho,delta, z = notDCT)) +
            geom_raster(aes(fill = notDCT), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case41$notDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "The probability of none detected")+ ggtitle("The probability of none detected") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    ## PROBABILITY FIRST DTECTION
    ##
    output$plotFDTC41 <- renderPlot({
        df = subset(case41, time ==input$timeFDTC41 &!is.na(firstDCT) )
        ggplot(df, aes(rho,delta, z= firstDCT)) +
            geom_raster(aes(fill = firstDCT), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case41$firstDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Probability of first detection")+ ggtitle("Probability of first detection") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotFDTC42 <- renderPlot({
        df = subset(case42, time ==input$timeFDTC42 & lambda == input$growthFDTC42 &!is.na(firstDCT) )
        ggplot(df, aes(rho,delta,z= firstDCT)) +
            geom_raster(aes(fill = firstDCT), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case42$firstDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Probability of first detection")+ ggtitle("Probability of first detection") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })


    output$plotFDTC43 <- renderPlot({
        df = subset(case43, time ==input$timeFDTC43 &!is.na(firstDCT) )
        ggplot(df, aes(rho,delta,z= firstDCT)) +
            geom_raster(aes(fill = firstDCT), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case43$firstDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Probability of first detection")+ ggtitle("Probability of first detection") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotFDTC44 <- renderPlot({
        df = subset(case44, time ==input$timeFDTC44 &lambda == input$growthFDTC44 &!is.na(firstDCT) )
        ggplot(df, aes(rho,delta,z= firstDCT)) +
            geom_raster(aes(fill = firstDCT), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),
                                 limit = range(case44$firstDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Probability of first detection")+ ggtitle("Probability of first detection") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

## PROBABILITY of none detected given population is not absence
    ##
    output$plotNODCTPN041 <- renderPlot({
        df = subset(case41, !is.na(notDTCPop))
        ggplot(df, aes(rho,delta,z=notDTCPop)) +
            geom_raster(aes(fill = notDTCPop), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case41$notDTCPop))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Probability of none detected given population is present")+
            ggtitle("Probability of none detected given population is present") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotNODCTPN042 <- renderPlot({
        df = subset(case42, time ==input$timeNODTCPN042 & lambda == input$growthNODTCPN042 &!is.na(notDTCPop) )
        ggplot(df, aes(rho,delta,z=notDTCPop)) +
            geom_raster(aes(fill = notDTCPop), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case42$notDTCPop))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Probability of none detected given population is present")+
            ggtitle("Probability of none detected given population is present") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotNODCTPN043 <- renderPlot({
        df = subset(case43, time ==input$timeNODTCPN043 &!is.na(notDTCPop) )
        ggplot(df, aes(rho,delta,z=notDTCPop)) +
            geom_raster(aes(fill = notDTCPop), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case43$notDTCPop))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Probability of none detected given population is present")+
            ggtitle("Probability of none detected given population is present") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })

    output$plotNODTCPN044 <- renderPlot({
        df = subset(case44, time ==input$timeNODTPN044 &lambda == input$growthNODTCPN044 &!is.na(notDTCPop) )
        ggplot(df, aes(rho,delta,z=notDTCPop)) +
            geom_raster(aes(fill = notDTCPop), interpolate= T)+
            scale_fill_gradientn(colours=c("white","black"),limit = range(case41$notDTCPop))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Mean initial population size", y ="Survey sensitivity", fill= "Probability of none detected given population is present")+
            ggtitle("Probability of none detected given population is present") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())+geom_contour(aes(colour = ..level..)) +
            scale_colour_gradient(low = "yellow", high = "red")
    })


}

shinyApp(ui, server)

