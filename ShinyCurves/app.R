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
library(slickR)

# load the data
load("../data/allCases1506.rdata")

# adjust cell
widthCell =150
widthbar =270
steps = 6
colsize =3
maxgrowth =3

## cuarate the data
case41 = subset(case41, time<=6 & lambda >=1)
case41$rho = round(case41$rho, digits = 1)
case41$delta = round(case41$delta, digits = 2)
case41$lambda = round(case41$lambda, digits = 1)
case41$time = case41$time+1


case42 = subset(case42, time<=6 & lambda >=1)
case42$rho = round(case42$rho, digits = 1)
case42$delta = round(case42$delta, digits = 2)
case42$lambda = round(case42$lambda, digits = 1)
case42$time = case42$time+1

case43 = subset(case43, time<=6 & lambda >=1)
case43$rho = round(case43$rho, digits = 1)
case43$delta = round(case43$delta, digits = 2)
case43$lambda = round(case43$lambda, digits = 1)
case43$time = case43$time+1

case44 = subset(case44, time<=6 & lambda >=1)
case44$rho = round(case44$rho, digits = 1)
case44$delta = round(case44$delta, digits = 2)
case44$lambda = round(case44$lambda, digits = 1)
case44$time = case44$time+1

textInputRow<-function (inputId, label, value = "")
{
    div(style="display:inline-block",
        tags$label(label, `for` = inputId),
        tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

# server code
ui = navbarPage("Eradication Inference Tool",
                tabPanel("Models and Inference", slickROutput("slickr", width="1000px")),
                tabPanel("Inferred population mean and standard deviation",
                         fluidRow( div(style="display:inline-block"), responsive=TRUE,br(),
                                   column(colsize,h5("First Model"),
                                          HTML("<ul><li>Without the inclusion of population growth</li>
                            <li>With learning from negative survey results</li></ul>"),
                                          plotOutput("plotMN43v2", height=400), br(),
                                          sliderInput("rhoMN43v2", "Mean initial population size",
                                                      min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                          sliderInput("deltaMN43v2", "Survey sensitivity",
                                                      min = 0, max = 1, step = .1, value = 0,width = widthbar),br()),
                                   column(colsize, h5("Second Model"),
                                          HTML("<ul><li>With the inclusion of population growth</li>
                            <li>With learning from negative survey results</li></ul>"),
                                          plotOutput("plotMN44v2", height=400), br(),sliderInput("rhoMN44v2", "Mean initial population size",
                                                                                                  min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                          sliderInput("deltaMN44v2", "Survey sensitivity",
                                                      min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                          sliderInput("lambdaMN44v2", "Population growth rate",
                                                      min = 1, max = 3, step = .5, value = 0,width = widthbar)))),
                tabPanel("Inferred probability of absence",
                         fluidRow( div(style="display:inline-block"), responsive=TRUE,br(),
                                   column(colsize,h5("First Model"),
                                          HTML("<ul><li>Without the inclusion of population growth</li>
                            <li>With learning from negative survey results</li></ul>"),
                                          plotOutput("plotABS43v2", height=400), br(),
                                          sliderInput("rhoABS43v2", "Mean initial population size",
                                                      min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                          sliderInput("deltaABS43v2", "Survey sensitivity",
                                                      min = 0, max = 1, step = .1, value = 0,width = widthbar),br()),
                                   column(colsize, h5("Second Model"),
                                          HTML("<ul><li>With the inclusion of population growth</li>
                            <li>With learning from negative survey results</li></ul>"),
                                          plotOutput("plotABS44v2", height=400), br(),sliderInput("rhoABS44v2", "Mean initial population size",
                                                                                                     min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                          sliderInput("deltaABS44v2", "Survey sensitivity",
                                                      min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                          sliderInput("lambdaABS44v2", "Population growth rate",
                                                      min = 1, max = 3, step = .5, value = 0,width = widthbar)))),
    tabPanel("Inferred probability of none detected",
        fluidRow( div(style="display:inline-block"), responsive=TRUE,br(),
                           column(colsize,h5("First Model"),
                        HTML("<ul><li>Without the inclusion of population growth</li>
                            <li>With learning from negative survey results</li></ul>"),
                plotOutput("plotNONDCT43v2", height=400), br(),
                         sliderInput("rhoNONDCT43v2", "Mean initial population size",
                            min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                         sliderInput("deltaNONDCT43v2", "Survey sensitivity",
                            min = 0, max = 1, step = .1, value = 0,width = widthbar),br()),
                column(colsize, h5("Second Model"),
                    HTML("<ul><li>With the inclusion of population growth</li>
                            <li>With learning from negative survey results</li></ul>"),
                    plotOutput("plotNONDCT44v2", height=400), br(),sliderInput("rhoNONDCT44v2", "Mean initial population size",
                                 min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                            sliderInput("deltaNONDCT44v2", "Survey sensitivity",
                                 min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                sliderInput("lambdaNONDCT44v2", "Population growth rate",
                                min = 1, max = 3, step = .5, value = 0,width = widthbar)))),
#
#
tabPanel("Inferred probability of first detection", fluidRow( div(style="display:inline-block"), responsive=TRUE,br(),
                                                                       column(colsize,h5("First Model"),
                                                                              HTML("<ul><li>Without the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                                              plotOutput("plotFDTC43v2", height=400), br(),
                                                                              sliderInput("rhoFDTC43v2", "Mean initial population size",
                                                                                          min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                                                              sliderInput("deltaFDTC43v2", "Survey sensitivity",
                                                                                          min = 0, max = 1, step = .1, value = 0,width = widthbar),br()),
                                                                       column(colsize, h5("Second Model"),
                                                                              HTML("<ul><li>With the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                                              plotOutput("plotFDTC44v2", height=400), br(),sliderInput("rhoFDTC44v2", "Mean initial population size",
                                                                                                                                       min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                                                              sliderInput("deltaFDTC44v2", "Survey sensitivity",
                                                                                          min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                                                              sliderInput("lambdaFDTC44v2", "Population growth rate",
                                                                                          min = 1, max = 3, step = .5, value = 0,width = widthbar)))),
tabPanel("Inferred probability of none detected given population is present", fluidRow( div(style="display:inline-block"), responsive=TRUE,br(),
                                                              column(colsize,h5("First Model"),
                                                                     HTML("<ul><li>Without the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                                     plotOutput("plotNODCTPN43v2", height=400), br(),
                                                                     sliderInput("rhoNODCTPN43v2", "Mean initial population size",
                                                                                 min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                                                     sliderInput("deltaNODCTPN43v2", "Survey sensitivity",
                                                                                 min = 0, max = 1, step = .1, value = 0,width = widthbar),br()),
                                                              column(colsize, h5("Second Model"),
                                                                     HTML("<ul><li>With the inclusion of population growth</li>
                                                                <li>With learning from negative survey results</li></ul>"),
                                                                     plotOutput("plotNODCTPN44v2", height=400), br(),sliderInput("rhoNODCTPN44v2", "Mean initial population size",
                                                                                                                              min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                                                     sliderInput("deltaNODCTPN44v2", "Survey sensitivity",
                                                                                 min = 0, max = 1, step = .1, value = 0,width = widthbar),br(),
                                                                     sliderInput("lambdaNODCTPN44v2", "Population growth rate",
                                                                                 min = 1, max = 3, step = .5, value = 0,width = widthbar))))

)

server <- function(input, output,session) {
## infered population mean
    output$plotMN41v2 <- renderPlot({
        df = case41[rho == input$rhoMN41v2 & !is.na(mean),]
        # ggplot(df, aes(time, mean)) +
        #     geom_point()+ geom_path()+
        #     theme(plot.title = element_text(hjust = 0.5))+scale_y_continuous(limits = range(case41$mean))+
        #     labs(x ="Number of negative survyes", y ="Inferred population mean")+ ggtitle("Inferred population mean") +
        #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        #           panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        #     theme(legend.position="right")+theme(legend.title=element_blank())
        ggplot(df, aes(x = time))+
            geom_point(aes(x=time, y=mean, colour = "Mean"))+ geom_path(aes(y = mean, colour = "Mean"))+
            geom_path(aes(y = var^.5, colour = "Standard deviation"))+geom_point(aes(x=time, y=var^.5,colour = "Standard deviation"))+
            scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Standard deviation"))+
            scale_colour_manual(values = c("blue", "red"))+ labs(y = "Population mean",x = "Number of negative survyes", colour = "Parameter")+
            theme(legend.position = c(0.8, 0.9))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                       panel.background = element_blank(),
                                                       axis.line = element_line(colour = "black"))+theme(legend.title=element_blank())
    })

    output$plotMN42v2 <- renderPlot({
        df = case42[rho == input$rhoMN42v2 & lambda == input$lambdaMN42v2 & !is.na(mean),]
        # ggplot(df, aes(time, mean)) +
        #     geom_point()+ geom_path()+
        #     theme(plot.title = element_text(hjust = 0.5))+
        #     labs(x ="Number of negative survyes", y ="Inferred population mean")+ ggtitle("Inferred population mean") +
        #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        #           panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        #     theme(legend.position="right")+theme(legend.title=element_blank())

        ggplot(df, aes(x = time))+
            geom_point(aes(x=time, y=mean, colour = "Mean"))+ geom_path(aes(y = mean, colour = "Mean"))+
            geom_path(aes(y = var^.5, colour = "Standard deviation"))+geom_point(aes(x=time, y=var^.5,colour = "Standard deviation"))+
            scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Standard deviation"))+
            scale_colour_manual(values = c("blue", "red"))+ labs(y = "Population mean",x = "Number of negative survyes", colour = "Parameter")+
            theme(legend.position = c(0.8, 0.9))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                       panel.background = element_blank(),
                                                       axis.line = element_line(colour = "black"))+theme(legend.title=element_blank())

    })

    output$plotMN43v2 <- renderPlot({
        df = case43[delta == input$deltaMN43v2 & rho == input$rhoMN43v2 & !is.na(mean),]
        # ggplot(df, aes(time, mean)) +
        #     geom_point()+ geom_path()+
        #     theme(plot.title = element_text(hjust = 0.5))+
        #     labs(x ="Number of negative survyes", y ="Inferred population mean")+ ggtitle("Inferred population mean") +
        #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        #           panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        #     theme(legend.position="right")+theme(legend.title=element_blank())
        ggplot(df, aes(x = time))+
            geom_point(aes(x=time, y=mean, colour = "Mean"))+ geom_path(aes(y = mean, colour = "Mean"))+
            geom_path(aes(y = var^.5, colour = "Standard deviation"))+geom_point(aes(x=time, y=var^.5,colour = "Standard deviation"))+
            scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Standard deviation"))+
            scale_colour_manual(values = c("blue", "red"))+ labs(y = "Population mean",x = "Number of negative survyes", colour = "Parameter")+
            theme(legend.position = c(0.8, 0.9))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                       panel.background = element_blank(),
                                                       axis.line = element_line(colour = "black"))+theme(legend.title=element_blank())
    })

    output$plotMN44v2 <- renderPlot({
        df = case44[delta == input$deltaMN44v2 & rho == input$rhoMN44v2 & lambda == input$lambdaMN44v2 & !is.na(mean),]
        # ggplot(df, aes(time, mean)) +
        #     geom_point()+ geom_path()+
        #     theme(plot.title = element_text(hjust = 0.5))+
        #     labs(x ="Number of negative survyes", y ="Inferred population mean")+ ggtitle("Inferred population mean") +
        #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        #           panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        #     theme(legend.position="right")+theme(legend.title=element_blank())
        ggplot(df, aes(x = time))+
            geom_point(aes(x=time, y=mean, colour = "Mean"))+ geom_path(aes(y = mean, colour = "Mean"))+
            geom_path(aes(y = var^.5, colour = "Standard deviation"))+geom_point(aes(x=time, y=var^.5,colour = "Standard deviation"))+
            scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Standard deviation"))+
            scale_colour_manual(values = c("blue", "red"))+ labs(y = "Population mean",x = "Number of negative survyes", colour = "Parameter")+
            theme(legend.position = c(0.8, 0.9))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                       panel.background = element_blank(),
                                                       axis.line = element_line(colour = "black"))+theme(legend.title=element_blank())
    })


## Infered population std
##
    ##
    output$plotSTD41v2 <- renderPlot({
        df = case41[rho == input$rhoSTD41v2 & !is.na(var),]
        ggplot(df, aes(time, sqrt(var))) +
            geom_point()+ geom_path()+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Inferred population standard deviation")+ ggtitle("Inferred population standard deviation") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotSTD42v2 <- renderPlot({
        df = case42[rho == input$rhoSTD42v2 & lambda == input$lambdaSTD42v2 & !is.na(var),]
        ggplot(df, aes(time, sqrt(var))) +
            geom_point()+ geom_path()+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Inferred population standard deviation")+ ggtitle("Inferred population standard deviation") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotSTD43v2 <- renderPlot({
        df = case43[delta == input$deltaSTD43v2 & rho == input$rhoSTD43v2 & !is.na(var),]
        ggplot(df, aes(time, sqrt(var))) +
            geom_point()+ geom_path()+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Inferred population standard deviation")+ ggtitle("Inferred population standard deviation") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotSTD44v2 <- renderPlot({
        df = case44[delta == input$deltaSTD44v2 & rho == input$rhoSTD44v2 & lambda == input$lambdaSTD44v2 & !is.na(var),]
        ggplot(df, aes(time, sqrt(var))) +
            geom_point()+ geom_path()+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Inferred population standard deviation")+ ggtitle("Inferred population standard deviation") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })
## INFRED probability of Absence
##
    output$plotABS41v2 <- renderPlot({
        df = case41[rho == input$rhoABS41v2 & !is.na(extinct),]
        ggplot(df, aes(time, extinct)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case41$extinct))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Inferred probability of absence")+ ggtitle("Inferred probability of absence") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotABS42v2 <- renderPlot({
        df = case42[rho == input$rhoABS42v2 & lambda == input$lambdaABS42v2 & !is.na(extinct),]
        ggplot(df, aes(time, extinct)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case42$extinct))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Inferred probability of absence")+ ggtitle("Inferred probability of absence") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotABS43v2 <- renderPlot({
        df = case43[delta == input$deltaABS43v2 & rho == input$rhoABS43v2 & !is.na(extinct),]
        ggplot(df, aes(time, extinct)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case43$extinct))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Inferred probability of absence")+ ggtitle("Inferred probability of absence") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotABS44v2 <- renderPlot({
        df = case44[delta == input$deltaABS44v2 & rho == input$rhoABS44v2 & lambda == input$lambdaABS44v2 & !is.na(extinct),]
        ggplot(df, aes(time, extinct)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case44$extinct))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Inferred probability of absence")+ ggtitle("Inferred probability of absence") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })


## PROBABILITY none detected v2
##
    output$plotNONDCT41v2 <- renderPlot({
        df = case41[delta == input$deltaNONDCT41v2 & rho == input$rhoNONDCT41v2 & !is.na(notDCT),]
        ggplot(df, aes(time, notDCT)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case41$notDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Inferred probability of none detected")+ ggtitle("Inferred probability of none detected") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotNONDCT42v2 <- renderPlot({
        df = case42[delta == input$deltaNONDCT42v2 & rho == input$rhoNONDCT42v2 & lambda == input$lambdaNONDCT42v2 & !is.na(notDCT),]
        ggplot(df, aes(time, notDCT)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case42$notDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Inferred probability of none detected")+ ggtitle("Inferred probability of none detected") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotNONDCT43v2 <- renderPlot({
        df = case43[delta == input$deltaNONDCT43v2 & rho == input$rhoNONDCT43v2 & !is.na(notDCT),]
        ggplot(df, aes(time, notDCT)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case43$notDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Inferred probability of none detected")+ ggtitle("Inferred probability of none detected") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotNONDCT44v2 <- renderPlot({
        df = case44[delta == input$deltaNONDCT44v2 & rho == input$rhoNONDCT44v2 & lambda == input$lambdaNONDCT44v2 & !is.na(notDCT),]
        ggplot(df, aes(time, notDCT)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case44$notDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Inferred probability of none detected")+ ggtitle("Inferred probability of none detected") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    ## PROBABILITY FIRST DTECTION V2
    ##
    output$plotFDTC41v2 <- renderPlot({
        df = case41[delta == input$deltaFDTC41v2 & rho == input$rhoFDTC41v2 & !is.na(firstDCT),]
        ggplot(df, aes(time, firstDCT)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case41$firstDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Probability of first detection")+ ggtitle("Inferred probability of first detection") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotFDTC42v2 <- renderPlot({
        df = case42[delta == input$deltaFDTC42v2 & rho == input$rhoFDTC42v2 & lambda == input$lambdaFDTC42v2 & !is.na(firstDCT),]
        ggplot(df, aes(time, firstDCT)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case42$firstDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Probability of first detection")+ ggtitle("Inferred probability of first detection") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotFDTC43v2 <- renderPlot({
        df = case43[delta == input$deltaFDTC43v2 & rho == input$rhoFDTC43v2 & !is.na(firstDCT),]
        ggplot(df, aes(time, firstDCT)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case43$firstDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Probability of first detection")+ ggtitle("Inferred probability of first detection") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotFDTC44v2 <- renderPlot({
        df = case44[delta == input$deltaFDTC44v2 & rho == input$rhoFDTC44v2 & lambda == input$lambdaFDTC44v2 & !is.na(firstDCT),]
        ggplot(df, aes(time, firstDCT)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case44$firstDCT))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="Probability of first detection")+ ggtitle("Inferred probability of first detection") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })
## no detect while population is not absent
##

    output$plotNODCTPN41v2 <- renderPlot({
        df = case41[delta == input$deltaNODCTPN41v2 & rho == input$rhoNODCTPN41v2 & !is.na(notDTCPop),]
        ggplot(df, aes(time, notDTCPop)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case41$notDTCPop))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="P(none detected| Population is present)")+ ggtitle("Inferred probability of none detected given population is present") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotNODCTPN42v2 <- renderPlot({
        df = case42[delta == input$deltaNODCTPN42v2 & rho == input$rhoNODCTPN42v2 & lambda == input$lambdaNODCTPN42v2 & !is.na(notDTCPop),]
        ggplot(df, aes(time+1, notDTCPop)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case42$notDTCPop))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="P(none detected| Population is present)")+ ggtitle("Inferred probability of none detected given population is present") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotNODCTPN43v2 <- renderPlot({
        df = case43[delta == input$deltaNODCTPN43v2 & rho == input$rhoNODCTPN43v2 & !is.na(notDTCPop),]
        ggplot(df, aes(time, notDTCPop)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case43$notDTCPop))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="P(none detected| Population is present)")+ ggtitle("Inferred probability of none detected given population is present") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$plotNODCTPN44v2 <- renderPlot({
        df = case44[delta == input$deltaNODCTPN44v2 & rho == input$rhoNODCTPN44v2 & lambda == input$lambdaNODCTPN44v2 & !is.na(notDTCPop),]
        ggplot(df, aes(time, notDTCPop)) +
            geom_point()+ geom_path()+scale_y_continuous(limits = range(case44$notDTCPop))+
            theme(plot.title = element_text(hjust = 0.5))+
            labs(x ="Number of negative survyes", y ="P(none detected| Population is present)")+ ggtitle("Inferred probability of none detected given population is present") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            theme(legend.position="right")+theme(legend.title=element_blank())
    })

    output$slickr <- renderSlickR({
        imgs <- list.files("InferenceB2020", pattern=".PNG", full.names = TRUE)
        slickR(imgs)+ settings(dots = TRUE)
    })

}

shinyApp(ui, server)

