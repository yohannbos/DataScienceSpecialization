library(shiny)

server <- function(input, output) {
  output$bmi <- renderPlot({ library(ggplot2)
    wt <- seq(50,170, 0.7)
    ht <- seq(100,220, 0.7)
    bmi = function (wt, ht){wt/(ht*ht*(10^-4))}
    df <- expand.grid(x = as.numeric(wt), y = as.numeric(ht))
    df$z <- as.factor(with(df, ifelse(bmi(x,y)<=18, "underweighted",
                                    ifelse(bmi(x,y)<=25,"normal",
                                           ifelse(bmi(x,y)<=30, "overweigthed","obese")))))
    colnames(df) <- c("weight", 'height', "lvl")
    inpu<-c(input$height, input$weight)
    g<-ggplot(df, aes(y=weight,x=height, colour =lvl))
    g + geom_point() + geom_point(aes(x=inpu[1], y=inpu[2]), color = 'red', size = 3)})

}

ui<-pageWithSidebar(
  headerPanel ('Compute your Body Mass Index (BMI)'),
  sidebarPanel (numericInput('height', 'Your height (cm)', min = 100, max = 220 ),
                numericInput('weight', 'Your weight (kg)', min = 50, max = 170 ),
                helpText('BMI Categories:
                         Underweight = <18.5
                         Normal weight = 18.5-24.9
                         Overweight = 25-29.9
                         Obesity = BMI of 30 or greater')),
  mainPanel(plotOutput('bmi'))
)


shinyApp(ui = ui, server = server)