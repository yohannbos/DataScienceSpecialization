
bmi<- function (wt, ht){wt/(ht*ht*(10^-3))}

shinyServer (function(input, output) {
  output$bmi <- renderPlot({ library(ggplot2)
    library(RColorBrewer)
    wt <- seq(50,170, 0.7)
    ht <- seq(100,220, 0.7)
    df <- expand.grid(x = as.numeric(wt), y = as.numeric(ht))
    df$z <- as.factor(with(df, ifelse(bmi(x,y)<=1.8, "underweighted",
                                      ifelse(bmi(x,y)<=2.5,"normal",
                                             ifelse(bmi(x,y)<=3.0, "overweigthed","obese")))))
    colnames(df) <- c("weight", 'height', "bmi")
    inpu<-c(input$height, input$weight)
    g<-ggplot(df, aes(y=weight,x=height, colour =bmi))
    g + geom_point() +scale_color_brewer(palette="YlOrRd")+ geom_point(aes(x=inpu[1], y=inpu[2]), color = 'black', size = 4)})

  output$text <- renderText(paste("Your BMI is: ",round(bmi(input$height, input$weight), 2) ))}
)