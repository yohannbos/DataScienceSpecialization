library(shiny)

shinyUI(pageWithSidebar(
  headerPanel ('Compute your Body Mass Index (BMI)'),
  sidebarPanel (numericInput('height', 'Enter your height (cm)', 180,min = 100, max = 220 ),
                numericInput('weight', 'Enter your weight (kg)', 75, min = 50, max = 170 ),
                helpText('How does it work: This simple application allows you to compute your BMI based on your weight and height.
                          BMI Categories:
                         Underweight = <18.5,
                         Normal weight = 18.5-24.9,
                        Overweight = 25-29.9,
                         Obesity = BMI of 30 or greater')),
  mainPanel(plotOutput('bmi'),
            textOutput("text"))
)
)

