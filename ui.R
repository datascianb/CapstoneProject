# install.packages("shiny")

library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Predict the Next Word!"),
    sidebarPanel(
        h4("This Shiny Application predicts the word that is most probable to occur after your
           input phrase. Data from twitter, news and blogs was cleaned to create n-grams with 
           frequencies. The prediction model this data to predict the next word using 
           Katz's Back-Off model and Good Turing Matrix."),
        helpText("Please enter the phrase for which you want 
                 to predict the next word and hit Submit!"),    
        textInput("userIp", "Phrase", "you are a"),
        br(),
        submitButton('Predict Next Word'),
        br()
    ),
    mainPanel(
        h3('Confirming Your Input'),
        verbatimTextOutput("userIp"),
        br(),
        h4('The next word:'),
        verbatimTextOutput("pWordMethod")
    )
))

