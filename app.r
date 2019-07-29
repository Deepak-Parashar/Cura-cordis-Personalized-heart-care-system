library(shiny)
source("rForest.R")


ui <- fluidPage(
  titlePanel("CURA CORDIS"),
  sidebarLayout(
    sidebarPanel(("Side bar title goes here"),
                 sliderInput("Age", "AGE:",
                             min = 1, max = 100,
                             value = 30,
                             step = 1),
                 radioButtons("Gender" , "Select gender" ,list("Male" = 1 , "Female" = 0) ,"" ),
                 selectInput("chest_pain_type" , "chest pain type" , c("typical angina" = 1 , "atypical angina" = 2 , "non-anginal pain" = 3 , "asymptomatic" = 4) ),
                 sliderInput("Trestbps", "resting blood pressure(in mmhg):",
                             min = 1, max = 100,
                             value = 30,
                             step = 1),
                 textInput("Cholestrol" , "serum cholestoral(in mg/d)"),
                 radioButtons("fasting_blood_sugar" , "fasting blood sugar" , list("0" , "1"), ""),
                 selectInput("restecg" , "restecg" , c(0,1,2)),
                 textInput("thalach" , "thalach" , ""),
                 radioButtons("exang" , "exang" ,list(0,1),""),
                 submitButton("Predict")),
    mainPanel(("Output"),
              verbatimTextOutput("accuracy"),
              verbatimTextOutput("op")
              
              # textOutput("chest_pain_type"),
              # textOutput("Trestbps"),
              # textOutput("Cholestrol") 
              )
  )
)

server <- function(input , output){
  output$Age = renderText(input$Age)
  output$Gender = renderText(input$Gender)
  output$chest_pain_type = renderText(input$chest_pain_type)
  output$Trestbps = renderText(input$Trestbps)
  output$Cholestrol = renderText(input$Cholestrol)
  
  # fn <- reactive({
  #   inp = data.frame(Age = input$Age , 
  #                    Sex  = input$Gender , 
  #                    chest_pain_type = input$chest_pain_type ,
  #                    trestbps = input$Trestbps,
  #                    cholestrol = input$Cholestrol,
  #                    fasting_blood_sugar = input$fasting_blood_sugar,
  #                    restecg = input$restecg,
  #                    thalach = input$thalach,
  #                    exang = input$exang)
  #   accuracy = rForest(inp) * 100
  #   output$accuracy = renderText(paste("Accuracy is :" , accuracy , "%"))
  # })
  
  
  
   accuracy = rForest() * 100
  
    output$accuracy = renderText(paste("Accuracy is :" , accuracy , "%"))
   output$op = renderText(paste("Heart disease risk on scale(0-4) is : 1"))
}


shinyApp(ui = ui, server = server)