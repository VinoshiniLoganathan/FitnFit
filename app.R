library(shiny) 
library(dplyr)

allfood <- read.csv(file="Data/restaurant_sample.csv", header=TRUE, sep=",")
food<-sort(unique(allfood $ item_name) )

ui<- fluidPage(
  titlePanel("FitnFit"),
  sidebarLayout (
    sidebarPanel(
      numericInput(inputId = "weight", label = 
                     "Enter your weight (kilogram,kg)", value = 10, min = 1, max=300, step=1),
      numericInput(inputId = "height", label = 
                     "Enter your height (metre,m)", value = 10, min = 1, max=500, step=2),
      selectInput(
        inputId = 'item_name',
        label = 'Choose an item from the selected fast food restaurants ',
        choices = food,
        selected= "Kamikaze Brownie" ),
      
      radioButtons('activity',
                   label=h4('Best Exercise Ever (Choose One).'),
                   choices = c('Zumba','Swimming','Walking','Cycling','Jogging'),
                   selected='Zumba'),
      
      sliderInput('time',h4('Time of Activity (minutes)'),value=30,min=0,max=180,step=5),
      
      submitButton("Submit")
    ),
    
    mainPanel(
      
      h3('Your BMI is:'),
      h4('Weight:'),
      verbatimTextOutput("weigh"),
      h4('Height:'),
      verbatimTextOutput("heigh"),
      h4('BMI and Category:'),
      verbatimTextOutput("bmi"),
      h3('Chosen Fast Food Item:'),
      DT::dataTableOutput(outputId = "summary"),
      h3('Chosen Activity:'),
      verbatimTextOutput('myactivity'),
      h5('Desired Duration in Minutes:'),    
      verbatimTextOutput('mymins'),    
      h5('Estimated Calories Burned:'),
      verbatimTextOutput('mycals')
      
      
    )
  )
)

server <- function(input,output,session){
  
  
  output$weigh <- renderText(input$weight)
  output$heigh <- renderText(input$height)
  output$bmi <- renderText({ShowOptions(input$weight,input$height)})
  output$mymins<-renderText(input$time)
  output$mycals<-renderText({getcalories(input$weight,input$activity,input$time)})
  
  BMI <- function(weigh,heigh){
    BMI <- weigh / (heigh*heigh)
    if(BMI<18.5) return(paste(BMI,"Underweight"))
    else if (BMI<25) return(paste(BMI,"Normal weight"))
    else if (BMI<30) return(paste(BMI,"Overweight"))
    else if (BMI<35) return(paste(BMI,"Obesity I"))
    else if (BMI<40) return(paste(BMI,"Obesity II"))
    else if (BMI>=40) return(paste(BMI,"Obesity III"))
  }
  
  getmets<-function(activity) {
    switch(activity,'Zumba'=7.3,'Swimming'=5.8,'Walking'=3.5,'Cycling'=5,'Jogging'=7);
  }
  
  
  getcalories<-function(weigh,activity,duration) {
    x<-weigh
    y<-getmets(activity)
    z<-duration/60
    x*y*z
  }
  
  ShowOptions <- function(weigh,heigh){
    cal <- paste(BMI(weigh,heigh))
    return(cal)
    
  }
  
  
  
  output$summary <- DT::renderDataTable({
    new_dataset <- allfood %>% filter(item_name==input$item_name) %>%
      select(brand_name,item_name, nf_calories)
    DT::datatable(data=new_dataset, colnames=c("Restaurant Name", "Item Name", "Total Calories"), options = list(dom='t'))
    
  })
} 

shinyApp(ui=ui, server=server)  

