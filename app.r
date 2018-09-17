library(shiny)
library(shinyBS)

class <- readRDS("model.rds")
dist <- readRDS("dist2.rds")
final_ds <- read.csv("final_ds.csv")
ui <- fluidPage(
  
  titlePanel(title=(img(src="data_project.jpg", height = 300, width = 1300, style = "display: block;"))),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("age",
                     "Укажите возраст ребёнка",
                     min = 1,
                     max = 20,
                     value = 5),
      radioButtons("sex", "Укажите пол ребёнка", choices = c("Мужской" = 1,
                                                           "Женский" = 0,
                                                           "Не важно" = 3)),
      
      sliderInput("price",
                  "Укажите ориентировочную цену",
                  min = 1,
                  max = 150,
                  value = 5),submitButton("Подобрать")),
      
      # Show a plot of the generated distribution
      mainPanel(
        htmlOutput("demo"),
        actionButton("contacts", "О нас", style = "margin-top:32%; width:100%",class = "btn-primary")
      
        ,bsModal("modalWindow", "О нас", "contacts", size = "large", h1("Шапошникова Полина"),
                 a("https://vk.com/id61105498"),h1("Фролов Сергей"),
                 a("https://vk.com/id244198430"),
                 h1("Малыгина Ольга"),
                 a("https://vk.com/leli13"),
                 h1("Наумова Анастасия"),
                 a("https://vk.com/id12965220"),h1("Github"),
                 a("https://github.com/HsergeyF/toy_expert"))
         
      )
   
      
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   #input$price

  
  output$demo <- renderUI({
    
    man = 0
    female = 0
    
    if(input$sex == 1){
      man =1  
    }
    else if(input$sex == 0)
    {
      female =1 
    }
    else if(input$sex == 3){
      man = 1
      female = 1
    }
    product = "p"
    for (i in 1:10000)
    {if(class$price[i]< as.numeric(input$price) && class$boy[i] == man && class$girl[i] == female && class$recommended_age[i]<as.numeric(input$age)&& product == "p"){
      class$id <- rownames(class)
      consumerChoice = class$id[i]
      break;
    }
    }
    
    class$id <- rownames(class)
    
    finalrecommend_X = tail(sort(dist[consumerChoice,]))
    finalrecommend_X = finalrecommend_X[-1]
    rec_id_x = rownames(as.data.frame(finalrecommend_X))
 
    toys_names_x <- final_ds$product_name[match(rec_id_x, final_ds$uniq_id)]
    toys_names_x = as.vector(toys_names_x)
    for(i in 1:5){
      toys_names_x[i] = paste0(") ",toys_names_x[i]) 
      toys_names_x[i] = paste0(as.character(i),toys_names_x[i]) 
      toys_names_x[i] = paste0(toys_names_x[i],"</br>") 
    }
HTML(toys_names_x)

    })
  
}

shinyApp(ui = ui, server = server)

