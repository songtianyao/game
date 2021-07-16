library(shiny)
library(stringi)
library(shinythemes)
??shinythemes
#read the idioms
Idioms <- read.table(file = "D:/桌面/A_Z.txt")
#trans to vector
Vector_Idioms <- as.vector(as.matrix(Idioms))
#function read the last char
getlastKey <- function(input_idioms)
{
  key <- stri_sub(input_idioms, from = -1)
  return(key)
}
#function read the first char
getfirstKeys <- function(input_idioms)
{
  key <- stri_sub(input_idioms, from = 1,to = 1)
  return(key)
}

#get the fisrt char index
keys <- getfirstKeys(Vector_Idioms)
lookupIdioms <- function (find_idioms,max_found = 1) {
  if(length(which(Vector_Idioms == find_idioms))==0) return("哈哈，你输入的不是成语，我赢了!")
  #get the last char
  key <- getlastKey(find_idioms)
  found <- which(keys == key)
  if (length(found) > max_found) return(Vector_Idioms[found[1:max_found]])
  if (length(found) == 0) return("(〃＞目＜),找不到可以接龙的成语,我认输！")
  if (length(found) <= max_found) return(Vector_Idioms[found[1:max_found]])
}
help(shiny)
ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme = shinytheme("flatly"),
  # Application title
  titlePanel("Start Game"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("idioms_id",label ="请输入一个成语",value = " "),
      actionButton("update", "continue"),
      actionButton("update", "   exit   "),
    ),
    selectInput("var",
                label = "Choose a variable to display",
                choices = c("Percent White", "Percent Black",
                            "Percent Hispanic", "Percent Asian"),
                selected = "Percent White"),
    # Show the result
    mainPanel(
      h3("成语接龙:"),
      verbatimTextOutput("textPlot")
    )
  )
)
server <- function(input, output) {
  results <- eventReactive(input$update, {
    lookupIdioms(trimws(input$idioms_id),max_found = 1)
  })
  output$textPlot <- renderText(results())
}
# Run the application
shinyApp(ui = ui, server = server)



