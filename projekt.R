library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)

circuits <- read.csv("2014/circuits_2014.csv", stringsAsFactors = F)
constructorResults <- read.csv("2014/constructorResults_2014.csv", stringsAsFactors = F)
constructors <- read.csv("2014/constructors_2014.csv", stringsAsFactors = F)
constructorStandings <- read.csv("2014/constructorStandings_2014.csv", stringsAsFactors = F)
drivers <- read.csv("2014/drivers_2014.csv", stringsAsFactors = F)
driverStandings <- read.csv("2014/driverStandings_2014.csv", stringsAsFactors = F)
lapTimes <- read.csv("2014/lapTimes_2014.csv", stringsAsFactors = F)
pitStops <- read.csv("2014/pitStops_2014.csv", stringsAsFactors = F)
qualifying <- read.csv("2014/qualifying_2014.csv", stringsAsFactors = F)
races <- read.csv("2014/races_2014.csv", stringsAsFactors = F)
results <- read.csv("2014/results_2014.csv", stringsAsFactors = F)
seasons <- read.csv("2014/seasons_2014.csv", stringsAsFactors = F)
status <- read.csv("2014/status_2014.csv", stringsAsFactors = F)

res <- left_join(results, races, by = "raceId")
res_drv <- left_join(res, drivers, by = "driverId")
res_drv_con <- left_join(res_drv, constructors, by = "constructorId")
res_drv_con_cir <- left_join(res_drv_con, circuits, by = 'circuitId')
all <- left_join(res_drv_con_cir, status, by = "statusId")

ui <- fluidPage(theme=shinytheme("sandstone"),
                titlePanel("2014-2017 F1 results analysis"),
                sidebarLayout(
                  
                  # Inputs
                  sidebarPanel(width = 4,
                               
                               selectInput(inputId = "driver", 
                                           label = "Driver:",
                                           choices = c("Lewis Hamilton", "Nico Rosberg", "Fernando Alonso", "Kimi Raikkonen", "Felipe Massa", "Adrian Sutil",
                                                       "Jenson Button", "Sebastian Vettel", "Romain Grosjean", "Kamui Kobayashi", "Pierre Gasly", "Nico Hulkenberg",
                                                       "Pastor Maldonado", "Paul di Resta", "Sergio Perez", "Daniel Ricciardo", "Jean-Eric Vergne", "Max Chilton",
                                                       "Esteban Gutierrez", "Valtteri Bottas", "Jules Bianchi", "Kevin Magnussen", "Daniil Kvyat", "Andre Lotterer",
                                                       "Marcus Ericsson", "Will Stevens", "Max Verstappen", "Felipe Nasr", "Carlos Sainz", "Roberto Merhi",
                                                       "Alexander Rossi", "Jolyon Palmer", "Pascal Wehrlein", "Rio Haryanto", "Stoffel Vandoorne", "Esteban Ocon",
                                                       "Lance Stroll", "Antonio Giovinazzi", "Brendon Hartley"), 
                                           selected = "Lewis Hamilton"),
                               selectInput(inputId = "grand_prix", 
                                           label = "Grand Prix:",
                                           choices = c("All", "Australian Grand Prix", "Chinese Grand Prix", "Bahrain Grand Prix","Russian Grand Prix",
                                                       "Spanish Grand Prix", "Monaco Grand Prix", "Canadian Grand Prix", "Azerbaijan Grand Prix",
                                                       "Austrian Grand Prix", "British Grand Prix", "Hungarian Grand Prix", "Belgian Grand Prix",
                                                       "Italian Grand Prix", "Singapore Grand Prix", "Malaysian Grand Prix", "Japanese Grand Prix",
                                                       "United States Grand Prix", "Mexican Grand Prix", "Brazilian Grand Prix", "Abu Dhabi Grand Prix",
                                                       "German Grand Prix", "European Grand Prix"),
                                           selected = "All"),
                               selectInput(inputId = "year", 
                                           label = "Year:",
                                           choices = c("All", "2014", "2015", "2016", "2017"),
                                           selected = "All")
                  ),
                  
                  mainPanel(
                    tabsetPanel(type="tabs",
                                tabPanel("Points", plotOutput("points")),
                                tabPanel("Not finished", plotOutput("not_finished"))
                                #,
                                #tabPanel("description1", plotOutput("id1")),
                                #tabPanel("description2", plotOutput("id2"))
                    )
                  )
                )
)

server <- function(input, output) {
  points_limits = c(0,25)
  status_limits = c(0, 136)
  finished_status <- c(1, 11, 12, 13, 14, 16, 18)
  
  output$points <- renderPlot({
    if(input$year == "All"){
      if(input$grand_prix == "All") {
        data <- all
      } else {
        data <- all %>% subset(name.x == input$grand_prix)
      }
    } else {
      if(input$grand_prix == "All") {
        data <- all
        data <- data %>% subset(year  == input$year)
      } else {
        data <- all
        data <- data %>% subset(name.x == input$grand_prix)
        data <- data %>% subset(year  == input$year)
      }
    }
      
    drv <- input$driver
    drv_split <- unlist(strsplit(drv, " "))
    drv_id <- drivers %>% subset(surname == drv_split[2])
    drv_id <- drv_id$driverId
    drv_results <- data %>% subset(driverId  == drv_id)
    drv_results <- drv_results[order(drv_results$date),]
    ggplot(drv_results, aes(date, points, colour = year, Label = name.y)) + geom_point(size=3) + ylim(points_limits) + theme_bw() +
      xlab("Date") + ylab("Points")

  })
  
  output$not_finished <- renderPlot({
    if(input$year == "All"){
      if(input$grand_prix == "All") {
        data <- all
      } else {
        data <- all %>% subset(name.x == input$grand_prix)
      }
    } else {
      if(input$grand_prix == "All") {
        data <- all
        data <- data %>% subset(year  == input$year)
      } else {
        data <- all
        data <- data %>% subset(name.x == input$grand_prix)
        data <- data %>% subset(year  == input$year)
      }
    }
    
    drv <- input$driver
    drv_split <- unlist(strsplit(drv, " "))
    drv_id <- drivers %>% subset(surname == drv_split[2])
    drv_id <- drv_id$driverId
    drv_results <- data %>% subset(driverId  == drv_id)
    drv_results <- drv_results[order(drv_results$date),]
    drv_dnf <- drv_results %>% subset(!statusId %in% finished_status)
    ggplot(drv_dnf, aes(date, statusId, colour = as.factor(statusId))) + geom_point(size=3) + ylim(status_limits) + theme_bw() + 
      scale_color_discrete(name  ="Reason of retirement", breaks = drv_dnf$statusId, labels = drv_dnf$status) +
      xlab("Date") + ylab("Reason of retirement")
  })  
}

shinyApp(ui = ui, server = server)