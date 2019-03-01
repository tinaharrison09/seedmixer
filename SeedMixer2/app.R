require(shiny)
require(shinythemes)
require(shinyjs)

ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel(div("Seed Mixer",
                  img(src = "banner_seeds.jpg", height = 100))), 
  navbarPage("Mixer Tools",
             tabPanel("Home", value="tab-home",
                      br(),
                      h3("Optimized wildflower seed mixes for pollinator enhancement plantings"),
                      br(),
                      "Seed Mixer provides evidence-based recommendations for selecting wildflower species 
                      to include in pollinator enhancement plantings, with a focus on agricultural pollination. 
                      Seed Mixer's recommendations currently apply to north-central CA only (see Details). 
                      However, anyone can use Seed Mixer to explore how combinations of wildflower species support bee diversity.",
                      br(),
                      img(src = "banner_bees.jpg", height = 150),
                      hr(),
                      br(),
                      h4("Consider goals for your pollinator enhancement planting"),
                      br(),
                      br(),
                      strong("Bee diversity."), 
                      "Pollinator plantings need to include multiple plants to support bees with different flower preferences. 
                      Choosing plants that bloom at different times helps bees find food all year long. ",
                      strong("Crop pollination"),
                      "Some bee species are particularly valuable crop pollinators. 
                      Choosing plants that bloom before, during or after crop bloom may encourage pollinators to visit crop flower.",
                      strong("Herbivores"),
                      "Plants differ in the number and kind of insect herbivores they support. 
                      Some herbivorous insects are harmless or valuable as food for beneficial predator insects. 
                      Others may be damaging crop pests.",
                      strong("Cost"),
                      "A pollinator enhancement planting is an investment. 
                      Wildflower species vary in their cost to establish and maintain, 
                      so cheaper species should be used whenever they provide sufficient benefit.",
                      br(),
                      br(),
                      "Decisions about which plant species to include should ultimately be made 
                      in consultation with local extension and other resources", 
                      # actionLink("to_deets1", "resources"), ".",
                      br(),
                      br(),
                      br(),
                      br()

             ), # close Home tabPanel
             
             #### new panel
             tabPanel("Explorer", value="tab-expl"), # close Explorer tabPanel
             tabPanel("Optimizer", value="tab-optim",
                      
                 # a place to input the budget and goals
                 
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Input"),
                          numericInput("budgetInput", "Budget", value = 500, min = 100, max = 1000, step = 50),
                          radioButtons("radio", "Goal",
                                       choices = list("biodiversity" = 1, "pollination" = 2), 
                                       selected = 1),
                          actionButton("button", "Run Optimizer"),
                          useShinyjs() # for disabling button during runs
                        ), # close sidebar panel

                        
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Main",
                                               plotOutput("optPlot", width = "80%"),
                                               textOutput("TestText")
                                               ),
                                      tabPanel("Explore"),
                                      tabPanel("Export")

                        )) # end tabset, main Panel
                      )
                      
                      
                      ), # close Optimizer tabPanel
             tabPanel("More Info", value="tab-deets",
                      h3("Details on underlying data and models"),
                      br(),
                      "The currently supported plant list is 41 native CA wildflower species that have been preselected 
                      to have desireable properties for pollinator enhancement plantings, and their attractiveness 
                      to pollinators and other insects has been tested at field sites near Davis, CA.",
                      br(),
                      br(),
                      img(src = "ill_bbplots.jpg", width =400, tags$figcaption("Experimental plantings at UC Davis")),
                      br(),
                      br(),
                      h4("Other resources"),
                      uiOutput("Calscape"),
                      uiOutput("UC.IPM")
             ) # close Details tabPanel
             ) # close navbarPage
             ) # close fluidPage
server <- function(input, output) {
  
  url1 <- a("Calscape", href = "https://calscape.org/about.php")
  output$Calscape <- renderUI({tagList(url1)})
  
  url2 <- a("UC.IPM", href = "http://ipm.ucanr.edu/")
  output$UC.IPM <- renderUI({tagList(url2)})
  
  
  #### GA for a budget
  #### put in reactive expression so it ONLY updates when wiget changes
   GA.out <- reactiveValues()
   observeEvent(input$button, {
     withProgress(message = "Optimizing...", value=0, {
                          disable("button") # nice! stop people from poking button while calc is running
       
                          Suggest = randmix[, which(rand.cost < input$budgetInput)[1:min(20, nrow(randmix))]] %>% t
       
                          GA = ga(type = "binary",
                                   fitness = function(x, B = input$budgetInput) {
                                                        elastic = 1/(cost(x) - B)^2
                                                        biodiv(x)*ifelse(cost(x) - B <= 1, 1, elastic)},
                                   # function(x) {biodiv(x)*ifelse(cost(x) < input$budgetInput, 1, 0)}, # inelastic
                                   nBits = S,
                                  popSize = 200,
                                  pcrossover = 0.5,
                                  pmutation = 0.2,
                                  # elitism is the number of best individuals to survive
                                  elitism = base::max(1, round(50*0.05)),
                                  maxiter = 500, # max iteration before GA stops
                                  run = 500, # no. consecutive gens before GA stops
                                  maxFitness = max.bd,
                                  names = NULL,
                                  suggestions = Suggest,
                                  optim = FALSE,
                                  # optimArgs = long blah,
                                  keepBest = TRUE,
                                  parallel = FALSE,
                                  monitor = barMonitor,
                                  seed = NULL)
    } ) # close withProgress
     enable("button")
                           
                           GA.out$costs = GA@solution %>% apply(1, cost)
                           GA.out$bees = rep(GA@fitnessValue, nrow(GA@solution))
                           GA.out$comment = ifelse(GA@fitnessValue < 0.1*max.bd, 
                                                   "Sorry, we haven't found a solution. 
                                                   Please increase budget and run Optimizer again.",
                                                   "Solution found! Explore.")
                          
                       #output$optPlot <- renderPlot(points(ga.costs, rep(GA@fitnessValue, length(ga.costs))))
     #GA.out$costs = c(1,100,500,1000)
     #GA.out$bees = c(1, 10, 20, 40)
     }) # close observe event
   


  #### plot
  
output$optPlot <- renderPlot({
  
  # static plot  
  plot(c(1, max.cost), c(1,max.bd), 
       type = "n", ylab = "bee species", xlab = "mix cost")
  # fill with some random values
  points(rand.cost, rand.bd, col=alpha("gray", 0.5), pch=19,
         cex=log(colSums(randmix), 10)+0.5)
  points(monocost, monobees, col=alpha("black", 0.8), pch=19, cex=0.5)
  
  # interactive budget slider
  abline(v=input$budgetInput)
  
  # add point from optimization output
  points(GA.out$costs, GA.out$bees, col="red", pch=19)
  # observeEvent(input$button, points(c(1,100,500,1000), c(1, 10, 20, 40)))
  }) # close render bee plot

output$TestText <- renderText(GA.out$comment)
  
} # end server function
shinyApp(ui = ui, server = server)


