require(shiny)
require(shinythemes)
require(shinyjs)
require(DT)

# testing push from local RStudio commit to online repo

source("code/ecology.R")
# source("SeedMixer2/code/decision.R")

ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel(div("Seed Mixer",
                  img(src = "banner_seeds.jpg", height = 100))), 
  navbarPage("",
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
             tabPanel("Plants", value="tab-expl",
                      dataTableOutput("plant_table")
                      ), # close Explorer tabPanel
             tabPanel("Optimize Mix", value="tab-optim",
                      
                 # a place to input the budget and goals
                 
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel(h2("Input")),
                          numericInput("budgetInput", "Budget", 
                                       value = 300, min = 100, max = 1000, step = 50,
                                       width = "50%"),
                          radioButtons("radio", "Goal",
                                       choices = list("biodiversity" = 1, "pollination" = 2), 
                                       selected = 1),
                          # numericInput("runInput", "Runs", 
                          # value = 100, min = 50, max = 500, step = 50,
                          #                  width="50%"),
                          actionButton("button", "Run Optimizer"),
                          useShinyjs() # for disabling button during runs
                        ), # close sidebar panel

                        
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Mixes",
                                               plotOutput("optPlot", width = "80%",
                                                          brush = "plot_brush",
                                                          click = "plot_click"),
                                               br(),
                                               hr(),
                                               textOutput("mix_select"),
                                               br(),
                                               verbatimTextOutput("TestText")
                                               ),

                                      tabPanel("Compare", 
                                               plotOutput("decisionPlot")),
                                        # note, adding interactive plotting would require
                                      # switching from single to multiple plots,
                                      # so we can add points to all of them based on selection of points from one of them
                                      
                                      tabPanel("View",
                                               tableOutput("mixTable"))

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


##############################################################################
##############################################################################

server <- function(input, output) {
  
  url1 <- a("Calscape", href = "https://calscape.org/about.php")
  output$Calscape <- renderUI({tagList(url1)})
  
  url2 <- a("UC.IPM", href = "http://ipm.ucanr.edu/")
  output$UC.IPM <- renderUI({tagList(url2)})
  ?renderDataTable
  
  ### PLANTS
  output$plant_table <- renderDataTable(tidy_plant, 
                          colnames=c("Common name", "Scientific name", "Family", "Growth", "Peak Bloom"),
                          rownames=F)
  
  #### GA for a budget
  #### put in reactive expression so it ONLY updates when wiget changes
   GA.out <- reactiveValues(obj = data.frame())
   observeEvent(input$button, {
     withProgress(message = "Optimizing...", value=0, {
                          disable("button") # nice! stop people from poking button while calc is running
       
                          Suggest = randmix[, which(rand.cost < input$budgetInput)[1:min(20, nrow(randmix))]] %>% t
                          
                          
                          fitness = function(x, B = input$budgetInput) {
                            elastic = 1/(cost(x) - B)^2
                            biodiv(x)*ifelse(cost(x) - B <= 1, 1, elastic)}
       
                          GA <- ga(type = "binary", # my decision variables are binary
                                   fitness <- fitness, # any function receiving a potential solution and returning a single "fitness" score to max/min
                                   # min/max not relevant for binary decision variables
                                   nBits = Sp,
                                   popSize = 200,
                                   pcrossover = 0.5,
                                   pmutation = 0.2,
                                   # elitism is the number of best individuals to survive
                                   elitism = base::max(1, round(200*0.05)),
                                   maxiter = Runs, # max iteration before GA stops
                                   run = Runs, # no. consecutive gens before GA stops
                                   maxFitness = max.bees,
                                   names = NULL,
                                   suggestions = Suggest,
                                   optim = FALSE,
                                   # optimArgs = long blah,
                                   keepBest = F, # I want the evolved pop, not best of early runs
                                   parallel = FALSE,
                                   monitor = barMonitor,
                                   seed = NULL # 
                          ) # close GA
    } ) # close withProgress
     enable("button")
     
     # plants in columns, mixes in rows
     mixtab = GA@population %>% unique %>% t
     rownames(mixtab) = Tp$Common.name

     obj = data.frame(
       is.opt = apply(mixtab, 2, fitness) == GA@fitnessValue,
       bees = mixtab %>% apply(2, biodiv),
       cost = mixtab %>% apply(2, cost),
       herb = mixtab %>% apply(2, herbivore),
       pollen = mixtab %>% apply(2, pollen, "sunflower"),
       plants = colSums(mixtab)
            )

     GA.out$mixes = mixtab[, order(obj$bees, decreasing = F)]             
     GA.out$obj = obj[order(obj$bees, decreasing = F), ]

                           # #GA.out$comment = ifelse(GA@fitnessValue < 0.1*max.bees, 
                           #                         "Sorry, we haven't found a solution. 
                           #                         Please increase budget and run Optimizer again.",
                           #                         "Solution found! Explore.")
    
      GA.out$bestout = GA.out$mixes[, GA.out$obj$is.opt] %>% .[order(rowSums(.), decreasing=T), ]
     
                          
     }) # close observe event
   


  #### plot
  
output$optPlot <- renderPlot({
  
  # static plot  
  plot(c(1, max.cost), c(1,max.bees), 
       type = "n", ylab = "bee species", xlab = "mix cost",
       cex.axis = 1.3, cex.lab = 1.3, lwd=2)
  legend("bottomright", 
         c("single plant", "random mix", "better mix", "best mix"), 
         pch=19, pt.cex=c(0.5, rep(1.5, 3)), 
         col=c("darkgray", "lightgray", marker[1], "tomato"), 
         bty="n")
  # fill with some random values
  points(rand.cost, rand.bees, col=alpha("lightgray", 0.5), pch=19,
         cex=log(colSums(randmix), 10)+0.5)
  points(mono.cost, mono.bees, col=alpha("darkgray", 0.8), pch=19, cex=0.5)
  
  # interactive budget slider
  abline(v=input$budgetInput, lwd = 2)
  
  # add point from optimization output
  ### Reactive - add points from GA output
    if(nrow(GA.out$obj) > 0) {
    subset(GA.out$obj, !is.opt) %>% with(points(cost, bees, cex = log(plants, 10)+0.5, pch=19, col=alpha(good.blue, 0.8)))
    subset(GA.out$obj, is.opt) %>% with(points(cost, bees, cex = log(plants, 10)+0.5, pch=19, col="tomato"))}
  
#  points(GA.out$costs, GA.out$bees, col="red", pch=19)
  # observeEvent(input$button, points(c(1,100,500,1000), c(1, 10, 20, 40)))
  }) # close render bee plot

### main plot: brushing and clicking
### click = print plant list of nearest plant
### brush = empty for now.

  
output$mix_select <- renderPrint({
  get = nearPoints(rand, input$plot_click, xvar = "cost", yvar="bees", 
             threshold = 20, maxpoints = 1)$findme
  paste0(Tp$Common.name[which(randmix[, get] == 1)])
  # brushedPoints(rand, input$plot_brush, xvar = "cost", yvar="bees")
  #paste0(Tp$Common.name[which(randmix[, clicked$findme] == 1)])
  # helper functions for selecting data table rows from plot clicks/brushes
  # brushedPoints(GA.out$obj, input$plot_brush, xvar = "cost", yvar="bees")
})

output$TestText <- renderText(paste0(input$plot_click))

## table

output$mixTable <- renderTable(GA.out$bestout, rownames = TRUE, colnames=FALSE, digits = 0)

## decision plot
output$decisionPlot <- renderPlot(
         ### check v. max-mins
           # max = c(1, Sb, max.cost, max(Tp$Herbivores), sum(Tb$sunflower), Sp)
            # min = apply(obj, 2, min)
  if(nrow(GA.out$obj) > 0) {

    par(mfrow=c(2,3), mar=c(1,2,2,1))
    sapply(1:6, function(i) {plot(as.numeric(GA.out$obj[,i]), 
                                  main = colnames(GA.out$obj)[i], 
                                  xlab = "", ylab="", xaxt = "n",
                                  pch=19, col=c(good.blue, "tomato")[as.numeric(GA.out$obj$is.opt)+1])
      if(i==3) abline(h=input$budgetInput, lwd=2)}
      
    ) # closes sapply
    
    
  } # closes if 
  ) # closes renderPlot
  
  
} # end server function
shinyApp(ui = ui, server = server)


