'''
linear functions: biodiv, cost.s, crop pollinator, herbivore.s
non-linear functions: cost, herbivore.
'''
require(scales)


diag(S) %>% apply(1, herbivore) %>% max
monocost = diag(S) %>% apply(1, cost) 
monobees = diag(S) %>% apply(1, biodiv)
Budget = 600
max.bd = biodiv(rep(1,S))
max.cost = rep(1, S) %>% cost

randmix = sapply(1:1000, function(x) rbinom(S, 1, runif(1)))
dim(randmix)
rand.bd = randmix %>% apply(2, function(x) biodiv(x))
rand.cost = randmix %>% apply(2, function(x) cost(x))



plot(c(1, max.cost), c(1,max.bd), 
     type = "n", ylab = "bee species", xlab = "mix cost")
# fill with some random values
points(rand.cost, rand.bd, col=alpha("gray", 0.5), pch=19,
       cex=log(colSums(randmix), 10)+0.5)
points(monocost, monobees, col=alpha("black", 0.8), pch=19, cex=0.5)
abline(v=Budget)

fitness = function(x, B = Budget) {
    elastic = 1/(cost(x) - B)^2
  biodiv(x)*ifelse(cost(x) - B <= 1, 1, elastic)}
# what if you used a more elastic threshold? so the *best* mix is under budget
# but that way the GA can learn from mixes that happen to be a bit over

rbinom(S, 1, 0.2) %>% fitness
Suggest %>% apply(1, fitness)

require(GA)
require(ecr)
require(vegan)

Suggest = randmix[, which(rand.cost < Budget)[1:min(20, nrow(randmix))]] %>% t
dim(Suggest)

GA <- ga(type = "binary", # my decision variables are binary
         fitness <- fitness, # any function receiving a potential solution and returning a single "fitness" score to max/min
         # min/max not relevant for binary decision variables
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
         monitor = myMonitor,
         seed = NULL # 
)

# modify GA monitor to return something I want
myMonitor <- function (object, ...) 
{
  fitness <- na.exclude(object@fitness)
  sumryStat <- max(fitness) %>% format(digits = 0)
  cat(paste("Try no.", object@iter, "| Best =", sumryStat))
}

barMonitor <- function (object, ...) 
{
  best = na.exclude(object@fitness) %>% max %>% format(digits = 0)
  incProgress(1/500, detail = paste("Try no.", object@iter, "| Best =", best))
}



# clear cache
# forget(m.get_gamma)
str(GA)

# check costs
ga.costs = GA@solution %>% apply(1, cost)
points(ga.costs, rep(GA@fitnessValue, length(ga.costs)))

c(GA@solution) %>% fitness
c(GA@solution) %>% cost
c(GA@solution) %>% biodiv

rbinom(S, 1, 0.5) %>% fitness

Budget

fitness(GA@solution, Budget)

# return cheapest mix
cheapest = GA@population[which.min(ga.costs), ]
# return plant-rich mix
plantiest = GA@population[which.max(rowSums(GA@population)), ]
# return a mix different from either above
soldist = vegdist(GA@solution, diag = T, upper = T) %>% as.matrix 
maxdiff = rbind(
  -soldist[which.min(ga.costs), ],
  -soldist[which.max(rowSums(GA@solution)), ]) %>% 
  doNondominatedSorting() %$% which(ranks == 1)
wildcard = GA@population[maxdiff[1], ]

########### 

sum(wildcard)
sum(plantiest)
sum(cheapest)
