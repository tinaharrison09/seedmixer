'''
linear functions: biodiv, cost.s, crop pollinator, herbivore.s
non-linear functions: cost, herbivore.
'''
require(scales)
require(GA)
require(RColorBrewer)

good.blue = "#8DD3C7"
marker = list(color = brewer.pal(3, "Set3"))$color

              
# require(ecr)
# require(vegan)

diag(Sp) %>% apply(1, herbivore) %>% max
mono.cost = diag(Sp) %>% apply(1, cost) 
mono.bees = diag(Sp) %>% apply(1, biodiv)

max.bees = biodiv(rep(1,Sp))
max.cost = rep(1, Sp) %>% cost

randmix = sapply(1:1000, function(x) rbinom(Sp, 1, runif(1)))
dim(randmix)

rand.bees = randmix %>% apply(2, function(x) biodiv(x))
rand.cost = randmix %>% apply(2, function(x) cost(x))

rand = data.frame(findme = 1:length(rand.cost), cost = rand.cost, bees = rand.bees)

Budget = 200

##### GA fitness function
fitness = function(x, B = Budget) {
    elastic = 1/(cost(x) - B)^2
  biodiv(x)*ifelse(cost(x) - B <= 1, 1, elastic)}
# what if you used a more elastic threshold? so the *best* mix is under budget
# but that way the GA can learn from mixes that happen to be a bit over

Suggest = randmix[, which(rand.cost < Budget)[1:min(20, nrow(randmix))]] %>% t
# modify GA monitor to return something I want

barMonitor <- function (object, ...) 
{
  best = na.exclude(object@fitness) %>% max %>% format(digits = 0)
  incProgress(1/Runs, detail = paste("Try no.", object@iter, "| Best =", best))
}


### memoization to speed up calculations
# require(memoise)
# mfitness <- memoise(fitness)
# start.time <- Sys.time()

GA <- ga(type = "binary", # my decision variables are binary
         fitness <- fitness, # any function receiving a potential solution and returning a single "fitness" score to max/min
         # min/max not relevant for binary decision variables
         nBits = Sp,
         popSize = 200,
         pcrossover = 0.5,
         pmutation = 0.2,
         # elitism is the number of best individuals to survive
         elitism = base::max(1, round(200*0.05)),
         maxiter = 500, # max iteration before GA stops
         run = 500, # no. consecutive gens before GA stops
         maxFitness = max.bees,
         names = NULL,
         suggestions = Suggest,
         optim = FALSE,
         # optimArgs = long blah,
         keepBest = F, # I want the evolved pop, not best of early runs
         parallel = FALSE,
         # monitor = barMonitor,
         seed = NULL # 
)
# end.time <- Sys.time()
# end.time - start.time
# forget(mfitness)

### organize objective and decision-space output into two table with a shared index
### so users can select mixes with a click-and-drag
### no point discriminating between random and better where they overlap
### but I don't want to bother with the code right now so wev.

### objective space
obj = data.frame(
is.opt = GA@fitness == GA@fitnessValue,
bees = GA@population %>% apply(1, biodiv),
cost = GA@population %>% apply(1, cost),
herb = GA@population %>% apply(1, herbivore),
pollen = GA@population %>% apply(1, pollen, "sunflower"),
plants = rowSums(GA@population)
)


### plotting objective space output
#### plot: random, other, and optimals
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

### Reactive - add line from input
abline(v=Budget, lwd=2)

### Reactive - add points from GA output
subset(obj, !is.opt) %>% with(points(cost, bees, cex = log(plants, 10)+0.5, pch=19, col=alpha(good.blue, 0.8)))
subset(obj, is.opt) %>% with(points(cost, bees, cex = log(plants, 10)+0.5, pch=19, col="tomato"))

### Reactive - allow user to select subset of points

### how variable are best mixes?
### check v. max-mins
# max = c(1, Sb, max.cost, max(Tp$Herbivores), sum(Tb$sunflower), Sp)
# min = apply(obj, 2, min)
obj2 = obj[order(obj$bees), ]
par(mfrow=c(2,3), mar=c(1,2,2,1))
sapply(1:6, function(i) {plot(as.numeric(obj2[,i]), 
                             main = colnames(obj)[i], 
                             xlab = "", ylab="", xaxt = "n",
                             pch=19, col=c(good.blue, "tomato")[as.numeric(obj2$is.opt)+1])
            if(i==3) abline(h=Budget, lwd=2)
  })

############################################################################
### interesting analysis of decision space that won't make it into the tool:
### are the mixes very different?
### not really between best and better
require(vegan)
m = metaMDS(GA@population)
### but very much between better and random
full = rbind(GA@population, t(randmix)[colSums(randmix)>10, ][1:200,])
mf = metaMDS(full)

par(mfrow=c(1,2))
plot(scores(m), pch=19, cex=1.1, col=ifelse(obj$is.opt, "tomato", good.blue))
plot(scores(mf), pch=19, cex=log(rowSums(full), 10), 
       col= c(ifelse(obj$is.opt, "tomato", good.blue), rep("lightgray", 200)))
############################################################################

GA@solution %>% colSums
GA@solution %>% dim

# are there plants that are ALWAYS selected into best mix
# and plants that are NEVER selected into best mix
# yes usually it seems.



### DECISION SPACE OUTPUT
# return cheapest mix
# cheapest = GA@population[which.min(opt.costs), ]
# # return plant-rich mix
# plantiest = GA@population[which.max(rowSums(GA@population)), ]
# # return a mix different from either above
# soldist = vegdist(GA@solution, diag = T, upper = T) %>% as.matrix 
# maxdiff = rbind(
#   -soldist[which.min(ga.costs), ],
#   -soldist[which.max(rowSums(GA@solution)), ]) %>% 
#   doNondominatedSorting() %$% which(ranks == 1)
# wildcard = GA@population[maxdiff[1], ]

# sum(wildcard)
# sum(plantiest)
# sum(cheapest)

########### 
tidy_plant = subset(plant_trait, select = c("Common.name", "gen_sp", "family", "A.P", "peak"))

###################################################
### visualization functions
###################################################
Mon = c(8.5, 21.5, 35, 48); names(Mon) = c("Mar", "Jun", "Sep", "Dec")


plot(c(1,52), c(1, Sp), type = "n", xaxt="n", yaxt="n", xlab="", ylab="")
sapply(1:Sp, function(i) {
  bloom.week = c(1:52)[Pp[i,] == 1]
  segments(min(bloom.week), i, max(bloom.week), i,
           lwd = 3, col="gray")})
mtext(names(Mon), at=Mon, side = 1, line=1, cex=1.2)


mix = GA@solution[1,]

### highlight a mix
highlight.mix = function(mix) {
  
  sapply(1:Sp, function(i) {
    bloom.week = c(1:52)[Pp[i, ] == 1]
    segments(min(bloom.week), i, max(bloom.week), i,
             lwd = 3, col=c("gray", "tomato")[mix[i]+1])})
}

highlight.mix(rbinom(Sp, 1, 0.3))

abline(v=c(20, 30))



###################################################
### (2) plant and bee diversity through time
temporal(rep(1, Sp)) %>% spline(1:52, .) %>%
  plot(type = "l", col="gray", xaxt="n", xlab="", ylab="bee species", lwd=3, lty=3)
mtext(names(Mon), at=Mon, side = 1, line=1, cex=1.2)
### add mix
temporal.mix = function(mix, Col=good.blue) {
  temporal(mix) %>% spline(1:52, .) %>%
    points(type = "l", col=Col, lwd = 3)
}

temporal.mix(mix, "tomato")
temporal.mix(rbinom(Sp, 1, 0.3), "gray")



###################################################
### (2) plant and bee diversity through time
temporal(rep(1, Sp)) %>% spline(1:52, .) %>%
  plot(type = "l", col="gray", xaxt="n", xlab="", ylab="bee species", lwd=3, lty=3)
mtext(names(Mon), at=Mon, side = 1, line=1, cex=1.2)
### add mix
temporal.mix = function(mix, Col=good.blue) {
  temporal(mix) %>% spline(1:52, .) %>%
    points(type = "l", col=Col, lwd = 3)
}

temporal.mix(mix, "tomato")
temporal.mix(rbinom(Sp, 1, 0.3), "gray")

