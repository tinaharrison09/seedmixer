require(plyr)
require(magrittr)
require(stringr)

setwd("C:/Users/ceratina/Desktop/webtool2/")
list.files()


###################################################
### get data objects needed by rest of the code
###################################################

# plant_trait is a data frame of plant traits, names, Ola's coefs, crop pollintor, etc.
# plant_phen is matrix of plant bloom phenology (sticks)
# plant_cost is a vector of cost per seed from plant_traits

# bee_phen is a matrix of 126 bee flight seasons (sticks)
# bee_plant is a matrix of (non-phenological) interaction rules
# NOTE currently based on 3000+ bee specimens from bee bio

### GET PLANT DATA
plant_trait = read.csv("data/bee_bio_plants.csv") %>%
			subset(is.na(lower) == F)
plant_trait = plant_trait[-42,]

plant_phen = lapply(1:nrow(plant_trait), function(x) {
				cal = rep(0, 365)
				cal[plant_trait$lower[x]:plant_trait$upper[x]] = 1
				cal}) %>% do.call(rbind, .) 
# aggregate plant data by week
week = rep(1:52, each = 7) %>% c(52)
temp = apply(plant_phen, 1, function(x) tapply(x, week, sum))
plant_phen = ifelse(temp>3, 1, 0) %>% t
# plant_phen %>% t %>% image

### plant cost
plant_cost = plant_trait$price_kw14
plant_cost[is.na(plant_cost)] = 70 # FLAG

### GET BEE DATA
bee_trait = read.csv("data/bee_traits.csv", row.names = 1) %>%
			subset(n.obs > 4)
bee_phen = lapply(1:nrow(bee_trait), function(x) {
				cal = rep(0, 365)
				cal[bee_trait$min.day[x]:bee_trait$max.day[x]] = 1
				cal}) %>% do.call(rbind, .) 
temp = apply(bee_phen, 1, function(x) tapply(x, week, sum))
bee_phen = ifelse(temp>3, 1, 0) %>% t


# bee-plant interaction matrix
# ALL ONES FOR NOW - PHENOLOGY IS SOLE PREDICTOR OF INTERACTIONS
# bee_plant = matrix(1, nrow = nrow(bee_trait), ncol = nrow(plant_trait))
	int = read.csv("data/bee_bio_interactions.csv", row.names = 1)
# bee bio interactions
bee_plant = int[match(bee_trait$bee, rownames(int)), match(plant_trait$gen_sp, gsub("_", " ", colnames(int)))] %>% .[!is.na(.[,1]), ]
# TRIM bee data to interaction data
bee_trait = bee_trait[match(rownames(bee_plant), bee_trait$bee), ]
bee_phen = bee_phen[match(rownames(bee_plant), bee_trait$bee), ]
bee_trait = bee_trait[match(rownames(bee_plant), bee_trait$bee), ]


	nrow(bee_phen) == nrow(bee_plant)
	nrow(plant_phen) == ncol(bee_plant)
	nrow(plant_trait) == nrow(plant_phen)
	nrow(bee_trait) == nrow(bee_phen)

	bee_plant = as.matrix(bee_plant)

	S = nrow(plant_trait)
	
###################################################
### objective and cost functions
###################################################

############
### supported bee biodiversity
############

biodiv = function(x, B = bee_phen, I = bee_plant, P = plant_phen) {
	
#	if(sum(pool) < S) {I = I[, pool]; P = P[pool, ]} 

b.hat = (I %*% (P * x)) * B
b.j = rowSums(ifelse(b.hat > 0, 1, 0)*B)/rowSums(B)

	# RETURN number of bee species with adequate coverage
	sum(b.j > 0.9)
}



############
### crop pollination
############

pollen = function(x, crop = "sunflower", B = bee_phen, I = bee_plant, P = plant_phen, Tr = bee_trait, pool = rep(1,S)) {

	if(sum(pool) < S) {I = I[, pool]; P = P[pool, ]} 

b.hat = (I %*% (P * x)) * B
b.j = rowSums(ifelse(b.hat > 0, 1, 0)*B)/rowSums(B)

	sum(b.j[Tr[, crop] == 1] > 0.9)
} 


############
### cost function
############
### note that this function is non-linear for x if s is not fixed
cost.s = function(x, A = 10, C = plant_cost, s = 10) {	
				A*sum(C*x)/s}

	cost = function(x, A = 10, C = plant_cost) {	
	     # if(sum(pool) < S) {C = plant_cost[pool]}
				A*sum(C*x)/sum(x)}

	
	cost = function(x, C = plant_cost) {	
	  # if(sum(pool) < S) {C = plant_cost[pool]}
	  sum(C*x)}
	
############
### temporal bee function
### returns vector
############

temporal = function(x, B = bee_phen, I = bee_plant, P = plant_phen, T = bee_trait, pool = rep(1,S)) {
  if(sum(pool) < S) {I = I[, pool]; P = P[pool, ]} 
  
		b.hat = (I %*% (P * x)) * B
		b.j = rowSums(ifelse(b.hat > 0, 1, 0)*B)/rowSums(B)
	colSums(B[b.j > 0.9,])
}



############
### herbivore abundance
### dilution model - nonlinear
############

herbivore = function(x, F = plant_trait, pool = rep(1, S), A = 10) {

  	if(sum(pool) < S) {F = F[pool, ]} 
	
	A*sum(F$Herbivores*x)/sum(x)		

}

biodiv(rbinom(S, 1, 0.2), pool = rep(1,S))

S = nrow(plant_phen)
R = nrow(bee_phen)


rbinom(S, 1, 0.2) %>% biodiv
rbinom(S, 1, 0.2) %>% pollen(crop = "tomato")
rbinom(S, 1, 0.2) %>% cost
rbinom(S, 1, 0.2) %>% temporal
rbinom(S, 1, 0.1) %>% herbivore




