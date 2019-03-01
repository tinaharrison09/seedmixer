###################################################
### visualization functions
###################################################
Mon = c(8.5, 21.5, 35, 48); names(Mon) = c("Mar", "Jun", "Sep", "Dec")

### all plant phenology 
### make the plot a function of pool, mix
plot.pool.phen = function(pool = rep(1, S)) {
  
  s = sum(pool)
  
  plot(c(1,52), c(1, S), type = "n", xaxt="n", yaxt="n", xlab="", ylab="")
  sapply(1:s, function(p) {
    find = which(pool == 1)[p]
    bloom.week = c(1:52)[plant_phen[find, ] == 1]
  segments(min(bloom.week), find, max(bloom.week), find,
         lwd = 2, col="gray")})
mtext(names(Mon), at=Mon, side = 1, line=1, cex=1.2)
}

rbinom(S, 1, 0.2) %>% plot.pool.phen

cheapest %>% plot.pool.phen


### highlight mix
highlight.mix = function(mix) {
  sapply(which(mix == 1), function(p) {bloom.week = c(1:52)[plant_phen[p, ] == 1]
  segments(min(bloom.week), p, max(bloom.week), p,
           lwd = 2, col="black")})}
}
highlight.mix(cheapest)

?spline

abline(v=c(20, 30))

### (2) plant and bee diversity through time
temporal(rep(1, S)) %>% spline(1:52, .) %>%
  plot(type = "l", col="gray", xaxt="n", xlab="", ylab="bee species")
mtext(names(Mon), at=Mon, side = 1, line=1, cex=1.2)
### add mix
temporal.mix = function(mix) {
  temporal(mix) %>% spline(1:52, .) %>%
    points(type = "l", col="black")
}



temporal.mix(cheapest)
