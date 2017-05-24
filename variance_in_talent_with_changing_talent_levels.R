
# SIMULATE DATA TO VERIFY MATH IN HARDBALL TIMES ARTICLE


#############################################
# SIMULATE TALENT CHANGES AND OBSERVED DATA #
#############################################

n <- 100000		# number of players to simulate
PA <- 4			# number of PAs to simulate per day
d <- 180		# number of days to simulate
u <- .33		# average value of stat to simulate
sd <- .030		# standard deviation of true talent in the population
#sd.rnd <- .001	# sd of random talent changes (comment out to set r instead)
r <- .998		# correlation of talent from one day to the next

sd.rnd <- sqrt(sd^2/r^2 - sd^2)	# set sd of random talent changes to fit given values of r and sd


p.full <- matrix(data=rep(0,n*d),nrow=n)	# create matrix to store true talent probabilities
x.full <- p.full		# create matrix to store simulated results

p.full[,1] <- rnorm(n,u,sd)		# populate talent levels for first day of sim
x.full[,1] <- rbinom(n,PA,p.full[,1])		# simulate first day of results


# simulate random talent changes and results for each day up to day d
for (i in 2:d) {
	delta <- rnorm(n,0,sd.rnd)		# random changes for talent levels
	new <- p.full[,(i-1)] + delta
	# correct variance to offset additional variance from random talent changes
	factor <- sqrt(var(p.full[,(i-1)])/var(new))
	p.full[,i] <- factor*(new) + (1-factor)*mean(p.full[,(i-1)])
	x.full[,i] <- rbinom(n,PA,p.full[,i])	
}



#######################################
# CALCULATE CHANGE IN TALENT VARIANCE #
#######################################

var(apply(p.full,1,mean))	# variance of average talent for each player
sd^2						# variance of talent for single day
(d*(1 - r^2) - 2*r*(1-r^d))/(d^2*(1-r)^2)	# ratio of talent variance over d days to talent variance over one day

sd^2 * (d*(1 - r^2) - 2*r*(1-r^d))/(d^2*(1-r)^2)	# predicted variance over d days - compare to value "var(apply(p.full,1,mean))"



###########################################
# ESTIMATE r FROM SIMULATED OBSERVED DATA #
###########################################


######################
# CORRELATION METHOD #
######################

length <- 30	# number of days to include in each sample

# compare correlation of samples at varying intervals, derive value of r implied by the change in correlation
(
cor(rowSums(x.full[,1:length]),rowSums(x.full[,(2*length+1):(3*length)])) /
cor(rowSums(x.full[,1:length]),rowSums(x.full[,(length+1):(2*length)])) 
) ^ (1/length)

# second estimate using a different interval
(
cor(rowSums(x.full[,1:length]),rowSums(x.full[,(3*length+1):(4*length)])) /
cor(rowSums(x.full[,1:length]),rowSums(x.full[,(length+1):(2*length)])) 
) ^ (1/(2*length))


###################
# VARIANCE METHOD #
###################

length1 <- 30	# number of days in first sample
length2 <- 180	# number of days in second sample

var1 <- var(apply(x.full[,1:length1]/PA,1,mean))	#variance in observed data over length1 days
var1.rnd <- mean((x.full[,1:length1]/PA)) * (1 - mean((x.full[,1:length1]/PA))) / (PA*length1)	# random binomial variance = p*q/n
var1.true <- length1*PA/(length1*PA - 1) * (var1 - var1.rnd)

var2 <- var(apply(x.full[,1:length2]/PA,1,mean))	#variance in observed data over length2 days
var2.rnd <- mean((x.full[,1:length2]/PA)) * (1 - mean((x.full[,1:length2]/PA))) / (PA*length2)	# random binomial variance = p*q/n
var2.true <- length2*PA/(length2*PA - 1) * (var2 - var2.rnd)

var2.true / var1.true	# drop in variance from d=length1 to d=length2

# r <- 					# try new values of r to find fit for data (if no value is set here, just use value of r from sim)
ratio1 <- (length1*(1 - r^2) - 2*r*(1-r^length1))/(length1^2*(1-r)^2)
ratio2 <- (length2*(1 - r^2) - 2*r*(1-r^length2))/(length2^2*(1-r)^2)
ratio2/ratio1			# predicted drop in variance from d=length1 to d=length2 for stated value of r, compare to var2.true / var1.true (if desired, try different values of r until the values match to get the implied value of r)

