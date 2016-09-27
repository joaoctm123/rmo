pause <- function() {
  invisible(readline("\nPress <return> to continue: "))
}

## Optimization Problem: Bag Prices
## A detailed description of this problem can be found in: Paulo Cortez, Modern Optimization with R, Use R! series, Springer, September 2014.
## This problem it is about a company who produces woman bags, and pretends to get the best prices for each bag in order to get the maximum profit.
## The price of each bag is influenced by the price production of each bag, the number of expected sales and the marketing effort being applied.


## This is a maximization problem, because the objective of this problem is to maximize the profit of the company.
## Also this problem, it is a integer problem, which means that the solution always returns an integer value.
## The minimum possible price for each bag is $1 and the maximum possible price is $1000.
## So the lower is the minimum price and upper is the maximum price.
## The company wants to produce 5 different bags, so the Dimension (D) value is 5.
## lower: 1
## upper: 1000
## D: 5
## objective function: profit
## type: "max"



pause()
## Method: Full Blind Search
## This method works with a defined search space, which means that the optimum solution is always found.
## rmoptim('fbs',5,1,1000,'max',profit)
rmoptim("fbs",5,1,1000,"max",profit)


pause()
## Method: Grid Search
## The Grid Search method requires always the "step" parameter. This parameter it is the "jump" that the execution does, to find another solution.
## This parameter needs to be defined in the control list.
## This method takes some time to find de optimum solution.
## rmoptim('grs',5,1,1000,'max',profit,list(step=100))
rmoptim("grs",5,1,1000,"max",profit,list(step=100))


pause()
## Method: Particle Swarm Optimization
## To show the different functionalities of this package, the user can set a lower and upper value for each dimension.
## And the optimium solution is found respecting the lower and upper values.
## The rep function performs this: rep(1000,5) is the same as getting (1000,1000,1000,1000,1000)
## rmoptim('pso',5,rep(1,5),rep(1000,5),'max',profit)
rmoptim("pso",5,rep(1,5),rep(1000,5),"max",profit)


pause()
## Method: Simulated Annealing
## In this example, will be shown that is possible to change the behavior of the execution by adding some parameters of the native method.
## To know what parameters the user can add to the execution, is better to read the official documentation of the respective package.
## rmoptim('sann',5,1,1000,'max',profit,list(temp=100))
rmoptim("sann",5,1,1000,"max",profit,list(temp=100))


pause()
## Method: Hill Climb
## By default the maximum number of iterations of each method to find the optimum solution is 100. So in this case will be shown the execution with 200 iterations.
## rmoptim('hlc',5,1,1000,'max',profit,list(iter=200))
rmoptim("hlc",5,1,1000,"max",profit,list(iter=200))


pause()
## Method: Monte Carlo Search
## In this case will be shown the lower and upper being set by a vector.
## rmoptim('mcs',5,c(1,1,1,1,1),c(1000,1000,1000,1000,1000),'max',profit)
rmoptim("mcs",5,c(1,1,1,1,1),c(1000,1000,1000,1000,1000),"max",profit)


pause()
## Method: Differential Evolution
## For this execution will be set the maximum price for of $100 for the first bag.
## rmoptim('dfev',5,1,c(100,1000,1000,1000,1000),'max',profit)
rmoptim("dfev",5,1,c(100,1000,1000,1000,1000),"max",profit)


pause()
## Method: Genetic and Evolutionary Algorithms
## Because this is a integer problem, and the profit function always returns an integer value, there is no need to indicate that this is a integer problem.
## If the user wants to indicate that this is a integer problem, it is only necessary to add the parameter "numb" in the control list and set them to "int".
## rmoptim('gea',5,1,1000,'max',profit,list(numb="int"))
rmoptim("gea",5,1,1000,"max",profit,list(numb="int"))


pause()
## Method: Estimation of Distribution Algorithm
## rmoptim('eda',5,1,1000,'max',profit)
#rmoptim("eda",5,1,1000,"max",profit)


pause()
## Method: Tabu Search
## The method Tabu Search only works with one lower and one upper. That means if the user puts a vector as a parameter, the function only sets the lower and upper
## with maximum and minimum values.
## rmoptim('tabu',5,1,1000,'max',profit)
rmoptim("tabu",5,1,1000,"max",profit)


pause()
## Method: Artificial Bee Colony
## Other possibility is to set the initial values to search. This is possible with the "par" parameter.
## For this case will be set with the following result rep(NA,5)
## rmoptim('abc',5,1,1000,'max',profit,list(par=rep(NA,5)))
rmoptim("abc",5,1,1000,"max",profit,list(par=rep(NA,5)))


pause()
##
## Real Maximization Problem
## By default this package performs a integer optimization. But in this case because this is a real optimization problem.
## So there is mandatory to define the parameter "numb" as "real".
## lower: -20
## upper: 20
## D: 2


fun <- function(x) {
	-cos(x[1])*cos(x[2])*exp(-((x[1] - pi)^2 + (x[2] - pi)^2))
}

pause()
## Method: Grid Search
## rmoptim('grs',2,-20,20,'min',fun,list(numb='real',step=2))
rmoptim("grs",2,-20,20,"min",fun,list(numb="real",step=2))


pause()
## Method: Particle Swarm Optimization
## rmoptim('pso',2,-20,20,'min',fun,list(numb='real'))
rmoptim("pso",2,-20,20,"min",fun,list(numb="real"))


pause()
## Method: Simulated Annealing
## rmoptim('sann',5,1,1000,'max',profit,)
rmoptim("sann",2,-20,20,"min",fun,list(numb="real"))


pause()
## Method: Hill Climb
## rmoptim('hlc',2,-20,20,'min',fun,list(numb='real'))
rmoptim("hlc",2,-20,20,"min",fun,list(numb="real"))


pause()
## Method: Monte Carlo Search
## rmoptim('mcs',2,-20,20,'min',fun,list(numb='real'))
rmoptim("mcs",2,-20,20,"min",fun,list(numb="real"))


pause()
## Method: Differential Evolution
## rmoptim('dfev',2,-20,20,'min',fun,list(numb='real'))
rmoptim("dfev",2,-20,20,"min",fun,list(numb="real"))


pause()
## Method: Genetic and Evolutionary Algorithms
## rmoptim('gea',2,-20,20,'min',fun,list(numb='real'))
rmoptim("gea",2,-20,20,"min",fun,list(numb="real"))


pause()
## Method: Estimation of Distributioon Algorithm
## rmoptim('eda',2,-20,20,'min',fun,list(numb='real'))
#rmoptim("eda",2,-20,20,"min",fun,list(numb="real"))


pause()
## Method: Tabu Search
## rmoptim('tabu',2,-20,20,'min',fun,list(numb='real'))
rmoptim("tabu",2,-20,20,"min",fun,list(numb="real"))


pause()
## Method: Artificial Bee Colony
## rmoptim('abc',2,-20,20,'min',fun,list(numb='real'))
rmoptim("abc",2,-20,20,"min",fun,list(numb="real"))


pause()
## Binary Maximization Problem
## This method performs the sum of all the dimension values.
## Only with two iterations can perform the sum of all values.
## For example, the first solution is c(0,1,0,0,1), so the sum will be 2. After that will be done one more iteration, and the best solution will be found.
## lower: 0
## upper: 1
## D: 5
sumbin <- function(x) sum(x)


pause()
## Method: Full Blind Search
## rmoptim('fbs',5,0,1,'max',sumbin,list(numb='bin',iter=2))
rmoptim("fbs",5,0,1,"max",sumbin,list(numb="bin",iter=2))


pause()
## Method: Particle Swarm Optimization
## rmoptim('pso',5,0,1,'max',sumbin,list(numb='bin',iter=2))
rmoptim("pso",5,0,1,"max",sumbin,list(numb="bin",iter=2))


pause()
## Method: Simulated Annealing
## rmoptim('sann',5,1,1000,'max',profit,list(numb="bin",iter=2))
rmoptim("sann",5,0,1,"max",sumbin,list(numb="bin",iter=2))


pause()
## Method: Hill Climb
## rmoptim('hlc',5,0,1,'max',sumbin,list(numb='bin',iter=2))
rmoptim("hlc",5,0,1,"max",sumbin,list(numb="bin",iter=2))


pause()
## Method: Monte Carlo Search
## rmoptim('mcs',5,0,1,'max',sumbin,list(numb='bin',iter=2))
rmoptim("mcs",5,0,1,"max",sumbin,list(numb="bin",iter=2))


pause()
## Method: Differential Evolution
## rmoptim('dfev',5,0,1,'max',sumbin,list(numb='bin',iter=2))
rmoptim("dfev",5,0,1,"max",sumbin,list(iter=2))


pause()
## Method: Genetic and Evolutionary Algorithms
## rmoptim('gea',5,0,1,'max',sumbin,list(numb='bin',iter=2))
rmoptim("gea",5,0,1,"max",sumbin,list(numb="bin",iter=2))


pause()
## Method: Estimation of Distributioon Algorithm
## rmoptim('eda',5,0,1,'max',sumbin,list(numb='bin',iter=2))
#rmoptim("eda",5,0,1,"max",sumbin,list(numb="bin"))


pause()
## Method: Tabu Search
## rmoptim('tabu',3,0,1,'max',sumbin,list(numb='bin',iter=2))
rmoptim("tabu",5,0,1,"max",sumbin,list(numb="bin",iter=2))


pause()
## Method: Artificial Bee Colony
## rmoptim('abc',5,0,1,'max',sumbin,list(numb='bin',iter=2))
rmoptim("abc",5,0,1,"max",sumbin,list(numb="bin",iter=2))
