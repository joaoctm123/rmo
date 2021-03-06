% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{rmoptim}
\alias{rmoptim}
\title{R Modern Optimization function}
\usage{
rmoptim(method, D, lb, ub, type, eval, control = list())
}
\arguments{
\item{method}{- type of optimization method (search engine)
\itemize{
\item \code{"fbs"} - Full Blind Search - searches all the possible alternatives. It only works for binary and integer optimization problems.
\item \code{"grs"} - Grid Search - uses a hyper dimensional grid search that is dependent on a step size. It only works on integer and real optimization tasks.
\item \code{"mcs"} - Monte Carlo Search - random search according to a given probability distribution.
\item \code{"hlc"} - Hill Climb  - local search method.
\item \code{"sann"} - Simulated Annealing - uses \code{\link{optim}} function \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html}.
\item \code{"tabu"} - Tabu Search - uses \code{\link[tabuSearch]{tabuSearch}}
\item \code{"gea"} - Genetic and Evolutionary Algorithms - population based method that
uses \code{\link[genalg]{rbga}} (or \code{\link[genalg]{rbga.bin}} for binary tasks).
\item \code{"dfev"} - Differential Evolution - uses \code{\link[DEoptim]{DEoptim}}.
\item \code{"pso"} - Particle Swarm Optimization - uses \code{\link[pso]{psoptim}}.
\item \code{"eda"} - Estimation of Distribution Algorithm - uses \code{\link[copulaedas]{edaRun}}.
\item \code{"abc"} - Artificial Bee Colony - uses \code{\link[ABCoptim]{abc_optim}}.
}}
\item{D}{- Dimension, the number of elements, variables or parameters to be optimized}

\item{lb,ub}{- two values or two vectors setting the real lower and upper bounds for each parameter
                  to be optimized (used for integer or real value optimization).}

\item{type}{- type of optimization goal - \code{"max"} for maximization or \code{"min"} for minimization.}

\item{eval}{- Evaluation function to be minimized or maximized. This function needs to return a numeric value.}

\item{control}{- A list of control parameters. See "Details"}
}
\value{
The output of the function \code{rmoptim} is a list containing the optimized parameters \code{"$sol"} and the optimum searched value \code{"$fun"}.
}
\description{
A vast number of real-world tasks can be viewed as an optimization problem, where the goal is to minimize or maximize a given goal. This package encompasses in a single and simply to use function (rmoptim) several modern optimization algorithms (metaheuristics) that are spread across distinct packages. Examples of implemented methods are: grid search, hill climbing, tabu search, simulated annealing, differential evolution, estimation of distribution algorithms, genetic and evolutionary algorithms, particle swarm optimization and artificial bee colony.
}
\details{
The control argument is a list that can supply any of the following components:
\itemize{
\item \strong{iter} - The maximum number of iterations.
\item \strong{step} - Mandatory control parameter to the use of Grid Search (see examples).
  \item \strong{numb} - Type of data representation of the final solution. \code{bin} for binary solutions and automatically sets lower and upper bounds to 0 and 1 respectively. \code{real} for real number solutions or \code{int} for integer solutions. By default sets \code{real} for optimization problems.
 \item \strong{...} - Other parameters. For example, in the \code{"SANN"} method of the \code{\link{optim}} function the default
value for \code{"temp"} is 10. But if the user wants to use a different value, then she/he just needs to insert the parameter \code{"temp"} in the control list.}
}
\examples{
  ## Demo function that executes three types of optimization. The objective of this Demo function
  ## is to help the user to understand the parameters and the operation of the package.
  ## The first problem being presented is an integer optimization problem. This problem can
  ## be found on the book Modern Optimization with R. The objective of the problem is to find
  ## the best price for five different bags that a company produces. The objective of this
  ## function is to maximize the profit.
  ## The second problem, is a real number optimization problem which is executed a minimization
  ## function only with two dimensions.
  ## Finally the last one, is a binary optimization problem, only with two iterations.
  ## The objective function performs the summation of all dimensions. Per example, if the
  ## dimensions are (0,1,1,0,0,1,1,0), the sum is 4.
  \dontrun{

    demo(rmo)
  }

  ## More examples:

  ## Real Number Optimization Problem
  ## Objective Function:

  fun <- function(x) {
	-cos(x[1])*cos(x[2])*exp(-((x[1] - pi)^2 + (x[2] - pi)^2))
  }

  ## Minimization Function
  ## Dimension: 2
  ## Lower: -20
  ## Upper: 20


  ## using the Particle Swarm Optimization method
  rmoptim("pso",2,-20,20,"min",fun,list(numb="real",restart=2))

  ## Also in this case can be percetible the use of the restart variable in the control list
  ## which makes two restarts in the search process.
  ## The user can set some parametrs in the control list, parameters from the official documentation
  ## of the PSO package.

  ## Output:
  ## Particle Swarm Optimization
  ## Optimum Solution:  3.141602 3.141586  -  1

  ## Now the same method but with reporting
  ## To set the reporting thats necessary to have the trace value set as TRUE
  ## Then the package PSO, reports in 10 iterations.
  rmoptim("pso",2,-20,20,"min",fun,list(numb="real",restart=2,REPORT=10,trace=TRUE))

  ## Output:
  ## Particle Swarm Optimization
  ## S=12, K=3, p=0.2297, w0=0.7213, w1=0.7213, c.p=1.193, c.g=1.193
  ## v.max=NA, d=56.57, vectorize=FALSE, hybrid=off
  ## It 10: fitness=-0.5297
  ## It 20: fitness=-0.9478
  ## It 30: fitness=-0.9968
  ## It 40: fitness=-0.9998
  ## It 50: fitness=-0.9999
  ## It 60: fitness=-1
  ## It 70: fitness=-1
  ## It 80: fitness=-1
  ## It 90: fitness=-1
  ## It 100: fitness=-1
  ## Maximal number of iterations reached
  ## Optimum Solution:  3.141594 3.141592  -  1

  #using the Differential Evolution method
  rmoptim("dfev",2,-20,20,"min",fun,list(numb="real"))

  ## Output:
  ## Differential Evolution
  ## Optimum Solution:  3.141593 3.141593  -  1

  ## By default the package rmo, has in the control list a parameter to set the
  ## maximum number of iterations. By default: iter=100.
  ## In the DEoptim package who runs the Differential Evolution Method
  ## has the parameter itermax. By default itermax=200.
  rmoptim("dfev",2,-20,20,"min",fun,list(numb="real",iter=200))

  ## Now a example with the itermax parameter
  rmoptim("dfev",2,-20,20,"min",fun,list(numb="real",itermax=200))

  ## With this, is pretended to show the way that can be manipulated



  rmoptim("abc",2,-20,20,"min",fun,list(numb="int",maxCycle=100))

  ## The same optimization problem was used with Artificial Bee Colony, but in this case
  ## the final representation of the solution is with integer values.
  ## Also it is set the maxCycle in control list, with the value of 100.
  ## This means that the maximum number of iterations is 100. The user instead
  ## could set the value iter, which both does the same.

  ## Output:
  ## Artificial Bee Colony
  ## Optimum Solution:  3 3 -  1


  ####################################################################################
  ## Now will be represented a binary optimization problem
  sumbin <- function(x) sum(x)


  ## This function represents the sum of the binary values
  ## Example x=c(0,1,0,1,1,0,0,0)
  ## sum(x) = 3

  ## For this example, will run only with two iterations, because if it is set more iterations
  ## Will be always found the solution c(1,1,1,1,1,1,1,1)

  ## First it's going to be tested the Full Blind Search method.
  ## Because the search space is defined on the start and
  ## all the possible solutions are tested

  rmoptim("fbs",8,0,1,"max",sumbin,list(numb="bin"))

  ## Output:
  ## Full Blind Search
  ## Optimum Solution:  1 1 1 1 1 1 1 1  -  8


  ## This example will show, the method running with the number of iterations in default
  rmoptim("pso",8,0,1,"max",sumbin,list(numb="bin"))

  ## Output:
  ## Particle Swarm Optimization
  ## Optimum Solution:  1 1 1 1 1 1 1 1  -  8

  ## But if the number of iterations sets to 2 iterations
  ## The final value is going to be different

  rmoptim("pso",8,0,1,"max",sumbin,list(numb="bin",iter=2))

  ## Output:
  ## Particle Swarm Optimization
  ## Optimum Solution:  1 0 0 1 1 1 1 1  -  6

  ## The execution is the same for every method.
  rmoptim("hlc",8,0,1,"max",sumbin,list(numb="bin",iter=2))
  ## Hill Climbing
  ## Optimum Solution:  0 0 1 1 1 1 1 1  -  6

  ## For this execution will be shown the execution of Tabu Search method
  ## This method works only with binary values, which makes it the best
  ## for this type of tasks.

  rmoptim("tabu",8,0,1,"max",sumbin,list(numb="bin",iter=2))
  ## Tabu Search
  ## Optimum Solution:  1 0 0 1 1 0 0 1  -  4
}
\references{
\itemize{
 \item - To check from where was inspired this package and to find some Optimization Problems:
 Paulo Cortez, Modern Optimization with R, Use R! series, Springer, September 2014.
 \item - ABCoptim: George, A., Yon, V., & George, M. (2015). Package "ABCoptim" Title Implementation of Artificial Bee Colony (ABC) Optimization. Retrieved from http://github.com/gvegayon/ABCoptim,
 \item - DEoptim: Ardia, D., Mullen, K. M., & Peterson, B. G. (n.d.). DEoptim : An R Package for Global Optimization by Differential Evolution, (2006).
 \item - TabuSearch: Katarina, D. (2015). Package tabuSearch (c)
 \item - Genalg: Willighagen, E., Maintainer, M. B., & Ballings, M. (2015). Package "genalg" Title R Based Genetic Algorithm. Retrieved from https://github.com/egonw/genalg
 \item - PSO: Bendtsen, C. (2015). Package "pso" Title Particle Swarm Optimization.
 \item - EDA: Gonzalez-Fernandez, Y., Soto, M., & Maintainer, ]. (2015). Title Estimation of Distribution Algorithms Based on Copulas. Retrieved from https://github.com/yasserglez/copulaedas
}
}

