## RMO - R Modern Optimization Package

## Description

A vast number of real-world tasks can be viewed as an optimization problem, where the goal is to minimize or maximize a given goal. This package encompasses in a single and simply to use function (rmoptim) several modern optimization algorithms (metaheuristics) that are spread across distinct packages. Examples of implemented methods are: grid search, hill climbing, tabu search, simulated annealing, differential evolution, estimation of distribution algorithms, genetic and evolutionary algorithms, particle swarm optimization and artificial bee colony.



## Installation

To make the installation of this package through GitHub it is necessary to follow theese steps:

```r
install.packages("devtools")
devtools::install_github("joaoctm123/rmo")
library(rmo)
```

## Usage

```r
rmoptim(method, D, lb, ub, type, eval, control = list())
```

method:
  - "fbs" - Full Blind Search
  - "grs" - Grid search
  - "hlc" - Hill Climb
  - "msc" - Monte Carlo Search
  - "tabu" - Tabu Search
  - "sann" - Simulated Annealing
  - "dfev" - Differential Evolution
  - "eda" - Estimation of Distibution Algorithm
  - "gea" - Genetic and Evolutionary Algorithms
  - "pso" - Particle Swarm Optimization
  - "abc" - Artificial Bee Colony
  
D: Dimension, the number of elements, variables or parameters to be optimized.

lb: Lower bounds.

ub: Upper bounds.

type: type of optimization goal - max for maximization or min for minimization.

eval: Evaluation function to be minimized or maximized. This function needs to return a numeric value.

control: list with extra parameters to change the execution behavior

## Values

The output of the function rmoptim is a list containing the optimized parameters "$sol" and the
optimum value of the function "$fun".


## Examples

To run some examples, type the following code:

```r
library(rmo)
demo("rmo")
```

## Feedback

We would like to know your opinion about the package by filling this 
short form: https://goo.gl/forms/4GirrUm8rNHGWzis1


## Contact 

Author: João Carlos Torres Arantes Maia

With supervision of: Paulo Cortez

Email: joao.arantes.maia@gmail.com
