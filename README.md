## RMO - R Modern Optimization Package

## Description

Today, a great number of tasks can be viewed as an optimization problem, where the goal is to minimize of maximize a given goal. Otimization problems exists in many areas such as Agriculture, Banking, Engineering, Finance and many more. With the optimization problems getting complex and computers processing getting better by the time, the use of computers to solve optimization problems was more frequent. For that were created many algorithms to solve the optimization task. The propose of this package is to make a junction of the many optimization algorithms that exists to the R tool. With this package a user can perform optimization tasks in a easier way, than using a single package with a single optimization method.

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
## Contact 

Author: João Carlos Torres Arantes Maia

With supervision of: Paulo Alexandre Ribeiro Cortez

Email: joao.arantes.maia@gmail.com
