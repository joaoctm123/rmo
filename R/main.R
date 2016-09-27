library(tabuSearch)
library(ABCoptim)
library(genalg)
library(DEoptim)
library(copulaedas)
library(pso)


#Control List 	- iter - value ex:iter=100
#				- numb - bin/real/int
#				- step - value ex: step=1000 - only for the grid search method
#				- par - starting search value
rmoptim<-function(method,D,lb,ub,type,eval,control = list()){

	#Global vars to return the values
	res=list(sol=0, fun=0)

	if(!typeof(method)=="character" || method==""){
		stop("The method must be a string")
	}
	if(!D%%1==0){
		stop("Dimenson value must be an integer value")
	}
	if(!class(lb)=="numeric" || !class(ub)=="numeric"){
		stop("Lower and Upper bounds must be an numeric value")
	}
	if(!typeof(type)=="character" || (!type=="max" && !type=="min" && !type=="MAX" && !type=="MIN") || type==""){
		stop("The type of the evaluation function variable must be a string \n Ex: 'max' or 'min'")
	}
	if(!typeof(control)=="list" && !is.null(control)){
		stop("The type of control parameter must be a list")
	}
	#if(!control$iter%%1==0 && !is.null(control) && !is.null(control$iter)){
	#	stop("Number of iterations must be an integer value")
	#}
	if(!typeof(control$numb)=="character" && !is.null(control) && !is.null(control$numb)){
		stop("The type of data representation must be a string value\n  Ex: 'real' or 'bin' or 'int'")
	}
	if(!typeof(control$trace)=="logical" && !is.null(control) && !is.null(control$trace)){
		stop("Trace parameter must be type logical")
	}
	options(warn=-1)

	#If to know what's the length of var lower or upper. This helps to know how to manipulate the lower bounds
	lower=0
	upper=0

	if(method=="tabu" || method=="fbs"){

		lower=min(lb)
		upper=max(ub)


	}else {

		if(length(lb)==1){

		  lower=rep(lb,D)
		} else {
		  lower=lb
		}
		if(length(ub)==1){
			upper=rep(ub,D)
		} else {
		  upper=ub
		}
	}

	numb=""
	if(is.null(control)){
		numb="int"
	} else if(is.null(control$numb)){
		numb="int"
	} else if(!is.null(control$numb)){
		numb=control$numb
		if(control$numb=="bin"){
			upper=rep(1,D)
			lower=rep(0,D)
		}
	}

	min=min(lower) #get the maximum and minimum values for the sample and change function
	max=max(upper)



  ####################################################################################################################################
  ##################                                FULL BLIND SEARCH                             ####################################
	####################################################################################################################################


	if(method=="fbs"){

		cat("Full Blind Search\n")
	  if(numb=="real"){
	    stop("You cannot run the Full Blind Search method in Real optimization problems because of his search space it too big.")
	  }

		isearch=function(D){
			x=lower:upper;
			search=matrix(ncol=D,nrow=length(x))
			search[]=1; search[,D]=x
			S1=fsearch(search,eval,type)
			S1$sol[D];
		}

		S=sapply(1:D,isearch)


		#in case the type of number is bin or integer, the number must be round

		res$sol=S
		res$fun=eval(S)
		cat("Optimum Solution: ",round(S)," - ",round(eval(S)),"\n")

	}

	####################################################################################################################################
	##################                                GRID SEARCH                                   ####################################
	####################################################################################################################################

	if(method=="grs"){
		cat("Grid Search \n")
		step=0

		if(numb=="bin"){ #verification to know if it is a binary optimization problem
		  stop("You cannot run the Grid Search method in Binary optimization problems.")
		}
		if((lower==0 && upper==1) || (lower==rep(0,D) && upper==rep(1,D))){
		  stop("You cannot run the Grid Search method in Binary optimization problems.")
		}
		if(is.null(control)){
		  stop("The step parameter must be on control list")
		} else if(is.null(control$step)){
		  stop("The step parameter must be on control list")
		} else {
		  step=control$step
		}

		S1=gsearch(rep(step,D),lower,upper,eval,type)
		S2=gsearch2(rep(step,D),lower,upper,eval,type)

		if(numb=="real"){

			res$sol=S1$sol
			res$fun=S1$eval
			res$sol1=S2$sol
			res$fun1=S2$eval
			cat("Optimum Solution of gsearch method: ",S1$sol," - ",S1$eval,"\n")
			cat("Optimum Solution of gsearch2 method: ",S2$sol," - ",S2$eval,"\n")

		} else{

			#in case the type of number is bin or integer, the number must be round
			res$sol=S1$sol
			res$fun=S1$eval
			res$sol1=round(S2$sol)
			res$fun1=round(S2$eval)
			cat("Optimum Solution of gsearch method: ",round(S1$sol)," - ",round(S1$eval),"\n")
			cat("Optimum Solution of gsearch2 method: ",round(S2$sol)," - ",round(S2$eval),"\n")

		}


	}
	####################################################################################################################################
	##################                                Monte Carlo SEARCH                             ###################################
	####################################################################################################################################

	if(method=="mcs"){

		#if to get the number of iterations
		iter=0
		if(is.null(control)){
			iter=100
		} else if(is.null(control$iter)){
			iter=100
		} else {
		  iter=control$iter
		}
		cat("Monte Carlo Search\n")
		S=mcsearch(iter,lower,upper,eval,type)

		if(numb=="real"){

			cat("Optimum Solution: ", S$sol, " - ", S$eval, "\n")
			res$sol=S$sol
			res$fun=S$eval

		} else{

			#in case the type of number is bin or integer, the number must be round
			cat("Optimum Solution: ", round(S$sol), " - ", round(S$eval), "\n")
			res$sol=round(S$sol)
			res$fun=round(S$eval)
		}


	}

	####################################################################################################################################
	##################                                Hill Climb                                    ####################################
	####################################################################################################################################

	if(method=="hlc"){

		cat("Hill Climbing\n")
		#if to get the number of iterations
		iter=0
		if(is.null(control)){
			iter=100
		} else if(is.null(control$maxit) && is.null(control$iter)){
			iter=100
		} else if(!is.null(control$maxit) && is.null(control$iter)){
			iter=control$maxit
		} else if(is.null(control$maxit) &&!is.null(control$iter)){
			iter=control$iter
		} else {
			iter=control$maxit
		}
		C=list(maxit=iter,REPORT=iter)


		s=0
    if(is.null(control)){
      s=sample(min:max,D,replace=TRUE)
    } else if(is.null(control$par)){
      s=sample(min:max,D,replace=TRUE)
    } else {
      s=control$par
    }



		ichange=function(par,min,max){
			hchange(par,min,max,rnorm,mean=0,sd=1)
		}


		a=hclimbing(s,eval,change=ichange,lower=lower,upper=upper,control=C,type=type)


		if(numb=="real"){

			cat("Optimum Solution: ",a$sol," - ",a$eval,"\n")
			res$sol=a$sol
			res$fun=a$eval

		} else{

			#in case the type of number is bin or integer, the number must be round
			cat("Optimum Solution: ",round(a$sol)," - ",round(a$eval),"\n")
			res$sol=round(a$sol)
			res$fun=round(a$eval)
		}

	}


	####################################################################################################################################
	##################                                Simulated Annealing                             ##################################
	####################################################################################################################################
	if(method=="sann"){

		cat("Simulated Annealing\n")
		#if to get the number of iterations
		iter=0
		if(is.null(control)){
			iter=100
		} else if(is.null(control$maxit) && is.null(control$iter)){
			iter=100
		} else if(!is.null(control$maxit) && is.null(control$iter)){
			iter=control$maxit
		} else if(is.null(control$maxit) && !is.null(control$iter)){
			iter=control$iter
		} else {
			iter=control$maxit
		}

		cl=list()
		cl$maxit=iter
		cl=c(cl,control)
		#if to get the number of iterations
		ichange=function(par){
			D=length(par)
			hchange(par,min,max,rnorm,mean=0,sd=1)
		}
		s=sample(min:max,D,replace=TRUE)
		if(is.null(control)){
			numb="int"
			cl$maxit=iter
			cl$trace=FALSE
			cl$sample=sample(min:max,D,replace=TRUE)
			cl$hessian=FALSE
			cl$gr=ichange
		} else if(is.null(control$numb) || is.null(control$par) || is.null(control$gr) || is.null(control$hessian)){

			if(is.null(control$numb)){
				numb="int"
			}
			if(is.null(control$par)){
				cl$sample=sample(min:max,D,replace=TRUE)
			}
			if(is.null(control$gr)){
				cl$gr=ichange
			}
			if(is.null(control$hessian)){
				cl$hessian=FALSE
			}
		}

		if(type=="max"){
			cl$fnscale=-1
		}

		s=optim(par=cl$sample,eval,gr=cl$gr,method="SANN",control=cl,hessian=FALSE)

		if(numb=="real"){

			cat("Optimum Solution: ", s$par, " - " , abs(s$value),"\n")
			res$sol=s$par
			res$fun=abs(s$value)

		} else{

			#in case the type of number is bin or integer, the number must be round
			cat("Optimum Solution: ", round(s$par), " - " , round(abs(s$value)),"\n")
			res$sol=round(s$par)
			res$fun=round(abs(s$value))

		}
		options(warn=0)
	}

	####################################################################################################################################
	##################                               Tabu SEARCH                                    ####################################
	####################################################################################################################################


	if(method=="tabu"){


		cat("Tabu Search\n")
		#if to get the number of iterations
		iter=0
		if(is.null(control)){
		  iter=100
		} else if(is.null(control$iters) && is.null(control$iter)){
		  iter=100
		} else if(!is.null(control$iters) && is.null(control$iter)){
		  iter=control$iters
		} else if(is.null(control$iters) && !is.null(control$iter)){
		  iter=control$iter
		} else {
		  iter=control$iters
		}

		cl=control
		BITS=8 # bits per dimension
		D=D
		Low=lower
		Up=upper
		evalFun=eval

		if(is.null(control)){
		  numb="int"
		  cl$neigh=D
		  cl$listSize=D-1
		  cl$nRestarts=1
		  cl$repeatAll=1
		  cl$verbose=FALSE
		} else if(is.null(control$numb) || is.null(control$neigh) || is.null(control$listSize) ||
		          is.null(control$nRestarts) || is.null(control$repeatAll)){

		  if(is.null(control$numb)){
		    numb="int"
		  }
		  if(is.null(control$neigh)){
		    cl$neigh=D
		  }
		  if(is.null(control$listSize)){
		    cl$listSize=(D*BITS)-1
		    if(numb=="real" || numb=="int"){

		      cl$listSize=(D*BITS)-1

		    } else {

		      cl$listSize=D-1

		    }
		  }
		  if(is.null(control$nRestarts)){
		    cl$nRestarts=1
		  }
		  if(is.null(control$repeatAll)){
		    cl$repeatAll=1
		  }
		  if(is.null(control$verbose)){
		    cl$verbose=FALSE
		  }
		}

		#return real in Low to Up using binary x:
		bin2real=function(x){ # x is vector of bits
		  x=paste(x,collapse="") # x is now string
		  n=strtoi(x,base=2)
		  return (Low+(Up-Low)*n/(2^BITS-1))
		}

		generateSample=function(x){ #function to generate sample according to the number of D
		  #x number of dimensions
		  i=1
		  a=c()
		  while(i<=x){

		    a=c(a,sample(0:1,BITS,replace=TRUE))
		    i=i+1

		  }
		  return(a)
		}

		bin2list=function(x){ #function to get the values from binary

		  i=1
		  a=c()
		  start=1
		  end=8
		  while(i<=D){

		    a=c(a,bin2real(x[start:end]))
		    start=start+8
		    end=end+8
		    i=i+1

		  }
		  return(a)
		}
		beval=function(x){ #auxiliary objective function to work with binaries

		  b=bin2list(x)
		  f=eval(b)
		  return(f)
		}
		if(numb=="int" || numb=="real"){

		  if(type=="max"){
		    evalFun=beval
		  } else {

		    evalFun=function(x) -beval(x)

		  }

		  a=generateSample(D)
		  size=D*BITS
		  #cat("size: ", size, " sample: ", length(a))
		} else {

		  a=rep(0,D)
		  size=D
		}
		#s=tabuSearch(size,iters=iter,objFunc=evalFun,config=a,neigh=cl$neigh,listSize=cl$listSize,
		             #nRestarts=cl$nRestarts,repeatAll=cl$repeatAll,verbose=cl$verbose)
		s=tabuSearch(size,iters=iter,objFunc=evalFun,config=a,neigh=cl$neigh,nRestarts=cl$nRestarts,
		             repeatAll=cl$repeatAll,verbose=cl$verbose,listSize=cl$listSize)

		b=which.max(s$eUtilityKeep) # best index
		bs=s$configKeep[b,]
		if(numb=="real"){
		  cat("Optimum Solution: ",bin2list(bs)," - ",s$eUtilityKeep[b],"\n")
		  res$sol=bin2list(bs)
		  res$fun=s$eUtilityKeep[b]
		} else if(numb=="int") {
		  cat("Optimum Solution: ",round(bin2list(bs))," - ",round(s$eUtilityKeep[b]),"\n")
		  res$sol=round(bin2list(bs))
		  res$fun=s$eUtilityKeep[b]
		} else {
		  cat("Optimum Solution: ",round(bs)," - ",round(s$eUtilityKeep[b]),"\n")
		  res$sol=round(bs)
		  res$fun=s$eUtilityKeep[b]
		}



	}

	####################################################################################################################################
	##################                       Genetic And Evolutionary Algorightms                             ##########################
	####################################################################################################################################
	if(method=="gea"){
		cat("Genetic and evolutionary Algorithms\n")


		iter=0
		if(is.null(control)){
			iter=100
		} else if(is.null(control$iter)){
			iter=100
		} else {
		  iter=control$iter
		}

		cl=control

		if(numb=="bin"){
			if(is.null(control$suggestions) || is.null(control$popSize) ||
						is.null(control$mutationChance) || is.null(control$elitism) || is.null(control$monitorFunc) || is.null(control$showSettings)
						|| is.null(control$verbose) || is.null(control$zeroToOneRatio)){

				if(is.null(control$suggestions)){
					cl$suggestions=NULL
				}
				if(is.null(control$popsize)){
					cl$popsize=200
				}
				if(is.null(control$mutationChance)){
					cl$mutationChance=NA
				}
				if(is.null(control$elitism)){
					cl$elitism=NA
				}
				if(is.null(control$monitorFunc)){
					cl$monitorFunc=NULL
				}
				if(is.null(control$showSettings)){
					cl$showSettings=FALSE
				}
				if(is.null(control$verbose)){
					cl$verbose=FALSE
				}
				if(is.null(control$zeroToOneRatio)){
					cl$zeroToOneRatio=10
				}
			} else {
				numb=control$numb
			}
		} else {
			if(is.null(control)){
			numb="int"
			cl$suggestions=NULL
			cl$popsize=200
			cl$iters=100
			cl$mutationChance=NA
			cl$elitism=NA
			cl$monitorFunc=NULL
			cl$showSettings=FALSE
			cl$verbose=FALSE
			} else if(is.null(control$numb) || is.null(control$suggestions) || is.null(control$popSize) ||
							is.null(control$mutationChance) || is.null(control$elitism) || is.null(control$monitorFunc) || is.null(control$showSettings)){
				if(is.null(control$numb)){
					numb="int"
				}
				if(is.null(control$suggestions)){
					cl$suggestions=NULL
				}
				if(is.null(control$popsize)){
					cl$popsize=200
				}
				if(is.null(control$mutationChance)){
					cl$mutationChance=NA
				}
				if(is.null(control$elitism)){
					cl$elitism=NA
				}
				if(is.null(control$monitorFunc)){
					cl$monitorFunc=NULL
				}
				if(is.null(control$showSettings)){
					cl$showSettings=FALSE
				}
				if(is.null(control$verbose)){
					cl$verbose=FALSE
				}
			} else {
				numb=control$numb
			}
		}

		#if to get the number of iterations
		iter=0
		if(is.null(control)){
			iter=100
		} else if(is.null(control$iters) && is.null(control$iter)){
			iter=100
		} else if(!is.null(control$iters) && is.null(control$iter)){
			iter=control$iters
		} else if(is.null(control$iters) && !is.null(control$iter)){
			iter=control$iter
		} else {
			iter=control$iters
		}

		if(type=="max"){
			fn=function(x) -eval(x)
		} else if(type=="min"){
			fn=function(x) eval(x)
		} else {
			fn=function(x) eval(x)
		}
		#cat("lower ",lower, " upper ", upper, " D ", D)
		if(numb=="bin"){

			G=rbga.bin(size=D,popSize=cl$popsize,iters=iter,zeroToOneRatio=cl$zeroToOneRatio,evalFunc=fn,elitism=cl$elitism,suggestions=cl$suggestions
					,mutationChance=cl$mutationChance,verbose=cl$verbose,showSettings=cl$showSettings,monitorFunc=cl$monitorFunc)

		} else {

			G=rbga(evalFunc=fn,stringMin=lower,stringMax=upper,popSize=cl$popsize,iters=iter,verbose=cl$verbose,showSettings=cl$showSettings,
					monitorFunc=cl$monitorFunc,elitism=cl$elitism,mutationChance=cl$mutationChance)

		}

		if(numb=="real"){

			cat("Optimum Solution: ",G$population[1,]," - ",eval(G$population[1,]),"\n")
			res$sol=G$population[1,]
			res$fun=eval(G$population[1,])

		} else{

			#in case the type of number is bin or integer, the number must be round
			cat("Optimum Solution: ",round(G$population[1,])," - ",round(eval(G$population[1,])),"\n")
			res$sol=round(G$population[1,])
			res$fun=round(eval(G$population[1,]))
		}
	}

	####################################################################################################################################
	##################                                Differential Evolution                             ###############################
	####################################################################################################################################

	if(method=="dfev"){
		cat("Differential Evolution\n")



		trace=FALSE
		fn=0

		if(type=="max"){
			fn=function(x) -eval(x)
		} else if(type=="min"){
			fn=function(x) eval(x)
		} else {
			fn=function(x) eval(x)
		}
		if(is.null(control)){
			trace=FALSE
		} else if(is.null(control$trace)){
			trace=FALSE
		} else {
		  trace=control$trace
		}

		#if to get the number of iterations
		iter=0
		if(is.null(control)){
			iter=100
		} else if(is.null(control$itermax) && is.null(control$iter)){
			iter=100
		} else if(!is.null(control$itermax) && is.null(control$iter)){
			iter=control$itermax
		} else if(is.null(control$itermax) && !is.null(control$iter)){
			iter=control$iter
		} else {
			iter=control$itermax
		}


		cl=control
		if(is.null(control)){
			numb="int"
			cl$VTR=-Inf
			cl$strategy=2
			cl$bs=FALSE
			cl$NP=NA
			cl$CR=0.5
			cl$F=0.8
			cl$trace=trace
			cl$initialpop=NULL
			cl$storepopfrom=iter+1
			cl$storepopfreq=1
			cl$p=0.2
			cl$c=0
			cl$steptol=iter
			cl$parallelType=0
			cl$packages=c()
			cl$parvar=c()
			cl$forearchArgs=list()
		} else if(length(control)<19){
			if(is.null(control$numb)){
				numb="int"
			}
			if(is.null(control$VTR)){
				cl$VTR=-Inf
			}
			if(is.null(control$strategy)){
				cl$strategy=2
			}
			if(is.null(control$bs)){
				cl$bs=FALSE
			}
			if(is.null(control$NP)){
				cl$NP=NA
			}
			if(is.null(control$CR)){
				cl$CR=0.5
			}
			if(is.null(control$F)){
				cl$F=0.8
			}
			if(is.null(control$trace)){
				cl$trace=trace
			}
			if(is.null(control$initialpop)){
				cl$initialpop=NULL
			}
			if(is.null(control$storepopfrom)){
				cl$storepopfrom=iter+1
			}
			if(is.null(control$storepopfreq)){
				cl$storepopfreq=1
			}
			if(is.null(control$p)){
				cl$p=0.2
			}
			if(is.null(control$c)){
				cl$c=0
			}
			if(is.null(control$steptol)){
				cl$steptol=iter
			}
			if(is.null(control$parallelType)){
				cl$parallelType=0
			}
			if(is.null(control$packages)){
				cl$packages=NULL
			}
			if(is.null(control$parvar)){
				cl$parvar=NULL
			}
			if(is.null(control$forearchArgs)){
				cl$forearchArgs=NULL
			}
		}


		C=DEoptim.control(itermax=iter,trace=trace,foreachArgs = cl$foreachArgs,parVar = cl$parvar,packages = cl$packages,parallelType = cl$parallelType,
		                  steptol = cl$steptol,c=cl$c,p=cl$p,storepopfrom = cl$storepopfrom,storepopfreq = cl$storepopfreq,initialpop = cl$initialpop,
		                  F=cl$F,CR=cl$CR,NP=cl$NP,bs=cl$bs,strategy = cl$strategy,VTR = cl$VTR)
		D=DEoptim(fn,lower,upper,control=C)




		if(numb=="real"){

			cat("Optimum Solution: ", D$optim$bestmem, " - ", abs(D$optim$bestval),"\n")
			res$sol=D$optim$bestmem
			res$fun=abs(D$optim$bestval)

		} else{

			#in case the type of number is bin or integer, the number must be round
			cat("Optimum Solution: ",round(D$optim$bestmem), " - ", round(abs(D$optim$bestval)),"\n")
			res$sol=round(D$optim$bestmem)
			res$fun=round(abs(D$optim$bestval))
		}

	}

	####################################################################################################################################
	##################                                Particle Swarm optimization                            ###########################
	####################################################################################################################################

	if(method=="pso"){
		cat("Particle Swarm Optimization\n")

		cl=control
		#if to get the number of iterations
		iter=0
		if(is.null(control)){
			iter=100
		} else if(is.null(control$maxit) && is.null(control$iter)){
			iter=100
		} else if(!is.null(control$maxit) && is.null(control$iter)){
			iter=control$maxit
		} else if(is.null(control$maxit) && !is.null(control$iter)){
			iter=control$iter
		} else {
			iter=control$maxit
		}

		fnscale=1
		if(is.null(cl$fnscale)){

			if(type=="max"){
				cl$fnscale=-1
			} else {
				cl$fnscale=1
			}
		}
		if(!is.null(cl$trace)){

			if(cl$trace==TRUE){
				cl$trace=1
			} else {
				cl$trace=0
			}
		}
		cl$maxit=iter
		s=0
		if(is.null(control)){
		  s=sample(min:max,D,replace=TRUE)
		} else if(is.null(control$par)){
		  s=sample(min:max,D,replace=TRUE)
		} else {
		  s=control$par
		}

		#C=list(maxit=control$iter,fnscale=fnscale)
		s=psoptim(s,fn=eval,lower=lower,upper=upper,control=cl)


		if(numb=="real"){

			cat("Optimum Solution: ", s$par, " - ", abs(s$value),"\n")
			res$sol=s$par
			res$fun=abs(s$value)

		} else{

			#in case the type of number is bin or integer, the number must be round
			cat("Optimum Solution: ", round(s$par), " - ", round(abs(s$value)),"\n")
			res$sol=round(s$par)
			res$fun=round(abs(s$value))
		}

	}
	####################################################################################################################################
	##################                                Estimation of Distribution Algorithm                           ###################
	####################################################################################################################################
	if(method=="eda"){


		cat("Estimation of Distribution Algorithm\n")
		LP=50;
		cl=control

		#if to get the number of iterations
		iter=0
		if(is.null(control)){
			iter=100
		} else if(is.null(control$maxgen) && is.null(control$iter)){
			iter=100
		} else if(!is.null(control$maxgen) && is.null(control$iter)){
			iter=control$maxgen
		} else if(is.null(control$maxgen) && !is.null(control$iter)){
			iter=control$iter
		} else {
			iter=control$maxgen
		}

		fn=0
		if(type=="max"){
			fn=function(x) -eval(x)
		} else if(type=="min"){
			fn=function(x) eval(x)
		} else {
			fn=function(x) eval(x)
		}

		if(is.null(control)){
			numb="int"
			cl$copula="normal"
			cl$margin="norm"
			cl$popSize=100

		} else if(is.null(control$numb) || is.null(control$copula) || is.null(control$margin) || is.null(control$popsize)){

			if(is.null(control$numb)){
				numb="int"
			}
			if(is.null(control$copula)){
				cl$copula="normal"
			}
			if(is.null(control$margin)){
				cl$margin="norm"
			}
			if(is.null(control$popSize)){
				cl$popSize=100
			}
		} else {
			cl$iter=iter
		}

		setMethod("edaTerminate","EDA",edaTerminateMaxGen)
		GCEDA=CEDA(copula=cl$copula,margin=cl$margin,popSize=cl$popSize,maxGen=iter)
		GCEDA@name="GCEDA"
		E=edaRun(GCEDA,fn,lower,upper)



		if(numb=="real"){

			cat("Optimum Solution: ", E@bestSol, " - ", abs(E@bestEval),"\n")
			res$sol=E@bestSol
			res$fun=abs(E@bestEval)

		} else{

			#in case the type of number is bin or integer, the number must be round
			cat("Optimum Solution: ", round(E@bestSol), " - ", round(abs(E@bestEval)),"\n")
			res$sol=round(E@bestSol)
			res$fun=round(abs(E@bestEval))
		}
	}

	####################################################################################################################################
	##################                                Artificial Bee Colony                         ####################################
	####################################################################################################################################
	if(method=="abc"){


		cat("Artificial Bee Colony\n")
		cl=control
		#if to get the number of iterations
		iter=0
		if(is.null(control)){
			iter=100
		} else if(is.null(control$maxCycle) && is.null(control$iter)){
			iter=100
		} else if(!is.null(control$maxCycle) && is.null(control$iter)){
			iter=control$maxCycle
		} else if(is.null(control$maxCycle) && !is.null(control$iter)){
			iter=control$iter
		} else {
			iter=control$maxCycle
		}

		if(is.null(control)){
			numb="int"
			cl$par=sample(min:max,D,replace=TRUE)
			cl$NP=40
			cl$FoodNumber=cl$NP/2
			cl$limit=100
			cl$optiinteger=FALSE
			cl$criter=50

		} else if(is.null(control$numb) || is.null(control$par) || is.null(control$NP) || is.null(control$limit) || is.null(control$optiinteger)
					|| is.null(control$criter)){

			if(is.null(control$numb)){
				numb="int"
			}
			if(is.null(control$par)){
				cl$par=sample(min:max,D,replace=TRUE)
			}
			if(is.null(control$NP)){
				cl$NP=40
			}
			if(is.null(control$FoodNumber)){
				cl$FoodNumber=cl$NP/2
			}
			if(is.null(control$limit)){
				cl$limit=100
			}
			if(is.null(control$optiinteger)){
				cl$optiinteger=FALSE
			}
			if(is.null(control$criter)){
				cl$criter=50
			}
		} else {
			numb=control$numb
		}
		fn=0
		if(type=="max"){
			fn=function(x) -eval(x)
		} else if(type=="min"){
			fn=function(x) eval(x)
		} else {
			fn=function(x) eval(x)
		}

		a=abc_optim(par=cl$par,fn=fn,D=D,NP=cl$NP,FoodNumber=cl$FoodNumber,lb=lower,ub=upper,limit=cl$limit,maxCycle=iter,optiinteger=cl$optiinteger,criter=cl$criter)
		if(numb=="real"){

		  if(type=="max"){
		    cat("Optimum Solution: ", a$par, " - ", abs(a$value),"\n")
		    res$sol=a$par
		    res$fun=abs(a$value)
		  }else {
		    cat("Optimum Solution: ", a$par, " - ", abs(a$value),"\n")
		    res$sol=a$par
		    res$fun=a$value
		  }

		} else {
		  if(type=="max"){
		    cat("Optimum Solution: ", round(a$par), " - ", round(abs(a$value)),"\n")
		    res$sol=round(a$par)
		    res$fun=round(abs(a$value))
		  }else {
		    cat("Optimum Solution: ", round(a$par), " - ", round(abs(a$value)),"\n")
		    res$sol=round(a$par)
		    res$fun=round(a$value)
		  }
		}
	}
	invisible(res)
}
