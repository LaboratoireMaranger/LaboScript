#Fonction nll
#Le paramètre pars doit toujours avoir 4 entrées de la forme suivante c(a,b,c,sigma)
#Le paramètre model inclu les fonctions: "hyperbolic", "michaelis-menten", "hollingIII" et "hollingIV)
nllRationalFunction <- function(pars,xObs,yObs,model)
{
	if(model == "hyperbolic")
	{
	#Unpack parameters
	a <- pars[1]
	b <- pars[2]
	sigma <- pars[4]
	#Calculate predictions
	yFit <- a /(b + xObs) #Hyperbolic
	#Calculate error 
	e <- yObs - yFit
	#Calculate and return NLL
	}
	
	if(model == "michaelis-menten")
	{
		#Unpack parameters
		a <- pars[1]
		b <- pars[2]
		sigma <- pars[4]
		#Calculate predictions
		yFit <- a * xObs /(b + xObs) #Hyperbolic
		#Calculate error 
		e <- yObs - yFit
		#Calculate and return NLL
	}
	
	if(model == "hollingIII")
	{
		#Unpack parameters
		a <- pars[1]
		b <- pars[2]
		sigma <- pars[4]
		#Calculate predictions
		yFit <- a*xObs^2/(b^2+xObs^2) #Holling type III
		#Calculate error 
		e <- yObs - yFit
	}
	
	if(model == "hollingIV")
	{
		#Unpack parameters
		a <- pars[1]
		b <- pars[2]
		c <- pars[3]
		sigma <- pars[4]
		#Calculate predictions
		yFit <- a * xObs^2/(b + c * xObs + xObs^2) #Holling type III
		#Calculate error 
		e <- yObs - yFit
	}
	
	#Calculate and return NLL
	nll <- -sum(dnorm(e,0,sigma,log=T)) 
	return(nll)
}