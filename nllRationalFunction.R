#Fonction nll
#Le paramètre pars doit toujours avoir 4 entrées de la forme suivante c(a,b,c,sigma)
#Le paramètre model inclu les fonctions: "hyperbolic", "michaelis-menten", "hollingIII" et "hollingIV)
nllRationalFunction <- function(pars,xObs,yObs,model)
{
	#Unpack parameters
	a <- pars[1]
	b <- pars[2]
	c <- pars[3]
	sigma <- pars[4]
	
	if(model == "hyperbolic")
	{
	#Calculate predictions
	yFit <- a /(b + xObs)
	#Calculate error 
	e <- yObs - yFit
	#Calculate and return NLL
	}
	
	if(model == "michaelis-menten")
	{
		#Calculate predictions
		yFit <- a * xObs /(b + xObs)
		#Calculate error 
		e <- yObs - yFit
		#Calculate and return NLL
	}
	
	if(model == "hollingIII")
	{
		#Calculate predictions
		yFit <- a*xObs^2/(b^2+xObs^2)
		#Calculate error 
		e <- yObs - yFit
	}
	
	if(model == "hollingIV")
	{
		#Calculate predictions
		yFit <- a * xObs^2/(b + c * xObs + xObs^2)
		#Calculate error 
		e <- yObs - yFit
	}
	
	#Calculate and return NLL
	nll <- -sum(dnorm(e,0,sigma,log=T)) 
	return(nll)
}