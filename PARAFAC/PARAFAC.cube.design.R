#'@title Combine EEMs produce by the Cary Eclipse fluorometer into a cube
#'ready for PARAFAC analysis in Matlab
#'
#'@description  This function reads any number of EEMs and produce a cube of EEMs.
#'It enables the standardization into Raman Unit and instrument's corrections.
#'
#'@path Full path of the working directory (can be called using getwd())
#'@param excitation is a vector of three variables of the scanning setup (min,max,interval).
#'Default is c(220,450,5)
#'@param emission is a vector of three variables of the scanning setup (min,max,interval).
#'Default is c(230,600,2)
#'@param  EMCOL  is a logical parameter indicating whether or not the emission are
#'stored as column in the csv file. Default is FALSE.
#'@param 
#'@param samplepercsv is a parameter which indicates the number of sample in the csv file coming from the fluorometer.
#'@param RU is a logical parameter to transform fluorescence intensities into Raman Unit at Ex = 350 nm.
#'Default is TRUE.
#'@param EmEx.cor is a logical parameter to correct EEMs for emission and excitation corrections.
#'Default is True. Emission and excitation file must be numerics only stored in csv file.
#'
#'
#'@export
#'
#'



PARAFAC.cube.design = function(path, excitation = c(220,450,5), emission = c(230, 600, 2), EMCOL = F, samplepercsv = 1,Subtract.Blank = T, RU = T, rm.corner=T, EmEx.cor = T, Inner=T, rm.scat = T, split="_")
{
  wlex <- seq(excitation[1], excitation[2], excitation[3])
  wlem <- seq(emission[1], emission[2], emission[3])
  nex <- length(wlex)
  nem  <- length(wlem)

	setwd(path)
	
	file.dir = list.files()
	nano.temp = grep("nano", file.dir)
	cdom.temp = grep("CDOM",file.dir)
	file.dir = file.dir[-nano.temp]
	file.dir = file.dir[-cdom.temp]
	file.list = list()
	  
	for(i in 1:length(file.dir))
	{
	  file.list[[i]] = paste(file.dir[i],"/",list.files(file.dir[i]),sep="")
	}
  file.list = unlist(file.list)
  csv.count <- str_count(file.list,"_") + 1
	  
  file.sample = file.list[csv.count==1]
  file.sample2 = file.list[csv.count==2]
  file.sample3 = file.list[csv.count==3]
  file.sample4 = file.list[csv.count==4]
  
  
	#Reading the sample files and creating a list of EEMs
	counter = 1
	data.list <- list()
	filename <- list()
	index = 0
	list.length = 0
	while(counter <= samplepercsv)
	{
		if(counter == 1)
		{
			file.data <- file.sample
		}
		
		if(counter == 2)
		{
			file.data <- file.sample2
		}
		
		if(counter == 3)
		{
			file.data <- file.sample3
		}
		
		if(counter == 4)
		{
			file.data <- file.sample4
		}
				
		if(length(file.data) > 1)
		{
			for (i in 1:length(file.data))
			{
					EEM = read.EEM.Richard(file.data[i],excitation,emission, EMCOL, counter, split=split)
					data.list[[i + index]] <- EEM$EEM.list
					filename[[i + index]] <- unlist(EEM$EEM.name)
			}
			index = index + length(file.data)
			list.length = list.length + length(file.data) * counter
		}
		else
		{
			if(length(file.data)==1)
			{
				EEM = read.EEM.Richard(file.data,excitation,emission, EMCOL, counter, split=split)
				data.list[[index + 1]] = EEM$EEM.list
				filename[[index + 1]] = unlist(EEM$EEM.name)
				index = index + 1
				list.length = list.length + counter
			}
		}
		counter = counter + 1
	}
	cube <- array(unlist(data.list),dim=c(nex,nem,list.length))
	if(Subtract.Blank)
	{
	  Raman = NanoMean(path, excitation, emission, EMCOL,RU=T, split=split)
	  for(k in 1:length(cube[1,1,]))
	  {
	    cube[,,k] <- cube[,,k]- Raman[[1]]$eem[,,1]
	  }
	}
	if(Inner)
	{
	  cube = InnerFilter(path, cube, excitation = c(220,450,5), emission = c(230, 600, 2),pathlength=1)
	}

	if(rm.corner)
	for(k in 1:length(cube[1,1,]))
	{
	  for(i in 1:length(cube[,1,1]))
	  {
	    cube[i,wlem<=(wlex[i]-10),k]=0 #Put all data below 1st order Rayleigh equal 0
	  }
	}
	
  if(RU)
  {
    if(Subtract.Blank == F)
    {
        Raman = NanoMean(path, excitation, emission, EMCOL, split=split,RU=T)
    }
    RAMANInt = plot.integrate.RAMAN(Raman, maxF, graph=F)
    cube.RU=cube
    for(k in 1:length(cube[1,1,]))
    {
      cube.RU[,,k] <- cube[,,k]/RAMANInt
    }
    
    if(EmEx.cor)
    {
      file.Em = read.csv("F:/Richard/PARAFAC/FDOM/2016/Emcorr_220 to 600.csv")
      file.Ex = read.csv("F:/Richard/PARAFAC/FDOM/2016/Excorr.csv")
      Ex.cor = as.numeric(na.omit(file.Ex[match(round(file.Ex[,1]),wlex),2]))
      Em.cor = t(as.numeric(na.omit(file.Em[match(round(file.Em[,1]),wlem),2])))
      Cor.mat = Ex.cor %*% Em.cor
      cube.RU.EmEx = cube.RU
      for(k in 1:length(cube.RU[1,1,]))
      {
        cube.RU.EmEx[,,k] = cube.RU[,,k] * Cor.mat
      }
      if(rm.scat)
      {
        cube.RU.EmEx.Scat = cube.RU.EmEx
        for(k in 1:length(cube.RU.EmEx[1,1,]))
        {
          for(i in 1:length(cube.RU.EmEx[,1,1]))
          {
            #cube.RU.EmEx.Scat[i,wlem =>(2*wlex[i]-15),k]=NA #Removes top left data after 2nd order Raman
            #cube.RU.EmEx.Scat[i,wlem<=(wlex[i]+14),k]=NA #Removes bottom right data below 1st order Rayleigh
            cube.RU.EmEx.Scat[i,wlem<=(wlex[i]+10) & wlem >=(wlex[i]-10),k]=NA #Removes band of 1st order Rayleigh
            cube.RU.EmEx.Scat[i,wlem<=(-1*wlex[i]/(0.00036*wlex[i]-1)+10) & wlem >=(-1*wlex[i]/(0.00036*wlex[i]-1)-10),k]=NA #Removes band of 1st order Raman
            #cube.RU.EmEx.Scat[i,wlem<=(1.3*wlex[i]-38),k]=NA #Removes bottom-right below 1st order Raman
            cube.RU.EmEx.Scat[i,wlem<=(2*wlex[i]+16) & wlem >=(2*wlex[i]-10),k]=NA #Removes band of 2nd order Rayleigh
            cube.RU.EmEx.Scat[i,wlem<=(-2*wlex[i]/(0.00036*wlex[i]-1)+14) & wlem >=(-2*wlex[i]/(0.00036*wlex[i]-1)-14),k]=NA #Removes band of 2nd order Raman
            #cube.RU.EmEx.Scat[i,wlem<=(2.32*wlex[i]-17) & wlem >=(2*wlex[i]-14),k]=NA #Removes between 2nd order Rayleigh and Raman
            #cube.RU.EmEx.Scat[wlex<=300 & wlex>=275,wlem <= 302 & wlem >= 290,k]=NA #Remove all missed value by previous inequations
            cube.RU.EmEx.Scat[i,wlem<=(wlex[i]-8),k]=0 #Put all data below 1st order Rayleigh equal 0
          }
        }
        return(list(cube.RU.EmEx.Scat,filename,nex,nem,list.length))
      }
      return(list(cube.RU.EmEx,filename,nex,nem,list.length))
    }
    if(rm.scat)
    {
      cube.RU.Scat = cube.RU
      for(k in 1:length(cube.RU[1,1,]))
      {
        for(i in 1:length(cube.RU[,1,1]))
        {
          #cube.RU.Scat[i,wlem =>(2*wlex[i]-15),k]=NA #Removes top left data after 2nd order Raman
          #cube.RU.Scat[i,wlem<=(wlex[i]+14),k]=NA #Removes bottom right data below 1st order Rayleigh
          cube.RU.Scat[i,wlem<=(wlex[i]+14) & wlem >=(wlex[i]-10),k]=NA #Removes band of 1st order Rayleigh
          cube.RU.Scat[i,wlem<=(1.3*wlex[i]-38) & wlem >=(1.3*wlex[i]-67),k]=NA #Removes band of 1st order Raman
          #cube.RU.Scat[i,wlem<=(1.3*wlex[i]-38),k]=NA #Removes bottom-right below 1st order Raman
          cube.RU.Scat[i,wlem<=(2*wlex[i]+17) & wlem >=(2*wlex[i]-13),k]=NA #Removes band of 2nd order Rayleigh
          cube.RU.Scat[i,wlem<=(2.32*wlex[i]-16) & wlem >=(2.32*wlex[i]-43),k]=NA #Removes band of 2nd order Raman
          #cube.RU.Scat[i,wlem<=(2.32*wlex[i]-17) & wlem >=(2*wlex[i]-14),k]=NA #Removes between 2nd order Rayleigh and Raman
          cube.RU.Scat[wlex<=300 & wlex>=275,wlem <= 302 & wlem >= 290,k]=NA #Remove all missed value by previous inequations
          cube.RU.Scat[i,wlem<=(wlex[i]-10),k]=0 #Put all data below 1st order Rayleigh equal 0
        }
        return(list(cube.RU.Scat,filename,nex,nem,list.length))
      }
    }
      return(list(cube.RU,filename,nex,nem,list.length))
  }
  if(EmEx.cor)
  {
    file.Em = read.csv(choose.files(caption = "Select Emission correction file"))
    file.Ex = read.csv(choose.files(caption = "Select Excitation correction file"))
    Ex.cor = as.numeric(na.omit(file.Ex[match(round(file.Ex[,1]),wlex),2]))
    Em.cor = t(as.numeric(na.omit(file.Em[match(round(file.Em[,1]),wlem),2])))
    Cor.mat = Ex.cor %*% Em.cor
    cube.EmEx = cube
    for(k in 1:length(cube[1,1,]))
    {
      cube.EmEx[,,k] = cube[,,k] * Cor.mat
    }
    if(rm.scat)
    {
      cube.EmEx.Scat = cube.EmEx
      for(k in 1:length(cube.EmEx[1,1,]))
      {
        for(i in 1:length(cube.EmEx[,1,1]))
        {
          #cube.EmEx.Scat[i,wlem =>(2*wlex[i]-15),k]=NA #Removes top left data after 2nd order Raman
          #cube.EmEx.Scat[i,wlem<=(wlex[i]+14),k]=NA #Removes bottom right data below 1st order Rayleigh
          cube.EmEx.Scat[i,wlem<=(wlex[i]+14) & wlem >=(wlex[i]-10),k]=NA #Removes band of 1st order Rayleigh
          cube.EmEx.Scat[i,wlem<=(1.3*wlex[i]-38) & wlem >=(1.3*wlex[i]-67),k]=NA #Removes band of 1st order Raman
          #cube.EmEx.Scat[i,wlem<=(1.3*wlex[i]-38),k]=NA #Removes bottom-right below 1st order Raman
          cube.EmEx.Scat[i,wlem<=(2*wlex[i]+17) & wlem >=(2*wlex[i]-13),k]=NA #Removes band of 2nd order Rayleigh
          cube.EmEx.Scat[i,wlem<=(2.32*wlex[i]-16) & wlem >=(2.32*wlex[i]-43),k]=NA #Removes band of 2nd order Raman
          #cube.EmEx.Scat[i,wlem<=(2.32*wlex[i]-17) & wlem >=(2*wlex[i]-14),k]=NA #Removes between 2nd order Rayleigh and Raman
          cube.EmEx.Scat[wlex<=300 & wlex>=275,wlem <= 302 & wlem >= 290,k]=NA #Remove all missed value by previous inequations
          cube.EmEx.Scat[i,wlem<=(wlex[i]-10),k]=0 #Put all data below 1st order Rayleigh equal 0
        }
      }
      return(list(cube.EmEx.Scat,filename,nex,nem,list.length))
    }
    return(list(cube.EmEx,filename,nex,nem,list.length))
  }
  if(rm.scat)
  {
    cube.Scat = cube
    for(k in 1:length(cube[1,1,]))
    {
      for(i in 1:length(cube[,1,1]))
      {
        #cube.Scat[i,wlem =>(2*wlex[i]-15),k]=NA #Removes top left data after 2nd order Raman
        #cube.Scat[i,wlem<=(wlex[i]+14),k]=NA #Removes bottom right data below 1st order Rayleigh
        cube.Scat[i,wlem<=(wlex[i]+14) & wlem >=(wlex[i]-10),k]=NA #Removes band of 1st order Rayleigh
        cube.Scat[i,wlem<=(1.3*wlex[i]-38) & wlem >=(1.3*wlex[i]-67),k]=NA #Removes band of 1st order Raman
        #cube.Scat[i,wlem<=(1.3*wlex[i]-38),k]=NA #Removes bottom-right below 1st order Raman
        cube.Scat[i,wlem<=(2*wlex[i]+17) & wlem >=(2*wlex[i]-13),k]=NA #Removes band of 2nd order Rayleigh
        cube.Scat[i,wlem<=(2.32*wlex[i]-16) & wlem >=(2.32*wlex[i]-43),k]=NA #Removes band of 2nd order Raman
        #cube.Scat[i,wlem<=(2.32*wlex[i]-17) & wlem >=(2*wlex[i]-14),k]=NA #Removes between 2nd order Rayleigh and Raman
        cube.Scat[wlex<=300 & wlex>=275,wlem <= 302 & wlem >= 290,k]=NA #Remove all missed value by previous inequations
        cube.Scat[i,wlem<=(wlex[i]-10),k]=0 #Put all data below 1st order Rayleigh equal 0
      }
    }
    return(list(cube.Scat,filename,nex,nem,list.length))
  }
	return(list(cube,filename,nex,nem,list.length))
}