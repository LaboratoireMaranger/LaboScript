CDOMOverlay <- function()
{
	require(lattice)
	file.data = choose.files(caption="Select CDOM file(s)")
	CDOM = list()
	for(i in 1:length(file.data))
	{
		data = read.table(file.data[i],skip=1,header=1,sep=",")
		WV <- data[,1]
		abs <- data[,2]
		CDOM[[i]] = cbind(WV,abs)
	}
	CDOM = unlist(CDOM)
	abs.number = seq(2,2*length(file.data),2)
	plot.CDOM = cbind(WV,CDOM[,abs.number])
	for(i in 2:length(plot.CDOM))
	{
		if(i==2)
		{
			plot(plot.CDOM[,i]~plot.CDOM$WV,xlim=c(190,900),ylim=c(0,3),type="l",main="")
		}
		else
		{
			lines(plot.CDOM[,i]~plot.CDOM$WV)
		}
	}
}
