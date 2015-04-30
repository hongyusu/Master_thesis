
# 50 0.1-1

data = as.matrix(read.table('../data/walk_contribution.txt'))
for(i in c(1:length(data[,1])))
{
	a = data[i,length(data[i,])]
	data[i,] = data[i,]/a
}
data = cbind(rep(0,7),data)
for(i in c(2:51))
{
	data[,i-1] = data[,i] - data[,i-1]
}
data = data[,-51]

pdf('../plots/walk_contribution.pdf', width = 8, height = 8)
par(lwd = 4, cex = 1.3, cex.axis = 1.5, cex.lab = 1.5, oma = c(0,0,0,0), mar = c(4.5,4,0,0))
plot(data[1,1:10], type = 'b',ylim = c(0,1),xlim = c(1,10),xlab = 'Walk length',ylab = 'Contribution %',pch = 1,lty = 1)
grid(10,10)
for(i in c(2:6))
{
	lines(data[i,1:10],type = 'b',col = i,pch = i,lty = i)
}
legend('topright',c('0.11','0.1','0.09','0.07','0.05','0.03'),col = 1:6,lty = 1:6,pch = 1:6)
dev.off()
