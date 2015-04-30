







# data
x.mid = seq(5,80,5)
y.mid = -1.2*x.mid + 100

x.low = seq(0,65,5)
y.low = -1.2*x.low + 85

x.high = seq(20,90,5)
y.high = -1.2*x.high + 115

low = c()
for(i in c(1:20))
{
	x = sample(c(0:60),1)
	y = sample(c(5:(-1.2*x + 77)),1)
	low = rbind(low, c(x,y))
}
low = rbind(low, c(x.low[5],y.low[5]))
low = rbind(low, c(x.low[10],y.low[10]))
low = rbind(low, c(x.low[12],y.low[12]))

low.error.x = c(50,60)
low.error.y = c(70,30)
low.error = cbind(low.error.x, low.error.y)


high = c()
for(i in c(1:20))
{
	x = sample(c(30:100),1)
	y = sample(c((-1.2*x + 123):100),1)
	high = rbind(high, c(x,y))
}
high = rbind(high, c(x.high[3],y.high[3]))
high = rbind(high, c(x.high[8],y.high[8]))
high = rbind(high, c(x.high[12],y.high[12]))

high.error.x = c(20,30)
high.error.y = c(75,40)
high.error = cbind(high.error.x, high.error.y)


# empirical risk
pdf('../plots/empirical_risk.pdf', height = 8, width = 8)
par(lwd = 4, cex = 2, cex.axis = 1.5, cex.lab = 1.4, oma = c(0,0,0,0), mar = c(0,0,0,0))
plot(c(0,80), c(100,0), type = 'l', xlab = '', ylab = '', xlim = c(-0,100), ylim = c(0,100), xaxt = 'n', yaxt = 'n', col = 'blue')
grid(10,10)
lines(c(0,80), c(60,100), col = 'brown')
lines(c(0,90), c(75,30), col = 'dark green')

points(low, col = 'red')
points(high, col = 'purple')
dev.off()

# linear perceptron
pdf('../plots/linear_perceptron.pdf', height = 8, width = 8)
par(lwd = 4, cex = 2, cex.axis = 1.5, cex.lab = 1.4, oma = c(0,0,0,0), mar = c(0,0,0,0))
plot(c(5,75), c(100,0), type = 'l', xlab = '', ylab = '', xlim = c(-0,100), ylim = c(0,100), xaxt = 'n', yaxt = 'n', col = 'blue')
grid(10,10)
lines(c(0,80), c(90,10), col = 'blue')
lines(c(5,80), c(80,0), col = 'blue')

points(low, col = 'red')
points(high, col = 'purple')
dev.off()

# margin marximization
pdf('../plots/margin_maximization.pdf', height = 8, width = 8)
par(lwd = 4, cex = 2, cex.axis = 1.5, cex.lab = 1.4, oma = c(0,0,0,0), mar = c(0,0,0,0))

plot(y.mid~x.mid, type = 'l', xlab = '', ylab = '', xlim = c(-0,100), ylim = c(0,100), xaxt = 'n', yaxt = 'n', col = 'blue')
grid(10,10)
lines(y.low~x.low, lty = 2, col = 'dark green')
lines(y.high~x.high, lty = 2, col = 'dark green')
points(low, col = 'red')
points(low[(length(low[,1])-2):length(low[,1]),], col = 'red', pch = 19)
points(high, col = 'purple')
points(high[(length(high[,1])-2):length(high[,1]),], col = 'purple', pch = 19)

par(lwd = 3)
arrows(39, 39*5/6 + 20, 51, 51*5/6+20, angle = 20,length = 0.2)
text(51,51*5/6+20,'w',cex =1,pos = 4)
arrows(55, 55*5/6 - 27, 62, 62*5/6-27, angle = 20,length = 0.1,code  =3)
arrows(68, 68*5/6 - 38, 75, 75*5/6-38, angle = 20,length = 0.1,code = 3)
text(srt = 39, 66,66*5/6-38,'Margin',cex =1,pos = 3)

dev.off()

# soft margin optimization
pdf('../plots/soft_margin.pdf', height = 8, width = 8)
par(lwd = 4, cex = 2, cex.axis = 1.5, cex.lab = 1.4, oma = c(0,0,0,0), mar = c(0,0,0,0))

plot(y.mid~x.mid, type = 'l', xlab = '', ylab = '', xlim = c(-0,100), ylim = c(0,100), xaxt = 'n', yaxt = 'n', col = 'blue')
grid(10,10)
lines(y.low~x.low, lty = 2, col = 'dark green')
lines(y.high~x.high, lty = 2, col = 'dark green')
points(low, col = 'red')
points(low[(length(low[,1])-2):length(low[,1]),], col = 'red', pch = 19)
points(high, col = 'purple')
points(high[(length(high[,1])-2):length(high[,1]),], col = 'purple', pch = 19)

points(low.error, col = 'red', pch = 2)
points(high.error, col = 'purple', pch = 2)

par(lwd = 3)
arrows(39, 39*5/6 + 20, 51, 51*5/6+20, angle = 20,length = 0.2)
text(51,51*5/6+20,'w',cex =1,pos = 4)
arrows(55, 55*5/6 - 27, 62, 62*5/6-27, angle = 20,length = 0.1,code  =3)
arrows(68, 68*5/6 - 38, 75, 75*5/6-38, angle = 20,length = 0.1,code = 3)
text(srt = 39, 66,66*5/6-38,'Margin',cex =1,pos = 3)

arrows(28, 28*5/6 + 28, 49, 49*5/6+28, angle = 20,length = 0.1,col = 'red')
arrows(31, 31*5/6 + 15, 49, 49*5/6+15, angle = 20,length = 0.1,code =1,col = 'purple')
#arrows(52, 52*5/6 - 20, 59, 59*5/6-20, angle = 20,length = 0.1,col = 'red')
#text(srt = 37, 54,54*5/6-20,'e',cex =1,pos = 3)
text(srt = 39, 38,38*5/6+28,'error',cex =1,pos = 3)
text(srt = 39, 40,40*5/6+15,'error',cex =1,pos = 1)

dev.off()



