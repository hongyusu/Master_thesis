

inside = c()
for(i in c(1:30))
{
	x = sample(c(20:80),1)
	y = sample(c(0:100),1)
	# inside
	flag = 0
	if(x < 40)
	{if(y > sqrt(3600-x^2)+10){flag = 1}}
	else if(x > 60)
	{if(y < -sqrt(3600-(x-100)^2)+90){flag = 1}}
	else
	{if(y > sqrt(3600-x^2) +10 && y <= -sqrt(3600-(x-100)^2)+90){flag = 1}}
	
	while(flag == 0)
	{
		y = sample(c(0:100),1)
			# inside
		if(x < 40)
		{if(y > sqrt(3600-x^2)+10){flag = 1}}
		else if(x > 60)
		{if(y < -sqrt(3600-(x-100)^2)+90){flag = 1}}
		else
		{if(y > sqrt(3600-x^2) +10 && y <= -sqrt(3600-(x-100)^2)+90){flag = 1}}
	}
	inside = rbind(inside, c(x,y))
}

outside = c()
for(i in c(1:15))
{
	x = sample(c(8:60),1)
	y = sample(c(8:(sqrt(3600-x^2)-5)),1)
	outside = rbind(outside, c(x,y))
}
for(i in c(1:15))
{
	x = sample(c(45:100),1)
	y = sample(c((-sqrt(3600-(x-100)^2)+108):100),1)
	outside = rbind(outside, c(x,y))
}

up = c()
for(i in c(1:30))
{
	x = sample(c(10:90),1)
	y = sample(c((x+10):90), 1)
	up = rbind(up, c(x,y))
}

down = c()
for(i in c(1:30))
{
	x = sample(c(10:90),1)
	y = sample(c(10:(x-10)), 1)
	down = rbind(down, c(x,y))
}


pdf('../plots/kernel_svm.pdf', height = 8, width = 16)
par(mfrow = c(1,2), lwd = 4, cex = 2, cex.axis = 1.5, cex.lab = 1.4, oma = c(0,0,0,0), mar = c(0,0,0,1))
plot(inside, type = 'p', xlab = '', ylab = '', xlim = c(-0,100), ylim = c(0,100), xaxt = 'n', yaxt = 'n', col = 'red')
grid(10,10)
points(outside, col = 'purple')
curve(sqrt(3600-x^2), 0, 60, add = TRUE, col = 'blue')
curve(-sqrt(3600-(x-100)^2)+100, 40, 100, add = TRUE, col = 'blue')
text(5,95,'X',cex = 2)

par(mar = c(0,1,0,0))
plot(c(1,100),c(1,100), type = 'l', xlab = '', ylab = '', xlim = c(-0,100), ylim = c(0,100), xaxt = 'n', yaxt = 'n', col = 'blue')
grid(10,10)
points(up, col = 'red')
points(down, col = 'purple')
text(5,95,'H',cex = 2)
dev.off()








