f1 = function(x)
{
	return(-2*x + 110)
}

f2 = function(x)
{
	return(2*x - 40)
}

f3 = function(x)
{
	return(0.5*x + 40)
}






# lines
#pdf('../plots/multiclass_svm.pdf', width = 16, height = 8)
par(mfrow = c(1,2), lwd = 4, cex = 2, cex.axis = 1.5, cex.lab = 1.4, oma = c(0,0,0,0), mar = c(0,0,0,1))
plot(c(10,55), c(f1(10), f1(55)), type = 'l', xlab = '', ylab = '', xlim = c(-0,100), ylim = c(0,100), xaxt = 'n', yaxt = 'n', col = 'blue')

p.x = c(28,38,53)
p.y = c(54,35,66)
polygon(p.x,p.y, col = 'dark green', density = 15,border = FALSE)

p.x.1 = c(98,70,53)
p.y.1 = c(f3(98),f2(70),66)
polygon(p.x.1,p.y.1, col = 'orange', density = 15,border = FALSE)

p.x.2 = c(37,25,55)
p.y.2 = c(35,f2(25),f1(55))
polygon(p.x.2,p.y.2, col = 'orange', density = 15,border = FALSE)

p.x.2 = c(28,10,5)
p.y.2 = c(54,f1(10),f3(5))
polygon(p.x.2,p.y.2, col = 'orange', density = 15,border = FALSE)

lines(c(10,55),c(f1(10),f1(55)), col = 'blue')
lines(c(25,70),c(f2(25),f2(70)), col = 'blue')
lines(c(5 ,90),c(f3(5 ),f3(90)), col = 'blue')
grid(10,10)

# points
c1 = c()
for(i in c(1:15))
{
	x = sample(c(20:60), 1)
	y = sample(c((max(c(f1(x),f2(x),f3(x)))+5):90),1)
	c1 = rbind(c1, c(x,y))
}
points(c1, col = 'dark red')

c2 = c()
for(i in c(1:20))
{
	x = sample(c(42:90), 1)
	if(x>=38 && x <53) 
	{m = f2(x)-5}
	else 
	{m = f3(x)-5}
	y = sample(c((max(f1(x),5)+2): m),1)
	c2 = rbind(c2, c(x,y))
}
points(c2, col = 'purple')

c3 = c()
for(i in c(1:15))
{
	x = sample(c(5:33), 1)
	if(x>=28 && x <35) 
	{m = f1(x)-2}
	else 
	{m = f3(x)-5}
	y = sample(c((max(f2(x),5)+1): m),1)
	c3 = rbind(c3, c(x,y))
}
points(c3, col = 'red')


text(c(40,90, 5), c(98,5,5), c('Class1','Class2','Class3'),col = c('dark red','purple','red'))

arrows(65, f2(65), 70, f2(70)-13, angle = 20,length = 0.1,col = 'black')
arrows(65, f2(65), 60, f2(60)+13, angle = 20,length = 0.1,col = 'black')
text(70, (f2(70)-13),c('is Class2'),cex = 0.6,pos =1,col ='purple' )
text(60, (f2(60)+13),c('is not Class2'),cex = 0.6,pos = 3,col = 'black')

arrows(85, f3(85), 90, f3(90)-13, angle = 20,length = 0.1,col = 'black')
arrows(85, f3(85), 80, f3(80)+13, angle = 20,length = 0.1,col = 'black')
text(90, (f3(90)-13),c('is not Class1'),cex = 0.6,pos =1,col ='black' )
text(80, (f3(80)+13),c('is Class1'),cex = 0.6,pos = 3,col = 'dark red')

arrows(50, f1(50), 60, f1(40)-13, angle = 20,length = 0.1,col = 'black')
arrows(50, f1(50), 40, f1(60)+13, angle = 20,length = 0.1,col = 'black')
text(60, (f1(40)-13),c('is not Class3'),cex = 0.6,pos =4,col ='black' )
text(40, (f1(60)+13),c('is Class3'),cex = 0.6,pos = 1,col = 'red')


#
#
#


par(mar = c(0,1,0,0))
plot(c(10,37), c(f1(10), f1(37)), type = 'l', xlab = '', ylab = '', xlim = c(-0,100), ylim = c(0,100), xaxt = 'n', yaxt = 'n', col = 'blue')

p.x = c(28,38,53)
p.y = c(54,35,66)
polygon(p.x,p.y, col = 'orange', density = 15,border = FALSE)


lines(c(10,37),c(f1(10),f1(37)), col = 'blue')
lines(c(25,53),c(f2(25),f2(53)), col = 'blue')
lines(c(28,90),c(f3(28),f3(90)), col = 'blue')
grid(10,10)

# points

points(c1, col = 'dark red')

points(c2, col = 'purple')

points(c3, col = 'red')


text(c(40,90, 5), c(98,5,5), c('Class1','Class2','Class3'),col = c('dark red','purple','red'))


arrows(85, f3(85), 90, f3(90)-13, angle = 20,length = 0.1,col = 'black')
arrows(85, f3(85), 80, f3(80)+13, angle = 20,length = 0.1,col = 'black')
text(90, (f3(90)-13),c('is Class2'),cex = 0.6,pos =1,col ='purple' )
text(80, (f3(80)+13),c('is Class1'),cex = 0.6,pos = 3,col = 'dark red')

arrows(30, f2(30), 40, f2(40)-23, angle = 20,length = 0.1,col = 'black')
arrows(30, f2(30), 20, f2(20)+23, angle = 20,length = 0.1,col = 'black')
text(20, (f2(20)+23),c('is Class3'),cex = 0.6,pos =1,col ='red' )
text(40, (f2(40)-23),c('is Class2'),cex = 0.6,pos = 1,col = 'purple')

arrows(18, f1(18), 8, f1(8)-23, angle = 20,length = 0.1,col = 'black')
arrows(18, f1(18), 28, f1(28)+23, angle = 20,length = 0.1,col = 'black')
text(8, (f1(8)-23),c('is Class1'),cex = 0.6,pos =1,col ='red' )
text(28, (f1(28)+23),c('is Class2'),cex = 0.6,pos = 1,col = 'dark red')

dev.off()














