


######################## begin: Functions ############################3

get_real = function(real.file, out.file)
{
	real = as.matrix(read.table(real.file))
	result = real[(ceiling(length(real[,1])*0.765)+1):length(real[,1]),]
	write.table(result, out.file, row.names = FALSE, col.names = FALSE)
}



measure = function(data.1, data.2)
{
	temp.data = data.1 + data.2
#	cat(1-length(temp.data[temp.data == 0])/length(temp.data),'\n')
	tp = length(temp.data[temp.data ==2])
	tn = length(temp.data[temp.data ==-2])

	temp.data = data.1 - data.2
	fp = length(temp.data[temp.data ==-2])
	fn = length(temp.data[temp.data ==2])

	result.acc = (tp+tn)/(tp+tn+fp+fn)
	result.pre = tp/(tp + fp)
	result.rec = tp/(tp + fn)
	result.f1 = 2*result.pre*result.rec/(result.pre+result.rec)
	return(c(result.acc, result.pre, result.rec, result.f1))
	
}

######################## end: Functions ############################3


############################# dataset .3
data.3.real = as.matrix(read.table('../../data/ncicancer/cancer_bins.3.real'))
data.3.act = as.matrix(read.table('../../data/ncicancer/cancer_acts.3.real'))
data.3.thf = as.matrix(read.table('../../data/ncicancer/result_thf.3'))
data.3.thf.mmcrf = as.matrix(read.table('../../data/ncicancer/result_thf.mmcrf.3'))


pos = c()
for(i in c(0:data.3.act[length(data.3.act)]))
{
	for(j in c(1:length(data.3.act)))
	{
		if(data.3.act[j] == i)
		{pos = c(pos, j);break}
	}
}
pos = c(pos, length(data.3.act))


result.svm = c()
result.mmcrf = c()

for(i in c(1:(length(pos)-1)))
{
#	cat(i,' ')
	data.real.cur = data.3.real[pos[i]:(pos[i+1]-1), ]
	data.svm.cur = data.3.thf[pos[i]:(pos[i+1]-1), ]
	data.mmcrf.cur = data.3.thf.mmcrf[pos[i]:(pos[i+1]-1), ]	

	result.svm = rbind(result.svm, measure(data.real.cur, data.svm.cur))
	result.mmcrf = rbind(result.mmcrf, measure(data.real.cur, data.mmcrf.cur))
}


result.svm.k = c()
result.mmcrf.k = c()

k = 7

lim = floor(length(result.svm[,1])/k)

for(i in c(1:lim))
{
	result.svm.k = rbind(result.svm.k, c(mean(na.rm = TRUE, result.svm[((i-1)*k+1):(i*k),1]),mean(na.rm = TRUE, result.svm[((i-1)*k+1):(i*k),4])))
	result.mmcrf.k = rbind(result.mmcrf.k, c(mean(na.rm=TRUE, result.mmcrf[((i-1)*k+1):(i*k),1]),mean(na.rm = TRUE, result.mmcrf[((i-1)*k+1):(i*k),4])))
}

result.mmcrf.k = rbind(result.mmcrf.k, c(mean(result.mmcrf[(i*k+1):length(result.mmcrf[,1]),1]),mean(result.mmcrf[(i*k+1):length(result.mmcrf[,1]),4])))
result.svm.k = rbind(result.svm.k, c(mean(result.svm[(i*k+1):length(result.svm[,1]),1]),mean(result.svm[(i*k+1):length(result.svm[,1]),4])))


pdf('../../plots/acc-f1-combined-cellline.pdf',height = 24,width = 16)
par(mfrow = c(3,2), lwd = 4, cex.axis = 1.5, cex.lab = 1.4, cex.main = 1.4, oma = c(0,0,0,0), mar = c(4,4,2,1))
plot(result.svm.k[,1], ylim = c(0.3,.9), type = 'b',lty  =3, xlab = 'Number of active cell lines', ylab = 'Accuracy %',pch = 1, col = 'blue', xaxt = 'n' ,cex=2)
grid(10,10, lwd = 3)
lines(cex=2,result.mmcrf.k[,1], col = 'red', type = 'b', lty  =2, pch = 2)
legend(cex=1.5,'bottomright', c('MMCRF', 'SVM'), col = c('red', 'blue'), lty = 2:3, pch = 2:1)
axis(1,c(1:length(result.svm.k[,1])), seq(0,59,k) )

plot(result.svm.k[,2], ylim = c(0.1,.8), type = 'b',lty  =3, xlab = 'Number of active cell lines', ylab = 'F1 score %',pch = 1, col = 'blue', xaxt = 'n' ,cex=2)
grid(10,10, lwd = 3)
lines(cex=2,result.mmcrf.k[,2], col = 'red', type = 'b', lty  =2, pch = 2)
legend(cex=1.5,'bottomright', c('MMCRF', 'SVM'), col = c('red', 'blue'), lty = 2:3, pch = 2:1)
axis(1,c(1:length(result.svm.k[,1])), seq(0,59,k) )







############################# dataset .2
data.2.real = as.matrix(read.table('../../data/ncicancer/cancer_bins.2.real'))
data.2.act = as.matrix(read.table('../../data/ncicancer/cancer_acts.2.real'))
data.2.thf = as.matrix(read.table('../../data/ncicancer/result_thf.2'))
data.2.thf.mmcrf = as.matrix(read.table('../../data/ncicancer/result_thf.mmcrf.2'))


pos = c()
for(i in c(1:data.2.act[length(data.2.act)]))
{
	for(j in c(1:length(data.2.act)))
	{
		if(data.2.act[j] == i)
		{pos = c(pos, j);break}
	}
}
pos = c(pos, length(data.2.act))


result.svm = c()
result.mmcrf = c()

for(i in c(1:(length(pos)-1)))
{
#	cat(i,' ')
	data.real.cur = data.2.real[pos[i]:(pos[i+1]-1), ]
	data.svm.cur = data.2.thf[pos[i]:(pos[i+1]-1), ]
	data.mmcrf.cur = data.2.thf.mmcrf[pos[i]:(pos[i+1]-1), ]	

	result.svm = rbind(result.svm, measure(data.real.cur, data.svm.cur))
	result.mmcrf = rbind(result.mmcrf, measure(data.real.cur, data.mmcrf.cur))
}


result.svm.k = c()
result.mmcrf.k = c()

k = 7

lim = floor(length(result.svm[,1])/k)

for(i in c(1:lim))
{
	result.svm.k = rbind(result.svm.k, c(mean(result.svm[((i-1)*k+1):(i*k),1]),mean(result.svm[((i-1)*k+1):(i*k),4])))
	result.mmcrf.k = rbind(result.mmcrf.k, c(mean(result.mmcrf[((i-1)*k+1):(i*k),1]),mean(result.mmcrf[((i-1)*k+1):(i*k),4])))
}

result.mmcrf.k = rbind(result.mmcrf.k, c(mean(result.mmcrf[(i*k+1):length(result.mmcrf[,1]),1]),mean(result.mmcrf[(i*k+1):length(result.mmcrf[,1]),4])))
result.svm.k = rbind(result.svm.k, c(mean(result.svm[(i*k+1):length(result.svm[,1]),1]),mean(result.svm[(i*k+1):length(result.svm[,1]),4])))



plot(result.svm.k[,1], ylim = c(0.5,.8), type = 'b',lty  =3, xlab = 'Number of active cell lines', ylab = 'Accuracy %',pch = 1, col = 'blue', xaxt = 'n' ,cex=2)
grid(10,10, lwd = 3)
lines(cex=2,result.mmcrf.k[,1], col = 'red', type = 'b', lty  =2, pch = 2)
legend(cex=1.5,'bottomright', c('MMCRF', 'SVM'), col = c('red', 'blue'), lty = 2:3, pch = 2:1)
axis(1,c(1:length(result.svm.k[,1])), seq(1,59,k) )

plot(result.svm.k[,2], ylim = c(0.2,.8), type = 'b',lty  =3, xlab = 'Number of active cell lines', ylab = 'F1 score %',pch = 1, col = 'blue', xaxt = 'n' ,cex=2)
grid(10,10, lwd = 3)
lines(cex=2,result.mmcrf.k[,2], col = 'red', type = 'b', lty  =2, pch = 2)
legend(cex=1.5,'bottomright', c('MMCRF', 'SVM'), col = c('red', 'blue'), lty = 2:3, pch = 2:1)
axis(1,c(1:length(result.svm.k[,1])), seq(1,59,k) )




############################# dataset .1
data.1.real = as.matrix(read.table('../../data/ncicancer/cancer_bins.1.real'))
data.1.act = as.matrix(read.table('../../data/ncicancer/cancer_acts.1.real'))
data.1.thf = as.matrix(read.table('../../data/ncicancer/result_thf.1'))
data.1.thf.mmcrf = as.matrix(read.table('../../data/ncicancer/result_thf.mmcrf.1'))


pos = c()
for(i in c(data.1.act[1]:data.1.act[length(data.1.act)]))
{
	for(j in c(1:length(data.1.act)))
	{
		if(data.1.act[j] == i)
		{pos = c(pos, j);break}
	}
}
pos = c(pos, length(data.1.act))


result.svm = c()
result.mmcrf = c()

for(i in c(1:(length(pos)-1)))
{
#	cat(i,' ')
	data.real.cur = data.1.real[pos[i]:(pos[i+1]-1), ]
	data.svm.cur = data.1.thf[pos[i]:(pos[i+1]-1), ]
	data.mmcrf.cur = data.1.thf.mmcrf[pos[i]:(pos[i+1]-1), ]	

	result.svm = rbind(result.svm, measure(data.real.cur, data.svm.cur))
	result.mmcrf = rbind(result.mmcrf, measure(data.real.cur, data.mmcrf.cur))
}


result.svm.k = c()
result.mmcrf.k = c()

k = 5

lim = floor(length(result.svm[,1])/k)

for(i in c(1:lim))
{
	result.svm.k = rbind(result.svm.k, c(mean(result.svm[((i-1)*k+1):(i*k),1]),mean(result.svm[((i-1)*k+1):(i*k),4])))
	result.mmcrf.k = rbind(result.mmcrf.k, c(mean(result.mmcrf[((i-1)*k+1):(i*k),1]),mean(result.mmcrf[((i-1)*k+1):(i*k),4])))
}

result.mmcrf.k = rbind(result.mmcrf.k, c(mean(result.mmcrf[(i*k+1):length(result.mmcrf[,1]),1]),mean(result.mmcrf[(i*k+1):length(result.mmcrf[,1]),4])))
result.svm.k = rbind(result.svm.k, c(mean(result.svm[(i*k+1):length(result.svm[,1]),1]),mean(result.svm[(i*k+1):length(result.svm[,1]),4])))


plot(result.svm.k[,1], ylim = c(0.4,.8), type = 'b',lty  =3, xlab = 'Number of active cell lines', ylab = 'Accuracy %',pch = 1, col = 'blue', xaxt = 'n' ,cex=2)
grid(10,10, lwd = 3)
lines(cex=2,result.mmcrf.k[,1], col = 'red', type = 'b', lty  =2, pch = 2)
legend(cex=1.5,'bottomright', c('MMCRF', 'SVM'), col = c('red', 'blue'), lty = 2:3, pch = 2:1)
axis(1,c(1:length(result.svm.k[,1])), seq(11,49,k) )

plot(result.svm.k[,2], ylim = c(0.4,.8), type = 'b',lty  =3, xlab = 'Number of active cell lines', ylab = 'F1 score %',pch = 1, col = 'blue', xaxt = 'n' ,cex=2)
grid(10,10, lwd = 3)
lines(cex=2,result.mmcrf.k[,2], col = 'red', type = 'b', lty  =2, pch = 2)
legend(cex=1.5,'bottomright', c('MMCRF', 'SVM'), col = c('red', 'blue'), lty = 2:3, pch = 2:1)
axis(1,c(1:length(result.svm.k[,1])), seq(11,49,k) )



dev.off()
