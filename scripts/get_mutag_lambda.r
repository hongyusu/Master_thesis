

pdf('../plots/result_mutag_lambda_l.pdf', width = 16, height = 8)
par(mfrow = c(1,2), lwd = 4, cex = 1.3, cex.axis = 1.5, cex.lab = 1.5, oma = c(0,0,0,0), mar = c(4.5,4,0,0))

####
data = as.matrix(read.table('../data/mutag/lambda.acc'))
result = data
data = as.matrix(read.table('../data/mutag/lambda.auc'))
result = cbind(result, data)
data = as.matrix(read.table('../data/mutag/lambda.f1'))
result = cbind(result, data)

plot(result[,1], type = 'b',ylim = c(80,98),xlab = 'lambda parameter',ylab = 'performance %',pch = 1,lty = 2, col = 'red',xaxt = 'n')
grid(10,10)
lines(result[,2], type = 'b',pch = 2,lty = 3, col = 'blue')
lines(result[,3], type = 'b',pch = 3,lty = 4, col = 'purple')
legend('topright',c('Accuracy','AUC score','F1 score'),col = c('red','blue','purple'),lty = 2:4,pch = 1:3)
axis(1,1:11,c('0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08','0.09','0.10','0.11'))


####
data = as.matrix(read.table('../data/mutag/l.acc'))
result = data
data = as.matrix(read.table('../data/mutag/l.auc'))
result = cbind(result, data)
data = as.matrix(read.table('../data/mutag/l.f1'))
result = cbind(result, data)

plot(result[,1], type = 'b',ylim = c(80,98),xlab = 'lambda parameter',ylab = 'performance %',pch = 1,lty = 2, col = 'red',xaxt = 'n')
grid(10,10)
lines(result[,2], type = 'b',pch = 2,lty = 3, col = 'blue')
lines(result[,3], type = 'b',pch = 3,lty = 4, col = 'purple')
legend('topright',c('Accuracy','AUC score','F1 score'),col = c('red','blue','purple'),lty = 2:4,pch = 1:3)
axis(1,1:10,c('1','2','3','4','5','6','7','8','9','10'))

dev.off()
