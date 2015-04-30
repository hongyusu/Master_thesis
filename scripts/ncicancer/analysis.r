

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





######################## begin: main #################################

label = as.matrix(read.table('~/workspace/newdata/cellline_information/nci60_connection.txt'))

label = cbind(as.numeric(label[,1]), label[,2])
label = label[-58,]
cellline.label = label[order(as.numeric(label[,1])),][,2]
cellline.name = cellline.label
for(i in c(1:length(label[,1])))
{
cellline.label[i] = unlist(strsplit(cellline.label[i], ':'))[2]
}

#########################################################


################## dataset 1

get_real('../../data/ncicancer/cancer_bins.1', '../../data/ncicancer/cancer_bins.1.real')
get_real('../../data/ncicancer/cancer_acts.1', '../../data/ncicancer/cancer_acts.1.real')


data.1.real = as.matrix(read.table('../../data/ncicancer/cancer_bins.1.real'))
data.1.thf = as.matrix(read.table('../../data/ncicancer/result_thf.1'))
data.1.tfg = as.matrix(read.table('../../data/ncicancer/result_tfg.1'))
data.1.wdk = as.matrix(read.table('../../data/ncicancer/result_wdk.1'))
data.1.wk = as.matrix(read.table('../../data/ncicancer/result_wk.1'))

result.1.thf = c()
result.1.tfg = c()
result.1.wdk = c()
result.1.wk = c()
for(i in c(1:59))
{
	result.1.thf = rbind(result.1.thf, measure(data.1.real[,i], data.1.thf[,i]))
	result.1.tfg = rbind(result.1.tfg, measure(data.1.real[,i], data.1.tfg[,i]))
	result.1.wdk = rbind(result.1.wdk, measure(data.1.real[,i], data.1.wdk[,i]))
	result.1.wk = rbind(result.1.wk, measure(data.1.real[,i], data.1.wk[,i]))
}


apply(result.1.thf, 2, mean)
apply(result.1.tfg, 2, mean)
apply(result.1.wdk, 2, mean)
apply(result.1.wk, 2, mean)


################## dataset 2

get_real('../../data/ncicancer/cancer_bins.2', '../../data/ncicancer/cancer_bins.2.real')
get_real('../../data/ncicancer/cancer_acts.2', '../../data/ncicancer/cancer_acts.2.real')


data.2.real = as.matrix(read.table('../../data/ncicancer/cancer_bins.2.real'))
data.2.thf = as.matrix(read.table('../../data/ncicancer/result_thf.2'))
data.2.tfg = as.matrix(read.table('../../data/ncicancer/result_tfg.2'))
data.2.wdk = as.matrix(read.table('../../data/ncicancer/result_wdk.2'))
data.2.wk = as.matrix(read.table('../../data/ncicancer/result_wk.2'))

result.2.thf = c()
result.2.tfg = c()
result.2.wdk = c()
result.2.wk = c()
for(i in c(1:59))
{
	result.2.thf = rbind(result.2.thf, measure(data.2.real[,i], data.2.thf[,i]))
	result.2.tfg = rbind(result.2.tfg, measure(data.2.real[,i], data.2.tfg[,i]))
	result.2.wdk = rbind(result.2.wdk, measure(data.2.real[,i], data.2.wdk[,i]))
	result.2.wk = rbind(result.2.wk, measure(data.2.real[,i], data.2.wk[,i]))
}


apply(result.2.thf, 2, mean)
apply(result.2.tfg, 2, mean)
apply(result.2.wdk, 2, mean)
apply(result.2.wk, 2, mean)


################## dataset 3

get_real('../../data/ncicancer/cancer_bins.3', '../../data/ncicancer/cancer_bins.3.real')
get_real('../../data/ncicancer/cancer_acts.3', '../../data/ncicancer/cancer_acts.3.real')


data.3.real = as.matrix(read.table('../../data/ncicancer/cancer_bins.3.real'))
data.3.thf = as.matrix(read.table('../../data/ncicancer/result_thf.3'))
data.3.tfg = as.matrix(read.table('../../data/ncicancer/result_tfg.3'))
data.3.wdk = as.matrix(read.table('../../data/ncicancer/result_wdk.3'))
data.3.wk = as.matrix(read.table('../../data/ncicancer/result_wk.3'))

result.3.thf = c()
result.3.tfg = c()
result.3.wdk = c()
result.3.wk = c()
for(i in c(1:59))
{
	result.3.thf = rbind(result.3.thf, measure(data.3.real[,i], data.3.thf[,i]))
	result.3.tfg = rbind(result.3.tfg, measure(data.3.real[,i], data.3.tfg[,i]))
	result.3.wdk = rbind(result.3.wdk, measure(data.3.real[,i], data.3.wdk[,i]))
	result.3.wk = rbind(result.3.wk, measure(data.3.real[,i], data.3.wk[,i]))
}


apply(result.3.thf, 2, mean)
apply(result.3.tfg, 2, mean)
apply(result.3.wdk, 2, mean)
apply(result.3.wk, 2, mean)














