

data = as.matrix(read.table('../data/cancer_bins.ex10'))
label = as.matrix(read.table('../data/cancer_name.txt'))
data = t(data)

result = c()
for(i in c(1:length(data[,1])))
{
	line = data[i,]
	pos = length(line[line == 1])
	#result = rbind(result, c(pos, round((pos/length(line))*100,2)))
	result = rbind(result, c(pos, (length(line)-pos)))
	#result = rbind(result, c(pos, (length(line)-pos), round((pos/length(line))*100,2)))
}
result = cbind(label, result)

write.table(result, '../data/cancer_table.raw', row.names = FALSE, col.names = FALSE)



