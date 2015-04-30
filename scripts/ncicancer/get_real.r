


get_real = function(real.file, out.file)
{
	real = as.matrix(read.table(real.file))
	result = real[(ceiling(length(real[,1])*0.765)+1):length(real[,1]),]
	write.table(result, out.file, row.names = FALSE, col.names = FALSE)
}

get_real('../data/cancer_bins.ex0', '../data/cancer_result.real')
get_real('../data/cancer_acts.ex0', '../data/cancer_acts.real')
