
library(lattice)


data = as.matrix(read.table('../data/mutag_statistics.txt'),col.names = c('a','b','c'));
data = data.frame(data)
names(data) = c('Class', 'AtomNumber', 'BondNumber')
for(i in c(1:length(data$Class)))
{
	if(data$Class[i] == 1)
	{data$Class[i] = 'Positive'}
	else
	{data$Class[i] = 'Negative'}
}


theme.novpadding <-
   list(layout.heights =
        list(top.padding = 0,
            main.key.padding = 0,
            key.axis.padding = 0,
            axis.xlab.padding = 0,
            xlab.key.padding = 0,
            key.sub.padding = 0,
            bottom.padding = 0),
        layout.widths =
        list(left.padding = 0,
            key.ylab.padding = 0,
            ylab.axis.padding = 0,
            axis.key.padding = 0,
            right.padding = 0),
	fontsize = 
	list(text=30, points = 20))


pdf('../plots/mutag_atom_statistics.pdf', width = 16, height = 8)
histogram(~AtomNumber|Class, data, type="density", breaks=c(6:30), col = 'blue', border = 'grey', par.settings = theme.novpadding, ylab = 'Frequency of molecules', xlab = 'Number of atoms')
dev.off()


pdf('../plots/mutag_bond_statistics.pdf', width = 16, height = 8)
histogram(~BondNumber|Class, data, type="density", breaks=c(6:35), col = 'blue', border = 'grey', par.settings = theme.novpadding, ylab = 'Frequency of molecules', xlab = 'Number of bonds')
dev.off()

