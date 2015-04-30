

use strict;
use warnings;

open(IN, "<../data/cancer_table.raw") or die;
open(OUT, ">../data/cancer_table.new") or die;
my $index = 1;
foreach(<IN>)
{
	chomp($_);
	s/"//g;
	s/ /& /g;

	print OUT $_;
	if($index%2 == 1)
	{print OUT ' &'}
	else
	{print OUT " \\\\ \\hline\n"}
	$index ++;
}
print OUT "& & \\\\ \\hline";
close(OUT);
close(IN);

