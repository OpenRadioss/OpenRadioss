my ($file1, $file2) = @ARGV;

open(FILE1,$file1) or die "Cannot open file $file1 !";
open(FILE2,$file2) or die "Cannot open file $file2 !";

while (<FILE1>) {
    print $_;    
}

while (<FILE2>) {
    print $_;    
}




