$file=shift;
open(MYFILE,$file) or die "Cannot open file $file !";@myfile=<MYFILE>;
my $dim=0;for (@myfile) {chomp;$_=~s/[\t\s]*$//;if (length($_) and substr($_,0,1) ne "#"){$dim++}};
# print "      CHARACTER*ncharline DIMENSION(:), POINTER :: MESSAGESFILE\n";
print "      CHARACTER*ncharline MESSAGESDATA($dim)\n";
# print "      DATA MESSAGESDATA/\n";
$cel='';
$lini=1;
$maxlini=132;
$lnum=1;
my $tmpc=sprintf("%c",39);
for (@myfile) {
  my $tmpc=sprintf("%c",39);
  if (length($_) and substr($_,0,1) ne "#") {if ($lnum==$lnum){&pline($_,$lnum++);}else{$lnum++;}}
}
# print "     ./\n";
print "      SMESSAGESFILE=$dim\n";
sub pline() {
  my ($strfull,$lnum)=@_;
  my $maxl=$maxlini-$lini;
  my $lastmaxl=$maxl;
  my $lastlen=0;
  print "      MESSAGESDATA(".$lnum.")=\n";
  my @str1=split(chr(39),$strfull);
#   for (@str1) {print $_.":=:";}; print "\n";
  my $ifirst=1;
  for (@str1) {
    $str=$_;
    my $curlen=length($str);
    if ($ifirst==0) {
      printf "%c\n",39;
      printf "     .";
      printf "//";
      printf "%c",39;
      printf "%c",39;
      printf "%c",39;
      printf "%c",39;
      printf "//\n";
#       printf "     .";
#       printf "%c",39;
    }
    if ($curlen==0) {
      printf "     .'";
      $lastlen=length($str);}
    else {
      while ($curlen) {
        my $tmpstr=substr($str,0,$maxl);
        printf "     .";
        if ($maxl<$maxlini) {printf "%s",$cel;};
        if ($maxl<$maxlini or $ifirst == 0) {printf "%c",39;};
        printf "%s",$tmpstr;
        $lastlen=length($tmpstr);
        $str=substr($str,$maxl);
        $curlen=length($str);
        if ($curlen) {printf "\n"};
  #     print "$maxl\n";
        $lastmaxl=$maxl;
        $maxl=$maxlini;
      }
    }
    $ifirst=0;
  }
  if ($lastlen<$lastmaxl) { printf "%c",39;}
  else { printf "\n";printf "     .%c",39;}
  printf "\n"; 
  if (substr($strfull,-1,1) eq "'") {
    printf "     .//";
    printf "%c",39;
    printf "%c",39;
    printf "%c",39;
    printf "%c",39;
    printf "\n";
  }
}

