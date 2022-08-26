#!/usr/bin/perl -w 

# Copyright>        OpenRadioss
# Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
# Copyright>    
# Copyright>        This program is free software: you can redistribute it and/or modify
# Copyright>        it under the terms of the GNU Affero General Public License as published by
# Copyright>        the Free Software Foundation, either version 3 of the License, or
# Copyright>        (at your option) any later version.
# Copyright>    
# Copyright>        This program is distributed in the hope that it will be useful,
# Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
# Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# Copyright>        GNU Affero General Public License for more details.
# Copyright>    
# Copyright>        You should have received a copy of the GNU Affero General Public License
# Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
# Copyright>    
# Copyright>    
# Copyright>        Commercial Alternative: Altair Radioss Software 
# Copyright>    
# Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
# Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
# Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    

use Data::Dumper;
use File::Basename;

print "\nRunning RADIOSS BLOCK ...\n";

# print "or_radioss.pl args $_\n";
# print "  --debut\n";
# for (@ARGV) {print "  =>$_\n";}
# print "  --fin\n";

$script_name=$0;

$debug_print=0;
if (exists $ENV{'SCRIPTDEBUG'} and $ENV{'SCRIPTDEBUG'} eq "ON") { $debug_print=1; }

my @tmp_argv=@ARGV;
my @tmp_argv_backup=@ARGV;
#my $output_failures_stdout=0;
my $output_xtra_infos=0;
my $batch="";
my $listing_file0="";
my $pass_args="";
my $selfref_pass=-1;
my $chkpt_qa=0;
my $modif=0;
my $submodel;
my $sudmodel_add_file="";
my $submodel_id_shift=1;
my $hwsolvermanager=0;
my $hwsolvermanager_args="";
my $hwsolvermanager_1pass=0;
my $ref_exe=0;
my @self_difftxt_file=();
my $timeoutscript=0;
my $fem_file_path;
my $extract_file_name="RD-qa.extract";
my $id=1;
my $limit_cycles=0;
my $extract_only=0;
my $i_create_extract=0;
# XXXXXX QA ERRORS and WARNINGS
my $i_create_extract_starter=0;
my $selected_values="";
my $last_go=-1;
my $parith_on=0;
my $duplicate_anims2h3d=0;
my $parith_off=0;
my $full_test_id;
# XXXXXX QA ERRORS and WARNINGS
my $extract_from_starter_rules_file;
# END XXXXXX
# XXXXXX Check ERRORS in screen_save (in addition to diff in ref.extract) + Check bounds (ignore diff in ref.extract)
my $ignore_check_errors = 0;
# END XXXXXX

@ARGV=();
while (@tmp_argv) {
  my $my_arg=$tmp_argv[0];
  if    ($my_arg =~ /^--verbose$/ or $my_arg =~ /^--dbgout$/  or $my_arg =~ /^-out$/ )          {
    $debug_print=1;
    shift @tmp_argv;
  }
#  elsif    ($my_arg =~ /^--output_failures_stdout$/)          {
#    $output_failures_stdout=1;
#    shift @tmp_argv;
#  }
  elsif    ($my_arg =~ /^--output_xtra_infos$/)          {
    $output_xtra_infos=1;
    shift @tmp_argv;
  }
  elsif    ($my_arg =~ /^--duplicate_anims2h3d$/)          {
    $duplicate_anims2h3d=1;
    shift @tmp_argv;
  }
  elsif    ($my_arg =~ /^--parith_off$/)          {
    $parith_off=1;
    shift @tmp_argv;
  }
  elsif    ($my_arg =~ /^last_go=/)          {
    if    ($my_arg =~ /^last_go=(.+)$/)          {
      $last_go=$1;
    }
    shift @tmp_argv;
  }
  elsif    ($my_arg =~ /^--extract_only$/)          {
    $extract_only=1;
    $i_create_extract=1;
    shift @tmp_argv;
  }
  elsif    ($my_arg =~ /^--listing$/)          {
    shift @tmp_argv;
    $my_arg=$tmp_argv[0];
    $listing_file0=$my_arg;
    shift @tmp_argv;
  }
  elsif    ($my_arg =~ /^--select_values$/)          {
    shift @tmp_argv;
    $my_arg=$tmp_argv[0];
    $selected_values=$my_arg;
    shift @tmp_argv;
  }
  elsif    ($my_arg =~ /^--timeoutscript=(.+)$/)          {
    $timeoutscript=$1;
    shift @tmp_argv;
  }
  elsif    ($my_arg =~ /^--full_test_id=(.+)$/)          {
    $full_test_id=$1;
    shift @tmp_argv;
  }  
  elsif    ($my_arg =~ /^--fem_file_path=(.+)$/)          {
    $fem_file_path=$1;
    shift @tmp_argv;
  } 
  elsif    ($my_arg =~ /^--self_difftxt_file=(.+)$/)          {
    push @self_difftxt_file,$1;
    shift @tmp_argv;
  }
  elsif    ($my_arg =~ /^--difftxt_file=(.+)$/)          {
    push @difftxt_file,$1;
    $output_xtra_infos=1;
    shift @tmp_argv;
  }
  elsif    ($my_arg =~ /^--scriptpost=(.+)$/)          {
    $scriptpost=$1;
    shift @tmp_argv;
  }
  elsif    ($my_arg =~ /^-pass([0-9]+)$/)          {
    $selfref_pass=$1;
    shift @tmp_argv;
    $my_arg=$tmp_argv[0];
    shift @tmp_argv;
    my @tmp_arg0=split(',',$my_arg);
    for (@tmp_arg0) { 
      if ($_ =~ /^-ref_exe=(.*)/) { 
        $ref_exe=$1 
      } 
      elsif    ($_ =~ /^-parith_on$/)          {
        $parith_on=1;
      }
    }
    my @tmp_arg1=grep {not $_ =~ /^-ref_exe=(.*)/ and not $_ =~ /^-ref_env_(.*)/ and not $_ =~ /^-track_ref_exe/ and not $_ =~ /^-parith_on/} @tmp_arg0;
    push @ARGV,@tmp_arg1;
  }
  else { 
    push @ARGV,$my_arg;
    shift @tmp_argv;
  }
}

if (1==1) {print ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> begin <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";}
if ($debug_print==1) {print ">>>> $0 args=\n";print ">>>> ";print join(' ',@tmp_argv_backup);print "\n";}

# if (grep {$_ eq "--debug_radioss"} @ARGV) {
#   Devel::Trace::trace('on');
# }

# print "or_radioss.pl args $_\n";
# print "  --debut\n";
# for (@ARGV) {print "  =>$_\n";}
# print "  --fin\n";
# print "  verbose=$debug_print\n";
# exit 0;

####################
##  WRAPPER ...   ##
####################
## Initializations ...
my $ismpi=0;
# on ne sait pas encore exactement a quoi ça sert mais c est recupere
my $self_reference=""; 

my $xpath="../../exec";
my $engine="none";
my $engine_short="none";
my $version="none";
my $starter="none";
my $radflex="none";
my $liblmx="none";
my $rad2rad="none";
my $abf_converter="none";
my $mpi_script="none";
my $nspmd=0;
my $nthread=0;
my $nthread_st=1;
my $plateform="linux64";
my $i_xtype=0;
my $xtype="linux64";
my $input="none";
# defined in set_vars
my @exec_script_args=();
my $input1="";

## Initializations end.
my $i_screen_save=0;
$screen_save=sprintf("screen_save_%02d",$i_screen_save);
while (-f $screen_save) {
  if ($i_screen_save > 99) {
    die "No more screen_save_[ii] file available !";
  }
  $i_screen_save++;
  $screen_save=sprintf("screen_save_%02d",$i_screen_save);
}
if (-f $screen_save) {unlink $screen_save}
open(my $fh,">>$screen_save");
if (exists $ENV{'SCRIPTDEBUG'}) {
  print $fh ">>>> $0 ".join(' ',@tmp_argv_backup)."\n";
}
close($fh);

## Read options ...
if ($extract_only == 0) {&set_vars($debug_print);}
$sep_screen="o" x 72;
## After computation
#if ($extract_only == 0) {
#  system("echo $sep_screen >> screen_save;echo $screen_save >> screen_save;cat $screen_save | head -20 | grep -E \"RADIOSS (STARTER|ENGINE)\" >> screen_save;cat $screen_save | tail -100 >> screen_save;");
#}
## Read options end.

# XXXXXX QA ERRORS and WARNINGS
# If the starter extract starter has been asked (starter step + QAERROR set to 1), get the 0.out file and extract error and warning values (call extract_starter)
# The variable is set in set_vars
if ($i_create_extract_starter == 1) {
  my @extract_tab=();

  my $listing_file_starter;
  my $listing_file=$input;

  if ($input =~ /D0[0-9]/) {
    $listing_file =~ s/D0[0-9]//;
    my @tmp2=qx|ls -1tr $listing_file*0.out 2> /dev/null|;chomp(@tmp2);
    $listing_file_starter=$tmp2[-1];
    # print "----------------------\n";
    # system("ls -l $listing_file*");
    # print "----------------------\n";
  }
  elsif ($input =~ /[0-9]\.rad/) {
    $listing_file =~ s/[0-9]\.rad//;
    my @tmp2=qx|ls -1tr $listing_file*0.out 2> /dev/null|;chomp(@tmp2);
    $listing_file_starter=$tmp2[-1];
    # print "----------------------\n";
    # system("pwd; ls -l $listing_file*");
    # print "----------------------\n";  
  }
  elsif ($input =~ /\.k$/ or $input =~ /\.key$/) {
    $listing_file =~ s/\.key$//;
    $listing_file =~ s/\.k$//;
    my @tmp2=qx|ls -1tr $listing_file*0.out 2> /dev/null|;chomp(@tmp2);
    $listing_file_starter=$tmp2[-1];
    # print "----------------------\n";
    # system("pwd; ls -l $listing_file*");
    # print "----------------------\n";  
  }

  if (! defined $ENV{'DO_QA'} or $ENV{'DO_QA'} ne 'ON') {
      # Extracting last line in .out
      print "Extracting lines of $listing_file_starter\n";
      @extract_tab=&extract_starter($debug_print,$listing_file_starter);

      # We create it only if it does not exist with a TIMEOUT line, because if it exists as is, this is because a timeout occured (XXXXXX
      # So we don't want to overwrite it
      my $has_timeouted = 0;
      if (-f $extract_file_name) {
        open(RDFILE,"< $extract_file_name") or die "Error : cannot open input file $extract_file_name !\n";
        my @rules_array = <RDFILE>;
        close(RDFILE);        
        if (grep(/-1\s+TIMEOUT/,@rules_array)) {
          $has_timeouted = 1;
        }
      }

      if (! $has_timeouted) {
        open(FILE,">$extract_file_name") 
          or die "cannot open extract file $extract_file_name for writing !";
        if ($debug_print == 1) { print " --- Extract file : ---\n" }
        for (@extract_tab) {
          print FILE $_;
          if ($debug_print == 1) { print $_;}
        };
        if ($debug_print == 1) { print " --- Extract file end. ---\n" }
        close(FILE);
      }
  }
}

# XXXXXX QA ERRORS and WARNINGS
# We do standard engine 1.out extraction as usual, excepted if the QAERROR env is set (only starter error and warning extractions)
if ($i_create_extract == 1 and !(defined $extract_from_starter_rules_file)) {
  my @extract_tab=();
  my $num=99999;
  if ($input1) {
    $input=$input1;
  }
  if ($scriptpost) {
    print "running : $scriptpost\n";
    system($scriptpost);
  }
  my $listing_file=$input;
  if (-f $screen_save) {
    open(my $fh1,$screen_save);my @out=<$fh1>;close($fh1);
    for (my $i=0;$i<=$#out;$i++) {
      if ($out[$i] =~ /$sep_screen/) {
        my $j=$i-10;
        for ($j=$i-10;$j<=$i+1;$j++) {
          if ($j>=0) {push @errors,$out[$j]}
        }
      }
    }
    for (my $j=$#out-10;$j<=$#out+1;$j++) {
      if ($j>=0) {
  if ($out[$j]) { 
    push @errors,$out[$j];
  }
      }
    }
    @errors=grep {
                  $_ =~ /USER BREAK/
                  or $_ =~ /NOT COMPATIBLE/
                  or $_ =~ /NOT AVAILABLE/
                  or $_ =~ /error/i
                  or $_ =~ /forrtl/i
                  or $_ =~ /exception/i
                  } @out;
    if (@errors) {
      @errors=grep {$_ =~ /RADIOSS STARTER/
                  or $_ =~ /RADIOSS ENGINE/
                  or $_ =~ /USER BREAK/
                  or $_ =~ /NOT COMPATIBLE/
                  or $_ =~ /NOT AVAILABLE/
                  or $_ =~ /$sep_screen/
                  or $_ =~ /error/i
                  or $_ =~ /forrtl/i
                  or $_ =~ /exception/i
                  or $_ =~ /TERMINATION/} @out;
      open($fh1,">>screen_save_errors");
        print $fh1 join('',@errors);
      close($fh1);
    }
  }
  if ($output_xtra_infos) {
    open(my $fh1,"screen_save");my @out=<$fh1>;close($fh1);
    push @xtra_infos,"----------------------------XTRA-INFOS----------------------------\n";
    push @xtra_infos,grep {$_ =~ /ISTOP =/
                  or $_ =~ /TOTAL C.G. ITERATION/
                  or $_ =~ /IMPLICIT COMPUTATION TERMINATED WITH/
                  or $_ =~ /TOTAL LANCZOS ITERATION/
                  or $_ =~ /NOT AVAILABLE/
                  or $_ =~ /DIRECT SOLVER TERMINED WITH RELATIVE/
                  or $_ =~ /NUMBER  CRITICAL LOAD/
                  or $_ =~ /NOT AVAILABLE IN SINGLE PRECISION VERSION/
                  or $_ =~ /SIMPLE PRECISION IS NOT COMPATIBLE WITH/} @out;
    my $i_print=0;
    my @crit_load=();
    for (@out) {
      my $line=$_;
      if ($line =~ /NUMBER  CRITICAL LOAD/) {
        $i_print=1;}
      if ($i_print > 0 and $i_print <=20) {$i_print++;push @crit_load,$line}
    }
    if ($i_print) {push @xtra_infos,@crit_load}
    if (@self_difftxt_file and $selfref_pass == 2) { 
      my $sep='-'x20;
      for my $file (@self_difftxt_file) {
        push @xtra_infos,"$sep\nDiffs txt : $file\n";
        for (my $i=1;$i<=9;$i++) {
          my $num=sprintf("%04d",$i);
          my $file1="$file.$num.pass.1.txt";
          my $file2="$file.$num.pass.2.txt";
          if (-f $file1 or -f $file2) {
            if (-f $file1 and -f $file2) {
                    print "diff $file1 $file2\n";
              my @tmpout=qx(diff $file1 $file2);
              if (@tmpout) {push @xtra_infos,@tmpout;} else { push @xtra_infos,"Ok.\n" }
            }
            elsif (not -f $file1) {
              push @xtra_infos," > !!! ERROR in --self_difftxt_file : $file1 is missing.\n";
            }
            elsif (not -f $file2) {
              push @xtra_infos," > !!! ERROR in --self_difftxt_file : $file2 is missing.\n";
            }
          }
          else {
            push @xtra_infos," > !!! ERROR in --self_difftxt_file : $file1 and $file2 are missing.\n";
          }
        }
      }
    }
    if (@difftxt_file) { 
      my $sep='-'x20;
      for my $file (@difftxt_file) {
        push @xtra_infos,"$sep\nDiffs txt : $file\n";
        my $file1="ref.$file";
        my $file2="$file";
        if ($file eq "outp.txt") {
          if ($last_go==0) {
            print "Generate outp.txt :\n";
            system("../tools/outp.py")
          }
        }
  if (-f $file1 and -f $file2) {
          print "diff $file1 $file2\n";
          my @tmpout=qx(diff $file1 $file2);
          if (@tmpout) {
            push @xtra_infos,@tmpout;
            push @extract_tab_plus,"DIFFS_$file2,1";
          } 
          else { 
            push @xtra_infos,"No diff: ok.\n" 
          }
        }
  elsif (not -f $file1) {
          my $msg=" > !!! ERROR in --difftxt_file : $file1 missing.\n";
          push @extract_tab_plus,"MISSING_$file1,1";
          push @xtra_infos,$msg;
          print $msg;
        }
        elsif (not -f $file2) {
          my $msg=" > !!! ERROR in --difftxt_file : $file2 missing.\n";
          push @extract_tab_plus,"MISSING_$file2,1";
          push @xtra_infos,$msg;
          print $msg;
        }
      }
    }
    push @xtra_infos,"----------------------------XTRA-INFOS-END------------------------\n";
    open($fh1,">screen_xtra_infos");print $fh1 join('',@xtra_infos);close($fh1);
  }
#  grep "ISTOP =" $nom_lis > tempo
#  grep "TOTAL C.G. ITERATION" $nom_lis >> tempo
#  grep "IMPLICIT COMPUTATION TERMINATED WITH" $nom_lis | awk '{gsub("NONLINEAR ","");print;}' >> tempo
#  grep "TOTAL LANCZOS ITERATION" $nom_lis >> tempo
#  grep "DIRECT SOLVER TERMINED WITH RELATIVE" $nom_lis >> tempo
#  # Les AUTRES CAS :
#  ## CAS IMPLICITE/linear/BUCKLING/PLABUCK/* : nom_lis=BOXBEAMF
#  awk '/NUMBER  CRITICAL LOAD/{i_pri=1} {if (i_pri==1){i_lig++};if (i_pri==1 && i_lig<=20) {print}}' $nom_lis >> results4qa
#  # Limitation implicite en SP (affichage dans le 1.out seulement) :
#  grep "NOT AVAILABLE IN SINGLE PRECISION VERSION" $nom_lis >> results4qa
#  grep "SIMPLE PRECISION IS NOT COMPATIBLE WITH" $nom_lis >> results4qa
#  grep "SINGLE PRECISION IS NOT COMPATIBLE WITH" $nom_lis >> results4qa 
  if ($input =~ /D0[0-9]/) {
    $num=substr($input,length($input)-1)+0;
    $listing_file =~ s/D0[0-9]//;
    my @tmp=qx|ls -1tr $listing_file*$num.out 2> /dev/null|;chomp(@tmp);
    $listing_file=$tmp[-1];
  }
  elsif ($input =~ /[0-9]\.rad/) {
    $num=substr($input,length($input)-5,1)+0;
# temporary fix for onefilerad
    if ($num == 0)  {$num=1;}
    $listing_file =~ s/[0-9]\.rad//;
    my @tmp=qx|ls -1tr $listing_file*$num.out 2> /dev/null|;chomp(@tmp);
    $listing_file=$tmp[-1];
  }
  if ($listing_file0) {$listing_file=$listing_file0}
  if ($extract_only == 1) {
    $listing_file=$listing_file0;
  }

  if ( (! defined $ENV{'DO_QA'} or $ENV{'DO_QA'} ne 'ON') and (defined $listing_file and -s $listing_file) ) {
      # Extracting last line in .out
      print "Extracting lines of $listing_file\n";
      @extract_tab=&extract_engine($debug_print,$listing_file);
      for my $ext_val (@extract_tab_plus) {
        my @fields=split(',',$ext_val);
        $tmpout=sprintf "%5d %s %10d\n",$id++,$fields[0],$fields[1];
        push @extract_tab,$tmpout;
      }
      # We create it only if it does not exist with a TIMEOUT line, because if it exists as is, this is because a timeout occured (XXXXXX
      # So we don't want to overwrite it
      my $has_timeouted = 0;
      if (-f $extract_file_name) {
        open(RDFILE,"< $extract_file_name") or die "Error : cannot open input file $extract_file_name !\n";
        my @rules_array = <RDFILE>;
        close(RDFILE);        
        if (grep(/-1\s+TIMEOUT/,@rules_array)) {
          $has_timeouted = 1;
        }
      }

      if (! $has_timeouted) {
        open(FILE,">$extract_file_name") 
          or die "cannot open extract file $extract_file_name for writing !";
        if ($debug_print == 1) { print " --- Extract file : ---\n" }
        for (@extract_tab) {
          print FILE $_;
          if ($debug_print == 1) { print $_;}
        };
        if ($debug_print == 1) { print " --- Extract file end. ---\n" }
        close(FILE);
      }
  }  
}

# XXXXXX Check ERRORS in screen_save (in addition to diff in ref.extract) + Check bounds (ignore diff in ref.extract)
# Check and set error in extract file if present else create it
if (! $ignore_check_errors) {
  # print "\n\n--------------- REF EXTRACT BEFORE ------------------\n";
  # system("cat RD-qa.extract");
  # print "------------------------------------------------\n\n";

  &check_and_set_error_in_extract();

  # print "\n\n--------------- REF EXTRACT AFTER ------------------\n";
  # system("cat RD-qa.extract");
  # print "------------------------------------------------\n\n";
}
# END XXXXXX



####################
##  WRAPPER end.  ##
####################
print "\nDone.\n";
if ($debug_print == 1) { system("cat _ps_killed_*"); }
system("rm -f _ps_killed_*");

if (1==1) {print ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> end <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";}

#===================================================================
## Subroutine functions ...
#===================================================================
# read options 
sub set_vars() {
  my ($debug_print)=@_;
  $ismpi=shift @ARGV;
  my $given_engine=shift @ARGV;
  print "GIVEN ENGINE : $given_engine\n";
  if ($ref_exe and $selfref_pass eq 1) { $given_engine=$ref_exe }
  
  &check_file($debug_print,$given_engine,"engine");
## Set up executables : decode from engine path ...
  my $xsuffix = "undefined";
#   print ">>>> verbose=$debug_print\n";
  if ( $given_engine =~ m|(.*)(/e_)([^_]*_main)(_)(.*)| or $given_engine =~ m|(.*)(/e_)([^_]*)(.*)| ) {
    $xpath = $1;
    $version = $3;
    $xsuffix = $4;
    $sp = '';
    if ($xsuffix =~ /(.*)_sp$/) {
        $xsuffix = $1;
        $sp = '_sp';
    }
    if ($debug_print == 1) { print ">>>> xpath=$xpath\n";
                             print ">>>> version=$version\n";
                             print ">>>> xsuffix=$xsuffix\n";
                             print ">>>> sp=$sp\n";}
    if (not $xsuffix) {die "$script_name error : undefined exe type !"}
    $engine = $given_engine;
#
    $xsuffix=~s/_[a-z0-9]*mpi//;
    $xsuffix=~s/_cuda//;
    $xsuffix=~s/_x_/_/;
    $xsuffix=~s/_avx512//;
    $starter = "$xpath/s_".$version.$xsuffix.$sp;

  }
  elsif ( $given_engine =~ m|(.*)(/engine_)(.*)| ) { # OSS
    $xpath = $1;
    $xsuffix = $3;
    $sp = '';
    if ($xsuffix =~ /(.*)_sp$/) {
        $xsuffix = $1;
        $sp = '_sp';
    }
    if ($debug_print == 1) { print ">>>> xpath=$xpath\n";
                             print ">>>> version=$version\n";
                             print ">>>> xsuffix=$xsuffix\n";
                             print ">>>> sp=$sp\n";}
    if (not $xsuffix) {die "$script_name error : undefined exe type !"}
    $engine = $given_engine;
#
    $xsuffix=~s/_[a-z0-9]*mpi//;
    $xsuffix=~s/_cuda//;
    $xsuffix=~s/_x_/_/;
    $xsuffix=~s/_avx512//;
    $starter = "$xpath/starter_".$xsuffix.$sp;
    $version = '';
  }

  &check_file($debug_print,$starter,"starter");
#
  my $mpi_suffix="";
  if ($engine =~ /_([a-z0-9]*mpi)(.*)/) { $mpi_suffix=$1 }
  $xsuffix =~ s/pgi_//;
  $xsuffix =~ s/_def//;
  if ($mpi_suffix) {
    my $ext="";
    my $pf=$xsuffix;
    if ($xsuffix =~ /(.*)(\.exe)/) {
      $pf=$1;$ext=$2;}
    $rad2rad = "$xpath/r2r_".$version.$pf."_".$mpi_suffix.$ext.$sp;}
  else {
    $rad2rad = "$xpath/r2r_".$version.$xsuffix.$sp;}
  &check_file($debug_print,$rad2rad,"rad2rad",1);
#
  if (not defined $ENV{'RADFLEX_PATH'}) {$ENV{'RADFLEX_PATH'}=$xpath;}
#
  if (substr($xsuffix,1,3) eq "win") {
    $ENV{'PATH'}=$ENV{'PATH'}.':'.$xpath;}
  else {
    $ENV{'LD_LIBRARY_PATH'}=$ENV{'LD_LIBRARY_PATH'}.':'.$xpath;}
#
  if (not defined $ENV{'ABF_PATH'}) {$ENV{'ABF_PATH'}=$xpath;}

## Set up executables : decode from engine path end.
## Just to print what is used ...
    my @exes_used=("starter=$starter",
                   "engine=$engine",
                   "rad2rad=$rad2rad",);
    print "    Exes used :\n";
    for $my_exe (@exes_used) {
      $my_exe =~ m|(.*)(=)(.*)(/)(.*)|;
      printf "%15s = %s\n",$1,$5;
    }
## Just to print what is used end.
## 
    
##
    @ARGV=reverse @ARGV;
    my @arg_2_process=();
    my $i_batch=0;
# split @ARGV in 2 arrays :
# @arg_2_process and @exec_script_args
    while (@ARGV) {
      my $my_arg=$ARGV[0];
      if    ($my_arg =~ /^-starter$/
             or $my_arg =~ /^-starter_batch$/
             or $my_arg =~ /^-rad2rad$/
             or $my_arg =~ /^-madymo$/
             or $my_arg =~ /^-engine$/
             or $my_arg =~ /^-engine_batch$/)          {

        push @arg_2_process,$my_arg;
        shift @ARGV;$my_arg=$ARGV[0];
        push @arg_2_process,$my_arg;
      }
      elsif    ($my_arg =~ /^-starter_batch$/
             or $my_arg =~ /^-engine_batch$/)          {
        push @arg_2_process,$my_arg;
        shift @ARGV;$my_arg=$ARGV[0];
        push @arg_2_process,$my_arg;
        $i_batch=1;
      }
      elsif    ($my_arg =~ /^-chkpt=([0-9]+)$/)          {
        $chkpt_qa=$1;
      }
      elsif    ($my_arg =~ /^-modif$/)          {
        $modif=1;
      }
      elsif    ($my_arg =~ /^-limit_cycles=([0-9]+)$/)          {
        $limit_cycles=$1;
      }
      elsif    ($my_arg =~ /^-hwsolvermanager$/)          {
        $hwsolvermanager = 1;
      }
      elsif    ($my_arg =~ /^-hwsolvermanager_args=(.*)$/)          {
        $hwsolvermanager_args = $1;
      }
      elsif    ($my_arg =~ /^-hwsolvermanager_1pass$/)          {
        $hwsolvermanager_1pass = 1;
      }
      elsif    ($my_arg =~ /^-submodel$/)          {
        # A way to handle old syntax and exit with error message about new syntax
        $submodel = "";
      }
      elsif    ($my_arg =~ /^-submodel=([a-zA-Z_\-\.0-9]+)$/)          {
        $submodel=$1;
      }      
      elsif    ($my_arg =~ /^-submodel_add=([a-zA-Z_\-\.0-9]+)$/)          {
        $submodel_add=$1;
      }   
      # XXXXXX QA ERRORS and WARNINGS
      elsif    ($my_arg =~ /^--extract_from_starter=(.+)$/)          {
        $extract_from_starter_rules_file=$1;
      }
      # END XXXXXX
      # XXXXXX Check ERRORS in screen_save (in addition to diff in ref.extract) + Check bounds (ignore diff in ref.extract)
      # Add new option
      elsif    ($my_arg =~ /^--ignore_check_errors=([0-1])$/)          {
        $ignore_check_errors=$1;
      }
      # END XXXXXX
      elsif    ($my_arg =~ /^-id_shift=([0-9]+)$/)          {
        $submodel_id_shift=$1;
      }
      elsif    ($my_arg =~ /^-nt=([0-9]+)$/)          {
        $nthread=$1;
      }
      elsif    ($my_arg =~ /^-np=([0-9]+)$/)          {
        $nspmd=$1;
      }
      elsif    ($my_arg =~ /^\+p([0-9]+)$/)          {
        $nspmd=$1;
        push @exec_script_args,$my_arg;
      }
      elsif    ($my_arg =~ /^-nt([0-9]+)=([0-9]+)$/)          {
        my $selfie=$1;
        if ($selfref_pass == $selfie) { $nthread=$2; }
      }
      elsif    ($my_arg =~ /^-np([0-9]+)=([0-9]+)$/)          {
        my $selfie=$1;
        if ($selfref_pass == $selfie) { $nspmd=$2; }
      }
      else { 
        push @exec_script_args,$my_arg; 
      }
     shift @ARGV;
    }

    # XXXXXX integrate solver run manager in RD QA
    # If -np and or -nt has been forced in QA.files, and if we run hwsolver (1 pass or 2 passes)
    # we must also use this settings for the hwsolver run
    if ($nspmd and ($hwsolvermanager or $hwsolvermanager_1pass)) {
      if ($hwsolvermanager_args =~ /-np \d+/) {
        $hwsolvermanager_args =~ s/-np \d+/-np $nspmd/;
      }
      else {
        $hwsolvermanager_args .= " -np $nspmd";
      }
    }
    if ($nthread and ($hwsolvermanager or $hwsolvermanager_1pass)) {
      if ($hwsolvermanager_args =~ /-nt \d+/) {
        $hwsolvermanager_args =~ s/-nt \d+/-nt $nthread/;
      }
      else {
        $hwsolvermanager_args .= " -nt $nthread";
      }
    }    

    @exec_script_args=reverse @exec_script_args;
    my @exec_script_args_pre=();
    my @exec_script_args_post=();
    my $postargs_separator="--postargs";
    my $i_post_args=0;
    for (@exec_script_args) {
      my $my_arg=$_;
      if ($my_arg =~ /^--postargs-separator=(.+)/) {
        $postargs_separator="$1";
      }
      elsif ($my_arg eq $postargs_separator) {
        $i_post_args=1;
      }
      else {
        if ($i_post_args == 0) {
          push @exec_script_args_pre,$my_arg;
        }
        elsif ($i_post_args == 1) {
          push @exec_script_args_post,$my_arg;
        }
      }
    }
    if ($i_batch == 1) {
      push @exec_script_args_post,"&";
    }

# print "exec_script_args : \n";
# print "  --debut\n";
# for (@exec_script_args) {print "  =>$_\n";}
# print "  --fin\n";

# Updating exec_script_args
    my $is_mpi_script=0;
    for (@exec_script_args_pre) {
      if (/mpi/) { $is_mpi_script=1; }
    }
    if ( $is_mpi_script == 1 ) {
      for ($i=1;$i<=$#exec_script_args_pre;$i++) {
        if ($exec_script_args_pre[$i] =~ /-np/ 
         or $exec_script_args_pre[$i] =~ /-n/) { 
          if ($nspmd) {
            $exec_script_args_pre[$i+1]=$nspmd;}
          else {
            $nspmd=$exec_script_args_pre[$i+1];}
        }
      }
    }
    my $script_args_starter_pre="";
    my $script_args_starter_post=join(' ',@exec_script_args_post);
    my $script_args_engine_pre=join(' ',@exec_script_args_pre);
    my $script_args_engine_post=join(' ',@exec_script_args_post);
    if ($nthread) { 
      $script_args_starter_post.=" -nt $nthread";
      $script_args_engine_post.=" -nt $nthread";
    }
    if ($nspmd) { 
      $script_args_starter_post.=" -np $nspmd";
#      done in "Updating exec_script_args" few lines above
#      $script_args_engine_post.="-np $nspmd";
    }
    else {
      $script_args_starter_post.=" -np 1";
    }
#     while (@exec_script_args) {
#       my $my_arg=$exec_script_args[0];
#       print "$my_arg\n";
#       if ($my_arg =~ /^--debug_radioss$/) {
#         $debug_print=1;
#         shift @exec_script_args;
#       }
#       elsif ($my_arg =~ /^--debug_radioss$/) {
#         $debug_print=1;
#         shift @exec_script_args;
#       }
#       else { 
#         die("Wrong option $my_arg\n");
#       }
#     }    
# copy radflex et liblmx dans rep loc
    my $file="";my $file1="";
    $file=$radflex; 
    $file1=(split('/',$file))[-1]; 
    if ($file1 ne $file) {
        if (-f $file1) {unlink $file1;}
        system("cp $file .;chmod +x $file1");
    }
    $file=$liblmx; 
    $file1=(split('/',$file))[-1]; 
    if ($file1 ne $file) {
        if (-f $file1) {unlink $file1;}
        system("cp $file .;chmod +x $file1");
    }
    if (not @arg_2_process) {
      $input=$arg_2_process[0];
      &run_engine($debug_print,$engine,$input);
    }
    my $modif_input_en="";
    my $modif_input_st="";
    while (@arg_2_process) {

      my $my_arg=$arg_2_process[0];
      print "> $my_arg\n";
      if ($my_arg =~ /_batch$/) {$batch=" &";}
      if    ($my_arg =~ /^-starter$/ or $my_arg =~ /^-starter_batch$/)          {

        if (not $my_arg =~ /_batch$/ and (defined $extract_from_starter_rules_file) ) {
          # XXXXXX QA ERRORS and WARNINGS
          # Set variable to cause the starter 0.out extraction
          $i_create_extract_starter=1;
        }
        # Remove possible old extract file (XXXXXX just in case of ...
        unlink ($extract_file_name) if (-f $extract_file_name);

        # Force elapsed time to be displayed for starter
        $script_args_starter_post .=  " -timer";
        shift @arg_2_process;
        $input=$arg_2_process[0];
        shift @arg_2_process;

        # We copy all the engine input files while running the starter because the domain decomposition may need information
        # from this file for a better decomposition (and run like chkpt or submodel may have different result when
        # Running a pass without 1.rad and running a pass with 1.rad)
        # See XXXXXX + XXXXXX
        my $disable_copy_opt = $ENV{'DISABLE_COPY_ENGINE_AT_STARTER_EXEC'};
        if (! defined $disable_copy_opt or $disable_copy_opt != 1) {
          # Get input file basename
          my $input_file_basename = $input;
          my $ext;
          if ($input =~ /0\.rad$/) {
            $input_file_basename =~ s/0000\.rad$//g;
            $ext = 'rad';
          }
          elsif ($input =~ /D00$/) {
            $input_file_basename =~ s/D00$//g;
            $ext = 'D';
          }

          if (defined $ext) {
            open($screen_save_fh,">> $screen_save");
            # Identifying all engine related files
            opendir(my $dh, $fem_file_path) or die "Cannot browse $fem_file_path to look for engine files";
            while (my $foundfile = readdir($dh)) {
              ! -d $foundfile and $foundfile ne '.' and $foundfile ne '..' and do {
                 if ( ($ext eq 'rad' and $foundfile =~ /^${input_file_basename}[0-9][0-9][0-9][1-9]\.rad$/) 
                  or ($ext eq 'D' and $foundfile =~ /^${input_file_basename}D[0-9][1-9]$/) ) {
                  system("cp -f $fem_file_path/$foundfile .")
                }
              };

            }
            closedir($dh);
            close($screen_save_fh)
          }

        }
        ## Debug traces to display rad file present in working directory
        # my $result = `ls -l *[0-9][0-9][0-9][0-9].rad *D[0-9][1-9] *.inc 2> /dev/null`;
        # system("echo -e \"Running ($my_arg) at ".time()." TR1 pass $selfref_pass with files: \n$result\" >> /tmp/radioss.traces_rad_files.log");
        # end XXXXXX  + XXXXXX

        if ($my_arg =~ /^-starter$/ and defined $submodel and $selfref_pass == 2) {
          &check_sub_kw();
          if (-f $submodel) {
            print "SUDMODEL run -- starter --\n";
            print "clean potential pending MAINSUB_xxx.rst before submodel run\n";
            system("rm -f MAINSUB_*.rst");

            # We create the first engine rad file now, at starter time, because the domain decomposition may need information
            # from this file for a better decomposition (and run like chkpt or submodel may have different result when
            # Running a pass without 1.rad and running a pass with 1.rad)
            # See XXXXXX
            if (! defined $disable_copy_opt or $disable_copy_opt != 1) {
                # system("echo -e 'STARTER NEW METHOD' >> /tmp/radioss.traces_rad_files.log");
                ## Reading the rules (offset ids)
                my %rules = &read_qa_submodel_engine_offset_id_rules("qa_submodel_engine_offset_id.rules");
                (my $engine_Run_file = $input) =~ s/0000\.rad$/0001\.rad/g;
                $engine_Run_file =~ s/D00$/D01/g;
                if (-f "$fem_file_path/$engine_Run_file") {
                  $modif_input_en=&write_engine_input_file($engine_Run_file,1,"MAINSUB",\%rules);
                  # system("echo -e \"CREATING $modif_input_en\" >> /tmp/radioss.traces_rad_files.log");
                }
            }

            print "**** TRACE SUBMODEL : call modif_input_st=&write_submodel_input_file($input,$submodel_id_shift,$submodel_add,$submodel)\n";
            $modif_input_st=&write_submodel_input_file($input,$submodel_id_shift,$submodel_add,$submodel);

            ## Debug traces to display rad file present in working directory
            # my $result = `ls -l *.rad *D0*`;
            # system("echo -e \"Running STARTER TR2 pass $selfref_pass  with files: \n$result\" >> /tmp/radioss.traces_rad_files.log");
            # end XXXXXX

            &run_starter($debug_print,$starter,$modif_input_st,$script_args_starter_pre,$script_args_starter_post,$batch);
          }
          else { 
            foreach $elem_fh ("STDOUT",$screen_save_fh)  {
              if ($elem_fh ne "STDOUT") { open($screen_save_fh,">> $screen_save"); }
              print $elem_fh "ERROR : The template file '".$submodel."' doesn't exist, use the option -submodel=<qa_submodel file name> !\n";
              if ($elem_fh ne "STDOUT") { close($screen_save_fh); }            
            }
          }
        }
        # XXXXXX integrate solver run manager in RD QA
        # We can run binaries through hwsolver tcl scrip either in one pass (non reg is done against ref.extract file)
        # or in 2 passes (one std run one trhough the hwsolver script) strict non reg
        elsif ($my_arg =~ /^-starter$/ and $hwsolvermanager and $selfref_pass == 2) {
          # Remove possible previous CHECK_DATA file, because the presence generates an error with script
          # Found CHECK_DATA file, specify option -checkpoint or remove CHECK_DATA file
          if (-f 'CHECK_DATA') { unlink('CHECK_DATA'); }

          # We must disable all variable that are used in the tcl script else the script will keep our defined variables.
          # We want that the script set itself the variable to point into its local directory
          &run_hwsolver($debug_print,$input,$hwsolvermanager_args,$batch);
        }
        elsif ($my_arg =~ /^-starter$/ and $hwsolvermanager_1pass) {
          # Remove possible previous CHECK_DATA file, because the presence generate an error with script
          # Found CHECK_DATA file, specify option -checkpoint or remove CHECK_DATA file
          if (-f 'CHECK_DATA') { unlink('CHECK_DATA'); }

          &run_hwsolver($debug_print,$input,$hwsolvermanager_args,$batch);
        }

        else {
          &run_starter($debug_print,$starter,$input,$script_args_starter_pre,$script_args_starter_post,$batch);
        }
      }
      elsif ($my_arg =~ /^-engine$/ or $my_arg =~ /^-engine_batch$/)           {

        ## Debug traces to display rad file present in working directory
        # print "LIST OF INPUT FILE (DEBUG)\n";
        # my $result = `ls -l *[0-9][0-9][0-9][0-9].rad *D[0-9][0-9] *.inc *[0-9][0-9][0-9][1-9].ctl 2> /dev/null`;
        # print "$result\nEND\n";
        # system("echo -e \"Running ($my_arg) with files : \n$result\" >> /tmp/radioss.traces_rad_files.log");

        # XXXXXX Force input .ctl files with u+w permission
        system("chmod u+w *.ctl > /dev/null 2>&1");
        # END XXXXXX

        my $disable_copy_opt = $ENV{'DISABLE_COPY_ENGINE_AT_STARTER_EXEC'};

        shift @arg_2_process;
        $input=$arg_2_process[0];
        shift @arg_2_process;
        my ($root,$number)=&get_rootname($input);
        if (defined $submodel and $selfref_pass == 2) {
          $root="MAINSUB";
        }
        my $number_rst=$number-1;
        if ($number_rst < 0) {$number_rst=0;}
        $number_rst=sprintf("%04d",$number_rst);
        my $rst_file=$root."_".$number_rst."_0001".".rst";
        if (not $my_arg =~ /_batch$/) {
            $i_create_extract=1;
        }
        if (-f $rst_file) {
            if ($my_arg =~ /^-engine$/ and $chkpt_qa > 0 and $selfref_pass == 2) {
                print "CHKPT run\n";
                &write_ctl_file($number,$root,$chkpt_qa,1);
                # Remove possible previous CHECK_DATA file, because model < 99 CYCLE don't generate any CHECK_DATA file
                # we don't want to get the previous one in this case
                if (-f 'CHECK_DATA') { unlink('CHECK_DATA'); }
            }
            if ($my_arg =~ /^-engine$/ and $limit_cycles > 0) {
                print "LIMIT_CYCLES=$limit_cycles\n";
                if ($selfref_pass == 1) {
                  print "LIMIT_CYCLES=$limit_cycles reference run\n";
                }
                if ($selfref_pass == 2) {
                  print "LIMIT_CYCLES=$limit_cycles dbg run\n";
                }
                &write_ctl_file($number,$root,$limit_cycles,0);
            }
            if ($my_arg =~ /^-engine$/ and defined $submodel and $selfref_pass == 2) {
                print "SUDMODEL run -- engine --\n";
                ## Reading the rules (offset ids)
                my %rules = &read_qa_submodel_engine_offset_id_rules("qa_submodel_engine_offset_id.rules");

                # Old method or new method (other engine files than first)
                if ( (defined $disable_copy_opt and $disable_copy_opt == 1) or $number > 1) {
                  # system("echo 'ENGINE USING OLD METHOD OR NUMBER > 1' >> /tmp/radioss.traces_rad_files.log");
                  $modif_input_en=&write_engine_input_file($input,$number,"MAINSUB",\%rules);
                  # system("echo -e \"CREATING $modif_input_en\" >> /tmp/radioss.traces_rad_files.log");
                  $input1=$modif_input_en;
                }
                # New method (first engine file)
                else {
                  # system("echo 'USING NEW METHOD NUMBER == 1' >> /tmp/radioss.traces_rad_files.log");
                  $modif_input_en=$root."_".$number.".rad";
                  $input1=$modif_input_en;
                }

                &run_engine($debug_print,$engine,$modif_input_en,$script_args_engine_pre,$script_args_engine_post,$batch);
            }
            # XXXXXX integrate solver run manager in RD QA
            # skip engine, the hwsolver tcl script (called at starter step) runs both starter and engines
            elsif ($my_arg =~ /^-engine$/ and $hwsolvermanager and $selfref_pass == 2) {
              print ">>>> ABORTING ENGINE RUN, this has already been done in starter pass (HW SOLVER Manager)\n";
            }
            elsif ($my_arg =~ /^-engine$/ and $hwsolvermanager_1pass) {
              print ">>>> ABORTING ENGINE RUN, this has already been done in starter pass (HW SOLVER Manager)\n";
            }

            else {
                if ($duplicate_anims2h3d == 1 or $parith_off == 1) {
                  if ($duplicate_anims2h3d == 1) { &add_h3d_2_engine_input_file($input); }
                  if ($parith_off == 1)          { &switch_engine_input_file_2_poff($input); }
                  &run_engine($debug_print,$engine,$input,$script_args_engine_pre,$script_args_engine_post,$batch);
                }
                else {
  ########  MAIN CASE
                  &run_engine($debug_print,$engine,$input,$script_args_engine_pre,$script_args_engine_post,$batch);
                }
            }
            if ($my_arg =~ /^-engine$/ and $chkpt_qa > 0 and $selfref_pass == 2) {
                print "CHKPT run\n";
                $modif_input_en=&write_engine_input_file($input,$number,$root);
                $input1=$modif_input_en;
                &run_engine($debug_print,$engine,"CHECK_DATA",$script_args_engine_pre,$script_args_engine_post,$batch);
            }
            # Obsolete option
            elsif ($my_arg =~ /^-engine$/ and $modif == 1 and $selfref_pass == 1) {
                print "MODIF reference run\n";
                $modif_input_en=&write_engine_input_file($input,$number+1,$root);
                $input1=$modif_input_en;
                &run_engine($debug_print,$engine,$input1,$script_args_engine_pre,$script_args_engine_post,$batch);
            }
            # Obsolete option
            elsif ($my_arg =~ /^-engine$/ and $modif == 1 and $selfref_pass == 2) {
                print "MODIF modif run\n";
                $modif_input_st=&write_modif_starter_input_file($number+1,$root);
                &run_starter($debug_print,$starter,$modif_input_st,$script_args_starter_pre,$script_args_starter_post,$batch);
                $modif_input_en=&write_engine_input_file($input,$number+2,$root);
                $input1=$modif_input_en;
                &run_engine($debug_print,$engine,$modif_input_en,$script_args_engine_pre,$script_args_engine_post,$batch);
            }
            if (@self_difftxt_file) { 
              for (@self_difftxt_file) {
                my $cmd="mv $_ $_.$number.pass.$selfref_pass.txt";
                if ($debug_print==1) {
                  print "$cmd\n";;
                }
                qx($cmd);
              }
            }
        }
        else {
            print "No restart file found : \n\t$rst_file does not exist\n";
        }
      }
      elsif ($my_arg =~ /^-rad2rad$/ or $my_arg =~ /^-rad2rad_batch$/)           {
        shift @arg_2_process;
        $input=$arg_2_process[0];
        shift @arg_2_process;
        &run_engine($debug_print,$rad2rad,$input,$script_args_engine_pre,$script_args_engine_post,$batch);
        if (not $my_arg =~ /_batch$/) {
            $i_create_extract=1;
        }
      }
      elsif ($my_arg =~ /^-madymo$/)           {
        shift @arg_2_process;
        $input=$arg_2_process[0];
        shift @arg_2_process;
        &run_madymo_engine($debug_print,$engine,$input,$script_args_engine_pre,$script_args_engine_post);
  print "SLEEPING 5 sec after madymo run to avoid MPI server connection issues\n";
  sleep 5;
        $i_create_extract=1;
      }      
      else {
        die("Wrong run option $my_arg\n");
      }
    }
}
#===================================================================
# check if a given exes is a file and executable 
sub check_file() {
  my ($debug_print,$file,$name,$warn)=@_;
  if (not $warn ) {
    if (not -f $file) { die "$script_name error : $name file $file does not exist !"}
    if (not -x $file) { die "$script_name error : $name file $file is not executable !";}
  }
  else {
    if (not -f $file) { warn "$script_name warning : $name file $file does not exist !"}
    if (not -x $file) { warn "$script_name warning : $name file $file is not executable !";}
  }
}
#===================================================================
# run atomic starter
sub run_starter() { 
  my ($debug_print,$starter,$input,$script_pre,$script_post,$batch)=(@_);
  if ($debug_print==1) {print ">>>> script_pre=$script_pre\n";}
  if ($debug_print==1) {print ">>>> script_post=$script_post\n";}
  my $stdout_args="";
  $stdout_args =" 2>&1 | tee -a $screen_save";
  if ($batch) { $stdout_args =" $batch "; } 

  # XXXXXX DBEF QA Stabilization => disable usage of starter dbef, use standard instead
  #☺ if env variable DISABLE_STARTER_DBEF = 1
  if (exists $ENV{'DISABLE_STARTER_DBEF'} and $ENV{'DISABLE_STARTER_DBEF'} == 1) {

    $starter =~ /^(.*\/linux64)(_db)(\/.*)(_dbef)$/ and do {
      print ">>>> > DISABLING STARTER DBEF (XXXXXX )\n";
      $starter = $1.$3;
    };
  }

  my $cmd="$script_pre $starter -i $input $script_post $stdout_args\n";
  open(my $fh,">>$screen_save");print $fh "> $cmd";close($fh);
  if ($timeoutscript > 0) {
    my $pid=$$;
    print ">>>> > perl or_qa_timeout.pl $pid $timeoutscript $extract_file_name &\n";
  }
  print ">>>>  STARTER COMMAND LINE = \n";
  print ">>>>  > ".$cmd;
  if ($timeoutscript > 0) {
    my $pid=$$;
    system("rm -f running_pids 2>/dev/null") if (defined $ENV{'ENABLE_PID_PRINT'} and $ENV{'ENABLE_PID_PRINT'} eq 'ON');
    # system("echo 'rm running_pids (STARTER)' >> /tmp/trace.log") if (defined $ENV{'ENABLE_PID_PRINT'} and $ENV{'ENABLE_PID_PRINT'} eq 'ON');
    system("perl or_qa_timeout.pl $pid $timeoutscript $extract_file_name &");
  }
  system $cmd;
}
#===================================================================
# run hwsolver (starter + engine)
sub run_hwsolver() { 
  my ($debug_print,$input,$hwsolvermanager_args,$batch)=(@_);
  if ($debug_print==1) {print ">>>> hwsolvermanager_args=$hwsolvermanager_args\n";}

  print "\n*** USING HWSOLVER (2 PASS), unsetting some environment variable to let the script set them ***\n";
  my @env2delete = ('KMP_STACKSIZE','HWSOLVERS_TEMPLEX','RAD_H3D_PATH','RAD_CFG_PATH','HM_MSG_DIR','HM_MV_CFG_DIR','HM_MV_UNITS_DIR','ABF_PATH','RADFLEX_PATH');
  for my $elem (@env2delete) {
    print "  - Unsetting $elem \n";
    delete $ENV{$elem};
  }
  print "\n";
  
  my $stdout_args="";
  $stdout_args =" 2>&1 | tee -a $screen_save";
  if ($batch) { $stdout_args =" $batch "; } 

  my $cmd = '"'.$ENV{'TCL_EXE'}.'" "'.$ENV{'TCL_SCRIPT'}.'" -solver RD '.$input.' '.$hwsolvermanager_args;

  open(my $fh,">>$screen_save");print $fh "> $cmd";close($fh);
  if ($timeoutscript > 0) {
    my $pid=$$;
    print ">>>> > perl or_qa_timeout.pl $pid $timeoutscript $extract_file_name &\n";
  }
  print ">>>>  HWSOLVER COMMAND LINE = \n";
  print ">>>>  > ".$cmd;
  if ($timeoutscript > 0) {
    my $pid=$$;
    system("rm -f running_pids 2>/dev/null") if (defined $ENV{'ENABLE_PID_PRINT'} and $ENV{'ENABLE_PID_PRINT'} eq 'ON');
    system("perl or_qa_timeout.pl $pid $timeoutscript $extract_file_name &");
  }
  print "\n";
  system $cmd;

  print ">>>> END run_hwsolver\n";
}

#===================================================================
# run atomic engine
sub run_engine() {
  my ($debug_print,$engine,$input,$script_pre,$script_post,$batch)=(@_);
  if ($debug_print==1) {print ">>>> script_pre=$script_pre\n";}
  if ($debug_print==1) {print ">>>> script_post=$script_post\n";}
  my $stdout_args="";
  $stdout_args =" 2>&1 | tee -a $screen_save";
  if ($batch) { $stdout_args =" $batch "; } 
#  if ($output_failures_stdout) { $stdout_args =" 2>&1 | tee $screen_save"}
  my $opt="-i";
  if ( $engine =~ /r2r/ ) {$opt=""} 
  my $cmd="$script_pre $engine $opt $input $script_post $stdout_args\n";
  if ( $engine =~ /r2r/ ) { $cmd=~s/-notrap//;}
  print ">>>>  script_pre = $script_pre\n";
  $cmd =~ s/==/'/g;
  open(my $fh,">>$screen_save");print $fh "> $cmd";close($fh);
  if ($timeoutscript > 0) {
    my $pid=$$;
    print ">>>> > perl or_qa_timeout.pl $pid $timeoutscript $extract_file_name  &\n";
  }
  print ">>>>  ENGINE COMMAND LINE = \n";
  print ">>>>  > ".$cmd;
  if ($timeoutscript > 0 and $batch eq "") {
    my $pid=$$;
    system("rm -f running_pids 2>/dev/null") if (defined $ENV{'ENABLE_PID_PRINT'} and $ENV{'ENABLE_PID_PRINT'} eq 'ON');
    # system("echo 'rm running_pids (ENGINE)' >> /tmp/trace.log") if (defined $ENV{'ENABLE_PID_PRINT'} and $ENV{'ENABLE_PID_PRINT'} eq 'ON');
    # system("perl or_qa_timeout.pl $pid $timeoutscript $extract_file_name 1 >> /tmp/trace.log &");
    system("perl or_qa_timeout.pl $pid $timeoutscript $extract_file_name &");    
  }
  if ($batch) { 
    my $header=""; 
    if (not -f "runcmd") {$header="#!/bin/sh\n"}; 
    open(RUNCMD,">>runcmd");
    if ($header) {print RUNCMD $header;}
    print RUNCMD $cmd; 
    close(RUNCMD); 
    print ">>>>  > Command stored.\n";
  }
  elsif ($engine =~ /r2r/) { 
    if (-f "runcmd") {
        open(RUNCMD,">>runcmd");print RUNCMD $cmd; close(RUNCMD);
        system "cat runcmd";
        system "sh runcmd";
        unlink("runcmd");
    }
    else {
        print ">>>>  > File runcmd not found.\n";
    }
  }
  else { system $cmd; }
}

#===================================================================
# run atomic engine through madymo CPL
sub run_madymo_engine() {
  my ($debug_print,$engine,$input,$script_pre,$script_post)=(@_);
  if ($debug_print==1) {print ">>>> script_pre=$script_pre\n";}
  if ($debug_print==1) {print ">>>> script_post=$script_post\n";}
  my $stdout_args="";
  $stdout_args =" 2>&1 | tee -a $screen_save";
#  if ($output_failures_stdout) { $stdout_args =" 2>&1 | tee $screen_save"}
  my $opt="-i";
  $ENV{'ENGINE4MADYMO'} = $engine;
  my $cmd=$ENV{'MAD_CPL_SCRIPT'}." -config ./madymo_cpl.conf $script_pre $input $script_post $stdout_args\n";
  print ">>>>  script_pre = $script_pre\n";
  $cmd =~ s/==/'/g;
  open(my $fh,">>$screen_save");print $fh "> $cmd";close($fh);
  if ($timeoutscript > 0) {
    my $pid=$$;
    print ">>>> > perl or_qa_timeout.pl $pid $timeoutscript $extract_file_name  &\n";
  }
  print ">>>>  ENGINE (MADYMO) COMMAND LINE = \n";
  print ">>>>  > ".$cmd;
  if ($timeoutscript > 0) {
    my $pid=$$;
    system("rm -f running_pids 2>/dev/null") if (defined $ENV{'ENABLE_PID_PRINT'} and $ENV{'ENABLE_PID_PRINT'} eq 'ON');
    system("perl or_qa_timeout.pl $pid $timeoutscript $extract_file_name &");
  }

  system $cmd; 
}

# #===================================================================
# # run atomic engine
# sub run_mpi_engine() {
#   my ($debug_print,$engine,$input)=(@_);
#   print " ENGINE => \n";
#   my $cmd="$engine -i $input\n";
#   print $cmd;
#   system $cmd;
# }
#===================================================================
# create extract

# XXXXXX QA ERRORS and WARNINGS
# Function that extracts error and warning information from the 0.out file into a extract_tab array
sub extract_starter($) {
  my ($debug_print,$input_file)=@_;
#----------------------------------
  my @extract_tab=();
  my $tmpout="";
  my @listing=();
  my @error_tab=();
  my $found_error = 0;
  my $extract_separator = '---------------------------------';
  my $s_id = 1;

  # Reading the rules file to know the beginning and end delimiters
  open(my $fhrules,"< $extract_from_starter_rules_file") or die "Error : cannot open input file $extract_from_starter_rules_file !\n";
  my @rules_array = <$fhrules>;
  close($fhrules);
  
  # This hash contains all begin lines and the index of the block information
  my %begins;
  my $begins_idx = 0;
  # This hash contains the block rules information (excepted the begin)
  my @rules;

  my %current_rule;

  foreach $elem (@rules_array) {
    next if ( $elem =~ /^\s*#/ or $elem =~ /^\s*$/);

    # print "Found elem : $elem\n";
    chomp $elem;
    my $var, my $value;
    if ($elem =~ /\s*([^:|\s]+)\s*:\s*\|\|(.*)\|\|/) {
      $var = $1;
      $value = $2;
    }

    # If it is a line beginning with BEGIN, we store possible previous value in temp, and store new one in fresh clean temp
    if ($var eq 'BEGIN') {
      if (%current_rule) {
        # Save rule in array
        push (@rules, {%current_rule}); # Clone hash, else ref is overwritten
        undef %current_rule;
      }
      $begins{$value} = $begins_idx;
      $begins_idx ++;
    }
    $current_rule{$var} = $value;
  }

  # Store last rule in temp
  if (%current_rule) {
    # Save rule in array
    push (@rules,{%current_rule});
  }

  # print "---------------- \%begins -----------------\n";
  # print Dumper %begins;
  # print "------------------------------------------\n";
  # print "---------------- \@rules -----------------\n";
  # print Dumper @rules;
  # print "------------------------------------------\n";
  # exit;

  $tmpout=sprintf "%5d %s %20.13g\n",$s_id,"Start",0;$s_id++;
  push(@error_tab,$tmpout);

  open($fh,$input_file) or print "Warning : cannot open input file $input_file !\n";
  @listing=<$fh>;
  close($fh);

  $tmpout=sprintf "%5d %s %1d\n",$s_id,$extract_separator,0;$s_id++;
  push(@error_tab,$tmpout);

  my $current_rule_idx;

  for (@listing) {
    my $line=$_;
    chomp $line;

    # print "LINE is $line\n";

    $found_error == 0 and do {
      # Checking if the line is one of our BEGIN rule
      for my $elem (keys %begins) {
        if ($line =~ /$elem/) {
          # print "   Found regexp $elem, idx is ".$begins{$elem}."\n";
          $found_error = 1;
          $current_rule_idx = $begins{$elem};
          last;
        }
      }
    };

    $found_error == 1 and do {
      my %temp = %{$rules[$current_rule_idx]};
      my $replace_blank = '';
      $replace_blank = $temp{'REPLACE_BLANK_WITH'} if (defined $temp{'REPLACE_BLANK_WITH'});
      my $end = $temp{'END'};

      if ($line =~ /$end/) {
        # print "  -> end of rule\n";
        $found_error = 0;
        $line = $extract_separator;
      }
      else {
        $line =~ s/ /$replace_blank/g if ($replace_blank ne '');
      }
      $tmpout=sprintf "%5d %s %1d\n",$s_id,$line,0;$s_id++;
      push(@error_tab,$tmpout);      
    };
  }

  push @extract_tab,@error_tab;

  $tmpout=sprintf "%5d %s %20.13g\n",$s_id++,"Stop",0;
  push @extract_tab,$tmpout;

  return @extract_tab;

}


sub extract_engine() {
  my ($debug_print,@input_files)=@_;
#----------------------------------
  my @results=();
  my $last_line="";
  my @extract_tab=();
  my $tmpout="";
  my @listing=();

  # XXXXXX Don't take into account the 2 last columns TOTAL MASS & MASS ADDED if they exist
  my $flag_remove_fields = 0;
  # END XXXXXX

  for $input (@input_files) {
    if (not $input) { next }
    open($fh,$input) or print "Warning : cannot open input file $input !\n";
    @listing=<$fh>;
    for (@listing) {
      my $line=$_;my @fields=split(' ',$line);

      chomp $line;

      # XXXXXX Don't take into account abount 2 last columns TOTAL MASS & MASS ADDED if they exist
      # This is a workaround waiting to be able to deal with the old and new format (option ? rebuild all ref ? ...)
      # If the line begins with CYCLE TIME and ends with TOTAL MASS & MASS ADDED, we set a flag to remove the last fields from the last line
      next if $line =~ /^\s*$/; # Improve loop

      if (!$flag_remove_fields and $line =~ /^\s+CYCLE\s+TIME\s+.*TOTAL MASS\s+MASS ADDED/) {
        $flag_remove_fields = 1;
        next;
      }

      if ( ($flag_remove_fields and $#fields == 12 and $fields[5] =~ /%/) 
        or (! $flag_remove_fields and $#fields == 10 and $fields[5] =~ /%/) ) {

        if ($flag_remove_fields) {
          # In the case where there are 12 fields we rebuild the last lines without the 2 last elements
          $last_line = '';
          for(my $i = 0; $i <= $#fields-2; $i++) {
            if ($i > 0) { $last_line .= ' '; }
            $last_line .= $fields[$i];
          }
        }
        else { $last_line=$line; }
      } 
      # END XXXXXX

    }
    close($fh);
    if ($selfref_pass == 1) {
      qx(mv $input $input"_self_ref_"$selfref_pass);
    }
  }
  chomp $last_line;
  if ($debug_print == 1) {
    print " --- Last line : ---\n";
    print "$last_line\n";
    # XXXXXX Don't take into account abount 2 last columns TOTAL MASS & MASS ADDED if they exist
    if ($flag_remove_fields) {
      print "NOTE: The last columns TOTAL MASS and MASS ADDED have been ignored (XXXXXX )\n";
    }
    # END XXXXXX
    print " --- Last line end. ---\n";
  }
  @results=split(' ',$last_line);
  $tmpout=sprintf "%5d %s %20.13g\n",$id,"Start",0;$id++;
  push @extract_tab,$tmpout;
  @labels=("CYCLE","TIME","TSTEP","ELEMENT","ELTID","ERROR",
              "IENERGY","KENERGYT","KENERGYR","EXTWORK","MASERR","EMAX");
#  $format="%-20.13g";
  my $i=1;
  my @tab_enmax=();
  my @small_labels=("IE","KET","KER","EFW");

  for ($i=6;$i<=9;$i++) {
    push @tab_enmax,[$labels[$i],$results[$i]];
  }
  my $emax= $results[6];
  for (@tab_enmax) {
    if (@{$_}[1] > $emax) {$emax=@{$_}[1]}
  }
  for $field (@results) {
    if ($id == 7) { 
      $field=~s/%//;
    }
    if ($id == 5) { $field = 0; }
## integers    
    if ($id == 2 or $id == 6) {
      $tmpout=sprintf "%5d %s\t%10d\n",$id,$labels[$id-2],$field;
    }
## floats    
    elsif ($id == 3 or $id == 4) {
      $tmpout=sprintf "%5d %s\t%11.4e\n",$id,$labels[$id-2],$field;
    }
## energies    
    elsif (8 <= $id and $id <= 11) {
      $tmpout=sprintf "%5d %s\t%11.4e%11.4e\n",$id,$labels[$id-2],$field,$emax;
    }
## error    
    elsif ($id == 7) {
      $tmpout=sprintf "%5d %s\t%5.1f\n",$id,$labels[$id-2],$field;
    }
    else {
      $tmpout=sprintf "%5d %s\t%s\n",$id,$labels[$id-2],$field;
    }
    if ($selected_values) {
      if (grep {$_ eq $labels[$id-2]} split(',',$selected_values)) {
        push @extract_tab,$tmpout;
      }
    }
    else {
      push @extract_tab,$tmpout;
    }
    $id++;
  }
#   my @tol_en=();
#   print "EMAX=$emax\n";
#   for (@tab_enmax) {
#     if (@{$_}[1] <= 1e-6*$emax) {push @tol_en,['NORM_'.@{$_}[0],@{$_}[1]/$emax]}
#     else {push @tol_en,['ABS_'.@{$_}[0],@{$_}[1]]}
#   }
#   push @tol_en,['EMAX',$emax];
#   for (@tol_en) {
#     $tmpout=sprintf "%5d %s %11.4e\n",$id++,@{$_}[0],@{$_}[1];
#     push @extract_tab,$tmpout;  
#   }
########## 4 implicit ##########
  my @xtra_infos=();
  @xtra_infos=map { if (/ISTOP[\s]*=[\s]*([\S]*)/) { $1 }} grep {$_ =~ /ISTOP =/} @listing;
  if ($#xtra_infos >= 0) {
    $tmpout=sprintf "%5d %s %10d\n",$id++,"ISTOP",$xtra_infos[-1];
    push @extract_tab,$tmpout;}
## ---
  @xtra_infos=map { if (/TOTAL NONLINEAR ITERATIONS:[\s]*([\S]*)/) { $1 }} grep {$_ =~ /IMPLICIT COMPUTATION TERMINATED WITH/} @listing;
  if ($#xtra_infos >= 0) {
    $tmpout=sprintf "%5d %s %10d\n",$id++,"NON_LINEAR_IT",$xtra_infos[-1];
    push @extract_tab,$tmpout;}
## ---
  @xtra_infos=map { if (/TOTAL C.G. ITERATION=[\s]*([\S]*)[\s]*RELATIVE RESIDUAL NORM=[\s]*([\S]*)/) { $1.":".$2 }} grep {$_ =~ /TOTAL C.G. ITERATION/} @listing;
  if ($#xtra_infos >= 0) {
    my @flds=split(':',$xtra_infos[-1]);
    $tmpout=sprintf "%5d %s %20.13g\n",$id++,"TOT_CG_IT",$flds[0];
    push @extract_tab,$tmpout;
    $tmpout=sprintf "%5d %s %20.13g\n",$id++,"REL_RES_NORM",$flds[1];
    push @extract_tab,$tmpout;}
## ---
  @xtra_infos=map { if (/=[\s]*([\S]*)/) { $1 }} grep {$_ =~ /DIRECT SOLVER TERMINED WITH RELATIVE/} @listing;
  if ($#xtra_infos >= 0) {
    $tmpout=sprintf "%5d %s %20.13g\n",$id++,"DIRECT_SOLV_R",$xtra_infos[-1];
    push @extract_tab,$tmpout;}
## ---
  @xtra_infos=map { if (/NUMBER OF BUCKLING CRITICAL LOADS[\s]*([\S]*)/) { $1 }} grep {$_ =~ /NUMBER OF BUCKLING CRITICAL LOADS/} @listing;
  if ($#xtra_infos >= 0) {
    $tmpout=sprintf "%5d %s %20.13g\n",$id++,"CRIT_LOADS",$xtra_infos[-1];
    push @extract_tab,$tmpout;}
## ---
  @xtra_infos=map { if (/=[\s]*([\S]*)/) { $1 }} grep {$_ =~ /ISTOP=/} @listing;
  if ($#xtra_infos >= 0) {
    $tmpout=sprintf "%5d %s %10d\n",$id++,"ISTOP",$xtra_infos[-1];
    push @extract_tab,$tmpout;}
## ---
########## 4 implicit end ######



  $tmpout=sprintf "%5d %s %20.13g\n",$id++,"Stop",0;
  push @extract_tab,$tmpout;
#   print join('',@extract_tab);
  return @extract_tab;
}

sub check_and_set_error_in_extract() {

  # XXXXXX Check ERRORS in screen_save (in addition to diff in ref.extract) + Check bounds (ignore diff in ref.extract)
  # Look for possible error in screen_save_#
  # Excepted if --ignore_check_errors is used
  # Add FOUND_ERROR entry in extract file
  # Search pattern can be restricted to forrtl only if variable RESTRICT_CHECK_ERRORS_TO_FORRTL is used in the QA.file for the test

  my $last_screen_save = `ls -c1 screen_save_[0-9][0-9]* | sort | tail -n 1`;
  chomp $last_screen_save;
  # print "TRACE ------- RESTRICT_CHECK_ERRORS_TO_FORRTL : ".$ENV{'RESTRICT_CHECK_ERRORS_TO_FORRTL'}."\n";

  my $error_pattern = 'ERROR TERMINATION|forrtl\\s*:|BAD TERMINATION';
  if (defined $ENV{'RESTRICT_CHECK_ERRORS_TO_FORRTL'} and $ENV{'RESTRICT_CHECK_ERRORS_TO_FORRTL'} eq 'ON') {
    $error_pattern = 'forrtl\\s*:';
  }

  if (-s $last_screen_save and ! system("grep -qE \"$error_pattern\" $last_screen_save")) {

    (my $screen_save_number = $last_screen_save) =~ s/screen_save_//;

    print "\n !!!!!!!!!!!!!!!!!!! FOUND ERROR from pattern '$error_pattern' in $last_screen_save !!!!!!!!!!!!!!!!!!!\n\n";

    # Insert line in extract file
    my @output;
    my @ref_array;
    if (-f $extract_file_name) {
      # Get last id
      open(RDFILE,"< $extract_file_name") or die "Error : cannot open input file $extract_file_name !\n";
      @ref_array = <RDFILE>;
      chomp @ref_array;
      close(RDFILE);

      # If last elem is Stop, remove it and add it later
      if ($ref_array[$#ref_array] =~ /Stop\s+0\s*$/) {
        pop(@ref_array);
      }

      $new_id_to_insert=1;
      foreach my $elem (reverse @ref_array) {
        $elem =~ /^\s*(\d+)\s+/ and do {
          $new_id_to_insert = $1+1;
          last;
        };
      }
    }
    else {
      $tmpout = sprintf "%5d %s %20.13g\n",$id,"Start",0;$id++;
      push (@output,$tmpout);
      $new_id_to_insert = 2;      
    }

    $tmpout = sprintf "%5d %s %20.13g\n",$new_id_to_insert,"FOUND_ERROR",$screen_save_number;
    push (@output,$tmpout);

    $new_id_to_insert++;
    # (re-)set the Stop at the end
    $tmpout = sprintf "%5d %s %20.13g\n",$new_id_to_insert,"Stop",0;
    push (@output,$tmpout);    

    open(FILE,">$extract_file_name") or die "cannot open extract file $extract_file_name for writing !";
    print FILE join("\n",@ref_array)."\n".join("",@output);
    close(FILE);
  }

}

#===================================================================
## Subroutine functions end.
#===================================================================
#===================================================================
# run classical sequences of exes encountered in qa tests
# sub run_generic() {
#   my ($rad2rad_listing)=(@_);
#   my @inputs=();
#   my $i_rad2rad=0;
#   if ($rad2rad_listing) {$i_rad2rad=1}
#   if (-f "input.dat")   {$i_rad2rad=1}
#   if (not $rad2rad_listing) {
#   for ($i=0;$i<=9;$i++) {my @tmp=qx|ls -1 *$i.rad *D0$i 2> /dev/null|;push @inputs,@tmp}}
#   else { @inputs=qx|ls -1 *[0-9].rad *D0[0-9]| }
#   chomp(@inputs);
#   my @listings=();
#   for (@inputs) {
# #     print "$_\n";
#     my $input_file_name=$_;
#     my $is_starter=1;
#     my $out="=> ".$input_file_name. " ... |";my $len=length($out);
#     print "+";print "-" x $len;print "+\n";
#     print "| $out\n";
#     print "+";print "-" x $len;print "+\n";
#     open(FILE,$_);my @file=<FILE>;close(FILE);if (grep {m|^/RUN|} @file){$is_starter=0};
#     if (   substr($input_file_name,-5) eq "0.rad"
#         or substr($input_file_name,-3) eq "D00"
#         or $is_starter==1) {
#       &runstarter($input_file_name);
#       my @tmp_r2r_files=qx|ls -1 *.r2r 2> /dev/null|;
#       if (@tmp_r2r_files) {$i_rad2rad=1}
#     }
#     else {
#       if ($i_rad2rad==1) {&runengine($input_file_name,"&")}
#       else               {&runengine($input_file_name)    }
#     }
#     my $out_suffix=".out";
#     for ($i=0;$i<=9;$i++) { 
#       if (   substr($input_file_name,-5) eq "$i.rad"
#         or substr($input_file_name,-3) eq "D0$i") {
#         $out_suffix="$i.out";
#       }
#     }
#     my @tmp=qx|ls -1tr *$out_suffix 2> /dev/null|;chomp(@tmp);
#     push @listings,$tmp[-1];
#   }
# #   my @listings=qx|ls -1tr *.out|;chomp(@listings);
# #   if (not $rad2rad_listing) {
# #   for ($i=0;$i<=9;$i++) {my @tmp=qx|ls -1tr *$i.out 2> /dev/null|;push @listings,@tmp}}
# #   else { @inputs=qx|ls -1 *[0-9].rad *D0[0-9]| }
#   my @listing=();
#   for (@listings) {
#     print "$_\n";
#     open(FILE,$_);my @file=<FILE>;close(FILE);
#     push @listing,@file;
#   }
#   open(FILE,">listing_0001.out");for(@listing) { print FILE $_};close(FILE);
# }
sub get_rootname() {
  my($input_name)=@_;
  $input_name =~ s/D([0-9][0-9])$//;
  $input_name =~ s/_([0-9][0-9][0-9][0-9])\.rad$//;
  $number=$1;$number=sprintf("%04d",$number);
  print "$input_name,$number\n";
  return ($input_name,$number);
}

sub write_ctl_file() {
  my ($number,$root,$nbcycle,$opt)=@_;
  # For single input file (starter + engine in 0.rad) force number = 0 to 1
  if ($number == 0) { $number = 1; }
  $number=sprintf("%04d",$number);
  my $file_name=$root."_".$number.".ctl";
  print "ctl_file=$file_name\n";
  open(OUT1,">$file_name");
  print OUT1 "/CYCLE/$nbcycle\n";
  if ($opt == 1) { print OUT1 "/CHKPT\n"; }
  print OUT1 "/STOP\n";
  close(OUT1);
  print "Control file $file_name :\n";
  &print_shifted_file("   >   ",$file_name);
  print "------------------------\n";
}

sub switch_engine_input_file_2_poff () {
  #only $filename is used below normaly ...
  my ($file_name,$number,$root)=@_;
  print "Switch engine input file to /PARITH/OFF ...\n";
  open(INP1,"$file_name");my @file=<INP1>;close(INP1);
  #back up original engine input file 
  system("cp $file_name $file_name"."_origin");
  unlink($file_name);
  open(my $fh1,">$file_name");
  my $i_print_poff=0;
  for my $line (@file) {
    #single file specific case
    if ($line =~ /^\/END\/ENGINE/) {
      print $fh1 "\n/PARITH/OFF\n";
      print $fh1 "/END/ENGINE\n";
      $i_print_poff=1;
    }
    #remove line if /PARITH/ON or /PARITH/OFF already in file
    else {
      if (not $line =~ /^\/PARITH\//) {
        print $fh1 $line;
      }
    }
  }
  #general case : add /PARITH/OFF at the end of the temporary file
  if ($i_print_poff == 0) {
    print $fh1 "\n/PARITH/OFF\n";
  }
  close($fh1);
  print "New engine input file $file_name (original saved into $file_name"."_origin) :\n";
  &print_shifted_file("   >   ",$file_name);
  print "------------------------\n";
}

sub add_h3d_2_engine_input_file() {
  #only $filename is used below normaly ...
  my ($file_name,$number,$root)=@_;
  print "Duplicate /ANIM keywords to /H3D ...\n";
  open(INP1,"$file_name");my @file=<INP1>;close(INP1);
  #back up original engine input file 
  system("cp $file_name $file_name"."_origin");
  my @h3d_lines;
  for (my $i=0;$i<=$#file;$i++) {
    my $line=$file[$i];
    if ($line =~ /^\/ANIM\// and not $line =~ /^\/ANIM\/GZIP/) {
      my $line_h3d=$line;
      $line_h3d =~ s/\/ANIM\//\/H3D\//;
      push @h3d_lines,$line_h3d;
      my $j=$i+1;
      while(not $file[$j] =~ /^\// and $file[$j]) {
        push @h3d_lines,$file[$j];
        $j++;
      }
    }
  }
  unlink($file_name);
  open(my $fh1,">$file_name");
  my $i_print_h3d=0;
  for my $line (@file) {
    if ($line =~ /^\/END\/ENGINE/) {
      for my $h3d_line (@h3d_lines) {
        print $fh1 $h3d_line; 
        $i_print_h3d=1;
      }
      print $fh1 $line;
    }
    else {
      print $fh1 $line;
    }
  }
  if ($i_print_h3d == 0) {
    for my $h3d_line (@h3d_lines) {
      print $fh1 $h3d_line;
    }
  }
  close($fh1);
  print "New engine input file $file_name (original saved into $file_name"."_origin) :\n";
  &print_shifted_file("   >   ",$file_name);
  print "------------------------\n";
}

sub read_qa_submodel_engine_offset_id_rules($) {

    my $input_file = shift;

    return if (! -f $input_file);
    ## Reading the rules (offset ids)
    open(INP,$input_file);my @offset_id_rules=<INP>;close(INP);
    my $cur_card;
    my %rules;

    for my $rule (@offset_id_rules) {

      $rule =~ s/\r\n//; # Remove CRLF terminator if CRLF file
      chomp $rule;

      # Found a new card rule
        $rule =~ /^### CARD (.*)$/ and do {
            my $new_card = $1;
            if (!defined $cur_card or $cur_card ne $new_card) {
              $cur_card = $new_card;
              # print "**** Found card $cur_card \n";
              next;
            }
        };

        # Ignore empty and comment lines
      ($rule =~ /^\s*$/ or $rule =~ /^#/) and do { next; };

        my @rules_split = split(' ',$rule);
        my @modif_lines = split(';',$rules_split[0]);

        for my $elem (@modif_lines) {
          my %hash_temp;

          (my $elements,my $action) = split('\|',$rules_split[1]);
          my @modif_elements = split(';',$elements);

          for my $elem2 (@modif_elements) {
          $hash_temp{$elem2} = $action;
          }
          push (@{$rules{$cur_card}},{$elem => \%hash_temp});
        }
    }

    return %rules;
}

sub write_engine_input_file() {
    my ($file_name,$number,$root,$offset_id_rules)=@_;
    $number=sprintf("%04d",$number);
    my $modif_input_en=$root."_".$number.".rad";
    my %rules;
    %rules = %$offset_id_rules if (defined $offset_id_rules);

    sub _check_card_in_array($$) {
      my $card2check = shift;
      my $array = shift;

      for my $elem (@$array) {
        # Case of a card name base e.g. /VEL/*
        if ($elem =~ /\*$/) {
          my $base_name = $elem;
          # Remove *
          $base_name =~  s/\*$// ;

          # OK if the card begin with the array element
          return 1,$elem if ($card2check =~ /^$base_name/);
        }
        # Case of a full card name e.g. /STATE/DT
        else { 
          return 1,$elem if ($card2check eq $elem);
        }
      }     
      return 0,undef;
    }

    my @card2modify = keys %rules;

    open(INP1,"$file_name");my @file=<INP1>;close(INP1);
    qx(chmod +w $modif_input_en) if (-f $modif_input_en);
    open(OUT1,">$modif_input_en") or die "Cannot open file $modif_input_en for writing !";

    my @card_lines;
    my $flag_new_card = '';
    my $flag_prev_card = 'undef';

    my $nb_lines = scalar(@file);

    # For each line of the original rad file, we do the following
    # If card starts with /RUN
    # /RUN/A/B/C/D => /RUN/A/B
    # /RUN/A/ => /RUN/A
    # If some qa_sumbodel offsed id rules exist we apply them and write the final rad file with modifications

    my $last_line_met = 0;
    my $indice_in_file = 0;

    # For all line (+1 ghost line, with an empty $flag_new_card, to run previous one)
    for (my $indice_in_file = 0; $indice_in_file <= $nb_lines; $indice_in_file ++) {

      # If we are at the last ghost line we see the card as empty to deal with the previous last one
      if ($indice_in_file == $nb_lines) {
        $flag_new_card = '';
      }
      else {
        my $line = $file[$indice_in_file];
        $line =~ s/\r\n//; # Remove CRLF terminator if CRLF file
        chomp $line;

        my $line_result = $line;
          my @tab=split('/',$line);
          my $current_line_is_card = 0;
          if (scalar(@tab) > 1) {
            if (defined $tab[1] and $tab[1] eq "RUN") {
                chomp($tab[0]); 
                chomp($tab[1]); 
                if (! defined $tab[2]) { $tab[2] = ''; }
                else { chomp($tab[2]); }
                if ($tab[3]) {
                    $line_result = $tab[0]."/".$tab[1]."/".$tab[2]."/".$tab[3];
                }
                else {
                  $line_result = $tab[0]."/".$tab[1]."/".$tab[2];
                }
            }

          # We found a card => storing it in a temporary array (to modif with offset id at next card iteration if needed)
          $flag_new_card = $line_result;
          $flag_new_card =~ s/\s*$//; # Remove space or tab at the end
          push(@card_lines,$flag_new_card);
            $current_line_is_card = 1;
        }
        else {
          # This is not a card we store it in the array if needed
          push(@card_lines,$line_result);
        }
      }

      chomp $flag_new_card;

      # Checking if we are at the last line or if we have changed card, if yes we deal with previous one in buffer
      if ($indice_in_file == $nb_lines or ($flag_new_card ne '' and $flag_prev_card ne $flag_new_card)) {

        my $last_elem;
        if ($indice_in_file < $nb_lines) { # we are not at the last line
          # Removing last elem (next card), we want to apply rules only on current card, and last line is the new one
          $last_elem = pop(@card_lines);
        }

        # Check if the card must be modified and get the card name (as stored in the rules structure : may contain * !)
        (my $return_code,my $rule_name) = _check_card_in_array($flag_prev_card,\@card2modify);

        # If the card we deal with is in the temporary array must be modified by the rule
        if ($return_code) {

          my $rules = $rules{$rule_name};

          # For each rule
          for my $rule (@$rules) {

            my @keys = keys(%$rule);
            my $line_number_range = $keys[0]; # We get the first elem, there is only one
            my $value = $$rule{$line_number_range};

            # We store the comment in a hash { position in array => value}, and we will restore the comment after modification
            # We do that because the rule line number is not aware of comment on a line
            my %comment_save;
            my $comment_ind = 0;
            my @tmp_card_lines = @card_lines;
            my $new_ind_card_lines = 0;
            my $card_lines_save_nb_element = scalar(@card_lines);
            @card_lines = ();
            for my $comment_elem (@tmp_card_lines) {

              if ($tmp_card_lines[$comment_ind] =~ /^#/) {
                $comment_save{$comment_ind} = $comment_elem;
              }
              else {
                $card_lines[$new_ind_card_lines] = $tmp_card_lines[$comment_ind];
                $new_ind_card_lines ++;
              }

              $comment_ind ++;
            }

            # For each line of the rule (can be a range)
            while ((my $element_number_range,my $action) = each %$value) {
               my @line_numbers = split(':',$line_number_range);
              # Case there is no range but only one value
              if (scalar (@line_numbers) < 2) { 

                # If the line is 0, it means we want to modify the card options e.g. /DAMP/1 => /DAMP/2
                if ($line_numbers[0] == 0) {

                  # Storing each element in card_element,
                  my @card_elements = split(/\//,$card_lines[0]);
                  my @element_numbers  = split(':',$element_number_range);
                  # Case there is no range but only one value
                  if (scalar (@element_numbers) < 2) { $element_numbers[1] = $element_numbers[0]; }
                  # Case range is until N the end of the lines
                  elsif ($element_numbers[1] eq 'N') { $element_numbers[1] = scalar(@card_elements); }

                  # For each element on the line (in the range)
                  for (my $j = $element_numbers[0]; $j <= $element_numbers[1]; $j ++)
                  {
                    my $indice = $j;
                    if (defined $card_elements[$indice]) {
                      my $result = eval "$card_elements[$indice] $action";
                      $card_elements[$indice] = $result;
                    }
                  }

                  # Rewriting the line with new values
                  $card_lines[0] = join('/',@card_elements);
                  next;
                }

                # Else last line = 1st line, because only one line
                $line_numbers[1] = $line_numbers[0]; 
              }
              # Case range is until N the end of the lines
              elsif ($line_numbers[1] eq 'N') { $line_numbers[1] = scalar(@card_lines); }

              # For each line of the rule (in the range)
              for (my $i = $line_numbers[0]; $i <= $line_numbers[1]; $i ++)
              {
                if (defined $card_lines[$i] and $card_lines[$i] !~ /^#/) {
                  # Storing each element in card_element, saving the spaces between them for restoring later
                  my @card_elements;
                  my @temp = split(//,$card_lines[$i]);

                  my $prev = 'undef';
                  my $regexp_space = '^\s+$';
                  for my $elem (@temp) {
                    if ($prev eq 'undef') { $prev = $elem; }
                    elsif ($prev !~ /$regexp_space/ and $elem !~ /$regexp_space/) {
                      $prev .= $elem;
                    }
                    elsif ($prev !~ /$regexp_space/ and $elem =~ /$regexp_space/) {
                      push (@card_elements,$prev);
                      $prev = $elem;
                    }
                    elsif ($prev =~ /$regexp_space/ and $elem !~ /$regexp_space/) {
                      push (@card_elements,$prev);
                      $prev = $elem;
                    }
                    elsif ($prev =~ /$regexp_space/ and $elem =~ /$regexp_space/) {
                      $prev .= $elem;
                    }
                  }
                  push (@card_elements,$prev) if ($prev ne 'undef');

                  # We store the spaces in a hash { position in array => value}, and we will restore the spaces after modification
                  # We do that because the rule element number is not aware of spaces and we want to restore the exact number of spaces
                  my %space_save;
                  my $space_ind = 0;
                  my @tmp_card_elements = @card_elements;
                  my $new_ind_card_elements = 0;
                  my $card_elements_save_nb_element = scalar(@card_elements);
                  @card_elements = ();
                  for my $space_elem (@tmp_card_elements) {

                    if ($tmp_card_elements[$space_ind] =~ /^\s+/) {
                      $space_save{$space_ind} = $space_elem;
                    }
                    else {
                      $card_elements[$new_ind_card_elements] = $tmp_card_elements[$space_ind];
                      $new_ind_card_elements ++;
                    }
                    $space_ind ++;
                  }

                  my @element_numbers  = split(':',$element_number_range);
                  # Case there is no range but only one value
                  if (scalar (@element_numbers) < 2) { $element_numbers[1] = $element_numbers[0]; }
                  # Case range is until N the end of the lines
                  elsif ($element_numbers[1] eq 'N') { $element_numbers[1] = scalar(@card_elements); }

                  # For each element on the line (in the range)
                  for (my $j = $element_numbers[0]; $j <= $element_numbers[1]; $j ++)
                  {
                      my $indice = $j-1;
                      # We can have a card on the last line, we don't want to deal with this if lines x:N has been asked by previous card
                      my $result = eval "$card_elements[$indice] $action";
                      $card_elements[$indice] = $result;
                  }

                  # Restore the saved comments
                  my @tmp_card_elements2 = @card_elements;
                  @card_elements = ();
                  my $new_ind_card_elements2 = 0;
                  my @keys_space = keys %space_save;
                  for (my $i = 0; $i < $card_elements_save_nb_element; $i ++) {
                      if (grep(/^$i$/,@keys_space)) { # If we had a space at this position we restore it
                        $card_elements[$i] = $space_save{$i};
                      }
                      else { # We get datas
                        $card_elements[$i] = $tmp_card_elements2[$new_ind_card_elements2];
                        $new_ind_card_elements2 ++;
                      }
                  }

                  # Rewriting the line with new values and saved spaces
                  $card_lines[$i] = join('',@card_elements);
                }
              }
            }

            # Restore the saved comments
            my @tmp_card_lines2 = @card_lines;
            @card_lines = ();
            my $new_ind_card_lines2 = 0;
            my @keys_comment = keys %comment_save;

            for (my $i = 0; $i < $card_lines_save_nb_element; $i ++) {
              if (grep(/^$i$/,@keys_comment)) { # If we had a comment at this position we restore it
                $card_lines[$i] = $comment_save{$i};
              }
              else { # We get datas
                $card_lines[$i] = $tmp_card_lines2[$new_ind_card_lines2];
                $new_ind_card_lines2 ++;
              }
            }
          }
        }

        # For each line in the temporary array we write it in the output file
        for my $elem (@card_lines) {
          print OUT1 $elem."\n";
        }

        # Exmpty the temporary array for next iteration
        @card_lines = ();

        # We re-store the next card for next iteration
        push(@card_lines,$last_elem) if (defined $last_elem); 
        $flag_prev_card = $flag_new_card;
      }
    }

    close(OUT1);

    # Usefult for deguggin, keep the original and modified rad files to check for the modifications
    # system("mkdir -p /tmp/submaxx");
    # system("cp $file_name /tmp/submaxx/".$full_test_id."_".$file_name);
    # system("cp $modif_input_en /tmp/submaxx/".$full_test_id."_".basename($file_name)."_MAINSUB");

    # This prints on STDOUT when --use_stdout option is used
    # This prints in the current screen_save to de displayed when option --output_failures_stdout is used
    foreach $elem_fh ("STDOUT",$screen_save_fh)  {
      if ($elem_fh ne "STDOUT") { open($screen_save_fh,">> $screen_save"); }
      print $elem_fh "2nd engine input file $modif_input_en :\n";
      &print_shifted_file("   >   ",$modif_input_en,$elem_fh);
      print $elem_fh "------------------------\n";
      if ($elem_fh ne "STDOUT") { close($screen_save_fh); }
    }

    return $modif_input_en;
}

sub write_modif_starter_input_file() {
  my ($number,$root)=@_;
    $number=sprintf("%04d",$number);
    my $modif_input_st=$root."_".$number.".rad";
    open(OUT1,">$modif_input_st") or die "Cannot open file $modif_input_st for writing !";
        print OUT1 "#RADIOSS STARTER\n";
        print OUT1 "/BEGIN/2\n";
        print OUT1 "$root\n";     
        print OUT1 "     120       0\n"; 
        print OUT1 "\n";                    
        print OUT1 "\n";                      
        print OUT1 "/END\n";
    close(OUT1);
    print "2nd starter input file $modif_input_st :\n";
    &print_shifted_file("   >   ",$modif_input_st);
    print "------------------------\n";
    return $modif_input_st;
}
sub print_shifted_file() {
  my ($shift,$file_name,$outfh)=@_;
  open(INP1,$file_name);my @file=<INP1>;close(INP1);

  if (!defined $outfh) { $outfh = "STDOUT"; }

  for my $line (@file) {
    print $outfh $shift.$line; 
  }

}
sub write_submodel_input_file() {
  my ($input,$id_shift,$add_file,$sub_template)=@_;
  my $id_10=sprintf("%10d",$id_shift);
  my $modif_input_st="MAINSUB_0000.rad";
  my $submodel_version="";
  open(IN1,"$sub_template");my @file=<IN1>;close(IN1);
  open(IN1,$input);my @input1=<IN1>;close(IN1);
  if (substr($input1[0],22,1) eq "4") {
    $submodel_version=substr($input1[0],22,2);} 
  my @begin_block=();
  my $i_pri=0;
  my $i_begin=0;
  for  my $line (@input1) {
    $line=~s/\r//g;                                                          
    if ($line =~ /^\//) {$i_pri=0}
  #exclude include file from constructed new MAINSUB_0000.rad (already include in original 0.rad)
    if ($line =~ /.inc/) {$i_pri=0}
    if ($i_begin > 0) {$i_begin++}
    if ($line =~ /^\/BEGIN/) {$i_pri=1;$i_begin=1;}
    if ($line =~ /^\/DEF_SHELL/) {$i_pri=1}
    if ($line =~ /^\/DEF_SOLID/) {$i_pri=1}
    if ($line =~ /^\/RANDOM/) {$i_pri=1}
    if ($line =~ /^\/ANALY/) {$i_pri=1}
    if ($i_begin == 3) {
      chomp($line);
      $line=sprintf("%-30s",substr($line,0,30))."         1".substr($line,40)."\n";
    }
    if ($i_pri == 1) { push @begin_block,$line }
  }
  open(OUT1,">$modif_input_st") or die "Cannot open file $modif_input_st for writing !";
  for my $line (@file) {
    if ($line =~ /my_input_deck/) {
      $line=~ s/my_input_deck/$input/;
    }
    elsif ($line =~ /version_submodel/) {
      if ($submodel_version) {
        $line=~ s/\/version_submodel/\/V$submodel_version/;}
      else { 
        $line=~ s/\/version_submodel//;}
    }
    elsif ($line =~ /qa_submodel_add/) {
      if ($add_file) {
        $line=~ s/qa_submodel_add/$add_file/;
      }
    }
    elsif ($line =~ /my_begin_block/) {
      $line=""; for (@begin_block) {$line .= $_}
    }
    if ($line =~ /IDSHIFTVAL/) {
      $line=~ s/IDSHIFTVAL/$id_10/;
    }
    print OUT1 $line;
  }
  close(OUT1);

  # This prints on STDOUT when --use_stdout option is used
  # This prints in the current screen_save to de displayed when option --output_failures_stdout is used
  foreach $elem_fh ("STDOUT",$screen_save_fh)  {
    if ($elem_fh ne "STDOUT") { open($screen_save_fh,">> $screen_save"); }
    print $elem_fh "Submodel input file $modif_input_st :\n";
    &print_shifted_file("   >   ",$modif_input_st,$elem_fh);
    print $elem_fh "------------------------\n";

    if ($elem_fh ne "STDOUT") { close($screen_save_fh); }
  }

  return $modif_input_st;
}

sub check_sub_kw() {

  # We are in starter and submodel check pass, we executes the script to detect if the model may have submodel incompatibility

  # This prints on STDOUT when --use_stdout option is used
  # This prints in the current screen_save to de displayed when option --output_failures_stdout is used
  foreach $elem_fh ("STDOUT",$screen_save_fh)  {
    if ($elem_fh ne "STDOUT") { open($screen_save_fh,">> $screen_save"); }

    print $elem_fh "\n##########################\n";
    print $elem_fh "#### SUBMODEL SECTION ####\n";
    print $elem_fh "##########################\n\n";

    # Call the get_sub_kw.pl
    print $elem_fh "***********************************\n";
    print $elem_fh "*** Check SUBMODEL compatiblity ***\n";
    print $elem_fh "***********************************\n\n";

    print $elem_fh "### RUN get_sub_kw_list.pl script to detect possible incompatibilities:\n";

    if ($debug_print == 1) { print $elem_fh "Running ../tools/get_sub_kw_list.pl $fem_file_path\n";}

    if ($elem_fh ne "STDOUT") { close($screen_save_fh); }
  }

  # # We run it only one time    
  system("../tools/get_sub_kw_list.pl $fem_file_path 1>tmp_out_kw 2>&1");
  open(my $fh,"tmp_out_kw");my @tmp=<$fh>;close($fh); 
  
  foreach $elem_fh ("STDOUT",$screen_save_fh)  {
    if ($elem_fh ne "STDOUT") { open($screen_save_fh,">> $screen_save"); }

    # Print the output of the command
    for (@tmp) {print $elem_fh $_;}
    print $elem_fh "*** END Check SUBMODEL compatiblity ***\n\n";

    if ($elem_fh ne "STDOUT") { close($screen_save_fh); }
  }

}
