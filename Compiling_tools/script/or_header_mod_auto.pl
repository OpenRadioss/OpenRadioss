#!/usr/bin/perl -w

use File::Find;
use IO::Handle;
use Data::Dumper;
# $dir=shift;
# $routine=shift;

$i_error=0;
$mydir="";
@mydirs=('common_source','starter','engine');
@my_excluded_files=('common_source/modules/unitab_mod.F');
@mydirs_replace=();
sub print_usage () {
    print "Usage $0 update_headers\n";
    print "   in directory .../Radioss/[mainline]/ \n";
    exit 1;
}

if (-f "lock_headers") {
  print "Error : file lock_headers exists\n";
  exit 3;
}

if ($ARGV[0]) {
  if ($ARGV[0] eq "update_headers") {
    @mydirs_replace=@mydirs;
  }
  else {
    @mydirs_replace=@ARGV;
  }
} else { &print_usage(); }
    


# Build subroutines/functions tree
&init();
$current_dir=qx(pwd);chomp($current_dir);
#@mydirs=('common_source','starter','engine');
for $mydir (@mydirs) {
  print "Analysing $mydir ...\n";
  chdir($current_dir.'/'.$mydir);
  &scan();
  print "\n";
}
for $mydir0 (@mydirs_replace) {
  $mydir=(split(':',$mydir0))[0];
  my $file_dir='.';
  my $file1=(split(':',$mydir0))[1];
  if ($file1) { $file_dir=$file1 };
  print "Processing $mydir ...\n";
  chdir($current_dir.'/'.$mydir);
  find(\&Wanted_replace,$file_dir) and die "Cannot find $file_dir";
  print "\n";
}
chdir($current_dir);
if ($i_error) { 
  die "Error(s) : some files wo comments are differents before and after header replacement, no submit"; 
}

sub init () {
%subroutines_sub=();
%subroutines_fun=();
%functions_sub=();
%functions_fun=();
@function_names=();
@fcall_names=();
%subroutines_files=();
%functions_files=();
%subfun_type=();
$lprint_routine=30;
$lprint_file=30;
$sep="="x68;
$header="Chd|";
$sep="$header".$sep;
$nbfiles=0;
$nbfiles_cur=0;
$perc=0;
%files_calls=();
%tag_tree=();
@tree_stack=();
$sub_call=0;
$| = 1;
# $log_filename=">header.log";
# open($LOG,$log_filename) or die "Cannot open $log_filename !";
# $LOG->autoflush;
}

sub scan () {
# Just extract function names to know that to look for ...
print "Look for function names\n";
find(\&Wanted_functions, "./") and die "Cannot find $dir";
# print "Look for module names\n";
# find(\&Wanted_modules, "./") and die "Cannot find $dir";
print "Look for fcall\n";
find(\&Wanted_fcall, "./") and die "Cannot find $dir";
printf "Nb of files : %8d\n",$nbfiles;
# Build subroutines/functions tree
printf "Build tree  : %8d",0;
find(\&Wanted, "./") and die "Cannot find $dir";
print "\n";

@subroutines_def=();
push @subroutines_def,keys %subroutines_sub;
push @subroutines_def,keys %functions_fun;
push @functions_def,keys %subroutines_sub;
push @functions_def,keys %functions_sub;
}
sub print_tree() {
  open (TREE,">tree.txt");
  my %full_call = (); @uniqu_call = grep { ! $full_call{$_} ++ } 
                                 ((map {"aSUBROUTINE:::".$_} keys %subroutines_sub),(map {"aSUBROUTINE:::".$_} keys %subroutines_fun),(map {"aSUBROUTINE:::".$_} keys %subroutines_mod),
                                  (map {"bFUNCTION:::".$_} keys %functions_sub),(map {"bFUNCTION:::".$_} keys %functions_fun),(map {"bFUNCTION:::".$_} keys %functions_mod),
                                  (map {"cMODULE:::".$_} keys %modules_sub),(map {"cMODULE:::".$_} keys %modules_fun),(map {"cMODULE:::".$_} keys %modules_mod));
    my @tab_sub=();
    my @tab_fun=();
    my @tab_mod=();
  for $my_call (sort {$a cmp $b} (@uniqu_call)) {
    if ($my_call) {
    my $type=substr((split(':::',$my_call))[0],1);
    my $call=(split(':::',$my_call))[1];
    if ($call) {
      print TREE "+-------------------------------------------------+\n";
      print TREE "$type : $call \n";
      &header($call);
      print TREE join('',@header);
    }
    }
  }
  my $main_sub="STARTER00";
  $sub_call=0;
  &tree_routine($main_sub,"");
  $main_sub="RADIOSS0";
  $sub_call=0;
  &tree_routine($main_sub,"");
  for $key (keys %files_calls) {
    if (not $tag_tree{$key}) {
      print TREE "!!! ------------- DEAD BRANCH ------------- !!!\n";
      my $sub=(split(':',$key))[0];
      $sub_call=0;
      &tree_routine($sub,"");
      $tag_tree{$sub}=1;}
  }
  close(TREE);
}

sub tree_routine {
  my ($routine,$indent)=@_;
  my @calls=();
  my $cur_sub="";
  push @tree_stack,$routine;
  for $key (keys %files_calls) {
    if ((split(':',$key))[0] eq $routine) {
      $cur_sub=$key;@calls=@{$files_calls{$key}};}
  }
  if ($cur_sub) {
    my @infos=split(':',$cur_sub);
    if ($sub_call == 1) {
    print TREE "$indent|   ($infos[1]:$infos[2])\n";}
    else {
    print TREE "$indent $infos[0]   ($infos[1]:$infos[2])\n";}
    $sub_call=1;
#    $indent.="|   ".$indent;
    for $sub (@calls) {
      my @infos=split(':',$sub);
      print TREE "$indent+-- $infos[1] ($infos[2])\n";
      my $new_indent="|   ".$indent;
      if (not grep {$_ eq $infos[1]} @tree_stack) {
        $tag_tree{$cur_sub}=1;
        &tree_routine($infos[1],$new_indent);
      }
      else { 
#       print "$indent    (recursive calls :".join(',',@tree_stack).")\n";
      $tag_tree{$cur_sub}=1; }
    }
  }
}

sub header() {
  my ($routine)=@_;
  my $tmpstr="";
  if (not $routine) { return; }
  @header=();
  my %sub_files=%subroutines_files;
  my %fun_files=%functions_files;
  my %mod_files=%modules_files;

  my %routine_sub=();
  my %routine_fun=();
  my %routine_mod=();
  my %subroutines=();
  my %functions=();
  my %modules=();
  my $file="";
  if ($subfun_type{$routine} == 1) {
    %routine_sub=%functions_sub;
    %routine_fun=%functions_fun;
    %routine_mod=%functions_mod;
    %subroutines=%subroutines_fun;
    %functions=%functions_fun;
    %modules=%modules_fun;
    if (exists ($fun_files{$routine})) {$file=$fun_files{$routine};}
  }
  elsif ($subfun_type{$routine} == 2) {
    %routine_sub=%subroutines_sub;
    %routine_fun=%subroutines_fun;
    %routine_mod=%subroutines_mod;
    %subroutines=%subroutines_sub;
    %functions=%functions_sub;
    %modules=%modules_sub;
    if (exists ($sub_files{$routine})) {$file=$sub_files{$routine};}
  }
  elsif ($subfun_type{$routine} == 3) {
    %routine_sub=%modules_sub;
    %routine_fun=%modules_fun;
    %routine_mod=%modules_mod;
    %subroutines=%subroutines_mod;
    %functions=%functions_mod;
    %modules=%modules_mod;
    if (exists ($mod_files{$routine})) {$file=$mod_files{$routine};}
  }

  # Print subroutine header
  push @header, "$sep\n";
#   print "file=$file\n";
  my $file1="";
  $file1=(split(':',$file))[1];
  if (substr($file1,0,3) eq "pub") {
    # Securite max
# no calling reference
    $file="";
    $file1="";
    %subroutines=();
    %functions=();
    %modules=();
# no path to routines called
    for $my_key (keys %sub_files) {
        $sub_files{$my_key}="";
    }
    for $my_key (keys %fun_files) {
        $fun_files{$my_key}="";
    }
    for $my_key (keys %mod_files) {
        $mod_files{$my_key}="";
    }
  }
#   elsif (substr($file,0,3) ne "con") {
#   # Securite middle
#     for $my_key (keys %sub_files) {
#       if ( substr($sub_files{$my_key},0,3) eq "con" ) {
#         delete $sub_files{$my_key};
#       }
#     }
#     for $my_key (keys %fun_files) {
#       if ( substr($fun_files{$my_key},0,3) eq "con" ) {
#         delete $fun_files{$my_key};
#       }
#     } 
#     for $my_key (keys %mod_files) {
#       if ( substr($mod_files{$my_key},0,3) eq "con" ) {
#         delete $mod_files{$my_key};
#       }
#     } 
#   }
#   if (substr($file,0,3) eq "pub") {
#     @header=();
#     return;
#   }
  my $routine1="";
  if ( (split(':',$routine))[1] ) { $routine1=(split(':',$routine))[1]; }
  else { $routine1=''; }
  $tmpstr=sprintf "$header  %-".$lprint_routine."s%-".$lprint_file."s\n",$routine1,$file1;
  push @header,$tmpstr;
  push @header, "$header-- called by -----------\n";
  # recherche des routines appelantes
  ## dans les def des subroutines/sub
#   push @header, "$header---+ subroutines -------\n";
  if ($routine1) {
    my @header_called=();
    if ($routine) {
      my $dir1=(split(':',$routine))[0];
      my @ldirs=($dir1);
      if ($dir1 eq 'common_source') {
        # dans le cas d une routine de common source, elle peut être appelée
        # bien evidemment par des routine du starter ou de l engine
        push @ldirs,('starter','engine');
      }
      else {
        # dans le cas d'une routine du starter ou de l'engine 
        # les routines appelantes peuvent être également dans common source
        # cas de upgrade_multimp qui appelle restmod dans le starter ou l'engine
          push @ldirs,'common_source';
      }
      foreach $my_sub (sort keys %subroutines) {
        for my $dir_local (@ldirs) {
          my $prefix_path='';
          if ($dir_local eq 'common_source') {
            $prefix_path='../common_source/';
          }
          if ($dir1 eq 'common_source') {
            $prefix_path=$dir_local.'/';
          }
          my $routine_local=$dir_local.':'.$routine1;
          if (grep {$_ eq $routine_local} @{ $subroutines{$my_sub} }) {
            my $file="";
            if (exists ($sub_files{$my_sub})) {
              $file=$sub_files{$my_sub};
              $file1=(split(':',$file))[1];
              $file1=$prefix_path.$file1;
              $my_sub1=(split(':',$my_sub))[1];
              $tmpstr=sprintf "$header        %-".$lprint_routine."s%-".$lprint_file."s\n",$my_sub1,$file1;
              push @header_called,$tmpstr;
            }
          }
        }
      }
  #   push @header, "$header---+ functions ---------\n";
    ## dans les def des functions/sub
      foreach $my_fun (sort keys %functions) {
        for my $dir_local (@ldirs) {
          my $prefix_path='';
          if ($dir_local eq 'common_source') {
            $prefix_path='../common_source/';
          }
          if ($dir1 eq 'common_source') {
            $prefix_path=$dir_local.'/';
          }
          my $routine_local=$dir_local.':'.$routine1;
          if (grep {$_ eq $routine_local} @{ $functions{$my_fun} }) {
            my $file="";
            if (exists ($fun_files{$my_fun})) {
              $file=$fun_files{$my_fun};
              $file1=(split(':',$file))[1];
              $file1=$prefix_path.$file1;
              $my_fun1=(split(':',$my_fun))[1];
              $tmpstr=sprintf "$header        %-".$lprint_routine."s%-".$lprint_file."s\n",$my_fun1,$file1;
              push @header_called,$tmpstr;
            }
          }
        }
      }
  #   push @header, "$header---+ modules ---------\n";
    ## dans les def des modules/sub
      foreach $my_mod (sort keys %modules) {
        for my $dir_local (@ldirs) {
          my $prefix_path='';
          if ($dir_local eq 'common_source') {
            $prefix_path='../common_source/';
          }
          if ($dir1 eq 'common_source') {
            $prefix_path=$dir_local.'/';
          }
          my $routine_local=$dir_local.':'.$routine1;
          if (grep {$_ eq $routine_local} @{ $modules{$my_mod} }) {
            my $file="";
            if (exists ($mod_files{$my_mod})) {
              $file=$mod_files{$my_mod};
              $file1=(split(':',$file))[1];
              $file1=$prefix_path.$file1;
              $my_mod1=(split(':',$my_mod))[1];
              $tmpstr=sprintf "$header        %-".$lprint_routine."s%-".$lprint_file."s\n",$my_mod1,$file1;
              push @header_called,$tmpstr;
            }
          }
        }
      }
    }
    push @header,grep {$_ =~ / common_source\//} @header_called;
    push @header,grep {$_ =~ /\/common_source\//} @header_called;
    push @header,grep {$_ =~ / starter\//} @header_called;
    push @header,grep {$_ =~ / engine\//} @header_called;
    push @header,grep {$_ !~ / common_source\//
                  and $_ !~ /\/common_source\//
                  and $_ !~ / starter\//
                  and $_ !~ / engine\//} @header_called;
  }
  push @header, "$header-- calls ---------------\n";
#   push @header, "$header---+ subroutines -------\n";
  # recherche des routines appelees
  ## subroutines
      foreach $my_call (@{ $routine_sub{$routine} }) {
        my $file="";
        my $dir1=(split(':',$my_call))[0];
        my @ldirs=($dir1);
        if ($dir1 eq 'common_source') {
          # dans le cas d une routine de common source, elle peut être appelée
          # bien evidemment par des routine du starter ou de l engine
          push @ldirs,('starter','engine');
        }
        else {
          # dans le cas d'une routine du starter ou de l'engine 
          # les routines appelantes peuvent être également dans common source
          # cas de upgrade_multimp qui appelle restmod dans le starter ou l'engine
            push @ldirs,'common_source';
        }
        my $my_call_short=(split(':',$my_call))[1];
        for my $dir_local (@ldirs) {
          my $my_call_local=$dir_local.':'.$my_call_short;
          if (exists ($sub_files{$my_call_local})) {
            my $prefix_path='';
            if ($dir1 ne 'common_source' and $dir_local eq 'common_source') {
              $prefix_path='../common_source/';
            }
            if ($dir1 eq 'common_source' and $dir_local ne 'common_source') {
              $prefix_path=$dir_local.'/';
            }
            $file=$sub_files{$my_call_local};
            if ( (split(':',$file))[1] ) { $file1=$prefix_path.(split(':',$file))[1]; }
            else { $file1=$prefix_path; }
            $my_call1=(split(':',$my_call_local))[1];
            $tmpstr=sprintf "$header        %-".$lprint_routine."s%-".$lprint_file."s\n",$my_call1,$file1;
            if ( not ($file =~ /tools\/mumps5_seq\/dummy_mpi\.F/)) {
              push @header,$tmpstr;
            }
          }
        }
      }
#         push @header, "$header---+ functions ---------\n";
  ## functions
      foreach $my_call (@{ $routine_fun{$routine} }) {
        my $file="";
        my $dir1=(split(':',$my_call))[0];
        my @ldirs=($dir1);
        if ($dir1 eq 'common_source') {
          # dans le cas d une routine de common source, elle peut être appelée
          # bien evidemment par des routine du starter ou de l engine
          push @ldirs,('starter','engine');
        }
        else {
          # dans le cas d'une routine du starter ou de l'engine 
          # les routines appelantes peuvent être également dans common source
          # cas de upgrade_multimp qui appelle restmod dans le starter ou l'engine
            push @ldirs,'common_source';
        }
        my $my_call_short=(split(':',$my_call))[1];
        for my $dir_local (@ldirs) {
          my $my_call_local=$dir_local.':'.$my_call_short;
          if (exists ($fun_files{$my_call_local})) {
            my $prefix_path='';
            if ($dir1 ne 'common_source' and $dir_local eq 'common_source') {
              $prefix_path='../common_source/';
            }
            if ($dir1 eq 'common_source' and $dir_local ne 'common_source') {
              $prefix_path=$dir_local.'/';
            }
            $file=$fun_files{$my_call_local};
            if ( (split(':',$file))[1] ) { $file1=$prefix_path.(split(':',$file))[1]; }
            else { $file1=$prefix_path; }
            $my_call1=(split(':',$my_call_local))[1];
            $tmpstr=sprintf "$header        %-".$lprint_routine."s%-".$lprint_file."s\n",$my_call1,$file1;
            if ( not ($file =~ /tools\/mumps5_seq\/dummy_mpi\.F/)) {
              push @header,$tmpstr;
            }
          }
        }
      }
#         push @header, "$header---+ modules -----------\n";
#   ## modules
      foreach $my_call (@{ $routine_mod{$routine} }) {
        my $file="";
        my $dir1=(split(':',$my_call))[0];
        my @ldirs=($dir1);
        if ($dir1 eq 'common_source') {
          # dans le cas d une routine de common source, elle peut être appelée
          # bien evidemment par des routine du starter ou de l engine
          push @ldirs,('starter','engine');
        }
        else {
          # dans le cas d'une routine du starter ou de l'engine 
          # les routines appelantes peuvent être également dans common source
          # cas de upgrade_multimp qui appelle restmod dans le starter ou l'engine
            push @ldirs,'common_source';
        }
        my $my_call_short=(split(':',$my_call))[1];
        for my $dir_local (@ldirs) {
          my $my_call_local=$dir_local.':'.$my_call_short;
          if (exists ($mod_files{$my_call_local})) {
            my $prefix_path='';
            if ($dir1 ne 'common_source' and $dir_local eq 'common_source') {
              $prefix_path='../common_source/';
            }
            if ($dir1 eq 'common_source' and $dir_local ne 'common_source') {
              $prefix_path=$dir_local.'/';
            }
            $file=$mod_files{$my_call_local};
            if ( (split(':',$file))[1] ) { $file1=$prefix_path.(split(':',$file))[1]; }
            else { $file1=$prefix_path; }
            $my_call1=(split(':',$my_call_local))[1];
            $tmpstr=sprintf "$header        %-".$lprint_routine."s%-".$lprint_file."s\n",$my_call1,$file1;
            if ( not ($file =~ /tools\/mumps5_seq\/dummy_mpi\.F/)) {
              push @header,$tmpstr;
            }
          }
        }
      }
#   for my $existing_headers (@chd_header_mod) {
#     if (not grep {$_ eq $existing_headers} @header) {
#       push @header,$existing_headers;
#     }
#   }
  push @header, "$sep\n";
}

# close($LOG);

# Input : file full path
# Output : 1 if the file must be excluded from browsing, 0 else
sub must_file_be_excluded($) {

  my $file2check = shift;
  $file2check =~ s/\.\///;
  $file2check = $mydir.'/'.$file2check;

  if (grep(/^$file2check$/,@my_excluded_files)) {
    return 1;
  }
  
  return 0;
}

sub Wanted
{
# Only .F files are accounted for
  /\.F$/ or return;

  # Check if the file is part of excluded files
  return if (must_file_be_excluded($File::Find::name));

  $nbfiles_cur++;
  my $file_name_clean=substr($File::Find::name,2);
  if (substr($File::Find::name,0,3) eq "../") {
    $file_name_clean=$File::Find::name;
  }
  if ((($nbfiles_cur % 100) == 0) or ($nbfiles_cur eq $nbfiles)) {
  printf "\rBuild tree  : %8d -> %-50s",$nbfiles_cur,$file_name_clean;}
  $perc=int($nbfiles_cur/$nbfiles*100);
# Store file
  $file_name=$_;
  open (FILE, $file_name);
  my @tabfile=<FILE>;
  close(FILE);
  push @tabfile,"        SUBROUTINE dump_last_routine(empty)";
# Build routine/function tree
  $my_routine="none";
  @tmp_fun=();
  @tmp_sub=();
  @tmp_mod=();
  my $ligne=0;
  my $i_prevfunsub=0;
  my $i_funsub=0;
  my $i_interface=0;
  my $routine_line="$my_routine:$file_name_clean:$ligne";
  for (@tabfile) {
    my $curline=$_;
    chomp($curline);
    $ligne++;
    if ($i_interface==1) {
      if ($curline =~ /^[^Cc#!]([\s\t]*)END([\s\t]+)INTERFACE([\s\t]*)$/) {
        $i_interface=0;
      }
      else { next; }
    }
    if ($curline =~ /^[^Cc#!]([\s\t]*)INTERFACE([\s\t]*)$/) { 
      $i_interface=1;
      next;
    }
# Check if current line match subroutine/function header
    if ($curline =~ /^[^Cc#!]([\s\t]*)END[\s\t_]*FUNCTION/) {next}
    if ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]+)FUNCTION[\s\t]*/
     or $curline =~ /^([\s\t]{5})([.])([\s\t]*)FUNCTION /) {  
#       print "curline=$curline\n";
      if ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]+)(FUNCTION)[\s\t]+([^\(\s\t]*)[\s\t]*\(/) { 
          $prev_routine=$mydir.":".$my_routine;
          $my_routine=$5;
          $i_funsub=1;
          $routine_line="$my_routine:$file_name_clean:$ligne";
      }
      elsif ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]+)(FUNCTION)[\s\t]+([^\(\s\t]+)[\s\t]*/) { 
          $prev_routine=$mydir.":".$my_routine;
          $my_routine=$5;
          $i_funsub=1;
          $routine_line="$my_routine:$file_name_clean:$ligne";
      }
      elsif ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]*)(FUNCTION)[\s\t]*/) { 
        $i=0;
        while ($tabfile[$ligne+$i] =~ /^[Cc#!*]/) {$i++};
        if ($tabfile[$ligne+$i] =~ /^([\s\t]{5})([.])([\s\t]*)([^\(\s\t]+)/) { 
          $prev_routine=$mydir.":".$my_routine;
          $my_routine=$4;
          $i_funsub=1;
          $routine_line="$my_routine:$file_name_clean:$ligne";
        }
      }
    }
    elsif ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]+)FUNCTION[\s\t]+([^\(\s\t]*)/) {  
      $prev_routine=$mydir.":".$my_routine;
      $my_routine=$4;
#       print "$File::Find::name $my_routine\n";
      $i_funsub=1;
      $routine_line="$my_routine:$file_name_clean:$ligne";}
    elsif ($curline =~ /^[^Cc#!]([\s\t]*)(SUBROUTINE)[\s\t]+([^\(\s\t]*)/){
      $prev_routine=$mydir.":".$my_routine;
      $my_routine=$3;
      $i_funsub=2;
      $routine_line="$my_routine:$file_name_clean:$ligne";}
    elsif ($curline =~ /^[^Cc#!]([\s\t]*)(MODULE)[\s\t]+([^\(\s\t]*)/){
      $prev_routine=$mydir.":".$my_routine;
      $my_routine=$3;
      $i_funsub=3;
      $routine_line="$my_routine:$file_name_clean:$ligne";}
    if ($i_funsub > 0) {
      $subfun_type{$mydir.":".$my_routine}=$i_funsub;
      if ($i_funsub == 1) {
        $functions_files{$mydir.":".$my_routine}=$mydir.":".$file_name_clean;
      }
      elsif ($i_funsub == 2) {
        $subroutines_files{$mydir.":".$my_routine}=$mydir.":".$file_name_clean;
      }
      elsif ($i_funsub == 3) {
        $modules_files{$mydir.":".$my_routine}=$mydir.":".$file_name_clean;
      }
      if ($prev_routine ne "none") {
        %seen_fun = (); @uniqu_fun = grep { ! $seen_fun{$_} ++ } @tmp_fun;
        %seen_sub = (); @uniqu_sub = grep { ! $seen_sub{$_} ++ } @tmp_sub;
        %seen_mod = (); @uniqu_mod = grep { ! $seen_mod{$_} ++ } @tmp_mod;
        if ($i_prevfunsub == 1) {
          $functions_sub{$prev_routine}      = [ sort @uniqu_sub ];
          $functions_fun{$prev_routine}      = [ sort @uniqu_fun ];
          $functions_mod{$prev_routine}      = [ sort @uniqu_mod ];
        }
        if ($i_prevfunsub == 2) {
          $subroutines_sub{$prev_routine}      = [ sort @uniqu_sub ];
          $subroutines_fun{$prev_routine}      = [ sort @uniqu_fun ];
          $subroutines_mod{$prev_routine}      = [ sort @uniqu_mod ];
        }
        if ($i_prevfunsub == 3) {
          $modules_sub{$prev_routine}          = [ sort @uniqu_sub ];
          $modules_fun{$prev_routine}          = [ sort @uniqu_fun ];
          $modules_mod{$prev_routine}          = [ sort @uniqu_mod ];
        }
#         $subroutines_def_glob{$prev_routine} = [ sort @uniqu ];
      }
      @tmp_fun=();
      @tmp_sub=();
      @tmp_mod=();
      $i_prevfunsub=$i_funsub;
      $i_funsub=-1;  
    }
    elsif ($curline !~ /^\s*!/) { # We ignore also lines commented where comment ! is the first char on the line (not always at the beginning of the line)

      if ($curline =~ /^[^Cc#!]([\s\t]+)(USE)([\s\t]+)([^\(\s\t]*)/) {
        push @tmp_mod,"$mydir:$4";push @{$files_calls{$routine_line}},"$mydir:MODULE:".$4.":".$ligne;
      }
      elsif ($curline =~ /^[^Cc#!](.*)([^\w])(CALL)([\s\t]+)([A-Za-z][\w]{0,30})/) {
        $curline =~s/\t/\ \ \ \ /;
        if ($curline =~ /^[^Cc#!](.*)([^\w])(CALL)([\s\t]+)([A-Za-z][\w]{0,30})/) {
          push @tmp_sub,"$mydir:$5";push @{$files_calls{$routine_line}},"$mydir:SUBROUTINE:".$5.":".$ligne;
        }
        elsif ($curline =~ /^[^Cc#!]([\s]{4})[^\s]([\s\t\)]*)(CALL)([\s\t]+)([^\(\s\t]*)/) {
  #         print "$curline\n";
  #         print "add $5 routine\n";
          push @tmp_sub,"$mydir:$5";push @{$files_calls{$routine_line}},"$mydir:SUBROUTINE:".$5.":".$ligne;
        }
        elsif ($curline =~ /^[^Cc#!]([\s\t\)]*)(CALL)([\s\t]+)([^\(\s\t]*)/) {
  #         print "$curline\n";
  #         print "add $4 routine\n";
          push @tmp_sub,"$mydir:$4";push @{$files_calls{$routine_line}},"$mydir:SUBROUTINE:".$4.":".$ligne;
        }
      }
    }
    if ($i_funsub == 0) {
      for $fct_name (@function_names) {
          my $fct_name1=(split(':',$fct_name))[1];
          if ($curline =~ /^[^Cc#!].*($fct_name1)/) {
          $curline =~s/\t/\ \ \ \ /;
          if ($curline =~ /^[^Cc#!]*.*[^a-zA-Z0-9_]($fct_name1)[\s\t]*([\(])/) {
            push @tmp_fun,"$mydir:$1";push @{$files_calls{$routine_line}},"$mydir:FUNCTION:".$1.":".$ligne;
          }
          elsif ($curline =~ /^[^Cc#!]([\s]{4})[^\s]([\s\t]*)($fct_name1)[\s\t]*([\(])/) {
            push @tmp_fun,"$mydir:$3";push @{$files_calls{$routine_line}},"$mydir:FUNCTION:".$3.":".$ligne;
          }
        }
      }
    }
    else { $i_funsub=0; }
# remplace les tabulations par des espaces
#     my $curline_mod="";
#     for ($i=1;$i<=length($curline);$i++) {
#       if (substr($curline,$i-1,1) eq "\t") {
#          $curline_mod.= "    ";
#       }
#       else {
#          $curline_mod.=substr($curline,$i-1,1);
#       }
#     }
#     if (not /^C/ and not /^c/ and 
#         not /^#/ and 
#         not substr($curline_mod,0,6) eq "      " and 
#         not /^[\s]*$/ and 
#         not /^!/ and not &chk_num(substr($curline_mod,0,5),$ligne) eq 1) {
# #       print "$ligne:$curline\n";
#     }
# remplace les tabulations par des espaces fin
  }
#   print $LOG "$File::Find::name\n";
}

sub Wanted_functions
{
# Only .F files are accounted for
  /\.F$/ or return;

  # Check if the file is part of excluded files
  return if (must_file_be_excluded($File::Find::name));

  $nbfiles++;
  my $file_name_clean=substr($File::Find::name,2);
  if (substr($File::Find::name,0,3) eq "../") {
    $file_name_clean=$File::Find::name;
  }
# Store file
  $file_name=$_;
  open (FILE, $file_name);
  my @tabfile=<FILE>;
  close(FILE);
  push @tabfile,"        SUBROUTINE dump_last_routine(empty)";
# Look for function names  
  $my_routine="$mydir:none";
  my $ligne=0;
  my $i_function_line=0;
  for (@tabfile) {
    my $curline=$_;
    chomp($curline);
    $ligne++;
#     if ($file_name eq "uaccess.F" and $ligne == 181) {
#       print "coucou\n";
#     }
    if ($curline =~ /^[Cc#!*]/) {next}
#   Francis case
    if ($curline =~ /^[\s\t]*[!]/) {next}
    if ($curline =~ /^[^Cc#!]([\s\t]*)END[\s\t_]*FUNCTION/) {next}
    if ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]+)FUNCTION[\s\t]*/
     or $curline =~ /^([\s\t]{5})([.])([\s\t]*)FUNCTION /) {  
#       print "curline=$curline\n";
      if ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]+)(FUNCTION)[\s\t]+([^\(\s\t]*)[\s\t]*\(/) { 
          $my_routine=$5;
#       print "curline1=$curline\n";
          push @function_names,"$mydir:$my_routine";
#            print "2 $mydir:$my_routine\n";
#            print "$File::Find::name +$ligne --> $curline\n";
      }
      elsif ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]+)(FUNCTION)[\s\t]+([^\(\s\t]+)[\s\t]*/) { 
          $my_routine=$5;
#       print "curline1=$curline\n";
          push @function_names,"$mydir:$my_routine";
#            print "2 $mydir:$my_routine\n";
#            print "$File::Find::name +$ligne --> $curline\n";
      }
      elsif ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]*)(FUNCTION)[\s\t]*/) { 
        $i=0;
        while ($tabfile[$ligne+$i] =~ /^[Cc#!*]/) {$i++};
        if ($tabfile[$ligne+$i] =~ /^([\s\t]{5})([.])([\s\t]*)([^\(\s\t]+)/) { 
          $my_routine=$4;
          push @function_names,"$mydir:$my_routine";
#            print "2 $mydir:$my_routine\n";
#          if ($my_routine eq "G") {
#           print "$File::Find::name +$ligne --> $curline\n";
#          }
        }
      }
    }
  }
}


# sub Wanted_modules
# {
# # Only .F files are accounted for
#   /\.F$/ or return;
# # Store file
#   $file_name=$_;
#   open (FILE, $file_name);
#   my @tabfile=<FILE>;
#   close(FILE);
# # Look for function names  
#   $my_routine="none";
#   my $ligne=0;
#   for (@tabfile) {
#     my $curline=$_;
#     chomp($curline);
#     $ligne++;
#     if ($curline =~ /^[^Cc#!]([\s\t]*)MODULE([\s\t]+)([\S]+)([\s\t]*)/) {  
#       $i=0;
#       $my_routine=$4;
# #       push @module_names,$my_routine;
#     }
#   }
# }


sub Wanted_fcall
{
# Only .F files are accounted for
  /\.c$/ or return;

  # Check if the file is part of excluded files
  return if (must_file_be_excluded($File::Find::name));

# Store file
  my $file_name_clean=substr($File::Find::name,2);
  if (substr($File::Find::name,0,3) eq "../") {
    $file_name_clean=$File::Find::name;
  }
  $file_name=$_;
  open (FILE, $file_name);
  my @tabfile=<FILE>;
  close(FILE);
  push @tabfile,"        SUBROUTINE dump_last_routine(empty)";
# Look for function names  
  $my_routine="none";
  my $ligne=0;
  for (@tabfile) {
    my $curline=$_;
    chomp($curline);
    $ligne++;
    if (not $curline =~ /^\//) {
    if ($curline =~ /^([\s\t]*)(void)([\s\t]+)(_FCALL)([\s\t]+)([^\(\s\t]*)/) {  
        $my_routine="$mydir:$6";
        push @fcall_names,$my_routine;
        $subroutines_files{$my_routine}="$mydir:$file_name_clean";
    }
    }
  }
}

sub Wanted_replace
{
# Only .F files are accounted for
  /\.F$/ or return;

  # Check if the file is part of excluded files
  return if (must_file_be_excluded($File::Find::name));

  $nbfiles_cur++;
#   printf "\r %s%c  ",$perc,37;
#   $perc=int($nbfiles_cur/$nbfiles*100);
# Store file
  $file_name=$_;
  open (FILE, $file_name) or return;
  my @tabfile=<FILE>;
  close(FILE);

  my @newfile=();
  my $ligne=0;
#  my @chd_header_mod=();
  if (     substr($File::Find::name,2,3) ne "too" 
       and substr($File::Find::name,2,3) ne "pub" ) {
  # Build routine/function tree
  my $i_interface=0;
  my @buf_function_line=();
    for (@tabfile) {
      my $curline=$_;
  #     chomp($curline);
      $ligne++;
#       if ($file_name eq "uaccess.F" and $ligne == 181) {
#         print "coucou\n";
#       }
      if ($i_interface==1) {
        if ($curline =~ /^[^Cc#!]([\s\t]*)END([\s\t]+)INTERFACE([\s\t]*)$/) {
          $i_interface=0;
        }
        else {
          push @newfile,$curline; 
          next;
        }
      }
      if ($curline =~ /^[^Cc#!]([\s\t]*)INTERFACE([\s\t]*)$/) { 
        push @newfile,$curline; 
        $i_interface=1;
        next;
      }
  # Check if current line match subroutine/function header
      my $i_routine=0;
      if ($curline =~ /^[^Cc#!]([\s\t]*)END[\s\t_]*FUNCTION/) {
        push @newfile,$curline; 
        next;
      }
      if ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]+)FUNCTION[\s\t]*/
      or $curline =~ /^([\s\t]{5})([.])([\s\t]*)FUNCTION /) {  

        if ($curline =~ /[\'\"]/ or $curline =~ /^([\s\t]+)!/)  { # Ignoring the line if there is at least one ' or " or it is a comment that begins after some spaces RD-9329
            push @newfile,$curline;
            next;
        }

        if ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]+)(FUNCTION)[\s\t]+([^\(\s\t]*)[\s\t]*\(/) { 
            $my_routine=$mydir.":".$5;
            &header($my_routine);push @newfile,@header;$i_routine=1;
        }
        elsif ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]+)(FUNCTION)[\s\t]+([^\(\s\t]+)[\s\t]*/) { 
            $my_routine=$mydir.":".$5;
            &header($my_routine);push @newfile,@header;$i_routine=1;
        }
        elsif ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]*)(FUNCTION)[\s\t]*/) { 
          $i=0;
          while ($tabfile[$ligne+$i] =~ /^[Cc#!*]/) {$i++};
          if ($tabfile[$ligne+$i] =~ /^([\s\t]{5})([.])([\s\t]*)([^\(\s\t]+)/) { 
            $my_routine=$mydir.":".$4;
            &header($my_routine);push @newfile,@header;$i_routine=1;
          }
        }
      }
      elsif ($curline =~ /^[^Cc#!]([\s\t]*)([\S]+)([\s\t]+)FUNCTION[\s\t]*([^\(\s\t]*)/ and not $curline =~ /^[^Cc#!]([\s\t]*)END/) {  

        if ($curline =~ /[\'\"]/ or $curline =~ /^([\s\t]+)!/) { # Ignoring the line if there is at least one ' or " or it is a comment that begins after some spaces RD-9329, I'm not sure we'll pass here one day
            push @newfile,$curline;
            next;
        }    

        $my_routine=$mydir.":".$4;
        &header($my_routine);push @newfile,@header;$i_routine=1;
      }
      elsif ($curline =~ /^[^Cc#!]([\s\t]+)(SUBROUTINE)[\s\t]*([^\(\s\t]*)/){
        $my_routine=$mydir.":".$3;
        &header($my_routine);push @newfile,@header;$i_routine=1; 
      }
      elsif ($curline =~ /^[^Cc#!]([\s\t]+)(MODULE)[\s\t]*([^\(\s\t]*)/){
        $my_routine=$mydir.":".$3;chomp($my_routine);
        &header($my_routine);push @newfile,@header;$i_routine=1;
      }
      elsif (not $curline =~ /^Cgw\|/ 
            and not $curline =~ /^CGW\|/ 
            and not $curline =~ /^Cls\|/ 
            and not $curline =~ /^Csv\|/ 
            and not $curline =~ /^Cfp\|/ 
            and not $curline =~ /^Cts\|/ 
            and not $curline =~ /^Cow\|/ 
            and not $curline =~ /^Cfa\|/ 
            and not $curline =~ /^Cpm\|/ 
            and not $curline =~ /^Chd\|/) { 
        push @newfile,$curline; 
      }
      if ($i_routine == 1) {
        push @newfile,$curline;
      }
    }
    open (FILE,">".$file_name."__") or die 'Cannot open file : '.$file_name."__";
    for (@newfile) {
      print FILE $_;
    }
    close(FILE);
#    system("cp $file_name $file_name"."__unixfmt__");
#    system("dos2unix $file_name"."__unixfmt__");
#    system("cp $file_name"."__"." $file_name"."__unixfmt____");
#    system("dos2unix $file_name"."__unixfmt____");
    my $file_name__1=$file_name."__";
    if (system("diff $file_name $file_name__1")) {
      my $file_name__wo_c=$file_name."__wo_c";
      my $file_name____wo_c=$file_name__1."__wo_c";
      system("cat $file_name | grep -v ^C > $file_name__wo_c");
      system("cat $file_name__1 | grep -v ^C > $file_name____wo_c");
      if (system("diff $file_name__wo_c $file_name____wo_c")) {
        $i_error=1;
        print "Error while processing file $file_name\n";
        print "\t > Files without comments are differents.";
      }
      else {
        print "$File::Find::name ....\n";
#      my $answer=<STDIN>;
        my $curdir=qx(pwd);
        chomp($curdir);
        &Run_Command("mv $file_name"."__ $file_name","MV :");
        &Run_Command("git add $file_name","GIT ADD :");

        print "$File::Find::name done.\n";
      }
    }
  }
  else {
    print "$File::Find::name : pub or tools file, skipped.\n";
  }
}


sub chk_num() {
  my ($str,$ligne)=@_;
  my $i=0;
  my $i_num=1;
  if ($ligne == 1) {
    if (substr($str,$i-1,1) =~ /[\s]/ and not substr($str,$i-1,1) =~ /[0-9]/ ) {
      $i_num=0;
      return $i_num;
    }
    for ($i=1;$i<=length($str);$i++) {
      if (substr($str,$i-1,1) =~ /[\s]/ and not substr($str,$i-1,1) =~ /[0-9]/ ) {
        $i_num=0;
        return $i_num;
      }
    }
    return $i_num;
  }
}

sub Run_Command() {
  my ($runcmd,$cmdinfo)=@_;
  my ($printout);
  $printout = "$cmdinfo $runcmd\n";
  print "$printout\n";
  system($runcmd);
}

