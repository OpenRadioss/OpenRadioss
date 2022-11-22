#!/usr/bin/perl

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

my $pid=$ARGV[0];
my $timeout=$ARGV[1];
my $extract=$ARGV[2];
my $debug=$ARGV[3];
$debug = 0 if (!defined $debug or ($debug != 1 and $debug != 0));
my @os=qx(uname);
my $os1=$os[0];


print "[ QA TIMEOUT ] let's go running on pid $pid\n" if ($debug);
system("echo \"[ QA TIMEOUT ] let's go running on pid $pid\" >> /tmp/mypid") if ($debug);
system("echo 'dolldoll is ".$$."' >> /tmp/mypid") if ($debug);

my @ps2k;
push (@ps2k,$pid);

sub get_children_from($) {
  my $cur_pid = shift;
  my @children = ();

  my @allps = `ps -ef`;
  chomp @allps;
  # Remove columns
  shift @allps;

  for my $elem (@allps) {
    my @cur_line = split(' ',$elem);
    if ($cur_line[2] == $cur_pid) {
      push(@children,$cur_line[1])
    }
  }

  # print "------ \@allps ----------\n";
  # for my $elem (@allps) { print "$elem\n"; }
  # print "-------------------------\n";

  return @children;

}

my $mytab='';
sub kill_descendants($) {

  my $cur_pid = shift;

  $mytab .= '  ' if ($debug);
  print "RUNOF $cur_pid $mytab EXEC kill_descendants with PID $cur_pid\n" if ($debug);

  my @children = get_children_from($cur_pid);

  # print "---- Children of $cur_pid ----\n";
  # for my $elem (@children) { print "$elem\n"; }
  # print "-----------------------------\n";

  if (scalar(@children)) {
    for my $elem (@children) {
      if ($debug) {
        print "RUNOF $cur_pid $mytab run ELEM $elem\n";
        print "RUNOF $cur_pid $mytab --------------------- \@ps2k -----\n";
        print Dumper @ps2k;
        print "RUNOF $cur_pid $mytab --------------------------------------\n";
      }
      if (!grep(/^$elem$/,@ps2k)) {
         push (@ps2k,$elem) ;
         print "RUNOF $cur_pid $mytab CALL kill_descendants($elem)\n" if ($debug);
         kill_descendants($elem);
      }
    }
  }
  print "RUNOF $cur_pid $mytab END EXEC kill_descendants with PID $cur_pid\n" if ($debug);

}

my $os1=substr($os1,0,6);
my $mykill="kill -9";

my $time0=time;
system("echo start : perl or_qa_timeout.pl $ARGV[0] $ARGV[1] $ARGV[2] > _ps_killed_$$");

while ($timeout > 0) {
  my $time1=time;
  my $delay=$time1-$time0;

  if (not -f "_ps_killed_$$") { exit }
  if ($delay > $timeout) {

      my $print_ok = 1;
      # print "TRACE I'M GONNA OPEN $extract\n";
      # system("ls -l $extract");
      # print "TRACE content 1 is \n";
      # system ("cat $extract");
      # print "-------------------\n";

      # my $ladate = `date "+%s.%N"`; chomp $ladate;
      # print "WRITING TIMEOUT in extract file at $ladate\n";
      my $tmpout=sprintf "%5d %s %10d\n",-1,"TIMEOUT",$timeout;
      if (open(FILE,">>$extract")) {
        print FILE $tmpout;
        close(FILE);
        print "\n*** The script TIMEOUT has been reached ***\n\n";
      }
      else {
        $print_ok = 0; 
        print "Cannot open $extract ! Let's try later (may be a lock on it)\n";
      }
      
      # print "TRACE content 2 is \n";
      # system ("cat $extract");
      # print "-------------------\n";

      kill_descendants($pid);

      if ($debug) {
        print "======================= \@ps2k ============================\n";
        for my $elem (@ps2k) { 
          print " $elem\n"; 
          if ($os1 eq "CYGWIN") {
            system("cat /proc/$elem/cmdline");
            print "\n";
          }
        }
        print "\n=======================================================\n";
      }

      for my $pid1 (reverse @ps2k) {
        print "echo $mykill $pid1 >> _ps_killed_$$ \n";
        if ($pid1 == $$) {next}
        system("echo $mykill $pid1 >> _ps_killed_$$");
        print "KILLING $pid1\n" if ($debug);
        system("$mykill $pid1");

        # We try to print TIMEOUT in the extract file after each kill, because in case of using QAPRINT
        # We can't write in it (on windows) since the file is locked
        # When starter is killed, so we are able to print in it
        if (! $print_ok) {
          $print_ok = 1;
          # print "WRITING TIMEOUT in extract file at $ladate\n";
          my $tmpout=sprintf "%5d %s %10d\n",-1,"TIMEOUT",$timeout;
          if (open(FILE,">>$extract")) {
            print FILE $tmpout;
            close(FILE);
            print "\n*** The script TIMEOUT has been reached ***\n\n";
          }
          else {
            $print_ok = 0; 
            print "Cannot open $extract ! Let's try later (may be a lock on it)\n";
          }
        }
        # print "TRACE content 2 is \n";
        # system ("cat $extract");
        # print "-------------------\n";

      } 

      # For winddows machines we use env variable ENABLE_PID_PRINT to print detached exec pid in a file
      # Then, at kill time, we read the file and kill related pids
      if (defined $ENV{'ENABLE_PID_PRINT'} and $ENV{'ENABLE_PID_PRINT'} eq 'ON') {
        if (-f 'running_pids') {
          open(RUNNINGPIDS,"< running_pids");
          my @results = <RUNNINGPIDS>;
          close(RUNNINGPIDS);

          chomp @results;
          map { s/^\s+|\s+$//g; } @results;
          my @running_pids = grep { $_ ne '' } @results;

          # print "ENV var is ".$ENV{'ENABLE_PID_PRINT'}."\n".join(',',@running_pids)."\n" if ($debug); 
          if (scalar(@running_pids)) {
            my $kill_pid_str = "taskkill.exe /F /T /PID ".join(' /PID ',@running_pids);
            print "STR: $my $kill_pid_str\n" if ($debug); 
            system($kill_pid_str);
          }

        }
      }

      exit;
  }
  sleep 1;
}

exit;
