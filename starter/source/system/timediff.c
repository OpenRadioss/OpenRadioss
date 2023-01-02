//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2023 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define _FCALL

#ifdef _WIN64
    #include <memory.h>
#else
    #include <string.h>
#endif

void strptime_impl(char* date, struct tm *tm);
/* ------------------------------------------------------------
   time_difference : get the difference between 2 dates in UTC
   ----------------------------------------------------------- */
void c_time_difference(char* time_string,int * time_string_len, int * difference)
{
 char * c_time_string;
 int i;
 time_t now,now_utc,dummy_time;
 double tdiff;
 struct tm *tm_dum,*tm_now;
 char datestring[32], curtime[32];

 c_time_string = malloc(*time_string_len+1);
 for (i=0;i<*time_string_len;i++) c_time_string[i]=time_string[i];
 c_time_string[*time_string_len]='\0';

 
/* Current Time in UTC */
 time( &now );
 strftime(datestring, 32, "%Y-%m-%dT%H:%M:%S", gmtime( &now ));
 tm_now = (struct tm*)malloc(sizeof (struct tm));
 strptime_impl(datestring,tm_now);
 now_utc=mktime(tm_now);

/* convert time in header*/
 tm_dum = malloc(sizeof (struct tm));
 strptime_impl(c_time_string,tm_dum);
 dummy_time= mktime(tm_dum);
 tdiff=difftime(dummy_time,now_utc);

 if (tdiff > 0 ){
  * difference = 1;
 }else{
  * difference = -1;

 }
// *difference=(int) tdiff;
// printf("c_time_string: %s\n",c_time_string);
// printf("Datestring   : %s\n",datestring);
// printf("DIFFERENCE: %lf \n",tdiff);
// printf("Integer difference: %i \n", * difference);
// fflush(stdout);
}

void c_time_difference_(char* time_string,int * time_string_len, int * difference){
   c_time_difference(time_string,time_string_len, difference);
}

void _FCALL C_TIME_DIFFERENCE(char* time_string,int * time_string_len, int * difference){
   c_time_difference(time_string,time_string_len, difference);
}

void strptime_impl(char* date, struct tm *tm)
{
  int year, month, day, hour, min, sec ;

  memset(tm,0,sizeof(struct tm));

  sscanf(date,"%d-%d-%dT%d:%d:%d",&year,&month,&day,&hour,&min,&sec );
  tm->tm_year = year - 1900;
  tm->tm_mon = month-1;
  tm->tm_mday = day;
  tm->tm_hour = hour;
  tm->tm_min = min;
  tm->tm_sec = sec;
 
}

