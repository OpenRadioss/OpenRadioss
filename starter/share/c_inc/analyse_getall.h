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
#ifndef __ANALYSE_GETALL_H__
#define __ANALYSE_GETALL_H__

void analyse_fitline(char *line);

int analyse_getint(int *x,int ncount,char *line);
int analyse_getdouble(double *x,int ncount,char *line);
int analyse_getstring(char *str,int ncount,char *line);

int analyse_getline(char line[],FILE *infile,char *infilename,int *linecount);
int analyse_getline_with_dollars(char line[],FILE *infile,char *infilename,int *linecount);
int analyse_getcommentline(char *comment, FILE *infile,char *infilename,int *linecount);

int analyse_getlist_of_int(int *nb_elt, int **elt, FILE *infile,char *infilename,int *linecount);
int analyse_getlist_of_char(int *nb_elt, char ***elt, FILE *infile,char *infilename,int *linecount);
int analyse_getlist_of_char_2(int *nb_elt, char ***elt, FILE *infile,char *infilename,int *linecount);

int analyse_getkey(int pos, char *line, char *name);
int analyse_getsize_of_enum ( int *nb_line, int *nb_dataline, FILE *infile,char *infilename,int *linecount);

#endif
