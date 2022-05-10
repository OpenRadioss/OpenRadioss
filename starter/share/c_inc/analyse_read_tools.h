//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2022 Altair Engineering Inc.
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
#ifndef __ANALYSE_READ_TOOLS_H__
#define __ANALYSE_READ_TOOLS_H__

#include <stdio.h>

#include "analyse_comment.h"

int analyse_find_id( int size, int *tab, int id);
char *analyse_read_text(char *line, FILE *infile, char *filename, int *linecount_p);
/* lm41n7 +1 */
char *analyse_read_text_default(char *line, FILE *infile, char *filename, int *linecount_p);
analyse_comment_t *analyse_read_comment(char *line, FILE *infile, char *filename, int *linecount_p);
void analyse_count_error(char *line,  FILE *infile, char *infilename, int *linecount_p, int *cnt_p);

void analyse_count_check(char *line,  FILE *infile, char *infilename, int *linecount_p, int *cnt_check_p, int *cnt_group_p);

void analyse_stack_error(char *line, FILE *infile, char *infilename, int *linecount_p, 
			 int *cnt_p, int *size_p, int **tab_p);

void analyse_stack_check(char *line, FILE *infile, char *infilename, int *linecount_p, 
			 int *nb_check_group_p, int *size_p, int **tab_p,  
			 int *nb_check_p, int *size2_p, int **tab2_p);
#endif
