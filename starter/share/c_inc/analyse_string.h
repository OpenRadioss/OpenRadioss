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
#ifndef __ANALYSE_STRING_H__
#define __ANALYSE_STRING_H__

char *analyse_string_strset(char *name, int ch);
char *analyse_string_strrev(char *name);
char *analyse_string_fit_start(char *name);
char *analyse_string_fit_end(char *name);
char *analyse_string_fit_start_end(char *name);
char *analyse_string_fit_all(char *name);

int analyse_string_length_brackett(char *string, int n1, int n2) ;

void analyse_convert_int_to_string(int nb_int, int *tab_int, char *message);
void analyse_convert_string_to_int(char *message, int *nb_int, int **tab_int);

#endif
