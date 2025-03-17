/*Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/
#ifndef STR_UTILS_H
#define STR_UTILS_H

#include <stdarg.h>


#ifdef __cplusplus
#include <UTILS/mv_cstdio.h> 
#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#endif /* __cplusplus */
#include <HCDI/hcdi.h>


#define MAC_NAME_BLC(buff){     \
char    *p_c1 ;                 \
p_c1 = buff ;                   \
while ( *p_c1 && *p_c1 != ' ' ) \
    p_c1++ ;                    \
if ( *p_c1 == ' ' )             \
    *p_c1 = '\0' ;              \
}

#ifdef __cplusplus

char   *get_path(char *s,char *s1);
void    retire_nl_blc(char *s);
char   *vire_blancs_debut_et_fin(char *str,int flg);
char   *vire_blancs_fin(char *str);
void    retire_newline(char *s);
int     my_isxdigit(char i);
int     get_strings (char *str,int n,char **ppstr);
HC_DATA_DLL_API char   *mystrcpy(char *dest,const char *src);
HC_DATA_DLL_API string  str_printf(const char *fmt,...);
HC_DATA_DLL_API bool    is_blank(const char *s);
HC_DATA_DLL_API char    myupcase(char c);
int     mygetstring(char *str,int ncount,char *line);

void StringTokenize(const string& str,vector<string>& pTokens, const string &delem);
char *my_strconvtolowercase(const char *str);
bool my_compare_str(const char *title, const char *src_text, bool is_match_case=false, bool is_whole_name=false, bool do_wild_card_check=false);
void convert_vect_to_string(string& arg_list, const vector<string>& argvect);
#endif /* __cplusplus */


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define utility_fitline my_fitline
#define utility_getline my_getline

char *utility_string_fit_end(char *name);
char *utility_string_fit_start(char *name);

HC_DATA_DLL_API char *utility_string_fit_start_end(char *name);
char *utility_string_fit_all(char *name);

char *utility_add_extent_string(char *filename, char *extension);



void my_fitline(char *line);
int my_getline(char *line,FILE *infile,char *infilename,int *linecount);



int   radioss_getint(int *x,int ncount,char *line);
int   radioss_getdouble(double *x,int ncount,char *line);

/*  For read_unv only... */
char * slash_conversion(char * str);



/* always_signed takes either 0 or 1. With 0, no loss of precision and no space is left for sign.
   With 1, one space is left for sign and might be a loss of precision as 1 digit left empty.
   Example: Printing a double value of 16 characters (nb_chars = 16)
   always_signed=0 -> Output: 9.09090909091E-5 (16 chars)
   always_signed=1 -> Output:  9.0909090909E-5 (15 chars) (one space left for sign)*/
HC_DATA_DLL_API double my_print_double(double value,int nb_chars,char *result,int always_signed);

double my_print_double_if_shift(double value,int nb_chars,char *result,int always_signed,int if_shift);

void my_print_double_in_simple_field(double value, int nb_char, char *field) ;


HC_DATA_DLL_API double hc_scan_double(const char *str, const char *format, int nb_chars, int* nb_read);

HC_DATA_DLL_API char *my_strdup(const char *src);
HC_DATA_DLL_API int   mystrncasecmp(const char *s0, const char *s1,size_t n);
int   my_compare_str_in_c(const char *title, const char *src_text, int is_match_case, int is_whole_name, int do_wild_card_check);


void general_file_replace_path_separator_by_slash (char *path);
int
utility_store_comment_lines(FILE *file, int nbr_line, char **line);
#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* STR_UTILS_H */




