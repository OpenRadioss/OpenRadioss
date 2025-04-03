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
#include "mv_cstdio.h"
#include "mv_cmath.h"
#include "mv_cstdarg.h"
#include "d00_defs.h"
#include "memory_utils.h"
#include "mv_cstdlib.h"
#include "mv_cstring.h"
#include "str_utils.h"
#include "error.h"

#ifdef _WIN32
//#include "vsscanf.win.h"
#endif

#ifdef _WIN32
#define strcasecmp strcmpi
#define strncasecmp strnicmp
#endif

#define LOC_NB_DIGITS_MAX 15
#define NBR_NAS_FIELD 10 
static int 
WaitForReal(char *spec) ;
static void
CorrectDoubleFormat(int ispec, char *format) ;
static void
CorrectIntegerFormat(int ispec, char *format) ;
static int 
ftnline_into_cline(const char *f_line, const char *format, char c_line[], char new_format[]) ;

static void loc_shift_zeros(char *result,int nb_chars);


static char *loc_pre_print_value(double value,bool is_neg,int nb_decs,int nb_chars,char *result);


/***************************************************************/
/* function read_FTN_format pour lire les double correctement  */
/* utilisation de libF77.a et libI77.a                         */
/* besoin aussi de f2cc.h                                      */
/***************************************************************/
char *get_path (char *s,char *s1) {
  char	*p_c ;
  strcpy ( s1, s ) ;



#ifndef WIN32
  if ( !(p_c = strrchr ( s1, '/' ) ) ) {
#else   //WIN32
  if ( !((p_c=strrchr(s1,'\\')) || (p_c=strrchr(s1,'/'))) ) {
#endif  //WIN32


    s1[0] = '\0' ;
  } else {
    *(p_c+1) = '\0' ;
  }
  return ( s1 ) ;
}

void retire_nl_blc (char *s) {
  char *p_c ;
  if( ( p_c = strrchr ( s, '\n' ) ) )
    while ( p_c >= s && *p_c <= 0x20 )
      *(p_c--) = '\0' ;
}

char *vire_blancs_debut_et_fin (char *str,int flg) {
  char    *p_c ;
  if ( *str == '\0' )
    return ( str ) ;
  p_c = str ;
  while ( *(p_c++) ) {    /* en cas de '\n' */
    *p_c = ( *p_c < 0x20 ) ? '\0' : *p_c ;
    if ( flg )
            *p_c = ( *p_c == '/' ) ? '_'  : *p_c ;
  }
  /* positionnement avant le '\0' */
  p_c -= 2 ;
  while(p_c>=str && *p_c==' ') p_c--;
  *(++p_c) = '\0' ;
  /* on retire les blancs du debut */
  p_c = str ;
  while ( *p_c == ' ' )
    p_c++ ;  
  return ( p_c ) ;
}

char *vire_blancs_fin(char *str) {
  char *p_c=str+strlen(str)-1;
  while(p_c>=str && *p_c==' ') p_c--;
  *(++p_c)='\0';
  return str;
}

void retire_newline(char *s) {
  char	*p_c ;
  p_c = s ;
  while ( *p_c ) {
    /* 0x09 = tabulation horizontale */
    *p_c = ( *p_c < 0x20 && *p_c != 0x09 ) ? '\0' : *p_c ;
    p_c++ ;
  }
}

int my_isxdigit	(char i) {
    if ( i<'0' || (i>'9' && i<'A') || (i>'Z' && i<'a') || i>'z' )
	return ( FALSE ) ;
    else
	return ( TRUE ) ;
} 

/*--------------------------------------*/
/* lecture des lignes de mot cles	*/
/* ex: /MAT/law/mat_id/mat_title	*/
/* les != segment sont stockes ds ppstr */
/*--------------------------------------*/
int get_strings (char *str,int n,char **ppstr) {
  char *p_c ;
  int   i ;
  
  if (*str != '/') return (0) ;
  p_c = str+1 ;
  for ( i = 0 ; i < n ; i++ ) {
    if ( i ) {
      p_c = strchr ( ppstr[i-1], '/' ) ;
      if ( !p_c )
	return ( i ) ;
      *p_c = '\0' ;
      p_c++ ;
    }
    ppstr[i] = p_c ;
  }
    
  return ( i ) ;
}

char *mystrcpy(char *dest,const char *src) {
  int nb = (src==NULL) ? 0 : (int)(strlen(src));
  myfree(dest);
  dest=(char *)mymalloc((nb+1)*sizeof(char));
  if(nb) strcpy(dest,src); else *dest='\0';
  return dest;
}

string str_printf(const char *fmt,...) {
  char a_buffer[L_PATH];     
  //
  va_list a_arglist;
  va_start(a_arglist,fmt);
  vsprintf(a_buffer,fmt,a_arglist);
  va_end(a_arglist);
  //
  return a_buffer;
}

bool is_blank(const char *s) {
  const char *a_ptr=s;
  while((*a_ptr)==' ') ++a_ptr;
  return (*a_ptr)=='\n' || (*a_ptr)=='\0';
}

char myupcase(char c) {
  static char a_offset='A'-'a';
  return (c>='a' && c<='z') ? c+a_offset : c;
}

int mystrncasecmp(const char *s0, const char *s1,size_t n) {
  const char *a_s0=s0,*a_s1=s1;
  int         a_result=1;
  size_t      a_n=0;
  while(a_result && a_n<n && *a_s0 && *a_s1) {
    a_result=(myupcase(*(a_s0++))==myupcase(*(a_s1++)));
    ++a_n;
  }
  if((a_n<n) && (!(*a_s0) || !(*a_s1))) a_result=((*a_s0)==(*a_s1));
  return !a_result;
}

int mygetstring(char *str,int ncount,char *line)
{
  int taille = 0 ;
  int i ;

  taille = (int)strlen(line) ;
  if (taille < ncount)
  {
     ncount = taille ;
  }
  for (i=0 ; i<ncount ; i++)
  {
      if (line[i] == '\n')
      {
	 ncount = i ;
	 break ;
      }
  }
  strncpy(str,line,ncount);
  *(str+ncount)='\0';  
  return(0);
}

char *utility_string_fit_start(char *name)
{
	int nb_spaces = 0;
	if (name != NULL)
	{
		while (name[nb_spaces] == ' ' || name[nb_spaces] == '\t') nb_spaces ++;
		if (nb_spaces > 0) memmove (name, name + nb_spaces, (strlen (name + nb_spaces) + 1) * sizeof(char));
	}
	return name;
}
char *utility_string_fit_end(char *name)
{
  int length;
  if ( name != NULL)
  {
      length = (int)strlen(name);
      /*CS/333/17/12/2003*/
      /*      while ( ( length > 0) && ( *(name+length-1) == ' ' ) )*/
      while ( ( length > 0) && ( (*(name+length-1) == ' ' )  ||
				 (*(name+length-1) == '\r') || (*(name+length-1) == '\n')
                 || (*(name+length-1) == '\t'))  )
      {
	  *(name+length-1)= '\0';
	  length = length -1;
      }
 
  }
  return name;	   
}
extern "C" char *utility_string_fit_start_end(char *name)
{
  if (name != NULL)
    {
      utility_string_fit_start(name);
      utility_string_fit_end(name);
    }
  return name;
}

char *utility_add_extent_string(char *filename, char *extension)
{
   char *new_filename = NULL ;
   int size ;
   size = (int)strlen(filename) + (int)strlen(extension) + 1 ;
   new_filename = (char *)calloc(size,
                                     sizeof(char)) ;
   if (new_filename)
   {
       strcpy(new_filename, filename);
       strcat(new_filename, extension);
       new_filename[size - 1] = '\0';
   }
   return (new_filename) ;
}

char *utility_string_fit_all(char *name)
{
    int i= 0, length = 0, i_new = 0; 
    if (name == NULL) 
        return NULL;
    length = (int)strlen (name);
    i_new = 0;
    for (i = 0; i < length; i++) 
    {
        if (name[i] != ' ') 
        {
            name[i_new] = name[i];
            i_new++;
        }
    }
    name[i_new] = '\0';
    
    return name;
}

extern "C" void my_fitline(char *line)
{
        int index,charcount;
/*345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
        char *blank = "                                                                                ";

        index = (int)(strlen(line));
        
        while ((index > 0) && ((line[index - 1] == '\r') || (line[index - 1] == '\n'))) {
			index = index - 1;
		} 
		
        charcount = 80 - index;
        if(charcount > 0)
        {
                strncpy(&line[index],blank,charcount);
        }
        strncpy(&line[80],"\0",1);
}


extern "C" int my_getline(char *line,FILE *infile,char *infilename,int *linecount)
{
        do
        {
                if(fgets(line,160,infile) == NULL)
                {
                        printf("Error in getline : premature EOF line %d, File : %s \n",*linecount,
                               infilename);
                        return(-1);
                }
                *linecount = *linecount + 1;
                if(!strncmp(line, "#include", 8))
                {
                   printf("Error : include files not yet supported,file : %s\n",infilename);
                }
        } while (!strncmp(line,"#",1) || !strncmp(line,"$",1));
        utility_fitline(line);
        return(0);
}

extern "C" int radioss_getint(int *x,int ncount,char *line)
{
  int readret;
  char linecopy[21];
  
  strncpy(linecopy,line,ncount);
  *(linecopy+ncount)='\0';
  readret = sscanf(linecopy,"%d",x);
  if(readret < 0) 
    {
      *x=0;
      return(0);
    }
  return(readret);
}
extern "C" int radioss_getdouble(double *x,int ncount,char *line)
     
{
  int readret;
  char linecopy[21];
  
  strncpy(linecopy,line,ncount);
  *(linecopy+ncount)='\0';
  readret = sscanf(linecopy,"%lf",x);
  if(readret < 0) 
    {
      *x=0;
      return(0);
    }
  return(readret);
}

extern "C" char *slash_conversion(char * str) {
  char    *p_c ;
  if ( *str == '\0' )
    return ( str ) ;
  p_c = str ;
  while ( *(p_c++) ) {    /* en cas de '\n' */
    *p_c = ( *p_c < 0x20 ) ? '\0' : *p_c ;
    *p_c = ( *p_c == '/' ) ? '_'  : *p_c ;
  }
  /* positionnement avant le '\0' */
  p_c -= 2 ;
  while(p_c>=str && *p_c==' ') p_c--;
  *(++p_c) = '\0' ;
  /* on retire les blancs du debut */
  p_c = str ;
  while ( *p_c == ' ' )
    p_c++ ;  
  return ( p_c ) ;
}
extern int
utility_store_comment_lines(FILE *file, int nbr_line, char **line)
{
   int linecount = 0 ;
   char line_read[160] ;
   int icount = 0;
   do 
   {
     if (fgets(line_read, 160 ,file) == NULL)
     {
        printf("Bad File Termination : Attempt of reading too much commentline !\n") ;
        return -1 ;
     }
     else if ((line_read[0] == '#') || (line_read[0] == '$'))
     {
         
         if((line_read[0] == '#') && 
             ((!strncmp(line_read,"#include ",9)) || 
               (!strncmp(line_read,"#INCLUDE ",9))) )
         {
            linecount = linecount + 1 ;
         }
         else
         {

            int ilen = 0;
            utility_string_fit_end(line_read);
            ilen = (int)strlen(line_read);
            line[icount] = (char *)calloc(ilen+2,sizeof(char)) ;
            strcpy(line[icount], line_read) ;
            line[icount][ilen] = '\n';
    
            icount++;
            linecount = linecount + 1 ;
         }
     } 
   } while(linecount <nbr_line) ;
   return linecount ;
}



extern "C" double my_print_double(double value,int nb_chars,char *result,int always_signed) {
    return my_print_double_if_shift(value,nb_chars,result, always_signed,1) ;
}
extern "C" double my_print_double_if_shift(double value,int nb_chars,char *result,int always_signed,int if_shift) {
    char a_format[50];
    static bool init_fmod_value = false;
    if(nb_chars == 0)
        nb_chars = 8;/*should never happen*/
#ifdef WIN32
    if(init_fmod_value==false)
    {
        //double return_value = fmod(1.0,1.0);
        init_fmod_value = true;

    }
#endif
    //
    //
    if(value==0.) {
        if(result!=NULL) {
            sprintf(a_format,"%%%d.1f",nb_chars); //print 0.0 format
            sprintf(result,a_format,value);
        }
        return value;
    }
    //  
    int   a_nb_chars  = nb_chars;
    bool  a_is_neg    = (value<0.);
    bool  a_is_signed = (always_signed || a_is_neg);
    char *a_result    = result;
    if(a_is_signed) {  
        --a_nb_chars; 
        if(result!=NULL) { a_result[0]=' '; ++a_result; }
    }
    //
    
    double a_abs_value    = (a_is_neg ? (-value) : value);
    double  a_pre_exp      = log10(a_abs_value);
    double a_floor        = floor(a_pre_exp);
 
    int    a_exp          = (int)a_floor;
    bool   a_is_exp_neg   = false;
    double a_new          = 0.00;

    a_new=a_abs_value*pow(10.0,-a_exp);
    if (a_new<10 && a_new>9.999999999)
        a_exp++;
    a_is_exp_neg= (a_exp<0);

    
    //
    if(a_exp<(-290) || a_exp>290) {
        if(result!=NULL) {
            sprintf(a_format,"%%%dg",nb_chars);
            sprintf(result,a_format,value);    
        }
        return value;
    }
    //
    if(!a_is_exp_neg) {
        double a_value;
        //
        if(a_exp<a_nb_chars) {

            int a_nb_decs=a_nb_chars-a_exp-2;
            if((a_nb_decs+a_exp+1)>LOC_NB_DIGITS_MAX) a_nb_decs=LOC_NB_DIGITS_MAX-a_exp-1;
            if(a_nb_decs<0) a_nb_decs=0;
            //
            double a_coeff     = pow(10.,(double)a_nb_decs);
            double a_tmp_value = floor(a_abs_value*a_coeff+.5);
            a_value = a_tmp_value/a_coeff;
            if((a_nb_chars - a_exp) > 2 && result!=NULL && fmod(a_value,1) == 0) //should come here without decimal and has (a_nb_chars - a_exp) > 2
            {
                sprintf(a_format,"%%%d.1f",nb_chars);//print 1.0 format
                sprintf(result,a_format,value);
                return value;
            }

            /*If value is increasing its number of digit*/
            {
                double  a_pre_exp_tmp      = log10(a_value);
                double a_floor_tmp        = floor(a_pre_exp_tmp);
                int    a_exp_tmp          = (int)a_floor_tmp;
                if(a_exp_tmp>a_exp)
                {
                    if(a_is_neg) a_value=(-a_value);
                    return my_print_double(a_value, nb_chars,result, always_signed);
                }
            }
            /*Normal case*/

            if(a_is_neg) a_value=(-a_value);
            //
            if(a_result!=NULL) {
                loc_pre_print_value(a_tmp_value,a_is_neg,a_nb_decs,a_nb_chars,a_result);
                if(a_is_signed) { ++a_nb_chars; --a_result; }
                if(if_shift)loc_shift_zeros(a_result,a_nb_chars);
            }
        } else {
            int a_exp_nb_chars = (int)log10((double)a_exp)+1;
            int a_nb_decs      = a_nb_chars-a_exp_nb_chars-3;
            if((a_nb_decs+1)>LOC_NB_DIGITS_MAX) a_nb_decs=LOC_NB_DIGITS_MAX-1;
            if(a_nb_decs<0) a_nb_decs=0;
            //
            double a_coeff     = pow(10.,(double)(a_nb_decs-a_exp));
            double a_tmp_value = a_abs_value*a_coeff+.5;
            a_value = floor(a_tmp_value)/a_coeff;
            if(a_is_neg) a_value=(-a_value);

            //
            if(a_result!=NULL) {
                int a_loc_nb_chars=a_nb_chars-a_exp_nb_chars-1;
                loc_pre_print_value(a_tmp_value,a_is_neg,a_nb_decs,a_loc_nb_chars,a_result);
                if(a_is_signed) { ++a_loc_nb_chars; --a_result; }
                if(if_shift) loc_shift_zeros(a_result,a_loc_nb_chars);
                sprintf(a_result+a_loc_nb_chars,"E%d",a_exp);
            }
        }
        //
        return a_value;    
    } else {
        if(a_exp>=(-3)) {
            int a_nb_decs=a_nb_chars-1;
            if((a_nb_decs+a_exp+1)>LOC_NB_DIGITS_MAX) a_nb_decs=LOC_NB_DIGITS_MAX-a_exp-1;
            if(a_nb_decs<0) a_nb_decs=0;
            //
            double a_coeff     = pow(10.,(double)a_nb_decs);
            double a_tmp_value = floor(a_abs_value*a_coeff+.5);
            double a_value     = a_tmp_value;
            if(a_tmp_value!=0.) while(fmod(a_tmp_value,10.)==0.) {
                a_tmp_value/=10.;
                --a_nb_decs;
            }
            //
            if(a_result!=NULL) {
                bool shift_zero = false;
                if(a_is_signed) --a_result;
                int a_shift=a_nb_chars-a_nb_decs-1;
                if(a_shift>0)
                    a_shift--; // for format -0.1, instead of -.1 or .1 -> 0.1
                else
                    shift_zero=true;
                for(int i=0;i<a_shift;++i) a_result[i]=' ';
                if(a_is_signed) {
                    if(a_is_neg) 
                    {
                        a_result[a_shift++]='-';
                    }
                    else         a_result[a_shift++]=' ';
                }
                if(false == shift_zero)
                   a_result[a_shift++]='0';

                a_result[a_shift++]='.';
                loc_pre_print_value(a_tmp_value,false,0,a_nb_decs,a_result+a_shift);
                //
                
                while(a_result[a_shift]==' ') a_result[a_shift++]='0';
                //if(a_exp<(-1)) {
                //  a_result[a_shift]='0';
                //  if(a_exp<(-2)) a_result[++a_shift]='0';
                //}
                
            }
            //
            a_value/=(a_is_neg ? (-a_coeff) : a_coeff);
            return a_value;
        } else {
            int a_exp_nb_chars = (int)log10((double)(-a_exp))+2;
            int a_nb_decs      = a_nb_chars-a_exp_nb_chars-3;
            if(a_is_signed) a_nb_decs--;
            if((a_nb_decs+1)>LOC_NB_DIGITS_MAX) a_nb_decs=LOC_NB_DIGITS_MAX-1;
            if(a_nb_decs<0) a_nb_decs=0;
            //
            double a_coeff     = pow(10.,(double)(a_nb_decs-a_exp));
            double a_tmp_value = floor(a_abs_value*a_coeff+.5);
            double a_value     = a_tmp_value/a_coeff;
            if(a_is_neg) a_value=(-a_value);
            //
            if(a_result!=NULL) {
                int a_loc_nb_chars=a_nb_chars-a_exp_nb_chars-1;
                //
                loc_pre_print_value(a_tmp_value,a_is_neg,a_nb_decs,a_loc_nb_chars,a_result);	
                if(a_is_signed) { ++a_loc_nb_chars; --a_result; }
                if(if_shift) loc_shift_zeros(a_result,a_loc_nb_chars);
                sprintf(a_result+a_loc_nb_chars,"E%d",a_exp);
            }
            //
            return a_value;                
        }
    }
    return value;
}






static void loc_shift_zeros(char *result,int nb_chars) {
  int a_ind=nb_chars-1,a_shift=0;
  while(a_ind>0 && result[a_ind]=='0') { ++a_shift; --a_ind; }
  
  if(a_ind>0 && result[a_ind]=='.') {
    ++a_shift;
  } else {
    while(a_ind>=0 && result[a_ind]!='.') --a_ind;
    if(a_ind<0) a_shift=0;
  }
  
  //
  if(a_shift) {
    int i,a_nb_shifted_chars=nb_chars-a_shift;
    for(i=0;i<a_nb_shifted_chars;++i) result[nb_chars-1-i]=result[nb_chars-1-i-a_shift];
    for(i=0;i<a_shift;++i) result[i]=' ';
  }
}



static char *loc_pre_print_value(double value,bool is_neg,int nb_decs,int nb_chars,char *result) {
  double a_value = value;
  double a_left  = 0.;
  int    a_ind   = 0;
  //
  result[nb_chars]='\0';
  //
  do {
    a_left=fmod(a_value,10.);
    result[nb_chars-(++a_ind)]='0'+(char)a_left;
    //
    if(a_ind==nb_decs) result[nb_chars-(++a_ind)]='.';
    //
    a_value-=a_left;
    a_value/=10.;
  } while(a_value>0.);
  //
  if(is_neg) result[nb_chars-(++a_ind)]='-';
  while(a_ind<nb_chars) result[nb_chars-(++a_ind)]=' ';
  //
  return result;
}


/* This file contains functions for reading with C functions some ascii files
 * written with some FORTRAN routines, in which you may have 0.999+12 instead of 
 * 0.999E+012
 */
#if defined LINUX && defined _64BITS
static int 
ftnline_into_cline(const char *f_line, const char *format, char c_line[], char new_format[])
{
   char *spec     = NULL ;
   int   spec_pos[NBR_NAS_FIELD] = {0,0,0,0,0,0,0,0,0,0} ;
   int   spec_type[NBR_NAS_FIELD] = {0,0,0,0,0,0,0,0,0,0} ;
   int   spec_length[NBR_NAS_FIELD] = {0,0,0,0,0,0,0,0,0,0} ;
   int   ispec = 0 ; /* ith specifier */
   int   length = 0 ;
   int   current_pos = 0 ;
   int   i=0, j=0;

   
   /* we allow format string to be empty
    * just to get the continuation card
    * and refill the buffer */
   if (format==NULL) return 0 ;
   if (strlen(format)==0) return 0 ;

   strcpy(new_format, format) ;
   spec       = strtok(new_format, "%") ;
   spec_pos[0] = 0 ;
   sscanf(spec, "%d", spec_length) ;
   /* Are we waiting for a real data ? */
   spec_type[ispec] = WaitForReal(spec) ;
  
   spec = strtok(NULL, "%") ;
   while (spec != NULL)
   {
      ispec++ ;
      spec_pos[ispec] = spec_pos[ispec-1]+ spec_length[ispec-1] ;
      sscanf(spec, "%d", spec_length+ispec) ;
      /* Are we waiting for a real data ? */
      spec_type[ispec] = WaitForReal(spec) ;
      spec = strtok(NULL, "%") ;
   }
   
   strcpy(new_format, format) ;

   for (i=0 ; i<21; i++)
   {
       c_line[strlen(f_line)+i] = '\0' ;
   }
   current_pos = 0 ;
   for (i=0 ; i<= ispec ; i++)
   {
       int length = 0 ;

       length = spec_length[i] ;
       /*if (i<ispec)
       {
          length = spec_pos[i+1]-spec_pos[i] ;
       }
       else
       {
          int f_pos = 0 ;
          f_pos = spec_pos[i] ;
          while (f_line[f_pos] != '\0')
          {
             length++ ;
             f_pos ++ ;
          }
       }*/
       if (spec_type[i] != 1)
       {
          if (length >0)
          {
             bool ntd = false ;

             strncpy(c_line+current_pos, f_line+spec_pos[i],length) ;
             for (j=0 ; j<length ; j++)
             {
                 if (c_line[current_pos+j] != ' ')
                 {
                    ntd = true ;
                    break ;
                 }
             }
             if (!ntd)
             {
                c_line[current_pos+length-1] = '0' ;
             }
             current_pos = current_pos+length ;
             c_line[current_pos] = ' ' ;
             current_pos++ ;
             CorrectIntegerFormat(i, new_format) ;
          }
       }
       else
       {
          int begun = 0 ;
          int f_pos = 0 ;
          bool ntd = false ;

          f_pos = spec_pos[i] ;
          if (length >0)
          {
             
             for (j=0 ; j< length ; j++)
             {
                 if (begun == 0)
                 {
                    if (f_line[f_pos] != ' ')
                    {
                        begun = 1 ;
                    }
                    c_line[current_pos] = f_line[f_pos] ;
                    current_pos++ ;
                    f_pos++ ;
                 }
                 else if (begun == 1)
                 {
                    if ((f_line[f_pos] == '+') || (f_line[f_pos] == '-'))
                    {
                       if ((f_line[f_pos-1] != 'E') && (f_line[f_pos-1] != 'e'))
                       {
                          c_line[current_pos] = 'E' ;
                          current_pos++ ;
                          CorrectDoubleFormat(i,new_format) ;
                       }
                    }
                    c_line[current_pos] = f_line[f_pos] ;
                    current_pos++ ;
                    f_pos++ ;
                }
             }
             c_line[current_pos] = ' ' ;
             current_pos++ ;
             for (j=0 ; j<length ; j++)
             {
                 if (c_line[current_pos-length-1+j] != ' ')
                 {
                    ntd = true ;
                    break ;
                 }
             }
             if (!ntd)
             {
                c_line[current_pos+-3] = '0' ;
                c_line[current_pos+length-2] = '.' ;
             }
             CorrectIntegerFormat(i, new_format) ;
          }
          else
          {
          }
       }
   }
   return 0 ;
}
static int 
WaitForReal(char *spec)
{
   /* Are we waiting for a real data ? */
   if ((strstr(spec, "f")) || (strstr(spec, "F"))
    || (strstr(spec, "e")) || (strstr(spec, "E"))
    || (strstr(spec, "g")) || (strstr(spec, "G")))
   {
      /* Yes */
      return 1 ;
   }
   /* No */
   return 0 ;
}
static void
CorrectDoubleFormat(int ispec, char *format)
{
   int j=0, jspec = 0 ;

   while (format[j] != '\0')
   {
      if (format[j] == '%')
      {
         jspec++ ;
         if (jspec == ispec+1) 
         {
            j++ ;
            break ;
         }
      }
      j++ ;
   }
   if (format[j] == '8') 
   {
      format[j]= '9' ;
      return ;
   }
   if ((format[j] == '1') && (format[j+1] == '6')) 
   {
      format[j]= '7' ;
      return ;
   }
   return ;

}
static void
CorrectIntegerFormat(int ispec, char *format)
{
  int i = 0, jspec = 0, blank_pos = 0 ;
  char *dup_format = NULL ;

  while (format[i] != '\0')
  {
     if (format[i] == '%')
     {
        jspec++ ;
        if (jspec == ispec+2)
        {
           blank_pos = i ;
           break ;
        }
     }
     i++ ;
  }
  dup_format = strdup(format) ;
  format[blank_pos] = ' ' ;
  strcpy(format+blank_pos+1, dup_format+blank_pos) ;
  free(dup_format);
  return ;
  
}
#endif
void my_print_double_in_simple_field(double value, int nb_char, char *field)
{
   char fmt_spec[7] ;
   int begin_exponent = -1 ;
   int nb_chars = 0 ;
   int i ;

   fmt_spec[0] = '%' ;
#ifdef WIN32
   nb_chars = nb_char+2 ;
#else
   nb_chars = nb_char+1 ;
#endif
   my_print_double(value,nb_chars,field,0) ;

   for (i=nb_chars-1 ; i>=0 ; i--)
   {
       if ((field[i]=='e')|| (field[i] == 'E'))
       {
          begin_exponent = i ;
	      break ;
       }
   }

#ifdef WIN32
   /* Under Windows :
      it is written : y.xxxE+abc, and we want
                      y.xxx+bc (suppose that a == 0)
    */
   if (begin_exponent != -1)
   {
      if (begin_exponent == nb_chars-5)
      {
         field[nb_chars-5] = field[nb_chars-4] ;
         field[nb_chars-4] = field[nb_chars-2] ;
         field[nb_chars-2] = field[nb_chars-1] ;
         field[nb_chars-1]   = '\0' ;
      }
      else if (begin_exponent > nb_chars-5)
      {
         field[begin_exponent] = field[begin_exponent+1] ;
	     field[begin_exponent+1] = field[begin_exponent+2] ;
	     field[begin_exponent+2] = field[begin_exponent+3] ;
	     if (field[begin_exponent+3] != '\0')
	     {
		    field[begin_exponent+3] = '\0' ;
	     }
      }
	  if (((int)strlen(field)) > nb_char)
	  {
         if (field[0] != ' ')
		 {
	        field[begin_exponent-1] = field[begin_exponent] ;
            field[begin_exponent] = field[begin_exponent+1] ;
		    field[begin_exponent+1] = field[begin_exponent+2] ;
		    field[begin_exponent+2] = field[begin_exponent+3] ;
		 }
		 else
		 {
			 int tmp_length = (int)(strlen(field)) ;

			 for (i=0 ; i<tmp_length ; i++)
			 {
                 field[i] = field[i+1] ;
			 }
		 }
	  }
   }
#else
   /* Under Unix System :
      it is written : Y.xxxE+bc, and we want
                      y.xxx+bc */
   if (begin_exponent == nb_char - 2)
   {
      field[nb_char-2] = field[nb_char-1] ;
      field[nb_char-1] = field[nb_char] ;
      field[nb_char] = field[nb_char+1] ;
      field[nb_char+1] = '\0' ;
   }
#endif
   else
   {
      my_print_double(value,nb_char,field,0) ;
   }
   return ;
}


double hc_scan_double(const char *str, const char *format, int nb_chars, int* nb_read)
{
    char  *a_cell_begin_p = const_cast<char *>(str);
    char   *a_cell_end_p  = a_cell_begin_p+nb_chars;
    char   a_cell_end     = (*a_cell_end_p);  
    char expanded_buffer[31];    
    int i;
    int indx = 1 ;
    int n_occur = 0;

    if (nb_chars>0) 
        *a_cell_end_p='\0';

    bool   a_do_continue  = true;
    char  *a_cell_p       = a_cell_begin_p;
    char   a_cell ='\0';
    while (*a_cell_p==' ') 
    {
        a_cell_p++; // Skip leading spaces 
        a_cell_begin_p++; 
    }
    double result = 0;
    int a_nb_char = 1;
    
    if(nb_chars == 0 && *a_cell_p == ',')
    {
        a_cell_p++; 
        a_cell_begin_p++; 
        *nb_read = -2;
        return result;
    }
    else
    {
        while (a_do_continue)
        {
            switch (*a_cell_p) {
            case '\0': a_do_continue = false;                                        break;
            case ' ':  a_do_continue = false;                                        break;
            case 'D':  a_cell = (*a_cell_p); *a_cell_p = 'E'; a_do_continue = false; break;
            case 'd':  a_cell = (*a_cell_p); *a_cell_p = 'e'; a_do_continue = false; break;
            case '+':  n_occur++; a_cell_p++;                                        break;
            case '-':  n_occur++; a_cell_p++;                                        break;
            default:   ++a_cell_p;                                                   break;
            }

            // Only for free format it should break the loop when it finds a ' in the input data.
            // It is added in a safe way to not disturb the existing flow of fixed format.
            if (!nb_chars)
            {
                if (*a_cell_p == ',')
                    a_do_continue = false;
                a_nb_char++;
            }
        }
    }

    

    if(n_occur >= 1)
    {        
        if ( !((n_occur == 1) &&((a_cell_begin_p[0]=='-') || (a_cell_begin_p[0]=='+'))))
        if(!(strrchr(a_cell_begin_p,'E') || strrchr(a_cell_begin_p,'e')))  
        { 
            expanded_buffer[indx-1] = a_cell_begin_p[0];
            if (nb_chars)
                a_nb_char = nb_chars;
            for (i=1 ; i< a_nb_char; ++i)
            {
                if((a_cell_begin_p[i] == '-' || a_cell_begin_p[i] == '+'))
                {
                    if(!(a_cell_begin_p[i-1] == 'E' || a_cell_begin_p[i-1] == 'e'))
                    {
                        expanded_buffer[indx] = 'E';
                        expanded_buffer[++indx] = a_cell_begin_p[i];
                    }
                }
                else
                    expanded_buffer[indx] = a_cell_begin_p[i];          
                indx++;
            }
            expanded_buffer[i] = '\0'; // Assigining of '\0' at i index fails for fixed format for a usecase explained below and needs to be corrected.
            // If the number of character read is equal to nb_char (= 10 for fixed format in dyna) and the exponential representation does hot 
            // have 'E' eg:- -1.0020+20. Then after the above process the string in the expanded_buffer = -1.0020E+20 and i = 10 and then after  
            // expanded_buffer[i] = '\0' statement expanded_buffer = -1.0020E+2 which is wrong.
            a_cell_begin_p = expanded_buffer;
        }  
    }

    if (format[1] == '-')
    {
        static const char *a_10lf = "%10lf";
        static const char *a_20lf = "%20lf";
        if (nb_chars == 10)
        {
            *nb_read = sscanf(a_cell_begin_p, a_10lf, &result);
        }
        else if (nb_chars == 20)
        {
            *nb_read = sscanf(a_cell_begin_p, a_20lf, &result);
        }
        else
        {
            char mod_format[10];
            sprintf(mod_format, "%%%dlf%c", nb_chars,'\0');
            *nb_read = sscanf(a_cell_begin_p, mod_format, &result);
        }
    }
    else
    {
        *nb_read = sscanf(a_cell_begin_p, format, &result);
    }
    //
    if (nb_chars>0) 
        *a_cell_end_p=a_cell_end;

    if(a_cell!='\0') *a_cell_p=a_cell; // if(a_cell_p!=a_cell_end_p)        

    return result;
}

/*------------------------------------------------------------------
    This function is used to tokenizes the string based on the given
    delimitor.
------------------------------------------------------------------*/
void StringTokenize(const string& str,vector<string>& pTokens, const string &delem)
{
    string delimiters = delem;

    // Skip delimiters at beginning.
    string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // Find first "non-delimiter".
    string::size_type pos     = str.find_first_of(delimiters, lastPos);

    while (string::npos != pos || string::npos != lastPos)
    {
        // Found a token, add it to the vector.
        pTokens.push_back(str.substr(lastPos, pos - lastPos));
        // Skip delimiters.  Note the "not_of"
        lastPos = str.find_first_not_of(delimiters, pos);
        // Find next "non-delimiter"
        pos = str.find_first_of(delimiters, lastPos);
    }
}

char *my_strdup(const char *src)
{
#ifdef _WIN32
  return _strdup(src);
#else
  return strdup(src);
#endif
}

char *my_strconvtolowercase(const char *str)
{
    char *result = (char *)own_calloc((int)(strlen(str))+1, sizeof(char));
    strcpy(result, str);

    int i = 0;
    for(i=0; i<=strlen(str); i++)
    {
        if(str[i]>='A' && str[i]<='Z')
        {
            result[i] = str[i]+32;
        }
    }
    if(strlen(result) == 0)
    {
        own_free(result);
        result = NULL;
    }
    return result;
}

bool my_compare_str(const char *title, const char *src_text, bool is_match_case, bool is_whole_name, bool do_wild_card_check)
{
    bool case_comp_check = false;
    bool whole_name_check = false;
    bool wild_card_check = false;

    if(is_match_case)
    {
        // if the obj_title contains search title
        if(strstr(src_text, title))
            case_comp_check = true;
    }
    else
    {
        if(strcasecmp(src_text, title) == 0)
            case_comp_check = true;

        if(!is_whole_name && case_comp_check==false)
        {
            // if the obj_title contains search title
            if(strstr(src_text, title))
                case_comp_check = true;
            if(case_comp_check==false)
            {
                char *str1 = my_strconvtolowercase(src_text);
                char *str2 = my_strconvtolowercase(title);
                if(str1 != NULL && str2 != NULL && strstr(str1, str2))
                    case_comp_check = true;
                own_free(str1);
                own_free(str2);
            }
        }
    }

    if(!is_whole_name)
    {
        if(strncmp(title, src_text, strlen(title)) == 0)
            whole_name_check = true;
        if(!is_match_case && whole_name_check== false)
        {
            if(strncasecmp(title, src_text, strlen(title)) == 0)
                whole_name_check = true;
        }
    }
    else
    {
        if(strcmp(src_text, title) == 0)
            whole_name_check = true;
    }

    if(do_wild_card_check)
    {
        // ignore '*' at the beg and end
        int size = (int)(strlen(title));
        if(size > 0)
        {
            char *copy_title = (char *)own_calloc(size, sizeof(char));
            int i = 0, j = 0;
            if(title[0] == '*' && title[size-1] == '*')
            {
                for(i=1; i<size-1; i++)
                {
                    copy_title[j] = title[i];
                    j++;
                }
                
                if(!is_match_case)
                {
                    char *str1 = my_strconvtolowercase(copy_title);
                    char *str2 = my_strconvtolowercase(src_text);
                    if(str1 != NULL && str2 != NULL && strstr(str2, str1))
                        wild_card_check = true;
                    own_free(str1);
                    own_free(str2);
                }
                else
                {
                    if(strstr(src_text, copy_title))
                        wild_card_check = true;
                }
            }
            else if(title[size-1] == '*')
            {
                // ignore '*' at the end
                // search only beg
                for(i=0; i<size-1; i++)
                {
                    copy_title[j] = title[i];
                    j++;
                }
                int search_size = (int)(strlen(src_text));
                int test_size = size-1;
                if(search_size >= test_size)
                {
                    bool beg_same = true;
                    if(!is_match_case)
                    {
                        char *str1 = my_strconvtolowercase(copy_title);
                        char *str2 = my_strconvtolowercase(src_text);
                        if(str1 != NULL && str2 != NULL)
                        {
                            for(i=0; i<test_size; i++)
                            {
                                if(str2[i] != str1[i])
                                {
                                    beg_same = false;
                                }
                            }
                        }
                        own_free(str1);
                        own_free(str2);
                    }
                    else
                    {
                        for(i=0; i<test_size; i++)
                        {
                            if(src_text[i] != copy_title[i])
                            {
                                beg_same = false;
                            }
                        }
                    }
                    if(beg_same)
                        wild_card_check = true;
                }
            }
            else if(title[0] == '*')
            {
                // ignore '*' at the beg
                // search only end
                for(i=1; i<size; i++)
                {
                    copy_title[j] = title[i];
                    j++;
                }

                int search_size = (int)strlen(src_text);
                int test_size = size-1;
                if(search_size >= test_size)
                {
                    j = 0;
                    bool end_same = true;
                    if(!is_match_case)
                    {
                        char *str1 = my_strconvtolowercase(copy_title);
                        char *str2 = my_strconvtolowercase(src_text);
                        if(str1 != NULL && str2 != NULL)
                        {
                            for(i=search_size-test_size; i<search_size; i++, j++)
                            {
                                if(str2[i] != str1[j])
                                {
                                    end_same = false;
                                }
                            }
                        }
                        own_free(str1);
                        own_free(str2);
                    }
                    else
                    {
                        for(i=search_size-test_size; i<search_size; i++, j++)
                        {
                            if(src_text[i] != copy_title[j])
                            {
                                end_same = false;
                            }
                        }
                    }
                    if(end_same)
                        wild_card_check = true;
                }
            }
            own_free(copy_title);
        }
    }
    if(case_comp_check || whole_name_check || wild_card_check)
        return true;
    return false;
}

int my_compare_str_in_c(const char *title, const char *src_text, int is_match_case, int is_whole_name, int do_wild_card_check)
{
    bool mcase = (is_match_case == 1)?true:false;
   // bool wname = (is_whole_name == 1)?true:false;
   // bool wcard = (do_wild_card_check == 1)?true:false;
    return (int)my_compare_str(title, src_text, mcase, (bool)(is_whole_name != 0), (bool)(do_wild_card_check != 0));
}

void general_file_replace_path_separator_by_slash (char *path)
{
    int length = 0, i;
    if (path == NULL) return;
    length = (int)strlen (path);
    for (i = 0; i < length; i++) {
        if (path [i] == '\\') path [i] = '/';
    }
}

void convert_vect_to_string(string& arg_list, const vector<string>& argvect)
{
    if (!argvect.empty())
    {
        int size = (int)argvect.size();
        for (int i = 0; i < size; i++)
        {
            string arg = argvect[i];
            if (i == 0)
            {
                arg_list += "<";
                arg_list += arg;
            }
            if (i != size - 1)
                arg_list += ",";
        }
        arg_list += ">";
    }
}
