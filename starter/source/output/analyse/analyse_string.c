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
#include <string.h>

#include "analyse_memory.h"

#include "analyse_string.h"

/*****************************************************************************
 * replace the strset function which is not in ANSI string.h
 *****************************************************************************/
char *analyse_string_strset(char *name, int ch)
{
  int length,i;

  if ( name != NULL)
    {
      length = strlen(name);
 
      for ( i=0; i<length; i++)
	{
	  name[i]= ch;
	}

    }
  return name;
}


/*****************************************************************************
 * replace the strrev function which is not in ANSI string.h
 *****************************************************************************/
char *analyse_string_strrev(char *name)
{
  char *reverse;
  int length,i;

  if ( name != NULL)
    {
      length = strlen(name);
      reverse = (char *) analyse_malloc( ( length)*sizeof(char));
      
      for ( i=0; i<length; i++)
	{
	  reverse[i]=name[length-1-i];
	}
 
      for ( i=0; i<length; i++)
	{
	  name[i]=reverse[i];
	}

      analyse_free(reverse);
    }
  return name;
}

/*****************************************************************************
 * remove blanks at the beginning of a string " a a cc  " -> "a a cc  " 
 *****************************************************************************/
char *analyse_string_fit_start(char *name)
{
  if (name != NULL)
    {
      while ( (*name) == ' ')
	{
	  strcpy(name,name+1);
	}
    }
  return name;
}


/*****************************************************************************
 * remove blanks at the end of a string " a a cc  " -> " a a cc" 
 *****************************************************************************/
char *analyse_string_fit_end(char *name)
{
  int length;

  if ( name != NULL)
    {
      length = strlen(name);

      while ( ( length > 0) && ( *(name+length-1) == ' ' ) )
	{
	  *(name+length-1)= '\0';
	  length = length -1;
	}
 
    }
  return name;	   
}



/*****************************************************************************
 * remove blanks at the beginning AND the end of a string " a a cc  " -> "a a cc" 
 *****************************************************************************/
char *analyse_string_fit_start_end(char *name)
{
  if (name != NULL)
    {
      analyse_string_fit_start(name);
      analyse_string_fit_end(name);
    }
  return name;
}


/*****************************************************************************
 * remove blanks in a string " a a cc  " ->"aacc" 
 *****************************************************************************/
char *analyse_string_fit_all(char *name)
{
  char *blank;
  
  if (name != NULL)
    {
      blank = strchr(name,' ');
      
      while ( blank != NULL )
	{
	  strcpy(blank,blank+1);
	  blank = strchr(name,' ');
	}
    }
  return name;
}
/*****************************************************************************
 * returns 0 if strlen(string) is between n1 and n2
 * returns 1 if > max(n1,n2) ;
 * returns -1 if < min(n1,n2) ;
 *****************************************************************************/
extern int analyse_string_length_brackett(char *string, int n1, int n2)
{
   int length = 0 ;
   int buf  = 0 ;
   char *buf_string = NULL ;
   char *pos = NULL ;

   if (strlen(string)==0) return -1 ;
   if (n1-n2 > 0)
   {
      buf = n1 ;
      n1 = n2 ;
      n2 = buf ;
   }

   buf_string = (char *)analyse_calloc(strlen(string)+1, sizeof(char)) ;

   strcpy(buf_string, string) ;
   pos = strrchr(buf_string, '/') ;
   if (pos != NULL)
   {
      pos = pos + 1 ;
      length  = strlen(pos) ;
   }
   else
   {
      length  = strlen(buf_string) ;
   }

   if (length < n1) 
   {
      analyse_free(buf_string) ;
      return -1 ;
   }
   if (length > n2) 
   {
      analyse_free(buf_string) ;
      return 1 ;
   }

   analyse_free(buf_string) ;
   return 0 ;
}

/*****************************************************************************
 * converts an int, into a char
 *****************************************************************************/
void analyse_convert_int_to_string(int nb_int, int *tab_int, char *message)
{
  int i;

  if ( ( message != NULL ) &&
       ( tab_int != NULL ))
    {
      for(i=0; i<nb_int; i++)
	{
	  message[i]=(char)(tab_int[i]);
	}
      
      message[nb_int]='\0';
    }
  else
    {
      if ( message != NULL ) message[0]='\0';
    }
}
/*****************************************************************************
 * converts a char into a tab of int
 *****************************************************************************/
void analyse_convert_string_to_int(char *message, int *nb_int, int **tab_int)
{
 int i;
 
 if ( ( message != NULL ) &&
      ( nb_int != NULL ) &&
      ( tab_int != NULL ))
   {
     *nb_int=strlen(message);
     (*tab_int) = (int *)analyse_malloc((*nb_int)*sizeof(int));
     
     for(i=0; i<(*nb_int); i++)
       {
	 (*tab_int)[i] = (int)message[i];
       }
   }
 else
   {
     if ( nb_int != NULL) *nb_int = 0;
     if ( tab_int != NULL) *tab_int = NULL;
   }
}
