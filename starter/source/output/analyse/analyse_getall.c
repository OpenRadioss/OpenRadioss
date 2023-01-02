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
/*********************************************************************
 *                        INCLUDES
 *********************************************************************/
#include <string.h>
#include <stdio.h>

#include "analyse_define.h"

#include "analyse_print.h"
#include "analyse_string.h"
#include "analyse_memory.h"

#include "analyse_getall.h"

/*********************************************************************
 *
 * GLOBAL FUNCTIONS
 *
 ********************************************************************/

void analyse_fitline(char *line)
{
  int index,charcount;
  
  char *blank = "                                                                                ";
    
  index = strlen(line);

  if( *(line+index - 1) == '\n')
    index = index - 1;
  charcount = 80 - index;
  if(charcount > 0)
    {
      strncpy(line+index,blank,charcount);
    }
  *(line+80)='\0';
}


int analyse_getint(int *x,int ncount,char *line)

{
  int readret;
  char linecopy[21];
  #ifdef _WIN64
  strncpy_s(linecopy,21,line,ncount);
  #else
  strncpy(linecopy,line,ncount);
  #endif
  *(linecopy+ncount)='\0';
  readret = sscanf(linecopy,"%d",x);
  if(readret < 0) 
    {
      *x=0;
      return(0);
    }
  return(readret);
}

int analyse_getdouble(double *x,int ncount,char *line)
     
{
  int readret;
  char linecopy[21];
  
  #ifdef _WIN64
  strncpy_s(linecopy,21,line,ncount);
  #else
  strncpy(linecopy,line,ncount);
  #endif
  *(linecopy+ncount)='\0';
  readret = sscanf(linecopy,"%lf",x);
  if(readret < 0) 
    {
      *x=0;
      return(0);
    }
  return(readret);
}


int analyse_getstring(char *str,int ncount,char *line)
{
  strncpy(str,line,ncount);
  *(str+ncount)='\0';

  analyse_string_fit_start_end(str);
  return(0);
}

/*
 * Get Line
 */
int analyse_getline(char line[],FILE *infile,char *infilename,int *linecount)
{
  char tline[ANALYSE_SIZE_OF_LINE];
  do
    {
      if(fgets(line,ANALYSE_SIZE_OF_LINE,infile) == NULL)
	{
	  sprintf(tline, "\n*** ERROR: premature EOF line: %d, File: %s ***\n\n\n",*linecount,infilename); 
	  Analyse_Print_Error_Level(tline,2);
	  return(-1);
	}
      *linecount = (*linecount) + 1;

      if(!strncmp(line, "#include", 8) || !strncmp(line, "#enddata", 8))
	{
	  break;
	}
    } while (( *line == '#' ) || ( *line == '$'));

  analyse_fitline(line);
  return(0);
}

int analyse_getline_with_dollars(char line[],FILE *infile,char *infilename,int *linecount)
{
  char tline[ANALYSE_SIZE_OF_LINE];
  do
    {
      if(fgets(line,ANALYSE_SIZE_OF_LINE,infile) == NULL)
	{
	  sprintf(tline, "\n*** ERROR: premature EOF line: %d, File: %s ***\n\n\n",*linecount,infilename); 
	  Analyse_Print_Error_Level(tline,2);
	  return(-1);
	}
      *linecount = (*linecount) + 1;

      if(!strncmp(line, "#include", 8) || !strncmp(line, "#enddata", 8))
	{
	  break;
	}
    } while ( *line == '#' );

  analyse_fitline(line);
  return(0);
}

int analyse_getcommentline(char *comment, FILE *infile,char *infilename,int *linecount)
{
  long infile_save;

  char line[ANALYSE_SIZE_OF_LINE];
  char tline[ANALYSE_SIZE_OF_LINE];

  infile_save = ftell(infile);

  if(fgets(line,ANALYSE_SIZE_OF_LINE,infile) == NULL)
    {
      sprintf(tline, "\n*** ERROR: premature EOF line: %d, File: %s ***\n\n\n",*linecount,infilename);
      Analyse_Print_Error_Level(tline,2);
      return(-1);
    }

  if ( (strncmp(line, "#include", 8) == 0) || (strncmp(line, "#enddata", 8) == 0))
    {
      sprintf(tline," *** Read Error : #include or #enddata inside a block in File %s at line %d\n\n***",infilename, *linecount);
      Analyse_Print_Error_Level(tline,2);
      return(-1);
    }

  if( ( *line == '#') && (strncmp(line, "#-", 2) != 0)  )
    {
      analyse_fitline(line);
      analyse_string_fit_end(line);
      strcpy(comment, line);
      *linecount = (*linecount)+1;
    }
  else
    {
      fseek(infile,infile_save,0);
    }
  
  return(0);
}


/* 
 * gets a list of ids (int), on a underminate nb of lines, 10 ids, per line
 */

int analyse_getlist_of_int(int *nb_elt, int **elt, FILE *infile,char *infilename,int *linecount)
{
  char line[ANALYSE_SIZE_OF_LINE];

  long infile_save;
  int linecount_save;

  int stop_flag = 0;
  int elt_count = 0;

  int *elt_tmp = NULL;

  int i = 0;

#ifdef ANALYSE_DEBUG
  sprintf(tline,"Entering analyse_getlist_of_int at line %d\n",*linecount);
  Analyse_Print_Debug(tline);
#endif


  elt_tmp = (int *) analyse_malloc(10*sizeof(int));

  while(!stop_flag)
    {
      infile_save = ftell(infile);
      linecount_save=*linecount;

      /* if trouble in reading line, then exit */
      if(analyse_getline(line,infile,infilename,linecount))
	{
	  analyse_free(elt_tmp);
	  *elt = NULL;
	  *nb_elt=0;
	  return (-1);
	}

      /* if new line is start of a new block, exit */
      if ( ( *line == '/' ) || (strncmp(line, "#include", 8) == 0) || (strncmp(line, "#enddata", 8) == 0) )
	{
	  fseek(infile,infile_save,0);
	  *linecount = linecount_save;
	  stop_flag = 1;
	  continue;
	}

      /* if comment line, forget it */
      if ( ( *line == '#' ) || ( *line == '$') )
	continue;
	
      for(i=0;i<10;i++)
	{
	  if((!analyse_getint(elt_tmp+elt_count,8,line+8*i)) || (elt_tmp[elt_count] == 0))
	    { 
	      stop_flag = 1; 
	      break; 
	    }
	  else
	    {
	      elt_count = elt_count+1;
	    }
	}
      
      if ( i == 10)
	elt_tmp= (int *) analyse_realloc(elt_tmp, (size_t) ((elt_count+10) * sizeof(int)));

    }
  
  *nb_elt= elt_count;

  if (*nb_elt == 0)
    {
      analyse_free(elt_tmp);
      *elt=NULL;
    }
  else
    {
      elt_tmp = (int *) analyse_realloc(elt_tmp, (size_t) ((*nb_elt) * sizeof(int)));
      *elt= elt_tmp;
    }

#ifdef ANALYSE_DEBUG
  sprintf(tline,"Exit analyse_getlist_of_int at line %d, with %d elts\n",*linecount,*nb_elt);
  Analyse_Print_Debug(tline);
#endif


  return(0);    
}


/* 
 * gets a list of char[9] , on a underminate nb of lines, 10 ids, per line
 */

int analyse_getlist_of_char(int *nb_elt, char ***elt, FILE *infile,char *infilename,int *linecount)
{
  char line[ANALYSE_SIZE_OF_LINE];

  long infile_save;
  int linecount_save;

  int stop_flag = 0;
  int elt_count = 0;

  char **elt_tmp;

  int i = 0;

#ifdef ANALYSE_DEBUG
  sprintf(tline,"Entering analyse_getlist_of_char at line %d\n",*linecount);
  Analyse_Print_Debug(tline);
#endif

  elt_tmp = (char **) analyse_malloc(10*sizeof( char *));

  while(!stop_flag)
    {
      infile_save = ftell(infile);
      linecount_save=*linecount;

      /* if trouble in reading line, then exit */
      if(analyse_getline(line,infile,infilename,linecount))
	{
	  analyse_free(elt_tmp); 
	  *elt= NULL;
	  *nb_elt=0;
	  return (-1);
	}

      /* if new line is start of a new block, exit */
      if ( ( *line == '/' ) || (strncmp(line, "#include", 8) == 0) || (strncmp(line, "#enddata", 8) == 0) )
	{
	  fseek(infile,infile_save,0);
	  *linecount = linecount_save;
	  stop_flag = 1;
	  continue;
	}

      /* if comment line, forget it */
      if ( ( *line == '#' ) || ( *line == '$' )) 
	continue;
	

      for(i=0;i<10;i++)
	{
	  *(elt_tmp+elt_count) = (char *) analyse_malloc(9*sizeof(char));
	  analyse_getstring(*(elt_tmp+elt_count),8,line+8*i);
	  if (strlen(*(elt_tmp+elt_count)) == 0)
	    {
	      analyse_free(*(elt_tmp+elt_count));
	      stop_flag = 1;
	      break; 
	    }
	  else
	    {
	      elt_count = elt_count+1;
	    }
	}
      
      if ( i == 10) /* if we read a whole line, prepare memory for the next one */
	elt_tmp = (char **) analyse_realloc(elt_tmp, (size_t) ((elt_count+10) * sizeof( char *)));

    }
  
  *nb_elt= elt_count;

  if ( *nb_elt == 0)
    {
      analyse_free(elt_tmp);
      *elt = NULL;
    }
  else
    {
      elt_tmp = (char **) analyse_realloc(elt_tmp, (size_t) ( (*nb_elt) * sizeof( char *)));
      *elt= elt_tmp;
    }

#ifdef ANALYSE_DEBUG
  sprintf(tline,"Exit analyse_getlist_of_char at line %d, with %d elts\n",*linecount,*nb_elt);
  Analyse_Print_Debug(tline);
#endif

  return(0);    
}

/* 
 * gets a list of char[9] , on a underminate nb of lines, 10 ids, per line
 *
 * if a following char, starts with '@', cat it with the previous one
 *
 */
int analyse_getlist_of_char_2(int *nb_elt, char ***elt, FILE *infile,char *infilename,int *linecount)
{
  char line[ANALYSE_SIZE_OF_LINE];

  long infile_save;
  int linecount_save;

  int stop_flag = 0;
  int elt_count = 0;

  char **elt_tmp;
  char *tmp;

  int i = 0;

#ifdef ANALYSE_DEBUG
  sprintf(tline,"Entering analyse_getlist_of_char at line %d\n",*linecount);
  Analyse_Print_Debug(tline);
#endif

  elt_tmp = (char **) analyse_malloc(10*sizeof( char *));

  while(!stop_flag)
    {
      infile_save = ftell(infile);
      linecount_save=*linecount;

      /* if trouble in reading line, then exit */
      if(analyse_getline(line,infile,infilename,linecount))
	{
	  analyse_free(elt_tmp); 
	  *elt= NULL;
	  *nb_elt=0;
	  return (-1);
	}

      /* if new line is start of a new block, exit */
      if ( ( *line == '/' ) || (strncmp(line, "#include", 8) == 0) || (strncmp(line, "#enddata", 8) == 0) )
	{
	  fseek(infile,infile_save,0);
	  *linecount = linecount_save;
	  stop_flag = 1;
	  continue;
	}

      /* if comment line, forget it */
      if ( ( *line == '#' ) || ( *line == '$' )) 
	continue;
	

      for(i=0;i<10;i++)
	{
	  *(elt_tmp+elt_count) = (char *) analyse_malloc(9*sizeof(char));
	  analyse_getstring(*(elt_tmp+elt_count),8,line+8*i);
	  if (strlen(*(elt_tmp+elt_count)) == 0)
	    {
	      analyse_free(*(elt_tmp+elt_count));
	      stop_flag = 1;
	      break; 
	    }
	  else
	    {
	      if ( ( elt_count > 0) &&
		   (*(elt_tmp+elt_count))[0] == '@' )
		{
		  tmp = (char *) analyse_malloc( (strlen(*(elt_tmp+elt_count-1))+strlen(*(elt_tmp+elt_count)))*sizeof(char));
		  strcpy(tmp, *(elt_tmp+elt_count-1));
		  strcat(tmp, *(elt_tmp+elt_count) +1);
		  analyse_free(*(elt_tmp+elt_count-1));
		  analyse_free(*(elt_tmp+elt_count));
		  *(elt_tmp+elt_count-1) = tmp;
		}
	      else
		{
		  elt_count = elt_count+1;
		}
	    }
	}
      
      if ( i == 10) /* if we read a whole line, prepare memory for the next one */
	elt_tmp = (char **) analyse_realloc(elt_tmp, (size_t) ((elt_count+10) * sizeof( char *)));

    }
  
  *nb_elt= elt_count;

  if ( *nb_elt == 0)
    {
      analyse_free(elt_tmp);
      *elt = NULL;
    }
  else
    {
      elt_tmp = (char **) analyse_realloc(elt_tmp, (size_t) ( (*nb_elt) * sizeof( char *)));
      *elt= elt_tmp;
    }

#ifdef ANALYSE_DEBUG
  sprintf(tline,"Exit analyse_getlist_of_char at line %d, with %d elts\n",*linecount,*nb_elt);
  Analyse_Print_Debug(tline);
#endif

  return(0);    
}

int analyse_getkey(int pos, char *line, char *name)
{
  char *point_pos;
  char *point_next;

  int count = 0;
  char *scan;

  scan = line;

  while ( count < pos )
    {
      point_pos=strchr(scan,'/');
      if ( point_pos == NULL) return (-1);
      scan = point_pos+1;
      count = count+1;
    }
  
  point_next= strchr(scan,'/');
  if ( point_next == NULL)
    strcpy(name, scan);
  else
    {
      strncpy(name, scan, point_next - point_pos -1);
      name[point_next - point_pos -1]='\0';
    }

  analyse_string_fit_start_end(name);


  return (0);
}


/*
 * counts the lines btw the current line and the next /KEYWORD line
 */
int analyse_getsize_of_enum ( int *nb_line, int *nb_dataline, FILE *infile,char *infilename,int *linecount)
{
  char line[ANALYSE_SIZE_OF_LINE];
  char tline[ANALYSE_SIZE_OF_LINE];

  long infile_save;
  int linecount_save;

  *nb_line = 0;
  *nb_dataline = 0;

#ifdef ANALYSE_DEBUG
  sprintf(tline, " Entering analyse_getsize_of_enum at line %d\n",*linecount);
  Analyse_Print_Debug(tline);
#endif

  infile_save = ftell(infile);
  linecount_save= *linecount;

  do
    {
      if(fgets(line,ANALYSE_SIZE_OF_LINE,infile) == NULL)
	{
	  sprintf(tline, "\n*** ERROR: premature EOF line: %d, File: %s ***\n\n\n", ( *linecount+1) ,infilename);
	  Analyse_Print_Error_Level(tline,2);
	  return(-1);
	}

      if ( ( *line == '/' ) || (strncmp(line, "#include", 8) == 0) || (strncmp(line, "#enddata", 8) == 0) )
	{
	  fseek(infile,infile_save,0);
	  *linecount= linecount_save;

#ifdef ANALYSE_DEBUG
	  sprintf(tline, " Exit analyse_getsize_of_enum with nb_line = %d, and nb_dataline = %d\n",*nb_line,*nb_dataline);
	  Analyse_Print_Debug(tline);
#endif	  
	  return (0);
	}
      
      /* if no comment line, one more data */
      if ( ( *line != '#' ) && ( *line != '$') )
	{
	  *nb_dataline = (*nb_dataline)+1;
	}
      
      *nb_line = (*nb_line)+1;
      *linecount = (*linecount)+1;
      
    } while (1);
  
}   
