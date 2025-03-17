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

//

#include <UTILS/win32_utils.h>
#include <UTILS/mv_cstring.h>
#include "mv_utils.h"
#include "mv_type.h"

#include <KERNEL_BASE/utils.h>

extern "C" int mvc_isNumber(const char *s)
{
   if(NULL == s)
        return 0;
   size_t len = strlen(s);
   for (int i = 0; i < len; ++i)
   {
      if ((isdigit(s[i]) == false) && ((s[i] != '.') && (s[i] != '-')))
           return 0;
   }
   return 1;
}

extern "C" int mvc_setenv (const char *name, const char *value, int overwrite)
{
    char *name_and_value=NULL;
    int length = 0;
  
    if (overwrite == 0)
    {
        if (getenv (name) != NULL) return 0;
    }

     if (value == NULL) return -1; 

    length = (int)strlen (name) + (int)strlen (value) + 2;
    name_and_value = (char*) calloc (length, sizeof(char));
    if (name_and_value == NULL) return -1;

    sprintf (name_and_value, "%s=%s", name, value);

/*
    string str_name(name);
    string str_val(value);
    string strname_value = str_name + "=" + str_val; 
*/
    return putenv (name_and_value);
      
    /* if I understand the man of putenv, we must not free name_and_value! */
}


