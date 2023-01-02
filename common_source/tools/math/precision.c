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
#include <string.h>
#include <stdlib.h>
#include <math.h>

#define _FCALL

int _FCALL MY_SHIFTL(a,n)
int *a,*n;
{
        return *a << *n;
}
int my_shiftl(a,n)
int *a,*n;
{
        return *a << *n;
}
int my_shiftl_(a,n)
int *a,*n;
{
        return *a << *n;
}
int _FCALL MY_SHIFTR(a,n)
int *a,*n;
{
        return *a >> *n;
}
int my_shiftr(a,n)
int *a,*n;
{
        return *a >> *n;
}
int my_shiftr_(a,n)
int *a,*n;
{
        return *a >> *n;
}
int _FCALL MY_AND(a,b)
int *a,*b;
{
        return *a & *b;
}
int my_and(a,b)
int *a,*b;
{
        return *a & *b;
}
int my_and_(a,b)
int *a,*b;
{
        return *a & *b;
}
int _FCALL MY_OR(a,b)
int *a,*b;
{
        return *a | *b;
}
int my_or(a,b)
int *a,*b;
{
        return *a | *b;
}
int my_or_(a,b)
int *a,*b;
{
        return *a | *b;
}

/* routines calcul de la precision flotante minimum */

void floatmin(a,b,flm)
char *a, *b;
float *flm;
{
 int l;
 l = b-a;
 if(l==4) *flm=1.2E-7 ;
 else *flm=2.2E-16 ;
}

void floatmin_(a,b,flm)
char *a, *b;
float *flm;
{
 int l;
 l = b-a;
 if(l==4) *flm=1.2E-7 ;
 else *flm=2.2E-16 ;
}

void _FCALL FLOATMIN(a,b,flm)
char *a, *b;
float *flm;
{
 int l;
 l = b-a;
 if(l==4) *flm=1.2E-7 ;
 else *flm=2.2E-16 ;
}

void floatmin__(a,b,flm)
char *a, *b;
float *flm;
{
 int l;
 l = b-a;
 if(l==4) *flm=1.2E-7 ;
 else *flm=2.2E-16 ;
}

