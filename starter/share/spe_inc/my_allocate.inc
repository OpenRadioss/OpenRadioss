Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
#ifdef COMP_ARMFLANG

#ifndef MY_ALLOCATE
#define MY_ALLOCATE(A,L)\
ALLOCATE(A(L),STAT=MY_IERR);\
CALL ERROR_ALLOC(#A,MY_IERR)
#endif

#ifndef MY_ALLOCATE2D
#define MY_ALLOCATE2D(A,L1,L2)\
ALLOCATE(A(L1,L2),STAT=MY_IERR);\
CALL ERROR_ALLOC(#A,MY_IERR)
#endif

#ifndef MY_ALLOCATE3D
#define MY_ALLOCATE3D(A,L1,L2,L3)\
ALLOCATE(A(L1,L2,L3),STAT=MY_IERR);\
CALL ERROR_ALLOC(#A,MY_IERR)
#endif

#else

#ifdef COMP_GFORTRAN

#ifndef MY_ALLOCATE
#define MY_ALLOCATE(ARRAY,LENGTH)\
ALLOCATE(ARRAY(LENGTH),STAT=MY_IERR);\
IF(MY_IERR/=0) THEN;\
CALL ANCMSG(MSGID=268,MSGTYPE=MSGERROR,ANMODE=ANSTOP,C1="#ARRAY");\
ENDIF
#endif

#ifndef MY_ALLOCATE2D
#define MY_ALLOCATE2D(ARRAY,LENGTH1,LENGTH2)\
ALLOCATE(ARRAY(LENGTH1,LENGTH2),STAT=MY_IERR);\
IF(MY_IERR/=0) THEN;\
CALL ANCMSG(MSGID=268,MSGTYPE=MSGERROR,ANMODE=ANSTOP,C1="#ARRAY");\
ENDIF
#endif

#ifndef MY_ALLOCATE3D
#define MY_ALLOCATE3D(ARRAY,LENGTH1,LENGTH2,LENGTH3)\
ALLOCATE(ARRAY(LENGTH1,LENGTH2,LENGTH3),STAT=MY_IERR);\
IF(MY_IERR/=0) THEN;\
CALL ANCMSG(MSGID=268,MSGTYPE=MSGERROR,ANMODE=ANSTOP,C1="#ARRAY");\
ENDIF
#endif

#else
#ifndef MY_ALLOCATE
#define MY_ALLOCATE(ARRAY,LENGTH)\
ALLOCATE(ARRAY(LENGTH),STAT=MY_IERR);\
IF(MY_IERR/=0) THEN;\
CALL ANCMSG(MSGID=268,MSGTYPE=MSGERROR,ANMODE=ANSTOP,C1=#ARRAY);\
ENDIF
#endif
#ifndef MY_ALLOCATE2D
#define MY_ALLOCATE2D(ARRAY,LENGTH1,LENGTH2)\
ALLOCATE(ARRAY(LENGTH1,LENGTH2),STAT=MY_IERR);\
IF(MY_IERR/=0) THEN;\
CALL ANCMSG(MSGID=268,MSGTYPE=MSGERROR,ANMODE=ANSTOP,C1=#ARRAY);\
ENDIF
#endif
#ifndef MY_ALLOCATE3D
#define MY_ALLOCATE3D(ARRAY,LENGTH1,LENGTH2,LENGTH3)\
ALLOCATE(ARRAY(LENGTH1,LENGTH2,LENGTH3),STAT=MY_IERR);\
IF(MY_IERR/=0) THEN;\
CALL ANCMSG(MSGID=268,MSGTYPE=MSGERROR,ANMODE=ANSTOP,C1=#ARRAY);\
ENDIF
#endif

#endif

#endif

