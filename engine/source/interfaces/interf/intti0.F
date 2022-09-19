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
Chd|====================================================================
Chd|  INTTI0                        source/interfaces/interf/intti0.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        INTTI12A                      source/interfaces/interf/intti12.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE INTTI0(IPARI ,X      ,V    ,WA     ,ITAB ,
     .                  IPARG ,IXS,SEGVAR,SKEW,ISKWN,INTBUF_TAB )
      USE SEGVAR_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),ITAB(*),IPARG(*),IXS(*),ISKWN(*)
      my_real X(*), V(*), WA(*), SKEW(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(t_segvar) :: SEGVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NTY, JI, JB,TEST,IPRI
C-----------------------------------------------
      SAVE TEST
      DATA TEST/0/
C======================================================================= 
      DO N=1,NINTER
        NTY   =IPARI(7,N)
        IF(NTY==12)THEN
          CALL INTTI12A(
     .     IPARI(1,N),X      ,WA   ,ITAB,
     .     SEGVAR,TEST,SKEW,ISKWN,INTBUF_TAB(N))
        ENDIF
      ENDDO
C
      IF(NINTER>0)THEN
        IPRI=MOD(NCYCLE,IABS(NCPRI))
        IF(IPRI==0 .AND.TEST/=0)
     .  WRITE(ISTDO,'(A,A)')
     . ' *** WARNING FLUID INTERFACES SOME SECONDARY NODES',
     . ' LIE OUTSIDE OF MAIN SEGMENT'
       TEST=0
      ENDIF
c-----------
      RETURN
      END
