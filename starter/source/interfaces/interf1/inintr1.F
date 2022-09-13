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
Chd|  ININTR1                       source/interfaces/interf1/inintr1.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        I20STIFN                      source/interfaces/inter3d1/i20stifn.F
Chd|        I24STSECND                    source/interfaces/inter3d1/i24stslav.F
Chd|        I25STSECND                    source/interfaces/inter3d1/i25stslav.F
Chd|        I7STSECND                     source/interfaces/inter3d1/i7stslav.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ININTR1(IPARI   ,STIFINT, INTBUF_TAB ,STFAC)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*)
C     REAL
      my_real
     .   STIFINT(*), STFAC(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N
      INTEGER NTY, IGSTI
C
      IF(N2D/=0)RETURN
C
      DO N=1,NINTER
       NTY=IPARI(7,N)
       IF (NTY==7.OR.NTY==22)THEN
         IGSTI=IPARI(34,N)
         IF(IGSTI>1)THEN
           CALL I7STSECND(
     1      IPARI(1,N),STIFINT,INTBUF_TAB(N))
         ENDIF
       ELSEIF (NTY==20)THEN
         CALL I20STIFN(IPARI(1,N),STIFINT,INTBUF_TAB(N))
       ELSEIF (NTY==21)THEN
         IGSTI=IPARI(34,N)
         IF(IGSTI==0)THEN
           CALL I7STSECND(
     1      IPARI(1,N),STIFINT,INTBUF_TAB(N))
         END IF
       ELSEIF (NTY==23)THEN
         IGSTI=IPARI(34,N)
         IF(IGSTI==0)THEN
           CALL I7STSECND(
     1      IPARI(1,N),STIFINT,INTBUF_TAB(N))
         END IF
       ELSEIF (NTY==24)THEN
         IGSTI=IPARI(34,N)
         IF(IGSTI>1)THEN
           CALL I24STSECND(
     1      IPARI(1,N),STIFINT,INTBUF_TAB(N),STFAC(N))
         ENDIF
       ELSEIF (NTY==25)THEN
         IGSTI=IPARI(34,N)
         IF(IGSTI>1)THEN
           CALL I25STSECND(
     1      IPARI(1,N),STIFINT,INTBUF_TAB(N),STFAC(N))
         ENDIF
       ENDIF
      ENDDO
C
      RETURN
      END
