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
Chd|  SPHRES44B                     source/elements/sph/sphres44b.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPHRES44B(
     1    KXSP    ,IXSP    ,NOD2SP  ,IPARG   ,SPBUF   )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .    KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*),
     .    IPARG(NPARG,*)
C     REAL
      my_real
     .   SPBUF(NSPBUF,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,IG,NG,I,OFFSET,NEL
C-----------------------------------------------
C     Construit les Numeros d'appartenance aux groupes. 
C-----------------------------------------------
      DO NG=1,NGROUP
      NEL=IPARG(2,NG)
      NFT=IPARG(3,NG)
      ITY=IPARG(5,NG)
      DO IG= 1,NEL,MVSIZ
        OFFSET = IG - 1
        LFT=1
        LLT=MIN(MVSIZ,NEL-OFFSET)  
        IF(ITY==51)THEN
         DO I=LFT,LLT
          N =I+NFT
          IF(SPBUF(9,N)>ZERO)THEN
           KXSP(1,N) =0
           KXSP(2,N) =NG
           SPBUF(9,N)=ZERO	   
          ENDIF
         ENDDO
        ENDIF
      ENDDO
      ENDDO
C-----------------------------------------------
      RETURN
      END
