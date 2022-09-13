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
Chd|  BHOL2                         source/ale/ale2d/bhol2.F      
Chd|-- called by -----------
Chd|        ALETHE                        source/ale/alethe.F           
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BHOL2(ITASK ,IPARG ,PM ,BHOLE ,IXQ   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK,IPARG(NPARG,NGROUP),BHOLE(NMULT,*),IXQ(NIXQ,NUMELQ)
      my_real PM(NPROPM,NUMMAT)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NM, NG, JMUL, I, J, MX
C-----------------------------------------------
C   S o u r c e   L i n e s
C------------------------------------------------
      DO NM=1,NMULT
       DO NG=ITASK+1,NGROUP,NTHREAD
C       ALE ON / OFF
        IF (IPARG(76, NG)  ==  1) CYCLE ! --> OFF
        JALE=IPARG(7,NG)
        JEUL=IPARG(11,NG)
        IF(JALE+JEUL == 0)   CYCLE
        IF(IPARG(8,NG) == 1) CYCLE
        JMUL=IPARG(20,NG)
        LLT=IPARG(2,NG)
        NFT=IPARG(3,NG)
        LFT=1
        IF(JMUL == 0)THEN
          DO I=LFT,LLT
           J=I+NFT
           MX=IXQ(1,J)
           BHOLE(NM,J)=MX
          ENDDO
C       ELSEIF(JMUL<NM)THEN
C         DO I=LFT,LLT
C          J=I+NFT
C          MX=IXQ(1,J)
C          BHOLE(NM,J)=0
C         ENDDO
        ELSE
          DO I=LFT,LLT
           J=I+NFT
           MX=IXQ(1,J)
           BHOLE(NM,J)=NINT(PM( 20+NM,MX))
           IF(NINT(PM(185+NM,MX)) /= 0)BHOLE(NM,J)=-BHOLE(NM,J)
          ENDDO
        ENDIF
       ENDDO! next NG
      ENDDO !next NM=1,NMULT
C-----------------------------------------------      
      RETURN
      END
