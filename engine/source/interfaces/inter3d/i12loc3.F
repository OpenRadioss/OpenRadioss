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
Chd|  I12LOC3                       source/interfaces/inter3d/i12loc3.F
Chd|-- called by -----------
Chd|        INTTI12A                      source/interfaces/interf/intti12.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I12LOC3(XS,XM,IRECT,LMSR,
     &                   MSR,NSV,ILOC,NSEG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), LMSR(*), MSR(*), NSV(*), ILOC(*), NSEG(*)
      my_real
     .   XS(3,*),XM(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr04_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL,  JL, JLNEW, LL2, LL1, LL, LG, J, K, M, N
      my_real
     .   GMS, CMS,
     .   DMS, EMS, FMS
C-----------------------------------------------
      DO I=LFT,LLT
        IL=I+NFT
        JL=ILOC(IL)
        JLNEW=JL
        LL2=NSEG(JL+1)-1
        LL1=NSEG(JL)
        GMS=1.E30
        DO LL=LL1,LL2
          LG=LMSR(LL)
          J=IRECT(1,LG)
          K=IRECT(2,LG)
          M=IRECT(3,LG)
          N=IRECT(4,LG)
          CMS=(XS(1,IL)-XM(1,J))**2+(XS(2,IL)-XM(2,J))**2
     &                             +(XS(3,IL)-XM(3,J))**2
          DMS=(XS(1,IL)-XM(1,K))**2+(XS(2,IL)-XM(2,K))**2
     &                             +(XS(3,IL)-XM(3,K))**2
          EMS=(XS(1,IL)-XM(1,M))**2+(XS(2,IL)-XM(2,M))**2
     &                             +(XS(3,IL)-XM(3,M))**2
          FMS=(XS(1,IL)-XM(1,N))**2+(XS(2,IL)-XM(2,N))**2
     &                             +(XS(3,IL)-XM(3,N))**2
          IF(CMS<GMS) THEN
            GMS=CMS
            JLNEW=J
          ENDIF
          IF(DMS<GMS) THEN
            GMS=DMS
            JLNEW=K
          ENDIF
          IF(EMS<GMS) THEN
            GMS=EMS
            JLNEW=M
          ENDIF
          IF(FMS<GMS) THEN
            GMS=FMS
            JLNEW=N
          ENDIF
        END DO !LL=LL1,LL2	
        ILOC(IL)=JLNEW
      END DO !I=LFT,LLT

      RETURN
      END
