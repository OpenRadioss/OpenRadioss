Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  S10FINT3                      source/elements/solid/solide10/s10fint3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10FINT3(SIG, PX, PY, PZ,
     .                    FX,FY,FZ,VOL,QVIS,STI,STIG,
     .                    EINT,RHO,Q,EPLAS,EPSD,EPSDG,
     .                    SIGG,EINTG,RHOG,QG,EPLASG,
     .                    WIP ,G_PLA,NEL,CONDE,CONDEG,
     .                    G_EPSD)
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
      INTEGER, INTENT(IN) :: G_PLA,NEL,G_EPSD
C     REAL
      my_real
     .  PX(MVSIZ,10),PY(MVSIZ,10),PZ(MVSIZ,10),
     .  FX(MVSIZ,10), FY(MVSIZ,10), FZ(MVSIZ,10),
     .  SIG(NEL,6),VOL(*),QVIS(*),EINT(*),RHO(*),Q(*),EPLASG(*),EPLAS(*),
     .  SIGG(NEL,6),EINTG(*),RHOG(*),QG(*),WIP,STI(*),STIG(*),EPSD(*),
     .  EPSDG(*),CONDE(*),CONDEG(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "nsvis_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
C     REAL
      my_real
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ),
     .   S4(MVSIZ), S5(MVSIZ), S6(MVSIZ),
     .   FINT, QVIS_LOC, VOL_LOC
C-----------------------------------------------
      DO I=LFT,LLT
       QVIS_LOC = QVIS(I)
       VOL_LOC = VOL(I)
       SIGG(I,1) = SIGG(I,1) + WIP * SIG(I,1)
       SIGG(I,2) = SIGG(I,2) + WIP * SIG(I,2)
       SIGG(I,3) = SIGG(I,3) + WIP * SIG(I,3)
       SIGG(I,4) = SIGG(I,4) + WIP * SIG(I,4)
       SIGG(I,5) = SIGG(I,5) + WIP * SIG(I,5)
       SIGG(I,6) = SIGG(I,6) + WIP * SIG(I,6)
       RHOG(I)   = RHOG(I)   + WIP * RHO(I)
       EINTG(I)  = EINTG(I)  + WIP * EINT(I)
       QG(I)     = QG(I)     + WIP * Q(I)
       STIG(I)=STIG(I)+STI(I)
       S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS_LOC)*VOL_LOC
       S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS_LOC)*VOL_LOC
       S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS_LOC)*VOL_LOC
       S4(I)=(SIG(I,4)+SVIS(I,4))*VOL_LOC
       S5(I)=(SIG(I,5)+SVIS(I,5))*VOL_LOC
       S6(I)=(SIG(I,6)+SVIS(I,6))*VOL_LOC
      ENDDO
C
      IF(NODADT_THERM == 1) THEN
        DO I=LFT,LLT
          CONDEG(I)= CONDEG(I)+ CONDE(I)
        ENDDO
      ENDIF
c
      IF ((ISRAT > 0).OR.(G_EPSD > 0)) THEN                                                    
        DO I=LFT,LLT
         EPSDG(I) = EPSDG(I) + WIP * EPSD(I)
        ENDDO
      ENDIF
      IF (G_PLA > 0) THEN                                                    
        DO I=LFT,LLT                                                          
          EPLASG(I)  = EPLASG(I)  + WIP * EPLAS(I)                              
        ENDDO                                                                 
      ENDIF                                                                  
C
      DO N=1,10
        DO I=LFT,LLT
          FX(I,N)=FX(I,N)-(S1(I)*PX(I,N)+S4(I)*PY(I,N)+S6(I)*PZ(I,N))
          FY(I,N)=FY(I,N)-(S2(I)*PY(I,N)+S5(I)*PZ(I,N)+S4(I)*PX(I,N))
          FZ(I,N)=FZ(I,N)-(S3(I)*PZ(I,N)+S6(I)*PX(I,N)+S5(I)*PY(I,N))
        ENDDO
      ENDDO
C-----------
      RETURN
      END
