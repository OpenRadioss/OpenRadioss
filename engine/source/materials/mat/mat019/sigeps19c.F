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
Chd|  SIGEPS19C                     source/materials/mat/mat019/sigeps19c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        M19CPLR                       source/materials/mat/mat019/m19cplr.F
Chd|====================================================================
      SUBROUTINE SIGEPS19C(
     1            JFT    ,JLT    ,PM     ,THK    ,FLAG_ZCFAC,
     2            GSTR   ,DIR    ,SHF    ,EXY    ,JHBE      ,
     3            IPM    ,LSENSOR,SENSOR ,ZCFAC  ,NEL       ,
     4            SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX    ,
     5            SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX    ,
     6            IMAT   ,NPTTOT ,ZZ     ,SIGI   )
C-----------------------------------------------
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
#include      "units_c.inc"
#include      "scr06_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com20_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,JHBE,ISMSTR,NEL,IMAT,NPTTOT
      INTEGER IPM(NPROPMI,*),LSENSOR(NSENSI,*)
      LOGICAL :: FLAG_ZCFAC
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*),THK(*),DIR(NEL,2), GSTR(NEL,8),SHF(*)
      my_real
     .   EXY(MVSIZ),SENSOR(NSENSR,*),ZCFAC(MVSIZ,2),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),
     .   SIGOZX(NEL),SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL),ZZ(MVSIZ),SIGI(NEL,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II(5),I1,ISENS(MVSIZ),ILAYER
      my_real
     .   FAC(MVSIZ),Z(MVSIZ,5),EPS(MVSIZ,5),DEGMB(MVSIZ),DEGFX(MVSIZ), 
     .   CA(MVSIZ),CB(MVSIZ),CN(MVSIZ),CC(MVSIZ),EPDR(MVSIZ),
     .   VISC(MVSIZ),VISG(MVSIZ),E11(MVSIZ),E22(MVSIZ), 
     .   ANU12(MVSIZ),ANU21(MVSIZ),G12(MVSIZ),G23(MVSIZ),G31(MVSIZ),
     .   DE(MVSIZ),EPST(MVSIZ,5),ET(MVSIZ),ZT
      my_real
     .   R1,FF,AA,VV,E11_1,E22_1,ANU12_1,ANU21_1,
     .   G12_1,G23_1,G31_1,DE_1,CA_1,CB_1,CN_1,CC_1
C=======================================================================
      E11_1   = PM(33,IMAT)
      E22_1   = PM(34,IMAT)
      ANU12_1 = PM(35,IMAT)
      ANU21_1 = PM(36,IMAT)
      G12_1   = PM(37,IMAT)
      G23_1   = PM(38,IMAT)
      G31_1   = PM(39,IMAT)
      DE_1    = PM(44,IMAT)
      CA_1    = PM(45,IMAT)
      CB_1    = PM(46,IMAT)
      CN_1    = PM(47,IMAT)
      CC_1    = PM(50,IMAT)
!
      DO I=JFT,JLT
        E11(I)   = E11_1
        E22(I)   = E22_1
        ANU12(I) = ANU12_1
        ANU21(I) = ANU21_1
        G12(I)   = G12_1
        G23(I)   = G23_1
        G31(I)   = G31_1
        DE(I)	 = DE_1
        CA(I)	 = CA_1
        CB(I)	 = CB_1
        CN(I)	 = CN_1
        CC(I)	 = CC_1
      ENDDO
C-----------------------------------------------------------
C     ROTATION DU 1/2 CISAILLEMENT
C-----------------------------------------------------------
      IF (ISHFRAM==1 .AND. JHBE>=2) THEN
        DO I=JFT,JLT
          R1 = DIR(I,2)*EXY(I)*HALF
          DIR(I,2)=DIR(I,2)-DIR(I,1)*EXY(I)*HALF
          DIR(I,1)=DIR(I,1)+R1
          R1 = SQRT(DIR(I,1)*DIR(I,1)+DIR(I,2)*DIR(I,2))
          DIR(I,1)=DIR(I,1)/R1
          DIR(I,2)=DIR(I,2)/R1
        ENDDO
      ENDIF
C-----------------------------------------------------------
C
C      DEFORMATIONS TOTALES FA 09/92
C
       DO  I=JFT,JLT
         ZT = ZZ(I)*THK(I)
         EPST(I,1)= GSTR(I,1)+ZT*GSTR(I,6)
         EPST(I,2)= GSTR(I,2)+ZT*GSTR(I,7)
         EPST(I,3)=(GSTR(I,3)+ZT*GSTR(I,8))*HALF
         EPST(I,4)= GSTR(I,4)*HALF
         EPST(I,5)= GSTR(I,5)*HALF
       ENDDO
C-----------------------------------------------------------
c      CONTRAINTES
       CALL M19CPLR(
     2              JFT    ,JLT    ,DIR     ,SHF    ,NEL    ,
     3              E11    ,E22    ,ANU12   ,ANU21  ,G12    ,
     4              G23    ,G31    ,DE      ,EPST   ,PM     ,
     5              IMAT   ,SIGI   ,LSENSOR ,SENSOR ,IPM    ,
     6              SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ ,SIGOZX ,
     7              SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ ,SIGNZX ,
     8              ET     )
C------for QEPH-------       
       IF (FLAG_ZCFAC) THEN
         DO I=JFT,JLT
           ZCFAC(I,1) = ZCFAC(I,1) + ET(I) / NPTTOT
           ZCFAC(I,2) = MIN(ET(I),ZCFAC(I,2))
         ENDDO
       ENDIF
c--------------------------------------------------------
      RETURN
      END
