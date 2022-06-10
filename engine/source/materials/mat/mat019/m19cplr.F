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
Chd|  M19CPLR                       source/materials/mat/mat019/m19cplr.F
Chd|-- called by -----------
Chd|        SIGEPS19C                     source/materials/mat/mat019/sigeps19c.F
Chd|-- calls ---------------
Chd|        ROTOV                         source/airbag/roto.F          
Chd|        ROTO_SIG                      source/airbag/roto.F          
Chd|        UROTO_SIG                     source/airbag/uroto.F         
Chd|====================================================================
      SUBROUTINE M19CPLR(
     2           JFT     ,JLT     ,DIR     ,SHF     ,NEL     ,
     3           E11     ,E22     ,AN12    ,AN21    ,G12     ,
     4           G23     ,G31     ,DE      ,EPST    ,PM      ,
     5           IMAT    ,SIGI    ,LSENSOR ,SENSOR  ,IPM     ,
     6           SIGOXX  ,SIGOYY  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     7           SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     8           ET      )
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NEL
      INTEGER IMAT,IPM(NPROPMI,*),LSENSOR(NSENSI,*)
C     REAL
      my_real
     .   DIR(*),SHF(*),ET(*)
C     REAL
      my_real
     .   E11(MVSIZ), E22(MVSIZ), AN12(MVSIZ), AN21(MVSIZ), 
     .   G12(MVSIZ), G23(MVSIZ), G31(MVSIZ),DE(MVSIZ),  
     .   EPST(MVSIZ,5),PM(NPROPM,*),SIGI(NEL,3),SENSOR(NSENSR,*),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),
     .   SIGOZX(NEL),SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,I1,ISENS
      my_real
     .   SIG(NEL,5),SIGO(NEL,3)
      my_real
     .   SCALE , S, D, T, P1, P2,R,BETA,
     .   A11, A12, A22, ZEROSTRESS,TSTART,DSIG
C=======================================================================
C     ROTATIONS DANS LE REPERE D ORTHOTROPIE  --- EPST, SIG ---
C-------------------------------------------------
      CALL ROTOV(JFT,JLT,EPST,DIR,NEL)
!
      DO I=JFT,JLT
        SIG(I,1)=SIGOXX(I)
        SIG(I,2)=SIGOYY(I)
        SIG(I,3)=SIGOXY(I)
        SIG(I,4)=SIGOYZ(I)
        SIG(I,5)=SIGOZX(I)
      ENDDO
!
      CALL ROTO_SIG(JFT,JLT,SIG,DIR,NEL)
C-------------------------------------------------
      DO  I=JFT,JLT
         SIGO(I,1) = SIG(I,1)
         SIGO(I,2) = SIG(I,2)
         SIGO(I,3) = SIG(I,3)
         A12=(ONE-AN12(I)*AN21(I))
         A11=E11(I)/A12
         A22=E22(I)/A12
         A12=AN21(I)*A11
C
         SIG(I,1)=A11*EPST(I,1)+A12*EPST(I,2)
         SIG(I,2)=A12*EPST(I,1)+A22*EPST(I,2)
         SIG(I,3)=G12(I)*EPST(I,3)*TWO
         SIG(I,4)=G23(I)*SHF(I)*EPST(I,4)*TWO
         SIG(I,5)=G31(I)*SHF(I)*EPST(I,5)*TWO
      ENDDO
C-----------------------------
C     CONTRAINTES reduites en compression
C-----------------------------
      DO  I=JFT,JLT
       S  = HALF*(SIG(I,1)+SIG(I,2))
       D  = HALF*(SIG(I,1)-SIG(I,2))
       R = SQRT(SIG(I,3)*SIG(I,3) + D*D)
       P1 = S - R 
       ET(I) = ONE       
       IF(P1.LT.ZERO)THEN
         P2 = S + R 
         IF(P2.GT.ZERO)THEN
           BETA = HALF*((ONE-DE(I))*S/R + ONE+DE(I)) 
           SIG(I,1)=BETA*(SIG(I,1)-P2) + P2 
           SIG(I,2)=BETA*(SIG(I,2)-P2) + P2 
           SIG(I,3)=BETA*SIG(I,3)
         ELSE
           BETA = DE(I)
           SIG(I,1)=BETA*SIG(I,1)
           SIG(I,2)=BETA*SIG(I,2) 
           SIG(I,3)=BETA*SIG(I,3)
         ENDIF
         ET(I) = BETA       
       ENDIF
      ENDDO
C-----------------------------------------------------------
C     REF-STATE ZEROSTRESS OPTION
C-----------------------------------------------------------
       ZEROSTRESS = PM(55,IMAT)
       ISENS  = IPM(5,IMAT)
       IF (ZEROSTRESS /= ZERO)THEN
       DO I=JFT,JLT
           TSTART = ZERO
           DO II=1,NSENSOR
             IF (ISENS == LSENSOR(1,II)) THEN
               TSTART  = SENSOR(2,II)
               EXIT
             ENDIF
           ENDDO
C           
           IF (TT <= TSTART) THEN
             SIGI(I,1) = SIG(I,1)
             SIGI(I,2) = SIG(I,2)
             SIGI(I,3) = SIG(I,3)
             SIG(I,1) = ZERO
             SIG(I,2) = ZERO
             SIG(I,3) = ZERO
           ELSE
             DSIG = SIG(I,1) - SIGO(I,1) - SIGI(I,1)
             IF((SIGI(I,1) > ZERO).AND.(DSIG < ZERO))THEN
               SIGI(I,1) = MAX(ZERO,SIGI(I,1)+ZEROSTRESS*DSIG)
             ELSEIF((SIGI(I,1) < ZERO).AND.(DSIG > ZERO))THEN
               SIGI(I,1) = MIN(ZERO,SIGI(I,1)+ZEROSTRESS*DSIG)
             ENDIF
             DSIG = SIG(I,2) - SIGO(I,2) - SIGI(I,2)
             IF((SIGI(I,2) > ZERO).AND.(DSIG < ZERO))THEN
               SIGI(I,2) = MAX(ZERO,SIGI(I,2)+ZEROSTRESS*DSIG)
             ELSEIF((SIGI(I,2) < ZERO).AND.(DSIG > ZERO))THEN
               SIGI(I,2) = MIN(ZERO,SIGI(I,2)+ZEROSTRESS*DSIG)
             ENDIF
             DSIG = SIG(I,3) - SIGO(I,3) - SIGI(I,3)
             IF((SIGI(I,3) > ZERO).AND.(DSIG < ZERO))THEN
               SIGI(I,3) = MAX(ZERO,SIGI(I,3)+ZEROSTRESS*DSIG)
             ELSEIF((SIGI(I,3) < ZERO).AND.(DSIG > ZERO))THEN
               SIGI(I,3) = MIN(ZERO,SIGI(I,3)+ZEROSTRESS*DSIG)
             ENDIF
             SIG(I,1) = SIG(I,1) - SIGI(I,1)
             SIG(I,2) = SIG(I,2) - SIGI(I,2)
             SIG(I,3) = SIG(I,3) - SIGI(I,3)
           ENDIF
         ENDDO
      ENDIF
C--------------------------------
C     RETOUR DANS LE REPERE COQUE
C--------------------------------
      CALL UROTO_SIG(JFT,JLT,SIG,DIR,NEL)
!
      DO I=JFT,JLT
        SIGNXX(I) = SIG(I,1)
        SIGNYY(I) = SIG(I,2)
        SIGNXY(I) = SIG(I,3)
        SIGNYZ(I) = SIG(I,4)
        SIGNZX(I) = SIG(I,5)
      ENDDO
!
c-----------
      RETURN
      END
