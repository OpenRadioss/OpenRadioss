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
Chd|  M15CRAK                       source/materials/mat/mat015/m15crak.F
Chd|-- called by -----------
Chd|        SIGEPS15C                     source/materials/mat/mat015/sigeps15c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M15CRAK(JFT  ,JLT   ,PM    ,DAMT ,DIR ,
     1                   SIGR ,IMAT ,ILAYER ,SIG  ,NGL ,
     2                   NEL  )
Cx-----------------------------------------------
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
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NGL(MVSIZ),IMAT
      INTEGER JFT, JLT,ILAYER,ICHA,IFLAG,NEL
C     REAL
      my_real
     .   PM(NPROPM,*),DAMT(NEL,2),DIR(NEL,2),
     .   SIGR(NEL,6),SIG(MVSIZ,5)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IDIR
      INTEGER, DIMENSION(MVSIZ) :: FAILURE
       my_real
     .   EF2,EFC2,EM22,EMC2
      my_real
     .   SIGT1(MVSIZ),SIGT2(MVSIZ),SIGC1(MVSIZ),SIGC2(MVSIZ),
     .   SIGR12(MVSIZ),BETA(MVSIZ),SCH1(MVSIZ,5),TAUX,S1,S2,S3,
     .   S4,S5, TMAX(MVSIZ) 
C-----------------------------------------------
        DO  I=JFT,JLT
          IFLAG = NINT(PM(147,IMAT))
          IF (IFLAG == 1) THEN           
            SIGT1(I)  = PM(141,IMAT)
            SIGT2(I)  = PM(142,IMAT)
            SIGC1(I)  = PM(143,IMAT)
            SIGC2(I)  = PM(144,IMAT)
            SIGR12(I) = PM(145,IMAT)
          ELSE
            SIGT1(I)  = PM(148,IMAT)
            SIGT2(I)  = PM(149,IMAT)
            SIGC1(I)  = PM(150,IMAT)
            SIGC2(I)  = PM(151,IMAT)
            SIGR12(I) = PM(152,IMAT)
          ENDIF
C
          BETA(I) = PM(60,IMAT)
          TMAX(I) = PM(61,IMAT)
C
          SCH1(I,1) = ZERO
          SCH1(I,2) = ZERO
          SCH1(I,3) = ZERO
          SCH1(I,4) = ZERO
          SCH1(I,5) = ZERO          
        ENDDO
C
C.....STRESS IN ORTHOTROPIC DIRECTIONS
C.....MODELE DE CHANG_CHANG>
C
      FAILURE(1:MVSIZ) = 0
      DO  I=JFT,JLT
        EF2  =ZERO
        EFC2 =ZERO
        EM22 =ZERO
        EMC2 =ZERO     
!---
        IF (DAMT(I,1) < ONE) THEN       
!---
          DAMT(I,1)=EXP(-(TT-SIGR(I,6))/TMAX(I))   
          IF (DAMT(I,1) < EM02) DAMT(I,1)=ZERO  
!---
        ELSEIF (DAMT(I,2) < ONE) THEN
!---
          DAMT(I,2)=EXP(-(TT-SIGR(I,6))/TMAX(I))        
          IF (DAMT(I,2) < EM02) DAMT(I,2)=ZERO          
          SIG(I,2) = SIGR(I,2)*DAMT(I,2)
          SIG(I,3) = SIGR(I,3)*DAMT(I,2)
          SIG(I,4) = SIGR(I,4)*DAMT(I,2)
          SIG(I,5) = SIGR(I,5)*DAMT(I,2)
          IF (SIG(I,1)> ZERO) THEN             
            EF2=(SIG(I,1)/SIGT1(I))**2 + BETA(I)*(SIG(I,3)/SIGR12(I))**2
            DAMT(I,1)=ONE
          ELSE 
            EFC2 = (SIG(I,1)/SIGC1(I))**2
            DAMT(I,1)=ONE
          ENDIF 
          IF (EF2 >= ONE .OR. EFC2 >= ONE) THEN
            SIGR(I,6) = TT
            DAMT(I,1) = ZEP9        
            SCH1(I,1) = SIG(I,1)
            SCH1(I,2) = SIGR(I,2)*DAMT(I,2)
            SCH1(I,3) = SIGR(I,3)*DAMT(I,2)
            SCH1(I,4) = SIGR(I,4)*DAMT(I,2)
            SCH1(I,5) = SIGR(I,5)*DAMT(I,2)
          ENDIF 
!---
        ELSE           
!---
!
! fiber breakage failure criteria
!
          IF (SIG(I,1) > ZERO) THEN
!   tensile fiber mode
            EF2=(SIG(I,1)/SIGT1(I))**2 + BETA(I)*(SIG(I,3)/SIGR12(I))**2
            DAMT(I,1)=ONE ! obsolete (already initialized to 1)
          ELSE
!   compressive fiber mode
            EFC2 = (SIG(I,1)/SIGC1(I))**2
            DAMT(I,1)=ONE ! obsolete (already initialized to 1)
          ENDIF 
          IF (EF2 >= ONE .OR. EFC2 >= ONE) THEN
            DAMT(I,1) = ZEP9
            SIGR(I,6) = TT
!            IDIR = 1 
            SCH1(I,1) = SIG(I,1)
            SCH1(I,2) = SIG(I,2)
            SCH1(I,3) = SIG(I,3)
            SCH1(I,4) = SIG(I,4)
            SCH1(I,5) = SIG(I,5)
            FAILURE(I) = 1  ! fiber breakage failure
!            WRITE(IOUT, '(A,I1,A,I10,5X,A,I3,A,1PE11.4)')
!     +     ' FAILURE-',1,' ELEMENT #',NGL(I),
!     +     ' LAYER #',ILAYER,' TIME=',TT
!!          ENDIF ! IF (EF2 >= ONE .OR. EFC2 >= ONE)
!
! matrix cracking failure criteria
!
          ELSE
            IF (SIG(I,2) >= ZERO) THEN
!   tensile matrix mode
              EM22=(SIG(I,2)/SIGC2(I))**2 + (SIG(I,3)/SIGR12(I))**2 
              DAMT(I,2)=ONE ! obsolete (already initialized to 1)
            ELSE
! compressive matrix mode
              EMC2=(SIG(I,2)/(TWO*SIGR12(I)))**2 
     +            +(SIG(I,3)/SIGR12(I))**2 
     +            +SIG(I,2)*((SIGC2(I)/(TWO*SIGR12(I)))**2-ONE)/SIGC2(I)
              DAMT(I,2)=ONE ! obsolete (already initialized to 1)
            ENDIF
            IF (EM22 >= ONE .OR. EMC2 >= ONE) THEN
              DAMT(I,2) = ZEP9
              SIGR(I,6) = TT
              SIGR(I,1) = SIG(I,1)
              SIGR(I,2) = SIG(I,2)
              SIGR(I,3) = SIG(I,3)
              SIGR(I,4) = SIG(I,4)
              SIGR(I,5) = SIG(I,5)
              FAILURE(I) = 2  ! matrix cracking failure
!              WRITE(IOUT, '(A,I1,A,I10,5X,A,I3,A,1PE11.4)')
!     +        ' FAILURE-',2,' ELEMENT #',NGL(I),
!     +        ' LAYER #',ILAYER,' TIME=',TT      
            ENDIF
          ENDIF
        ENDIF   
      ENDDO
!
      DO  I=JFT,JLT
            SIGR(I,1) = SCH1(I,1)
            SIGR(I,2) = SCH1(I,2)              
            SIGR(I,3) = SCH1(I,3)
            SIGR(I,4) = SCH1(I,4)
            SIGR(I,5) = SCH1(I,5)
      ENDDO
!
      DO  I=JFT,JLT
        IF(FAILURE(I) == 1 ) THEN
                  WRITE(IOUT, '(A,I1,A,I10,5X,A,I3,A,1PE11.4)')
     +     ' FAILURE-',1,' ELEMENT #',NGL(I),
     +     ' LAYER #',ILAYER,' TIME=',TT
        ELSEIF(FAILURE(I) == 2 ) THEN
              WRITE(IOUT, '(A,I1,A,I10,5X,A,I3,A,1PE11.4)')
     +        ' FAILURE-',2,' ELEMENT #',NGL(I),
     +        ' LAYER #',ILAYER,' TIME=',TT      
        ENDIF
      ENDDO
C---
      RETURN
      END
