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
Chd|  MATING                        source/materials/mat_share/mating.F
Chd|-- called by -----------
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MATING(PM   ,VOL ,OFF    ,EINT  ,RHO ,
     .                  SIG  ,IX  ,NIX    ,SIGI  ,EPSP,
     .                  NSIG ,MAT ,NUMS   ,PT    ,NEL ,
     .                  FILL ,TEMP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "scry_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER   NIX, NSIG, NUMS, NEL
      INTEGER   IX(NIX,*),PT(*),MAT(*)
      my_real :: PM(NPROPM,*), SIG(NEL,6), SIGI(NSIG,*)
      my_real, DIMENSION(NEL) :: VOL,OFF,EINT,RHO,EPSP,FILL,TEMP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,JJ,N,MA,IFLAGINI
C=======================================================================
      DO I=LFT,LLT
       IFLAGINI = 0
       MA=MAT(I)
       OFF(I) =ONE
       IF(MA == 0)CYCLE
       EINT(I)=PM(23,MA)
       RHO(I) =PM(89,MA)
       TEMP(I)=PM(79,MA)
C-----------------------------
       IF (ISIGI == 0) THEN
C-----------------------------
        SIG(I,1)=-PM(104,MA)
        SIG(I,2)=-PM(104,MA)
        SIG(I,3)=-PM(104,MA)
C 
        IF (JLAG/=0 .AND. JSPH == 0) THEN
          VOL(I) = VOL(I) * ( RHO(I) / PM(1,MA) )
        ENDIF
        IF (JEUL+JALE /= 0 .AND.  PM(1,MA)/=ZERO) THEN
          EINT(I) = EINT(I) * RHO(I) / PM(1,MA)
        ENDIF
C 
        FILL(I)=ONE
C-----------------------------
       ELSE ! CONTRAINTES INITIALES
C-----------------------------
        IF (ABS(ISIGI)/=3.AND.ABS(ISIGI)/=4.AND.ABS(ISIGI)/=5) THEN
          II = I+NFT
          N = NINT(SIGI(7,II))
          IF(N == IX(NIX,II))THEN
            JJ = II
            IFLAGINI = 1
          ELSE
            IF(JSPH == 0)THEN
              DO J = 1,MAX(NUMSOL+NUMQUAD,NUMELS+NUMELQ)
                JJ= J
                N = NINT(SIGI(7,J))
                IF(N==0)GOTO 200
                IF(N==IX(NIX,II))THEN
                  IFLAGINI = 1
                  GOTO 60
                ENDIF
              ENDDO
            ELSE
              DO J = 1,NUMSPH
                JJ= J
                N = NINT(SIGI(7,J))
                IF(N==0)GOTO 200
                IF(N==IX(NIX,II))THEN
                  IFLAGINI = 1
                  GOTO 60
                ENDIF
              ENDDO
            ENDIF
            GOTO 200
  60        CONTINUE
          ENDIF
        ELSE
          II=NFT+I
          N =IX(NIX,II)
          JJ=PT(II)
          IF (JJ == 0)GOTO 200
          IFLAGINI = 1
        END IF
C-----------
        IF (IFLAGINI == 1)THEN
          SIG(I,1)=SIGI(1,JJ)
          SIG(I,2)=SIGI(2,JJ)
          SIG(I,3)=SIGI(3,JJ)
          SIG(I,4)=SIGI(4,JJ)
          SIG(I,5)=SIGI(5,JJ)
          SIG(I,6)=SIGI(6,JJ)
          IF (ISIGI == 3.OR.ISIGI == 4.OR.ISIGI == 5) THEN
            IF(SIGI(8,JJ)/=ZERO) THEN
              IF(JLAG/=0.AND.JSPH == 0)THEN
                VOL(I) = SIGI(8,JJ)*VOL(I) / PM(1,MA)
                RHO(I) = SIGI(8,JJ)
              ELSE
                RHO(I) = SIGI(8,JJ)
              ENDIF
            ELSEIF (JLAG/=0.AND.JSPH == 0) THEN
              VOL(I) = VOL(I) * RHO(I) / PM(1,MA)
            ENDIF
C           EPSP NON UTILISE DANS MAT TYPE 1 ET ECRASE PAR EINT
            IF (SIGI(10,JJ)/=ZERO) EPSP(I) = SIGI(10,JJ)
            IF (SIGI( 9,JJ)/=ZERO) EINT(I) = SIGI(9,JJ)
C           TAUX DE REMPLISSAGE
            IF(SIGI(11,JJ)/=ZERO) FILL(I)=SIGI(11,JJ)
          ENDIF
        ENDIF
  200   CONTINUE
       ENDIF
      ENDDO
C-----------
      RETURN
      END
