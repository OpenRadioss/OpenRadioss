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
Chd|  CRIT24                        source/materials/mat/mat024/crit24.F
Chd|-- called by -----------
Chd|        CONC24                        source/materials/mat/mat024/conc24.F
Chd|-- calls ---------------
Chd|        FRV                           source/materials/mat/mat024/fr.F
Chd|====================================================================
      SUBROUTINE CRIT24(NEL,PM,SIG,VK0,OFF,
     .                  ROB,NGL,SEQ,
     .                  S01,S02,S03,S04,S05,S06,
     .                  S1 ,S2 ,S3 ,S4 ,S5 ,S6 ,
     .                  SCAL1,SCAL2,SCAL3,SCLE1,SCLE2,SCLE3,
     .                  SM,DSM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      INTEGER NGL(NEL)
      my_real, DIMENSION(NEL,6)  :: SIG
      my_real, DIMENSION(NPROPM) :: PM
      my_real, DIMENSION(NEL), INTENT(IN)  :: S01,S02,S03,S04,S05,S06,
     .   SCAL1,SCAL2,SCAL3,VK0,ROB,OFF,SEQ
      my_real, DIMENSION(NEL), INTENT(OUT) :: S1,S2,S3,S4,S5,S6,
     .   SCLE1,SCLE2,SCLE3,SM,DSM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NIT,IBUG,NINDEX,NINDEX2,ICRIT_OUTP
      INTEGER INDEX(NEL)
      my_real H1, H2, H3, H4, H5, H6, RO0,ROK0, TOLF,
     .   FC,RT,RC,RCT1,RCT2,AA,AC,BC,BT,TOL
      my_real DS1(NEL),DS2(NEL),DS3(NEL),DS4(NEL),DS5(NEL),DS6(NEL),
     .   FA(NEL), XN(NEL),FN(NEL),SN1(NEL),SN2(NEL),
     .   SN3(NEL),SN4(NEL),SN5(NEL),SN6(NEL),SN7(NEL),ROK(NEL) 
C---------------------
      DATA TOLF/0.005/ 
C=======================================================================
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C ELIMINE LES DIRECTIONS ENDOMMAGEES POUR TESTER LE CRITERE
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
      DO I=1,NEL
         S1(I)  = S01(I) * SCAL1(I)
         S2(I)  = S02(I) * SCAL2(I)
         S3(I)  = S03(I) * SCAL3(I)
         S4(I)  = S04(I) * SCAL1(I)*SCAL2(I)
         S5(I)  = S05(I) * SCAL2(I)*SCAL3(I)
         S6(I)  = S06(I) * SCAL3(I)*SCAL1(I)
         SM(I)  = THIRD * (S1(I) + S2(I) + S3(I))
c
         DS1(I) = (S1(I) - SIG(I,1)) * SCAL1(I)
         DS2(I) = (S2(I) - SIG(I,2)) * SCAL2(I)
         DS3(I) = (S3(I) - SIG(I,3)) * SCAL3(I)
         DS4(I) = (S4(I) - SIG(I,4)) * SCAL1(I)*SCAL2(I)
         DS5(I) = (S5(I) - SIG(I,5)) * SCAL2(I)*SCAL3(I)
         DS6(I) = (S6(I) - SIG(I,6)) * SCAL3(I)*SCAL1(I)
         DSM(I) = THIRD * (DS1(I)+DS2(I)+DS3(I))
      ENDDO

      DO I=1,NEL
         S1(I)  = S1(I) -SM(I)
         S2(I)  = S2(I) -SM(I)
         S3(I)  = S3(I) -SM(I)
         DS1(I) = DS1(I)-DSM(I)
         DS2(I) = DS2(I)-DSM(I)
         DS3(I) = DS3(I)-DSM(I)
      ENDDO
c
      FC   = PM(33)
      RT   = PM(34)
      RC   = PM(35)
      RCT1 = PM(36)
      RCT2 = PM(37)
      AA   = PM(38)
      AC   = PM(41)
      BC   = PM(39)
      BT   = PM(40)
      ROK0 = PM(29)
      RO0  = PM(30)
      TOL  = (RT-RC)/TWENTY
      NINDEX = 0
      DO I = 1,NEL
        SCLE1(I)=ONE
        SCLE2(I)=ZERO
        SCLE3(I)=-ONE
        IF (OFF(I) >= ONE) THEN
          NINDEX = NINDEX + 1
          INDEX(NINDEX) = I      
          ROK(I)  = ROK0+ROB(I)-RO0
        ENDIF
      ENDDO
c
c----------------------------------------------
      ICRIT_OUTP = 0
      IF (NINDEX > 0) THEN
        IBUG = NINT(PM(59))
        CALL FRV(S1,S2,S3,S4,S5,S6,
     .           SM,VK0,ROB,FC,RT,RC,
     .           RCT1,RCT2,AA,AC,BC,BT,
     .           ROK,TOL,FA,NINDEX,INDEX,IBUG,
     .           NEL,SEQ,ICRIT_OUTP)
      ENDIF 
c----------------------------------------------
      NINDEX2 = 0
C
#include "vectorize.inc"
      DO N = 1, NINDEX
        I = INDEX(N)
        IF (FA(I) < ZERO) THEN
          SCLE3(I)=-ONE
        ELSEIF(ABS(FA(I)) < EM10) THEN
          SCLE3(I)=ONE
        ELSE
          SCLE3(I)=ONE
          NINDEX2 = NINDEX2 + 1
          INDEX(NINDEX2) = I
          XN(I) = ONE
        ENDIF
      ENDDO
c--------------
c     ITERATIONS
c---------------
      DO NIT = 1,10

#include "vectorize.inc"
        DO N = 1,NINDEX2
          I = INDEX(N)
          IF (I > 0) THEN
           SN1(I) = S1(I)-XN(I)*DS1(I)
           SN2(I) = S2(I)-XN(I)*DS2(I)
           SN3(I) = S3(I)-XN(I)*DS3(I)
           SN4(I) = S4(I)-XN(I)*DS4(I)
           SN5(I) = S5(I)-XN(I)*DS5(I)
           SN6(I) = S6(I)-XN(I)*DS6(I)
           SN7(I) = SM(I)-XN(I)*DSM(I)
          ENDIF         
        ENDDO
c----------------------------------------------
        CALL FRV(SN1,SN2,SN3,SN4,SN5,SN6,
     .           SN7,VK0,ROB,FC,RT,RC,
     .           RCT1,RCT2,AA,AC,BC,BT,
     .           ROK,TOL,FN,NINDEX2,INDEX,IBUG,
     .           NEL,SEQ,ICRIT_OUTP)
c----------------------------------------------
c
#include "vectorize.inc"
        DO N = 1,NINDEX2
          I = INDEX(N)
          IF (I > 0) THEN
            IF (NIT==1 .AND. FN(I) > -TOLF) THEN
              SCLE2(I) = ONE
              INDEX(N) = 0
            ELSE
             SCLE2(I)=XN(I)/(ONE-FN(I)/FA(I))
             IF (ABS(FN(I)) < TOLF) THEN
               INDEX(N) = 0
               SCLE2(I) = MIN(ONE  ,SCLE2(I))
               SCLE2(I) = MAX(ZERO,SCLE2(I))
             ELSE
               XN(I) = SCLE2(I)
             ENDIF
            ENDIF
          ENDIF
        ENDDO        
c
      ENDDO  ! NIT
c--------------
c     END ITERATIONS
c---------------
C verif convergence
#include "vectorize.inc"
      DO N = 1,NINDEX2
        I = INDEX(N)
        IF (I/=0) THEN
          SCLE2(I) = MIN(ONE,SCLE2(I))
          SCLE2(I) = MAX(ZERO,SCLE2(I))
        ENDIF
      ENDDO             

      DO I=1,NEL
        SCLE1(I) = ONE-SCLE2(I)
        S1(I) = S1(I) - SCLE2(I)*DS1(I)
        S2(I) = S2(I) - SCLE2(I)*DS2(I)
        S3(I) = S3(I) - SCLE2(I)*DS3(I)
        S4(I) = S4(I) - SCLE2(I)*DS4(I)
        S5(I) = S5(I) - SCLE2(I)*DS5(I)
        S6(I) = S6(I) - SCLE2(I)*DS6(I)
        SM(I) = SM(I) - SCLE2(I)*DSM(I)
       ENDDO
c-----------
      RETURN
      END
