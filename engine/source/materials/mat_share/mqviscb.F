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
Chd|  MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|-- called by -----------
Chd|        M1LAW                         source/materials/mat/mat001/m1law.F
Chd|        M1LAWI                        source/materials/mat/mat001/m1law.F
Chd|        M1LAWTOT                      source/materials/mat/mat001/m1law.F
Chd|        M22LAW                        source/materials/mat/mat022/m22law.F
Chd|        M24LAW                        source/materials/mat/mat024/m24law.F
Chd|        M2LAW                         source/materials/mat/mat002/m2law.F
Chd|        M46LAW                        source/materials/mat/mat046/m46law.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        SBOLTLAW                      source/elements/solid/solide/sboltlaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE MQVISCB(
     1   PM,      OFF,     RHO,     RK,
     2   T,       SSP,     RE,      STI,
     3   DT2T,    NELTST,  ITYPTST, AIRE,
     4   OFFG,    GEO,     PID,     VOL,
     5   VD2,     DELTAX,  VIS,     D1,
     6   D2,      D3,      PNEW,    PSH,
     7   MAT,     NGL,     QVIS,    SSP_EQ,
     8   VOL0,    MSSA,    DMELS,   IGEO,
     9   FACQ0,   CONDE,   DTEL,    G_DT,
     A   IPM,     RHOREF,  RHOSP,   NEL,
     B   ITY,     ISMSTR,  JTUR,    JTHE,
     C   JSMS)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22TRI_MOD
      USE I22BUFBRIC_MOD
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "cong1_c.inc"
#include      "impl1_c.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "inter22.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: ITY
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: JSMS
      INTEGER NELTST,ITYPTST,PID(*),MAT(*), NGL(*),IGEO(NPROPGI,NUMGEO),G_DT,IPM(NPROPMI,NUMMAT)
      my_real DT2T
      my_real PM(NPROPM,NUMMAT), OFF(*), RHO(*), RK(*), T(*), RE(*),STI(*),
     .        OFFG(*),GEO(NPROPG,NUMGEO),
     .        VOL(*), VD2(*), DELTAX(*), SSP(*), AIRE(*), VIS(*), 
     .        PSH(*), PNEW(*),QVIS(*) ,SSP_EQ(*),CONDE(*), 
     .        D1(*), D2(*), D3(*), VOL0(*), MSSA(*), DMELS(*),FACQ0
      my_real, INTENT(IN) :: RHOREF(MVSIZ), RHOSP(MVSIZ)
      my_real, INTENT(INOUT) :: DTEL(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, MT, K,ICOUNT,LIST(MVSIZ),ERROR, ALE_OR_EULER, ID_MIN
      my_real DD(MVSIZ), AL(MVSIZ),
     .        DTX(MVSIZ), AD(MVSIZ), QX(MVSIZ), CX(MVSIZ), RHO0(MVSIZ), NRHO(MVSIZ),
     .        DTY(MVSIZ), QA, QB, VISI, FACQ,QAA,
     .        CNS1, CNS2, SPH, AK1, BK1, AK2, BK2, TLI, AKK, XMU, TMU, RPR,
     .        ATU, QAD, QBD, QAAP, DT, NUI
      my_real TIDT,TVOL,TRHO,TAIRE,  DTMIN(MVSIZ), RHOS(MVSIZ)
#ifdef MYREAL8 
      my_real, PARAMETER :: REAL_THREE = 3.0D0
      my_real, PARAMETER :: REAL_ONE = 1.0D0
#else
      my_real, PARAMETER :: REAL_THREE = 3.0
      my_real, PARAMETER :: REAL_ONE = 1.0
#endif
     
C=======================================================================

       ! by default don't apply /DT/NODA/* to ALE/EULER cells ; unless if /DT/NODA/ALEON is enabled (hidden card introduced for backward compatibility)
       MT = MAT(1)
       ALE_OR_EULER = 0 
       IF(NINT(PM(72,MT))==1 .OR. NINT(PM(72,MT))==2)ALE_OR_EULER = 1
       IF( ALE%GLOBAL%I_DT_NODA_ALE_ON==1) ALE_OR_EULER = 0   

      IF(IMPL==ZERO)THEN
          DO I=1,NEL
            DD(I)=-D1(I)-D2(I)-D3(I)
            AD(I)=ZERO
            AL(I)=ZERO
            CX(I)=SSP(I) + SQRT(VD2(I))
          ENDDO
          IF((IMPL_S>0.AND.IDYNA==0).OR.ISMDISP>0)THEN
              VISI=ZERO
              FACQ=ZERO
          ELSE
             VISI=ONE
             FACQ=FACQ0
          ENDIF
          IF(IMCONV<0) THEN
             DO I=1,NEL
                VOL(I)= ABS(VOL(I))
             ENDDO
          ENDIF
      ELSE
          DO I=1,NEL
            DD(I)=-D1(I)-D2(I)-D3(I)
            AD(I)=ZERO
            AL(I)=ZERO
            CX(I)=SQRT(VD2(I))
          ENDDO
          VISI=ZERO
          FACQ=ZERO
      ENDIF
C
      IF(N2D>0) THEN
        DO I=1,NEL
         IF(OFF(I)==ONE)THEN
           AL(I)=SQRT(AIRE(I))
           AD(I)= MAX(ZERO,DD(I))
         ENDIF
        ENDDO
      ELSE
        DO I=1,NEL
         IF(OFF(I)==ONE)THEN
           IF(VOL(I) > ZERO)THEN
              AL(I) = VOL(i)**(REAL_ONE/REAL_THREE)
           ELSE
             AL(I)=ZERO
           END IF
           AD(I)= MAX(ZERO,DD(I))
         ENDIF
        ENDDO
      ENDIF
C
       !Numerical viscosity is managed inside sigeps51.f for law51
       !qa, qb should be 0 here otherwise viscosity will be twice.	
       IF(.NOT.(IDTMINS==2.AND.JSMS/=0))THEN
        MT = MAT(1)
        DO I=1,NEL
          RHO0(I)=PM(1,MT)
          NRHO(I)=SQRT(RHOREF(I)*RHO0(I))
        ENDDO
        DO I=1,NEL
          QA =FACQ*GEO(14,PID(1))
          QB =FACQ*GEO(15,PID(1))
          CNS1=GEO(16,PID(1))*AL(I)*NRHO(I)*SSP(I)*OFF(I)
          CNS2=GEO(17,PID(1))*AL(I)*NRHO(I)*SSP(I)*OFF(I)
          PSH(I)=PM(88,MT)
          PNEW(I)=ZERO
C
C   warns : Stability vs artificial viscosity should use
C           RHO(I)*AL(I)*(QAA*AL(I)+QB*SSP(I)) / MAX(EM20,RHOREF(I)*DELTAX(I))
C           Same for VIS(I) since SOUNDSPEED is generally computed wrt RHO0, not RHO ../..
          QAA = QA*QA*AD(I)
          QX(I)=QB*SSP(I)+DELTAX(I) * QAA
     .     + VISI*TWO*VIS(I)   / MAX(EM20,RHO(I)*DELTAX(I))
     .     + (CNS1+VISI*CNS2) / MAX(EM20,RHOREF(I)*DELTAX(I))
          QVIS(I)=RHO(I)*AD(I)*AL(I)*(QAA*AL(I)+QB*SSP(I))
          DTMIN(I) = GEO(172,PID(1))      
        ENDDO
       ELSE
        MT = MAT(1)
        DO I=1,NEL
          RHO0(I)=PM(1,MT)
          NRHO(I)=SQRT(RHOREF(I)*RHO0(I))
        ENDDO
        IF(GEO(16,PID(1)) >= ZERO)THEN
          DO I=1,NEL
            QA =FACQ*GEO(14,PID(1))
            QB =FACQ*GEO(15,PID(1))
            CNS1=GEO(16,PID(1))*AL(I)*NRHO(I)*SSP(I)*OFF(I)
            CNS2=GEO(17,PID(1))*AL(I)*NRHO(I)*SSP(I)*OFF(I)
            PSH(I)=PM(88,MT)
            PNEW(I)=ZERO
            QAA = QA*QA*AD(I)
            NUI    =RHO(I)*AL(I)*(QAA*AL(I)+QB*SSP(I))
            QVIS(I)=NUI*AD(I)
            QX(I)  =(NUI + CNS1+VISI*(TWO*VIS(I)+CNS2))/MAX(EM20,RHOREF(I)*DELTAX(I))
            DTMIN(I) = GEO(172,PID(1))      
          ENDDO
       ELSE
          DO I=1,NEL
            QA =FACQ*GEO(14,PID(1))
            QB =FACQ*GEO(15,PID(1))
            CNS1=ABS(GEO(16,PID(1)))*NRHO(I)*SSP(I)**2*OFF(I)
            CNS2=ABS(GEO(17,PID(1)))*NRHO(I)*SSP(I)**2*OFF(I)
            PSH(I)=PM(88,MT)
            PNEW(I)=ZERO
            QAA = QA*QA*AD(I)
            NUI    =RHO(I)*AL(I)*(QAA*AL(I)+QB*SSP(I))
            QVIS(I)=NUI*AD(I)
            QX(I)  =(NUI + CNS1+VISI*(TWO*VIS(I)+CNS2))/MAX(EM20,RHOREF(I)*DELTAX(I))
            DTMIN(I) = GEO(172,PID(1))      
          ENDDO
       END IF
      ENDIF
C
      DO I=1,NEL
        SSP_EQ(I) = MAX(EM20,QX(I)+SQRT(QX(I)*QX(I)+CX(I)*CX(I)))
        DTX(I)    = DELTAX(I) / SSP_EQ(I)
      ENDDO
C
      !This would output CFL value without scale factor.
      !IF (G_DT>0)THEN
      !  DO I=1,NEL
      !    DTEL(I)   = DTX(I)
      !  ENDDO      
      !ENDIF      
C
      IF(INT22 > 0)THEN
        !check if at least one brick has time step smaller than cinematic one => elementary time step.
        DO I=1,NEL 
          IF(DTX(I)<dt22_min)THEN
            IDT_INT22 = 0
            !NELTST    = NGL(I)       
            !ITYPTST   = ITY 
            EXIT ! just set IDT_INT22
          ENDIF
        ENDDO
      ENDIF
C
      IF(JTHE > 0)THEN
        MT  = MAT(1)
        SPH = PM(69,MT)
        AK1 = PM(75,MT)
        BK1 = PM(76,MT)
        AK2 = PM(77,MT)
        BK2 = PM(78,MT)
        TLI = PM(80,MT)
       DO I=1,NEL       
        IF(T(I)<TLI)THEN
         AKK=AK1+BK1*T(I)
        ELSE
         AKK=AK2+BK2*T(I)
        ENDIF
        IF(JTUR/=0)THEN
         XMU = RHO(I)*PM(24,MT)
         TMU = PM(81,MT)
         RPR = PM(95,MT)
         ATU=RPR*TMU*RK(I)*RK(I)/(MAX(EM15,RE(I)*VOL(I))*XMU)
         AKK=AKK*(1.+ATU)
        ENDIF
        DTX(I) = MIN(DTX(I),HALF*DELTAX(I)*DELTAX(I)*SPH/MAX(AKK,EM20))
       ENDDO     
      ENDIF
C
      IF(.NOT.(IDTMINS==2.AND.JSMS/=0))THEN
       IF(KDTSMSTR==1.AND.ISMSTR==1.OR.
     .         ((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3))THEN
        !   KDTSMSTR==1 en version 5, par defaut.
        MT  = MAT(1)
        DO I=1,NEL
          RHO0(I)=PM(1,MT)
        END DO
        DO 50 I=1,NEL
          STI(I) = ZERO
          IF(OFF(I)==ZERO.OR.OFFG(I)<ZERO) GO TO 50
	  IF(IPM(251,MT)/=0) GO TO 50
          IF(N2D==0) THEN 
            TIDT = ONE/DTX(I)
            IF(OFFG(I)>ONE)THEN
              TRHO = RHO0(I) * TIDT
              TVOL = VOL0(I) * TIDT
            ELSE
              TRHO = RHO(I) * TIDT
              TVOL = VOL(I) * TIDT
            END IF
            ! STI will be changed to 2*STI/NNE in SxCUMU  
            ! [ STI(I) = FOURTH * TRHO * TVOL for 8node bricks ]
            STI(I) = TRHO * TVOL
          ELSE
            ! small strain is not available in 2D analysis
            TIDT = ONE/DTX(I)
            TRHO = RHO(I) * TIDT
            TAIRE = AIRE(I) * TIDT
            STI(I) = HALF * TRHO * TAIRE
          ENDIF
C         dt2 remplace par dt2t
  50    CONTINUE
        IF(ALE_OR_EULER==0)THEN
          DO I=1,NEL
            DTX(I)= DTFAC1(ITY)*DTX(I)
          ENDDO
        ELSE
          DO I=1,NEL
            DTX(I)= DTFAC1(102)*DTX(I)
          ENDDO        
        ENDIF  
        !/DT/NODA & /DT/NODA/CST HAS NO EFFECT WITH ALE/EULER
        IF(ALE_OR_EULER==1 .OR. NODADT==0)THEN
          DO I=1,NEL
            IF(VOL(I)>ZERO .AND. (OFF(I)/=ZERO.AND.OFFG(I)>=ZERO))DT2T= MIN(DTX(I),DT2T)    
          ENDDO     
        ENDIF
        
       ELSE 
        IF(ISMSTR == 11) THEN
          IF(N2D == 0) THEN
            DO I=1,NEL
              STI(I) = ZERO
              IF(OFF(I)==ZERO.OR.OFFG(I)<ZERO) CYCLE
              IF(IPM(251,MT)/=0) CYCLE
                TIDT = ONE/DTX(I)
                TRHO = RHO0(I) * TIDT
                TVOL = VOL0(I) * TIDT
                STI(I) = TRHO * TVOL
            ENDDO 
          ELSE
            DO I=1,NEL
              STI(I) = ZERO
              IF(OFF(I)==ZERO.OR.OFFG(I)<ZERO) CYCLE
              IF(IPM(251,MT)/=0) CYCLE
              TIDT = ONE/DTX(I)
              TRHO = RHO(I) * TIDT
              TAIRE = AIRE(I) * TIDT
              STI(I) = HALF * TRHO * TAIRE
            ENDDO 
          ENDIF  ! N2D
        ELSE
           DO 60 I=1,NEL
             STI(I) = ZERO
             IF(OFF(I)==ZERO.OR.OFFG(I)<ZERO) GO TO 60
             IF(IPM(251,MT)/=0) GO TO 60
             IF(N2D==0) THEN
               TIDT = ONE/DTX(I)
               TRHO = RHO(I) * TIDT
               TVOL = VOL(I) * TIDT
               STI(I) = TRHO * TVOL
C              STI will be changed to 2*STI/NNE in SxCUMU  
C              [ STI(I) = FOURTH * TRHO * TVOL for 8node bricks ]
             ELSE
               TIDT = ONE/DTX(I)
               TRHO = RHO(I) * TIDT
               TAIRE = AIRE(I) * TIDT
               STI(I) = HALF * TRHO * TAIRE
             ENDIF
C            dt2 remplace par dt2t
  60       CONTINUE
         ENDIF ! ISMSTR == 11
        IF(ALE_OR_EULER==0)THEN
          DO I=1,NEL
            DTX(I)= DTFAC1(ITY)*DTX(I)
          ENDDO
        ELSE
          DO I=1,NEL
            DTX(I)= DTFAC1(102)*DTX(I)
          ENDDO        
        ENDIF
        !/DT/NODA & /DT/NODA/CST HAS NO EFFECT WITH ALE/EULER
        IF(ALE_OR_EULER==1 .OR. NODADT==0)THEN
          DO I=1,NEL
            IF(VOL(I)>ZERO .AND. (OFF(I)/=ZERO.AND.OFFG(I)>=ZERO))DT2T= MIN(DTX(I),DT2T)      
          ENDDO      
        ENDIF
       END IF
C
      ELSE ! IDTMINS == 2 .AND. JSMS /= 0
       DO I=1,NEL
         DTY(I)= DTX(I)
         DTX(I)= DTFAC1(ITY)*DTX(I)
       END DO
      END IF
C----
      IF(IMCONV==1)THEN
       IF(IDTMIN(ITY)==1)THEN
        ERROR=0
        DO 70 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO. 
     .        OR.OFFG(I)<ZERO) GO TO 70
          ERROR=1
   70   CONTINUE

        IF (ERROR==1) THEN
          TSTOP = TT
          DO I=1,NEL
            IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO. 
     .         OR.OFFG(I)<ZERO) CYCLE
#include "lockon.inc"
            WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENT',
     . NGL(I)
#include "lockoff.inc"
          END DO
#include "lockon.inc"
          WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENTS'
#include "lockoff.inc"
        ENDIF
       ELSEIF(IDTMIN(ITY)==2)THEN
        ICOUNT=0
        DO 75 I=1,NEL
          IF(DTMIN(I)/=ZERO) THEN
           IF(DTX(I)>DTMIN(I).OR.OFF(I)==ZERO.
     .        OR.OFFG(I)<ZERO) GO TO 75
          ELSE
           IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO.
     .        OR.OFFG(I)<ZERO) GO TO 75
          ENDIF
          OFF(I) = ZERO
          ICOUNT=ICOUNT+1
          LIST(ICOUNT)=I
   75   CONTINUE

        DO J=1,ICOUNT
          I = LIST(J)
#include "lockon.inc"
          WRITE(IOUT,*)
     . ' -- DELETE SOLID ELEMENTS',NGL(I)
          WRITE(ISTDO,*)
     . ' -- DELETE SOLID ELEMENTS',NGL(I)
#include "lockoff.inc"
         IDEL7NOK = 1
        ENDDO
       ELSEIF(IDTMIN(ITY)==3.AND.(ISMSTR==2.OR.ISMSTR==12))THEN
        ICOUNT = 0
        DO 76 I=1,NEL
           IF(DTMIN(I)/=ZERO) THEN
             IF(DTX(I)>DTMIN(I).OR.
     .      OFF(I)<ONE.OR.OFFG(I)<=ZERO.OR.OFFG(I)==TWO) GO TO 76
           ELSE
              IF(DTX(I)>DTMIN1(ITY).OR.
     .      OFF(I)<ONE.OR.OFFG(I)<=ZERO.OR.OFFG(I)==TWO) GO TO 76
           ENDIF
          OFFG(I) = TWO
          ICOUNT=ICOUNT+1
          LIST(ICOUNT)=I
   76   CONTINUE

        DO J=1,ICOUNT
          I=LIST(J)
#include "lockon.inc"
          WRITE(IOUT,*)
     . '-- CONSTANT TIME STEP FOR SOLID ELEMENT NUMBER ',NGL(I)
          WRITE(ISTDO,*)
     . '-- CONSTANT TIME STEP FOR SOLID ELEMENT NUMBER ',NGL(I)
#include "lockoff.inc"
        ENDDO
       ELSEIF(IDTMIN(ITY)==5)THEN
        ERROR=0
        DO 570 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO.
     .      OR.OFFG(I)<ZERO) GO TO 570
          ERROR=1
  570   CONTINUE
        IF (ERROR==1) THEN
          MSTOP = 2
          DO I=1,NEL
            IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO. 
     .         OR.OFFG(I)<ZERO) CYCLE
#include "lockon.inc"
            WRITE(IOUT,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENT',
     . NGL(I)
#include "lockoff.inc"
          END DO
#include "lockon.inc"
          WRITE(ISTDO,*)
     . ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENTS'
#include "lockoff.inc"
        ENDIF
       ENDIF
      END IF ! IF(IMCONV==1)
C
      IF(IDTMINS == 2 .AND. JSMS /= 0)THEN
       IF(ISMSTR==1.OR.
     +   ((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3))THEN
        MT  = MAT(1)
        DO I=1,NEL
          RHO0(I)=PM(1,MT)
        END DO
        DO I=1,NEL
          STI(I) = ZERO
          IF(OFF(I)==ZERO.OR.OFFG(I)<ZERO) CYCLE
          IF(IPM(251,MT)/=0) CYCLE
C 1st compute stiffness
          IF(N2D==0) THEN
            TIDT = ONE/DTY(I)
            IF(OFFG(I)>ONE)THEN
              TRHO = RHO0(I) * TIDT
              TVOL = VOL0(I) * TIDT
            ELSE
              TRHO = RHO(I) * TIDT
              TVOL = VOL(I) * TIDT
            END IF
            STI(I) = TRHO * TVOL
C           STI will be changed to 2*STI/NNE in SxCUMU  
C           [ STI(I) = FOURTH * TRHO * TVOL for 8node bricks ]
          ELSE
            TIDT = ONE/DTY(I)
            TRHO = RHO(I) * TIDT
            TAIRE = AIRE(I) * TIDT
            STI(I) = HALF * TRHO * TAIRE
          ENDIF
c
c dmels = 2*dmels !!   
c w^2 < 2k / (m+dmels+dmels/...) < 2k / (m+dmels)
c dt = 2/w = sqrt( 2*(m+dmels)/k), k=2m/dty^2
          DMELS(I)=MAX(DMELS(I),
     .             TWO*MSSA(I)*((DTMINS/(DTFACS*DTY(I)))**2 - ONE))
          DTX(I)=DTFACS*SQRT(ONE+DMELS(I)/(TWO*MSSA(I)))*DTY(I)

        END DO
       ELSE
        DO I=1,NEL
          STI(I) = ZERO
          IF(OFF(I)==ZERO.OR.OFFG(I)<ZERO) CYCLE
          IF(IPM(251,MT)/=0) CYCLE
C 1st compute stiffness
          IF(N2D==0) THEN
            TIDT = ONE/DTY(I)
            TRHO = RHO(I) * TIDT
            TVOL = VOL(I) * TIDT
            STI(I) = TRHO * TVOL
C           STI will be changed to 2*STI/NNE in SxCUMU  
C           [ STI(I) = FOURTH * TRHO * TVOL for 8node bricks ]
          ELSE
            TIDT = ONE/DTY(I)
            TRHO = RHO(I) * TIDT
            TAIRE = AIRE(I) * TIDT
            STI(I) = HALF * TRHO * TAIRE
          ENDIF
c
c dmels = 2*dmels !!   
c w^2 < 2k / (m+dmels+dmels/...) < 2k / (m+dmels)
c dt = 2/w = sqrt( 2*(m+dmels)/k), k=2m/dty^2
          DMELS(I)=MAX(DMELS(I),
     .             TWO*MSSA(I)*((DTMINS/(DTFACS*DTY(I)))**2 - ONE))
          DTX(I)=DTFACS*SQRT(ONE+DMELS(I)/(TWO*MSSA(I)))*DTY(I)
        END DO
       END IF
C
       IF(JTHE > 0)THEN
         MT  = MAT(1)
         DO I=1,NEL
          IF(OFF(I)==ZERO.OR.OFFG(I)<ZERO) CYCLE
          SPH = PM(69,MT)
          AK1 = PM(75,MT)
          BK1 = PM(76,MT)
          AK2 = PM(77,MT)
          BK2 = PM(78,MT)
          TLI = PM(80,MT)
          IF(T(I)<TLI)THEN
           AKK=AK1+BK1*T(I)
          ELSE
           AKK=AK2+BK2*T(I)
          ENDIF
          IF(JTUR/=0)THEN
           XMU = RHO(I)*PM(24,MT)
           TMU = PM(81,MT)
           RPR = PM(95,MT)
           ATU=RPR*TMU*RK(I)*RK(I)/(MAX(EM15,RE(I)*VOL(I))*XMU)
           AKK=AKK*(1.+ATU)
          ENDIF
          DTX(I) = MIN(DTX(I),DTFACS*HALF*DELTAX(I)*DELTAX(I)*
     .  				    SPH/MAX(AKK,EM20))
         ENDDO
       END IF
      END IF
C
      IF(NODADT==0 .OR. (IDTMINS == 2 .AND. JSMS /= 0)  .OR.   ALE_OR_EULER==1)THEN
        ID_MIN=0
        DO I=1,NEL
          IF(DTX(I)>DT2T .OR. OFF(I)<=ZERO .OR. OFFG(I)<=ZERO) CYCLE
          ! nelts et itypts remplaces par neltst et itypst
          IF(VOL(I)<=ZERO)CYCLE
          DT2T     = DTX(I)
          NELTST   = NGL(I)
          ITYPTST  = ITY 
          ID_MIN   = I        
        ENDDO
        IF(DT2T<DTMIN1(102).AND.ID_MIN>0)THEN
          TSTOP = TT
          print *, "DT=",DT2T,DTMIN1(102)
#include "lockon.inc"
          WRITE(IOUT,*) ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR ALE/EULER CELL',NGL(ID_MIN)
          WRITE(ISTDO,*)' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR ALE/EULER CELL',NGL(ID_MIN)
#include "lockoff.inc"          
        ENDIF      
      ENDIF
C
C--------------------------
C     THERMAL TIME STEP
C--------------------------    
      IF(JTHE < 0.AND.IDT_THERM == 1)THEN
        MT  = MAT(1)
        SPH = PM(69,MT)
        AK1 = PM(75,MT)
        BK1 = PM(76,MT)
        AK2 = PM(77,MT)
        BK2 = PM(78,MT)
        TLI = PM(80,MT)
       DO I=1,NEL
        IF(T(I)<TLI)THEN
         AKK=AK1+BK1*T(I)
        ELSE
         AKK=AK2+BK2*T(I)
        ENDIF
        IF(JTUR/=0)THEN
         XMU = RHO(I)*PM(24,MT)
         TMU = PM(81,MT)
         RPR = PM(95,MT)
         ATU = RPR*TMU*RK(I)*RK(I)/(MAX(EM15,RE(I)*VOL(I))*XMU)
         AKK = AKK*(1.+ATU)
        ENDIF
        AKK = AKK * THEACCFACT
        DT  = DTFACTHERM*HALF*DELTAX(I)*DELTAX(I)*SPH/MAX(AKK,EM20)
        IF(DT<DT_THERM.AND.OFF(I)>ZERO.AND.OFFG(I)>ZERO) DT_THERM = DT
        CONDE(I) = FOUR*VOL(I)*AKK/(DELTAX(I)*DELTAX(I))
        CONDE(I) = CONDE(I)*OFF(I) 
       ENDDO
      ENDIF  
C
C--------------------------
C     ELEMENT TIME STEP
C--------------------------  
      !here scale factor already applied   
      IF (G_DT>0)THEN
        DO I=1,NEL
          DTEL(I)   = DTX(I)
        ENDDO      
      ENDIF
C--------------------------

      RETURN
      END
