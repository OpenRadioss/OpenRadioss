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
Chd|  FMQVISCB                      source/materials/mat_share/mqviscb.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE FMQVISCB(
     1   NEL,     PM,      GEO,     PID,
     2   MAT,     NGL,     NELTST,  ITYPTST,
     3   DT2T,    UVAR,    SSP,     OFF,
     4   OFFG,    AIRE,    DELTAX,  VIS,
     5   VD2,     QVIS,    ITY,     ISMSTR)
C============================================================================
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ITY
      INTEGER, INTENT(IN) :: ISMSTR
C
      INTEGER NELTST,ITYPTST,PID(*),MAT(*),NEL,NGL(*)
      my_real DT2T
      my_real PM(NPROPM,NUMMAT),GEO(NPROPG,NUMGEO),DELTAX(*),SSP(*),AIRE(*),QVIS(*),VIS(*),UVAR(NEL,*),OFF(*),VD2(*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, MT, K,ICOUNT,LIST(MVSIZ),ERROR, ALE_OR_EULER
      my_real AL(MVSIZ),
     .        DTX(MVSIZ), AD(MVSIZ), QX(MVSIZ), CX(MVSIZ),SSP_EQ(MVSIZ),
     .        RHO0(MVSIZ),DTMIN(MVSIZ),QA, QB, VISI, FACQ,QAA,
     .        CNS1, CNS2, SPH, AK1, BK1, AK2, BK2, TLI, AKK, XMU, TMU, RPR,
     .        ATU, QAD, QBD, QAAP,DD
      my_real TIDT,TVOL,TRHO,TAIRE, DTINV
C-----------------------------------------------

      ! by default don't apply /DT/NODA/* to ALE/EULER cells ; unless if /DT/NODA/ALEON is enabled (hidden card introduced for backward compatibility)
      MT = MAT(1)
      ALE_OR_EULER = 0 
      IF(NINT(PM(72,MT))==1 .OR. NINT(PM(72,MT))==2)ALE_OR_EULER = 1
      IF( ALE%GLOBAL%I_DT_NODA_ALE_ON==1)ALE_OR_EULER = 0   

      DTINV = DT1 / MAX(EM20 , DT1**2)
      IF(IMPL==ZERO)THEN
        DO I=1,NEL
          AD(I)=ZERO
          AL(I)=ZERO
          CX(I)=SSP(I)+SQRT(VD2(I))
        ENDDO
      ENDIF

       VISI=ONE
       FACQ=ONE
      
      IF(N2D>0) THEN
        MT = MAT(1)
        DO I=1,NEL
         IF(OFF(I)==ONE)THEN
           AL(I)=SQRT(AIRE(I))
           RHO0(I)=PM(192,MT)
            DD = QVIS(I)  ! volumic strain
           AD(I)= MAX(ZERO,DD)
         ENDIF
        ENDDO
      ELSE
        MT = MAT(1)
        DO I=1,NEL
         IF(OFF(I)==ONE)THEN
           AL(I)=UVAR(I,3)**THIRD
           RHO0(I)=PM(192,MT)
            DD = QVIS(I)  ! volumic strain
            AD(I)= MAX(ZERO,DD)
         ENDIF
        ENDDO
      ENDIF
C      
      DO I=1,NEL
          QA =FACQ*GEO(14,PID(I))
          QB =FACQ*GEO(15,PID(I))
          CNS1=GEO(16,PID(I))
          CNS2=GEO(17,PID(I))*SSP(I)*AL(I)*UVAR(I,1)
          QAA = QA*QA*AD(I)
          QX(I)=(QB+CNS1)*SSP(I)+DELTAX(I) * QAA + VISI*(TWO*VIS(I)+CNS2) / MAX(EM20,UVAR(I,1)*DELTAX(I))
          QVIS(I)=UVAR(I,1)*AD(I)*AL(I)*(QAA*AL(I)+QB*SSP(I))  
           DTMIN(I) = GEO(172,PID(I)) 
      ENDDO
C
      DO I=1,NEL
        SSP_EQ(I) = MAX(EM20,QX(I)+SQRT(QX(I)*QX(I)+CX(I)*CX(I)))
        DTX(I) = DELTAX(I) / SSP_EQ(I)      
      ENDDO
C
      ! KDTSMSTR==1 en version 5, par defaut.
      IF(KDTSMSTR==1.AND.ISMSTR==1.OR.(ISMSTR==2.AND.IDTMIN(1)==3))THEN
        DO 50 I=1,NEL
          IF(OFF(I)==ZERO.OR.OFFG(I)<ZERO) GO TO 50
          IF(N2D==0) THEN 
            TIDT = ONE/DTX(I)
            IF(OFFG(I)>ONE)THEN
              TRHO = RHO0(I) * TIDT
            ELSE
              TRHO = UVAR(I,1) * TIDT
              TVOL = UVAR(I,3) * TIDT
            END IF
            !STI will be changed to 2*STI/NNE in SxCUMU  
          ELSE
            !small strain is not available in 2D analysis
            TIDT = ONE/DTX(I)
            TRHO = UVAR(I,1) * TIDT
            TAIRE = AIRE(I) * TIDT
          ENDIF
C         dt2 replaced with dt2t                 
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
            IF(OFF(I)/=ZERO.AND.OFFG(I)>=ZERO)DT2T = MIN(DTX(I),DT2T)        
          ENDDO
        ENDIF            
      ELSE 
        DO 60 I=1,NEL
          IF(OFF(I)==ZERO.OR.OFFG(I)<ZERO) GO TO 60
          IF(N2D==0) THEN
            TIDT = ONE/DTX(I)
            TRHO = UVAR(I,1) * TIDT
            TVOL = UVAR(I,3) * TIDT
            !STI will be changed to 2*STI/NNE in SxCUMU  
          ELSE
            TIDT = ONE/DTX(I)
            TRHO = UVAR(I,1) * TIDT
            TAIRE = AIRE(I) * TIDT
          ENDIF
C         dt2 remplace par dt2t
  60    CONTINUE
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
        IF(ALE_OR_EULER.EQ .1.OR. NODADT==0)THEN
          DO I=1,NEL
            IF(OFF(I)/=ZERO.AND.OFFG(I)>=ZERO)DT2T = MIN(DTX(I),DT2T)        
          ENDDO
        ENDIF  
      END IF
C
      IF(IMCONV==1)THEN
      IF(IDTMIN(ITY)==1)THEN
        ERROR=0
        DO 70 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO .OR.OFFG(I)<ZERO) GO TO 70
          ERROR=1
   70   CONTINUE

        IF (ERROR==1) THEN
          TSTOP = TT
#include "lockon.inc"
          WRITE(IOUT,*) ' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENTS'
          WRITE(ISTDO,*)' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENTS'
#include "lockoff.inc"
        ENDIF
      ELSEIF(IDTMIN(ITY)==2)THEN
        ICOUNT=0
        DO 75 I=1,NEL
        IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)==ZERO.OR.OFFG(I)<ZERO) GO TO 75
          OFF(I) = ZERO
          ICOUNT=ICOUNT+1
          LIST(ICOUNT)=I
   75   CONTINUE

        DO J=1,ICOUNT
          I = LIST(J)
#include "lockon.inc"
          WRITE(IOUT,*)' -- DELETE SOLID ELEMENTS',NGL(I)
          WRITE(ISTDO,*)' -- DELETE SOLID ELEMENTS',NGL(I)
#include "lockoff.inc"
         IDEL7NOK = 1
        ENDDO
      ELSEIF(IDTMIN(ITY)==3.AND.ISMSTR==2)THEN
        ICOUNT = 0
        DO 76 I=1,NEL
           IF(DTMIN(I)/=0) THEN
             IF(DTX(I)>DTMIN(I).OR.OFF(I)<ONE.OR.OFFG(I)<=ZERO.OR.OFFG(I)==TWO) GO TO 76
            ELSE
              IF(DTX(I)>DTMIN1(ITY).OR.OFF(I)<ONE.OR.OFFG(I)<=ZERO.OR.OFFG(I)==TWO) GO TO 76
           ENDIF
          OFFG(I) = TWO
          ICOUNT=ICOUNT+1
          LIST(ICOUNT)=I
   76   CONTINUE

        DO J=1,ICOUNT
          I=LIST(J)
#include "lockon.inc"
          WRITE(IOUT,*)'-- CONSTANT TIME STEP FOR SOLID ELEMENT NUMBER ',NGL(I)
          WRITE(ISTDO,*)'-- CONSTANT TIME STEP FOR SOLID ELEMENT NUMBER ',NGL(I)
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
#include "lockon.inc"
          WRITE(IOUT,*)' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENTS'
          WRITE(ISTDO,*)' **ERROR : TIME STEP LESS OR EQUAL DTMIN FOR SOLID ELEMENTS'
#include "lockoff.inc"
        ENDIF
      ENDIF
      END IF ! IF(IMCONV==1)
C
      !/DT/NODA & /DT/NODA/CST HAS NO EFFECT WITH ALE/EULER    
      IF(NODADT==0 .OR. ALE_OR_EULER==1)THEN
       DO 80 I=1,NEL
        IF(DTX(I)>DT2T.OR.OFF(I)<=ZERO.OR.OFFG(I)<=ZERO) GO TO 80 
C nelts et itypts remplaces par neltst et itypst
        DT2T    = DTX(I)
        NELTST =NGL(I)
        ITYPTST=ITY
   80  CONTINUE
      ENDIF

      RETURN
      END      
