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
Chd|  C3DT3                         source/elements/sh3n/coque3n/c3dt3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|        CSSP2A11                      source/elements/sh3n/coque3n/cssp2a11.F
Chd|====================================================================
      SUBROUTINE C3DT3(JFT   ,JLT    ,PM     ,OFF    ,DT2T   ,
     2                 NELTST,ITYPTST,STI    ,STIR   ,OFFG   ,
     3                 SSP   ,VISCMX ,ISMSTR ,NFT    ,IOFC   ,
     4                 ALPE  ,MSTG   ,DMELTG ,JSMS   ,PTG    ,
     5                 SHF   ,IGTYP  ,IGMAT  ,G      ,A1     ,
     6                 A11R  ,G_DT   ,DTEL   ,ALDT   ,THK0   ,
     7                 AREA  ,NGL    ,IMAT   ,MTN    )
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
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "couple_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NELTST,ITYPTST,ISMSTR,NFT,IOFC, JSMS,IGTYP,
     .        IGMAT,IMAT,MTN
      INTEGER NGL(MVSIZ)
      my_real
     .   PM(NPROPM,*), OFF(*),STI(*),STIR(*),OFFG(*),SSP(MVSIZ),
     .   VISCMX(MVSIZ),DT2T, MSTG(*), DMELTG(*), PTG(3,*),SHF(*), G(MVSIZ),
     .   A11R(MVSIZ),A1(MVSIZ),ALDT(MVSIZ),THK0(MVSIZ),AREA(MVSIZ),ALPE(MVSIZ)
      my_real,DIMENSION(JLT), INTENT(INOUT) :: DTEL
      INTEGER,INTENT(IN)    :: G_DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDX(MVSIZ),I, II, NINDX,IDT
      my_real DT(MVSIZ)
      my_real ATHK, MMIN,FAC
C=======================================================================       
      DO I=JFT,JLT
        VISCMX(I) = SQRT(ONE + VISCMX(I)*VISCMX(I)) - VISCMX(I)
        ALDT(I)   = ALDT(I)*VISCMX(I) / SQRT(ALPE(I))
      ENDDO
c---------------------------------------------------
C
      IF (NODADT/=0) THEN
        IF(IGTYP == 52 .OR. 
     .   ((IGTYP == 11 .OR. IGTYP == 17 .OR. IGTYP == 51)
     .                                   .AND. IGMAT > 0 )) THEN
           DO  I=JFT,JLT
               IF (OFF(I)==ZERO) THEN
                 STI(I) = ZERO
                 STIR(I) = ZERO
               ELSE
                 ATHK   = AREA(I) * THK0(I)
                 STI(I) = ATHK * A1(I) / ALDT(I)**2
                 FAC =A11R(I)*AREA(I)/ ALDT(I)**2
                 STIR(I) = FAC*(ONE_OVER_12* THK0(I)**3
     .                 + THK0(I)*HALF * SHF(I) * AREA(I) * G(I)/A1(I))
                ENDIF
          ENDDO       
        ELSE
           DO  I=JFT,JLT
            A1(I) = PM(24,IMAT)
            G(I)  = PM(22,IMAT)
           ENDDO
           IF (MTN ==58 ) CALL CSSP2A11(PM   ,IMAT  ,SSP  ,A1   ,JLT   )
           DO  I=JFT,JLT
               IF (OFF(I)==ZERO) THEN
                 STI(I) = ZERO
                 STIR(I) = ZERO
               ELSE
                 ATHK   = AREA(I) * THK0(I)
                 STI(I) = ATHK * A1(I) / ALDT(I)**2
                 STIR(I) = STI(I) * (THK0(I) * THK0(I) * ONE_OVER_12 
     .                   + HALF * SHF(I) * AREA(I) * G(I)/A1(I))
c                 STI(I) = 0.5 * ATHK * A1(I) / ALDT(I)**2
c                 STIR(I) = STI(I) * (THK0(I) * THK0(I) / 12. 
c     .                   + AREA(I) /9.) 
               ENDIF
          ENDDO
        ENDIF 
C
        IF (IDTMIN(7) == 0) RETURN
C
      ELSEIF(IDTMINS == 2)THEN
       IF(IGTYP == 52 .OR. 
     .    ((IGTYP == 11 .OR. IGTYP == 17 .OR. IGTYP == 51)
     .                                   .AND. IGMAT > 0 )) THEN
          DO I=JFT,JLT
              IF (OFF(I)==ZERO) THEN
                STI(I) = ZERO
                STIR(I) = ZERO
              ELSE
                ATHK   = AREA(I) * THK0(I)
                STI(I) = ATHK * A1(I) / ALDT(I)**2
                FAC = A11R(I)*AREA(I)/ ALDT(I)**2 
                STIR(I) = FAC * (ONE_OVER_12* (THK0(I)**3)
     .                 + HALF * THK0(I)*SHF(I) * AREA(I) * G(I)/A1(I))
              ENDIF
           END DO 
        ELSE
           DO  I=JFT,JLT
            A1(I) = PM(24,IMAT)
            G(I)  = PM(22,IMAT)
           ENDDO
           DO I=JFT,JLT
              IF (OFF(I)==ZERO) THEN
                STI(I) = ZERO
                STIR(I) = ZERO
              ELSE
                ATHK   = AREA(I) * THK0(I)
                STI(I) = ATHK * A1(I) / ALDT(I)**2
                STIR(I) = STI(I) * (THK0(I) * THK0(I) * ONE_OVER_12 
     .                 + HALF * SHF(I) * AREA(I) * G(I)/A1(I))
              ENDIF
           END DO
        ENDIF  
C
        IF(JSMS /= 0)THEN
         DO I=JFT,JLT
          IF(OFFG(I) < ZERO .OR. OFF(I) == ZERO) CYCLE
c
          MMIN=MSTG(I)*MIN(PTG(1,I),PTG(2,I),PTG(3,I))
c
c dmelc = 2*dmelc !!   
c w^2 < 2k / (m+dmelc+dmelc/2) < 2k / (m+dmelc)
c dt = 2/w = sqrt( 2*(m+dmelc)/k)
          DMELTG(I)=MAX(DMELTG(I),
     .             (DTMINS/DTFACS)**2 * STI(I) - TWO*MMIN)
          DT(I)  = DTFACS*
     .              SQRT((TWO*MMIN+DMELTG(I))/MAX(EM20,STI(I)))
          IF(DT(I)<DT2T)THEN
            DT2T    = DT(I)
            NELTST  = NGL(I)
            ITYPTST = 7
          END  IF
         END DO
        ENDIF
C
        IF(IDTMIN(7)==0)RETURN
C
      ENDIF
C
      DO I=JFT,JLT
        DT(I)=DTFAC1(7)*ALDT(I)/SSP(I)
      END DO
      IF(G_DT/=ZERO)THEN
         DO I=JFT,JLT
           DTEL(I) = DT(I)
         ENDDO
      ENDIF
      
      
C
      NINDX=IOFC
      IF(IDTMIN(7)==1)THEN
        DO 100 I=JFT,JLT
        IF(DT(I)>DTMIN1(7).OR.OFF(I)<ONE
     .       .OR.OFFG(I)==TWO.OR.OFFG(I)<ZERO) GO TO 100
         TSTOP = TT
C
#include "lockon.inc"
         WRITE(IOUT,1000)  NGL(I)
         WRITE(ISTDO,1000) NGL(I)
#include "lockoff.inc"
  100   CONTINUE
      ELSEIF(IDTMIN(7)==2)THEN
        DO 120 I=JFT,JLT
        IF(DT(I)>DTMIN1(7).OR.OFF(I)<ONE
     .       .OR.OFFG(I)<ZERO) GO TO 120
         OFF(I)=ZERO

         II=I+NFT
         NINDX=NINDX+1
         INDX(NINDX)=I
         IDEL7NOK = 1
C
#include "lockon.inc"
         WRITE(IOUT,1200)  NGL(I)
         WRITE(ISTDO,1300) NGL(I),TT
#include "lockoff.inc"
  120   CONTINUE
        IOFC = NINDX
      ELSEIF(ISMSTR==2.AND.IDTMIN(7)==3)THEN
        DO 140 I=JFT,JLT
        IF(DT(I)>DTMIN1(7).OR.
     .   OFF(I)<ONE.OR.OFFG(I)==TWO.OR.OFFG(I)<ZERO) GO TO 140
         OFFG(I)=TWO
C
#include "lockon.inc"
         WRITE(IOUT,1400)  NGL(I)
         WRITE(ISTDO,1400) NGL(I)
#include "lockoff.inc"
  140   CONTINUE
      ELSEIF(IDTMIN(7)==5)THEN
        DO 150 I=JFT,JLT
        IF(DT(I)>DTMIN1(7).OR.OFF(I)<ONE.
     .     OR.OFFG(I)==TWO.OR.OFFG(I)<ZERO) GO TO 150
         MSTOP = 2
C
#include "lockon.inc"
         WRITE(IOUT,1000)  NGL(I)
         WRITE(ISTDO,1000) NGL(I)
#include "lockoff.inc"
  150   CONTINUE
      ENDIF
C
      IF(NODADT/=0.OR.(IDTMINS==2.AND.JSMS/=0))RETURN
C
C- VECTOR
      IDT=0
      DO I=JFT,JLT
        IF(OFFG(I)>ZERO.AND.OFF(I)/=ZERO.AND.DT(I)<DT2T) IDT=1
      ENDDO
C- NON VECTOR
      IF(IDT==1)THEN
       DO I=JFT,JLT
       IF(OFFG(I)>ZERO.AND.OFF(I)/=ZERO.AND.DT(I)<DT2T)THEN
         DT2T    = DT(I)
         NELTST  = NGL(I)
         ITYPTST = 7
       ENDIF
       ENDDO
      ENDIF
C
      IF(IDTMINS==2)RETURN
C
      DO I=JFT,JLT
         STI(I) = AREA(I) * THK0(I) * A1(I) / ALDT(I)**2
         STI(I) = ZEP81 * ZEP81 * STI(I) * OFF(I)
         STIR(I)= ZERO
      ENDDO
C-----------
 1000 FORMAT(1X,'-- MINIMUM TIME STEP 3N SHELL ELEMENT NUMBER ',I10)
 1200 FORMAT(1X,'-- DELETE 3N SHELL ELEMENT NUMBER ',I10)
 1300 FORMAT(1X,'-- DELETE 3N SHELL ELEMENT :',I10,' AT TIME :',G11.4)
 1400 FORMAT(1X,'-- CONSTANT TIME STEP 3N SHELL ELEMENT NUMBER ',I10)
C-----------
      RETURN
      END
