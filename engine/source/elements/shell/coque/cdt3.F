Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    cdt3         ../engine/source/elements/shell/coque/cdt3.F
!||--- called by ------------------------------------------------------
!||    cforc3       ../engine/source/elements/shell/coque/cforc3.F
!||    cforc3_crk   ../engine/source/elements/xfem/cforc3_crk.F
!||====================================================================
      SUBROUTINE CDT3(JFT   ,JLT    ,YM   ,OFF  ,DT2T,
     2                NELTST,ITYPTST,STI  ,STIR ,OFFG,
     3                DTC   ,NDT    ,DT2C ,IXC  ,SSP ,
     4                VISCMX,PX1    ,PX2  ,PY1  ,PY2 ,
     5                VOL0  ,VOL00  ,RHO  ,ALDT ,ALPE,
     6                INDXOF,NGL    ,ISMSTR,IOFC,MSC ,
     7                DMELC ,JSMS   ,G_DT ,DTEL)
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
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDT(*),IXC(NIXC,*),INDXOF(MVSIZ),NGL(MVSIZ),
     .   JFT, JLT,NELTST,ITYPTST,NINDX,ISMSTR,IOFC, JSMS
      my_real
     .   YM(*), OFF(*),STI(*),STIR(*),OFFG(*),DTC(*),
     .   SSP(*), DT2C(*),VISCMX(*),VOL0(*),VOL00(*),
     .   PX1(*), PX2(*), PY1(*), PY2(*), RHO(*), ALDT(*), ALPE(*),
     .   DT2T,
     .   MSC(*), DMELC(*)
      my_real,INTENT(INOUT) :: DTEL(JFT:JLT)
      INTEGER,INTENT(IN)    :: G_DT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  J, I, II,IDT
      my_real
     .   DT(MVSIZ)     
      my_real DIVM,MAS 
C=======================================================================
      IF(IDTMINS == 2 .AND. JSMS /= 0)THEN
        DO I=JFT,JLT
          IF(OFFG(I) < ZERO .OR. OFF(I) == ZERO) CYCLE
c
c dmelc = 2*dmelc !!   
c w^2 < 2k / (m+dmelc+dmelc/3) < 2k / (m+dmelc)
c dt = 2/w = sqrt( 2*(m+dmelc)/k)
          DMELC(I)=MAX(DMELC(I),
     .                (DTMINS/DTFACS)**2 * STI(I) - TWO*MSC(I))
          DT(I)  = DTFACS*
     .             SQRT((TWO*MSC(I)+DMELC(I))/MAX(EM20,STI(I)))
          IF(DT(I)<DT2T)THEN
            DT2T    = DT(I)
            NELTST  = NGL(I)
            ITYPTST = 3
          END  IF
        END DO        
C
        IF(IDTMIN(3)/=0)THEN
          DO I=JFT,JLT
           MAS   = VOL00(I)*RHO(I)
           DT(I) = DTFAC1(3)*SQRT(HALF*MAS/MAX(EM20,STI(I)))
          ENDDO
        END IF
C
      ELSEIF(IDT1SH==1.OR.IDTMINS==2)THEN
          DO I=JFT,JLT
           MAS   = VOL00(I)*RHO(I)
           DT(I) = DTFAC1(3)*SQRT(HALF*MAS/MAX(EM20,STI(I)))
          ENDDO
      ELSE
         DO I=JFT,JLT
           ALDT(I)=ALDT(I)*VISCMX(I)
     .             / SQRT(ALPE(I))
           DT(I)=DTFAC1(3)*ALDT(I)/SSP(I)
         ENDDO
      ENDIF
      
C------------------------------
      IF(NODADT == 0)THEN
        IF(IDTMIN(3) == 0)RETURN
      ENDIF
C------------------------------
        IF(G_DT /= ZERO)THEN
           DO I=JFT,JLT
             DTEL(I) = DT(I)
           ENDDO
        ENDIF
C------------------------------
      IF(IDTMIN(3) == 1)THEN
        NINDX=IOFC
        DO I=JFT,JLT
        IF(DT(I) > DTMIN1(3) .OR. OFF(I) < ONE .OR. OFFG(I) == TWO .OR. OFFG(I) < ZERO) CYCLE
         NINDX=NINDX+1
         INDXOF(NINDX)=I
        ENDDO
C
        DO J=IOFC+1,NINDX
         I = INDXOF(J)
         TSTOP = TT
#include "lockon.inc"
         WRITE(IOUT,1000)  NGL(I)
         WRITE(ISTDO,1000) NGL(I)
#include "lockoff.inc"
        ENDDO
        NINDX=IOFC
      ELSEIF(IDTMIN(3)==2)THEN
        NINDX=IOFC
        DO I=JFT,JLT
        IF(DT(I)>DTMIN1(3).OR.OFF(I)<ONE .OR.OFFG(I)<ZERO) CYCLE
         NINDX=NINDX+1
         INDXOF(NINDX)=I
        ENDDO
C
        DO J=IOFC+1,NINDX
         I = INDXOF(J)
         OFF(I)=0.
         IDEL7NOK = 1         
#include "lockon.inc"
         WRITE(IOUT,1200)  NGL(I)
         WRITE(ISTDO,1300) NGL(I),TT
#include "lockoff.inc"
        ENDDO
        IOFC = NINDX
      ELSEIF(IDTMIN(3)==3.AND.ISMSTR==2)THEN
        NINDX=IOFC
        DO I=JFT,JLT
        IF(DT(I)>DTMIN1(3).OR.OFF(I)<ONE.OR.OFFG(I)==TWO.OR.OFFG(I)<ZERO)CYCLE
         NINDX=NINDX+1
         INDXOF(NINDX)=I
       ENDDO
C
        DO J=IOFC+1,NINDX
         I = INDXOF(J)
         OFFG(I)=2.
#include "lockon.inc"
         WRITE(IOUT,1400)  NGL(I)
         WRITE(ISTDO,1400) NGL(I)
#include "lockoff.inc"
        ENDDO
        NINDX=IOFC
      ELSEIF(IDTMIN(3)==5)THEN
        NINDX=IOFC
        DO I=JFT,JLT
        IF(DT(I)>DTMIN1(3).OR.OFF(I)<ONE.OR.OFFG(I)==TWO.OR.OFFG(I)<ZERO)CYCLE
         NINDX=NINDX+1
         INDXOF(NINDX)=I
       ENDDO
C
        DO J=IOFC+1,NINDX
         I = INDXOF(J)
         MSTOP = 2
#include "lockon.inc"
         WRITE(IOUT,1000)  NGL(I)
         WRITE(ISTDO,1000) NGL(I)
#include "lockoff.inc"
        ENDDO
        NINDX=IOFC
      ENDIF
C
      IF (NODADT/=0.OR.(IDTMINS==2.AND.JSMS/=0)) RETURN
C-------------------------------------------------------------------
C-    VECTOR
      IDT=0
      DO I=JFT,JLT
       IF(OFFG(I)>ZERO.AND.OFF(I)/=ZERO.AND.DT(I)<DT2T)IDT=1
      ENDDO
C-    NON VECTOR
      IF(IDT==1)THEN
       DO I=JFT,JLT
       IF(OFFG(I)>ZERO.AND.OFF(I)/=ZERO.AND.DT(I)<DT2T)THEN
         DT2T    = DT(I)
         NELTST  = NGL(I)
         ITYPTST = 3
       ENDIF
       ENDDO
      ENDIF
C
      IF (IDTMINS==2) RETURN
C-------------------------------------------------------------------
      IF(IDT1SH==1)THEN
       DO I=JFT,JLT
         STI(I) = STI(I) * OFF(I)
         STIR(I)= ZERO
       ENDDO
      ELSE
       DO I=JFT,JLT
         DIVM=MAX(ALDT(I)*ALDT(I),EM20)
         STI(I) = HALF * VOL0(I) * YM(I) / DIVM
         STI(I) = ZEP81 * STI(I) * OFF(I)
         STIR(I)= ZERO
       ENDDO
      ENDIF
C-------------------------------------------------------------------
 1000 FORMAT(1X,'-- MINIMUM TIME STEP SHELL ELEMENT NUMBER ',I10)
 1200 FORMAT(1X,'-- DELETE  OF SHELL ELEMENT NUMBER ',I10)
 1300 FORMAT(1X,'-- DELETE  OF SHELL ELEMENT :',I10,' AT TIME :',G11.4)
 1400 FORMAT(1X,'-- CONSTANT TIME STEP FOR SHELL ELEMENT NUMBER ',I10)
C-------------------------------------------------------------------
      RETURN
      END
