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
Chd|  CNDT3PINCH                    source/elements/shell/coqueba/cndt3pinch.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CNDT3PINCH(JFT   ,JLT     ,OFF  ,DT2T ,AMU  ,
     1                 NELTST,ITYPTST ,STI  ,STIR ,OFFG,
     2                 SSP   ,VISCMX  ,RHO  ,VOL0 ,THK0,THK02,
     3                 A1    ,ALDT    ,ALPE ,NGL  ,ISMSTR,
     4                 IOFC  ,NNE     ,AREA ,G    ,SHF   ,
     5                 MSC   ,DMELC   ,JSMS ,PTG  ,IGTYP ,
     6                 IGMAT ,A11R    ,G_DT ,DTEL ,A11PINCH)
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
#include      "com08_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NELTST,ITYPTST,ISMSTR,IOFC,NNE, JSMS,IGTYP
      INTEGER NGL(*),IGMAT,G_DT
C     REAL
      my_real
     .   OFF(*),STI(*),STIR(*),OFFG(*),SSP(*),AMU(*),
     .   ALDT(*), ALPE(*), A1(*), THK0(*),THK02(*),
     .   VOL0(*), VISCMX(*), RHO(*),DT2T, AREA(*), G(*), SHF(*),
     .   MSC(*), DMELC(*), PTG(3,*),A11R(*),DTEL(MVSIZ), A11PINCH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDXOF(MVSIZ),
     .        I, J, II, NINDX,IDT,ITYEL
      my_real DT(MVSIZ),FAC,MAS,DIVM,MMIN  ,IZ   
C=======================================================================
      DO I=JFT,JLT
        VISCMX(I) = MAX(VISCMX(I), AMU(I))
        VISCMX(I) = SQRT(ONE + VISCMX(I)*VISCMX(I)) - VISCMX(I)
        ALDT(I)   = ALDT(I) * VISCMX(I) / SQRT(ALPE(I))
      ENDDO
c---------------------------------------------------
      ITYEL = 3
      IF (NNE==3) ITYEL = 7
C----- isub will add here-----
      IF(IDTMINS == 2)THEN
        DO I=JFT,JLT
         IF (OFF(I)==ZERO) THEN
          STI(I) = ZERO
          STIR(I) = ZERO          
         ELSE
          STI(I) = HALF*VOL0(I) * A11PINCH / ALDT(I)**2

       IF(IGTYP == 52 .OR. 
     .   ((IGTYP == 11 .OR. IGTYP == 17 .OR. IGTYP == 51)
     .                                   .AND. IGMAT > 0 )) THEN
            FAC = HALF*VOL0(I)/ ALDT(I)**2
            STIR(I) =(FAC * A11R(I)*THK02(I) + 
     .                 FAC * A1(I) *AREA(I)) *ONE_OVER_12
          ELSE
            STIR(I) = STI(I) * (THK02(I)+AREA(I)) *ONE_OVER_12
          ENDIF
         ENDIF 
        ENDDO
C
        IF(JSMS /= 0)THEN
         IF(ITYEL==3)THEN
           DO I=JFT,JLT
            IF(OFFG(I) < ZERO .OR. OFF(I) == ZERO) CYCLE
c
c dmelc = 2*dmelc !!   
c w^2 < 2k / (m+dmelc+dmelc/3) < 2k / (m+dmelc)
c dt = 2/w = sqrt( 2*(m+dmelc)/k)
            DMELC(I)=MAX(DMELC(I),
     .  		(DTMINS/DTFACS)**2 * STI(I) - TWO*MSC(I))
            DT(I)  = DTFACS*
     .  	     SQRT((TWO*MSC(I)+DMELC(I))/MAX(EM20,STI(I)))
            IF(DT(I)<DT2T)THEN
              DT2T    = DT(I)
              NELTST  = NGL(I)
              ITYPTST = ITYEL
            END  IF
           END DO
         ELSE
           DO I=JFT,JLT
            IF(OFFG(I) < ZERO .OR. OFF(I) == ZERO) CYCLE
c
            MMIN=MSC(I)*MIN(PTG(1,I),PTG(2,I),PTG(3,I))
c
c dmelc = 2*dmelc !!   
c w^2 < 2k / (m+dmelc+dmelc/2) < 2k / (m+dmelc)
c dt = 2/w = sqrt( 2*(m+dmelc)/k)
            DMELC(I)=MAX(DMELC(I),
     .  		(DTMINS/DTFACS)**2 * STI(I) - TWO*MMIN)
            DT(I)  = DTFACS*
     .  	     SQRT((TWO*MMIN+DMELC(I))/MAX(EM20,STI(I)))
            IF(DT(I)<DT2T)THEN
              DT2T    = DT(I)
              NELTST  = NGL(I)
              ITYPTST = ITYEL
            END  IF
           END DO
         END IF
        ENDIF
      ELSEIF(NODADT/=0)THEN
C          IF(IGTYP == 52 .OR. 
C     .        ((IGTYP == 11 .OR. IGTYP == 17 .OR. IGTYP == 51)
C     .                                   .AND. IGMAT > 0 ))THEN
C            DO I=JFT,JLT
C                IF (OFF(I)==ZERO) THEN
C                   STI(I) = ZERO
C                   STIR(I) = ZERO          
C                ELSE
C                   STI(I) = HALF*VOL0(I) * A11PINCH / ALDT(I)**2
C                   FAC =  HALF*VOL0(I)/ ALDT(I)**2
C                   STIR(I) = FAC*A11R(I)*THK02(I)*ONE_OVER_12 +
C     .                       FAC *A11PINCH*AREA(I)*ONE_OVER_12
C                  ENDIF
C             ENDDO
C          ELSE
             DO I=JFT,JLT
                IF (OFF(I)==ZERO) THEN
                   STI(I) = ZERO
                   STIR(I) = ZERO          
                ELSE
                   STI(I) = HALF*VOL0(I) * A11PINCH / ALDT(I)**2
                   STIR(I) = STI(I) * (THK02(I)+AREA(I)) *ONE_OVER_12
                ENDIF
             ENDDO
C          ENDIF 
      ENDIF
C
      DO I=JFT,JLT
        DT(I)=DTFAC1(ITYEL)*ALDT(I)/SSP(I)   
      ENDDO
      IF(G_DT>0)THEN
        DO I=JFT,JLT
          DTEL(I)=ALDT(I)/SSP(I)     ! DT=ALDT(I)/SSP(I) & DT=DT*DTFAC1 does not lead to same digits, so operation is made again here.
        ENDDO
      ENDIF
      IF((NODADT/=0.OR.IDTMINS==2).AND.IDTMIN(ITYEL)==0)RETURN
C
      IF(IDTMIN(ITYEL)>=1)THEN
        NINDX=IOFC
        DO I=JFT,JLT
         IF(DT(I)<=DTMIN1(ITYEL).AND.
     .     OFF(I)>=ONE.AND.OFFG(I)/=TWO.AND.OFFG(I)>=ZERO) THEN
          NINDX=NINDX+1
          INDXOF(NINDX)=I
         ENDIF
        ENDDO
      ENDIF
C
      IF(IDTMIN(ITYEL)==1)THEN

        IF(NINDX>IOFC) MSTOP = 2

        DO 100 J=IOFC+1,NINDX
          I = INDXOF(J)
#include "lockon.inc"
          WRITE(IOUT,1000)  NNE,NGL(I)
          WRITE(ISTDO,1000) NNE,NGL(I)
#include "lockoff.inc"
  100   CONTINUE
      ELSEIF(IDTMIN(ITYEL)==2)THEN
        IF(NINDX>IOFC) IDEL7NOK = 1         
        DO 125 J=IOFC+1,NINDX
         I = INDXOF(J)
         OFF(I)=0.
#include "lockon.inc"
         WRITE(IOUT,1200)  NNE,NGL(I)
         WRITE(ISTDO,1300) NNE,NGL(I),TT
#include "lockoff.inc"
  125   CONTINUE
        IOFC = NINDX
      ELSEIF(IDTMIN(ITYEL)==3.AND.ISMSTR==2)THEN
        DO 140 J=IOFC+1,NINDX
         I = INDXOF(J)
         OFFG(I)=2.
#include "lockon.inc"
         WRITE(IOUT,1400)  NNE,NGL(I)
         WRITE(ISTDO,1400) NNE,NGL(I)
#include "lockoff.inc"
  140   CONTINUE
        NINDX=IOFC
      ELSEIF(IDTMIN(ITYEL)==5)THEN
        IF(NINDX>IOFC) MSTOP = 2
        DO 160 J=IOFC+1,NINDX
         I = INDXOF(J)
#include "lockon.inc"
         WRITE(IOUT,1000)  NNE,NGL(I)
         WRITE(ISTDO,1000) NNE,NGL(I)
#include "lockoff.inc"
  160   CONTINUE
        NINDX=IOFC
      ENDIF
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
 1000 FORMAT(1X,'--MINIMUM TIME STEP ',I1,'N SHELL ELEMENT NUMBER ',I10)
 1200 FORMAT(1X,'--DELETE ',I1,'N SHELL ELEMENT NUMBER ',I10)
 1300 FORMAT(1X,'--DELETE ',I1',N SHELL ELEMENT:',I10,' AT TIME:',G11.4)
 1400 FORMAT(1X,'--CONSTANT TIME STEP ',I1,'N SHELL ELEMENT NUMBER',I10)
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
         ITYPTST = ITYEL
       ENDIF
       ENDDO
      ENDIF
C
      IF(IDTMINS==2)RETURN
C
      DO I=JFT,JLT
        DIVM=MAX(ALDT(I)*ALDT(I),EM20)
        STI(I) = HALF*VOL0(I) * A11PINCH* OFF(I) / DIVM
        STIR(I)= ZERO
      ENDDO
C
      RETURN
      END
