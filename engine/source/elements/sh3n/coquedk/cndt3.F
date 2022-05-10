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
Cqz|--nouvelle routine commune des elements coques---------
Chd|====================================================================
Chd|  CNDT3                         source/elements/sh3n/coquedk/cndt3.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CNDT3(JFT   ,JLT     ,OFF  ,DT2T ,AMU ,
     1                 NELTST,ITYPTST ,STI  ,STIR ,OFFG,
     2                 SSP   ,VISCMX  ,RHO  ,VOL0 ,THK0,THK02,
     3                 A1    ,ALDT    ,ALPE ,NGL  ,ISMSTR,
     4                 IOFC  ,NNE     ,AREA ,G    ,SHF   ,
     5                 MSC   ,DMELC   ,JSMS ,PTG  ,IGTYP ,
     6                 IGMAT ,A11R    ,G_DT ,DTEL)
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
#include      "com06_c.inc"
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
C     REAL ou REAL*8
      my_real OFF(*),STI(*),STIR(*),OFFG(*),SSP(*),AMU(*),
     .   ALDT(*), ALPE(*), A1(*), THK0(*),THK02(*),
     .   VOL0(*), VISCMX(*), RHO(*),DT2T, AREA(*), G(*), SHF(*),
     .   MSC(*), DMELC(*), PTG(3,*),A11R(*),DTEL(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDXOF(MVSIZ),
     .        I, J, II, NINDX,IDT,ITYEL
      my_real DT(MVSIZ),O12TH,FAC,MAS,DIVM,MMIN  ,IZ   
c=======================================================================
      DO I=JFT,JLT
        VISCMX(I) = MAX(VISCMX(I), AMU(I))
        VISCMX(I) = SQRT(UN + VISCMX(I)*VISCMX(I)) - VISCMX(I)
        ALDT(I)   = ALDT(I) * VISCMX(I) / SQRT(ALPE(I))
      ENDDO
c---------------------------------------------------
C
      ITYEL = 3
      IF (NNE.EQ.3) ITYEL = 7
C----- isub will add here-----
      IF(IDTMINS == 2)THEN
        DO I=JFT,JLT
         IF (OFF(I).EQ.ZERO) THEN
          STI(I) = ZERO
          STIR(I) = ZERO          
         ELSE
          STI(I) = UNDEMI*VOL0(I) * A1(I) / ALDT(I)**2
       IF(IGTYP == 52 .OR. 
     .   ((IGTYP == 11 .OR. IGTYP == 17 .OR. IGTYP == 51)
     .                                   .AND. IGMAT > 0 )) THEN
            FAC = UNDEMI*VOL0(I)/ ALDT(I)**2
            STIR(I) =(FAC * A11R(I)*THK02(I) + 
     .                 FAC * A1(I) *AREA(I)) *UNDOUZE
          ELSE
            STIR(I) = STI(I) * (THK02(I)+AREA(I)) *UNDOUZE
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
     .  		(DTMINS/DTFACS)**2 * STI(I) - DEUX*MSC(I))
            DT(I)  = DTFACS*
     .  	     SQRT((DEUX*MSC(I)+DMELC(I))/MAX(EM20,STI(I)))
            IF(DT(I).LT.DT2T)THEN
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
     .  		(DTMINS/DTFACS)**2 * STI(I) - DEUX*MMIN)
            DT(I)  = DTFACS*
     .  	     SQRT((DEUX*MMIN+DMELC(I))/MAX(EM20,STI(I)))
            IF(DT(I).LT.DT2T)THEN
              DT2T    = DT(I)
              NELTST  = NGL(I)
              ITYPTST = ITYEL
            END  IF
           END DO
         END IF
        ENDIF
      ELSEIF(NODADT.NE.0)THEN
          IF(IGTYP == 52 .OR. 
     .        ((IGTYP == 11 .OR. IGTYP == 17 .OR. IGTYP == 51)
     .                                   .AND. IGMAT > 0 ))THEN
            DO I=JFT,JLT
                IF (OFF(I).EQ.ZERO) THEN
                   STI(I) = ZERO
                   STIR(I) = ZERO          
                ELSE
                   STI(I) = UNDEMI*VOL0(I) * A1(I) / ALDT(I)**2
                   FAC =  UNDEMI*VOL0(I)/ ALDT(I)**2
                   STIR(I) = FAC*A11R(I)*THK02(I)*UNDOUZE +
     .                       FAC *A1(I)*AREA(I)*UNDOUZE
                  ENDIF
             ENDDO
          ELSE
             DO I=JFT,JLT
                IF (OFF(I).EQ.ZERO) THEN
                   STI(I) = ZERO
                   STIR(I) = ZERO          
                ELSE
                   STI(I) = UNDEMI*VOL0(I) * A1(I) / ALDT(I)**2
                   STIR(I) = STI(I) * (THK02(I)+AREA(I)) *UNDOUZE
                ENDIF
             ENDDO
          ENDIF 
      ENDIF
C
      DO I=JFT,JLT
        DT(I)=DTFAC1(ITYEL)*ALDT(I)/SSP(I)   
      ENDDO
      IF(G_DT.GT.0)THEN
        DO I=JFT,JLT
          DTEL(I)=ALDT(I)/SSP(I)     ! DT=ALDT(I)/SSP(I) & DT=DT*DTFAC1 does not lead to same digits, so operation is made again here.
        ENDDO
      ENDIF
      IF((NODADT/=0.OR.IDTMINS==2).AND.IDTMIN(ITYEL)==0)RETURN
C
      IF(IDTMIN(ITYEL).GE.1)THEN
        NINDX=IOFC
        DO I=JFT,JLT
         IF(DT(I).LE.DTMIN1(ITYEL).AND.
     .     OFF(I).GE.UN.AND.OFFG(I).NE.DEUX.AND.OFFG(I).GE.ZERO) THEN
          NINDX=NINDX+1
          INDXOF(NINDX)=I
         ENDIF
        ENDDO
      ENDIF
C
      IF(IDTMIN(ITYEL).EQ.1)THEN

        IF(NINDX.GT.IOFC) MSTOP = 2

        DO 100 J=IOFC+1,NINDX
          I = INDXOF(J)
#include "lockon.inc"
          WRITE(IOUT,1000)  NNE,NGL(I)
          WRITE(ISTDO,1000) NNE,NGL(I)
#include "lockoff.inc"
  100   CONTINUE
      ELSEIF(IDTMIN(ITYEL).EQ.2)THEN
        IF(NINDX.GT.IOFC) IDEL7NOK = 1         
        DO 125 J=IOFC+1,NINDX
         I = INDXOF(J)
         OFF(I)=0.
#include "lockon.inc"
         WRITE(IOUT,1200)  NNE,NGL(I)
         WRITE(ISTDO,1300) NNE,NGL(I),TT
#include "lockoff.inc"
  125   CONTINUE
        IOFC = NINDX
      ELSEIF(IDTMIN(ITYEL).EQ.3.AND.ISMSTR.EQ.2)THEN
        DO 140 J=IOFC+1,NINDX
         I = INDXOF(J)
         OFFG(I)=2.
#include "lockon.inc"
         WRITE(IOUT,1400)  NNE,NGL(I)
         WRITE(ISTDO,1400) NNE,NGL(I)
#include "lockoff.inc"
  140   CONTINUE
        NINDX=IOFC
      ELSEIF(IDTMIN(ITYEL).EQ.5)THEN
        IF(NINDX.GT.IOFC) MSTOP = 2
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
      IF(NODADT.NE.0.OR.(IDTMINS==2.AND.JSMS/=0))RETURN
C
C- VECTOR
      IDT=0
      DO I=JFT,JLT
       IF(OFFG(I).GT.ZERO.AND.OFF(I).NE.ZERO.AND.DT(I).LT.DT2T) IDT=1
      ENDDO
C- NON VECTOR
      IF(IDT.EQ.1)THEN
       DO I=JFT,JLT
       IF(OFFG(I).GT.ZERO.AND.OFF(I).NE.ZERO.AND.DT(I).LT.DT2T)THEN
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
        STI(I) = UNDEMI*VOL0(I) * A1(I)* OFF(I) / DIVM
        STIR(I)= ZERO
      ENDDO
C
      RETURN
      END


Chd|====================================================================
Chd|  DTTHERM                       source/elements/sh3n/coquedk/cndt3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|====================================================================
           SUBROUTINE DTTHERM(JFT  ,JLT  ,PM  ,T  ,RE  ,
     2                        RHO  ,RK   ,VOL ,LC ,MAT ,
     3                        DT_THERM   ,OFF,CONDE)
C============================================================================
C   I m p l i c i t   T y p e s
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT  ,JLT 
      INTEGER MAT(*)
C
      my_real
     .   PM(NPROPM,*),  T(*), LC(*),DT_THERM,OFF(*),CONDE(*),VOL(*),RHO(*),
     .   RK(*),RE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, MT
      my_real
     .  SPH, AK1, BK1, AK2, BK2, TLI, AKK, XMU, TMU,
     .   ATU, DT, LC2,RPR
C=======================================================================
C
c       DT_THERM = EP20
       MT  = MAT(JFT)
       SPH = PM(69,MT)
       AK1 = PM(75,MT)
       BK1 = PM(76,MT)
       AK2 = PM(77,MT)
       BK2 = PM(78,MT)
       TLI = PM(80,MT)
       DO I=JFT,JLT
          IF(T(I).LT.TLI)THEN
            AKK=AK1+BK1*T(I)
          ELSE
            AKK=AK2+BK2*T(I)
          ENDIF
          IF(JTUR.NE.0)THEN
            XMU = RHO(I)*PM(24,MT)
            TMU = PM(81,MT)
            RPR = PM(95,MT)
            ATU=RPR*TMU*RK(I)*RK(I)/(MAX(EM15,RE(I)*VOL(I))*XMU)
            AKK=AKK*(1.+ATU)
          ENDIF
          LC2 = LC(I)*LC(I)
          DT = UNDEMI*LC2*SPH/MAX(AKK,EM20)
          IF(DT.LT.DT_THERM)  DT_THERM = DT
          CONDE(I) = QUATRE*VOL(I)*AKK/LC2  
          CONDE(I) = CONDE(I)*OFF(I)
       ENDDO

C----6---------------------------------------------------------------7---------8
      RETURN
      END
