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
Chd|  MAIN_BEAM18                   source/elements/beam/main_beam18.F
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|        FAIL_BEAM18                   source/elements/beam/fail_beam18.F
Chd|        M2LAWPI                       source/materials/mat/mat002/m2lawpi.F
Chd|        MULAW_IB                      source/elements/beam/mulaw_ib.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MAIN_BEAM18(ELBUF_STR,
     1                  NEL     ,NPT     ,MTN     ,IMAT    ,
     2                  PID     ,NGL     ,PM      ,IPM     ,
     3                  GEO     ,OFF     ,FOR     ,MOM     ,
     4                  EINT    ,AL      ,EPSD    ,BUFMAT  ,NPF      ,
     5                  TF      ,EXX     ,EXY     ,EXZ     ,KXX      ,
     6                  KYY     ,KZZ     ,F1      ,F2      ,F3       ,
     7                  M1      ,M2      ,M3      ,JTHE    ,TEMPEL   ,
     8                  IFAIL   ,SBUFMAT ,SNPC    ,STF     ,NUMMAT   ,
     9                  NUMGEO  ,IOUT    ,ISTDO   ,NPROPMI ,NPROPM   ,
     A                  NPROPG  ,TIME    ,DTIME   ,IDEL7NOK,IMPL_S   ,
     B                  ISIGI   ,IDYNA   ,IMCONV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: IMAT
      INTEGER ,INTENT(IN) :: NEL,MTN,NPT,JTHE,IFAIL  
      INTEGER ,INTENT(IN) :: SBUFMAT
      INTEGER ,INTENT(IN) :: SNPC
      INTEGER ,INTENT(IN) :: STF
      INTEGER ,INTENT(IN) :: NUMMAT
      INTEGER ,INTENT(IN) :: NUMGEO
      INTEGER ,INTENT(IN) :: NPROPMI
      INTEGER ,INTENT(IN) :: NPROPM
      INTEGER ,INTENT(IN) :: NPROPG
      INTEGER ,INTENT(IN) :: IOUT
      INTEGER ,INTENT(IN) :: ISTDO
      INTEGER ,INTENT(IN) :: ISIGI
      INTEGER ,INTENT(IN) :: IMPL_S
      INTEGER ,INTENT(IN) :: IDYNA
      INTEGER ,INTENT(IN) :: IMCONV
      INTEGER ,DIMENSION(NEL)  ,INTENT(IN) :: PID
      INTEGER ,DIMENSION(NEL)  ,INTENT(IN) :: NGL
      INTEGER ,DIMENSION(SNPC) ,INTENT(IN) :: NPF
      INTEGER IPM(NPROPMI,NUMMAT)
      INTEGER ,INTENT(INOUT) :: IDEL7NOK
      my_real ,INTENT(IN) :: TIME
      my_real ,INTENT(IN) :: DTIME
      my_real ,INTENT(IN) :: PM(NPROPM,NUMMAT)
      my_real ,INTENT(IN) :: GEO(NPROPG,NUMGEO)
      my_real ,DIMENSION(NEL)  ,INTENT(IN)  :: AL
      my_real ,DIMENSION(NEL)  ,INTENT(IN)  :: TEMPEL
      my_real ,DIMENSION(NEL)  ,INTENT(OUT) :: EPSD
      my_real ,DIMENSION(SBUFMAT) ,INTENT(IN) :: BUFMAT
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: OFF
      my_real ,DIMENSION(NEL,2),INTENT(INOUT) :: EINT
      my_real ,DIMENSION(NEL,3),INTENT(INOUT) :: FOR,MOM
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: EXX,EXY,EXZ
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: KXX,KYY,KZZ
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: F1,F2,F3
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: M1,M2,M3
      my_real ,DIMENSION(STF)  ,INTENT(IN)    :: TF
C
      TYPE (ELBUF_STRUCT_) ,INTENT(INOUT) :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IPT,IPID,IPLA,NPAR,IADBUF,NFUNC,ISRATE,IPY,IPZ
      my_real :: DTINV,ASRATE,EPSDI,DMPM,DMPF,SPE,SPG,RHO,FACT,AREA,
     .           DMM,DMPFL,IXX,IYY,IZZ,G,E
      my_real ,DIMENSION(NEL ):: DEGMB,DEGSH,DEGFX
      my_real ,DIMENSION(:,:) ,ALLOCATABLE :: DPLA
C=======================================================================
      IPID = PID(1)
C-------------------
C     STRAIN RATE
C-------------------
      ISRATE = IPM(3,IMAT)                      
      ASRATE = MIN(ONE, PM(9,IMAT)*DTIME)
c
      DO I = 1,NEL                                              
        EPSDI  = HALF*(EXX(I)**2) + (HALF*EXY(I))**2 + (HALF*EXZ(I))**2
        EPSDI  = AL(I)*SQRT(THREE*EPSDI)/THREE_HALF                         
        IF (ISRATE > 0) THEN                                    
          EPSD(I)= ASRATE*EPSDI + (ONE - ASRATE)*EPSD(I)
        ELSE       
          EPSD(I)= EPSDI
        ENDIF                                                   
      ENDDO
c-------------------
c     STRAIN
c-------------------
      DO I = 1,NEL
        EXX(I) = EXX(I)*DTIME
        EXY(I) = EXY(I)*DTIME
        EXZ(I) = EXZ(I)*DTIME
        KXX(I) = KXX(I)*DTIME
        KYY(I) = KYY(I)*DTIME
        KZZ(I) = KZZ(I)*DTIME
      ENDDO
C
      DO I = 1,NEL
        DEGMB(I) = FOR(I,1)*EXX(I)
        DEGSH(I) = FOR(I,2)*EXY(I) + FOR(I,3)*EXZ(I)
        DEGFX(I) = MOM(I,1)*KXX(I) + MOM(I,2)*KYY(I) + MOM(I,3)*KZZ(I)
      ENDDO
C
      IF (ISIGI == 0 .OR. (ISIGI /= 0 .AND. TIME /= ZERO)) THEN
        DO I = 1,NEL
          FOR(I,1) = ZERO
          FOR(I,2) = ZERO
          FOR(I,3) = ZERO
          MOM(I,1) = ZERO
          MOM(I,2) = ZERO
          MOM(I,3) = ZERO
        ENDDO
      ENDIF
      IPLA = ELBUF_STR%BUFLY(1)%L_PLA
      ALLOCATE (DPLA(NEL*IPLA,NPT*IPLA))
      IF (IFAIL > 0 .and. IPLA > 0) THEN
        DO I = 1,NEL
          DO J=1,NPT
            DPLA(I,J) = ELBUF_STR%BUFLY(1)%LBUF(1,1,J)%PLA(I)
          ENDDO
        ENDDO
      ENDIF
c---------------------------
c     material models
c---------------------------
      IF (MTN == 2) THEN
        CALL M2LAWPI(ELBUF_STR,
     1               1      ,NEL    ,NPT    ,PM     ,GEO     ,
     2               FOR    ,MOM    ,EINT   ,OFF    ,IMAT    ,
     3               PID    ,EPSD   ,EXX    ,EXY    ,EXZ     ,
     4               KXX    ,KYY    ,KZZ    ,AL     ,NEL     ,
     5               IPM    ,ASRATE ,DTIME  ,NUMMAT )
c
      ELSE
c
        CALL MULAW_IB(ELBUF_STR,
     1                NEL     ,NPT     ,MTN     ,IMAT    ,
     2                PID     ,NGL     ,IPM     ,
     3                GEO     ,OFF     ,FOR     ,MOM     ,
     4                EPSD    ,BUFMAT  ,NPF     ,TF      ,
     5                EXX     ,EXY     ,EXZ     ,KXX     ,
     6                KYY     ,KZZ     ,JTHE    ,TEMPEL  )
      ENDIF
c---------------------------
c     failure models
c---------------------------
      IF (IFAIL > 0) THEN
        IF (IPLA > 0) THEN
          DO I = 1,NEL
            DO J=1,NPT
              DPLA(I,J) = ELBUF_STR%BUFLY(1)%LBUF(1,1,J)%PLA(I) - DPLA(I,J)
            ENDDO
          ENDDO
        END IF
c
        CALL FAIL_BEAM18(ELBUF_STR   ,NUMMAT  ,NUMGEO  ,NPROPM  ,
     .              NPROPMI ,NPROPG  ,SBUFMAT ,SNPC    ,STF     ,
     .              NEL     ,NPT     ,IMAT    ,IPID    ,JTHE    ,
     .              TEMPEL  ,NGL     ,PM      ,IPM     ,GEO     ,
     .              OFF     ,EPSD    ,BUFMAT  ,NPF     ,TF      ,
     .              DPLA    ,EINT    ,TIME    ,IOUT    ,ISTDO   )
      END IF
C-------------------------------------
      DO I = 1,NEL
        FOR(I,1) = FOR(I,1)*OFF(I)
        FOR(I,2) = FOR(I,2)*OFF(I)
        FOR(I,3) = FOR(I,3)*OFF(I)
        MOM(I,1) = MOM(I,1)*OFF(I)
        MOM(I,2) = MOM(I,2)*OFF(I)
        MOM(I,3) = MOM(I,3)*OFF(I)
      ENDDO
c---------------------------
c     element damping
c---------------------------
      DTINV = DTIME / MAX(DTIME**2,EM20)  
      IF (IMPL_S > 0 .AND. IDYNA == 0) DTINV = ZERO
C
      E    = PM(20,IMAT)                  
      G    = PM(22,IMAT)                  
      RHO  = PM(1 ,IMAT)
      IXX  = GEO(4,IPID)
      IYY  = GEO(2,IPID)
      IZZ  = GEO(18,IPID)
      AREA = GEO(1 ,IPID)
      DMPM = GEO(16,IPID)                                    
      DMPF = GEO(17,IPID) 
      SPE  = DTINV * SQRT(TWO*RHO*E)             
      SPG  = DTINV * SQRT(TWO*RHO*G)
c
      DO I = 1,NEL                                             
        DMM   = SPE  * AREA * AL(I)
        DMPFL = DMPF * AL(I)
        F1(I) = FOR(I,1) + DMPM  * EXX(I) * DMM   
        F2(I) = FOR(I,2) + DMPF  * EXY(I) * DMM    
        F3(I) = FOR(I,3) + DMPF  * EXZ(I) * DMM   
        M1(I) = MOM(I,1) + DMPFL * KXX(I) * SPG*IXX
        M2(I) = MOM(I,2) + DMPFL * KYY(I) * SPE*IYY
        M3(I) = MOM(I,3) + DMPFL * KZZ(I) * SPE*IZZ
      ENDDO                 
C---------------------------
C     Internal energy
C---------------------------
      DO I = 1,NEL
        DEGMB(I) = DEGMB(I) + FOR(I,1)*EXX(I)
        DEGSH(I) = DEGSH(I) + FOR(I,2)*EXY(I) + FOR(I,3)*EXZ(I)
        DEGFX(I) = DEGFX(I) 
     .           + MOM(I,1)*KXX(I) + MOM(I,2)*KYY(I) + MOM(I,3)*KZZ(I)
        FACT = HALF*OFF(I)*AL(I)
        EINT(I,1)  = EINT(I,1) + FACT*(DEGMB(I)+DEGSH(I))
        EINT(I,2)  = EINT(I,2) + FACT* DEGFX(I)
      ENDDO
c----------------------------
c     Check element erosion (IMCONV = 1 => convergence of implicit)
c----------------------------
      DO I = 1,NEL
        IF (OFF(I) == FOUR_OVER_5 .AND. IMCONV == 1) THEN 
          IDEL7NOK = 1            
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I)
          WRITE(ISTDO,1100) NGL(I),TIME
#include "lockoff.inc"
        ENDIF
      ENDDO
C      
      DO I = 1,NEL
        IF (OFF(I) < EM01)  OFF(I) = ZERO
        IF (OFF(I) < ONE )  OFF(I) = OFF(I)*FOUR_OVER_5
      ENDDO      
c
      DEALLOCATE (DPLA)
C------------------------------------------
 1000 FORMAT(1X,'-- RUPTURE OF BEAM ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF BEAM ELEMENT :',I10,' AT TIME :',G11.4)
C------------------------------------------
      RETURN
      END SUBROUTINE MAIN_BEAM18
