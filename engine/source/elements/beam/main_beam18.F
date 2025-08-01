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
!||    main_beam18    ../engine/source/elements/beam/main_beam18.F
!||--- called by ------------------------------------------------------
!||    pforc3         ../engine/source/elements/beam/pforc3.F
!||--- calls      -----------------------------------------------------
!||    fail_beam18    ../engine/source/elements/beam/fail_beam18.F
!||    m2lawpi        ../engine/source/materials/mat/mat002/m2lawpi.F
!||    mulaw_ib       ../engine/source/elements/beam/mulaw_ib.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    mat_elem_mod   ../common_source/modules/mat_elem/mat_elem_mod.F90
!||====================================================================
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
     A                  NPROPG  ,TIME    ,DTIME   ,IDEL7NOK,ISIGI    ,
     B                  IMCONV  ,ISMSTR  ,MAT_PARAM ,
     C                  NTABLE ,TABLE    )

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD
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
      INTEGER ,INTENT(IN) :: IMCONV
      INTEGER ,INTENT(IN) :: ISMSTR
      INTEGER,INTENT(IN) :: NTABLE
      TYPE(TTABLE), DIMENSION(NTABLE), INTENT(INOUT)    :: TABLE      ! TABLE DATA    
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
      my_real ,DIMENSION(NEL)  ,INTENT(OUT) :: EPSD
      my_real ,DIMENSION(SBUFMAT) ,INTENT(IN) :: BUFMAT
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: TEMPEL
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
      TYPE (MATPARAM_STRUCT_) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IPT,IPID,IPLA,NPAR,IADBUF,NFUNC,ISRATE,IPY,IPZ,IPA,II(3)
      my_real :: DTINV,ASRATE,EPSDI,DMPM,DMPF,SPE,SPG,RHO,FACT,AREA,
     .           DMM,DMPFL,IXX,IYY,IZZ,G,E,DAMAGE_LOC,DFXX,DFXY,DFXZ,
     .           SIGNXX,SIGNXY,SIGNXZ,SIGOXX,SIGOXY,SIGOXZ
      my_real ,DIMENSION(NEL ):: DEGMB,DEGSH,DEGFX,YPT,ZPT,APT,OFF_OLD
      my_real ,DIMENSION(:,:) ,ALLOCATABLE :: DPLA,SIGY,VM,VM0
C=======================================================================
      IPY  = 200        
      IPZ  = 300        
      IPA  = 400        
      IPID = PID(1)
      AREA = GEO(1 ,IPID)
      DO I=1,3
        II(I) = NEL*(I-1)
      ENDDO
C-------------------
C     STRAIN RATE
C-------------------
      ISRATE = IPM(3,IMAT)                      
      ASRATE = MIN(ONE, PM(9,IMAT)*DTIME)
c     calculate total strain rate on neutral fiber position
      DO I = 1,NEL                                              
        EPSDI  = SQRT(EXX(I)**2 + HALF*(EXY(I)**2 + EXZ(I)**2))                       
        IF (ISRATE > 0) THEN                                    
          EPSD(I)= ASRATE*EPSDI + (ONE - ASRATE)*EPSD(I)
        ELSE       
          EPSD(I)= EPSDI
        ENDIF                                                   
      ENDDO
c
      ! Save old OFF value
      OFF_OLD(1:NEL) = OFF(1:NEL)
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
c
      ! Plasticity specific tables
      IPLA = ELBUF_STR%BUFLY(1)%L_PLA
      ALLOCATE (DPLA(NEL*IPLA,NPT*IPLA))
      ALLOCATE (SIGY(NEL*IPLA,NPT*IPLA)) 
      ALLOCATE (VM0(NEL*IPLA,NPT*IPLA))   
      ALLOCATE (VM(NEL*IPLA,NPT*IPLA))  
      IF (IPLA > 0) THEN
        DO J=1,NPT
          DO I = 1,NEL
            DPLA(I,J) = ELBUF_STR%BUFLY(1)%LBUF(1,1,J)%PLA(I)
            SIGY(I,J) = 1.E30 
            SIGOXX    = ELBUF_STR%BUFLY(1)%LBUF(1,1,J)%SIG(II(1)+I)
            SIGOXY    = ELBUF_STR%BUFLY(1)%LBUF(1,1,J)%SIG(II(2)+I)
            SIGOXZ    = ELBUF_STR%BUFLY(1)%LBUF(1,1,J)%SIG(II(3)+I) 
            VM0(I,J)  = SQRT(SIGOXX**2 + THREE*(SIGOXY**2 + SIGOXZ**2))
          ENDDO
        ENDDO
      ENDIF
c---------------------------
c     material models
c---------------------------
      IF (MTN == 2) THEN
        CALL M2LAWPI(ELBUF_STR,
     1               1      ,NEL    ,NPT    ,PM     ,GEO     ,
     2               EINT   ,OFF    ,IMAT    ,
     3               PID    ,EPSD   ,EXX    ,EXY    ,EXZ     ,
     4               KXX    ,KYY    ,KZZ    ,AL     ,NEL     ,
     5               IPM    ,ASRATE ,DTIME  ,NUMMAT ,SIGY)
c
      ELSE
c     
        CALL MULAW_IB(ELBUF_STR,
     1                NEL     ,NPT     ,MTN     ,IMAT    ,
     2                PID     ,NGL     ,IPM     ,
     3                GEO     ,OFF     ,
     4                EPSD    ,BUFMAT  ,NPF     ,TF      ,
     5                EXX     ,EXY     ,EXZ     ,KXX     ,
     6                KYY     ,KZZ     ,JTHE    ,TEMPEL  ,SIGY  )
      ENDIF
!
c---------------------------
c     Computation of Plastic Work
c---------------------------
      IF (IPLA > 0) THEN
        DO J=1,NPT
          DO I = 1,NEL
            DPLA(I,J) = ELBUF_STR%BUFLY(1)%LBUF(1,1,J)%PLA(I) - DPLA(I,J)
            SIGNXX = ELBUF_STR%BUFLY(1)%LBUF(1,1,J)%SIG(II(1)+I)
            SIGNXY = ELBUF_STR%BUFLY(1)%LBUF(1,1,J)%SIG(II(2)+I)
            SIGNXZ = ELBUF_STR%BUFLY(1)%LBUF(1,1,J)%SIG(II(3)+I)
            VM(I,J) = SQRT(SIGNXX**2 + THREE*(SIGNXY**2 + SIGNXZ**2))
            IF (ELBUF_STR%GBUF%G_WPLA > 0) ELBUF_STR%GBUF%WPLA(I) = ELBUF_STR%GBUF%WPLA(I) + 
     .             HALF*(VM(I,J)+VM0(I,J))*DPLA(I,J)*AL(I)*AREA/NPT
          ENDDO
        ENDDO
      ENDIF  
c---------------------------
c     failure models
c---------------------------
      IF (IFAIL > 0) THEN
c
        CALL FAIL_BEAM18(
     .              ELBUF_STR,MAT_PARAM%FAIL(1),NUMMAT  ,NUMGEO  ,
     .              NPROPM   ,NPROPG  ,SNPC    ,STF     ,
     .              NEL      ,NPT     ,IMAT    ,IPID    ,JTHE    ,
     .              TEMPEL   ,NGL     ,PM      ,GEO     ,
     .              OFF      ,EPSD    ,NPF     ,TF      ,
     .              DPLA     ,EINT    ,TIME    ,IOUT    ,ISTDO   ,
     .              AL       ,ISMSTR  ,EXX     ,EXY     ,EXZ     ,
     .              KXX      ,KYY     ,KZZ     ,DTIME   ,
     .              NTABLE   ,TABLE   ,SIGY    )
       END IF
C------------------------------------
c     resultant force and moment
c------------------------------------
      IF (ELBUF_STR%BUFLY(1)%L_DMGSCL > 0 ) THEN
        DO IPT = 1,NPT        
          DO I=1,NEL
            YPT(I) = GEO(IPY+IPT,PID(I))              
            ZPT(I) = GEO(IPZ+IPT,PID(I))           
            APT(I) = GEO(IPA+IPT,PID(I))    
            DAMAGE_LOC = ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)%DMGSCL(I) 
            DFXX       = APT(I)*ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)%SIG(II(1)+I)*DAMAGE_LOC 
            DFXY       = APT(I)*ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)%SIG(II(2)+I)*DAMAGE_LOC 
            DFXZ       = APT(I)*ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)%SIG(II(3)+I)*DAMAGE_LOC 
            FOR(I,1) = FOR(I,1) + DFXX
            FOR(I,2) = FOR(I,2) + DFXY
            FOR(I,3) = FOR(I,3) + DFXZ
            MOM(I,1) = MOM(I,1) + DFXY*ZPT(I) - DFXZ*YPT(I)
            MOM(I,2) = MOM(I,2) + DFXX*ZPT(I)
            MOM(I,3) = MOM(I,3) - DFXX*YPT(I)
          ENDDO
         ENDDO
      ELSE     
C-----------------------
C      FORCES ET MOMENTS
C-----------------------
        DO IPT = 1,NPT        
         DO I=1,NEL
          YPT(I) = GEO(IPY+IPT,PID(I))              
          ZPT(I) = GEO(IPZ+IPT,PID(I))           
          APT(I) = GEO(IPA+IPT,PID(I))    
          DFXX = APT(I)*ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)%SIG(II(1)+I)
          DFXY = APT(I)*ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)%SIG(II(2)+I)
          DFXZ = APT(I)*ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)%SIG(II(3)+I)
          FOR(I,1) = FOR(I,1) + DFXX
          FOR(I,2) = FOR(I,2) + DFXY
          FOR(I,3) = FOR(I,3) + DFXZ
          MOM(I,1) = MOM(I,1) + DFXY*ZPT(I) - DFXZ*YPT(I)
          MOM(I,2) = MOM(I,2) + DFXX*ZPT(I)
          MOM(I,3) = MOM(I,3) - DFXX*YPT(I)
         ENDDO          
        ENDDO
      ENDIF     
       

C-------------------------------------
      DO I = 1,NEL
        FOR(I,1) = FOR(I,1)*OFF(I)
        FOR(I,2) = FOR(I,2)*OFF(I)
        FOR(I,3) = FOR(I,3)*OFF(I)
        MOM(I,1) = MOM(I,1)*OFF(I)
        MOM(I,2) = MOM(I,2)*OFF(I)
        MOM(I,3) = MOM(I,3)*OFF(I)
        F1(I) = FOR(I,1) 
        F2(I) = FOR(I,2) 
        F3(I) = FOR(I,3) 
        M1(I) = MOM(I,1) 
        M2(I) = MOM(I,2) 
        M3(I) = MOM(I,3) 
      ENDDO
c---------------------------
c     element damping is removed outside with a common call 
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
c--------------------------------------------------------
c     SHOOTING NODES ALGORITHM ACTIVATION
c--------------------------------------------------------
      DO I = 1,NEL
        IF ((OFF_OLD(I) > ZERO) .AND. (OFF(I) == ZERO)) THEN 
          IDEL7NOK = 1
        ENDIF
      ENDDO      
c
      DEALLOCATE (DPLA)
C------------------------------------------
 1000 FORMAT(1X,'-- RUPTURE OF BEAM ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF BEAM ELEMENT :',I10,' AT TIME :',G11.4)
C------------------------------------------
      RETURN
      END SUBROUTINE MAIN_BEAM18
