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
Chd|  MAIN_BEAM3                    source/elements/beam/main_beam3.F
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|        FAIL_BEAM3                    source/elements/beam/fail_beam3.F
Chd|        M1LAWP                        source/materials/mat/mat001/m1lawp.F
Chd|        M2LAWP                        source/materials/mat/mat002/m2lawp.F
Chd|        PDAMP3                        source/elements/beam/pdamp3.F 
Chd|        SIGEPS44P                     source/materials/mat/mat044/sigeps44p.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MAIN_BEAM3(
     .       ELBUF_STR,NEL      ,ILAW     ,JTHE     ,IFAIL    ,
     .       IPM      ,PM       ,GEO      ,TEMPEL   ,OFF      ,
     .       MAT      ,PID      ,NGL      ,TIME     ,DTIME    ,
     .       AL       ,NPF      ,TF       ,EXX      ,EXY      ,
     .       EXZ      ,KXX      ,KYY      ,KZZ      ,F1       ,
     .       F2       ,F3       ,M1       ,M2       ,M3       ,
     .       BUFMAT   ,NPROPG   ,NPROPMI  ,NPROPM   ,NUMMAT   ,
     .       NUMGEO   ,SBUFMAT  ,SNPC     ,STF      ,IOUT     ,
     .       ISTDO    ,NUVAR    ,UVAR     ,EPSD     ,IMAT     ,
     .       FOR      ,MOM      ,EINT     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL
      INTEGER ,INTENT(IN) :: ILAW
      INTEGER ,INTENT(IN) :: IMAT
      INTEGER ,INTENT(IN) :: JTHE
      INTEGER ,INTENT(IN) :: IFAIL
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
      INTEGER ,INTENT(IN) :: NUVAR
      INTEGER ,DIMENSION(SNPC) ,INTENT(IN) :: NPF
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(IN) :: IPM
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: MAT
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: PID
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME
      my_real ,INTENT(IN) :: DTIME
      my_real ,INTENT(IN) :: PM(NPROPM,NUMMAT)
      my_real ,INTENT(IN) :: GEO(NPROPG,NUMGEO)
      my_real ,INTENT(IN) :: TF(STF)
      my_real ,INTENT(IN) :: BUFMAT(SBUFMAT)
      my_real ,DIMENSION(NEL)  ,INTENT(IN)    :: AL
      my_real ,DIMENSION(NEL)  ,INTENT(IN)    :: TEMPEL
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: EXX,EXY,EXZ
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: KXX,KYY,KZZ
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: OFF
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: F1,F2,F3
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: M1,M2,M3
      my_real ,DIMENSION(NEL,2),INTENT(INOUT) :: EINT
      my_real ,DIMENSION(NEL,3),INTENT(INOUT) :: FOR,MOM
      my_real ,DIMENSION(NEL)  ,INTENT(OUT)   :: EPSD
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
      TYPE (ELBUF_STRUCT_) ,INTENT(INOUT) :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IPID,IPLA,ISRATE,NUPARAM,NFUNC,IADBUF,IFUNC(100)
      my_real :: AREA,IYY,IZZ,IXX,ASRATE,EPSDI
      my_real ,DIMENSION(NEL) :: DPLA,SVM,PRESSURE
c=======================================================================
      IPID = PID(1)
c-------------------
c     STRAIN RATE
c-------------------
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
      DO I=1,NEL
        EXX(I) = EXX(I) * DTIME
        EXY(I) = EXY(I) * DTIME
        EXZ(I) = EXZ(I) * DTIME
        KXX(I) = KXX(I) * DTIME
        KYY(I) = KYY(I) * DTIME
        KZZ(I) = KZZ(I) * DTIME
      ENDDO
c
      IPLA = ELBUF_STR%GBUF%G_PLA
      IF (IFAIL > 0 .and. IPLA > 0) THEN
        DO I = 1,NEL
          DPLA(I) = ELBUF_STR%GBUF%PLA(I)
        ENDDO
      ENDIF
c---------------------------
c     material models
c---------------------------
      SELECT CASE(ILAW)
c
        CASE (1)
          CALL M1LAWP(
     .         PM     ,FOR       ,MOM       ,EINT       ,GEO,
     .         OFF    ,EXX       ,EXY       ,EXZ        ,KXX,
     .         KYY    ,KZZ       ,AL        ,F1         ,F2 ,
     .         F3     ,M1        ,M2        ,M3         ,NEL,
     .         MAT    ,PID       )
c
        CASE (2)  ! Johnson-Cook
          CALL M2LAWP(
     .         PM     ,FOR        ,MOM      ,EINT      ,GEO      ,        
     .         OFF    ,ELBUF_STR%GBUF%PLA   ,EXX       ,EXY      ,EXZ      ,
     .         KXX    ,KYY        ,KZZ      ,AL        ,F1       ,
     .         F2     ,F3         ,M1       ,M2        ,M3       ,
     .         NEL    ,MAT        ,PID      ,NGL       ,IPM      ,
     .         NUMMAT ,NUVAR      ,UVAR     )
c
        CASE (44)  ! Cowper-Symonds
          IADBUF  = IPM(7 ,IMAT)
          NUPARAM = IPM(9 ,IMAT)       
          NFUNC   = IPM(10,IMAT)
          DO I=1,NFUNC
            IFUNC(I) = IPM(10+I,IMAT)
          ENDDO
          CALL SIGEPS44P(
     .         NEL      ,NGL      ,MAT      ,PID      ,NUPARAM  ,BUFMAT(IADBUF)   ,
     .         GEO      ,OFF      ,ELBUF_STR%GBUF%PLA ,EINT     ,AL      ,
     .         EXX      ,EXY      ,EXZ      ,KXX      ,KYY      ,KZZ     ,
     .         F1       ,F2       ,F3       ,M1       ,M2       ,M3      ,
     .         FOR      ,MOM      ,PM       ,NUVAR    ,UVAR     ,NFUNC   ,
     .         IFUNC    ,TF       ,NPF      )
c
      END SELECT
c
c---------------------------
c     failure models
c---------------------------
      IF (IFAIL > 0) THEN
        IF (IPLA > 0) THEN
          DO I = 1,NEL
            DPLA(I) = ELBUF_STR%GBUF%PLA(I) - DPLA(I)
          ENDDO
        ENDIF
        AREA = GEO(1 ,IPID)
        IXX  = GEO(4 ,IPID)
        IYY  = GEO(2 ,IPID)
        IZZ  = GEO(18,IPID)
        DO I = 1,NEL
          SVM(I) = F1(I)*F1(I) + THREE * AREA *
     .             ( M1(I)*M1(I) / MAX(IXX,EM20)
     .             + M2(I)*M2(I) / MAX(IYY,EM20)
     .             + M3(I)*M3(I) / MAX(IZZ,EM20) )
          SVM(I) = SQRT(SVM(I)) / AREA
          PRESSURE(I) = THIRD * F1(I) / AREA
        ENDDO
c
        CALL FAIL_BEAM3(ELBUF_STR    ,NUMMAT  ,NPROPM  ,
     .              NPROPMI ,SBUFMAT ,SNPC    ,STF     ,
     .              NEL     ,IMAT    ,JTHE    ,DPLA    ,
     .              TEMPEL  ,NGL     ,PM      ,IPM     ,
     .              OFF     ,EPSD    ,BUFMAT  ,NPF     ,TF      ,
     .              TIME    ,IOUT    ,ISTDO   ,
     .              SVM     ,PRESSURE,EINT    ,AREA    ,AL      ,
     .              F1      ,F2      ,F3      ,M1      ,M2      ,
     .              M3 )
      END IF
C-------------------------------------
      DO I=1,NEL
        FOR(I,1)=F1(I)*OFF(I)
        FOR(I,2)=FOR(I,2)*OFF(I)
        FOR(I,3)=FOR(I,3)*OFF(I)
        MOM(I,1)=M1(I)*OFF(I)
        MOM(I,2)=M2(I)*OFF(I)
        MOM(I,3)=M3(I)*OFF(I)
      ENDDO
C
      DO I=1,NEL
        F1(I) =  FOR(I,1)
        F2(I) =  FOR(I,2) 
        F3(I) =  FOR(I,3)  
        M1(I) =  MOM(I,1) 
        M2(I) =  MOM(I,2) 
        M3(I) =  MOM(I,3) 
      ENDDO               
c---------------------------
c     element damping
c---------------------------
      CALL PDAMP3(PM      ,GEO     ,OFF     ,IMAT    ,IPID ,
     .            NEL     ,NGL     ,EXX     ,EXY     ,EXZ  ,
     .            KXX     ,KYY     ,KZZ     ,AL      ,F1   ,
     .            F2      ,F3      ,M1      ,M2      ,M3   )
C-----------------------------------------------
      RETURN
      END SUBROUTINE MAIN_BEAM3
