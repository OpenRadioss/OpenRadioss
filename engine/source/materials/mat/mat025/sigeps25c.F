Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SIGEPS25C                     source/materials/mat/mat025/sigeps25c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        M25CRAK                       source/materials/mat/mat025/m25crak.F
Chd|        PRONY25C                      source/materials/mat/mat025/prony25c.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE SIGEPS25C(MAT_PARAM,
     1                    NEL      ,PM       ,OFF     ,GSTR    ,
     2                    DIR      ,THLY    ,TIME     ,TIMESTEP,SHF     ,
     3                    NGL      ,THK0    ,EXX      ,OFF_OLD ,
     4                    EYY      ,EXY     ,EXZ      ,EYZ     ,KXX     ,
     5                    KYY      ,KXY     ,ZZ       ,EPSPL   ,RHO0    ,
     6                    SOUNDSP  ,UPARAM  ,OFFL     ,
     7                    SIGY     ,ZCFAC   ,NPTT     ,ILAY    ,
     8                    NFIS1    ,NFIS2   ,NFIS3    ,WPLAR   ,
     9                    NPTTOT   ,IGTYP   ,SIGV     ,SIGPLY  ,
     A                    SIGOXX   ,SIGOYY  ,SIGOXY   ,SIGOYZ  ,SIGOZX  ,
     B                    SIGNXX   ,SIGNYY  ,SIGNXY   ,SIGNYZ  ,SIGNZX  ,
     C                    SIGVXX   ,SIGVYY  ,SIGVXY   ,SIGVYZ  ,SIGVZX  ,
     D                    ISRATE   ,UVARV   ,ISHPLYXFEM,IPT    ,SEQ_OUTP,
     E                    PLY_EXX  ,PLY_EYY ,PLY_EXY  ,PLY_EXZ ,PLY_EYZ ,
     F                    PLY_F    ,PLA     ,CRAK     ,IERR    ,
     G                    IOFF_DUCT,IFAILURE,PLY_ID   ,IPG     ,TSAIWU  ,
     H                    IMCONV   ,IOUT    ,DMG      ,L_DMG   ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
      use mat25_tsaiwu_c_mod
      use mat25_crasurv_c_mod
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,ISRATE,PLY_ID,
     .   IPT,NPTT,NPTTOT,ILAY,IGTYP,IFAILURE,ISHPLYXFEM,IPG
      INTEGER ,INTENT(IN) :: IMCONV
      INTEGER ,INTENT(IN) :: IOUT,L_DMG
      INTEGER NGL(MVSIZ),IERR(NEL),IOFF_DUCT(MVSIZ)
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: NFIS1,NFIS2,NFIS3
      my_real ,INTENT(IN) :: TIME
      my_real ,INTENT(IN) :: TIMESTEP
      my_real ,DIMENSION(NEL)   ,INTENT(INOUT) :: WPLAR
      my_real :: 
     .   PM(NPROPM),OFF(*),OFF_OLD(*),GSTR(NEL,8),DIR(*),
     .   THLY(*),SHF(*),THK0(MVSIZ),EXX(MVSIZ),EYY(MVSIZ),
     .   EXY(MVSIZ),EXZ(MVSIZ),EYZ(MVSIZ),KXX(MVSIZ),KYY(MVSIZ),
     .   KXY(MVSIZ),ZZ(*),EPSPL(*),RHO0(*),SOUNDSP(*),UPARAM(*),
     .   SIGY(*),ZCFAC(MVSIZ,2),SIGNXX(MVSIZ),
     .   SIGNYY(MVSIZ),SIGNXY(MVSIZ),SIGNYZ(MVSIZ),SIGNZX(MVSIZ),
     .   SIGVXX(MVSIZ),SIGVYY(MVSIZ),SIGVXY(MVSIZ),SIGVYZ(MVSIZ),
     .   SIGVZX(MVSIZ),UVARV(NEL,*),PLY_F(MVSIZ,5,*),
     .   PLY_EXX(MVSIZ,*),PLY_EYY(MVSIZ,*),PLY_EXY(MVSIZ,*), 
     .   PLY_EXZ(MVSIZ,*),PLY_EYZ(MVSIZ,*),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),
     .   SIGOZX(NEL),PLA(NEL),CRAK(NEL,2),SEQ_OUTP(NEL),
     .   OFFL(NEL),SIGV(NEL,5),SIGPLY(NEL,3),TSAIWU(NEL)
      TYPE(MATPARAM_STRUCT_) ,INTENT(IN) :: MAT_PARAM
      my_real, DIMENSION(NEL,L_DMG), INTENT(INOUT) :: DMG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IFLAG,IOFF,JOFF,FAILNPT,NPRONY
      my_real :: ZT,RATIO,KV,DTINV,EPST1,EPST2,EPSM1,EPSM2,DMAX
      my_real ::
     .   EPS(MVSIZ,5),STRN1(MVSIZ),STRN2(MVSIZ),STRN3(MVSIZ),
     .   STRP1(MVSIZ),STRP2(MVSIZ),SIGE(MVSIZ,5),
     .   EPSPXX(MVSIZ),ETSE(MVSIZ),YLD(MVSIZ),
     .   EPSPYY(MVSIZ),EPSPXY(MVSIZ),EPSPYZ(MVSIZ),EPSPZX(MVSIZ),
     .   SIG(NEL,5),SIGPE(MVSIZ,5)
      my_real,DIMENSION(:), ALLOCATABLE :: GV,BETA
C=======================================================================
!     Formulation flag
      IFLAG = MAT_PARAM%IPARAM(1)    ! IFLAG=0 : Tsai-Wu  , IFLAG=1 : Crasurv
!
      IOFF  = MAT_PARAM%IPARAM(2)

      EPST1  = mat_param%uparam(12)   !  PM(60)    
      EPST2  = mat_param%uparam(13)   !  PM(61)    
      EPSM1  = mat_param%uparam(14)   !  PM(62)    
      EPSM2  = mat_param%uparam(15)   !  PM(63)    
      DMAX   = mat_param%uparam(18)   !  PM(64)    
!
      RATIO  = mat_param%uparam(19)   !  PM(188)
C-----------------------------------------------------------
C     Visco elastic model 
C-----------------------------------------------------------
      IF (MAT_PARAM%IVISC == 1) THEN   ! PRONY model
        NPRONY = MAT_PARAM%VISC%IPARAM(1)
        KV     = MAT_PARAM%VISC%UPARAM(1)
        ALLOCATE(GV(NPRONY),BETA(NPRONY))
        DO I=1,NPRONY
          GV(I)   = MAT_PARAM%VISC%UPARAM(1 + I)         
          BETA(I) = MAT_PARAM%VISC%UPARAM(1 + NPRONY + I)
        ENDDO 
      ELSE
        ALLOCATE(GV(0),BETA(0))
      ENDIF      
C-----------------------------------------------------------
      IF (ISHPLYXFEM == 0 .OR. IPLYXFEM == 2) THEN
        DO I=1,NEL
          ZT=ZZ(I)*THK0(I)
          EPS(I,1)=EXX(I)+ZT*KXX(I)
          EPS(I,2)=EYY(I)+ZT*KYY(I)
          EPS(I,3)=EXY(I)+ZT*KXY(I)
          EPS(I,4)=EYZ(I)
          EPS(I,5)=EXZ(I)
          STRN1(I)= GSTR(I,1)+ZT*GSTR(I,6)
          STRN2(I)= GSTR(I,2)+ZT*GSTR(I,7)
          STRN3(I)=(GSTR(I,3)+ZT*GSTR(I,8))*HALF
        ENDDO
      ELSE 
        DO I=1,NEL
          ZT=ZZ(I)*THK0(I)
          EPS(I,1)=EXX(I) + ZT*KXX(I) + PLY_EXX(I,IPT)
          EPS(I,2)=EYY(I) + ZT*KYY(I) + PLY_EYY(I,IPT)
          EPS(I,3)=EXY(I) + ZT*KXY(I) + PLY_EXY(I,IPT)
          EPS(I,4)=EYZ(I) 
          EPS(I,5)=EXZ(I) 
          STRN1(I)= GSTR(I,1)+ ZT*GSTR(I,6)
          STRN2(I)= GSTR(I,2)+ ZT*GSTR(I,7)
          STRN3(I)=(GSTR(I,3)+ ZT*GSTR(I,8))*HALF  
        ENDDO
      ENDIF
C
      IF (MAT_PARAM%IVISC == 1) THEN 
        DTINV = TIMESTEP / MAX(TIMESTEP**2,EM20)
        DO I=1,NEL
          EPSPXX(I) = EPS(I,1)*DTINV
          EPSPYY(I) = EPS(I,2)*DTINV
          EPSPXY(I) = EPS(I,3)*DTINV
          EPSPYZ(I) = EPS(I,4)*DTINV
          EPSPZX(I) = EPS(I,5)*DTINV 
        ENDDO
      ENDIF 
C-----------------------
      DO I=1,NEL
        SIG(I,1)=SIGOXX(I)
        SIG(I,2)=SIGOYY(I)
        SIG(I,3)=SIGOXY(I)
        SIG(I,4)=SIGOYZ(I)
        SIG(I,5)=SIGOZX(I)
      ENDDO
!-----------------------------------------------------------------------
      IF (IFLAG == 0) THEN    ! Tsai-Wu
        CALL MAT25_TSAIWU_C(MAT_PARAM,
     1                      NEL      ,OFF     ,SIG     ,
     2                      PLA      ,DIR     ,CRAK    ,
     3                      NFIS1    ,NFIS2   ,NFIS3   ,ILAY    ,SHF   ,
     4                      NGL      ,EPS     ,IGTYP   ,WPLAR   ,STRN1 ,
     5                      STRN2    ,STRN3   ,STRP1   ,STRP2   ,SIGE  ,
     6                      EPSPL    ,ISRATE  ,OFFL    ,YLD     ,ETSE  ,
     7                      ISHPLYXFEM,PLY_EXX(1,IPT),PLY_EYY(1,IPT),
     8                      PLY_EXY(1,IPT),SIGPLY,SIGPE,PLY_ID ,
     9                      SIGNXX   ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX,
     A                      IPG      ,TSAIWU  ,IPLYXFEM,TIME    ,TIMESTEP,
     B                      IMCONV   ,MVSIZ   ,IOUT    ,DMG     ,L_DMG )
      
      ELSE    ! Crasurv 
      
        CALL MAT25_CRASURV_C(MAT_PARAM,
     1                       NEL      ,OFF     ,SIG     ,
     2                       PLA      ,DIR     ,CRAK    ,
     3                       NFIS1    ,NFIS2   ,NFIS3   ,ILAY    ,SHF   ,
     4                       NGL      ,EPS     ,WPLAR   ,STRN1 ,
     5                       STRN2    ,STRN3   ,STRP1   ,STRP2   ,SIGE  ,
     6                       EPSPL    ,ISRATE  ,OFFL    ,YLD     ,ETSE  ,
     7                       IERR     ,ISHPLYXFEM,PLY_EXX(1,IPT),PLY_EYY(1,IPT),
     8                       PLY_EXY(1,IPT),SIGPLY,SIGPE,PLY_ID ,
     9                       SIGNXX   ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX,
     A                       IPG      ,TSAIWU  ,IPLYXFEM,TIME    ,TIMESTEP,
     B                       IMCONV   ,MVSIZ   ,IOUT    ,DMG     ,L_DMG )

      END IF
!-----------------------------------------------------------------------
        IF (IPLYXFEM == 1) THEN
#include "vectorize.inc" 
          DO I=1,NEL
            PLY_F(I,1,IPT) = THLY(I)*SIGE(I,1)
            PLY_F(I,2,IPT) = THLY(I)*SIGE(I,2)
            PLY_F(I,3,IPT) = THLY(I)*SIGE(I,3)
            PLY_F(I,4,IPT) = THLY(I)*SIGE(I,4)
            PLY_F(I,5,IPT) = THLY(I)*SIGE(I,5)  
          ENDDO
        ELSEIF (IPLYXFEM == 2) THEN
#include "vectorize.inc" 
          DO I=1,NEL
            PLY_F(I,1,IPT) = THLY(I)*(SIGE(I,1)+SIGPE(I,1))
            PLY_F(I,2,IPT) = THLY(I)*(SIGE(I,2)+SIGPE(I,2))
            PLY_F(I,3,IPT) = THLY(I)*(SIGE(I,3)+SIGPE(I,3))
            PLY_F(I,4,IPT) = THLY(I)*SIGE(I,4)
            PLY_F(I,5,IPT) = THLY(I)*SIGE(I,5)  
          ENDDO
        ENDIF ! IF (IPLYXFEM == 1)
C-----------------------------------------------------------
C     Visco elastic model 
C-----------------------------------------------------------
      IF (MAT_PARAM%IVISC == 1 ) THEN 
        CALL PRONY25C(NEL     ,NPRONY  ,BETA     ,KV      ,
     1                GV      ,TIMESTEP,RHO0     ,OFF     ,DIR   ,   
     2                EPSPXX  ,EPSPYY  ,EPSPXY   ,EPSPYZ  ,EPSPZX,
     3                SIGVXX  ,SIGVYY  ,SIGVXY   ,SIGVYZ  ,SIGVZX,
     4                SIGV    ,SOUNDSP ,UVARV    ,IGTYP   )
C 
        IF (ISHPLYXFEM > 0) THEN
#include "vectorize.inc" 
          DO I=1,NEL
            PLY_F(I,1,IPT) = PLY_F(I,1,IPT) + THLY(I)*SIGVXX(I)
            PLY_F(I,2,IPT) = PLY_F(I,2,IPT) + THLY(I)*SIGVYY(I)  
            PLY_F(I,3,IPT) = PLY_F(I,3,IPT) + THLY(I)*SIGVXY(I)
            PLY_F(I,4,IPT) = PLY_F(I,4,IPT) + THLY(I)*SIGVYZ(I)
            PLY_F(I,5,IPT) = PLY_F(I,5,IPT) + THLY(I)*SIGVZX(I) 
          ENDDO
        ENDIF  ! IF (ISHPLYXFEM > 0)
      ENDIF    
C-----------------------
C     For QEPH
C-----------------------
      SIGY(1:NEL) = SIGY(1:NEL) + YLD(1:NEL)/NPTTOT  ! NPTT = MAX(NPTT) for IGTYP=51
      DO I=1,NEL
        ZCFAC(I,1) = ZCFAC(I,1) + ETSE(I) / NPTTOT
        ZCFAC(I,2) = MIN(ETSE(I),ZCFAC(I,2))
        IF (OFFL(I)==ZERO) ZCFAC(I,2)=-ABS(ZCFAC(I,2))
      ENDDO
C-----------------------
C     TENSILE RUPTURE
C-----------------------
      CALL M25CRAK(NEL    ,OFF    ,DMG    ,L_DMG  ,DIR    ,ILAY   ,
     .             THLY   ,NGL    ,STRP1  ,STRP2  ,PLY_ID ,IGTYP  ,
     .             IPG    ,EPST1  ,EPST2  ,EPSM1  ,EPSM2  ,DMAX   )
C-----------------------
      DEALLOCATE(GV,BETA)
C----  -------------------
      DO I=1,NEL
        IF (OFF(I) == ZERO .AND. NPTT > 1) OFFL(I) = ZERO
      ENDDO
C---------------------------
C     GLOBAL FAILURE INDEX
C---------------------------
      IF (IFLAG == 0) THEN    ! Tsai-Wu
        DO I=1,NEL
          DMG(I,1) = MAX(DMG(I,2),DMG(I,3),DMG(I,4))
        ENDDO
      ELSE                    ! Crasurv 
        DO I=1,NEL
          DMG(I,1) = MAX(DMG(I,2),DMG(I,3),DMG(I,4))
          IF (ABS(DMG(I,5)) >= ONE) THEN 
            DMG(I,1) = MAX(ABS(DMG(I,5))-ONE,DMG(I,1)) 
          ENDIF
          IF (ABS(DMG(I,6)) >= ONE) THEN
            DMG(I,1) = MAX(ABS(DMG(I,6))-ONE,DMG(I,1)) 
          ENDIF
          IF (DMG(I,7) >= ONE) THEN
            DMG(I,1) = MAX(DMG(I,7)-ONE,DMG(I,1)) 
          ENDIF
        ENDDO
      ENDIF 
C----------------------------
C     Failure test ---special treatment inside law25 ---
C----------------------------
      DO I=1,NEL
        IF (OFF(I) == OFF_OLD(I) .and. OFF(I) > ZERO) THEN
          IF (OFF(I) == ONE) THEN
            IF (RATIO < ZERO) THEN
              FAILNPT = NPTTOT - 1   ! NPTT =MAX(NPTT) for IGTYP=51
            ELSE
              FAILNPT = NPTTOT - NINT(NPTTOT*(ONE-RATIO))
c              FAILNPT = CEILING(NPTTOT*RATIO)
            END IF
            IF (IOFF < 0) IOFF=-(IOFF+1)
            JOFF=0
C       
            IF (IOFF == 0 .AND. WPLAR(I) >= ONE) JOFF=1
            IF (IOFF == 1 .AND. NINT(WPLAR(I)) >= FAILNPT) JOFF=1
            IF (IOFF == 2 .AND. NFIS1(I) >= FAILNPT) JOFF=1
            IF (IOFF == 3 .AND. NFIS2(I) >= FAILNPT) JOFF=1
            IF (IOFF == 4 .AND. NFIS1(I) >= FAILNPT 
     .                    .AND. NFIS2(I) == FAILNPT) JOFF=1
            IF (IOFF == 5 .AND. NFIS1(I) >= FAILNPT) JOFF=1
            IF (IOFF == 5 .AND. NFIS2(I) >= FAILNPT) JOFF=1
            IF (IOFF == 6 .AND. NFIS3(I) >= FAILNPT) JOFF=1
C                
            IF (JOFF == 1) THEN
              OFF(I) = FOUR_OVER_5 ! progressive rupture
              IF (IFAILURE == 0) IOFF_DUCT(I) = 1 ! flag for progressive rupture
            ENDIF
          ELSE IF (IFAILURE == 0 .and. OFF(I) < ONE ) THEN
            OFF(I) = OFF(I)*FOUR_OVER_5
          ENDIF
        ENDIF
      ENDDO
c-----------
      RETURN
      END
