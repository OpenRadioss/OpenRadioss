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
Chd|  SIGEPS25C                     source/materials/mat/mat025/sigeps25c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        M25CPLRC                      source/materials/mat/mat025/m25cplrc.F
Chd|        M25CRAK                       source/materials/mat/mat025/m25crak.F
Chd|        PRONY25C                      source/materials/mat/mat025/prony25c.F
Chd|====================================================================
      SUBROUTINE SIGEPS25C(
     1                    JFT      ,JLT     ,PM       ,OFF     ,GSTR    ,
     2                    DIR      ,THLY    ,IMATLY   ,DT1C    ,SHF     ,
     3                    IPRONY   ,NGL     ,THK0     ,EXX     ,OFF_OLD ,
     4                    EYY      ,EXY     ,EXZ      ,EYZ     ,KXX     ,
     5                    KYY      ,KXY     ,ZZ       ,EPSPL   ,RHO0    ,
     6                    SOUNDSP  ,IPM     ,UPARAM   ,DT_INV  ,OFFL    ,
     7                    SIGY     ,ZCFAC   ,NPTT     ,ILAY    ,DAMCR   ,
     8                    NFIS1    ,NFIS2   ,NFIS3    ,DMAXT   ,WPLAR   ,
     9                    NPTTOT   ,IGTYP   ,NEL      ,SIGV    ,SIGPLY  ,
     A                    SIGOXX   ,SIGOYY  ,SIGOXY   ,SIGOYZ  ,SIGOZX  ,
     B                    SIGNXX   ,SIGNYY  ,SIGNXY   ,SIGNYZ  ,SIGNZX  ,
     C                    SIGVXX   ,SIGVYY  ,SIGVXY   ,SIGVYZ  ,SIGVZX  ,
     D                    ISRATE   ,UVARV   ,ISHPLYXFEM,IPT    ,SEQ_OUTP,
     E                    PLY_EXX  ,PLY_EYY ,PLY_EXY  ,PLY_EXZ ,PLY_EYZ ,
     F                    PLY_F    ,PLA     ,DAMT     ,CRAK    ,IERR    ,
     G                    IOFF_DUCT,IFAILURE,PLY_ID   ,IPG     ,TSAIWU  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "units_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com20_c.inc"
#include      "secur_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NEL,IMATLY,ISRATE,IPRONY,PLY_ID,
     .   IR,IS,IT,IPT,NPTT,NPTTOT,ILAY,IGTYP,IFAILURE,ISHPLYXFEM,IPG
      INTEGER NGL(MVSIZ),IERR(NEL),IPM(NPROPMI,*),
     .        NFIS1(MVSIZ),NFIS2(MVSIZ),NFIS3(MVSIZ),IOFF_DUCT(MVSIZ)
C     REAL
      my_real
     .   PM(NPROPM,*),OFF(*),OFF_OLD(*),GSTR(NEL,8),DIR(*),
     .   THLY(*),DT1C(*),SHF(*),THK0(MVSIZ),EXX(MVSIZ),EYY(MVSIZ),
     .   EXY(MVSIZ),EXZ(MVSIZ),EYZ(MVSIZ),KXX(MVSIZ),KYY(MVSIZ),
     .   KXY(MVSIZ),ZZ(*),EPSPL(*),RHO0(*),SOUNDSP(*),UPARAM(*),
     .   SIGY(*),ZCFAC(MVSIZ,2),DAMCR(MVSIZ,2),
     .   DMAXT(MVSIZ),WPLAR(MVSIZ),SIGNXX(MVSIZ),
     .   SIGNYY(MVSIZ),SIGNXY(MVSIZ),SIGNYZ(MVSIZ),SIGNZX(MVSIZ),
     .   SIGVXX(MVSIZ),SIGVYY(MVSIZ),SIGVXY(MVSIZ),SIGVYZ(MVSIZ),
     .   SIGVZX(MVSIZ),
     .   UVARV(NEL,*),PLY_F(MVSIZ,5,*),
     .   PLY_EXX(MVSIZ,*),PLY_EYY(MVSIZ,*),PLY_EXY(MVSIZ,*), 
     .   PLY_EXZ(MVSIZ,*),PLY_EYZ(MVSIZ,*),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),
     .   SIGOZX(NEL),PLA(NEL),DAMT(NEL,2),CRAK(NEL,2),SEQ_OUTP(NEL),
     .   OFFL(NEL),SIGV(NEL,5),DT_INV(MVSIZ),SIGPLY(NEL,3),TSAIWU(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,IADBUF,NPMAX,MX,IOFF,JOFF,FAILNPT,NPRONY
      INTEGER II(5)
C
      my_real ZT,RATIO
      my_real
     .   EPS(MVSIZ,5),STRN1(MVSIZ),STRN2(MVSIZ),STRN3(MVSIZ),
     .   STRP1(MVSIZ),STRP2(MVSIZ),SIGE(MVSIZ,5),
     .   DTINV,KV(MVSIZ),EPSPXX(MVSIZ),ETSE(MVSIZ),YLD(MVSIZ),
     .   EPSPYY(MVSIZ),EPSPXY(MVSIZ),EPSPYZ(MVSIZ),EPSPZX(MVSIZ),
     .   SIG(NEL,5),SIGPE(MVSIZ,5)
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: GV,BETA
C=======================================================================
C     PLASTICITE AUX POINTS D'INTEGRATION
C-----------------------------------------------------------
C
C     DEFORMATIONS INCREMENTALES
C
C  Check visco elastic model 
C             
      NPMAX = 0
      IF (IPRONY > 0) THEN 
        IADBUF = IPM(223,IMATLY)
        DO I=JFT,JLT    
          KV(I) = UPARAM(IADBUF)
          NPRONY = INT(UPARAM(IADBUF+1))
          NPMAX = MAX(NPRONY,NPMAX)
        ENDDO   
        ALLOCATE(GV(NPMAX),BETA(NPMAX))
        GV(1:NPMAX) = ZERO
        BETA(1:NPMAX) = ZERO
        DO JJ=1,NPRONY
          GV(JJ)   = UPARAM(IADBUF + 1 + JJ)
          BETA(JJ) = UPARAM(IADBUF + 1 + NPRONY + JJ)
        ENDDO 
      ELSE
        ALLOCATE(GV(0),BETA(0))
      ENDIF      
C
      IF (ISHPLYXFEM == 0 .OR. IPLYXFEM == 2) THEN
        DO I=JFT,JLT
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
        DO I=JFT,JLT
          ZT=ZZ(I)*THK0(I)
          EPS(I,1)=EXX(I) + ZT*KXX(I) + PLY_EXX(I,IPT)
          EPS(I,2)=EYY(I) + ZT*KYY(I) + PLY_EYY(I,IPT)
          EPS(I,3)=EXY(I) + ZT*KXY(I) + PLY_EXY(I,IPT)
          EPS(I,4)=EYZ(I) 
          EPS(I,5)=EXZ(I) 
!          EPS(4,I)=EYZ(I) + PLY_EYZ(I,J)
!          EPS(5,I)=EXZ(I) + PLY_EXZ(I,J) 
          STRN1(I)= GSTR(I,1)+ ZT*GSTR(I,6)
          STRN2(I)= GSTR(I,2)+ ZT*GSTR(I,7)
          STRN3(I)=(GSTR(I,3)+ ZT*GSTR(I,8))*HALF  
        ENDDO
      ENDIF
C
      IF (IPRONY > 0) THEN 
        DO I=JFT,JLT
          DTINV = DT_INV(I)
          EPSPXX(I) = EPS(I,1)*DTINV
          EPSPYY(I) = EPS(I,2)*DTINV
          EPSPXY(I) = EPS(I,3)*DTINV
          EPSPYZ(I) = EPS(I,4)*DTINV
          EPSPZX(I) = EPS(I,5)*DTINV 
        ENDDO
      ENDIF 
C-----------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C-----------------------
      DO I=JFT,JLT
        SIG(I,1)=SIGOXX(I)
        SIG(I,2)=SIGOYY(I)
        SIG(I,3)=SIGOXY(I)
        SIG(I,4)=SIGOYZ(I)
        SIG(I,5)=SIGOZX(I)
      ENDDO
!
      CALL M25CPLRC(JFT     ,JLT     ,PM      ,OFF     ,SIG   ,
     2              PLA     ,DIR     ,IMATLY  ,DAMT    ,CRAK  ,
     3              NFIS1   ,NFIS2   ,NFIS3   ,ILAY    ,SHF   ,
     4              NGL     ,EPS     ,IGTYP   ,WPLAR   ,STRN1 ,
     5              STRN2   ,STRN3   ,STRP1   ,STRP2   ,SIGE  ,
     6              EPSPL   ,ISRATE  ,OFFL    ,YLD     ,ETSE  ,
     7              IERR    ,SEQ_OUTP,ISHPLYXFEM,PLY_EXX(1,IPT),PLY_EYY(1,IPT),
     8              PLY_EXY(1,IPT),SIGPLY,SIGPE,PLY_ID ,NEL   ,
     9              SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX,
     A              IPG     ,TSAIWU  )
!----------------------------------
        IF (IPLYXFEM == 1) THEN
#include "vectorize.inc" 
          DO I=JFT,JLT
            PLY_F(I,1,IPT) = THLY(I)*SIGE(I,1)
            PLY_F(I,2,IPT) = THLY(I)*SIGE(I,2)
            PLY_F(I,3,IPT) = THLY(I)*SIGE(I,3)
            PLY_F(I,4,IPT) = THLY(I)*SIGE(I,4)
            PLY_F(I,5,IPT) = THLY(I)*SIGE(I,5)  
          ENDDO
        ELSEIF (IPLYXFEM == 2) THEN
#include "vectorize.inc" 
          DO I=JFT,JLT
            PLY_F(I,1,IPT) = THLY(I)*(SIGE(I,1)+SIGPE(I,1))
            PLY_F(I,2,IPT) = THLY(I)*(SIGE(I,2)+SIGPE(I,2))
            PLY_F(I,3,IPT) = THLY(I)*(SIGE(I,3)+SIGPE(I,3))
            PLY_F(I,4,IPT) = THLY(I)*SIGE(I,4)
            PLY_F(I,5,IPT) = THLY(I)*SIGE(I,5)  
          ENDDO
        ENDIF ! IF (IPLYXFEM == 1)
!----------------------------------
C
C  visco elastic model 
C             
      IF (IPRONY > 0) THEN 
        CALL PRONY25C(JLT      ,NPRONY ,NPMAX    ,BETA    ,KV    ,
     1                GV       ,DT1C   ,RHO0     ,OFF     ,DIR   ,   
     2                EPSPXX   ,EPSPYY ,EPSPXY   ,EPSPYZ  ,EPSPZX,
     3                SIGVXX   ,SIGVYY ,SIGVXY   ,SIGVYZ  ,SIGVZX,
     4                SIGV     ,SOUNDSP,UVARV    ,IGTYP   )
C 
          IF (ISHPLYXFEM > 0) THEN
#include "vectorize.inc" 
            DO I=JFT,JLT
              PLY_F(I,1,IPT) = PLY_F(I,1,IPT) + THLY(I)*SIGVXX(I)
              PLY_F(I,2,IPT) = PLY_F(I,2,IPT) + THLY(I)*SIGVYY(I)  
              PLY_F(I,3,IPT) = PLY_F(I,3,IPT) + THLY(I)*SIGVXY(I)
              PLY_F(I,4,IPT) = PLY_F(I,4,IPT) + THLY(I)*SIGVYZ(I)
              PLY_F(I,5,IPT) = PLY_F(I,5,IPT) + THLY(I)*SIGVZX(I) 
            ENDDO
          ENDIF  ! IF (ISHPLYXFEM > 0)
        ENDIF ! IF (IPRONY > 0)
C-----------------------
C     For QEPH
C-----------------------
      SIGY(JFT:JLT) = SIGY(JFT:JLT) + YLD(JFT:JLT)/NPTTOT  ! NPTT =MAX(NPTT) for IGTYP=51
!!      IF(IMPL_S>0) THEN
        DO I=JFT,JLT
                ZCFAC(I,1) = ZCFAC(I,1) + ETSE(I) / NPTTOT  ! NPTT =MAX(NPTT) for IGTYP=51
                ZCFAC(I,2) = MIN(ETSE(I),ZCFAC(I,2))
!!                IF (LBUF%OFF(I)==ZERO) ZCFAC(I,2)=-ABS(ZCFAC(I,2))
                IF (OFFL(I)==ZERO) ZCFAC(I,2)=-ABS(ZCFAC(I,2))
        ENDDO
C-----------------------
C     TENSILE RUPTURE
C-----------------------
      CALL M25CRAK(JFT      ,JLT   ,PM      ,OFF    ,DAMT    ,DIR ,
     1             IMATLY   ,ILAY  ,THLY    ,DAMCR  ,DMAXT   ,
     2             NGL      ,STRP1 ,STRP2   ,PLY_ID ,IGTYP   ,NEL ,
     3             IPG      )
C-----------------------
      DEALLOCATE(GV,BETA)
C----  -------------------
      DO I=JFT,JLT
        IF (OFF(I) == ZERO .AND. NPTT > 1) OFFL(I) = ZERO
      ENDDO
C----------------------------
C     TEST DE RUPTURE ---special treatment inside law25 ---
C----------------------------
      DO I=JFT,JLT
        IF (OFF(I) == OFF_OLD(I) .and. OFF(I) > ZERO) THEN
          IF (OFF(I) == ONE) THEN
            IOFF = NINT(PM(42,IMATLY))
            RATIO = PM(188,IMATLY)
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
