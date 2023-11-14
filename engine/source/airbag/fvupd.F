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
Chd|  FVUPD0                        source/airbag/fvupd.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FVUPD1                        source/airbag/fvupd.F         
Chd|        SPMD_FVB_GATH_BEGIN           source/mpi/airbags/spmd_fvb.F 
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE FVUPD0(MONVOL, X, V, VOLMON, SMONVOL, SVOLMON)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr18_c.inc" !DTMIN1
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: SMONVOL, SVOLMON
      INTEGER MONVOL(SMONVOL)
      my_real X(3,NUMNOD), V(3,NUMNOD), VOLMON(SVOLMON)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADPOLH, K1, K2, KIBJET, KIBHOL, KIBALE, N, ITYP, NNS,
     .        NTG, NBRIC, KI1, KI2, NNFV, NTRFV, NPOLH, KK1, NPOLY, ID,
     .        IFV, IMESH, IVMIN, NBA, NTGA, KIA1, KIA2, KIA3, KIA4,
     .        KIA5, KIA6, KIA7, KIA8, NNA, ILVOUT, NNI, NTGI, NNT ,
     .        KK2, KRBJET, KRBHOL, KRBALE, KR1, KRA5, KRA6
      INTEGER NFVMERGE(4), NSKIP, IEQUI, IVINI
      INTEGER NSKIP_TAB(NVOLU)
      my_real :: FVBAG_DTMIN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IADPOLH=1
      K1=1
      K2=1+NIMV*NVOLU
      KIBJET=K2+LICBAG
      KIBHOL=KIBJET+LIBAGJET
      KIBALE=KIBHOL+LIBAGHOL
      KK1=1
      KK2=1+NRVOLU*NVOLU
      KRBJET=KK2+LRCBAG
      KRBHOL=KRBJET+LRBAGJET
      KRBALE=KRBHOL+LRBAGHOL
      IFV=0
C
      DO N=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP==6.OR.ITYP==8) THEN
           IFV=MONVOL(K1-1+45)

C
           IEQUI=MONVOL(K1-1+15)
           IVINI=MONVOL(K1-1+38)
           IF(TT < VOLMON(KK1-1+49).AND.IEQUI >= 1) THEN
              IF(IVINI == 1) THEN
                 NSKIP=MOD(NCYCLE,IEQUI)
                 IF(NSKIP == 0) THEN
                    MONVOL(K1-1+39)=1
                 ELSE
                    NSKIP=0
                    MONVOL(K1-1+39)=0
                 ENDIF
              ELSE
                 MONVOL(K1-1+39)=1
                 NSKIP=MOD(NCYCLE,IEQUI)
              ENDIF
           ELSE
              MONVOL(K1-1+39)=1
              NSKIP=0
           ENDIF
              NSKIP_TAB(N) = NSKIP
              IF(NSKIP >= 1) GO TO 200

              IMESH=MONVOL(K1-1+56)
              IF (IMESH==0) THEN
                                
        
               NNS=MONVOL(K1-1+32)
               NNI=MONVOL(K1-1+68)
               NNT= NNS+NNI
               NNA= MONVOL(K1-1+64)

               ALLOCATE(FVSPMD(IFV)%XXX(3,MAX(1,NNT)))
               ALLOCATE(FVSPMD(IFV)%VVV(3,MAX(1,NNT))) 
               ALLOCATE(FVSPMD(IFV)%WAV(3,MAX(1,NNA))) 
               ALLOCATE(FVSPMD(IFV)%WAX(3,MAX(1,NNA))) 
               IF( NSPMD > 1 ) THEN 
                 CALL SPMD_FVB_GATH_BEGIN(IFV,X,FVSPMD(IFV)%XXX,FVSPMD(IFV)%WAX,
     .                                 V,FVSPMD(IFV)%VVV,FVSPMD(IFV)%WAV )
                 ENDIF
              ENDIF
           ENDIF
  200     K1=K1+NIMV
          KK1=KK1+NRVOLU
        ENDDO

      IADPOLH=1
      K1=1
      K2=1+NIMV*NVOLU
      KIBJET=K2+LICBAG
      KIBHOL=KIBJET+LIBAGJET
      KIBALE=KIBHOL+LIBAGHOL
      KK1=1
      KK2=1+NRVOLU*NVOLU
      KRBJET=KK2+LRCBAG
      KRBHOL=KRBJET+LRBAGJET
      KRBALE=KRBHOL+LRBAGHOL
      IFV=0
C

      DO N=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP==6.OR.ITYP==8) THEN
           IFV=MONVOL(K1-1+45)
           IF(NSKIP_TAB(N) >= 1) GO TO 100
           IMESH=MONVOL(K1-1+56)
           IF (IMESH==1) THEN
            MONVOL(K1-1+56)=0
           ELSE
C
            ID=MONVOL(K1)
            NNS=MONVOL(K1-1+32)
            NTG=MONVOL(K1-1+33)
            NNI=MONVOL(K1-1+68)
            NNT= NNS+NNI
            NTGI= MONVOL(K1-1+69)
            NBRIC=MONVOL(K1-1+35)
            KI1=KIBALE+MONVOL(K1-1+31)
            KI2=KI1+NNT
            KR1=KRBALE+MONVOL(K1-1+34)
C
            NNFV= MONVOL(K1-1+46)
            NTRFV=MONVOL(K1-1+47)
            NPOLY=MONVOL(K1-1+48)
            NPOLH=MONVOL(K1-1+49)
            IVMIN=MONVOL(K1-1+60)
            ILVOUT=MONVOL(K1-1+44)
C
            NBA= MONVOL(K1-1+62)
            NTGA=MONVOL(K1-1+63)
            NNA= MONVOL(K1-1+64)
            KIA1=KI2 +6*(NTG+NTGI)
            KIA2=KIA1+2*NBA
            KIA3=KIA2+12*NBA
            KIA4=KIA3+2*(NTG+NTGI)
            KIA5=KIA4+NNA
            KIA6=KIA5+3*NTGA
            KIA7=KIA6+NTGA
            KIA8=KIA7+8*NBA
C
            KRA5=MIN(SVOLMON, KR1+7*(NNS+NNI)+4*(NTG+NTGI)+6*NNA)
            KRA6=KRA5+3*NNA
C
            NFVMERGE(1)=0
            NFVMERGE(2)=0
            NFVMERGE(3)=0
            NFVMERGE(4)=0
C
            IF (ITYP == 8) THEN
               FVBAG_DTMIN = FVDATA(IFV)%DTMIN
            ELSE
               FVBAG_DTMIN = DTMIN1(52)
            ENDIF
            CALL FVUPD1(
     .        NTG,                   MONVOL(KI1),         MONVOL(KI2)          , X          , NPOLH ,
     .        FVDATA(IFV)%MPOLH,     FVDATA(IFV)%QPOLH,   FVDATA(IFV)%EPOLH    , V          ,
     .        FVDATA(IFV)%PPOLH,     FVDATA(IFV)%RPOLH,   FVDATA(IFV)%GPOLH    ,    
     .        FVDATA(IFV)%IFVNOD,    FVDATA(IFV)%RFVNOD,  FVDATA(IFV)%IFVTRI   ,   
     .        FVDATA(IFV)%IFVPOLY,   FVDATA(IFV)%IFVTADR, FVDATA(IFV)%IFVPOLH  ,  
     .        FVDATA(IFV)%IFVPADR,   NNFV,                NTRFV                , VOLMON(KK1), NPOLY , 
     .        ID,                    FVDATA(IFV)%CPAPOLH, FVDATA(IFV)%CPBPOLH  ,  
     .        FVDATA(IFV)%CPCPOLH,   FVDATA(IFV)%RMWPOLH, FVDATA(IFV)%VPOLH_INI,
     .        IVMIN,                 FVDATA(IFV)%IDPOLH , MONVOL(KIA4)         ,         
     .        MONVOL(KIA5),          MONVOL(KIA6),        FVDATA(IFV)%IBPOLH   ,   
     .        FVDATA(IFV)%REDIR_ANIM,FVDATA(IFV)%NOD_ANIM,FVDATA(IFV)%NNS_ANIM ,
     .        FVDATA(IFV)%NPOLH_ANIM,FVDATA(IFV)%DTPOLH  ,ILVOUT               , NNT        , NNA   ,
     .        IFV,                   VOLMON(KRA5),        FVDATA(IFV)%TPOLH    ,
     .        FVDATA(IFV)%CPDPOLH,   FVDATA(IFV)%CPEPOLH, FVDATA(IFV)%CPFPOLH  ,
     .        ITYP,                  NFVMERGE,            VOLMON(KRA6)         ,
     .        MONVOL(KIA8),          MONVOL(K1)         , FVBAG_DTMIN          , NUMNOD)  
C
            MONVOL(K1-1+49)=NPOLH
            FVDATA(IFV)%NPOLH=NPOLH
            MONVOL(K1-1+70)=MONVOL(K1-1+70)+NFVMERGE(1)
            MONVOL(K1-1+71)=MONVOL(K1-1+71)+NFVMERGE(2)
            MONVOL(K1-1+72)=MONVOL(K1-1+72)+NFVMERGE(3)
            MONVOL(K1-1+73)=MONVOL(K1-1+73)+NFVMERGE(4)
           ENDIF
         ENDIF
  100    K1=K1+NIMV
         KK1=KK1+NRVOLU
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  FVUPD1                        source/airbag/fvupd.F         
Chd|-- called by -----------
Chd|        FVUPD0                        source/airbag/fvupd.F         
Chd|-- calls ---------------
Chd|        FVTEMP                        source/airbag/fvtemp.F        
Chd|        SPMD_FVB_GATH_END             source/mpi/airbags/spmd_fvb.F 
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/airbag/fvmbag_meshcontrol_mod.F
Chd|====================================================================
      SUBROUTINE FVUPD1(NEL       , IBUF    , ELEM     , X    , NPOLH ,
     .                  MPOLH     , QPOLH   , EPOLH    , V    ,
     .                  PPOLH     , RPOLH   , GPOLH    , 
     .                  IFVNOD    , RFVNOD  , IFVTRI   , 
     .                  IFVPOLY   , IFVTADR , IFVPOLH  , 
     .                  IFVPADR   , NNS     , NNTR     , RVOLU, NPOLY ,
     .                  ID        , CPAPOLH , CPBPOLH  ,
     .                  CPCPOLH   , RMWPOLH , VPOLH_INI,
     .                  IVMIN     , IDPOLH  , IBUFA    ,
     .                  ELEMA     , TAGELA  , IBPOLH   ,
     .                  REDIR_ANIM, NOD_ANIM, NNS_ANIM ,
     .                  NPOLH_ANIM, DTPOLH  , ILVOUT   , NNT  , NNA   ,
     .                  IFV       , XXXA    , TPOLH, 
     .                  CPDPOLH   , CPEPOLH , CPFPOLH  ,
     .                  ITYP      , NFVMERGE, VVVA     , 
     .                  NCONA     , IVOLU   ,FVBAG_DTMIN, NUMNOD)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
      USE FVMBAG_MESHCONTROL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr18_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NUMNOD, NNTR, NNS,NNS_ANIM, NPOLH_ANIM,ILVOUT, NNT, NNA, IFV, ITYP,NPOLY, ID, IVMIN, NEL
      INTEGER,INTENT(INOUT) ::NPOLH
      INTEGER IBUF(*), ELEM(3,*), IFVNOD(3,*), IFVTRI(6,NNTR),
     .        IFVPOLY(*), IFVTADR(*), IFVPOLH(*), IFVPADR(*), 
     .        IDPOLH(*), IBUFA(*), ELEMA(3,*),
     .        TAGELA(*), IBPOLH(*), REDIR_ANIM(*), 
     .        NFVMERGE(4), NCONA(16,*),
     .        IVOLU(*)
      my_real
     .        X(3,NUMNOD), MPOLH(NPOLH), QPOLH(3,NPOLH), EPOLH(NPOLH), PPOLH(NPOLH), 
     .        RPOLH(NPOLH), GPOLH(NPOLH), RFVNOD(2,NNS), RVOLU(*),
     .        CPAPOLH(NPOLH), CPBPOLH(NPOLH), CPCPOLH(NPOLH), RMWPOLH(NPOLH),
     .        VPOLH_INI(NPOLH), NOD_ANIM(3,NNS_ANIM), DTPOLH(NPOLH), XXXA(3,*),
     .        TPOLH(NPOLH), CPDPOLH(NPOLH), CPEPOLH(NPOLH), CPFPOLH(NPOLH),
     .        V(3,NUMNOD)  , VVVA(3,*), FVBAG_DTMIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IEL, N1, N2, N3, NN1, NN2, NN3, J, JJ, K, KK, NPA,
     .        IMAX, IP1, IP2, ITAG(NPOLH), ITAGP(NPOLY),
     .        COUNT(NPOLH), II, CC, LEN, NPOLH_OLD, NNP,
     .        IFVPADR_OLD(NPOLH+1), REDIR(NPOLH), ILOOP,
     .        POLHAPP(2,NPOLY), CMAX, JMIN, ITYPM,
     .        IDP1, IDP2, IDPOLH_OLD(NPOLH), IBPOLH_OLD(NPOLH), I1, I2,
     .        NNSA,JJJ,KKK,IP3,ITYPL,DTMERGV12
      my_real
     .        KSI, ETA, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
     .        PNOD(3,NNS), X12, Y12, Z12, X13, Y13, Z13, NRX, NRY,
     .        NRZ, AREA2, PAREA(NNTR), PNORM(3,NNTR), PVOLU(NPOLH),
     .        AREA, NX, NY, NZ, VM, AREAMAX, MPOLH_OLD(NPOLH),
     .        QPOLH_OLD(3,NPOLH), EPOLH_OLD(NPOLH), PVOLU_OLD(NPOLH),
     .        VOLUMIN, AREAPOLY(NPOLY), CPAPOLH_OLD(NPOLH),
     .        CPBPOLH_OLD(NPOLH), CPCPOLH_OLD(NPOLH), 
     .        RMWPOLH_OLD(NPOLH), GPOLH_OLD(NPOLH), VOLUSORT(NPOLH),
     .        VMIN, VTMP, VPOLH_INI_OLD(NPOLH), VVMAX(NPOLH), VOL1,
     .        VOL2, DTMIN, FAC, DTPOLH_OLD(NPOLH),
     .        TPOLH_OLD(NPOLH), CPDPOLH_OLD(NPOLH), CPEPOLH_OLD(NPOLH), 
     .        CPFPOLH_OLD(NPOLH), EFAC, CPA, CPB, CPC, CPD, CPE, CPF,
     .        RMW, TEMP0, TEMP, PVOLTMP,
     .        MASSPOLH, DTI
C
      INTEGER, ALLOCATABLE :: MERGE(:,:), IFVPOLH_OLD(:)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (NSPMD == 1) THEN
C traitement necessaire pour rester p/on
         DO I=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
           I1=FVSPMD(IFV)%IBUF_L(1,I)
           I2=FVSPMD(IFV)%IBUF_L(2,I)
           FVSPMD(IFV)%XXX(1,I1)=X(1,I2)
           FVSPMD(IFV)%XXX(2,I1)=X(2,I2)
           FVSPMD(IFV)%XXX(3,I1)=X(3,I2)
         ENDDO
C
         IF (KMESH(IFV) >= 2) THEN
            DO I = 1, FVSPMD(IFV)%NNA_L
               I1=FVSPMD(IFV)%IBUFA_L(1,I)
               I2=FVSPMD(IFV)%IBUFA_L(2,I)
               IF(NCONA(2,I) == 1) THEN
                  FVSPMD(IFV)%WAX(1,I1)=X(1,I2)
                  FVSPMD(IFV)%WAX(2,I1)=X(2,I2)
                  FVSPMD(IFV)%WAX(3,I1)=X(3,I2)
               ELSE
                  FVSPMD(IFV)%WAX(1,I1)=XXXA(1,I1)
                  FVSPMD(IFV)%WAX(2,I1)=XXXA(2,I1)
                  FVSPMD(IFV)%WAX(3,I1)=XXXA(3,I1)        
               ENDIF
            ENDDO
         ELSE    
            DO I=1,FVSPMD(IFV)%NNA_L
               I1=FVSPMD(IFV)%IBUFA_L(1,I)
               I2=FVSPMD(IFV)%IBUFA_L(2,I)
               FVSPMD(IFV)%WAX(1,I1)=X(1,I2)
               FVSPMD(IFV)%WAX(2,I1)=X(2,I2)
               FVSPMD(IFV)%WAX(3,I1)=X(3,I2)
            ENDDO
         ENDIF
C WA utilise temporairement pour stocker XXXA
         DO I=1,NNA
           XXXA(1,I)=FVSPMD(IFV)%WAX(1,I)
           XXXA(2,I)=FVSPMD(IFV)%WAX(2,I)
           XXXA(3,I)=FVSPMD(IFV)%WAX(3,I)
         END DO
C
         DO I=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
           I1=FVSPMD(IFV)%IBUF_L(1,I)
           I2=FVSPMD(IFV)%IBUF_L(2,I)
           FVSPMD(IFV)%VVV(1,I1)=V(1,I2)
           FVSPMD(IFV)%VVV(2,I1)=V(2,I2)
           FVSPMD(IFV)%VVV(3,I1)=V(3,I2)
         ENDDO
C
         IF (KMESH(IFV) >= 2) THEN
            DO I = 1, FVSPMD(IFV)%NNA_L
               I1=FVSPMD(IFV)%IBUFA_L(1,I)
               I2=FVSPMD(IFV)%IBUFA_L(2,I)
               IF(NCONA(2,I) == 1) THEN
                  FVSPMD(IFV)%WAV(1,I1)=V(1,I2)
                  FVSPMD(IFV)%WAV(2,I1)=V(2,I2)
                  FVSPMD(IFV)%WAV(3,I1)=V(3,I2)
               ELSE
                  FVSPMD(IFV)%WAV(1,I1)=VVVA(1,I1)
                  FVSPMD(IFV)%WAV(2,I1)=VVVA(2,I1)
                  FVSPMD(IFV)%WAV(3,I1)=VVVA(3,I1)        
               ENDIF
            ENDDO
         ELSE        
            DO I=1,FVSPMD(IFV)%NNA_L
               I1=FVSPMD(IFV)%IBUFA_L(1,I)
               I2=FVSPMD(IFV)%IBUFA_L(2,I)
               FVSPMD(IFV)%WAV(1,I1)=V(1,I2)
               FVSPMD(IFV)%WAV(2,I1)=V(2,I2)
               FVSPMD(IFV)%WAV(3,I1)=V(3,I2)
            ENDDO
         ENDIF
C WA utilise temporairement pour stocker VVVA
         DO I=1,NNA
            IF(NCONA(2,I) == 1) THEN
              VVVA(1,I)=FVSPMD(IFV)%WAV(1,I)
              VVVA(2,I)=FVSPMD(IFV)%WAV(2,I)
              VVVA(3,I)=FVSPMD(IFV)%WAV(3,I)
            ENDIF
         ENDDO
C
      ELSE
C
         NNSA=FVSPMD(IFV)%NNSA
C Cacher XXX, VVV WAX WA dans FVSPMD 
c        CALL SPMD_FVB_GATH_BEGIN(IFV,X,FVSPMD(IFV)%XXX,FVSPMD(IFV)%WAX,
c    .                                V,FVSPMD(IFV)%VVV,FVSPMD(IFV)%WAV )
         CALL SPMD_FVB_GATH_END(IFV,X,FVSPMD(IFV)%XXX,FVSPMD(IFV)%WAX,
     .                                V,FVSPMD(IFV)%VVV,FVSPMD(IFV)%WAV )

c        CALL SPMD_FVB_GATH(IFV, X, XXX, WA, XXXSA, 3)
C WA utilise temporairement pour stocker XXXA
         IF (KMESH(IFV) >= 2) THEN
            DO I=1,NNA
               IF (NCONA(2, I) /= 0) THEN
                  XXXA(1,I)=FVSPMD(IFV)%WAX(1,I)
                  XXXA(2,I)=FVSPMD(IFV)%WAX(2,I)
                  XXXA(3,I)=FVSPMD(IFV)%WAX(3,I)
               ENDIF
            END DO
         ELSE    
            
            DO I=1,NNA
               XXXA(1,I)=FVSPMD(IFV)%WAX(1,I)
               XXXA(2,I)=FVSPMD(IFV)%WAX(2,I)
               XXXA(3,I)=FVSPMD(IFV)%WAX(3,I)
            END DO
         ENDIF
C

C XXXSA et VVVSA utilise temporairement
C WA utilise temporairement pour stocker VVVA
         DO I=1,NNA
           IF(NCONA(2,I) == 1) THEN
              VVVA(1,I)=FVSPMD(IFV)%WAV(1,I)
              VVVA(2,I)=FVSPMD(IFV)%WAV(2,I)
              VVVA(3,I)=FVSPMD(IFV)%WAV(3,I)
           END IF
         END DO
C
         IF (ISPMD/=FVSPMD(IFV)%PMAIN-1) GOTO 300      
      ENDIF
C
      IF(TT == DT1) THEN
         DTI = DT1
      ELSE
         DTI = DT12
      ENDIF
            

!$OMP PARALLEL PRIVATE(I,II,J,JJ,K,KK,IP1,IP2,IEL,AREA,NX,NY,NZ,PVOLTMP)
!$OMP+ PRIVATE(X1,Y1,Z1,N1,N2,N3,X12,Y12,Z12,X13,Y13,Z13)
!$OMP+ PRIVATE(X2,Y2,Z2,X3,Y3,Z3,NRX,NRY,NRZ,AREA2) 
!$OMP+ PRIVATE(KSI,ETA,FAC,I1,I2)
C
C Calcul position et vitesse des noeuds internes 
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NNA
        IF(NCONA(2,I) == 0) THEN
           FVSPMD(IFV)%WAV(1,I)=ZERO
           FVSPMD(IFV)%WAV(2,I)=ZERO
           FVSPMD(IFV)%WAV(3,I)=ZERO
           II=NCONA(1,I)
          IF(II==0) CYCLE
           DO J=1,II
             K=NCONA(J+2,I)
             FVSPMD(IFV)%WAV(1,I)=FVSPMD(IFV)%WAV(1,I)+VVVA(1,K)
             FVSPMD(IFV)%WAV(2,I)=FVSPMD(IFV)%WAV(2,I)+VVVA(2,K)
             FVSPMD(IFV)%WAV(3,I)=FVSPMD(IFV)%WAV(3,I)+VVVA(3,K)
           ENDDO
           FVSPMD(IFV)%WAV(1,I)=FVSPMD(IFV)%WAV(1,I)/II
           FVSPMD(IFV)%WAV(2,I)=FVSPMD(IFV)%WAV(2,I)/II
           FVSPMD(IFV)%WAV(3,I)=FVSPMD(IFV)%WAV(3,I)/II
         ENDIF
      ENDDO
!$OMP END DO

      IF(DT1 == ZERO) THEN         
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
        DO I=1,NNA
          IF(NCONA(2,I) == 0) THEN
             FVSPMD(IFV)%WAV(1,I)=RVOLU(67)
             FVSPMD(IFV)%WAV(2,I)=RVOLU(68)
             FVSPMD(IFV)%WAV(3,I)=RVOLU(69)
          ENDIF
        ENDDO
!$OMP END DO
      ENDIF

C Calcul la position des noeuds internes 
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NNA
        IF(NCONA(2,I) == 0) THEN
           VVVA(1,I)=FVSPMD(IFV)%WAV(1,I)
           VVVA(2,I)=FVSPMD(IFV)%WAV(2,I)
           VVVA(3,I)=FVSPMD(IFV)%WAV(3,I)
           XXXA(1,I)=XXXA(1,I)+DTI*FVSPMD(IFV)%WAV(1,I)
           XXXA(2,I)=XXXA(2,I)+DTI*FVSPMD(IFV)%WAV(2,I)
           XXXA(3,I)=XXXA(3,I)+DTI*FVSPMD(IFV)%WAV(3,I)
        ENDIF
      ENDDO
!$OMP END DO 


C Calcul la position de tous les points 
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NNS
         IF (IFVNOD(1,I)==1) THEN
            IEL=IFVNOD(2,I)
            KSI=RFVNOD(1,I)
            ETA=RFVNOD(2,I)
C
            N1=ELEMA(1,IEL)
            N2=ELEMA(2,IEL)
            N3=ELEMA(3,IEL)
            IF (TAGELA(IEL)>0) THEN
               X1=FVSPMD(IFV)%XXX(1,N1)
               Y1=FVSPMD(IFV)%XXX(2,N1)
               Z1=FVSPMD(IFV)%XXX(3,N1)
               X2=FVSPMD(IFV)%XXX(1,N2)
               Y2=FVSPMD(IFV)%XXX(2,N2)
               Z2=FVSPMD(IFV)%XXX(3,N2)
               X3=FVSPMD(IFV)%XXX(1,N3)
               Y3=FVSPMD(IFV)%XXX(2,N3)
               Z3=FVSPMD(IFV)%XXX(3,N3)
            ELSEIF (TAGELA(IEL)<0) THEN
               X1=XXXA(1,N1)
               Y1=XXXA(2,N1)
               Z1=XXXA(3,N1)
               X2=XXXA(1,N2)
               Y2=XXXA(2,N2)
               Z2=XXXA(3,N2)
               X3=XXXA(1,N3)
               Y3=XXXA(2,N3)
               Z3=XXXA(3,N3)
            ENDIF
            PNOD(1,I)=(ONE-KSI-ETA)*X1+KSI*X2+ETA*X3
            PNOD(2,I)=(ONE-KSI-ETA)*Y1+KSI*Y2+ETA*Y3
            PNOD(3,I)=(ONE-KSI-ETA)*Z1+KSI*Z2+ETA*Z3
         ELSEIF (IFVNOD(1,I)==2) THEN
            I2=IFVNOD(3,I)
            PNOD(1,I)=XXXA(1,I2)
            PNOD(2,I)=XXXA(2,I2)
            PNOD(3,I)=XXXA(3,I2)
         ENDIF
      ENDDO
!$OMP END DO

!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NNS
         IF (IFVNOD(1,I)==3) THEN
            I1=IFVNOD(2,I)
            I2=IFVNOD(3,I)
            FAC=RFVNOD(1,I)
            PNOD(1,I)=FAC*PNOD(1,I1)+(ONE-FAC)*PNOD(1,I2)
            PNOD(2,I)=FAC*PNOD(2,I1)+(ONE-FAC)*PNOD(2,I2)
            PNOD(3,I)=FAC*PNOD(3,I1)+(ONE-FAC)*PNOD(3,I2)
         ENDIF
      ENDDO
!$OMP END DO

      IF (NPOLH_ANIM>0) THEN
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
         DO I=1,NNS_ANIM             
            II=REDIR_ANIM(I)         
            NOD_ANIM(1,I)=PNOD(1,II) 
            NOD_ANIM(2,I)=PNOD(2,II) 
            NOD_ANIM(3,I)=PNOD(3,II) 
         ENDDO 
!$OMP END DO
      ENDIF 
            
C Normale et aire des triangles
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NNTR
         N1=IFVTRI(1,I)
         N2=IFVTRI(2,I)
         N3=IFVTRI(3,I)
         X1=PNOD(1,N1)
         Y1=PNOD(2,N1)
         Z1=PNOD(3,N1)
         X2=PNOD(1,N2)
         Y2=PNOD(2,N2)
         Z2=PNOD(3,N2)
         X3=PNOD(1,N3)
         Y3=PNOD(2,N3)
         Z3=PNOD(3,N3)
         X12=X2-X1
         Y12=Y2-Y1
         Z12=Z2-Z1
         X13=X3-X1
         Y13=Y3-Y1
         Z13=Z3-Z1
         NRX=Y12*Z13-Z12*Y13
         NRY=Z12*X13-X12*Z13
         NRZ=X12*Y13-Y12*X13
         AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
         PAREA(I)=HALF*AREA2
         IF (AREA2>0) THEN
            PNORM(1,I)=NRX/AREA2
            PNORM(2,I)=NRY/AREA2
            PNORM(3,I)=NRZ/AREA2
         ELSE
            PNORM(1,I)=ZERO
            PNORM(2,I)=ZERO
            PNORM(3,I)=ZERO
         ENDIF
      ENDDO
!$OMP END DO
C
C Volume des polyhedres
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NPOLH
         PVOLU(I)= ZERO
         PVOLTMP = ZERO
C Boucle sur les polygones du polyhedre
         DO J=IFVPADR(I),IFVPADR(I+1)-1
            JJ=IFVPOLH(J)
C Boucle sur les triangles du polygone
            DO K=IFVTADR(JJ), IFVTADR(JJ+1)-1
               KK=IFVPOLY(K)
               AREA=PAREA(KK)
               IEL=IFVTRI(4,KK)
               IF (IEL>0) THEN
                  NX=PNORM(1,KK)
                  NY=PNORM(2,KK)
                  NZ=PNORM(3,KK)
               ELSE
                  IP1=IFVTRI(5,KK)
                  IP2=IFVTRI(6,KK)
                  IF (IP1==I) THEN
                     NX=PNORM(1,KK)
                     NY=PNORM(2,KK)
                     NZ=PNORM(3,KK)
                  ENDIF
                  IF (IP2==I) THEN
                     NX=-PNORM(1,KK)
                     NY=-PNORM(2,KK)
                     NZ=-PNORM(3,KK)
                  ENDIF   
                  IF (IP1==I.AND.IP2==I) THEN
                     NX=ZERO
                     NY=ZERO
                     NZ=ZERO
                  ENDIF   
               ENDIF      
               N1=IFVTRI(1,KK)
               X1=PNOD(1,N1)
               Y1=PNOD(2,N1)
               Z1=PNOD(3,N1)
               PVOLTMP=PVOLTMP+THIRD*AREA*(X1*NX+Y1*NY+Z1*NZ)
            ENDDO
         ENDDO
         PVOLU(I) = PVOLTMP
      ENDDO
!$OMP END DO 
!$OMP END PARALLEL

      IF(IVOLU(39) == 0) RETURN

C Pas de temps mini
C      DTMIN=DTMIN1(52)
      DTMIN = FVBAG_DTMIN
      DTMERGV12=IDTMIN(52)
      IF(DTMERGV12==2) DTMERGV12=1
C Volume moyen
      VM=ZERO
      NPA=0
      DO I=1,NPOLH
         IF (PVOLU(I)>ZERO) THEN
            VM=VM+PVOLU(I)
            NPA=NPA+1
         ENDIF
      ENDDO
      IF(NPA>0)THEN
        VM=VM/NPA
      ENDIF
      
      !RVOLU(31) : cgmerg
      !RVOLU(33) : VM(from starter)
      !IVOLU(60) : IVMIN/Igmerg
      IF (IVMIN == 1) THEN
         ! mean volume is current one
         VOLUMIN=VM*RVOLU(31) 
      ELSEIF (IVMIN == -1)THEN
         ! specific case : Iswitch=2 : full merging request on Tswitch/Pswitch criteria
         VOLUMIN = EP20
      ELSE 
         ! mean volume is initial one
         VOLUMIN=RVOLU(33)*RVOLU(31)
      ENDIF

C Aire des polygones et polyhedres appuyes
      DO I=1,NPOLY
         AREAPOLY(I)=ZERO
         POLHAPP(1,I)=0
         POLHAPP(2,I)=0
         DO J=IFVTADR(I),IFVTADR(I+1)-1
            JJ=IFVPOLY(J)
            IF (JJ==-1) GOTO 50
            IEL=IFVTRI(4,JJ)
            IF (IEL==0) THEN
              IP1=IFVTRI(5,JJ)
              IP2=IFVTRI(6,JJ)
              AREAPOLY(I)=AREAPOLY(I)+PAREA(JJ)
              POLHAPP(1,I)=IP1
              POLHAPP(2,I)=IP2
            ENDIF
         ENDDO
  50  ENDDO
C
      IF (NPOLH==1) GOTO 300      
  100 DO I=1,NPOLH
         ITAG(I)=0
      ENDDO
      DO I=1,NPOLY
         ITAGP(I)=0
      ENDDO
C Volume max de voisins
!$OMP PARALLEL PRIVATE(I,J,JJ,K,KK,IEL,II)
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NPOLH
         VVMAX(I)=ZERO
         DO J=IFVPADR(I),IFVPADR(I+1)-1
            JJ=IFVPOLH(J)
            DO K=IFVTADR(JJ), IFVTADR(JJ+1)-1
               KK=IFVPOLY(K)
               IEL=IFVTRI(4,KK)
               IF (IEL==0) THEN
                  IF (IFVTRI(5,KK)==I) THEN
                     II=IFVTRI(6,KK)
                  ELSEIF (IFVTRI(6,KK)==I) THEN
                     II=IFVTRI(5,KK)
                  ENDIF
                  VVMAX(I)=MAX(VVMAX(I),PVOLU(II))
               ENDIF
            ENDDO
         ENDDO
         VVMAX(I)=RVOLU(34)*VVMAX(I)
      ENDDO
!$OMP END DO NOWAIT
C
!$OMP SINGLE
      PVOLU_OLD(1:NPOLH)=PVOLU(1:NPOLH)
!$OMP END SINGLE
!$OMP END PARALLEL
C
      ILOOP=0
      DO I=1,NPOLH
         IF (ITAG(I)/=0) CYCLE
         IF (PVOLU(I)<=VOLUMIN.OR.PVOLU(I)<=VVMAX(I).OR.
     .       MPOLH(I)<=ZERO.OR.EPOLH(I)<=ZERO.OR.
     .       (DTMERGV12 == 0 .AND. DTPOLH(I) <= DTMIN .AND. 
     .       PVOLU(I) <= TEN*VOLUMIN) .OR.
     .       (DTMERGV12 == 1 .AND. DTPOLH(I)<=DTMIN) ) THEN
C
            ITYPM=1
            IF (PVOLU(I)>VOLUMIN) ITYPM=2
            IF (MPOLH(I)<=ZERO.OR.EPOLH(I)<=ZERO) ITYPM=3
            IF (DTPOLH(I)<=DTMIN) ITYPM=4
C
C Recherche le polyedre voisin Imax ayant la plus grande surface commune avec le polyedre I
C
            AREAMAX=ZERO
            IMAX=0
            DO J=IFVPADR(I),IFVPADR(I+1)-1
               JJ=IFVPOLH(J)
               AREA=AREAPOLY(JJ)
               IP1=POLHAPP(1,JJ)
               IP2=POLHAPP(2,JJ)
               IF (AREA>AREAMAX) THEN
                  IF (IP1==I) THEN
                     IMAX=IP2
                     AREAMAX=AREA
                  ELSEIF (IP2==I) THEN
                     IMAX=IP1
                     AREAMAX=AREA
                  ENDIF
               ENDIF
            ENDDO
C Only one polyhedron remaining
            IF(IMAX==0) CYCLE
C
            IF (ITAG(IMAX)/=0) THEN 
C Polyedre Imax a deja re   u un polyedre
               ILOOP=1
            ELSE
C Merge polyedre I dans polyedre Imax
               DO J=IFVPADR(IMAX),IFVPADR(IMAX+1)-1
                  JJ=IFVPOLH(J)
                  K=IFVTADR(JJ)
                  KK=IFVPOLY(K)
C Tag polygone commun aux polyedres Imax et I
                  IF (IFVTRI(4,KK)==0.AND.(IFVTRI(5,KK)==I.OR.
     .                                       IFVTRI(6,KK)==I))
     .               ITAGP(JJ)=1
               ENDDO
C
               ITAG(I)=IMAX
               ITAG(IMAX)=-I
               VOL1=PVOLU(I)
               VOL2=PVOLU(IMAX)
               PVOLU(IMAX)=PVOLU(IMAX)+PVOLU(I)
C
               IF(ITYPM == 1) NFVMERGE(1)=NFVMERGE(1)+1
               IF(ITYPM == 2) NFVMERGE(2)=NFVMERGE(2)+1
               IF(ITYPM == 3) NFVMERGE(3)=NFVMERGE(3)+1
               IF(ITYPM == 4) NFVMERGE(4)=NFVMERGE(4)+1
C
               IF (ILVOUT >= 2) THEN
                  IDP1=IDPOLH(I)
                  IDP2=IDPOLH(IMAX)
                  IF (ITYPM==1) THEN
                     WRITE(IOUT,
     .               '(A46,I8,A6,G11.4,A1,A20,I8,A7,G11.4,A1,A12,I10)') 
     .  ' ** GLOBAL MERGE:       MERGING FINITE VOLUME ',IDP1,
     .  ' (VOL=',VOL1,')',
     .  ' WITH FINITE VOLUME ',IDP2,' (VOL=',VOL2,')','  MONVOL ID ',ID
                  ELSEIF (ITYPM==2) THEN
                     WRITE(IOUT,
     .               '(A46,I8,A6,G11.4,A1,A20,I8,A7,G11.4,A1,A12,I10)') 
     .  ' ** NEIGHBORHOOD MERGE: MERGING FINITE VOLUME ',IDP1,
     .  ' (VOL=',VOL1,')',
     .  ' WITH FINITE VOLUME ',IDP2,' (VOL=',VOL2,')','  MONVOL ID ',ID
                  ELSEIF (ITYPM==3) THEN
                     WRITE(IOUT,
     .               '(A46,I8,A6,G11.4,A1,A20,I8,A7,G11.4,A1,A12,I10)') 
     .  ' ** STABILITY MERGE:    MERGING FINITE VOLUME ',IDP1,
     .  ' (VOL=',VOL1,')',
     .  ' WITH FINITE VOLUME ',IDP2,' (VOL=',VOL2,')','  MONVOL ID ',ID
                  ELSEIF (ITYPM==4) THEN
                     WRITE(IOUT,
     .               '(A46,I8,A6,G11.4,A1,A20,I8,A7,G11.4,A1,A12,I10)') 
     .  ' ** TIME STEP MERGE:    MERGING FINITE VOLUME ',IDP1,
     .  ' (VOL=',VOL1,')',
     .  ' WITH FINITE VOLUME ',IDP2,' (VOL=',VOL2,')','  MONVOL ID ',ID
                  ENDIF
               ENDIF   
            ENDIF
         ENDIF
      ENDDO
C
      DO I=1,NPOLH
         DO J=IFVPADR(I),IFVPADR(I+1)-1
            JJ=IFVPOLH(J)
            K=IFVTADR(JJ)
            KK=IFVPOLY(K)
            IF (IFVTRI(4,KK)==0.AND.IFVTRI(5,KK)==IFVTRI(6,KK)) THEN
               ITAGP(JJ)=1
            ENDIF
         ENDDO
      ENDDO
      DO I=1,NPOLH
         COUNT(I)=1
      ENDDO
      DO I=1,NPOLH
         II=ITAG(I)
         IF (II>0) COUNT(II)=COUNT(II)+1
      ENDDO

      CMAX=0
      DO I=1,NPOLH
         CMAX=MAX(CMAX,COUNT(I))
      ENDDO
      IF (CMAX==1) GOTO 300      
C
      ALLOCATE(MERGE(CMAX+1,NPOLH))
      DO I=1,NPOLH
         MERGE(1,I)=1
         MERGE(2,I)=I
      ENDDO
      DO I=1,NPOLH
         II=ITAG(I)
         IF (II>0) THEN
            CC=MERGE(1,II)
            CC=CC+1
            MERGE(1,II)=CC
            MERGE(CC+1,II)=I
            MERGE(1,I)=0
         ENDIF
      ENDDO
C
      LEN=IFVPADR(NPOLH+1)-1
      ALLOCATE(IFVPOLH_OLD(LEN))

!$OMP PARALLEL PRIVATE(I)
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ) 
      DO I=1,IFVPADR(NPOLH+1)-1
         IFVPOLH_OLD(I)=IFVPOLH(I)
      ENDDO
!$OMP END DO
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ) 
      DO I=1,NPOLH+1
         IFVPADR_OLD(I)=IFVPADR(I)
      ENDDO
!$OMP END DO
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ) 
      DO I=1,NPOLH
         MPOLH_OLD(I)=MPOLH(I)
         QPOLH_OLD(1,I)=QPOLH(1,I)
         QPOLH_OLD(2,I)=QPOLH(2,I)
         QPOLH_OLD(3,I)=QPOLH(3,I)
         EPOLH_OLD(I)=EPOLH(I)
         GPOLH_OLD(I)=GPOLH(I)
         CPAPOLH_OLD(I)=CPAPOLH(I)
         CPBPOLH_OLD(I)=CPBPOLH(I)
         CPCPOLH_OLD(I)=CPCPOLH(I)
         RMWPOLH_OLD(I)=RMWPOLH(I)
         VPOLH_INI_OLD(I)=VPOLH_INI(I)
         IDPOLH_OLD(I)=IDPOLH(I)
         IBPOLH_OLD(I)=IBPOLH(I)
         TPOLH_OLD(I)=TPOLH(I)
         CPDPOLH_OLD(I)=CPDPOLH(I)
         CPEPOLH_OLD(I)=CPEPOLH(I)
         CPFPOLH_OLD(I)=CPFPOLH(I)
         DTPOLH_OLD(I)=DTPOLH(I)
C
         MPOLH(I)=ZERO
         QPOLH(1,I)=ZERO
         QPOLH(2,I)=ZERO
         QPOLH(3,I)=ZERO
         EPOLH(I)=ZERO
         PVOLU(I)=ZERO
         GPOLH(I)=ZERO
         CPAPOLH(I)=ZERO
         CPBPOLH(I)=ZERO
         CPCPOLH(I)=ZERO
         RMWPOLH(I)=ZERO
         TPOLH(I)=ZERO
         CPDPOLH(I)=ZERO
         CPEPOLH(I)=ZERO
         CPFPOLH(I)=ZERO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

      NPOLH_OLD=NPOLH
      NPOLH=0
      NNP=0
      DO I=1,NPOLH_OLD
        CC=MERGE(1,I)
        IF (CC==0) CYCLE
        NPOLH=NPOLH+1
        IFVPADR(NPOLH)=NNP+1
        IF(CC == 1) THEN
          JJ=MERGE(2,I)
          REDIR(JJ)=NPOLH
          DO K=IFVPADR_OLD(JJ),IFVPADR_OLD(JJ+1)-1
             KK=IFVPOLH_OLD(K)
             IF (ITAGP(KK)==1) CYCLE
             NNP=NNP+1
             IFVPOLH(NNP)=KK
          ENDDO
C
          MPOLH(NPOLH)=MPOLH_OLD(JJ)
          QPOLH(1,NPOLH)=QPOLH_OLD(1,JJ)
          QPOLH(2,NPOLH)=QPOLH_OLD(2,JJ)
          QPOLH(3,NPOLH)=QPOLH_OLD(3,JJ)
          EPOLH(NPOLH)=EPOLH_OLD(JJ)
C
          IF (MPOLH(NPOLH)<=ZERO.OR.EPOLH(NPOLH)<=ZERO) ILOOP=1
C
          PVOLU(NPOLH)=PVOLU_OLD(JJ)
          GPOLH(NPOLH)=GPOLH_OLD(JJ)
          CPAPOLH(NPOLH)=CPAPOLH_OLD(JJ)
          CPBPOLH(NPOLH)=CPBPOLH_OLD(JJ)
          CPCPOLH(NPOLH)=CPCPOLH_OLD(JJ)
          RMWPOLH(NPOLH)=RMWPOLH_OLD(JJ)
          CPDPOLH(NPOLH)=CPDPOLH_OLD(JJ)
          CPEPOLH(NPOLH)=CPEPOLH_OLD(JJ)
          CPFPOLH(NPOLH)=CPFPOLH_OLD(JJ)
          VPOLH_INI(NPOLH)=VPOLH_INI_OLD(I)
          IDPOLH(NPOLH)=IDPOLH_OLD(I)
          IBPOLH(NPOLH)=IBPOLH_OLD(I)
          DTPOLH(NPOLH)=DTPOLH_OLD(I)
        ELSE
          MASSPOLH=ZERO
          DO J=1,CC
            JJ=MERGE(J+1,I)
            REDIR(JJ)=NPOLH
            DO K=IFVPADR_OLD(JJ),IFVPADR_OLD(JJ+1)-1
               KK=IFVPOLH_OLD(K)
               IF (ITAGP(KK)==1) CYCLE
               NNP=NNP+1
               IFVPOLH(NNP)=KK
            ENDDO
C
            MPOLH(NPOLH)=MPOLH(NPOLH)+MPOLH_OLD(JJ)
            QPOLH(1,NPOLH)=QPOLH(1,NPOLH)+QPOLH_OLD(1,JJ)
            QPOLH(2,NPOLH)=QPOLH(2,NPOLH)+QPOLH_OLD(2,JJ)
            QPOLH(3,NPOLH)=QPOLH(3,NPOLH)+QPOLH_OLD(3,JJ)
            EPOLH(NPOLH)=EPOLH(NPOLH)+EPOLH_OLD(JJ)
            PVOLU(NPOLH)=PVOLU(NPOLH)+PVOLU_OLD(JJ)
C
            IF (MPOLH(NPOLH)<=ZERO.OR.EPOLH(NPOLH)<=ZERO) ILOOP=1
            IF (PVOLU(NPOLH) <= ZERO) ILOOP=1
C
            IF(MPOLH_OLD(JJ) > 0) THEN
             MASSPOLH=MASSPOLH+MPOLH_OLD(JJ)
             GPOLH(NPOLH)  =GPOLH(NPOLH)  +MPOLH_OLD(JJ)*GPOLH_OLD(JJ)
             CPAPOLH(NPOLH)=CPAPOLH(NPOLH)+MPOLH_OLD(JJ)*CPAPOLH_OLD(JJ)
             CPBPOLH(NPOLH)=CPBPOLH(NPOLH)+MPOLH_OLD(JJ)*CPBPOLH_OLD(JJ)
             CPCPOLH(NPOLH)=CPCPOLH(NPOLH)+MPOLH_OLD(JJ)*CPCPOLH_OLD(JJ)
             RMWPOLH(NPOLH)=RMWPOLH(NPOLH)+MPOLH_OLD(JJ)*RMWPOLH_OLD(JJ)
             CPDPOLH(NPOLH)=CPDPOLH(NPOLH)+MPOLH_OLD(JJ)*CPDPOLH_OLD(JJ)
             CPEPOLH(NPOLH)=CPEPOLH(NPOLH)+MPOLH_OLD(JJ)*CPEPOLH_OLD(JJ)
             CPFPOLH(NPOLH)=CPFPOLH(NPOLH)+MPOLH_OLD(JJ)*CPFPOLH_OLD(JJ)
            ENDIF
          ENDDO

          IF(MASSPOLH > ZERO) THEN
            GPOLH(NPOLH)  =GPOLH(NPOLH)  /MASSPOLH
            CPAPOLH(NPOLH)=CPAPOLH(NPOLH)/MASSPOLH
            CPBPOLH(NPOLH)=CPBPOLH(NPOLH)/MASSPOLH
            CPCPOLH(NPOLH)=CPCPOLH(NPOLH)/MASSPOLH
            RMWPOLH(NPOLH)=RMWPOLH(NPOLH)/MASSPOLH
            CPDPOLH(NPOLH)=CPDPOLH(NPOLH)/MASSPOLH
            CPEPOLH(NPOLH)=CPEPOLH(NPOLH)/MASSPOLH
            CPFPOLH(NPOLH)=CPFPOLH(NPOLH)/MASSPOLH
          ENDIF
          VPOLH_INI(NPOLH)=VPOLH_INI_OLD(I)
          IDPOLH(NPOLH)=IDPOLH_OLD(I)
          IF (DT1 /= ZERO) THEN
C     In case of initial engine merging, time step HAS TO be same as the one that would have
C     been calculated if mergind had occurred during starter
             IBPOLH(NPOLH)=0
          ENDIF
          DTPOLH(NPOLH)=EP30
        ENDIF
      ENDDO
      IFVPADR(NPOLH+1)=NNP+1
C

      DO I=1,NNTR
         IF (IFVTRI(4,I)<=0) THEN
            IP1=IFVTRI(5,I)
            IP2=IFVTRI(6,I)
            IFVTRI(5,I)=REDIR(IP1)
            IFVTRI(6,I)=REDIR(IP2)
         ENDIF
      ENDDO
      DO I=1,NPOLY
         IF (ITAGP(I)==1) THEN
            DO J=IFVTADR(I),IFVTADR(I+1)-1
               IFVPOLY(J)=-1
            ENDDO
         ENDIF
         IP1=POLHAPP(1,I)
         IP2=POLHAPP(2,I)
         IF (IP1>0) THEN
            POLHAPP(1,I)=REDIR(IP1)
            POLHAPP(2,I)=REDIR(IP2)
         ENDIF
      ENDDO
      DEALLOCATE(MERGE, IFVPOLH_OLD)


!$OMP PARALLEL PRIVATE(I)
!$OMP+ PRIVATE(TEMP,TEMP0,EFAC,CPA,CPB,CPC,CPD,CPE,CPF,RMW)
!$OMP+ PRIVATE(ITYPL)
      ITYPL = ITYP
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ) 
      DO I=1,NPOLH
         IF( EPOLH(I) <= ZERO .OR. 
     .       MPOLH(I) <= ZERO .OR. 
     .       PVOLU(I) <= ZERO) CYCLE
         RPOLH(I)=MPOLH(I)/PVOLU(I)
         EFAC =EPOLH(I)/MPOLH(I)
         CPA  =CPAPOLH(I)
         CPB  =CPBPOLH(I)
         CPC  =CPCPOLH(I)
         CPD  =CPDPOLH(I)
         CPE  =CPEPOLH(I)
         CPF  =CPFPOLH(I)
         RMW  =RMWPOLH(I)
         TEMP0=RVOLU(25)
         CALL FVTEMP(ITYPL  , EFAC , CPA  , CPB  , CPC  ,
     .               CPD   , CPE  , CPF  , RMW  , TEMP0,
     .               TEMP  )
         TPOLH(I)=TEMP
         PPOLH(I)=RPOLH(I)*RMWPOLH(I)*TEMP
      ENDDO   
!$OMP END DO
!$OMP END PARALLEL

C--------------------------
C     Impression
C--------------------------
      IF(ILVOUT ==4 .OR. ILVOUT ==5) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
        WRITE(IOUT,'(/,4A)') '           FINITE VOLUME','  BRICK  ',
     .             ' VOLUME     MASS     TEMPER.   POLYGONE   TRIANGLE',
     .             '     AREA            TRIANGLE  BRICK1     BRICK2  '
C
        DO I=1,NPOLH
         I1= IDPOLH(I)
         I2= IBPOLH(I)
         IF(I2==0 .OR. ILVOUT==5) THEN
          II=0
          KKK=0
          DO J=IFVPADR(I),IFVPADR(I+1)-1
           JJ=IFVPOLH(J)
            DO K=IFVTADR(JJ),IFVTADR(JJ+1)-1
             KKK=KKK+1
             KK=IFVPOLY(K)
             AREA=PAREA(KK)
             IEL=IFVTRI(4,KK)
             IP1=IFVTRI(5,KK)
             IP2=IFVTRI(6,KK)
             IP3=IFVTRI(1,KK)
             IF(KKK==1) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               WRITE(IOUT,'(3I10,3G10.3,5X,I6,4X,I6,4X,G14.6,3I10,
     .                      G14.6)') I,I1,I2,PVOLU(I),MPOLH(I),TPOLH(I),
     .                                     JJ,KK,AREA,IEL,IP1,IP2,
     .                                     DTPOLH_OLD(I)
             ELSE
               WRITE(IOUT,'(65X,I6,4X,I6,4X,G14.6,3I10,G14.6)') 
     .                                     JJ,KK,AREA,IEL,IP1,IP2,
     .                                     PNOD(1,IP3)
             ENDIF
            ENDDO
          ENDDO
         ENDIF
        ENDDO
      ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (ILOOP==1) THEN
         IF (NPOLH==1) THEN
            IF (ILVOUT >= 1) THEN
               WRITE(IOUT,'(A,I10)') ' ** MONVOL ID ',ID
               WRITE(IOUT,'(A)')'  ONLY ONE FINITE VOLUME REMAIN - EXITING'
            ENDIF
            GOTO 300      
         ELSE
            IF (ILVOUT >= 1) THEN
               WRITE(IOUT,'(A,I10,2A,I10)') ' ** MONVOL ID ',ID,
     .              ' FINITE VOLUME MESH UPDATE - LOOPING -',
     .              ' NUMBER OF FINITE VOLUMES : ',NPOLH
            ENDIF
         ENDIF
         GOTO 100
      ENDIF
C

  300 CONTINUE 

      DEALLOCATE(FVSPMD(IFV)%XXX)
      DEALLOCATE(FVSPMD(IFV)%VVV) 
      DEALLOCATE(FVSPMD(IFV)%WAV) 
      DEALLOCATE(FVSPMD(IFV)%WAX) 

      RETURN
      END
