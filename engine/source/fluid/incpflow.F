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
Chd|  INCPFLOW                      source/fluid/incpflow.F       
Chd|-- called by -----------
Chd|        FLOW0                         source/fluid/flow0.F          
Chd|-- calls ---------------
Chd|        BEMSOLV                       source/fluid/bemsolv.F        
Chd|        BEMSOLVP                      source/fluid/bemsolvp.F       
Chd|        SPMD_FL_SUM                   source/mpi/generic/spmd_fl_sum.F
Chd|        TRGRAD                        source/fluid/incpflow.F       
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        FINTER_SMOOTH                 source/tools/curve/finter_smooth.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE INCPFLOW(NNO   , NEL   , NINOUT , NNI    , IFLOW  ,
     .                    IBUF  , ELEM  , IINOUT , IBUFI  , IBUFR  ,
     .                    IBUFC , IBUFL , RFLOW  , PHI    , PRES   ,
     .                    U     , RINOUT, X      , V      , A      ,
     .                    NPC   , TF    , SENSOR_TAB,
     .                    CNP   , ITAGEL, IBUFELR, IBUFELC, IPIV   ,
     .                    HBEM  , GBEM  , IBUFIL , CNPI   , IPINT  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "flowcom.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNO, NEL, NINOUT, NNI, IFLOW(*), IBUF(*), ELEM(3,*),
     .        IINOUT(NIIOFLOW,*), IBUFI(*), IBUFR(*), IBUFC(*), 
     .        IBUFL(*), NPC(*),CNP(*),
     .        ITAGEL(*), IBUFELR(*), IBUFELC(*), IPIV(*), IBUFIL(*),
     .        CNPI(*), IPINT, ISMOOTH
      my_real
     .        RFLOW(*), PHI(*), PRES(*), U(3,*), RINOUT(NRIOFLOW,*),
     .        X(3,*), V(3,*), A(3,*), TF(*), HBEM(*),
     .        GBEM(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ILVOUT, I, II, NNO_L, III, LENBUF, PP, NNO_L2, J, 
     .        IBUFL2(NNO), JJ, N1, N2, N3, NN1, NN2, NN3, NNRP, NNCP,
     .        CR_LOC_GLOB(NNO), CR_GLOB_LOC(NNO), CC_LOC_GLOB(NNO),
     .        CC_GLOB_LOC(NNO), NREIP, NCEIP, NEP, NR1, NR2, NR3, NC1,
     .        NC2, NC3, CREI_LOC_GLOB(NEL), CCEI_LOC_GLOB(NEL), 
     .        CE_LOC_GLOB(NEL), CE_GLOB_LOC(NEL), NPROW, NPCOL, NBLOC,
     .        IFORM, IFREE, ISENS, IFUNC, IIO, NERP, IC, NNI_L2,
     .        IBUFIL2(NNI), ITEST, ITAGNI(NNI), NNI_L, IFT, ILT, IR, NL,
     .        IFPA, IFPRES, NELF, LELF(NEL), K, IAD, IAD2, IFVINI
      my_real
     .        DTSUB, TL, XL(3,NNO+NNI), VL(3,NNO+NNI), X1, Y1, Z1, X2, 
     .        Y2, Z2, X3, Y3, Z3, X12, Y12, Z12, X13, Y13, Z13, X23, 
     .        Y23, Z23, NRX, NRY, NRZ, NORM(3,NEL),Q(NEL), QL(NEL),  
     .        AREA, VX, VY, VZ, QI(NINOUT), VEL, TSG, TSTART, DYDX, 
     .        SFREE, ISQ, VFREE, ELAREA(NEL), SCALT, NODAREA(NNO),
     .        PHI1, PHI2, PHI3, UX, UY, UZ, AREAN1, AREAN2, AREAN3, 
     .        XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, XX, YY, ZZ, ST, 
     .        VX1, VY1, VZ1, VX2, VY2, VZ2, VX3, VY3, VZ3, NX1, NY1,
     .        NZ1, NX2, NY2, NZ2, NX3, NY3, NZ3, RR1, RR2, RR3, SS1,
     .        SS2, SS3, SAREA, NX, NY, NZ, XC, YC ,ZC, SS, PHIM, QM,
     .        R, FAC, FAC2, DPSDX, DPSDY, DPSDZ, DQSDX, DQSDY, DQSDZ,
     .        GRX, GRY, GRZ, TOLE, XM, YM, ZM, PHIS, QS,
     .        SFPA, SFPRES, RHO, PA, PIO, PHI_OLD(NNO+NNI), UU,
     .        COEF, AREA2, W, KSI, ETA, VAL1, VAL2, VAL3, X0, Y0, Z0,
     .        R2, D2, VGX_OLD, VGY_OLD, VGZ_OLD, VGX, VGY, VGZ, AGX,
     .        AGY, AGZ, SFVINI, VV, PHIP(NNO+NNI), PHI_INF(NNO+NNI)
C
      INTEGER NPG
      my_real
     .        PG(50), WPG(25)
      DATA PG  /.33333333,.33333333,
     .          .33333333,.33333333,
     .          .60000000,.20000000,
     .          .20000000,.60000000,
     .          .20000000,.20000000,
     .          .33333333,.33333333,
     .          .79742699,.10128651,
     .          .10128651,.79742699,
     .          .10128651,.10128651,
     .          .05971587,.47014206,
     .          .47014206,.05971587,
     .          .47014206,.47014206,
     .          .06513010,.06513010,
     .          .86973979,.06513010,
     .          .06513010,.86973979,
     .          .31286550,.04869031,
     .          .63844419,.31286550,
     .          .04869031,.63844419,
     .          .63844419,.04869031,
     .          .31286550,.63844419,
     .          .04869031,.31286550,
     .          .26034597,.26034597,
     .          .47930807,.26034597,
     .          .26034597,.47930807,
     .          .33333333,.33333333/
      DATA WPG  /1.00000000,
     .           -.56250000,.52083333,
     .            .52083333,.52083333,
     .            .22500000,.12593918,
     .            .12593918,.12593918,
     .            .13239415,.13239415,
     .            .13239415,
     .            .05334724,.05334724,
     .            .05334724,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.07711376,
     .            .07711376,.17561526,
     .            .17561526,.17561526,
     .           -.14957004/
C
      my_real
     .       , ALLOCATABLE :: SBUF(:), RBUF(:)
      LOGICAL L_ASSEMB
C
      my_real
     .        FINTER,FINTER_SMOOTH
      EXTERNAL FINTER,FINTER_SMOOTH
C-----------------------------------------------
      VV = ZERO
C      
      IFORM=IFLOW(13)
      ILVOUT=IFLOW(17)
C Subcycling
      L_ASSEMB=.TRUE.
      DTSUB=RFLOW(3)
      IF (DTSUB>ZERO) THEN
         L_ASSEMB=.FALSE.
         TL=RFLOW(4)
         IF (TT>TL) THEN
            L_ASSEMB=.TRUE.
            TL=MAX(TL+DTSUB,TT)
            RFLOW(4)=TL
         ENDIF 
      ENDIF  
C---------------------------------------------------
C 0- Preliminaires
C---------------------------------------------------
C Coordonnees et vitesses locales SPMD
      IF (NSPMD == 1) THEN
         DO I=1,NNO
            II=IBUF(I)
            XL(1,I)=X(1,II)
            XL(2,I)=X(2,II)
            XL(3,I)=X(3,II)
            VL(1,I)=V(1,II)
            VL(2,I)=V(2,II)
            VL(3,I)=V(3,II)
         ENDDO
         DO I=1,NNI
            II=IBUFI(I)
            XL(1,NNO+I)=X(1,II)
            XL(2,NNO+I)=X(2,II)
            XL(3,NNO+I)=X(3,II)
            VL(1,NNO+I)=V(1,II)
            VL(2,NNO+I)=V(2,II)
            VL(3,NNO+I)=V(3,II)
         ENDDO
      ELSE
         ALLOCATE(SBUF(6*(NNO+NNI)), RBUF(6*(NNO+NNI)))
C
         LENBUF=6*(NNO+NNI)
         DO I=1,LENBUF
            SBUF(I)=ZERO
            RBUF(I)=ZERO
         ENDDO
         NNO_L=IFLOW(16)
C
         DO I=1,NNO_L
            II=IBUFL(I)
            III=IBUF(II)
            SBUF(6*(II-1)+1)=X(1,III)/CNP(II)
            SBUF(6*(II-1)+2)=X(2,III)/CNP(II)
            SBUF(6*(II-1)+3)=X(3,III)/CNP(II)
            SBUF(6*(II-1)+4)=V(1,III)/CNP(II)
            SBUF(6*(II-1)+5)=V(2,III)/CNP(II)
            SBUF(6*(II-1)+6)=V(3,III)/CNP(II)
         ENDDO
         NNI_L=IFLOW(22)
         DO I=1,NNI_L
            II=IBUFIL(I)
            III=IBUFI(II)
            SBUF(6*NNO+6*(II-1)+1)=X(1,III)/CNPI(II)
            SBUF(6*NNO+6*(II-1)+2)=X(2,III)/CNPI(II)
            SBUF(6*NNO+6*(II-1)+3)=X(3,III)/CNPI(II)
            SBUF(6*NNO+6*(II-1)+4)=V(1,III)/CNPI(II)
            SBUF(6*NNO+6*(II-1)+5)=V(2,III)/CNPI(II)
            SBUF(6*NNO+6*(II-1)+6)=V(3,III)/CNPI(II)
         ENDDO   
         CALL SPMD_FL_SUM(SBUF, LENBUF, RBUF)
         DO I=1,NNO+NNI
            XL(1,I)=RBUF(6*(I-1)+1)
            XL(2,I)=RBUF(6*(I-1)+2)
            XL(3,I)=RBUF(6*(I-1)+3)
            VL(1,I)=RBUF(6*(I-1)+4)
            VL(2,I)=RBUF(6*(I-1)+5)
            VL(3,I)=RBUF(6*(I-1)+6)
         ENDDO
C
         DEALLOCATE(SBUF, RBUF)
      ENDIF
C Normales sortantes elementaires
      DO I=1,NNO
         NODAREA(I)=ZERO
      ENDDO
C
      DO I=1,NEL
         N1=ELEM(1,I)
         N2=ELEM(2,I)
         N3=ELEM(3,I)
         X1=XL(1,N1)
         X2=XL(1,N2)
         X3=XL(1,N3)
         Y1=XL(2,N1)
         Y2=XL(2,N2)
         Y3=XL(2,N3)
         Z1=XL(3,N1)
         Z2=XL(3,N2)
         Z3=XL(3,N3)
         X12=X2-X1
         Y12=Y2-Y1
         Z12=Z2-Z1
         X13=X3-X1
         Y13=Y3-Y1
         Z13=Z3-Z1
         X23=X3-X2
         Y23=Y3-Y2
         Z23=Z3-Z2
         NRX=Y12*Z13-Z12*Y13
         NRY=Z12*X13-X12*Z13
         NRZ=X12*Y13-Y12*X13
         NORM(1,I)=NRX
         NORM(2,I)=NRY
         NORM(3,I)=NRZ
         AREA=SQRT(NRX**2+NRY**2+NRZ**2)
         ELAREA(I)=HALF*AREA
         NODAREA(N1)=NODAREA(N1)+ONE_OVER_6*AREA
         NODAREA(N2)=NODAREA(N2)+ONE_OVER_6*AREA
         NODAREA(N3)=NODAREA(N3)+ONE_OVER_6*AREA
      ENDDO
C
      DO I=1,NNO+NNI
         PHI_OLD(I)=PHI(I)
      ENDDO
C Vitesse a l'infini
      IFVINI=IFLOW(24)
      VGX_OLD=RFLOW(12)*RFLOW(9)
      VGY_OLD=RFLOW(12)*RFLOW(10)
      VGZ_OLD=RFLOW(12)*RFLOW(11)
      VGX=ZERO
      VGY=ZERO
      VGZ=ZERO
      AGX=ZERO
      AGY=ZERO
      AGZ=ZERO
      IF (IFVINI>0) THEN
         TSTART=ZERO
         SFVINI=RFLOW(7)
         SCALT=RFLOW(8)
         ISMOOTH = 0
         IF (IFVINI > 0) ISMOOTH = NPC(2*NFUNCT+IFVINI+1)
         IF (TT>TSTART.AND.DT1>ZERO) THEN
            TSG=(TT-TSTART)*SCALT
!!            VV=SFVINI*FINTER(IFVINI, TSG, NPC, TF, DYDX)
           IF (ISMOOTH == 0) THEN
             VV=SFVINI*FINTER(IFVINI, TSG, NPC, TF, DYDX)
           ELSE
             VV=SFVINI*FINTER_SMOOTH(IFVINI, TSG, NPC, TF, DYDX)
           ENDIF ! IF (ISMOOTH == 0)
         ENDIF
         RFLOW(12)=VV
         VGX=RFLOW(9)*VV
         VGY=RFLOW(10)*VV
         VGZ=RFLOW(11)*VV
         IF (DT1>ZERO) THEN
            AGX=(VGX-VGX_OLD)/DT1
            AGY=(VGY-VGY_OLD)/DT1
            AGZ=(VGZ-VGZ_OLD)/DT1
         ENDIF
C Potentiel du champ infini
         DO I=1,NNO+NNI
            XX=XL(1,I)
            YY=XL(2,I)
            ZZ=XL(3,I)
            PHI_INF(I)=VGX*(XX-XL(1,NNO))
     .                +VGY*(YY-XL(2,NNO))
     .                +VGZ*(ZZ-XL(3,NNO))
         ENDDO
      ELSE
         DO I=1,NNO+NNI
            PHI_INF(I)=ZERO
         ENDDO      
      ENDIF
C---------------------------------------------------
C 1- Flux sur la surface
C---------------------------------------------------
      DO I=1,NEL
         N1=ELEM(1,I)
         N2=ELEM(2,I)
         N3=ELEM(3,I)
         NRX=NORM(1,I)
         NRY=NORM(2,I)
         NRZ=NORM(3,I)
         AREA=ELAREA(I)
         IF(AREA>EM20)THEN
           AREA2=TWO*AREA         
           NRX=NRX/AREA2
           NRY=NRY/AREA2
           NRZ=NRZ/AREA2
           VX=THIRD*(VL(1,N1)+VL(1,N2)+VL(1,N3))
           VY=THIRD*(VL(2,N1)+VL(2,N2)+VL(2,N3))
           VZ=THIRD*(VL(3,N1)+VL(3,N2)+VL(3,N3))
           Q(I)=VX*NRX+VY*NRY+VZ*NRZ           
         ELSE
           Q(I)=ZERO
         ENDIF

      ENDDO
C      
      DO I=1,NINOUT
         QI(I)=ZERO
         IF (IINOUT(3,I)==0) THEN
            IFREE=I
         ELSE
            ISENS=IINOUT(2,I)
            IFUNC=IINOUT(3,I)
            VEL=RINOUT(1,I)
            SCALT=RINOUT(3,I)
            ISMOOTH = 0
            IF (IFUNC > 0) ISMOOTH = NPC(2*NFUNCT+IFUNC+1)
            IF (ISENS==0) THEN
               TSTART=ZERO
            ELSE
               TSTART=SENSOR_TAB(ISENS)%TSTART
            ENDIF
            IF (TT>=TSTART.AND.DT1>ZERO) THEN
               TSG=(TT-TSTART)*SCALT
!!               QI(I)=VEL*FINTER(IFUNC, TSG, NPC, TF, DYDX)
              IF (ISMOOTH == 0) THEN
                QI(I)=VEL*FINTER(IFUNC, TSG, NPC, TF, DYDX)
              ELSE
                QI(I)=VEL*FINTER_SMOOTH(IFUNC, TSG, NPC, TF, DYDX)
              ENDIF ! IF (ISMOOTH == 0)
            ENDIF
         ENDIF
      ENDDO
C
      IF (NINOUT>ZERO) THEN
         SFREE=ZERO
         ISQ=ZERO
         DO I=1,NEL
            IIO=ITAGEL(I)
            IF (IIO/=0) THEN
               IF (IIO==IFREE) THEN
                  SFREE=SFREE+ELAREA(I)
               ELSE
                  Q(I)=Q(I)+QI(IIO)
               ENDIF
            ENDIF
            ISQ=ISQ+Q(I)*ELAREA(I)
         ENDDO
C
         IF(SFREE/=ZERO)THEN
           VFREE=-ISQ/SFREE
         ELSE
           VFREE=ZERO
         ENDIF
         QI(IFREE)=VFREE
         DO I=1,NEL
            IIO=ITAGEL(I)
            IF (IIO==IFREE) Q(I)=Q(I)+VFREE
         ENDDO
      ENDIF
C
      IF (NSPMD > 1) THEN
         DO I=1,NEL
            QL(I)=ZERO
         ENDDO
         NERP=0
         IC=IBUFELC(1)
         IF (IC>0) THEN
            DO I=1,NEL
               IF (IBUFELR(I)>0) THEN
                  NERP=NERP+1
                  QL(NERP)=Q(I)
               ENDIF
            ENDDO
         ENDIF
      ENDIF     
C---------------------------------------------------
C 2- Resolution BEM
C---------------------------------------------------
      IF (NSPMD == 1) THEN
         IF (DT1>ZERO)
     .      CALL BEMSOLV(L_ASSEMB, ILVOUT, IFORM, NNO,  NEL,
     .                   HBEM,     GBEM,   IBUF,  ELEM, NORM,
     .                   X,        Q,      PHI,   IPIV, PHI_INF)
      ELSE
C Tableaux de correspondance
         NNRP=0
         NNCP=0
         DO I=1,NNO
            CR_LOC_GLOB(I)=0
            CR_GLOB_LOC(I)=0
            CC_LOC_GLOB(I)=0
            CC_GLOB_LOC(I)=0
         ENDDO
         DO I=1,NEL
            CREI_LOC_GLOB(I)=0
            CCEI_LOC_GLOB(I)=0
            CE_LOC_GLOB(I)=0
            CE_GLOB_LOC(I)=0
         ENDDO
C
         DO I=1,NNO
            IF (IBUFR(I)>0) THEN
               NNRP=NNRP+1
               CR_LOC_GLOB(NNRP)=I
               CR_GLOB_LOC(I)=NNRP
            ENDIF
            IF (IBUFC(I)>0) THEN
               NNCP=NNCP+1
               CC_LOC_GLOB(NNCP)=I
               CC_GLOB_LOC(I)=NNCP
            ENDIF
         ENDDO
         NREIP=0
         NCEIP=0
         NEP=0
         DO I=1,NEL
            N1=ELEM(1,I)
            N2=ELEM(2,I)
            N3=ELEM(3,I)
            NR1=IBUFR(N1)
            NR2=IBUFR(N2)
            NR3=IBUFR(N3)
            NC1=IBUFC(N1)
            NC2=IBUFC(N2)
            NC3=IBUFC(N3)
            IF (NR1/=0.OR.NR2/=0.OR.NR3/=0) THEN
               NREIP=NREIP+1
               CREI_LOC_GLOB(NREIP)=I
            ENDIF
            IF (NC1/=0.OR.NC2/=0.OR.NC3/=0) THEN
               NCEIP=NCEIP+1
               CCEI_LOC_GLOB(NCEIP)=I
            ENDIF
            IF (IBUFELC(I)>0) THEN
               NEP=NEP+1
               CE_LOC_GLOB(NEP)=I
               CE_GLOB_LOC(I)=NEP
            ENDIF
         ENDDO
C
         NPROW=IFLOW(18)
         NPCOL=IFLOW(19)
         NBLOC=IFLOW(12)
         IF (DT1>ZERO) 
     .      CALL BEMSOLVP(
     . ELEM,          NORM,  XL,          NNRP,        CR_LOC_GLOB,
     . CR_GLOB_LOC,   NNCP,  CC_LOC_GLOB, CC_GLOB_LOC, NCEIP,
     . CCEI_LOC_GLOB, NEP,   CE_LOC_GLOB, CE_GLOB_LOC, NREIP,
     . CREI_LOC_GLOB, NNO,   NEL,         HBEM,        GBEM,
     . QL,            PHI,   IFORM,       L_ASSEMB,    NPROW,
     . NPCOL,         NBLOC, IPIV,        NERP,        ILVOUT,
     . PHI_INF      )
      ENDIF
C---------------------------------------------------
C 3- Calcul des vitesses
C---------------------------------------------------
      IF (NSPMD == 1) THEN
         PHI(NNO)=ZERO
C Vitesses sur la surface
         DO I=1,NNO
            U(1,I)=ZERO
            U(2,I)=ZERO
            U(3,I)=ZERO
         ENDDO
C
         DO I=1,NEL
            N1=ELEM(1,I)
            N2=ELEM(2,I)
            N3=ELEM(3,I)
            X1=XL(1,N1)
            Y1=XL(2,N1)
            Z1=XL(3,N1)
            X2=XL(1,N2)
            Y2=XL(2,N2)
            Z2=XL(3,N2)
            X3=XL(1,N3)
            Y3=XL(2,N3)
            Z3=XL(3,N3)
            PHI1=PHI(N1)
            PHI2=PHI(N2)
            PHI3=PHI(N3)
            CALL TRGRAD(X1,   X2,   X3,  Y1,  Y2,
     .                  Y3,   Z1,   Z2,  Z3,  PHI1,
     .                  PHI2, PHI3, GRX, GRY, GRZ )
            NRX=NORM(1,I)
            NRY=NORM(2,I)
            NRZ=NORM(3,I)
            AREA=ELAREA(I)
            AREA2=TWO*AREA
            UX=GRX+Q(I)*NRX/AREA2
            UY=GRY+Q(I)*NRY/AREA2
            UZ=GRZ+Q(I)*NRZ/AREA2
            AREAN1=NODAREA(N1)
            AREAN2=NODAREA(N2)
            AREAN3=NODAREA(N3)
            U(1,N1)=U(1,N1)+UX*THIRD*AREA/AREAN1
            U(2,N1)=U(2,N1)+UY*THIRD*AREA/AREAN1
            U(3,N1)=U(3,N1)+UZ*THIRD*AREA/AREAN1
            U(1,N2)=U(1,N2)+UX*THIRD*AREA/AREAN2
            U(2,N2)=U(2,N2)+UY*THIRD*AREA/AREAN2
            U(3,N2)=U(3,N2)+UZ*THIRD*AREA/AREAN2
            U(1,N3)=U(1,N3)+UX*THIRD*AREA/AREAN3
            U(2,N3)=U(2,N3)+UY*THIRD*AREA/AREAN3
            U(3,N3)=U(3,N3)+UZ*THIRD*AREA/AREAN3
         ENDDO
C Vitesses sur les points auxiliaire
         IF (NNI>0.AND.IPINT==1) THEN
            ITEST=IFLOW(21)
            IF (ITEST>0) THEN
               TOLE=RFLOW(6)
               XMIN=EP30
               XMAX=-EP30
               YMIN=EP30
               YMAX=-EP30
               ZMIN=EP30
               ZMAX=-EP30
               DO I=1,NNO
                  XX=XL(1,I)
                  YY=XL(2,I)
                  ZZ=XL(3,I)
                  XMIN=MIN(XMIN,XX)
                  XMAX=MAX(XMAX,XX)
                  YMIN=MIN(YMIN,YY)
                  YMAX=MAX(YMAX,YY)
                  ZMIN=MIN(ZMIN,ZZ)
                  ZMAX=MAX(ZMAX,ZZ)
               ENDDO
C
               DO I=1,NNI
                  ITAGNI(I)=0
                  XX=XL(1,NNO+I)
                  YY=XL(2,NNO+I)
                  ZZ=XL(3,NNO+I)
                  IF (XX>XMAX.OR.XX<XMIN.OR.
     .                YY>YMAX.OR.YY<YMIN.OR.
     .                ZZ>ZMAX.OR.ZZ<ZMIN) CYCLE
                  ST=ZERO
                  DO J=1,NEL
                     N1=ELEM(1,J)
                     N2=ELEM(2,J)
                     N3=ELEM(3,J)
                     X1=XL(1,N1)
                     Y1=XL(2,N1)
                     Z1=XL(3,N1)
                     X2=XL(1,N2)
                     Y2=XL(2,N2)
                     Z2=XL(3,N2)
                     X3=XL(1,N3)
                     Y3=XL(2,N3)
                     Z3=XL(3,N3)
                     VX1=X1-XX
                     VY1=Y1-YY
                     VZ1=Z1-ZZ
                     VX2=X2-XX
                     VY2=Y2-YY
                     VZ2=Z2-ZZ
                     VX3=X3-XX
                     VY3=Y3-YY
                     VZ3=Z3-ZZ
C
                     NX1=VY1*VZ2-VZ1*VY2
                     NY1=VZ1*VX2-VX1*VZ2
                     NZ1=VX1*VY2-VY1*VX2
                     NX2=VY2*VZ3-VZ2*VY3
                     NY2=VZ2*VX3-VX2*VZ3
                     NZ2=VX2*VY3-VY2*VX3
                     NX3=VY3*VZ1-VZ3*VY1
                     NY3=VZ3*VX1-VX3*VZ1
                     NZ3=VX3*VY1-VY3*VX1
                     RR1=SQRT(NX1**2+NY1**2+NZ1**2)
                     RR2=SQRT(NX2**2+NY2**2+NZ2**2)
                     RR3=SQRT(NX3**2+NY3**2+NZ3**2)
                     SS1=-(NX1*NX2+NY1*NY2+NZ1*NZ2)/RR1/RR2
                     SS2=-(NX2*NX3+NY2*NY3+NZ2*NZ3)/RR2/RR3
                     SS3=-(NX3*NX1+NY3*NY1+NZ3*NZ1)/RR3/RR1
                     IF (SS1>ONE) SS1=ONE
                     IF (SS1<-ONE) SS1=-ONE
                     IF (SS2>ONE) SS2=ONE
                     IF (SS2<-ONE) SS2=-ONE
                     IF (SS3>ONE) SS3=ONE
                     IF (SS3<-ONE) SS3=-ONE
                     SAREA=ACOS(SS1)+ACOS(SS2)+ACOS(SS3)-PI
C
                     VX1=X2-X1
                     VY1=Y2-Y1
                     VZ1=Z2-Z1
                     VX2=X3-X1
                     VY2=Y3-Y1
                     VZ2=Z3-Z1
                     NX=VY1*VZ2-VZ1*VY2
                     NY=VZ1*VX2-VX1*VZ2
                     NZ=VX1*VY2-VY1*VX2
                     XC=THIRD*(X1+X2+X3)
                     YC=THIRD*(Y1+Y2+Y3)
                     ZC=THIRD*(Z1+Z2+Z3)
                     SS=NX*(XC-XX)+NY*(YC-YY)+NZ*(ZC-ZZ)
                     IF (SS<ZERO) SAREA=-SAREA
                     ST=ST+SAREA
                  ENDDO
                  IF (ITEST==1.AND.ABS(ST)>TOLE) ITAGNI(I)=1
                  IF (ITEST==2.AND.ABS(ST)<=TOLE) ITAGNI(I)=1
               ENDDO
            ELSE
               DO I=1,NNI
                  ITAGNI(I)=1
               ENDDO
            ENDIF
C
            DO I=1,NNI
               PHI(NNO+I)=ZERO
               U(1,NNO+I)=ZERO
               U(2,NNO+I)=ZERO
               U(3,NNO+I)=ZERO
               IF (ITAGNI(I)==0) CYCLE
               XX=XL(1,NNO+I)
               YY=XL(2,NNO+I)
               ZZ=XL(3,NNO+I)
               DO J=1,NEL
                  N1=ELEM(1,J)
                  N2=ELEM(2,J)
                  N3=ELEM(3,J)
                  X1=XL(1,N1)
                  Y1=XL(2,N1)
                  Z1=XL(3,N1)
                  X2=XL(1,N2)
                  Y2=XL(2,N2)
                  Z2=XL(3,N2)
                  X3=XL(1,N3)
                  Y3=XL(2,N3)
                  Z3=XL(3,N3)
                  X0=THIRD*(X1+X2+X3)
                  Y0=THIRD*(Y1+Y2+Y3)
                  Z0=THIRD*(Z1+Z2+Z3)
                  R2=(X0-XX)**2+(Y0-YY)**2+(Z0-ZZ)**2
                  D2=MIN((X0-X1)**2+(Y0-Y1)**2+(Z0-Z1)**2,
     .                   (X0-X2)**2+(Y0-Y2)**2+(Z0-Z2)**2,
     .                   (X0-X3)**2+(Y0-Y3)**2+(Z0-Z3)**2)
C Nombre de points de gauss sur le triangle
                  IF (R2>HUNDRED*D2) THEN
                     NPG=1
                     IAD=1
                  ELSEIF (R2>TWENTY5*D2) THEN
                     NPG=4
                     IAD=2
                  ELSEIF (R2>FOUR*D2) THEN
                     NPG=7
                     IAD=6
                  ELSE
                     NPG=13
                     IAD=13
                  ENDIF
C
                  QM=Q(J)
                  AREA=ELAREA(J)
                  AREA2=TWO*AREA
                  NRX=NORM(1,J)/AREA2
                  NRY=NORM(2,J)/AREA2
                  NRZ=NORM(3,J)/AREA2
                  IAD2=2*(IAD-1)+1
                  DO K=1,NPG
                     W=WPG(IAD)
                     KSI=PG(IAD2)
                     ETA=PG(IAD2+1)
                     IAD=IAD+1
                     IAD2=IAD2+2
                     VAL1=ONE-KSI-ETA
                     VAL2=KSI
                     VAL3=ETA
                     XM=X1*VAL1+X2*VAL2+X3*VAL3
                     YM=Y1*VAL1+Y2*VAL2+Y3*VAL3
                     ZM=Z1*VAL1+Z2*VAL2+Z3*VAL3
                     PHIM=PHI(N1)*VAL1+PHI(N2)*VAL2+PHI(N3)*VAL3
                     R=SQRT((XX-XM)**2+(YY-YM)**2+(ZZ-ZM)**2)
                     FAC=FOURTH/PI/R**3
                     FAC2=THREE/R**2*
     .                       ((XX-XM)*NRX+(YY-YM)*NRY+(ZZ-ZM)*NRZ)
C
                     PHIS=FOURTH/PI/R
                     QS=FAC*((XX-XM)*NRX+(YY-YM)*NRY+(ZZ-ZM)*NRZ)
                     DPSDX=-FAC*(XX-XM)
                     DPSDY=-FAC*(YY-YM)
                     DPSDZ=-FAC*(ZZ-ZM)
                     DQSDX=FAC*(NRX-(XX-XM)*FAC2)
                     DQSDY=FAC*(NRY-(YY-YM)*FAC2)
                     DQSDZ=FAC*(NRZ-(ZZ-ZM)*FAC2)
C
                     PHI(NNO+I)=PHI(NNO+I)+AREA*W*(QM*PHIS-PHIM*QS)
                     U(1,NNO+I)=U(1,NNO+I)+AREA*W*(QM*DPSDX-PHIM*DQSDX)
                     U(2,NNO+I)=U(2,NNO+I)+AREA*W*(QM*DPSDY-PHIM*DQSDY)
                     U(3,NNO+I)=U(3,NNO+I)+AREA*W*(QM*DPSDZ-PHIM*DQSDZ)
                  ENDDO
               ENDDO
               PHI(NNO+I)=PHI(NNO+I)+PHI_INF(NNO+I)
               U(1,NNO+I)=U(1,NNO+I)+VGX
               U(2,NNO+I)=U(2,NNO+I)+VGY
               U(3,NNO+I)=U(3,NNO+I)+VGZ
            ENDDO
         ENDIF
      ELSE
C Echange des potentiels
         ALLOCATE(SBUF(NNO), RBUF(NNO))
         DO I=1,NNO
            SBUF(I)=ZERO
            RBUF(I)=ZERO
         ENDDO
         IC=IBUFC(1)
         IF (IC>0) THEN
            II=0
            DO I=1,NNO
               IR=IBUFR(I)
               IF (IR>0) THEN
                  II=II+1
                  SBUF(I)=PHI(II)
               ENDIF
            ENDDO
         ENDIF
         LENBUF=NNO
         CALL SPMD_FL_SUM(SBUF, LENBUF, RBUF)
         DO I=1,NNO
            PHI(I)=RBUF(I)
         ENDDO
         DEALLOCATE(SBUF, RBUF)
         PHI(NNO)=ZERO
C Vitesses sur la surface
         DO I=1,NNO
            U(1,I)=ZERO
            U(2,I)=ZERO
            U(3,I)=ZERO
         ENDDO
C
         DO I=1,NEL
            N1=ELEM(1,I)
            N2=ELEM(2,I)
            N3=ELEM(3,I)
            X1=XL(1,N1)
            Y1=XL(2,N1)
            Z1=XL(3,N1)
            X2=XL(1,N2)
            Y2=XL(2,N2)
            Z2=XL(3,N2)
            X3=XL(1,N3)
            Y3=XL(2,N3)
            Z3=XL(3,N3)
            PHI1=PHI(N1)
            PHI2=PHI(N2)
            PHI3=PHI(N3)
            CALL TRGRAD(X1,   X2,   X3,  Y1,  Y2,
     .                  Y3,   Z1,   Z2,  Z3,  PHI1,
     .                  PHI2, PHI3, GRX, GRY, GRZ )
            NRX=NORM(1,I)
            NRY=NORM(2,I)
            NRZ=NORM(3,I)
            AREA=ELAREA(I)
            IF(AREA>EM20)THEN
              AREA2=TWO*AREA
              UX=GRX+Q(I)*NRX/AREA2
              UY=GRY+Q(I)*NRY/AREA2
              UZ=GRZ+Q(I)*NRZ/AREA2
              AREAN1=NODAREA(N1)
              AREAN2=NODAREA(N2)
              AREAN3=NODAREA(N3)
              U(1,N1)=U(1,N1)+UX*THIRD*AREA/AREAN1
              U(2,N1)=U(2,N1)+UY*THIRD*AREA/AREAN1
              U(3,N1)=U(3,N1)+UZ*THIRD*AREA/AREAN1
              U(1,N2)=U(1,N2)+UX*THIRD*AREA/AREAN2
              U(2,N2)=U(2,N2)+UY*THIRD*AREA/AREAN2
              U(3,N2)=U(3,N2)+UZ*THIRD*AREA/AREAN2
              U(1,N3)=U(1,N3)+UX*THIRD*AREA/AREAN3
              U(2,N3)=U(2,N3)+UY*THIRD*AREA/AREAN3
              U(3,N3)=U(3,N3)+UZ*THIRD*AREA/AREAN3              
            ENDIF              

         ENDDO    
C On repartit les noeuds auxiliaires sur les procs
         IF (NNI>0.AND.IPINT==1) THEN
            NL=NNI/NSPMD+1
            IFT=ISPMD*NL+1
            ILT=MIN(IFT+NL-1,NNI)
            ITEST=IFLOW(21)
            IF (ITEST>0) THEN
               TOLE=RFLOW(6)
               XMIN=EP30
               XMAX=-EP30
               YMIN=EP30
               YMAX=-EP30
               ZMIN=EP30
               ZMAX=-EP30
               DO I=1,NNO
                  XX=XL(1,I)
                  YY=XL(2,I)
                  ZZ=XL(3,I)
                  XMIN=MIN(XMIN,XX)
                  XMAX=MAX(XMAX,XX)
                  YMIN=MIN(YMIN,YY)
                  YMAX=MAX(YMAX,YY)
                  ZMIN=MIN(ZMIN,ZZ)
                  ZMAX=MAX(ZMAX,ZZ)
               ENDDO
C
               DO I=IFT,ILT
                  ITAGNI(I)=0
                  XX=XL(1,NNO+I)
                  YY=XL(2,NNO+I)
                  ZZ=XL(3,NNO+I)
                  IF (XX>XMAX.OR.XX<XMIN.OR.
     .                YY>YMAX.OR.YY<YMIN.OR.
     .                ZZ>ZMAX.OR.ZZ<ZMIN) CYCLE
                  ST=ZERO
                  DO J=1,NEL
                     N1=ELEM(1,J)
                     N2=ELEM(2,J)
                     N3=ELEM(3,J)
                     X1=XL(1,N1)
                     Y1=XL(2,N1)
                     Z1=XL(3,N1)
                     X2=XL(1,N2)
                     Y2=XL(2,N2)
                     Z2=XL(3,N2)
                     X3=XL(1,N3)
                     Y3=XL(2,N3)
                     Z3=XL(3,N3)
                     VX1=X1-XX
                     VY1=Y1-YY
                     VZ1=Z1-ZZ
                     VX2=X2-XX
                     VY2=Y2-YY
                     VZ2=Z2-ZZ
                     VX3=X3-XX
                     VY3=Y3-YY
                     VZ3=Z3-ZZ
C
                     NX1=VY1*VZ2-VZ1*VY2
                     NY1=VZ1*VX2-VX1*VZ2
                     NZ1=VX1*VY2-VY1*VX2
                     NX2=VY2*VZ3-VZ2*VY3
                     NY2=VZ2*VX3-VX2*VZ3
                     NZ2=VX2*VY3-VY2*VX3
                     NX3=VY3*VZ1-VZ3*VY1
                     NY3=VZ3*VX1-VX3*VZ1
                     NZ3=VX3*VY1-VY3*VX1
                     RR1=SQRT(NX1**2+NY1**2+NZ1**2)
                     RR2=SQRT(NX2**2+NY2**2+NZ2**2)
                     RR3=SQRT(NX3**2+NY3**2+NZ3**2)
                     SS1=-(NX1*NX2+NY1*NY2+NZ1*NZ2)/RR1/RR2
                     SS2=-(NX2*NX3+NY2*NY3+NZ2*NZ3)/RR2/RR3
                     SS3=-(NX3*NX1+NY3*NY1+NZ3*NZ1)/RR3/RR1
                     IF (SS1>ONE) SS1=ONE
                     IF (SS1<-ONE) SS1=-ONE
                     IF (SS2>ONE) SS2=ONE
                     IF (SS2<-ONE) SS2=-ONE
                     IF (SS3>ONE) SS3=ONE
                     IF (SS3<-ONE) SS3=-ONE
                     SAREA=ACOS(SS1)+ACOS(SS2)+ACOS(SS3)-PI
C
                     VX1=X2-X1
                     VY1=Y2-Y1
                     VZ1=Z2-Z1
                     VX2=X3-X1
                     VY2=Y3-Y1
                     VZ2=Z3-Z1
                     NX=VY1*VZ2-VZ1*VY2
                     NY=VZ1*VX2-VX1*VZ2
                     NZ=VX1*VY2-VY1*VX2
                     XC=THIRD*(X1+X2+X3)
                     YC=THIRD*(Y1+Y2+Y3)
                     ZC=THIRD*(Z1+Z2+Z3)
                     SS=NX*(XC-XX)+NY*(YC-YY)+NZ*(ZC-ZZ)
                     IF (SS<ZERO) SAREA=-SAREA
                     ST=ST+SAREA
                  ENDDO
                  IF (ITEST==1.AND.ABS(ST)>TOLE) ITAGNI(I)=1
                  IF (ITEST==2.AND.ABS(ST)<=TOLE) ITAGNI(I)=1
               ENDDO
            ELSE
               DO I=IFT,ILT
                  ITAGNI(I)=1
               ENDDO
            ENDIF
C
            DO I=IFT,ILT
               PHI(NNO+I)=ZERO
               U(1,NNO+I)=ZERO
               U(2,NNO+I)=ZERO
               U(3,NNO+I)=ZERO
               IF (ITAGNI(I)==0) CYCLE
               XX=XL(1,NNO+I)
               YY=XL(2,NNO+I)
               ZZ=XL(3,NNO+I)
               DO J=1,NEL
                  N1=ELEM(1,J)
                  N2=ELEM(2,J)
                  N3=ELEM(3,J)
                  X1=XL(1,N1)
                  Y1=XL(2,N1)
                  Z1=XL(3,N1)
                  X2=XL(1,N2)
                  Y2=XL(2,N2)
                  Z2=XL(3,N2)
                  X3=XL(1,N3)
                  Y3=XL(2,N3)
                  Z3=XL(3,N3)
                  X0=THIRD*(X1+X2+X3)
                  Y0=THIRD*(Y1+Y2+Y3)
                  Z0=THIRD*(Z1+Z2+Z3)
                  R2=(X0-XX)**2+(Y0-YY)**2+(Z0-ZZ)**2
                  D2=MIN((X0-X1)**2+(Y0-Y1)**2+(Z0-Z1)**2,
     .                   (X0-X2)**2+(Y0-Y2)**2+(Z0-Z2)**2,
     .                   (X0-X3)**2+(Y0-Y3)**2+(Z0-Z3)**2)
C Nombre de points de gauss sur le triangle
                  IF (R2>HUNDRED*D2) THEN
                     NPG=1
                     IAD=1
                  ELSEIF (R2>TWENTY5*D2) THEN
                     NPG=4
                     IAD=2
                  ELSEIF (R2>FOUR*D2) THEN
                     NPG=7
                     IAD=6
                  ELSE
                     NPG=13
                     IAD=13
                  ENDIF
C
                  QM=Q(J)
                  AREA=ELAREA(J)
                  IF(AREA>EM20)THEN
                    AREA2=TWO*AREA
                    NRX=NORM(1,J)/AREA2
                    NRY=NORM(2,J)/AREA2
                    NRZ=NORM(3,J)/AREA2
                  ELSE
                    NRX=ZERO
                    NRY=ZERO
                    NRZ=ZERO
                  ENDIF
                  IAD2=2*(IAD-1)+1
                  DO K=1,NPG
                     W=WPG(IAD)
                     KSI=PG(IAD2)
                     ETA=PG(IAD2+1)
                     IAD=IAD+1
                     IAD2=IAD2+2
                     VAL1=ONE-KSI-ETA
                     VAL2=KSI
                     VAL3=ETA
                     XM=X1*VAL1+X2*VAL2+X3*VAL3
                     YM=Y1*VAL1+Y2*VAL2+Y3*VAL3
                     ZM=Z1*VAL1+Z2*VAL2+Z3*VAL3
                     PHIM=PHI(N1)*VAL1+PHI(N2)*VAL2+PHI(N3)*VAL3
                     R=SQRT((XX-XM)**2+(YY-YM)**2+(ZZ-ZM)**2)
                     IF(R>EM20)THEN
                       FAC=FOURTH/PI/R**3
                       FAC2=THREE/R**2*
     .                       ((XX-XM)*NRX+(YY-YM)*NRY+(ZZ-ZM)*NRZ)
                       PHIS=FOURTH/PI/R
                     ELSE
                       FAC  = ZERO
                       FAC2 = ZERO
                       PHIS = ZERO
                     ENDIF
                     QS=FAC*((XX-XM)*NRX+(YY-YM)*NRY+(ZZ-ZM)*NRZ)
                     DPSDX=-FAC*(XX-XM)
                     DPSDY=-FAC*(YY-YM)
                     DPSDZ=-FAC*(ZZ-ZM)
                     DQSDX=FAC*(NRX-(XX-XM)*FAC2)
                     DQSDY=FAC*(NRY-(YY-YM)*FAC2)
                     DQSDZ=FAC*(NRZ-(ZZ-ZM)*FAC2)
C
                     PHI(NNO+I)=PHI(NNO+I)+AREA*W*(QM*PHIS-PHIM*QS)
                     U(1,NNO+I)=U(1,NNO+I)+AREA*W*(QM*DPSDX-PHIM*DQSDX)
                     U(2,NNO+I)=U(2,NNO+I)+AREA*W*(QM*DPSDY-PHIM*DQSDY)
                     U(3,NNO+I)=U(3,NNO+I)+AREA*W*(QM*DPSDZ-PHIM*DQSDZ)
                  ENDDO
               ENDDO
               PHI(NNO+I)=PHI(NNO+I)+PHI_INF(NNO+I)
               U(1,NNO+I)=U(1,NNO+I)+VGX
               U(2,NNO+I)=U(2,NNO+I)+VGY
               U(3,NNO+I)=U(3,NNO+I)+VGZ
            ENDDO
C Echange des potentiels et vitesses sur noeuds internes
            ALLOCATE(SBUF(4*NNI), RBUF(4*NNI))
            LENBUF=4*NNI
            DO I=1,LENBUF
               SBUF(I)=ZERO
               RBUF(I)=ZERO
            ENDDO
            DO I=IFT,ILT
               SBUF(4*(I-1)+1)=PHI(NNO+I)
               SBUF(4*(I-1)+2)=U(1,NNO+I)
               SBUF(4*(I-1)+3)=U(2,NNO+I)
               SBUF(4*(I-1)+4)=U(3,NNO+I)
            ENDDO
            CALL SPMD_FL_SUM(SBUF, LENBUF, RBUF)
            DO I=1,NNI
               PHI(NNO+I)=RBUF(4*(I-1)+1)
               U(1,NNO+I)=RBUF(4*(I-1)+2)   
               U(2,NNO+I)=RBUF(4*(I-1)+3)   
               U(3,NNO+I)=RBUF(4*(I-1)+4) 
            ENDDO  
         ENDIF
      ENDIF
C---------------------------------------------------
C 4- Calcul des pressions
C---------------------------------------------------
      RHO=RFLOW(5)
      IFPA=IFLOW(23)
      IF (IFPA/=0) THEN
C La pression d'arret est imposee directement
         SFPA=RFLOW(1)
         SCALT=RFLOW(2)
         TSTART=ZERO
         ISMOOTH = 0
         IF (IFPA > 0) ISMOOTH = NPC(2*NFUNCT+IFPA+1)
         IF (TT>=TSTART.AND.DT1>ZERO) THEN
            TSG=(TT-TSTART)*SCALT
!!            PA=SFPA*FINTER(IFPA, TSG, NPC, TF, DYDX)
           IF (ISMOOTH == 0) THEN
             PA=SFPA*FINTER(IFPA, TSG, NPC, TF, DYDX)
           ELSE
             PA=SFPA*FINTER_SMOOTH(IFPA, TSG, NPC, TF, DYDX)
           ENDIF ! IF (ISMOOTH == 0)
         ENDIF
      ELSE
C La pression est imposee a une des entrees-sorties
         DO I=1,NINOUT
            IF (IINOUT(4,I)/=0) THEN
               IFPRES=IINOUT(4,I)
               SFPRES=RINOUT(2,I)
               SCALT=RINOUT(3,I)
               TSTART=0
               PIO=ZERO
               ISMOOTH = 0
               IF (IFPRES > 0) ISMOOTH = NPC(2*NFUNCT+IFPRES+1)
               IF (TT>TSTART.AND.DT1>ZERO) THEN
                  TSG=(TT-TSTART)*SCALT
!!                  PIO=SFPRES*FINTER(IFPRES, TSG, NPC, TF, DYDX)
                 IF (ISMOOTH == 0) THEN
                   PIO=SFPRES*FINTER(IFPRES, TSG, NPC, TF, DYDX)
                 ELSE
                   PIO=SFPRES*FINTER_SMOOTH(IFPRES, TSG, NPC, TF, DYDX)
                 ENDIF ! IF (ISMOOTH == 0)
               ENDIF
               PA=PIO+HALF*RHO*QI(I)**2
            ENDIF
         ENDDO
      ENDIF
C
      IF (DT1>ZERO) THEN
         IF (NCYCLE>1) THEN
            DO I=1,NNO+NNI
               XX=XL(1,I)
               YY=XL(2,I)
               ZZ=XL(3,I)
               PHIP(I)=(PHI(I)-PHI_OLD(I))/DT1
     .             +AGX*(XX-XL(1,NNO))+AGY*(YY-XL(2,NNO))
     .             +AGZ*(ZZ-XL(3,NNO))
            ENDDO
         ELSE
            DO I=1,NNO+NNI
               PHIP(I)=ZERO
            ENDDO
         ENDIF
C
         DO I=1,NNO+NNI
            UU=U(1,I)**2+U(2,I)**2+U(3,I)**2
            PRES(I)=PA-RHO*PHIP(I)-HALF*RHO*UU
         ENDDO
      ELSE
         DO I=1,NNO+NNI
            PRES(I)=ZERO
         ENDDO
      ENDIF   
C---------------------------------------------------
C 5- Forces sur l'enveloppe
C---------------------------------------------------
      IF (NSPMD == 1) THEN
         DO I=1,NEL
            N1=ELEM(1,I)
            N2=ELEM(2,I)
            N3=ELEM(3,I)
            COEF=ONE_OVER_6*THIRD*(PRES(N1)+PRES(N2)+PRES(N3))
            NRX=NORM(1,I)
            NRY=NORM(2,I)
            NRZ=NORM(3,I)
            NN1=IBUF(N1)
            NN2=IBUF(N2)
            NN3=IBUF(N3)
            A(1,NN1)=A(1,NN1)+COEF*NRX
            A(2,NN1)=A(2,NN1)+COEF*NRY
            A(3,NN1)=A(3,NN1)+COEF*NRZ
            A(1,NN2)=A(1,NN2)+COEF*NRX
            A(2,NN2)=A(2,NN2)+COEF*NRY
            A(3,NN2)=A(3,NN2)+COEF*NRZ
            A(1,NN3)=A(1,NN3)+COEF*NRX
            A(2,NN3)=A(2,NN3)+COEF*NRY
            A(3,NN3)=A(3,NN3)+COEF*NRZ
            TFEXT=TFEXT+COEF*NRX*V(1,NN1)*DT1
     .                 +COEF*NRY*V(2,NN1)*DT1
     .                 +COEF*NRZ*V(3,NN1)*DT1
            TFEXT=TFEXT+COEF*NRX*V(1,NN2)*DT1
     .                 +COEF*NRY*V(2,NN2)*DT1
     .                 +COEF*NRZ*V(3,NN2)*DT1
            TFEXT=TFEXT+COEF*NRX*V(1,NN3)*DT1
     .                 +COEF*NRY*V(2,NN3)*DT1
     .                 +COEF*NRZ*V(3,NN3)*DT1
         ENDDO
      ELSE
         NELF=0
         DO I=1,NEL
            N1=ELEM(1,I)
            N2=ELEM(2,I)
            N3=ELEM(3,I)
            IF (IBUF(N1)/=0.OR.IBUF(N2)/=0.OR.IBUF(N3)/=0) THEN
               NELF=NELF+1
               LELF(NELF)=I
            ENDIF
         ENDDO
         DO I=1,NELF
            II=LELF(I)
            N1=ELEM(1,II)
            N2=ELEM(2,II)
            N3=ELEM(3,II)
            COEF=ONE_OVER_6*THIRD*(PRES(N1)+PRES(N2)+PRES(N3))
            NRX=NORM(1,II)
            NRY=NORM(2,II)
            NRZ=NORM(3,II)
            IF (IBUF(N1)/=0) THEN
               NN1=IBUF(N1)
               A(1,NN1)=A(1,NN1)+COEF*NRX
               A(2,NN1)=A(2,NN1)+COEF*NRY
               A(3,NN1)=A(3,NN1)+COEF*NRZ
               TFEXT=TFEXT+COEF*NRX*V(1,NN1)*DT1
     .                    +COEF*NRY*V(2,NN1)*DT1
     .                    +COEF*NRZ*V(3,NN1)*DT1
            ENDIF
            IF (IBUF(N2)/=0) THEN
               NN2=IBUF(N2)
               A(1,NN2)=A(1,NN2)+COEF*NRX
               A(2,NN2)=A(2,NN2)+COEF*NRY
               A(3,NN2)=A(3,NN2)+COEF*NRZ
               TFEXT=TFEXT+COEF*NRX*V(1,NN2)*DT1
     .                    +COEF*NRY*V(2,NN2)*DT1
     .                    +COEF*NRZ*V(3,NN2)*DT1
            ENDIF
            IF (IBUF(N3)/=0) THEN
               NN3=IBUF(N3)
               A(1,NN3)=A(1,NN3)+COEF*NRX
               A(2,NN3)=A(2,NN3)+COEF*NRY
               A(3,NN3)=A(3,NN3)+COEF*NRZ
               TFEXT=TFEXT+COEF*NRX*V(1,NN3)*DT1
     .                    +COEF*NRY*V(2,NN3)*DT1
     .                    +COEF*NRZ*V(3,NN3)*DT1
            ENDIF
         ENDDO      
      ENDIF 
C
      RETURN
      END
Chd|====================================================================
Chd|  TRGRAD                        source/fluid/incpflow.F       
Chd|-- called by -----------
Chd|        INCPFLOW                      source/fluid/incpflow.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRGRAD(X1  , X2  , X3 , Y1 , Y2  ,
     .                  Y3  , Z1  , Z2 , Z3 , PHI1,
     .                  PHI2, PHI3, GRX, GRY, GRZ )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        X1, X2, X3, Y1, Y2, Y3, Z1, Z2, Z3,
     .        PHI1, PHI2, PHI3, GRX, GRY, GRZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .        VX1, VY1, VZ1, VX2, VY2, VZ2, EX1, EX2, EY2,
     .        XL1, YL1, XL2, YL2, XL3, YL3, JAC, JJ11, JJ12, JJ21, JJ22,
     .        GRLX, GRLY
C
      VX1=X2-X1
      VY1=Y2-Y1
      VZ1=Z2-Z1
      EX1=SQRT(VX1**2+VY1**2+VZ1**2)
      IF(EX1>EM20)THEN
        VX1=VX1/EX1
        VY1=VY1/EX1
        VZ1=VZ1/EX1
      ELSE
        VX1=ZERO
        VY1=ZERO
        VZ1=ZERO
      ENDIF
C
      VX2=X3-X1
      VY2=Y3-Y1
      VZ2=Z3-Z1
      EX2=VX1*VX2+VY1*VY2+VZ1*VZ2
      VX2=VX2-EX2*VX1
      VY2=VY2-EX2*VY1
      VZ2=VZ2-EX2*VZ1
      EY2=SQRT(VX2**2+VY2**2+VZ2**2)
      IF(EY2>EM20)THEN      
        VX2=VX2/EY2
        VY2=VY2/EY2
        VZ2=VZ2/EY2
      ELSE
        VX2=ZERO
        VY2=ZERO
        VZ2=ZERO
      ENDIF
C
      XL1=ZERO
      YL1=ZERO
      XL2=EX1
      YL2=ZERO
      XL3=EX2
      YL3=EY2
      JAC=(XL2-XL1)*(YL3-YL1)-(YL2-YL1)*(XL3-XL1)
      IF(ABS(JAC)>EM20)THEN
        JJ11=(YL3-YL1)/JAC
        JJ12=-(XL3-XL1)/JAC
        JJ21=-(YL2-YL1)/JAC
        JJ22=(XL2-XL1)/JAC
      ELSE
        JJ11=ZERO
        JJ12=ZERO
        JJ21=ZERO
        JJ22=ZERO
      ENDIF
C
      GRLX=(PHI2-PHI1)*JJ11+(PHI3-PHI1)*JJ21
      GRLY=(PHI2-PHI1)*JJ12+(PHI3-PHI1)*JJ22
C
      GRX=GRLX*VX1+GRLY*VX2
      GRY=GRLX*VY1+GRLY*VY2
      GRZ=GRLX*VZ1+GRLY*VZ2
C
      RETURN
      END
      
