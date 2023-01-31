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
Chd|  FVBAG1                        source/airbag/fvbag1.F        
Chd|-- called by -----------
Chd|        FVBAG0                        source/airbag/fvbag0.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FVINJT6                       source/airbag/fvinjt6.F       
Chd|        FVINJT8                       source/airbag/fvinjt8.F       
Chd|        FVINJT8_1                     source/airbag/fvinjt8_1.F     
Chd|        FVTEMP                        source/airbag/fvtemp.F        
Chd|        FVVENT0                       source/airbag/fvvent0.F       
Chd|        SPMD_FVB_GATH                 source/mpi/airbags/spmd_fvb_gath.F
Chd|        SPMD_FVB_IGATH                source/mpi/airbags/spmd_fvb_igath.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/fvmbag_meshcontrol_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE FVBAG1(NN     , NEL    , IBUF     , ELEM   , NJET    ,
     2                  IBAGJET, RBAGJET, NVENT    , IBAGHOL, RBAGHOL ,
     3                  P      , RHO    , TK       , U      , SSPK    ,
     4                           X      , V        , A      , SENSOR_TAB,
     5                  FSAV   , NPC    , TF       , IVOLU  , RVOLU   ,
     6                  MPOLH  , QPOLH  , EPOLH    ,
     7                  PPOLH  , RPOLH  , GPOLH    ,
     8                  NPOLH  , IFVNOD , RFVNOD   , IFVTRI ,
     9                  IFVPOLY, IFVTADR, IFVPOLH  ,
     A                  IFVPADR, INFO   , NNS      , NNTR   , IFV     ,
     B                  NPOLHA , DLH    , CPAPOLH  ,
     C                  CPBPOLH, CPCPOLH, RMWPOLH  ,
     D                  ITAGEL , ELSINI , ICONTACT , IDPOLH ,
     E                  ELFMASS, ELFVEL , IBUFA    , ELEMA  , TAGELA  ,
     F                  PA     , RHOA   , TKA      , UA     , BRNA    ,
     G                  NNA    , NTGA   , IBPOLH   , DTPOLH , NNT     ,
     H                  NELT   , XXXA   , VVVA     , NCONA  , POROSITY,
     I                  LGAUGE , GAUGE  , ITYP     , IGEO   , SSPKA   ,
     J                  GEO    , PM     , IPM      , TPOLH  , ELFEHPY ,
     K                  CPDPOLH, CPEPOLH, CPFPOLH  ,
     L                  ELTG   , IPARG  , MATTG    ,
     M                  IGROUPTG,IGROUPC, ELBUF_TAB, FEXT ,CFL_COEF,
     N                  PDISP_OLD, PDISP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE SENSOR_MOD
      USE ANIM_MOD
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
#include      "scr02_c.inc"
#include      "scr07_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN, NEL, IBUF(*), ELEM(3,*), NJET, IBAGJET(NIBJET,*),
     .        NVENT, IBAGHOL(NIBHOL,*),
     .        NPC(*), IVOLU(*), NPOLH, IFVNOD(3,*), IFVTRI(6,*),
     .        IFVPOLY(*),IFVTADR(*), IFVPOLH(*), IFVPADR(*), INFO,
     .        NNS, NNTR, IFV, NPOLHA, ITAGEL(*), ICONTACT(*),
     .        IDPOLH(*), IBUFA(*), ELEMA(3,*), TAGELA(*), BRNA(8,*),
     .        NNA, NTGA, IBPOLH(*), NNT, NELT, NCONA(16,*),
     .        LGAUGE(3,*), ITYP, IGEO(NPROPGI,*)
      INTEGER ELTG(*)
      INTEGER IPM(NPROPMI,*), IPARG(NPARG,*)
      INTEGER MATTG(*), IGROUPTG(*), IGROUPC(*)

      my_real RBAGJET(NRBJET,*), RBAGHOL(NRBHOL,*), P(*), RHO(*),
     .        TK(*), U(3,*), SSPK(*), X(3,*), V(3,*), A(3,*),
     .        FSAV(NTHVKI), TF(*), RVOLU(*), MPOLH(*), QPOLH(3,*),
     .        EPOLH(*), PPOLH(*), RPOLH(*), GPOLH(*), RFVNOD(2,*), DLH,
     .        CPAPOLH(*), CPBPOLH(*), CPCPOLH(*), RMWPOLH(*), ELSINI(*),
     .        ELFMASS(*), ELFVEL(*), PA(*), RHOA(*), TKA(*),SSPKA(*), UA(3,*),
     .        DTPOLH(*), XXXA(3,*), VVVA(3,*), POROSITY(*),
     .        GAUGE(LLGAUGE,*), GEO(NPROPG,*), PM(NPROPM,*), ELFEHPY(*),
     .        TPOLH(*), CPDPOLH(*), CPEPOLH(*), CPFPOLH(*), FEXT(3,*), CFL_COEF,
     .        PDISP_OLD, PDISP
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, IINJ, ISU, IEL, N1, N2, N3, JEL,
     .        NN1, NN2, NN3, J, JJ, K, KK, IMASS, IFLU, ISENS, IVEL,
     .        ITEMP, ITAGP(NPOLH), NV, ILVOUT, NPOLYG,NTRIA, IPRI,
     .        IVENT, IDEF, IPORT, IPORP, IPORA, IPORT1, IPORP1, IPORA1,
     .        ITVENT, PHDT, I1, I2, NNSA, ICONT(NNT),IVDP,ITTF, IDPDEF,
     .        IDTPDEF,PMAIN,N21,N22,TAGSURF(2,NELT+1),ITYPL,
     .        PREPARE_ANIM,IVENTYP,IMETHOD_LC, IEL1, IEL2, IDT_FVMBAG
      INTEGER TITREVENT(20)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: SURF_INT,SURF_EXT

      my_real NODAREA(NNT), X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
     .        X12, Y12, Z12, X13, Y13, Z13, NRX, NRY, NRZ, AREA2,
     .        ELAREA(NELT), NORM(3,NELT), KSI, ETA, PNOD(3,NNS),
     .        PAREA(NNTR), PNORM(3,NNTR), PVOLU(NPOLH), AREA,Denom,Denom_max, AREA_MAX(NPOLH), NX, NY,
     .        NZ, GAMAI, CPAI, CPBI, CPCI, RMWI, TI,
     .        CPDI, CPEI, CPFI,
     .        RHOI, DATAINJ(6,NJET), SCALT, FMASS, GMASS_OLD,
     .        GMTOT_OLD, FVEL, TSTART, TSG, GMASS, DGMASS, INJVEL,
     .        DYDX, GMTOT, RMWG, CPA, CPB, CPC, FTEMP, EFAC, CPG,
     .        CVG, PU(3,NPOLH), PW(3,NPOLH),WSURF(3), GAMA, FEL(NELT), DM(NPOLH),
     .        DQ(3,NPOLH), DE(NPOLH), DMI, DQI(3), DEI, RHO1, RE1, UX1,
     .        UY1, UZ1, RHO2, RE2, UX2, UY2, UZ2, VFX, VFY, VFZ, SS,
     .        ALPHA, RHOM, REM, RUXM, RUYM, RUZM, P1, P2,
     .        UEL(3,NELT), AREA1, AREA3, FF, TEMP, PEXT, VOLG, VOLPH,
     .        AREAP, AREAEL, XX, YY, ZZ, QA, QB, DTX, AL, DD, AD, SSP,
     .        QX, VV, DMINI(NPOLH), DMCPA(NPOLH), DMCPB(NPOLH),
     .        DMCPC(NPOLH), DMRMW(NPOLH), MSINI,
     .        RMW, CP, CV, RHOEL(NELT), TKEL(NELT), SSPEL(NELT), GAMA1, GAMA2,
     .        QVISC(NPOLH), AMTOT, PMEAN, PMEAN2, ELSOUT(NELT),TMP(NEL),
     .        AREA_VENT(NVENT), PM_VENT(NVENT), SCALP, SCALS, EXTEN,
     .        AVENT, BVENT, AINI, FPORT, FPORP, FPORA, FPORT1, FPORP1,
     .        FPORA1, AOUT, AOUT1, DERI, PDEF, PVOLTMP,
     .        DTPDEFI, DTPDEFC, TVENT, PORO(NELT), VOLA(NNA), HMIN(NNTR),
     .        X23, Y23, Z23, X31, Y31, Z31,  L12, L23, L31, H1, H2, H3,
     .        FAC1, FAC2, FAC3, HM1, AREA_OLD, PP, DLS(NPOLH), VMAX, UU,
     .        PCRIT, RHOINJ, PINJ, VX1, VY1, VZ1, VX2, VY2, VZ2,
     .        VX3, VY3, VZ3, VVX, VVY, VVZ, VEL(NELT), FAC, VNOD(3,NNS),
     .        VGRID(3,NNTR), AOUTOT, SVENT(NVENT), SVENTOT, XXX(3,NNT),
     .        VVV(3,NNT),  FVDP, RBID, TTF,
     .        DMCPD(NPOLH), DMCPE(NPOLH), DMCPF(NPOLH),
     .        CPD, CPE, CPF, TEMP0, TT1, PEL(NELT), PSTAG, VOLNO,TSTOPE,
     .        ENINT, MASSFLOW, TFEXT0, ELDMASS(NEL), ELDEHPY(NEL), DMOUT, DHOUT,
     .        CPAM, CPBM, CPCM, CPDM, CPEM, CPFM, RMWM, TMP_VEC(3), VMAT(3),SSP1,SSP2,SSPt,NTRIA_TOT,
     .        PRES_AV, RHO_AV, TEMP_AV, CP_AV, CV_AV, RGAS_AV

      LOGICAL :: boolTAGEL,EXIT_NEG_VOL,INJECTION_STARTED
      LOGICAL :: UP_SWITCH
C
      my_real FINTER, GET_U_FUNC
      EXTERNAL FINTER, GET_U_FUNC
      CHARACTER*20 VENTTITLE
      MY_REAL :: DTOLD, PARAM_LAMBDA
      INTEGER :: PARAM_L_TYPE

C-----------------------------------------------
C   C o m m e n t s
C-----------------------------------------------
C   NEL : number of triangles (external surface only)
C  NELT : number of triangles (both internal & external surfaces)
C   NNS : number of nodes of TETRA mesh
C   NNA : number of 'additional' nodes
C   NNT : total number of nodes
C  NNTR : number of triangles (all polyhedra)
C NPOLH : number of polyhedron
C NPOLY : number of polygons

C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      VOLPH = ZERO

      IF(NBGAUGE > 0 ) THEN
        ALLOCATE(FVSPMD(IFV)%GGG(3,NNT))
        ALLOCATE(FVSPMD(IFV)%GGA(3,NNA))
      ENDIF
      ALLOCATE(FVSPMD(IFV)%AAA(3,NNT))
      FVSPMD(IFV)%AAA(1:3,1:NNT) = ZERO

      IF (NSPMD == 1) THEN
C needed for partih/on
         DO I=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
           I1=FVSPMD(IFV)%IBUF_L(1,I)
           I2=FVSPMD(IFV)%IBUF_L(2,I)
           XXX(1,I1)=X(1,I2)
           XXX(2,I1)=X(2,I2)
           XXX(3,I1)=X(3,I2)
         ENDDO
C
         DO I=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
           I1=FVSPMD(IFV)%IBUF_L(1,I)
           I2=FVSPMD(IFV)%IBUF_L(2,I)
           VVV(1,I1)=V(1,I2)
           VVV(2,I1)=V(2,I2)
           VVV(3,I1)=V(3,I2)
         ENDDO
C
         IF (INTBAG /= 0.AND.NVENT > 0.AND.IVOLU(39) /= 0) THEN
           DO I=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
             I1=FVSPMD(IFV)%IBUF_L(1,I)
             I2=FVSPMD(IFV)%IBUF_L(2,I)
             ICONT(I1)=ICONTACT(I2)
            ENDDO
         ENDIF

      ELSE
         CALL SPMD_FVB_GATH(IFV, X, XXX, RBID, RBID, 4)
C
         CALL SPMD_FVB_GATH(IFV, V, VVV, RBID, RBID, 4)
C
         IF (INTBAG /= 0.AND.NVENT > 0.AND.IVOLU(39) /= 0)
     .      CALL SPMD_FVB_IGATH(IFV, ICONTACT, ICONT)

C
         IF (ISPMD/=FVSPMD(IFV)%PMAIN-1) GOTO 1000
      END IF
C

!$OMP PARALLEL PRIVATE(I,J)
!$OMP+ PRIVATE(I1,I2,KSI,ETA,N1,N2,N3,IEL,X1,Y1,Z1,FAC)
!$OMP+ PRIVATE(X2,Y2,Z2,X3,Y3,Z3,VX1,VX2,VX3,VY1,VY2,VY3,VZ1,VZ2,VZ3)
!$OMP+ PRIVATE(X12,Y12,Z12,X13,Y13,Z13,AREA2,NRX,NRY,NRZ,JJ,AREA,KK,K)
!$OMP+ PRIVATE(NX,NY,NZ,PVOLTMP)

C---------------------------------------------------
C Normal Vectors & Surfaces
C---------------------------------------------------

!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NNT
         NODAREA(I)=ZERO
      ENDDO
!$OMP END DO
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO IEL=1,NELT
        FEL(IEL)=ZERO
      ENDDO
!$OMP END DO
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO IEL=1,NELT
         N1=ELEM(1,IEL)
         N2=ELEM(2,IEL)
         N3=ELEM(3,IEL)
         X1=XXX(1,N1)
         X2=XXX(1,N2)
         X3=XXX(1,N3)
         Y1=XXX(2,N1)
         Y2=XXX(2,N2)
         Y3=XXX(2,N3)
         Z1=XXX(3,N1)
         Z2=XXX(3,N2)
         Z3=XXX(3,N3)
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
         ELAREA(IEL)=AREA2
         NORM(1,IEL)=NRX/AREA2
         NORM(2,IEL)=NRY/AREA2
         NORM(3,IEL)=NRZ/AREA2
         IF(IEL <= NEL) THEN
           TMP(IEL) = ONE_OVER_6 * (X1*NRX+Y1*NRY+Z1*NRZ)
         ENDIF
         PEL(IEL)=ZERO
      ENDDO
!$OMP END DO

!$OMP SINGLE
      VOLG=ZERO
      DO IEL=1,NELT
         N1=ELEM(1,IEL)
         N2=ELEM(2,IEL)
         N3=ELEM(3,IEL)
         AREA2 = ELAREA(IEL) 
         ELAREA(IEL) = HALF * AREA2
         NODAREA(N1)=NODAREA(N1)+ONE_OVER_6*AREA2
         NODAREA(N2)=NODAREA(N2)+ONE_OVER_6*AREA2
         NODAREA(N3)=NODAREA(N3)+ONE_OVER_6*AREA2
         IF(IEL <= NEL) THEN
           VOLG=VOLG+TMP(IEL)                      
         ENDIF
      ENDDO
C
      IF (DT1==ZERO.OR.IVOLU(39)==0) THEN
         DO IEL=1,NELT
            ELFMASS(IEL)=ZERO
            ELFEHPY(IEL)=ZERO
         ENDDO
         FSAV(1:NTHVKI)=ZERO
      ENDIF
!$OMP END SINGLE
C---------------------------------------------------
C Evolution of FV mesh
C---------------------------------------------------
C Nodes
C


!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NNS
         IF (IFVNOD(1,I) == 1) THEN
            IEL=IFVNOD(2,I)
            KSI=RFVNOD(1,I)
            ETA=RFVNOD(2,I)
C
            N1=ELEMA(1,IEL)
            N2=ELEMA(2,IEL)
            N3=ELEMA(3,IEL)
C
            IF (TAGELA(IEL) > 0) THEN
               X1=XXX(1,N1)
               Y1=XXX(2,N1)
               Z1=XXX(3,N1)
               X2=XXX(1,N2)
               Y2=XXX(2,N2)
               Z2=XXX(3,N2)
               X3=XXX(1,N3)
               Y3=XXX(2,N3)
               Z3=XXX(3,N3)
               VX1=VVV(1,N1)
               VX2=VVV(1,N2)
               VX3=VVV(1,N3)
               VY1=VVV(2,N1)
               VY2=VVV(2,N2)
               VY3=VVV(2,N3)
               VZ1=VVV(3,N1)
               VZ2=VVV(3,N2)
               VZ3=VVV(3,N3)
            ELSEIF (TAGELA(IEL) < 0) THEN
               X1=XXXA(1,N1)
               Y1=XXXA(2,N1)
               Z1=XXXA(3,N1)
               X2=XXXA(1,N2)
               Y2=XXXA(2,N2)
               Z2=XXXA(3,N2)
               X3=XXXA(1,N3)
               Y3=XXXA(2,N3)
               Z3=XXXA(3,N3)
               VX1=VVVA(1,N1)
               VX2=VVVA(1,N2)
               VX3=VVVA(1,N3)
               VY1=VVVA(2,N1)
               VY2=VVVA(2,N2)
               VY3=VVVA(2,N3)
               VZ1=VVVA(3,N1)
               VZ2=VVVA(3,N2)
               VZ3=VVVA(3,N3)
            ENDIF
            PNOD(1,I)=(ONE-KSI-ETA)*X1+KSI*X2+ETA*X3
            PNOD(2,I)=(ONE-KSI-ETA)*Y1+KSI*Y2+ETA*Y3
            PNOD(3,I)=(ONE-KSI-ETA)*Z1+KSI*Z2+ETA*Z3
            VNOD(1,I)=(ONE-KSI-ETA)*VX1+KSI*VX2+ETA*VX3
            VNOD(2,I)=(ONE-KSI-ETA)*VY1+KSI*VY2+ETA*VY3
            VNOD(3,I)=(ONE-KSI-ETA)*VZ1+KSI*VZ2+ETA*VZ3
C
         ELSEIF (IFVNOD(1,I) == 2) THEN
            I2=IFVNOD(3,I)
            PNOD(1:3,I) = XXXA(1:3,I2)
            VNOD(1:3,I) = VVVA(1:3,I2)
         ENDIF
      ENDDO
!$OMP END DO

C internal node
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NNS
         IF (IFVNOD(1,I) == 3) THEN
            I1=IFVNOD(2,I)
            I2=IFVNOD(3,I)
            FAC=RFVNOD(1,I)
            PNOD(1,I)=FAC*PNOD(1,I1)+(ONE-FAC)*PNOD(1,I2)
            PNOD(2,I)=FAC*PNOD(2,I1)+(ONE-FAC)*PNOD(2,I2)
            PNOD(3,I)=FAC*PNOD(3,I1)+(ONE-FAC)*PNOD(3,I2)
            VNOD(1,I)=FAC*VNOD(1,I1)+(ONE-FAC)*VNOD(1,I2)
            VNOD(2,I)=FAC*VNOD(2,I1)+(ONE-FAC)*VNOD(2,I2)
            VNOD(3,I)=FAC*VNOD(3,I1)+(ONE-FAC)*VNOD(3,I2)
         ENDIF
      ENDDO
!$OMP END DO
C Normal vector, area, grid velocity in triangle center
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
         IF (AREA2 > 0) THEN
            PNORM(1,I)=NRX/AREA2
            PNORM(2,I)=NRY/AREA2
            PNORM(3,I)=NRZ/AREA2
         ELSE
            PNORM(1,I)=ZERO
            PNORM(2,I)=ZERO
            PNORM(3,I)=ZERO
         ENDIF
         IF(IVOLU(39)==0) CYCLE
         VX1=VNOD(1,N1)
         VY1=VNOD(2,N1)
         VZ1=VNOD(3,N1)
         VX2=VNOD(1,N2)
         VY2=VNOD(2,N2)
         VZ2=VNOD(3,N2)
         VX3=VNOD(1,N3)
         VY3=VNOD(2,N3)
         VZ3=VNOD(3,N3)
         VGRID(1,I)=THIRD*(VX1+VX2+VX3)
         VGRID(2,I)=THIRD*(VY1+VY2+VY3)
         VGRID(3,I)=THIRD*(VZ1+VZ2+VZ3)
      ENDDO
!$OMP END DO
C Polyhedra centers

!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NPOLH
         PVOLU(I)= ZERO
         PVOLTMP = ZERO
         AREA_MAX(I) = ZERO
         DO J=IFVPADR(I),IFVPADR(I+1)-1  !polygons composing the poyhedron
            JJ=IFVPOLH(J)
            DO K=IFVTADR(JJ), IFVTADR(JJ+1)-1 !triangle composing the polygon
               KK=IFVPOLY(K)
               AREA=PAREA(KK) !triangle area7
               AREA_MAX(I)=MAX(AREA_MAX(I), AREA)
               IEL=IFVTRI(4,KK)
               IF (IEL > 0) THEN
                  NX=PNORM(1,KK)
                  NY=PNORM(2,KK)
                  NZ=PNORM(3,KK)
               ELSE
                  I1 =IFVTRI(5,KK)
                  I2 =IFVTRI(6,KK)
                  IF (I1 == I) THEN
                     NX=PNORM(1,KK)
                     NY=PNORM(2,KK)
                     NZ=PNORM(3,KK)
                  ENDIF
                  IF (I2 == I) THEN
                     NX=-PNORM(1,KK)
                     NY=-PNORM(2,KK)
                     NZ=-PNORM(3,KK)
                  ENDIF
                  IF (I1 == I.AND.I2 == I) THEN
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

            ENDDO! next K(triangle)
         ENDDO! next J (polygon)
         PVOLU(I) = PVOLTMP
      ENDDO! next I (polyhedron)
!$OMP END DO
!$OMP END PARALLEL
C---------------------------------------------------
C Verifications
C---------------------------------------------------
      AREAP=ZERO
      DO I=1,NNTR
         IEL=IFVTRI(4,I)
         IF (IEL > 0) AREAP=AREAP+PAREA(I)
      ENDDO
      AREAEL=ZERO
      DO IEL=1,NEL
         AREAEL=AREAEL+ELAREA(IEL)
      ENDDO
      RVOLU(18)=AREAEL
      ILVOUT=IVOLU(44)
      VOLPH=ZERO
      EXIT_NEG_VOL = .FALSE.
      DO I=1,NPOLH
         VOLPH=VOLPH+PVOLU(I)
         IF(PVOLU(I)  <=  0) EXIT_NEG_VOL = .TRUE.
      ENDDO

      IPRI=MOD(NCYCLE,IABS(NCPRI))
      IF(EXIT_NEG_VOL) THEN
       DO I=1,NPOLH
         IF (PVOLU(I) <= ZERO) THEN
            INFO=1
            IERR=IERR+1
            CALL ANCMSG(MSGID=185,ANMODE=ANINFO,
     .            I1=IDPOLH(I),R1=PVOLU(I),I3=I,I4=NPOLH)
            CALL ARRET(2)
         ENDIF
       ENDDO
      ENDIF


C---------------------------------------------------
C Calcul des quantites dans les polyhedres
C---------------------------------------------------
      IF (DT1 == ZERO) THEN
         GAMAI=RVOLU(1)
         GPOLH(1:NPOLH)=GAMAI
         CPAI =RVOLU(7)
         CPAPOLH(1:NPOLH)=CPAI
         CPBI =RVOLU(8)
         CPBPOLH(1:NPOLH)=CPBI
         CPCI =RVOLU(9)
         CPCPOLH(1:NPOLH)=CPCI
         RMWI =RVOLU(10)
         RMWPOLH(1:NPOLH)=RMWI
         TI   =RVOLU(13)
         TPOLH(1:NPOLH)=TI
         CPDI =RVOLU(56)
         CPDPOLH(1:NPOLH)=CPDI
         CPEI =RVOLU(57)
         CPEPOLH(1:NPOLH)=CPEI
         CPFI =RVOLU(58)
         CPFPOLH(1:NPOLH)=CPFI
         RHOI =RVOLU(62)
         RPOLH(1:NPOLH)=RHOI
      ENDIF
      IF (DT1==ZERO.OR.IVOLU(39)==0) THEN
         RHOI =RVOLU(62)
         EFAC =RVOLU(66)
         MPOLH(1:NPOLH)=RHOI*PVOLU(1:NPOLH)
         EPOLH(1:NPOLH)=MPOLH(1:NPOLH)*EFAC
         PU(1,1:NPOLH) = RVOLU(67)
         PU(2,1:NPOLH) = RVOLU(68)
         PU(3,1:NPOLH) = RVOLU(69)
         QPOLH(1,1:NPOLH) = MPOLH(1:NPOLH)*PU(1,1:NPOLH)
         QPOLH(2,1:NPOLH) = MPOLH(1:NPOLH)*PU(2,1:NPOLH)
         QPOLH(3,1:NPOLH) = MPOLH(1:NPOLH)*PU(3,1:NPOLH)
      ENDIF

      PEXT =RVOLU(3)
      IF(IVOLU(39) == 0) GO TO 250

C---------------------------------------------------
C Injecteurs
C---------------------------------------------------
      DATAINJ(1:6,1:NJET)=ZERO
      DO IEL=1,NELT
         IF (ITAGEL(IEL) > 0) THEN
            IINJ=ITAGEL(IEL)
            DATAINJ(1,IINJ)=DATAINJ(1,IINJ)+ELAREA(IEL)
         ENDIF
      ENDDO
C
      SCALT =RVOLU(26)
      IF(ITYP == 6) THEN
        CALL FVINJT6(NJET, IBAGJET, RBAGJET, NPC, TF, SENSOR_TAB,
     .               SCALT, DATAINJ )
      ELSEIF(ITYP == 8) THEN
        CALL FVINJT8(NJET, IBAGJET, RBAGJET, NPC, TF, SENSOR_TAB,
     .               SCALT, IGEO, GEO, PM, IVOLU, DATAINJ )
      ENDIF
C
      DO IINJ=1,NJET
         ISENS=IBAGJET(4,IINJ)
         FVEL=RBAGJET(15,IINJ)
         IVEL=IBAGJET(11,IINJ)
         IF(ISENS == 0)THEN
          TSTART=ZERO
         ELSE
          TSTART=SENSOR_TAB(ISENS)%TSTART
         ENDIF
C        ITTF : 1 shift event time (can be removed in a later version)
C        ITTF : 2 shift event time + event function
C        ITTF : 1 or 2 +10 1 flag has been activated and RVOLU(60) is filled
         ITTF=IVOLU(17)
         IF (TT >= TSTART.AND.(ITTF == 1.OR.ITTF == 2.OR.ITTF == 3))THEN
C        ITTF ne doit etre declenche que si TT plus grand que TSTART
C        ce qui etait fait avant n'avait pas d'effet
           ITTF=ITTF+10
           RVOLU(60)=TSTART
           IVOLU(17)=ITTF
         END IF
         IF (TT >= TSTART.AND.DT1 > ZERO)THEN
           TSG=(TT-TSTART)*SCALT
           IF (IVEL > 0) THEN
              INJVEL=FVEL*FINTER(IVEL,TSG,NPC,TF,DYDX)
           ELSE
              INJVEL = FVEL
           ENDIF
         ELSE
           INJVEL=ZERO
         ENDIF
         DATAINJ(3,IINJ)=INJVEL
      ENDDO
C---------------------------------------------------
C Vent holes
C---------------------------------------------------
      IF (INTBAG == 0) THEN
         PORO(1:NELT)=ZERO
      ELSE
!$OMP PARALLEL PRIVATE(IEL,N1,N2,N3)
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
         DO IEL=1,NELT
            N1=ELEM(1,IEL)
            N2=ELEM(2,IEL)
            N3=ELEM(3,IEL)
            PORO(IEL)=ZERO
            IF (ICONT(N1) /= 0) PORO(IEL)=PORO(IEL)+THIRD
            IF (ICONT(N2) /= 0) PORO(IEL)=PORO(IEL)+THIRD
            IF (ICONT(N3) /= 0) PORO(IEL)=PORO(IEL)+THIRD
         ENDDO
!$OMP END DO
!$OMP END PARALLEL
      ENDIF
C
      PEXT =RVOLU(3)
      SCALT=RVOLU(26)
      SCALP=RVOLU(27)
      SCALS=RVOLU(28)
      TTF  =RVOLU(60)
C-----------------------------------------------------------
C Surface de fuite effective des triangles de l'airbag
C Correction des scale factors sur la surface des vent holes
C-----------------------------------------------------------
      CALL FVVENT0(
     1             ELSOUT  ,AOUTOT  ,NVENT    ,NELT    ,ITTF    ,
     2             ELAREA  ,ELSINI  ,ELEM     ,ITAGEL  ,SVENT   ,
     3             IBAGHOL ,RVOLU   ,RBAGHOL  ,PORO    ,P       ,
     4             ELTG    ,IPARG   ,MATTG    ,NEL     ,POROSITY,
     5             IPM     ,PM      ,ELBUF_TAB,IGROUPC ,IGROUPTG)
C---------------------------------------------------
C Bilans volumes finis
C---------------------------------------------------
!$OMP PARALLEL PRIVATE(I,J,JJ,K,KK,IEL,JEL,AREA,N1,N2,N3)
!$OMP+ PRIVATE(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X12,Y12,Z12,X23,Y23,Z23,Z31,Y31,X31)
!$OMP+ PRIVATE(L12,L23,L31,H1)
!$OMP SECTIONS
!$OMP SECTION
         FEL(1:NELT) = ZERO
         ELFVEL(1:NELT)=ZERO
!$OMP SECTION
         ELDMASS(1:NEL)=ZERO
         ELDEHPY(1:NEL)=ZERO
!$OMP SECTION
         ITAGP(1:NPOLH)=0
         DM(1:NPOLH)=ZERO
!$OMP SECTION
         DQ(1,1:NPOLH)=ZERO
         DQ(2,1:NPOLH)=ZERO
!$OMP SECTION
         DQ(3,1:NPOLH)=ZERO
         DE(1:NPOLH)=ZERO
!$OMP SECTION
         DMINI(1:NPOLH)=ZERO
         DMCPA(1:NPOLH)=ZERO
!$OMP SECTION
         DMCPB(1:NPOLH)=ZERO
         DMCPC(1:NPOLH)=ZERO
!$OMP SECTION
         DMRMW(1:NPOLH)=ZERO
         DMCPD(1:NPOLH)=ZERO
!$OMP SECTION
         DMCPE(1:NPOLH)=ZERO
         DMCPF(1:NPOLH)=ZERO
!$OMP SECTION
C----------------------
C Transports
C----------------------
         PU(1,1:NPOLH)=QPOLH(1,1:NPOLH)/MPOLH(1:NPOLH)
         PU(2,1:NPOLH)=QPOLH(2,1:NPOLH)/MPOLH(1:NPOLH)
         PU(3,1:NPOLH)=QPOLH(3,1:NPOLH)/MPOLH(1:NPOLH)
!$OMP END SECTIONS
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NPOLH
         DO J=IFVPADR(I),IFVPADR(I+1)-1
            JJ=IFVPOLH(J)
            DO K=IFVTADR(JJ),IFVTADR(JJ+1)-1
              KK=IFVPOLY(K)
              IEL=ABS(IFVTRI(4,KK))
              JEL=IFVTRI(4,KK)
              AREA=PAREA(KK)
              IF(JEL == 0) THEN
C Permeabilite du triangle interne
                 N1=IFVTRI(1,KK)
                 N2=IFVTRI(2,KK)
                 N3=IFVTRI(3,KK)
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
                 X23=X3-X2
                 Y23=Y3-Y2
                 Z23=Z3-Z2
                 X31=X1-X3
                 Y31=Y1-Y3
                 Z31=Z1-Z3
                 L12=X12**2+Y12**2+Z12**2
                 L23=X23**2+Y23**2+Z23**2
                 L31=X31**2+Y31**2+Z31**2
                 H1=FOUR*AREA**2/MAX(L12,L23,L31)
                 HMIN(KK)=SQRT(H1)
                ENDIF
              ENDDO
           ENDDO
        ENDDO
!$OMP END PARALLEL
C----------------
C Calcul des flux
C----------------
      DO I=1,NPOLH
         DO J=IFVPADR(I),IFVPADR(I+1)-1
            JJ=IFVPOLH(J)
            DO K=IFVTADR(JJ),IFVTADR(JJ+1)-1
               KK  =IFVPOLY(K)
               IEL =ABS(IFVTRI(4,KK))
               JEL =IFVTRI(4,KK)
               AREA=PAREA(KK)
               IF (AREA == ZERO) CYCLE
               boolTAGEL = .FALSE.
               IF(IEL > 0)THEN
                 IF(ITAGEL(IEL) > 0)boolTAGEL = .TRUE.
               ENDIF
               ! IF (JEL > 0.OR.(JEL < 0.AND.ITAGEL(IEL) > 0)) THEN debugger issue in case of IEL=0 (out of bounds)
               IF (JEL > 0.OR.(JEL < 0.AND.boolTAGEL)) THEN
                  II=I
                  NRX=NORM(1,IEL)
                  NRY=NORM(2,IEL)
                  NRZ=NORM(3,IEL)
                  NV = 0
                  IF(JEL < 0) THEN
                     IF    (IFVTRI(5,KK) == I) THEN
                        NV=IFVTRI(6,KK)
                        NX=PNORM(1,KK)
                        NY=PNORM(2,KK)
                        NZ=PNORM(3,KK)
                     ELSEIF(IFVTRI(6,KK) == I) THEN
                        NV=IFVTRI(5,KK)
                        NX=-PNORM(1,KK)
                        NY=-PNORM(2,KK)
                        NZ=-PNORM(3,KK)
                     ENDIF 
                     !check if adjacent polyhedron already updated
                     IF (NV  <  I) GOTO 100
                     FAC=NX*NRX+NY*NRY+NZ*NRZ
                     IF(FAC < 0) II=NV
                  ENDIF
C--------------------------------------------
C Triangle on airbag envelope
C internal triangle is an injector
C--------------------------------------------
                  IF (ITAGEL(IEL) > 0) THEN
C Injector
                     IINJ=ITAGEL(IEL)
                     DMI=DATAINJ(2,IINJ)*AREA/DATAINJ(1,IINJ)
                     IF (NV == I .AND. JEL < 0) DMI=DMI*HALF    ! le triangle est traite deux fois
                     DQI(1)=-DMI*DATAINJ(3,IINJ)*NRX
                     DQI(2)=-DMI*DATAINJ(3,IINJ)*NRY
                     DQI(3)=-DMI*DATAINJ(3,IINJ)*NRZ
C inj. enthalpy                     DEI=DMI*DATAINJ(4,IINJ)/DATAINJ(5,IINJ)
                     DEI=DMI*DATAINJ(4,IINJ)
C
                     DM(II)=DM(II)+DMI
                     DQ(1,II)=DQ(1,II)+DQI(1)
                     DQ(2,II)=DQ(2,II)+DQI(2)
                     DQ(3,II)=DQ(3,II)+DQI(3)
                     DE(II)=DE(II)+DEI
C gas mixture
                     DMCPA(II)=DMCPA(II)+DMI*RBAGJET(2,IINJ)
                     DMCPB(II)=DMCPB(II)+DMI*RBAGJET(3,IINJ)
                     DMCPC(II)=DMCPC(II)+DMI*RBAGJET(4,IINJ)
                     DMRMW(II)=DMRMW(II)+DMI*RBAGJET(1,IINJ)
                     IF(ITYP == 8) THEN
                       DMCPD(II)=DMCPD(II)+DMI*RBAGJET(16,IINJ)
                       DMCPE(II)=DMCPE(II)+DMI*RBAGJET(17,IINJ)
                       DMCPF(II)=DMCPF(II)+DMI*RBAGJET(18,IINJ)
                     ENDIF
C
                     ELFMASS(IEL)=ELFMASS(IEL)+DMI*DT1
                     ELFEHPY(IEL)=ELFEHPY(IEL)+DEI*DT1
                     ELFVEL(IEL) =-DATAINJ(3,IINJ)
                  ELSEIF (ITAGEL(IEL) < 0) THEN
C Vent hole
                     IVENT  =-ITAGEL(IEL)
                     IDEF   =IBAGHOL(1,IVENT)
                     ITVENT =IBAGHOL(10,IVENT)
C
                     NX=NORM(1,IEL)
                     NY=NORM(2,IEL)
                     NZ=NORM(3,IEL)
                     AOUT=ELSOUT(IEL)*AREA/ELAREA(IEL)
                     P1=PPOLH(I)
                     RHO1=RPOLH(I)
                     RE1=EPOLH(I)/PVOLU(I)
                     GAMA1=GPOLH(I)
                     UX1=PU(1,I)
                     UY1=PU(2,I)
                     UZ1=PU(3,I)
                     UU =ZERO
C LOCAL VELOCITY
                     IF (ITVENT == 1) THEN
                        UU=NX*UX1+NY*UY1+NZ*UZ1
                        RHO2=RHO1
                        IF(P1 < PEXT) UU=ZERO
C BERNOULLI
                     ELSEIF ((ITVENT==2.AND.P1>ZERO).OR.(ITVENT==4.AND.P1>=PEXT)) THEN
                        VV=NX*UX1+NY*UY1+NZ*UZ1
                        VV=MAX(VV,ZERO)
                        ETA=(GAMA1-ONE)/GAMA1
                        KSI=ONE/ETA
                        PSTAG=P1*(ONE+HALF*ETA*RHO1*VV*VV/P1)**KSI
                        PCRIT=PSTAG*(TWO/(GAMA1+ONE))**KSI
                        P2 = MAX(PEXT,PCRIT)
                        RHO2 =RHO1*(P2/P1)**(ONE/GAMA1)
                        UU=TWO*P1*(ONE-(P2/P1)**ETA)/(RHO1*ETA)
                        UU=MAX(UU,ZERO)
                        UU=SQRT(UU)
                        VMAX=HALF*((P1-PEXT)*PVOLU(I)/(GAMA1-ONE))
     .                      /MAX(EM20,RHO2*AOUT*DT1*GAMA1*RE1/RHO1)
                        VMAX=MAX(VMAX,ZERO)
                        UU=MIN(UU,VMAX)
C CHEMKIN
                     ELSEIF (ITVENT == 3) THEN
                        P2=P1-PEXT
                        IVDP=IBAGHOL(9,IVENT)
                        FVDP=RBAGHOL(13,IVENT)
                        UU=FVDP*GET_U_FUNC(IVDP,P2*SCALP,DERI)
                        IF(P2 < ZERO) THEN
                          RHO2=RVOLU(62)
                        ELSE
                          RHO2=RHO1
                        ENDIF
C BERNOULLI with flow-in
                     ELSEIF (ITVENT==4.AND.P1<PEXT) THEN
                        GAMAI=RVOLU(1)
                        RHOI=RVOLU(62)
                        ETA=(GAMAI-ONE)/GAMAI
                        PCRIT=PEXT*(TWO/(GAMAI+ONE))**(ONE/ETA)
                        P2 = MAX(P1,PCRIT)
                        RHO2 =RHOI*(P2/PEXT)**(ONE/GAMAI)
                        UU=TWO*PEXT*(ONE-(P2/PEXT)**ETA)/(RHOI*ETA)
                        UU=MAX(UU,ZERO)
                        UU=SQRT(UU)
                        VMAX=HALF*((PEXT-P1)*PVOLU(I)/(GAMA1-ONE))
     .                      /MAX(EM20,RHO2*AOUT*DT1*RVOLU(63))
                        VMAX=MAX(VMAX,ZERO)
                        UU=MIN(UU,VMAX)
                        UU=-UU
C GRAEFE
                     ELSEIF (ITVENT==5) THEN
                        P2=P1-PEXT
                        RHO2=RHO1
                        UU=TWO*P2/RHO2
                        UU=MAX(UU,ZERO)
                        UU=SQRT(UU)
C
                     ENDIF
C
                     IF (UU > ZERO.AND.IDEF == 1) THEN
                        DMOUT=UU*RHO2*AOUT
                        DM(I)=DM(I)-DMOUT
                        DQ(1,I)=DQ(1,I)-DMOUT*UX1
                        DQ(2,I)=DQ(2,I)-DMOUT*UY1
                        DQ(3,I)=DQ(3,I)-DMOUT*UZ1
                        DHOUT=DMOUT*GAMA1*RE1/RHO1
                        DE(I)=DE(I)-DHOUT
                        !DMINI(I)=DMINI(I)-DMOUT
                        CPAI=CPAPOLH(I)
                        CPBI=CPBPOLH(I)
                        CPCI=CPCPOLH(I)
                        RMWI=RMWPOLH(I)
                        DMCPA(I)=DMCPA(I)-DMOUT*CPAI
                        DMCPB(I)=DMCPB(I)-DMOUT*CPBI
                        DMCPC(I)=DMCPC(I)-DMOUT*CPCI
                        DMRMW(I)=DMRMW(I)-DMOUT*RMWI
                        IF(ITYP == 8) THEN
                          CPDI=CPDPOLH(I)
                          CPEI=CPEPOLH(I)
                          CPFI=CPFPOLH(I)
                          DMCPD(I)=DMCPD(I)-DMOUT*CPDI
                          DMCPE(I)=DMCPE(I)-DMOUT*CPEI
                          DMCPF(I)=DMCPF(I)-DMOUT*CPFI
                        ENDIF
                        ELFMASS(IEL)=ELFMASS(IEL)-DMOUT*DT1
                        ELFEHPY(IEL)=ELFEHPY(IEL)-DHOUT*DT1
                        ELFVEL(IEL)=ELFVEL(IEL)+UU*AREA/ELAREA(IEL)
                        ELDMASS(IEL)=-DMOUT
                        ELDEHPY(IEL)=-DHOUT
                     ENDIF
C
                     IF (UU < ZERO.AND.IDEF == 1) THEN
                        DMI=-UU*RHO2*AOUT
                        DM(I)=DM(I)+DMI
                        DQ(1,I)=DQ(1,I)+DMI*UX1
                        DQ(2,I)=DQ(2,I)+DMI*UY1
                        DQ(3,I)=DQ(3,I)+DMI*UZ1
                        CPAI=RVOLU(7)
                        CPBI=RVOLU(8)
                        CPCI=RVOLU(9)
                        RMWI=RVOLU(10)
                        DMCPA(I)=DMCPA(I)+DMI*CPAI
                        DMCPB(I)=DMCPB(I)+DMI*CPBI
                        DMCPC(I)=DMCPC(I)+DMI*CPCI
                        DMRMW(I)=DMRMW(I)+DMI*RMWI
                        IF(ITYP == 8) THEN
                          CPDI=RVOLU(56)
                          CPEI=RVOLU(57)
                          CPFI=RVOLU(58)
                          DMCPD(I)=DMCPD(I)+DMI*CPDI
                          DMCPE(I)=DMCPE(I)+DMI*CPEI
                          DMCPF(I)=DMCPF(I)+DMI*CPFI
                        ENDIF
                        DE(I)=DE(I)+DMI*RVOLU(63)
                        ELFMASS(IEL)=ELFMASS(IEL)+DMI*DT1
                        ELFEHPY(IEL)=ELFEHPY(IEL)+DMI*DT1*RVOLU(63)
                        ELFVEL(IEL) =ELFVEL(IEL) +UU*AREA/ELAREA(IEL)
                        ELDMASS(IEL)=DMI
                        ELDEHPY(IEL)=DMI*RVOLU(63)
                     ENDIF
                  ENDIF
               ELSE
C----------------------------------------------------------------
C internal triangle is not an injector => communication between polyhedra
C----------------------------------------------------------------
                  NV = 0
                  IF (IFVTRI(5,KK) == I) THEN
                     NV=IFVTRI(6,KK)
                     NX=PNORM(1,KK)
                     NY=PNORM(2,KK)
                     NZ=PNORM(3,KK)
                  ELSEIF (IFVTRI(6,KK) == I) THEN
                     NV=IFVTRI(5,KK)
                     NX=-PNORM(1,KK)
                     NY=-PNORM(2,KK)
                     NZ=-PNORM(3,KK)
                  ENDIF
                  !check if adjacent polyhedron already updated
                  IF (NV  <=  I) GOTO 100
                  RHO1=RPOLH(I)
                  RE1=EPOLH(I)/PVOLU(I)
                  UX1=PU(1,I)
                  UY1=PU(2,I)
                  UZ1=PU(3,I)
                  GAMA1=GPOLH(I)
                  RHO2=RPOLH(NV)
                  RE2=EPOLH(NV)/PVOLU(NV)
                  UX2=PU(1,NV)
                  UY2=PU(2,NV)
                  UZ2=PU(3,NV)
                  GAMA2=GPOLH(NV)
C
                  VFX=HALF*(UX1+UX2)                  
                  VFY=HALF*(UY1+UY2)
                  VFZ=HALF*(UZ1+UZ2)
                  IF(JEL == 0) THEN
C Permeabilite du triangle interne
                    HM1=HMIN(KK)
                    H1 = ONE-RVOLU(51)/HM1
                    IF(H1 > 0) THEN 
                      AREA=AREA*H1
                    ELSE
                      AREA = ZERO
                    ENDIF

C                   AREA=AREA*MAX(ZERO,ONE-RVOLU(51)/HM1)
                  ELSE
C Triangle appuy sur structure interne => flux fonction de la porosite
                    IF(POROSITY(IEL-NEL) == ZERO) GOTO 50
                    AREA=AREA*POROSITY(IEL-NEL)
                  ENDIF
C
                  SS=NX*(VFX-VGRID(1,KK))+NY*(VFY-VGRID(2,KK))
     .              +NZ*(VFZ-VGRID(3,KK))
C Full upwind
                  IF (SS <= ZERO) THEN
                     ALPHA=ZERO
                  ELSE
                     ALPHA=ONE
                  ENDIF
C
                  RHOM=ALPHA*RHO1+(ONE-ALPHA)*RHO2
                  RUXM=ALPHA*RHO1*UX1+(ONE-ALPHA)*RHO2*UX2
                  RUYM=ALPHA*RHO1*UY1+(ONE-ALPHA)*RHO2*UY2
                  RUZM=ALPHA*RHO1*UZ1+(ONE-ALPHA)*RHO2*UZ2
                  REM=ALPHA*GAMA1*RE1+(ONE-ALPHA)*GAMA2*RE2
C
                  MASSFLOW = RHOM*SS*AREA
                  DM(I)=DM(I)-MASSFLOW
                  DQ(1,I)=DQ(1,I)-RUXM*SS*AREA
                  DQ(2,I)=DQ(2,I)-RUYM*SS*AREA
                  DQ(3,I)=DQ(3,I)-RUZM*SS*AREA
                  DE(I)=DE(I)-REM*SS*AREA
                  DM(NV)=DM(NV)+MASSFLOW
                  DQ(1,NV)=DQ(1,NV)+RUXM*SS*AREA
                  DQ(2,NV)=DQ(2,NV)+RUYM*SS*AREA
                  DQ(3,NV)=DQ(3,NV)+RUZM*SS*AREA
                  DE(NV)=DE(NV)+REM*SS*AREA
                  CPAM = ALPHA * CPAPOLH(I) + (ONE - ALPHA) * CPAPOLH(NV)
                  CPBM = ALPHA * CPBPOLH(I) + (ONE - ALPHA) * CPBPOLH(NV)
                  CPCM = ALPHA * CPCPOLH(I) + (ONE - ALPHA) * CPCPOLH(NV)
                  RMWM = ALPHA * RMWPOLH(I) + (ONE - ALPHA) * RMWPOLH(NV)
                  DMCPA(I) = DMCPA(I) - MASSFLOW * CPAM
                  DMCPA(NV) = DMCPA(NV) + MASSFLOW * CPAM
                  DMCPB(I) = DMCPB(I) - MASSFLOW * CPBM
                  DMCPB(NV) = DMCPB(NV) + MASSFLOW * CPBM
                  DMCPC(I) = DMCPC(I) - MASSFLOW * CPCM
                  DMCPC(NV) = DMCPC(NV) + MASSFLOW * CPCM
                  DMRMW(I) = DMRMW(I) - MASSFLOW * RMWM
                  DMRMW(NV) = DMRMW(NV) + MASSFLOW * RMWM
                  IF (ITYP == 8) THEN
                     CPDM = ALPHA * CPDPOLH(I) + (ONE - ALPHA) * CPDPOLH(NV)
                     CPEM = ALPHA * CPEPOLH(I) + (ONE - ALPHA) * CPEPOLH(NV)
                     CPFM = ALPHA * CPFPOLH(I) + (ONE - ALPHA) * CPFPOLH(NV)
                     DMCPD(I) = DMCPD(I) - MASSFLOW * CPDM
                     DMCPD(NV) = DMCPD(NV) + MASSFLOW * CPDM
                     DMCPE(I) = DMCPE(I) - MASSFLOW * CPEM
                     DMCPE(NV) = DMCPE(NV) + MASSFLOW * CPEM
                     DMCPF(I) = DMCPF(I) - MASSFLOW * CPFM
                     DMCPF(NV) = DMCPF(NV) + MASSFLOW * CPFM
                  ENDIF
C
                  IF(JEL < 0) THEN
                     ELFMASS(IEL)=ELFMASS(IEL)+MASSFLOW*DT1
                     ELFVEL(IEL) =ELFVEL(IEL) +SS*AREA/ELAREA(IEL)
                  ENDIF
C gas mixture
c$$$                  IF (SS > ZERO) THEN
c$$$                     DMINI(I)=DMINI(I)-MASSFLOW
c$$$                     DMCPA(NV)=DMCPA(NV)+MASSFLOW*CPAPOLH(I)
c$$$                     DMCPB(NV)=DMCPB(NV)+MASSFLOW*CPBPOLH(I)
c$$$                     DMCPC(NV)=DMCPC(NV)+MASSFLOW*CPCPOLH(I)
c$$$                     DMRMW(NV)=DMRMW(NV)+MASSFLOW*RMWPOLH(I)
c$$$                  ELSE
c$$$                     DMINI(NV)=DMINI(NV)+MASSFLOW
c$$$                     DMCPA(I)=DMCPA(I)-MASSFLOW*CPAPOLH(NV)
c$$$                     DMCPB(I)=DMCPB(I)-MASSFLOW*CPBPOLH(NV)
c$$$                     DMCPC(I)=DMCPC(I)-MASSFLOW*CPCPOLH(NV)
c$$$                     DMRMW(I)=DMRMW(I)-MASSFLOW*RMWPOLH(NV)
c$$$                  ENDIF
c$$$                  IF(ITYP == 8) THEN
c$$$                    IF (SS > ZERO) THEN
c$$$                       DMCPD(NV)=DMCPD(NV)+MASSFLOW*CPDPOLH(I)
c$$$                       DMCPE(NV)=DMCPE(NV)+MASSFLOW*CPEPOLH(I)
c$$$                       DMCPF(NV)=DMCPF(NV)+MASSFLOW*CPFPOLH(I)
c$$$                    ELSE
c$$$                       DMCPD(I)=DMCPD(I)-MASSFLOW*CPDPOLH(NV)
c$$$                       DMCPE(I)=DMCPE(I)-MASSFLOW*CPEPOLH(NV)
c$$$                       DMCPF(I)=DMCPF(I)-MASSFLOW*CPFPOLH(NV)
c$$$                    ENDIF
c$$$                  ENDIF
               ENDIF
   50       ENDDO  ! next triangle
  100    ENDDO     ! next polygon
      ENDDO        ! next polyhedron
C------------------------
C

!$OMP PARALLEL PRIVATE(I,J,JJ,MSINI,CPA,CPB,CPC,CPD,CPE,CPF,EFAC,TEMP0)
!$OMP+ PRIVATE(NTRIA,NTRIA_TOT,NPOLYG,WSURF,SSP1,SSP2,SSPt)
!$OMP+ PRIVATE(TEMP,CP,CV,RMW,Denom,Denom_max,VMAT,KK, AREA,IEL1,IEL2)
!$OMP+ PRIVATE(AL,DD,AD,GAMA,SSP,QX,VV,TMP_VEC)
!$OMP+ PRIVATE(ITYPL)

      ITYPL = ITYP
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NPOLH
         ITAGP(I)=0
         MSINI=MPOLH(I)
         MPOLH(I)=MPOLH(I)+DM(I)*DT1
         QPOLH(1,I)=QPOLH(1,I)+DQ(1,I)*DT1
         QPOLH(2,I)=QPOLH(2,I)+DQ(2,I)*DT1
         QPOLH(3,I)=QPOLH(3,I)+DQ(3,I)*DT1
         EPOLH(I)=EPOLH(I)+DE(I)*DT1
         DQ(1,I)=ZERO
         DQ(2,I)=ZERO
         DQ(3,I)=ZERO
         DE(I)=ZERO
         IF(MPOLH(I) <= ZERO.OR.EPOLH(I) <= ZERO) THEN
           TPOLH(I)=ZERO
         ELSE
C gas mixture
           MSINI=MSINI+DMINI(I)*DT1
           CPA=(MSINI*CPAPOLH(I)+DMCPA(I)*DT1)/MPOLH(I)
           CPB=(MSINI*CPBPOLH(I)+DMCPB(I)*DT1)/MPOLH(I)
           CPC=(MSINI*CPCPOLH(I)+DMCPC(I)*DT1)/MPOLH(I)
           RMW=(MSINI*RMWPOLH(I)+DMRMW(I)*DT1)/MPOLH(I)
           EFAC=EPOLH(I)/MPOLH(I)
           IF(ITYP == 8) THEN
             CPD=(MSINI*CPDPOLH(I)+DMCPD(I)*DT1)/MPOLH(I)
             CPE=(MSINI*CPEPOLH(I)+DMCPE(I)*DT1)/MPOLH(I)
             CPF=(MSINI*CPFPOLH(I)+DMCPF(I)*DT1)/MPOLH(I)
           ENDIF
C-------------------------
C Temperature
C-------------------------
           TEMP0=RVOLU(25)
           TEMP = TEMP0
           CALL FVTEMP(ITYPL , EFAC , CPA  , CPB  , CPC  ,
     .                 CPD   , CPE  , CPF  , RMW  , TEMP0,
     .                 TEMP  )
           CP=CPA+CPB*TEMP+CPC*TEMP*TEMP
           CPAPOLH(I)=CPA
           CPBPOLH(I)=CPB
           CPCPOLH(I)=CPC
           IF(ITYP == 8) THEN
             CPDPOLH(I)=CPD
             CPEPOLH(I)=CPE
             CPFPOLH(I)=CPF
             CP=CP+CPD*TEMP**3+CPF*TEMP**4
             IF(CPE /= ZERO .AND. TEMP > ZERO) THEN
                CP=CP+CPE/(TEMP*TEMP)
             ENDIF
           ENDIF
           CV=CP-RMW
           RMWPOLH(I)=RMW
           GPOLH(I)=CP/CV
           TPOLH(I)=TEMP
C---------------------------------------------------
C Pressure & Density
C---------------------------------------------------
           RPOLH(I)=MPOLH(I)/PVOLU(I)
           PPOLH(I)=RPOLH(I)*RMW*TEMP
         ENDIF
      ENDDO
!$OMP END DO NOWAIT

C---------------------------------------------------
C TIME STEP & ARTIFICIAL VISCOSITY
C---------------------------------------------------
      DTX=EP30
      PHDT=0
      QA=RVOLU(46)
      QB=RVOLU(47)


      ! /DT/FVMBAG/[ID_DT_OPTION]
      !   ID_DT_OPTION = 0 obsolete
      !   ID_DT_OPTION = 1 (legacy method), apply to all FVMBAG1
      !   ID_DT_OPTION = 2 (2022.3), apply to given FVMBAG1 or FVMBAG2. 
      !                              in this case characteristic length is calculated depending on L_TYPE flag
      !                              and smoothing is applied :  dt <- min(dt,  dt_old*(1+lambda))
                   !--------!-------------------------------------------------------------------!
                   ! VALUE  !   DESCRIPTION                                                     !
      !------------!--------!-------------------------------------------------------------------!
      !            ! 0,1    !    Constant (Starter minimum characteristic length)                !
      !  L_TYPE    !  2     !    VOLUME^1/3 at current time                                     !
      !            !  3     !    VOLUME / SMAX at current time                                  !
      !------------!--------!-------------------------------------------------------------------!
      

      !------------------------------!
      IDT_FVMBAG = FVDATA(IFV)%ID_DT_OPTION  ! /DT/FVMBAG/[IDT_FVMBAG]
               
      SELECT CASE(IDT_FVMBAG)

        ! /DT/FVMBAG/1 
        CASE(0,1)
        ! ------------ 
          PARAM_L_TYPE = 0

!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)        
          DO I=1,NPOLH
             IF (IBPOLH(I) == 0 .AND.IDTMIN(52) < 2) THEN
               DLS(I)=DLH ! constant length calculated during starter
             ELSE
               DLS(I)=PVOLU(I)**THIRD
             ENDIF
          ENDDO
!$OMP END DO          

        ! /DT/FVMBAG/2
        CASE(2) 
        ! ------------    
          PARAM_L_TYPE = FVDATA(IFV)%L_TYPE
              
          SELECT CASE(PARAM_L_TYPE)

            ! /DT/FVMBAG/2 +L_TYPE=0,1          
            CASE(0,1) !dl = starter constant
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
              DO I=1,NPOLH
                DLS(I)=DLH 
              ENDDO
!$OMP END DO         

            ! /DT/FVMBAG/2 +L_TYPE=2 
            CASE(2) ! dl=VOL**1/3
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
              DO I=1,NPOLH
                DLS(I)=PVOLU(I)**THIRD  
              ENDDO
!$OMP END DO          

            ! /DT/FVMBAG/2 +L_TYPE=3
            CASE(3) 
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
              DO I=1,NPOLH
                DLS(I)=PVOLU(I)
              ENDDO
!$OMP END DO

          END SELECT!PARAM_L_TYPE


      END SELECT!IDT_FVMBAG

      !------------------------------!
      ! ---      TIME STEP        ---!
      !------------------------------!
      SELECT CASE (IDT_FVMBAG)   ! IDT_FVMBAG=FVDATA(IFV)%ID_DT_OPTION

        CASE(0,1)  !/DT/FVMBAG/0 & /DT/FVMBAG/1
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
         DO I=1,NPOLH
           IF(MPOLH(I)<=ZERO.OR.EPOLH(I)<=ZERO.OR.GPOLH(I)<=ONE) THEN
             DTPOLH(I)=EP30
             QVISC(I) =ZERO
           ELSE 
             AL=PVOLU(I)**THIRD
             DD=DM(I)/MPOLH(I)
             AD=MAX(ZERO,DD)
             GAMA=GPOLH(I)
             SSP=SQRT((GAMA-ONE)*GAMA*EPOLH(I)/MPOLH(I))
             QVISC(I)=RPOLH(I)*AD*AL*(QA*QA*AL*AD+QB*SSP)
             QX=QB*SSP+AL*QA*QA*AD
             SSP=QX+SQRT(QX*QX+SSP*SSP)
             VV=SQRT(PU(1,I)**2+PU(2,I)**2+PU(3,I)**2)
             DTPOLH(I)=CFL_COEF*DLS(I)/(SSP+VV)
           ENDIF
         ENDDO
!$OMP END DO

        CASE(2) ! !/DT/FVMBAG/2 
          PARAM_L_TYPE = FVDATA(IFV)%L_TYPE !characteristic length formulation
        
          IF(PARAM_L_TYPE /= 3)THEN !L_TYPE=0/1 & 2
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
            DO I=1,NPOLH
               IF(MPOLH(I) <= ZERO.OR.EPOLH(I) <= ZERO.OR.GPOLH(I) <= ONE) THEN
                  DTPOLH(I)=EP30
                  QVISC(I) =ZERO
               ELSE
                 ! 1.calculating Grid Velocity for each polyhedron
                 PW(1:3,I)=ZERO
                 NPOLYG = IFVPADR(I+1) - IFVPADR(I)
                 NTRIA_TOT = ZERO
                 !loop over polygons
                 DO J=IFVPADR(I),IFVPADR(I+1)-1
                    JJ=IFVPOLH(J)
                    NTRIA = IFVTADR(JJ+1) - IFVTADR(JJ)
                    WSURF(1:3)=ZERO
                    NTRIA_TOT = NTRIA_TOT + NTRIA
                    !loop over triangles
                    DO K=IFVTADR(JJ),IFVTADR(JJ+1)-1
                       KK  =IFVPOLY(K)
                       IF(KK>0)THEN
                         WSURF(1)=WSURF(1)+VGRID(1,KK)
                         WSURF(2)=WSURF(2)+VGRID(2,KK)
                         WSURF(3)=WSURF(3)+VGRID(3,KK)
                       ENDIF
                    ENDDO! next K (triangle)
                    PW(1,I)=PW(1,I)+WSURF(1)
                    PW(2,I)=PW(2,I)+WSURF(2)
                    PW(3,I)=PW(3,I)+WSURF(3)
                 ENDDO! next J (polygon)
                 !PW:polyhedron mean value over all triangles
                 PW(1,I)=PW(1,I)/NTRIA_TOT
                 PW(2,I)=PW(2,I)/NTRIA_TOT
                 PW(3,I)=PW(3,I)/NTRIA_TOT
                 ! 2.artificial viscosity
                 AL=PVOLU(I)**THIRD
                 DD=DM(I)/MPOLH(I)
                 AD=MAX(ZERO,DD)
                 GAMA=GPOLH(I)
                 SSP=SQRT((GAMA-ONE)*GAMA*EPOLH(I)/MPOLH(I))
                 QVISC(I)=RPOLH(I)*AD*AL*(QA*QA*AL*AD+QB*SSP)
                 QX=QB*SSP+AL*QA*QA*AD
                 SSP=QX+SQRT(QX*QX+SSP*SSP)
                 ! 3.relative velocty (PU:material velocity : PW:grid velocity)
                 TMP_VEC(1)=PU(1,I)-PW(1,I)
                 TMP_VEC(2)=PU(2,I)-PW(2,I)
                 TMP_VEC(3)=PU(3,I)-PW(3,I)
                 VV=SQRT(TMP_VEC(1)*TMP_VEC(1)+TMP_VEC(2)*TMP_VEC(2)+TMP_VEC(3)*TMP_VEC(3))
                 ! 4.time step
                 DTPOLH(I)=CFL_COEF*DLS(I)/(SSP+VV)
               ENDIF
            ENDDO
!$OMP END DO

          ELSE !L_TYPE=3
           
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
             DO I=1,NPOLH
                IF(MPOLH(I) <= ZERO.OR.EPOLH(I) <= ZERO.OR.GPOLH(I) <= ONE) THEN
                   DTPOLH(I)=EP30
                   QVISC(I) =ZERO
                ELSE
                  ! 1.calculating Grid Velocity for each polyhedron
                  PW(1:3,I)=ZERO
                  Denom_max = ZERO
                  DO J=IFVPADR(I),IFVPADR(I+1)-1
                     JJ=IFVPOLH(J)
                     NTRIA = IFVTADR(JJ+1) - IFVTADR(JJ)
                     WSURF(1:3)=ZERO
                     DO K=IFVTADR(JJ),IFVTADR(JJ+1)-1
                        KK  =IFVPOLY(K)
                        AREA=PAREA(KK)
                        IEL1=IFVTRI(5, KK)
                        IEL2=IFVTRI(6, KK)
                        IF(IEL2==0)IEL2=I !boundary surface
                        IF(IEL1==0)IEL1=I
                        VMAT(1) = HALF*(PU(1,IEL1)+PU(1,IEL2))
                        VMAT(2) = HALF*(PU(2,IEL1)+PU(2,IEL2))
                        VMAT(3) = HALF*(PU(3,IEL1)+PU(3,IEL2))
                        GAMA=GPOLH(I)                     
                        SSP1= SQRT((GAMA-ONE)*GAMA*EPOLH(IEL1)/MPOLH(IEL1))
                        SSP2= SQRT((GAMA-ONE)*GAMA*EPOLH(IEL2)/MPOLH(IEL2))
                        SSPt = HALF*(SSP1+SSP2)
                        TMP_VEC(1) = (VMAT(1)-VGRID(1,KK))*PNORM(1,KK)
                        TMP_VEC(2) = (VMAT(2)-VGRID(2,KK))*PNORM(2,KK)
                        TMP_VEC(3) = (VMAT(3)-VGRID(3,KK))*PNORM(3,KK)
                        Denom = AREA*(SQRT(TMP_VEC(1)*TMP_VEC(1)+TMP_VEC(2)*TMP_VEC(2)+TMP_VEC(3)*TMP_VEC(3)) + SSPt)
                        Denom_max = MAX(Denom, Denom_max)
                     ENDDO! next K (triangle)
                  ENDDO! next J (polygon)
                  ! 2.artificial viscosity
                  AL=PVOLU(I)**THIRD
                  DD=DM(I)/MPOLH(I)
                  AD=MAX(ZERO,DD)
                  GAMA=GPOLH(I)
                  SSP=SQRT((GAMA-ONE)*GAMA*EPOLH(I)/MPOLH(I))
                  QVISC(I)=RPOLH(I)*AD*AL*(QA*QA*AL*AD+QB*SSP)
                  QX=QB*SSP+AL*QA*QA*AD
                  SSP=QX+SQRT(QX*QX+SSP*SSP)
                  ! 3.time step
                  DTPOLH(I)=CFL_COEF*DLS(I)/(denom_max)
                ENDIF
             ENDDO
!$OMP END DO
           
          ENDIF


      END SELECT


!$OMP END PARALLEL

      ! FVMBAG minimal time step
      DO I=1,NPOLH
         IF (DTPOLH(I) < DTX) THEN
            DTX=DTPOLH(I)
            PHDT=I
         ENDIF
      ENDDO
      
      !optional time step growth limitor (/DT/FVMBAG/2, lambda parameter)
      IF(NCYCLE > 0 .AND. IDT_FVMBAG == 2)THEN
        PARAM_LAMBDA =  FVDATA(IFV)%LAMBDA
        IF(PARAM_LAMBDA > ZERO)THEN
          DTOLD = FVDATA(IFV)%DTOLD
          IF(DTOLD > ZERO)THEN        
            DTX = MIN( DTX, DTOLD*(ONE+PARAM_LAMBDA))
          ENDIF
        ENDIF
      ENDIF
      FVDATA(IFV)%DTOLD = DTX

      IF (ILVOUT >= 1.AND.IPRI == 0) THEN
         WRITE(IOUT,'(A,I10,A,E12.4,A,I10)')' ** MONITORED VOLUME ID: ',IVOLU(1),' TIME STEP:',DTX,' FINITE VOLUME:',IDPOLH(PHDT)
      ENDIF

      IF (DTX < DT2) THEN
         DT2=DTX
         NELTS=IVOLU(1)
         ITYPTS=52
      ENDIF
C---------------------------------------------------
C Ibntegration of pressure gradient and surfacic forces
C---------------------------------------------------
!$OMP PARALLEL PRIVATE(N1,N2,N3,IEL,NX,NY,NZ)
!$OMP+ PRIVATE(VX1,VX2,VX3,VY1,VY2,VY3,VZ1,VZ2,VZ3)
!$OMP+ PRIVATE(VVX,VVY,VVZ)
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO IEL=1,NELT
         N1=ELEM(1,IEL)
         N2=ELEM(2,IEL)
         N3=ELEM(3,IEL)
         VX1=VVV(1,N1)
         VY1=VVV(2,N1)
         VZ1=VVV(3,N1)
         VX2=VVV(1,N2)
         VY2=VVV(2,N2)
         VZ2=VVV(3,N2)
         VX3=VVV(1,N3)
         VY3=VVV(2,N3)
         VZ3=VVV(3,N3)
         VVX=THIRD*(VX1+VX2+VX3)
         VVY=THIRD*(VY1+VY2+VY3)
         VVZ=THIRD*(VZ1+VZ2+VZ3)
         NX=NORM(1,IEL)
         NY=NORM(2,IEL)
         NZ=NORM(3,IEL)
         VEL(IEL)=NX*VVX+NY*VVY+NZ*VVZ
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
      DO I=1,NPOLH
         IF (MPOLH(I) <= ZERO.OR.EPOLH(I) <= ZERO) CYCLE
         DO J=IFVPADR(I),IFVPADR(I+1)-1
            JJ=IFVPOLH(J)
            DO K=IFVTADR(JJ),IFVTADR(JJ+1)-1
               KK=IFVPOLY(K)
               IEL=IFVTRI(4,KK)
               AREA=PAREA(KK)
               IF (AREA == ZERO) CYCLE
               IF (IEL > 0) THEN
                  NX=NORM(1,IEL)
                  NY=NORM(2,IEL)
                  NZ=NORM(3,IEL)
                  IF (ITAGEL(IEL) < 0) THEN
C Vent hole
                     IVENT  =-ITAGEL(IEL)
                     IDEF   =IBAGHOL(1,IVENT)
                     IVENTYP=IBAGHOL(13,IVENT)
C
                     IF(IDEF==1.AND.IVENTYP==0) THEN
                       AOUT=ELSOUT(IEL)*AREA/ELAREA(IEL)
                       AREA=AREA-AOUT
                       PP=PEXT
                       FEL(IEL)=FEL(IEL)+PP*AOUT
                       DQ(1,I)=DQ(1,I)-PP*AOUT*NX
                       DQ(2,I)=DQ(2,I)-PP*AOUT*NY
                       DQ(3,I)=DQ(3,I)-PP*AOUT*NZ
                       DE(I)=DE(I)-PP*VEL(IEL)*AOUT
                     ENDIF
                  ENDIF
C Triangle on the airbag envelope
                  PP=PPOLH(I)+QVISC(I)
                  FEL(IEL)=FEL(IEL)+PP*AREA
                  DQ(1,I)=DQ(1,I)-PP*AREA*NX
                  DQ(2,I)=DQ(2,I)-PP*AREA*NY
                  DQ(3,I)=DQ(3,I)-PP*AREA*NZ
C Work of pressure forces in the airbag displacement
                  DE(I)=DE(I)-PP*VEL(IEL)*AREA
C Heat Loss or Gain (version 2018)
                  DE(I)=DE(I)-RVOLU(19)*AREA*(TPOLH(I)-RVOLU(25))
               ELSEIF(IEL == 0) THEN
C triangle permeability
                  AREA_OLD=AREA
                  AREA=AREA*MAX(ZERO,ONE-RVOLU(51)/HMIN(KK))
                  AREA1=AREA_OLD-AREA
C internal triangle = communication between polyhedra
                  NV = 0
                  IF (IFVTRI(5,KK) == I) THEN
                     NV=IFVTRI(6,KK)
                     NX=PNORM(1,KK)
                     NY=PNORM(2,KK)
                     NZ=PNORM(3,KK)
                  ELSEIF (IFVTRI(6,KK) == I) THEN
                     NV=IFVTRI(5,KK)
                     NX=-PNORM(1,KK)
                     NY=-PNORM(2,KK)
                     NZ=-PNORM(3,KK)
                  ENDIF
                  IF (NV < I .OR.MPOLH(NV) <= ZERO.OR.
     .                                  EPOLH(NV) <= ZERO) GOTO 200
                  P1=PPOLH(I)+QVISC(I)
                  P2=PPOLH(NV)+QVISC(NV)
                  PMEAN=HALF*(P1+P2)
C                 
                  DQ(1,I)=DQ(1,I)-PMEAN*NX*AREA-P1*NX*AREA1
                  DQ(2,I)=DQ(2,I)-PMEAN*NY*AREA-P1*NY*AREA1
                  DQ(3,I)=DQ(3,I)-PMEAN*NZ*AREA-P1*NZ*AREA1
                  DQ(1,NV)=DQ(1,NV)+PMEAN*NX*AREA+P2*NX*AREA1
                  DQ(2,NV)=DQ(2,NV)+PMEAN*NY*AREA+P2*NY*AREA1
                  DQ(3,NV)=DQ(3,NV)+PMEAN*NZ*AREA+P2*NZ*AREA1
C added FA
                  SS=NX*VGRID(1,KK)+NY*VGRID(2,KK)+NZ*VGRID(3,KK)
                  DE(I) =DE(I) -PMEAN*SS*AREA-P1*SS*AREA1
                  DE(NV)=DE(NV)+PMEAN*SS*AREA+P2*SS*AREA1
               ELSEIF(IEL < 0) THEN
C internal triangle on internal surface = communication between polyhedra
                  NV = 0
                  IF (IFVTRI(5,KK) == I) THEN
                     NV=IFVTRI(6,KK)
                     NX=PNORM(1,KK)
                     NY=PNORM(2,KK)
                     NZ=PNORM(3,KK)
                  ELSEIF (IFVTRI(6,KK) == I) THEN
                     NV=IFVTRI(5,KK)
                     NX=-PNORM(1,KK)
                     NY=-PNORM(2,KK)
                     NZ=-PNORM(3,KK)
                  ENDIF
                  IF (NV  <=  I .OR.MPOLH(NV) <= ZERO.OR.
     .                                  EPOLH(NV) <= ZERO) GOTO 200
                  P1=PPOLH(I)+QVISC(I)
                  P2=PPOLH(NV)+QVISC(NV)
                  JEL=-IEL
                  PMEAN=HALF*(P1+P2)
                  AREA_OLD=AREA
                  AREA=AREA*POROSITY(JEL-NEL)
                  AREA1=AREA_OLD-AREA
                  SS=NX*VGRID(1,KK)+NY*VGRID(2,KK)+NZ*VGRID(3,KK)
C
                  NRX=NORM(1,JEL)
                  NRY=NORM(2,JEL)
                  NRZ=NORM(3,JEL)
                  FAC=NX*NRX+NY*NRY+NZ*NRZ
C
                  IF(ITAGEL(JEL) > 0) THEN
                     IF(FAC < 0) THEN
                       FEL(JEL)=FEL(JEL)+(P2-P1)*AREA1
                       DQ(1,I)=DQ(1,I)-PMEAN*NX*AREA+P1*NRX*AREA1
                       DQ(2,I)=DQ(2,I)-PMEAN*NY*AREA+P1*NRY*AREA1
                       DQ(3,I)=DQ(3,I)-PMEAN*NZ*AREA+P1*NRZ*AREA1
                       DE(I)=DE(I)-PMEAN*SS*AREA+P1*VEL(JEL)*AREA1
                       DQ(1,NV)=DQ(1,NV)+PMEAN*NX*AREA-P2*NRX*AREA1
                       DQ(2,NV)=DQ(2,NV)+PMEAN*NY*AREA-P2*NRY*AREA1
                       DQ(3,NV)=DQ(3,NV)+PMEAN*NZ*AREA-P2*NRZ*AREA1
                       DE(NV)=DE(NV)+PMEAN*SS*AREA-P2*VEL(JEL)*AREA1
                     ELSE
                       FEL(JEL)=FEL(JEL)+(P1-P2)*AREA1
                       DQ(1,I)=DQ(1,I)-PMEAN*NX*AREA-P1*NRX*AREA1
                       DQ(2,I)=DQ(2,I)-PMEAN*NY*AREA-P1*NRY*AREA1
                       DQ(3,I)=DQ(3,I)-PMEAN*NZ*AREA-P1*NRZ*AREA1
                       DE(I)=DE(I)-PMEAN*SS*AREA-P1*VEL(JEL)*AREA1
                       DQ(1,NV)=DQ(1,NV)+PMEAN*NX*AREA+P2*NRX*AREA1
                       DQ(2,NV)=DQ(2,NV)+PMEAN*NY*AREA+P2*NRY*AREA1
                       DQ(3,NV)=DQ(3,NV)+PMEAN*NZ*AREA+P2*NRZ*AREA1
                       DE(NV)=DE(NV)+PMEAN*SS*AREA+P2*VEL(JEL)*AREA1
                     ENDIF
                  ELSE
                     IF(FAC < 0) THEN
                       FEL(JEL)=FEL(JEL)+(P2-P1)*AREA1
                     ELSE
                       FEL(JEL)=FEL(JEL)+(P1-P2)*AREA1
                     ENDIF
                     DQ(1,I)=DQ(1,I)-PMEAN*NX*AREA-P1*NX*AREA1
                     DQ(2,I)=DQ(2,I)-PMEAN*NY*AREA-P1*NY*AREA1
                     DQ(3,I)=DQ(3,I)-PMEAN*NZ*AREA-P1*NZ*AREA1
                     DE(I)=DE(I)-PMEAN*SS*AREA-P1*SS*AREA1
                     DQ(1,NV)=DQ(1,NV)+PMEAN*NX*AREA+P2*NX*AREA1
                     DQ(2,NV)=DQ(2,NV)+PMEAN*NY*AREA+P2*NY*AREA1
                     DQ(3,NV)=DQ(3,NV)+PMEAN*NZ*AREA+P2*NZ*AREA1
                     DE(NV)=DE(NV)+PMEAN*SS*AREA+P2*SS*AREA1
                  ENDIF
               ENDIF
            ENDDO
  200    ENDDO
      ENDDO

C---------------------------------------------------
C New Polyhedra quantities
C---------------------------------------------------
!$OMP PARALLEL PRIVATE(I)
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NPOLH
         QPOLH(1,I)=QPOLH(1,I)+DQ(1,I)*DT1
         QPOLH(2,I)=QPOLH(2,I)+DQ(2,I)*DT1
         QPOLH(3,I)=QPOLH(3,I)+DQ(3,I)*DT1
         EPOLH(I)=EPOLH(I)+DE(I)*DT1
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

C---------------------------------------------------
C Impressions
C---------------------------------------------------
 250  IF (ILVOUT >= 1 .AND. IPRI == 0) THEN
         WRITE(IOUT,*)
         WRITE(IOUT,'(A25,I10,A31)')' ** MONITORED VOLUME ID: ',IVOLU(1), ' - FINITE VOLUME MESH UPDATE **'
         WRITE(IOUT,'(A,I8)')'    NUMBER OF FINITE VOLUMES : ',NPOLH
         WRITE(IOUT,'(A,G16.9,A17,G16.9,A1)')'    SUM VOLUME FINITE VOLUMES : ',VOLPH,'    (VOLUME AIRBAG: ',VOLG,')'
         WRITE(IOUT,'(A,G16.9,A17,G16.9,A1)')'    SUM AREA SURFACE POLYGONS : ',AREAP,'    (AREA AIRBAG  : ',AREAEL,')'
         WRITE(IOUT,*)
      ENDIF
C---------------------------------------------------
C TH output
C---------------------------------------------------
      AMTOT=ZERO
      PMEAN=ZERO
      ENINT=ZERO
      FSAV(1:17)=ZERO
      DO I=1,NPOLH
         AMTOT=AMTOT+MPOLH(I)
         ENINT=ENINT+EPOLH(I)
         PMEAN=PMEAN+PPOLH(I)*PVOLU(I)
         GAMA=GPOLH(I)
         RMW=RMWPOLH(I)
         TEMP=TPOLH(I)
         CPA=CPAPOLH(I)
         CPB=CPBPOLH(I)
         CPC=CPCPOLH(I)
         CP=CPA+CPB*TEMP+CPC*TEMP*TEMP
         IF(ITYP == 8) THEN
           CPD=CPDPOLH(I)
           CPE=CPEPOLH(I)
           CPF=CPFPOLH(I)
           CP=CP+CPD*TEMP**3+CPF*TEMP**4
           IF(TEMP > ZERO) CP=CP+CPE/(TEMP*TEMP)
         ENDIF
         CV=CP-RMW
         FSAV(5) =FSAV(5) +MPOLH(I)*TEMP
         FSAV(10)=FSAV(10)+MPOLH(I)*CP
         FSAV(11)=FSAV(11)+MPOLH(I)*CV
         FSAV(12)=FSAV(12)+MPOLH(I)*GAMA
      ENDDO
C     Pressure dispersion
      PDISP = ZERO
      PMEAN2 = PMEAN / VOLPH
      DO I = 1, NPOLH
         PDISP = PDISP + PVOLU(I) * (PPOLH(I) - PMEAN2)**2
      ENDDO
      PDISP = SQRT(PDISP / VOLPH) / PMEAN2
      FSAV(19) = PDISP
      INJECTION_STARTED = .FALSE.
      DO IINJ = 1, NJET
         INJECTION_STARTED = INJECTION_STARTED .OR. (DATAINJ(2, IINJ)  >  ZERO)
      ENDDO
      IF (.NOT. INJECTION_STARTED) THEN
         PDISP = ZERO
      ENDIF

C
      FSAV(1)=AMTOT
      FSAV(2)=VOLPH
      PRES_AV = PMEAN / VOLPH
      FSAV(3) = PRES_AV
      FSAV(4)=AREAP
      FSAV(7)=ZERO
      DMOUT  =ZERO
      DHOUT  =ZERO
      CP_AV = FSAV(10) / AMTOT
      FSAV(10) = CP_AV
      CV_AV = FSAV(11) / AMTOT
      FSAV(11) = CV_AV
      FSAV(12) = CP_AV / CV_AV
      FSAV(14) = NPOLH
      RGAS_AV = CP_AV - CV_AV
      RHO_AV = AMTOT / VOLPH
      FSAV(5) = FSAV(5) / AMTOT ! sum(mass[i]*T[i])/total_mass    

      IF(IVOLU(39) == 0) THEN
        FSAV(6) =ZERO
        FSAV(13)=ZERO
      ELSE
        FSAV(6) =AOUTOT
        DO IEL=1,NEL
           IF (ITAGEL(IEL) < 0) THEN
             IVENT=-ITAGEL(IEL)
             RBAGHOL(18,IVENT)=RBAGHOL(18,IVENT)+ELFVEL(IEL)*ELSOUT(IEL)
             RBAGHOL(19,IVENT)=RBAGHOL(19,IVENT)-ELFMASS(IEL)
             RBAGHOL(20,IVENT)=RBAGHOL(20,IVENT)-ELFEHPY(IEL)
             RBAGHOL(21,IVENT)=RBAGHOL(21,IVENT)+ELDMASS(IEL)
             RBAGHOL(22,IVENT)=RBAGHOL(22,IVENT)+ELDEHPY(IEL)
           ENDIF
        ENDDO
        DO IVENT=1,NVENT
           SVENTOT=RBAGHOL(16,IVENT)+RBAGHOL(17,IVENT)
           IF(SVENTOT <= ZERO) CYCLE
           RBAGHOL(18,IVENT)=RBAGHOL(18,IVENT)/SVENTOT
        ENDDO
        DO IVENT=1,NVENT
           FSAV(7)=FSAV(7)+(RBAGHOL(16,IVENT)+RBAGHOL(17,IVENT))*RBAGHOL(18,IVENT)
           DMOUT  =DMOUT+RBAGHOL(21,IVENT)
           DHOUT  =DHOUT+RBAGHOL(22,IVENT)
        ENDDO
        FSAV(7) =FSAV(7) /MAX(AOUTOT,EM20)
        FSAV(13)=DTX ! not DTPOLH(PHDT) since DTX may be updated with smoothing factor

        IF(ITYP == 8) THEN
           CALL FVINJT8_1(NJET, IBAGJET , RBAGJET , IGEO, GEO, PM,
     .                    IVOLU, RVOLU, DMOUT, DHOUT)
           TTF  =RVOLU(60)
           IF (IVOLU(74)  ==  0) THEN
              UP_SWITCH = TT-TTF >= RVOLU(70) .OR. NPOLH <= IVOLU(37)
           ENDIF
           IF (IVOLU(74)  ==  1) THEN
C     Automatic switch to uniform pressure when dispersion of pressure is low
              UP_SWITCH = ((PDISP  <  PDISP_OLD) .AND. (PDISP  <  RVOLU(73)))
              UP_SWITCH = UP_SWITCH .OR. TT-TTF >= RVOLU(70)
           ENDIF
           IF (IVOLU(74)  ==  2) THEN
C     Automatic switch to uniform pressure when dispersion of pressure is low
              UP_SWITCH = ((PDISP  <  PDISP_OLD) .AND. (PDISP  <  RVOLU(73)))
              UP_SWITCH = UP_SWITCH .AND. TT-TTF >= RVOLU(70)
           ENDIF

           IF (UP_SWITCH) THEN
              RVOLU(12)=FSAV(3)  ! pression
              RVOLU(13)=FSAV(5)  ! temperature
              RVOLU(16)=FSAV(2)  ! volume
              RVOLU(20)=FSAV(1)  ! masse
           ENDIF
        ENDIF

      ENDIF

      

      DO IEL=1,NELT
         IF (ITAGEL(IEL) > 0) THEN
            FSAV(15)=FSAV(15)+ELFMASS(IEL)
            FSAV(16)=FSAV(16)+ELFEHPY(IEL)
         ENDIF
      ENDDO
      FSAV(17)=ENINT


C---------------------------------------------------
C Fluid velocity on nodes
C---------------------------------------------------
      IF( (TT>=TANIM .AND. TT<=TANIM_STOP) .OR.TT >= TOUTP.OR. NBGAUGE > 0 .OR.
     .              (MANIM >= 4.AND.MANIM <= 15)) THEN
C     Computations for anim, also needed if there are gauges.
        PREPARE_ANIM = 1
        UEL(1:3,1:NELT)=ZERO
        RHOEL(1:NELT)=ZERO
        TKEL(1:NELT) =ZERO
        SSPEL(1:NELT)=ZERO
      ELSE
        PREPARE_ANIM = 0
      ENDIF

!$OMP PARALLEL PRIVATE(I,J,JJ,K,KK,IEL,JEL,II,NV)
!$OMP+ PRIVATE(NX,NY,NZ,NRX,NRY,NRZ)
!$OMP+ PRIVATE(FAC,AREA)
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NPOLH
         DO J=IFVPADR(I),IFVPADR(I+1)-1
            JJ=IFVPOLH(J)
            DO K=IFVTADR(JJ),IFVTADR(JJ+1)-1
               KK=IFVPOLY(K)
               IEL=ABS(IFVTRI(4,KK))
               JEL=IFVTRI(4,KK)
               AREA=PAREA(KK)
               IF (JEL > 0) THEN
                  PEL(IEL)  =PEL(IEL)  +PPOLH(I)*AREA/ELAREA(IEL)
               ELSEIF (JEL < 0) THEN
                  NRX=NORM(1,IEL)
                  NRY=NORM(2,IEL)
                  NRZ=NORM(3,IEL)
                  IF    (IFVTRI(5,KK) == I) THEN
                        NV=IFVTRI(6,KK)
                        NX=PNORM(1,KK)
                        NY=PNORM(2,KK)
                        NZ=PNORM(3,KK)
                  ELSEIF(IFVTRI(6,KK) == I) THEN
                        NV=IFVTRI(5,KK)
                        NX=-PNORM(1,KK)
                        NY=-PNORM(2,KK)
                        NZ=-PNORM(3,KK)
                  ENDIF
                  IF (NV >= I) THEN 
                    IF (NV == I) AREA=AREA*HALF
                    II=I
                    FAC=NX*NRX+NY*NRY+NZ*NRZ
                    IF(FAC < 0) II=NV
                    PEL(IEL)  =PEL(IEL)  +PPOLH(II)*AREA/ELAREA(IEL)
                  ENDIF
               ENDIF
            ENDDO  ! next triangle
         ENDDO     ! next polygon
      ENDDO        ! next polyhedron
!$OMP END DO

      IF(PREPARE_ANIM == 1) THEN
!$OMP SINGLE
      DO I=1,NPOLH
         DO J=IFVPADR(I),IFVPADR(I+1)-1
            JJ=IFVPOLH(J)
            DO K=IFVTADR(JJ),IFVTADR(JJ+1)-1
               KK=IFVPOLY(K)
               IEL=ABS(IFVTRI(4,KK))
               JEL=IFVTRI(4,KK)
               AREA=PAREA(KK)
               IF (JEL > 0) THEN
                 UEL(1,IEL)=UEL(1,IEL)+PU(1,I)*AREA/ELAREA(IEL)
                 UEL(2,IEL)=UEL(2,IEL)+PU(2,I)*AREA/ELAREA(IEL)
                 UEL(3,IEL)=UEL(3,IEL)+PU(3,I)*AREA/ELAREA(IEL)
                 RHOEL(IEL)=RHOEL(IEL)+RPOLH(I)*AREA/ELAREA(IEL)
                 TKEL(IEL) =TKEL(IEL) +TPOLH(I)*AREA/ELAREA(IEL)
                 SSPEL(IEL)=SSPEL(IEL)+SQRT(GPOLH(I)*PPOLH(I)/RPOLH(I))*AREA/ELAREA(IEL)
               ELSEIF (JEL < 0) THEN
                 NRX=NORM(1,IEL)
                 NRY=NORM(2,IEL)
                 NRZ=NORM(3,IEL)
                 IF    (IFVTRI(5,KK) == I) THEN
                       NV=IFVTRI(6,KK)
                       NX=PNORM(1,KK)
                       NY=PNORM(2,KK)
                       NZ=PNORM(3,KK)
                 ELSEIF(IFVTRI(6,KK) == I) THEN
                       NV=IFVTRI(5,KK)
                       NX=-PNORM(1,KK)
                       NY=-PNORM(2,KK)
                       NZ=-PNORM(3,KK)
                 ENDIF
                 IF (NV < I) GOTO 310
                 IF (NV == I) AREA=AREA*HALF
                 II=I
                 FAC=NX*NRX+NY*NRY+NZ*NRZ
                 IF(FAC < 0) II=NV
                 UEL(1,IEL)=UEL(1,IEL)+PU(1,II)*AREA/ELAREA(IEL)
                 UEL(2,IEL)=UEL(2,IEL)+PU(2,II)*AREA/ELAREA(IEL)
                 UEL(3,IEL)=UEL(3,IEL)+PU(3,II)*AREA/ELAREA(IEL)
                 RHOEL(IEL)=RHOEL(IEL)+RPOLH(II)*AREA/ELAREA(IEL)
                 TKEL(IEL) =TKEL(IEL) +TPOLH(II)*AREA/ELAREA(IEL)
                 SSPEL(IEL)=SSPEL(IEL)+SQRT(GPOLH(II)*PPOLH(II)/RPOLH(II))*AREA/ELAREA(IEL)
               ENDIF
            ENDDO  ! next triangle
  310    ENDDO     ! next  polygon
      ENDDO        ! next polyhedron
!$OMP END SINGLE
      ENDIF



C------------------
C VENT HOLES
C------------------
      IF(PREPARE_ANIM == 1) THEN
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
        DO IEL=1,NEL
           IF(ITAGEL(IEL)<0) THEN
             UEL(1,IEL)=ELFVEL(IEL)*NORM(1,IEL)
             UEL(2,IEL)=ELFVEL(IEL)*NORM(2,IEL)
             UEL(3,IEL)=ELFVEL(IEL)*NORM(3,IEL)
           ENDIF
        ENDDO
!$OMP END DO NOWAIT

!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
        DO I=1,NNT
          U(1,I)=ZERO
          U(2,I)=ZERO
          U(3,I)=ZERO
          RHO(I)  =ZERO
          TK(I)   =ZERO
          SSPK(I) =ZERO
        ENDDO
!OMP END DO

!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
        DO I=1,NNA
          VOLA(I)=ZERO
          PA(I)=ZERO
          RHOA(I)=ZERO
          TKA(I)=ZERO
          SSPKA(I)=ZERO
          UA(1,I)=ZERO
          UA(2,I)=ZERO
          UA(3,I)=ZERO
        ENDDO
!$OMP ENDDO

      ENDIF

!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NNT
        P(I)=ZERO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

      DO IEL=1,NELT
         N1=ELEM(1,IEL)
         N2=ELEM(2,IEL)
         N3=ELEM(3,IEL)
         AREA1=NODAREA(N1)
         AREA2=NODAREA(N2)
         AREA3=NODAREA(N3)
         FAC1=THIRD*ELAREA(IEL)/AREA1
         FAC2=THIRD*ELAREA(IEL)/AREA2
         FAC3=THIRD*ELAREA(IEL)/AREA3
         P(N1) =P(N1) +FAC1*PEL(IEL)
         P(N2) =P(N2) +FAC2*PEL(IEL)
         P(N3) =P(N3) +FAC3*PEL(IEL)
      ENDDO

      IF(PREPARE_ANIM == 1) THEN
        DO IEL=1,NELT
           N1=ELEM(1,IEL)
           N2=ELEM(2,IEL)
           N3=ELEM(3,IEL)
           AREA1=NODAREA(N1)
           AREA2=NODAREA(N2)
           AREA3=NODAREA(N3)
           FAC1=THIRD*ELAREA(IEL)/AREA1
           FAC2=THIRD*ELAREA(IEL)/AREA2
           FAC3=THIRD*ELAREA(IEL)/AREA3
           U(1,N1)=U(1,N1)+FAC1*UEL(1,IEL)
           U(2,N1)=U(2,N1)+FAC1*UEL(2,IEL)
           U(3,N1)=U(3,N1)+FAC1*UEL(3,IEL)
           U(1,N2)=U(1,N2)+FAC2*UEL(1,IEL)
           U(2,N2)=U(2,N2)+FAC2*UEL(2,IEL)
           U(3,N2)=U(3,N2)+FAC2*UEL(3,IEL)
           U(1,N3)=U(1,N3)+FAC3*UEL(1,IEL)
           U(2,N3)=U(2,N3)+FAC3*UEL(2,IEL)
           U(3,N3)=U(3,N3)+FAC3*UEL(3,IEL)
           RHO(N1)=RHO(N1)+FAC1*RHOEL(IEL)
           RHO(N2)=RHO(N2)+FAC2*RHOEL(IEL)
           RHO(N3)=RHO(N3)+FAC3*RHOEL(IEL)
           TK(N1)=TK(N1)+FAC1*TKEL(IEL)
           TK(N2)=TK(N2)+FAC2*TKEL(IEL)
           TK(N3)=TK(N3)+FAC3*TKEL(IEL)
           SSPK(N1)=SSPK(N1)+FAC1*SSPEL(IEL)
           SSPK(N2)=SSPK(N2)+FAC2*SSPEL(IEL)
           SSPK(N3)=SSPK(N3)+FAC3*SSPEL(IEL)
        ENDDO

        DO I=1,NPOLH
           IF (IBPOLH(I) /= 0) THEN
             TEMP=TPOLH(I)
             II=IABS(IBPOLH(I))
             IF(BRNA(1,II) == BRNA(4,II).AND.BRNA(5,II) == BRNA(8,II))THEN
C Pentahedron
                DO K=1,6
                 J=K
                 IF(K >= 4) J=J+1
                 JJ=BRNA(J,II)
                 VOLA(JJ)=VOLA(JJ)+ONE_OVER_6*PVOLU(I)
                 PA(JJ)=PA(JJ)+ONE_OVER_6*PPOLH(I)*PVOLU(I)
                 RHOA(JJ)=RHOA(JJ)+ONE_OVER_6*RPOLH(I)*PVOLU(I)
                 TKA(JJ)=TKA(JJ)+ONE_OVER_6*TEMP*PVOLU(I)
                 SSPKA(JJ)=SSPKA(JJ)+ONE_OVER_6*SQRT(GPOLH(I)*PPOLH(I)/RPOLH(I))*PVOLU(I)
                 UA(1,JJ)=UA(1,JJ)+ONE_OVER_6*PU(1,I)*PVOLU(I)
                 UA(2,JJ)=UA(2,JJ)+ONE_OVER_6*PU(2,I)*PVOLU(I)
                 UA(3,JJ)=UA(3,JJ)+ONE_OVER_6*PU(3,I)*PVOLU(I)
                ENDDO           
             ELSEIF(BRNA(5,II)==BRNA(8,II).AND.
     .              BRNA(6,II)==BRNA(8,II).AND.
     .              BRNA(7,II)==BRNA(8,II))THEN
C Pyramid
                DO J=1,5
                 JJ=BRNA(J,II)
                 VOLA(JJ)=VOLA(JJ)+ONE_FIFTH*PVOLU(I)
                 PA(JJ)=PA(JJ)+ONE_FIFTH*PPOLH(I)*PVOLU(I)
                 RHOA(JJ)=RHOA(JJ)+ONE_FIFTH*RPOLH(I)*PVOLU(I)
                 TKA(JJ)=TKA(JJ)+ONE_FIFTH*TEMP*PVOLU(I)
                 SSPKA(JJ)=SSPKA(JJ)+ONE_FIFTH*SQRT(GPOLH(I)*PPOLH(I)/RPOLH(I))*PVOLU(I)
                 UA(1,JJ)=UA(1,JJ)+ONE_FIFTH*PU(1,I)*PVOLU(I)
                 UA(2,JJ)=UA(2,JJ)+ONE_FIFTH*PU(2,I)*PVOLU(I)
                 UA(3,JJ)=UA(3,JJ)+ONE_FIFTH*PU(3,I)*PVOLU(I)
                ENDDO
             ELSE
C Hexahedron or Tetrahedron
                DO J=1,8
                 JJ=BRNA(J,II)
                 VOLA(JJ)=VOLA(JJ)+ONE_OVER_8*PVOLU(I)
                 PA(JJ)=PA(JJ)+ONE_OVER_8*PPOLH(I)*PVOLU(I)
                 RHOA(JJ)=RHOA(JJ)+ONE_OVER_8*RPOLH(I)*PVOLU(I)
                 TKA(JJ)=TKA(JJ)+ONE_OVER_8*TEMP*PVOLU(I)
                 SSPKA(JJ)=SSPKA(JJ)+ONE_OVER_8*SQRT(GPOLH(I)*PPOLH(I)/RPOLH(I))*PVOLU(I)
                 UA(1,JJ)=UA(1,JJ)+ONE_OVER_8*PU(1,I)*PVOLU(I)
                 UA(2,JJ)=UA(2,JJ)+ONE_OVER_8*PU(2,I)*PVOLU(I)
                 UA(3,JJ)=UA(3,JJ)+ONE_OVER_8*PU(3,I)*PVOLU(I)
                ENDDO
             ENDIF
           ENDIF
        ENDDO

        DO I=1,NNA
           VOLNO=VOLA(I)
           IF(VOLNO > ZERO) THEN
             PA(I)  =PA(I)/VOLNO
             RHOA(I)=RHOA(I)/VOLNO
             TKA(I) =TKA(I)/VOLNO
             SSPKA(I) =SSPKA(I)/VOLNO
             UA(1,I)=UA(1,I)/VOLNO
             UA(2,I)=UA(2,I)/VOLNO
             UA(3,I)=UA(3,I)/VOLNO
           ENDIF
        ENDDO
      ENDIF ! prepare_anim


C---------------------------------------------------
C Potential opening of vent holes
C---------------------------------------------------
      DO IVENT=1,NVENT
         AREA_VENT(IVENT)=ZERO
         PM_VENT(IVENT)=ZERO
      ENDDO

C
      DO IEL=1,NEL
         IF (ITAGEL(IEL) < 0) THEN
            IVENT=-ITAGEL(IEL)
            AREA_VENT(IVENT)=AREA_VENT(IVENT)+ELAREA(IEL)
            PM_VENT(IVENT)=PM_VENT(IVENT)+FEL(IEL)
         ENDIF
      ENDDO



      DO IVENT=1,NVENT
         IF (AREA_VENT(IVENT) > ZERO) THEN
            PM_VENT(IVENT)=PM_VENT(IVENT)/AREA_VENT(IVENT)
         ELSE
            PM_VENT(IVENT)=ZERO
         ENDIF
C
         IDEF   =IBAGHOL(1,IVENT)
         IDTPDEF= IBAGHOL(11,IVENT)
         IDPDEF = IBAGHOL(12,IVENT)
C
         IF(IDEF == 2) CYCLE
C
         PDEF   =RBAGHOL(1,IVENT)
         DTPDEFI=RBAGHOL(4,IVENT)
         DTPDEFC=RBAGHOL(5,IVENT)
         TVENT  =RBAGHOL(3,IVENT)
         TSTOPE =RBAGHOL(14,IVENT)
         IF (IDTPDEF == 0) THEN
          IF(PM_VENT(IVENT) > PDEF+PEXT) THEN
            DTPDEFC = DTPDEFC+DT1
          END IF
         ELSE IF (IDTPDEF == 1) THEN
           IF (PM_VENT(IVENT) > PDEF+PEXT) THEN
             IDPDEF = 1
           END IF
           IF (IDPDEF == 1) THEN
             DTPDEFC = DTPDEFC+DT1
           END IF
         END IF
         IBAGHOL(12,IVENT) = IDPDEF
         RBAGHOL(5,IVENT)  = DTPDEFC
C
         ITTF  = IVOLU(17)
         TTF    =RVOLU(60)
         DO K=1,20
           TITREVENT(K)=IBAGHOL(14+K,IVENT)
           VENTTITLE(K:K) = ACHAR(TITREVENT(K))
         ENDDO
         IF(ITTF==11.OR.ITTF==12.OR.ITTF==13) THEN
          IF (IDEF==0.AND.PM_VENT(IVENT) > PDEF+PEXT
     .                 .AND.DTPDEFC > DTPDEFI
     .                 .AND.VOLPH > EM3*AREAP**THREE_HALF
     .                 .AND.TT < TSTOPE+TTF) THEN
            IDEF=1
            WRITE(IOUT,'(A)')' ** AIRBAG VENT HOLE MEMBRANE IS DEFLATED **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),' VENT HOLE NUMBER',IVENT,' ',VENTTITLE
            WRITE(ISTDO,'(2A)')' ** VENT HOLE MEMBRANE IS DEFLATED ',VENTTITLE
          ENDIF
          IF(IDEF == 0 .AND. TT > TVENT+TTF.AND. TT < TSTOPE+TTF) THEN
            IDEF=1
            WRITE(IOUT,'(A)') ' ** AIRBAG VENTING STARTS **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),' VENT HOLE NUMBER',IVENT,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') ' ** VENTING STARTS ',VENTTITLE
          ENDIF
          IF(IDEF == 1 .AND. TT >= TSTOPE+TTF) THEN
            IDEF=2
            WRITE(IOUT,'(A)') ' ** AIRBAG VENTING STOPS **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),' VENT HOLE NUMBER',IVENT,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') ' ** VENTING STOPS ',VENTTITLE
          ENDIF

         ELSE IF(ITTF==0) THEN
          IF (IDEF==0.AND.PM_VENT(IVENT)>PDEF+PEXT
     .                 .AND.DTPDEFC>DTPDEFI
     .                 .AND.VOLPH>EM3*AREAP**THREE_HALF
     .                 .AND.TT<TSTOPE) THEN
            IDEF=1
            WRITE(IOUT,'(A)')' ** AIRBAG VENT HOLE MEMBRANE IS DEFLATED **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1), ' VENT HOLE NUMBER',IVENT,' ',VENTTITLE
            WRITE(ISTDO,'(2A)')' ** VENT HOLE MEMBRANE IS DEFLATED ',VENTTITLE
          ENDIF
          IF(IDEF == 0 .AND. TT > TVENT .AND. TT < TSTOPE) THEN
            IDEF=1
            WRITE(IOUT,'(A)') ' ** AIRBAG VENTING STARTS **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),' VENT HOLE NUMBER',IVENT,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') ' ** VENTING STARTS ',VENTTITLE
          ENDIF
          IF(IDEF == 1 .AND. TT >= TSTOPE) THEN
            IDEF=2
            WRITE(IOUT,'(A)') ' ** AIRBAG VENTING STOPS **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IVENT,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') ' ** VENTING STOPS ',VENTTITLE
          ENDIF
         END IF
         IBAGHOL(1,IVENT)=IDEF
      ENDDO
C---------------------------------------------------
C Force on Airbag + OUTPUT
C---------------------------------------------------
      IF(IVOLU(39)==1) THEN
       TFEXT0=TFEXT
       DO IEL=1,NELT
         N1=ELEM(1,IEL)
         N2=ELEM(2,IEL)
         N3=ELEM(3,IEL)
         NRX=NORM(1,IEL)
         NRY=NORM(2,IEL)
         NRZ=NORM(3,IEL)
         IF(IEL <= NEL) THEN
           FF=FEL(IEL)-PEXT*ELAREA(IEL)
         ELSE
           FF=FEL(IEL)
         ENDIF
         FVSPMD(IFV)%AAA(1,N1)=FVSPMD(IFV)%AAA(1,N1)+THIRD*FF*NRX
         FVSPMD(IFV)%AAA(2,N1)=FVSPMD(IFV)%AAA(2,N1)+THIRD*FF*NRY
         FVSPMD(IFV)%AAA(3,N1)=FVSPMD(IFV)%AAA(3,N1)+THIRD*FF*NRZ
         FVSPMD(IFV)%AAA(1,N2)=FVSPMD(IFV)%AAA(1,N2)+THIRD*FF*NRX
         FVSPMD(IFV)%AAA(2,N2)=FVSPMD(IFV)%AAA(2,N2)+THIRD*FF*NRY
         FVSPMD(IFV)%AAA(3,N2)=FVSPMD(IFV)%AAA(3,N2)+THIRD*FF*NRZ
         FVSPMD(IFV)%AAA(1,N3)=FVSPMD(IFV)%AAA(1,N3)+THIRD*FF*NRX
         FVSPMD(IFV)%AAA(2,N3)=FVSPMD(IFV)%AAA(2,N3)+THIRD*FF*NRY
         FVSPMD(IFV)%AAA(3,N3)=FVSPMD(IFV)%AAA(3,N3)+THIRD*FF*NRZ
C
         TFEXT=TFEXT+FF*VEL(IEL)*DT1
       ENDDO
       FSAV(18)=FSAV(18)+TFEXT-TFEXT0
      ENDIF
C----------------------
C     TH Gauges output
C----------------------
      IF (NBGAUGE > 0) THEN
C To be done even if gauge nodes are not related to FVMBAG.
C otherwise we may to miss some in SPMD
        DO I=1,NNT
          FVSPMD(IFV)%GGG(1,I)=P(I)
          FVSPMD(IFV)%GGG(2,I)=RHO(I)
          FVSPMD(IFV)%GGG(3,I)=TK(I)
        ENDDO

        DO I=1,NNA
          FVSPMD(IFV)%GGA(1,I)=PA(I)
          FVSPMD(IFV)%GGA(2,I)=RHOA(I)
          FVSPMD(IFV)%GGA(3,I)=TKA(I)
        ENDDO
      ENDIF

 1000 CONTINUE

      RETURN
      END