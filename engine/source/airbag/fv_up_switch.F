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
Chd|  FV_UP_SWITCH                  source/airbag/fv_up_switch.F  
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
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/airbag/fvmbag_meshcontrol_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE FV_UP_SWITCH(NN, NEL, ELEM, NJET, NPOLY,LENH,NBA,
     2     IBAGJET, RBAGJET, NVENT    , IBAGHOL, RBAGHOL ,
     3     P      , RHO    , TK       , U      , SSPK    ,
     4     X      , V        , A      , NSENSOR, SENSOR_TAB,
     5     FSAV   , NPC    , TF       , IVOLU  , RVOLU   ,
     6     MPOLH  , QPOLH  , EPOLH    ,
     7     PPOLH  , RPOLH  , GPOLH    ,
     8     NPOLH  , IFVNOD , RFVNOD   , IFVTRI ,
     9     IFVPOLY, IFVTADR, IFVPOLH  ,
     A     IFVPADR, INFO   , NNS      , NNTR   , IFV     ,
     B     NPOLHA , DLH    , CPAPOLH  ,
     C     CPBPOLH, CPCPOLH, RMWPOLH  ,
     D     ITAGEL , ELSINI , ICONTACT , IDPOLH ,
     E     ELFMASS, ELFVEL , IBUFA    , ELEMA  , TAGELA  ,
     F     PA     , RHOA   , TKA      , UA     , BRNA    ,
     G     NNA    , NTGA   , IBPOLH   , DTPOLH , NNT     ,
     H     NELT   , XXXA   , VVVA     , NCONA  , POROSITY,
     I     ITYP     , IGEO   , SSPKA   ,
     J     GEO    , PM     , IPM      , TPOLH  , ELFEHPY ,
     K     CPDPOLH, CPEPOLH, CPFPOLH  ,
     L     ELTG   , IPARG  , MATTG    ,
     M     IGROUPTG,IGROUPC, ELBUF_TAB, CFL_COEF,
     N     PDISP_OLD, PDISP)
C-----------------------------------------------
C   M O D U L E S
C-----------------------------------------------
      USE FVBAG_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD
      USE FVMBAG_MESHCONTROL_MOD
!     USE OMP_LIB
      USE SENSOR_MOD
      USE ANIM_MOD
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr07_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "tabsiz_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPOLY, LENH, NPOLH, NBA,NSENSOR
      INTEGER, INTENT(IN) :: NN, NEL, NELT, ELEM(3, NELT), NJET, IBAGJET(NIBJET, NJET),
     .        NVENT, NPC(SNPC),  IFVNOD(3,NNS), IFVTRI(6,NNTR),
     .        IFVPOLY(NNTR),IFVTADR(NPOLY+1), IFVPOLH(LENH), IFVPADR(NPOLH + 1),
     .        NNS, NNTR, IFV, NPOLHA, ITAGEL(NELT), ICONTACT(SICONTACT),
     .        IDPOLH(NPOLH), IBUFA(NNA), ELEMA(3,NTGA), TAGELA(NTGA), BRNA(8,NBA),
     .        NNA, NTGA, IBPOLH(NPOLH), NNT, NCONA(16,NNA),
     .        ITYP, IGEO(NPROPGI,*), IPM(NPROPMI,NUMMAT), IPARG(NPARG,NGROUP)
      INTEGER, INTENT(INOUT) :: IVOLU(NIMV), INFO, IBAGHOL(NIBHOL, NVENT)
      INTEGER, INTENT(IN) ::  ELTG(NELT), MATTG(NELT), IGROUPTG(NUMELTG), IGROUPC(NUMELC)

      MY_REAL, INTENT(IN) ::
     .     RBAGJET(NRBJET,NJET), X(3,NUMNOD), V(3,NUMNOD), A(3,NUMNOD),
     .     TF(STF), XXXA(3,NNA), VVVA(3,NNA), POROSITY(NELT - NEL),
     .     GEO(NPROPG,NUMGEO), PM(NPROPM,NUMMAT), CFL_COEF,  PDISP_OLD
      MY_REAL, INTENT(INOUT) ::
     .     P(NNT), RHO(NNT), TK(NNT), U(3,NNT), SSPK(NNT), MPOLH(NPOLH), QPOLH(3,NPOLH),
     .     EPOLH(NPOLH), PPOLH(NPOLH), RPOLH(NPOLH), GPOLH(NPOLH), RFVNOD(2,NNS), DLH,
     .     CPAPOLH(NPOLH), CPBPOLH(NPOLH), CPCPOLH(NPOLH), RMWPOLH(NPOLH), ELSINI(NELT),
     .     ELFMASS(NELT), ELFVEL(NELT), PA(NNA), RHOA(NNA), TKA(NNA),SSPKA(NNA), UA(3,NNA),
     .     DTPOLH(NPOLH), FSAV(NTHVKI), TPOLH(NPOLH), CPDPOLH(NPOLH), CPEPOLH(NPOLH),
     .     CPFPOLH(NPOLH), RBAGHOL(NRBHOL,NVENT), PDISP, ELFEHPY(NELT), RVOLU(NRVOLU)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), INTENT(IN) :: ELBUF_TAB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, II, IINJ, IEL, N1, N2, N3, JEL,
     .        NN1, NN2, NN3, J, JJ, K, KK, ISENS, IVEL,
     .        ITAGP(NPOLH), NV, ILVOUT, IPRI,
     .        IVENT, IDEF, IPORP, 
     .        ITVENT, PHDT, I1, I2, ICONT(NNT),IVDP,ITTF, IDPDEF,
     .        IDTPDEF,ITYPL,
     .        PREPARE_ANIM,IVENTYP
      INTEGER TITREVENT(20)

      MY_REAL
     .        NODAREA(NNT), X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
     .        X12, Y12, Z12, X13, Y13, Z13, NRX, NRY, NRZ, AREA2,
     .        ELAREA(NELT), NORM(3,NELT), KSI, ETA, 
     .        PNORM(3,NNTR), PVOLU(NPOLH), AREA, NX, NY,
     .        NZ, GAMAI, CPAI, CPBI, CPCI, RMWI, TI,
     .        CPDI, CPEI, CPFI,
     .        RHOI, DATAINJ(6,NJET), SCALT,
     .        FVEL, TSTART, TSG, INJVEL,
     .        DYDX, CPA, CPB, CPC, EFAC,
     .        PU(3,NPOLH), GAMA, FEL(NELT), DM(NPOLH),
     .        DQ(3,NPOLH), DE(NPOLH), DMI, RHO1, RE1, UX1,
     .        UY1, UZ1, RHO2, RE2, UX2, UY2, UZ2, VFX, VFY, VFZ, VREL(NELT), SS(NELT), SS_,
     .        ALPHA, RHOM, REM, RUXM, RUYM, RUZM, P1, P2,
     .        UEL(3,NELT), AREA1, AREA3, FF, TEMP, PEXT, VOLG, VOLPH,
     .        AREAP, AREAEL, QA, QB, DTX, AL, DD, AD, SSP,
     .        QX, VV, DMINI(NPOLH), DMCPA(NPOLH), DMCPB(NPOLH),
     .        DMCPC(NPOLH), DMRMW(NPOLH), MSINI,
     .        RMW, CP, CV, RHOEL(NELT), TKEL(NELT), SSPEL(NELT), GAMA1, GAMA2,
     .        QVISC(NPOLH), AMTOT, PMEAN, PMEAN2, ELSOUT(NELT),TMP(NEL),
     .        AREA_VENT(NVENT), PM_VENT(NVENT), SCALP, SCALS, 
     .        AOUT, DERI, PDEF, PVOLTMP,
     .        DTPDEFI, DTPDEFC, TVENT, PORO(NELT), VOLA(NNA), 
     .        X23, Y23, Z23, X31, Y31, Z31,  L12, L23, L31, H1,
     .        FAC1, FAC2, FAC3, AREA_OLD, PP, DLS(NPOLH), VMAX, UU,
     .        PCRIT, VX1, VY1, VZ1, VX2, VY2, VZ2,
     .        VX3, VY3, VZ3, VVX, VVY, VVZ, VEL(NELT), FAC, 
     .        AOUTOT, SVENT(NVENT), SVENTOT, XXX(3,NNT),
     .        VVV(3,NNT),  FVDP, RBID, TTF,
     .        DMCPD(NPOLH), DMCPE(NPOLH), DMCPF(NPOLH),
     .        CPD, CPE, CPF, TEMP0, PEL(NELT), PSTAG, VOLNO,TSTOPE,
     .        ENINT, MASSFLOW, TFEXT0, ELDMASS(NEL), ELDEHPY(NEL), DMOUT, DHOUT,
     .        XX1, XX2, XX3, YY1, YY2, YY3, ZZ1, ZZ2, ZZ3, NNX, NNY, NNZ,
     .        PRES_AV, CP_AV, CV_AV, RGAS_AV, RHO_AV


      LOGICAL :: EXIT_NEG_VOL,INJECTION_STARTED
      LOGICAL :: UP_SWITCH
C
      MY_REAL
     .         FINTER, GET_U_FUNC
      EXTERNAL FINTER, GET_U_FUNC
      CHARACTER*20 VENTTITLE
      INTEGER :: NODE_ID(5), INODE
      MY_REAL :: TAB_PVOL(NELT), DOT_PROD
      MY_REAL :: MOMENTUM_FLUX_X(NELT), MOMENTUM_FLUX_Y(NELT), MOMENTUM_FLUX_Z(NELT),
     .     MASS_FLUX(NELT), ENERGY_FLUX(NELT),
     .     CPA_FLUX(NELT), CPB_FLUX(NELT), CPC_FLUX(NELT), CPD_FLUX(NELT),
     .     CPE_FLUX(NELT), CPF_FLUX(NELT), RGAS_FLUX(NELT)
      INTEGER(8) :: VEC_PTR_PLUS, VEC_PTR_MINUS
      INTEGER :: COUNT, IDT_FVMBAG, PARAM_L_TYPE
      MY_REAL :: CPAM, CPBM, CPCM, RGASM, CPDM, CPEM, CPFM
      MY_REAL :: DTOLD, LAMBDA

C-----------------------------------------------
C   C O M M E N T S
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
C   S O U R C E   L I N E S
C-----------------------------------------------
      VOLPH = ZERO
      
      IF(NBGAUGE > 0 ) THEN
         ALLOCATE(FVSPMD(IFV)%GGG(3,NNT))
         ALLOCATE(FVSPMD(IFV)%GGA(3,NNA))
      ENDIF
      ALLOCATE(FVSPMD(IFV)%AAA(3,NNT))
      FVSPMD(IFV)%AAA(1:3,1:NNT) = ZERO

!     ===================     !
!     PARITH/ON TREATMENT     !
!     ===================     !
      IF (NSPMD == 1) THEN
         DO I=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
            I1=FVSPMD(IFV)%IBUF_L(1,I)
            I2=FVSPMD(IFV)%IBUF_L(2,I)
            XXX(1,I1)=X(1,I2)
            XXX(2,I1)=X(2,I2)
            XXX(3,I1)=X(3,I2)
         ENDDO

         DO I=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
            I1=FVSPMD(IFV)%IBUF_L(1,I)
            I2=FVSPMD(IFV)%IBUF_L(2,I)
            VVV(1,I1)=V(1,I2)
            VVV(2,I1)=V(2,I2)
            VVV(3,I1)=V(3,I2)
         ENDDO

         IF (INTBAG/=0.AND.NVENT>0.AND.IVOLU(39)/=0) THEN
            DO I=1,FVSPMD(IFV)%NN_L+FVSPMD(IFV)%NNI_L
               I1=FVSPMD(IFV)%IBUF_L(1,I)
               I2=FVSPMD(IFV)%IBUF_L(2,I)
               ICONT(I1)=ICONTACT(I2)
            ENDDO
         ENDIF

      ELSE
         CALL SPMD_FVB_GATH(IFV, X, XXX, RBID, RBID, 4)

         CALL SPMD_FVB_GATH(IFV, V, VVV, RBID, RBID, 4)

         IF (INTBAG/=0.AND.NVENT>0.AND.IVOLU(39)/=0)
     .        CALL SPMD_FVB_IGATH(IFV, ICONTACT, ICONT)


         IF (ISPMD/=FVSPMD(IFV)%PMAIN-1) RETURN
      END IF

!     =================================     !
!     REBUILD CONNECTIVITY AFTER SWITCH
!     =================================     !

!     INITIALIZATION PHASE, DONE ONLY ONCE
!     ------------------------------------

!     NB, FOR AN ELEMENT IEL, THE 'MINUS SIDE' IS BEHIND THE NORMAL, WHILE 'PLUS SIDE' IS IN THE DIRECTION OF
!     THE NORMAL

      IF (IVOLU(74) == -2) THEN
         ALLOCATE(FVDATA(IFV)%TRI_TO_ELEM(2, NELT))
         FVDATA(IFV)%TRI_TO_ELEM(:, :) = 0
         ALLOCATE(FVDATA(IFV)%ELEM_TO_TRI(NPOLH))
         CALL INTVECTOR_CREATE(VEC_PTR_PLUS)
         CALL INTVECTOR_CREATE(VEC_PTR_MINUS)
         DO I = 1, NPOLH
!     CLEAR INT VECTORS
            CALL INTVECTOR_CLEAR(VEC_PTR_PLUS)
            CALL INTVECTOR_CLEAR(VEC_PTR_MINUS)
            COUNT = 0
!     LOOP ON POLYHEDRAL FACES
            DO J = IFVPADR(I), IFVPADR(I+1)-1
               JJ = IFVPOLH(J)
!     LOOP ON TRIANGLES OF THE POLYHEDRAL FACE
               DO K = IFVTADR(JJ), IFVTADR(JJ+1)-1
                  KK = IFVPOLY(K)
                  IEL = IFVTRI(4, KK)
                  N1 = IFVTRI(1, KK)
                  IF (IFVNOD(1, N1) /= 2) THEN
                     CALL ABORT()
                  ENDIF
                  N1 = IFVNOD(3, N1)
                  X1 = XXXA(1, N1)
                  Y1 = XXXA(2, N1)
                  Z1 = XXXA(3, N1)
                  N2 = IFVTRI(2, KK)
                  IF (IFVNOD(1, N2) /= 2) THEN
                     CALL ABORT()
                  ENDIF
                  N2 = IFVNOD(3, N2)
                  X2 = XXXA(1, N2)
                  Y2 = XXXA(2, N2)
                  Z2 = XXXA(3, N2)
                  N3 = IFVTRI(3, KK)
                  IF (IFVNOD(1, N3) /= 2) THEN
                     CALL ABORT()
                  ENDIF
                  N3 = IFVNOD(3, N3)
                  X3 = XXXA(1, N3)
                  Y3 = XXXA(2, N3)
                  Z3 = XXXA(3, N3)
                  NX = (Y2 - Y1) * (Z3 - Z1) - (Z2 - Z1) * (Y3 - Y1)
                  NY = (Z2 - Z1) * (X3 - X1) - (X2 - X1) * (Z3 - Z1)
                  NZ = (X2 - X1) * (Y3 - Y1) - (Y2 - Y1) * (X3 - X1)
                  IF (IEL /= 0) THEN
                     COUNT = COUNT + 1
                     NN1 = ELEM(1, ABS(IEL))
                     NN2 = ELEM(2, ABS(IEL))
                     NN3 = ELEM(3, ABS(IEL))
                     XX1 = XXX(1, NN1)
                     YY1 = XXX(2, NN1)
                     ZZ1 = XXX(3, NN1)
                     XX2 = XXX(1, NN2)
                     YY2 = XXX(2, NN2)
                     ZZ2 = XXX(3, NN2)
                     XX3 = XXX(1, NN3)
                     YY3 = XXX(2, NN3)
                     ZZ3 = XXX(3, NN3)
                     NNX = (YY2 - YY1) * (ZZ3 - ZZ1) - (ZZ2 - ZZ1) * (YY3 - YY1)
                     NNY = (ZZ2 - ZZ1) * (XX3 - XX1) - (XX2 - XX1) * (ZZ3 - ZZ1)
                     NNZ = (XX2 - XX1) * (YY3 - YY1) - (YY2 - YY1) * (XX3 - XX1)
                     DOT_PROD = NX * NNX + NY * NNY + NZ * NNZ
!     EXTERNAL (IEL > 0) OR INTERNAL (IEL < 0) SURFACE ELEMENT
                     IF (IEL < 0) THEN
!     INTERNAL, THERE IS A POTENTIAL NEIGHBOR
                        IEL = -IEL
                        I1 = IFVTRI(5, KK)
                        I2 = IFVTRI(6, KK)
                        IF (I1 == I2) THEN
                           CALL ABORT()
                        ENDIF
                        FVDATA(IFV)%TRI_TO_ELEM(1, IEL) = I1
                        FVDATA(IFV)%TRI_TO_ELEM(2, IEL) = I2
                        IF (I1 == I) THEN
                           IF (DOT_PROD >= ZERO) THEN
                              CALL INTVECTOR_PUSH_BACK(VEC_PTR_MINUS, IEL)
                           ELSE
                              CALL INTVECTOR_PUSH_BACK(VEC_PTR_PLUS, IEL)
                              FVDATA(IFV)%TRI_TO_ELEM(1, IEL) = I2
                              FVDATA(IFV)%TRI_TO_ELEM(2, IEL) = I1
                           ENDIF
                        ELSE
                           IF (DOT_PROD >= ZERO) THEN
                              CALL INTVECTOR_PUSH_BACK(VEC_PTR_PLUS, IEL)
                           ELSE
                              CALL INTVECTOR_PUSH_BACK(VEC_PTR_MINUS, IEL)
                              FVDATA(IFV)%TRI_TO_ELEM(1, IEL) = I2
                              FVDATA(IFV)%TRI_TO_ELEM(2, IEL) = I1
                           ENDIF
                        ENDIF
                     ELSE
!     EXTERNAL SURFACE : NO NEIGHBOR
                        IF (IFVTRI(5, KK) /= IFVTRI(6, KK)) THEN
                           CALL ABORT()
                        ENDIF
                        FVDATA(IFV)%TRI_TO_ELEM(1, IEL) = I
                        FVDATA(IFV)%TRI_TO_ELEM(2, IEL) = I
                        IF (DOT_PROD >= ZERO) THEN
                           CALL INTVECTOR_PUSH_BACK(VEC_PTR_MINUS, IEL)
                        ELSE
                           CALL INTVECTOR_PUSH_BACK(VEC_PTR_PLUS, IEL)
                        ENDIF
                     ENDIF
                  ELSE
                     CALL ABORT()
                  ENDIF
               ENDDO
            ENDDO
            CALL INTVECTOR_GET_SIZE(VEC_PTR_MINUS, FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS_SIZE)
            CALL INTVECTOR_GET_SIZE(VEC_PTR_PLUS, FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS_SIZE)
            ALLOCATE(FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS(FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS_SIZE))
            CALL INTVECTOR_COPY_TO(VEC_PTR_MINUS, FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS(1))
            ALLOCATE(FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS(FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS_SIZE))
            CALL INTVECTOR_COPY_TO(VEC_PTR_PLUS, FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS(1))
         ENDDO
         IVOLU(74) = -20
         CALL INTVECTOR_DELETE(VEC_PTR_PLUS)
         CALL INTVECTOR_DELETE(VEC_PTR_MINUS)
      ENDIF


!$OMP PARALLEL PRIVATE(I,J)
!$OMP+ PRIVATE(I1,I2,KSI,ETA,N1,N2,N3,IEL,X1,Y1,Z1,FAC,NN1,NN2,NN3)
!$OMP+ PRIVATE(X2,Y2,Z2,X3,Y3,Z3,VX1,VX2,VX3,VY1,VY2,VY3,VZ1,VZ2,VZ3)
!$OMP+ PRIVATE(X12,Y12,Z12,X13,Y13,Z13,AREA2,NRX,NRY,NRZ,JJ,AREA,KK,K)
!$OMP+ PRIVATE(NX,NY,NZ,PVOLTMP)

!     =================================================    !
!     SURFACE AND NORMAL COMPUTATION FOR FABRIC ELEMENT    !
!     =================================================    !

!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I=1,NNT
         NODAREA(I)=ZERO
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
         IF(IEL<=NEL) THEN
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
         IF(IEL<=NEL) THEN
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

!     ============================    !
!     VOLUME OF THE FINITE VOLUMES    !
!     ============================    !

!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO IEL = 1, NELT
         NX = NORM(1, IEL)
         NY = NORM(2, IEL)
         NZ = NORM(3, IEL)
         N1 = ELEM(1, IEL)
         X1 = XXX(1, N1)
         X2 = XXX(2, N1)
         X3 = XXX(3, N1)
         AREA = ELAREA(IEL)
         PVOLTMP = THIRD * AREA * (NX * X1 + NY * X2 + NZ * X3)
         TAB_PVOL(IEL) = PVOLTMP
      ENDDO
!$OMP END DO

!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I = 1, NPOLH
         PVOLU(I) = ZERO
         DO J = 1, FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS_SIZE
            IEL = FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS(J)
!     FINITE VOLUME I IS ON THE LEFT SIDE OF ELEMENT IEL, HENCE A + SIGN
            PVOLU(I) = PVOLU(I) + TAB_PVOL(IEL)
         ENDDO
         DO J = 1, FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS_SIZE
            IEL = FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS(J)
!     FINITE VOLUME I IS ON THE RIGHT SIDE OF ELEMENT IEL, HENCE A - SIGN
            PVOLU(I) = PVOLU(I) - TAB_PVOL(IEL)
         ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL


C---------------------------------------------------
C VERIFICATIONS
C---------------------------------------------------
      AREAEL=ZERO
      DO IEL=1,NEL
         AREAEL=AREAEL+ELAREA(IEL)
      ENDDO
      AREAP=AREAEL
      RVOLU(18)=AREAEL
      ILVOUT=IVOLU(44)
      VOLPH=ZERO
      EXIT_NEG_VOL = .FALSE.
      DO I=1,NPOLH
         VOLPH=VOLPH+PVOLU(I)
         IF(PVOLU(I) <= 0) EXIT_NEG_VOL = .TRUE.
      ENDDO

      IPRI=MOD(NCYCLE,IABS(NCPRI))
      IF(EXIT_NEG_VOL) THEN
       DO I=1,NPOLH
         IF (PVOLU(I)<=ZERO) THEN
            INFO=1
            IERR=IERR+1
            CALL ANCMSG(MSGID=185,ANMODE=ANINFO,
     .            I1=IDPOLH(I),R1=PVOLU(I),I3=I,I4=NPOLH)
            CALL ARRET(2)
         ENDIF
       ENDDO
      ENDIF


C---------------------------------------------------
C     COMPUTATION OF QUANTITIES IN POLYEDRA
C---------------------------------------------------
      IF (DT1==ZERO) THEN
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
      IF(IVOLU(39) /= 0) THEN

C---------------------------------------------------
C     INJECTORS
C---------------------------------------------------
      DATAINJ(1:6,1:NJET)=ZERO
      DO IEL=1,NELT
         IF (ITAGEL(IEL)>0) THEN
            IINJ=ITAGEL(IEL)
            DATAINJ(1,IINJ)=DATAINJ(1,IINJ)+ELAREA(IEL)
         ENDIF
      ENDDO
C
      SCALT =RVOLU(26)
      IF(ITYP==6) THEN
        CALL FVINJT6(NJET, IBAGJET, RBAGJET, NPC, TF,NSENSOR, 
     .               SENSOR_TAB,SCALT, DATAINJ )
      ELSEIF(ITYP==8) THEN
        CALL FVINJT8(NJET, IBAGJET, RBAGJET, NPC, TF,NSENSOR,  
     .               SENSOR_TAB,SCALT, IGEO, GEO, PM, IVOLU, DATAINJ )
      ENDIF
C
      DO IINJ=1,NJET
         ISENS=IBAGJET(4,IINJ)
         FVEL=RBAGJET(15,IINJ)
         IVEL=IBAGJET(11,IINJ)
         IF(ISENS==0)THEN
          TSTART=ZERO
         ELSE
          TSTART=SENSOR_TAB(ISENS)%TSTART
         ENDIF
C        ITTF : 1 SHIFT EVENT TIME (CAN BE REMOVED IN A LATER VERSION)
C        ITTF : 2 SHIFT EVENT TIME + EVENT FUNCTION
C        ITTF : 1 OR 2 +10 1 FLAG HAS BEEN ACTIVATED AND RVOLU(60) IS FILLED
         ITTF=IVOLU(17)
         IF (TT>=TSTART.AND.(ITTF==1.OR.ITTF==2.OR.ITTF==3))THEN
           ITTF=ITTF+10
           RVOLU(60)=TSTART
           IVOLU(17)=ITTF
         END IF
         IF (TT>=TSTART.AND.DT1>ZERO)THEN
           TSG=(TT-TSTART)*SCALT
           IF (IVEL>0) THEN
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
C VENT HOLES
C---------------------------------------------------
      IF (INTBAG==0) THEN
         PORO(1:NELT)=ZERO
      ELSE
!$OMP PARALLEL PRIVATE(IEL,N1,N2,N3)
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
         DO IEL=1,NELT
            N1=ELEM(1,IEL)
            N2=ELEM(2,IEL)
            N3=ELEM(3,IEL)
            PORO(IEL)=ZERO
            IF (ICONT(N1)/=0) PORO(IEL)=PORO(IEL)+THIRD
            IF (ICONT(N2)/=0) PORO(IEL)=PORO(IEL)+THIRD
            IF (ICONT(N3)/=0) PORO(IEL)=PORO(IEL)+THIRD
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
C EFFECTIVE LEAKAGE SURFACE OF TRIANGLES
C CORRECTION OF SCALE FACTORS ON THE VENT HOLES SURFACE
C-----------------------------------------------------------
      CALL FVVENT0(
     1             ELSOUT  ,AOUTOT  ,NVENT    ,NELT    ,ITTF    ,
     2             ELAREA  ,ELSINI  ,ELEM     ,ITAGEL  ,SVENT   ,
     3             IBAGHOL ,RVOLU   ,RBAGHOL  ,PORO    ,P       ,
     4             ELTG    ,IPARG   ,MATTG    ,NEL     ,POROSITY,
     5             IPM     ,PM      ,ELBUF_TAB,IGROUPC ,IGROUPTG)
C---------------------------------------------------
C FINITE VOLUME BALANCES
C---------------------------------------------------
!$OMP PARALLEL PRIVATE(I,J,JJ,K,KK,IEL,JEL,AREA,N1,N2,N3)
!$OMP+ PRIVATE(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X12,Y12,Z12,X23,Y23,Z23,Z31,Y31,X31)
!$OMP+ PRIVATE(L12,L23,L31,H1,NX,NY,NZ,IINJ,DMI,IVENT,IDEF,ITVENT,AOUT)
!$OMP+ PRIVATE(P1,RHO1,RE1,GAMA1,UX1,UY1,UZ1,UU,RHO2,VV,ETA,KSI,PSTAG)
!$OMP+ PRIVATE(PCRIT,P2,VMAX,IVDP,FVDP,GAMAI,RHOI,DMOUT,DHOUT,CPAI,CPBI,CPCI)
!$OMP+ PRIVATE(RMWI,CPDI,CPEI,CPFI,VX1,VY1,VZ1,VX2,VY2,VZ2,VX3,VY3,VZ3,VVX,VVY,VVZ,I1,I2,GAMA2)
!$OMP+ PRIVATE(VFX,VFY,VFZ,SS_,ALPHA,RHOM,RUXM,RUYM,RUZM,REM,MASSFLOW,CPAM,CPBM,CPCM,RGASM,CPDM,CPEM,CPFM)
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
C TRANSPORTS
C----------------------
         PU(1,1:NPOLH)=QPOLH(1,1:NPOLH)/MPOLH(1:NPOLH)
         PU(2,1:NPOLH)=QPOLH(2,1:NPOLH)/MPOLH(1:NPOLH)
         PU(3,1:NPOLH)=QPOLH(3,1:NPOLH)/MPOLH(1:NPOLH)
!$OMP END SECTIONS


!     ==================    !
!     FLUXES COMPUTATION    !
!     ==================    !
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
         DO IEL = 1, NELT
            MASS_FLUX(IEL) = ZERO
            MOMENTUM_FLUX_X(IEL) = ZERO
            MOMENTUM_FLUX_Y(IEL) = ZERO
            MOMENTUM_FLUX_Z(IEL) = ZERO
            ENERGY_FLUX(IEL) = ZERO
            CPA_FLUX(IEL) = ZERO
            CPB_FLUX(IEL) = ZERO
            CPC_FLUX(IEL) = ZERO
            RGAS_FLUX(IEL) = ZERO
            IF (ITYP == 8) THEN
               CPD_FLUX(IEL) = ZERO
               CPE_FLUX(IEL) = ZERO
               CPF_FLUX(IEL) = ZERO
            ENDIF
            ELFMASS(IEL) = ZERO
            ELFEHPY(IEL) = ZERO
            ELFVEL(IEL) = ZERO
            VEL(IEL) = ZERO
!     SURFACE
            AREA = ELAREA(IEL)
            SS(IEL) = ZERO
            VREL(IEL) = ZERO
!     NORMAL VECTOR
            NX = NORM(1, IEL)
            NY = NORM(2, IEL)
            NZ = NORM(3, IEL)
            IF (ITAGEL(IEL) > 0) THEN
!     INJECTOR
               IINJ = ITAGEL(IEL)
               DMI = DATAINJ(2, IINJ) * AREA / DATAINJ(1, IINJ)
               MASS_FLUX(IEL) = DMI
               MOMENTUM_FLUX_X(IEL) = -DMI * NX * DATAINJ(3, IINJ)
               MOMENTUM_FLUX_Y(IEL) = -DMI * NY * DATAINJ(3, IINJ)
               MOMENTUM_FLUX_Z(IEL) = -DMI * NZ * DATAINJ(3, IINJ)
               ENERGY_FLUX(IEL) = DMI * DATAINJ(4, IINJ)
               CPA_FLUX(IEL) = DMI * RBAGJET(2, IINJ)
               CPB_FLUX(IEL) = DMI * RBAGJET(3, IINJ)
               CPC_FLUX(IEL) = DMI * RBAGJET(4, IINJ)
               RGAS_FLUX(IEL) = DMI * RBAGJET(1, IINJ)
               IF (ITYP == 8) THEN
                  CPD_FLUX(IEL) = DMI * RBAGJET(16, IINJ)
                  CPE_FLUX(IEL) = DMI * RBAGJET(17, IINJ)
                  CPF_FLUX(IEL) = DMI * RBAGJET(18, IINJ)
               ENDIF
               ELFMASS(IEL) = ELFMASS(IEL) + DMI * DT1
               ELFEHPY(IEL) = ELFEHPY(IEL) + DMI * DATAINJ(4, IINJ) * DT1
               ELFVEL(IEL) = -DATAINJ(3, IINJ)
            ELSE IF (ITAGEL(IEL) < 0) THEN
!     VENT HOLE
               IVENT = -ITAGEL(IEL)
               IDEF = IBAGHOL(1, IVENT)
               ITVENT = IBAGHOL(10, IVENT)
               AOUT = ELSOUT(IEL)
               IF (FVDATA(IFV)%TRI_TO_ELEM(1, IEL) /= FVDATA(IFV)%TRI_TO_ELEM(2, IEL)) THEN
                  CALL ABORT()
               ENDIF
               I = FVDATA(IFV)%TRI_TO_ELEM(1, IEL)
               P1 = PPOLH(I)
               RHO1 = RPOLH(I)
               RE1 = EPOLH(I) / PVOLU(I)
               GAMA1 = GPOLH(I)
               UX1 = PU(1,I)
               UY1 = PU(2,I)
               UZ1 = PU(3,I)
               UU = ZERO
!     LOCAL VELOCITY
               IF (ITVENT == 1) THEN
                  UU = NX * UX1 + NY * UY1 + NZ * UZ1
                  RHO2 = RHO1
                  IF (P1 < PEXT) UU = ZERO
!     BERNOULLI
               ELSEIF ((ITVENT == 2 .AND. P1 > ZERO) .OR. (ITVENT == 4 .AND. P1 >= PEXT)) THEN
                  VV = NX * UX1 + NY * UY1 + NZ * UZ1
                  VV = MAX(VV, ZERO)
                  ETA = (GAMA1 - ONE) / GAMA1
                  KSI = ONE / ETA
                  PSTAG = P1 * (ONE + HALF * ETA * RHO1 * VV * VV / P1)**KSI
                  PCRIT = PSTAG * (TWO / (GAMA1+ONE))**KSI
                  P2 = MAX(PEXT, PCRIT)
                  RHO2 = RHO1 * (P2 / P1)**(ONE / GAMA1)
                  UU = TWO * P1 * (ONE - (P2 / P1)**ETA) / (RHO1 * ETA)
                  UU = MAX(UU, ZERO)
                  UU = SQRT(UU)
                  VMAX = HALF * ((P1 - PEXT) * PVOLU(I) / (GAMA1 - ONE))
     .                 / MAX(EM20, RHO2 * AOUT * DT1 * GAMA1 * RE1 / RHO1)
                  VMAX = MAX(VMAX, ZERO)
                  UU = MIN(UU, VMAX)
!     CHEMKIN
               ELSE IF (ITVENT == 3) THEN
                  P2 = P1 - PEXT
                  IVDP = IBAGHOL(9, IVENT)
                  FVDP = RBAGHOL(13, IVENT)
                  UU = FVDP * GET_U_FUNC(IVDP, P2 * SCALP, DERI)
                  IF (P2 < ZERO) THEN
                     RHO2 = RVOLU(62)
                  ELSE
                     RHO2 = RHO1
                  ENDIF
!     BERNOULLI WITH FLOW-IN
               ELSEIF (ITVENT == 4 .AND. P1 < PEXT) THEN
                  GAMAI = RVOLU(1)
                  RHOI = RVOLU(62)
                  ETA = (GAMAI - ONE) / GAMAI
                  PCRIT = PEXT * (TWO / (GAMAI + ONE))**(ONE / ETA)
                  P2 = MAX(P1, PCRIT)
                  RHO2 = RHOI * (P2 / PEXT)**(ONE / GAMAI)
                  UU = TWO * PEXT * (ONE - (P2 / PEXT)**ETA)/(RHOI * ETA)
                  UU = MAX(UU, ZERO)
                  UU = SQRT(UU)
                  VMAX = HALF*((PEXT - P1) * PVOLU(I) / (GAMA1 - ONE))
     .                 / MAX(EM20, RHO2 * AOUT * DT1 * RVOLU(63))
                  VMAX = MAX(VMAX, ZERO)
                  UU = MIN(UU, VMAX)
                  UU = -UU
!     GRAEFE
               ELSE IF (ITVENT == 5) THEN
                  P2 = P1 - PEXT
                  RHO2 = RHO1
                  UU = TWO * P2 / RHO2
                  UU = MAX(UU, ZERO)
                  UU = SQRT(UU)
               ENDIF

               IF (UU > ZERO .AND. IDEF == 1) THEN
                  DMOUT = UU * RHO2 * AOUT
                  MASS_FLUX(IEL) = -DMOUT
                  MOMENTUM_FLUX_X(IEL) = -DMOUT * UX1
                  MOMENTUM_FLUX_Y(IEL) = -DMOUT * UY1
                  MOMENTUM_FLUX_Z(IEL) = -DMOUT * UZ1
                  DHOUT = DMOUT * GAMA1 * RE1 / RHO1
                  ENERGY_FLUX(IEL) = -DHOUT
                  CPAI = CPAPOLH(I)
                  CPBI = CPBPOLH(I)
                  CPCI = CPCPOLH(I)
                  RMWI = RMWPOLH(I)
                  CPA_FLUX(IEL) = -DMOUT * CPAI
                  CPB_FLUX(IEL) = -DMOUT * CPBI
                  CPC_FLUX(IEL) = -DMOUT * CPCI
                  RGAS_FLUX(IEL) = -DMOUT * RMWI
                  IF(ITYP == 8) THEN
                     CPDI = CPDPOLH(I)
                     CPEI = CPEPOLH(I)
                     CPFI = CPFPOLH(I)
                     CPD_FLUX(IEL) = -DMOUT * CPDI
                     CPE_FLUX(IEL) = -DMOUT * CPEI
                     CPF_FLUX(IEL) = -DMOUT * CPFI
                  ENDIF
                  !/TODO
                  !DMINI(I)=DMINI(I)-DMOUT
                  ELFMASS(IEL)=ELFMASS(IEL)-DMOUT*DT1
                  ELFEHPY(IEL)=ELFEHPY(IEL)-DHOUT*DT1
                  ELFVEL(IEL)=ELFVEL(IEL)+UU*AREA/ELAREA(IEL)
                  ELDMASS(IEL)=-DMOUT
                  ELDEHPY(IEL)=-DHOUT
               ENDIF
C
               IF (UU < ZERO .AND. IDEF == 1) THEN
                  DMI = -UU * RHO2 * AOUT
                  MASS_FLUX(IEL) = DMI
                  MOMENTUM_FLUX_X(IEL) = DMI * UX1
                  MOMENTUM_FLUX_Y(IEL) = DMI * UY1
                  MOMENTUM_FLUX_Z(IEL) = DMI * UZ1
                  CPAI = RVOLU(7)
                  CPBI = RVOLU(8)
                  CPCI = RVOLU(9)
                  RMWI = RVOLU(10)
                  CPA_FLUX(IEL) = DMI * CPAI
                  CPB_FLUX(IEL) = DMI * CPBI
                  CPC_FLUX(IEL) = DMI * CPCI
                  RGAS_FLUX(IEL) = DMI * RMWI
                  IF (ITYP == 8) THEN
                     CPDI = RVOLU(56)
                     CPEI = RVOLU(57)
                     CPFI = RVOLU(58)
                     CPD_FLUX(IEL) = DMI * CPDI
                     CPE_FLUX(IEL) = DMI * CPEI
                     CPF_FLUX(IEL) = DMI * CPFI
                  ENDIF
                  ENERGY_FLUX(IEL) = DMI * RVOLU(63)
                  ELFMASS(IEL) = ELFMASS(IEL) + DMI * DT1
                  ELFEHPY(IEL) = ELFEHPY(IEL) + DMI * DT1 * RVOLU(63)
                  ELFVEL(IEL) = ELFVEL(IEL) + UU * AREA / ELAREA(IEL)
                  ELDMASS(IEL) = DMI
                  ELDEHPY(IEL) = DMI * RVOLU(63)
               ENDIF
            ELSE IF (IEL > NEL) THEN
!     TRIANGLE ON INTERNAL SURFACE TAKE POROSITY INTO ACCOUNT
               IF (POROSITY(IEL - NEL) /= ZERO) THEN
                  N1=ELEM(1,IEL)
                  N2=ELEM(2,IEL)
                  N3=ELEM(3,IEL)
                  IF (TAGELA(IEL) > 0) THEN
                     VX1=VVV(1,N1)
                     VY1=VVV(2,N1)
                     VZ1=VVV(3,N1)
                     VX2=VVV(1,N2)
                     VY2=VVV(2,N2)
                     VZ2=VVV(3,N2)
                     VX3=VVV(1,N3)
                     VY3=VVV(2,N3)
                     VZ3=VVV(3,N3)
                  ELSE
                     VX1=VVVA(1,N1)
                     VY1=VVVA(2,N1)
                     VZ1=VVVA(3,N1)
                     VX2=VVVA(1,N2)
                     VY2=VVVA(2,N2)
                     VZ2=VVVA(3,N2)
                     VX3=VVVA(1,N3)
                     VY3=VVVA(2,N3)
                     VZ3=VVVA(3,N3)
                  ENDIF
                  !grid velocity
                  VVX=THIRD*(VX1+VX2+VX3)
                  VVY=THIRD*(VY1+VY2+VY3)
                  VVZ=THIRD*(VZ1+VZ2+VZ3)
                  !normal vector
                  NX=NORM(1,IEL)
                  NY=NORM(2,IEL)
                  NZ=NORM(3,IEL)
                  !normal velocity
                  VEL(IEL)=NX*VVX+NY*VVY+NZ*VVZ
                  I1 = FVDATA(IFV)%TRI_TO_ELEM(1, IEL)
                  I2 = FVDATA(IFV)%TRI_TO_ELEM(2, IEL)
                  IF (I1 == I2) THEN
                     CALL ABORT()
                  ENDIF
                  AREA = ELAREA(IEL) * POROSITY(IEL - NEL)
                  RHO1 = RPOLH(I1)
                  RE1 = EPOLH(I1) / PVOLU(I1)
                  UX1 = PU(1, I1)
                  UY1 = PU(2, I1)
                  UZ1 = PU(3, I1)
                  GAMA1 = GPOLH(I1)
                  RHO2 = RPOLH(I2)
                  RE2 = EPOLH(I2) / PVOLU(I2)
                  UX2 = PU(1, I2)
                  UY2 = PU(2, I2)
                  UZ2 = PU(3, I2)
                  GAMA2 = GPOLH(I2)
                  VFX = HALF * (UX1 + UX2)                 
                  VFY = HALF * (UY1 + UY2)
                  VFZ = HALF * (UZ1 + UZ2)
                  VREL(IEL) = (VFX - VVX)*(VFX - VVX) + (VFY - VVY)*(VFY - VVY) +  (VFZ - VVZ)*(VFZ - VVZ)
                  SS_= NX * (VFX - VVX) + NY * (VFY - VVY) + NZ * (VFZ - VVZ)
                  SS(IEL) = ELAREA(IEL)*ABS(SS_)
                  IF (SS_ <= ZERO) THEN
                     ALPHA = ZERO
                  ELSE
                     ALPHA = ONE
                  ENDIF
                  RHOM = ALPHA * RHO1 + (ONE - ALPHA) * RHO2
                  RUXM = ALPHA * RHO1 * UX1 + (ONE - ALPHA) * RHO2 * UX2
                  RUYM = ALPHA * RHO1 * UY1 + (ONE - ALPHA) * RHO2 * UY2
                  RUZM = ALPHA * RHO1 * UZ1 + (ONE - ALPHA) * RHO2 * UZ2
                  REM = ALPHA * GAMA1 * RE1 + (ONE - ALPHA) * GAMA2 * RE2

                  MASSFLOW = RHOM * SS_ * AREA
                  MASS_FLUX(IEL) = -MASSFLOW
                  MOMENTUM_FLUX_X(IEL) = -RUXM * SS_ * AREA
                  MOMENTUM_FLUX_Y(IEL) = -RUYM * SS_ * AREA
                  MOMENTUM_FLUX_Z(IEL) = -RUZM * SS_ * AREA
                  ENERGY_FLUX(IEL) = -REM * SS_ * AREA

                  ELFMASS(IEL) = ELFMASS(IEL) + MASSFLOW * DT1
                  ELFVEL(IEL) = ELFVEL(IEL) + SS_ * AREA

                  CPAM = ALPHA * CPAPOLH(I1) + (ONE - ALPHA) * CPAPOLH(I2)
                  CPBM = ALPHA * CPBPOLH(I1) + (ONE - ALPHA) * CPBPOLH(I2)
                  CPCM = ALPHA * CPCPOLH(I1) + (ONE - ALPHA) * CPCPOLH(I2)
                  RGASM = ALPHA * RMWPOLH(I1) + (ONE - ALPHA) * RMWPOLH(I2)
                  CPA_FLUX(IEL) = -MASSFLOW * CPAM
                  CPB_FLUX(IEL) = -MASSFLOW * CPBM
                  CPC_FLUX(IEL) = -MASSFLOW * CPCM
                  RGAS_FLUX(IEL) = -MASSFLOW * RGASM
                  IF (ITYP == 8) THEN
                     CPDM = ALPHA * CPDPOLH(I1) + (ONE - ALPHA) * CPDPOLH(I2)
                     CPEM = ALPHA * CPEPOLH(I1) + (ONE - ALPHA) * CPEPOLH(I2)
                     CPFM = ALPHA * CPFPOLH(I1) + (ONE - ALPHA) * CPFPOLH(I2)
                     CPD_FLUX(IEL) = -MASSFLOW * CPDM
                     CPE_FLUX(IEL) = -MASSFLOW * CPEM
                     CPF_FLUX(IEL) = -MASSFLOW * CPFM
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
!$OMP END DO

!$OMP END PARALLEL

!$OMP PARALLEL PRIVATE(I,MSINI,CPA,CPB,CPC,CPD,CPE,CPF,EFAC,TEMP0)
!$OMP+ PRIVATE(TEMP,CP,CV,RMW)
!$OMP+ PRIVATE(AL,DD,AD,GAMA,SSP,QX,VV,SS_,AREA)
! --- PRIVATE(AREAMAX)
!$OMP+ PRIVATE(ITYPL,J,IEL,COUNT)

!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I = 1, NPOLH
         MSINI = MPOLH(I)
         CPAPOLH(I) = MSINI * CPAPOLH(I)
         CPBPOLH(I) = MSINI * CPBPOLH(I)
         CPCPOLH(I) = MSINI * CPCPOLH(I)
         RMWPOLH(I) = MSINI * RMWPOLH(I)
         IF (ITYP == 8) THEN
            CPDPOLH(I) = MSINI * CPDPOLH(I)
            CPEPOLH(I) = MSINI * CPEPOLH(I)
            CPFPOLH(I) = MSINI * CPFPOLH(I)
         ENDIF
         DO J = 1, FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS_SIZE
            IEL = FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS(J)
!     FINITE VOLUME I IS ON THE LEFT SIDE OF ELEMENT IEL, HENCE A + SIGN
            MPOLH(I) = MPOLH(I) + MASS_FLUX(IEL) * DT1
            QPOLH(1, I) = QPOLH(1, I) + MOMENTUM_FLUX_X(IEL) * DT1
            QPOLH(2, I) = QPOLH(2, I) + MOMENTUM_FLUX_Y(IEL) * DT1
            QPOLH(3, I) = QPOLH(3, I) + MOMENTUM_FLUX_Z(IEL) * DT1
            EPOLH(I) = EPOLH(I) + ENERGY_FLUX(IEL) * DT1
            IF (MPOLH(I) <= ZERO .OR. EPOLH(I) <= ZERO) THEN
               TPOLH(I) = ZERO
            ELSE
               CPAPOLH(I) = CPAPOLH(I) + CPA_FLUX(IEL) * DT1
               CPBPOLH(I) = CPBPOLH(I) + CPB_FLUX(IEL) * DT1
               CPCPOLH(I) = CPCPOLH(I) + CPC_FLUX(IEL) * DT1
               RMWPOLH(I) = RMWPOLH(I) + RGAS_FLUX(IEL) * DT1
               IF (ITYP == 8) THEN
                  CPDPOLH(I) = CPDPOLH(I) + CPD_FLUX(IEL) * DT1
                  CPEPOLH(I) = CPEPOLH(I) + CPE_FLUX(IEL) * DT1
                  CPFPOLH(I) = CPFPOLH(I) + CPF_FLUX(IEL) * DT1
               ENDIF
            ENDIF
         ENDDO
         DO J = 1, FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS_SIZE
            IEL = FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS(J)
!     FINITE VOLUME I IS ON THE RIGHT SIDE OF ELEMENT IEL, HENCE A - SIGN
            MPOLH(I) = MPOLH(I) - MASS_FLUX(IEL) * DT1
            QPOLH(1, I) = QPOLH(1, I) - MOMENTUM_FLUX_X(IEL) * DT1
            QPOLH(2, I) = QPOLH(2, I) - MOMENTUM_FLUX_Y(IEL) * DT1
            QPOLH(3, I) = QPOLH(3, I) - MOMENTUM_FLUX_Z(IEL) * DT1
            EPOLH(I) = EPOLH(I) - ENERGY_FLUX(IEL) * DT1
            IF (MPOLH(I) <= ZERO .OR. EPOLH(I) <= ZERO) THEN
               TPOLH(I) = ZERO
            ELSE
               CPAPOLH(I) = CPAPOLH(I) - CPA_FLUX(IEL) * DT1
               CPBPOLH(I) = CPBPOLH(I) - CPB_FLUX(IEL) * DT1
               CPCPOLH(I) = CPCPOLH(I) - CPC_FLUX(IEL) * DT1
               RMWPOLH(I) = RMWPOLH(I) - RGAS_FLUX(IEL) * DT1
               IF (ITYP == 8) THEN
                  CPDPOLH(I) = CPDPOLH(I) - CPD_FLUX(IEL) * DT1
                  CPEPOLH(I) = CPEPOLH(I) - CPE_FLUX(IEL) * DT1
                  CPFPOLH(I) = CPFPOLH(I) - CPF_FLUX(IEL) * DT1
               ENDIF
            ENDIF
         ENDDO
!     TEMPERATURE COMPUTATION
         EFAC = EPOLH(I) / MPOLH(I)
         CPAPOLH(I) = CPAPOLH(I) / MPOLH(I)
         CPBPOLH(I) = CPBPOLH(I) / MPOLH(I)
         CPCPOLH(I) = CPCPOLH(I) / MPOLH(I)
         RMWPOLH(I) = RMWPOLH(I) / MPOLH(I)
         IF (ITYP == 8) THEN
            CPDPOLH(I) = CPDPOLH(I) / MPOLH(I)
            CPEPOLH(I) = CPEPOLH(I) / MPOLH(I)
            CPFPOLH(I) = CPFPOLH(I) / MPOLH(I)
         ENDIF
         CPA = CPAPOLH(I)
         CPB = CPBPOLH(I)
         CPC = CPCPOLH(I)
         RMW = RMWPOLH(I)
         IF (ITYP == 8) THEN
            CPD = CPDPOLH(I)
            CPE = CPEPOLH(I)
            CPF = CPFPOLH(I)
         ENDIF
         TEMP0 = RVOLU(25)
         CALL FVTEMP(ITYP , EFAC , CPA  , CPB  , CPC  ,
     .        CPD   , CPE  , CPF  , RMW  , TEMP0,
     .        TEMP  )
         CP = CPA + CPB * TEMP + CPC * TEMP * TEMP
         IF (ITYP == 8) THEN
            CP = CP + CPD * TEMP**3 + CPF * TEMP**4
            IF (CPE /= ZERO .AND. TEMP > ZERO) THEN
               CP = CP + CPE / (TEMP * TEMP)
            ENDIF
         ENDIF
         CV = CP - RMW
         RMWPOLH(I) = RMW
         GPOLH(I) = CP / CV
         TPOLH(I) = TEMP
!     PRESSURE AND MASS DENSITY
         RPOLH(I) = MPOLH(I) / PVOLU(I)
         PPOLH(I) = RPOLH(I) * RMW * TEMP
      ENDDO
!$OMP END DO

C---------------------------------------------------
C QA, QB AND STABILITY
C---------------------------------------------------
!$OMP SINGLE

      ! /DT/FVMBAG/[ID_DT_OPTION]
      !   ID_DT_OPTION = 0 obsolete
      !   ID_DT_OPTION = 1 (legacy method), apply to all FVMBAG1
      !   ID_DT_OPTION = 2 (2022.3), apply to given FVMBAG1 or FVMBAG2. 
      !                              in this case characteristic length is calculated depending on L_TYPE flag
      !                              and smoothing is applied :  dt <- min(dt,  dt_old*(1+lambda))
                   !--------!-------------------------------------------------------------------!
                   ! VALUE  !   DESCRIPTION                                                     !
      !------------!--------!-------------------------------------------------------------------!
      !            ! 0,1    !    Constant (Starter minimum characteristic length)               !
      !  L_TYPE    !  2     !    VOLUME^1/3 at current time                                     !
      !            !  3     !    VOLUME / SMAX at current time                                  !
      !------------!--------!-------------------------------------------------------------------!
      

      !------------------------------!      
      IDT_FVMBAG = FVDATA(IFV)%ID_DT_OPTION     !IDTMIN(52)  ! /DT/FVMBAG/[IDT_FVMBAG]

      QA=RVOLU(46)
      QB=RVOLU(47)
      DTX=EP30
      PHDT=0
      
!$OMP END SINGLE

      !------------------------------!
      ! --- CHARACTERISTIC LENGTH ---!
      !------------------------------!
      IDT_FVMBAG = FVDATA(IFV)%ID_DT_OPTION  ! /DT/FVMBAG/[IDT_FVMBAG]
              
      SELECT CASE(IDT_FVMBAG)

        ! /DT/FVMBAG/1 
        CASE(0,1)
        ! ------------ 
           PARAM_L_TYPE = 0
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)        
          DO I=1,NPOLH
             IF (IBPOLH(I) == 0) THEN
                DLS(I)=DLH !POLYHEDRON CONSTANT CHARACTERISTIC LENGTH COMPUTED IN THE STARTER
             ELSE
               DLS(I)=PVOLU(I)**THIRD !ADDITIONAL BRICKS
             ENDIF
          ENDDO
!$OMP END DO          

        ! /DT/FVMBAG/2
        CASE(2) 
        ! ------------   
          PARAM_L_TYPE = FVDATA(IFV)%L_TYPE  
                
          SELECT CASE(PARAM_L_TYPE)
            CASE(0,1) !dlh = starter constant
            ! /DT/FVMBAG/2 + L_TYPE=0,1 
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
              DO I=1,NPOLH
                DLS(I)=DLH 
              ENDDO
!$OMP END DO          
            CASE(2) ! dl=VOL**1/3
            ! /DT/FVMBAG/2 + L_TYPE=2
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
              DO I=1,NPOLH
                DLS(I)=PVOLU(I)**THIRD  
              ENDDO
!$OMP END DO       
            CASE(3) ! dl=VOL**1/3
            ! /DT/FVMBAG/2 + L_TYPE=3
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
      SELECT CASE (IDT_FVMBAG)

        CASE(0,1)
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
          DO I=1,NPOLH
             IF(MPOLH(I)<=ZERO.OR.EPOLH(I)<=ZERO.OR.GPOLH(I)<=ONE) THEN
               DTPOLH(I)=EP30
               QVISC(I) =ZERO
             ELSE 
C
               AL=PVOLU(I)**THIRD
               DD=DM(I)/MPOLH(I)
               AD=MAX(ZERO,DD)
               GAMA=GPOLH(I)
               SSP=SQRT((GAMA-ONE)*GAMA*EPOLH(I)/MPOLH(I))
               QVISC(I)=RPOLH(I)*AD*AL*(QA*QA*AL*AD+QB*SSP)
C
               QX=QB*SSP+AL*QA*QA*AD                        
               SSP=QX+SQRT(QX*QX+SSP*SSP)                   
               VV=SQRT(PU(1,I)**2+PU(2,I)**2+PU(3,I)**2)    
               DTPOLH(I)=CFL_COEF*DLS(I)/(SSP+VV)           
             ENDIF
          ENDDO
!$OMP END DO

        CASE(2) !time step computed from a characteristic length
        
         PARAM_L_TYPE = FVDATA(IFV)%L_TYPE !characteristic length formulation
        
          IF(PARAM_L_TYPE /= 3)THEN !L_TYPE=0/1 & 2        
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
           DO I=1,NPOLH
             IF(MPOLH(I) <= ZERO.OR.EPOLH(I) <= ZERO.OR.GPOLH(I) <= ONE) THEN
                DTPOLH(I)=EP30
                QVISC(I) =ZERO
             ELSE
               ! 1.calculating Grid Velocity for each polyedron
               COUNT = 0
               VV = ZERO
               !loop over polygons
               !AREAMAX=ZERO
               DO J = 1, FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS_SIZE
                  IEL = FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS(J)
                  AREA = ELAREA(IEL)          
!                  FINITE VOLUME I IS ON THE LEFT SIDE OF ELEMENT IEL, HENCE A + SIGN
                  !AREAMAX=MAX(AREAMAX,AREA)
                  VV = VV+SQRT(VREL(IEL))
                  COUNT=COUNT+1
               ENDDO
               DO J = 1, FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS_SIZE
                  IEL = FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS(J)    
!                 FINITE VOLUME I IS ON THE RIGHT SIDE OF ELEMENT IEL, HENCE A - SIGN
                  VV = VV+SQRT(VREL(IEL))                  
                  COUNT=COUNT+1
               ENDDO
               AL=PVOLU(I)**THIRD
               DD=DM(I)/MPOLH(I)
               AD=MAX(ZERO,DD)
               GAMA=GPOLH(I)
               SSP=SQRT((GAMA-ONE)*GAMA*EPOLH(I)/MPOLH(I))
               QVISC(I)=RPOLH(I)*AD*AL*(QA*QA*AL*AD+QB*SSP)
               QX=QB*SSP+AL*QA*QA*AD
               SSP=QX+SQRT(QX*QX+SSP*SSP)
               ! 3.relative velocty (PU:material velocity : PW:grid velocity)
               IF(COUNT > 0)THEN
                 VV = VV / FLOAT(COUNT) 
               ENDIF               
               ! 4.time step            
               DTPOLH(I)=CFL_COEF*DLS(I)/(SSP+VV)
             ENDIF
           ENDDO
!$OMP END DO

          ELSE !L_TYPE=3 ! check all faces and compute normal relative velocities
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
           DO I = 1, NPOLH
             SS_=ZERO
             DO J = 1, FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS_SIZE
                IEL = FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS(J)
!          FINITE VOLUME I IS ON THE LEFT SIDE OF ELEMENT IEL, HENCE A + SIGN
                SS_=MAX(SS_,SS(IEL)) ! <|u-w|,n>
             ENDDO
             DO J = 1, FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS_SIZE
                IEL = FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS(J)
!          FINITE VOLUME I IS ON THE RIGHT SIDE OF ELEMENT IEL, HENCE A - SIGN
                SS_=MAX(SS_,SS(IEL)) ! <|u-w|,n>
             ENDDO
             
             AL=PVOLU(I)**THIRD                                                          
             DD=DM(I)/MPOLH(I)                                                           
             AD=MAX(ZERO,DD)                                                             
             GAMA=GPOLH(I)                                                               
             SSP=SQRT((GAMA-ONE)*GAMA*EPOLH(I)/MPOLH(I))                                 
             QVISC(I)=RPOLH(I)*AD*AL*(QA*QA*AL*AD+QB*SSP)                                
             QX=QB*SSP+AL*QA*QA*AD                                                       
             SSP=QX+SQRT(QX*QX+SSP*SSP)                                                  
             ! 3.relative velocty (PU:material velocity : PW:grid velocity)              
             !    SS_ <- max( area*(u-w).n )
             !    u-w = 0 unless in case of porosity
             ! 4.time step                                                             
             DTPOLH(I)=CFL_COEF*DLS(I)/(SSP+SS_)                                  
           ENDDO
!$OMP END DO
          ENDIF

      END SELECT


!$OMP END PARALLEL

      DO I=1,NPOLH
         IF (DTPOLH(I)<DTX) THEN
            DTX=DTPOLH(I)
            PHDT=I
         ENDIF
      ENDDO
      
      !optional time step growth limitor (/DT/FVMBAG/2, 3rd parameter)
      IF(NCYCLE > 0  .AND.  IDT_FVMBAG == 2)THEN
        LAMBDA = FVDATA(IFV)%LAMBDA
        IF(LAMBDA > ZERO)THEN
          DTOLD = FVDATA(IFV)%DTOLD   
          IF(DTOLD > ZERO)THEN     
            DTX = MIN( DTX, DTOLD*(ONE+LAMBDA))
          ENDIF
        ENDIF
      ENDIF
      FVDATA(IFV)%DTOLD = DTX      

      IF (ILVOUT >= 1.AND.IPRI==0) THEN
         WRITE(IOUT,'(A,I10,A,E12.4,A,I10)')' ** MONITORED VOLUME ID: ',IVOLU(1),' TIME STEP:',DTX,' FINITE VOLUME:',IDPOLH(PHDT)
      ENDIF

C
      IF (DTX<DT2) THEN
         DT2=DTX
         NELTS=IVOLU(1)
         ITYPTS=52
      ENDIF
C---------------------------------------------------
C PRESSURE FORCES AND WORK COMPUTATION
C---------------------------------------------------
!$OMP PARALLEL PRIVATE(N1,N2,N3,IEL,NX,NY,NZ)
!$OMP+ PRIVATE(VX1,VX2,VX3,VY1,VY2,VY3,VZ1,VZ2,VZ3)
!$OMP+ PRIVATE(VVX,VVY,VVZ,I1,I2,AREA,IVENT,IDEF,IVENTYP,AOUT,PP,P1,P2,PMEAN,AREA_OLD,AREA1,SS_)
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
         I1 = FVDATA(IFV)%TRI_TO_ELEM(1, IEL)
         I2 = FVDATA(IFV)%TRI_TO_ELEM(2, IEL)
         AREA = ELAREA(IEL)
         MOMENTUM_FLUX_X(IEL) = ZERO
         MOMENTUM_FLUX_Y(IEL) = ZERO
         MOMENTUM_FLUX_Z(IEL) = ZERO
         ENERGY_FLUX(IEL) = ZERO
         FEL(IEL) = ZERO
         IF (I1 == I2) THEN
!     EXTERNAL TRIANGLE
            IF (ITAGEL(IEL) < 0) THEN
!     VENT HOLE
               IVENT = -ITAGEL(IEL)
               IDEF = IBAGHOL(1, IVENT)
               IVENTYP = IBAGHOL(13, IVENT)
               IF (IDEF == 1 .AND. IVENTYP == 0) THEN
                  AOUT = ELSOUT(IEL)
                  AREA = AREA - AOUT
                  PP = PEXT
                  FEL(IEL) = PP * AOUT
                  MOMENTUM_FLUX_X(IEL) = -PP * AOUT * NX
                  MOMENTUM_FLUX_Y(IEL) = -PP * AOUT * NY
                  MOMENTUM_FLUX_Z(IEL) = -PP * AOUT * NZ
                  ENERGY_FLUX(IEL) = -PP * VEL(IEL) * AOUT
               ENDIF
            ENDIF
            PP = PPOLH(I1) + QVISC(I1)
            FEL(IEL) = FEL(IEL) + PP * AREA
            MOMENTUM_FLUX_X(IEL) = MOMENTUM_FLUX_X(IEL) - PP * AREA * NX
            MOMENTUM_FLUX_Y(IEL) = MOMENTUM_FLUX_Y(IEL) - PP * AREA * NY
            MOMENTUM_FLUX_Z(IEL) = MOMENTUM_FLUX_Z(IEL) - PP * AREA * NZ
            ENERGY_FLUX(IEL) = ENERGY_FLUX(IEL) - PP * VEL(IEL) * AREA
            ENERGY_FLUX(IEL) = ENERGY_FLUX(IEL) - RVOLU(19) * AREA * (TPOLH(I1) - RVOLU(25))
         ELSE
!     INTERNAL TRIANGLE, SEPARATES 2 FINITE VOLUMES
            P1 = PPOLH(I1) + QVISC(I1)
            P2 = PPOLH(I2) + QVISC(I2)
            PMEAN = HALF * (P1 + P2)
            AREA_OLD = AREA
            AREA = AREA * POROSITY(IEL - NEL)
            AREA1 = AREA_OLD - AREA
            SS_ = NX * VVX + NY * VVY + NZ * VVZ
            IF (ITAGEL(IEL) > 0) THEN
!     INJECTOR
               FEL(IEL) = (P1 - P2) * AREA1
               MOMENTUM_FLUX_X(IEL) = -PMEAN * NX * AREA
               MOMENTUM_FLUX_Y(IEL) = -PMEAN * NY * AREA
               MOMENTUM_FLUX_Z(IEL) = -PMEAN * NZ * AREA
               ENERGY_FLUX(IEL) = -PMEAN * SS_ * AREA
            ELSE
               FEL(IEL) = (P1 - P2) * AREA1
               MOMENTUM_FLUX_X(IEL) = -PMEAN * NX * AREA
               MOMENTUM_FLUX_Y(IEL) = -PMEAN * NY * AREA
               MOMENTUM_FLUX_Z(IEL) = -PMEAN * NZ * AREA
               ENERGY_FLUX(IEL) = -PMEAN * SS_ * AREA
            ENDIF
         ENDIF
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

C---------------------------------------------------
C NEW QUANTITIES IN POLYEDRA
C---------------------------------------------------
!$OMP PARALLEL PRIVATE(I, J, IEL)
!$OMP+ PRIVATE(NX, NY, NZ, N1, N2, N3, VX1, VX2, VX3, VY1, VY2, VY3, VZ1, VZ2, VZ3)
!$OMP+ PRIVATE(VVX, VVY, VVZ, I1, I2, PP, AREA_OLD, AREA, AREA1, SS_)
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO I = 1, NPOLH
         DO J = 1, FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS_SIZE
            IEL = FVDATA(IFV)%ELEM_TO_TRI(I)%MINUS(J)
            NX=NORM(1,IEL)
            NY=NORM(2,IEL)
            NZ=NORM(3,IEL)
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
            AREA = ELAREA(IEL)
            I1 = FVDATA(IFV)%TRI_TO_ELEM(1, IEL)
            I2 = FVDATA(IFV)%TRI_TO_ELEM(2, IEL)
            IF (I1 /= I) THEN
               CALL ABORT()
            ENDIF
            PP = PPOLH(I) + QVISC(I)
            IF (IEL > NEL) THEN
               AREA_OLD = AREA
               AREA = AREA * POROSITY(IEL - NEL)
               AREA1 = AREA_OLD - AREA
            ELSE
               AREA1 = ZERO
            ENDIF

            SS_ = NX * VVX + NY * VVY + NZ * VVZ

!     FINITE VOLUME I IS ON THE LEFT SIDE OF ELEMENT IEL, HENCE A + SIGN
            QPOLH(1, I) = QPOLH(1, I) + (MOMENTUM_FLUX_X(IEL) - PP * NX * AREA1) * DT1
            QPOLH(2, I) = QPOLH(2, I) + (MOMENTUM_FLUX_Y(IEL) - PP * NY * AREA1) * DT1
            QPOLH(3, I) = QPOLH(3, I) + (MOMENTUM_FLUX_Z(IEL) - PP * NZ * AREA1) * DT1
            IF (ITAGEL(IEL) > 0) THEN
               EPOLH(I) = EPOLH(I) + (ENERGY_FLUX(IEL) + PP * VEL(IEL) * AREA1) * DT1
            ELSE
               EPOLH(I) = EPOLH(I) + (ENERGY_FLUX(IEL) - PP * SS_ * AREA1) * DT1
            ENDIF
         ENDDO
         DO J = 1, FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS_SIZE
            IEL = FVDATA(IFV)%ELEM_TO_TRI(I)%PLUS(J)
            NX=NORM(1,IEL)
            NY=NORM(2,IEL)
            NZ=NORM(3,IEL)
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
            AREA = ELAREA(IEL)
            I1 = FVDATA(IFV)%TRI_TO_ELEM(1, IEL)
            I2 = FVDATA(IFV)%TRI_TO_ELEM(2, IEL)
            IF (I2 /= I) THEN
               CALL ABORT()
            ENDIF
            PP = PPOLH(I) + QVISC(I)
            IF (IEL > NEL) THEN
               AREA_OLD = AREA
               AREA = AREA * POROSITY(IEL - NEL)
               AREA1 = AREA_OLD - AREA
            ELSE
               AREA1 = ZERO
            ENDIF

            SS_ = NX * VVX + NY * VVY + NZ * VVZ


!     FINITE VOLUME I IS ON THE RIGHT SIDE OF ELEMENT IEL, HENCE A - SIGN
            QPOLH(1, I) = QPOLH(1, I) - (MOMENTUM_FLUX_X(IEL) - PP * NX * AREA1) * DT1
            QPOLH(2, I) = QPOLH(2, I) - (MOMENTUM_FLUX_Y(IEL) - PP * NY * AREA1) * DT1
            QPOLH(3, I) = QPOLH(3, I) - (MOMENTUM_FLUX_Z(IEL) - PP * NZ * AREA1) * DT1
            IF (ITAGEL(IEL) > 0) THEN
               EPOLH(I) = EPOLH(I) - (ENERGY_FLUX(IEL) - PP * VEL(IEL) * AREA1) * DT1
            ELSE
               EPOLH(I) = EPOLH(I) - (ENERGY_FLUX(IEL) - PP * SS_ * AREA1) * DT1
            ENDIF
         ENDDO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL

      ENDIF

C---------------------------------------------------
C PRINT OUTS
C---------------------------------------------------
 250  IF (ILVOUT >= 1 .AND. IPRI == 0) THEN
         WRITE(IOUT,*)
         WRITE(IOUT,'(A25,I10,A31)')' ** MONITORED VOLUME ID: ',IVOLU(1),' - FINITE VOLUME MESH UPDATE **'
         WRITE(IOUT,'(A,I8)')'    NUMBER OF FINITE VOLUMES : ',NPOLH
         WRITE(IOUT,'(A,G16.9,A17,G16.9,A1)')'    SUM VOLUME FINITE VOLUMES : ',VOLPH,'    (VOLUME AIRBAG: ',VOLG,')'
         WRITE(IOUT,'(A,G16.9,A17,G16.9,A1)')'    SUM AREA SURFACE POLYGONS : ',AREAP,'    (AREA AIRBAG  : ',AREAEL,')'
         WRITE(IOUT,*)
      ENDIF
C---------------------------------------------------
C TIME HISTORY
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
         IF(ITYP==8) THEN
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
C     PRESSURE DISPERSION
      PDISP = ZERO
      PMEAN2 = PMEAN / VOLPH
      DO I = 1, NPOLH
         PDISP = PDISP + PVOLU(I) * (PPOLH(I) - PMEAN2)**2
      ENDDO
      PDISP = SQRT(PDISP / VOLPH) / PMEAN2
      FSAV(19) = PDISP
      INJECTION_STARTED = .FALSE.
      DO IINJ = 1, NJET
         INJECTION_STARTED = INJECTION_STARTED .OR. (DATAINJ(2, IINJ) > ZERO)
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
           IF (ITAGEL(IEL)<0) THEN
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
           IF (IVOLU(74) == 0) THEN
              UP_SWITCH = TT-TTF >= RVOLU(70) .OR. NPOLH <= IVOLU(37)
           ENDIF
           IF (IVOLU(74) == 1) THEN
C     AUTOMATIC SWITCH TO UNIFORM PRESSURE WHEN DISPERSION OF PRESSURE IS LOW
              UP_SWITCH = ((PDISP < PDISP_OLD) .AND. (PDISP < RVOLU(73)))
              UP_SWITCH = UP_SWITCH .OR. TT-TTF >= RVOLU(70)
           ENDIF
           IF (IVOLU(74) == 2) THEN
C     AUTOMATIC SWITCH TO UNIFORM PRESSURE WHEN DISPERSION OF PRESSURE IS LOW
              UP_SWITCH = ((PDISP < PDISP_OLD) .AND. (PDISP < RVOLU(73)))
              UP_SWITCH = UP_SWITCH .AND. TT-TTF >= RVOLU(70)
           ENDIF

           IF (UP_SWITCH) THEN
              RVOLU(12)=FSAV(3)  ! PRESSION
              RVOLU(13)=FSAV(5)  ! TEMPERATURE
              RVOLU(16)=FSAV(2)  ! VOLUME
              RVOLU(20)=FSAV(1)  ! MASSE
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
C NODAL FLUID VELOCITIES
C---------------------------------------------------
      IF( (TT>=TANIM .AND. TT<=TANIM_STOP) .OR.TT>=TOUTP.OR. NBGAUGE > 0 .OR.
     .              (MANIM>=4.AND.MANIM<=15) ) THEN
C     COMPUTATIONS FOR ANIM, ALSO NEEDED IF THERE ARE GAUGES.
        PREPARE_ANIM = 1
        UEL(1:3,1:NELT)=ZERO
        RHOEL(1:NELT)=ZERO
        TKEL(1:NELT) =ZERO
        SSPEL(1:NELT)=ZERO
      ELSE
        PREPARE_ANIM = 0
      ENDIF

C     PEL(1:NELT)=ZERO

!$OMP PARALLEL PRIVATE(I,J,JJ,K,KK,IEL,JEL,II,NV)
!$OMP+ PRIVATE(NX,NY,NZ,NRX,NRY,NRZ)
!$OMP+ PRIVATE(FAC,AREA,I1,I2)
!$OMP DO SCHEDULE(DYNAMIC,MVSIZ)
      DO IEL = 1, NELT
         AREA = ELAREA(IEL)
         I1 = FVDATA(IFV)%TRI_TO_ELEM(1, IEL)
         I2 = FVDATA(IFV)%TRI_TO_ELEM(2, IEL)
         PEL(IEL) = HALF * (PPOLH(I1) + PPOLH(I2))
         IF (PREPARE_ANIM == 1) THEN
            UEL(1, IEL) = HALF * (PU(1, I1) + PU(1, I2))
            UEL(2, IEL) = HALF * (PU(2, I1) + PU(2, I2))
            UEL(3, IEL) = HALF * (PU(3, I1) + PU(3, I2))
            RHOEL(IEL) = HALF * (RPOLH(I1) + RPOLH(I2))
            TKEL(IEL) = HALF * (TPOLH(I1) + TPOLH(I2))
            SSPEL(IEL) = HALF * (SQRT(GPOLH(I1)*PPOLH(I1)/RPOLH(I1)) + SQRT(GPOLH(I2)*PPOLH(I2)/RPOLH(I2)))
         ENDIF
      ENDDO
!$OMP END DO

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
!$OMP SINGLE
        U(1:3,1:NNT)=ZERO
        RHO(1:NNT)  =ZERO
        TK(1:NNT)   =ZERO
        SSPK(1:NNT) =ZERO
        VOLA(1:NNA)=ZERO
        PA(1:NNA)=ZERO
        RHOA(1:NNA)=ZERO
        TKA(1:NNA)=ZERO
        SSPKA(1:NNA)=ZERO
        UA(1:3,1:NNA)=ZERO
!$OMP END SINGLE
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
           IF (IBPOLH(I)/=0) THEN
             TEMP=TPOLH(I)
             II=IABS(IBPOLH(I))
             IF(BRNA(1,II)==BRNA(4,II).AND.BRNA(5,II)==BRNA(8,II))THEN
C PENTAHEDRON
                DO K=1,6
                 J=K
                 IF(K>=4) J=J+1
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
C PYRAMID
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
C HEXAHEDRON OR TETRAEDRON
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
      ENDIF ! PREPARE_ANIM


C---------------------------------------------------
C OPENING OF VENTHOLES
C---------------------------------------------------
      DO IVENT=1,NVENT
         AREA_VENT(IVENT)=ZERO
         PM_VENT(IVENT)=ZERO
      ENDDO

C
      DO IEL=1,NEL
         IF (ITAGEL(IEL)<0) THEN
            IVENT=-ITAGEL(IEL)
            AREA_VENT(IVENT)=AREA_VENT(IVENT)+ELAREA(IEL)
            PM_VENT(IVENT)=PM_VENT(IVENT)+FEL(IEL)
         ENDIF
      ENDDO



      DO IVENT=1,NVENT
         IF (AREA_VENT(IVENT)>ZERO) THEN
            PM_VENT(IVENT)=PM_VENT(IVENT)/AREA_VENT(IVENT)
         ELSE
            PM_VENT(IVENT)=ZERO
         ENDIF
C
         IDEF   =IBAGHOL(1,IVENT)
         IDTPDEF= IBAGHOL(11,IVENT)
         IDPDEF = IBAGHOL(12,IVENT)
C
         IF(IDEF==2) CYCLE
C
         PDEF   =RBAGHOL(1,IVENT)
         DTPDEFI=RBAGHOL(4,IVENT)
         DTPDEFC=RBAGHOL(5,IVENT)
         TVENT  =RBAGHOL(3,IVENT)
         TSTOPE =RBAGHOL(14,IVENT)
         IF (IDTPDEF==0) THEN
          IF(PM_VENT(IVENT)>PDEF+PEXT) THEN
            DTPDEFC = DTPDEFC+DT1
          END IF
         ELSE IF (IDTPDEF==1) THEN
           IF (PM_VENT(IVENT)>PDEF+PEXT) THEN
             IDPDEF = 1
           END IF
           IF (IDPDEF==1) THEN
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
          IF (IDEF==0.AND.PM_VENT(IVENT)>PDEF+PEXT
     .                 .AND.DTPDEFC>DTPDEFI
     .                 .AND.VOLPH>EM3*AREAP**THREE_HALF
     .                 .AND.TT<TSTOPE+TTF) THEN
            IDEF=1
            WRITE(IOUT,'(A)')
     .                  ' ** AIRBAG VENT HOLE MEMBRANE IS DEFLATED **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IVENT,' ',VENTTITLE
            WRITE(ISTDO,'(2A)')
     .                  ' ** VENT HOLE MEMBRANE IS DEFLATED ',VENTTITLE
          ENDIF
          IF(IDEF==0 .AND. TT>TVENT+TTF
     .                 .AND. TT<TSTOPE+TTF) THEN
            IDEF=1
            WRITE(IOUT,'(A)') ' ** AIRBAG VENTING STARTS **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IVENT,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') ' ** VENTING STARTS ',VENTTITLE
          ENDIF
          IF(IDEF==1 .AND. TT>=TSTOPE+TTF) THEN
            IDEF=2
            WRITE(IOUT,'(A)') ' ** AIRBAG VENTING STOPS **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IVENT,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') ' ** VENTING STOPS ',VENTTITLE
          ENDIF
C
         ELSE IF(ITTF==0) THEN
          IF (IDEF==0.AND.PM_VENT(IVENT)>PDEF+PEXT
     .                 .AND.DTPDEFC>DTPDEFI
     .                 .AND.VOLPH>EM3*AREAP**THREE_HALF
     .                 .AND.TT<TSTOPE) THEN
            IDEF=1
            WRITE(IOUT,'(A)')
     .                  ' ** AIRBAG VENT HOLE MEMBRANE IS DEFLATED **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IVENT,' ',VENTTITLE
            WRITE(ISTDO,'(2A)')
     .                  ' ** VENT HOLE MEMBRANE IS DEFLATED ',VENTTITLE
          ENDIF
          IF(IDEF==0 .AND. TT>TVENT
     .                 .AND. TT<TSTOPE) THEN
            IDEF=1
            WRITE(IOUT,'(A)') ' ** AIRBAG VENTING STARTS **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IVENT,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') ' ** VENTING STARTS ',VENTTITLE
          ENDIF
          IF(IDEF==1 .AND. TT>=TSTOPE) THEN
            IDEF=2
            WRITE(IOUT,'(A)') ' ** AIRBAG VENTING STOPS **'
            WRITE(IOUT,'(3X,2(A,I10),2A)')' MONITORED VOLUME ',IVOLU(1),
     .                  ' VENT HOLE NUMBER',IVENT,' ',VENTTITLE
            WRITE(ISTDO,'(2A)') ' ** VENTING STOPS ',VENTTITLE
          ENDIF
         END IF
C
         IBAGHOL(1,IVENT)=IDEF
      ENDDO
C---------------------------------------------------
C FORCES ON AIRBAGS AND OUTPUTS
C---------------------------------------------------
C     FVSPMD(IFV)%AAA(1:3,1:NNT) = ZERO
C
      IF(IVOLU(39)==1) THEN
       TFEXT0=TFEXT
       DO IEL=1,NELT
         N1=ELEM(1,IEL)
         N2=ELEM(2,IEL)
         N3=ELEM(3,IEL)
         NRX=NORM(1,IEL)
         NRY=NORM(2,IEL)
         NRZ=NORM(3,IEL)
         IF(IEL<=NEL) THEN
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
C     OUTPUT TH GAUGES
C----------------------
      IF (NBGAUGE > 0) THEN
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

      RETURN
      END
