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
Chd|====================================================================
Chd|  FVBAG0                        source/airbag/fvbag.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FVBAG1                        source/airbag/fvbag.F         
Chd|        FVBAG2                        source/airbag/fvbag2.F        
Chd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/fvmbag_meshcontrol_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE FVBAG0(MONVOL,   VOLMON , X        , SENSOR_TAB, V      ,
     .                  A     ,   NPC    , TF       ,
     .                  FSAV  ,   IFVMESH, ICONTACT , LGAUGE ,
     .                  GAUGE ,   IGEO   , GEO      , PM     , IPM,
     .                  IPARG ,  IGROUPTG, IGROUPC  , ELBUF_TAB, FEXT,
     .                  FLAG  ,  H3D_DATA)
C
C FVBAG0 called two times from RESOL
C  FLAG = 1 : Gather information on PMAIN + Computations
C  FLAG = 2 : Scatter information from PMAIN (inc. switch to UP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD
      USE ELBUFDEF_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE H3D_MOD
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
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MONVOL(*), NPC(*),IFVMESH,ICONTACT(*), LGAUGE(3,*), IGEO(NPROPGI,*)
      INTEGER IPM(NPROPMI,*), IPARG(NPARG,*)
      INTEGER IGROUPTG(*), IGROUPC(*)
      INTEGER FLAG ! 1: gather + computations
                   ! 2: scatter
      my_real VOLMON(*), X(3,*),V(3,*), A(3,*),
     .        TF(*), FSAV(NTHVKI,*), GAUGE(LLGAUGE,*), GEO(NPROPG,*),
     .        PM(NPROPM,*), FEXT(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADPOLH, N, ITYP, NNFV, NTRFV, NPOLH, IFV, INFO, IDONE
      INTEGER NJET, IADJET, RADJET, NVENT, IADHOL, RADHOL
      INTEGER K1,  K2,  KIBJET, KIBHOL, KIBALE
      INTEGER KK1, KK2, KRBJET, KRBHOL, KRBALE,IERR
      INTEGER NNS, NTG, NBA, NTGA, NNA, NNI, NTGI, NNT, NTGT
      INTEGER KI1, KI2, KI3, KI4, KI5
      INTEGER KR1, KR2, KR3, KR4, KR5, KR6, KR7, KR8, KR9
      INTEGER KIA1, KIA2, KIA3, KIA4, KIA5, KIA6, KIA7, KIA8
      INTEGER KRA1, KRA2, KRA3, KRA4, KRA5, KRA6, KRA7, KRA8
      INTEGER NSKIP, IEQUI
      my_real CFL_COEF
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C-----------------------------------------------
c     GAUGE(3,*)
c 1:  -Isolid				   -(NUMELS_G+1) if SPH gauge
c 2:  GaugeId
c 3:  +Node or -Shell
c
c     => GAUGE(LLGAUGE,*), LLGAUGE = 37
c 1:  Dist (distance from Shell)	   Dist (distance from Shell)
c 2:  XG				   XG
c 3:  YG				   YG
c 4:  ZG				   ZG
c 5:  Alpha (Solid penetration ratio)	   not yet used
c 6:    				   XSAV (SPH sorting)
c 7:    				   YSAV (SPH sorting)
c 8:    				   ZSAV (SPH sorting)
c 9:    				   FF (sph only)
c 10:   				   intantaneous Pressure
c 11:   				   intantaneous PA
c 12:   				   intantaneous Rho
c 13:   				   intantaneous E
c 14:   				   ! Butterworth !
c 15:   				   ! Butterworth !
c 16:   				   ! Butterworth !
c 17:   				   ! Butterworth !
c 18:   				   ! Butterworth !
c 19:   				   ! Butterworth !
c 20:   			           ! Butterworth !
c 21: 				           ! Butterworth !
c 22: 				           ! Butterworth !
c 23: 				           ! Butterworth !
c 24:   				   ! Butterworth !
c 25:   				   ! Butterworth !
c 26:   				   ! Butterworth !
c 27:   				   ! Butterworth !
c 28:   				   ! Butterworth !
c 29:   				   ! Butterworth !
c 30:  Pressure      		  	   filtered Pressure
c 31:  PA            			   filtered PA
c 32:  Rho           			   filtered Rho
c 33:  E             			   filtered E
c 34:   				   ! Butterworth !
c 35:   				   ! Butterworth !
c 36:   				   ! Butterworth !
c 37:   				   ! Butterworth !
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
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
      IFVMESH=0
C
      DO N=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP == 6.OR.ITYP == 8) THEN
            IFV = MONVOL(K1 -1 +45)
C
           IEQUI=MONVOL(K1-1+15)
           IF(TT < VOLMON(KK1-1+49).AND.IEQUI >= 1) THEN
              MONVOL(K1-1+39)=0
              NSKIP=MOD(NCYCLE,IEQUI)
           ELSE
              MONVOL(K1-1+39)=1
              NSKIP=0
           ENDIF
           IF(NSKIP >= 1 .AND. KMESH(IFV) > 1) GO TO 100
C
            IDONE=MONVOL(K1-1+57)
            IF(IDONE == 1) GO TO 100
            INFO=0
C
            NJET=MONVOL(K1+7)
            IADJET=KIBJET+MONVOL(K1+8)
            RADJET=KRBJET+MONVOL(K1+9)
            NVENT=MONVOL(K1+10)
            IADHOL=KIBHOL+MONVOL(K1+11)
            RADHOL=KRBHOL+MONVOL(K1+12)
C
            NNS= MONVOL(K1-1+32)
            NTG= MONVOL(K1-1+33)
            NNI= MONVOL(K1-1+68)
            NTGI=MONVOL(K1-1+69)
            NNT= NNS+NNI
            NTGT=NTG+NTGI
            KI1=KIBALE+MONVOL(K1-1+31)
            KI2=KI1+NNT
            KI3=KI2+3*NTGT
            KI4=KI3+NTGT
            KI5=KI4+NTGT
            KR1=KRBALE+MONVOL(K1-1+34)
            KR2=KR1+NNT
            KR3=KR2+NNT
            KR4=KR3+NNT
            KR5=KR4+3*NNT
            KR6=KR5+NTGT
            KR7=KR6+NTGT
            KR8=KR7+NTGT
            KR9=KR8+NTGT
C
            NNFV= MONVOL(K1-1+46)
            NTRFV=MONVOL(K1-1+47)
            NPOLH=MONVOL(K1-1+49)
C
            NBA= MONVOL(K1-1+62)
            NTGA=MONVOL(K1-1+63)
            NNA= MONVOL(K1-1+64)
C
            KIA1=KI4 +2*NTGT
            KIA2=KIA1+2*NBA
            KIA3=KIA2+12*NBA
            KIA4=KIA3+2*NTGT
            KIA5=KIA4+NNA
            KIA6=KIA5+3*NTGA
            KIA7=KIA6+NTGA
            KIA8=KIA7+8*NBA
C
            KRA1=KR9 +NNT
            KRA2=KRA1+NNA
            KRA3=KRA2+NNA
            KRA4=KRA3+NNA
            KRA5=KRA4+3*NNA
            KRA6=KRA5+3*NNA
            KRA7=KRA6+3*NNA
            KRA8=KRA7+  NTGI

            IF (ITYP  ==  8) THEN
               CFL_COEF = FVDATA(IFV)%CFL_COEF
            ELSE
               CFL_COEF = DTFAC1(52)
            ENDIF
            IF(FLAG == 1 .AND. NSKIP < 1) THEN
               IF (MONVOL(K1+74-1) >= 0) THEN
                  CALL FVBAG1(
     1                 NNS,           NTG,           MONVOL(KI1), MONVOL(KI2), NJET,
     2                 MONVOL(IADJET),VOLMON(RADJET),NVENT,MONVOL(IADHOL),VOLMON(RADHOL),
     3                 VOLMON(KR1),   VOLMON(KR2),   VOLMON(KR3), VOLMON(KR4),VOLMON(KR9),
     4                 X,             V,           A,           SENSOR_TAB,
     5                 FSAV(1,N),     NPC,           TF,          MONVOL(K1),VOLMON(KK1),
     6                 FVDATA(IFV)%MPOLH, FVDATA(IFV)%QPOLH, FVDATA(IFV)%EPOLH,
     7                 FVDATA(IFV)%PPOLH, FVDATA(IFV)%RPOLH, FVDATA(IFV)%GPOLH,
     8                 NPOLH, FVDATA(IFV)%IFVNOD, FVDATA(IFV)%RFVNOD, FVDATA(IFV)%IFVTRI,
     9                 FVDATA(IFV)%IFVPOLY, FVDATA(IFV)%IFVTADR, FVDATA(IFV)%IFVPOLH,
     A                 FVDATA(IFV)%IFVPADR, INFO,    NNFV,        NTRFV      , IFV,
     B                 FVDATA(IFV)%NPOLH_ANIM, FVDATA(IFV)%DLH, FVDATA(IFV)%CPAPOLH,
     C                 FVDATA(IFV)%CPBPOLH,    FVDATA(IFV)%CPCPOLH,  FVDATA(IFV)%RMWPOLH,
     D                 MONVOL(KI3),   VOLMON(KR5),   ICONTACT,   FVDATA(IFV)%IDPOLH,
     E                 VOLMON(KR6),   VOLMON(KR7),MONVOL(KIA4),MONVOL(KIA5),MONVOL(KIA6),
     F                 VOLMON(KRA1), VOLMON(KRA2),VOLMON(KRA3),VOLMON(KRA4),MONVOL(KIA7),
     G                 NNA,   NTGA,        FVDATA(IFV)%IBPOLH, FVDATA(IFV)%DTPOLH,  NNT,
     H                 NTGT,  VOLMON(KRA5),VOLMON(KRA6),       MONVOL(KIA8),VOLMON(KRA7),
     I                 LGAUGE,       GAUGE,        ITYP,               IGEO,VOLMON(KRA8),
     J                 GEO,   PM,    IPM,      FVDATA(IFV)%TPOLH,  VOLMON(KR8),
     K                 FVDATA(IFV)%CPDPOLH,    FVDATA(IFV)%CPEPOLH,  FVDATA(IFV)%CPFPOLH,
     L                 MONVOL(KI4),  IPARG,  MONVOL(KI5),
     M                 IGROUPTG,   IGROUPC,  ELBUF_TAB, FEXT, CFL_COEF,
     N                 FVDATA(IFV)%PDISP_OLD, FVDATA(IFV)%PDISP)
               ELSE
                  CALL FV_UP_SWITCH(
     1                 NNS, NTG, MONVOL(KI2), NJET, FVDATA(IFV)%NPOLY, FVDATA(IFV)%LENH,NBA,
     2                 MONVOL(IADJET),VOLMON(RADJET),NVENT,MONVOL(IADHOL),VOLMON(RADHOL),
     3                 VOLMON(KR1),   VOLMON(KR2),   VOLMON(KR3), VOLMON(KR4),VOLMON(KR9),
     4                 X,             V,           A,           SENSOR_TAB,
     5                 FSAV(1,N),     NPC,           TF,          MONVOL(K1),VOLMON(KK1),
     6                 FVDATA(IFV)%MPOLH, FVDATA(IFV)%QPOLH, FVDATA(IFV)%EPOLH,
     7                 FVDATA(IFV)%PPOLH, FVDATA(IFV)%RPOLH, FVDATA(IFV)%GPOLH,
     8                 NPOLH, FVDATA(IFV)%IFVNOD, FVDATA(IFV)%RFVNOD, FVDATA(IFV)%IFVTRI,
     9                 FVDATA(IFV)%IFVPOLY, FVDATA(IFV)%IFVTADR, FVDATA(IFV)%IFVPOLH,
     A                 FVDATA(IFV)%IFVPADR, INFO,    NNFV,        NTRFV      , IFV,
     B                 FVDATA(IFV)%NPOLH_ANIM, FVDATA(IFV)%DLH, FVDATA(IFV)%CPAPOLH,
     C                 FVDATA(IFV)%CPBPOLH,    FVDATA(IFV)%CPCPOLH,  FVDATA(IFV)%RMWPOLH,
     D                 MONVOL(KI3),   VOLMON(KR5),   ICONTACT,   FVDATA(IFV)%IDPOLH,
     E                 VOLMON(KR6),   VOLMON(KR7),MONVOL(KIA4),MONVOL(KIA5),MONVOL(KIA6),
     F                 VOLMON(KRA1), VOLMON(KRA2),VOLMON(KRA3),VOLMON(KRA4),MONVOL(KIA7),
     G                 NNA,   NTGA,        FVDATA(IFV)%IBPOLH, FVDATA(IFV)%DTPOLH,  NNT,
     H                 NTGT,  VOLMON(KRA5),VOLMON(KRA6),       MONVOL(KIA8),VOLMON(KRA7),
     I                 ITYP,               IGEO,VOLMON(KRA8),
     J                 GEO,   PM,    IPM,      FVDATA(IFV)%TPOLH,  VOLMON(KR8),
     K                 FVDATA(IFV)%CPDPOLH,    FVDATA(IFV)%CPEPOLH,  FVDATA(IFV)%CPFPOLH,
     L                 MONVOL(KI4),  IPARG,  MONVOL(KI5),
     M                 IGROUPTG,   IGROUPC,  ELBUF_TAB, CFL_COEF,
     N                 FVDATA(IFV)%PDISP_OLD, FVDATA(IFV)%PDISP)
               ENDIF
            ELSEIF (FLAG == 2) THEN
              CALL FVBAG2(IFV, ITYP, NNA, NVENT, NJET, MONVOL(K1), MONVOL(IADHOL), VOLMON(RADHOL),
     .                  X, VOLMON(KK1), VOLMON(KRA5), MONVOL(KIA8), VOLMON(RADJET),
     .                  A,LGAUGE,GAUGE,NNT,FEXT,NSKIP,
     .                  H3D_DATA )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
               MONVOL(K1-1+57)=1
            ENDIF
         ENDIF
  100    K1=K1+NIMV
         KK1=KK1+NRVOLU
      ENDDO
C
      RETURN
      END 