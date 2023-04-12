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
Chd|  S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        A4MASS3                       source/elements/solid/solide4/a4mass3.F
Chd|        A4MASS3P                      source/elements/solid/solide4/a4mass3.F
Chd|        A4MOMT3                       source/elements/solid/solide4/a4momt3.F
Chd|        BOLTST                        source/elements/solid/solide/boltst.F
Chd|        CHECK_OFF_ALE                 source/elements/solid/solide/check_off_ale.F
Chd|        E4PXLE3                       source/elements/solid/solide4/e4pxle3.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        S11FX3                        source/elements/solid/solide/s11fx3.F
Chd|        S4BILAN                       source/elements/solid/solide4/s4bilan.F
Chd|        S4COOR3                       source/elements/solid/solide4/s4coor3.F
Chd|        S4CUMU3                       source/elements/solid/solide4/s4cumu3.F
Chd|        S4CUMU3P                      source/elements/solid/solide4/s4cumu3.F
Chd|        S4DEFO3                       source/elements/solid/solide4/s4defo3.F
Chd|        S4DEFOT3                      source/elements/solid/solide4/s4defo3.F
Chd|        S4DERI3                       source/elements/solid/solide4/s4deri3.F
Chd|        S4DERIT3                      source/elements/solid/solide4/s4deri3.F
Chd|        S4DERITO3                     source/elements/solid/solide4/s4deri3.F
Chd|        S4FILLOPT                     source/elements/solid/solide4/s4fillopt.F
Chd|        S4FINT3                       source/elements/solid/solide4/s4fint3.F
Chd|        S4FINT_REG                    source/elements/solid/solide4/s4fint_reg.F
Chd|        S4MALLA3                      source/elements/solid/solide4/s4mall3.F
Chd|        S4RCOOR12                     source/elements/solid/solide4/s4coor3.F
Chd|        S4SAV12                       source/elements/solid/solide4/s4coor3.F
Chd|        S4SAV3                        source/elements/solid/solide4/s4coor3.F
Chd|        S4THERM                       source/elements/solid/solide4/s4therm.F
Chd|        S4UPD11T12                    source/elements/solid/solide4/s4forc3.F
Chd|        S4VOLN_M                      source/elements/solid/solide4_sfem/s4voln_m.F
Chd|        SGCOOR3                       source/elements/solid/solide/sgcoor3.F
Chd|        SGEODEL3                      source/elements/solid/solide/sgeodel3.F
Chd|        SMALLB3                       source/elements/solid/solide/small3.F
Chd|        SMALLGEO3                     source/elements/solid/solide/small3.F
Chd|        SORDEF12                      source/elements/solid/solidez/szforc3.F
Chd|        SORDEFT12                     source/elements/solid/solidez/szforc3.F
Chd|        SORTHDIR3                     source/elements/solid/solide/sorthdir3.F
Chd|        SREPLOC3                      source/elements/solid/solide/sreploc3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SROTA3                        source/elements/solid/solide/srota3.F
Chd|        SROTO12_SIG                   source/elements/solid/solidez/szforc3.F
Chd|        SSTRA3                        source/elements/solid/solide/sstra3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE S4FORC3 (ELBUF_TAB ,NG      ,
     1            PM       ,GEO     ,IXS     ,X        ,   
     2            A        ,V       ,MS      ,W        ,FLUX     ,  
     3            FLU1     ,VEUL    ,FV      ,ALE_CONNECT    ,IPARG    ,  
     4            TF       ,NPF     ,BUFMAT  ,PARTSAV  ,NLOC_DMG , 
     5            DT2T     ,NELTST  ,ITYPTST ,STIFN    ,FSKY     ,
     6            IADS     ,OFFSET  ,EANI    ,IPARTS   ,
     7            F11      ,F21     ,F31     ,F12      ,F22      ,
     8            F32      ,F13     ,F23     ,F33      ,F14      ,
     9            F24      ,F34     ,NEL     ,FSKYM    ,MSNF     ,                    
     A            IPM      ,IGEO    ,BUFVOIS ,ISTRAIN  ,ITASK    ,      
     B            TEMP     ,FTHE    ,FTHESKY ,IEXPAN   ,GRESAV   ,
     C            GRTH     ,IGRTH   ,MSSA    ,DMELS    ,TABLE    ,
     D            XDP      ,VARNOD  ,VOLN    ,CONDN    ,CONDNSKY ,
     E            D        ,SENSOR_TAB,IOUTPRT ,MATPARAM_TAB,H3D_STRAIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE ELBUFDEF_MOD         
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MATPARAM_DEF_MOD
      USE SENSOR_MOD
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(NPARG,NGROUP), NPF(*),IADS(8,*),
     .        IPARTS(*),IPM(NPROPMI,*),IGEO(NPROPGI,*),ITASK,
     .        GRTH(*),IGRTH(*),IOUTPRT
C
      INTEGER NELTST,ITYPTST,OFFSET,NEL,NG, ISTRAIN,
     .        IEXPAN,H3D_STRAIN
     
      DOUBLE PRECISION
     .        XDP(3,*)
 
      my_real
     .   DT2T
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), X(*), A(*), V(*), MS(*), W(*), FLUX(6,*),
     .   FLU1(*), VEUL(*), FV(*), TF(*), BUFMAT(*),
     .   PARTSAV(*),STIFN(*), FSKY(*),EANI(*), FSKYM(*),
     .   F11(MVSIZ),F21(MVSIZ),F31(MVSIZ),
     .   F12(MVSIZ),F22(MVSIZ),F32(MVSIZ),
     .   F13(MVSIZ),F23(MVSIZ),F33(MVSIZ),
     .   F14(MVSIZ),F24(MVSIZ),F34(MVSIZ),D(*),
     .   TEMP(*), FTHE(*), FTHESKY(*),GRESAV(*), MSSA(*), DMELS(*), VOLN(MVSIZ)
      my_real MSNF(*),VARNOD(*),CONDN(*),CONDNSKY(*),BUFVOIS(6,*)
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,I,IBID,ILCO,NF1,IFLAG,IPTR,IPTS,IPTT,ILAY
      INTEGER IBIDON(1),ITET,IP
      my_real SUM
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ)
      my_real
     . VD2(MVSIZ) , DVOL(MVSIZ),DELTAX(MVSIZ),
     . VIS(MVSIZ) , QVIS(MVSIZ), CXX(MVSIZ) ,
     . S1(MVSIZ)  , S2(MVSIZ)  , S3(MVSIZ)  ,
     . S4(MVSIZ)  , S5(MVSIZ)  , S6(MVSIZ)  ,
     . B1(MVSIZ)  , B2(MVSIZ)  , B3(MVSIZ)  ,
     . B4(MVSIZ)  , B5(MVSIZ)  , B6(MVSIZ)  ,
     . DXX(MVSIZ) , DYY(MVSIZ) , DZZ(MVSIZ) ,
     . D4(MVSIZ)  , D5(MVSIZ)  , D6(MVSIZ)  , 
     . RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     . SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     . TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) ,
     . VDX(MVSIZ), VDY(MVSIZ), VDZ(MVSIZ),SSP_EQ(MVSIZ),
     . CONDE(MVSIZ),DIVDE(MVSIZ)
     
      DOUBLE PRECISION 
     .   X0(MVSIZ,4),Y0(MVSIZ,4),Z0(MVSIZ,4),
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),VOLDP(MVSIZ)
          
      ! Variables utilisees en argument par les materiaux.
      my_real
     .   STI(MVSIZ), GAMA(MVSIZ,6),
     .   WXX(MVSIZ) , WYY(MVSIZ) , WZZ(MVSIZ),AIRE(MVSIZ)
      ! Variables utilisees en argument par les materiaux (SPH seulement).
      my_real
     . MUVOID(MVSIZ)

      ! Variables void MMAIN 
      my_real
     . SIGY(MVSIZ),ET(MVSIZ),R3_FREE(MVSIZ),R4_FREE(MVSIZ)
      ! Variables utilisees dans les routines solides uniquement(en arguments).
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ),MAT(MVSIZ)
      my_real
     .  OFF(MVSIZ) , RHOO(MVSIZ),OFFG0(MVSIZ) ,
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .  VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),
     .  VY1(MVSIZ),VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),
     .  VZ1(MVSIZ),VZ2(MVSIZ),VZ3(MVSIZ),VZ4(MVSIZ),
     .  PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .  PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .  PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .  VDX1(MVSIZ),VDX2(MVSIZ),VDX3(MVSIZ),VDX4(MVSIZ),
     .  VDY1(MVSIZ),VDY2(MVSIZ),VDY3(MVSIZ),VDY4(MVSIZ),
     .  VDZ1(MVSIZ),VDZ2(MVSIZ),VDZ3(MVSIZ),VDZ4(MVSIZ),
     .  DXY(MVSIZ),DYX(MVSIZ),
     .  DYZ(MVSIZ),DZY(MVSIZ),
     .  DZX(MVSIZ),DXZ(MVSIZ),
     .  E1X(MVSIZ) , E1Y(MVSIZ) , E1Z(MVSIZ) , 
     .  E2X(MVSIZ) , E2Y(MVSIZ) , E2Z(MVSIZ) , 
     .  E3X(MVSIZ) , E3Y(MVSIZ) , E3Z(MVSIZ) ,RHO_0,
     .  TEMPEL(MVSIZ), THEM(MVSIZ,4) , DIE(MVSIZ)

      my_real
     .  VX0(MVSIZ,4),VY0(MVSIZ,4),VZ0(MVSIZ,4),
     .  MFXX(MVSIZ),MFXY(MVSIZ),MFYX(MVSIZ),
     .  MFYY(MVSIZ),MFYZ(MVSIZ),MFZY(MVSIZ),
     .  MFZZ(MVSIZ),MFZX(MVSIZ),MFXZ(MVSIZ),BID(MVSIZ),AMU(MVSIZ)
      my_real, 
     .  DIMENSION(:), POINTER :: EINT

      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C----- Variables for non-local computation
      INTEGER INOD(4),IPOS(4), L_NLOC, INLOC,ISM12_11
      my_real, DIMENSION(:), ALLOCATABLE :: VAR_REG
      my_real, DIMENSION(:), POINTER :: DNL
C

C----- 
c     Flag Bolt Preloading
      INTEGER IBOLTP,NBPRELD,II(6)
      my_real, 
     .  DIMENSION(:), POINTER :: BPRELD
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF => ELBUF_TAB(NG)%GBUF
      LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
C-----special traitment when switching Ismstr from 12 to 11      
      ISM12_11 = ELBUF_TAB(NG)%BUFLY(1)%L_SIGL
      IBID = 0
      IBIDON(1) = 0
C
      IBOLTP = IPARG(72,NG)
      INLOC  = IPARG(78,NG)
      ALLOCATE(VAR_REG(NEL))
      NBPRELD = GBUF%G_BPRELD
      BPRELD =>GBUF%BPRELD(1:NBPRELD*NEL)
C-----------
      NF1 = NFT+1
      IGTYP = IGEO(11,IXS(10,NF1))   
c
      CALL S4COOR3(X,IXS(1,NF1),V,W,
     .   X1, X2, X3, X4, Y1, Y2, Y3, Y4, 
     .   Z1, Z2, Z3, Z4, VX1, VX2, VX3, VX4, 
     .   VY1, VY2, VY3, VY4, VZ1, VZ2, VZ3, VZ4, 
     .   VDX1, VDX2, VDX3, VDX4, 
     .   VDY1, VDY2, VDY3, VDY4,
     .   VDZ1, VDZ2, VDZ3, VDZ4, 
     .   VDX,VDY,VDZ,VD2,VIS,GBUF%OFF,OFF,GBUF%SMSTR,
     .   GBUF%RHO,RHOO,NC1,NC2,NC3,NC4,NGL,MXT,NGEO,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   XD1, XD2, XD3, XD4, YD1, YD2, YD3, YD4, 
     .   ZD1, ZD2, ZD3, ZD4, XDP,NEL)
C-----------
C GATHER NODAL VARIABLES FOR TOTAL STRAIN CASE.
C-----------
      IF ((ISMSTR >= 10.AND.ISMSTR <= 12).AND.JLAG > 0) THEN
        CALL SGCOOR3(TT,4,X,IXS(1,NF1),
     1   X0, Y0, Z0, VX0, VY0, VZ0, 
     2   GBUF%SMSTR ,D ,GBUF%OFF,OFFG0,NEL ,XDP )
       IF (ISMSTR ==  11) THEN
        CALL S4DERIT3(OFF,VOLN,NGL,DELTAX,MXT,
     .   X0(1,1), X0(1,2), X0(1,3), X0(1,4), 
     .   Y0(1,1), Y0(1,2), Y0(1,3), Y0(1,4), 
     .   Z0(1,1), Z0(1,2), Z0(1,3), Z0(1,4), 
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   RX,  RY,  RZ,  SX,  SY,  SZ, TX , TY , TZ  ,
     .   PM,VOLDP )
       ELSE
        IF (ISMSTR == 12.AND.IDTMIN(1)==3.AND.ISM12_11==0) THEN
C!!!!!!calcul local rep for ISMSTR 10 to 11 (offg>un) 
          CALL S4RCOOR12(GBUF%OFF,NC1  ,NC2  ,NC3  ,NC4   ,
     .                   X     ,XDP   ,D     ,
     .     E1X   ,E2X   ,E3X   ,E1Y   ,E2Y   ,E3Y   ,E1Z  ,E2Z  ,E3Z   ) 
        END IF 
        CALL S4DERITO3(OFF,VOLN,
     .   X0(1,1), X0(1,2), X0(1,3), X0(1,4), 
     .   Y0(1,1), Y0(1,2), Y0(1,3), Y0(1,4), 
     .   Z0(1,1), Z0(1,2), Z0(1,3), Z0(1,4), 
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   RX,  RY,  RZ,  SX,  SY,  SZ, TX , TY , TZ  ,
     .   GBUF%JAC_I)
       END IF !(ISMSTR ==  11) THEN
        CALL S4DEFOT3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   VX0(1,1), VX0(1,2), VX0(1,3), VX0(1,4), 
     .   VY0(1,1), VY0(1,2), VY0(1,3), VY0(1,4), 
     .   VZ0(1,1), VZ0(1,2), VZ0(1,3), VZ0(1,4), 
     .   MFXX, MFXY, MFXZ, MFYX, MFYY, MFYZ, MFZX, MFZY, MFZZ) 
        IF (ISORTH == 0) THEN            
          DO I=LFT,LLT                                            
            GAMA(I,1) = ONE                               
            GAMA(I,2) = ZERO                                
            GAMA(I,3) = ZERO             
            GAMA(I,4) = ZERO                                
            GAMA(I,5) = ONE                                
            GAMA(I,6) = ZERO             
          ENDDO                          
        ELSE                             
         CALL SREPLOC3(
     .       RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  ) 
          CALL SORTHDIR3(
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  , 
     .         GBUF%GAMA,GAMA )
        ENDIF
         IF (ISMSTR == 12.AND.ISM12_11==0.AND.IDTMIN(1)==3) THEN
          CALL SORDEFT12(LFT,LLT,MFXX, MFXY, MFXZ,
     .         MFYX, MFYY, MFYZ,
     .         MFZX, MFZY, MFZZ,
     .         E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,GBUF%OFF)
         ENDIF
      ENDIF
C
      IF(JALE+JLAG/=0)THEN
       IF (ISMSTR /= 11) THEN
C!!!!!!also add exception for ismstr10+offg>un
        CALL S4DERI3(OFF,VOLN,NGL,DELTAX,MXT,
     .   XD1, XD2, XD3, XD4, YD1, YD2, YD3, YD4,
     .   ZD1, ZD2, ZD3, ZD4, PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4, PZ1, PZ2, PZ3, PZ4, 
     .   RX,  RY,  RZ,  SX,  SY,  SZ, TX , TY , TZ,
     .   GBUF%SMSTR,GBUF%OFF,NEL, PM,VOLDP)
C     
        IF (ISORTH == 0) THEN            
          DO I=LFT,LLT                                            
            GAMA(I,1) = ONE                               
            GAMA(I,2) = ZERO                                
            GAMA(I,3) = ZERO             
            GAMA(I,4) = ZERO                                
            GAMA(I,5) = ONE                                
            GAMA(I,6) = ZERO             
          ENDDO                          
        ELSE                             
          CALL SREPLOC3(
     .       RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  ) 
          CALL SORTHDIR3(
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  , 
     .         GBUF%GAMA,GAMA )
        ENDIF
       END IF !(ISMSTR /= 11) THEN
C
      ELSEIF(JEUL/=0)THEN
        CALL E4PXLE3(
     1   GBUF%VOL,VEUL,    X1,      X2,
     2   X3,      X4,      Y1,      Y2,
     3   Y3,      Y4,      Z1,      Z2,
     4   Z3,      Z4,      PX1,     PX2,
     5   PX3,     PX4,     PY1,     PY2,
     6   PY3,     PY4,     PZ1,     PZ2,
     7   PZ3,     PZ4,     VOLN,    DELTAX,
     8   NEL,     NFT)
      ENDIF
C
      CALL S4DEFO3(
     .   PX1, PX2, PX3, PX4, PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4, VX1, VX2, VX3, VX4,
     .   VY1, VY2, VY3, VY4, VZ1, VZ2, VZ3, VZ4,
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .   WXX, WYY, WZZ)
         IF (IDTMIN(1)==3.AND.ISMSTR == 12.AND.ISM12_11==0) THEN
          CALL SORDEF12(LFT,LLT,DXX, DYY, DZZ,
     .         D4, D5, D6,
     .         E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,OFFG0)
         ENDIF
C
      CALL S11FX3(
     1   PM,         FLUX(1,NF1),ALE_CONNECT,IXS,
     2   IPM,        BUFMAT,     NEL,        NFT,
     3   JALE,       JEUL)
C-----------------------------------------------------
C     SFEM
C-----------------------------------------------------
      IF(JLAG > 0.AND.ISROT == 3) THEN
        RHO_0 = PM(1,MXT(1))
        CALL S4VOLN_M(VARNOD,NC1,NC2,NC3,NC4,MXT,GBUF%OFF,GBUF%RHO,RHO_0,
     .                MFXX,MFXY,MFXZ,MFYX,MFYY,MFYZ,MFZX,MFZY,MFZZ,
     .                GBUF%VOL,VOLN,LBUF%VOL0DP,VOLDP,GBUF%AMU,
     .                DXX,DYY,DZZ,MATPARAM_TAB,NEL)
      ENDIF
      IF(JALE > 0.AND.ISROT == 3.AND.MTN /= 37.AND.MTN /= 51.AND.
     .   MTN /= 18.AND.MTN /= 11) THEN
        DO I=LFT,LLT
         IF(OFF(I) /= 0) THEN
         SUM=VARNOD(NC1(I))+VARNOD(NC2(I))+VARNOD(NC3(I))+VARNOD(NC4(I))
         VOLN(I)=FOURTH*SUM*GBUF%RHO(I)/PM(1,MXT(I))
         ENDIF
        ENDDO
      ENDIF
C-------------------------------------------------------------
C     DENSITY
C-------------------------------------------------------------
       DIVDE(1:NEL) = DT1*(DXX(1:NEL)+ DYY(1:NEL)+ DZZ(1:NEL))   
      CALL SRHO3(PM          ,GBUF%VOL   ,GBUF%RHO ,GBUF%EINT  ,DIVDE,
     .   FLUX(1,NF1),FLU1(NF1),VOLN,DVOL,NGL,MXT,OFF,
     .           IPARG(64,NG)    ,GBUF%TAG22   , VOLDP,LBUF%VOL0DP,AMU ,GBUF%OFF)
C----for Eint compute----     
      IF (ISMSTR == 12.AND.IDTMIN(1)==3.AND.ISM12_11==0) THEN
          CALL SROTO12_SIG(LFT,LLT,LBUF%SIG,NEL,
     .                 E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,OFFG0)
!! temporary replaced by (the same) SROTO12_SIG() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!!          CALL SROTO12(LFT,LLT,LBUF%SIG,
!!     .                 E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z,GBUF%OFF)
      ENDIF


      CALL SROTA3(
     1   GBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WXX,
     3   WYY,     WZZ,     NEL,     MTN,
     4   ISMSTR)
C-----------------------------
C     SMALL STRAIN
C-----------------------------
      CALL S4MALLA3(
     1   GBUF%SMSTR,GBUF%OFF,  OFF,       WXX,
     2   WYY,       WZZ,       NEL,       ISMSTR,
     3   JLAG)
C--------------------------
C- UPDATE REF CONFIGURATION (possible future change to small strain option)
C- Total strain option doesn't change the Ref CONF.
C--------------------------
      IF (ISMSTR <= 3.OR.(ISMSTR==4.AND.JLAG>0)) THEN
       CALL S4SAV3(GBUF%OFF,GBUF%SMSTR,
     .   XD1, XD2, XD3, XD4, YD1, YD2, YD3, YD4, 
     .   ZD1, ZD2, ZD3, ZD4,NEL)
      END IF !(ISMSTR <= 3) THEN
C-----------------------------
C     heat transfert 
C
      IF(JTHE < 0 ) THEN
         DO I = LFT, LLT
           TEMPEL(I) =  FOURTH*( TEMP(NC1(I)) + TEMP(NC2(I))  +
     .                          TEMP(NC3(I)) + TEMP(NC4(I))  )    
         ENDDO  
      ENDIF
c-------------------------------------------
c    COMPUTE Regularized non local variable in Gauss point
c-------------------------------------------
      IF (INLOC > 0) THEN
        L_NLOC = NLOC_DMG%L_NLOC
        DNL  => NLOC_DMG%DNL(1:L_NLOC) ! DNL = non local variable increment
        DO I=LFT,LLT
          INOD(1) = NLOC_DMG%IDXI(NC1(I))
          INOD(2) = NLOC_DMG%IDXI(NC2(I))
          INOD(3) = NLOC_DMG%IDXI(NC3(I))
          INOD(4) = NLOC_DMG%IDXI(NC4(I))
          IPOS(1) = NLOC_DMG%POSI(INOD(1))
          IPOS(2) = NLOC_DMG%POSI(INOD(2))
          IPOS(3) = NLOC_DMG%POSI(INOD(3))
          IPOS(4) = NLOC_DMG%POSI(INOD(4))
          VAR_REG(I) = FOURTH*(DNL(IPOS(1)) + DNL(IPOS(2)) + DNL(IPOS(3)) + DNL(IPOS(4)))
        ENDDO
      ENDIF
C------------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C------------------------------------------------------
Cpour le fluide en SPMD : BUFVOIS LOI11
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)

      ILAY = 1                                                
      IPTR = 1                                                
      IPTS = 1                                                
      IPTT = 1  
      IP = 1

      IF(IBOLTP /= 0) THEN
        CALL SROTA3(
     1   BPRELD(3*NEL+1),B1,             B2,             B3,
     2   B4,             B5,             B6,             WXX,
     3   WYY,            WZZ,            NEL,            MTN,
     4   ISMSTR)
        CALL BOLTST(
     1   IBOLTP,    IP,        BPRELD,    LBUF%SIG,
     2   TT,        NEL,       SENSOR_TAB,NPT)
      END IF

      CALL MMAIN(
     1   ELBUF_TAB,NG      ,PM     ,GEO        ,FV	   ,ALE_CONNECT    ,
     2   IXS      ,IPARG   ,V      ,TF	     ,NPF	    ,BUFMAT   ,
     3   STI      ,X       ,DT2T   ,NELTST  ,ITYPTST ,OFFSET   ,
     4   NEL      ,W       ,OFF    ,NGEO       ,
     6   MXT      ,NGL     ,VOLN   ,VD2	    ,DVOL    ,DELTAX   ,
     7   VIS      ,QVIS    ,CXX    ,S1	     ,S2	     ,S3	      ,
     8   S4       ,S5      ,S6     ,DXX	    ,DYY	    ,DZZ      ,
     9   D4       ,D5      ,D6     ,WXX	    ,WYY	    ,WZZ      ,
     A   RX       ,RY      ,RZ     ,SX	     ,SY	     ,SZ	      ,
     B   VDX      ,VDY     ,VDZ    ,MUVOID  ,SSP_EQ  ,AIRE     ,
     C   SIGY     ,ET      ,BUFVOIS,LBUF%PLA   ,R3_FREE    ,AMU  ,
     D   MFXX     ,MFXY    ,MFXZ   ,MFYX    ,MFYY    ,MFYZ     , 
     E   MFZX     ,MFZY    ,MFZZ   ,IPM     ,GAMA    ,BID      ,
     F   BID      ,BID     ,BID    ,BID     ,BID     ,BID      ,
     G   ISTRAIN  ,TEMPEL  ,DIE    ,IEXPAN  ,ILAY    ,MSSA     ,
     H   DMELS    ,IPTR    ,IPTS   ,IPTT    ,TABLE   ,BID      ,
     I   BID      ,BID     ,BID    ,IPARG(1,NG),IGEO     ,CONDE    ,
     J   ITASK    ,NLOC_DMG,VAR_REG,MATPARAM_TAB,H3D_STRAIN)
c
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
C-----------  return to global system    
       IF (ISMSTR == 12.AND.IDTMIN(1)==3) THEN
         IF (ISM12_11==0) THEN
          CALL SROTO12_SIG(LFT,LLT,LBUF%SIG,NEL,
     .                 E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,OFFG0)
!! temporary replaced by (the same) SROTO12_SIG() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!!          CALL SROTO12(LFT,LLT,LBUF%SIG,
!!     .                 E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,GBUF%OFF)
          IF (ISTRAIN == 1) THEN 
           CALL SORDEF12(LFT,LLT,DXX, DXY, DXZ,
     .         D4, D5, D6,
     .         E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,OFFG0)
          ENDIF
         END IF 
       ENDIF
      IF (ISTRAIN == 1) THEN 
        CALL SSTRA3(DXX,DYY,DZZ,D4,D5,
     .              D6, LBUF%STRA, WXX, WYY, WZZ,
     .              OFF,NEL)
      ENDIF
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IOUTPRT>0)THEN
       IF (MTN == 11) THEN
         EINT => ELBUF_TAB(NG)%GBUF%EINS(1:NEL)
       ELSE
         EINT => ELBUF_TAB(NG)%GBUF%EINT(1:NEL)
       ENDIF
       CALL S4BILAN(PARTSAV,EINT,GBUF%RHO,GBUF%RK,GBUF%VOL,
     .   VX1, VX2, VX3, VX4, VY1, VY2, VY3, VY4, 
     .   VZ1, VZ2, VZ3, VZ4, VOLN,IPARTS,GRESAV,
     .   GRTH,IGRTH,IEXPAN,GBUF%EINTTH,GBUF%FILL,
     .   X1, X2, X3, X4, Y1, Y2, Y3, Y4, 
     .   Z1, Z2, Z3, Z4,ITASK,IPARG(1,NG),OFF)
      ENDIF
C
      IF(JLAG+JALE+JEUL == 0)RETURN
      ITET = 1
      CALL SGEODEL3(NGL,GBUF%OFF,VOLN,DELTAX,GBUF%VOL,GEO(1,NGEO(1)),ITET,NEL )
C-----------------------------
C     SMALL STRAIN 
C-----------------------------
      CALL SMALLB3(GBUF%OFF,OFF)
      CALL SMALLGEO3(NGL, GBUF%OFF ,VOLN ,DELTAX, GBUF%VOL ,ITET )
      IF (ISMSTR == 12.AND.IDTMIN(1)==3) THEN
         CALL S4SAV12(GBUF%OFF,OFFG0,GBUF%SMSTR,
     .                XD1, XD2, XD3, XD4, YD1, YD2, YD3, YD4, 
     .                ZD1, ZD2, ZD3, ZD4,NEL)
        IF (ISM12_11>0 .AND. ISORTH == 0) THEN
          CALL S4UPD11T12(GBUF%OFF,OFFG0  ,
     .     XD1, XD2, XD3, XD4, YD1, YD2, YD3, YD4, 
     .     ZD1, ZD2, ZD3, ZD4,GBUF%JAC_I,GBUF%SIG ,LBUF%SIGL,NEL   )
        END IF
      END IF
C--------------------------
C     UPDATE DES MASSES
C     TRANSPORT FORCES
C----------------------------
      IF (JALE+JEUL > 0 .AND. ALE%GLOBAL%INCOMP == 0)THEN
        IF(IPARIT == 0)THEN
          CALL A4MASS3(MS,GBUF%RHO,VOLN,NC1,NC2,NC3,NC4,MSNF,OFF)
        ELSE
          CALL A4MASS3P(FSKYM,GBUF%RHO,VOLN,IADS,OFF)
        ENDIF
      ENDIF
C-----------------------------
C     TRANSPORT FORCES
C-----------------------------
      IF(JALE == 1 .OR. JEUL == 1)THEN
C 
       CALL A4MOMT3(
     1   PM,      GBUF%RHO,VOLN,    X1,
     2   X2,      X3,      X4,      Y1,
     3   Y2,      Y3,      Y4,      Z1,
     4   Z2,      Z3,      Z4,      VX1,
     5   VX2,     VX3,     VX4,     VY1,
     6   VY2,     VY3,     VY4,     VZ1,
     7   VZ2,     VZ3,     VZ4,     F11,
     8   F21,     F31,     F12,     F22,
     9   F32,     F13,     F23,     F33,
     A   F14,     F24,     F34,     PX1,
     B   PX2,     PX3,     PX4,     PY1,
     C   PY2,     PY3,     PY4,     PZ1,
     D   PZ2,     PZ3,     PZ4,     DXX,
     E   DXY,     DXZ,     DYX,     DYY,
     F   DYZ,     DZX,     DZY,     DZZ,
     G   VDX1,    VDX2,    VDX3,    VDX4,
     H   VDY1,    VDY2,    VDY3,    VDY4,
     I   VDZ1,    VDZ2,    VDZ3,    VDZ4,
     J   VDX,     VDY,     VDZ,     DELTAX,
     K   VIS,     MXT,     RX,      RY,
     L   RZ,      SX,      SY,      SZ,
     M   TX,      TY,      TZ,      NEL)
      ENDIF

        IF(JEUL+JALE/=0) CALL CHECK_OFF_ALE(F11,F21,F31,F12,F22,
     1                           F32,F13,F23,F33,F14,
     2                           F24,F34,BID,BID,BID,
     3                           BID,BID,BID,BID,BID,
     4                           BID,BID,BID,BID,GBUF%OFF,
     5                           LFT,LLT,NEL)

C----------------------------
C     INTERNAL FORCES
C----------------------------
      CALL S4FINT3(GBUF%SIG,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   VOLN,QVIS,NEL)
C --------------------------
C  --- heat transfert 
C --------------------------
      IF(JTHE < 0 ) THEN     
        CALL S4THERM (
     1   PM,      MXT,     VOLN,    NC1,
     2   NC2,     NC3,     NC4,     PX1,
     3   PX2,     PX3,     PX4,     PY1,
     4   PY2,     PY3,     PY4,     PZ1,
     5   PZ2,     PZ3,     PZ4,     DT1,
     6   TEMP,    TEMPEL,  DIE,     THEM,
     7   GBUF%OFF,LBUF%OFF,NEL)
      
      ENDIF
c-------------------------
c     Virtual internal forces of regularized non local ddl 
c--------------------------
      IF (INLOC > 0) THEN   
        CALL S4FINT_REG(
     1   NLOC_DMG,VAR_REG, NEL,     LBUF%OFF,
     2   VOLN,    NC1,     NC2,     NC3,
     3   NC4,     PX1,     PX2,     PX3,
     4   PX4,     PY1,     PY2,     PY3,
     5   PY4,     PZ1,     PZ2,     PZ3,
     6   PZ4,     MXT(LFT),ITASK,   DT2T,
     7   GBUF%VOL,NFT)
      ENDIF 
C --------------------------
      IF(NFILSOL/=0) CALL S4FILLOPT(
     1   GBUF%FILL,STI,      F11,      F21,
     2   F31,      F12,      F22,      F32,
     3   F13,      F23,      F33,      F14,
     4   F24,      F34,      NEL)
C----------------------------
      IF (IPARIT == 0) THEN
        CALL S4CUMU3(GBUF%OFF,A,NC1, NC2, NC3, NC4,STIFN,STI,
     .       F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .       THEM,FTHE,CONDN,CONDE)
      ELSE
        CALL S4CUMU3P(GBUF%OFF,STI,FSKY,FSKY,IADS,
     .       F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .       THEM,FTHESKY,CONDNSKY,CONDE)
      ENDIF
      IF (ALLOCATED(VAR_REG)) DEALLOCATE(VAR_REG)
      RETURN
      END
Chd|====================================================================
Chd|  S4UPD11T12                    source/elements/solid/solide4/s4forc3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|        S4JAC_I                       source/elements/solid/solide4/s4forc3.F
Chd|====================================================================
      SUBROUTINE S4UPD11T12(OFFG   ,OFFG0  ,
     .     XD1, XD2, XD3, XD4, YD1, YD2, YD3, YD4, 
     .     ZD1, ZD2, ZD3, ZD4,JAC_1 ,SIG   ,SIGL  ,NEL   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   OFFG(*),OFFG0(*),JAC_1(10,NEL),SIG(NEL,6),SIGL(NEL,6)
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,NCH,INDEX(MVSIZ),J    
      my_real
     .   JAC_I(10,MVSIZ)
      DOUBLE PRECISION 
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ)
C=======================================================================
C-----Always in global system and fixed then (not like Ismstr=11
C-----for JCVT>0 Fixed SAV is rotated w/ local system not like Ismstr=1
        NCH=0
        DO I=LFT,LLT
         IF(OFFG(I)/=OFFG0(I).AND.ABS(OFFG(I)) > ONE ) THEN
           NCH = NCH +1
           INDEX(NCH) = I
         END IF
	END DO
	IF (NCH==0) RETURN
        DO J=1,NCH
          I= INDEX(J)
          X1(J)=XD1(I)
          X2(J)=XD2(I)
          X3(J)=XD3(I)
          X4(J)=XD4(I)
          Y1(J)=YD1(I)
          Y2(J)=YD2(I)
          Y3(J)=YD3(I)
          Y4(J)=YD4(I)
          Z1(J)=ZD1(I)
          Z2(J)=ZD2(I)
          Z3(J)=ZD3(I)
          Z4(J)=ZD4(I)
        END DO
C	
       CALL S4JAC_I(
     .              X1   ,X2   ,X3   ,X4   ,
     .              Y1   ,Y2   ,Y3   ,Y4   ,
     .              Z1   ,Z2   ,Z3   ,Z4   ,
     .              JAC_I ,NCH  )
#include "vectorize.inc"
        DO J=1,NCH
          I = INDEX(J)
          JAC_1(1:10,I) = JAC_I(1:10,J)
          SIGL(I,1:6) = SIG(I,1:6)
        ENDDO      
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S4JAC_I                       source/elements/solid/solide4/s4forc3.F
Chd|-- called by -----------
Chd|        S4UPD11T12                    source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S4JAC_I(
     .              X1   ,X2   ,X3   ,X4   ,
     .              Y1   ,Y2   ,Y3   ,Y4   ,
     .              Z1   ,Z2   ,Z3   ,Z4   ,
     .              JAC_I,NEL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   JAC_I(10,*)
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*), 
     .   Z1(*), Z2(*), Z3(*), Z4(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   B1(MVSIZ), B2(MVSIZ), B3(MVSIZ), B4(MVSIZ), 
     .   C1(MVSIZ), C2(MVSIZ), C3(MVSIZ), C4(MVSIZ),
     .   D1(MVSIZ), D2(MVSIZ), D3(MVSIZ), D4(MVSIZ),VOL(MVSIZ)
      DOUBLE PRECISION
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43,B1DP,C1DP,D1DP,VOLDP
      my_real
     .   D ,PX1,PX2,PX3,PX4,PY1,PY2,PY3,PY4,PZ1,PZ2,PZ3,PZ4
C=======================================================================
      DO I=1,NEL
       X43 = X4(I) - X3(I)
       Y43 = Y4(I) - Y3(I)
       Z43 = Z4(I) - Z3(I)
       X41 = X4(I) - X1(I)
       Y41 = Y4(I) - Y1(I)
       Z41 = Z4(I) - Z1(I)
       X42 = X4(I) - X2(I)
       Y42 = Y4(I) - Y2(I)
       Z42 = Z4(I) - Z2(I)
C
       B1DP  =  Y43*Z42 - Y42*Z43
       B1(I) =  B1DP
       B2(I) =  Y41*Z43 - Y43*Z41
       B3(I) =  Y42*Z41 - Y41*Z42
       B4(I) =  -(B1(I) + B2(I) + B3(I))
C
       C1DP  =  Z43*X42 - Z42*X43
       C1(I) =  C1DP
       C2(I) =  Z41*X43 - Z43*X41
       C3(I) =  Z42*X41 - Z41*X42
       C4(I) =  -(C1(I) + C2(I) + C3(I))
C
       D1DP  =  X43*Y42 - X42*Y43
       D1(I) =  D1DP
       D2(I) =  X41*Y43 - X43*Y41
       D3(I) =  X42*Y41 - X41*Y42
       D4(I) =  -(D1(I) + D2(I) + D3(I))
C
       VOLDP = (X41*B1DP + Y41*C1DP + Z41*D1DP)*ONE_OVER_6
       VOL(I) = VOLDP
      ENDDO
C
      DO I=1,NEL
        D = ONE/VOL(I)/SIX
        PX1=-B1(I)*D
        PY1=-C1(I)*D
        PZ1=-D1(I)*D
        PX2=-B2(I)*D
        PY2=-C2(I)*D
        PZ2=-D2(I)*D
        PX3=-B3(I)*D
        PY3=-C3(I)*D
        PZ3=-D3(I)*D
        PX4=-B4(I)*D
        PY4=-C4(I)*D
        PZ4=-D4(I)*D
        JAC_I(1,I) = PX1
        JAC_I(2,I) = PX2
        JAC_I(3,I) = PX3
        JAC_I(4,I) = PY1
        JAC_I(5,I) = PY2
        JAC_I(6,I) = PY3
        JAC_I(7,I) = PZ1
        JAC_I(8,I) = PZ2
        JAC_I(9,I) = PZ3
        JAC_I(10,I) = VOL(I)
      ENDDO
C-----------
      RETURN
      END
