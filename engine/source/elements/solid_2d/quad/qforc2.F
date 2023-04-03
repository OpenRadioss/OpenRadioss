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
Chd|  QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        ALERO2                        source/ale/ale2d/alero2.F     
Chd|        AMOMT2                        source/ale/ale2d/amomt2.F     
Chd|        CHECK_OFF_ALE                 source/elements/solid/solide/check_off_ale.F
Chd|        EDE112                        source/ale/euler2d/ede112.F   
Chd|        EDEFO2                        source/ale/euler2d/edefo2.F   
Chd|        EMOMT2                        source/ale/euler2d/emomt2.F   
Chd|        EULRO2                        source/ale/euler2d/eulro2.F   
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        QBILAN                        source/elements/solid_2d/quad/qbilan.F
Chd|        QCOOR2                        source/elements/solid_2d/quad/qcoor2.F
Chd|        QCUMU2                        source/elements/solid_2d/quad/qcumu2.F
Chd|        QCUMU2P                       source/elements/solid_2d/quad/qcumu2.F
Chd|        QDEFO2                        source/elements/solid_2d/quad/qdefo2.F
Chd|        QDLEN2                        source/elements/solid_2d/quad/qdlen2.F
Chd|        QFINT2                        source/elements/solid_2d/quad/qfint2.F
Chd|        QHVIS2                        source/elements/solid_2d/quad/qhvis2.F
Chd|        QLAGR2                        source/elements/solid_2d/quad/qlagr2.F
Chd|        QMASS2                        source/elements/solid_2d/quad/qmass2.F
Chd|        QMASS2P                       source/elements/solid_2d/quad/qmass2.F
Chd|        QMASSREAL2                    source/elements/solid_2d/quad/qmass2.F
Chd|        QMASSREAL2P                   source/elements/solid_2d/quad/qmass2.F
Chd|        QRCOOR2                       source/elements/solid_2d/quad/qrcoor2.F
Chd|        QRDEFO2                       source/elements/solid_2d/quad/qrdefo2.F
Chd|        QROTA2                        source/elements/solid_2d/quad/qrota2.F
Chd|        QRROTA2                       source/elements/solid_2d/quad/qrrota2.F
Chd|        QVOLU2                        source/elements/solid_2d/quad/qvolu2.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE QFORC2(ELBUF_TAB,NG      ,
     1                  PM       ,GEO     ,IC      ,X        ,A       ,
     2                  V        ,MS      ,W       ,FLUX     ,FLU1    ,
     3                  VEUL     ,FV      ,ALE_CONNECT   ,IPARG    ,NLOC_DMG,
     4                  TF       ,NPF     ,BUFMAT  ,PARTSAV  ,
     5                  DT2T     ,NELTST  ,ITYPTST ,STIFN    ,OFFSET  ,
     6                  EANI     ,IPARTQ  ,NEL     ,IADQ     ,FSKY    ,
     9                  IPM      ,BUFVOIS ,QMV     ,
     A                  GRESAV   ,GRTH    ,IGRTH   ,TABLE    ,IGEO    ,
     B                  VOLN     ,ITASK   ,MS_2D   ,FSKYM    ,IOUTPRT ,
     C                  MATPARAM_TAB,H3D_STRAIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE ELBUFDEF_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IC(*), IPARG(NPARG,*), NPF(*),IPARTQ(*),
     .        IPM(*), GRTH(*),IGRTH(*),IGEO(*), IADQ(4,*), ITASK
      INTEGER OFFSET,NEL,NG,NELTST,ITYPTST,IOUTPRT,H3D_STRAIN
      my_real
     .   DT2T
      my_real
     .   PM(*), GEO(*), X(*), A(*), V(*), MS(*), W(*), FLUX(4,*),
     .   FLU1(*), VEUL(*), FV(*), TF(*), BUFMAT(*), FSKY(*),
     .   PARTSAV(*), STIFN(*),EANI(*), BUFVOIS(6,*),QMV(8,*),GRESAV(*),VOLN(MVSIZ),
     .   MS_2D(*),FSKYM(*)
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C=======================================================================
c FUNCTION: Internal force compute of Quad element
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   PM ,GEO             Material and geometrical property data
c  I   IC(7,NUM_QUAD)      connectivity and mid,pid integer data
c  I   X(3,NUMNOD)         co-ordinate 
c  IO  A(3,NUMNOD)         nodal internal force
c  I   V(3,NUMNOD)         nodal velocity
c  IO  MS(NUMNOD)          nodal masse
c  I   EV()                internal element(material) data
c  I   FLUX(4,NEL)         flux at each side used w/ ALE or EULER
c  I   FLU1  ,VEUL  ,IELVS used w/ ALE or EULER 
c  I   IPARG(NG)           element group data
c  I   ELBUF()             internal element(material) data used w/ ALE or EULER
c  I   TF() ,NPF()         Radioss function (x=Time) data
c  I   BUFMAT()            internal material data
c  IO  PARTSAV()           output use per part
c  IO  DT2T                smallest elementary time step
c  O   NELTST,ITYPTST      element type (property type for spring) which determine DT2T
c  IO  STIFN(NUMNOD)       nodal stiffness to calcul nodal time step
c  IO  EANI()              anim output vector
c  I   IPARTQ()            quad element group data (output)
c  I   NEL                 nb of quad element in this group
c  I   IADQ() ,FSKY()      work arrays for special option of internal force assemlage   
c  IO  XPHI,FPHI,VPHI,MSPHI,PV ,
c      X0PHI ,EVD : variables for sensibility(opt) no more used
c  I   BUFVOIS()           work table for fluide w/ SPMD 
c  I   QMV(8,)             work table used w/ ALE or EULER
c  I   GRESAV,GRTH,IGRTH   work table used for TH (time history) output
c  I   TABLE               new alternative Radioss function(table) data
c  I   IGEO                geometrical property integer data
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LCO, NF1, IFLAG,I,II,IPTR,IPTS,IPTT,ILAY,ISTRAIN
      INTEGER IBIDON(1),IBID,N
cmi     .   RX(1),RY(1), RZ(1), SX(1), SY(1), SZ(1), TX(1), TY(1), TZ(1)
cmi+++
      my_real
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     .   TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ)
cmi---
      INTEGER, DIMENSION(MVSIZ) :: MAT,NC1,NC2,NC3,NC4,NGL,NGEO
      my_real, DIMENSION(MVSIZ) :: F11, F12, F21, F22
      my_real, DIMENSION(MVSIZ) :: AX1, AX2
      my_real, DIMENSION(MVSIZ) :: T11, T12, T13, T14, T21, T22, T23, T24
      my_real, DIMENSION(MVSIZ) :: Y1, Y2, Y3, Y4, Z1, Z2, Z3, Z4
      my_real, DIMENSION(MVSIZ) :: VY1, VY2, VY3, VY4, VZ1, VZ2, VZ3, VZ4
      my_real, DIMENSION(MVSIZ) :: PY1, PY2, PZ1, PZ2
      my_real, DIMENSION(MVSIZ) :: AIRE,AIREM,QVIS,VIS,WYZ
      my_real, DIMENSION(MVSIZ) :: S1,S2,S3,S4,S5,S6
      my_real, DIMENSION(MVSIZ) :: VD2,DVOL,DELTAX
      my_real, DIMENSION(MVSIZ) :: DYZ,DZY,SSP
      my_real, DIMENSION(MVSIZ) :: EYY,EZZ,ETT,EYZ,EYT,EZT
      my_real, DIMENSION(MVSIZ) :: VDY, VDZ
      my_real
     .   EHOU(MVSIZ),SSP_EQ(MVSIZ)
      my_real
     .       WYY(MVSIZ),WZZ(MVSIZ),VDX(MVSIZ)
C Variables utilisees en argument par les materiaux si SPH uniquement.
      my_real
     .       MUVOID(MVSIZ), STI(MVSIZ),BID(1), MBID(1),RSDA(1)
C  Variables void MMAIN 
      my_real
     . SIGY(MVSIZ),ET(MVSIZ),GAMA(MVSIZ,6),
     . R3_FREE(MVSIZ),R4_FREE(MVSIZ)
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   Y234(MVSIZ),Y124(MVSIZ),BIDM(MVSIZ)
      my_real VARNL(NEL)
      my_real, 
     .  DIMENSION(:), POINTER :: EINT
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----
C
C--------------------------------------------------------------------
      GBUF => ELBUF_TAB(NG)%GBUF
c
      IBIDON=0 
      IBID = 0    
      BIDM(1:MVSIZ) = ZERO
      DO I=1,NEL
        WYY(I)=ZERO
        WZZ(I)=ZERO
        VDX(I)=ZERO  
      ENDDO
C
      IF (ISORTH == 0) THEN 
C       propriete isotrope
        DO I=1,NEL                                            
          GAMA(I,1) = ONE                                
          GAMA(I,2) = ZERO                                  
          GAMA(I,3) = ZERO
          GAMA(I,4) = ZERO                                  
          GAMA(I,5) = ONE                                  
          GAMA(I,6) = ZERO 
        ENDDO
      ELSE
        DO I=1,NEL
          GAMA(I,1) = GBUF%GAMA(I        )
          GAMA(I,2) = GBUF%GAMA(I +   NEL)
          GAMA(I,3) = GBUF%GAMA(I + 2*NEL)
          GAMA(I,4) = GBUF%GAMA(I + 3*NEL)
          GAMA(I,5) = GBUF%GAMA(I + 4*NEL)
          GAMA(I,6) = GBUF%GAMA(I + 5*NEL) 
        ENDDO                                   
      ENDIF
      ISTRAIN = IPARG(44,NG)                                 
C
      LCO=1+7*NFT
      NF1=NFT+1
C
      IF (JCVT == 0)THEN
        CALL QCOOR2(
     1   X,       IC(LCO), Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      NC1,     NC2,
     4   NC3,     NC4,     NGL,     MAT,
     5   NGEO,    VD2,     VIS,     NEL)
      ELSE
C------ Co-rotational system (convective local system)      
C       JCVT/=0 => JLAG/=0
        CALL QRCOOR2(
     1   X,       IC(LCO), Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      NC1,     NC2,
     4   NC3,     NC4,     NGL,     MAT,
     5   NGEO,    VD2,     R11,     R12,
     6   R13,     R21,     R22,     R23,
     7   R31,     R32,     R33,     GAMA,
     8   Y234,    Y124,    VIS,     NEL,
     9   ISORTH)
      END IF
c
      IF (JLAG/=0) THEN
C--------------
C      LAGRANGE, VOLUME and CHARACTERISTIC length (for DT) compute
C--------------
        CALL QVOLU2(GBUF%OFF,AIRE,VOLN,NGL,
     .              Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .              Y234 ,Y124 )
        CALL QDLEN2(Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,AIRE,DELTAX,IPARG(63,NG))
        IF (JCVT == 0) THEN
          CALL QDEFO2(
     1   V,       V,       Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      VY1,     VY2,
     4   VY3,     VY4,     VZ1,     VZ2,
     5   VZ3,     VZ4,     PY1,     PY2,
     6   PZ1,     PZ2,     WYZ,     DYZ,
     7   DZY,     EYY,     EZZ,     ETT,
     8   EYZ,     EYT,     EZT,     RX,
     9   RY,      RZ,      SX,      SY,
     A   SZ,      TX,      TY,      TZ,
     B   VOLN,    AIRE,    AIREM,   NC1,
     C   NC2,     NC3,     NC4,     NEL)
        ELSE
          CALL QRDEFO2(
     1   V,       Y1,      Y2,      Y3,
     2   Y4,      Z1,      Z2,      Z3,
     3   Z4,      VY1,     VY2,     VY3,
     4   VY4,     VZ1,     VZ2,     VZ3,
     5   VZ4,     PY1,     PY2,     PZ1,
     6   PZ2,     WYZ,     DYZ,     DZY,
     7   EYY,     EZZ,     ETT,     EYZ,
     8   EYT,     EZT,     RX,      RY,
     9   RZ,      SX,      SY,      SZ,
     A   TX,      TY,      TZ,      VOLN,
     B   AIRE,    AIREM,   NC1,     NC2,
     C   NC3,     NC4,     R22,     R23,
     D   R32,     R33,     NEL,     JCVT)
        END IF
c
        CALL QLAGR2(
     1   PM,       GBUF%VOL, GBUF%RHO, GBUF%EINT,
     2   VOLN,     DVOL,     MAT,      NEL)
        CALL QROTA2(
     1   GBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WYZ,
     3   NEL,     JCVT)
      ELSEIF (JALE/=0) THEN
C------------
C      A.L.E.
C------------
        CALL QVOLU2(GBUF%OFF,AIRE,VOLN,NGL,
     .              Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .              BID,BID)
        CALL QDLEN2(Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,AIRE,DELTAX,IPARG(63,NG))
        CALL QDEFO2(
     1   V,       W,       Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      VY1,     VY2,
     4   VY3,     VY4,     VZ1,     VZ2,
     5   VZ3,     VZ4,     PY1,     PY2,
     6   PZ1,     PZ2,     WYZ,     DYZ,
     7   DZY,     EYY,     EZZ,     ETT,
     8   EYZ,     EYT,     EZT,     RX,
     9   RY,      RZ,      SX,      SY,
     A   SZ,      TX,      TY,      TZ,
     B   VOLN,    AIRE,    AIREM,   NC1,
     C   NC2,     NC3,     NC4,     NEL)
        CALL ALERO2(GBUF%OFF,GBUF%VOL,GBUF%RHO,FLUX(1,NF1),FLU1(NF1),W,
     .                   VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                   VOLN,DVOL,VD2,
     .                   NC1,NC2,NC3,NC4,NGL)
        CALL QROTA2(
     1   GBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WYZ,
     3   NEL,     JCVT)
      ELSEIF (JEUL/=0) THEN
C-----------
C      EULER
C-----------
        IF (MTN == 11) CALL EDE112(PM,V,VEUL,X,IC,ALE_CONNECT,
     .                  WYZ,DYZ,DZY,EYY,EZZ,ETT,EYZ,EYT,EZT)
        CALL EDEFO2(GBUF%VOL,V,VEUL,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  PY1,PY2,PZ1,PZ2,
     .                  WYZ,DYZ,DZY,EYY,EZZ,ETT,EYZ,EYT,EZT,
     .                  VOLN,AIRE,DELTAX,VDY,VDZ,VD2,
     .                  NC1,NC2,NC3,NC4)
        CALL EULRO2(GBUF%VOL,GBUF%RHO,FLUX(1,NF1),FLU1(NF1),VOLN,
     .             DVOL    ,NGL     )
        CALL QROTA2(
     1   GBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WYZ,
     3   NEL,     JCVT)
      ENDIF
C-----------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONTITUTIVES(Constitutive relation compute)
C-----------------------------------------------------
C pour le fluide en SPMD : BUFVOIS LOI11
c
      ILAY = 1                                                
      IPTR = 1                                                
      IPTS = 1                                                
      IPTT = 1                                                
c
      CALL MMAIN(
     1   ELBUF_TAB,NG     ,PM     ,GEO       ,FV      ,ALE_CONNECT   ,
     2   IC       ,IPARG  ,V      ,TF        ,NPF     ,BUFMAT  ,
     3   STI      ,X      ,DT2T   ,NELTST    ,ITYPTST ,OFFSET ,
     4   NEL      ,W      ,GBUF%OFF,NGEO     ,
     6   MAT      ,NGL    ,VOLN   ,VD2       ,DVOL    ,DELTAX ,
     7   VIS      ,QVIS   ,SSP    ,S1        ,S2      ,S3     ,
     8   S4       ,S5     ,S6     ,EYY       ,EZZ     ,ETT    ,
     9   EYZ      ,EYT    ,EZT    ,WYY       ,WZZ     ,WYZ    ,
     A   RX       ,RY     ,RZ     ,SX        ,SY      ,SZ     ,
     B   VDX      ,VDY    ,VDZ    ,MUVOID    ,SSP_EQ  ,AIRE   ,
     C   SIGY     ,ET     ,BUFVOIS,GBUF%PLA  ,R3_FREE ,R4_FREE,
     D   EYY      ,EZZ    ,ETT    ,EYZ       ,EYT     ,EZT    ,
     E   WYY      ,WZZ    ,WYZ    ,IPM       ,GAMA    ,BID    ,
     F   BID      ,BID    ,BID    ,BID       ,BID     ,BID    ,
     G   ISTRAIN  ,BID    ,BID    ,IBIDON(1) ,ILAY   ,MBID   ,
     H   MBID     ,IPTR   ,IPTS   ,IPTT      ,TABLE   ,BID    ,
     I   BID      ,BID    ,BID    ,IBIDON(1) ,IGEO    ,BID      ,
     J   ITASK    ,NLOC_DMG,VARNL ,MATPARAM_TAB,H3D_STRAIN)
C--------------------------
C       BILANS PAR MATERIAU thermique
C--------------------------
      IF(JLAG+JALE+JEUL == 0)THEN
        IFLAG=MOD(NCYCLE,NCPRI)
        IF(IOUTPRT>0)THEN
c
           IF (MTN == 11) THEN
             EINT => ELBUF_TAB(NG)%GBUF%EINS(1:NEL)
           ELSE
             EINT => ELBUF_TAB(NG)%GBUF%EINT(1:NEL)
           ENDIF
          CALL QBILAN(PARTSAV,GBUF%OFF,EINT,GBUF%RHO,GBUF%RK,
     2              GBUF%VOL,VY1,VY2,VY3,VY4,VZ1,
     3              VZ2,VZ3,VZ4,VOLN,IPARTQ,EHOU,R22  ,
     4              R23    ,R32  ,R33  ,GRESAV,GRTH,IGRTH,
     5              IBIDON(1),GBUF%EINTTH,ITASK)
        ENDIF
        RETURN
      ENDIF
C----------------------------------
C     PETROV-GALERKIN PSEUDO MASSES ET MASSES ALE
C----------------------------------
      IF (IPARIT == 0)THEN
       CALL QMASS2(GBUF%OFF,GBUF%RHO,MS,
     .                  AIRE,NC1,NC2,NC3,NC4)
      ELSE
       CALL QMASS2P(GBUF%OFF,GBUF%RHO,AIRE,FSKY,FSKY,IADQ)
      ENDIF
C--------------------------
C     UPDATE OF MASSES : ALE physical masses
C----------------------------   
      IF (JALE+JEUL > 0 )THEN
         IF (IPARIT == 0)THEN
          CALL QMASSREAL2(GBUF%OFF,GBUF%RHO,MS_2D,
     .                  VOLN,NC1,NC2,NC3,NC4)
         ELSE
          CALL QMASSREAL2P(GBUF%OFF,GBUF%RHO,VOLN,FSKYM,IADQ)
         ENDIF
      ENDIF
C------------------------
C     FORCES ANTI SABLIER (HOURGLASS CONTROL)
C------------------------
      CALL QHVIS2(PM,GBUF%OFF,GBUF%RHO,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  PY1,PY2,PZ1,PZ2,
     .                  T11,T12,T13,T14,T21,T22,T23,T24, 
     .                  AIRE,SSP,MAT,VD2,VIS,EANI,NGEO,GEO,
     .                  PARTSAV,IPARTQ,EHOU,IPARG(63,NG))
C--------------------------
C     BILANS PAR MATERIAU (output Result summary)
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IOUTPRT>0)THEN
c
        IF (MTN == 11) THEN                                      
          EINT => ELBUF_TAB(NG)%GBUF%EINS(1:NEL)                     
        ELSE                                                     
          EINT => ELBUF_TAB(NG)%GBUF%EINT(1:NEL)                     
        ENDIF                                                    
        CALL QBILAN(PARTSAV,GBUF%OFF,EINT,GBUF%RHO,GBUF%RK,
     2              GBUF%VOL,VY1,VY2,VY3,VY4,VZ1,
     3              VZ2,VZ3,VZ4,VOLN,IPARTQ, EHOU,R22  ,
     4              R23    ,R32  ,R33  ,GRESAV,GRTH,IGRTH,
     5              IBIDON(1) ,GBUF%EINTTH,ITASK)
      ENDIF
C
C------------------------
C     FORCES DE TRANSPORT
C------------------------
       IF(JALE>0.AND.MTN/=11)
     .   CALL AMOMT2(PM,V,W,GBUF%RHO,
     .   Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .   T11, T12, T13, T14, T21, T22, T23 , T24, 
     .   PY1, PY2, PZ1, PZ2, AIREM,
     .   VY1, VY2, VY3, VY4, VZ1, VZ2, VZ3, VZ4,
     .   EYY, EZZ, DYZ, DZY,
     .   NC1,NC2,NC3,NC4,MAT,GBUF%OFF,QMV,BUFMAT, DELTAX, VIS)
       IF(JEUL>0.)
     .   CALL EMOMT2(PM,GBUF%RHO,
     .   Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .   VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .   T11, T12, T13, T14, T21, T22, T23 , T24, 
     .   PY1, PY2, PZ1, PZ2, AIRE,
     .   EYY, EZZ, DYZ, DZY, VDY, VDZ,
     .   DELTAX,VIS,VD2,
     .   NC1,NC2,NC3,NC4,MAT,QMV,BUFMAT )
C--------------------
C     FORCES INTERNES
C--------------------
      CALL QFINT2(
     1   GBUF%SIG,PY1,     PY2,     PZ1,
     2   PZ2,     AIRE,    VOLN,    QVIS,
     3   F11,     F12,     F21,     F22,
     4   AX1,     AX2,     R22,     R23,
     5   R32,     R33,     NEL,     JCVT)
C--------------
      IF(JCVT/=0)THEN
        CALL QRROTA2(
     1   R22,     R32,     R23,     R33,
     2   F11,     F21,     F12,     F22,
     3   T11,     T21,     T12,     T22,
     4   T13,     T23,     T14,     T24,
     5   NEL)
      END IF
        IF(JEUL+JALE/=0) CALL CHECK_OFF_ALE(T11 ,T21 ,T12 ,T22 ,T13 ,
     1                                      T23 ,T14 ,T24 ,BIDM,BIDM,
     2                                      BIDM,BIDM,BIDM,BIDM,BIDM,
     3                                      BIDM,BIDM,BIDM,BIDM,BIDM,
     4                                      BIDM,BIDM,BIDM,BIDM,GBUF%OFF,
     5                                      1,NEL,NEL)
C--------------
C     ASSEMBLE
C--------------
      IF(IPARIT == 0)THEN
        CALL QCUMU2(A,
     .   F11, F12, F21, F22, AX1, AX2,
     .   T11, T12, T13, T14, T21, T22, T23, T24, 
     .   NC1, NC2, NC3, NC4, STI, STIFN)
      ELSE
        CALL QCUMU2P(
     .   F11, F12, F21, F22, AX1, AX2,
     .   T11, T12, T13, T14, T21, T22, T23, T24, 
     .   FSKY,FSKY,IADQ,STI)
      ENDIF
c-----------
      RETURN
      END
