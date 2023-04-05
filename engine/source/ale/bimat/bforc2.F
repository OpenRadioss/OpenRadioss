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
Chd|  BFORC2                        source/ale/bimat/bforc2.F     
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        BAFIL2                        source/ale/bimat/bafil2.F     
Chd|        BALPH2                        source/ale/bimat/balph2.F     
Chd|        BAMOM2                        source/ale/bimat/bamom2.F     
Chd|        BCUMU2                        source/ale/bimat/bcumu2.F     
Chd|        BCUMU2P                       source/ale/bimat/bcumu2.F     
Chd|        BCUMU2PA                      source/ale/bimat/bcumu2.F     
Chd|        BEFIL2                        source/ale/bimat/befil2.F     
Chd|        BEMOM2                        source/ale/bimat/bemom2.F     
Chd|        BLERO2                        source/ale/bimat/blero2.F     
Chd|        BREST2                        source/ale/bimat/brest2.F     
Chd|        CHECK_OFF_ALE                 source/elements/solid/solide/check_off_ale.F
Chd|        EDEFO2                        source/ale/euler2d/edefo2.F   
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        QBILAN                        source/elements/solid_2d/quad/qbilan.F
Chd|        QCOOR2                        source/elements/solid_2d/quad/qcoor2.F
Chd|        QDEFO2                        source/elements/solid_2d/quad/qdefo2.F
Chd|        QDLEN2                        source/elements/solid_2d/quad/qdlen2.F
Chd|        QFINT2                        source/elements/solid_2d/quad/qfint2.F
Chd|        QHVIS2                        source/elements/solid_2d/quad/qhvis2.F
Chd|        QMASS2                        source/elements/solid_2d/quad/qmass2.F
Chd|        QMASS2AP                      source/elements/solid_2d/quad/qmass2.F
Chd|        QMASS2P                       source/elements/solid_2d/quad/qmass2.F
Chd|        QMASSREAL2                    source/elements/solid_2d/quad/qmass2.F
Chd|        QMASSREAL2AP                  source/elements/solid_2d/quad/qmass2.F
Chd|        QMASSREAL2P                   source/elements/solid_2d/quad/qmass2.F
Chd|        QROTA2                        source/elements/solid_2d/quad/qrota2.F
Chd|        QVOLU2                        source/elements/solid_2d/quad/qvolu2.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE BFORC2 (ELBUF_TAB,NG   ,
     1                   PM     ,GEO    ,IC     ,X       ,
     2                   A      ,V      ,MS     ,W      ,FLUX    ,
     3                   FLU1   ,VEUL   ,FV     ,ALE_CONNECT  ,IPARG   ,
     4                   JPARG  ,FILL   ,DFILL  ,IMS    ,NLOC_DMG,
     5                   IFILL  ,TF     ,NPF    ,BUFMAT ,PARTSAV ,
     6                   DT2T   ,NELTST ,ITYPTST,STIFN  ,OFFSET  ,
     7                   EANI   ,IPARTQ ,NEL    ,IADQ   ,FSKY    ,
     8                   IPM    ,BUFVOIS,
     9                   GRESAV ,GRTH   ,IGRTH  ,TABLE  ,IGEO    ,
     O                   VOLN   ,ITASK  ,MS_2D  ,FSKYM  ,MATPARAM_TAB,
     B                   H3D_STRAIN)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE MESSAGE_MOD
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "vect01_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFILL,OFFSET,NEL,NG
      INTEGER IC(*), IPARG(NPARG,*), JPARG(*), IMS(*), 
     .        NPF(*),IPARTQ(*),IPM(*),IGEO(*),ITASK,H3D_STRAIN
      INTEGER NELTST,ITYPTST, IADQ(4,*),GRTH(*),IGRTH(*)
      my_real
     .   DT2T
      my_real
     .   PM(*), GEO(*), X(*), A(*), V(*), MS(*), W(*), FLUX(4,*),
     .   FLU1(*), VEUL(*), FV(*), FILL(NUMNOD,*),EANI(*),FSKY(*),
     .   DFILL(NUMNOD,*), TF(*), BUFMAT(*), PARTSAV(*), STIFN(*),
     .   BUFVOIS(6,*),GRESAV(*),VOLN(MVSIZ),MS_2D(*),FSKYM(*)
      TYPE (TTABLE) TABLE(*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NF1,NF2, MTN1, MTN2, LCO, IMULT, IFLAG,IBID
      INTEGER MAT(MVSIZ),NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ),
     .        NGL(MVSIZ),NGEO(MVSIZ), IBIDV(1) ,L_TEMP, L_PLA, L_BFRAC,L_BULK
      my_real
     .   SOFF
      my_real
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), 
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   AIRE(MVSIZ),AIRES(MVSIZ),AIREM(MVSIZ),VD2(MVSIZ),DVOL(MVSIZ),
     .   DELTAX(MVSIZ),VIS(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), VZ1(MVSIZ), 
     .   VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ),
     .   PY1(MVSIZ), PY2(MVSIZ), PZ1(MVSIZ), PZ2(MVSIZ),
     .   WYZ(MVSIZ),DYZ(MVSIZ),DZY(MVSIZ),QVIS(MVSIZ),SSP(MVSIZ),
     .   S1(MVSIZ),S2(MVSIZ),S3(MVSIZ),S4(MVSIZ),S5(MVSIZ),S6(MVSIZ),
     .   EYY(MVSIZ),EZZ(MVSIZ),ETT(MVSIZ),EYZ(MVSIZ),EYT(MVSIZ),
     .   EZT(MVSIZ),RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),
     .   SZ(MVSIZ), TX(MVSIZ), TY(MVSIZ), TZ(MVSIZ)
      my_real
     .   F11(MVSIZ), F12(MVSIZ), F21(MVSIZ), F22(MVSIZ),SSP_EQ(MVSIZ),
     .   T11(MVSIZ), T12(MVSIZ), T13(MVSIZ), T14(MVSIZ), T21(MVSIZ), 
     .   T22(MVSIZ), T23(MVSIZ),T24(MVSIZ), VDY(MVSIZ), VDZ(MVSIZ), 
     .   AX1(MVSIZ),AX2(MVSIZ), EHOU(MVSIZ)
C FORCE DE TRANSPORT BIMATERIAU
      my_real
     .   B11(MVSIZ), B12(MVSIZ), B13(MVSIZ), B14(MVSIZ),
     .   B21(MVSIZ), B22(MVSIZ), B23(MVSIZ), B24(MVSIZ)
      my_real
     .   WYY(MVSIZ),WZZ(MVSIZ),VDX(MVSIZ)
      my_real
     . MUVOID(MVSIZ), STI(MVSIZ),BID(1), MBID(1)
      my_real
     . SIGY(MVSIZ),ET(MVSIZ),GAMA(MVSIZ,6),
     . R1_FREE(MVSIZ),R3_FREE(MVSIZ),R4_FREE(MVSIZ)
      my_real
     . DALPH1(MVSIZ), DALPH2(MVSIZ)
      my_real
     . EYYS(MVSIZ), EZZS(MVSIZ), ETTS(MVSIZ), EYZS(MVSIZ), EYTS(MVSIZ), EZTS(MVSIZ),BIDM(MVSIZ)
      my_real VARNL(NEL)
C-------------------
      TYPE(L_BUFEL_)  ,POINTER :: LBUF, LBUF1,LBUF2     
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C--------------------------------------------------      
C
C=======================================================================
      GBUF  => ELBUF_TAB(NG)%GBUF
      LBUF1 => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)  ! buffer mat 1
      LBUF2 => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)  ! buffer mat 2
      MTN1  = JPARG(25)
      MTN2  = JPARG(26)
      NF1   = NFT + 1
      NF2   = NF1+NUMELQ
      LCO   = NFT*7 + 1
c
      IBIDV=0
      IBID = 0
      BID = ZERO
      BIDM(1:MVSIZ) = ZERO

      IF (NSPMD > 1) THEN
        CALL ANCMSG(MSGID=14,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
      DO I=1,NEL
       WYY(I)=ZERO
       WZZ(I)=ZERO
       VDX(I)=ZERO     
      ENDDO
C------------
      CALL QCOOR2(
     1   X,       IC(LCO), Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      NC1,     NC2,
     4   NC3,     NC4,     NGL,     MAT,
     5   NGEO,    VD2,     VIS,     NEL)
C------------
C      A.L.E.
C------------
      IF (JALE/=0) THEN
       CALL QVOLU2(GBUF%OFF,AIRE,VOLN,NGL,
     .             Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .             BID,BID)
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
       CALL BALPH2(PM,LBUF1%FRAC,LBUF2%FRAC,GBUF%VOL,FILL,
     .             LBUF1%SIG,LBUF1%EINT,LBUF1%VOL,LBUF1%RHO,
     .             FLUX(1,NF1),FLU1(NF1),LBUF1%OFF,
     .             LBUF2%SIG,LBUF2%EINT,LBUF2%VOL,LBUF2%RHO,
     .             FLUX(1,NF2),FLU1(NF2),LBUF2%OFF,
     .             GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%TEMP,
     .             MTN1,MTN2,NGL,GBUF%G_TEMP,GBUF%BFRAC,GBUF%G_BFRAC,
     .             GBUF%PLA,GBUF%G_PLA,VOLN,GBUF%QVIS,GBUF%G_QVIS,NEL, 
     .             AIRE, AIRES, 
     .             EYY, EZZ, ETT, EYZ, EYT, EZT,
     .             EYYS, EZZS, ETTS, EYZS, EYTS, EZTS, 
     .             MAT, NC1, NC2, NC3, NC4, 
     .             DALPH1, DALPH2)
       CALL BAFIL2(PM,V,W,FILL,DFILL,IMS,X,LBUF1%FRAC,LBUF2%FRAC, 
     .      AIRE, PY1, PY2, PZ1, PZ2, DALPH1, DALPH2, 
     .      MAT, NC1, NC2, NC3, NC4)
C-----------
C      EULER
C-----------
      ELSEIF(JEUL/=0)THEN
       CALL EDEFO2(GBUF%VOL,V,VEUL,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  PY1,PY2,PZ1,PZ2,
     .                  WYZ,DYZ,DZY,EYY,EZZ,ETT,EYZ,EYT,EZT,
     .                  VOLN,AIRE,DELTAX,VDY,VDZ,VD2,
     .                  NC1,NC2,NC3,NC4)
       CALL BALPH2(PM,LBUF1%FRAC,LBUF2%FRAC,GBUF%VOL,FILL,
     .             LBUF1%SIG,LBUF1%EINT,LBUF1%VOL,LBUF1%RHO,
     .             FLUX(1,NF1),FLU1(NF1),LBUF1%OFF,
     .             LBUF2%SIG,LBUF2%EINT,LBUF2%VOL,LBUF2%RHO,
     .             FLUX(1,NF2),FLU1(NF2),LBUF2%OFF,
     .             GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%TEMP,
     .             MTN1,MTN2,NGL,GBUF%G_TEMP,GBUF%BFRAC,GBUF%G_BFRAC,
     .             GBUF%PLA,GBUF%G_PLA,VOLN,GBUF%QVIS,GBUF%G_QVIS,NEL, 
     .             AIRE, AIRES, 
     .             EYY, EZZ, ETT, EYZ, EYT, EZT,
     .             EYYS, EZZS, ETTS, EYZS, EYTS, EZTS, 
     .             MAT, NC1, NC2, NC3, NC4, 
     .             DALPH1, DALPH2)
       CALL BEFIL2(PM,V,FILL,DFILL,IMS,X,LBUF1%FRAC,LBUF2%FRAC, 
     .      AIRE, PY1, PY2, PZ1, PZ2, DALPH1, DALPH2, 
     .      MAT, NC1, NC2, NC3, NC4)
      ENDIF
C-----------------------------------------------------
C     MULTIMATERIAUX 1
C-----------------------------------------------------
      IMULT=JMULT
      JMULT=1
      LBUF => ELBUF_TAB(NG)%BUFLY(JMULT)%LBUF(1,1,1)  ! buffer mat 1
      MTN=JPARG(25)
      L_TEMP = ELBUF_TAB(NG)%BUFLY(JMULT)%L_TEMP
      L_PLA  = ELBUF_TAB(NG)%BUFLY(JMULT)%L_PLA
      L_BFRAC= ELBUF_TAB(NG)%BUFLY(JMULT)%L_BFRAC 
      L_BULK = ELBUF_TAB(NG)%BUFLY(JMULT)%L_QVIS
c
      CALL BREST2(LBUF1%FRAC,GBUF%VOL,PM,
     .            LBUF1%OFF,LBUF1%VOL,SOFF,IC(LCO),VOLN,
     .            AIRE, AIRES, 
     .            EYY, EZZ, ETT, EYZ, EYT, EZT,
     .            EYYS, EZZS, ETTS, EYZS, EYTS, EZTS,
     .            VD2, DVOL,
     .            MAT)
      CALL BLERO2(LBUF1%FRAC,LBUF2%FRAC,LBUF1%VOL,LBUF1%RHO,
     .            FLUX(1,NF1),FLU1(NF1),LBUF1%OFF,PM,NGL,VOLN, 
     .            AIRE, EYY, EZZ, ETT, EYZ, EYT, EZT, VD2, DVOL, 
     .            VY1, VY2, VY3, VY4, 
     .            VZ1, VZ2, VZ3, VZ4,
     .            MAT)
      CALL QROTA2(LBUF1%SIG,S1,S2,S3,S4,S5,S6,WYZ,NEL)

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
          GAMA(I,1) = ONE                                
          GAMA(I,2) = ZERO                                  
          GAMA(I,3) = ZERO
          GAMA(I,4) = ZERO                                  
          GAMA(I,5) = ONE                                  
          GAMA(I,6) = ZERO 
        ENDDO
      ENDIF                                  
C-----------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONTITUTIVES
C-----------------------------------------------------
C pour le fluide en SPMD : BUFVOIS LOI11
      CALL MMAIN(
     1   ELBUF_TAB,NG   ,PM     ,GEO           ,FV	      ,ALE_CONNECT  ,
     2   IC     ,IPARG  ,V      ,TF            ,NPF	      ,BUFMAT ,
     3   STI    ,X      ,DT2T   ,NELTST        ,ITYPTST       ,OFFSET ,
     4   NEL    ,W      ,LBUF%OFF,NGEO	       ,
     6   MAT    ,NGL    ,VOLN   ,VD2	       ,DVOL	      ,DELTAX ,
     7   VIS    ,QVIS   ,SSP    ,S1	       ,S2	      ,S3     ,
     8   S4     ,S5     ,S6     ,EYY	       ,EZZ	      ,ETT    ,
     9   EYZ    ,EYT    ,EZT    ,WYY	       ,WZZ	      ,WYZ    ,
     A   RX     ,RY     ,RZ     ,SX	       ,SY	      ,SZ     ,
     B   VDX    ,VDY    ,VDZ    ,MUVOID        ,SSP_EQ        ,AIRE   ,
     C   SIGY   ,ET     ,BUFVOIS,LBUF%PLA      ,R3_FREE       ,R4_FREE,
     D   EYY    ,EZZ    ,ETT    ,EYZ	       ,EYT	      ,EZT    ,
     E   WYY    ,WZZ    ,WYZ    ,IPM	       ,GAMA	      ,BID    ,
     F   BID    ,BID    ,BID    ,BID           ,BID	      ,BID    ,
     G   IBID   ,BID    ,BID    ,IBID          ,JMULT	      ,MBID   ,
     H   MBID   ,1      ,1      ,1             ,TABLE	      ,BID    ,
     I   BID    ,BID    ,BID    ,IPARG(1,NG)   ,IGEO   ,BID    ,
     J   ITASK  ,NLOC_DMG,VARNL ,MATPARAM_TAB  ,H3D_STRAIN)
C----------------------------------
C     PETROV-GALERKIN PSEUDO MASSES ET MASSES ALE
C----------------------------------
      IF (IPARIT==0) THEN
        CALL QMASS2(LBUF%OFF,LBUF%RHO,MS,
     .                  AIRE,NC1,NC2,NC3,NC4)
      ELSE
        CALL QMASS2P(LBUF%OFF,LBUF%RHO,AIRE,FSKY,FSKY,IADQ)
      ENDIF
C--------------------------
C     UPDATE OF MASSES : ALE physical masses
C----------------------------  
      IF (JALE+JEUL > 0 )THEN
         IF (IPARIT == 0)THEN
          CALL QMASSREAL2(LBUF%OFF,LBUF%RHO,MS_2D,
     .                  VOLN,NC1,NC2,NC3,NC4)
         ELSE
          CALL QMASSREAL2P(LBUF%OFF,LBUF%RHO,VOLN,FSKYM,IADQ)
         ENDIF
      ENDIF
C------------------------
C     FORCES ANTI SABLIER
C------------------------
      CALL QHVIS2(PM,LBUF%OFF,LBUF%RHO,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  PY1,PY2,PZ1,PZ2,
     .                  T11,T12,T13,T14,T21,T22,T23,T24, 
     .                  AIRE,SSP,MAT,VD2,VIS,EANI,NGEO,GEO,
     .                  PARTSAV,IPARTQ,EHOU,IPARG(63,NG))
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IFLAG==0.OR.TT>=THIS.OR.MDESS/=0.
     .   OR.TT>=THIS1(1).OR.TT>=THIS1(2).
     .   OR.TT>=THIS1(3).OR.TT>=THIS1(4).OR.TT>=THIS1(5).
     .   OR.TT>=THIS1(6).OR.TT>=THIS1(7).OR.TT>=THIS1(8).
     .   OR.TT>=THIS1(9).OR.NTH/=0.OR.NANIM/=0          .
     .   OR.TT>=TABFIS(1).OR.TT>=TABFIS(2).
     .   OR.TT>=TABFIS(3).OR.TT>=TABFIS(4).OR.TT>=TABFIS(5).
     .   OR.TT>=TABFIS(6).OR.TT>=TABFIS(7).OR.TT>=TABFIS(8).
     .   OR.TT>=TABFIS(10))
     . CALL QBILAN(PARTSAV,LBUF%OFF,LBUF%EINT,LBUF%RHO,LBUF%RK,LBUF%VOL,
     .             VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,VOLN,IPARTQ,EHOU,
     .             BID,BID,BID,BID,GRESAV,GRTH,IGRTH,IBID,BID,ITASK)
C
C--------------------
C     FORCES INTERNES
C--------------------
      CALL QFINT2(LBUF%SIG,
     .   PY1, PY2, PZ1, PZ2, AIRE, VOLN, QVIS,
     .   F11, F12, F21, F22, AX1, AX2,
     .   BID, BID, BID, BID,NEL)
C------------------------
C     FORCES DE TRANSPORT
C------------------------
      IF (JALE>0.AND.MTN/=11)
     .  CALL BAMOM2(
     1   PM,        V,         W,         LBUF%RHO,
     2   LBUF1%FRAC,LBUF2%FRAC,FILL(1,1), B11,
     3   B12,       B13,       B14,       B21,
     4   B22,       B23,       B24,       PY1,
     5   PY2,       PZ1,       PZ2,       AIRE,
     6   MAT,       NC1,       NC2,       NC3,
     7   NC4,       NEL)
      IF (JEUL>0.AND.MTN/=11)
     .  CALL BEMOM2(
     1   PM,        V,         LBUF%RHO,  LBUF1%FRAC,
     2   LBUF2%FRAC,FILL(1,1), B11,       B12,
     3   B13,       B14,       B21,       B22,
     4   B23,       B24,       PY1,       PY2,
     5   PZ1,       PZ2,       AIRE,      MAT,
     6   NC1,       NC2,       NC3,       NC4,
     7   NEL)
C--------------
C     ASSEMBLE
C--------------
        IF(JEUL+JALE/=0) CALL CHECK_OFF_ALE(B11 ,B12 ,B13 ,B14 ,B21 ,
     1                                      B22 ,B23 ,B24 ,BIDM,BIDM,
     2                                      BIDM,BIDM,BIDM,BIDM,BIDM,
     3                                      BIDM,BIDM,BIDM,BIDM,BIDM,
     4                                      BIDM,BIDM,BIDM,BIDM,GBUF%OFF,
     5                                      1,NEL,NEL)

      IF (IPARIT==0) THEN
      CALL BCUMU2 (LBUF1%FRAC,LBUF2%FRAC,A,FILL(1,1),
     .             LBUF%SIG,LBUF%EINT,LBUF%RHO,LBUF%QVIS,GBUF%QVIS,
     .             GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%TEMP,LBUF%TEMP,
     .             GBUF%PLA,LBUF%PLA,GBUF%BFRAC,LBUF%BFRAC,    
     .             F11, F12, F21, F22, AX1,AX2,
     .             T11,T12,T13,T14,T21,T22,T23,T24, 
     .             B11,B12,B13,B14,B21,B22,B23,B24,
     .             NC1,NC2,NC3,NC4,STI,STIFN,
     .             L_TEMP, L_PLA, L_BFRAC, L_BULK,NEL)
      ELSE
       CALL BCUMU2P(LBUF1%FRAC,LBUF2%FRAC,FILL(1,1),
     .              LBUF%SIG,LBUF%EINT,LBUF%RHO,LBUF%QVIS,GBUF%QVIS,
     .              GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%TEMP,LBUF%TEMP,
     .              GBUF%PLA,LBUF%PLA,GBUF%BFRAC,LBUF%BFRAC,    
     .              F11, F12, F21, F22, AX1,AX2,
     .              T11,T12,T13,T14,T21,T22,T23,T24, 
     .              B11,B12,B13,B14,B21,B22,B23,B24,
     .              FSKY,FSKY,IADQ,STI,NC1,NC2,NC3,NC4,
     .              L_TEMP,L_PLA,L_BFRAC,L_BULK,NEL)
      ENDIF
      
      
      
      
      
      
C-----------------------------------------------------
C     MULTIMATERIAUX 2
C-----------------------------------------------------
C     NB1:OFF /NB2:SIG(6)/NB3:ENERGIE/NB4:RHO/NB5: Q /
C-----------------------------------------------------
      IF(IMULT>1)THEN
c
      JMULT=2
      LBUF => ELBUF_TAB(NG)%BUFLY(JMULT)%LBUF(1,1,1)  ! buffer mat 2
      MTN=JPARG(26)
      L_TEMP = ELBUF_TAB(NG)%BUFLY(JMULT)%L_TEMP
      L_PLA  = ELBUF_TAB(NG)%BUFLY(JMULT)%L_PLA      
      L_BFRAC= ELBUF_TAB(NG)%BUFLY(JMULT)%L_BFRAC      
      L_BULK = ELBUF_TAB(NG)%BUFLY(JMULT)%L_QVIS  
c
      CALL BREST2(LBUF2%FRAC,GBUF%VOL,PM,
     .            LBUF%OFF,LBUF%VOL,SOFF,IC(LCO),VOLN,
     .            AIRE, AIRES, 
     .            EYY, EZZ, ETT, EYZ, EYT, EZT,
     .            EYYS, EZZS, ETTS, EYZS, EYTS, EZTS,
     .            VD2, DVOL,
     .            MAT)
      CALL BLERO2(LBUF2%FRAC,LBUF1%FRAC,LBUF%VOL,LBUF%RHO,
     .            FLUX(1,NF2),FLU1(NF2),LBUF%OFF,PM,NGL,VOLN, 
     .            AIRE, EYY, EZZ, ETT, EYZ, EYT, EZT, VD2, DVOL, 
     .            VY1, VY2, VY3, VY4, 
     .            VZ1, VZ2, VZ3, VZ4,
     .            MAT)
      CALL QROTA2(LBUF%SIG,S1,S2,S3,S4,S5,S6,WYZ,NEL)
c
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
          GAMA(I,1) = ONE                                
          GAMA(I,2) = ZERO                                  
          GAMA(I,3) = ZERO
          GAMA(I,4) = ZERO                                  
          GAMA(I,5) = ONE                                  
          GAMA(I,6) = ZERO 
        ENDDO
      ENDIF                                  
C-----------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONTITUTIVES
C-----------------------------------------------------
      CALL MMAIN(ELBUF_TAB,NG   ,PM     ,GEO    ,FV      ,ALE_CONNECT   ,
     2           IC     ,IPARG  ,V      ,TF      ,NPF     ,BUFMAT ,
     3           STI    ,X      ,DT2T    ,NELTST  ,ITYPTST ,OFFSET ,
     4           NEL    ,W      ,LBUF%OFF,NGEO    ,
     6           MAT    ,NGL    ,VOLN   ,VD2     ,DVOL    ,DELTAX ,
     7           VIS    ,QVIS   ,SSP    ,S1      ,S2      ,S3     ,
     8           S4     ,S5     ,S6     ,EYY     ,EZZ     ,ETT    ,
     9           EYZ    ,EYT    ,EZT    ,WYY     ,WZZ     ,WYZ    ,
     A           RX     ,RY     ,RZ     ,SX      ,SY      ,SZ     ,
     B           VDX    ,VDY    ,VDZ    ,MUVOID  ,SSP_EQ  ,AIRE   ,
     C           SIGY   ,ET     ,BUFVOIS,LBUF%PLA,R3_FREE ,R4_FREE,
     D           EYY    ,EZZ    ,ETT    ,EYZ     ,EYT     ,EZT    ,
     E           WYY    ,WZZ    ,WYZ    ,IPM     ,GAMA    ,BID    ,
     F           BID    ,BID    ,BID    ,BID     ,BID      ,BID    ,
     G           IBID   ,BID    ,BID    ,IBID    ,JMULT   ,MBID   ,
     H           MBID   ,1      ,1      ,1       ,TABLE   ,BID    ,
     I           BID    ,BID    ,BID    ,IBIDV   ,IGEO   ,BID    ,
     J           ITASK  ,NLOC_DMG,VARNL ,MATPARAM_TAB,TH_STRAIN)
C----------------------------------
C     PETROV-GALERKIN PSEUDO MASSES ET MASSES ALE
C----------------------------------
      IF (IPARIT==0) THEN
      CALL QMASS2(LBUF%OFF,LBUF%RHO,MS,
     .                  AIRE,NC1,NC2,NC3,NC4)
      ELSE
       CALL QMASS2AP(LBUF%OFF,LBUF%RHO,AIRE,FSKY,FSKY,IADQ)
      ENDIF
C--------------------------
C     UPDATE OF MASSES : ALE physical masses
C----------------------------  
      IF (JALE+JEUL > 0 )THEN
         IF (IPARIT == 0)THEN
          CALL QMASSREAL2(LBUF%OFF,LBUF%RHO,MS_2D,
     .                  VOLN,NC1,NC2,NC3,NC4)
         ELSE
          CALL QMASSREAL2AP(LBUF%OFF,LBUF%RHO,VOLN,FSKYM,IADQ)
         ENDIF
      ENDIF
C------------------------
C     FORCES ANTI SABLIER
C------------------------
      CALL QHVIS2(PM,LBUF%OFF,LBUF%RHO,
     .            Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .            VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .            PY1,PY2,PZ1,PZ2,
     .            T11,T12,T13,T14,T21,T22,T23,T24, 
     .            AIRE,SSP,MAT,VD2,VIS,EANI,NGEO,GEO,
     .            PARTSAV,IPARTQ,EHOU,IPARG(63,NG))
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IFLAG==0.OR.TT>=THIS.OR.MDESS/=0.
     .   OR.TT>=THIS1(1).OR.TT>=THIS1(2).
     .   OR.TT>=THIS1(3).OR.TT>=THIS1(4).OR.TT>=THIS1(5).
     .   OR.TT>=THIS1(6).OR.TT>=THIS1(7).OR.TT>=THIS1(8).
     .   OR.TT>=THIS1(9).OR.NTH/=0.OR.NANIM/=0          .
     .   OR.TT>=TABFIS(1).OR.TT>=TABFIS(2).
     .   OR.TT>=TABFIS(3).OR.TT>=TABFIS(4).OR.TT>=TABFIS(5).
     .   OR.TT>=TABFIS(6).OR.TT>=TABFIS(7).OR.TT>=TABFIS(8).
     .   OR.TT>=TABFIS(10))
c
     . CALL QBILAN(PARTSAV,LBUF%OFF,LBUF%EINT,LBUF%RHO,LBUF%RK,
     .             LBUF%VOL,VY1,VY2,VY3,VY4,VZ1,
     .             VZ2,VZ3,VZ4,VOLN,IPARTQ,EHOU,
     .             BID,BID,BID,BID,GRESAV,GRTH,IGRTH,IBID,BID,ITASK)
C--------------------
C       FORCES INTERNES
C--------------------
        CALL QFINT2(LBUF%SIG,
     .     PY1, PY2, PZ1, PZ2, AIRE, VOLN, QVIS,
     .     F11, F12, F21, F22, AX1, AX2,
     .     BID, BID, BID, BID,NEL)
C------------------------
C       FORCES DE TRANSPORT
C------------------------
        IF(JALE>0.AND.MTN/=11)
     .    CALL BAMOM2(
     1   PM,        V,         W,         LBUF%RHO,
     2   LBUF2%FRAC,LBUF1%FRAC,FILL(1,2), B11,
     3   B12,       B13,       B14,       B21,
     4   B22,       B23,       B24,       PY1,
     5   PY2,       PZ1,       PZ2,       AIRE,
     6   MAT,       NC1,       NC2,       NC3,
     7   NC4,       NEL)
        IF(JEUL>0.AND.MTN/=11)
     .    CALL BEMOM2(
     1   PM,        V,         LBUF%RHO,  LBUF2%FRAC,
     2   LBUF1%FRAC,FILL(1,2), B11,       B12,
     3   B13,       B14,       B21,       B22,
     4   B23,       B24,       PY1,       PY2,
     5   PZ1,       PZ2,       AIRE,      MAT,
     6   NC1,       NC2,       NC3,       NC4,
     7   NEL)
C--------------
C       ASSEMBLE
C--------------
        IF(JEUL+JALE/=0) CALL CHECK_OFF_ALE(B11 ,B12 ,B13 ,B14 ,B21 ,
     1                                      B22 ,B23 ,B24 ,BIDM,BIDM,
     2                                      BIDM,BIDM,BIDM,BIDM,BIDM,
     3                                      BIDM,BIDM,BIDM,BIDM,BIDM,
     4                                      BIDM,BIDM,BIDM,BIDM,GBUF%OFF,
     5                                      1,NEL,NEL)
        IF(IPARIT==0)THEN
          CALL BCUMU2(LBUF2%FRAC,LBUF1%FRAC,A,FILL(1,2),
     .             LBUF%SIG,LBUF%EINT,LBUF%RHO,LBUF%QVIS,GBUF%QVIS,
     .             GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%TEMP,LBUF%TEMP,
     .             GBUF%PLA,LBUF%PLA,GBUF%BFRAC,LBUF%BFRAC,
     .             F11, F12, F21, F22, AX1,AX2,
     .             T11,T12,T13,T14,T21,T22,T23,T24, 
     .             B11,B12,B13,B14,B21,B22,B23,B24,
     .             NC1,NC2,NC3,NC4,STI,STIFN,L_TEMP,L_PLA,L_BFRAC,L_BULK,NEL)
        ELSE
          CALL BCUMU2PA(LBUF2%FRAC,LBUF1%FRAC,FILL(1,2),
     .                 LBUF%SIG,LBUF%EINT,LBUF%RHO,LBUF%QVIS,GBUF%QVIS,
     .                 GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%TEMP,LBUF%TEMP,
     .                 GBUF%PLA,LBUF%PLA,GBUF%BFRAC,LBUF%BFRAC,     
     .                 F11, F12, F21, F22, AX1,AX2,
     .                 T11,T12,T13,T14,T21,T22,T23,T24, 
     .                 B11,B12,B13,B14,B21,B22,B23,B24,
     .                 FSKY,FSKY,IADQ,STI,NC1,NC2,NC3,NC4,L_TEMP,L_PLA,
     .                 L_BFRAC,L_BULK,NEL)
        ENDIF
      ENDIF
C-----------
      RETURN
      END
