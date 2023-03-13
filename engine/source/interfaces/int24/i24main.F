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
Chd|  I24MAINF                      source/interfaces/int24/i24main.F
Chd|-- called by -----------
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FRICTIONPARTS_MODEL_ISOT      source/interfaces/int07/frictionparts_model.F
Chd|        FRICTIONPARTS_MODEL_ORTHO     source/interfaces/int07/frictionparts_model.F
Chd|        I24CDCOR3                     source/interfaces/int24/i24main.F
Chd|        I24COR3                       source/interfaces/int24/i24cor3.F
Chd|        I24DST3                       source/interfaces/int24/i24dst3.F
Chd|        I24DST3E                      source/interfaces/int24/i24dst3e.F
Chd|        I24FOR3                       source/interfaces/int24/i24for3.F
Chd|        I24GAP_PXFEM                  source/interfaces/int24/i24gap_pxfem.F
Chd|        I24IPLY_PXFEM                 source/interfaces/int24/i24iply_pxfem.F
Chd|        IMPL_SAV                      source/interfaces/int24/i24main.F
Chd|        IMPL_SAV0                     source/interfaces/int24/i24main.F
Chd|        IMPL_SAV1                     source/interfaces/int24/i24main.F
Chd|        I_CORPFIT3                    source/interfaces/int24/i24cor3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SUM_6_FLOAT_SENS              source/system/parit.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|====================================================================
      SUBROUTINE I24MAINF(
     1                   IPARI  ,INTBUF_TAB         ,X        ,A      ,
     2                   ICODT  ,FSAV    ,V         ,MS       ,DT2T   ,
     3                   NELTST ,ITYPTST ,ITAB      ,STIFN    ,FSKYI  ,
     4                   ISKY   ,FCONT   ,NIN       ,LINDMAX  ,KINET  ,
     5                   JTASK  ,NB_JLT  ,NB_JLT_NEW,NB_STOK_N,
     6                   NISKYFI,NEWFRONT,NSTRF     ,SECFCUM  ,ICONTACT  ,
     7                   VISCN    ,NUM_IMP,
     9                   NS_IMP ,NE_IMP  ,IND_IMP   ,FSAVSUB  ,NRTMDIM,
     A                   FSAVBAG,
     B                   EMINX  ,IXS     ,IXS16     ,IXS20    ,FNCONT ,
     C                   FTCONT ,IAD_ELEM,FR_ELEM   ,RCONTACT ,ACONTACT,
     D                   PCONTACT,TEMP      ,FTHE     ,FTHESKYI,
     E                   PM      ,IPARG ,IAD17   ,MSKYI_SMS  ,ISKYI_SMS,
     F                   NODNX_SMS,MS0  ,INOD_PXFEM,MS_PLY    ,WAGAP  ,
     G                   FBSAV6   ,ISENSINT,
     H                   DIMFB    ,H3D_DATA ,INTBUF_FRIC_TAB, T2MAIN_SMS,
     I                   FORNEQS  ,T2FAC_SMS,NPC    ,TF       ,TAGNCONT,
     J                   KLOADPINTER,LOADPINTER,LOADP_HYD_INTER,DGAPLOADINT,
     .                                                         S_LOADPINTER,
     K                   INTEREFRIC)
C========================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MOD_NODNORM
      USE MOD_RCURV
      USE INTBUFDEF_MOD
      USE IMP_INTBUF
      USE H3D_MOD
      USE INTBUF_FRIC_MOD
      USE MESSAGE_MOD
      USE OUTPUTS_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr04_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "impl1_c.inc"
#include      "macro.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST,NIN,NEWFRONT,
     .        NSTRF(*),
     .        NRTMDIM, IAD17, IPARSENS
      INTEGER IPARI(NPARI,NINTER), ICODT(*),ICONTACT(*),
     .        ITAB(*), ISKY(*), KINET(*), 
     .        IPARG(NPARG,*),INOD_PXFEM(*),NPC(*),TAGNCONT(NLOADP_HYD_INTER,NUMNOD)
      INTEGER  , INTENT(IN) :: S_LOADPINTER
      INTEGER  , INTENT(IN) :: KLOADPINTER(NINTER+1),LOADPINTER(S_LOADPINTER),
     .         LOADP_HYD_INTER(NLOADP_HYD)
      INTEGER NB_JLT,NB_JLT_NEW,NB_STOK_N,JTASK,
     .        NISKYFI, LINDMAX
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*),IND_IMP(*)
      INTEGER IXS(*)  ,IXS16(*) ,IXS20(*)
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), 
     .        ISKYI_SMS(*), NODNX_SMS(*), ISENSINT(*),DIMFB,T2MAIN_SMS(6,*)
      INTEGER  , INTENT(IN) :: INTEREFRIC
      my_real  , INTENT(IN) :: DGAPLOADINT(S_LOADPINTER)
      my_real 
     .        EMINX(*)
C     REAL
      my_real DT2T,
     .   X(*), A(3,*), FSAV(*), V(3,*),FSAVBAG(*),
     .   MS(*),STIFN(*),FSKYI(LSKYI,4),FCONT(3,*),MS0(*),
     .   SECFCUM(7,NUMNOD,NSECT),VISCN(*), FSAVSUB(*),
     .   FNCONT(3,*), FTCONT(3,*), RCONTACT(*), ACONTACT(*),
     .   PCONTACT(*),TF(*),
     .   TEMP(*),FTHE(*),FTHESKYI(LSKYI),PM(NPROPM,*),
     .   MSKYI_SMS(*),MS_PLY(*),WAGAP(*),FORNEQS(3,*),T2FAC_SMS(*)
      DOUBLE PRECISION FBSAV6(12,6,DIMFB)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(INTBUF_FRIC_STRUCT_), TARGET, DIMENSION(NINTERFRIC) ::  INTBUF_FRIC_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JD(50),KD(50), JFI, KFI,IEDGE,
     .        I, J, H, I_STOK, JLT_NEW, JLT , NFT, IVIS2,
     .        IBC, NOINT, NSEG, ISECIN, IBAG, IADM,
     .        IGAP, INACTI, IFQ, MFROT, IGSTI, NISUB,
     .        NB_LOC, I_STOK_LOC,DEBUT, I_STOK_GLO,
     .        ILAGM, LENR, LENT, MAXCC,INTTH,IFORM,INTPLY,INTFRIC,
     .        NSETPRTS ,NPARTFRIC, IERROR, INTNITSCHE, IORTHFRIC,
     .        NFORTH ,NFISOT ,JJ
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), CN_LOC(MVSIZ),CE_LOC(MVSIZ),
     .        CAND_N_N(MVSIZ),CAND_E_N(MVSIZ),CAND_T_N(MVSIZ),
     .        KINI(MVSIZ),
     .        SUBTRIA_OLD(MVSIZ),
c     .        INDEX2(LINDMAX),SUBTRIA_OLD(MVSIZ),
     .        ISDSIZ(NSPMD+1),IRCSIZ(NSPMD+1),
     .        IELECI(MVSIZ), NSMS(MVSIZ),IXX(MVSIZ,13),ITRIV(4,MVSIZ),
     .        IPLY(4,MVSIZ),ISPT2_LOC(MVSIZ),
     .        IPARTFRICSI(MVSIZ),IPARTFRICMI(MVSIZ),INDEXISOT(MVSIZ),
     .        INDEXORTH(MVSIZ),IREP_FRICMI(MVSIZ),IXX3(MVSIZ),IXX4(MVSIZ)
      INTEGER, ALLOCATABLE, DIMENSION(:) :: INDEX2
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,PMAX_GAP,
     .   VISC,VISCF,STIGLO,GAPMIN,
     .   KMIN, KMAX, GAPMAX,RSTIF,FHEAT,TINT,RHOH,EPS,IFRIC
C-----------------------------------------------
C     REAL
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     SUBTRIA(MVSIZ), 
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ),
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ),
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), 
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   GAPV(MVSIZ),MSI(MVSIZ),
     .   NM1(MVSIZ), NM2(MVSIZ), NM3(MVSIZ), 
     .   TEMPI(MVSIZ),PHI(MVSIZ),AREASI(MVSIZ),
     .   XX0(MVSIZ,17),YY0(MVSIZ,17),ZZ0(MVSIZ,17),
     .   VX(MVSIZ,17),VY(MVSIZ,17),VZ(MVSIZ,17),
     .   GAPS(MVSIZ),FORNEQSI(MVSIZ,3),DIST(MVSIZ)
      my_real
     .     , DIMENSION(:,:),ALLOCATABLE :: SURF
      my_real
     .     , DIMENSION(:), ALLOCATABLE ::  PRES
      my_real
     .     , DIMENSION(:,:,:), ALLOCATABLE :: FSAVPARIT
      SAVE SURF,PRES
      my_real
     .     RCURVI(MVSIZ), ANGLMI(MVSIZ), ANGLT, PADM,PENMIN,MARGE
      INTEGER NRTMFT, NRTMLT, NMNFT, NMNLT, NRADM,NCY_PFIT
      my_real
     .     T_PFIT,FA,FB,F_PFIT,TNCY,FINC
      INTEGER ICURV,ILEV,NREBOU,NPT ,NRTSE,IEDG4,SFSAVPARIT,NINLOADP
      my_real
     .    XFILTR_FRIC,FRIC_COEFS(MVSIZ,10),VISCFFRIC(MVSIZ),FRICC(MVSIZ),
     .    FRIC_COEFS2(MVSIZ,10),VISCFFRIC2(MVSIZ),FRICC2(MVSIZ),
     .    DIR1(MVSIZ,3),DIR2(MVSIZ,3),DIR_FRICMI(MVSIZ,2),
     .    XX1(MVSIZ), XX2(MVSIZ), XX3(MVSIZ), XX4(MVSIZ),
     .    YY1(MVSIZ), YY2(MVSIZ), YY3(MVSIZ), YY4(MVSIZ),
     .    ZZ1(MVSIZ), ZZ2(MVSIZ), ZZ3(MVSIZ), ZZ4(MVSIZ)
      INTEGER, DIMENSION(:) ,POINTER  :: TABCOUPLEPARTS_FRIC
      INTEGER, DIMENSION(:) ,POINTER  :: TABPARTS_FRIC 
      INTEGER, DIMENSION(:) ,POINTER  :: ADPARTS_FRIC 
      INTEGER, DIMENSION(:) ,POINTER  :: IFRICORTH 
      my_real, DIMENSION(:) ,POINTER  :: TABCOEF_FRIC
 
      INTEGER,TARGET, DIMENSION(1):: TABCOUPLEPARTS_FRIC_BID
      INTEGER,TARGET, DIMENSION(1):: TABPARTS_FRIC_BID
      INTEGER,TARGET, DIMENSION(1):: ADPARTS_FRIC_BID
      INTEGER,TARGET, DIMENSION(1):: IFRICORTH_BID
      my_real,TARGET, DIMENSION(1):: TABCOEF_FRIC_BID
C
C
      NRTM   =IPARI(4,NIN)
      NSN   =IPARI(5,NIN)
      NTY   =IPARI(7,NIN)
      IBC   =IPARI(11,NIN)
      IVIS2 =IPARI(14,NIN)
      IF(IPARI(33,NIN)==1) RETURN
      NOINT =IPARI(15,NIN)
      IGAP  =IPARI(21,NIN)
      INACTI=IPARI(22,NIN)
      ISECIN=IPARI(28,NIN)
      MFROT =IPARI(30,NIN)
      IFQ =IPARI(31,NIN) 
      IBAG =IPARI(32,NIN) 
      IGSTI=IPARI(34,NIN)
      NISUB =IPARI(36,NIN)
      ICURV =IPARI(39,NIN)
      IEDGE = IPARI(58,NIN)

C adaptive meshing
      IADM =IPARI(44,NIN) 
      NRADM=IPARI(49,NIN) 
      PADM =INTBUF_TAB%VARIABLES(24)
      ANGLT=INTBUF_TAB%VARIABLES(25)
      MARGE=INTBUF_TAB%VARIABLES(25)
C heat interface
      INTTH = IPARI(47,NIN)
      IFORM = IPARI(48,NIN)
      INTPLY = IPARI(66,NIN)
C-----implicit IGSTI=6      
      NREBOU=IPARI(53,NIN)
      NSNE = IPARI(55,NIN)
      IEDG4 = IPARI(59,NIN)
C---- IEDG4 =1 (IEDGE=4) w/o coulage extension...; =2(IEDGE=5) w/ coulage; 
C      
      STIGLO=-INTBUF_TAB%STFAC(1)
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C  
      FRIC  =INTBUF_TAB%VARIABLES(1)
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      VISC  =INTBUF_TAB%VARIABLES(14)
C      VISCF =INTBUF_TAB%VARIABLES(15)
      T_PFIT = INTBUF_TAB%VARIABLES(15)
      VISCF = ZERO
C
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
C

      RSTIF   = INTBUF_TAB%VARIABLES(20)
      FHEAT   = INTBUF_TAB%VARIABLES(21)
      TINT    = INTBUF_TAB%VARIABLES(22)
      PENMIN  = INTBUF_TAB%VARIABLES(38)
      EPS     = INTBUF_TAB%VARIABLES(39)
      PMAX_GAP = ZERO
      ILEV  = IPARI(20,NIN)
      NRTSE = IPARI(52,NIN)

      IFRIC = 0
C--- Corresponding Friction model  
      INTFRIC=IPARI(72,NIN)
      IORTHFRIC = 0
      NSETPRTS = 0
      NPARTFRIC = 0
      XFILTR_FRIC = ZERO
      IF(INTFRIC /= 0) THEN 
         TABCOUPLEPARTS_FRIC  => INTBUF_FRIC_TAB(INTFRIC)%TABCOUPLEPARTS_FRIC
         TABCOEF_FRIC  => INTBUF_FRIC_TAB(INTFRIC)%TABCOEF_FRIC
         TABPARTS_FRIC  => INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC
         ADPARTS_FRIC   => INTBUF_FRIC_TAB(INTFRIC)%ADPARTS_FRIC 
         XFILTR_FRIC   = INTBUF_FRIC_TAB(INTFRIC)%XFILTR_FRIC  
         NSETPRTS =   INTBUF_FRIC_TAB(INTFRIC)%NSETPRTS
         NPARTFRIC =   INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC
         IORTHFRIC = INTBUF_FRIC_TAB(INTFRIC)%IORTHFRIC
         IFRICORTH => INTBUF_FRIC_TAB(INTFRIC)%IFRICORTH
c         MFROT    =   INTBUF_FRIC_TAB(INTFRIC)%FRICMOD  ! These Flags are already put in Ipari
c         IFQ      =   INTBUF_FRIC_TAB(INTFRIC)%IFFILTER 
      ELSE
         TABCOUPLEPARTS_FRIC  => TABCOUPLEPARTS_FRIC_BID
         TABPARTS_FRIC  => TABPARTS_FRIC_BID
         TABCOEF_FRIC  => TABCOEF_FRIC_BID 
         ADPARTS_FRIC   => ADPARTS_FRIC_BID
         IFRICORTH   => IFRICORTH_BID
         IF (IFQ/=0) XFILTR_FRIC = INTBUF_TAB%XFILTR(1)
      ENDIF
C--- NITSCHE METHOD 
      INTNITSCHE=IPARI(86,NIN)
C
      NINLOADP = IPARI(95,NIN) ! load pressure related to inter
C
C-----T_FIT .OR. NCY_PFIT 
       F_PFIT = ZERO
       IF (STARTT>ZERO.AND.T_PFIT==ZERO) THEN
        T_PFIT=10000*DT12
        INTBUF_TAB%VARIABLES(15) = T_PFIT
       END IF
       IF (T_PFIT>ZERO) THEN
        IF (TT <=(STARTT+T_PFIT)) THEN 
         TNCY = (TT+EM05-STARTT)/T_PFIT
        ELSE
         IPARI(40,NIN)= 0
        END IF
       ELSE
        NCY_PFIT = IPARI(40,NIN)
        IF (NCY_PFIT >0 .AND. NCYCLE> NCY_PFIT) IPARI(40,NIN) = 0
		IF (IPARI(40,NIN)>0) THEN
          FINC= ONE/IPARI(40,NIN)
          TNCY = (NCYCLE+1)*FINC
		END IF
       END IF
      ALLOCATE(INDEX2(LINDMAX))

C ------ Move to another place ----
CCC        IF (IEDG4 >0) THEN
CCC          NPT = 3
CCC         IF (JTASK ==1 ) THEN
CCC          ALLOCATE(MSFIC(NSNE))
CCC          CALL I24FICS_INI(INTBUF_TAB%IRTSE   ,NSNE    ,INTBUF_TAB%IS2SE   ,
CCC     1                     INTBUF_TAB%IS2PT   ,NSN     ,INTBUF_TAB%NSV     ,
CCC     2                     MS                 ,MSFIC   ,NPT                )
CCC         END IF !(JTASK ==1 ) THEN
CCC         CALL MY_BARRIER()
CCC        END IF
C
c----------------------------------------------------
c   Courbure quadratique calcul des normales nodales
c----------------------------------------------------
      IF(ICURV==3)THEN
      ENDIF!(ICURV==3)
c----------------------------------------------------
c   Rayon de courbure : calcul des normales nodales (normees)
C   IADM!=0 + Icurv!=0 non available (starter error).
c----------------------------------------------------
      IF(IADM/=0)THEN
      END IF!(IADM/=0)
C----------------------------------------------------
C----------------------------------------------------
C
      I_STOK_GLO = INTBUF_TAB%I_STOK(1)
C
C decoupage statique

      NB_LOC = I_STOK_GLO / NTHREAD
      IF (JTASK==NTHREAD) THEN
        I_STOK_LOC = I_STOK_GLO-NB_LOC*(NTHREAD-1)
      ELSE
        I_STOK_LOC = NB_LOC
      ENDIF
      DEBUT = (JTASK-1)*NB_LOC

      I_STOK = 0
C
C recalcul du istok
C
      DO I = JTASK, I_STOK_GLO, NTHREAD
        IF(INTBUF_TAB%CAND_N(I)<0) THEN
          I_STOK = I_STOK + 1
          INDEX2(I_STOK) = I
          INTBUF_TAB%CAND_N(I) = -INTBUF_TAB%CAND_N(I)
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
      IF (IMPL_S==1) THEN
          NUM_IMP = 0
          VISC  =ZERO
          VISCF =ZERO
      ENDIF
c------------------------------------------------
C
        SFSAVPARIT = 0
        DO I=1,NISUB+1
          IF(ISENSINT(I)/=0) THEN
            SFSAVPARIT = SFSAVPARIT + 1
          ENDIF
        ENDDO
        IF (SFSAVPARIT /= 0) THEN
          ALLOCATE(FSAVPARIT(NISUB+1,11,I_STOK),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .           C1='(/INTER/TYPE7)')
           CALL ARRET(2)
          ENDIF
          FSAVPARIT(1:NISUB+1,1:11,1:I_STOK) = ZERO
        ELSE
          ALLOCATE(FSAVPARIT(0,0,0),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .           C1='(/INTER/TYPE24)')
           CALL ARRET(2)
          ENDIF
          
        ENDIF
c
c------------------------------------------------
      IF (DEBUG(3)>=1) THEN
          NB_JLT = NB_JLT + I_STOK_LOC
          NB_STOK_N = NB_STOK_N + I_STOK
      ENDIF
C
      DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C preparation candidats retenus
	  CALL I24CDCOR3(
     1     JLT,INDEX2(NFT+1),INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,INTBUF_TAB%CAND_T,
     2     CAND_E_N,CAND_N_N,CAND_T_N	  ,IEDGE	)
C cand_n et cand_e remplace par cand_n_n et cand_e_n
C-------ISPT2 change    dimension NSN, initialis   au Starter
          CALL I24COR3(
     1  JLT       ,X          ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV  ,CAND_E_N  ,
     2  CAND_N_N  ,CAND_T_N   ,INTBUF_TAB%STFM  ,INTBUF_TAB%STFNS,STIF      ,
     3  XX0       ,YY0        ,ZZ0       ,VX           ,VY           ,   
     5  VZ        ,XI         ,YI        ,ZI           ,VXI          ,
     7  VYI       ,VZI        ,IXX       ,NSVG         ,INTBUF_TAB%NVOISIN,
     9  MS,MSI    ,NSN        ,V         ,KINET        ,
     A  KINI      ,NTY        ,NIN       ,IGSTI        ,KMIN         ,  
     B  KMAX      ,INTBUF_TAB%GAP_S,GAPS ,NODNX_SMS    ,NSMS         ,
     C  ITRIV     ,INTBUF_TAB%XFIC,INTBUF_TAB%VFIC ,INTBUF_TAB%MSFIC  ,  
     D  INTBUF_TAB%IRTSE  ,INTBUF_TAB%IS2SE,INTBUF_TAB%IS2PT,INTBUF_TAB%ISEGPT,
     E  NSNE      ,INTBUF_TAB%IRTLM,NPT  ,NRTSE ,IEDG4,INTBUF_TAB%ISPT2,
     F  ISPT2_LOC ,INTFRIC    ,INTBUF_TAB%IPARTFRICS,IPARTFRICSI ,
     G  INTBUF_TAB%IPARTFRICM ,IPARTFRICMI,INTNITSCHE,FORNEQS ,FORNEQSI ,
     H  IORTHFRIC,INTBUF_TAB%IREP_FRICM,INTBUF_TAB%DIR_FRICM,IREP_FRICMI,DIR_FRICMI,
     I  IXX3     ,IXX4        , XX1       ,XX2         ,XX3          ,
     3  XX4      ,YY1         ,YY2        ,YY3         ,YY4          ,
     4  ZZ1      ,ZZ2         ,ZZ3        ,ZZ4         ,NINLOADP     ,
     5  DIST     ) 
          CALL I_CORPFIT3(
     1               JLT      ,INTBUF_TAB%STFM  ,INTBUF_TAB%STFNS,STIF      ,NSN    ,
     2               CAND_E_N ,CAND_N_N,NIN     ,IGSTI   ,KMIN     ,
     3               KMAX     ,INACTI  ,IPARI(40,NIN),TNCY ) 

          JLT_NEW = 0
          IF(IMPL_S > 0 ) THEN
	   CALL IMPL_SAV0(
     1        JLT    ,CAND_N_N ,SUBTRIA_OLD ,INTBUF_TAB%IRTLM,NSN  ,NIN  ) 
          END IF!(IMPL_S > 0 ) THEN
C
          CALL I24DST3(
     1 JLT         ,CAND_N_N    ,CAND_E_N    ,CN_LOC      ,CE_LOC      ,
     2 X1          ,X2          ,X3          ,X4          ,Y1          ,
     3 Y2          ,Y3          ,Y4          ,Z1          ,Z2          ,
     4 Z3          ,Z4          ,XI          ,YI          ,ZI          ,
     5 VX1         ,VX2         ,VX3         ,VX4         ,VXI         ,
     6 VY1         ,VY2         ,VY3         ,VY4         ,VYI         ,
     7 VZ1         ,VZ2         ,VZ3         ,VZ4         ,VZI         ,
     8 N1          ,N2          ,N3          ,H1          ,H2          ,
     9 H3          ,H4          ,NIN         ,NSN         ,IX1         ,
     A IX2         ,IX3         ,IX4         ,NSVG        ,STIF        ,
     B JLT_NEW     ,INACTI      ,INTBUF_TAB%MSEGLO,GAPS   ,INTBUF_TAB%GAP_NM,
     C KINI        ,INTBUF_TAB%IRECTM,INTBUF_TAB%IRTLM ,INTBUF_TAB%TIME_S,
     . SUBTRIA,
     D INTTH       ,NSMS        ,PENE        ,XX0         ,YY0         ,
     E ZZ0         ,VX          ,VY          ,VZ          ,IXX         ,
     F INTBUF_TAB%MVOISIN,PMAX_GAP,INTBUF_TAB%SECND_FR,INTBUF_TAB%GAP_M,
     . INTBUF_TAB%PENE_OLD,
     G INTBUF_TAB%STIF_OLD,ITRIV      ,ITAB       ,CAND_T_N    ,IEDGE  ,      
     H NFT         ,PENMIN      ,EPS        ,NM1         ,NM2          ,
     I NM3         ,INTPLY      ,INTBUF_TAB%DGAP_NM ,INTBUF_TAB%ICONT_I,
     J MARGE       ,IEDG4       ,ISPT2_LOC  ,IPARI(40,NIN))

          IF(IEDGE/=0)CALL I24DST3E(
     1 JLT         ,A           ,X           ,CAND_N_N    ,CAND_E_N    ,
     2 INTBUF_TAB%MBINFLG,INTBUF_TAB%ISEADD  ,INTBUF_TAB%ISEDGE,NSVG,NIN,
     3 IXX         ,STIF        ,
     4 JLT_NEW     ,INACTI      ,XI          ,YI          ,ZI          ,
     5 XX0         ,YY0         ,ZZ0         ,PMAX_GAP    ,
     6 FSKYI       ,ISKY        ,CAND_T_N    ,FCONT       ,H3D_DATA    )

          IF(IMPL_S > 0 ) THEN
           CALL IMPL_SAV(
     1                  JLT    ,CAND_N_N ,CAND_E_N ,INTBUF_TAB%IRTLM,STIF ,
     2                  PENE   ,SUBTRIA,NS_IMP ,NE_IMP ,IND_IMP,
     3                  NUM_IMP,JLT_NEW,INTBUF_TAB%NSV,SUBTRIA_OLD,
     4                  INTBUF_TAB%MSEGLO,NSN    ,NIN   ,NRTM ) 
          ELSE
            DO I = 1 ,JLT
             IF(PENE(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
              JLT_NEW = JLT_NEW + 1
              
             END IF
            ENDDO
          ENDIF 
C
          IF(NINLOADP==0.AND.JLT_NEW == 0)CYCLE
          IPARI(29,NIN) = 1
C
C   auxiliaire compute for int + plxfem
C          
          IF(INTPLY > 0) THEN
             CALL I24IPLY_PXFEM(
     1          JLT       ,CAND_E_N   ,INTBUF_TAB%MSEGTYP24     ,IX1     ,IX2 ,
     2          IX3      ,IX4          , PENE, MS_PLY     ,INOD_PXFEM   ,
     3          IPLY     ,ITAB ) 
          ENDIF   

          IF (DEBUG(3)>=1) NB_JLT_NEW = NB_JLT_NEW + JLT_NEW
          IF (IMONM > 0 .AND. JTASK == 1) CALL STARTIME(20,1) 

C-------------------------------------------------------------------------------
C Friction model : computation of friction coefficients based on Material of connected Parts
C-------------------------------------------------------------------------------
        IF(JTASK==1) CALL STARTIME(MACRO_TIMER_FRIC,1)
         JJ = 0
         IF(IORTHFRIC > 0) THEN
           CALL FRICTIONPARTS_MODEL_ORTHO(
     1       INTFRIC       ,JLT           ,IPARTFRICSI   ,IPARTFRICMI  ,ADPARTS_FRIC ,
     2       NSETPRTS      ,TABCOUPLEPARTS_FRIC,NPARTFRIC,TABPARTS_FRIC,TABCOEF_FRIC ,
     3       FRIC          ,VISCF         ,INTBUF_TAB%FRIC_P,FRIC_COEFS, FRICC       ,
     4       VISCFFRIC     ,NTY           ,MFROT         ,IORTHFRIC    , FRIC_COEFS2 ,
     5       FRICC2        ,VISCFFRIC2    ,IFRICORTH     ,NFORTH       , NFISOT      ,
     6       INDEXORTH     ,INDEXISOT     ,JJ            ,IREP_FRICMI  ,DIR_FRICMI   ,        
     7       IXX3          ,IXX4          ,XX1           ,YY1          , ZZ1         , 
     8       XX2           ,YY2           ,ZZ2           ,XX3          , YY3         ,
     9       ZZ3           ,XX4           ,YY4           ,ZZ4          ,CE_LOC       ,
     A       DIR1          ,DIR2          ) 
        ELSE
            NFORTH = 0
            NFISOT = 0
           CALL FRICTIONPARTS_MODEL_ISOT(
     1       INTFRIC       ,JLT           ,IPARTFRICSI   ,IPARTFRICMI  ,ADPARTS_FRIC ,
     2       NSETPRTS      ,TABCOUPLEPARTS_FRIC,NPARTFRIC,TABPARTS_FRIC,TABCOEF_FRIC ,
     3       FRIC          ,VISCF         ,INTBUF_TAB%FRIC_P,FRIC_COEFS, FRICC       ,
     4       VISCFFRIC     ,NTY           ,MFROT         ,IORTHFRIC     ,IFRIC       ,
     5       JJ            , TINT         ,TEMPI         ,NPC           ,TF          ,
     6       TEMP          , H1           ,H2            ,H3            ,H4          ,
     7       IX1          , IX2           ,IX3           ,IX4           ,IFORM       ) 
        ENDIF 
        IF(JTASK==1) CALL STOPTIME(MACRO_TIMER_FRIC,1)      

          CALL I24FOR3(
     1  JLT          ,A         ,V            ,IBC         ,ICODT    ,
     2  FSAV         ,GAP       ,FRIC         ,MS          ,VISC     ,
     3  VISCF        ,NOINT     ,INTBUF_TAB%STFNS,ITAB     ,CN_LOC   ,
     4  STIGLO       ,STIFN     ,STIF         ,FSKYI       ,ISKY     ,
     5  N1           ,N2        ,N3           ,H1          ,H2       ,
     6  H3           ,H4        ,FCONT        ,PENE        ,
     7  IX1          ,IX2       ,IX3          ,IX4         ,NSVG     ,
     8  IVIS2        ,NELTST    ,ITYPTST      ,DT2T        ,SUBTRIA  ,
     9  GAPV         ,INACTI    ,INDEX2(NFT+1),NISKYFI ,
     A  KINET        ,NEWFRONT  ,ISECIN       ,NSTRF       ,SECFCUM  ,
     B  X            ,INTBUF_TAB%IRECTM,CE_LOC    ,MFROT       ,IFQ  ,
     C  INTBUF_TAB%FRIC_P,INTBUF_TAB%SECND_FR,XFILTR_FRIC,
     D  IBAG         ,ICONTACT ,INTBUF_TAB%IRTLM,       
     E  VISCN        ,VXI       ,VYI          ,VZI         ,MSI      ,
     F  KINI         ,NIN       ,NISUB        ,INTBUF_TAB%LISUB,INTBUF_TAB%ADDSUBS,
     G  INTBUF_TAB%ADDSUBM,INTBUF_TAB%LISUBS,INTBUF_TAB%LISUBM,FSAVSUB,
     +                                              INTBUF_TAB%CAND_N,
     H  IPARI(33,NIN),IPARI(39,NIN),FNCONT     ,FTCONT   ,NSN        ,
     I  X1           ,X2        ,X3          ,X4         ,Y1         ,
     J  Y2           ,Y3        ,Y4          ,Z1         ,Z2         ,
     K  Z3           ,Z4        ,XI          ,YI         ,ZI         ,
     L  IADM         ,RCURVI    ,RCONTACT    ,ACONTACT   ,PCONTACT   ,
     M  ANGLMI       ,PADM      ,INTTH       , PHI       , FTHE      ,
     N  FTHESKYI     ,TEMP      , TEMPI      ,RSTIF      , IFORM     ,
     O  MSKYI_SMS    ,ISKYI_SMS ,NSMS        ,CAND_N_N   ,INTBUF_TAB%PENE_OLD,
     P  INTBUF_TAB%STIF_OLD,INTBUF_TAB%MBINFLG,ILEV     ,IGSTI      ,KMIN     ,
     Q  INTPLY       ,IPLY        ,INOD_PXFEM,NM1        ,NM2        ,
     R   NM3         ,NREBOU     ,INTBUF_TAB%IRTSE  ,NSNE    ,INTBUF_TAB%IS2SE   ,
     S  INTBUF_TAB%IS2PT,INTBUF_TAB%MSEGTYP24,JTASK       ,ISENSINT  ,
     U  FSAVPARIT   ,NFT        ,H3D_DATA    ,FRICC       ,VISCFFRIC ,
     V  FRIC_COEFS  ,T2MAIN_SMS ,INTNITSCHE ,FORNEQSI   ,IORTHFRIC ,
     W  FRIC_COEFS2 ,FRICC2      ,VISCFFRIC2  ,NFORTH     ,NFISOT    ,
     X  INDEXORTH   ,INDEXISOT   ,DIR1        ,DIR2       ,T2FAC_SMS ,F_PFIT,
     Y  TAGNCONT    ,KLOADPINTER ,LOADPINTER  ,LOADP_HYD_INTER,
     Z  INTBUF_TAB%TYPSUB,INTBUF_TAB%INFLG_SUBS,INTBUF_TAB%INFLG_SUBM,
     .                                      NINLOADP,DGAPLOADINT,
     1  S_LOADPINTER, DIST       ,IXX         ,INTEREFRIC   ) 
C
          IF(IMPL_S > 0 ) 
     +     CALL IMPL_SAV1(
     1                  JLT    ,CAND_N_N ,CAND_E_N ,INTBUF_TAB%IRTLM,STIF ,
     2                  PENE   ,SUBTRIA,INTBUF_TAB_IMP(NIN)%CAND_N,
     +                  INTBUF_TAB_IMP(NIN)%CAND_E,INTBUF_TAB_IMP(NIN)%INDSUBT,
     3                  INTBUF_TAB_IMP(NIN)%I_STOK(1),
     2                  N1     ,N2     ,N3     ,H1     ,H2     ,H3     ,
     4                  H4     ,INTBUF_TAB_IMP(NIN)%NJ,INTBUF_TAB_IMP(NIN)%HJ    ,
     5                  INTBUF_TAB_IMP(NIN)%STIF,NIN   ,NSN     ) 
          IF (IMONM > 0 .AND. JTASK == 1) CALL STOPTIME(20,1)

      ENDDO
c
        IF (SFSAVPARIT /= 0)THEN
            CALL SUM_6_FLOAT_SENS(FSAVPARIT, NISUB+1, 11, I_STOK,1,I_STOK,
     .                            FBSAV6, 12, 6, DIMFB, ISENSINT )
        ENDIF
        IF (ALLOCATED(FSAVPARIT)) DEALLOCATE (FSAVPARIT)
c
#include "lockon.inc"
      INTBUF_TAB%VARIABLES(23) = MAX(PMAX_GAP,INTBUF_TAB%VARIABLES(23))
#include "lockoff.inc"
C        
      IF(INTPLY > 0) THEN  
          CALL I24GAP_PXFEM(
     1    NRTM           ,INTBUF_TAB%IRECTM,INTBUF_TAB%IRTLM ,INTBUF_TAB%GAP_N0 ,
     2    INTBUF_TAB%MVOISIN ,INTBUF_TAB%NVOISIN,INTBUF_TAB%MSEGTYP24 ,INOD_PXFEM    ,
     3    X             ,MS_PLY        ,WAGAP        ,ITAB         ,
     4    INTBUF_TAB%ISEG_PXFEM ,INTBUF_TAB%ISEG_PLY,INTBUF_TAB%STFM) 
      ENDIF  !intply 
C

ccc      IF(NSNE > 0) CALL MY_BARRIER()
      DEALLOCATE(INDEX2)
      RETURN
      END
Chd|====================================================================
Chd|  IMPL_SAV                      source/interfaces/int24/i24main.F
Chd|-- called by -----------
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE IMPL_SAV(
     1                  JLT    ,CAND_N ,CAND_E ,IRTLM  ,STIF   ,
     2                  PENE   ,SUBTRIA,NS_IMP ,NE_IMP ,IND_IMP,
     3                  NUM_IMP,JLT_NEW,NSV    ,SUBTRIA_OLD,
     4                  MSEGLO ,NSN    ,NIN    ,NRTM  ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,JLT_NEW,NSV(*),SUBTRIA_OLD(*),NSN,NIN,NRTM
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*),IND_IMP(*),
     .        CAND_E(*),CAND_N(*),SUBTRIA(*),IRTLM(2,*),MSEGLO(*)
C     REAL
      my_real
     .   STIF(*), PENE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NS,NE,ITQ,NEG,J
C----------------------------------------------------
            DO I = 1 ,JLT
             IF(PENE(I)/=ZERO.AND.STIF(I)/=ZERO)THEN             
              JLT_NEW = JLT_NEW + 1
              NS=CAND_N(I)
              NE=CAND_E(I)
              ITQ=SUBTRIA_OLD(I)
C---------due tiny penetration removed to Engine---              
              IF (TT==ZERO.AND.ITQ==0) ITQ=1
C----sliding on the neibour : to verify if IRTLM_FI is updated already            
              IF (SUBTRIA(I)>4) THEN
C-----------case sliding different to i24dist3               
               NE = -CAND_E(I)
               ITQ=SUBTRIA(I)
              END IF
              NS_IMP(JLT_NEW+NUM_IMP)=NS
              NE_IMP(JLT_NEW+NUM_IMP)=NE
              IND_IMP(JLT_NEW+NUM_IMP)=ITQ
             END IF
            ENDDO
            NUM_IMP=NUM_IMP+JLT_NEW
C
      RETURN
      END
Chd|====================================================================
Chd|  IMPL_SAV0                     source/interfaces/int24/i24main.F
Chd|-- called by -----------
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE IMPL_SAV0(
     1                  JLT    ,CAND_N ,SUBTRIA_OLD ,IRTLM  ,NSN ,NIN) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NSN,NIN
      INTEGER CAND_N(*),SUBTRIA_OLD(*),IRTLM(2,*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NS,NE,ITQ
C----------------------------------------------------
            DO I = 1 ,JLT
              NS=CAND_N(I)
              IF(NS <= NSN)THEN
               SUBTRIA_OLD(I)=IRTLM(2,NS)
              ELSE
               SUBTRIA_OLD(I)=IRTLM_FI(NIN)%P(2,NS-NSN)
              ENDIF
            ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I24CDCOR3                     source/interfaces/int24/i24main.F
Chd|-- called by -----------
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24CDCOR3(JLT,INDEX,CAND_E,CAND_N,CAND_T,
     .                    CAND_E_N,CAND_N_N,CAND_T_N,IEDGE)      
C============================================================================
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, IEDGE,
     .        INDEX(*), CAND_E(*), CAND_N(*), CAND_T(*),
     .        CAND_E_N(*), CAND_N_N(*), CAND_T_N(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I 
C-----------------------------------------------
C
      DO I=1,JLT
          CAND_E_N(I) = CAND_E(INDEX(I))
          CAND_N_N(I) = CAND_N(INDEX(I))
      ENDDO

      IF(IEDGE/=0)THEN
        DO I=1,JLT
          CAND_T_N(I) = CAND_T(INDEX(I))
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  IMPL_SAV1                     source/interfaces/int24/i24main.F
Chd|-- called by -----------
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE IMPL_SAV1(
     1                  JLT    ,CAND_N ,CAND_E ,IRTLM  ,STIF   ,
     2                  PENE   ,SUBTRIA,NS_IMP ,NE_IMP ,IND_IMP,
     3                  II_STOK,N1     ,N2     ,N3     ,H1     ,
     4                  H2     ,H3     ,H4      ,NJ_IMP ,HJ_IMP,
     5                  STIF_IMP,NIN   ,NSN     ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,II_STOK,NIN   ,NSN
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*),IND_IMP(*),
     .        CAND_E(*),CAND_N(*),SUBTRIA(*),IRTLM(2,*)
C     REAL
      my_real
     .   STIF(*), PENE(*),N1(*),N2(*),N3(*),H1(*),H2(*),H3(*),
     .   H4(*),STIF_IMP(*), NJ_IMP(3,*),HJ_IMP(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NS,NE,ITQ,NEG,J,K_STOK,J_STOK,IC(4,4)
      DATA IC / 
     1    3, 4, 1, 2, 
     2    4, 1, 2, 3,
     3    1, 2, 3, 4,
     4    2, 3, 4, 1/
C----------------------------------------------------
      K_STOK = 0
      DO I=1,JLT
         IF(PENE(I)/=ZERO.AND.STIF(I)/=ZERO) K_STOK = K_STOK + 1
      ENDDO
      IF(K_STOK==0)RETURN
C
#include "lockon.inc"
      J_STOK = II_STOK
      II_STOK   = J_STOK + K_STOK
#include "lockoff.inc"
            DO I = 1 ,JLT
             IF(PENE(I)/=ZERO.AND.STIF(I)/=ZERO)THEN             
              J_STOK = J_STOK + 1
              NS=CAND_N(I)
              NE=CAND_E(I)
              ITQ=SUBTRIA(I)
C---------due tiny penetration removed to Engine---              
              IF (TT==ZERO.AND.ITQ==0) ITQ=1
C----sliding on the neibour : to verify if necessary            
              IF (ITQ>4) NE = -CAND_E(I)
              NS_IMP(J_STOK)=NS
              NE_IMP(J_STOK)=NE
              IND_IMP(J_STOK)=ITQ
              NJ_IMP(1,J_STOK) = N1(I)
              NJ_IMP(2,J_STOK) = N2(I)
              NJ_IMP(3,J_STOK) = N3(I)
              STIF_IMP(J_STOK) =STIF(I)
C-------order of HJ should be consisting w/ IXj------            
              IF (ITQ>4) THEN
               HJ_IMP(1,J_STOK) = H1(I)
               HJ_IMP(2,J_STOK) = H2(I)
               HJ_IMP(3,J_STOK) = H3(I)
               HJ_IMP(4,J_STOK) = H4(I)
              ELSE
               HJ_IMP(IC(1,ITQ),J_STOK) = H1(I)
               HJ_IMP(IC(2,ITQ),J_STOK) = H2(I)
               HJ_IMP(IC(3,ITQ),J_STOK) = H3(I)
               HJ_IMP(IC(4,ITQ),J_STOK) = H4(I)
              ENDIF
             END IF
            ENDDO
C
      RETURN
      END

