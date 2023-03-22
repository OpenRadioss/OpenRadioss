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
Chd|  I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- called by -----------
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FRICTIONPARTS_MODEL_ISOT      source/interfaces/int07/frictionparts_model.F
Chd|        FRICTIONPARTS_MODEL_ORTHO     source/interfaces/int07/frictionparts_model.F
Chd|        I25ASS3                       source/interfaces/int25/i25ass3.F
Chd|        I25CDCOR3                     source/interfaces/int25/i25mainf.F
Chd|        I25CDCOR3_E2S                 source/interfaces/int25/i25mainf.F
Chd|        I25COR3E                      source/interfaces/int25/i25cor3e.F
Chd|        I25COR3_3                     source/interfaces/int25/i25cor3.F
Chd|        I25COR3_E2S                   source/interfaces/int25/i25cor3_e2s.F
Chd|        I25DST3E                      source/interfaces/int25/i25dst3e.F
Chd|        I25DST3_3                     source/interfaces/int25/i25dst3_3.F
Chd|        I25DST3_E2S                   source/interfaces/int25/i25dst3_e2s.F
Chd|        I25FOR3                       source/interfaces/int25/i25for3.F
Chd|        I25FOR3E                      source/interfaces/int25/i25for3e.F
Chd|        I25FOR3_E2S                   source/interfaces/int25/i25for3_e2s.F
Chd|        I25KEEPF                      source/interfaces/int25/i25slid.F
Chd|        I25THERM                      source/interfaces/int25/i25therm.F
Chd|        I_CORPFIT3                    source/interfaces/int24/i24cor3.F
Chd|        I_COR_EPFIT3                  source/interfaces/int24/i24cor3.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SUM_6_FLOAT_SENS              source/system/parit.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25MAINF(
     1                   IPARI  ,INTBUF_TAB         ,X        ,A      ,
     2                   ICODT  ,FSAV    ,V         ,MS       ,DT2T   ,
     3                   NELTST ,ITYPTST ,ITAB      ,STIFN    ,FSKYI  ,
     4                   ISKY   ,FCONT   ,NIN       ,LINDMAX  ,KINET  ,
     5                   JTASK  ,NB_IMPCT,
     6                   NISKYFI,NEWFRONT,NSTRF     ,SECFCUM  ,ICONTACT,
     7                   VISCN  ,NUM_IMP,
     9                   NS_IMP ,NE_IMP  ,IND_IMP   ,FSAVSUB  ,NRTMDIM,
     A                   FSAVBAG,
     B                   EMINX  ,IXS     ,IXS16     ,IXS20    ,FNCONT ,
     C                   FTCONT ,IAD_ELEM,FR_ELEM   ,RCONTACT ,ACONTACT,
     D                   PCONTACT,TEMP      ,FTHE     ,FTHESKYI,
     E                   PM      ,IPARG ,IAD17   ,MSKYI_SMS  ,ISKYI_SMS,
     F                   NODNX_SMS,MS0  ,INOD_PXFEM,MS_PLY    ,WAGAP  ,
     G                   FBSAV6   ,ISENSINT,
     H                   DIMFB     ,H3D_DATA,INTBUF_FRIC_TAB  ,NISKYFIE,
     I                   APINCH    ,STIFPINCH,NPC   ,TF       ,CONDN   ,
     J                   CONDNSKYI ,QFRICINT,TAGNCONT,KLOADPINTER,LOADPINTER,
     K                   LOADP_HYD_INTER,DGAPLOADINT,S_LOADPINTER,INTEREFRIC,
     .                                                            INTERFACES)
C=======================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE TRI7BOX
      USE H3D_MOD
      USE INTBUF_FRIC_MOD
      USE MESSAGE_MOD
      USE TRI25EBOX
      USE OUTPUTS_MOD
      USE INTERFACES_MOD
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
#include      "assert.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "macro.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST,NIN,NEWFRONT,
     .        NSTRF(*),
     .        NRTMDIM, IAD17, IPARSENS
      INTEGER IPARI(NPARI,NINTER), ICODT(*),ICONTACT(*),
     .        ITAB(*), ISKY(*), KINET(*), 
     .        IPARG(NPARG,*),INOD_PXFEM(*),TAGNCONT(NLOADP_HYD_INTER,NUMNOD)
      INTEGER NB_IMPCT,JTASK,
     .        NISKYFI, LINDMAX, NISKYFIE
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*),IND_IMP(*)
      INTEGER IXS(*)  ,IXS16(*) ,IXS20(*)
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), NPC(*),
     .        ISKYI_SMS(*), NODNX_SMS(*), ISENSINT(*),DIMFB
      INTEGER  , INTENT(IN) :: S_LOADPINTER
      INTEGER  , INTENT(IN) :: KLOADPINTER(NINTER+1),LOADPINTER(S_LOADPINTER),
     .         LOADP_HYD_INTER(NLOADP_HYD)
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
     .   PCONTACT(*),
     .   TEMP(*),FTHE(*),FTHESKYI(LSKYI),PM(NPROPM,*),
     .   MSKYI_SMS(*),MS_PLY(*),WAGAP(*),
     .   APINCH(3,*),STIFPINCH(*),QFRICINT(*),TF(*),CONDN(*),
     .   CONDNSKYI(LSKYI)
      DOUBLE PRECISION FBSAV6(12,6,DIMFB)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(INTBUF_FRIC_STRUCT_), TARGET, DIMENSION(NINTERFRIC) ::  INTBUF_FRIC_TAB
      TYPE (INTERFACES_) ,INTENT(IN):: INTERFACES
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JD(50),KD(50), JFI, KFI, IEDGE, ISHARP, NEDGE, 
     .        I, J, L, H, I_STOK, JLT_NEW, JLT , NFT, IVIS2,
     .        IBC, NOINT, NSEG, ISECIN, IBAG, IADM,
     .        IGAP, INACTI, IFQ, MFROT, IGSTI, NISUB, IGAP0,
     .        NB_LOC, I_STOK_LOC,DEBUT,
     .        ILAGM, LENR, INTTH,IFORM,INTPLY,
     .        NADMSR, I_STOK_GLO, MGLOB, MG, N, NSNR, NN, IERROR,
     .        IE, I1, I2, IORTHFRIC ,NFORTH ,NFISOT ,JJ,FCOND,IKTHE,IFRIC
      INTEGER  LENT, MAXCC
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), CN_LOC(MVSIZ),CE_LOC(MVSIZ),
     .        CAND_N_N(MVSIZ),CAND_E_N(MVSIZ),
     .        KINI(MVSIZ),
     .        ISDSIZ(NSPMD+1),IRCSIZ(NSPMD+1),
     .        IELESI(MVSIZ), NSMS(MVSIZ), SUBTRIA(MVSIZ),
     .        NSNFT, NSNLT, NSNRFT, NSNRLT, INTFRIC,NSETPRTS ,NPARTFRIC,
     .        IPARTFRICSI(MVSIZ), IPARTFRICMI(MVSIZ), IFADHI(MVSIZ),
     .        MVOISN(MVSIZ,4),IBOUND(4,MVSIZ),INDEXISOT(MVSIZ),INDEXORTH(MVSIZ),
     .        IREP_FRICMI(MVSIZ),IPARTFRIC_ES(4*MVSIZ),IPARTFRIC_EM(4*MVSIZ),
     .        IELEMI(MVSIZ)
      INTEGER ::  EDGE_ID(2,4*MVSIZ)
      INTEGER 
     .        NE1(MVSIZ), NE2(MVSIZ), ME1(MVSIZ), ME2(MVSIZ), 
     .        CS_LOC(MVSIZ), CM_LOC(MVSIZ), 
     .        NS1(4*MVSIZ), NS2(4*MVSIZ), M1(4*MVSIZ), M2(4*MVSIZ), INDX1(4*MVSIZ), INDX2(4*MVSIZ),
     .        NSMSE(4*MVSIZ), CS_LOC4(4*MVSIZ), CM_LOC4(4*MVSIZ),
     .        TYPEDGS(MVSIZ),
     .        IAM(MVSIZ),JAM(MVSIZ),IBM(MVSIZ),JBM(MVSIZ),     
     .        IAS(MVSIZ),JAS(MVSIZ),IBS(MVSIZ),JBS(MVSIZ)

      INTEGER, ALLOCATABLE, DIMENSION(:) :: INDEX2
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT, PMAX_GAP,
     .   VISC,VISCF,STIGLO,GAPMIN,
     .   KMIN, KMAX, GAPMAX,KTHE,TINT,RHOH,EPS,
     .   VISCFLUID, SIGMAXADH, VISCADHFACT,
     .   FHEATS,FHEATM,XTHE,FRAD,DRAD,DCOND
C debug
      integer :: eidm,eids
C 
C-----------------------------------------------
C     REAL
      my_real
     .   XX(MVSIZ,5), YY(MVSIZ,5), ZZ(MVSIZ,5), 
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   NNX(MVSIZ,5), NNY(MVSIZ,5), NNZ(MVSIZ,5),
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ), 
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), 
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ), 
     .   VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), 
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   MSI(MVSIZ),
     .   NM1(MVSIZ), NM2(MVSIZ), NM3(MVSIZ), 
     .   TEMPI(MVSIZ),PHI(MVSIZ),AREASI(MVSIZ),
     .   LB(MVSIZ), LC(MVSIZ), 
     .   GAP_NM(4,MVSIZ), GAPS(MVSIZ), GAPMXL(MVSIZ),
     .   GAPV(MVSIZ), BASE_ADH(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   PHI1(MVSIZ), PHI2(MVSIZ),PHI3(MVSIZ),PHI4(MVSIZ) ,
     .   CONDINT(MVSIZ) ,EFRICT(MVSIZ)
      my_real
     .   GAPVE(4*MVSIZ), STIFE(4*MVSIZ), NX(4*MVSIZ), NY(4*MVSIZ), NZ(4*MVSIZ),
     .   HS1(4*MVSIZ), HS2(4*MVSIZ), HM1(4*MVSIZ), HM2(4*MVSIZ),
     .   XXS1(4*MVSIZ), XXS2(4*MVSIZ), XYS1(4*MVSIZ), XYS2(4*MVSIZ),
     .   XZS1(4*MVSIZ), XZS2(4*MVSIZ), XXM1(4*MVSIZ), XXM2(4*MVSIZ),
     .   XYM1(4*MVSIZ), XYM2(4*MVSIZ), XZM1(4*MVSIZ), XZM2(4*MVSIZ),
     .   VXS1(4*MVSIZ), VXS2(4*MVSIZ), VYS1(4*MVSIZ), VYS2(4*MVSIZ),
     .   VZS1(4*MVSIZ), VZS2(4*MVSIZ), VXM1(4*MVSIZ), VXM2(4*MVSIZ),
     .   VYM1(4*MVSIZ), VYM2(4*MVSIZ), VZM1(4*MVSIZ), VZM2(4*MVSIZ),
     .   MS1(4*MVSIZ),  MS2(4*MVSIZ),  MM1(4*MVSIZ),  MM2(4*MVSIZ),
     .   EX(4*MVSIZ),  EY(4*MVSIZ), EZ(4*MVSIZ), FX(MVSIZ), FY(MVSIZ), 
     .   FZ(MVSIZ)  , DIST(MVSIZ),
     .   NORMALN1(3,MVSIZ) ,NORMALN2(3,MVSIZ) ,NORMALM1(3,4,MVSIZ),NORMALM2(3,4,MVSIZ) 

      my_real
     .     , DIMENSION(:,:,:), ALLOCATABLE :: FSAVPARIT
      my_real
     .     RCURVI(MVSIZ), ANGLMI(MVSIZ), ANGLT, PADM,PENMIN,MARGE
      INTEGER NRTMFT, NRTMLT, NMNFT, NMNLT, NRADM, IS, IM, ISTIF_MSDT
      INTEGER ICURV,ILEV,NREBOU,NPT ,NRTSE,IEDG4,SFSAVPARIT,NCY_PFIT,NINLOADP
      my_real
     .    XFILTR_FRIC,FRIC_COEFS(MVSIZ,10),VISCFFRIC(MVSIZ),FRICC(MVSIZ),
     .    FRIC_COEFS2(MVSIZ,10),VISCFFRIC2(MVSIZ),FRICC2(MVSIZ),
     .    DIR1(MVSIZ,3),DIR2(MVSIZ,3),DIR_FRICMI(MVSIZ,2),FRICC_E(4*MVSIZ),
     .    VISCFFRIC_E(4*MVSIZ),TNCY,T_PFIT,FINC,DGAPLOADPMAX,DTSTIF

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

      INTEGER :: NEDGE_REM,NRTM,NSN,NTY
C=======================================================================
C
      NRTM   =IPARI(4,NIN)
      NSN   =IPARI(5,NIN)
      NSNR  =IPARI(24,NIN)
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
      IGAP0 =IPARI(53,NIN)
      IEDGE =IPARI(58,NIN)
      NADMSR=IPARI(67,NIN)
      ISHARP=IPARI(84,NIN)
      NEDGE =IPARI(68,NIN)
      NEDGE_REM = IPARI(69,NIN)
C     WRITE(6,*) "NEDGE REMOTE=", IPARI(69,NIN)
C adaptive meshing
      IADM =IPARI(44,NIN) 
      NRADM=IPARI(49,NIN) 
      PADM =INTBUF_TAB%VARIABLES(24)
      ANGLT=INTBUF_TAB%VARIABLES(25)
      MARGE=INTBUF_TAB%VARIABLES(25)
C heat interface
      INTTH = IPARI(47,NIN)
      IKTHE = IPARI(92,NIN)
      IFORM = IPARI(48,NIN)
      INTPLY = IPARI(66,NIN)
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
      KTHE    = INTBUF_TAB%VARIABLES(20) 
      FHEATS  = INTBUF_TAB%VARIABLES(21) 
      TINT    = INTBUF_TAB%VARIABLES(22)
      FHEATM  = INTBUF_TAB%VARIABLES(41)
      XTHE  =INTBUF_TAB%VARIABLES(33)
      FRAD = INTBUF_TAB%VARIABLES(31)
      DRAD = INTBUF_TAB%VARIABLES(32)
      FCOND = IPARI(93,NIN) ! function of variation of heat exchange as funct of distance
      DCOND =  INTBUF_TAB%VARIABLES(34)  ! max conduction distance
      IFRIC = 0
      IF(INTTH > 0) IFRIC =IPARI(50,NIN)
C
      PENMIN  = INTBUF_TAB%VARIABLES(38)
      EPS     = INTBUF_TAB%VARIABLES(39)
C 
      VISCFLUID = INTBUF_TAB%VARIABLES(42)
      SIGMAXADH = INTBUF_TAB%VARIABLES(43)
      VISCADHFACT  = INTBUF_TAB%VARIABLES(44)
C
      PMAX_GAP = ZERO
C
      ISTIF_MSDT =IPARI(97,NIN)
      DTSTIF = INTBUF_TAB%VARIABLES(48)
C
      ILEV  = IPARI(20,NIN)
      NRTSE = IPARI(52,NIN)
      ALLOCATE(INDEX2(LINDMAX))
C--- Corresponding Friction model  
      INTFRIC=IPARI(72,NIN)
      IORTHFRIC = 0
      NSETPRTS = 0
      XFILTR_FRIC = ZERO
      NPARTFRIC = 0
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
c         MFROT    =   INTBUF_FRIC_TAB(INTFRIC)%FRICMOD ! These Flags are already put in Ipari
c         IFQ      =   INTBUF_FRIC_TAB(INTFRIC)%IFFILTER
      ELSE
         TABCOUPLEPARTS_FRIC  => TABCOUPLEPARTS_FRIC_BID
         TABPARTS_FRIC  => TABPARTS_FRIC_BID
         TABCOEF_FRIC  => TABCOEF_FRIC_BID 
         ADPARTS_FRIC   => ADPARTS_FRIC_BID
         IFRICORTH   => IFRICORTH_BID
         IF (IFQ/=0) XFILTR_FRIC = INTBUF_TAB%XFILTR(1)
      ENDIF
      EFRICT = ZERO
C
      NINLOADP = IPARI(95,NIN) ! load pressure related to inter
      DGAPLOADPMAX = INTBUF_TAB%VARIABLES(46)
C
c----------------------------------------------------
c   Rayon de courbure : calcul des normales nodales (normees)
C   IADM!=0 + Icurv!=0 non available (starter error).
c----------------------------------------------------
      IF(IADM/=0)THEN
      END IF!(IADM/=0)
C-----Press-fit 
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
	   IF (INACTI/=-1) IPARI(40,NIN) = 0 
C----------------------------------------------------------------------
C     Secnd node previously impacted & Secnd node is leaving the contact
C----------------------------------------------------------------------
      NSNFT= 1+(JTASK-1)*NSN/ NTHREAD
      NSNLT= JTASK*NSN/NTHREAD

      NSNRFT= 1+(JTASK-1)*NSNR/ NTHREAD
      NSNRLT= JTASK*NSNR/NTHREAD

      IF(IVIS2/=-1) THEN
C
        DO N=NSNFT, NSNLT
c          if(itab(intbuf_tab%NSV(n))==27324)
c     .     print *,'natif',ispmd+1,INTBUF_TAB%IRTLM(N),INTBUF_TAB%TIME_S(N)
          IF(INTBUF_TAB%IRTLM(4*(N-1)+1) > 0 .AND. (INTBUF_TAB%TIME_S(2*(N-1)+1) == EP20 .OR.
     .                                           (INTBUF_TAB%IRTLM(4*(N-1)+2) < 0.AND.MOD(-INTBUF_TAB%IRTLM(4*(N-1)+2),5)==0)) )THEN
C
C           No more contact (Reset Irtlm & PENE_OLD)
            INTBUF_TAB%IRTLM(4*(N-1)+1)=0
            INTBUF_TAB%IRTLM(4*(N-1)+2)=0
            INTBUF_TAB%IRTLM(4*(N-1)+3)=0
            INTBUF_TAB%IRTLM(4*(N-1)+4)=0
C
            INTBUF_TAB%SECND_FR(6*(N-1)+1:6*N) = ZERO
            INTBUF_TAB%STIF_OLD(2*(N-1)+1:2*N)= ZERO
            INTBUF_TAB%PENE_OLD(5*(N-1)+1:5*N)= ZERO
C       
          END IF    
        END DO

        DO N=NSNRFT, NSNRLT
c          if(itafi(nin)%p(n)==29482)
c     .     print *,'remote',ispmd+1,IRTLM_FI(NIN)%P(1,N),TIME_SFI(NIN)%P(N)
          IF(IRTLM_FI(NIN)%P(1,N) > 0 .AND. (TIME_SFI(NIN)%P(2*(N-1)+1) == EP20 .OR.
     .                                    (IRTLM_FI(NIN)%P(2,N) < 0.AND.MOD(-IRTLM_FI(NIN)%P(2,N),5)==0)) )THEN
C
C           No more contact (Reset Irtlm & PENE_OLD)
            IRTLM_FI(NIN)%P(1,N)=0
            IRTLM_FI(NIN)%P(2,N)=0
            IRTLM_FI(NIN)%P(3,N)=0
            IRTLM_FI(NIN)%P(4,N)=0
C
            SECND_FRFI(NIN)%P (1:6,N)=ZERO
            PENE_OLDFI(NIN)%P(1:5,N)=ZERO
            STIF_OLDFI(NIN)%P(1:2,N)=ZERO
C     
          END IF    
        END DO
      ELSE ! IVIS2 == -1
        DO N=NSNFT, NSNLT
c          if(itab(intbuf_tab%NSV(n))==27324)
c     .     print *,'natif',ispmd+1,INTBUF_TAB%IRTLM(N),INTBUF_TAB%TIME_S(N)
          IF(INTBUF_TAB%IRTLM(4*(N-1)+1) > 0 .AND. (INTBUF_TAB%TIME_S(2*(N-1)+1) == EP20 .OR.
     .                                           (INTBUF_TAB%IRTLM(4*(N-1)+2) < 0.AND.MOD(-INTBUF_TAB%IRTLM(4*(N-1)+2),5)==0)) )THEN
C
C           No more contact (Reset Irtlm & PENE_OLD)
            INTBUF_TAB%IRTLM(4*(N-1)+1)=0
            INTBUF_TAB%IRTLM(4*(N-1)+2)=0
            INTBUF_TAB%IRTLM(4*(N-1)+3)=0
            INTBUF_TAB%IRTLM(4*(N-1)+4)=0
C
            INTBUF_TAB%SECND_FR(6*(N-1)+1:6*N) = ZERO
            INTBUF_TAB%STIF_OLD(2*(N-1)+1:2*N)= ZERO
            INTBUF_TAB%PENE_OLD(5*(N-1)+1:5*N)= ZERO
C       
            INTBUF_TAB%IF_ADH(N) = 0
          END IF    
        END DO

        DO N=NSNRFT, NSNRLT
c          if(itafi(nin)%p(n)==29482)
c     .     print *,'remote',ispmd+1,IRTLM_FI(NIN)%P(1,N),TIME_SFI(NIN)%P(N)
          IF(IRTLM_FI(NIN)%P(1,N) > 0 .AND. (TIME_SFI(NIN)%P(2*(N-1)+1) == EP20 .OR.
     .                                    (IRTLM_FI(NIN)%P(2,N) < 0.AND.MOD(-IRTLM_FI(NIN)%P(2,N),5)==0)) )THEN
C
C           No more contact (Reset Irtlm & PENE_OLD)
            IRTLM_FI(NIN)%P(1,N)=0
            IRTLM_FI(NIN)%P(2,N)=0
            IRTLM_FI(NIN)%P(3,N)=0
            IRTLM_FI(NIN)%P(4,N)=0
C
            SECND_FRFI(NIN)%P (1:6,N)=ZERO
            PENE_OLDFI(NIN)%P(1:5,N)=ZERO
            STIF_OLDFI(NIN)%P(1:2,N)=ZERO
C
            IF_ADHFI(NIN)%P(N) = 0               
          END IF    
        END DO
      ENDIF       

C-----------------------------------------------------------------------
      CALL MY_BARRIER
C-----------------------------------------------------------------------
C       Tag true impacts vs forces (CAND_N = -CAND_N)
C-----------------------------------------------------------------------
      I_STOK_GLO = INTBUF_TAB%I_STOK(2)
C
      NB_LOC = I_STOK_GLO / NTHREAD
      IF (JTASK==NTHREAD) THEN
        I_STOK_LOC = I_STOK_GLO-NB_LOC*(NTHREAD-1)
      ELSE
        I_STOK_LOC = NB_LOC
      ENDIF
      DEBUT = (JTASK-1)*NB_LOC

      I_STOK=0
      DO I = DEBUT+1, DEBUT+I_STOK_LOC
        IF(INTBUF_TAB%CAND_OPT_N(I)>0) THEN
          I_STOK = I_STOK + 1
          INDEX2(I_STOK) = I
        ENDIF
      END DO
C
C     filtrer => ne garder que les contacts vrais 
      CALL I25KEEPF(
     1  I_STOK   ,INDEX2 ,INTBUF_TAB%CAND_OPT_N,INTBUF_TAB%CAND_OPT_E,NIN    ,
     2  NSN      ,NSNR   ,INACTI ,INTBUF_TAB%MSEGLO ,INTBUF_TAB%IRTLM ,
     3  INTBUF_TAB%PENM ,INTBUF_TAB%PENE_OLD ,JTASK ,ITAB,
     4  INTBUF_TAB%NSV  ,INTBUF_TAB%SECND_FR,INTBUF_TAB%TIME_S,
     .  INTBUF_TAB%STIF_OLD) 
C
      CALL MY_BARRIER
C
C-----------------------------------------------------------------------
C
C (re)decoupage statique
C
      I_STOK_GLO = INTBUF_TAB%I_STOK(2)
C
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
        IF(INTBUF_TAB%CAND_OPT_N(I)>0) THEN
          I_STOK = I_STOK + 1
          INDEX2(I_STOK) = I
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
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
     .         C1='(/INTER/TYPE25)')
         CALL ARRET(2)
        ENDIF
        FSAVPARIT(1:NISUB+1,1:11,1:I_STOK) = ZERO
      ELSE
        ALLOCATE(FSAVPARIT(0,0,0),STAT=IERROR)
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .         C1='(/INTER/TYPE25)')
         CALL ARRET(2)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C     Forces computation
C-----------------------------------------------------------------------
      DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C preparation candidats retenus
          CALL I25CDCOR3(
     1     JLT,INDEX2(NFT+1),INTBUF_TAB%CAND_OPT_E,INTBUF_TAB%CAND_OPT_N,
     2     CAND_E_N,CAND_N_N )
C cand_n et cand_e remplace par cand_n_n et cand_e_n
C         Extraction of global data to local arrays
          CALL I25COR3_3(
     1  JLT       ,X          ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV  ,CAND_E_N  ,
     2  CAND_N_N  ,INTBUF_TAB%STFM  ,INTBUF_TAB%STFNS,STIF       ,
     .                                                 INTBUF_TAB%EDGE_BISECTOR,
     3  IGSTI     ,KMIN       ,KMAX             ,MS             ,MSI        ,
     3  XI         ,YI        ,ZI           ,VXI          ,VYI       ,
     4  VZI        ,IX1       ,IX2          ,IX3          ,IX4       ,
     5  NSVG       ,NSN       ,V            ,KINET        ,KINI      ,
     6  NIN        ,INTBUF_TAB%ADMSR ,INTBUF_TAB%IRTLM,SUBTRIA ,
     7  XX        ,YY         ,ZZ        ,INTBUF_TAB%LBOUND,IBOUND   ,
     8  NNX       ,NNY        ,NNZ       ,
     9  VX1       ,VX2        ,VX3       ,VX4          ,
     A  VY1       ,VY2        ,VY3       ,VY4          ,
     B  VZ1       ,VZ2        ,VZ3       ,VZ4          ,
     C  NODNX_SMS ,NSMS       ,INDEX2(NFT+1),INTBUF_TAB%PENM,INTBUF_TAB%LBM, 
     D  INTBUF_TAB%LCM,PENE   ,LB        , LC          ,
     E  INTBUF_TAB%GAP_NM     ,GAP_NM     ,INTBUF_TAB%GAP_S,GAPS,IGAP ,
     F  INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML,GAPMXL,INTFRIC,INTBUF_TAB%IPARTFRICS,
     G  IPARTFRICSI,INTBUF_TAB%IPARTFRICM,IPARTFRICMI,INTBUF_TAB%AREAS,AREASI,
     H  IVIS2      ,INTBUF_TAB%MVOISIN,MVOISN,IORTHFRIC,INTBUF_TAB%IREP_FRICM,
     I  INTBUF_TAB%DIR_FRICM ,IREP_FRICMI ,DIR_FRICMI  ,X1      ,Y1          , 
     J  Z1         ,X2       ,Y2          ,Z2          ,X3                   ,
     K  Y3         ,Z3       ,X4          ,Y4          ,Z4      ,
     L  INTTH      ,TEMP     ,TEMPI       ,INTBUF_TAB%IELES     ,IELESI      ,
     M  INTBUF_TAB%IELEM,IELEMI,ISTIF_MSDT,DTSTIF      ,INTBUF_TAB%STIFMSDT_S,
     N  INTBUF_TAB%STIFMSDT_M,NRTM        ,INTERFACES%PARAMETERS)
          CALL I_CORPFIT3(
     1               JLT      ,INTBUF_TAB%STFM  ,INTBUF_TAB%STFNS,STIF      ,NSN    ,
     2               CAND_E_N ,CAND_N_N,NIN     ,IGSTI   ,KMIN     ,
     3               KMAX     ,INACTI  ,IPARI(40,NIN),TNCY ) 
C
          JLT_NEW = 0
C
          CALL I25DST3_3(
     1 JLT         ,CAND_N_N    ,CAND_E_N    ,CN_LOC      ,CE_LOC      ,
     2 INTBUF_TAB%IRTLM,XX      ,YY          ,ZZ          ,GAP_NM      ,
     3 XI          ,YI         ,ZI           ,GAPS        ,GAPMXL      ,
     4 ISHARP      ,NNX        ,NNY          ,NNZ         ,
     5 N1          ,N2          ,N3          ,H1          ,H2          ,
     5 H3          ,H4          ,NIN         ,NSN         ,IX1         ,
     6 IX2         ,IX3         ,IX4         ,NSVG        ,STIF        ,
     7 INACTI      ,KINI        ,ITAB        ,LB          ,LC          ,
     8 PENMIN      ,EPS         ,PENE        ,INTBUF_TAB%PENE_OLD,SUBTRIA,
     9 GAPV        ,IVIS2       ,INTBUF_TAB%IF_ADH,IFADHI ,BASE_ADH      ,
     A MVOISN      ,IBOUND      ,INTBUF_TAB%VTX_BISECTOR  ,DIST)
C
          DO I = 1 ,JLT
C
C       Needs to compute STIF_OLD even if PENE ==0 (cf INACTI=5)
C          IF(PENE(I)/=ZERO.AND.STIF(I)/=ZERO)THEN
           IF(STIF(I)/=ZERO)THEN
             IF(PENE(I)==ZERO)THEN
               N = CAND_N_N(I)
               IF(N <= NSN)THEN
                 INTBUF_TAB%STIF_OLD(2*(N-1)+1)=MAX(INTBUF_TAB%STIF_OLD(2*(N-1)+1),STIF(I))
               ELSE
                 STIF_OLDFI(NIN)%P(1,N-NSN) = MAX(STIF_OLDFI(NIN)%P(1,N-NSN),STIF(I))
               END IF
             ELSE
               JLT_NEW = JLT_NEW + 1
             END IF
           END IF
          ENDDO
C
          IF(INTTH==0.AND.JLT_NEW == 0.AND.(NINLOADP == 0.OR.DGAPLOADPMAX==ZERO))CYCLE
          IPARI(29,NIN) = 1
C
          IF (DEBUG(3)>=1) NB_IMPCT = NB_IMPCT + JLT_NEW
          IF (IMONM > 0 .AND. JTASK == 1) CALL STARTIME(20,1) 

C-------------------------------------------------------------------------------
C Friction model : computation of friction coefficients based on Material of connected Parts
C-------------------------------------------------------------------------------
        IF(JTASK==1) CALL STARTIME(MACRO_TIMER_FRIC,1)
          JJ = 0
          IF(IORTHFRIC > 0) THEN
            CALL FRICTIONPARTS_MODEL_ORTHO(
     1       INTFRIC        ,JLT          ,IPARTFRICSI  ,IPARTFRICMI  ,ADPARTS_FRIC   ,
     2       NSETPRTS       ,TABCOUPLEPARTS_FRIC,NPARTFRIC,TABPARTS_FRIC,TABCOEF_FRIC ,
     3       FRIC           ,VISCF        ,INTBUF_TAB%FRIC_P,FRIC_COEFS   , FRICC     ,
     4       VISCFFRIC      ,NTY          ,MFROT            ,IORTHFRIC    , FRIC_COEFS2,
     5       FRICC2         ,VISCFFRIC2   ,IFRICORTH        ,NFORTH       , NFISOT    ,
     6       INDEXORTH      ,INDEXISOT    ,JJ               ,IREP_FRICMI  ,DIR_FRICMI ,
     7       IX3            ,IX4          ,X1               ,Y1           , Z1        , 
     8       X2             ,Y2           ,Z2               ,X3           , Y3        ,
     9       Z3             ,X4           ,Y4               ,Z4           ,CE_LOC     ,
     A       DIR1           ,DIR2         )
          ELSE
            NFORTH = 0
            NFISOT = 0
            CALL FRICTIONPARTS_MODEL_ISOT(
     1       INTFRIC        ,JLT          ,IPARTFRICSI  ,IPARTFRICMI  ,ADPARTS_FRIC   ,
     2       NSETPRTS       ,TABCOUPLEPARTS_FRIC,NPARTFRIC,TABPARTS_FRIC,TABCOEF_FRIC ,
     3       FRIC           ,VISCF        ,INTBUF_TAB%FRIC_P,FRIC_COEFS   , FRICC     ,
     4       VISCFFRIC      ,NTY          ,MFROT            ,IORTHFRIC    ,IFRIC       ,
     5       JJ             , TINT        ,TEMPI            ,NPC          ,TF          ,
     6       TEMP           , H1          ,H2               ,H3           ,H4          ,
     7       IX1            , IX2         ,IX3              ,IX4          ,IFORM       ) 
          ENDIF
        IF(JTASK==1) CALL STOPTIME(MACRO_TIMER_FRIC,1)

          CALL I25FOR3(
     1  JLT          ,A         ,V            ,IBC         ,ICODT    ,
     2  FSAV         ,MS        ,VISC     ,
     3  VISCF        ,NOINT     ,INTBUF_TAB%STFNS,ITAB     ,CN_LOC   ,
     4  STIGLO       ,STIFN     ,STIF         ,INACTI      ,INDEX2(NFT+1),
     5  N1           ,N2        ,N3           ,H1          ,H2       ,
     6  H3           ,H4        ,FCONT        ,PENE        ,NRTM     ,
     7  IX1          ,IX2       ,IX3          ,IX4         ,NSVG     ,
     8  IVIS2        ,NELTST    ,ITYPTST      ,DT2T        ,
     A  KINET        ,NEWFRONT  ,ISECIN       ,NSTRF       ,SECFCUM  ,
     B  X            ,INTBUF_TAB%IRECTM,CE_LOC    ,MFROT       ,IFQ  ,
     B  INTBUF_TAB%SECND_FR,XFILTR_FRIC,IBAG   ,ICONTACT ,INTBUF_TAB%IRTLM,       
     E  VISCN        ,VXI       ,VYI          ,VZI         ,MSI      ,
     F  KINI         ,NIN       ,NISUB        ,INTBUF_TAB%LISUB,INTBUF_TAB%ADDSUBS,
     G  INTBUF_TAB%ADDSUBM,INTBUF_TAB%LISUBS,INTBUF_TAB%LISUBM,
     .                                 INTBUF_TAB%INFLG_SUBS,INTBUF_TAB%INFLG_SUBM,
     H  FSAVSUB      ,IPARI(33,NIN),IPARI(39,NIN),FNCONT     ,FTCONT   ,
     I  NSN          ,XX         ,YY         ,ZZ             ,
     J  XI           ,YI         ,ZI         ,ANGLMI        ,PADM      ,
     K  IADM         ,RCURVI    ,RCONTACT    ,ACONTACT   ,PCONTACT   ,
     N  MSKYI_SMS    ,ISKYI_SMS ,NSMS        ,CAND_N_N   ,INTBUF_TAB%PENE_OLD,
     O  INTBUF_TAB%STIF_OLD,INTBUF_TAB%MBINFLG,ILEV     ,IGSTI      ,KMIN     ,
     P  INTPLY       ,NM1        ,NM2        ,NM3         ,
     Q  INTBUF_TAB%MSEGTYP24,JTASK    ,ISENSINT  ,
     T  FSAVPARIT(1,1,NFT+1),H3D_DATA,FRICC  ,VISCFFRIC  ,FRIC_COEFS, GAPV,
     U  VISCFLUID   , SIGMAXADH  , VISCADHFACT, IFADHI   , AREASI   , BASE_ADH ,
     V  IORTHFRIC   ,FRIC_COEFS2 ,FRICC2      ,VISCFFRIC2,NFORTH    ,NFISOT    ,
     W  INDEXORTH   , INDEXISOT  ,DIR1        ,DIR2      ,APINCH    ,STIFPINCH,
     C  FNI         ,FX1        ,FY1          ,FZ1       ,FX2        ,FY2       ,
     D  FZ2         ,FX3        ,FY3          ,FZ3        ,FX4       ,
     E  FY4         ,FZ4        ,FXI          ,FYI        ,FZI       ,
     C  INTTH       ,DRAD       ,FHEATS       ,FHEATM     ,QFRICINT(NIN),
     D  EFRICT      ,TAGNCONT   ,KLOADPINTER  ,LOADPINTER ,LOADP_HYD_INTER,
     E  INTBUF_TAB%TYPSUB,IPARI(40,NIN),NINLOADP,DGAPLOADINT,S_LOADPINTER,
     F  DIST        ,DGAPLOADPMAX,INTEREFRIC  )
C
          IF (IMONM > 0 .AND. JTASK == 1) CALL STOPTIME(20,1)

        IF(INTTH > 0) THEN

                   CALL I25THERM(
     1     JLT    ,KTHE   ,TEMPI  ,AREASI ,IELESI ,
     2     IELEMI ,GAPV   ,IKTHE  ,XTHE   ,FNI    ,
     3     NPC    ,TF     ,FRAD   ,DRAD   ,EFRICT ,
     4     FHEATS ,FHEATM ,CONDINT,IFORM  ,TEMP   ,
     5     H1     ,H2     ,H3     ,H4     ,FCOND  ,
     6     DCOND  ,TINT   ,XI     ,YI     ,ZI     ,
     7     X1     ,Y1     ,Z1     ,X2     ,Y2     ,
     8     Z2     ,X3     ,Y3     ,Z3     ,X4     ,
     9     Y4     ,Z4     ,IX1    ,IX2    ,IX3    ,
     A     IX4    ,PHI    ,PHI1   ,PHI2   ,PHI3   ,
     B     PHI4   ,PM     ,NSVG   ,ITAB   )
         
         ENDIF


            CALL I25ASS3(
     1    JLT         ,NSVG      ,ITAB      ,CE_LOC    ,
     2    JTASK       ,NIN       ,NOINT     ,INTPLY    ,A         ,
     3    STIF        ,STIFN     ,NISKYFI   ,FSKYI     ,ISKY      ,
     4    N1          ,N2        ,N3        ,H1        ,H2        ,
     5    H3          ,H4        ,IX1       ,IX2       ,IX3       ,
     6    IX4         ,INTTH     ,FTHE      ,FTHESKYI  ,
     7    PHI         ,PHI1      ,PHI2      ,PHI3      ,PHI4      ,
     8    FNI         , INTBUF_TAB%MSEGTYP24 ,APINCH    ,
     .                                                  STIFPINCH , 
     9    FX1         ,FY1      ,FZ1        ,FX2       ,FY2       ,
     A    FZ2         ,FX3      ,FY3        ,FZ3       ,FX4       ,
     B    FY4         ,FZ4      ,FXI        ,FYI       ,FZI       ,
     F    IFORM       ,CONDINT  ,CONDN      ,CONDNSKYI )

      ENDDO
C-----------------------------------------------------------------------
      IF (SFSAVPARIT /= 0)THEN
          CALL SUM_6_FLOAT_SENS(FSAVPARIT, NISUB+1, 11, I_STOK,1,I_STOK,
     .                          FBSAV6, 12, 6, DIMFB, ISENSINT )
      ENDIF
      DEALLOCATE (FSAVPARIT)
C-----------------------------------------------------------------------
      CALL MY_BARRIER
C-----------------------------------------------------------------------
      DO N=NSNFT, NSNLT
        IF(INTBUF_TAB%IRTLM(4*(N-1)+1) < 0) 
     .    INTBUF_TAB%IRTLM(4*(N-1)+1)  = -INTBUF_TAB%IRTLM(4*(N-1)+1)
      END DO
C
      DO N=NSNRFT, NSNRLT
        IF(IRTLM_FI(NIN)%P(1,N) < 0) IRTLM_FI(NIN)%P(1,N)  = -IRTLM_FI(NIN)%P(1,N)
      END DO
C----------------------------------------------------------------------
C     2- EDGES
C----------------------------------------------------------------------
      IF(NEDGE==0) GOTO 500
C-----------------------------------------------------------------------
C
      CALL MY_BARRIER
C
      I_STOK = INTBUF_TAB%I_STOK_E(1)
C  cette partie est effectuee en // apres le calcul des forces des elem.
C decoupage statique
      NB_LOC = I_STOK / NTHREAD
      IF (JTASK==NTHREAD) THEN
        I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
      ELSE
        I_STOK_LOC = NB_LOC
      ENDIF
      DEBUT = (JTASK-1)*NB_LOC
      I_STOK = 0
C recalcul du istok
C     WRITE(6,*) "NEDGE=",NEDGE
      DO I = DEBUT+1, DEBUT+I_STOK_LOC


C  =========== DEBUG
#ifdef D_EM
       eidm = intbuf_tab%ledge(NLEDGE*(intbuf_tab%candm_e2e(i)-1) + 8)
       eids = ABS(intbuf_tab%cands_e2e(i)) 
       if(eids > nedge) then
         eids = ledge_fie(NIN)%P(E_GLOBAL_ID,eids-nedge)
       else
         eids = intbuf_tab%ledge(NLEDGE*(eids-1)+8)
       endif
       if(eidm == D_EM) then
         IF(INTBUF_TAB%CANDS_E2E(I) < 0) THEN
           write(6,"(A,I10,A,2I10,Z20)") __FILE__,i,"E2E conserve",eidm,eids, intbuf_tab%CAND_P(i)
         ELSE
           write(6,"(A,I10,A,2I10,Z20)") __FILE__,i,"E2E exclude",eidm,eids, intbuf_tab%CAND_P(i)
         ENDIF
       endif
#endif
C ============== End debug


       IF(INTBUF_TAB%CANDS_E2E(I) < 0) THEN
          I_STOK = I_STOK + 1
          INDEX2(I_STOK) = I
C         inbuf == cand_S
          INTBUF_TAB%CANDS_E2E(I) = -INTBUF_TAB%CANDS_E2E(I)
       ELSE ! Reset CAND_P
          INTBUF_TAB%CAND_P(I) = ZERO
       ENDIF
      ENDDO
C
        SFSAVPARIT = 0
        DO I=1,NISUB+1
          IF(ISENSINT(I)/=0) THEN
            SFSAVPARIT = SFSAVPARIT + 1
          ENDIF
        ENDDO
        IF (SFSAVPARIT /= 0) THEN
          ALLOCATE(FSAVPARIT(NISUB+1,11,I_STOK))
          DO J=1,I_STOK
            DO I=1,11
              DO H=1,NISUB+1
                FSAVPARIT(H,I,J) = ZERO
              ENDDO
            ENDDO
          ENDDO
        ELSE
          ALLOCATE(FSAVPARIT(0,0,0))
        ENDIF
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C         preparation candidats retenus
          CALL I25CDCOR3(
     1       JLT,INDEX2(NFT+1),INTBUF_TAB%CANDM_E2E,INTBUF_TAB%CANDS_E2E,CM_LOC,
     2       CS_LOC)
          CALL I25COR3E(
     1 JLT          ,INTBUF_TAB%LEDGE,INTBUF_TAB%IRECTM,X      ,V      ,
     2 CS_LOC       ,CM_LOC       ,INTBUF_TAB%STFE     ,MS     ,EX     ,
     3 EY           ,EZ           ,FX           ,FY           ,FZ      ,
     4 STIF         ,XXS1         ,XXS2         ,XYS1         ,XYS2    ,
     5 XZS1         ,XZS2         ,XXM1         ,XXM2         ,XYM1    ,
     6 XYM2         ,XZM1         ,XZM2         ,VXS1         ,VXS2    ,
     7 VYS1         ,VYS2         ,VZS1         ,VZS2         ,VXM1    ,
     8 VXM2         ,VYM1         ,VYM2         ,VZM1         ,VZM2    ,
     9 MS1          ,MS2          ,MM1          ,MM2          ,NE1     ,
     A NE2          ,ME1          ,ME2          ,NEDGE        ,NIN     ,
     C INTBUF_TAB%STFAC,NODNX_SMS ,NSMS         ,INTBUF_TAB%GAPE,GAPVE,
     D IEDGE        ,INTBUF_TAB%ADMSR,INTBUF_TAB%LBOUND,INTBUF_TAB%EDGE_BISECTOR,
     E INTBUF_TAB%VTX_BISECTOR ,IGAP0,
     F IAM          ,JAM          ,IBM          ,JBM          ,IAS     ,
     G JAS          ,IBS          ,JBS          ,ITAB         ,EDGE_ID ,
     H INTFRIC      ,INTBUF_TAB%IPARTFRIC_E     ,IPARTFRICSI  ,IPARTFRICMI,
     I IGAP         ,INTBUF_TAB%GAP_E_L,IGSTI   ,KMIN         ,KMAX    ,
     J ISTIF_MSDT   ,DTSTIF        ,INTBUF_TAB%STIFMSDT_EDG,INTERFACES%PARAMETERS)
          CALL I_COR_EPFIT3(
     1               JLT      ,INTBUF_TAB%STFE,STIF    ,CS_LOC ,CM_LOC ,
     2               NEDGE    ,NIN     ,INACTI  ,IPARI(40,NIN),TNCY) 

          CALL I25DST3E(
     1      JLT    ,CS_LOC,CM_LOC ,HS1   ,HS2   ,
     2      HM1    ,HM2   ,NX     ,NY    ,NZ    ,
     3      STIF   ,NE1   ,NE2    ,ME1   ,ME2   ,
     4      JLT_NEW,XXS1  ,XXS2   ,XYS1  ,XYS2  ,
     5      XZS1   ,XZS2  ,XXM1   ,XXM2  ,XYM1  ,
     6      XYM2   ,XZM1  ,XZM2   ,VXS1  ,VXS2  ,
     7      VYS1   ,VYS2  ,VZS1   ,VZS2  ,VXM1  ,
     8      VXM2   ,VYM1  ,VYM2   ,VZM1  ,VZM2  ,
     9      MS1    ,MS2   ,MM1    ,MM2   ,IEDGE ,
     B      NSMS   ,INDEX2(NFT+1),INTFRIC   ,IPARTFRICSI,
     .                                       IPARTFRICMI,
     C      GAPVE  ,EX    ,EY     ,EZ    ,FX    ,
     D      FY     ,FZ    ,INTBUF_TAB%LEDGE,INTBUF_TAB%IRECTM,
     .                                      INTBUF_TAB%CAND_P,
     E      IAM   ,JAM    ,IBM    ,JBM   ,IAS   ,
     F      JAS   ,IBS    ,JBS    ,ITAB  ,EDGE_ID,
     G      DGAPLOADPMAX)

C
          IF (IMONM > 0 .AND. JTASK == 1) CALL STARTIME(20,1)


          JLT = JLT_NEW
          IF(JLT_NEW/=0) THEN
            IF (IMONM > 0 .AND. JTASK == 1) CALL STARTIME(20,1)
            IPARI(29,NIN) = 1
            IF (DEBUG(3)>=1) NB_IMPCT = NB_IMPCT + JLT

C-------------------------------------------------------------------------------
C Friction model : computation of friction coefficients based on Material of connected Parts
C-------------------------------------------------------------------------------
           IF(MFROT == 0 ) THEN
              JJ = 0
              IFRIC =0
             CALL FRICTIONPARTS_MODEL_ISOT(
     1 INTFRIC        ,JLT          ,IPARTFRICSI    ,IPARTFRICMI  ,ADPARTS_FRIC   ,
     2 NSETPRTS       ,TABCOUPLEPARTS_FRIC,NPARTFRIC,TABPARTS_FRIC,TABCOEF_FRIC ,
     3 FRIC           ,VISCF        ,INTBUF_TAB%FRIC_P,FRIC_COEFS   , FRICC     ,
     4 VISCFFRIC      ,NTY          ,MFROT            ,IORTHFRIC     ,IFRIC       ,
     5 JJ             , TINT        ,TEMPI            ,NPC          ,TF          ,
     6 TEMP           , H1          ,H2               ,H3           ,H4          ,
     7 IX1            , IX2         ,IX3              ,IX4          ,IFORM       ) 
           ELSE
             DO I=1,JLT
               FRICC(I) = ZERO
             ENDDO 
           ENDIF

            CALL I25FOR3E(
     1 JLT          ,A            ,V            ,IBC        ,ICODT    ,
     2 FSAV         ,GAP          ,FRIC         ,MS         ,VISC     ,
     3 VISCF        ,NOINT        ,ITAB         ,CS_LOC     ,CM_LOC   ,
     4 STIGLO       ,STIFN        ,STIF         ,FSKYI      ,ISKY     ,
     5 FCONT        ,DT2T         ,IBM          ,HS1        ,
     6 HS2          ,HM1          ,HM2          ,NE1        ,NE2      ,
     7 ME1          ,ME2          ,IVIS2        ,NELTST     ,ITYPTST  ,
     8 NX           ,NY           ,NZ           ,GAPVE      ,INACTI   ,
     9 INDEX2(NFT+1),INTBUF_TAB%CAND_P,NISKYFIE ,NEWFRONT   ,ISECIN   ,
     A NSTRF        ,SECFCUM      ,VISCN        ,NEDGE      ,MS1      ,
     B MS2          ,MM1          ,MM2          ,VXS1       ,VYS1     ,
     C VZS1         ,VXS2         ,VYS2         ,VZS2       ,VXM1     ,
     D VYM1         ,VZM1         ,VXM2         ,VYM2       ,VZM2     ,
     E NIN          ,NISUB        ,INTBUF_TAB%LISUB,INTBUF_TAB%ADDSUBE,
     .                                               INTBUF_TAB%LISUBE,
     F INTBUF_TAB%INFLG_SUBE ,FSAVSUB,MSKYI_SMS    ,ISKYI_SMS  ,NSMS  ,
     G JTASK        ,ISENSINT     ,FSAVPARIT(1,1,NFT+1),NFT,H3D_DATA  ,
     H ILEV         ,INTBUF_TAB%EBINFLG, EDGE_ID,FRICC,IFQ            ,
     I INTBUF_TAB%FTSAVX_E,INTBUF_TAB%FTSAVY_E, INTBUF_TAB%FTSAVZ_E   ,
     .                                          INTBUF_TAB%IFPEN_E    ,
     J TAGNCONT   ,KLOADPINTER  ,LOADPINTER ,LOADP_HYD_INTER, INTBUF_TAB%TYPSUB,
     K STARTT     ,NINLOADP,DGAPLOADINT,S_LOADPINTER)

            IF (IMONM > 0 .AND. JTASK == 1) CALL STOPTIME(20,1)



          ENDIF
        ENDDO
 
        IF (SFSAVPARIT /= 0)THEN
            CALL SUM_6_FLOAT_SENS(FSAVPARIT, NISUB+1, 11, I_STOK,1,I_STOK,
     .                            FBSAV6, 12, 6, DIMFB, ISENSINT )
        ENDIF
        DEALLOCATE (FSAVPARIT)
C
C-----------------------------------------------------------------------
C
      CALL MY_BARRIER
C
      I_STOK = INTBUF_TAB%I_STOK_E(2)
C  cette partie est effectuee en // apres le calcul des forces des elem.
C decoupage statique
      NB_LOC = I_STOK / NTHREAD
      IF (JTASK==NTHREAD) THEN
        I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
      ELSE
        I_STOK_LOC = NB_LOC
      ENDIF
C     WRITE(6,*) "I_STOK_LOC=",I_STOK_LOC
      DEBUT = (JTASK-1)*NB_LOC
      I_STOK = 0
C recalcul du istok
      DO I = DEBUT+1, DEBUT+I_STOK_LOC
C  =========== DEBUG
#ifdef D_EM
C      eidm = intbuf_tab%ledge(NLEDGE*(intbuf_tab%candm_e2e(i)-1) + 8)
       eids = ABS(intbuf_tab%cands_e2S(i)) 
       if(eids > nedge) then
         eids = ledge_fie(NIN)%P(E_GLOBAL_ID,eids-nedge)
       else
         eids = intbuf_tab%ledge(NLEDGE*(eids-1)+8)
       endif
       if(eids == D_ES) then
         IF(INTBUF_TAB%CANDS_E2S(I) < 0) THEN
           write(6,"(A,I10,A,2I10,4Z20)") __FILE__,i,"E2S conserve ",eidm,eids,INTBUF_TAB%CAND_PS(4*(I-1)+1:4*(I-1)+4)
         ELSE
C          write(6,"(A,I10,A,2I10,Z20)") __FILE__,i," exclude",eidm,eids, intbuf_tab%CAND_PS(i)
         ENDIF
       endif
#endif
C ============== End debug

       IF(INTBUF_TAB%CANDS_E2S(I) < 0) THEN
          I_STOK = I_STOK + 1
          INDEX2(I_STOK) = I
C         inbuf == cand_S
          INTBUF_TAB%CANDS_E2S(I) = -INTBUF_TAB%CANDS_E2S(I)
       ELSE ! Reset CAND_P
          INTBUF_TAB%CAND_PS(4*(I-1)+1:4*(I-1)+4) = ZERO
       ENDIF
      ENDDO
C     WRITE(6,*) "INDEX2(1:,",I_STOK,INTBUF_TAB%I_STOK_E(2),LINDMAX

C
        SFSAVPARIT = 0
        DO I=1,NISUB+1
          IF(ISENSINT(I)/=0) THEN
            SFSAVPARIT = SFSAVPARIT + 1
          ENDIF
        ENDDO
        IF (SFSAVPARIT /= 0) THEN
          ALLOCATE(FSAVPARIT(NISUB+1,11,I_STOK))
          DO J=1,I_STOK
            DO I=1,11
              DO H=1,NISUB+1
                FSAVPARIT(H,I,J) = ZERO
              ENDDO
            ENDDO
          ENDDO
        ELSE
          ALLOCATE(FSAVPARIT(0,0,0))
        ENDIF
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C         preparation candidats retenus
          CALL I25CDCOR3_E2S(
     1       JLT,INDEX2(NFT+1),INTBUF_TAB%CANDM_E2S,INTBUF_TAB%CANDS_E2S,
     2       CM_LOC,CS_LOC )
          CALL I25COR3_E2S(
     1 JLT          ,INTBUF_TAB%LEDGE,INTBUF_TAB%IRECTM,X      ,V      ,
     2 CS_LOC       ,CM_LOC       ,INTBUF_TAB%STFM     ,MS     ,EX     ,
     3 EY           ,EZ           ,FX           ,FY           ,FZ      ,
     4 STIFE        ,XXS1         ,XXS2         ,XYS1         ,XYS2    ,
     5 XZS1         ,XZS2         ,XXM1         ,XXM2         ,XYM1    ,
     6 XYM2         ,XZM1         ,XZM2         ,VXS1         ,VXS2    ,
     7 VYS1         ,VYS2         ,VZS1         ,VZS2         ,VXM1    ,
     8 VXM2         ,VYM1         ,VYM2         ,VZM1         ,VZM2    ,
     9 MS1          ,MS2          ,MM1          ,MM2          ,NS1     ,
     A NS2          ,M1           ,M2           ,NEDGE        ,NIN     ,
     C INTBUF_TAB%STFAC,NODNX_SMS ,NSMSE        ,INTBUF_TAB%GAPE,GAPVE ,
     D IEDGE        ,INTBUF_TAB%ADMSR,INTBUF_TAB%LBOUND,INTBUF_TAB%EDGE_BISECTOR,
     E INTBUF_TAB%VTX_BISECTOR ,TYPEDGS ,IAS    ,JAS          ,IBS     ,
     F JBS          ,IAM       ,INTBUF_TAB%STFE,EDGE_ID, ITAB,
     G INTFRIC      ,INTBUF_TAB%IPARTFRIC_E    ,IPARTFRIC_ES   ,IPARTFRIC_EM,
     H IGSTI       ,KMIN        ,KMAX  ,INTBUF_TAB%E2S_NOD_NORMAL,NADMSR,
     I NORMALN1   ,NORMALN2      ,NORMALM1      ,NORMALM2     , ISTIF_MSDT,
     J DTSTIF    ,INTBUF_TAB%STIFMSDT_EDG,INTBUF_TAB%STIFMSDT_M,NRTM,INTERFACES%PARAMETERS)

          CALL I25DST3_E2S(
     1      JLT    ,CS_LOC,CM_LOC ,HS1   ,HS2   ,
     2      HM1    ,HM2   ,NX     ,NY    ,NZ    ,
     3      STIFE  ,NS1   ,NS2    ,M1    ,M2    ,
     4      JLT_NEW,XXS1  ,XXS2   ,XYS1  ,XYS2  ,
     5      XZS1   ,XZS2  ,XXM1   ,XXM2  ,XYM1  ,
     6      XYM2   ,XZM1  ,XZM2   ,VXS1  ,VXS2  ,
     7      VYS1   ,VYS2  ,VZS1   ,VZS2  ,VXM1  ,
     8      VXM2   ,VYM1  ,VYM2   ,VZM1  ,VZM2  ,
     9      MS1    ,MS2   ,MM1    ,MM2   ,IEDGE ,
     B      NSMSE  ,INDEX2(NFT+1),INTFRIC   ,IPARTFRIC_ES,
     .                                       IPARTFRIC_EM,
     C      GAPVE  ,EX    ,EY     ,EZ    ,FX    ,
     D      FY     ,FZ    ,INTBUF_TAB%LEDGE,INTBUF_TAB%IRECTM,X  ,
     E      INTBUF_TAB%CAND_PS,TYPEDGS ,IAS ,JAS ,IBS ,
     F      JBS    ,IAM   ,ITAB   ,INDX1,INDX2,
     G      CS_LOC4,CM_LOC4,EDGE_ID, NEDGE, NIN,
     H      DGAPLOADPMAX,NORMALN1,NORMALN2,NORMALM1,NORMALM2)
C
          ASSERT(4*JLT>=JLT_NEW)

          JLT=JLT_NEW
          IF(JLT_NEW/=0) THEN
            IF (IMONM > 0 .AND. JTASK == 1) CALL STARTIME(20,1)
            IPARI(29,NIN) = 1
            IF (DEBUG(3)>=1) NB_IMPCT = NB_IMPCT + JLT

C-------------------------------------------------------------------------------
C Friction model : computation of friction coefficients based on Material of connected Parts
C-------------------------------------------------------------------------------
           IF(MFROT == 0 ) THEN
             JJ = 0
             IFRIC = 0
             CALL FRICTIONPARTS_MODEL_ISOT(
     1 INTFRIC        ,JLT          ,IPARTFRIC_ES   ,IPARTFRIC_EM ,ADPARTS_FRIC ,
     2 NSETPRTS       ,TABCOUPLEPARTS_FRIC,NPARTFRIC,TABPARTS_FRIC,TABCOEF_FRIC ,
     3 FRIC           ,VISCF        ,INTBUF_TAB%FRIC_P,FRIC_COEFS , FRICC_E     ,
     4 VISCFFRIC_E    ,NTY          ,MFROT            ,IORTHFRIC   ,IFRIC       ,
     5 JJ             , TINT        ,TEMPI            ,NPC          ,TF          ,
     6 TEMP           , H1          ,H2               ,H3           ,H4          ,
     7 IX1            , IX2         ,IX3              ,IX4          ,IFORM       ) 
           ELSE
             DO I=1,JLT
               FRICC_E(I) = ZERO
             ENDDO 
           ENDIF

            ASSERT(JLT < 4*MVSIZ)
            CALL I25FOR3_E2S(
     1 JLT          ,A            ,V            ,IBC        ,ICODT    ,
     2 FSAV         ,GAP          ,FRIC         ,MS         ,VISC     ,
     3 VISCF        ,NOINT        ,ITAB         ,CS_LOC4    ,CM_LOC4  ,
     4 STIGLO       ,STIFN        ,STIFE        ,FSKYI      ,ISKY     ,
     5 FCONT        ,DT2T         ,NRTM,INTBUF_TAB%MSEGTYP24,HS1      ,
     6 HS2          ,HM1          ,HM2          ,NS1        ,NS2      ,
     7 M1           ,M2           ,IVIS2        ,NELTST     ,ITYPTST  ,
     8 NX           ,NY           ,NZ           ,GAPVE      ,INACTI   ,
     9 INDEX2(NFT+1),INTBUF_TAB%CAND_PS,NISKYFIE ,NEWFRONT   ,ISECIN   ,
     A NSTRF        ,SECFCUM      ,VISCN        ,NEDGE      ,MS1      ,
     B MS2          ,MM1          ,MM2          ,VXS1       ,VYS1     ,
     C VZS1         ,VXS2         ,VYS2         ,VZS2       ,VXM1     ,
     D VYM1         ,VZM1         ,VXM2         ,VYM2       ,VZM2     ,
     E NIN          ,NISUB        ,INTBUF_TAB%LISUB,INTBUF_TAB%ADDSUBE,INTBUF_TAB%ADDSUBM,
     F INTBUF_TAB%LISUBE ,INTBUF_TAB%LISUBM ,INTBUF_TAB%INFLG_SUBE ,INTBUF_TAB%INFLG_SUBM ,
     .                                                                            FSAVSUB ,  
     G MSKYI_SMS    ,ISKYI_SMS    ,NSMSE        ,JTASK        ,ISENSINT   ,
     H FSAVPARIT(1,1,NFT+1),NFT   ,H3D_DATA     ,INDX1        ,INDX2      ,
     I ILEV         ,INTBUF_TAB%MBINFLG, EDGE_ID,NEDGE_REM    ,FRICC_E    ,
     J IFQ          ,INTBUF_TAB%FTSAVX_E2S,INTBUF_TAB%FTSAVY_E2S, INTBUF_TAB%FTSAVZ_E2S ,
     .                                            INTBUF_TAB%IFPEN_E2S    ,
     K TAGNCONT   ,KLOADPINTER  ,LOADPINTER ,LOADP_HYD_INTER,INTBUF_TAB%TYPSUB,
     O STARTT     ,NINLOADP,DGAPLOADINT,S_LOADPINTER)

            IF (IMONM > 0 .AND. JTASK == 1) CALL STOPTIME(20,1)
          ENDIF
        ENDDO
 
        IF (SFSAVPARIT /= 0)THEN
            CALL SUM_6_FLOAT_SENS(FSAVPARIT, NISUB+1, 11, I_STOK,1,I_STOK,
     .                            FBSAV6, 12, 6, DIMFB, ISENSINT )
        ENDIF
        DEALLOCATE (FSAVPARIT)
C
      CALL MY_BARRIER
C
C-----------------------------------------------------------------------
 500  CONTINUE
      DEALLOCATE(INDEX2)
      RETURN
      END
Chd|====================================================================
Chd|  I25CDCOR3                     source/interfaces/int25/i25mainf.F
Chd|-- called by -----------
Chd|        I25COMP_2                     source/interfaces/int25/i25comp_2.F
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I25CDCOR3(JLT,INDEX,CAND_E,CAND_N,
     .                    CAND_E_N,CAND_N_N)      
C============================================================================
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, 
     .        INDEX(*), CAND_E(*), CAND_N(*), 
     .        CAND_E_N(*), CAND_N_N(*)
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
C
      RETURN
      END
Chd|====================================================================
Chd|  I25CDCOR3_E2S                 source/interfaces/int25/i25mainf.F
Chd|-- called by -----------
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I25CDCOR3_E2S(JLT,INDEX,CAND_E,CAND_N,
     .                    CAND_E_N,CAND_N_N)      
C============================================================================
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, 
     .        INDEX(*), CAND_E(*), CAND_N(*), 
     .        CAND_E_N(*), CAND_N_N(*)
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
C
      RETURN
      END
