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
Chd|  I11MAINF                      source/interfaces/int11/i11mainf.F
Chd|-- called by -----------
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FRICTIONPARTS_MODEL_ISOT      source/interfaces/int07/frictionparts_model.F
Chd|        I11CDCOR3                     source/interfaces/int11/i11cdcor3.F
Chd|        I11COR3                       source/interfaces/int11/i11cor3.F
Chd|        I11DST3                       source/interfaces/int11/i11dst3.F
Chd|        I11FOR3                       source/interfaces/int11/i11for3.F
Chd|        I11THERM                      source/interfaces/int11/i11therm.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SUM_6_FLOAT_SENS              source/system/parit.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I11MAINF(
     1      IPARI   ,X         ,A       ,
     2      ICODT   ,FSAV      ,V        ,MS     ,DT2T    ,
     3      NELTST  ,ITYPTST   ,ITAB     ,STIFN  ,FSKYI   ,
     4      ISKY    ,FCONT     ,NIN      ,LINDMAX ,JTASK   ,
     5      NB_JLT  ,NB_JLT_NEW,NB_STOK_N,NISKYFI,NEWFRONT,
     6      NSTRF   ,SECFCUM   ,VISCN    ,NUM_IMP, NS_IMP ,
     7      NE_IMP  ,MSKYI_SMS ,ISKYI_SMS,NODNX_SMS,ICONTACT,
     8      INTBUF_TAB,PM       ,TEMP    , FTHE    ,FTHESKYI ,
     9      NPC     ,TF        ,CONDN    ,CONDNSKYI,FBSAV6,
     A      ISENSINT,DIMFB     ,FSAVSUB  ,H3D_DATA ,INTBUF_FRIC_TAB,
     B      ITASK   ,TAGNCONT  ,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER,
     C      DGAPLOADINT,S_LOADPINTER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE H3D_MOD
      USE INTBUF_FRIC_MOD
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "impl1_c.inc"
#include      "timeri_c.inc"
#include      "macro.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST,NIN,NEWFRONT,NSTRF(*)
      INTEGER IPARI(*), ICODT(*),
     .        ITAB(*), ISKY(*),ICONTACT(*),TAGNCONT(NLOADP_HYD_INTER,NUMNOD)
      INTEGER  , INTENT(IN) :: S_LOADPINTER
      INTEGER  , INTENT(IN) :: KLOADPINTER(NINTER+1),LOADPINTER(S_LOADPINTER),
     .         LOADP_HYD_INTER(NLOADP_HYD)
C statistique interface
      INTEGER NB_JLT,NB_JLT_NEW,NB_STOK_N,JTASK,
     .        NISKYFI, LINDMAX,ITASK
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*), 
     .        ISKYI_SMS(*), NODNX_SMS(*),NPC(*), ISENSINT(*),DIMFB
C     REAL
      my_real  , INTENT(IN) :: DGAPLOADINT(S_LOADPINTER)
      my_real DT2T,
     .   X(*), A(3,*), FSAV(*), V(3,*),
     .   MS(*), STIFN(*), FSKYI(LSKYI,NFSKYI), FCONT(3,*),
     .   SECFCUM(7,NUMNOD,NSECT), VISCN(*), MSKYI_SMS(*),
     .   TEMP(*),FTHE(*),FTHESKYI(*),TF(*),CONDN(*),CONDNSKYI(*),
     .   PM(NPROPM,*),FSAVSUB(*)

      DOUBLE PRECISION FBSAV6(12,6,DIMFB)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(INTBUF_FRIC_STRUCT_), TARGET, DIMENSION(NINTERFRIC) ::  INTBUF_FRIC_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KD(50), JD(50), JFI, KFI,
     .        I, J, H, IBC, NOINT, ISECIN, I_STOK, NSEG, 
     .        JLT , NFT, JLT_NEW, IGAP, IVIS2,
     .        NB_LOC, I_STOK_LOC,DEBUT,I3N, IGSTI,IFORM,INTTH,IKTHE,
     .        IFORMTH,SFSAVPARIT,NISUB,INTFRIC,NSETPRTS,NPARTFRIC,MFROT,IFQ,
     .        IERROR,IORTHFRIC,IFRIC,JJ,NINLOADP
      INTEGER N1(MVSIZ), N2(MVSIZ), M1(MVSIZ), M2(MVSIZ),
     .        NSVG(MVSIZ), CS_LOC(MVSIZ), CM_LOC(MVSIZ),
     .        NSMS(MVSIZ)
      INTEGER I_STOK_NEW,CAND_S(MVSIZ),CAND_M(MVSIZ), INDEX2(LINDMAX),
     .        INDEXM1(LINDMAX),IELECI(MVSIZ),IELESI(MVSIZ), IPARTFRICSI(MVSIZ),
     .         IPARTFRICMI(MVSIZ)
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,
     .   VISC,VISCF,STIGLO, GAPMIN, KMIN, KMAX,DTMINI,
     .   TINT,XTHE,KTHE,FRAD,DRAD,DGAPLOAD
C-----------------------------------------------
C     REAL
      my_real
     .     NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),
     .     HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .     STIF(MVSIZ),GAPV(MVSIZ),
     .     XXS1(MVSIZ), XXS2(MVSIZ), XYS1(MVSIZ), XYS2(MVSIZ),
     .     XZS1(MVSIZ), XZS2(MVSIZ), XXM1(MVSIZ), XXM2(MVSIZ),
     .     XYM1(MVSIZ), XYM2(MVSIZ), XZM1(MVSIZ), XZM2(MVSIZ),
     .     VXS1(MVSIZ), VXS2(MVSIZ), VYS1(MVSIZ), VYS2(MVSIZ),
     .     VZS1(MVSIZ), VZS2(MVSIZ), VXM1(MVSIZ), VXM2(MVSIZ),
     .     VYM1(MVSIZ), VYM2(MVSIZ), VZM1(MVSIZ), VZM2(MVSIZ),
     .     MS1(MVSIZ),  MS2(MVSIZ),  MM1(MVSIZ),  MM2(MVSIZ),
     .     FNI(MVSIZ), TEMPI1(MVSIZ),TEMPI2(MVSIZ),TEMPM1(MVSIZ),
     .     TEMPM2(MVSIZ),PHIS1(MVSIZ),PHIS2(MVSIZ),PHIM1(MVSIZ),
     .     PHIM2(MVSIZ),AREAC(MVSIZ), CONDINTS1(MVSIZ),
     .     CONDINTS2(MVSIZ),CONDINTM1(MVSIZ),CONDINTM2(MVSIZ),
     .     PENRAD(MVSIZ),FX1(MVSIZ), FX2(MVSIZ),
     .     FX3(MVSIZ), FX4(MVSIZ),FY1(MVSIZ), FY2(MVSIZ), 
     .     FY3(MVSIZ), FY4(MVSIZ),FZ1(MVSIZ), FZ2(MVSIZ), 
     .     FZ3(MVSIZ), FZ4(MVSIZ),K1(MVSIZ) , K2(MVSIZ) ,
     .      K3(MVSIZ) , K4(MVSIZ) ,C1(MVSIZ) , C2(MVSIZ) , 
     .      C3(MVSIZ) , C4(MVSIZ)
      my_real, DIMENSION(:,:,:), ALLOCATABLE :: FSAVPARIT 
      my_real
     .    XFILTR_FRIC,FRIC_COEFS(MVSIZ,10),VISCFFRIC(MVSIZ),FRICC(MVSIZ)
      INTEGER, DIMENSION(:) ,POINTER  :: TABCOUPLEPARTS_FRIC
      INTEGER, DIMENSION(:) ,POINTER  :: TABPARTS_FRIC 
      INTEGER, DIMENSION(:) ,POINTER  :: ADPARTS_FRIC 
      my_real, DIMENSION(:) ,POINTER  :: TABCOEF_FRIC
 
      INTEGER,TARGET, DIMENSION(1):: TABCOUPLEPARTS_FRIC_BID
      INTEGER,TARGET, DIMENSION(1):: TABPARTS_FRIC_BID
      INTEGER,TARGET, DIMENSION(1):: ADPARTS_FRIC_BID
      my_real,TARGET, DIMENSION(1):: TABCOEF_FRIC_BID
      INTEGER :: NRTS, NTY
C-----------------------------------------------
C	Calcul des adresses des buffers d'interfaces. 
C	Les adresses des buffers J10-JFI et K10-KFI 
C	sont remplaces systematiquement par des tableaux
C	JD(i) et KD(i), en gardant les memes numeros d'indexes.
C	Les anciens adresses directs Jn, Kn sont modifies
C	dans la routine sans commentaires additionnels habituels
C----------------------------------------------------
C
      NRTS  =IPARI(3)
      NTY   =IPARI(7)
      IBC   =IPARI(11)
      IVIS2 =IPARI(14)
      NOINT =IPARI(15)
      IGAP  =IPARI(21)
      ISECIN=IPARI(28)
      IFORM =IPARI(30) 
      IGSTI =IPARI(34)
      NISUB =IPARI(36)
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
      VISCF =INTBUF_TAB%VARIABLES(15)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
      DTMINI=INTBUF_TAB%VARIABLES(41)
      DGAPLOAD=INTBUF_TAB%VARIABLES(46)
      NUM_IMP = 0
      IF (IMPL_S==1) THEN
          VISC  =ZERO
          VISCF =ZERO
      ENDIF
C heat interface
      INTTH = IPARI(47)
      IKTHE = IPARI(43)
      IFORMTH =IPARI(44) 
      KTHE  = INTBUF_TAB%VARIABLES(20)
      XTHE  = INTBUF_TAB%VARIABLES(22)
      TINT  = INTBUF_TAB%VARIABLES(21)
      FRAD  = INTBUF_TAB%VARIABLES(23)
      DRAD  = INTBUF_TAB%VARIABLES(24)
      IFRIC = 0
C--- Corresponding Friction model   
      INTFRIC=IPARI(72)
      MFROT = 0
      IORTHFRIC = 0
      NPARTFRIC = 0 
      XFILTR_FRIC = 0 
      NSETPRTS = 0
      IF(INTFRIC /= 0) THEN 
         TABCOUPLEPARTS_FRIC  => INTBUF_FRIC_TAB(INTFRIC)%TABCOUPLEPARTS_FRIC
         TABCOEF_FRIC  => INTBUF_FRIC_TAB(INTFRIC)%TABCOEF_FRIC
         TABPARTS_FRIC  => INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC
         ADPARTS_FRIC   => INTBUF_FRIC_TAB(INTFRIC)%ADPARTS_FRIC 
         XFILTR_FRIC   = INTBUF_FRIC_TAB(INTFRIC)%XFILTR_FRIC
         NSETPRTS =   INTBUF_FRIC_TAB(INTFRIC)%NSETPRTS
         NPARTFRIC =   INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC
         IORTHFRIC = INTBUF_FRIC_TAB(INTFRIC)%IORTHFRIC
      ELSE
         TABCOUPLEPARTS_FRIC  => TABCOUPLEPARTS_FRIC_BID
         TABPARTS_FRIC  => TABPARTS_FRIC_BID
         TABCOEF_FRIC  => TABCOEF_FRIC_BID 
         ADPARTS_FRIC   => ADPARTS_FRIC_BID
      ENDIF
C
      NINLOADP = IPARI(95) ! load pressure related to inter
C
      I_STOK = INTBUF_TAB%I_STOK(1)
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
      DO I = DEBUT+1, DEBUT+I_STOK_LOC
          IF(INTBUF_TAB%CAND_N(I)<0) THEN
            I_STOK = I_STOK + 1
            INDEX2(I_STOK) = I
C inbuf == cand_S
            INTBUF_TAB%CAND_N(I) = -INTBUF_TAB%CAND_N(I)
          ENDIF
      ENDDO
      IF (DEBUG(3)>=1) THEN
          NB_JLT = NB_JLT + I_STOK_LOC
          NB_STOK_N = NB_STOK_N + I_STOK
      ENDIF
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
     .           C1='(/INTER/TYPE11)')
           CALL ARRET(2)
          ENDIF
        DO J=1,I_STOK
          DO I=1,11
            DO H=1,NISUB+1
              FSAVPARIT(H,I,J) = ZERO
            ENDDO
          ENDDO
        ENDDO
      ELSE
        ALLOCATE(FSAVPARIT(0,0,0),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .           C1='(/INTER/TYPE11)')
           CALL ARRET(2)
          ENDIF
      ENDIF
C
      DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C preparation candidats retenus
          CALL I11CDCOR3(
     1         JLT,INDEX2(NFT+1),INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,CM_LOC,
     2         CS_LOC)

          CALL I11COR3(
     1  JLT          ,INTBUF_TAB%IRECTS,INTBUF_TAB%IRECTM,X              ,V      ,
     2  CS_LOC       ,CM_LOC           ,INTBUF_TAB%STFS  ,INTBUF_TAB%STFM,GAPMIN ,
     3  INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,IGAP           ,GAPV           ,MS     ,
     4  STIF         ,XXS1            ,XXS2         ,XYS1         ,XYS2   ,
     5  XZS1         ,XZS2            ,XXM1         ,XXM2         ,XYM1   ,
     6  XYM2         ,XZM1            ,XZM2         ,VXS1         ,VXS2   ,
     7  VYS1         ,VYS2            ,VZS1         ,VZS2         ,VXM1   ,
     8  VXM2         ,VYM1            ,VYM2         ,VZM1         ,VZM2   ,
     9  MS1          ,MS2             ,MM1          ,MM2          ,N1     ,
     A  N2           ,M1              ,M2           ,NRTS         ,NIN    ,
     B  IGSTI        ,KMIN            ,KMAX         ,NODNX_SMS    ,NSMS   ,
     C  INTBUF_TAB%GAP_SL, INTBUF_TAB%GAP_ML,INTTH,TEMP        ,TEMPI1 ,
     D  TEMPI2       ,TEMPM1          ,TEMPM2,INTBUF_TAB%AREAS,INTBUF_TAB%AREAM,
     E  AREAC        ,IELECI          ,IELESI,INTBUF_TAB%IELEC,INTBUF_TAB%IELES,
     F  IFORMTH      , ITAB        ,INTFRIC  ,INTBUF_TAB%IPARTFRICS,IPARTFRICSI,
     G  INTBUF_TAB%IPARTFRICM,IPARTFRICMI)

          CALL I11DST3(
     1        JLT      ,CS_LOC    ,CM_LOC         ,HS1           ,HS2    ,
     2        HM1      , HM2      ,NX             ,NY            ,NZ     ,
     3        STIF     ,N1        ,N2             ,M1            ,M2     ,
     4        JLT_NEW  ,XXS1      ,XXS2           ,XYS1          ,XYS2   ,
     5        XZS1     ,XZS2      ,XXM1           ,XXM2          ,XYM1   ,
     6        XYM2     ,XZM1      ,XZM2           ,VXS1          ,VXS2   ,
     7        VYS1     ,VYS2      ,VZS1           ,VZS2          ,VXM1   ,
     8        VXM2     ,VYM1      ,VYM2           ,VZM1          ,VZM2   ,
     9        MS1      ,MS2       ,MM1            ,MM2           ,GAPV   ,
     A        NSMS     ,INDEX2(NFT+1),DRAD        , INTFRIC    ,IPARTFRICSI,
     B        IPARTFRICMI,DGAPLOAD)

          JLT = JLT_NEW
          IF(JLT_NEW/=0) THEN
            IF (IMONM > 0 .AND. JTASK == 1) CALL STARTIME(20,1)
            IPARI(29) = 1
            IF (DEBUG(3)>=1)
     .        NB_JLT_NEW = NB_JLT_NEW + JLT_NEW

C-------------------------------------------------------------------------------
C Friction model : computation of friction coefficients based on Material of connected Parts
C-------------------------------------------------------------------------------
      IF(ITASK==1) CALL STARTIME(MACRO_TIMER_FRIC,1)
       JJ = 0
      CALL FRICTIONPARTS_MODEL_ISOT(
     1   INTFRIC    ,JLT          ,IPARTFRICSI      ,IPARTFRICMI  ,ADPARTS_FRIC,
     2   NSETPRTS   ,TABCOUPLEPARTS_FRIC,NPARTFRIC  ,TABPARTS_FRIC,TABCOEF_FRIC,
     3   FRIC       ,VISCF        ,INTBUF_TAB%FRIC_P,FRIC_COEFS   , FRICC      ,
     4   VISCFFRIC  ,NTY          ,MFROT            ,IORTHFRIC    , IFRIC      ,
     5   JJ         , TINT        ,TEMPI1           ,NPC          ,TF          ,
     6   TEMP       , HS1         ,HS2              ,HM1          ,HM2         ,
     7   N1         , N2          ,M1               ,M2           ,IFORM       )  
      IF(ITASK==1) CALL STOPTIME(MACRO_TIMER_FRIC,1)

      CALL I11FOR3(
     1  JLT          ,FSAV    ,GAP                 ,FRIC       ,MS      ,
     2  VISC         ,VISCF   ,NOINT               ,ITAB      ,CS_LOC   ,
     3  CM_LOC       ,STIF    ,DT2T                ,HS1       ,HS2      ,
     4  HM1          ,HM2     ,N1                  ,N2        , M1      ,
     5  M2           ,IVIS2   ,NELTST              ,ITYPTST   ,NX       ,
     6  NY           ,NZ      ,GAPV,INTBUF_TAB%PENIS,INTBUF_TAB%PENIM   ,
     7  IPARI(22)    ,NEWFRONT,NRTS                ,MS1       ,MS2      , 
     8  MM1          ,MM2     ,VXS1                ,VYS1      ,VZS1     ,
     9  VXS2         ,VYS2    ,VZS2                ,VXM1      ,VYM1     ,
     A  VZM1         ,VXM2    ,VYM2                ,VZM2      ,NIN      ,
     B  DTMINI,IFORM ,INTBUF_TAB%FTSAVX,INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ,       
     C  INDEX2(NFT+1),INTBUF_TAB%IFPEN             ,INTBUF_TAB%STFS,FNI ,
     E  FX1          ,FY1     ,FZ1                 ,FX2       ,FY2      ,
     F  FZ2          ,FX3     ,FY3                 ,FZ3       ,FX4      ,
     G  FY4          ,FZ4     ,K1                  ,K2        ,K3       ,
     H  K4           ,C1      ,C2                  ,C3        ,C4       ,
     I  INTTH        ,DRAD    ,PENRAD     ,ISENSINT  ,FSAVPARIT         ,
     J  NISUB        ,NFT     ,INTBUF_TAB%ADDSUBS    ,INTBUF_TAB%ADDSUBM,
     K  INTBUF_TAB%LISUBS,INTBUF_TAB%LISUBM,INTBUF_TAB%LISUB,FSAVSUB,FRICC  ,
     L  VISCFFRIC    ,TAGNCONT ,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER  ,
     M  INTBUF_TAB%TYPSUB,INTBUF_TAB%INFLG_SUBS,INTBUF_TAB%INFLG_SUBM   ,
     .                                           NINLOADP   ,DGAPLOADINT,
     N  S_LOADPINTER )

C
        IF( INTTH > 0 ) THEN 
           CALL I11THERM(
     1          JLT       ,PM       ,INTTH      ,PENRAD   , KTHE       ,
     2          TEMPI1    ,TEMPI2   ,TEMPM1     ,TEMPM2   ,PHIS1       ,
     3          PHIS2     ,TINT     ,AREAC      ,IELECI   ,IELESI      ,
     4          FRAD      ,GAPV     ,FNI        ,IKTHE    ,XTHE        ,
     5          NPC       ,DRAD     ,TF         ,HS1      ,HS2         ,
     6          HM1       ,HM2      ,CONDINTS1  ,CONDINTS2,PHIM1       ,
     7          PHIM2     ,CONDINTM1,CONDINTM2  ,IFORMTH  ) 
        ENDIF
C
	CALL I11ASS3 (
     1  JLT          ,A            ,NIN         ,NOINT       ,CS_LOC   ,
     2  STIFN        ,STIF         ,FSKYI       ,ISKY        ,FCONT    ,
     3  HS1          ,HS2          ,HM1         ,HM2         ,N1       ,
     4  N2           ,M1           ,M2          ,NISKYFI     ,ISECIN   , 
     5  NSTRF        ,SECFCUM      ,VISCN       ,NRTS        ,ISKYI_SMS,
     6  NSMS         ,ICONTACT     ,MSKYI_SMS   ,FX1         ,FY1      ,
     7  FZ1          ,FX2          ,FY2         ,FZ2         ,FX3      ,
     8  FY3          ,FZ3          ,FX4         ,FY4         ,FZ4      ,
     9  K1           ,K2           ,K3          ,K4          ,C1       ,      
     A  C2           ,C3           ,C4          ,INTTH       ,PHIS1    ,
     B  PHIS2        ,PHIM1        ,PHIM2       ,FTHE        ,FTHESKYI ,
     C  CONDINTS1    ,CONDINTS2    ,CONDINTM1   ,CONDINTM2   ,CONDN    ,
     D  CONDNSKYI    ,JTASK        ,H3D_DATA    )
C
            IF(IMPL_S==1) THEN
              DO I = 1 ,JLT_NEW
                NS_IMP(I+NUM_IMP)=CS_LOC(I)
                NE_IMP(I+NUM_IMP)=CM_LOC(I)
              ENDDO
              NUM_IMP=NUM_IMP+JLT_NEW
            ENDIF
            IF (IMONM > 0 .AND. JTASK == 1) CALL STOPTIME(20,1)
          ENDIF
      ENDDO
      IF (SFSAVPARIT /= 0)THEN
          CALL SUM_6_FLOAT_SENS(FSAVPARIT, NISUB+1, 11, I_STOK,1,I_STOK,
     .  			FBSAV6, 12, 6, DIMFB, ISENSINT )
      ENDIF
      IF(ALLOCATED(FSAVPARIT)) DEALLOCATE (FSAVPARIT)

C
      RETURN
      END
