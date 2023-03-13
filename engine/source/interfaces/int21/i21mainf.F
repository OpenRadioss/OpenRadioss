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
Chd|  I21MAINF                      source/interfaces/int21/i21mainf.F
Chd|-- called by -----------
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I21ASS3                       source/interfaces/int21/i21ass3.F
Chd|        I21COR3                       source/interfaces/int21/i21cor3.F
Chd|        I21FOR3                       source/interfaces/int21/i21for3.F
Chd|        I21SMS2                       source/interfaces/int21/i21sms2.F
Chd|        I21THERM                      source/interfaces/int21/i21therm.F
Chd|        I7CDCOR3                      source/interfaces/int07/i7cdcor3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SUM_6_FLOAT_SENS              source/system/parit.F         
Chd|        STRI                          source/tools/univ/stri.F      
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|====================================================================
      SUBROUTINE I21MAINF(
     1                   IPARI  ,INTBUF_TAB         ,X        ,A      ,
     2                   ICODT  ,FSAV    ,V         ,MS       ,ITAB   ,
     3                   STIFN  ,FSKYI   ,ISKY      ,FCONT    ,NIN    ,
     4                   LINDMAX ,JTASK  ,NB_JLT ,NB_JLT_NEW,NB_STOK_N,
     5                   NSTRF  ,SECFCUM ,ICONTACT  ,VISCN   ,NUM_IMP ,
     6                   NS_IMP ,NE_IMP  ,IND_IMP   ,NRTMDIM ,FNCONT  ,
     7                   FTCONT ,RCONTACT ,ACONTACT ,PCONTACT,INTSTAMP,
     8                   WEIGHT ,TEMP     ,FTHE     ,FTHESKYI,MSKYI_SMS,
     9                   ISKYI_SMS ,NODNX_SMS ,NODGLOB,NPC   ,TF       ,
     A                   QFRICINT,NCONT   ,INDEXCONT ,TAGCONT,CONDN    ,
     B                   CONDNSKYI,DT2T   ,NELTST   ,ITYPTST ,KINET    ,
     C                   FBSAV6  ,ISENSINT,DIMFB    ,NISKYFI ,H3D_DATA ,
     D                   PSKIDS  ,TAGNCONT,KLOADPINTER,LOADPINTER,LOADP_HYD_INTER,
     E                   DGAPLOADINT,S_LOADPINTER,INTEREFRIC )                   
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
      USE INTBUFDEF_MOD
      USE H3D_MOD
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr04_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST, ITYPTST, NIN, NSTRF(*), NRTMDIM
      INTEGER IPARI(NPARI,NINTER), ICODT(*),ICONTACT(*),
     .        ITAB(*), ISKY(*), ISKYI_SMS(*), NODNX_SMS(*),
     .        TAGMSR_I21_SMS, NODGLOB(*), NPC(*),INDEXCONT(*),
     .        TAGCONT(*),KINET(*),
     .        TAGNCONT(NLOADP_HYD_INTER,NUMNOD)
      INTEGER NB_JLT,NB_JLT_NEW,NB_STOK_N,JTASK,
     .        LINDMAX, NCONT,NISKYFI
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*),IND_IMP(*), WEIGHT(*), 
     .        ISENSINT(*),DIMFB
      INTEGER  , INTENT(IN) :: S_LOADPINTER
      INTEGER  , INTENT(IN) :: KLOADPINTER(NINTER+1),LOADPINTER(S_LOADPINTER),
     .         LOADP_HYD_INTER(NLOADP_HYD)
      INTEGER  , INTENT(IN) :: INTEREFRIC
C     REAL
      my_real  , INTENT(IN) :: DGAPLOADINT(S_LOADPINTER)
      my_real 
     .   X(*), A(3,*), FSAV(*), V(3,*),
     .   MS(*),STIFN(*),FSKYI(LSKYI,4), FCONT(3,*),
     .   SECFCUM(7,NUMNOD,NSECT), VISCN(*),
     .   FNCONT(3,*), FTCONT(3,*), RCONTACT(*), ACONTACT(*),
     .   PCONTACT(*), TEMP(*), FTHE(*), FTHESKYI(LSKYI), MSKYI_SMS(*),
     .   TF(*), QFRICINT(*),CONDN(*),CONDNSKYI(LSKYI), PSKIDS(*), DT2T
      DOUBLE PRECISION FBSAV6(12,6,DIMFB)
      TYPE(INTSTAMP_DATA) INTSTAMP
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_STOK, JLT_NEW, JLT , NFT, J,
     .        IBC, NOINT, NSEG, ISECIN, IBAG, IADM,
     .        IGAP, INACTI, IFQ, MFROT, IGSTI, NISUB,
     .        NB_LOC, I_STOK_LOC,DEBUT,
     .        INTTH,IFORM, NCAND, MSTR, ILEV, IKTHE, IROT, H,
     .        IFORM_THE,INVN,IFTLIM, IERROR, NINSKID
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ),
     .        CAND_N_N(MVSIZ), CAND_E_N(MVSIZ), INDEX2(LINDMAX),
     .        NSMS(MVSIZ)
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,
     .   VISC,STIGLO,GAPMIN,
     .   KMIN, KMAX, GAPMAX, KTHE, XTHE, TINT, RHOH, DTI ,PMAXSKID
C-----------------------------------------------
C     REAL
      my_real
     .   LB(MVSIZ), LC(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), PENE(MVSIZ),
     .   GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .   GAP0(MVSIZ), AREA0(MVSIZ), TEMPI(MVSIZ), PHI(MVSIZ),
     .   MXI(MVSIZ), MYI(MVSIZ), MZI(MVSIZ), STRI(MVSIZ),
     .   ASI(MVSIZ), BSI(MVSIZ),DIST(MVSIZ),
     .   XP(MVSIZ), YP(MVSIZ), ZP(MVSIZ), KT(MVSIZ), C(MVSIZ),
     .   PENRAD(MVSIZ), TEMPM(MVSIZ),EFRICT(MVSIZ),CONDINT(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   PHI1(MVSIZ),PHI2(MVSIZ),PHI3(MVSIZ),PHI4(MVSIZ),PRATIO(MVSIZ),
     .   EFRIC_L(MVSIZ)
      my_real
     .     RCURVI(MVSIZ), ANGLMI(MVSIZ), ANGLT, PADM
      INTEGER NRTMFT, NRTMLT, NRADM, ITRIA(MVSIZ)
      my_real
     .    FNI(MVSIZ),
     .    FXT(MVSIZ), FYT(MVSIZ), FZT(MVSIZ),
     .    FXN(MVSIZ), FYN(MVSIZ), FZN(MVSIZ),
     .    VXM, VYM, VZM, FX, FY, FZ, STF, PMAX,
     .    XG(3), ROT(9), WXM, WYM, WZM, FRAD, DRAD, FHEAT, XFRIC,DCOND
      INTEGER ICURV,SFSAVPARIT,IFRIC,FCOND
      my_real
     .     , DIMENSION(:,:,:), ALLOCATABLE :: FSAVPARIT
C---------------------------------------------------- 
C
      NSN   =IPARI(5,NIN) 
      NTY   =IPARI(7,NIN)
      IBC   =IPARI(11,NIN)
      IF(IPARI(33,NIN)==1) RETURN
      NOINT =IPARI(15,NIN)
      ILEV  =IPARI(20,NIN)
      IGAP  =IPARI(21,NIN)
      INACTI=IPARI(22,NIN)
      ISECIN=IPARI(28,NIN)
      MFROT =IPARI(30,NIN)
      IFQ =IPARI(31,NIN) 
      IBAG =IPARI(32,NIN) 
      IGSTI=IPARI(34,NIN)
      NISUB =IPARI(36,NIN)
      ICURV =IPARI(39,NIN)
C adaptive meshing
      IADM =IPARI(44,NIN) 
      NRADM=IPARI(49,NIN)
      PADM =INTBUF_TAB%VARIABLES(24)
      ANGLT=INTBUF_TAB%VARIABLES(25)
C heat interface
      INTTH = IPARI(47,NIN)
      IKTHE = IPARI(42,NIN)
      IFORM_THE = IPARI(48,NIN)
      KTHE  = INTBUF_TAB%VARIABLES(20)
      XTHE  = INTBUF_TAB%VARIABLES(30)
      TINT  = INTBUF_TAB%VARIABLES(21)
      FRAD  = INTBUF_TAB%VARIABLES(31)
      DRAD  = INTBUF_TAB%VARIABLES(32)
      FCOND = IPARI(53,NIN) ! function of variation of heat exchange as funct of distance
      DCOND =  INTBUF_TAB%VARIABLES(36)  ! max conduction distance
C      
      MSTR=INTSTAMP%MSR
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
      IFRIC =IPARI(50,NIN)
      XFRIC =INTBUF_TAB%VARIABLES(34)
C
      PMAX  =INTBUF_TAB%VARIABLES(15)
C
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
C
      FHEAT =INTBUF_TAB%VARIABLES(33)
C
      INVN =IPARI(51,NIN) ! Flag to detect inverted normals
C
      IFTLIM =IPARI(52,NIN) ! Flag to limit tangential force using YLD/sqrt(3) 
C
      VXM=INTSTAMP%V(1)
      VYM=INTSTAMP%V(2)
      VZM=INTSTAMP%V(3)
      WXM=INTSTAMP%VR(1)
      WYM=INTSTAMP%VR(2)
      WZM=INTSTAMP%VR(3)
      IROT=INTSTAMP%IROT
      XG(1:3) =INTSTAMP%XG(1:3)
      ROT(1:9)=INTSTAMP%ROT(1:9)

      NINSKID = 0
      IF(H3D_DATA%N_SCAL_SKID > 0) THEN
       NINSKID = H3D_DATA%N_SKID_INTER(NIN)
      ENDIF
      PMAXSKID=INTBUF_TAB%VARIABLES(35)
C
c----------------------------------------------------
C   IADM!=0 + Icurv!=0 non available (starter error).
c----------------------------------------------------
        I_STOK = INTBUF_TAB%I_STOK(1)

C
C decoupage statique
        NB_LOC = I_STOK / NTHREAD
        IF (JTASK==NTHREAD) THEN
          I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
        ELSE
          I_STOK_LOC = NB_LOC
        ENDIF
        DEBUT = (JTASK-1)*NB_LOC
C
        I_STOK = 0
C
        IF (IMPL_S==1) THEN
          NUM_IMP = 0
          VISC  =ZERO
        ENDIF
C
        DO I = DEBUT+1, DEBUT+I_STOK_LOC
          J=INTBUF_TAB%CAND_N(I)
C irtlm(1,cand_n) == cand_e
          IF(ABS(INTBUF_TAB%IRTLM(1+2*(J-1)))==INTBUF_TAB%CAND_E(I)) THEN
            I_STOK = I_STOK + 1
            INDEX2(I_STOK) = I
c            INTBUF_TAB%CAND_N(I) = -INTBUF_TAB%CAND_N(I)
          ENDIF
        ENDDO
C
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
     .           C1='(/INTER/TYPE21)')
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
     .           C1='(/INTER/TYPE21)')
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
	  CALL I7CDCOR3(
     1         JLT,INDEX2(NFT+1),INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     2         CAND_E_N,CAND_N_N)
C cand_n et cand_e remplace par cand_n_n et cand_e_n
	  CALL I21COR3(
     1  JLT	  ,NIN       ,X 	,INTBUF_TAB%IRECTM,NSN	 ,
     2  INTBUF_TAB%NSV,CAND_E_N ,CAND_N_N ,INTBUF_TAB%STF,
     +  					 INTBUF_TAB%STFNS,
     3  INTBUF_TAB%XM0,INTBUF_TAB%NOD_NORMAL,INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,
     +  					  INTBUF_TAB%MSR_L,
     4  MS	 ,V	     ,XI	,YI	    ,ZI 	,
     5  IX1	 ,IX2	     ,IX3	,IX4	    ,NSVG	,
     6  IGSTI	 ,STIF       ,KMIN	,KMAX	    ,IGAP	,
     7  GAP	 ,INTBUF_TAB%GAP_S,GAPV	,GAPMAX     ,GAPMIN	,
     8  NX	,NY	   ,NZ         ,PENE	   ,VXM        ,
     9  VYM	 ,VZM	    ,VXI	,VYI	    ,VZI	,
     A  MSI	 ,ITRIA     ,LB 	,LC	    ,IADM	,
     B  INTBUF_TAB%RCURV,INTBUF_TAB%ANGLM,NRADM  ,ANGLT   ,RCURVI,
     C  ANGLMI   ,FXT	    ,FYT	,FZT	    ,INTBUF_TAB%FTSAVX,
     D  INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ ,INTBUF_TAB%GAP_S(1+NSN),
     .                                     INTBUF_TAB%AREAS,GAP0 ,
     E  AREA0	 ,INTTH     ,TEMP	,TEMPI     ,IROT	 ,
     F  XG	 ,ROT	    ,INTBUF_TAB%AS,INTBUF_TAB%BS,ASI	 ,
     G  BSI	 ,XP	    ,YP 	,ZP	   ,NODNX_SMS	 ,
     H  NSMS	 ,MSTR      ,INTBUF_TAB%PENIS,INTBUF_TAB%IFPEN,ILEV,   
     I  X1	 ,Y1	    ,Z1 	  ,X2	   ,Y2  	 ,
     J  Z2	 ,X3	    ,Y3 	  ,Z3	   ,X4  	 ,
     K  Y4	 ,Z4	    ,DRAD	  ,PENRAD  ,TINT	 ,
     L  TEMPM	 ,IFORM_THE ,H1           ,H2      ,H3           ,
     M  H4       ,DIST      ,ITAB         ,NOINT   ,INTBUF_TAB%VARIABLES(23),
     N  INVN     )
          
          IF (IMONM > 0 .AND. JTASK == 1) CALL STARTIME(20,1)

          IPARI(29,NIN) = 1
          IF (DEBUG(3)>=1)
     .      NB_JLT_NEW = NB_JLT_NEW + JLT         
C
	    CALL I21FOR3(
     1  JLT	     ,NIN	,NOINT        ,IBC	,ICODT        ,
     2  FSAV	     ,GAP	,STIGLO       ,FRIC	,VISC	      ,
     3  INACTI       ,MFROT	,IFQ	      ,IBAG	,IADM	      ,
     4  IPARI(39,NIN),STIF	,GAPV	      ,ITAB	,
     5  INTBUF_TAB%PENIS,INTBUF_TAB%FROT_P,INTBUF_TAB%ALPHA0,INTBUF_TAB%IFPEN,
     6  ICONTACT  ,   RCONTACT  ,ACONTACT ,PCONTACT	,
     7  NSVG	     ,X1	,Y1	      ,Z1	,X2	      ,
     8  Y2	     ,Z2	,X3	      ,Y3	,Z3	      ,
     9  X4	     ,Y4	,Z4	      ,XI	,YI	      ,
     A  ZI	     ,VXI	,VYI	      ,VZI	,MSI	      ,
     B  VXM	     ,VYM	,VZM	      ,NX	,NY	      ,
     C  NZ	     ,PENE	,FXT	      ,FYT	,FZT	      ,
     D  FXN	     ,FYN	,FZN	      ,RCURVI	,ANGLMI       ,
     E  PADM	     ,CAND_N_N  ,WEIGHT       ,IGAP	,GAP0	      ,
     F  AREA0	     ,PMAX	,IROT	      ,XG	,MXI	      ,
     G  MYI	     ,MZI	,STRI	      ,WXM	,WYM	      ,
     H  WZM	     ,XP	,YP	      ,ZP	,KT	      ,
     I  C	     ,ILEV	,FNI	      ,INTTH	,FHEAT        ,
     J  EFRICT       ,QFRICINT(NIN),IFRIC     ,XFRIC    ,TEMPI        ,
     K  TEMPM        ,NPC       ,TF           ,IX1      ,IX2          ,
     L  IX3          ,IX4       ,DT2T         ,NELTST   ,ITYPTST      ,
     M  KINET        ,NISUB     ,ISENSINT     ,FSAVPARIT,NFT          ,
     N  IFTLIM       ,NINSKID   ,PRATIO       ,PMAXSKID ,INTEREFRIC   ,
     O  EFRIC_L      )
C
          IF( INTTH > 0 ) THEN 
           CALL I21THERM(JLT   ,XI     ,YI    ,ZI    ,KTHE  ,
     2                   TEMPI ,PHI    ,AREA0 ,NOINT ,ASI   ,
     3                   BSI    ,GAPV  ,PENE  ,IKTHE ,XTHE  ,
     4                   FNI    ,NPC   ,TF    ,FRAD  ,DRAD  ,
     5                   PENRAD ,TEMPM ,FHEAT ,EFRICT,CONDINT,
     6                   IFORM_THE,H1  ,H2    ,H3    ,H4    ,
     7                   PHI1    ,PHI2 ,PHI3  ,PHI4  ,X1    ,
     8                   Y1      ,Z1   ,X2    ,Y2    ,Z2    ,
     9                   X3     ,Y3    ,Z3    ,X4    ,Y4    ,
     A                   Z4     ,ITAB  ,NSVG  ,INTBUF_TAB%MSR_L,IX1   ,
     B                   IX2    ,IX3   ,IX4    ,TEMP ,FCOND ,
     C                   DCOND )      
          ENDIF 
C
          IF(IDTMINS==2.OR.IDTMINS_INT/=0)THEN
            DTI=DT2T
            CALL I21SMS2(JLT   ,MSTR         ,NSVG ,NIN   ,NOINT ,
     2                   MSKYI_SMS ,ISKYI_SMS,NSMS ,KT    ,C     ,
     3                   DTI   )
            IF(DTI<DT2T)THEN
              DT2T    = DTI
              NELTST  = NOINT
              ITYPTST = 10
            ENDIF
          END IF
C
          IF(IDTMINS_INT/=0)THEN
            STIF(1:JLT)=ZERO
          END IF
C
	  CALL I21ASS3(JLT       ,A        ,NIN      ,NOINT    ,FXN    ,
     2  		FYN      ,FZN      ,FXT      ,FYT      ,FZT    ,
     3  		IX1      ,IX2      ,IX3      ,IX4      ,NSVG   ,
     4  		FCONT    ,FNCONT   ,FTCONT   ,LB       ,LC     ,
     5  		ITRIA    ,STIFN    ,STIF     ,FSKYI    ,ISKY   ,
     6  		ISECIN   ,NSTRF    ,SECFCUM  ,INTBUF_TAB%FTSAVX,
     .  					  INTBUF_TAB%FTSAVY,
     7  		INTBUF_TAB%FTSAVZ  ,CAND_N_N ,INTSTAMP,WEIGHT  ,
     .  					  INTBUF_TAB%MSR21,
     8  		INTTH    ,PHI      ,FTHE   ,FTHESKYI  ,MXI     ,
     9  		MYI      ,MZI      ,STRI   ,NODGLOB   ,NCONT   ,
     A  		INDEXCONT,TAGCONT  ,CONDN  ,CONDINT   ,CONDNSKYI,
     B                  IFORM_THE, PHI1    ,PHI2   ,PHI3      ,PHI4     ,
     C                  H1       ,H2       ,H3     ,H4        ,NISKYFI  ,
     D                  INTBUF_TAB%MSR_L   ,ITAB   ,H3D_DATA  ,NINSKID  ,
     E                  PRATIO   ,H3D_DATA%N_SCAL_SKID,PSKIDS ,IPARI(95,NIN),
     F                  TAGNCONT ,KLOADPINTER,LOADPINTER ,LOADP_HYD_INTER,
     G                  DGAPLOADINT,DIST,GAPV,S_LOADPINTER    ,EFRIC_L  ,
     H                  FHEAT     ,EFRICT ,INTEREFRIC )  
C
          IF (IMONM > 0 .AND. JTASK == 1) CALL STOPTIME(20,1)
C
        ENDDO

c
        IF (SFSAVPARIT /= 0)THEN
            CALL SUM_6_FLOAT_SENS(FSAVPARIT, NISUB+1, 11, I_STOK,1,I_STOK,
     .                            FBSAV6, 12, 6, DIMFB, ISENSINT )
        ENDIF
        IF (ALLOCATED(FSAVPARIT)) DEALLOCATE (FSAVPARIT)
C
      RETURN
      END
