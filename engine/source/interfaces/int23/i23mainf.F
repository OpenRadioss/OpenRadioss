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
      module I23MAINF_MOD
      implicit none
#include      "my_real.inc"
      INTEGER
     .       , DIMENSION(:), ALLOCATABLE :: IFPEN_SAV
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: CAND_P_SAV
      END MODULE I23MAINF_MOD
Chd|====================================================================
Chd|  I23MAINF                      source/interfaces/int23/i23mainf.F
Chd|-- called by -----------
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I23COR3                       source/interfaces/int23/i23cor3.F
Chd|        I23DST3                       source/interfaces/int23/i23dst3.F
Chd|        I23FOR3                       source/interfaces/int23/i23for3.F
Chd|        I7CDCOR3                      source/interfaces/int07/i7cdcor3.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SUM_6_FLOAT_SENS              source/system/parit.F         
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        STRI                          source/tools/univ/stri.F      
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I23MAINF(
     1                   IPARI  ,INTBUF_TAB         ,X        ,A      ,
     2                   ICODT  ,FSAV    ,V         ,MS       ,ITAB   ,
     3                   STIFN  ,FSKYI   ,ISKY      ,FCONT    ,NIN    ,
     4                   LINDMAX ,JTASK  ,NB_JLT ,NB_JLT_NEW,NB_STOK_N,
     5                   NSTRF  ,SECFCUM ,ICONTACT  ,VISCN   ,NUM_IMP ,
     6                   NS_IMP ,NE_IMP  ,IND_IMP   ,NRTMDIM ,FNCONT  ,
     7                   FTCONT ,RCONTACT ,ACONTACT ,PCONTACT,KINET   ,
     8                   WEIGHT ,MSKYI_SMS,ISKYI_SMS,NODNX_SMS ,NODGLOB,
     9                   NPC    ,TF      , NISKYFI  ,NEWFRONT  ,MWAG   ,
     A                   FBSAV6 ,ISENSINT,DIMFB     ,DT2T      ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I23MAINF_MOD
      USE INTBUFDEF_MOD 
      USE H3D_MOD 
      USE MESSAGE_MOD
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
#include      "units_c.inc"
#include      "scr04_c.inc"
#include      "scr07_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST, ITYPTST, NIN, NSTRF(*), NRTMDIM, NEWFRONT,
     .        NISKYFI
      INTEGER IPARI(NPARI,NINTER), ICODT(*),ICONTACT(*),
     .        ITAB(*), ISKY(*), KINET(*), ISKYI_SMS(*), NODNX_SMS(*),
     .        TAGMSR_I21_SMS, NODGLOB(*), NPC(*), MWAG(*)
      INTEGER NB_JLT,NB_JLT_NEW,NB_STOK_N,JTASK,
     .        LINDMAX,DIMFB
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*),IND_IMP(*), WEIGHT(*)
C     REAL
      my_real 
     .   X(*), A(3,*), FSAV(*), V(3,*),
     .   MS(*),STIFN(*),FSKYI(LSKYI,4), FCONT(3,*),
     .   SECFCUM(7,NUMNOD,NSECT), VISCN(*),
     .   FNCONT(3,*), FTCONT(3,*), RCONTACT(*), ACONTACT(*),
     .   PCONTACT(*), MSKYI_SMS(*),
     .   TF(*), DT2T
      DOUBLE PRECISION FBSAV6(12,6,DIMFB)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_STOK, JLT_NEW, JLT , NFT, J,
     .        IBC, NOINT, NSEG, ISECIN, IBAG, 
     .        IGAP, INACTI, IFQ, MFROT, IGSTI, NISUB,
     .        NB_LOC, I_STOK_LOC,DEBUT,
     .        INTTH,IFORM, NCAND, IKTHE, IFSTF, H, IERROR
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), CN_LOC(MVSIZ),CE_LOC(MVSIZ),
     .        CAND_N_N(MVSIZ), CAND_E_N(MVSIZ), KINI(MVSIZ),
     .        INDEX2(LINDMAX),
     .        NSMS(MVSIZ), ISENSINT(*)
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT, 
     .   VISC,STIGLO,GAPMIN,
     .   KMIN, KMAX, GAPMAX, KTHE, XTHE, TINT, RHOH,
     .   SCAL_T, DERI
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real  
     .        FINTER
C-----------------------------------------------
C     REAL
      my_real
     .   LB(MVSIZ), LC(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .   NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .   NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), PENE(MVSIZ),
     .   GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .   MXI(MVSIZ), MYI(MVSIZ), MZI(MVSIZ), STRI(MVSIZ),
     .   PENRAD(MVSIZ), FXT(MVSIZ), FYT(MVSIZ), FZT(MVSIZ)
      my_real
     .    VXM(MVSIZ), VYM(MVSIZ), VZM(MVSIZ), 
     .    H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .    FX, FY, FZ, STF
      INTEGER ICURV, IP0, IP1, IP2, IS, SFSAVPARIT
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGP
      my_real, DIMENSION(:,:,:), ALLOCATABLE :: FSAVPARIT
C---------------------------------------------------- 
C
      NSN   =IPARI(5,NIN) 
      NMN   =IPARI(6,NIN)
      NTY   =IPARI(7,NIN)
      IBC   =IPARI(11,NIN)
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
      IFSTF =IPARI(48,NIN)
C no heat interface
      INTTH = IPARI(47,NIN)
      SCAL_T= INTBUF_TAB%VARIABLES(33)
C      
      STIGLO=-INTBUF_TAB%STFAC(1)
      IF(IFSTF/=0)STIGLO = STIGLO*FINTER(IFSTF,TT/SCAL_T,NPC,TF,DERI)
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C  
      FRIC  =INTBUF_TAB%VARIABLES(1)
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      VISC  =INTBUF_TAB%VARIABLES(14)
C
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
C
C -------------------------------------------------------------
C     STOCKAGE DES ANCIENS CANDIDATS 
C -------------------------------------------------------------
C
C Barriere dans tous les cas pour bminma [et cur_max_max]
C
      CALL MY_BARRIER
C
      I_STOK     = INTBUF_TAB%I_STOK(1)
cC
c      ALLOCATE(ITAGP(NMN))
c      ITAGP(1:NMN)=0
cC
c      IP0 = 1
c      IP1 = IP0 + NSN + 3
cC     IP1 = IP0 + NSN + NSNROLD + 3
c      IP2 = IP1 + I_STOK
c      IF(JTASK==1)THEN
cC MWA = MWAG SUR TASK 0
c        CALL I23TRCF(
cC    1    NSN+NSNROLD  ,I_STOK       ,INBUF(KD(15)),INBUF(KD(14)),
c     1    NSN          ,I_STOK       ,INBUF(KD(15)),INBUF(KD(14)),
c     3    MWAG(IP0)    ,MWAG(IP1)    ,INBUF(KD(27)))
c      ENDIF
cC
c      IF(JTASK==1)THEN
c        ALLOCATE(IFPEN_SAV(I_STOK),CAND_P_SAV(I_STOK))
c        IFPEN_SAV(1:I_STOK) =INBUF(KD(27):KD(27)+I_STOK-1)
c        CAND_P_SAV(1:I_STOK)=BUFIN(JD(18):JD(18)+I_STOK-1)
c      END IF
C
C----------------------------------------------------
C
      CALL MY_BARRIER
C
C----------------------------------------------------
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
          IF(INTBUF_TAB%CAND_N(I)<0) THEN
            I_STOK = I_STOK + 1
            INDEX2(I_STOK) = I
C inbuf == cand_n
            INTBUF_TAB%CAND_N(I) = -INTBUF_TAB%CAND_N(I)
          ELSE
	    INTBUF_TAB%CAND_P(I) = ZERO
	    INTBUF_TAB%FTSAVX(I) = ZERO
	    INTBUF_TAB%FTSAVY(I) = ZERO
	    INTBUF_TAB%FTSAVZ(I) = ZERO
	    INTBUF_TAB%IFPEN(I) = 0
          ENDIF
        ENDDO
C
c------------------------------------------------
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
     .           C1='(/INTER/TYPE23)')
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
     .           C1='(/INTER/TYPE23)')
           CALL ARRET(2)
          ENDIF
        ENDIF
c
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C preparation candidats retenus
          CALL I7CDCOR3(
     1         JLT,INDEX2(NFT+1),INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     2         CAND_E_N,CAND_N_N)
C cand_n et cand_e remplace par cand_n_n et cand_e_n
	  CALL I23COR3(
     1  JLT	  ,NIN       ,X 	,INTBUF_TAB%IRECTM,NSN	 ,
     2  INTBUF_TAB%NSV,CAND_E_N ,CAND_N_N ,INTBUF_TAB%STFM,
     +  				        INTBUF_TAB%STFNS,
     3  INTBUF_TAB%MSR,MS     ,V 	 ,XI	    ,YI 	,
     4  ZI	   ,IX1      ,IX2	 ,IX3	    ,IX4	,
     5  NSVG	   ,IGSTI    ,STIF	 ,KMIN      ,KMAX	,
     6  IGAP	   ,GAP      ,INTBUF_TAB%GAP_S,GAPV    ,GAPMAX	,
     7  GAPMIN     ,INTBUF_TAB%GAP_M,VXI	   ,VYI     ,VZI,
     8  MSI	   ,NODNX_SMS,NSMS	   ,KINET   ,X1 	,
     9  Y1	    ,Z1       ,X2	,Y2	     ,Z2	,
     A  X3	    ,Y3       ,Z3	,X4	     ,Y4	,
     B  Z4	    ,NX1      ,NX2     ,NX3	    ,NX4       ,
     C  NY1	    ,NY2      ,NY3	,NY4	     ,NZ1      ,
     D  NZ2	   ,NZ3      ,NZ4      ,KINI	 ,INDEX2(NFT+1))
C
          JLT_NEW = 0
C
	  CALL I23DST3(
     1      JLT 	   ,CAND_N_N ,CAND_E_N ,CN_LOC ,CE_LOC ,
     2      X1     ,X2     ,X3     ,X4     ,Y1     ,
     3      Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     4      Z3     ,Z4     ,XI     ,YI     ,ZI     ,
     6      IX1    ,IX2    ,IX3    ,IX4    ,NSVG   ,
     7      GAPV   ,INACTI ,INDEX2(NFT+1),
     8      VXM    ,VYM    ,VZM    ,H1     ,H2     ,
     9      H3     ,H4     ,INTBUF_TAB%IRECTM,INTBUF_TAB%CAND_P,
     A      INTBUF_TAB%IFPEN,NX	 ,NY	 ,NZ	,INTBUF_TAB%FTSAVX,
     B      INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ,FXT    ,FYT   ,FZT,
     C      PENE   ,V	 ,VXI	 ,VYI	,VZI   ,
     D      MSI    ,STIF ,JLT_NEW,NSMS  ,KINI  )
          JLT = JLT_NEW
          IF (IMONM > 0) CALL STARTIME(20,1)

          IF(JLT_NEW/=0) THEN
            IPARI(29,NIN) = 1
            IF (DEBUG(3)>=1)
     .        NB_JLT_NEW = NB_JLT_NEW + JLT
C 
	    CALL I23FOR3(
     1  JLT	     ,NIN	,NOINT        ,IBC	,ICODT        ,
     2  FSAV	     ,GAP	,STIGLO       ,FRIC	,VISC	      ,
     3  INACTI       ,MFROT	,IFQ	      ,IBAG	,
     4  IPARI(39,NIN),STIF	,GAPV	      ,ITAB	,A	      ,
     5  INTBUF_TAB%CAND_P,INTBUF_TAB%FRIC_P,INTBUF_TAB%XFILTR,V	,ICONTACT,
     6  NISKYFI      ,NSVG	   ,X1        ,Y1	    ,Z1       ,
     7  X2	     ,Y2	   ,Z2        ,X3	    ,Y3       ,
     8  Z3	     ,X4	   ,Y4        ,Z4	    ,XI       ,
     9  YI	     ,ZI	   ,VXI       ,VYI	    ,VZI      ,
     A  MSI	     ,VXM	   ,VYM       ,VZM	    ,NX       ,
     B  NY	     ,NZ	   ,PENE      ,H1	    ,H2       ,
     C  H3	     ,H4	   ,INDEX2(NFT+1),CAND_N_N  ,WEIGHT   ,
     F  FXT	     ,FYT	   ,FZT       ,DT2T         ,
     G  FCONT	     ,FNCONT	   ,FTCONT    ,STIFN	    ,VISCN    ,
     H  NEWFRONT     ,ISECIN	   ,NSTRF     ,SECFCUM      ,FSKYI    ,
     I  ISKY	     ,INTTH	   ,MS        ,IX1	    ,IX2      ,
     J  IX3	 ,IX4	    ,INTBUF_TAB%FTSAVX,INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ,
     K  KMIN	     ,KMAX	  ,CN_LOC     ,CE_LOC	,MSKYI_SMS    ,
     L  ISKYI_SMS    ,NSMS	   ,JTASK     ,ISENSINT ,FSAVPARIT    ,
     M  NISUB        ,NFT          ,H3D_DATA  )
C
            ENDIF ! JLT_NEW/=0
          IF (IMONM > 0) CALL STOPTIME(20,1)
C
        ENDDO
c
        IF (SFSAVPARIT /= 0)THEN
            CALL SUM_6_FLOAT_SENS(FSAVPARIT, NISUB+1, 11, I_STOK,1,I_STOK,
     .                            FBSAV6, 12, 6, DIMFB, ISENSINT )
        ENDIF
        IF(ALLOCATED(FSAVPARIT)) DEALLOCATE (FSAVPARIT)
C
      CALL MY_BARRIER
C
c      DEALLOCATE(ITAGP)
c      IF(JTASK==1)DEALLOCATE(IFPEN_SAV,CAND_P_SAV)
      RETURN
      END
