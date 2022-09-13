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
Chd|  I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- called by -----------
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I11CDCOR3                     source/interfaces/int11/i11cdcor3.F
Chd|        I20COR3E                      source/interfaces/int20/i20cor3.F
Chd|        I20DST3E                      source/interfaces/int20/i20cor3.F
Chd|        I20FOR3                       source/interfaces/int20/i20for3.F
Chd|        I20FOR3C                      source/interfaces/int20/i20for3.F
Chd|        I20FOR3E                      source/interfaces/int20/i20for3.F
Chd|        I20NORM                       source/interfaces/int20/i20curv.F
Chd|        I20NORMCNT                    source/interfaces/int20/i20curv.F
Chd|        I20NORME                      source/interfaces/int20/i20rcurv.F
Chd|        I20NORMN                      source/interfaces/int20/i20rcurv.F
Chd|        I20NORMNP                     source/interfaces/int20/i20rcurv.F
Chd|        I20NORMP                      source/interfaces/int20/i20curv.F
Chd|        I20NORMS                      source/interfaces/int20/i20curv.F
Chd|        I20RCURV                      source/interfaces/int20/i20rcurv.F
Chd|        I7CDCOR3                      source/interfaces/int07/i7cdcor3.F
Chd|        I7THERM                       source/interfaces/int07/i7therm.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_EXCH_N                   source/mpi/generic/spmd_exch_n.F
Chd|        SPMD_I20CURVSZ                source/mpi/interfaces/spmd_i20curvsz.F
Chd|        SPMD_I20EXCH_N                source/mpi/interfaces/spmd_i20exch_n.F
Chd|        SPMD_I20NORMF                 source/mpi/interfaces/spmd_i20normf.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SUM_6_FLOAT_SENS              source/system/parit.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I20MAINF(
     1       IPARI     ,X         ,A         ,
     2       ICODT     ,FSAV      ,V         ,MS        ,DT2T      ,
     3       NELTST    ,ITYPTST   ,ITAB      ,STIFN     ,FSKYI     ,
     4       ISKY      ,FCONT     ,NIN       ,LINDMAX   ,KINET     ,
     5       JTASK     ,NB_JLT    ,NB_JLT_NEW,NB_STOK_N ,
     6       NISKYFI   ,NEWFRONT  ,NSTRF     ,SECFCUM   ,ICONTACT  ,
     7       VISCN     ,NUM_IMP   ,
     9       NS_IMP    ,NE_IMP    ,IND_IMP   ,FSAVSUB   ,NRTMDIM   ,
     A       FSAVBAG   ,
     B       EMINX     ,IXS       ,IXS16     ,IXS20     ,FNCONT    ,
     C       FTCONT    ,IAD_ELEM  ,FR_ELEM   ,RCONTACT  ,ACONTACT  ,
     D       PCONTACT  ,TEMP      ,FTHE      ,FTHESKYI  ,
     E       PM        ,IPARG     ,IAD17     ,WEIGHT    ,NISKYFIE  ,
     F       IRLEN20   ,ISLEN20   ,IRLEN20T  ,ISLEN20T  ,IRLEN20E  ,
     G       ISLEN20E  ,MSKYI_SMS ,ISKYI_SMS ,NODNX_SMS ,NPC       ,
     H       TF        ,INTBUF_TAB,FBSAV6    ,ISENSINT  ,DIMFB     ,
     I       H3D_DATA  )
C=======================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MOD_NODNORM
      USE MOD_RCURV
      USE INTBUFDEF_MOD
      USE H3D_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
C#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST,NIN,NEWFRONT,NSTRF(*),
     .        NRTMDIM, IAD17, IRLEN20, ISLEN20, IRLEN20T, ISLEN20T,
     .        IRLEN20E, ISLEN20E, DIMFB
      INTEGER IPARI(NPARI), ICODT(*),ICONTACT(*),
     .        ITAB(*), ISKY(*), KINET(*), 
     .        WEIGHT(*),IPARG(NPARG,*)
      INTEGER NB_JLT,NB_JLT_NEW,NB_STOK_N,JTASK,
     .        NISKYFI, LINDMAX, NISKYFIE
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*),IND_IMP(*)
      INTEGER IXS(*)  ,IXS16(*) ,IXS20(*)
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), 
     .        ISKYI_SMS(*), NODNX_SMS(*),NPC(*), ISENSINT(*)
      my_real 
     .        EMINX(*)
C     REAL
      my_real DT2T,
     .   X(*), A(3,*), FSAV(*), V(3,*),FSAVBAG(*),
     .   MS(*),STIFN(*),FSKYI(LSKYI,4),FCONT(3,*),
     .   SECFCUM(7,NUMNOD,NSECT),VISCN(*), FSAVSUB(*),
     .   FNCONT(3,*), FTCONT(3,*), RCONTACT(*), ACONTACT(*),
     .   PCONTACT(*),
     .   TEMP(*),FTHE(*),FTHESKYI(LSKYI),PM(NPROPM,*),
     .   MSKYI_SMS(*),TF(*)

      DOUBLE PRECISION FBSAV6(12,6,DIMFB)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
C=======================================================================
C     ALLOCATABLE
C=======================================================================
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_STOK, JLT_NEW, JLT , NFT, IVIS2,
     .        IBC, NOINT, NSEG, ISECIN, IBAG, IADM,
     .        IGAP, INACTI, IFQ, MFROT, IGSTI, NISUB,
     .        NB_LOC, I_STOK_LOC,DEBUT,JD42B,
     .        ILAGM, LENR, LENT, MAXCC,INTTH,IFORM,JD22B,JD22C,
     .        NLN, NRTMFT, NRTMLT, NMNFT, NMNLT, NRADM,
     .        NLNFT1, NLNLT, NLNL, IFUNCTK, SFSAVPARIT, J, H, IERROR
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), CN_LOC(MVSIZ),CE_LOC(MVSIZ),
     .        CAND_N_N(MVSIZ),CAND_E_N(MVSIZ),KINI(MVSIZ),
     .        INDEX2(LINDMAX),
     .        ISDSIZ(NSPMD+1),IRCSIZ(NSPMD+1),ITAG(NUMNOD),
     .        IELECI(MVSIZ)
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,
     .   VISC,VISCF,STIGLO,GAPMIN,
     .   KMIN, KMAX, GAPMAX,RSTIF,FHEAT,TINT,RHOH,FRAD,DRAD,
     .   XTHE,FHEATM,FHEATS
C-----------------------------------------------
C     REAL
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ),
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .     PENE(MVSIZ),
     .     H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .     GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .     GAPR(MVSIZ),TEMPI(MVSIZ),PHI(MVSIZ),AREASI(MVSIZ)
      my_real
     .     NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),
     .     HS1(MVSIZ), HS2(MVSIZ), HM1(MVSIZ), HM2(MVSIZ),
     .     XXS1(MVSIZ), XXS2(MVSIZ), XYS1(MVSIZ), XYS2(MVSIZ),
     .     XZS1(MVSIZ), XZS2(MVSIZ), XXM1(MVSIZ), XXM2(MVSIZ),
     .     XYM1(MVSIZ), XYM2(MVSIZ), XZM1(MVSIZ), XZM2(MVSIZ),
     .     VXS1(MVSIZ), VXS2(MVSIZ), VYS1(MVSIZ), VYS2(MVSIZ),
     .     VZS1(MVSIZ), VZS2(MVSIZ), VXM1(MVSIZ), VXM2(MVSIZ),
     .     VYM1(MVSIZ), VYM2(MVSIZ), VZM1(MVSIZ), VZM2(MVSIZ),
     .     MS1(MVSIZ),  MS2(MVSIZ),  MM1(MVSIZ),  MM2(MVSIZ)
      my_real
     .     SURF(3,NRTMDIM), PRES(NRTMDIM)
      my_real
     .     RCURVI(MVSIZ), ANGLMI(MVSIZ), ANGLT, PADM
      my_real
     .    NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .    NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .    NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ),
     .    CMAJ(MVSIZ), PENRAD(MVSIZ),CONDINT(MVSIZ),FNI(MVSIZ),
     .   PHI1(MVSIZ),PHI2(MVSIZ),PHI3(MVSIZ),PHI4(MVSIZ),EFRICT(MVSIZ)
      INTEGER N1(MVSIZ), N2(MVSIZ), M1(MVSIZ), M2(MVSIZ),
     .        NL1(MVSIZ), NL2(MVSIZ),ML1(MVSIZ), ML2(MVSIZ), 
     .        CS_LOC(MVSIZ), CM_LOC(MVSIZ), NSMS(MVSIZ)
      INTEGER ICURV,IMPL_S0
      my_real, DIMENSION(:,:,:), ALLOCATABLE :: FSAVPARIT
C
      NSN   =IPARI(5)
      NTY   =IPARI(7)
      IBC   =IPARI(11)
      IVIS2 =IPARI(14)
      IF(IPARI(33) == 1) RETURN
      NOINT =IPARI(15)
      IGAP  =IPARI(21)
      INACTI=IPARI(22)
      ISECIN=IPARI(28)
      MFROT =IPARI(30)
      IFQ =IPARI(31) 
      IBAG =IPARI(32) 
      IGSTI=IPARI(34)
      NLN   =IPARI(35)
      NISUB =IPARI(36)
      ICURV =IPARI(39)
C adaptive meshing
      IADM =IPARI(44) 

      NRADM=IPARI(49)
      PADM =INTBUF_TAB%VARIABLES(24)
      ANGLT=INTBUF_TAB%VARIABLES(25)
C heat interface
      INTTH = IPARI(47)
      IFORM = IPARI(48)
C      
      STIGLO=-INTBUF_TAB%STFAC(1)
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT > TT)     RETURN
      IF(TT     > STOPT)  RETURN
C  
      FRIC  =INTBUF_TAB%VARIABLES(1)
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      VISC  =INTBUF_TAB%VARIABLES(14)
      VISCF =INTBUF_TAB%VARIABLES(15)
C
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
C
      RSTIF  = INTBUF_TAB%VARIABLES(20)
      FHEAT   = INTBUF_TAB%VARIABLES(21)
      TINT    = INTBUF_TAB%VARIABLES(22)
      FRAD  = ZERO
      DRAD  = ZERO
C----deactive implicit part	  
      IMPL_S0 =0
      IF (IMPL_S0 == 1) THEN
         NUM_IMP = 0
         VISC  =ZERO
         VISCF =ZERO
      ENDIF
      IFUNCTK = 0
      XTHE   = ZERO
      FHEATM   = ZERO
      FHEATS   = ZERO
C----------------------------------------------------------------------
C     NOEUDS/SURFACE
C----------------------------------------------------------------------

c----------------------------------------------------
c   Calcul des normales nodales
c   Courbure quadratique ou Igap/=0 pour solides (GAP=0)
c----------------------------------------------------

      IF(IGAP/=0)THEN
        CALL MY_BARRIER
        IF(JTASK==1)THEN
          ALLOCATE(SOLIDN_NORMAL (3,NUMNOD))
          CALL I20NORMS(IPARI(4),INTBUF_TAB%IRECTM,NUMNOD,X,SOLIDN_NORMAL,
     2           IPARI(6),INTBUF_TAB%MSR,NLN,INTBUF_TAB%NLG,INTBUF_TAB%GAP_SH,
     3           IAD_ELEM,FR_ELEM,INTBUF_TAB%NSV,NSN)
 
          IF(NSPMD > 1)THEN
              LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            CALL SPMD_I20EXCH_N(SOLIDN_NORMAL,IAD_ELEM,FR_ELEM,LENR)
C Communication supplementaire normale sur proc remote en SPMD
C allocation partie non edge sur taille NSNR (nb noeuds secnd remote)
            ALLOCATE(SOLIDN_NORMAL_F (3,IPARI(24)))
C allocation partie edge sur taille NLINR (nb lignes secnd remote)
            ALLOCATE(SOLIDN_NORMAL_FE(3,2*IPARI(57)))
	    CALL SPMD_I20NORMF(
     1 SOLIDN_NORMAL,SOLIDN_NORMAL_F,SOLIDN_NORMAL_FE,NIN     ,IRLEN20 ,
     2 ISLEN20      ,IRLEN20T	    ,ISLEN20T	     ,IRLEN20E,ISLEN20E,
     3 INTBUF_TAB%NSV,INTBUF_TAB%NLG ,INTBUF_TAB%IXLINS   )
          END IF
C il s agit de la barriere matchant celle de i20norm sur tache1
        END IF
        CALL MY_BARRIER()
      ENDIF
c----------------------------------------------------
c   Calcul des normales nodales
c   Courbure quadratique ou Igap/=0 pour solides (GAP=0)
c----------------------------------------------------
      IF(ICURV==3)THEN
        CALL MY_BARRIER()
        IF(JTASK==1)THEN
          ALLOCATE(NOD_NORMAL (3,NUMNOD))
          IF(IPARIT==0)THEN
            CALL I20NORM(IPARI(4),INTBUF_TAB%IRECTM,NUMNOD,X,NOD_NORMAL,
     .                  IPARI(6),INTBUF_TAB%MSR,NLN,INTBUF_TAB%NLG)
cc        CALL MY_BARRIER()
            IF(NSPMD>1)THEN
              LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
              CALL SPMD_EXCH_N(NOD_NORMAL,IAD_ELEM,FR_ELEM,LENR)
            END IF
          ELSE
C Traitement d'assemblage parith/on spmd a optimiser si besoin
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            IF(NSPMD > 1)THEN
	      CALL SPMD_I20CURVSZ(
     1        IPARI(4),INTBUF_TAB%IRECTM,NUMNOD,IAD_ELEM,FR_ELEM,
     2        ISDSIZ	  ,IRCSIZ	,ITAG  ,LENR	,LENT	,
     3        MAXCC	  ,NLN    ,INTBUF_TAB%NLG)
            ELSE
              CALL I20NORMCNT(
     1        IPARI(4),INTBUF_TAB%IRECTM,NUMNOD      ,ITAG    ,LENT   ,
     2        MAXCC       ,NLN     ,INTBUF_TAB%NLG)
            ENDIF
	    CALL I20NORMP(
     1        IPARI(4),INTBUF_TAB%IRECTM,NUMNOD ,X    ,NOD_NORMAL,
     2        IPARI(6),INTBUF_TAB%MSR,LENT   ,MAXCC,ISDSIZ    ,
     3        IRCSIZ  ,IAD_ELEM     ,FR_ELEM,ITAG ,NLN,INTBUF_TAB%NLG)
          END IF
cc        ELSE
cc          CALL MY_BARRIER()
C il s agit de la barriere matchant celle de i20norm sur tache1
        END IF
        CALL MY_BARRIER()
      ENDIF
c----------------------------------------------------
c   Rayon de courbure : calcul des normales nodales (normees)
C   IADM!=0 + Icurv!=0 non available (starter error).
c----------------------------------------------------
      IF(IADM/=0)THEN
         CALL MY_BARRIER()
         IF(JTASK==1)THEN
           ALLOCATE(RCURV(NRTMDIM),ANGLM(NRTMDIM))
           ALLOCATE(NOD_NORMAL (3,NUMNOD))

           IF(IPARIT==0)THEN
	     CALL I20NORMN(
     .         IPARI(4),INTBUF_TAB%IRECTM,NUMNOD,X ,NOD_NORMAL,
     .  		 IPARI(6),INTBUF_TAB%MSR,NLN,INTBUF_TAB%NLG)
cc             CALL MY_BARRIER()
             IF(NSPMD>1)THEN
               LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
               CALL SPMD_EXCH_N(NOD_NORMAL,IAD_ELEM,FR_ELEM,LENR)
             END IF
           ELSE
C Traitement d'assemblage parith/on spmd a optimiser si besoin
             LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
             IF(NSPMD > 1)THEN
	      CALL SPMD_I20CURVSZ(
     1        IPARI(4),INTBUF_TAB%IRECTM,NUMNOD,IAD_ELEM,FR_ELEM,
     2        ISDSIZ	  ,IRCSIZ	,ITAG  ,LENR	,LENT	,
     3        MAXCC	  ,NLN    ,INTBUF_TAB%NLG)
             ELSE
	      CALL I20NORMCNT(
     1        IPARI(4),INTBUF_TAB%IRECTM,NUMNOD	 ,ITAG    ,LENT   ,
     2        MAXCC	  ,NLN     ,INTBUF_TAB%NLG)

             ENDIF
	     CALL I20NORMNP(
     1         IPARI(4),INTBUF_TAB%IRECTM,NUMNOD ,X    ,NOD_NORMAL,
     2         IPARI(6),INTBUF_TAB%MSR,LENT   ,MAXCC,ISDSIZ    ,
     3         IRCSIZ	   ,IAD_ELEM  ,FR_ELEM,ITAG ,NLN,INTBUF_TAB%NLG)

           END IF
cc         ELSE
cc           CALL MY_BARRIER()
C il s agit de la barriere matchant celle de I7NORMN sur tache1
         END IF
         CALL MY_BARRIER()

         NMNFT=1+(JTASK-1)*IPARI(6)/NTHREAD
         NMNLT=JTASK*IPARI(6)/NTHREAD

	  CALL I20NORME(
     .       NMNFT,NMNLT,NOD_NORMAL,INTBUF_TAB%MSR,NLN,INTBUF_TAB%NLG)
          CALL MY_BARRIER()

         NRTMFT=1+(JTASK-1)*IPARI(4)/NTHREAD
         NRTMLT=JTASK*IPARI(4)/NTHREAD
         CALL I20RCURV(NRTMFT, NRTMLT ,X ,NOD_NORMAL ,INTBUF_TAB%IRECTM ,
     .                RCURV , NRADM  ,ANGLM ,ANGLT,NLN,INTBUF_TAB%NLG )
         CALL MY_BARRIER()
      END IF
C----------------------------------------------------
C
      I_STOK = INTBUF_TAB%I_STOK(1)
C decoupage statique
      NB_LOC = I_STOK / NTHREAD
      IF (JTASK==NTHREAD) THEN
        I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
      ELSE
        I_STOK_LOC = NB_LOC
      ENDIF
      DEBUT = (JTASK-1)*NB_LOC

      I_STOK = 0
C
C
C recalcul du istok
C    
      DO I = DEBUT+1, DEBUT+I_STOK_LOC
        IF(INTBUF_TAB%CAND_N(I) < 0) THEN
          I_STOK = I_STOK + 1
          INDEX2(I_STOK) = I
C inbuf == cand_n
          INTBUF_TAB%CAND_N(I) = -INTBUF_TAB%CAND_N(I)
        ENDIF
      ENDDO
c------------------------------------------------
      IF (DEBUG(3)>=1) THEN
          NB_JLT = NB_JLT + I_STOK_LOC
          NB_STOK_N = NB_STOK_N + I_STOK
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
     .           C1='(/INTER/TYPE20)')
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
     .           C1='(/INTER/TYPE20)')
           CALL ARRET(2)
          ENDIF
        ENDIF
c
      DO NFT = 0 , I_STOK - 1 , NVSIZ
c------------------------------------------------
        JLT = MIN( NVSIZ, I_STOK - NFT )
C preparation candidats retenus
        CALL I7CDCOR3(
     1         JLT,INDEX2(NFT+1),INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     2         CAND_E_N,CAND_N_N)
C cand_n et cand_e remplace par cand_n_n et cand_e_n
	CALL I20COR3(
     1  JLT	  ,INTBUF_TAB%XA,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,CAND_E_N,
     2  CAND_N_N  ,INTBUF_TAB%STFM,INTBUF_TAB%STFA,X1      ,X2	     ,
     3  X3	  ,X4	      ,Y1	 ,Y2	       ,Y3	     ,
     4  Y4	  ,Z1	      ,Z2	 ,Z3	       ,Z4	     ,
     5  XI	  ,YI	      ,ZI	 ,STIF         ,IX1	     ,
     6  IX2	  ,IX3        ,IX4	 ,NSVG         ,IGAP	     ,
     7  GAP	  ,INTBUF_TAB%GAP_S,INTBUF_TAB%GAP_M,GAPV    ,GAPR	     ,
     8  MS	  ,VXI        ,VYI	 ,NLN	       ,INTBUF_TAB%NLG,
     9  VZI	  ,MSI        ,NSN	 ,INTBUF_TAB%VA,KINET	     ,
     A  KINI	  ,NTY        ,NIN	 ,IGSTI        ,KMIN	     ,  
     B  KMAX	  ,GAPMAX     ,GAPMIN	 ,IADM         ,RCURV	     ,
     C  RCURVI    ,ANGLM      ,ANGLMI	 ,INTTH        ,TEMP	     ,
     D  TEMPI	  ,PHI        ,INTBUF_TAB%AREAS,INTBUF_TAB%IELEC,AREASI    ,
     E  IELECI    ,INTBUF_TAB%GAP_SH,INTBUF_TAB%STFAC,NODNX_SMS,NSMS	     )
C
        JLT_NEW = 0
C
	CALL I20DST3(
     1 JLT	    ,CAND_N_N	 ,CAND_E_N   ,CN_LOC	,CE_LOC       ,
     2 X1	    ,X2 	 ,X3	     ,X4	,Y1	      ,
     3 Y2	    ,Y3 	 ,Y4	     ,Z1	,Z2	      ,
     4 Z3	    ,Z4 	 ,XI	     ,YI	,ZI	      ,
     5 NX1	    ,NX2	 ,NX3	     ,NX4	,NY1	      ,
     6 NY2	    ,NY3	 ,NY4	     ,NZ1	,NZ2	      ,
     7 NZ3	    ,NZ4	 ,LB1	     ,LB2	,LB3	      ,
     8 LB4	    ,LC1	 ,LC2	     ,LC3	,LC4	      ,
     9 P1	    ,P2 	 ,P3	     ,P4	,IX1	      ,
     A IX2	    ,IX3	 ,IX4	     ,NSVG	,STIF	      ,
     B JLT_NEW  		 ,GAPV       ,INACTI	,SOLIDN_NORMAL,
     C INDEX2(NFT+1),VXI	 ,VYI	     ,GAPR	,INTBUF_TAB%GAP_SH,
     D VZI	    ,MSI	 ,KINI       ,ICURV	,INTBUF_TAB%IRECTM,
     E NNX1	    ,NNX2	 ,NNX3       ,NNX4	,NNY1	      ,
     F NNY2	    ,NNY3	 ,NNY4       ,NNZ1	,NNZ2	      ,
     G NNZ3	    ,NNZ4	 ,NOD_NORMAL ,IADM	,RCURVI       ,
     H ANGLMI	    ,INTTH	 ,TEMPI      ,PHI	,AREASI       ,
     I IELECI	    ,NLN	 ,INTBUF_TAB%NLG,IGAP	,GAPMAX       ,
     J SOLIDN_NORMAL_F  	 ,NSMS    ,INTBUF_TAB%NBINFLG,INTBUF_TAB%GAP_M,
     K CMAJ)
        JLT = JLT_NEW
        IF (IMONM > 0 .AND. JTASK == 1) CALL STARTIME(20,1)
        IF(JLT_NEW/=0) THEN
          IPARI(29) = 1
          IF (DEBUG(3)>=1)NB_JLT_NEW = NB_JLT_NEW + JLT_NEW
C
          IF( INTTH > 0 ) THEN 
             CALL I7THERM(JLT   ,IPARG ,PM  ,IXS   ,IFORM ,X      ,   
     1                    XI    ,YI    ,ZI  ,X1   ,Y1    ,Z1     ,
     2                    X2    ,Y2    ,Z2  ,X3   ,Y3    ,Z3     ,
     3                    X4    ,Y4    ,Z4  ,IX1  ,IX2   ,IX3    ,
     4                    IX4   ,RSTIF ,TEMPI, INTBUF_TAB%IELEC,
     5                    PHI   ,TINT  ,AREASI, IELECI,FRAD,DRAD ,
     6                    GAPV  ,FNI    ,IFUNCTK,XTHE,NPC  ,TF    ,
     7                    CONDINT,PHI1,PHI2  ,PHI3   ,PHI4  ,FHEATS,
     7                    FHEATM,EFRICT,TEMP ,H1     ,H2     ,H3   ,
     8                    H4)   

          ENDIF
C 
          CALL I20FOR3(
     1  JLT          ,A         ,INTBUF_TAB%VA,IBC         ,ICODT    ,
     2  FSAV         ,GAP       ,FRIC         ,MS          ,VISC     ,
     3  VISCF        ,NOINT     ,INTBUF_TAB%STFA,ITAB        ,CN_LOC   ,
     4  STIGLO       ,STIFN     ,STIF         ,FSKYI       ,ISKY     ,
     5  NX1          ,NX2       ,NX3          ,NX4         ,NY1      ,
     6  NY2          ,NY3       ,NY4          ,NZ1         ,NZ2      ,
     7  NZ3          ,NZ4       ,LB1          ,LB2         ,LB3      ,
     8  LB4          ,LC1       ,LC2          ,LC3         ,LC4      ,
     9  P1           ,P2        ,P3           ,P4          ,FCONT    ,
     B  IX1          ,IX2       ,IX3          ,IX4         ,NSVG     ,
     C  IVIS2        ,NELTST    ,ITYPTST      ,DT2T        ,
     D  GAPV         ,INACTI    ,INDEX2(NFT+1),NISKYFI ,
     E  KINET        ,NEWFRONT  ,ISECIN       ,NSTRF       ,SECFCUM  ,
     F  X            ,INTBUF_TAB%XA,CE_LOC    ,MFROT       ,IFQ      ,
     G  INTBUF_TAB%FRIC_P,INTBUF_TAB%CAND_FX,INTBUF_TAB%CAND_FY,INTBUF_TAB%CAND_FZ,
     +                                                   INTBUF_TAB%XFILTR,
     H  INTBUF_TAB%IFPEN,GAPR,INTBUF_TAB%AVX_ANCR      ,NLN      ,INTBUF_TAB%NLG,
     I  IBAG         ,ICONTACT  ,INTBUF_TAB%NSV,INTBUF_TAB%PENIS,
     +                                                   INTBUF_TAB%PENIM,       
     J  VISCN        ,VXI       ,VYI          ,VZI         ,MSI      ,
     K  KINI         ,NIN       ,NISUB   ,INTBUF_TAB%LISUB,INTBUF_TAB%ADDSUBS,
     L  INTBUF_TAB%ADDSUBM,INTBUF_TAB%LISUBS,INTBUF_TAB%LISUBM,FSAVSUB,INTBUF_TAB%CAND_N,
     M  IPARI(33)    ,IPARI(39) ,NOD_NORMAL  ,FNCONT     ,FTCONT   ,
     N  X1	     ,X2        ,X3          ,X4         ,Y1	     ,
     O  Y2	     ,Y3        ,Y4          ,Z1         ,Z2	     ,
     P  Z3	     ,Z4        ,XI          ,YI         ,ZI	     ,
     Q  IADM         ,RCURVI    ,RCONTACT    ,ACONTACT   ,PCONTACT   ,
     R  ANGLMI       ,PADM      ,INTTH       , PHI       , FTHE      ,
     S  FTHESKYI     ,INTBUF_TAB%DAANC6,TEMP     ,TEMPI      ,RSTIF      ,
     T  IFORM        ,INTBUF_TAB%GAP_S,IGAP     ,INTBUF_TAB%ALPHAK,MSKYI_SMS,
     U  ISKYI_SMS    ,NSMS      ,CMAJ        ,JTASK      ,ISENSINT   ,
     V  FSAVPARIT    ,NFT       ,H3D_DATA    )
C
        ENDIF
        IF (IMONM > 0 .AND. JTASK == 1) CALL STOPTIME(20,1)

C
        IF(IMPL_S0 == 1) THEN
          DO I = 1 ,JLT_NEW
            NS_IMP(I+NUM_IMP)=CN_LOC(I)
            NE_IMP(I+NUM_IMP)=CE_LOC(I)
            IND_IMP(I+NUM_IMP)=INDEX2(I+NFT)
          ENDDO
          NUM_IMP=NUM_IMP+JLT_NEW
        ENDIF
C
      ENDDO
c
      IF (SFSAVPARIT /= 0)THEN
          CALL SUM_6_FLOAT_SENS(FSAVPARIT, NISUB+1, 11, I_STOK,1,I_STOK,
     .  			FBSAV6, 12, 6, DIMFB, ISENSINT )
      ENDIF
      IF(ALLOCATED(FSAVPARIT)) DEALLOCATE (FSAVPARIT)
C----------------------------------------------------------------------
C     2- EDGES
C----------------------------------------------------------------------
      NLINSA =IPARI(53)
      IF(NLINSA /= 0)THEN

        NLINS  =IPARI(51)
        NLINM  =IPARI(52)
        NLINSA =IPARI(53)
        NLINMA =IPARI(54)
        NSNE   =IPARI(55)
        NMNE   =IPARI(56)

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
        DO I = DEBUT+1, DEBUT+I_STOK_LOC
          IF(INTBUF_TAB%LCAND_S(I) < 0) THEN
            I_STOK = I_STOK + 1
            INDEX2(I_STOK) = I
C           inbuf == cand_S
            INTBUF_TAB%LCAND_S(I) = -INTBUF_TAB%LCAND_S(I)
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
     .           C1='(/INTER/TYPE20)')
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
     .           C1='(/INTER/TYPE20)')
           CALL ARRET(2)
          ENDIF
        ENDIF
C
        DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C         preparation candidats retenus
	  CALL I11CDCOR3(
     1       JLT,INDEX2(NFT+1),INTBUF_TAB%LCAND_N,INTBUF_TAB%LCAND_S,CM_LOC,
     2       CS_LOC)
	  CALL I20COR3E(
     1 JLT     ,INTBUF_TAB%IXLINS,INTBUF_TAB%IXLINM,INTBUF_TAB%XA,INTBUF_TAB%VA,
     2 CS_LOC	    ,CM_LOC	  ,INTBUF_TAB%STFS,INTBUF_TAB%STF,GAPMIN  ,
     3 INTBUF_TAB%GAP_SE,INTBUF_TAB%GAP_ME,IGAP 	,GAPV	      ,MS      ,
     4 STIF	    ,XXS1	  ,XXS2 	,XYS1	      ,XYS2    ,
     5 XZS1	    ,XZS2	  ,XXM1 	,XXM2	      ,XYM1    ,
     6 XYM2	    ,XZM1	  ,XZM2 	,VXS1	      ,VXS2    ,
     7 VYS1	    ,VYS2	  ,VZS1 	,VZS2	      ,VXM1    ,
     8 VXM2	    ,VYM1	  ,VYM2 	,VZM1	      ,VZM2    ,
     9 MS1	    ,MS2	  ,MM1  	,MM2	      ,N1      ,
     A N2	    ,M1 	  ,M2		,NLINSA       ,NIN     ,
     B NL1	    ,NL2	  ,ML1  	,ML2	 ,INTBUF_TAB%NLG,
     C INTBUF_TAB%STFAC,NODNX_SMS    ,NSMS 	)

	  CALL I20DST3E(
     1      JLT    ,CS_LOC,CM_LOC ,HS1   ,HS2	,
     2      HM1    ,HM2   ,NX	  ,NY	 ,NZ	,
     3      STIF   ,N1    ,N2	  ,M1	 ,M2	,
     4      JLT_NEW,XXS1  ,XXS2   ,XYS1  ,XYS2  ,
     5      XZS1   ,XZS2  ,XXM1   ,XXM2  ,XYM1  ,
     6      XYM2   ,XZM1  ,XZM2   ,VXS1  ,VXS2  ,
     7      VYS1   ,VYS2  ,VZS1   ,VZS2  ,VXM1  ,
     8      VXM2   ,VYM1  ,VYM2   ,VZM1  ,VZM2  ,
     9      MS1    ,MS2   ,MM1    ,MM2   ,GAPV  ,
     A      NL1    ,NL2   ,ML1    ,ML2   ,IGAP  ,
     B      SOLIDN_NORMAL,INTBUF_TAB%GAP_SE,INTBUF_TAB%GAP_ME,NLINSA,
     C      SOLIDN_NORMAL_FE,NSMS)
          JLT = JLT_NEW
          IF (IMONM > 0 .AND. JTASK == 1) CALL STARTIME(20,1)
          IF(JLT_NEW/=0) THEN
            IPARI(29) = 1
            IF (DEBUG(3)>=1)
     .        NB_JLT_NEW = NB_JLT_NEW + JLT_NEW
            CALL I20FOR3E(
     1 JLT          ,A            ,V            ,IBC        ,ICODT    ,
     2 FSAV         ,GAP          ,FRIC         ,MS         ,VISC     ,
     3 VISCF        ,NOINT        ,ITAB         ,CS_LOC     ,CM_LOC   ,
     4 STIGLO       ,STIFN        ,STIF         ,FSKYI      ,ISKY     ,
     5 FCONT        ,INTBUF_TAB%STFS,INTBUF_TAB%STF,DT2T       ,HS1      ,
     6 HS2          ,HM1          ,HM2          ,N1         ,N2       ,
     7 M1           ,M2           ,IVIS2        ,NELTST     ,ITYPTST  ,
     8 NX           ,NY           ,NZ           ,GAPV   ,INTBUF_TAB%PENISE,
     9 INTBUF_TAB%PENIME,IPARI(22)    ,NISKYFIE     ,NEWFRONT   ,ISECIN   ,
     A NSTRF        ,SECFCUM      ,VISCN        ,NLINSA     ,MS1      ,
     B MS2          ,MM1          ,MM2          ,VXS1       ,VYS1     ,
     C VZS1         ,VXS2         ,VYS2         ,VZS2       ,VXM1     ,
     D VYM1         ,VZM1         ,VXM2         ,VYM2       ,VZM2     ,
     E NIN          ,NL1          ,NL2          ,ML1        ,ML2      ,
     F INTBUF_TAB%DAANC6,INTBUF_TAB%ALPHAK,MSKYI_SMS    ,ISKYI_SMS  ,NSMS,
     G JTASK        ,ISENSINT   ,FSAVPARIT      ,NISUB      ,NFT      ,
     H H3D_DATA     )

          ENDIF
          IF (IMONM > 0 .AND. JTASK == 1) CALL STOPTIME(20,1)
          IF(IMPL_S0==1) THEN
            DO I = 1 ,JLT_NEW
             NS_IMP(I+NUM_IMP)=CS_LOC(I)
             NE_IMP(I+NUM_IMP)=CM_LOC(I)
            ENDDO
            NUM_IMP=NUM_IMP+JLT_NEW
          ENDIF
        ENDDO
        IF (SFSAVPARIT /= 0)THEN
            CALL SUM_6_FLOAT_SENS(FSAVPARIT, NISUB+1, 11, I_STOK,1,I_STOK,
     .                            FBSAV6, 12, 6, DIMFB, ISENSINT )
        ENDIF
        IF(ALLOCATED(FSAVPARIT)) DEALLOCATE (FSAVPARIT)
      ENDIF

C----------------------------------------------------------------------
C     3- FORCES entre NOEUD SECOND. et NOEUD D'ANCRAGE
C----------------------------------------------------------------------
      CALL MY_BARRIER
C----------------------------------------------------------------------
C     NOEUDS secnd,main,edge
C----------------------------------------------------------------------
      IF (IMONM > 0 .AND. JTASK == 1) CALL STARTIME(20,1)
      NLNFT1= (JTASK-1)*NLN/NTHREAD
      NLNLT = JTASK*NLN/NTHREAD
      NLNL  = NLNLT - NLNFT1
      CALL I20FOR3C(
     1      NLNL   ,INTBUF_TAB%NLG(1+NLNFT1),MS  ,INTBUF_TAB%AVX_ANCR(1+3*NLNFT1),
     2      INTBUF_TAB%AVX_ANCR(1+3*NLN+3*NLNFT1),INTBUF_TAB%STFA(1+NLNFT1),WEIGHT,INACTI,
     3      INTBUF_TAB%DAANC6(1+18*2*NLNFT1),INTBUF_TAB%STFAC(1),
     3      INTBUF_TAB%PENIA(1+5*NLNFT1),INTBUF_TAB%ALPHAK(1+3*NLNFT1),
     4      INTBUF_TAB%AVX_ANCR(1+6*NLN+3*NLNFT1),KMIN) 

      IF (IMONM > 0 .AND. JTASK == 1) CALL STOPTIME(20,1)
      IF(IGAP/=0)THEN
        CALL MY_BARRIER
        IF(JTASK == 1) THEN
          DEALLOCATE(SOLIDN_NORMAL)
          IF(NSPMD > 1) THEN
            DEALLOCATE(SOLIDN_NORMAL_F)
            DEALLOCATE(SOLIDN_NORMAL_FE)
          END IF
        END IF
      END IF
      IF(ICURV==3.OR.IADM/=0)THEN
          CALL MY_BARRIER()
          IF(JTASK == 1)DEALLOCATE(NOD_NORMAL)
      END IF
      IF(IADM/=0)THEN
          CALL MY_BARRIER()
          IF(JTASK == 1)DEALLOCATE(RCURV,ANGLM)
      END IF
C-----------
      RETURN
      END
