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
Chd|  I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|-- called by -----------
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I20BUC_EDGE                   source/interfaces/intsort/i20buce.F
Chd|        I20XSAVE                      source/interfaces/intsort/i20xsave.F
Chd|        I20XSINIR                     source/interfaces/intsort/i20main_tri.F
Chd|        I7TRC                         source/interfaces/intsort/i7trc.F
Chd|        IMP_RNUMCD                    source/implicit/imp_int_k.F   
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_GET_INACTI7              source/mpi/interfaces/send_cand.F
Chd|        SPMD_RNUMCD20                 source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_TRI20BOX                 source/mpi/interfaces/spmd_tri20box.F
Chd|        SPMD_TRI20BOXE                source/mpi/interfaces/spmd_tri20boxe.F
Chd|        SPMD_TRI20GATE                source/mpi/interfaces/spmd_i7crit.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I20MAIN_TRI(
     1                  IPARI   ,X       ,V       ,
     2                  MS      ,NIN     ,ITASK   ,MWAG    ,WEIGHT  ,
     3                  ISENDTO ,IRCVFROM,RETRI   ,IAD_ELEM,FR_ELEM ,
     4                  ITAB    ,KINET   ,TEMP    ,NRTM_T  ,RENUM   ,
     5                  NSNFIOLD,ESHIFT  ,NUM_IMP ,IND_IMP ,DIAG_SMS,
     6                  NODNX_SMS,INTBUF_TAB,H3D_DATA)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFMOD
      USE INTBUFDEF_MOD
      USE H3D_MOD
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
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "scr05_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
      COMMON /I20MAINC/BMINMA,BMINMAE,CURV_MAX_MAX,
     .                 RESULT,NSNR,NSNROLD,NLINSR,I_MEMG
      INTEGER RESULT,NSNR,NSNROLD,NLINSR,I_MEMG
      my_real 
     .        BMINMA(6),BMINMAE(6),CURV_MAX_MAX
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN ,ITASK, RETRI,NRTM_T,ESHIFT,
     .        NUM_IMP ,IND_IMP(*),
     .        ITAB(*), KINET(*),
     .        IPARI(NPARI,NINTER),MWAG(*),
     .        ISENDTO(NINTER+1,*),IRCVFROM(NINTER+1,*),
     .        WEIGHT(*), IAD_ELEM(2,*) ,FR_ELEM(*),
     .        RENUM(NUMNOD), NSNFIOLD(NSPMD), NODNX_SMS(*)
C     REAL ou REAL*8
      my_real 
     .   X(*), V(*), MS(*),TEMP(*),DIAG_SMS(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISYM,I, IP0, IP1, IP2, IP21, K11_T, I_SK_OLD, I_STOK1,IEDGE,  
     .        ADD1, NB_N_B, NOINT, INACTI, MULTIMP, IGAP, IFQ, ITIED
      INTEGER 
     .       ILD, NCONT, NCONTACT,NCONTE,NCONTACTE,
     .       INACTII,INACIMP,NSNF,NSNL,NLN,CAND_N_OLD,
     .       I_MEM
C     REAL ou REAL*8
      my_real
     .   STARTT,GAP,MAXBOX,MINBOX,STOPT,DIST,TZINF,
     .   XMAXL, YMAXL, ZMAXL, XMINL, YMINL, ZMINL, GAPMIN, GAPMAX,
     .   XMAXEL, YMAXEL, ZMAXEL, XMINEL, YMINEL, ZMINEL, C_MAXL,
     .   CURV_MAX(NRTM_T),GAP_SHIFT,RBID
C-----------------------------------------------
#include "sync_dir.inc"
C=======================================================================
      I_MEM = 0
      I_MEMG = 0
C
      NRTM   =IPARI(4,NIN)
      NSN    =IPARI(5,NIN)
      NMN    =IPARI(6,NIN)
      NTY    =IPARI(7,NIN)
      NOINT  =IPARI(15,NIN)
      NLN    =IPARI(35,NIN)

      ISYM  = IPARI(43,NIN)
      NLINS  =IPARI(51,NIN)
      NLINM  =IPARI(52,NIN)
      NLINSA =IPARI(53,NIN)
      NLINMA =IPARI(54,NIN)
      NSNE   =IPARI(55,NIN)
      NMNE   =IPARI(56,NIN)
      IGAP   =IPARI(21,NIN)
      IEDGE  =IPARI(58,NIN)
      NCONT  =IPARI(18,NIN)
      NCONTE = NCONT

      INACTI =IPARI(22,NIN)
      MULTIMP=IPARI(23,NIN)
      IFQ=IPARI(31,NIN)

      NCONTACT=MULTIMP*NCONT
      NCONTACTE=MULTIMP*NCONTE
C
C     Specific TYPE7 :
      ITIED = 0 
C
      IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7.OR.IFQ.GT.0.OR.
     .   NUM_IMP.GT.0)THEN
        NSNROLD = IPARI(24,NIN)
      ELSE
        NSNROLD = 0
      ENDIF
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
      GAP =INTBUF_TAB%VARIABLES(2)

      GAPMIN=INTBUF_TAB%VARIABLES(13)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      DIST = INTBUF_TAB%VARIABLES(5)
C
C -------------------------------------------------------------
C
      IF (DIST.GT.ZERO) THEN
C Appel routine calcul XS VS remote stocke dans XFI et VFI + partie edge
        IF (NSPMD.GT.1)THEN
	  CALL I20XSINIR(
     1      IPARI(24,NIN),IPARI(57,NIN),ITASK,NIN,INTBUF_TAB%STFAC(1))
        ENDIF
        RETURN
C Si retri XFI et VFI transferes directement au niveau de i20buce/SPMD_TRI20BOX
      END IF
      RETRI=1 
C -------------------------------------------------------------
C
      MAXBOX = INTBUF_TAB%VARIABLES(9)
      MINBOX = INTBUF_TAB%VARIABLES(12)
      TZINF  = INTBUF_TAB%VARIABLES(8)
      BMINMA(1)=-EP30
      BMINMA(2)=-EP30
      BMINMA(3)=-EP30
      BMINMA(4)=EP30
      BMINMA(5)=EP30
      BMINMA(6)=EP30
      CURV_MAX_MAX = ZERO
      BMINMAE(1)=-EP30
      BMINMAE(2)=-EP30
      BMINMAE(3)=-EP30
      BMINMAE(4)=EP30
      BMINMAE(5)=EP30
      BMINMAE(6)=EP30
C -------------------------------------------------------------
C     STOCKAGE DES ANCIENS CANDIDATS AVEC PENE INITIALE
C     OU AVEC DU FILTRAGE DE FROTTEMENT
C -------------------------------------------------------------
C
C Barriere dans tous les cas pour bminma [et cur_max_max]
C
      CALL MY_BARRIER

      IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.IFQ.GT.0.OR.NUM_IMP.GT.0.OR.
     .   NUM_IMP.GT.0)THEN
        IF(ITASK.EQ.0)THEN
          INACTII=INACTI
          IF (NUM_IMP.GT.0.AND.
     .        (INACTI.NE.5.AND.INACTI.NE.6.AND.IFQ.LE.0)) THEN
	   INACIMP = 0
	  ELSE
	   INACIMP = 1
	  ENDIF
          IP0 = 1

          IP1 = IP0 + NSN + NSNROLD + 3

          I_SK_OLD = INTBUF_TAB%I_STOK(1)
	  CALL I7TRC(
     1    NSN+NSNROLD      ,I_SK_OLD          ,INTBUF_TAB%CAND_N ,INTBUF_TAB%CAND_E ,
     2    INTBUF_TAB%CAND_P,INTBUF_TAB%CAND_FX,INTBUF_TAB%CAND_FY,INTBUF_TAB%CAND_FZ,
     3    MWAG(IP0)        ,INTBUF_TAB%IFPEN  ,INACTI	         ,IFQ 	            ,
     4    NUM_IMP          ,IND_IMP           ,INTBUF_TAB%STFA  ,NIN               ,
     5    NSN              ,ITIED             ,RBID)

ctmp          IF(I_SK_OLD.EQ.0)INACTI=0
          INTBUF_TAB%I_STOK(1)=I_SK_OLD
          IF(INACIMP.GT.0)THEN
            IF (NSPMD.GT.1) THEN
              CALL SPMD_GET_INACTI7(INACTI,IPARI(22,NIN),NIN,ISENDTO,
     .                              IRCVFROM,INACTII)
            ELSE
              IPARI(22,NIN) = INACTI
            ENDIF
          ENDIF
        ENDIF
      ELSE
        I_SK_OLD=0
        INTBUF_TAB%I_STOK(1)=ZERO
      ENDIF
C -------------------------------------------------------------
C     CALCUL BORNE DOMAINE REMONTE DANS I7XSAVE
C -------------------------------------------------------------
C eshift : decalage sur cand_e
C sauvegarde de XSAV (tableau BUFIN(JD(19)))
      CALL I20XSAVE(
     1 ITASK	    ,INTBUF_TAB%XA,NTY  	,NSN	      ,
     2 NMN	    ,NSNE	  ,NMNE 	,NLN	      ,
     3 INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%XSAV,
     4 INTBUF_TAB%NSVL,INTBUF_TAB%MSRL,INTBUF_TAB%CRITX,
     5 XMINL	    ,YMINL	  ,ZMINL	,XMAXL        ,
     6 YMAXL	    ,ZMAXL	  ,C_MAXL	,CURV_MAX     ,
     7 IPARI(39,NIN),INTBUF_TAB%IRECTM(1+4*ESHIFT) ,NRTM_T,XMINEL ,   
     8 YMINEL	    ,ZMINEL	  ,XMAXEL	, YMAXEL      ,
     9 ZMAXEL	    )
#include "lockon.inc"
      BMINMA(1) = MAX(BMINMA(1),XMAXL)
      BMINMA(2) = MAX(BMINMA(2),YMAXL)
      BMINMA(3) = MAX(BMINMA(3),ZMAXL)
      BMINMA(4) = MIN(BMINMA(4),XMINL)
      BMINMA(5) = MIN(BMINMA(5),YMINL)
      BMINMA(6) = MIN(BMINMA(6),ZMINL)
      CURV_MAX_MAX = MAX(CURV_MAX_MAX,C_MAXL)
#include "lockoff.inc"

C ----------------------------------------------------------
C     NOEUDS SURFACE
C ----------------------------------------------------------
      RESULT = 0
C BARRIER II_STOK et RESULT
      CALL MY_BARRIER
      INACTI = IPARI(22,NIN)
      IF(ITASK.EQ.0)THEN
        IF(ABS(BMINMA(6)-BMINMA(3)).GT.2*EP30.OR.
     +     ABS(BMINMA(5)-BMINMA(2)).GT.2*EP30.OR.
     +     ABS(BMINMA(4)-BMINMA(1)).GT.2*EP30)THEN
          CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .                I1=NOINT,C1='(I20BUCE)')
          CALL ARRET(2)
        END IF

        BMINMA(1)=BMINMA(1)+TZINF+CURV_MAX_MAX
        BMINMA(2)=BMINMA(2)+TZINF+CURV_MAX_MAX
        BMINMA(3)=BMINMA(3)+TZINF+CURV_MAX_MAX
        BMINMA(4)=BMINMA(4)-TZINF-CURV_MAX_MAX
        BMINMA(5)=BMINMA(5)-TZINF-CURV_MAX_MAX
        BMINMA(6)=BMINMA(6)-TZINF-CURV_MAX_MAX

        NSNR = 0
        IF(NSPMD.GT.1) THEN
C
C recuperation des noeuds remote NSNR stockes dans XREM
C
	  CALL SPMD_TRI20BOX(
     1      INTBUF_TAB%NSV,NSN	  ,INTBUF_TAB%XA,INTBUF_TAB%VA,MS     ,
     2      BMINMA	 ,WEIGHT  ,INTBUF_TAB%STFA,NIN	      ,ISENDTO,
     3      IRCVFROM	 ,IAD_ELEM,FR_ELEM	,NSNR	      ,IGAP   ,
     4      INTBUF_TAB%GAP_S,ITAB    ,KINET	,IFQ ,IPARI(22,NIN)   ,
     5      NSNFIOLD,IPARI(47,NIN),INTBUF_TAB%IELEC,INTBUF_TAB%AREAS,TEMP   ,
     6      NUM_IMP	 ,INTBUF_TAB%NLG,INTBUF_TAB%PENIS,INTBUF_TAB%PENIA   ,
     +  					      DIAG_SMS        ,
     7      NODNX_SMS	 ,INTBUF_TAB%NBINFLG,INTBUF_TAB%AVX_ANCR(1),INTBUF_TAB%AVX_ANCR(1+3*NLN) )
C
C renumerotation locale des anciens candidats
C

          IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7.OR.IFQ.GT.0
     .        .OR.NUM_IMP.GT.0)THEN

	    CALL SPMD_RNUMCD20(INTBUF_TAB%CAND_N,RENUM   ,INTBUF_TAB%I_STOK(1),
     1  		     NIN, NSN	  ,NSNFIOLD,NSNROLD	   )
          END IF
        END IF
      END IF

      CAND_N_OLD = INTBUF_TAB%I_STOK(1)
 40   Continue
C il faut conserver K11 global
      ILD = 0
      NB_N_B = 1
C
C Barrier comm spmd_tri20box + BMINMA + Retour I20BUCE
C      
 50   CALL MY_BARRIER

C
      IF (IMONM > 0) CALL STARTIME(30,1)
      IF(NRTM_T.NE.0)THEN
	CALL I20BUCE(
     1   INTBUF_TAB%XA,INTBUF_TAB%IRECTM(1+4*ESHIFT),INTBUF_TAB%NSV,IPARI(22,NIN),
     1  						 INTBUF_TAB%CAND_P,
     2   NMN   ,NRTM_T      ,NSN	  ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     3   GAP   ,NOINT	    ,INTBUF_TAB%I_STOK(1) ,TZINF      ,MAXBOX	    ,
     4   MINBOX,MWAG	    ,CURV_MAX	  ,NCONTACT	,BMINMA       ,
     5   NB_N_B,ESHIFT      ,ILD	  ,IFQ  	,INTBUF_TAB%IFPEN,
     6   INTBUF_TAB%STFA,NIN  ,INTBUF_TAB%STFM(1+ESHIFT) ,IGAP 	,INTBUF_TAB%GAP_S,
     7   NSNR  ,NCONT	    ,RENUM	  ,NSNROLD	,INTBUF_TAB%GAP_M(1+ESHIFT),
     8   GAPMIN,GAPMAX      ,NUM_IMP      ,NLN          ,INTBUF_TAB%NLG,
     9   INTBUF_TAB%GAP_SH,INTBUF_TAB%NBINFLG,INTBUF_TAB%MBINFLG,ISYM ,I_MEM     )

      ENDIF

C Upgrade MultiMP
        IF (I_MEM == 2 )THEN
#include "lockon.inc"
          I_MEMG = I_MEM
#include "lockoff.inc"
        ENDIF
C New barrier needed for Dynamic MultiMP
        CALL MY_BARRIER

        IF(I_MEMG /=0)THEN
C CARE : JINBUF & JBUFIN array are reallocated in
C        UPGRADE_MULTIMP routine !!!!
!$OMP SINGLE
           MULTIMP = IPARI(23,NIN) + 4
           CALL UPGRADE_MULTIMP(NIN,MULTIMP,INTBUF_TAB)
!$OMP END SINGLE
           I_MEM = 0
           I_MEMG = 0
           INTBUF_TAB%I_STOK(1)=CAND_N_OLD
           MULTIMP=IPARI(23,NIN)
           NCONTACT=MULTIMP*NCONT
           NCONTACTE=MULTIMP*NCONTE
           GOTO 40
        ENDIF


      IF (IMONM > 0) CALL STOPTIME(30,1)
C
#include "lockon.inc"
      INTBUF_TAB%VARIABLES(9)  = MIN(MAXBOX,INTBUF_TAB%VARIABLES(9))
      INTBUF_TAB%VARIABLES(12) = MIN(MINBOX,INTBUF_TAB%VARIABLES(12))
      INTBUF_TAB%VARIABLES(8)  = MIN(TZINF,INTBUF_TAB%VARIABLES(8))
      INTBUF_TAB%VARIABLES(5)  = INTBUF_TAB%VARIABLES(8)-GAP
      RESULT           = RESULT + ILD
#include "lockoff.inc"
C--------------------------------------------------------------
C--------------------------------------------------------------
      CALL MY_BARRIER
      IF (RESULT.NE.0) THEN
        CALL MY_BARRIER
        IF (ITASK.EQ.0) THEN
          INTBUF_TAB%I_STOK(1) = I_SK_OLD
          RESULT = 0
        ENDIF
        CALL MY_BARRIER
        ILD  = 0
        MAXBOX = INTBUF_TAB%VARIABLES(9)
        MINBOX = INTBUF_TAB%VARIABLES(12)
        TZINF  = INTBUF_TAB%VARIABLES(8)
        GOTO 50
      ENDIF
C mise a - de dist temporairement pour reperage dans partie frontiere
      IF(NSPMD.GT.1)THEN
C mono tache
!$OMP SINGLE
        IF (IMONM > 0) CALL STARTIME(26,1)
        INTBUF_TAB%VARIABLES(5) = -INTBUF_TAB%VARIABLES(5)

	CALL SPMD_TRI20GAT(
     1    RESULT       ,NSN ,INTBUF_TAB%CAND_N,INTBUF_TAB%I_STOK(1),NIN,
     2    IGAP         ,NSNR,MULTIMP	  ,NTY        ,IPARI(47,NIN),
     3    IPARI(22,NIN),H3D_DATA  )
        IPARI(24,NIN) = NSNR

        IF (NUM_IMP.GT.0) 
     .    CALL IMP_RNUMCD(INTBUF_TAB%CAND_N,NIN,NSN,NUM_IMP,IND_IMP )

        IF (IMONM > 0) CALL STOPTIME(26,1)

!$OMP END SINGLE
      ENDIF

C-----------------------------------------------------------
C     EDGES
C-----------------------------------------------------------

      I_SK_OLD=0
      IF(NLINMA /= 0.OR.NSPMD.GT.1)THEN
        INTBUF_TAB%I_STOK_E(1) = 0
C
C     Fin CALCUL BORNE DOMAINE Edge
C
#include "lockon.inc"
        BMINMAE(1) = MAX(BMINMAE(1),XMAXEL)
        BMINMAE(2) = MAX(BMINMAE(2),YMAXEL)
        BMINMAE(3) = MAX(BMINMAE(3),ZMAXEL)
        BMINMAE(4) = MIN(BMINMAE(4),XMINEL)
        BMINMAE(5) = MIN(BMINMAE(5),YMINEL)
        BMINMAE(6) = MIN(BMINMAE(6),ZMINEL)
#include "lockoff.inc"
        RESULT = 0
C BARRIER II_STOK et RESULT
        CALL MY_BARRIER
        IF(ITASK.EQ.0)THEN
          IF(ABS(BMINMAE(6)-BMINMAE(3)).GT.2*EP30.OR.
     +       ABS(BMINMAE(5)-BMINMAE(2)).GT.2*EP30.OR.
     +       ABS(BMINMAE(4)-BMINMAE(1)).GT.2*EP30)THEN
#include "lockon.inc"
            CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .                  I1=NOINT,C1='(I20BUCE)')
#include "lockoff.inc"
            CALL ARRET(2)
          END IF
          BMINMAE(1)=BMINMAE(1)+TZINF
          BMINMAE(2)=BMINMAE(2)+TZINF
          BMINMAE(3)=BMINMAE(3)+TZINF
          BMINMAE(4)=BMINMAE(4)-TZINF
          BMINMAE(5)=BMINMAE(5)-TZINF
          BMINMAE(6)=BMINMAE(6)-TZINF
C
C recuperation des noeuds remote NLINSR stockes dans XREM
C
          NLINSR = 0
          IF(NSPMD.GT.1) THEN
	    CALL SPMD_TRI20BOXE(
     1       INTBUF_TAB%IXLINS,NLINSA  ,INTBUF_TAB%XA,INTBUF_TAB%VA,MS     ,
     2       BMINMAE	  ,WEIGHT  ,INTBUF_TAB%STFS,NIN	       ,ISENDTO,
     3       IRCVFROM	  ,IAD_ELEM,FR_ELEM	 ,NLINSR ,IPARI(22,NIN),
     4       INTBUF_TAB%GAP_SE,INTBUF_TAB%PENISE,ITAB	 ,IGAP   ,TZINF  ,
     5       INTBUF_TAB%NLG,INTBUF_TAB%PENIA,DIAG_SMS,NODNX_SMS)
          END IF
        END IF
C -------------------------------------------------------------
        CAND_N_OLD = INTBUF_TAB%I_STOK_E(1)
 140    CONTINUE
        NRTM_T = NLINMA/NTHREAD
        ESHIFT = ITASK*NRTM_T
        IF(ITASK.EQ.NTHREAD-1)NRTM_T=NLINMA-(NTHREAD-1)*(NLINMA/NTHREAD)
        ILD = 0
C
C Barrier comm spmd_tri7box + BMINMA + Retour I7BUCE
C      
150     CALL MY_BARRIER
        GAP_SHIFT=ZERO
        IF(IGAP/=0)THEN
          GAP_SHIFT= GAP
          GAP   = GAP   + GAP_SHIFT
c fait dans le starter          TZINF = TZINF + GAP_SH
        ENDIF
        IF(NRTM_T.NE.0)THEN
          CALL I20BUC_EDGE(
     1 INTBUF_TAB%XA,INTBUF_TAB%IXLINS,INTBUF_TAB%IXLINM(1+2*ITASK*NRTM_T),INTBUF_TAB%NLG,
     2 NLINSA  ,NMNE	     ,NRTM_T	   ,INTBUF_TAB%LCAND_N,INTBUF_TAB%LCAND_S,
     3 GAP     ,NOINT	     ,INTBUF_TAB%I_STOK_E(1),BMINMAE   ,TZINF	   ,
     4 MAXBOX  ,MINBOX       ,NB_N_B	   , ESHIFT	 ,ILD	       ,
     6 NCONTACTE,INTBUF_TAB%ADCCM20(1+ITASK*NRTM_T) ,INTBUF_TAB%CHAIN20,NIN	 ,ITAB    ,
     7 NLINSR	,NCONT      ,INTBUF_TAB%GAP_SE,INTBUF_TAB%STFS,INTBUF_TAB%PENISE ,
     8 IGAP	,INTBUF_TAB%STF(1+ITASK*NRTM_T),IPARI(42,NIN) , I_MEM	    )
       ENDIF

C Upgrade MultiMP
           IF (I_MEM == 1 .OR. I_MEM == 2)THEN
#include "lockon.inc"
             I_MEMG = I_MEM
#include "lockoff.inc"
           ENDIF

C New barrier needed for Dynamic MultiMP
           CALL MY_BARRIER

           IF(I_MEMG /=0)THEN
C CARE : JINBUF & JBUFIN array are reallocated in
C        UPGRADE_MULTIMP routine !!!!
!$OMP SINGLE
             MULTIMP = IPARI(23,NIN) + 4
             CALL UPGRADE_MULTIMP(NIN,MULTIMP,INTBUF_TAB)
!$OMP END SINGLE
             I_MEMG = 0
             I_MEM = 0
             INTBUF_TAB%I_STOK_E(1)=CAND_N_OLD
             MULTIMP=IPARI(23,NIN)
             NCONTACT=MULTIMP*NCONT
             NCONTACTE=MULTIMP*NCONTE
             GOTO 140
           ENDIF

#include "lockon.inc"
        INTBUF_TAB%VARIABLES(9)  = MIN(MAXBOX,INTBUF_TAB%VARIABLES(9))
        INTBUF_TAB%VARIABLES(12) = MIN(MINBOX,INTBUF_TAB%VARIABLES(12))
        INTBUF_TAB%VARIABLES(8)  = MIN(TZINF,INTBUF_TAB%VARIABLES(8))
        INTBUF_TAB%VARIABLES(5)  = INTBUF_TAB%VARIABLES(8)-GAP
        RESULT           = RESULT + ILD
#include "lockoff.inc"
C--------------------------------------------------------------
C--------------------------------------------------------------
        CALL MY_BARRIER
        IF (RESULT.NE.0) THEN
          CALL MY_BARRIER
          IF (ITASK.EQ.0) THEN
            INTBUF_TAB%I_STOK_E(1) = I_SK_OLD
            RESULT = 0
          ENDIF
          CALL MY_BARRIER
          ILD  = 0  
          MAXBOX = INTBUF_TAB%VARIABLES(9)
          MINBOX = INTBUF_TAB%VARIABLES(12)
          TZINF  = INTBUF_TAB%VARIABLES(8)       
          GOTO 150
        ENDIF
        IF (NSPMD.GT.1) THEN
C mise a - de dist temporairement pour reperage dans partie frontiere
C mono tache
!$OMP SINGLE
          IF (IMONM > 0) CALL STARTIME(26,1)

          IF(INTBUF_TAB%VARIABLES(5).GE.ZERO) INTBUF_TAB%VARIABLES(5)= -INTBUF_TAB%VARIABLES(5)
	  CALL SPMD_TRI20GATE(
     1 RESULT	    ,NLINSA,INTBUF_TAB%LCAND_S,INTBUF_TAB%I_STOK_E(1),NIN,
     2 IPARI(22,NIN),NLINSR,MULTIMP	 ,IGAP      )
       IPARI(57,NIN) = NLINSR

          IF (IMONM > 0) CALL STOPTIME(26,1)

!$OMP END SINGLE
        ENDIF
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  I20XSINIR                     source/interfaces/intsort/i20main_tri.F
Chd|-- called by -----------
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
       SUBROUTINE I20XSINIR(NSNR,NLINSR,ITASK,NIN,STFAC   )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSNR, NLINSR, ITASK, NIN
      my_real
     .   STFAC
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I1, I2, NF, NL
      my_real 
     .       AAA, DA(3),AMASS 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      IF(STFAC > ZERO)THEN
        AMASS = MAX(DEUX,STFAC+SQRT(DEUX*STFAC))
      ELSE
        AMASS = DEUX
      ENDIF

      IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN

       NF = 1 + ITASK*NSNR / NTHREAD
       NL = (ITASK+1)*NSNR/ NTHREAD

       DO I=NF,NL
        IF(MSFI(NIN)%P(I) > ZERO)THEN
          AAA = DT12/(AMASS*MSFI(NIN)%P(I))
c           delta A parith on
          DA(1) = DAANCFI(NIN)%P(1,I)
          DA(2) = DAANCFI(NIN)%P(2,I)
          DA(3) = DAANCFI(NIN)%P(3,I)

          IF(ALPHAKFI(NIN)%P(I)<ZERO)THEN
            DA(1) = DAANC6FI(NIN)%P(1,1,I) + DAANC6FI(NIN)%P(1,2,I)
     .            + DAANC6FI(NIN)%P(1,3,I) + DAANC6FI(NIN)%P(1,4,I)
     .            + DAANC6FI(NIN)%P(1,5,I) + DAANC6FI(NIN)%P(1,6,I)
     .            + DA(1)
            DA(2) = DAANC6FI(NIN)%P(2,1,I) + DAANC6FI(NIN)%P(2,2,I)
     .            + DAANC6FI(NIN)%P(2,3,I) + DAANC6FI(NIN)%P(2,4,I)
     .            + DAANC6FI(NIN)%P(2,5,I) + DAANC6FI(NIN)%P(2,6,I)
     .            + DA(2)
            DA(3) = DAANC6FI(NIN)%P(3,1,I) + DAANC6FI(NIN)%P(3,2,I)
     .            + DAANC6FI(NIN)%P(3,3,I) + DAANC6FI(NIN)%P(3,4,I)
     .            + DAANC6FI(NIN)%P(3,5,I) + DAANC6FI(NIN)%P(3,6,I)
     .            + DA(3)
C


            DAANC6FI(NIN)%P(1,1,I) = ZERO
            DAANC6FI(NIN)%P(1,2,I) = ZERO
            DAANC6FI(NIN)%P(1,3,I) = ZERO
            DAANC6FI(NIN)%P(1,4,I) = ZERO
            DAANC6FI(NIN)%P(1,5,I) = ZERO
            DAANC6FI(NIN)%P(1,6,I) = ZERO
        
            DAANC6FI(NIN)%P(2,1,I) = ZERO
            DAANC6FI(NIN)%P(2,2,I) = ZERO
            DAANC6FI(NIN)%P(2,3,I) = ZERO
            DAANC6FI(NIN)%P(2,4,I) = ZERO
            DAANC6FI(NIN)%P(2,5,I) = ZERO
            DAANC6FI(NIN)%P(2,6,I) = ZERO
        
            DAANC6FI(NIN)%P(3,1,I) = ZERO
            DAANC6FI(NIN)%P(3,2,I) = ZERO
            DAANC6FI(NIN)%P(3,3,I) = ZERO
            DAANC6FI(NIN)%P(3,4,I) = ZERO
            DAANC6FI(NIN)%P(3,5,I) = ZERO
            DAANC6FI(NIN)%P(3,6,I) = ZERO
          ENDIF

            DVANCFI(NIN)%P(1,I) = DVANCFI(NIN)%P(1,I) + DA(1)*AAA
            DVANCFI(NIN)%P(2,I) = DVANCFI(NIN)%P(2,I) + DA(2)*AAA
            DVANCFI(NIN)%P(3,I) = DVANCFI(NIN)%P(3,I) + DA(3)*AAA
            DXANCFI(NIN)%P(1,I) = DXANCFI(NIN)%P(1,I)
     .                        + DVANCFI(NIN)%P(1,I)*DT1
            DXANCFI(NIN)%P(2,I) = DXANCFI(NIN)%P(2,I)
     .                        + DVANCFI(NIN)%P(2,I)*DT1
            DXANCFI(NIN)%P(3,I) = DXANCFI(NIN)%P(3,I)
     .                        + DVANCFI(NIN)%P(3,I)*DT1

        ELSE
          DVANCFI(NIN)%P(1,I) = ZERO
          DVANCFI(NIN)%P(2,I) = ZERO
          DVANCFI(NIN)%P(3,I) = ZERO
          DXANCFI(NIN)%P(1,I) = ZERO
          DXANCFI(NIN)%P(2,I) = ZERO
          DXANCFI(NIN)%P(3,I) = ZERO
        ENDIF

        XFI(NIN)%P(1,I) = XFI(NIN)%P(1,I)+DXANCFI(NIN)%P(1,I)
        XFI(NIN)%P(2,I) = XFI(NIN)%P(2,I)+DXANCFI(NIN)%P(2,I)
        XFI(NIN)%P(3,I) = XFI(NIN)%P(3,I)+DXANCFI(NIN)%P(3,I)
        VFI(NIN)%P(1,I) = VFI(NIN)%P(1,I)+DVANCFI(NIN)%P(1,I)
        VFI(NIN)%P(2,I) = VFI(NIN)%P(2,I)+DVANCFI(NIN)%P(2,I)
        VFI(NIN)%P(3,I) = VFI(NIN)%P(3,I)+DVANCFI(NIN)%P(3,I)
        
C ALPHAK(2) remote
        ALPHAKFI(NIN)%P(I) = UN

       END DO
C
       NF = 1 + ITASK*NLINSR / NTHREAD
       NL = (ITASK+1)*NLINSR/ NTHREAD

       DO I=NF,NL
C traiter les 2 noeuds autant de fois qu'ils apparaissent
        I1 = 2*I-1
        I2 = 2*I
C
        IF(MSFIE(NIN)%P(I1) > ZERO)THEN
          AAA = DT12/(AMASS*MSFIE(NIN)%P(I1))
          DA(1) = DAANCFIE(NIN)%P(1,I1)
          DA(2) = DAANCFIE(NIN)%P(2,I1)
          DA(3) = DAANCFIE(NIN)%P(3,I1)

          IF(ALPHAKFIE(NIN)%P(I1)<ZERO)THEN
             DA(1) = DAANC6FIE(NIN)%P(1,1,I1) + DAANC6FIE(NIN)%P(1,2,I1)
     .             + DAANC6FIE(NIN)%P(1,3,I1) + DAANC6FIE(NIN)%P(1,4,I1)
     .             + DAANC6FIE(NIN)%P(1,5,I1) + DAANC6FIE(NIN)%P(1,6,I1)
     .             + DA(1)
             DA(2) = DAANC6FIE(NIN)%P(2,1,I1) + DAANC6FIE(NIN)%P(2,2,I1)
     .             + DAANC6FIE(NIN)%P(2,3,I1) + DAANC6FIE(NIN)%P(2,4,I1) 
     .             + DAANC6FIE(NIN)%P(2,5,I1) + DAANC6FIE(NIN)%P(2,6,I1)
     .             + DA(2)
             DA(3) = DAANC6FIE(NIN)%P(3,1,I1) + DAANC6FIE(NIN)%P(3,2,I1)
     .             + DAANC6FIE(NIN)%P(3,3,I1) + DAANC6FIE(NIN)%P(3,4,I1)
     .             + DAANC6FIE(NIN)%P(3,5,I1) + DAANC6FIE(NIN)%P(3,6,I1)
     .             + DA(3)
             DAANC6FIE(NIN)%P(1,1,I1) = ZERO
             DAANC6FIE(NIN)%P(1,2,I1) = ZERO
             DAANC6FIE(NIN)%P(1,3,I1) = ZERO
             DAANC6FIE(NIN)%P(1,4,I1) = ZERO
             DAANC6FIE(NIN)%P(1,5,I1) = ZERO
             DAANC6FIE(NIN)%P(1,6,I1) = ZERO
        
             DAANC6FIE(NIN)%P(2,1,I1) = ZERO
             DAANC6FIE(NIN)%P(2,2,I1) = ZERO
             DAANC6FIE(NIN)%P(2,3,I1) = ZERO
             DAANC6FIE(NIN)%P(2,4,I1) = ZERO
             DAANC6FIE(NIN)%P(2,5,I1) = ZERO
             DAANC6FIE(NIN)%P(2,6,I1) = ZERO
        
             DAANC6FIE(NIN)%P(3,1,I1) = ZERO
             DAANC6FIE(NIN)%P(3,2,I1) = ZERO
             DAANC6FIE(NIN)%P(3,3,I1) = ZERO
             DAANC6FIE(NIN)%P(3,4,I1) = ZERO
             DAANC6FIE(NIN)%P(3,5,I1) = ZERO
             DAANC6FIE(NIN)%P(3,6,I1) = ZERO
          ENDIF

          DVANCFIE(NIN)%P(1,I1) = DVANCFIE(NIN)%P(1,I1) + DA(1)*AAA
          DVANCFIE(NIN)%P(2,I1) = DVANCFIE(NIN)%P(2,I1) + DA(2)*AAA
          DVANCFIE(NIN)%P(3,I1) = DVANCFIE(NIN)%P(3,I1) + DA(3)*AAA
          DXANCFIE(NIN)%P(1,I1) = DXANCFIE(NIN)%P(1,I1)
     .                          + DVANCFIE(NIN)%P(1,I1)*DT1
          DXANCFIE(NIN)%P(2,I1) = DXANCFIE(NIN)%P(2,I1)
     .                          + DVANCFIE(NIN)%P(2,I1)*DT1
          DXANCFIE(NIN)%P(3,I1) = DXANCFIE(NIN)%P(3,I1)
     .                          + DVANCFIE(NIN)%P(3,I1)*DT1

        ELSE
          DVANCFIE(NIN)%P(1,I1) = ZERO
          DVANCFIE(NIN)%P(2,I1) = ZERO
          DVANCFIE(NIN)%P(3,I1) = ZERO
          DXANCFIE(NIN)%P(1,I1) = ZERO
          DXANCFIE(NIN)%P(2,I1) = ZERO
          DXANCFIE(NIN)%P(3,I1) = ZERO
        ENDIF

        XFIE(NIN)%P(1,I1) = XFIE(NIN)%P(1,I1)+DXANCFIE(NIN)%P(1,I1)
        XFIE(NIN)%P(2,I1) = XFIE(NIN)%P(2,I1)+DXANCFIE(NIN)%P(2,I1)
        XFIE(NIN)%P(3,I1) = XFIE(NIN)%P(3,I1)+DXANCFIE(NIN)%P(3,I1)
        VFIE(NIN)%P(1,I1) = VFIE(NIN)%P(1,I1)+DVANCFIE(NIN)%P(1,I1)
        VFIE(NIN)%P(2,I1) = VFIE(NIN)%P(2,I1)+DVANCFIE(NIN)%P(2,I1)
        VFIE(NIN)%P(3,I1) = VFIE(NIN)%P(3,I1)+DVANCFIE(NIN)%P(3,I1)

C ALPHAK(2) remote
        ALPHAKFIE(NIN)%P(I1) = UN


        IF(MSFIE(NIN)%P(I2) > ZERO)THEN
          AAA = DT12/(AMASS*MSFIE(NIN)%P(I2))
          DA(1) = DAANCFIE(NIN)%P(1,I2)
          DA(2) = DAANCFIE(NIN)%P(2,I2)
          DA(3) = DAANCFIE(NIN)%P(3,I2)

          IF(ALPHAKFIE(NIN)%P(I2)<ZERO)THEN
             DA(1) = DAANC6FIE(NIN)%P(1,1,I2) + DAANC6FIE(NIN)%P(1,2,I2)
     .             + DAANC6FIE(NIN)%P(1,3,I2) + DAANC6FIE(NIN)%P(1,4,I2)
     .             + DAANC6FIE(NIN)%P(1,5,I2) + DAANC6FIE(NIN)%P(1,6,I2)
     .             + DA(1)
             DA(2) = DAANC6FIE(NIN)%P(2,1,I2) + DAANC6FIE(NIN)%P(2,2,I2)
     .             + DAANC6FIE(NIN)%P(2,3,I2) + DAANC6FIE(NIN)%P(2,4,I2) 
     .             + DAANC6FIE(NIN)%P(2,5,I2) + DAANC6FIE(NIN)%P(2,6,I2)
     .             + DA(2)
             DA(3) = DAANC6FIE(NIN)%P(3,1,I2) + DAANC6FIE(NIN)%P(3,2,I2)
     .             + DAANC6FIE(NIN)%P(3,3,I2) + DAANC6FIE(NIN)%P(3,4,I2)
     .             + DAANC6FIE(NIN)%P(3,5,I2) + DAANC6FIE(NIN)%P(3,6,I2)
     .             + DA(3)
             DAANC6FIE(NIN)%P(1,1,I2) = ZERO
             DAANC6FIE(NIN)%P(1,2,I2) = ZERO
             DAANC6FIE(NIN)%P(1,3,I2) = ZERO
             DAANC6FIE(NIN)%P(1,4,I2) = ZERO
             DAANC6FIE(NIN)%P(1,5,I2) = ZERO
             DAANC6FIE(NIN)%P(1,6,I2) = ZERO
        
             DAANC6FIE(NIN)%P(2,1,I2) = ZERO
             DAANC6FIE(NIN)%P(2,2,I2) = ZERO
             DAANC6FIE(NIN)%P(2,3,I2) = ZERO
             DAANC6FIE(NIN)%P(2,4,I2) = ZERO
             DAANC6FIE(NIN)%P(2,5,I2) = ZERO
             DAANC6FIE(NIN)%P(2,6,I2) = ZERO
        
             DAANC6FIE(NIN)%P(3,1,I2) = ZERO
             DAANC6FIE(NIN)%P(3,2,I2) = ZERO
             DAANC6FIE(NIN)%P(3,3,I2) = ZERO
             DAANC6FIE(NIN)%P(3,4,I2) = ZERO
             DAANC6FIE(NIN)%P(3,5,I2) = ZERO
             DAANC6FIE(NIN)%P(3,6,I2) = ZERO
          ENDIF

          DVANCFIE(NIN)%P(1,I2) = DVANCFIE(NIN)%P(1,I2) + DA(1)*AAA
          DVANCFIE(NIN)%P(2,I2) = DVANCFIE(NIN)%P(2,I2) + DA(2)*AAA
          DVANCFIE(NIN)%P(3,I2) = DVANCFIE(NIN)%P(3,I2) + DA(3)*AAA
          DXANCFIE(NIN)%P(1,I2) = DXANCFIE(NIN)%P(1,I2)
     .                          + DVANCFIE(NIN)%P(1,I2)*DT1
          DXANCFIE(NIN)%P(2,I2) = DXANCFIE(NIN)%P(2,I2)
     .                          + DVANCFIE(NIN)%P(2,I2)*DT1
          DXANCFIE(NIN)%P(3,I2) = DXANCFIE(NIN)%P(3,I2)
     .                          + DVANCFIE(NIN)%P(3,I2)*DT1
        ELSE
          DVANCFIE(NIN)%P(1,I2) = ZERO
          DVANCFIE(NIN)%P(2,I2) = ZERO
          DVANCFIE(NIN)%P(3,I2) = ZERO
          DXANCFIE(NIN)%P(1,I2) = ZERO
          DXANCFIE(NIN)%P(2,I2) = ZERO
          DXANCFIE(NIN)%P(3,I2) = ZERO
        ENDIF

        XFIE(NIN)%P(1,I2) = XFIE(NIN)%P(1,I2)+DXANCFIE(NIN)%P(1,I2)
        XFIE(NIN)%P(2,I2) = XFIE(NIN)%P(2,I2)+DXANCFIE(NIN)%P(2,I2)
        XFIE(NIN)%P(3,I2) = XFIE(NIN)%P(3,I2)+DXANCFIE(NIN)%P(3,I2)
        VFIE(NIN)%P(1,I2) = VFIE(NIN)%P(1,I2)+DVANCFIE(NIN)%P(1,I2)
        VFIE(NIN)%P(2,I2) = VFIE(NIN)%P(2,I2)+DVANCFIE(NIN)%P(2,I2)
        VFIE(NIN)%P(3,I2) = VFIE(NIN)%P(3,I2)+DVANCFIE(NIN)%P(3,I2)
        
        
C ALPHAK(2) remote
        ALPHAKFIE(NIN)%P(I2) = UN

       END DO

      ELSE

C AMS

       NF = 1 + ITASK*NSNR / NTHREAD
       NL = (ITASK+1)*NSNR/ NTHREAD

       DO I=NF,NL
        IF(DIAG_SMSFI(NIN)%P(I) > ZERO)THEN
          AAA = DT12/(AMASS*DIAG_SMSFI(NIN)%P(I))

          DA(1) = DAANCFI(NIN)%P(1,I)
          DA(2) = DAANCFI(NIN)%P(2,I)
          DA(3) = DAANCFI(NIN)%P(3,I)

          IF(ALPHAKFI(NIN)%P(I)<ZERO)THEN
            DA(1) = DAANC6FI(NIN)%P(1,1,I) + DAANC6FI(NIN)%P(1,2,I)
     .            + DAANC6FI(NIN)%P(1,3,I) + DAANC6FI(NIN)%P(1,4,I)
     .            + DAANC6FI(NIN)%P(1,5,I) + DAANC6FI(NIN)%P(1,6,I)
     .            + DA(1)
            DA(2) = DAANC6FI(NIN)%P(2,1,I) + DAANC6FI(NIN)%P(2,2,I)
     .            + DAANC6FI(NIN)%P(2,3,I) + DAANC6FI(NIN)%P(2,4,I)
     .            + DAANC6FI(NIN)%P(2,5,I) + DAANC6FI(NIN)%P(2,6,I)
     .            + DA(2)
            DA(3) = DAANC6FI(NIN)%P(3,1,I) + DAANC6FI(NIN)%P(3,2,I)
     .            + DAANC6FI(NIN)%P(3,3,I) + DAANC6FI(NIN)%P(3,4,I)
     .            + DAANC6FI(NIN)%P(3,5,I) + DAANC6FI(NIN)%P(3,6,I)
     .            + DA(3)
C

            DAANC6FI(NIN)%P(1,1,I) = ZERO
            DAANC6FI(NIN)%P(1,2,I) = ZERO
            DAANC6FI(NIN)%P(1,3,I) = ZERO
            DAANC6FI(NIN)%P(1,4,I) = ZERO
            DAANC6FI(NIN)%P(1,5,I) = ZERO
            DAANC6FI(NIN)%P(1,6,I) = ZERO
        
            DAANC6FI(NIN)%P(2,1,I) = ZERO
            DAANC6FI(NIN)%P(2,2,I) = ZERO
            DAANC6FI(NIN)%P(2,3,I) = ZERO
            DAANC6FI(NIN)%P(2,4,I) = ZERO
            DAANC6FI(NIN)%P(2,5,I) = ZERO
            DAANC6FI(NIN)%P(2,6,I) = ZERO
        
            DAANC6FI(NIN)%P(3,1,I) = ZERO
            DAANC6FI(NIN)%P(3,2,I) = ZERO
            DAANC6FI(NIN)%P(3,3,I) = ZERO
            DAANC6FI(NIN)%P(3,4,I) = ZERO
            DAANC6FI(NIN)%P(3,5,I) = ZERO
            DAANC6FI(NIN)%P(3,6,I) = ZERO
          ENDIF

          DVANCFI(NIN)%P(1,I) = DVANCFI(NIN)%P(1,I) + DA(1)*AAA
          DVANCFI(NIN)%P(2,I) = DVANCFI(NIN)%P(2,I) + DA(2)*AAA
          DVANCFI(NIN)%P(3,I) = DVANCFI(NIN)%P(3,I) + DA(3)*AAA
          DXANCFI(NIN)%P(1,I) = DXANCFI(NIN)%P(1,I)
     .                        + DVANCFI(NIN)%P(1,I)*DT1
          DXANCFI(NIN)%P(2,I) = DXANCFI(NIN)%P(2,I)
     .                        + DVANCFI(NIN)%P(2,I)*DT1
          DXANCFI(NIN)%P(3,I) = DXANCFI(NIN)%P(3,I)
     .                        + DVANCFI(NIN)%P(3,I)*DT1
        ELSE
          DVANCFI(NIN)%P(1,I) = ZERO
          DVANCFI(NIN)%P(2,I) = ZERO
          DVANCFI(NIN)%P(3,I) = ZERO
          DXANCFI(NIN)%P(1,I) = ZERO
          DXANCFI(NIN)%P(2,I) = ZERO
          DXANCFI(NIN)%P(3,I) = ZERO
        ENDIF

        XFI(NIN)%P(1,I) = XFI(NIN)%P(1,I)+DXANCFI(NIN)%P(1,I)
        XFI(NIN)%P(2,I) = XFI(NIN)%P(2,I)+DXANCFI(NIN)%P(2,I)
        XFI(NIN)%P(3,I) = XFI(NIN)%P(3,I)+DXANCFI(NIN)%P(3,I)
        VFI(NIN)%P(1,I) = VFI(NIN)%P(1,I)+DVANCFI(NIN)%P(1,I)
        VFI(NIN)%P(2,I) = VFI(NIN)%P(2,I)+DVANCFI(NIN)%P(2,I)
        VFI(NIN)%P(3,I) = VFI(NIN)%P(3,I)+DVANCFI(NIN)%P(3,I)
        

C ALPHAK(2) remote
        ALPHAKFI(NIN)%P(I) = UN

       END DO
C
       NF = 1 + ITASK*NLINSR / NTHREAD
       NL = (ITASK+1)*NLINSR/ NTHREAD

       DO I=NF,NL
C traiter les 2 noeuds autant de fois qu'ils apparaissent
        I1 = 2*I-1
        I2 = 2*I
C
        IF(DIAG_SMSFIE(NIN)%P(I1) > ZERO)THEN
          AAA = DT12/(AMASS*DIAG_SMSFIE(NIN)%P(I1))
          DA(1) = DAANCFIE(NIN)%P(1,I1)
          DA(2) = DAANCFIE(NIN)%P(2,I1)
          DA(3) = DAANCFIE(NIN)%P(3,I1)

          IF(ALPHAKFIE(NIN)%P(I1)<ZERO)THEN

             DA(1) = DAANC6FIE(NIN)%P(1,1,I1) + DAANC6FIE(NIN)%P(1,2,I1)
     .             + DAANC6FIE(NIN)%P(1,3,I1) + DAANC6FIE(NIN)%P(1,4,I1)
     .             + DAANC6FIE(NIN)%P(1,5,I1) + DAANC6FIE(NIN)%P(1,6,I1)
     .             + DA(1)
             DA(2) = DAANC6FIE(NIN)%P(2,1,I1) + DAANC6FIE(NIN)%P(2,2,I1)
     .             + DAANC6FIE(NIN)%P(2,3,I1) + DAANC6FIE(NIN)%P(2,4,I1) 
     .             + DAANC6FIE(NIN)%P(2,5,I1) + DAANC6FIE(NIN)%P(2,6,I1)
     .             + DA(2)
             DA(3) = DAANC6FIE(NIN)%P(3,1,I1) + DAANC6FIE(NIN)%P(3,2,I1)
     .             + DAANC6FIE(NIN)%P(3,3,I1) + DAANC6FIE(NIN)%P(3,4,I1)
     .             + DAANC6FIE(NIN)%P(3,5,I1) + DAANC6FIE(NIN)%P(3,6,I1)
     .             + DA(3)

             DAANC6FIE(NIN)%P(1,1,I1) = ZERO
             DAANC6FIE(NIN)%P(1,2,I1) = ZERO
             DAANC6FIE(NIN)%P(1,3,I1) = ZERO
             DAANC6FIE(NIN)%P(1,4,I1) = ZERO
             DAANC6FIE(NIN)%P(1,5,I1) = ZERO
             DAANC6FIE(NIN)%P(1,6,I1) = ZERO
        
             DAANC6FIE(NIN)%P(2,1,I1) = ZERO
             DAANC6FIE(NIN)%P(2,2,I1) = ZERO
             DAANC6FIE(NIN)%P(2,3,I1) = ZERO
             DAANC6FIE(NIN)%P(2,4,I1) = ZERO
             DAANC6FIE(NIN)%P(2,5,I1) = ZERO
             DAANC6FIE(NIN)%P(2,6,I1) = ZERO
        
             DAANC6FIE(NIN)%P(3,1,I1) = ZERO
             DAANC6FIE(NIN)%P(3,2,I1) = ZERO
             DAANC6FIE(NIN)%P(3,3,I1) = ZERO
             DAANC6FIE(NIN)%P(3,4,I1) = ZERO
             DAANC6FIE(NIN)%P(3,5,I1) = ZERO
             DAANC6FIE(NIN)%P(3,6,I1) = ZERO
          ENDIF
          DVANCFIE(NIN)%P(1,I1) = DVANCFIE(NIN)%P(1,I1) + DA(1)*AAA
          DVANCFIE(NIN)%P(2,I1) = DVANCFIE(NIN)%P(2,I1) + DA(2)*AAA
          DVANCFIE(NIN)%P(3,I1) = DVANCFIE(NIN)%P(3,I1) + DA(3)*AAA
          DXANCFIE(NIN)%P(1,I1) = DXANCFIE(NIN)%P(1,I1)
     .                          + DVANCFIE(NIN)%P(1,I1)*DT1
          DXANCFIE(NIN)%P(2,I1) = DXANCFIE(NIN)%P(2,I1)
     .                          + DVANCFIE(NIN)%P(2,I1)*DT1
          DXANCFIE(NIN)%P(3,I1) = DXANCFIE(NIN)%P(3,I1)
     .                          + DVANCFIE(NIN)%P(3,I1)*DT1

        ELSE
          DVANCFIE(NIN)%P(1,I1) = ZERO
          DVANCFIE(NIN)%P(2,I1) = ZERO
          DVANCFIE(NIN)%P(3,I1) = ZERO
          DXANCFIE(NIN)%P(1,I1) = ZERO
          DXANCFIE(NIN)%P(2,I1) = ZERO
          DXANCFIE(NIN)%P(3,I1) = ZERO
        ENDIF

        XFIE(NIN)%P(1,I1) = XFIE(NIN)%P(1,I1)+DXANCFIE(NIN)%P(1,I1)
        XFIE(NIN)%P(2,I1) = XFIE(NIN)%P(2,I1)+DXANCFIE(NIN)%P(2,I1)
        XFIE(NIN)%P(3,I1) = XFIE(NIN)%P(3,I1)+DXANCFIE(NIN)%P(3,I1)
        VFIE(NIN)%P(1,I1) = VFIE(NIN)%P(1,I1)+DVANCFIE(NIN)%P(1,I1)
        VFIE(NIN)%P(2,I1) = VFIE(NIN)%P(2,I1)+DVANCFIE(NIN)%P(2,I1)
        VFIE(NIN)%P(3,I1) = VFIE(NIN)%P(3,I1)+DVANCFIE(NIN)%P(3,I1)


C ALPHAK(2) remote
        ALPHAKFIE(NIN)%P(I1) = UN


        IF(DIAG_SMSFIE(NIN)%P(I2) > ZERO)THEN
          AAA = DT12/(AMASS*DIAG_SMSFIE(NIN)%P(I2))
          DA(1) = DAANCFIE(NIN)%P(1,I2)
          DA(2) = DAANCFIE(NIN)%P(2,I2)
          DA(3) = DAANCFIE(NIN)%P(3,I2)

          IF(ALPHAKFIE(NIN)%P(I2)<ZERO)THEN
             DA(1) = DAANC6FIE(NIN)%P(1,1,I2) + DAANC6FIE(NIN)%P(1,2,I2)
     .             + DAANC6FIE(NIN)%P(1,3,I2) + DAANC6FIE(NIN)%P(1,4,I2)
     .             + DAANC6FIE(NIN)%P(1,5,I2) + DAANC6FIE(NIN)%P(1,6,I2)
     .             + DA(1)
             DA(2) = DAANC6FIE(NIN)%P(2,1,I2) + DAANC6FIE(NIN)%P(2,2,I2)
     .             + DAANC6FIE(NIN)%P(2,3,I2) + DAANC6FIE(NIN)%P(2,4,I2) 
     .             + DAANC6FIE(NIN)%P(2,5,I2) + DAANC6FIE(NIN)%P(2,6,I2)
     .             + DA(2)
             DA(3) = DAANC6FIE(NIN)%P(3,1,I2) + DAANC6FIE(NIN)%P(3,2,I2)
     .             + DAANC6FIE(NIN)%P(3,3,I2) + DAANC6FIE(NIN)%P(3,4,I2)
     .             + DAANC6FIE(NIN)%P(3,5,I2) + DAANC6FIE(NIN)%P(3,6,I2)
     .             + DA(3)
             DAANC6FIE(NIN)%P(1,1,I2) = ZERO
             DAANC6FIE(NIN)%P(1,2,I2) = ZERO
             DAANC6FIE(NIN)%P(1,3,I2) = ZERO
             DAANC6FIE(NIN)%P(1,4,I2) = ZERO
             DAANC6FIE(NIN)%P(1,5,I2) = ZERO
             DAANC6FIE(NIN)%P(1,6,I2) = ZERO
        
             DAANC6FIE(NIN)%P(2,1,I2) = ZERO
             DAANC6FIE(NIN)%P(2,2,I2) = ZERO
             DAANC6FIE(NIN)%P(2,3,I2) = ZERO
             DAANC6FIE(NIN)%P(2,4,I2) = ZERO
             DAANC6FIE(NIN)%P(2,5,I2) = ZERO
             DAANC6FIE(NIN)%P(2,6,I2) = ZERO
        
             DAANC6FIE(NIN)%P(3,1,I2) = ZERO
             DAANC6FIE(NIN)%P(3,2,I2) = ZERO
             DAANC6FIE(NIN)%P(3,3,I2) = ZERO
             DAANC6FIE(NIN)%P(3,4,I2) = ZERO
             DAANC6FIE(NIN)%P(3,5,I2) = ZERO
             DAANC6FIE(NIN)%P(3,6,I2) = ZERO
          ENDIF

          DVANCFIE(NIN)%P(1,I2) = DVANCFIE(NIN)%P(1,I2) + DA(1)*AAA
          DVANCFIE(NIN)%P(2,I2) = DVANCFIE(NIN)%P(2,I2) + DA(2)*AAA
          DVANCFIE(NIN)%P(3,I2) = DVANCFIE(NIN)%P(3,I2) + DA(3)*AAA
          DXANCFIE(NIN)%P(1,I2) = DXANCFIE(NIN)%P(1,I2)
     .                          + DVANCFIE(NIN)%P(1,I2)*DT1
          DXANCFIE(NIN)%P(2,I2) = DXANCFIE(NIN)%P(2,I2)
     .                          + DVANCFIE(NIN)%P(2,I2)*DT1
          DXANCFIE(NIN)%P(3,I2) = DXANCFIE(NIN)%P(3,I2)
     .                          + DVANCFIE(NIN)%P(3,I2)*DT1
        ELSE
          DVANCFIE(NIN)%P(1,I2) = ZERO
          DVANCFIE(NIN)%P(2,I2) = ZERO
          DVANCFIE(NIN)%P(3,I2) = ZERO
          DXANCFIE(NIN)%P(1,I2) = ZERO
          DXANCFIE(NIN)%P(2,I2) = ZERO
          DXANCFIE(NIN)%P(3,I2) = ZERO
        ENDIF

        XFIE(NIN)%P(1,I2) = XFIE(NIN)%P(1,I2)+DXANCFIE(NIN)%P(1,I2)
        XFIE(NIN)%P(2,I2) = XFIE(NIN)%P(2,I2)+DXANCFIE(NIN)%P(2,I2)
        XFIE(NIN)%P(3,I2) = XFIE(NIN)%P(3,I2)+DXANCFIE(NIN)%P(3,I2)
        VFIE(NIN)%P(1,I2) = VFIE(NIN)%P(1,I2)+DVANCFIE(NIN)%P(1,I2)
        VFIE(NIN)%P(2,I2) = VFIE(NIN)%P(2,I2)+DVANCFIE(NIN)%P(2,I2)
        VFIE(NIN)%P(3,I2) = VFIE(NIN)%P(3,I2)+DVANCFIE(NIN)%P(3,I2)
        
        
C ALPHAK(2) remote
        ALPHAKFIE(NIN)%P(I2) = UN

       END DO
      END IF
C
      RETURN
      END
