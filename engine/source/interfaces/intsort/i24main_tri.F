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
Chd|  I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I24BUCE                       source/interfaces/intsort/i24buce.F
Chd|        I24TRC                        source/interfaces/intsort/i7trc.F
Chd|        I7XSAVE                       source/interfaces/intsort/i7xsave.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_RNUMCD                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_TRI24GAT                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI24VOX                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI24VOX0                source/mpi/interfaces/spmd_int.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24MAIN_TRI( 
     1                  IPARI   ,X       ,V       ,INTBUF_TAB,
     2                  MS      ,NIN     ,ITASK   ,MWAG    ,WEIGHT  ,
     3                  ISENDTO ,IRCVFROM,RETRI   ,IAD_ELEM,FR_ELEM ,
     4                  ITAB    ,KINET   ,TEMP    ,NRTM_T  ,RENUM   ,
     5                  NSNFIOLD,ESHIFT  ,NUM_IMP ,IND_IMP ,NODNX_SMS,
     6                  H3D_DATA,T2MAIN_SMS,FORNEQS,T2FAC_SMS)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE INTBUFMOD
      USE MESSAGE_MOD
      USE IMP_INTBUF
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
#include      "impl1_c.inc"
C common pour variable globale en memoire partagee
      COMMON /I7MAINC/BMINMA,CURV_MAX_MAX,RESULT,NSNR,NSNROLD,I_MEMG,NMN_G
      INTEGER RESULT,NSNR,NSNROLD,I_MEMG,NMN_G
      my_real 
     .        BMINMA(6),CURV_MAX_MAX
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN ,ITASK, RETRI, NRTM_T,ESHIFT,
     .        NUM_IMP ,IND_IMP(*),
     .        ITAB(*), KINET(*),
     .        IPARI(NPARI,NINTER),  MWAG(*),
     .        ISENDTO(NINTER+1,*),IRCVFROM(NINTER+1,*),
     .        WEIGHT(*), IAD_ELEM(2,*) ,FR_ELEM(*),
     .        RENUM(NUMNOD), NSNFIOLD(NSPMD), NODNX_SMS(*), T2MAIN_SMS(6,*)
C     REAL
      my_real 
     .    X(3,*), V(*), MS(*),TEMP(*),T2FAC_SMS(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LOC_PROC,IEDGE,IGSTI,
     .        I, IP0, IP1, IP2, IP21, K11_T, I_SK_OLD, I_STOK1, 
     .        ADD1, NB_N_B, NOINT, INACTI, MULTIMP, IGAP, IFQ ,
     .        INTNITSCHE
      INTEGER 
     .        ILD, NCONT, NCONTACT, INACTII, INACIMP, INTTH,
     .        I_MEM,CAND_N_OLD,ILEV,FLAGREMN, NRTSE ,NMN_L
C     REAL
      my_real
     .   STARTT,GAP,MAXBOX,MINBOX,STOPT,DIST,DGAPLOAD,
     .   XMAXL, YMAXL, ZMAXL, XMINL, YMINL, ZMINL, GAPMIN, GAPMAX,
     .   C_MAXL,PMAX_GAP,VMAXDT,MARGE,TZINF,SX,SY,SZ,SX2,SY2,SZ2,
     .   CURV_MAX(NRTM_T),BMINMA_OLD(6),FORNEQS(*)
C
      INTEGER, DIMENSION(:),ALLOCATABLE :: CAND_A
      SAVE CAND_A
C-----------------------------------------------
#include "sync_dir.inc"
C-----------------------------------------------
      I_MEM = 0
      I_MEMG = 0
      NMN_G = 0
      NMN_L = 0
C
      LOC_PROC=ISPMD+1
      NRTM    = IPARI(4,NIN)
      NSN     = IPARI(5,NIN)
      NMN     = IPARI(6,NIN)
      NTY     = IPARI(7,NIN)
      NOINT   = IPARI(15,NIN)
      NCONT   = IPARI(18,NIN)
      ILEV    = IPARI(20,NIN)
      INACTI  = IPARI(22,NIN)
      MULTIMP = IPARI(23,NIN)
      NCONTACT= MULTIMP*NCONT
      IFQ     = IPARI(31,NIN)
      INTTH   = IPARI(47,NIN)
      IEDGE   = IPARI(58,NIN)
      FLAGREMN= IPARI(63,NIN)
      IGSTI   = IPARI(34,NIN)
      NRTSE   = IPARI(52,NIN)
      NSNE    = IPARI(55,NIN)
      INTNITSCHE = IPARI(86,NIN)

C
c      IF(NUM_IMP>0)THEN
c        NSNROLD = IPARI(24,NIN)
c      ELSE
         NSNROLD = IPARI(24,NIN)
c      ENDIF
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
      GAP =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      PMAX_GAP=INTBUF_TAB%VARIABLES(23)
      VMAXDT  =INTBUF_TAB%VARIABLES(24)
C

C
C -------------------------------------------------------------
C
      DIST = INTBUF_TAB%VARIABLES(5)
      IF (DIST>ZERO) RETURN
      RETRI = 1
C
C -------------------------------------------------------------
C
      MAXBOX = INTBUF_TAB%VARIABLES(9)
      MINBOX = INTBUF_TAB%VARIABLES(12)
      MARGE  = INTBUF_TAB%VARIABLES(25)
      DGAPLOAD = INTBUF_TAB%VARIABLES(46)
      BMINMA(1)=-EP30
      BMINMA(2)=-EP30
      BMINMA(3)=-EP30
      BMINMA(4)=EP30
      BMINMA(5)=EP30
      BMINMA(6)=EP30
      CURV_MAX_MAX = ZERO
C
C -------------------------------------------------------------
C     STOCKAGE DES ANCIENS CANDIDATS AVEC PENE INITIALE
C     OU AVEC DU FILTRAGE DE FROTTEMENT
C -------------------------------------------------------------
      CALL MY_BARRIER

      IF(ITASK==0)THEN
       ALLOCATE (CAND_A(NSN+NSNROLD+3))
       CAND_A(1:NSN+NSNROLD+3)=0
        I_SK_OLD = INTBUF_TAB%I_STOK(1)
        CALL I24TRC(
     1               NSN+NSNROLD  ,I_SK_OLD ,INTBUF_TAB%CAND_N,INTBUF_TAB%CAND_E,CAND_A,
     2               NIN, NSN,INTBUF_TAB%IRTLM,INTBUF_TAB%NSV,ITAB,
     2               INTBUF_TAB%MSEGLO,INTBUF_TAB%MSEGTYP24)
        INTBUF_TAB%I_STOK(1)=I_SK_OLD
      ENDIF

C
C Barriere dans tous les cas pour bminma [et cur_max_max]
C
      CALL MY_BARRIER
C -------------------------------------------------------------
C     CALCUL BORNE DOMAINE REMONTE DANS I7XSAVE
C -------------------------------------------------------------
C eshift : decalage sur cand_e
      CALL I7XSAVE(
     1       X            ,INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN ,NMN     ,
     2       ITASK        ,INTBUF_TAB%XSAV,XMINL      ,YMINL ,ZMINL   ,
     3       XMAXL        ,YMAXL        ,ZMAXL        ,C_MAXL,CURV_MAX,
     4       IPARI(39,NIN),INTBUF_TAB%IRECTM(1+4*ESHIFT),NRTM_T       ,SX    ,SY      ,
     5       SZ           ,SX2          ,SY2          ,SZ2            ,NMN_L)
#include "lockon.inc"
      BMINMA(1) = MAX(BMINMA(1),XMAXL)
      BMINMA(2) = MAX(BMINMA(2),YMAXL)
      BMINMA(3) = MAX(BMINMA(3),ZMAXL)
      BMINMA(4) = MIN(BMINMA(4),XMINL)
      BMINMA(5) = MIN(BMINMA(5),YMINL)
      BMINMA(6) = MIN(BMINMA(6),ZMINL)
      CURV_MAX_MAX = MAX(CURV_MAX_MAX,C_MAXL)
      NMN_G = NMN_G + NMN_L
#include "lockoff.inc"

      RESULT = 0
C BARRIER II_STOK et RESULT
      CALL MY_BARRIER
C a conserver pour cas inacti est modifie sur p0
      IF(ITASK==0)THEN
        IF(ABS(BMINMA(6)-BMINMA(3))>2*EP30.OR.
     +     ABS(BMINMA(5)-BMINMA(2))>2*EP30.OR.
     +     ABS(BMINMA(4)-BMINMA(1))>2*EP30)THEN
          CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I24BUCE)')
          CALL ARRET(2)
        END IF
C
        TZINF = MARGE+MAX(GAP+DGAPLOAD,PMAX_GAP)+CURV_MAX_MAX

        BMINMA(1)=BMINMA(1)+TZINF
        BMINMA(2)=BMINMA(2)+TZINF
        BMINMA(3)=BMINMA(3)+TZINF
        BMINMA(4)=BMINMA(4)-TZINF
        BMINMA(5)=BMINMA(5)-TZINF
        BMINMA(6)=BMINMA(6)-TZINF

        IF(NSPMD > LRVOXELP)THEN
          CALL ANCMSG(MSGID=36,ANMODE=ANINFO,
     .            C1='(I24MAINTRI)')
          CALL ARRET(2)
        END IF

        NSNR = 0
       IF (IMPL_S >0 .AND. NCYCLE>0 .AND. INCONV==1) THEN
        BMINMA_OLD(1)=BMINMA_IMP(1,NIN)
        BMINMA_OLD(2)=BMINMA_IMP(2,NIN)
        BMINMA_OLD(3)=BMINMA_IMP(3,NIN)
        BMINMA_OLD(4)=BMINMA_IMP(4,NIN)
        BMINMA_OLD(5)=BMINMA_IMP(5,NIN)
        BMINMA_OLD(6)=BMINMA_IMP(6,NIN)
C        
        BMINMA_IMP(1,NIN)=BMINMA(1)
        BMINMA_IMP(2,NIN)=BMINMA(2)
        BMINMA_IMP(3,NIN)=BMINMA(3)
        BMINMA_IMP(4,NIN)=BMINMA(4)
        BMINMA_IMP(5,NIN)=BMINMA(5)
        BMINMA_IMP(6,NIN)=BMINMA(6)
C        
        BMINMA(1)=MAX(BMINMA(1),BMINMA_OLD(1))
        BMINMA(2)=MAX(BMINMA(2),BMINMA_OLD(2))
        BMINMA(3)=MAX(BMINMA(3),BMINMA_OLD(3))
        BMINMA(4)=MIN(BMINMA(4),BMINMA_OLD(4))
        BMINMA(5)=MIN(BMINMA(5),BMINMA_OLD(5))
        BMINMA(6)=MIN(BMINMA(6),BMINMA_OLD(6))
       END IF
      END IF


      IF(NSPMD > 1) THEN

        IF(ITASK==0) CRVOXEL(0:LRVOXEL,0:LRVOXEL,LOC_PROC)=0
c        goto 150
        CALL MY_BARRIER
        IF (IMONM > 0 .AND. ITASK == 0) CALL STARTIME(26,1)

        CALL SPMD_TRI24VOX0(
     1      X           ,BMINMA  ,NRTM_T,INTBUF_TAB%STFM(1+ESHIFT),MARGE ,
     2      CURV_MAX,INTBUF_TAB%GAP_M(1+ESHIFT),INTBUF_TAB%IRECTM(1+4*ESHIFT),GAP,
     +                                                    INTBUF_TAB%VARIABLES(7),
     3      PMAX_GAP,VMAXDT      ,DGAPLOAD )
 
        CALL MY_BARRIER
        IF (IMONM > 0 .AND. ITASK == 0) CALL STOPTIME(26,1)

        IF(ITASK==0)THEN
C
C recuperation des noeuds remote NSNR stockes dans XREM
C
          INACTI=0
          CALL SPMD_TRI24VOX(
     1      INTBUF_TAB%NSV,NSN     ,X            ,V            ,MS     ,
     2      BMINMA       ,WEIGHT  ,INTBUF_TAB%STFNS,NIN        ,ISENDTO,
     3      IRCVFROM     ,IAD_ELEM,FR_ELEM      ,NSNR   ,IPARI(21,NIN),
     4      INTBUF_TAB%GAP_S,ITAB    ,KINET        ,IFQ          ,INACTI ,
     5      NSNFIOLD,IPARI(47,NIN),INTBUF_TAB%IELEC,INTBUF_TAB%AREAS,TEMP,
     6      NUM_IMP     ,NODNX_SMS,INTBUF_TAB%STIF_OLD,NTY   ,
     7      INTBUF_TAB%IRTLM,INTBUF_TAB%TIME_S,INTBUF_TAB%SECND_FR,INTBUF_TAB%PENE_OLD,
     8      INTBUF_TAB%STIF_OLD , INTBUF_TAB%NBINFLG,ILEV ,INTBUF_TAB%ICONT_I  , 
     9      INTBUF_TAB%XFIC ,INTBUF_TAB%VFIC ,IPARI(59,NIN),NSNE,INTBUF_TAB%IS2SE,
     A      INTBUF_TAB%IRTSE, INTBUF_TAB%IS2PT,INTBUF_TAB%ISEGPT,INTBUF_TAB%MSFIC,NRTSE,
     B      INTBUF_TAB%IS2ID,INTBUF_TAB%ISPT2,IPARI(72,NIN),INTBUF_TAB%IPARTFRICS,T2MAIN_SMS,
     C      INTNITSCHE      ,FORNEQS         ,T2FAC_SMS)
C
C renumerotation locale des anciens candidats
C
ccc          IF(NUM_IMP>0)THEN
            CALL SPMD_RNUMCD(
     1        INTBUF_TAB%CAND_N,RENUM  ,INTBUF_TAB%I_STOK(1), NIN, NSN,
     2        NSNFIOLD     ,NSNROLD)
ccc          END IF
        END IF
      END IF
C
      CAND_N_OLD = INTBUF_TAB%I_STOK(1)
 40   continue
C
      ILD = 0
      NB_N_B = 1
C
C Barrier comm spmd_tri7vox + BMINMA + Retour I7BUCE
C      
 50   CALL MY_BARRIER
C
      IF (IMONM > 0) CALL STARTIME(30,1)
C
      CALL I24BUCE(
     1 X      ,V           ,INTBUF_TAB%IRECTM(1+4*ESHIFT),INTBUF_TAB%NSV,
     +                                                 INTBUF_TAB%STFNS,
     2 NMN    ,NRTM_T      ,NSN            ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     3 GAP    ,NOINT       ,INTBUF_TAB%I_STOK(1)   ,NCONTACT     ,BMINMA       ,
     4 MARGE  ,CURV_MAX    ,PMAX_GAP       ,VMAXDT       ,NB_N_B       ,
     5 ESHIFT ,ILD         ,NIN            ,INTBUF_TAB%STFM(1+ESHIFT) ,INTBUF_TAB%GAP_S,
     6 NSNR   ,NCONT       ,INTBUF_TAB%GAP_M(1+ESHIFT)   ,ITASK      ,INTBUF_TAB%VARIABLES(7),
     7 I_MEM  ,INTBUF_TAB%PENE_OLD,ITAB          ,INTBUF_TAB%NBINFLG,INTBUF_TAB%MBINFLG,
     8 ILEV   ,INTBUF_TAB%MSEGTYP24,INTBUF_TAB%EDGE8L2 ,IEDGE        ,INTBUF_TAB%ISEADD,
     9 INTBUF_TAB%ISEDGE,INTBUF_TAB%CAND_T,FLAGREMN,INTBUF_TAB%KREMNODE(1+2*ESHIFT),INTBUF_TAB%REMNODE ,
     A CAND_A,RENUM,NSNROLD,INTBUF_TAB%IRTSE,INTBUF_TAB%IS2SE,NSNE   ,DGAPLOAD)
C
C Upgrade MultiMP
      IF (I_MEM == 2)THEN
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
        MULTIMP = IPARI(23,NIN) * 1.3
        CALL UPGRADE_MULTIMP(NIN,MULTIMP,INTBUF_TAB)
!$OMP END SINGLE
        I_MEM = 0
        I_MEMG = 0
        INTBUF_TAB%I_STOK(1)=CAND_N_OLD
        MULTIMP=IPARI(23,NIN)
        NCONTACT=MULTIMP*NCONT
        GOTO 40
      ENDIF

      IF (IMONM > 0) CALL STOPTIME(30,1)
C
#include "lockon.inc"
      RESULT        = RESULT + ILD
#include "lockoff.inc"
C--------------------------------------------------------------
C--------------------------------------------------------------
      CALL MY_BARRIER
      IF (RESULT/=0) THEN
        CALL MY_BARRIER
        IF (ITASK==0) THEN
C utile si on revient
          INTBUF_TAB%I_STOK(1) =  I_SK_OLD
          RESULT = 0
        ENDIF
        CALL MY_BARRIER
        ILD  = 0
        GOTO 50
      ENDIF
C mise a - de dist temporairement pour reperage dans partie frontiere
      IF(NSPMD>1)THEN
C mono tache
!$OMP SINGLE
        IF (IMONM > 0) CALL STARTIME(26,1)
        INTBUF_TAB%VARIABLES(5) = - ONE
C
        CALL SPMD_TRI24GAT(
     1      RESULT       ,NSN ,INTBUF_TAB%CAND_N,INTBUF_TAB%I_STOK(1),NIN,
     2      IPARI(21,NIN),NSNR,MULTIMP      ,NTY,IPARI(47,NIN),
     3      ILEV  ,IPARI(59,NIN) ,H3D_DATA  ,IPARI(72,NIN)    ,INTNITSCHE )
        IPARI(24,NIN) = NSNR
C
        IF (NTY==24.AND.RESULT==0.AND.IMPL_S>0.AND.IGSTI==6) THEN 
          STIF_OLDFI(NIN)%P(1,1:NSNR)=STIF_OLDFI(NIN)%P(2,1:NSNR)
        END IF
C
        IF (IMONM > 0) CALL STOPTIME(26,1)
!$OMP END SINGLE
      END IF
      IF(ITASK==0)DEALLOCATE(CAND_A)
C
      RETURN
      END
