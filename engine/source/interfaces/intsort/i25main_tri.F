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
      module MOD_I25MAIN_TRI
      implicit none
      INTEGER, DIMENSION(:), ALLOCATABLE :: CAND_A, CAND_B
      END MODULE MOD_I25MAIN_TRI
Chd|====================================================================
Chd|  I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I25BUCE                       source/interfaces/intsort/i25buce.F
Chd|        I25BUCE_EDG                   source/interfaces/intsort/i25buce_edg.F
Chd|        I25GAPMSAVE                   source/interfaces/intsort/i25gapmsave.F
Chd|        I25TRC_E2S                    source/interfaces/intsort/i25trc_e2s.F
Chd|        I25TRC_EDG                    source/interfaces/intsort/i25trc_edg.F
Chd|        I7XSAVE                       source/interfaces/intsort/i7xsave.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_RNUM25                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_RNUM25_EDGE              source/mpi/interfaces/spmd_rnum25_edge.F
Chd|        SPMD_TRI25EGAT                source/mpi/interfaces/spmd_tri25egat.F
Chd|        SPMD_TRI25GAT                 source/mpi/interfaces/spmd_tri25gat.F
Chd|        SPMD_TRI25VOX                 source/mpi/interfaces/spmd_tri25vox.F
Chd|        SPMD_TRI25VOX0                source/mpi/interfaces/spmd_tri25vox0.F
Chd|        SPMD_TRI25VOX0_EDGE           source/mpi/interfaces/spmd_tri25vox0.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        UPGRADE_LCAND_E2S             ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_LCAND_EDG             ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        PARAMETERS_MOD                ../common_source/modules/interfaces/parameters_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25MAIN_TRI(
     1                  IPARI   ,X       ,V       ,INTBUF_TAB,
     2                  MS      ,NIN     ,ITASK   ,WEIGHT  ,
     3                  ISENDTO ,IRCVFROM,RETRI   ,IAD_ELEM,FR_ELEM ,
     4                  ITAB    ,KINET   ,TEMP    ,RENUM   ,
     5                  NSNFIOLD,NUM_IMP ,IND_IMP ,NODNX_SMS,
     6                  H3D_DATA,ESHIFT  ,NEDGE_T ,SSHIFT   ,NRTM_T ,
     7                  ICODT   ,ISKEW   ,PARAMETERS)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
      USE TRI7BOX
      USE INTBUFMOD
      USE MESSAGE_MOD
      USE IMP_INTBUF
      USE INTBUFDEF_MOD
      USE H3D_MOD
      USE MOD_I25MAIN_TRI
      USE PARAMETERS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
      COMMON /I25MAINC/BMINMA,CURV_MAX_MAX,RESULT,NSNR,NSNROLD,I_MEMG,I_MEMG_E,I_MEMG_S,NMN_G
      INTEGER RESULT,NSNR,NSNROLD,I_MEMG,I_MEMG_E,I_MEMG_S,NMN_G
      my_real 
     .        BMINMA(6),CURV_MAX_MAX
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN ,ITASK, RETRI, NEDGE_T, NRTM_T, SSHIFT, ESHIFT,
     .        NUM_IMP ,IND_IMP(*),
     .        ITAB(*), KINET(*),
     .        IPARI(NPARI,NINTER),  
     .        ISENDTO(NINTER+1,*),IRCVFROM(NINTER+1,*),
     .        WEIGHT(*), IAD_ELEM(2,*) ,FR_ELEM(*),
     .        RENUM(*), NSNFIOLD(NSPMD), NODNX_SMS(*), ICODT(*), ISKEW(*)
C     REAL
      my_real 
     .    X(3,*), V(*), MS(*),TEMP(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (PARAMETERS_) ,INTENT(IN):: PARAMETERS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LOC_PROC,IEDGE,NEDGE,IGSTI,ITIED,
     .        I,J, IP0, IP1, IP2, IP21, K11_T, I_SK_OLD, I_SK_OLD_E, I_STOK1, 
     .        ADD1, NOINT, INACTI, MULTIMP, IGAP, IFQ,
     .        N, NSNF, NSNL, NSNRF, NSNRL,NMN_L, IVIS2, IGAP0, IFSUB_CAREA
      INTEGER 
     .        NCONT, NCONTE, MULNSN, MULNSNE, MULNSNS, INACTII, INACIMP, INTTH,
     .        I_MEM,I_MEME(2),CAND_N_OLD,CAND_E_OLD(2),ILEV,FLAGREMN, LREMNORMAX,
     .        IDUM1(1), ITHK

      INTEGER NEDGE_TOTAL,NEDGE_LOCAL
      INTEGER :: ISENS
C     REAL
      my_real
     .   STARTT,GAP,STOPT,DIST,
     .   XMAXL, YMAXL, ZMAXL, XMINL, YMINL, ZMINL, GAPMIN, GAPMAX,DRAD,
     .   C_MAXL,PMAX_GAP,VMAXDT,MARGE,TZINF,SX,SY,SZ,SX2,SY2,SZ2,DGAPLOAD,
     .   CURV_MAX(NRTM_T),BMINMA_OLD(6),BGAPEMX,BSAV(6)
      INTEGER :: NMN,NRTM,NSN,NTY
C-----------------------------------------------
      I_MEM = 0
!$OMP SINGLE
      I_MEMG = 0
      NMN_G = 0
!$OMP END SINGLE NOWAIT
      NMN_L = 0
C
      LOC_PROC=ISPMD+1
      NRTM    = IPARI(4,NIN)
      NSN     = IPARI(5,NIN)
      NMN     = IPARI(6,NIN)
      NTY     = IPARI(7,NIN)
      IVIS2   = IPARI(14,NIN)
      NOINT   = IPARI(15,NIN)
      NCONT   = IPARI(18,NIN)
      ILEV    = IPARI(20,NIN)
      INACTI  = IPARI(22,NIN)
      IFQ     = IPARI(31,NIN)
      INTTH   = IPARI(47,NIN)
      IEDGE   = IPARI(58,NIN)
      FLAGREMN= IPARI(63,NIN)
      IGSTI   = IPARI(34,NIN)
      IGAP    = IPARI(21,NIN)
      IGAP0   = IPARI(53,NIN)
      FLAGREMN =IPARI(63,NIN)
      LREMNORMAX =IPARI(82,NIN)
      NEDGE   = IPARI(68,NIN)
!$OMP SINGLE
      NEDGE_REMOTE_OLD   = IPARI(69,NIN)
      NSNROLD = IPARI(24,NIN)
!$OMP END SINGLE NOWAIT
      NCONTE  = IPARI(88,NIN)
      ITHK    = IPARI(91,NIN)
      IFSUB_CAREA =0
      IF(IPARI(36,NIN)> 0.AND.PARAMETERS%INTCAREA > 0) IFSUB_CAREA = 1
C
C
      ISENS = IPARI(64,NIN)  
      IF(ISENS == 0) THEN
         STOPT =INTBUF_TAB%VARIABLES(11)
         IF(TT > STOPT) RETURN
      ENDIF

      GAP =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      PMAX_GAP=INTBUF_TAB%VARIABLES(23)
      VMAXDT  =INTBUF_TAB%VARIABLES(24)
C
      DRAD = ZERO
      IF(IPARI(47,NIN) > 0)  DRAD =INTBUF_TAB%VARIABLES(32)
C
      DGAPLOAD  =INTBUF_TAB%VARIABLES(46)
C
      ITIED = 0 
C
C -------------------------------------------------------------
C
      DIST = INTBUF_TAB%VARIABLES(5)
C     PRINTIF(ISPMD == 0,DIST)
      IF (DIST>ZERO) RETURN
      RETRI = 1
C
C -------------------------------------------------------------
C
      MARGE  = INTBUF_TAB%VARIABLES(25)
C
C -------------------------------------------------------------
C
      IF(ITASK==0) THEN
        BMINMA(1)=-EP30
        BMINMA(2)=-EP30
        BMINMA(3)=-EP30
        BMINMA(4)=EP30
        BMINMA(5)=EP30
        BMINMA(6)=EP30
        CURV_MAX_MAX = ZERO
        I_SK_OLD = INTBUF_TAB%I_STOK(1)
        INTBUF_TAB%I_STOK(1) = 0
        IF(IEDGE /= 0) ALLOCATE(NSNFIEOLD(NSPMD))
      END IF
C
C wait bminma [and cur_max_max]
C
      CALL MY_BARRIER

      IF(IEDGE/=0) THEN
C
        INACTI  = IPARI(22,NIN)
        IF(ITASK==0)THEN


          NEDGE_TOTAL = NEDGE + NEDGE_REMOTE_OLD
          ALLOCATE(CAND_A(NEDGE_TOTAL + 3)) ! attention SPMD : NEDGE + NEDGEROLD + 3 

          I_SK_OLD_E = INTBUF_TAB%I_STOK_E(1)
          CALL I25TRC_EDG(
     1      NEDGE_TOTAL            ,I_SK_OLD_E ,INTBUF_TAB%CANDS_E2E,INTBUF_TAB%CANDM_E2E,
     2      INTBUF_TAB%CAND_P      ,CAND_A     ,NIN                 ,NEDGE,IFQ           ,
     3      INTBUF_TAB%FTSAVX_E,INTBUF_TAB%FTSAVY_E, INTBUF_TAB%FTSAVZ_E,INTBUF_TAB%IFPEN_E)

          INTBUF_TAB%I_STOK_E(1)=I_SK_OLD_E

          ALLOCATE(CAND_B(NEDGE_TOTAL + 3)) ! attention SPMD : NEDGE + NEDGEROLD + 3 

          I_SK_OLD_E = INTBUF_TAB%I_STOK_E(2)
          CALL I25TRC_E2S(
            ! attention SPMD
     1      NEDGE_TOTAL            ,I_SK_OLD_E ,INTBUF_TAB%CANDS_E2S,
     .                                    INTBUF_TAB%CANDM_E2S,
     2      INTBUF_TAB%CAND_PS,CAND_B     ,NIN               ,NEDGE,
     3      INTBUF_TAB%LEDGE,IFQ          ,INTBUF_TAB%FTSAVX_E2S,
     4      INTBUF_TAB%FTSAVY_E2S, INTBUF_TAB%FTSAVZ_E2S,INTBUF_TAB%IFPEN_E2S )

          INTBUF_TAB%I_STOK_E(2)=I_SK_OLD_E

          
        ENDIF
      ELSE ! IEDGE
          IF(SIZE(INTBUF_TAB%I_STOK_E) > 1 ) THEN
! size is 0 or 2
            INTBUF_TAB%I_STOK_E(1) = 0
            INTBUF_TAB%I_STOK_E(2) = 0
          ENDIF
      ENDIF
C -------------------------------------------------------------
C     Bounds of the domains                                         
C -------------------------------------------------------------
      CALL I7XSAVE(
     1       X            ,INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN ,NMN     ,
     2       ITASK        ,INTBUF_TAB%XSAV,XMINL      ,YMINL ,ZMINL   ,
     3       XMAXL        ,YMAXL        ,ZMAXL        ,C_MAXL,CURV_MAX,
     4       IPARI(39,NIN),INTBUF_TAB%IRECTM(1+4*SSHIFT),NRTM_T       ,SX    ,SY      ,
     5       SZ           ,SX2          ,SY2          ,SZ2            ,NMN_L)
C ITHK == 1 : main gap should be updated with change in thickness
      IF(ITHK == 1) THEN
        CALL I25GAPMSAVE(
     1       INTBUF_TAB%GAP_M ,INTBUF_TAB%GAPMSAV ,
     2       INTBUF_TAB%MSR   ,NRTM               , ITASK)
      ENDIF      
C
#include "lockon.inc"
      BMINMA(1) = MAX(BMINMA(1),XMAXL)
      BMINMA(2) = MAX(BMINMA(2),YMAXL)
      BMINMA(3) = MAX(BMINMA(3),ZMAXL)
      BMINMA(4) = MIN(BMINMA(4),XMINL)
      BMINMA(5) = MIN(BMINMA(5),YMINL)
      BMINMA(6) = MIN(BMINMA(6),ZMINL)
      CURV_MAX_MAX = MAX(CURV_MAX_MAX,C_MAXL)
      NMN_G = NMN_G + NMN_L
      RESULT = 0
#include "lockoff.inc"

C BARRIER II_STOK, II_STOK_E et RESULT
      CALL MY_BARRIER

C a conserver pour cas inacti est modifie sur p0
      IF(ITASK==0)THEN
        BSAV(1:6)=BMINMA(1:6) ! pour boite edge to edge
        IF(ABS(BMINMA(6)-BMINMA(3))>2*EP30.OR.
     +     ABS(BMINMA(5)-BMINMA(2))>2*EP30.OR.
     +     ABS(BMINMA(4)-BMINMA(1))>2*EP30)THEN
          CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I25BUCE)')
          CALL ARRET(2)
        END IF
C
        TZINF = MARGE+MAX(GAP+DGAPLOAD,DRAD)+VMAXDT

        IF(IEDGE > 0)THEN
          BGAPEMX=INTBUF_TAB%VARIABLES(40)
          TZINF = MAX(TZINF,MARGE+TWO*BGAPEMX+DGAPLOAD+VMAXDT)
        ENDIF

        BMINMA(1)=BMINMA(1)+TZINF
        BMINMA(2)=BMINMA(2)+TZINF
        BMINMA(3)=BMINMA(3)+TZINF
        BMINMA(4)=BMINMA(4)-TZINF
        BMINMA(5)=BMINMA(5)-TZINF
        BMINMA(6)=BMINMA(6)-TZINF

        IF(NSPMD > LRVOXELP)THEN
          CALL ANCMSG(MSGID=36,ANMODE=ANINFO,
     .            C1='(I25MAINTRI)')
          CALL ARRET(2)
        END IF

        NSNR = 0

      END IF

      IF(NSPMD > 1) THEN

        IF(ITASK==0) THEN 
          ALLOCATE(CRVOXEL25(0:LRVOXEL25,0:LRVOXEL25,1:2,1:NSPMD))
          CRVOXEL25(0:LRVOXEL25,0:LRVOXEL25,1:2,LOC_PROC) = 0
        ENDIF

        CALL MY_BARRIER
        IF (IMONM > 0 .AND. ITASK == 0) CALL STARTIME(26,1)
        BGAPEMX=INTBUF_TAB%VARIABLES(40)

        CALL SPMD_TRI25VOX0(
     1      X           ,BMINMA  ,NRTM_T,INTBUF_TAB%STFM(1+SSHIFT),MARGE ,
     2      CURV_MAX,INTBUF_TAB%GAP_M(1+SSHIFT),INTBUF_TAB%IRECTM(1+4*SSHIFT),GAP,
     +                                                    INTBUF_TAB%VARIABLES(7),
     3      PMAX_GAP,VMAXDT,BGAPEMX,IEDGE,
     .      INTBUF_TAB%LEDGE,NEDGE,NLEDGE,
     .      INTBUF_TAB%GAPE ,DRAD ,DGAPLOAD)

        CALL SPMD_TRI25VOX0_EDGE(
     1      X           ,BMINMA  ,NRTM,INTBUF_TAB%STFE,MARGE ,
     2      CURV_MAX,INTBUF_TAB%GAP_M,INTBUF_TAB%IRECTM,GAP,
     +                                   INTBUF_TAB%VARIABLES(7),
     3      PMAX_GAP,VMAXDT,BGAPEMX,IEDGE,IGAP0,
     .      INTBUF_TAB%LEDGE,NEDGE,NLEDGE,
     .      INTBUF_TAB%GAPE,DGAPLOAD)

        CALL MY_BARRIER
        IF (IMONM > 0 .AND. ITASK == 0) CALL STOPTIME(26,1)

        IF(ITASK==0)THEN

C
C Get remote nodes in XREM/IREM
C
          INACTI=0
          NEDGE_LOCAL = INTBUF_TAB%NB_INTERNAL_EDGES + INTBUF_TAB%NB_BOUNDARY_EDGES_LOCAL
          IEDGE   = IPARI(58,NIN)
          CALL SPMD_TRI25VOX(
     1      INTBUF_TAB%NSV   ,NSN         ,X               ,V            ,MS           ,
     2      BMINMA          ,WEIGHT       ,INTBUF_TAB%STFNS,NIN          ,ISENDTO      ,
     3      IRCVFROM        ,IAD_ELEM     ,FR_ELEM         ,NSNR         ,IPARI(21,NIN),
     4      INTBUF_TAB%GAP_S,ITAB         ,KINET           ,IFQ          ,INACTI       ,
     5      NSNFIOLD        ,IPARI(47,NIN),INTBUF_TAB%IELES,INTBUF_TAB%AREAS,TEMP      ,
     6      NUM_IMP         ,NODNX_SMS    ,INTBUF_TAB%GAP_SL,NTY         ,INTBUF_TAB%IRTLM,
     7      INTBUF_TAB%TIME_S,INTBUF_TAB%SECND_FR,INTBUF_TAB%PENE_OLD,INTBUF_TAB%STIF_OLD ,
     8      INTBUF_TAB%NBINFLG,ILEV       ,INTBUF_TAB%ICONT_I,IPARI(72,NIN),INTBUF_TAB%IPARTFRICS,
     9      ITIED           ,IVIS2        , INTBUF_TAB%IF_ADH,INTBUF_TAB%LEDGE,NEDGE   ,
     A      NLEDGE          ,INTBUF_TAB%STFM,NEDGE_LOCAL,INTBUF_TAB%GAPE,INTBUF_TAB%GAP_E_L,      
     B      INTBUF_TAB%STFE ,INTBUF_TAB%EDGE_BISECTOR,INTBUF_TAB%VTX_BISECTOR,INTBUF_TAB%ADMSR,
     .                                                                       INTBUF_TAB%IRECTM,
     D      INTBUF_TAB%EBINFLG,INTBUF_TAB%MVOISIN,IEDGE      ,ICODT       , ISKEW            ,
     E      INTBUF_TAB%IPARTFRIC_E,INTBUF_TAB%E2S_NOD_NORMAL,IPARI(97,NIN),INTBUF_TAB%STIFMSDT_S,
     .                                                                     INTBUF_TAB%STIFMSDT_EDG,
     F      IFSUB_CAREA     ,PARAMETERS%INTAREAN)

C local renumbering of old candidates to collision
          CALL SPMD_RNUM25(
     1      RENUM  ,NIN, NSN,NSNFIOLD     ,NSNROLD)
          IF(IEDGE /= 0) THEN
            CALL SPMD_RNUM25_EDGE(NIN,NEDGE,INTBUF_TAB%CANDS_E2E,INTBUF_TAB%I_STOK_E(1),
     .       INTBUF_TAB%CANDS_E2S,INTBUF_TAB%I_STOK_E(2))
          END IF

          IF(ALLOCATED(CRVOXEL25)) DEALLOCATE(CRVOXEL25)
      END IF
      END IF ! ITASK == 0

C
      CAND_N_OLD = INTBUF_TAB%I_STOK(1)
 40   continue
C
C Barrier comm spmd_tri7vox + BMINMA + Retour I7BUCE
C      
      CALL MY_BARRIER
C
      IF (IMONM > 0) CALL STARTIME(30,1)
C
C     NCONT additional candidates (if all secondary nodes are already impacted, cf i25optcd)
      MULTIMP = IPARI(23,NIN)
      MULNSN  = INTBUF_TAB%S_CAND_N - NCONT
      CALL I25BUCE(
     1 X      ,V           ,INTBUF_TAB%IRECTM(1+4*SSHIFT),INTBUF_TAB%NSV,
     +                                                 INTBUF_TAB%STFNS,
     2 NMN    ,NRTM_T      ,NSN            ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     3 GAP    ,NOINT       ,INTBUF_TAB%I_STOK(1)   ,MULNSN     ,BMINMA       ,
     4 MARGE  ,CURV_MAX    ,PMAX_GAP       ,VMAXDT       ,
     5 SSHIFT ,NIN         ,INTBUF_TAB%STFM(1+SSHIFT) ,INTBUF_TAB%GAP_S,
     6 NSNR   ,NCONT       ,INTBUF_TAB%GAP_M(1+SSHIFT)   ,ITASK      ,INTBUF_TAB%VARIABLES(7),
     7 I_MEM  ,INTBUF_TAB%PENE_OLD,ITAB          ,INTBUF_TAB%NBINFLG,INTBUF_TAB%MBINFLG,
     8 ILEV   ,INTBUF_TAB%MSEGTYP24,
     9 FLAGREMN,INTBUF_TAB%KREMNODE(1+2*SSHIFT),INTBUF_TAB%REMNODE,
     A IGAP   ,INTBUF_TAB%GAP_SL,INTBUF_TAB%GAP_ML(1+SSHIFT),ICODT,ISKEW   ,
     B DRAD   ,DGAPLOAD    )
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
!$OMP SINGLE
        MULTIMP = IPARI(23,NIN) * 1.3
        CALL UPGRADE_MULTIMP(NIN,MULTIMP,INTBUF_TAB)
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
        I_MEM = 0
        I_MEMG = 0
        INTBUF_TAB%I_STOK(1)=CAND_N_OLD
        GOTO 40
      ENDIF

      IF (IMONM > 0) CALL STOPTIME(30,1)
C--------------------------------------------------------------
C     Edges
C--------------------------------------------------------------
      IF(IEDGE==0) GOTO 200
      INACTI  = IPARI(22,NIN)
      CAND_E_OLD(1:2) = INTBUF_TAB%I_STOK_E(1:2)
      BGAPEMX=INTBUF_TAB%VARIABLES(40)
      IF(ITASK == 0)THEN

        BMINMA(1)=BSAV(1)
        BMINMA(2)=BSAV(2)
        BMINMA(3)=BSAV(3)
        BMINMA(4)=BSAV(4)
        BMINMA(5)=BSAV(5)
        BMINMA(6)=BSAV(6)

        TZINF = MARGE+TWO*BGAPEMX+DGAPLOAD+VMAXDT

        BMINMA(1)=BMINMA(1)+TZINF
        BMINMA(2)=BMINMA(2)+TZINF
        BMINMA(3)=BMINMA(3)+TZINF
        BMINMA(4)=BMINMA(4)-TZINF
        BMINMA(5)=BMINMA(5)-TZINF
        BMINMA(6)=BMINMA(6)-TZINF

      END IF
C
      I_MEME(1:2)=0
      I_MEMG_E = 0
      I_MEMG_S = 0
C
 140  continue
C
C Barrier comm spmd_tri7vox + BMINMA + Retour I7BUCE
C      
      CALL MY_BARRIER
C
      IF (IMONM > 0) CALL STARTIME(30,1)
C
      MULNSNE = INTBUF_TAB%S_CANDM_E2E
      MULNSNS = INTBUF_TAB%S_CANDM_E2S
      NEDGE_LOCAL = INTBUF_TAB%NB_INTERNAL_EDGES + INTBUF_TAB%NB_BOUNDARY_EDGES_LOCAL
      CALL I25BUCE_EDG(
     1 X      ,V           ,INTBUF_TAB%IRECTM,INACTI ,
     2 NSN    ,NMN    ,INTBUF_TAB%CANDM_E2E,INTBUF_TAB%CANDS_E2E,
     3 GAP    ,NOINT       ,INTBUF_TAB%I_STOK_E(1) ,MULNSNE    ,BMINMA       ,
     4 MARGE  ,VMAXDT      ,DRAD                   ,
     5 ESHIFT ,NEDGE_T     ,SSHIFT ,NRTM_T      ,INTBUF_TAB%STFM ,
     6 INTBUF_TAB%STFE ,NCONTE ,INTBUF_TAB%GAP_M  ,ITASK ,BGAPEMX,
     7 I_MEME ,ITAB        ,INTBUF_TAB%MBINFLG,INTBUF_TAB%EBINFLG,INTBUF_TAB%I_STOK_E(2),
     8 MULNSNS,ILEV        ,CAND_A      ,INTBUF_TAB%CAND_P ,IGAP0 ,
     9 FLAGREMN,INTBUF_TAB%KREMNODE_EDG ,INTBUF_TAB%REMNODE_EDG,INTBUF_TAB%KREMNODE_E2S,
     .                                                          INTBUF_TAB%REMNODE_E2S,
     A IGAP   ,INTBUF_TAB%GAP_ML,IEDGE ,NEDGE ,INTBUF_TAB%MSEGTYP24,
     B INTBUF_TAB%LEDGE,INTBUF_TAB%ADMSR,INTBUF_TAB%EDGE_BISECTOR,INTBUF_TAB%VTX_BISECTOR,
     C INTBUF_TAB%CANDM_E2S,INTBUF_TAB%CANDS_E2S,CAND_B,INTBUF_TAB%CAND_PS,INTBUF_TAB%GAPE,
     D INTBUF_TAB%GAP_E_L,NEDGE_LOCAL,IFQ  ,       INTBUF_TAB%FTSAVX_E,INTBUF_TAB%FTSAVY_E,
     E INTBUF_TAB%FTSAVZ_E,INTBUF_TAB%FTSAVX_E2S,INTBUF_TAB%FTSAVY_E2S, INTBUF_TAB%FTSAVZ_E2S,
     F INTBUF_TAB%IFPEN_E,INTBUF_TAB%IFPEN_E2S,INTBUF_TAB%S_KREMNODE_EDG ,INTBUF_TAB%S_REMNODE_EDG,
     G INTBUF_TAB%S_KREMNODE_E2S,INTBUF_TAB%S_REMNODE_E2S,DGAPLOAD)
C
C Upgrade MultiMP
      IF (I_MEME(1)/=0)THEN
#include "lockon.inc"
         I_MEMG_E = I_MEME(1)
#include "lockoff.inc"
      ENDIF
      IF (I_MEME(2)/=0)THEN
#include "lockon.inc"
         I_MEMG_S = I_MEME(2)
#include "lockoff.inc"
      ENDIF

C New barrier needed for Dynamic MultiMP
      CALL MY_BARRIER

      IF(I_MEMG_E /=0 .OR. I_MEMG_S/=0)THEN
!$OMP SINGLE
        IF(I_MEMG_E/=0)THEN
          !              same as TYPE11:     increase > 4 for small interfaces
          MULTIMP = MAX(IPARI(87,NIN) +4,IPARI(87,NIN)+MIN(20,(250000/NCONTE)))
          CALL UPGRADE_LCAND_EDG(NIN,MULTIMP,INTBUF_TAB)
        END IF
        IF(I_MEMG_S/=0)THEN
          !              same as TYPE11:     increase > 4 for small interfaces
          MULTIMP = MAX(IPARI(89,NIN) +4,IPARI(89,NIN)+MIN(20,(250000/NCONTE)))
          CALL UPGRADE_LCAND_E2S(NIN,MULTIMP,INTBUF_TAB)
        END IF
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
        I_MEME(1:2)= 0
        I_MEMG_E  = 0
        I_MEMG_S  = 0
        INTBUF_TAB%I_STOK_E(1:2)=CAND_E_OLD(1:2) ! on retrie tout
        GOTO 140
      ENDIF
C
      IF(ITASK==0)DEALLOCATE(CAND_A,CAND_B)
C
      IF (IMONM > 0) CALL STOPTIME(30,1)
C--------------------------------------------------------------
 200  CONTINUE
C--------------------------------------------------------------
C Negative value in "dist" : tag boundary part
      IF(NSPMD>1)THEN
!$OMP SINGLE
        IF (IMONM > 0) CALL STARTIME(26,1)
        INTBUF_TAB%VARIABLES(5) = - ONE
C
        CALL SPMD_TRI25GAT(
     1      RESULT       ,NSN ,INTBUF_TAB%CAND_N,INTBUF_TAB%I_STOK(1),NIN,
     2      IPARI(21,NIN),NSNR,MULTIMP      ,NTY,IPARI(47,NIN),
     3      ILEV         ,NSNFIOLD,IPARI   ,NSNROLD, RENUM, H3D_DATA ,
     4      IPARI(72,NIN),FLAGREMN,LREMNORMAX,NRTM ,INTBUF_TAB%KREMNODE,
     5      INTBUF_TAB%REMNODE,IVIS2,IPARI(97,NIN),IFSUB_CAREA )
        IPARI(24,NIN) = NSNR

        NEDGE_REMOTE_OLD = NEDGE_REMOTE

        IF(IEDGE /= 0) THEN
        CALL SPMD_TRI25EGAT(
     1        RESULT       ,NIN       , NEDGE,INTBUF_TAB%CANDS_E2E,INTBUF_TAB%I_STOK_E(1),
     2        INTBUF_TAB%CANDS_E2S,INTBUF_TAB%I_STOK_E(2),IGAP,IPARI(72,NIN),IPARI(97,NIN))      
        ENDIF

        IPARI(69 ,NIN) = NEDGE_REMOTE_OLD
C
        IF (IMONM > 0) CALL STOPTIME(26,1)
        IF(IEDGE /= 0) THEN
            DEALLOCATE(RENUM_EDGE)
            DEALLOCATE(OLDNUM_EDGE)
        ENDIF
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
      END IF
        IF(IEDGE /= 0) THEN
!$OMP SINGLE
            DEALLOCATE(NSNFIEOLD)
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
        ENDIF
C
      CALL MY_BARRIER
      RETURN
      END
