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
Chd|  I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|-- called by -----------
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I18XSAVE                      source/interfaces/int18/i18xsave.F
Chd|        I7BUCE                        source/interfaces/intsort/i7buce.F
Chd|        I7BUCE_VOX                    source/interfaces/intsort/i7buce.F
Chd|        I7TRC                         source/interfaces/intsort/i7trc.F
Chd|        I7XSAVE                       source/interfaces/intsort/i7xsave.F
Chd|        IMP_RNUMCD                    source/implicit/imp_int_k.F   
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_GET_INACTI7              source/mpi/interfaces/send_cand.F
Chd|        SPMD_RNUMCD                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_TRI18_151VOX             source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI7GAT                  source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI7VOX0                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI7VOX_OPTIMIZED        source/mpi/interfaces/spmd_tri7vox_optimized.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7MAIN_TRI(
     1                  IPARI   ,X       ,V       ,
     2                  MS      ,NIN     ,ITASK   ,MWAG    ,WEIGHT  ,
     3                  ISENDTO ,IRCVFROM,RETRI   ,IAD_ELEM,FR_ELEM ,
     4                  ITAB    ,KINET   ,TEMP    ,NRTM_T  ,RENUM   ,
     5                  NSNFIOLD,ESHIFT  ,NUM_IMP ,IND_IMP ,NODNX_SMS,
     6                  INTBUF_TAB,H3D_DATA,IXS,MULTI_FVM)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE H3D_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#ifdef MPI
#include      "mpif.h"
#endif
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
C common pour variable globale en memoire partagee
      COMMON /I7MAINC/BMINMA,CURV_MAX_MAX,RESULT,NSNR,NSNROLD,I_MEMG,NMN_G
      INTEGER RESULT,NSNR,NSNROLD,I_MEMG,NMN_G
      my_real 
     .        BMINMA(12),CURV_MAX_MAX
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN ,ITASK, RETRI, NRTM_T,ESHIFT,
     .        NUM_IMP ,IND_IMP(*),
     .        ITAB(*), KINET(*),
     .        IPARI(NPARI,NINTER),  MWAG(*),
     .        ISENDTO(NINTER+1,*),IRCVFROM(NINTER+1,*),
     .        WEIGHT(*), IAD_ELEM(2,*) ,FR_ELEM(*),
     .        RENUM(*), NSNFIOLD(NSPMD), NODNX_SMS(*), IXS(NIXS, *)
C     REAL ou REAL*8
      my_real 
     .   X(*), V(*), MS(*),TEMP(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        LOC_PROC,
     .        I, IP0, IP1, IP2, IP21, I_SK_OLD, I_STOK1, 
     .        ADD1, NB_N_B, NOINT, INACTI, MULTIMP, IGAP, IFQ, ITIED
      INTEGER 
     .        ILD, NCONT, NCONTACT, INACTII, INACIMP, INTTH,
     .        I_MEM,CAND_N_OLD,IDUM1(1),NMN_L, IVIS2
      my_real
     .   STARTT,GAP,MAXBOX,MINBOX,STOPT,DIST,TZINF,
     .   XMAXL, YMAXL, ZMAXL, XMINL, YMINL, ZMINL, GAPMIN, GAPMAX,
     .   C_MAXL,DRAD,MX,MY,MZ,DX,DY,DZ,SX,SY,SZ,SX2,SY2,SZ2,
     .   CURV_MAX(NRTM_T),RDUM1(1)
      REAL T1  !elapsed time in smp
      LOGICAL TYPE18
C-----------------------------------------------
#include "sync_dir.inc"
C-----------------------------------------------
      I_MEM  = 0
      I_MEMG = 0
      NMN_G  = 0
      NMN_L  = 0
C

      NRTM    =IPARI(4,NIN)
      NSN     =IPARI(5,NIN)
      NMN     =IPARI(6,NIN)
      NTY     =IPARI(7,NIN)
      IVIS2   =IPARI(14,NIN)
      NOINT   =IPARI(15,NIN)
      NCONT   =IPARI(18,NIN)
      INACTI  =IPARI(22,NIN)
      MULTIMP =IPARI(23,NIN)
      IFQ     =IPARI(31,NIN)
      INTTH   =IPARI(47,NIN)
      ITIED   =IPARI(85,NIN)
      
      LOC_PROC=ISPMD+1      
      NCONTACT=MULTIMP*NCONT
      
      TYPE18=.FALSE.
      IF(NTY==7 .AND. INACTI==7)TYPE18=.TRUE.

      IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7.OR.IFQ.GT.0.OR.
     .   NUM_IMP.GT.0.OR.ITIED.NE.0)THEN
        NSNROLD = IPARI(24,NIN)
      ELSE
        NSNROLD = 0
      ENDIF

      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
      GAP    =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      DRAD = ZERO
      IF(IPARI(7,NIN)==7) DRAD =INTBUF_TAB%VARIABLES(32)
C
C
C -------------------------------------------------------------
C
      DIST = INTBUF_TAB%VARIABLES(5)
      IF (DIST.GT.ZERO) RETURN
      RETRI = 1
C
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
      BMINMA(7)=ZERO
      BMINMA(8)=ZERO
      BMINMA(9)=ZERO
      BMINMA(10)=ZERO
      BMINMA(11)=ZERO
      BMINMA(12)=ZERO
      CURV_MAX_MAX = ZERO
C
C -------------------------------------------------------------
C     STOCKAGE DES ANCIENS CANDIDATS AVEC PENE INITIALE
C     OU AVEC DU FILTRAGE DE FROTTEMENT
C -------------------------------------------------------------
C
C Barriere dans tous les cas pour bminma [et cur_max_max]
C
      CALL MY_BARRIER
C
      IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7.OR.IFQ.GT.0.OR.
     .   NUM_IMP.GT.0.OR.ITIED.NE.0)THEN
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
C MWA = MWAG SUR TASK 0
          I_SK_OLD = INTBUF_TAB%I_STOK(1)
          CALL I7TRC(
     1      NSN+NSNROLD      ,I_SK_OLD         ,INTBUF_TAB%CAND_N,INTBUF_TAB%CAND_E,
     2      INTBUF_TAB%CAND_P,INTBUF_TAB%FTSAVX,INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ,
     3      MWAG(IP0)        ,INTBUF_TAB%IFPEN ,INACTI           ,IFQ              ,
     4      NUM_IMP          ,IND_IMP          ,INTBUF_TAB%STFNS ,NIN              ,
     5      NSN ,ITIED,INTBUF_TAB%CAND_F )
C stockage INACTI en negatif
          IF(I_SK_OLD.EQ.0)INACTI=-ABS(INACTI)
          INTBUF_TAB%I_STOK(1)=I_SK_OLD
          IF(INACTII.NE.7.AND.INACIMP.GT.0)THEN
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
        INTBUF_TAB%I_STOK(1)=0
      ENDIF
C -------------------------------------------------------------
C     CALCUL BORNE DOMAINE REMONTE DANS I7XSAVE
C -------------------------------------------------------------
C eshift : decalage sur cand_e
      IF(TYPE18)THEN
        CALL I18XSAVE(
     1         X            ,INTBUF_TAB%NSV ,INTBUF_TAB%MSR,NSN  ,NMN ,
     2         ITASK        ,INTBUF_TAB%XSAV,XMINL  ,YMINL ,ZMINL ,
     3         XMAXL        ,YMAXL          ,ZMAXL       ,C_MAXL,CURV_MAX,
     4         IPARI(39,NIN),INTBUF_TAB%IRECTM(1+4*ESHIFT) ,NRTM_T,SX,SY,
     5         SZ           ,SX2          ,SY2          ,SZ2      ,NMN_L )
      ELSE
        CALL I7XSAVE(
     1         X            ,INTBUF_TAB%NSV ,INTBUF_TAB%MSR,NSN  ,NMN ,
     2         ITASK        ,INTBUF_TAB%XSAV,XMINL  ,YMINL ,ZMINL ,
     3         XMAXL        ,YMAXL          ,ZMAXL       ,C_MAXL,CURV_MAX,
     4         IPARI(39,NIN),INTBUF_TAB%IRECTM(1+4*ESHIFT) ,NRTM_T,SX,SY,
     5         SZ           ,SX2          ,SY2          ,SZ2      ,NMN_L )
       ENDIF
#include "lockon.inc"
      BMINMA(1) = MAX(BMINMA(1),XMAXL)
      BMINMA(2) = MAX(BMINMA(2),YMAXL)
      BMINMA(3) = MAX(BMINMA(3),ZMAXL)
      BMINMA(4) = MIN(BMINMA(4),XMINL)
      BMINMA(5) = MIN(BMINMA(5),YMINL)
      BMINMA(6) = MIN(BMINMA(6),ZMINL)
      CURV_MAX_MAX = MAX(CURV_MAX_MAX,C_MAXL)
      BMINMA(7) = BMINMA(7)+SX
      BMINMA(8) = BMINMA(8)+SY
      BMINMA(9) = BMINMA(9)+SZ
      BMINMA(10)= BMINMA(10)+SX2
      BMINMA(11)= BMINMA(11)+SY2
      BMINMA(12)= BMINMA(12)+SZ2
      NMN_G = NMN_G + NMN_L
#include "lockoff.inc"

      RESULT = 0
C BARRIER II_STOK et RESULT
      CALL MY_BARRIER
C a conserver pour cas inacti est modifie sur p0
      INACTI=IPARI(22,NIN)
      IF(ITASK.EQ.0)THEN
        IF(ABS(BMINMA(6)-BMINMA(3)).GT.2*EP30.OR.
     +     ABS(BMINMA(5)-BMINMA(2)).GT.2*EP30.OR.
     +     ABS(BMINMA(4)-BMINMA(1)).GT.2*EP30)THEN
          CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .                I1=NOINT,C1='(I7BUCE)')
          CALL ARRET(2)
        END IF
C
        BMINMA(1)=BMINMA(1)+TZINF+CURV_MAX_MAX
        BMINMA(2)=BMINMA(2)+TZINF+CURV_MAX_MAX
        BMINMA(3)=BMINMA(3)+TZINF+CURV_MAX_MAX
        BMINMA(4)=BMINMA(4)-TZINF-CURV_MAX_MAX
        BMINMA(5)=BMINMA(5)-TZINF-CURV_MAX_MAX
        BMINMA(6)=BMINMA(6)-TZINF-CURV_MAX_MAX
C Computation of standard deviation of X master
C use the formula dev = sum(xi  )-n.m  
C mean value m by direction
        MX=BMINMA(7)/MAX(NMN_G,1)
        MY=BMINMA(8)/MAX(NMN_G,1)
        MZ=BMINMA(9)/MAX(NMN_G,1)
c        print*,noint,'m=',mx,my,mz,NMN,NRTM
C standard deviation by direction
C        DX=SQRT(BMINMA(10)/MAX(NMN,1)-MX**2)
C        DY=SQRT(BMINMA(11)/MAX(NMN,1)-MY**2)
C        DZ=SQRT(BMINMA(12)/MAX(NMN,1)-MZ**2)
        DX=SQRT(MAX(BMINMA(10)/MAX(NMN_G,1)-MX**2,ZERO))
        DY=SQRT(MAX(BMINMA(11)/MAX(NMN_G,1)-MY**2,ZERO))
        DZ=SQRT(MAX(BMINMA(12)/MAX(NMN_G,1)-MZ**2,ZERO))
c        print*,noint,'var=',dx,dy,dz
C Computation of new boundary of the domain mean values +/- 2 sigma
C => 95% of the population for normal distribution
        BMINMA(7)  = MIN(MX+2*DX,BMINMA(1))
        BMINMA(8)  = MIN(MY+2*DY,BMINMA(2))
        BMINMA(9)  = MIN(MZ+2*DZ,BMINMA(3))
        BMINMA(10) = MAX(MX-2*DX,BMINMA(4))
        BMINMA(11) = MAX(MY-2*DY,BMINMA(5))
        BMINMA(12) = MAX(MZ-2*DZ,BMINMA(6))
C
        IF(ABS(BMINMA(10)-BMINMA(7))<EM10)THEN
          BMINMA(10)=BMINMA(4)
          BMINMA(7)=BMINMA(1)
        END IF
        IF(ABS(BMINMA(11)-BMINMA(8))<EM10)THEN
          BMINMA(11)=BMINMA(5)
          BMINMA(8)=BMINMA(2)
        END IF
        IF(ABS(BMINMA(12)-BMINMA(9))<EM10)THEN
          BMINMA(12)=BMINMA(6)
          BMINMA(9)=BMINMA(3)
        END IF
        
        IF(NSPMD > LRVOXELP)THEN
          CALL ANCMSG(MSGID=36,ANMODE=ANINFO,
     .            C1='(I7MAINTRI)')
          CALL ARRET(2)
        END IF

        NSNR = 0

      END IF


      IF(NSPMD > 1) THEN

        IF(ITASK.EQ.0) CRVOXEL(0:LRVOXEL,0:LRVOXEL,LOC_PROC)=0

        CALL MY_BARRIER

        IF (IMONM > 0 .AND. ITASK == 0) CALL STARTIME(26,1)
        CALL SPMD_TRI7VOX0(
     1      X           ,BMINMA  ,IPARI(21,NIN),NRTM_T,INTBUF_TAB%STFM(1+ESHIFT),
     2      TZINF       ,CURV_MAX,GAPMIN ,GAPMAX,INTBUF_TAB%GAP_M(1+ESHIFT),
     3      INTBUF_TAB%IRECTM(1+4*ESHIFT),GAP  ,INTBUF_TAB%VARIABLES(7),DRAD  )
 
        CALL MY_BARRIER
        IF (IMONM > 0 .AND. ITASK == 0) CALL STOPTIME(26,1)

        IF(ITASK.EQ.0)THEN
C
C recuperation des noeuds remote NSNR stockes dans XREM
C    
           IF (MULTI_FVM%IS_USED .AND. NTY .EQ. 7 .AND. INACTI .EQ. 7) THEN
C     Interface type 18 and law151
              CALL SPMD_TRI18_151VOX(
     1             INTBUF_TAB%NSV,NSN     ,X            ,V        ,MS  ,
     2             BMINMA       ,WEIGHT  ,INTBUF_TAB%STFNS,NIN  ,ISENDTO,
     3             IRCVFROM     ,IAD_ELEM,FR_ELEM      ,NSNR   ,IPARI(21,NIN),
     4             INTBUF_TAB%GAP_S,ITAB    ,KINET        ,IFQ    ,INACTI ,
     5             NSNFIOLD,IPARI(47,NIN),INTBUF_TAB%IELEC,INTBUF_TAB%AREAS,TEMP   ,
     6             NUM_IMP     ,NODNX_SMS,INTBUF_TAB%GAP_SL,NTY   ,IDUM1  ,
     7             RDUM1       ,RDUM1,RDUM1,RDUM1,IDUM1  ,IDUM1 ,IDUM1, IXS, MULTI_FVM,
     8             IPARI(72,NIN),INTBUF_TAB%IPARTFRICS)
              
           ELSE
              CALL SPMD_TRI7VOX_OPTIMIZED(
     1             INTBUF_TAB%NSV,NSN     ,X            ,V        ,MS  ,
     2             BMINMA       ,WEIGHT  ,INTBUF_TAB%STFNS,NIN  ,ISENDTO,
     3             IRCVFROM     ,IAD_ELEM,FR_ELEM      ,NSNR   ,IPARI(21,NIN),
     4             INTBUF_TAB%GAP_S,ITAB    ,KINET        ,IFQ    ,INACTI ,
     5             NSNFIOLD,IPARI(47,NIN),INTBUF_TAB%IELEC,INTBUF_TAB%AREAS,TEMP   ,
     6             NUM_IMP     ,NODNX_SMS,INTBUF_TAB%GAP_SL,NTY   ,IDUM1  ,
     7             RDUM1       ,RDUM1,RDUM1,RDUM1,IDUM1  ,IDUM1   ,IDUM1 ,
     8             IPARI(72,NIN),INTBUF_TAB%IPARTFRICS   ,ITIED, IVIS2, INTBUF_TAB%IF_ADH)

           ENDIF
C
C renumerotation locale des anciens candidats
C
          IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7.OR.
     +       IFQ.GT.0.OR.NUM_IMP.GT.0.OR.ITIED.NE.0)THEN
            CALL SPMD_RNUMCD(
     1        INTBUF_TAB%CAND_N,RENUM  ,INTBUF_TAB%I_STOK(1), NIN,NSN,
     2        NSNFIOLD     ,NSNROLD)
          END IF
        END IF
      END IF

      CAND_N_OLD = INTBUF_TAB%I_STOK(1)
 40   CONTINUE
C
      ILD = 0
      NB_N_B = 1
C
C Barrier comm spmd_tri7box + BMINMA + Retour I7BUCE
C     IF(ITASK == 0) THEN
c       IF(INTBUF_TAB%METRIC%CYCLE0 == 10000) THEN
c         INTBUF_TAB%METRIC%CYCLE0 = 0
c         INTBUF_TAB%METRIC%ALGO = TRY_ALGO_VOXEL
c       ELSE
c         INTBUF_TAB%METRIC%CYCLE0 = INTBUF_TAB%METRIC%CYCLE0+1
c       ENDIF
C       INTBUF_TAB%METRIC%ALGO = ALGO_VOXEL 
C       INTBUF_TAB%METRIC%ALGO = ALGO_BUCKET
C      ENDIF

 50   CALL MY_BARRIER
!     IF REMNODE Then VOXEL                               
      IF(IPARI(63,NIN) ==2 ) INTBUF_TAB%METRIC%ALGO = ALGO_VOXEL 
 
C
#ifdef MPI
      IF(ITASK == 0) INTBUF_TAB%METRIC%TIC = MPI_WTIME()
#else
      IF(ITASK == 0) THEN
        CALL CPU_TIME(T1)
        INTBUF_TAB%METRIC%TIC = NINT(100.0 * T1) 
      ENDIF
#endif
      IF (IMONM > 0 .AND. ITASK == 0) CALL STARTIME(30,1)
C
      IF(INTBUF_TAB%METRIC%ALGO == ALGO_VOXEL .OR.  INTBUF_TAB%METRIC%ALGO == TRY_ALGO_VOXEL) THEN
        CALL I7BUCE_VOX(
     1 X      ,INTBUF_TAB%IRECTM(1+4*ESHIFT),INTBUF_TAB%NSV  ,INACTI    ,INTBUF_TAB%CAND_P,
     2 NMN_G  ,NRTM_T                       ,NSN     ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     3 GAP    ,NOINT       ,INTBUF_TAB%I_STOK(1) ,NCONTACT ,BMINMA ,
     4 TZINF  ,MAXBOX      ,MINBOX         ,MWAG         ,CURV_MAX  ,
     6 NB_N_B ,ESHIFT      ,ILD            ,IFQ    ,INTBUF_TAB%IFPEN,
     8 INTBUF_TAB%STFNS,NIN ,INTBUF_TAB%STFM(1+ESHIFT),IPARI(21,NIN),INTBUF_TAB%GAP_S,
     A NSNR   ,NCONT  ,RENUM  ,NSNROLD  ,INTBUF_TAB%GAP_M(1+ESHIFT),
     B GAPMIN ,GAPMAX      ,CURV_MAX_MAX   ,NUM_IMP   ,INTBUF_TAB%GAP_SL,
     C INTBUF_TAB%GAP_ML(1+ESHIFT),INTTH ,ITASK  , INTBUF_TAB%VARIABLES(7),I_MEM     ,  
     D INTBUF_TAB%KREMNODE(1+2*ESHIFT),INTBUF_TAB%REMNODE,ITAB , IPARI(63,NIN),DRAD         ,
     E ITIED ,INTBUF_TAB%CAND_F)
      ELSE
        CALL I7BUCE(
     1 X      ,INTBUF_TAB%IRECTM(1+4*ESHIFT),INTBUF_TAB%NSV  ,INACTI    ,INTBUF_TAB%CAND_P,
     2 NMN_G  ,NRTM_T                       ,NSN     ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     3 GAP    ,NOINT       ,INTBUF_TAB%I_STOK(1) ,NCONTACT ,BMINMA ,
     4 TZINF  ,MAXBOX      ,MINBOX         ,MWAG         ,CURV_MAX  ,
     6 NB_N_B ,ESHIFT      ,ILD            ,IFQ    ,INTBUF_TAB%IFPEN,
     8 INTBUF_TAB%STFNS,NIN ,INTBUF_TAB%STFM(1+ESHIFT),IPARI(21,NIN),INTBUF_TAB%GAP_S,
     A NSNR   ,NCONT  ,RENUM  ,NSNROLD  ,INTBUF_TAB%GAP_M(1+ESHIFT),
     B GAPMIN ,GAPMAX      ,CURV_MAX_MAX   ,NUM_IMP   ,INTBUF_TAB%GAP_SL,
     C INTBUF_TAB%GAP_ML(1+ESHIFT),INTTH ,ITASK  , INTBUF_TAB%VARIABLES(7),I_MEM     ,  
     D INTBUF_TAB%KREMNODE(1+2*ESHIFT),INTBUF_TAB%REMNODE,ITAB , IPARI(63,NIN),DRAD         ,
     E ITIED ,INTBUF_TAB%CAND_F)

      ENDIF
      
      IF (I_MEM >= 1 )THEN
#include "lockon.inc"
         I_MEMG = I_MEM
#include "lockoff.inc"
      ENDIF

C New barrier needed for Dynamic MultiMP
      CALL MY_BARRIER

#ifdef MPI
      IF(ITASK == 0 ) INTBUF_TAB%METRIC%TOC = MPI_WTIME()
#else
      IF(ITASK == 0) THEN
        CALL CPU_TIME(T1)
        INTBUF_TAB%METRIC%TOC = NINT(100.0 * T1) 
      ENDIF
#endif


      IF(I_MEMG /=0)THEN
        IF(I_MEMG == 3 .OR. I_MEMG == 1) INTBUF_TAB%METRIC%ALGO = ALGO_VOXEL
C CARE : JINBUF & JBUFIN array are reallocated in
C        UPGRADE_MULTIMP routine !!!!

!$OMP SINGLE
        MULTIMP = IPARI(23,NIN) + 4
        CALL UPGRADE_MULTIMP(NIN,MULTIMP,INTBUF_TAB)
!$OMP END SINGLE
        I_MEM = 0
        I_MEMG = 0
        INTBUF_TAB%I_STOK(1) = CAND_N_OLD
        MULTIMP=IPARI(23,NIN)
        NCONTACT=MULTIMP*NCONT
        GOTO 40
      ENDIF

C
      IF (IMONM > 0 .AND. ITASK == 0) CALL STOPTIME(30,1)
      IF( ITASK == 0) THEN
        IF( INTBUF_TAB%METRIC%ALGO == TRY_ALGO_VOXEL) THEN ! if test phase
          INTBUF_TAB%METRIC%ALGO =  TRY_ALGO_BUCKET
          INTBUF_TAB%METRIC%TOLD =  INTBUF_TAB%METRIC%TOC -  INTBUF_TAB%METRIC%TIC
        ELSEIF ( INTBUF_TAB%METRIC%ALGO == TRY_ALGO_BUCKET) THEN
          IF( 1.2D0 * (INTBUF_TAB%METRIC%TOC-INTBUF_TAB%METRIC%TIC) < INTBUF_TAB%METRIC%TOLD) THEN
            INTBUF_TAB%METRIC%ALGO = ALGO_BUCKET
            WRITE(IOUT,*) "INFO: DOMAIN",ISPMD,
     .                    "USES SORT2 FOR CONTACT INTERFACE",NOINT
          ELSE
            INTBUF_TAB%METRIC%ALGO = ALGO_VOXEL
c           WRITE(IOUT,*) "INFO: DOMAIN",ISPMD,
c    .                    "USES SORT1 FOR CONTACT INTERFACE",NOINT
          ENDIF
        ENDIF
      ENDIF
C
#include "lockon.inc"
      INTBUF_TAB%VARIABLES(9)  = MIN(MAXBOX,INTBUF_TAB%VARIABLES(9))
      INTBUF_TAB%VARIABLES(12) = MIN(MINBOX,INTBUF_TAB%VARIABLES(12))
      INTBUF_TAB%VARIABLES(8)  = MIN(TZINF,INTBUF_TAB%VARIABLES(8))
      INTBUF_TAB%VARIABLES(5)  = INTBUF_TAB%VARIABLES(8)-GAP
      RESULT        = RESULT + ILD
#include "lockoff.inc"
C--------------------------------------------------------------
C--------------------------------------------------------------
      CALL MY_BARRIER
      IF (RESULT.NE.0) THEN
        CALL MY_BARRIER
        IF (ITASK.EQ.0) THEN
C utile si on revient
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
C
        CALL SPMD_TRI7GAT(
     1      RESULT       ,NSN     ,INTBUF_TAB%CAND_N,INTBUF_TAB%I_STOK(1),NIN,
     2      IPARI(21,NIN),NSNR    ,MULTIMP        ,NTY         ,IPARI(47,NIN),  
     3      IDUM1        ,NSNFIOLD, IPARI        , H3D_DATA    ,IPARI(72,NIN),
     4      MULTI_FVM)
        IPARI(24,NIN) = NSNR
C
        IF (NUM_IMP.GT.0) 
     .     CALL IMP_RNUMCD(INTBUF_TAB%CAND_N,NIN,NSN,NUM_IMP,IND_IMP )
C
        IF (IMONM > 0) CALL STOPTIME(26,1)
!$OMP END SINGLE
      END IF
C
      RETURN
      END
