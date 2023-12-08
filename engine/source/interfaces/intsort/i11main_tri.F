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
C  |         I10XSAVE                        /intsort/i10xsave.F
Chd|====================================================================
Chd|  I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|-- called by -----------
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I10XSAVE                      source/interfaces/intsort/i10xsave.F
Chd|        I11BUCE_VOX                   source/interfaces/intsort/i11buce.F
Chd|        I11TRC                        source/interfaces/intsort/i11trc.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_RNUMCD11                 source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_TRI11GAT                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI11VOX                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI11VOX0                source/mpi/interfaces/spmd_int.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11MAIN_TRI(
     1                  IPARI   ,X       ,V       ,
     2                  MS      ,NIN   ,ITASK   ,WEIGHT  ,ISENDTO ,
     3                  IRCVFROM,RETRI ,IAD_ELEM,FR_ELEM ,ITAB    ,
     4                  NRTM_T  ,ESHIFT,NODNX_SMS,RENUM,NSNFIOLD  ,
     5                  INTBUF_TAB,TEMP)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TRI7BOX
      USE INTBUFDEF_MOD
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
      COMMON /I11MAINC/BMINMA,RESULT,NRTSR,I_MEMG,NSNROLD
      INTEGER RESULT,NRTSR,I_MEMG,NSNROLD
      my_real 
     .        BMINMA(6)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN ,ITASK, RETRI, NRTM_T, ESHIFT
      INTEGER IPARI(NPARI,NINTER),  ITAB(*),
     .        WEIGHT(*), IAD_ELEM(2,*) ,FR_ELEM(*),
     .        ISENDTO(NINTER+1,*),IRCVFROM(NINTER+1,*),NODNX_SMS(*), 
     .        RENUM(*),NSNFIOLD(NSPMD)
C     REAL
      my_real 
     .   X(*), V(3,*), MS(*),TEMP(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KD(50), JD(50), JFI, KFI, MULTIMP,
     .        I, ILD, I_SK_OLD, I_STOK1, 
     .        ADD1, NB_N_B, NOINT,
     .        NCONT, NCONTACT,I_MEM,CAND_N_OLD,
     .        LOC_PROC, KD11_T,I_SK_NEW,NFT,JLT,J,I_STOK,IADFIN,IFORM
      INTEGER, DIMENSION(:), ALLOCATABLE :: OLDINBUF1, OLDINBUF2
C     REAL
      my_real
     .   STARTT, GAP, MAXBOX, MINBOX, STOPT,DIST,TZINF,
     .   XMAXL, YMAXL, ZMAXL, XMINL, YMINL, ZMINL, INACTI,DRAD,DGAPLOAD
      INTEGER :: NMN, NSN,NTY,NRTS,NRTM
C-----------------------------------------------
      I_MEM = 0
      I_MEMG = 0
C
      LOC_PROC=ISPMD+1
      NRTS    =IPARI(3,NIN)
      NRTM    =IPARI(4,NIN)
      NSN     =IPARI(5,NIN)
      NMN     =IPARI(6,NIN)
      NTY     =IPARI(7,NIN)
      NOINT   =IPARI(15,NIN)
      MULTIMP =IPARI(23,NIN)
      NCONT   =IPARI(18,NIN)
      IFORM   =IPARI(30,NIN)
      NCONTACT=MULTIMP*NCONT
C
      IF(IFORM == 2)THEN
        NSNROLD = IPARI(24,NIN)
      ELSE
        NSNROLD = 0
      ENDIF
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
      GAP  = INTBUF_TAB%VARIABLES(2)
      DRAD =INTBUF_TAB%VARIABLES(24)
      DIST = INTBUF_TAB%VARIABLES(5)
      DGAPLOAD =INTBUF_TAB%VARIABLES(46)
      IF (DIST>ZERO) RETURN
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
C pas de modification de INACTI : donc pas de comm
C
      CALL MY_BARRIER
      IF (ITASK == 0) THEN
        IF(IFORM == 2)THEN
          DO I=1,NRTM
            INTBUF_TAB%ADCCM(I) = 0
          ENDDO
          DO I=1,2*NCONTACT
            INTBUF_TAB%CHAIN(I) = 0
          ENDDO
        ENDIF
      ENDIF
C
      CALL MY_BARRIER
      IF (ITASK == 0) THEN
        I_STOK = INTBUF_TAB%I_STOK(1)
        I_SK_NEW = 0
        I_SK_OLD = I_STOK
        INTBUF_TAB%I_STOK(1)=0
C
        IF(IFORM == 2)THEN
          DO NFT=0, I_SK_OLD - 1 , NVSIZ
            JLT = MIN( NVSIZ, I_SK_OLD - NFT )

            CALL I11TRC(
     1        I_SK_NEW         ,INTBUF_TAB%CAND_N, INTBUF_TAB%CAND_E, INTBUF_TAB%FTSAVX, INTBUF_TAB%FTSAVY,
     2        INTBUF_TAB%FTSAVZ,IFORM            , INTBUF_TAB%ADCCM , INTBUF_TAB%CHAIN , NCONTACT,
     .        ITAB,JLT, NFT,INTBUF_TAB%IFPEN,INTBUF_TAB%STFS,NIN,NRTS)

          ENDDO
          INTBUF_TAB%I_STOK(1) = I_SK_NEW
        ELSE
          I_SK_OLD=0
        ENDIF
      ENDIF
C BARRIER BMINMA
      CALL MY_BARRIER
C -------------------------------------------------------------
C     CALCUL BORNE DOMAINE REMONTE DANS I7XSAVE
C -------------------------------------------------------------
C sauvegarde de XSAV (tableau BUFIN(JD(19)))
      CALL I10XSAVE(
     1       X     ,INTBUF_TAB%NSV ,INTBUF_TAB%MSR,NSN   ,NMN   ,
     2       ITASK ,INTBUF_TAB%XSAV,XMINL         ,YMINL ,ZMINL ,
     3       XMAXL ,YMAXL          ,ZMAXL        )
#include "lockon.inc"
      BMINMA(1) = MAX(BMINMA(1),XMAXL)
      BMINMA(2) = MAX(BMINMA(2),YMAXL)
      BMINMA(3) = MAX(BMINMA(3),ZMAXL)
      BMINMA(4) = MIN(BMINMA(4),XMINL)
      BMINMA(5) = MIN(BMINMA(5),YMINL)
      BMINMA(6) = MIN(BMINMA(6),ZMINL)
#include "lockoff.inc"
      RESULT = 0
C BARRIER II_STOK et RESULT
      CALL MY_BARRIER
C -------------------------------------------------------------
C a conserver pour cas inacti est modifie sur p0
      INACTI=IPARI(22,NIN)
      IF(ITASK==0)THEN
        IF(ABS(BMINMA(6)-BMINMA(3))>2*EP30.OR.
     +     ABS(BMINMA(5)-BMINMA(2))>2*EP30.OR.
     +     ABS(BMINMA(4)-BMINMA(1))>2*EP30)THEN
          CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .                I1=NOINT,C1='(I7BUCE)')
          CALL ARRET(2)
        END IF
C
        BMINMA(1)=BMINMA(1)+TZINF
        BMINMA(2)=BMINMA(2)+TZINF
        BMINMA(3)=BMINMA(3)+TZINF
        BMINMA(4)=BMINMA(4)-TZINF
        BMINMA(5)=BMINMA(5)-TZINF
        BMINMA(6)=BMINMA(6)-TZINF

        IF(NSPMD > LRVOXELP)THEN
          CALL ANCMSG(MSGID=36,ANMODE=ANINFO,
     .            C1='(I11MAINTRI)')
          CALL ARRET(2)
        END IF

      END IF

        NRTSR = 0

      IF(NSPMD > 1) THEN

        IF(ITASK==0) CRVOXEL(0:LRVOXEL,0:LRVOXEL,LOC_PROC)=0

        CALL MY_BARRIER

        IF (IMONM > 0 .AND. ITASK == 0) CALL STARTIME(26,1)

        CALL SPMD_TRI11VOX0(
     1      X        ,BMINMA  ,IPARI(21,NIN),NRTM_T  , INTBUF_TAB%STFM(1+ESHIFT),
     2      TZINF    ,INTBUF_TAB%IRECTM(1+2*ESHIFT),GAP,INTBUF_TAB%GAP_M(1+ESHIFT),
     3      INTBUF_TAB%VARIABLES(13) ,INTBUF_TAB%VARIABLES(7),DRAD,DGAPLOAD)
 
        CALL MY_BARRIER
        IF (IMONM > 0 .AND. ITASK == 0) CALL STOPTIME(26,1)
        IF(ITASK==0)THEN
C
C recuperation des noeuds remote NSNR stockes dans XREM
C    
          CALL SPMD_TRI11VOX(
     1 INTBUF_TAB%IRECTS,NRTS     ,X              ,V         ,MS           ,
     2 BMINMA           ,WEIGHT   ,INTBUF_TAB%STFS,NIN       ,ISENDTO      ,
     3 IRCVFROM         ,IAD_ELEM ,FR_ELEM        ,NRTSR     ,IPARI(22,NIN),
     4 INTBUF_TAB%GAP_S ,INTBUF_TAB%PENIS ,  ITAB  ,IPARI(21,NIN),TZINF    ,
     5 NODNX_SMS        ,INTBUF_TAB%GAP_SL,NSNFIOLD,IFORM        ,IPARI(47,NIN),
     6 INTBUF_TAB%IELEC,INTBUF_TAB%AREAS ,TEMP     ,IPARI(36,NIN),INTBUF_TAB%ADDSUBS,
     7 INTBUF_TAB%LISUBS,IPARI(72,NIN),INTBUF_TAB%IPARTFRICS,INTBUF_TAB%INFLG_SUBS)

c 300      continue
C
C renumerotation locale des anciens candidats
C
          IF(IFORM == 2) THEN
            CALL SPMD_RNUMCD11(
     1        INTBUF_TAB%CAND_N,RENUM   ,INTBUF_TAB%I_STOK(1), NIN, NRTS,
     2        NSNFIOLD         ,NSNROLD ,INTBUF_TAB%ADCCM,INTBUF_TAB%CHAIN,
     3        INTBUF_TAB%CAND_E,NCONTACT,NRTM)
              
          ENDIF
        END IF
      END IF
C -------------------------------------------------------------

C -------------------------------------------------------------
      CAND_N_OLD = INTBUF_TAB%I_STOK(1)
 40   CONTINUE

      IF(ITASK==0)THEN
        IF(IFORM == 2) THEN
          IF (ALLOCATED(OLDINBUF1)) DEALLOCATE(OLDINBUF1)
          IF (ALLOCATED(OLDINBUF2)) DEALLOCATE(OLDINBUF2)

          ALLOCATE(OLDINBUF1(NRTM), OLDINBUF2(2*NCONTACT))

          OLDINBUF1(1:NRTM) = 0
          OLDINBUF2(1:2*NCONTACT) = 0

          DO I=1,NRTM
             OLDINBUF1(I) = INTBUF_TAB%ADCCM(I)
          ENDDO
          DO I=1,2*NCONTACT
             OLDINBUF2(I) = INTBUF_TAB%CHAIN(I)
          ENDDO
        ENDIF
      ENDIF
      ILD = 0
      NB_N_B = 1
C
C Barrier comm spmd_tri11box + BMINMA + Retour I7BUCE
C      
 50   CALL MY_BARRIER
      !IF(NRTM_T/=0.OR.ITASK==0)
c      CALL I11BUCE(
c    1   X     ,INTBUF_TAB%IRECTS   ,INTBUF_TAB%IRECTM(1+2*ESHIFT) ,NRTS ,NMN  ,
c    2   NRTM_T,NSN                 ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,GAP  ,
c    3   NOINT ,INTBUF_TAB%I_STOK(1),TZINF         ,MAXBOX      ,MINBOX       ,
c    4   NB_N_B,ESHIFT              ,ILD           ,BMINMA     ,NCONTACT      ,
c    6   INTBUF_TAB%ADCCM(1+ESHIFT) ,INTBUF_TAB%CHAIN,NIN          ,ITAB   ,NRTSR  ,
c    7   NCONT ,INTBUF_TAB%GAP_S    ,INTBUF_TAB%STFS,INTBUF_TAB%PENIS,IPARI(21,NIN),
c    8   INTBUF_TAB%STFM(1+ESHIFT),IPARI(42,NIN),I_MEM  , ITASK       ,IFORM       ,
c    9   INTBUF_TAB%IFPEN         ,DRAD)
       CALL I11BUCE_VOX(
     1   X     ,INTBUF_TAB%IRECTS   ,INTBUF_TAB%IRECTM(1+2*ESHIFT) ,NRTS ,NMN  ,
     2   NRTM_T,NSN                 ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,GAP  ,
     3   NOINT ,INTBUF_TAB%I_STOK(1),TZINF         ,MAXBOX      ,MINBOX       ,
     4   NB_N_B,ESHIFT              ,ILD           ,BMINMA     ,NCONTACT      ,
     6   INTBUF_TAB%ADCCM(1+ESHIFT) ,INTBUF_TAB%CHAIN,NIN          ,ITAB   ,NRTSR  ,
     7   NCONT ,INTBUF_TAB%GAP_S    ,INTBUF_TAB%STFS,INTBUF_TAB%PENIS,IPARI(21,NIN),
     8   INTBUF_TAB%STFM(1+ESHIFT),IPARI(42,NIN),I_MEM  , ITASK       ,IFORM       ,
     9   INTBUF_TAB%IFPEN         ,DRAD, INTBUF_TAB%GAP_M(1+ESHIFT), INTBUF_TAB%GAP_SL,
     1   INTBUF_TAB%GAP_ML(1+ESHIFT),INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(7), GAP,
     2   IPARI(63,NIN),INTBUF_TAB%KREMNODE(1+2*ESHIFT),INTBUF_TAB%REMNODE,DGAPLOAD)

C Upgrade MultiMP
      IF (I_MEM == 2)THEN
#include "lockon.inc"
         I_MEMG = I_MEM
#include "lockoff.inc"
      ENDIF

C New barrier needed for Dynamic MultiMP
      CALL MY_BARRIER

      IF(I_MEMG /=0)THEN
        IF (IFORM == 2)THEN
          IF(ITASK == 0) THEN
            DO I=1,NRTM
              INTBUF_TAB%ADCCM(I)= OLDINBUF1(I)
            ENDDO
            DO I=1,2*NCONTACT
              INTBUF_TAB%CHAIN(I)= OLDINBUF2(I)
            ENDDO
            DEALLOCATE(OLDINBUF1,OLDINBUF2)
          ENDIF
c         /---------------/
          CALL MY_BARRIER
c         /---------------/
        ENDIF
C CARE : JINBUF & JBUFIN array are reallocated in
C        UPGRADE_MULTIMP routine !!!!
!$OMP SINGLE
        !    I                             increase > 4 for small interfaces
        MULTIMP = MAX(IPARI(23,NIN) +4,IPARI(23,NIN)+MIN(20,(250000/NCONT)))
        CALL UPGRADE_MULTIMP(NIN,MULTIMP,INTBUF_TAB)
C        WRITE(6,*) "UPGRADE_MULTIMP",ISPMD,NOINT                              
c       WRITE(6,*) ISPMD,NOINT,"NMN,NSN=",NMN,NSN
c       WRITE(6,*) ISPMD,NOINT,"NRTS,NRTSR=", NRTS,NRTSR
c       WRITE(6,*) ISPMD,NOINT,"NSNROLD=",NSNROLD
c       WRITE(6,*) ISPMD,NOINT,"NCONT=",NCONT
c       WRITE(6,*) ISPMD,NOINT,"I_STOK=",INTBUF_TAB%I_STOK(1)
c       CALL FLUSH(6)
c       IF(MULTIMP > 200) STOP
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
        I_MEM = 0
        I_MEMG = 0
        INTBUF_TAB%I_STOK(1)=CAND_N_OLD
        MULTIMP=IPARI(23,NIN)
        NCONTACT=MULTIMP*NCONT
        GOTO 40
      ENDIF

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
      IF (RESULT/=0) THEN
        CALL MY_BARRIER
        IF (ITASK==0) THEN
          INTBUF_TAB%I_STOK(1) = I_SK_OLD
          RESULT = 0
        ENDIF
        CALL MY_BARRIER
        ILD    = 0         
        MAXBOX = INTBUF_TAB%VARIABLES(9)
        MINBOX = INTBUF_TAB%VARIABLES(12)
        TZINF  = INTBUF_TAB%VARIABLES(8)
        GOTO 50
      ENDIF
      IF(NSPMD>1)THEN
C mono tache
!$OMP SINGLE
        IF (IMONM > 0) CALL STARTIME(26,1)
C
        INTBUF_TAB%VARIABLES(5) = -INTBUF_TAB%VARIABLES(5)
        CALL SPMD_TRI11GAT(
     1    RESULT       ,NRTS ,INTBUF_TAB%CAND_N,INTBUF_TAB%I_STOK(1),NIN,
     2    IPARI(22,NIN),NRTSR,MULTIMP          ,IPARI(21,NIN),IPARI(47,NIN),
     2    IPARI(36,NIN),IPARI(72,NIN))

C sauvegarde des candidats additionnels dans IPARI(24)
        IPARI(24,NIN) = NRTSR
C
        IF (IMONM > 0) CALL STOPTIME(26,1)
!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
      ENDIF

      IF (ALLOCATED(OLDINBUF1)) DEALLOCATE(OLDINBUF1)
      IF (ALLOCATED(OLDINBUF2)) DEALLOCATE(OLDINBUF2)
C
      RETURN
      END
