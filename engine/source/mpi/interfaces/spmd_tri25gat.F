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
Chd|  SPMD_TRI25GAT                 source/mpi/interfaces/spmd_tri25gat.F
Chd|-- called by -----------
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI25GAT(RESULT,NSN ,CAND_N ,I_STOK,NIN,
     2                        IGAP  ,NSNR,MULTIMP,ITY,INTTH ,
     3                        ILEV  ,NSNFIOLD,IPARI,NSNROLD ,
     4                        RENUM ,H3D_DATA,INTFRIC,FLAGREMN,
     5                        LREMNORMAX,NRTM,KREMNOD,REMNOD,
     6                        IVIS2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER RESULT, NIN, NSN, I_STOK, IGAP, NSNR, MULTIMP, ITY,
     .        FLAGREMN,LREMNORMAX,NRTM,
     .        CAND_N(*),INTTH,ILEV, INTFRIC, IVIS2,
     .        NSNFIOLD(*), IPARI(NPARI,NINTER), NSNROLD, RENUM(*),
     .        KREMNOD(*), REMNOD(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER OLDNSNR,NODFI,NNP,LSKYFI,
     .        NOD, LOC_PROC, I, N, NN, P, IDEB, J, K, NI,
     .        IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,IERROR6,IERROR7,
     .        IERROR8,IERROR9,IERROR0,IERROR11,IERROR12,
     .        IERROR13,IERROR14,IERROR15,IERROR16,IERROR17,IERROR18,IERROR19,IERROR20,
     .        IERROR21,INDEX(NSNR),NN2,RSHIFT,ISHIFT, IOLDNSNFI, ND, JDEB, NSNR_OLD, Q,
     .        KK ,SIZREMNORFI, NE, KI, KM, LL

      INTEGER, DIMENSION(:), ALLOCATABLE :: IAUX, IFFI_ADH
      my_real,
     .    DIMENSION(:), ALLOCATABLE :: STIFFI_OLD
      my_real,
     .     DIMENSION(:,:), ALLOCATABLE :: PENEFI_OLD, SECND_FRFI_OLD
      INTEGER, DIMENSION(:), ALLOCATABLE :: REMNOR_FI_TMP
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
C
      NODFI = 0
      LSKYFI= 0

      IF(.NOT.(ALLOCATED(CURRENT_FI_SIZE))) ALLOCATE(CURRENT_FI_SIZE(NINTER))
      IF(.NOT.(ALLOCATED(CURRENT_NODFI))) ALLOCATE(CURRENT_NODFI(NINTER))


      IF(RESULT==0) THEN
C
C Reperage des candidats
C
        DO NI = 1, NSNROLD
C         STIFI a ete mis a jour dans SPMD_GET_STIF !
C         STIFI was not set at time=0 (not read from restart)
          IF((TT==ZERO.OR.STIFI(NIN)%P(NI)>ZERO).AND.IRTLM_FI(NIN)%P(4,NI)==LOC_PROC)THEN
            NODFI = NODFI + 1
            NN = RENUM(NI)
            IREM(1,NN) = -IREM(1,NN)
          END IF
        END DO
C
        DO I = 1, I_STOK
          N = CAND_N(I)
          NN = N-NSN
          IF(NN>0)THEN
            IF(IREM(1,NN)>0)THEN
              NODFI = NODFI + 1
              IREM(1,NN) = -IREM(1,NN)
            ENDIF
          ENDIF
        ENDDO
C
C Allocation des tableaux de frontieres interfaces
C
        IERROR1 = 0
        IERROR2 = 0
        IERROR3 = 0
        IERROR4 = 0
        IERROR5 = 0
        IERROR6 = 0
        IERROR7 = 0
        IERROR8 = 0
        IERROR9 = 0
        IERROR0 = 0
        IERROR11 = 0
        IERROR12 = 0
        IERROR13 = 0
        IERROR14 = 0
        IERROR15 = 0
        IERROR16 = 0
        IERROR17 = 0
        IERROR18 = 0
        IERROR19 = 0
        IERROR20 = 0
        IERROR21 = 0


        ! margin can be implemented here in order to avoid
        ! allocation / deallocation for type 25 in case of sliding
        CURRENT_FI_SIZE(NIN) = NODFI
        CURRENT_NODFI(NIN) = NODFI

        IF(ASSOCIATED(NSVFI(NIN)%P)) DEALLOCATE(NSVFI(NIN)%P)
        ALLOCATE(NSVFI(NIN)%P(NODFI),STAT=IERROR1)
        IF(ASSOCIATED(PMAINFI(NIN)%P)) DEALLOCATE(PMAINFI(NIN)%P)
        ALLOCATE(PMAINFI(NIN)%P(NODFI),STAT=IERROR2)
        IERROR1 = IERROR2 + IERROR1
        IF(ASSOCIATED(XFI(NIN)%P)) DEALLOCATE(XFI(NIN)%P)
        ALLOCATE(XFI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(ASSOCIATED(VFI(NIN)%P)) DEALLOCATE(VFI(NIN)%P)
        ALLOCATE(VFI(NIN)%P(3,NODFI),STAT=IERROR3)
        IF(ASSOCIATED(MSFI(NIN)%P)) DEALLOCATE(MSFI(NIN)%P)
        ALLOCATE(MSFI(NIN)%P(NODFI),STAT=IERROR4)
        IF(ASSOCIATED(STIFI(NIN)%P)) DEALLOCATE(STIFI(NIN)%P)
        ALLOCATE(STIFI(NIN)%P(NODFI),STAT=IERROR5)
        IF(ASSOCIATED(ITAFI(NIN)%P)) DEALLOCATE(ITAFI(NIN)%P)
        ALLOCATE(ITAFI(NIN)%P(NODFI),STAT=IERROR6)
        IF(ITY==7.OR.ITY==22.OR.ITY==23.OR.ITY==24.OR.
     +     ITY==25) THEN
          IF(ASSOCIATED(KINFI(NIN)%P)) DEALLOCATE(KINFI(NIN)%P)
          ALLOCATE(KINFI(NIN)%P(NODFI),STAT=IERROR8)
          IF(INTTH > 0 ) THEN
           IF(ASSOCIATED(TEMPFI(NIN)%P)) DEALLOCATE(TEMPFI(NIN)%P)
           ALLOCATE(TEMPFI(NIN)%P(NODFI),STAT=IERROR9)
           IF(ASSOCIATED(MATSFI(NIN)%P)) DEALLOCATE(MATSFI(NIN)%P)
           ALLOCATE(MATSFI(NIN)%P(NODFI),STAT=IERROR0)
           IF(ASSOCIATED(AREASFI(NIN)%P)) DEALLOCATE(AREASFI(NIN)%P)
           ALLOCATE(AREASFI(NIN)%P(NODFI),STAT=IERROR11)
          ENDIF
          IF(INTTH==0.AND.IVIS2==-1) THEN
           IF(ASSOCIATED(AREASFI(NIN)%P)) DEALLOCATE(AREASFI(NIN)%P)
           ALLOCATE(AREASFI(NIN)%P(NODFI),STAT=IERROR11)
c IF_ADHFI allocation is done later once NSNR_OLD is calculated
          ENDIF
        ENDIF
        IF(IDTMINS == 2) THEN
         IF(ASSOCIATED(NODNXFI(NIN)%P)) DEALLOCATE(NODNXFI(NIN)%P)
         ALLOCATE(NODNXFI(NIN)%P(NODFI),STAT=IERROR12)
         IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
         ALLOCATE(NODAMSFI(NIN)%P(NODFI),STAT=IERROR13)
         IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
         ALLOCATE(PROCAMSFI(NIN)%P(NODFI),STAT=IERROR14)
        ELSEIF(IDTMINS_INT /= 0) THEN
         IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
         ALLOCATE(NODAMSFI(NIN)%P(NODFI),STAT=IERROR13)
         IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
         ALLOCATE(PROCAMSFI(NIN)%P(NODFI),STAT=IERROR14)
        ENDIF
        IF(IGAP/=0) THEN
          IF(ASSOCIATED(GAPFI(NIN)%P)) DEALLOCATE(GAPFI(NIN)%P)
          ALLOCATE(GAPFI(NIN)%P(NODFI),STAT=IERROR7)
          IF(IGAP==3) THEN
            IF(ASSOCIATED(GAP_LFI(NIN)%P)) DEALLOCATE(GAP_LFI(NIN)%P)
            ALLOCATE(GAP_LFI(NIN)%P(NODFI),STAT=IERROR7)
          ENDIF
        ENDIF
C
        NSNR_OLD=IPARI(24,NIN)
        ALLOCATE(SECND_FRFI_OLD(3,NSNR_OLD),PENEFI_OLD(4,NSNR_OLD),
     .           STIFFI_OLD(NSNR_OLD),
     .           STAT=IERROR16)
        SECND_FRFI_OLD(1:3,1:NSNR_OLD)=ZERO
        PENEFI_OLD(1:4,1:NSNR_OLD)   =ZERO
        STIFFI_OLD(1:NSNR_OLD)       =ZERO

C create a temporary array IFFI_ADH to copy old values

        IF(ITY==25.AND.IVIS2==-1.AND.INTTH==0) THEN
          ALLOCATE(IFFI_ADH(NSNR_OLD), STAT=IERROR16)
          IFFI_ADH(1:NSNR_OLD) = 0
        ENDIF
C
        IF(ASSOCIATED(IRTLM_FI(NIN)%P)) DEALLOCATE(IRTLM_FI(NIN)%P)
        ALLOCATE(IRTLM_FI(NIN)%P(4,NODFI),STAT=IERROR15)
C
        IF(ASSOCIATED(TIME_SFI(NIN)%P)) DEALLOCATE(TIME_SFI(NIN)%P)
        ALLOCATE(TIME_SFI(NIN)%P(2*NODFI),STAT=IERROR16)
C
        IF(ASSOCIATED(SECND_FRFI(NIN)%P)) THEN
          SECND_FRFI_OLD(1:3,1:NSNR_OLD)=SECND_FRFI(NIN)%P(1:3,1:NSNR_OLD)
          DEALLOCATE(SECND_FRFI(NIN)%P)
        END IF
        ALLOCATE(SECND_FRFI(NIN)%P(6,NODFI),STAT=IERROR16)
C
        IF(ASSOCIATED(PENE_OLDFI(NIN)%P)) THEN
          PENEFI_OLD(1,1:NSNR_OLD)=PENE_OLDFI(NIN)%P(1,1:NSNR_OLD)
          PENEFI_OLD(2,1:NSNR_OLD)=PENE_OLDFI(NIN)%P(5,1:NSNR_OLD)
          PENEFI_OLD(3,1:NSNR_OLD)=PENE_OLDFI(NIN)%P(3,1:NSNR_OLD)
          PENEFI_OLD(4,1:NSNR_OLD)=PENE_OLDFI(NIN)%P(4,1:NSNR_OLD)
          DEALLOCATE(PENE_OLDFI(NIN)%P)
        END IF
        ALLOCATE(PENE_OLDFI(NIN)%P(5,NODFI),STAT=IERROR16)
C
        IF(ASSOCIATED(STIF_OLDFI(NIN)%P)) THEN
          STIFFI_OLD(1:NSNR_OLD)=STIF_OLDFI(NIN)%P(1,1:NSNR_OLD)
          DEALLOCATE(STIF_OLDFI(NIN)%P)
        END IF
        ALLOCATE(STIF_OLDFI(NIN)%P(2,NODFI),STAT=IERROR16)
C
C copy old values of if_adh
        IF(IVIS2==-1.AND.INTTH==0) THEN
          IF(ASSOCIATED(IF_ADHFI(NIN)%P)) THEN
              IFFI_ADH(1:NSNR_OLD)=IF_ADHFI(NIN)%P(1:NSNR_OLD)
              DEALLOCATE(IF_ADHFI(NIN)%P)
            END IF
          ALLOCATE(IF_ADHFI(NIN)%P(NODFI),STAT=IERROR16)
        ENDIF
C
        IF(ASSOCIATED(ICONT_I_FI(NIN)%P))DEALLOCATE(ICONT_I_FI(NIN)%P)
        ALLOCATE(ICONT_I_FI(NIN)%P(NODFI),STAT=IERROR16)

        IF(ASSOCIATED(ISKEW_FI(NIN)%P))DEALLOCATE(ISKEW_FI(NIN)%P)
        ALLOCATE(ISKEW_FI(NIN)%P(NODFI),STAT=IERROR17)

        IF(ASSOCIATED(ICODT_FI(NIN)%P))DEALLOCATE(ICODT_FI(NIN)%P)
        ALLOCATE(ICODT_FI(NIN)%P(NODFI),STAT=IERROR17)
C
        IF(ASSOCIATED(ISLIDE_FI(NIN)%P))DEALLOCATE(ISLIDE_FI(NIN)%P)
        ALLOCATE(ISLIDE_FI(NIN)%P(4,NODFI),STAT=IERROR17)
C
C Friction model
        IF(INTFRIC > 0 ) THEN
           IF(ASSOCIATED(IPARTFRICSFI(NIN)%P)) DEALLOCATE(IPARTFRICSFI(NIN)%P)
           ALLOCATE(IPARTFRICSFI(NIN)%P(NODFI),STAT=IERROR18)
        ENDIF
C REMOVE NODES FLAGREMNOD
        IF(FLAGREMN == 2 ) THEN
           IF(ASSOCIATED(KREMNOR_FI(NIN)%P)) DEALLOCATE(KREMNOR_FI(NIN)%P)
           ALLOCATE(KREMNOR_FI(NIN)%P(NODFI+1),STAT=IERROR19)
           KREMNOR_FI(NIN)%P(1:NODFI+1) = 0
           IF(ASSOCIATED(REMNOR_FI(NIN)%P)) DEALLOCATE(REMNOR_FI(NIN)%P)
           NULLIFY(REMNOR_FI(NIN)%P)
        ENDIF
C
        IF(IERROR1+IERROR2+IERROR3+IERROR4+IERROR5+
     +     IERROR6+IERROR7+IERROR8 + IERROR9 + IERROR0 +
     +     IERROR11+IERROR12+IERROR13+IERROR14+IERROR15+
     +     IERROR16+IERROR17+IERROR18+IERROR19/= 0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
C
        SECND_FRFI(NIN)%P (1:6,1:NODFI)=ZERO
        PENE_OLDFI(NIN)%P(1:5,1:NODFI)=ZERO
        STIF_OLDFI(NIN)%P(1:2,1:NODFI)=ZERO
C
C reset FI
        IF(IVIS2==-1.AND.INTTH==0) IF_ADHFI(NIN)%P(1:NODFI) = 0
C
        IF(FLAGREMN == 2 ) THEN
           ALLOCATE(REMNOR_FI_TMP(NODFI*LREMNORMAX),STAT=IERROR20)
           IF(IERROR20/= 0) THEN
             CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
             CALL ARRET(2)
           ENDIF
C
        ENDIF
C
C Packing candidates
C
        IDEB = 0
        NN2 = 0
        JDEB = 0

        DO P = 1, NSPMD
          NN = 0
          OLDNSNR = NSNFI(NIN)%P(P)
          IF(OLDNSNR/=0) THEN
            ALLOCATE(IAUX(OLDNSNR),STAT=IERROR17)
            IF(IERROR17/=0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            NNP = NN2
            DO I = 1, OLDNSNR
              IF(IREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                IAUX(NN) = I
              ENDIF
            ENDDO
c general case
#include      "vectorize.inc"
            DO J = 1, NN
          I = IAUX(J)
              INDEX(I+IDEB) = NN2+J
              XFI(NIN)%P(1,NN2+J) = XREM(1,I+IDEB)
              XFI(NIN)%P(2,NN2+J) = XREM(2,I+IDEB)
              XFI(NIN)%P(3,NN2+J) = XREM(3,I+IDEB)
              VFI(NIN)%P(1,NN2+J) = XREM(4,I+IDEB)
              VFI(NIN)%P(2,NN2+J) = XREM(5,I+IDEB)
              VFI(NIN)%P(3,NN2+J) = XREM(6,I+IDEB)
              MSFI(NIN)%P(NN2+J)  = XREM(7,I+IDEB)
              STIFI(NIN)%P(NN2+J) = XREM(8,I+IDEB)
              NSVFI(NIN)%P(NN2+J) = -IREM(1,I+IDEB)
              ITAFI(NIN)%P(NN2+J) = IREM(2,I+IDEB)
              KINFI(NIN)%P(NN2+J) = IREM(3,I+IDEB)
              PMAINFI(NIN)%P(NN2+J) = P

              !ignore specifics IREM and XREM indexes for INT24 sorting
              !IGAPXREMP = IREM(4,I+IDEB)
              !I24XREMP  = IREM(5,I+IDEB)
              !I24IREMP  = IREM(6,I+IDEB)
            ENDDO

c shift for real variables (prepare for next setting)
            RSHIFT = 9
c shift for integer variables (prepare for next setting)
            ISHIFT = 7

            IF(.TRUE.)THEN
#include      "vectorize.inc"
              DO J = 1, NN
                I = IAUX(J)
                ICODT_FI(NIN)%P(NN2+J)  = IREM(ISHIFT+0,I+IDEB)
                ISKEW_FI(NIN)%P(NN2+J)  = IREM(ISHIFT+1,I+IDEB)
              ENDDO
              ISHIFT = ISHIFT + 2
            ENDIF

c IGAP=1 or IGAP=2
            IF(IGAP==1 .OR. IGAP==2)THEN
#include      "vectorize.inc"
              DO J = 1, NN
                I = IAUX(J)
                GAPFI(NIN)%P(NN2+J) = XREM(RSHIFT,I+IDEB)
              ENDDO
              RSHIFT = RSHIFT + 1
c IGAP=3
            ELSEIF(IGAP==3)THEN
#include      "vectorize.inc"
              DO J = 1, NN
                I = IAUX(J)
                GAPFI(NIN)%P(NN2+J)   = XREM(RSHIFT,I+IDEB)
                GAP_LFI(NIN)%P(NN2+J) = XREM(RSHIFT+1,I+IDEB)
              ENDDO
              RSHIFT = RSHIFT + 2
            ENDIF

C thermic
            IF(INTTH>0)THEN
#include      "vectorize.inc"
              DO J = 1, NN
                I = IAUX(J)
                TEMPFI(NIN)%P(NN2+J)  = XREM(RSHIFT,I+IDEB)
                AREASFI(NIN)%P(NN2+J) = XREM(RSHIFT+1,I+IDEB)
                MATSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB)
              ENDDO
              RSHIFT = RSHIFT + 2
              ISHIFT = ISHIFT + 1
            ENDIF
C Interface Adhesion
            IF(IVIS2==-1.AND.INTTH==0)THEN
              JDEB = 0
              DO Q=1,P-1
                JDEB = JDEB + NSNFIOLD(Q)
              END DO
              IF(TT==0) THEN
#include      "vectorize.inc"
                DO J = 1, NN
                  I = IAUX(J)
                  AREASFI(NIN)%P(NN2+J) = XREM(RSHIFT,I+IDEB)
                  IF_ADHFI(NIN)%P(NN2+J) = IREM(ISHIFT,I+IDEB)
                  IOLDNSNFI = IREM(ISHIFT+1,I+IDEB)
                ENDDO
              ELSE
#include      "vectorize.inc"
                DO J = 1, NN
                  I = IAUX(J)
                  AREASFI(NIN)%P(NN2+J) = XREM(RSHIFT,I+IDEB)
                  IOLDNSNFI = IREM(ISHIFT+1,I+IDEB)
                  IF(IOLDNSNFI /= 0)THEN
                    IF_ADHFI(NIN)%P(NN2+J)=IFFI_ADH(IOLDNSNFI+JDEB)
                  ELSE
                    IF_ADHFI(NIN)%P(NN2+J)=0
                  ENDIF
                ENDDO
              ENDIF
              RSHIFT = RSHIFT + 1
              ISHIFT = ISHIFT + 2
            ENDIF

C Friction model
          IF(INTFRIC>0)THEN
#include      "vectorize.inc"
            DO J = 1, NN
              I = IAUX(J)
              IPARTFRICSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB)
            ENDDO
            ISHIFT = ISHIFT + 1
          ENDIF
C -- IDTMINS==2
          IF(IDTMINS==2)THEN
#include      "vectorize.inc"
              DO J = 1, NN
                I = IAUX(J)
                NODNXFI(NIN)%P(NN2+J)   = IREM(ISHIFT,I+IDEB)
                NODAMSFI(NIN)%P(NN2+J)  = IREM(ISHIFT+1,I+IDEB)
                PROCAMSFI(NIN)%P(NN2+J) = P
              ENDDO
              ISHIFT = ISHIFT + 2
C -- IDTMINS_INT /= 0
            ELSEIF(IDTMINS_INT/=0)THEN
#include      "vectorize.inc"
              DO J = 1, NN
                I = IAUX(J)
                NODAMSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB)
                PROCAMSFI(NIN)%P(NN2+J) = P
              ENDDO
              ISHIFT = ISHIFT + 1
            ENDIF

            JDEB = 0
            DO Q=1,P-1
              JDEB = JDEB + NSNFIOLD(Q)
            END DO

            IF(TT==ZERO)THEN
#include    "vectorize.inc"
              DO J = 1, NN
                I = IAUX(J)
                IRTLM_FI(NIN)%P(1,NN2+J) = IREM(ISHIFT,I+IDEB)
                IRTLM_FI(NIN)%P(2,NN2+J) = IREM(ISHIFT+1,I+IDEB)
                IRTLM_FI(NIN)%P(3,NN2+J) = IREM(ISHIFT+2,I+IDEB)
                IRTLM_FI(NIN)%P(4,NN2+J) = IREM(ISHIFT+3,I+IDEB)
                ICONT_I_FI(NIN)%P(NN2+J) = IREM(ISHIFT+4,I+IDEB)

                TIME_SFI(NIN)%P(2*(NN2+J-1)+1) =XREM(RSHIFT,I+IDEB)
                TIME_SFI(NIN)%P(2*(NN2+J-1)+2) =XREM(RSHIFT+1,I+IDEB)
                PENE_OLDFI(NIN)%P(5,NN2+J)     =XREM(RSHIFT+2,I+IDEB)
                IOLDNSNFI = IREM(ISHIFT+5,I+IDEB)

                IF(IOLDNSNFI /= 0)THEN
                  SECND_FRFI(NIN)%P(1,NN2+J) =SECND_FRFI_OLD(1,IOLDNSNFI+JDEB)
                  SECND_FRFI(NIN)%P(2,NN2+J) =SECND_FRFI_OLD(2,IOLDNSNFI+JDEB)
                  SECND_FRFI(NIN)%P(3,NN2+J) =SECND_FRFI_OLD(3,IOLDNSNFI+JDEB)
                  PENE_OLDFI(NIN)%P(1,NN2+J)=PENEFI_OLD(1,IOLDNSNFI+JDEB)
                  STIF_OLDFI(NIN)%P(1,NN2+J)=STIFFI_OLD(IOLDNSNFI+JDEB)
                ELSE
                  SECND_FRFI(NIN)%P(1,NN2+J) =ZERO
                  SECND_FRFI(NIN)%P(2,NN2+J) =ZERO
                  SECND_FRFI(NIN)%P(3,NN2+J) =ZERO
                  PENE_OLDFI(NIN)%P(1,NN2+J)=ZERO
                  STIF_OLDFI(NIN)%P(1,NN2+J)=ZERO
                END IF
                PENE_OLDFI(NIN)%P(2,NN2+J)=ZERO
                STIF_OLDFI(NIN)%P(2,NN2+J)=ZERO
              ENDDO
            ELSE
#include    "vectorize.inc"
              DO J = 1, NN
                I = IAUX(J)
                IRTLM_FI(NIN)%P(1,NN2+J) = IREM(ISHIFT,I+IDEB)
                IRTLM_FI(NIN)%P(2,NN2+J) = IREM(ISHIFT+1,I+IDEB)
                IRTLM_FI(NIN)%P(3,NN2+J) = IREM(ISHIFT+2,I+IDEB)
                IRTLM_FI(NIN)%P(4,NN2+J) = IREM(ISHIFT+3,I+IDEB)
                ICONT_I_FI(NIN)%P(NN2+J) = IREM(ISHIFT+4,I+IDEB)

                TIME_SFI(NIN)%P(2*(NN2+J-1)+1) =XREM(RSHIFT,I+IDEB)
                TIME_SFI(NIN)%P(2*(NN2+J-1)+2) =XREM(RSHIFT+1,I+IDEB)
                IOLDNSNFI = IREM(ISHIFT+5,I+IDEB)

                IF(IOLDNSNFI /= 0)THEN
                  SECND_FRFI(NIN)%P(1,NN2+J) =SECND_FRFI_OLD(1,IOLDNSNFI+JDEB)
                  SECND_FRFI(NIN)%P(2,NN2+J) =SECND_FRFI_OLD(2,IOLDNSNFI+JDEB)
                  SECND_FRFI(NIN)%P(3,NN2+J) =SECND_FRFI_OLD(3,IOLDNSNFI+JDEB)
                  PENE_OLDFI(NIN)%P(1,NN2+J)=PENEFI_OLD(1,IOLDNSNFI+JDEB)
                  STIF_OLDFI(NIN)%P(1,NN2+J)=STIFFI_OLD(IOLDNSNFI+JDEB)
                  PENE_OLDFI(NIN)%P(5,NN2+J)=PENEFI_OLD(2,IOLDNSNFI+JDEB)
                  PENE_OLDFI(NIN)%P(3,NN2+J)=PENEFI_OLD(3,IOLDNSNFI+JDEB)
                  PENE_OLDFI(NIN)%P(4,NN2+J)=PENEFI_OLD(4,IOLDNSNFI+JDEB)
                ELSE
                  SECND_FRFI(NIN)%P(1,NN2+J) =ZERO
                  SECND_FRFI(NIN)%P(2,NN2+J) =ZERO
                  SECND_FRFI(NIN)%P(3,NN2+J) =ZERO
                  PENE_OLDFI(NIN)%P(1,NN2+J)=ZERO
                  STIF_OLDFI(NIN)%P(1,NN2+J)=ZERO
                  PENE_OLDFI(NIN)%P(5,NN2+J)=ZERO
                  PENE_OLDFI(NIN)%P(3,NN2+J)=ZERO
                  PENE_OLDFI(NIN)%P(4,NN2+J)=ZERO
                END IF
                PENE_OLDFI(NIN)%P(2,NN2+J)=ZERO
                STIF_OLDFI(NIN)%P(2,NN2+J)=ZERO
              ENDDO
            END IF
            RSHIFT = RSHIFT + 3
            ISHIFT = ISHIFT + 6
            IF (ILEV==2) ISHIFT = ISHIFT + 1

            NN2 = NN2 + NN
            IDEB = IDEB + OLDNSNR
            NSNFI(NIN)%P(P) = NN2-NNP
            DEALLOCATE(IAUX)

          ENDIF !IF(OLDNSNR/=0)

        ENDDO  ! end do NSPMD
        LSKYFI = NN2*MULTIMAX
        NSNR = NN2
      ENDIF
C-------------------------------------------------------------------------------
C FLAGREMN REMOVE main SEGMENTS : no reception but reconstruction of the tab REMNOR_FI
C-------------------------------------------------------------------------------
      IF(FLAGREMN == 2 ) THEN
         KI = 0
         DO N = 1, NODFI
           DO NE=1,NRTM
              KK = KREMNOD(2*(NE-1)+2) + 1
              LL = KREMNOD(2*(NE-1)+3)
              DO KM=KK,LL
                 IF(REMNOD(KM) == -ITAFI(NIN)%P(N) ) THEN
                    KREMNOR_FI(NIN)%P(N)=KREMNOR_FI(NIN)%P(N)+1
                    KI = KI+1
                    REMNOR_FI_TMP(KI) = NE
                 ENDIF
               ENDDO
            ENDDO
         ENDDO
         DO N=1,NODFI
           KREMNOR_FI(NIN)%P(N+1) = KREMNOR_FI(NIN)%P(N+1) + KREMNOR_FI(NIN)%P(N)
         END DO
         DO N=NODFI,1,-1
           KREMNOR_FI(NIN)%P(N+1)=KREMNOR_FI(NIN)%P(N)
         END DO
         KREMNOR_FI(NIN)%P(1)=0

         SIZREMNORFI = KREMNOR_FI(NIN)%P(NODFI+1)
         ALLOCATE(REMNOR_FI(NIN)%P(SIZREMNORFI),STAT=IERROR21)
         IF(SIZREMNORFI /= 0) THEN
           IF(IERROR21/= 0) THEN
             CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
             CALL ARRET(2)
           ENDIF
#include      "vectorize.inc"
           DO N=1,SIZREMNORFI
             REMNOR_FI(NIN)%P(N) =REMNOR_FI_TMP(N)
           ENDDO
         ENDIF
         DEALLOCATE(REMNOR_FI_TMP)
      ENDIF

      IF(ALLOCATED(XREM)) DEALLOCATE(XREM)
      IF(ALLOCATED(IREM)) DEALLOCATE(IREM)

      IF(ITY==25)THEN
        DEALLOCATE(SECND_FRFI_OLD,PENEFI_OLD,STIFFI_OLD)
        IF(IVIS2==-1) DEALLOCATE(IFFI_ADH)
      END IF

C
      IERROR1=0
      IERROR2=0
      IERROR3=0
      IERROR4=0
      IF(INTTH == 0 ) THEN
C
C Allocation Parith/OFF
C
        IF(IPARIT==0) THEN

          IF(ASSOCIATED(AFI(NIN)%P)) THEN
              DEALLOCATE(AFI(NIN)%P)
              NULLIFY(AFI(NIN)%P)
          ENDIF
          IF(ASSOCIATED(STNFI(NIN)%P)) THEN
              DEALLOCATE(STNFI(NIN)%P)
              NULLIFY(AFI(NIN)%P)
          ENDIF

          IF(NODFI>0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
          IF(NODFI>0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
C Init a 0
          DO I = 1, NODFI*NTHREAD
            AFI(NIN)%P(1,I) = ZERO
            AFI(NIN)%P(2,I) = ZERO
            AFI(NIN)%P(3,I) = ZERO
            STNFI(NIN)%P(I) = ZERO
          ENDDO
C
          IF(KDTINT/=0)THEN
            IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
            IF(NODFI>0)ALLOCATE(VSCFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C Init a 0
            DO I = 1, NODFI*NTHREAD
              VSCFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
          NLSKYFI(NIN) = NODFI
C
        ELSE
C
C Allocation Parith/ON Done in upgrade_rem_slv
C
        ENDIF
      ELSE
C
C Allocation Parith/OFF
C
        IF(IPARIT==0) THEN
          IF(ASSOCIATED(AFI(NIN)%P)) DEALLOCATE(AFI(NIN)%P)
          IF(ASSOCIATED(STNFI(NIN)%P)) DEALLOCATE(STNFI(NIN)%P)
          IF(ASSOCIATED(FTHEFI(NIN)%P)) DEALLOCATE(FTHEFI(NIN)%P)
          IF(NODFI>0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
          IF(NODFI>0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
          IF(NODFI>0)ALLOCATE(FTHEFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C
          IF(NODADT_THERM ==1) THEN
            IF(ASSOCIATED(CONDNFI(NIN)%P)) DEALLOCATE(CONDNFI(NIN)%P)
            IF(NODFI>0.AND.NODADT_THERM ==1)ALLOCATE(CONDNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR4)
          ENDIF
C Init a 0
          DO I = 1, NODFI*NTHREAD
            AFI(NIN)%P(1,I) = ZERO
            AFI(NIN)%P(2,I) = ZERO
            AFI(NIN)%P(3,I) = ZERO
            STNFI(NIN)%P(I) = ZERO
            FTHEFI(NIN)%P(I) = ZERO
          ENDDO
          IF(NODADT_THERM ==1) THEN
            DO I = 1, NODFI
               CONDNFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
C
          IF(KDTINT/=0)THEN
            IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
            IF(NODFI>0)ALLOCATE(VSCFI(NIN)%P(NODFI),STAT=IERROR4)
C Init a 0
            DO I = 1, NODFI
              VSCFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
C
        ELSE
C
C Allocation Parith/ON
C

C Done in upgrade_rem_slv
        ENDIF
      ENDIF
C
      IF(IERROR1+IERROR2+IERROR3+IERROR4/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
C pressure output
C
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT >0)THEN
        IF(ASSOCIATED(FNCONTI(NIN)%P)) DEALLOCATE(FNCONTI(NIN)%P)
        IF(ASSOCIATED(FTCONTI(NIN)%P)) DEALLOCATE(FTCONTI(NIN)%P)
        ALLOCATE(FNCONTI(NIN)%P(3,NODFI),STAT=IERROR1)
        ALLOCATE(FTCONTI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(IERROR1+IERROR2/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ELSE
          DO J = 1, NODFI
            FNCONTI(NIN)%P(1,J)=ZERO
            FNCONTI(NIN)%P(2,J)=ZERO
            FNCONTI(NIN)%P(3,J)=ZERO
            FTCONTI(NIN)%P(1,J)=ZERO
            FTCONTI(NIN)%P(2,J)=ZERO
            FTCONTI(NIN)%P(3,J)=ZERO
          END DO
        END IF
      END IF
C
C Renumbering candidate
C
      DO I = 1, I_STOK
        N = CAND_N(I)
        NN = N-NSN
        IF(NN>0)THEN
          CAND_N(I) = INDEX(NN)+NSN
        ENDIF
      ENDDO
#endif
      RETURN
      END

