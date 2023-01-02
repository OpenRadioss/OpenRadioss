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
Chd|  SPMD_TRI20GAT                 source/mpi/interfaces/spmd_i7crit.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI20GAT(RESULT,NSN ,CAND_N ,I_STOK,NIN  ,
     2                         IGAP  ,NSNR,MULTIMP,ITY   ,INTTH,
     3                         INACTI,H3D_DATA)
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
#ifdef  MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER RESULT, NIN, NSN, I_STOK, IGAP, NSNR, MULTIMP, ITY,
     .        CAND_N(*),INTTH,INACTI
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER OLDNSNR,NODFI,NNP,LSKYFI,
     .        NOD, LOC_PROC, I, N, NN, P, IDEB, J, K,
     .        IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,IERROR6,IERROR7,
     .        IERROR8,IERROR9,IERROR0,IERROR11,IERROR12,IERROR13,
     .        IERROR14,IERROR15,IERROR16,IERROR17,
     .        INDEX(NSNR)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
C Test succes du tri ?
C
      NODFI = 0
      LSKYFI= 0
      IF(RESULT==0) THEN
C
C Reperage des candidats
C
        NODFI = 0
        DO I = 1, I_STOK
          N = CAND_N(I)
          NN = N-NSN
          IF(NN>0)THEN
            IF(XREM(4,NN)>0)THEN
              NODFI = NODFI + 1
              XREM(4,NN) = -XREM(4,NN) 
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
        IF(ASSOCIATED(NSVFI(NIN)%P)) DEALLOCATE(NSVFI(NIN)%P)
        ALLOCATE(NSVFI(NIN)%P(NODFI),STAT=IERROR1)
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
        IF(ASSOCIATED(KINFI(NIN)%P)) DEALLOCATE(KINFI(NIN)%P)
        ALLOCATE(KINFI(NIN)%P(NODFI),STAT=IERROR8)
        IF(ASSOCIATED(NBINFLFI(NIN)%P)) DEALLOCATE(NBINFLFI(NIN)%P)
        ALLOCATE(NBINFLFI(NIN)%P(NODFI),STAT=IERROR14)
        IF(INTTH > 0 ) THEN
          IF(ASSOCIATED(TEMPFI(NIN)%P)) DEALLOCATE(TEMPFI(NIN)%P)
          ALLOCATE(TEMPFI(NIN)%P(NODFI),STAT=IERROR9)
          IF(ASSOCIATED(MATSFI(NIN)%P)) DEALLOCATE(MATSFI(NIN)%P)
          ALLOCATE(MATSFI(NIN)%P(NODFI),STAT=IERROR0)
          IF(ASSOCIATED(AREASFI(NIN)%P)) DEALLOCATE(AREASFI(NIN)%P)
          ALLOCATE(AREASFI(NIN)%P(NODFI),STAT=IERROR11)
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
        ENDIF
        IF(INACTI==5.OR.INACTI==6) THEN
          IF(ASSOCIATED(PENFI(NIN)%P)) DEALLOCATE(PENFI(NIN)%P)
          ALLOCATE(PENFI(NIN)%P(2,NODFI),STAT=IERROR15)
          IF(ASSOCIATED(PENFIA(NIN)%P)) DEALLOCATE(PENFIA(NIN)%P)
          ALLOCATE(PENFIA(NIN)%P(5,NODFI),STAT=IERROR16)
        END IF
        IF(ASSOCIATED(DIAG_SMSFI(NIN)%P)) DEALLOCATE(DIAG_SMSFI(NIN)%P)
        ALLOCATE(DIAG_SMSFI(NIN)%P(NODFI),STAT=IERROR17)

      IF(ITY==20) THEN
        IF(ASSOCIATED(DAANC6FI(NIN)%P)) DEALLOCATE(DAANC6FI(NIN)%P)    
        ALLOCATE(DAANC6FI(NIN)%P(3,6,NODFI),STAT=IERROR1)
        IF(ASSOCIATED(DXANCFI(NIN)%P)) DEALLOCATE(DXANCFI(NIN)%P)    
        ALLOCATE(DXANCFI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(ASSOCIATED(DVANCFI(NIN)%P)) DEALLOCATE(DVANCFI(NIN)%P)    
        ALLOCATE(DVANCFI(NIN)%P(3,NODFI),STAT=IERROR3)
      ENDIF
C
        IF(IERROR1+IERROR2+IERROR3+IERROR4+IERROR5+
     +     IERROR6+IERROR7+IERROR8 + IERROR9 + IERROR0 + 
     +     IERROR11+IERROR12+IERROR13+IERROR14+IERROR15+
     +     IERROR16+IERROR17 /= 0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
C
C Compactage des candidats
C
        IDEB = 0
        NN = 0
        IF(INACTI/=5.AND.INACTI/=6) THEN
         IF(INTTH == 0 ) THEN 
          DO P = 1, NSPMD
           OLDNSNR = NSNFI(NIN)%P(P)
           IF(OLDNSNR/=0) THEN
            NNP = NN
            IF(IDTMINS/=2 .AND. IDTMINS_INT == 0)THEN
             IF(IGAP==0)THEN
              IF(IDTMINS==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 DXANCFI(NIN)%P(1,NN) = XREM(13,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(14,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(15,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(14,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(15,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(16,I+IDEB)
                ENDIF
               ENDDO
C /DT/NODA/AMS
              ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 DIAG_SMSFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(14,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(15,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(16,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(17,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(18,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(19,I+IDEB)
                ENDIF
               ENDDO
C fin /DT/NODA/AMS
              ENDIF
             ELSE
              IF(IDTMINS==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(14,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(15,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(16,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(17,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(18,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(19,I+IDEB)
                ENDIF
               ENDDO
C /DT/NODA/AMS
              ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(14,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(15,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(16,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(17,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(18,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(19,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(20,I+IDEB)
                ENDIF
               ENDDO
C fin /DT/NODA/AMS
              ENDIF
             END IF
            ELSEIF(IDTMINS==2)THEN
C /DT/AMS
             IF(IGAP==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 DIAG_SMSFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 NODNXFI(NIN)%P(NN)   = NINT(XREM(14,I+IDEB))
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(15,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(16,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(17,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(18,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(19,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(20,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(21,I+IDEB)
                ENDIF
               ENDDO
             ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(14,I+IDEB)
                 NODNXFI(NIN)%P(NN)   = NINT(XREM(15,I+IDEB))
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(16,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(17,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(18,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(19,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(20,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(21,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(22,I+IDEB)
                ENDIF
               ENDDO
             ENDIF
C fin /DT/AMS
            ELSE ! IDTMINS_INT/=0
C /DT/INTER/AMS
             IF(IGAP==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 DIAG_SMSFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(14,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(15,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(16,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(17,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(18,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(19,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(20,I+IDEB)
                ENDIF
               ENDDO
             ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(14,I+IDEB)
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(15,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(16,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(17,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(18,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(19,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(20,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(21,I+IDEB)
                ENDIF
               ENDDO
             ENDIF
C fin /DT/INTER/AMS
            END IF
            IDEB = IDEB + OLDNSNR
            NSNFI(NIN)%P(P) = NN-NNP
           ENDIF
          ENDDO
C
         ELSE  ! thermique
          DO P = 1, NSPMD
           OLDNSNR = NSNFI(NIN)%P(P)
           IF(OLDNSNR/=0) THEN
            NNP = NN     
            IF(IDTMINS/=2 .AND. IDTMINS_INT == 0)THEN
             IF(IGAP==0)THEN
              IF(IDTMINS==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 TEMPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(14,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(15,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(16,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(17,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(18,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(19,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(20,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(21,I+IDEB)
                ENDIF
               ENDDO
C /DT/NODA/AMS
              ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 TEMPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(14,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(15,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(16,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(17,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(18,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(19,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(20,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(21,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(22,I+IDEB)
                ENDIF
               ENDDO
C fin /DT/NODA/AMS
              ENDIF
             ELSE
              IF(IDTMINS==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 TEMPFI(NIN)%P(NN) = XREM(14,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(15,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(16,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(17,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(18,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(19,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(20,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(21,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(22,I+IDEB)
                ENDIF
               ENDDO
C /DT/NODA/AMS
              ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 TEMPFI(NIN)%P(NN) = XREM(14,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(15,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(16,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(17,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(18,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(19,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(20,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(21,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(22,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(23,I+IDEB)
                ENDIF
               ENDDO
C fin /DT/NODA/AMS
              ENDIF
             ENDIF
            ELSEIF(IDTMINS==2)THEN
C /DT/AMS
             IF(IGAP==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 TEMPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(14,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(15,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(16,I+IDEB)
                 NODNXFI(NIN)%P(NN)   = NINT(XREM(17,I+IDEB))
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(18,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(19,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(20,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(21,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(22,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(23,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(24,I+IDEB)
                ENDIF
               ENDDO
             ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 TEMPFI(NIN)%P(NN) = XREM(14,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(15,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(16,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(17,I+IDEB)
                 NODNXFI(NIN)%P(NN)   = NINT(XREM(18,I+IDEB))
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(19,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(20,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(21,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(22,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(23,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(24,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(25,I+IDEB)
                ENDIF
               ENDDO
             ENDIF
C fin /DT/AMS
            ELSE ! IDTMINS_INT/=0
C /DT/INTER/AMS
             IF(IGAP==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 TEMPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(14,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(15,I+IDEB)
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(16,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(17,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(18,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(19,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(20,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(21,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(22,I+IDEB)
                ENDIF
               ENDDO
             ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 TEMPFI(NIN)%P(NN) = XREM(14,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(15,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(16,I+IDEB)
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(17,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(18,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(19,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(20,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(21,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(22,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(23,I+IDEB)
                ENDIF
               ENDDO
             ENDIF
C fin /DT/INTER/AMS
            END IF
            IDEB = IDEB + OLDNSNR
            NSNFI(NIN)%P(P) = NN-NNP
           ENDIF
          ENDDO         
         ENDIF
        ELSE ! cas inacti = 5 ou 6
         IF(INTTH == 0 ) THEN 
          DO P = 1, NSPMD
           OLDNSNR = NSNFI(NIN)%P(P)
           IF(OLDNSNR/=0) THEN
            NNP = NN
            IF(IDTMINS/=2 .AND. IDTMINS_INT == 0)THEN
             IF(IGAP==0)THEN
              IF(IDTMINS==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 PENFI(NIN)%P(1,NN) = XREM(13,I+IDEB)
                 PENFI(NIN)%P(2,NN) = XREM(14,I+IDEB)
                 PENFIA(NIN)%P(1,NN)= XREM(15,I+IDEB)
                 PENFIA(NIN)%P(2,NN)= XREM(16,I+IDEB)
                 PENFIA(NIN)%P(3,NN)= XREM(17,I+IDEB)
                 PENFIA(NIN)%P(4,NN)= XREM(18,I+IDEB)
                 PENFIA(NIN)%P(5,NN)= XREM(19,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(20,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(21,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(22,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(23,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(24,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(25,I+IDEB)
                ENDIF
               ENDDO
C /DT/NODA/AMS
              ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 PENFI(NIN)%P(1,NN) = XREM(13,I+IDEB)
                 PENFI(NIN)%P(2,NN) = XREM(14,I+IDEB)
                 PENFIA(NIN)%P(1,NN)= XREM(15,I+IDEB)
                 PENFIA(NIN)%P(2,NN)= XREM(16,I+IDEB)
                 PENFIA(NIN)%P(3,NN)= XREM(17,I+IDEB)
                 PENFIA(NIN)%P(4,NN)= XREM(18,I+IDEB)
                 PENFIA(NIN)%P(5,NN)= XREM(19,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(20,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(21,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(22,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(23,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(24,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(25,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(26,I+IDEB)
                ENDIF
               ENDDO
C fin /DT/NODA/AMS
              ENDIF
             ELSE
              IF(IDTMINS==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN)    = XREM(13,I+IDEB)
                 PENFI(NIN)%P(1,NN)  = XREM(14,I+IDEB)
                 PENFI(NIN)%P(2,NN)  = XREM(15,I+IDEB)
                 PENFIA(NIN)%P(1,NN) = XREM(16,I+IDEB)
                 PENFIA(NIN)%P(2,NN) = XREM(17,I+IDEB)
                 PENFIA(NIN)%P(3,NN) = XREM(18,I+IDEB)
                 PENFIA(NIN)%P(4,NN) = XREM(19,I+IDEB)
                 PENFIA(NIN)%P(5,NN) = XREM(20,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(21,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(22,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(23,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(24,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(25,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(26,I+IDEB)
                ENDIF
               ENDDO
C /DT/NODA/AMS
              ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN)      = XREM(13,I+IDEB)
                 PENFI(NIN)%P(1,NN)    = XREM(14,I+IDEB)
                 PENFI(NIN)%P(2,NN)    = XREM(15,I+IDEB)
                 PENFIA(NIN)%P(1,NN)   = XREM(16,I+IDEB)
                 PENFIA(NIN)%P(2,NN)   = XREM(17,I+IDEB)
                 PENFIA(NIN)%P(3,NN)   = XREM(18,I+IDEB)
                 PENFIA(NIN)%P(4,NN)   = XREM(19,I+IDEB)
                 PENFIA(NIN)%P(5,NN)   = XREM(20,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(21,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(22,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(23,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(24,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(25,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(26,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(27,I+IDEB)
                ENDIF
               ENDDO
C fin /DT/NODA/AMS
              ENDIF
             ENDIF
            ELSEIF(IDTMINS==2)THEN
C /DT/AMS
             IF(IGAP==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN)= NINT(XREM(12,I+IDEB))
                 PENFI(NIN)%P(1,NN)   = XREM(13,I+IDEB)
                 PENFI(NIN)%P(2,NN)   = XREM(14,I+IDEB)
                 PENFIA(NIN)%P(1,NN)  = XREM(15,I+IDEB)
                 PENFIA(NIN)%P(2,NN)  = XREM(16,I+IDEB)
                 PENFIA(NIN)%P(3,NN)  = XREM(17,I+IDEB)
                 PENFIA(NIN)%P(4,NN)  = XREM(18,I+IDEB)
                 PENFIA(NIN)%P(5,NN)  = XREM(19,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(20,I+IDEB)
                 NODNXFI(NIN)%P(NN)   = NINT(XREM(21,I+IDEB))
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(22,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(23,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(24,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(25,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(26,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(27,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(28,I+IDEB)
                ENDIF
               ENDDO
             ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN)= NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN)   = XREM(13,I+IDEB)
                 PENFI(NIN)%P(1,NN) = XREM(14,I+IDEB)
                 PENFI(NIN)%P(2,NN) = XREM(15,I+IDEB)
                 PENFIA(NIN)%P(1,NN)= XREM(16,I+IDEB)
                 PENFIA(NIN)%P(2,NN)= XREM(17,I+IDEB)
                 PENFIA(NIN)%P(3,NN)= XREM(18,I+IDEB)
                 PENFIA(NIN)%P(4,NN)= XREM(19,I+IDEB)
                 PENFIA(NIN)%P(5,NN)= XREM(20,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(21,I+IDEB)
                 NODNXFI(NIN)%P(NN)   = NINT(XREM(22,I+IDEB))
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(23,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(24,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(25,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(26,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(27,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(28,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(29,I+IDEB)
                ENDIF
               ENDDO
             ENDIF
C fin /DT/AMS
            ELSE ! IDTMINS_INT/=0
C /DT/INTER/AMS
             IF(IGAP==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN)= NINT(XREM(12,I+IDEB))
                 PENFI(NIN)%P(1,NN)   = XREM(13,I+IDEB)
                 PENFI(NIN)%P(2,NN)   = XREM(14,I+IDEB)
                 PENFIA(NIN)%P(1,NN)  = XREM(15,I+IDEB)
                 PENFIA(NIN)%P(2,NN)  = XREM(16,I+IDEB)
                 PENFIA(NIN)%P(3,NN)  = XREM(17,I+IDEB)
                 PENFIA(NIN)%P(4,NN)  = XREM(18,I+IDEB)
                 PENFIA(NIN)%P(5,NN)  = XREM(19,I+IDEB)
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(20,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(21,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(22,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(23,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(24,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(25,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(26,I+IDEB)
                ENDIF
               ENDDO
             ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN)= NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN)   = XREM(13,I+IDEB)
                 PENFI(NIN)%P(1,NN) = XREM(14,I+IDEB)
                 PENFI(NIN)%P(2,NN) = XREM(15,I+IDEB)
                 PENFIA(NIN)%P(1,NN)= XREM(16,I+IDEB)
                 PENFIA(NIN)%P(2,NN)= XREM(17,I+IDEB)
                 PENFIA(NIN)%P(3,NN)= XREM(18,I+IDEB)
                 PENFIA(NIN)%P(4,NN)= XREM(19,I+IDEB)
                 PENFIA(NIN)%P(5,NN)= XREM(20,I+IDEB)
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(21,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(22,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(23,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(24,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(25,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(26,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(27,I+IDEB)
                ENDIF
               ENDDO
             ENDIF
C fin /DT/INTER/AMS
            END IF
            IDEB = IDEB + OLDNSNR
            NSNFI(NIN)%P(P) = NN-NNP
           ENDIF
          ENDDO
C
         ELSE  ! thermique
          DO P = 1, NSPMD
           OLDNSNR = NSNFI(NIN)%P(P)
           IF(OLDNSNR/=0) THEN
            NNP = NN     
            IF(IDTMINS/=2 .AND. IDTMINS_INT == 0)THEN
             IF(IGAP==0)THEN
              IF(IDTMINS==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN)= NINT(XREM(12,I+IDEB))
                 TEMPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(14,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(15,I+IDEB)
                 PENFI(NIN)%P(1,NN) = XREM(16,I+IDEB)
                 PENFI(NIN)%P(2,NN) = XREM(17,I+IDEB)
                 PENFIA(NIN)%P(1,NN)= XREM(18,I+IDEB)
                 PENFIA(NIN)%P(2,NN)= XREM(19,I+IDEB)
                 PENFIA(NIN)%P(3,NN)= XREM(20,I+IDEB)
                 PENFIA(NIN)%P(4,NN)= XREM(21,I+IDEB)
                 PENFIA(NIN)%P(5,NN)= XREM(22,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(23,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(24,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(25,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(26,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(27,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(28,I+IDEB)
                ENDIF
               ENDDO
C /DT/NODA/AMS
              ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN)= NINT(XREM(12,I+IDEB))
                 TEMPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(14,I+IDEB))
                 AREASFI(NIN)%P(NN)    = XREM(15,I+IDEB)
                 PENFI(NIN)%P(1,NN)    = XREM(16,I+IDEB)
                 PENFI(NIN)%P(2,NN)    = XREM(17,I+IDEB)
                 PENFIA(NIN)%P(1,NN)   = XREM(18,I+IDEB)
                 PENFIA(NIN)%P(2,NN)   = XREM(19,I+IDEB)
                 PENFIA(NIN)%P(3,NN)   = XREM(20,I+IDEB)
                 PENFIA(NIN)%P(4,NN)   = XREM(21,I+IDEB)
                 PENFIA(NIN)%P(5,NN)   = XREM(22,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(23,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(24,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(25,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(26,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(27,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(28,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(29,I+IDEB)
                ENDIF
               ENDDO
C fin /DT/NODA/AMS
              ENDIF
             ELSE
              IF(IDTMINS==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN)= NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 TEMPFI(NIN)%P(NN) = XREM(14,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(15,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(16,I+IDEB)
                 PENFI(NIN)%P(1,NN) = XREM(17,I+IDEB)
                 PENFI(NIN)%P(2,NN) = XREM(18,I+IDEB)
                 PENFIA(NIN)%P(1,NN)= XREM(19,I+IDEB)
                 PENFIA(NIN)%P(2,NN)= XREM(20,I+IDEB)
                 PENFIA(NIN)%P(3,NN)= XREM(21,I+IDEB)
                 PENFIA(NIN)%P(4,NN)= XREM(22,I+IDEB)
                 PENFIA(NIN)%P(5,NN)= XREM(23,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(24,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(25,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(26,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(27,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(28,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(29,I+IDEB)
                ENDIF
               ENDDO
C /DT/NODA/AMS
              ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN)= NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 TEMPFI(NIN)%P(NN) = XREM(14,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(15,I+IDEB))
                 AREASFI(NIN)%P(NN)    = XREM(16,I+IDEB)
                 PENFI(NIN)%P(1,NN)    = XREM(17,I+IDEB)
                 PENFI(NIN)%P(2,NN)    = XREM(18,I+IDEB)
                 PENFIA(NIN)%P(1,NN)   = XREM(19,I+IDEB)
                 PENFIA(NIN)%P(2,NN)   = XREM(20,I+IDEB)
                 PENFIA(NIN)%P(3,NN)   = XREM(21,I+IDEB)
                 PENFIA(NIN)%P(4,NN)   = XREM(22,I+IDEB)
                 PENFIA(NIN)%P(5,NN)   = XREM(23,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(24,I+IDEB)
                 DXANCFI(NIN)%P(1,NN) = XREM(25,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(26,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(27,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(28,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(29,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(30,I+IDEB)
                ENDIF
               ENDDO
C fin /DT/NODA/AMS
              ENDIF
             ENDIF
            ELSEIF(IDTMINS==2)THEN
C /DT/AMS
             IF(IGAP==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 TEMPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(14,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(15,I+IDEB)
                 PENFI(NIN)%P(1,NN) = XREM(16,I+IDEB)
                 PENFI(NIN)%P(2,NN) = XREM(17,I+IDEB)
                 PENFIA(NIN)%P(1,NN)= XREM(18,I+IDEB)
                 PENFIA(NIN)%P(2,NN)= XREM(19,I+IDEB)
                 PENFIA(NIN)%P(3,NN)= XREM(20,I+IDEB)
                 PENFIA(NIN)%P(4,NN)= XREM(21,I+IDEB)
                 PENFIA(NIN)%P(5,NN)= XREM(22,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(23,I+IDEB)
                 NODNXFI(NIN)%P(NN)   = NINT(XREM(24,I+IDEB))
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(25,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(26,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(27,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(28,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(29,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(30,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(31,I+IDEB)
                ENDIF
               ENDDO
             ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 TEMPFI(NIN)%P(NN) = XREM(14,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(15,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(16,I+IDEB)
                 PENFI(NIN)%P(1,NN) = XREM(17,I+IDEB)
                 PENFI(NIN)%P(2,NN) = XREM(18,I+IDEB)
                 PENFIA(NIN)%P(1,NN)= XREM(19,I+IDEB)
                 PENFIA(NIN)%P(2,NN)= XREM(20,I+IDEB)
                 PENFIA(NIN)%P(3,NN)= XREM(21,I+IDEB)
                 PENFIA(NIN)%P(4,NN)= XREM(22,I+IDEB)
                 PENFIA(NIN)%P(5,NN)= XREM(23,I+IDEB)
                 DIAG_SMSFI(NIN)%P(NN) = XREM(24,I+IDEB)
                 NODNXFI(NIN)%P(NN)   = NINT(XREM(25,I+IDEB))
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(26,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(27,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(28,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(29,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(30,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(31,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(32,I+IDEB)
                ENDIF
               ENDDO
             ENDIF
C fin /DT/AMS
            ELSE ! IDTMINS_INT/=0
C /DT/INTER/AMS
             IF(IGAP==0)THEN
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 TEMPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(14,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(15,I+IDEB)
                 PENFI(NIN)%P(1,NN) = XREM(16,I+IDEB)
                 PENFI(NIN)%P(2,NN) = XREM(17,I+IDEB)
                 PENFIA(NIN)%P(1,NN)= XREM(18,I+IDEB)
                 PENFIA(NIN)%P(2,NN)= XREM(19,I+IDEB)
                 PENFIA(NIN)%P(3,NN)= XREM(20,I+IDEB)
                 PENFIA(NIN)%P(4,NN)= XREM(21,I+IDEB)
                 PENFIA(NIN)%P(5,NN)= XREM(22,I+IDEB)
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(23,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(24,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(25,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(26,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(27,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(28,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(29,I+IDEB)
                ENDIF
               ENDDO
             ELSE
               DO I = 1, OLDNSNR
                IF(XREM(4,I+IDEB)<0) THEN
                 NN = NN + 1
                 INDEX(I+IDEB) = NN
                 NSVFI(NIN)%P(NN) = NINT(-XREM(4,I+IDEB))
                 XFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                 XFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                 XFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                 VFI(NIN)%P(1,NN) = XREM(5,I+IDEB)
                 VFI(NIN)%P(2,NN) = XREM(6,I+IDEB)
                 VFI(NIN)%P(3,NN) = XREM(7,I+IDEB)
                 MSFI(NIN)%P(NN)  = XREM(8,I+IDEB)
                 STIFI(NIN)%P(NN) = XREM(9,I+IDEB)
                 IF(IR4R8 == 2) THEN
                   ITAFI(NIN)%P(NN) = NINT(XREM(10,I+IDEB))
                 ELSE
                   ITAFI(NIN)%P(NN) = IREM(1,I+IDEB)
                 END IF
                 KINFI(NIN)%P(NN) = NINT(XREM(11,I+IDEB))
                 NBINFLFI(NIN)%P(NN) = NINT(XREM(12,I+IDEB))
                 GAPFI(NIN)%P(NN) = XREM(13,I+IDEB)
                 TEMPFI(NIN)%P(NN) = XREM(14,I+IDEB)
                 MATSFI(NIN)%P(NN) = NINT(XREM(15,I+IDEB))
                 AREASFI(NIN)%P(NN) = XREM(16,I+IDEB)
                 PENFI(NIN)%P(1,NN) = XREM(17,I+IDEB)
                 PENFI(NIN)%P(2,NN) = XREM(18,I+IDEB)
                 PENFIA(NIN)%P(1,NN)= XREM(19,I+IDEB)
                 PENFIA(NIN)%P(2,NN)= XREM(20,I+IDEB)
                 PENFIA(NIN)%P(3,NN)= XREM(21,I+IDEB)
                 PENFIA(NIN)%P(4,NN)= XREM(22,I+IDEB)
                 PENFIA(NIN)%P(5,NN)= XREM(23,I+IDEB)
                 NODAMSFI(NIN)%P(NN)  = NINT(XREM(24,I+IDEB))
                 PROCAMSFI(NIN)%P(NN) = P
                 DXANCFI(NIN)%P(1,NN) = XREM(25,I+IDEB)
                 DXANCFI(NIN)%P(2,NN) = XREM(26,I+IDEB)
                 DXANCFI(NIN)%P(3,NN) = XREM(27,I+IDEB)
                 DVANCFI(NIN)%P(1,NN) = XREM(28,I+IDEB)
                 DVANCFI(NIN)%P(2,NN) = XREM(29,I+IDEB)
                 DVANCFI(NIN)%P(3,NN) = XREM(30,I+IDEB)
                ENDIF
               ENDDO
             ENDIF
C fin /DT/INTER/AMS
            END IF
            IDEB = IDEB + OLDNSNR
            NSNFI(NIN)%P(P) = NN-NNP
           ENDIF
          ENDDO         
         ENDIF
        END IF
        LSKYFI = NN*MULTIMAX
Cel nsnr nouveau utile pour inacti
        NSNR = NN
      ENDIF
C
C Deallocation de XREM
C
      IF(ALLOCATED(XREM)) DEALLOCATE(XREM)
      IF(IR4R8==1)THEN
        IF(ALLOCATED(IREM)) DEALLOCATE(IREM)
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
        IF(ASSOCIATED(AFI(NIN)%P)) DEALLOCATE(AFI(NIN)%P)
        IF(ASSOCIATED(STNFI(NIN)%P)) DEALLOCATE(STNFI(NIN)%P)
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
C
       ELSE
C
C Allocation Parith/ON
C
        IF(ASSOCIATED(FSKYFI(NIN)%P)) DEALLOCATE(FSKYFI(NIN)%P)
        IF(ASSOCIATED(ISKYFI(NIN)%P)) DEALLOCATE(ISKYFI(NIN)%P)
        NLSKYFI(NIN) = LSKYFI
        IF(LSKYFI>0) THEN
          ALLOCATE(ISKYFI(NIN)%P(LSKYFI),STAT=IERROR1)
          IF(KDTINT==0) THEN
            ALLOCATE(FSKYFI(NIN)%P(4,LSKYFI),STAT=IERROR2)
          ELSE
            ALLOCATE(FSKYFI(NIN)%P(5,LSKYFI),STAT=IERROR2)
          ENDIF
        ENDIF
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
        
C Init a 0
        DO I = 1, NODFI*NTHREAD
          AFI(NIN)%P(1,I) = ZERO
          AFI(NIN)%P(2,I) = ZERO
          AFI(NIN)%P(3,I) = ZERO
          STNFI(NIN)%P(I) = ZERO
          FTHEFI(NIN)%P(I) = ZERO
        ENDDO
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
        IF(ASSOCIATED(FSKYFI(NIN)%P)) DEALLOCATE(FSKYFI(NIN)%P)
        IF(ASSOCIATED(ISKYFI(NIN)%P)) DEALLOCATE(ISKYFI(NIN)%P)
        IF(ASSOCIATED(FTHESKYFI(NIN)%P)) DEALLOCATE(FTHESKYFI(NIN)%P)
        NLSKYFI(NIN) = LSKYFI
        IF(LSKYFI>0) THEN
          ALLOCATE(ISKYFI(NIN)%P(LSKYFI),STAT=IERROR1)
          IF(KDTINT==0) THEN
            ALLOCATE(FSKYFI(NIN)%P(4,LSKYFI),STAT=IERROR2)
            ALLOCATE(FTHESKYFI(NIN)%P(LSKYFI),STAT=IERROR3)
          ELSE
            ALLOCATE(FSKYFI(NIN)%P(5,LSKYFI),STAT=IERROR2)
            ALLOCATE(FTHESKYFI(NIN)%P(LSKYFI),STAT=IERROR3)
          ENDIF
        ENDIF
       ENDIF        
      ENDIF    
C
      IF(IERROR1+IERROR2+IERROR3+IERROR4/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
C allocations conditionnelles output pression
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
C allocations additionnelles pour interface type20
C
      IF(ITY==20) THEN
        IF(ASSOCIATED(ALPHAKFI(NIN)%P)) DEALLOCATE(ALPHAKFI(NIN)%P)    
        ALLOCATE(ALPHAKFI(NIN)%P(NODFI),STAT=IERROR4)
        IF(ASSOCIATED(DAANCFI(NIN)%P)) DEALLOCATE(DAANCFI(NIN)%P)    
        ALLOCATE(DAANCFI(NIN)%P(3,NODFI),STAT=IERROR4)

        IF(IERROR1+IERROR2+IERROR3+IERROR4/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
        DO K = 1, NODFI
          DO I = 1, 3
            DO J = 1, 6
              DAANC6FI(NIN)%P(I,J,K) = ZERO
            END DO
          END DO
          ALPHAKFI(NIN)%P(K) = ONE
        END DO
      ENDIF
C
      DAANCFI(NIN)%P(1:3,1:NODFI)=ZERO
C
C Renumerotation des candidats
C
      DO I = 1, I_STOK
        N = CAND_N(I)
        NN = N-NSN
        IF(NN>0)THEN
          CAND_N(I) = INDEX(NN)+NSN
        ENDIF
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_TRI20GATE                source/mpi/interfaces/spmd_i7crit.F
Chd|-- called by -----------
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI20GATE(RESULT,NRTS ,CAND_S ,I_STOK,NIN,
     2                          INACTI,NRTSR,MULTIMP,IGAP  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER RESULT, NIN, NRTS, I_STOK, INACTI, NRTSR, MULTIMP, IGAP,
     .        CAND_S(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER OLDNRTSR,SEGFI,NODFI,NNP,LSKYFI,
     .        NOD, LOC_PROC, I, J, K, N, NN, P, IDEB, N1, N2,
     .        IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,IERROR6,IERROR7,
     .        IERROR8,IERROR9,IERROR0,IERROR11,IERROR12,IERROR13,
     .        INDEX(NRTSR)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      IERROR1=0
      IERROR2=0
      IERROR3=0
      IERROR4=0
      IERROR5=0
      IERROR6=0
      IERROR7=0
      IERROR8=0
      IERROR9=0
      IERROR0=0
      IERROR11=0
      IERROR12=0
      IERROR13=0
C
C Test succes du tri ?
C
      SEGFI = 0
      LSKYFI= 0
      IF(RESULT==0) THEN
C
C Reperage des candidats
C
        SEGFI = 0
        DO I = 1, I_STOK
          N = CAND_S(I)
          NN = N-NRTS
          IF(NN>0)THEN
            IF(XREM(1,NN)>0)THEN
              SEGFI = SEGFI + 1
              XREM(1,NN) = -XREM(1,NN)
            ENDIF
          ENDIF
        ENDDO
C non optimal car noeuds dupliques potentiellement
        NODFI = 2*SEGFI
C
C Allocation des tableaux de frontieres interfaces
C
        IF(ASSOCIATED(NSVFIE(NIN)%P)) DEALLOCATE(NSVFIE(NIN)%P)
        ALLOCATE(NSVFIE(NIN)%P(SEGFI),STAT=IERROR1)
        IF(ASSOCIATED(XFIE(NIN)%P)) DEALLOCATE(XFIE(NIN)%P)
        ALLOCATE(XFIE(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(ASSOCIATED(VFIE(NIN)%P)) DEALLOCATE(VFIE(NIN)%P)
        ALLOCATE(VFIE(NIN)%P(3,NODFI),STAT=IERROR3)
        IF(ASSOCIATED(MSFIE(NIN)%P)) DEALLOCATE(MSFIE(NIN)%P)
        ALLOCATE(MSFIE(NIN)%P(NODFI),STAT=IERROR4)

        IF(ASSOCIATED(STIFIE(NIN)%P)) DEALLOCATE(STIFIE(NIN)%P)
        ALLOCATE(STIFIE(NIN)%P(SEGFI),STAT=IERROR5)
        IF(ASSOCIATED(ITAFIE(NIN)%P)) DEALLOCATE(ITAFIE(NIN)%P)
        ALLOCATE(ITAFIE(NIN)%P(NODFI),STAT=IERROR6)
        IF(IGAP/=0) THEN
          IF(ASSOCIATED(GAPFIE(NIN)%P)) DEALLOCATE(GAPFIE(NIN)%P)
          ALLOCATE(GAPFIE(NIN)%P(SEGFI),STAT=IERROR7)
        ELSE
          IERROR7 = 0
        END IF

        IF(INACTI==5.OR.INACTI==6) THEN
          IF(ASSOCIATED(PENFIE(NIN)%P)) DEALLOCATE(PENFIE(NIN)%P)
          ALLOCATE(PENFIE(NIN)%P(2,SEGFI),STAT=IERROR8)
          IF(ASSOCIATED(PENFIAE(NIN)%P)) DEALLOCATE(PENFIAE(NIN)%P)
          ALLOCATE(PENFIAE(NIN)%P(5,NODFI),STAT=IERROR9)
        ELSE
          IERROR8 = 0
          IERROR9 = 0
        END IF
C
        IF(ASSOCIATED(DIAG_SMSFIE(NIN)%P))DEALLOCATE(DIAG_SMSFIE(NIN)%P)
        ALLOCATE(DIAG_SMSFIE(NIN)%P(NODFI),STAT=IERROR0)
C
        IF(IDTMINS == 2) THEN
         IF(ASSOCIATED(NODNXFIE(NIN)%P)) DEALLOCATE(NODNXFIE(NIN)%P)
         ALLOCATE(NODNXFIE(NIN)%P(NODFI),STAT=IERROR11)
         IF(ASSOCIATED(NODAMSFIE(NIN)%P)) DEALLOCATE(NODAMSFIE(NIN)%P)
         ALLOCATE(NODAMSFIE(NIN)%P(NODFI),STAT=IERROR12)
         IF(ASSOCIATED(PROCAMSFIE(NIN)%P)) DEALLOCATE(PROCAMSFIE(NIN)%P)
         ALLOCATE(PROCAMSFIE(NIN)%P(NODFI),STAT=IERROR13)
        ELSEIF(IDTMINS_INT /= 0) THEN
         IF(ASSOCIATED(NODAMSFIE(NIN)%P)) DEALLOCATE(NODAMSFIE(NIN)%P)
         ALLOCATE(NODAMSFIE(NIN)%P(NODFI),STAT=IERROR12)
         IF(ASSOCIATED(PROCAMSFIE(NIN)%P)) DEALLOCATE(PROCAMSFIE(NIN)%P)
         ALLOCATE(PROCAMSFIE(NIN)%P(NODFI),STAT=IERROR13)
        ELSE
          IERROR11 = 0
          IERROR12 = 0
          IERROR13 = 0
        ENDIF 
C
        IF(IERROR1+IERROR2+IERROR3+IERROR4+IERROR5+
     +     IERROR6+IERROR7+IERROR8+IERROR9+IERROR0+
     +     IERROR11+IERROR12+IERROR13/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
C
C Compactage des candidats
C
C
        IDEB = 0
        NN = 0
        DO P = 1, NSPMD
         OLDNRTSR = NSNFIE(NIN)%P(P)
         IF(OLDNRTSR/=0) THEN
          NNP = NN
          IF(IDTMINS/=2 .AND. IDTMINS_INT == 0)THEN
           IF(INACTI/=5.AND.INACTI/=6) THEN
            IF(IGAP/=0) THEN
             IF(IDTMINS==0)THEN
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                GAPFIE(NIN)%P(NN) = XREM(19,I+IDEB)
               END IF
              END DO
C /DT/NODA/AMS
             ELSE
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                GAPFIE(NIN)%P(NN) = XREM(19,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N1) = XREM(20,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N2) = XREM(21,I+IDEB)
               END IF
              END DO
C fin /DT/NODA/AMS
             ENDIF
            ELSE
             IF(IDTMINS==0)THEN
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
               END IF
              END DO
C /DT/NODA/AMS
             ELSE
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N1) = XREM(19,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N2) = XREM(20,I+IDEB)
               END IF
              END DO
C fin /DT/NODA/AMS
             ENDIF
            END IF
           ELSE
            IF(IGAP/=0) THEN
             IF(IDTMINS==0)THEN
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                GAPFIE(NIN)%P(NN)  = XREM(19,I+IDEB)
                PENFIE(NIN)%P(1,NN) = XREM(20,I+IDEB)
                PENFIE(NIN)%P(2,NN) = XREM(21,I+IDEB)
                PENFIAE(NIN)%P(1,N1)= XREM(22,I+IDEB)
                PENFIAE(NIN)%P(2,N1)= XREM(23,I+IDEB)
                PENFIAE(NIN)%P(3,N1)= XREM(24,I+IDEB)
                PENFIAE(NIN)%P(4,N1)= XREM(25,I+IDEB)
                PENFIAE(NIN)%P(5,N1)= XREM(26,I+IDEB)
                PENFIAE(NIN)%P(1,N2)= XREM(27,I+IDEB)
                PENFIAE(NIN)%P(2,N2)= XREM(28,I+IDEB)
                PENFIAE(NIN)%P(3,N2)= XREM(29,I+IDEB)
                PENFIAE(NIN)%P(4,N2)= XREM(30,I+IDEB)
                PENFIAE(NIN)%P(5,N2)= XREM(31,I+IDEB)
               ENDIF
              ENDDO
C /DT/NODA/AMS
             ELSE
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                GAPFIE(NIN)%P(NN)  = XREM(19,I+IDEB)
                PENFIE(NIN)%P(1,NN) = XREM(20,I+IDEB)
                PENFIE(NIN)%P(2,NN) = XREM(21,I+IDEB)
                PENFIAE(NIN)%P(1,N1)= XREM(22,I+IDEB)
                PENFIAE(NIN)%P(2,N1)= XREM(23,I+IDEB)
                PENFIAE(NIN)%P(3,N1)= XREM(24,I+IDEB)
                PENFIAE(NIN)%P(4,N1)= XREM(25,I+IDEB)
                PENFIAE(NIN)%P(5,N1)= XREM(26,I+IDEB)
                PENFIAE(NIN)%P(1,N2)= XREM(27,I+IDEB)
                PENFIAE(NIN)%P(2,N2)= XREM(28,I+IDEB)
                PENFIAE(NIN)%P(3,N2)= XREM(29,I+IDEB)
                PENFIAE(NIN)%P(4,N2)= XREM(30,I+IDEB)
                PENFIAE(NIN)%P(5,N2)= XREM(31,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N1) = XREM(32,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N2) = XREM(33,I+IDEB)
               ENDIF
              ENDDO
C fin /DT/NODA/AMS
             ENDIF
            ELSE
             IF(IDTMINS==0)THEN
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                PENFIE(NIN)%P(1,NN) = XREM(19,I+IDEB)
                PENFIE(NIN)%P(2,NN) = XREM(20,I+IDEB)
                PENFIAE(NIN)%P(1,N1)= XREM(21,I+IDEB)
                PENFIAE(NIN)%P(2,N1)= XREM(22,I+IDEB)
                PENFIAE(NIN)%P(3,N1)= XREM(23,I+IDEB)
                PENFIAE(NIN)%P(4,N1)= XREM(24,I+IDEB)
                PENFIAE(NIN)%P(5,N1)= XREM(25,I+IDEB)
                PENFIAE(NIN)%P(1,N2)= XREM(26,I+IDEB)
                PENFIAE(NIN)%P(2,N2)= XREM(27,I+IDEB)
                PENFIAE(NIN)%P(3,N2)= XREM(28,I+IDEB)
                PENFIAE(NIN)%P(4,N2)= XREM(29,I+IDEB)
                PENFIAE(NIN)%P(5,N2)= XREM(30,I+IDEB)
               ENDIF
              ENDDO
C /DT/NODA/AMS
             ELSE
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                PENFIE(NIN)%P(1,NN) = XREM(19,I+IDEB)
                PENFIE(NIN)%P(2,NN) = XREM(20,I+IDEB)
                PENFIAE(NIN)%P(1,N1)= XREM(21,I+IDEB)
                PENFIAE(NIN)%P(2,N1)= XREM(22,I+IDEB)
                PENFIAE(NIN)%P(3,N1)= XREM(23,I+IDEB)
                PENFIAE(NIN)%P(4,N1)= XREM(24,I+IDEB)
                PENFIAE(NIN)%P(5,N1)= XREM(25,I+IDEB)
                PENFIAE(NIN)%P(1,N2)= XREM(26,I+IDEB)
                PENFIAE(NIN)%P(2,N2)= XREM(27,I+IDEB)
                PENFIAE(NIN)%P(3,N2)= XREM(28,I+IDEB)
                PENFIAE(NIN)%P(4,N2)= XREM(29,I+IDEB)
                PENFIAE(NIN)%P(5,N2)= XREM(30,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N1) = XREM(31,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N2) = XREM(32,I+IDEB)
               ENDIF
              ENDDO
C fin /DT/NODA/AMS
             ENDIF
            END IF
           END IF
          ELSEIF(IDTMINS==2)THEN
C /DT/AMS
           IF(INACTI/=5.AND.INACTI/=6) THEN
            IF(IGAP/=0) THEN
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                GAPFIE(NIN)%P(NN) = XREM(19,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N1) = XREM(20,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N2) = XREM(21,I+IDEB)
                NODNXFIE(NIN)%P(N1)   = NINT(XREM(22,I+IDEB))
                NODAMSFIE(NIN)%P(N1)  = NINT(XREM(23,I+IDEB))
                PROCAMSFIE(NIN)%P(N1) = P
                NODNXFIE(NIN)%P(N2)   = NINT(XREM(24,I+IDEB))
                NODAMSFIE(NIN)%P(N2)  = NINT(XREM(25,I+IDEB))
                PROCAMSFIE(NIN)%P(N2) = P
               END IF
              END DO
            ELSE
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N1) = XREM(19,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N2) = XREM(20,I+IDEB)
                NODNXFIE(NIN)%P(N1)   = NINT(XREM(21,I+IDEB))
                NODAMSFIE(NIN)%P(N1)  = NINT(XREM(22,I+IDEB))
                PROCAMSFIE(NIN)%P(N1) = P
                NODNXFIE(NIN)%P(N2)   = NINT(XREM(23,I+IDEB))
                NODAMSFIE(NIN)%P(N2)  = NINT(XREM(24,I+IDEB))
                PROCAMSFIE(NIN)%P(N2) = P
               END IF
              END DO
            END IF
           ELSE
            IF(IGAP/=0) THEN
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                GAPFIE(NIN)%P(NN)  = XREM(19,I+IDEB)
                PENFIE(NIN)%P(1,NN) = XREM(20,I+IDEB)
                PENFIE(NIN)%P(2,NN) = XREM(21,I+IDEB)
                PENFIAE(NIN)%P(1,N1)= XREM(22,I+IDEB)
                PENFIAE(NIN)%P(2,N1)= XREM(23,I+IDEB)
                PENFIAE(NIN)%P(3,N1)= XREM(24,I+IDEB)
                PENFIAE(NIN)%P(4,N1)= XREM(25,I+IDEB)
                PENFIAE(NIN)%P(5,N1)= XREM(26,I+IDEB)
                PENFIAE(NIN)%P(1,N2)= XREM(27,I+IDEB)
                PENFIAE(NIN)%P(2,N2)= XREM(28,I+IDEB)
                PENFIAE(NIN)%P(3,N2)= XREM(29,I+IDEB)
                PENFIAE(NIN)%P(4,N2)= XREM(30,I+IDEB)
                PENFIAE(NIN)%P(5,N2)= XREM(31,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N1) = XREM(32,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N2) = XREM(33,I+IDEB)
                NODNXFIE(NIN)%P(N1)   = NINT(XREM(34,I+IDEB))
                NODAMSFIE(NIN)%P(N1)  = NINT(XREM(35,I+IDEB))
                PROCAMSFIE(NIN)%P(N1) = P
                NODNXFIE(NIN)%P(N2)   = NINT(XREM(36,I+IDEB))
                NODAMSFIE(NIN)%P(N2)  = NINT(XREM(37,I+IDEB))
                PROCAMSFIE(NIN)%P(N2) = P
               ENDIF
              ENDDO
            ELSE
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                PENFIE(NIN)%P(1,NN) = XREM(19,I+IDEB)
                PENFIE(NIN)%P(2,NN) = XREM(20,I+IDEB)
                PENFIAE(NIN)%P(1,N1)= XREM(21,I+IDEB)
                PENFIAE(NIN)%P(2,N1)= XREM(22,I+IDEB)
                PENFIAE(NIN)%P(3,N1)= XREM(23,I+IDEB)
                PENFIAE(NIN)%P(4,N1)= XREM(24,I+IDEB)
                PENFIAE(NIN)%P(5,N1)= XREM(25,I+IDEB)
                PENFIAE(NIN)%P(1,N2)= XREM(26,I+IDEB)
                PENFIAE(NIN)%P(2,N2)= XREM(27,I+IDEB)
                PENFIAE(NIN)%P(3,N2)= XREM(28,I+IDEB)
                PENFIAE(NIN)%P(4,N2)= XREM(29,I+IDEB)
                PENFIAE(NIN)%P(5,N2)= XREM(30,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N1) = XREM(31,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N2) = XREM(32,I+IDEB)
                NODNXFIE(NIN)%P(N1)   = NINT(XREM(33,I+IDEB))
                NODAMSFIE(NIN)%P(N1)  = NINT(XREM(34,I+IDEB))
                PROCAMSFIE(NIN)%P(N1) = P
                NODNXFIE(NIN)%P(N2)   = NINT(XREM(35,I+IDEB))
                NODAMSFIE(NIN)%P(N2)  = NINT(XREM(36,I+IDEB))
                PROCAMSFIE(NIN)%P(N2) = P
               ENDIF
              ENDDO
            END IF
           END IF
C fin /DT/AMS
          ELSE ! IDTMINS_INT/=0
C /DT/INTER/AMS
           IF(INACTI/=5.AND.INACTI/=6) THEN
            IF(IGAP/=0) THEN
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                GAPFIE(NIN)%P(NN) = XREM(19,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N1) = XREM(20,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N2) = XREM(21,I+IDEB)
                NODAMSFIE(NIN)%P(N1)  = NINT(XREM(22,I+IDEB))
                PROCAMSFIE(NIN)%P(N1) = P
                NODAMSFIE(NIN)%P(N2)  = NINT(XREM(23,I+IDEB))
                PROCAMSFIE(NIN)%P(N2) = P
               END IF
              END DO
            ELSE
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N1) = XREM(19,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N2) = XREM(20,I+IDEB)
                NODAMSFIE(NIN)%P(N1)  = NINT(XREM(21,I+IDEB))
                PROCAMSFIE(NIN)%P(N1) = P
                NODAMSFIE(NIN)%P(N2)  = NINT(XREM(22,I+IDEB))
                PROCAMSFIE(NIN)%P(N2) = P
               END IF
              END DO
            END IF
           ELSE
            IF(IGAP/=0) THEN
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                GAPFIE(NIN)%P(NN)  = XREM(19,I+IDEB)
                PENFIE(NIN)%P(1,NN) = XREM(20,I+IDEB)
                PENFIE(NIN)%P(2,NN) = XREM(21,I+IDEB)
                PENFIAE(NIN)%P(1,N1)= XREM(22,I+IDEB)
                PENFIAE(NIN)%P(2,N1)= XREM(23,I+IDEB)
                PENFIAE(NIN)%P(3,N1)= XREM(24,I+IDEB)
                PENFIAE(NIN)%P(4,N1)= XREM(25,I+IDEB)
                PENFIAE(NIN)%P(5,N1)= XREM(26,I+IDEB)
                PENFIAE(NIN)%P(1,N2)= XREM(27,I+IDEB)
                PENFIAE(NIN)%P(2,N2)= XREM(28,I+IDEB)
                PENFIAE(NIN)%P(3,N2)= XREM(29,I+IDEB)
                PENFIAE(NIN)%P(4,N2)= XREM(30,I+IDEB)
                PENFIAE(NIN)%P(5,N2)= XREM(31,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N1) = XREM(32,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N2) = XREM(33,I+IDEB)
                NODAMSFIE(NIN)%P(N1)  = NINT(XREM(34,I+IDEB))
                PROCAMSFIE(NIN)%P(N1) = P
                NODAMSFIE(NIN)%P(N2)  = NINT(XREM(35,I+IDEB))
                PROCAMSFIE(NIN)%P(N2) = P
               ENDIF
              ENDDO
            ELSE
              DO I = 1, OLDNRTSR
               IF(XREM(1,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                N1 = 2*(NN-1)+1
                N2 = 2*NN
                NSVFIE(NIN)%P(NN) = NINT(-XREM(1,I+IDEB))
                XFIE(NIN)%P(1,N1) = XREM(2,I+IDEB)
                XFIE(NIN)%P(2,N1) = XREM(3,I+IDEB)
                XFIE(NIN)%P(3,N1) = XREM(4,I+IDEB)
                VFIE(NIN)%P(1,N1) = XREM(5,I+IDEB)
                VFIE(NIN)%P(2,N1) = XREM(6,I+IDEB)
                VFIE(NIN)%P(3,N1) = XREM(7,I+IDEB)
                MSFIE(NIN)%P(N1)  = XREM(8,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N1) = NINT(XREM(9,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N1) = IREM(1,I+IDEB)
                END IF
                XFIE(NIN)%P(1,N2) = XREM(10,I+IDEB)
                XFIE(NIN)%P(2,N2) = XREM(11,I+IDEB)
                XFIE(NIN)%P(3,N2) = XREM(12,I+IDEB)
                VFIE(NIN)%P(1,N2) = XREM(13,I+IDEB)
                VFIE(NIN)%P(2,N2) = XREM(14,I+IDEB)
                VFIE(NIN)%P(3,N2) = XREM(15,I+IDEB)
                MSFIE(NIN)%P(N2)  = XREM(16,I+IDEB)
                IF(IR4R8 == 2) THEN
                  ITAFIE(NIN)%P(N2) = NINT(XREM(17,I+IDEB))
                ELSE
                  ITAFIE(NIN)%P(N2) = IREM(2,I+IDEB)
                END IF
                STIFIE(NIN)%P(NN) = XREM(18,I+IDEB)
                PENFIE(NIN)%P(1,NN) = XREM(19,I+IDEB)
                PENFIE(NIN)%P(2,NN) = XREM(20,I+IDEB)
                PENFIAE(NIN)%P(1,N1)= XREM(21,I+IDEB)
                PENFIAE(NIN)%P(2,N1)= XREM(22,I+IDEB)
                PENFIAE(NIN)%P(3,N1)= XREM(23,I+IDEB)
                PENFIAE(NIN)%P(4,N1)= XREM(24,I+IDEB)
                PENFIAE(NIN)%P(5,N1)= XREM(25,I+IDEB)
                PENFIAE(NIN)%P(1,N2)= XREM(26,I+IDEB)
                PENFIAE(NIN)%P(2,N2)= XREM(27,I+IDEB)
                PENFIAE(NIN)%P(3,N2)= XREM(28,I+IDEB)
                PENFIAE(NIN)%P(4,N2)= XREM(29,I+IDEB)
                PENFIAE(NIN)%P(5,N2)= XREM(30,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N1) = XREM(31,I+IDEB)
                DIAG_SMSFIE(NIN)%P(N2) = XREM(32,I+IDEB)
                NODAMSFIE(NIN)%P(N1)  = NINT(XREM(33,I+IDEB))
                PROCAMSFIE(NIN)%P(N1) = P
                NODAMSFIE(NIN)%P(N2)  = NINT(XREM(34,I+IDEB))
                PROCAMSFIE(NIN)%P(N2) = P
               ENDIF
              ENDDO
            END IF
           END IF
C fin /DT/INTER/AMS
          END IF
          IDEB = IDEB + OLDNRTSR
          NSNFIE(NIN)%P(P) = NN-NNP
         END IF
        ENDDO
Cel facteur 2 car 2 noeuds par segment => pris en compte au niveau
Cel de la 1ere dimension allouee
        LSKYFI = NN*MULTIMAX
Cel nrtsr nouveau utile pour inacti et noeuds encrage
        NRTSR = NN
      ENDIF
C
C Deallocation de XREM
C
      IF(ALLOCATED(XREM)) DEALLOCATE(XREM)
      IF(IR4R8==1)THEN
        IF(ALLOCATED(IREM)) DEALLOCATE(IREM)
      END IF
C
      IERROR1=0 
      IERROR2=0 
      IERROR3=0 
C
C Allocation Parith/OFF
C
      IF(IPARIT==0) THEN
        IF(ASSOCIATED(AFIE(NIN)%P)) DEALLOCATE(AFIE(NIN)%P)
        IF(ASSOCIATED(STNFIE(NIN)%P)) DEALLOCATE(STNFIE(NIN)%P)
        IF(NODFI>0)ALLOCATE(AFIE(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
        IF(NODFI>0)ALLOCATE(STNFIE(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
C Init a 0
        DO I = 1, NODFI*NTHREAD
          AFIE(NIN)%P(1,I) = ZERO
          AFIE(NIN)%P(2,I) = ZERO
          AFIE(NIN)%P(3,I) = ZERO
          STNFIE(NIN)%P(I) = ZERO
        ENDDO
C
        IF(KDTINT/=0)THEN
          IF(ASSOCIATED(VSCFIE(NIN)%P)) DEALLOCATE(VSCFIE(NIN)%P)
          IF(NODFI>0)ALLOCATE(VSCFIE(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C Init a 0
          DO I = 1, NODFI*NTHREAD
            VSCFIE(NIN)%P(I) = ZERO
          ENDDO
        ENDIF
C
        NLSKYFIE(NIN) = NODFI
C
      ELSE
C
C Allocation Parith/ON
C
        IF(ASSOCIATED(FSKYFIE(NIN)%P)) DEALLOCATE(FSKYFIE(NIN)%P)
        IF(ASSOCIATED(ISKYFIE(NIN)%P)) DEALLOCATE(ISKYFIE(NIN)%P)
        NLSKYFIE(NIN) = LSKYFI
        IF(LSKYFI>0) THEN
          ALLOCATE(ISKYFIE(NIN)%P(LSKYFI),STAT=IERROR1)
          IF(KDTINT==0) THEN
C facteur 2 pour stocker les 2 noeuds
            ALLOCATE(FSKYFIE(NIN)%P(8,LSKYFI),STAT=IERROR2)
          ELSE
            ALLOCATE(FSKYFIE(NIN)%P(10,LSKYFI),STAT=IERROR2)
          ENDIF
        ENDIF
      ENDIF
C
C allocations specifiques interface type20
C
      IF(ASSOCIATED(DAANC6FIE(NIN)%P)) DEALLOCATE(DAANC6FIE(NIN)%P)    
      ALLOCATE(DAANC6FIE(NIN)%P(3,6,NODFI),STAT=IERROR1)
      IF(ASSOCIATED(DXANCFIE(NIN)%P)) DEALLOCATE(DXANCFIE(NIN)%P)    
      ALLOCATE(DXANCFIE(NIN)%P(3,NODFI),STAT=IERROR2)
      IF(ASSOCIATED(DVANCFIE(NIN)%P)) DEALLOCATE(DVANCFIE(NIN)%P)    
      ALLOCATE(DVANCFIE(NIN)%P(3,NODFI),STAT=IERROR3)
      IF(ASSOCIATED(ALPHAKFIE(NIN)%P)) DEALLOCATE(ALPHAKFIE(NIN)%P)    
      ALLOCATE(ALPHAKFIE(NIN)%P(NODFI),STAT=IERROR4)
      IF(ASSOCIATED(DAANCFIE(NIN)%P)) DEALLOCATE(DAANCFIE(NIN)%P)    
      ALLOCATE(DAANCFIE(NIN)%P(3,NODFI),STAT=IERROR4)

      IF(IERROR1+IERROR2+IERROR3+IERROR4/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
      DO K = 1, NODFI
        DO I = 1, 3
          DO J = 1, 6
            DAANC6FIE(NIN)%P(I,J,K) = ZERO
          END DO
        END DO
        ALPHAKFIE(NIN)%P(K) = ONE
      END DO

      DAANCFIE(NIN)%P(1:3,1:NODFI) = ZERO
C
      IF(IERROR1+IERROR2+IERROR3/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
C Renumerotation des candidats
C
      DO I = 1, I_STOK
        N = CAND_S(I)
        NN = N-NRTS
        IF(NN>0)THEN
          CAND_S(I) = INDEX(NN)+NRTS
        ENDIF
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_TRI17GAT                 source/mpi/interfaces/spmd_i7crit.F
Chd|-- called by -----------
Chd|        I17MAIN_TRI                   source/interfaces/int17/i17main_pena.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI17GAT(RESULT,NMES ,CAND_N ,I_STOK,NIN,
     2                         NMESR )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER RESULT, NIN, NMES, I_STOK, INACTI, NMESR,
     .        CAND_N(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER OLDNRTSR,ELEMFI,NNP,LSKYFI,
     .        NOD, LOC_PROC, I, N, NN, P, IDEB, N1, N2,
     .        IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,IERROR6,IERROR7,
     .        IERROR8,
     .        INDEX(NMESR)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
C Test succes du tri ?
C
      ELEMFI = 0
      IF(RESULT==0) THEN
C
C Reperage des candidats
C
        DO I = 1, I_STOK
          N = CAND_N(I)
          NN = N-NMES
          IF(NN>0)THEN
            IF(XREM(7,NN)>0)THEN
              ELEMFI = ELEMFI + 1
              XREM(7,NN) = -XREM(7,NN)
            ENDIF
          ENDIF
        ENDDO
C
C Allocation des tableaux de frontieres interfaces
C
        IF(ASSOCIATED(NSVFI(NIN)%P)) DEALLOCATE(NSVFI(NIN)%P)
        ALLOCATE(NSVFI(NIN)%P(ELEMFI),STAT=IERROR1)
        IF(ASSOCIATED(XFI17(NIN)%P)) DEALLOCATE(XFI17(NIN)%P)
        ALLOCATE(XFI17(NIN)%P(3,16,ELEMFI),STAT=IERROR2)
        IF(ASSOCIATED(VFI17(NIN)%P)) DEALLOCATE(VFI17(NIN)%P)
        ALLOCATE(VFI17(NIN)%P(3,16,ELEMFI),STAT=IERROR3)
        IF(ASSOCIATED(FROTSFI(NIN)%P)) DEALLOCATE(FROTSFI(NIN)%P)
        ALLOCATE(FROTSFI(NIN)%P(7,ELEMFI),STAT=IERROR4)
        IF(ASSOCIATED(KSFI(NIN)%P)) DEALLOCATE(KSFI(NIN)%P)
        ALLOCATE(KSFI(NIN)%P(2,ELEMFI),STAT=IERROR5)
        IF(ASSOCIATED(EMINXFI(NIN)%P)) DEALLOCATE(EMINXFI(NIN)%P)
        ALLOCATE(EMINXFI(NIN)%P(6,ELEMFI),STAT=IERROR6)
C
        IF(IERROR1+IERROR2+IERROR3+IERROR4+IERROR5+IERROR6/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
C
C Compactage des candidats
C
        IDEB = 0
        NN = 0
        DO P = 1, NSPMD
          OLDNRTSR = NSNFI(NIN)%P(P)
          IF(OLDNRTSR/=0) THEN
            NNP = NN
            DO I = 1, OLDNRTSR
              IF(XREM(7,I+IDEB)<0) THEN
                NN = NN + 1
                INDEX(I+IDEB) = NN
                EMINXFI(NIN)%P(1,NN) = XREM(1,I+IDEB)
                EMINXFI(NIN)%P(2,NN) = XREM(2,I+IDEB)
                EMINXFI(NIN)%P(3,NN) = XREM(3,I+IDEB)
                EMINXFI(NIN)%P(4,NN) = XREM(4,I+IDEB)
                EMINXFI(NIN)%P(5,NN) = XREM(5,I+IDEB)
                EMINXFI(NIN)%P(6,NN) = XREM(6,I+IDEB)
C                
                NSVFI(NIN)%P(NN) = NINT(-XREM(7,I+IDEB))
C
                XFI17(NIN)%P(1,1,NN) = XREM(8,I+IDEB)
                XFI17(NIN)%P(2,1,NN) = XREM(9,I+IDEB)
                XFI17(NIN)%P(3,1,NN) = XREM(10,I+IDEB)
                VFI17(NIN)%P(1,1,NN) = XREM(11,I+IDEB)
                VFI17(NIN)%P(2,1,NN) = XREM(12,I+IDEB)
                VFI17(NIN)%P(3,1,NN) = XREM(13,I+IDEB)
                XFI17(NIN)%P(1,2,NN) = XREM(14,I+IDEB)
                XFI17(NIN)%P(2,2,NN) = XREM(15,I+IDEB)
                XFI17(NIN)%P(3,2,NN) = XREM(16,I+IDEB)
                VFI17(NIN)%P(1,2,NN) = XREM(17,I+IDEB)
                VFI17(NIN)%P(2,2,NN) = XREM(18,I+IDEB)
                VFI17(NIN)%P(3,2,NN) = XREM(19,I+IDEB)
                XFI17(NIN)%P(1,3,NN) = XREM(20,I+IDEB)
                XFI17(NIN)%P(2,3,NN) = XREM(21,I+IDEB)
                XFI17(NIN)%P(3,3,NN) = XREM(22,I+IDEB)
                VFI17(NIN)%P(1,3,NN) = XREM(23,I+IDEB)
                VFI17(NIN)%P(2,3,NN) = XREM(24,I+IDEB)
                VFI17(NIN)%P(3,3,NN) = XREM(25,I+IDEB)
                XFI17(NIN)%P(1,4,NN) = XREM(26,I+IDEB)
                XFI17(NIN)%P(2,4,NN) = XREM(27,I+IDEB)
                XFI17(NIN)%P(3,4,NN) = XREM(28,I+IDEB)
                VFI17(NIN)%P(1,4,NN) = XREM(29,I+IDEB)
                VFI17(NIN)%P(2,4,NN) = XREM(30,I+IDEB)
                VFI17(NIN)%P(3,4,NN) = XREM(31,I+IDEB)
                XFI17(NIN)%P(1,5,NN) = XREM(32,I+IDEB)
                XFI17(NIN)%P(2,5,NN) = XREM(33,I+IDEB)
                XFI17(NIN)%P(3,5,NN) = XREM(34,I+IDEB)
                VFI17(NIN)%P(1,5,NN) = XREM(35,I+IDEB)
                VFI17(NIN)%P(2,5,NN) = XREM(36,I+IDEB)
                VFI17(NIN)%P(3,5,NN) = XREM(37,I+IDEB)
                XFI17(NIN)%P(1,6,NN) = XREM(38,I+IDEB)
                XFI17(NIN)%P(2,6,NN) = XREM(39,I+IDEB)
                XFI17(NIN)%P(3,6,NN) = XREM(40,I+IDEB)
                VFI17(NIN)%P(1,6,NN) = XREM(41,I+IDEB)
                VFI17(NIN)%P(2,6,NN) = XREM(42,I+IDEB)
                VFI17(NIN)%P(3,6,NN) = XREM(43,I+IDEB)
                XFI17(NIN)%P(1,7,NN) = XREM(44,I+IDEB)
                XFI17(NIN)%P(2,7,NN) = XREM(45,I+IDEB)
                XFI17(NIN)%P(3,7,NN) = XREM(46,I+IDEB)
                VFI17(NIN)%P(1,7,NN) = XREM(47,I+IDEB)
                VFI17(NIN)%P(2,7,NN) = XREM(48,I+IDEB)
                VFI17(NIN)%P(3,7,NN) = XREM(49,I+IDEB)
                XFI17(NIN)%P(1,8,NN) = XREM(50,I+IDEB)
                XFI17(NIN)%P(2,8,NN) = XREM(51,I+IDEB)
                XFI17(NIN)%P(3,8,NN) = XREM(52,I+IDEB)
                VFI17(NIN)%P(1,8,NN) = XREM(53,I+IDEB)
                VFI17(NIN)%P(2,8,NN) = XREM(54,I+IDEB)
                VFI17(NIN)%P(3,8,NN) = XREM(55,I+IDEB)
                XFI17(NIN)%P(1,9,NN) = XREM(56,I+IDEB)
                XFI17(NIN)%P(2,9,NN) = XREM(57,I+IDEB)
                XFI17(NIN)%P(3,9,NN) = XREM(58,I+IDEB)
                VFI17(NIN)%P(1,9,NN) = XREM(59,I+IDEB)
                VFI17(NIN)%P(2,9,NN) = XREM(60,I+IDEB)
                VFI17(NIN)%P(3,9,NN) = XREM(61,I+IDEB)
                XFI17(NIN)%P(1,10,NN) = XREM(62,I+IDEB)
                XFI17(NIN)%P(2,10,NN) = XREM(63,I+IDEB)
                XFI17(NIN)%P(3,10,NN) = XREM(64,I+IDEB)
                VFI17(NIN)%P(1,10,NN) = XREM(65,I+IDEB)
                VFI17(NIN)%P(2,10,NN) = XREM(66,I+IDEB)
                VFI17(NIN)%P(3,10,NN) = XREM(67,I+IDEB)
                XFI17(NIN)%P(1,11,NN) = XREM(68,I+IDEB)
                XFI17(NIN)%P(2,11,NN) = XREM(69,I+IDEB)
                XFI17(NIN)%P(3,11,NN) = XREM(70,I+IDEB)
                VFI17(NIN)%P(1,11,NN) = XREM(71,I+IDEB)
                VFI17(NIN)%P(2,11,NN) = XREM(72,I+IDEB)
                VFI17(NIN)%P(3,11,NN) = XREM(73,I+IDEB)
                XFI17(NIN)%P(1,12,NN) = XREM(74,I+IDEB)
                XFI17(NIN)%P(2,12,NN) = XREM(75,I+IDEB)
                XFI17(NIN)%P(3,12,NN) = XREM(76,I+IDEB)
                VFI17(NIN)%P(1,12,NN) = XREM(77,I+IDEB)
                VFI17(NIN)%P(2,12,NN) = XREM(78,I+IDEB)
                VFI17(NIN)%P(3,12,NN) = XREM(79,I+IDEB)
                XFI17(NIN)%P(1,13,NN) = XREM(80,I+IDEB)
                XFI17(NIN)%P(2,13,NN) = XREM(81,I+IDEB)
                XFI17(NIN)%P(3,13,NN) = XREM(82,I+IDEB)
                VFI17(NIN)%P(1,13,NN) = XREM(83,I+IDEB)
                VFI17(NIN)%P(2,13,NN) = XREM(84,I+IDEB)
                VFI17(NIN)%P(3,13,NN) = XREM(85,I+IDEB)
                XFI17(NIN)%P(1,14,NN) = XREM(86,I+IDEB)
                XFI17(NIN)%P(2,14,NN) = XREM(87,I+IDEB)
                XFI17(NIN)%P(3,14,NN) = XREM(88,I+IDEB)
                VFI17(NIN)%P(1,14,NN) = XREM(89,I+IDEB)
                VFI17(NIN)%P(2,14,NN) = XREM(90,I+IDEB)
                VFI17(NIN)%P(3,14,NN) = XREM(91,I+IDEB)
                XFI17(NIN)%P(1,15,NN) = XREM(92,I+IDEB)
                XFI17(NIN)%P(2,15,NN) = XREM(93,I+IDEB)
                XFI17(NIN)%P(3,15,NN) = XREM(94,I+IDEB)
                VFI17(NIN)%P(1,15,NN) = XREM(95,I+IDEB)
                VFI17(NIN)%P(2,15,NN) = XREM(96,I+IDEB)
                VFI17(NIN)%P(3,15,NN) = XREM(97,I+IDEB)
                XFI17(NIN)%P(1,16,NN) = XREM(98,I+IDEB)
                XFI17(NIN)%P(2,16,NN) = XREM(99,I+IDEB)
                XFI17(NIN)%P(3,16,NN) = XREM(100,I+IDEB)
                VFI17(NIN)%P(1,16,NN) = XREM(101,I+IDEB)
                VFI17(NIN)%P(2,16,NN) = XREM(102,I+IDEB)
                VFI17(NIN)%P(3,16,NN) = XREM(103,I+IDEB)
C
                FROTSFI(NIN)%P(1,NN) = XREM(104,I+IDEB)
                FROTSFI(NIN)%P(2,NN) = XREM(105,I+IDEB)
                FROTSFI(NIN)%P(3,NN) = XREM(106,I+IDEB)
                FROTSFI(NIN)%P(4,NN) = XREM(107,I+IDEB)
                FROTSFI(NIN)%P(5,NN) = XREM(108,I+IDEB)
                FROTSFI(NIN)%P(6,NN) = XREM(109,I+IDEB)
                FROTSFI(NIN)%P(7,NN) = XREM(110,I+IDEB)
C
                KSFI(NIN)%P(1,NN) = XREM(111,I+IDEB)
                KSFI(NIN)%P(2,NN) = XREM(112,I+IDEB)
C
              END IF
            END DO
            IDEB = IDEB + OLDNRTSR
            NSNFI(NIN)%P(P) = NN-NNP
          END IF
        ENDDO
C facteur 16 ?
        LSKYFI = NN*MULTIMAX
Cel nmesr inutile ici
        NMESR = NN
      ENDIF
C
C Deallocation de XREM
C
      IF(ALLOCATED(XREM)) DEALLOCATE(XREM)
C
      IERROR1=0 
      IERROR2=0 
      IERROR3=0 
C
C Allocation Parith/OFF
C
      IF(IPARIT==0) THEN
        IF(ASSOCIATED(AFI17(NIN)%P)) DEALLOCATE(AFI17(NIN)%P)
        IF(ASSOCIATED(STNFI17(NIN)%P)) DEALLOCATE(STNFI17(NIN)%P)
        IF(ELEMFI>0)ALLOCATE(AFI17(NIN)%P(3,16,ELEMFI),STAT=IERROR1)
        IF(ELEMFI>0)ALLOCATE(STNFI17(NIN)%P(16,ELEMFI),STAT=IERROR2)
C Init a 0
        DO I = 1, ELEMFI
          AFI17(NIN)%P(1,1,I) = ZERO
          AFI17(NIN)%P(2,1,I) = ZERO
          AFI17(NIN)%P(3,1,I) = ZERO
          STNFI17(NIN)%P(1,I) = ZERO
          AFI17(NIN)%P(1,2,I) = ZERO
          AFI17(NIN)%P(2,2,I) = ZERO
          AFI17(NIN)%P(3,2,I) = ZERO
          STNFI17(NIN)%P(2,I) = ZERO
          AFI17(NIN)%P(1,3,I) = ZERO
          AFI17(NIN)%P(2,3,I) = ZERO
          AFI17(NIN)%P(3,3,I) = ZERO
          STNFI17(NIN)%P(3,I) = ZERO
          AFI17(NIN)%P(1,4,I) = ZERO
          AFI17(NIN)%P(2,4,I) = ZERO
          AFI17(NIN)%P(3,4,I) = ZERO
          STNFI17(NIN)%P(4,I) = ZERO
          AFI17(NIN)%P(1,4,I) = ZERO
          AFI17(NIN)%P(2,4,I) = ZERO
          AFI17(NIN)%P(3,4,I) = ZERO
          STNFI17(NIN)%P(4,I) = ZERO
          AFI17(NIN)%P(1,5,I) = ZERO
          AFI17(NIN)%P(2,5,I) = ZERO
          AFI17(NIN)%P(3,5,I) = ZERO
          STNFI17(NIN)%P(5,I) = ZERO
          AFI17(NIN)%P(1,6,I) = ZERO
          AFI17(NIN)%P(2,6,I) = ZERO
          AFI17(NIN)%P(3,6,I) = ZERO
          STNFI17(NIN)%P(6,I) = ZERO
          AFI17(NIN)%P(1,7,I) = ZERO
          AFI17(NIN)%P(2,7,I) = ZERO
          AFI17(NIN)%P(3,7,I) = ZERO
          STNFI17(NIN)%P(7,I) = ZERO
          AFI17(NIN)%P(1,8,I) = ZERO
          AFI17(NIN)%P(2,8,I) = ZERO
          AFI17(NIN)%P(3,8,I) = ZERO
          STNFI17(NIN)%P(8,I) = ZERO
          AFI17(NIN)%P(1,9,I) = ZERO
          AFI17(NIN)%P(2,9,I) = ZERO
          AFI17(NIN)%P(3,9,I) = ZERO
          STNFI17(NIN)%P(9,I) = ZERO
          AFI17(NIN)%P(1,10,I) = ZERO
          AFI17(NIN)%P(2,10,I) = ZERO
          AFI17(NIN)%P(3,10,I) = ZERO
          STNFI17(NIN)%P(10,I) = ZERO
          AFI17(NIN)%P(1,11,I) = ZERO
          AFI17(NIN)%P(2,11,I) = ZERO
          AFI17(NIN)%P(3,11,I) = ZERO
          STNFI17(NIN)%P(11,I) = ZERO
          AFI17(NIN)%P(1,12,I) = ZERO
          AFI17(NIN)%P(2,12,I) = ZERO
          AFI17(NIN)%P(3,12,I) = ZERO
          STNFI17(NIN)%P(12,I) = ZERO
          AFI17(NIN)%P(1,13,I) = ZERO
          AFI17(NIN)%P(2,13,I) = ZERO
          AFI17(NIN)%P(3,13,I) = ZERO
          STNFI17(NIN)%P(13,I) = ZERO
          AFI17(NIN)%P(1,14,I) = ZERO
          AFI17(NIN)%P(2,14,I) = ZERO
          AFI17(NIN)%P(3,14,I) = ZERO
          STNFI17(NIN)%P(14,I) = ZERO
          AFI17(NIN)%P(1,15,I) = ZERO
          AFI17(NIN)%P(2,15,I) = ZERO
          AFI17(NIN)%P(3,15,I) = ZERO
          STNFI17(NIN)%P(15,I) = ZERO
          AFI17(NIN)%P(1,16,I) = ZERO
          AFI17(NIN)%P(2,16,I) = ZERO
          AFI17(NIN)%P(3,16,I) = ZERO
          STNFI17(NIN)%P(16,I) = ZERO
        ENDDO
C
C
      ELSE
C
C Allocation Parith/ON
C
        IF(ASSOCIATED(FSKYFI(NIN)%P)) DEALLOCATE(FSKYFI(NIN)%P)
        IF(ASSOCIATED(ISKYFI(NIN)%P)) DEALLOCATE(ISKYFI(NIN)%P)
        NLSKYFI(NIN) = LSKYFI
        IF(LSKYFI>0) THEN
          ALLOCATE(ISKYFI(NIN)%P(LSKYFI),STAT=IERROR1)
          ALLOCATE(FSKYFI(NIN)%P(40,LSKYFI),STAT=IERROR2)
        ENDIF
      ENDIF
C
      IF(IERROR1+IERROR2+IERROR3/=0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
C Renumerotation des candidats
C
      DO I = 1, I_STOK
        N = CAND_N(I)
        NN = N-NMES
        IF(NN>0)THEN
          CAND_N(I) = INDEX(NN)+NMES
        ENDIF
      ENDDO
C
      RETURN
      END
