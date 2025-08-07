Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    tri25tmp             ../engine/share/modules/tri25tmp_mod.F
!||--- called by ------------------------------------------------------
!||    check_fi             ../engine/source/mpi/interfaces/spmd_i25slide.F
!||    deallocate_fi1_tmp   ../engine/source/mpi/interfaces/spmd_i25slide.F
!||    reallocate_fi1       ../engine/source/mpi/interfaces/spmd_i25slide.F
!||    spmd_i25_slide_gat   ../engine/source/mpi/interfaces/spmd_i25slide.F
!||--- uses       -----------------------------------------------------
!||    pointerdef           ../engine/share/modules/pointerdef_mod.F
!||====================================================================
      MODULE TRI25TMP
#include "my_real.inc"
      USE POINTERDEF
c     USE TRI7BOX
c     TYPE int_pointer
c       INTEGER, DIMENSION(:), POINTER :: P
c     END TYPE int_pointer
c     TYPE int_pointer2
c       INTEGER, DIMENSION(:,:), POINTER :: P
c     END TYPE int_pointer2
c     TYPE real_pointer
c       my_real, DIMENSION(:), POINTER :: P
c     END TYPE real_pointer
c     TYPE real_pointer2
c       my_real, DIMENSION(:,:), POINTER :: P
c     END TYPE real_pointer2
c     TYPE real_pointer3
c       my_real, DIMENSION(:,:,:), POINTER :: P
c     END TYPE real_pointer3
c     TYPE r8_pointer
c       DOUBLE PRECISION, DIMENSION(:), POINTER :: P
c     END TYPE r8_pointer
c     TYPE r8_pointer3
c       DOUBLE PRECISION, DIMENSION(:,:,:), POINTER :: P
c     END TYPE r8_pointer3

      TYPE I25_TMP_STRUCT
C On utlise PANE_OLD
 
      INTEGER I24XREMP,IGAPXREMP,I24IREMP,I24IREMPNSNE
       
      INTEGER :: CURRENT_NODFI
      INTEGER :: CURRENT_FI_SIZE

C buffer envoi/reception forces interfaces
C pointer array NSVFI of size NINTER
C indiquant liste noeuds remote a recevoir du "proc main"
      TYPE(int_pointer) :: NSVFI

C Proc main of the secnd node. Assumes that only the main processor sends
C secnds nodes
      TYPE(int_pointer) PMAINFI

C pointer array NSNFI of size NINTER
C indicating for each interface type7 the number of secondary nodes in NSVFI
C per processor
      TYPE(int_pointer) :: 
     .  NSNFI, NSNFI_SAV, ITAFI, KINFI, MATSFI,
C tableau i20 specifique (flag nodal)
     .  NBINFLFI,
     .  NODNXFI, NODAMSFI, PROCAMSFI,
C tableau i20 partie edge
     .  NSNFIE,NSVFIE,ITAFIE,NODNXFIE, NODAMSFIE, PROCAMSFIE,
C Friction model : secnd parts id
     .  IPARTFRICSFI,IPARTFRIC_FIE,
C type25 interface adhesion
     .  IF_ADHFI
C tableaux frontieres interfaces pour MS, STFN, STIFN
      TYPE(int_pointer) :: ICODT_FI,ISKEW_FI
      TYPE(real_pointer) :: 
     .  MSFI, STIFI, STNFI, GAPFI,VSCFI, FTHEFI, FTHESKYFI,
     .  AREASFI, TEMPFI, ALPHAKFI, DIAG_SMSFI,GAP_LFI,CONDNFI,
     .  CONDNSKYFI,
C tableau i20 partie edge
     .  MSFIE, STIFIE, STNFIE, GAPFIE, VSCFIE, ALPHAKFIE,
     .  DIAG_SMSFIE,
C tableau I18 Kine
     .  MTFI_PENE,MTFI_PENEMIN
C interface boundary arrays for X, V A or FSKY
      TYPE(real_pointer2) :: 
     .  XFI, VFI, AFI, FSKYFI, PENFI, PENFIA,MTFI_V,MTFI_A,
     .  I18KAFI,MTFI_N,DAANCFI
C tableau i20 partie edge
     .  XFIE, VFIE, AFIE, FSKYFIE, PENFIE, PENFIAE, DAANCFIE
C tableaux frontieres interfaces 17 pour
      TYPE(real_pointer2) :: 
     .  EMINXFI, KSFI, FROTSFI, STNFI17
C tableaux frontieres interfaces 17 pour XFI17, VFI17, AFI17
      TYPE(real_pointer3) :: 
     .  XFI17, VFI17, AFI17
C tableaux frontieres interfaces 20 pour DXANC,DVANC
      TYPE(real_pointer2) :: 
     .  DXANCFI, DVANCFI, DXANCFIE, DVANCFIE
C tableaux frontieres interfaces 20 pour DAANC6
      TYPE(r8_pointer3) :: 
     .  DAANC6FI, DAANC6FIE
C pointer array ISKYFI of size NINTER indicating for each interface
C the list of remote nodes associated with a force in FSKYI
      TYPE(int_pointer) :: ISKYFI,
C tableau i20 partie edge
     .  ISKYFIE
C pointer array NSVSI of size NINTER
C indiquant liste noeuds remote a envoyer au "proc secnd"
      TYPE(int_pointer) :: NSVSI,
C tableau i20 partie edge
     .  NSVSIE
C pointer array NSNSI of size NINTER
C indicating for each interface type7, 10, 11 the number of secondary nodes in NSVSI
C per processor
      TYPE(int_pointer) :: NSNSI,
C tableau i20 partie edge
     .  NSNSIE
C pointer array NISUBSFI of size NINTER
C indicating for each interface type7, 10 the number of elements in LISUBSFI
C per processor
      TYPE(int_pointer) :: NISUBSFI
C pointer array LISUBSFI of size NINTER
C indicating for each interface type7, 10, and for each remote node of this interface
C the list of sub-interfaces
      TYPE(int_pointer) :: LISUBSFI
C pointer array INFLG_SUBSFI of size NINTER
C indicating for each interface type25 and for each remote node of this interface
C l appartenance a S1, S2, ou GRNOD
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: INFLG_SUBSFI
C pointer array ADDSUBSFI of size NINTER
C indicating for each interface type7, 10, and for each remote node of this interface
C son adresse de depart dans LISUBSFI et INFLG_SUBSFI(tableau dimensionne a NSNR+1 (noeud remote + 1)
      TYPE(int_pointer) :: ADDSUBSFI
      TYPE(real_pointer2) :: FNCONTI, FTCONTI
      TYPE(real_pointer) :: EFRICFI, EFRICGFI
      INTEGER  :: NLSKYFI, NLSKYFIE
C Flag array to tag if NSNFI was frozen into NSNFI_SAV - Case INACTI + TSTART
      INTEGER  :: NSNFI_FLAG
C Type 24 & 25 Interface
      TYPE(int_pointer) :: ICONT_I_FI
      TYPE(int_pointer2)  :: IRTLM_FI
C T24 E2E
      TYPE(int_pointer2)  :: IRTSE_FI
      TYPE(int_pointer2)  :: IS2SE_FI
      TYPE(int_pointer)   :: IS2PT_FI
      TYPE(int_pointer)   :: ISEGPT_FI
      TYPE(int_pointer)   :: ISEDGE_FI

      TYPE(real_pointer) :: TIME_SFI
      TYPE(real_pointer2) :: SECND_FRFI
      TYPE(real_pointer2) :: PENE_OLDFI
      TYPE(real_pointer2) :: STIF_OLDFI
C Type 25 Interface
      TYPE(int_pointer2)  :: ISLIDE_FI
C Type 25 Interface FLAGREMN
      TYPE(int_pointer)  :: REMNOR_FI
      TYPE(int_pointer)  :: KREMNOR_FI
      TYPE(real_pointer)  :: STIF_MSDT_FI
      TYPE(real_pointer)  :: INTAREANFI

      END TYPE I25_TMP_STRUCT

      END MODULE TRI25TMP
