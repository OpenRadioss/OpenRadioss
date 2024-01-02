Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  TRI25TMP                      share/modules/tri25tmp_mod.F  
Chd|-- called by -----------
Chd|        CHECK_FI                      source/mpi/interfaces/spmd_i25slide.F
Chd|        DEALLOCATE_FI1_TMP            source/mpi/interfaces/spmd_i25slide.F
Chd|        REALLOCATE_FI1                source/mpi/interfaces/spmd_i25slide.F
Chd|        SPMD_I25_SLIDE_GAT            source/mpi/interfaces/spmd_i25slide.F
Chd|-- calls ---------------
Chd|        POINTERDEF                    share/modules/pointerdef_mod.F
Chd|====================================================================
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
C tableau de pointeurs NSVFI de taille NINTER
C indiquant liste noeuds remote a recevoir du "proc main"
      TYPE(int_pointer) :: NSVFI

C Proc main of the secnd node. Assumes that only the main processor sends
C secnds nodes
      TYPE(int_pointer) PMAINFI

C tableau de pointeurs NSNFI de taille NINTER
C indiquant pour chaque interface type7 le nb de noeuds secnd dans NSVFI
C par processeur
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
C tableaux frontieres interfaces pour X, V A ou FSKY
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
C tableau de pointeurs ISKYFI de taille NINTER indiquant pour chaque interface
C la liste des noeuds remote associe a une force dans FSKYI
      TYPE(int_pointer) :: ISKYFI,
C tableau i20 partie edge
     .  ISKYFIE
C tableau de pointeurs NSVSI de taille NINTER
C indiquant liste noeuds remote a envoyer au "proc secnd"
      TYPE(int_pointer) :: NSVSI,
C tableau i20 partie edge
     .  NSVSIE
C tableau de pointeurs NSNSI de taille NINTER
C indiquant pour chaque interface type7, 10, 11 le nb de noeuds secnd dans NSVSI
C par processeur
      TYPE(int_pointer) :: NSNSI,
C tableau i20 partie edge
     .  NSNSIE
C tableau de pointeurs NISUBSFI de taille NINTER
C indiquant pour chaque interface type7, 10 le nbre d elt dans LISUBSFI
C par processeur
      TYPE(int_pointer) :: NISUBSFI
C tableau de pointeurs LISUBSFI de taille NINTER
C indiquant pour chaque interface type7, 10, et pour chaque noeud remote de cette interface
C la liste des sous interfaces
      TYPE(int_pointer) :: LISUBSFI
C tableau de pointeurs INFLG_SUBSFI de taille NINTER
C indiquant pour chaque interface type25 et pour chaque noeud remote de cette interface
C l appartenance a S1, S2, ou GRNOD
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: INFLG_SUBSFI
C tableau de pointeurs ADDSUBSFI de taille NINTER
C indiquant pour chaque interface type7, 10, et pour chaque noeud remote de cette interface
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
