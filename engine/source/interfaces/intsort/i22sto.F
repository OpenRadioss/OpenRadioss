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
Chd|  I22STO                        source/interfaces/intsort/i22sto.F
Chd|-- called by -----------
Chd|        I22TRIVOX                     source/interfaces/intsort/i22trivox.F
Chd|-- calls ---------------
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE I22STO(
     1      J_STOK ,IRECT   ,X       ,II_STOK ,CAND_B,
     2      CAND_E ,MULNSN  ,NOINT   ,MARGE   ,I_MEM ,
     3      PROV_B ,PROV_E  ,ESHIFT  ,ITASK   ,NC    ,
     4      IXS    ,BUFBRIC ,NBRIC   ,IsSTO )
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22TRI_MOD !pour le debug uniquement
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
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM, NSN, IGAP,NIN, ITASK, IXS(NIXS,*), nbric
      INTEGER J_STOK,MULNSN,NOINT,INACTI,ESHIFT,BUFBRIC(NBRIC)
      INTEGER IRECT(4,*),CAND_B(*),CAND_E(*),  NC
      INTEGER PROV_B(MVSIZ),PROV_E(MVSIZ),  II_STOK
      LOGICAL IsSTO
      my_real
     .        X(3,*),  MARGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K_STOK,I_STOK,N,NE,J

C-----------------------------------------------
!#include "sync_dir.inc"
C-----------------------------------------------

      IF(.NOT.IsSTO)THEN         
        IsSTO=.TRUE.
c        if(ibug22_trivox==1)print *, "   lockon", ITASK
#include "lockon.inc" 
      END IF

      I_STOK = II_STOK

      IF(I_STOK + J_STOK>MULNSN) THEN
        I_MEM = 2 ! Pas assez de place allouee  
        IsSTO = .FALSE.
        !II_STOK = ZERO fait dans i22buce     
c        if(ibug22_trivox==1)print *, "   lockoff", ITASK             
#include "lockoff.inc"  
        RETURN
      ENDIF

      DO I=1,J_STOK         
        I_STOK = I_STOK + 1                         
        CAND_B(I_STOK) = PROV_B(I) !numero de la brique dans le groupe local
        CAND_E(I_STOK) = PROV_E(I) !est deja un numero global (1:NIRECT_L) pas besoin ESHIFT    
c        if(ibug22_trivox==1)WRITE(*,FMT='(A,I8,A,I8,I8)')
c     .   "   I22sto : ecriture Bric=", 
c     .   IXS(11,BUFBRIC(PROV_B(I))), " ShellDiag= ",
c     .   NINT(IRECT_L((/1,3/),ABS(PROV_E(I))))
c        if(ibug22_trivox==1)print *, ""
      ENDDO
C-----------------------------------------------
      II_STOK = II_STOK + J_STOK
         
!#include "lockoff.inc"  lockoff seulement losque la brique courante est completement traitee

      RETURN
      END
