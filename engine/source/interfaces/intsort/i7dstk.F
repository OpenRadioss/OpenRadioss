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
!||    i7dstk        ../engine/source/interfaces/intsort/i7dstk.F
!||--- called by ------------------------------------------------------
!||    i10tri        ../engine/source/interfaces/intsort/i10tri.F
!||    i11tri        ../engine/source/interfaces/intsort/i11tri.F
!||    i20tri        ../engine/source/interfaces/intsort/i20tri.F
!||    i20tri_edge   ../engine/source/interfaces/intsort/i20tri.F
!||    i21tri        ../engine/source/interfaces/intsort/i21tri.F
!||    i7tri         ../engine/source/interfaces/intsort/i7tri.F
!||====================================================================
        SUBROUTINE I7DSTK(NB_NC,NB_EC,ADD,BPN,PN,BPE,PE)
C============================================================================
C  cette routine est appelee par : I7TRI(/int7/i7tri.F)
C                                  I10TRI(/int10/i10tri.F)
C                                  I11TRI(/int11/i11tri.F)
C----------------------------------------------------------------------------
C  cette routine appelle : -
C============================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   RECHAGE LES VECTEURS BAS DE PILE BPN ET BPE AVEC LES BAS DES
C   PILES PN ET PE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C     NOM          DESCRIPTION                       E/S
C
C     BPE          TABLEAU DES FACETTES A TRIER      S
C                  ET DU RESULTAT COTE MAX            
C     PE           TABLEAU DES FACETTES              E
C                  RESULTAT COTE MIN
C     BPN          TABLEAU DES NOEUDS A TRIER        S
C                  ET DU RESULTAT COTE MAX            
C     PN           TABLEAU DES NOEUDS                E
C                  RESULTAT COTE MIN
C     ADD(2,*)     TABLEAU DES ADRESSES              E  
C          1.......ADRESSES NOEUDS
C          2.......ADRESSES ELEMENTS
C     NB_NC        NOMBRE DE NOEUDS CANDIDATS        s 
C     NB_EC        NOMBRE D'ELTS CANDIDATS           s 
C 
C-----------------------------------------------
      INTEGER NB_NC,NB_EC
      INTEGER ADD(2,0:*),BPE(*),PE(*),BPN(*),PN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
C
C     1- PILE DES NOEUDS
C
      NB_NC = ADD(1,1) - ADD(1,0)
      DO 10 I=1,NB_NC
        BPN(I) = PN(ADD(1,0)+I)
   10 CONTINUE
C
C     2- PILE DES ELEMENTS
C
      NB_EC = ADD(2,1) - ADD(2,0)
      DO 20 I=1,NB_EC
        BPE(I) = PE(ADD(2,0)+I)
   20 CONTINUE
      RETURN
      END
