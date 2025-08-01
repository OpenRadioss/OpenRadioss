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
!||    s4coor3             ../starter/source/elements/solid/solide4/s4coor3.F
!||--- called by ------------------------------------------------------
!||    inirig_mat          ../starter/source/elements/initia/inirig_mat.F
!||    inisoldist          ../starter/source/initial_conditions/inivol/inisoldist.F
!||    inivoid             ../starter/source/elements/initia/inivoid.F
!||    multifluid_init3t   ../starter/source/multifluid/multifluid_init3t.F
!||    s4init3             ../starter/source/elements/solid/solide4/s4init3.F
!||--- calls      -----------------------------------------------------
!||    checkvolume_4n      ../starter/source/elements/solid/solide/checksvolume.F
!||--- uses       -----------------------------------------------------
!||    message_mod         ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE S4COOR3(X     ,XREFS   ,IXS    ,NGL     ,MXT    ,
     .                  NGEO  ,IX1     ,IX2    ,IX3     ,IX4    ,
     .                  X1   ,X2   ,X3   ,X4   ,Y1   ,Y2   ,
     .                  Y3   ,Y4   ,Z1   ,Z2   ,Z3   ,Z4   )
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), NGL(*), MXT(*),NGEO(*),
     .   IX1(*), IX2(*), IX3(*), IX4(*)
      DOUBLE PRECISION
     .   X1(*),X2(*),X3(*),X4(*),
     .   Y1(*),Y2(*),Y3(*),Y4(*),
     .   Z1(*),Z2(*),Z3(*),Z4(*) 
C     REAL
      my_real
     .   X(3,*),XREFS(8,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N1,N2,N3,N4
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      my_real
     .   CHECKVOLUME_4N
C=======================================================================
C     CONNECTIVITES ET NUMERO DE MATERIAU ET PID
C--------------------------------------------------
      DO I=LFT,LLT
        MXT(I) =IXS(1,I)
        NGEO(I)=IXS(NIXS-1,I)
        NGL(I) =IXS(NIXS,I)
        IX1(I) =IXS(2,I)
        IX2(I) =IXS(4,I)
        IX3(I) =IXS(7,I)
        IX4(I) =IXS(6,I)
      ENDDO
C
      IF (NXREF == 0) THEN
        DO I=LFT,LLT
          IF (CHECKVOLUME_4N(X ,IXS(1,I)) < ZERO) THEN
C           renumber connectivity
            IX2(I)=IXS(6,I)
            IX4(I)=IXS(4,I)
            IXS(4,I)=IX2(I)
            IXS(6,I)=IX4(I)
            IXS(5,I)=IX2(I)
            IXS(9,I)=IX4(I)
          ENDIF
          X1(I)=X(1,IX1(I))
          Y1(I)=X(2,IX1(I))
          Z1(I)=X(3,IX1(I))
          X2(I)=X(1,IX2(I))
          Y2(I)=X(2,IX2(I))
          Z2(I)=X(3,IX2(I))
          X3(I)=X(1,IX3(I))
          Y3(I)=X(2,IX3(I))
          Z3(I)=X(3,IX3(I))
          X4(I)=X(1,IX4(I))
          Y4(I)=X(2,IX4(I))
          Z4(I)=X(3,IX4(I))
        ENDDO
      ELSE  ! XREF
        DO I=LFT,LLT
          IF (CHECKVOLUME_4N(X ,IXS(1,I)) < ZERO) THEN
C           renumber connectivity
            IX2(I)=IXS(6,I)
            IX4(I)=IXS(4,I)
            IXS(4,I)=IX2(I)
            IXS(6,I)=IX4(I)
            IXS(5,I)=IX2(I)
            IXS(9,I)=IX4(I)
            X1(I) = XREFS(1,1,I)
            Y1(I) = XREFS(1,2,I)
            Z1(I) = XREFS(1,3,I)
            X2(I) = XREFS(5,1,I)
            Y2(I) = XREFS(5,2,I)
            Z2(I) = XREFS(5,3,I)
            X3(I) = XREFS(6,1,I)
            Y3(I) = XREFS(6,2,I)
            Z3(I) = XREFS(6,3,I)
            X4(I) = XREFS(3,1,I)
            Y4(I) = XREFS(3,2,I)
            Z4(I) = XREFS(3,3,I)
          ELSE
            X1(I) = XREFS(1,1,I)
            Y1(I) = XREFS(1,2,I)
            Z1(I) = XREFS(1,3,I)
            X2(I) = XREFS(3,1,I)
            Y2(I) = XREFS(3,2,I)
            Z2(I) = XREFS(3,3,I)
            X3(I) = XREFS(6,1,I)
            Y3(I) = XREFS(6,2,I)
            Z3(I) = XREFS(6,3,I)
            X4(I) = XREFS(5,1,I)
            Y4(I) = XREFS(5,2,I)
            Z4(I) = XREFS(5,3,I)
          ENDIF
          XREFS(1,1,I) = X1(I)  
          XREFS(1,2,I) = Y1(I)  
          XREFS(1,3,I) = Z1(I)  
          XREFS(2,1,I) = X2(I)  
          XREFS(2,2,I) = Y2(I)  
          XREFS(2,3,I) = Z2(I)  
          XREFS(3,1,I) = X3(I)  
          XREFS(3,2,I) = Y3(I)  
          XREFS(3,3,I) = Z3(I)  
          XREFS(4,1,I) = X4(I)  
          XREFS(4,2,I) = Y4(I)  
          XREFS(4,3,I) = Z4(I)  
        ENDDO
      ENDIF
C-----------
      RETURN
      END
