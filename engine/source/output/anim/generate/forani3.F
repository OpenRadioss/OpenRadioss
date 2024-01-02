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
Chd|  FORANI3                       source/output/anim/generate/forani3.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE FORANI3(FANI,A,MS,NFEA,NODFT,NODLT,H3D_DATA)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is related to output of FINT and FEXT vectors.
C   When FORANI1 is called, A is containing: FEXT
C   When FORANI2 is called, A is containing: FEXT+FINT
C   When FORANI3 is called, A is containing:(FEXT+FINT+FGRAV)/m
C
C                        forani1       forani2                   forani3
C       FANI(:,N+NFIA) =   -FEXT  +(FEXT+FINT)                               = FINT
C       FANI(:,N+NFEA) =   +FEXT  -(FEXT+FINT)  + [(FEXT+FINT+FRAV)/m]*m     = FEXT+FGRAV
C-----------------------------------------------
C
C  ANIM_V(5)            : /ANIM/VECT/FINT
C  OUTP_V(5)            : /OUTP/VECT/FINT
C  H3D_DATA%N_VECT_FINT : /H3D/NODA/FINT
C
C  ANIM_V(6)            : /ANIM/VECT/FEXT
C  OUTP_V(6)            : /OUTP/VECT/FEXT
C  H3D_DATA%N_VECT_FEXT : /H3D/NODA/FEXT
C
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
!ANIM_V
#include      "scr14_c.inc"
!OUTP_V
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFEA,NODFT,NODLT
      my_real A(3,*),FANI(3,*),MS(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,K
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT > 0) THEN
      !if FEXT is requested (ANIM or OUTP or H3D)
#include      "vectorize.inc"
        DO N=NODFT,NODLT
          FANI(1,N+NFEA)=FANI(1,N+NFEA) + A(1,N)*MS(N)
          FANI(2,N+NFEA)=FANI(2,N+NFEA) + A(2,N)*MS(N)
          FANI(3,N+NFEA)=FANI(3,N+NFEA) + A(3,N)*MS(N)
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END SUBROUTINE FORANI3
