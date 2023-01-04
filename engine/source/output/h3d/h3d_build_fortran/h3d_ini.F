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
Chd|  H3D_INI                       source/output/h3d/h3d_build_fortran/h3d_ini.F
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE H3D_INI(H3D_DATA,KH3D,NSLASH)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (H3D_DATABASE) :: H3D_DATA
      INTEGER KH3D,NSLASH(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NH3D,KCUR
     
C=========================================================================
      KCUR           = KH3D
      NH3D = NSLASH(KCUR)
c
      IF (NH3D /= 0) THEN 
        ALLOCATE(H3D_DATA%INPUT_LIST(10000))
        H3D_DATA%N_INPUT_H3D = 0
c
        ALLOCATE(H3D_DATA%PARTS(1))
c
        ALLOCATE(H3D_DATA%OUTPUT_LIST(10000))
        H3D_DATA%OUTPUT_LIST(1:10000)%OK = 0
      ELSE
        ALLOCATE(H3D_DATA%INPUT_LIST(1))
c
        ALLOCATE(H3D_DATA%PARTS(1))
c
        ALLOCATE(H3D_DATA%OUTPUT_LIST(1))
      ENDIF
c
      H3D_DATA%IH3D = 0
      H3D_DATA%IH3D_RUN = 0
      H3D_DATA%N_INPUT_H3D = 0
      H3D_DATA%IPART_SELECT = 0
      H3D_DATA%MH3D = 0
      H3D_DATA%N_OUTP_H3D = 0
c
      H3D_DATA%N_SCAL_DT = 0	 
      H3D_DATA%N_SCAL_DMAS = 0	 
      H3D_DATA%N_SCAL_DINER = 0	 
      H3D_DATA%N_SCAL_DAMA2 = 0	 
      H3D_DATA%N_SCAL_SKID = 0
      H3D_DATA%N_SCAL_STIFR = 0
      H3D_DATA%N_SCAL_STIFN = 0
c
      H3D_DATA%N_VECT_CONT = 0	 
      H3D_DATA%N_VECT_FINT = 0	 
      H3D_DATA%N_VECT_FEXT = 0	 
      H3D_DATA%N_VECT_PCONT = 0	 
      H3D_DATA%N_VECT_CONT2 = 0	 
      H3D_DATA%N_VECT_PCONT2 = 0	 
      H3D_DATA%N_VECT_DROT = 0	 
      H3D_DATA%N_VECT_DXANC = 0	 
      H3D_DATA%N_VECT_FREAC = 0	 
      H3D_DATA%N_VECT_MREAC = 0	 
      H3D_DATA%N_VECT_CLUST_FORCE = 0
      H3D_DATA%N_VECT_CLUST_MOM = 0  
      H3D_DATA%N_VECT_CONT_MAX = 0
      H3D_DATA%N_VECT_PCONT_MAX = 0
      H3D_DATA%N_VECT_CONT2_MAX = 0	 
      H3D_DATA%N_VECT_PCONT2_MAX = 0
      H3D_DATA%N_VECT_CONT2_MIN = 0	 
      H3D_DATA%N_VECT_PCONT2_MIN = 0
      H3D_DATA%UND_FORC = 0
c 
      H3D_DATA%RBODY_SINGLE = 0  
      H3D_DATA%RBE2_SINGLE = 0  
      H3D_DATA%RBE3_SINGLE = 0  
c 
      H3D_DATA%TH3D = EP30
      H3D_DATA%DTH3D = ZERO
      H3D_DATA%TH3D0 = ZERO
      H3D_DATA%DTH3D0 = ZERO
      H3D_DATA%PERCENTAGE_ERROR = ZERO
      H3D_DATA%COMP_LEVEL = 7
      H3D_DATA%STRAIN = 0

      END
