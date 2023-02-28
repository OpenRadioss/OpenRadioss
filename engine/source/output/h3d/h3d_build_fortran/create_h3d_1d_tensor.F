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
Chd|  CREATE_H3D_1D_TENSOR          source/output/h3d/h3d_build_fortran/create_h3d_1d_tensor.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE CREATE_H3D_1D_TENSOR(H3D_DATA,ID_1D_TENSOR,ID_INPUT,TEXT1,STEXT1,
     .                                COMMENT,SCOMMENT,IPART,KEY3_GLOB,IPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "chara_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (H3D_DATABASE) :: H3D_DATA
      INTEGER ID_1D_TENSOR,ID_INPUT,STEXT1,SCOMMENT,IPT
      INTEGER IPART(LIPART1,*)
      CHARACTER(LEN=STEXT1) :: TEXT1
      CHARACTER(LEN=SCOMMENT) ::  COMMENT
      CHARACTER(LEN=ncharline) ::  KEY3_GLOB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER(LEN=ncharline) :: CARTE,CARTE1
      INTEGER I,J,L,N_H3D_PART
C=========================================================================
      H3D_DATA%N_OUTP_H3D = H3D_DATA%N_OUTP_H3D + 1
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%OK = 1
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%KEYWORD = KEY3_GLOB
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%N_OUTP = H3D_DATA%N_OUTP_H3D + 3
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%ETYPE = 4
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%OUTP_TYPE = 3
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%IPT = IPT
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%IS_INFO1 = 0
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%S_STRING1 = STEXT1
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%STRING1(1:STEXT1) = TEXT1(1:STEXT1)
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%S_STRING2 = 0
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%S_COMMENT = SCOMMENT
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%COMMENT(1:SCOMMENT) = COMMENT(1:SCOMMENT)

c      print *,' H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%STRING1(1:STEXT1) = TEXT1(1:STEXT1)'
c      print *, H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%STRING1(1:STEXT1)

      N_H3D_PART = H3D_DATA%INPUT_LIST(ID_INPUT)%NB_PART
      H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%N_H3D_PART_LIST = N_H3D_PART

      IF (N_H3D_PART /= 0 )
     .       ALLOCATE(H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PART_LIST(N_H3D_PART))
      ALLOCATE(H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PART(NPART))
c
c      DO I=1,N_H3D_PART
c        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PART_LIST(I) =
c     .        H3D_DATA%INPUT_LIST(ID_INPUT)%PART_LIST(I)
c
c      ENDDO
      IF (N_H3D_PART /= 0 ) THEN
        DO I=1,NPART
          H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PART(I) = 0
        ENDDO
      ELSE
        DO I=1,NPART
          H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PART(I) = 1
        ENDDO
      ENDIF

c      DO J=1,N_H3D_PART
c        print *,'ds la construct_lect',H3D_DATA%INPUT_LIST(ID_INPUT)%PART_LIST(J)
c      ENDDO
 
      DO J=1,N_H3D_PART
        DO I=1,NPART
          IF(H3D_DATA%INPUT_LIST(ID_INPUT)%PART_LIST(J) == IPART(4,I)) THEN
              H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PART(I) = 1
          ENDIF
        ENDDO
      ENDDO

c      DO J=1,NPART
c        print *,'ds la construct',H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PART(J)
c      ENDDO

      RETURN
 999  print *,'error lecture'
      CALL ARRET(0)
      END
