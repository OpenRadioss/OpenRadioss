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
!||    create_h3d_shell_scalar          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_scalar.F
!||--- called by ------------------------------------------------------
!||    lech3d                           ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
!||--- calls      -----------------------------------------------------
!||    arret                            ../engine/source/system/arret.F
!||    create_h3d_output_per_part       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_output_per_part.F90
!||--- uses       -----------------------------------------------------
!||    create_h3d_output_per_part_mod   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_output_per_part.F90
!||    groupdef_mod                     ../common_source/modules/groupdef_mod.F
!||    h3d_mod                          ../engine/share/modules/h3d_mod.F
!||    names_and_titles_mod             ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE CREATE_H3D_SHELL_SCALAR(H3D_DATA,ID_SHELL_SCALAR ,N1       ,N2       ,ID_INPUT ,TEXT1  ,
     .                                       STEXT1       ,COMMENT    ,SCOMMENT  ,IPART    ,KEY3_GLOB ,
     .                                       LAYER    ,IPT    ,PLY             ,GAUSS    ,IUVAR       ,
     .                                       IDMDS    ,IVARMDS,ID_MAT_MDS,MDS_LABEL, SMDS_LABEL       ,
     .                                       ID       ,MODE   ,IGRPART)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE H3D_MOD
        USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
        USE GROUPDEF_MOD
        USE CREATE_H3D_OUTPUT_PER_PART_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE (H3D_DATABASE) :: H3D_DATA
        INTEGER ID_SHELL_SCALAR,ID_INPUT,STEXT1,N1,N2,SCOMMENT,
     .          LAYER,IPT,PLY,IUVAR,GAUSS,IDMDS,IVARMDS,SMDS_LABEL,
     .          ID_MAT_MDS,ID
        INTEGER IPART(LIPART1,*)
        CHARACTER(LEN=STEXT1) ::  TEXT1
        CHARACTER(LEN=NCHARLINE100) :: KEY3_GLOB
        CHARACTER(LEN=SCOMMENT) :: COMMENT
        CHARACTER(LEN=64) :: MDS_LABEL
        INTEGER ,INTENT(IN) :: MODE
        TYPE(GROUP_)  ,DIMENSION(NGRPART) ,INTENT(IN) :: IGRPART
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        CHARACTER(LEN=NCHARLINE100) :: CARTE,CARTE1,TEXT
        INTEGER I,J,L,N_H3D_PART,STEXT
C=========================================================================
        TEXT = ''
        STEXT = 0

        H3D_DATA%N_OUTP_H3D = H3D_DATA%N_OUTP_H3D + 1
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%OK = 1
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%KEYWORD = KEY3_GLOB
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%N_OUTP = H3D_DATA%N_OUTP_H3D + 3
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%ETYPE = 2
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%OUTP_TYPE = 1
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%LAYER = LAYER
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%IPT = IPT
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PLY = PLY
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%IUVAR = IUVAR
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%IDMDS = IDMDS
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%IDMATMDS = ID_MAT_MDS
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%IMDSVAR = IVARMDS
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%MDSVAR_NAME = TRIM(MDS_LABEL)
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%SMDSVAR_NAME = SMDS_LABEL
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%GAUSS = GAUSS
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%OBJECT_ID = ID
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%S_STRING1 = STEXT1
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%STRING1(1:STEXT1) = TEXT1(1:STEXT1)
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%S_COMMENT = SCOMMENT
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%COMMENT(1:SCOMMENT) = COMMENT(1:SCOMMENT)
        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%MODE = MODE

        N_H3D_PART = H3D_DATA%INPUT_LIST(ID_INPUT)%NB_PART

        H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%N_H3D_PART_LIST = N_H3D_PART

        IF (N_H3D_PART /= 0 )
     .         ALLOCATE(H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PART_LIST(N_H3D_PART))
        ALLOCATE(H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PART(NPART))
c
        IF (N_H3D_PART /= 0 ) THEN
          DO I=1,NPART
            H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PART(I) = 0
          ENDDO
        ELSE
          DO I=1,NPART
            H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PART(I) = 1
          ENDDO
        ENDIF
c 
       CALL CREATE_H3D_OUTPUT_PER_PART(N_H3D_PART,H3D_DATA,ID_INPUT,LIPART1,NPART,IPART,NGRPART,IGRPART)

c      DO J=1,NPART
c        print *,'ds la construct',H3D_DATA%OUTPUT_LIST(H3D_DATA%N_OUTP_H3D)%PART(J)
c      ENDDO

        RETURN
 999    print *,'error lecture'
        CALL ARRET(0)
      END
