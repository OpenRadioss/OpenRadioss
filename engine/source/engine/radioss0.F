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
Chd|  RADIOSS0                      source/engine/radioss0.F      
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        EXECARGCHECK                  source/engine/execargcheck.F  
Chd|        INIT_DYN_LIB_STRUC            share/modules/dynlib_mod.F    
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RADIOSS_SET_ENV_VARIABLE      ../common_source/comm/radioss_set_env_variable.c
Chd|        USERWINDOW_ACCESS_DUM         source/user_interface/userwindow_interface_routines.F
Chd|        DYNLIB_MOD                    share/modules/dynlib_mod.F    
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|====================================================================
      SUBROUTINE  RADIOSS0
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INOUTFILE_MOD
      USE DYNLIB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "chara_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MRDATA, MIDATA,IO_ERR,FVERS
      PARAMETER(MRDATA=3,MIDATA=9)
      INTEGER, DIMENSION(:),ALLOCATABLE :: IDATA
      my_real
     +       , DIMENSION(:),ALLOCATABLE :: RDATA
      INTEGER I
      CHARACTER FNAME*2048 
      INTEGER LFNAME
      INTEGER DUM_IERR
      INTEGER :: DYN_LIB_MODE
C-----------------------------------------------
C attention IBM/MPI ne supporte pas de REWIND sur STDIN !
C     La lecture du fichier D01 pour detection de l'option /MADYMO/ON
C     n'est pas autorisee en SPMD.
C     => A modifier si cette option doit etre supportee en SPMD ../..
      INTEGER ISTDI
      CHARACTER KEYA*80
C----------------------------------------------
C Parametres argument de run
C 
      ALLOCATE( DLIB_STRUCT(NBR_DLIB) )
      DYN_LIB_MODE = 0
      CALL INIT_DYN_LIB_STRUC(DYN_LIB_MODE)
      CALL EXECARGCHECK(GOT_INPUT, INPUT, LENINPUT,
     *                  GOT_PATH,  PATH,  LENPATH )

!     ----------------------------
!     if -infile or outfile cdl are used, then one needs to change $TMPDIR 
!     in order to write all scratch files in the user folder
      IF(INOUT_BOOL) CALL RADIOSS_SET_ENV_VARIABLE(OUTFILE_NAME , OUTFILE_NAME_LEN)
!     ---------------------------- 
C-----------------------------------------------
C     allocation minimale (en cas d'absence de couplage MADYMO).
      ALLOCATE(IDATA(MIDATA))
      ALLOCATE(RDATA(MRDATA))
      DO I=1,MIDATA
       IDATA(I)=0
      ENDDO
      DO I=1,MRDATA
       RDATA(I)=ZERO
      ENDDO
C-----------------------------------------------
      CALL RADIOSS2(IDATA, MIDATA, RDATA, MRDATA)
C-----------------------------------------------
9999  CONTINUE
      RETURN
      END 


