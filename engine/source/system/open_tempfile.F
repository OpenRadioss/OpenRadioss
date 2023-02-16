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
Chd|  SCRATCHFILE_MOD               share/modules/scratchfiles_mod.F
Chd|-- called by -----------
Chd|        ARRET                         source/system/arret.F         
Chd|        EIGRREST                      source/output/restart/rdresb.F
Chd|        EIGWREST                      source/output/restart/wrrest.F
Chd|        FXBRESB                       source/output/restart/rdresb.F
Chd|        FXBWREST                      source/output/restart/wrrest.F
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE OPEN_F_SCRATCH_FILE(FD,ACCES_TYPE,ROOTN,LENROOTN,FILN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C    ACCESS TYPES - File format
C T_FORMATED : FORM=FORMATED
#define  T_FORMATED 1  
C T_DIRECT : FORM=DIRECT_120 RECL=120
#define  T_DIRECT_120   2
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: FD
        INTEGER, INTENT(IN)  :: LENROOTN
        CHARACTER(LEN=LENROOTN),INTENT(IN) :: ROOTN
        CHARACTER(LEN=10192),INTENT(OUT) :: FILN
        INTEGER,INTENT(IN) ::  ACCES_TYPE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER MY_PID,TLEN
        CHARACTER(LEN=4096) TMPDIR
        CHARACTER(LEN=20) :: CPID,CISPMD
        CHARACTER(LEN=5) :: CFD
C-----------------------------------------------
        CALL MY_GETPID(MY_PID)
        TLEN=4096

        TMPDIR='' 
        CALL TMPENVF(TMPDIR,TLEN)
        
        WRITE(CPID,'(I10.10)') MY_PID
        WRITE(CISPMD,'(I6.6)') ISPMD
        WRITE(CFD,'(I2.2)') FD
#ifdef _WIN64
        FILN=TMPDIR(1:TLEN)//'\\'//TRIM(CFD)//'_'//ROOTN(1:LENROOTN)//'_'//TRIM(CPID)//'_'//TRIM(CISPMD)//'.tmp'
#else
        FILN=TMPDIR(1:TLEN)//'/'//TRIM(CFD)//'_'//ROOTN(1:LENROOTN)//'_'//TRIM(CPID)//'_'//TRIM(CISPMD)//'.tmp'
#endif
C       Create tempo file
        IF (ACCES_TYPE == T_FORMATED ) THEN

          OPEN(UNIT=FD,FORM='FORMATTED',FILE=TRIM(FILN))  

        ELSEIF (ACCES_TYPE == T_DIRECT_120 ) THEN

          OPEN(UNIT=FD,FORM='FORMATTED',ACCESS='DIRECT',RECL=120,FILE=TRIM(FILN)) 
        ENDIF
      END


