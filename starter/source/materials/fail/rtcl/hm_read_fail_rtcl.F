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
!||    hm_read_fail_rtcl        ../starter/source/materials/fail/rtcl/hm_read_fail_rtcl.F
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
       SUBROUTINE HM_READ_FAIL_RTCL(
     .           FAIL     ,MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .           LSUBMODEL,UNITAB   )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read RTCL failure model parameters
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------+---------+---+---+------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER             ,INTENT(IN) :: FAIL_ID       ! failure model ID
      INTEGER             ,INTENT(IN) :: MAT_ID        ! material law ID
      INTEGER             ,INTENT(IN) :: IRUPT         ! failure model number
      TYPE (UNIT_TYPE_)   ,INTENT(IN) :: UNITAB        ! table of input units
      TYPE (SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)  ! submodel table 
      TYPE (FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL       ! failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER Inst
      my_real :: N,EPSCAL
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C======================================================================================
C EXTRACT CARD DATA
C======================================================================================
      CALL HM_GET_FLOATV ('MAT_EPSCAL' ,EPSCAL    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('Inst'       ,Inst      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('MAT_N'      ,N         ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C======================================================================================
C DEFAULT VALUES
C======================================================================================
      ! Steel default value
      IF (EPSCAL == ZERO) EPSCAL = 0.3D0
      ! Flag for mesh sensitivity in shells
      IF (Inst == 0) Inst = 2
C======================================================================================
c     Filling buffer tables
C======================================================================================
      FAIL%KEYWORD = 'RTCL' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 3
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 2
      FAIL%NFUNC   = 0
      FAIL%NTABLE  = 0
      FAIL%NMOD    = 0
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
c
      FAIL%UPARAM(1) = EPSCAL
      FAIL%UPARAM(2) = Inst
      FAIL%UPARAM(3) = N
C======================================================================================
c--------------------------
c     Printout data
c-------------------------- 
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1000)
        WRITE(IOUT,1100) EPSCAL,Inst,N
        WRITE(IOUT,1200)
      ENDIF
c-----------------------------------------------------
 1000 FORMAT(
     & 5X,' ----------------------------------------------------',/
     & 5X,'             FAILURE CRITERION : RTCL                ',/,
     & 5X,' ----------------------------------------------------',/)
 1100 FORMAT(
     & 5X,'CALIBRATED SIMPLE TENSION FAILURE STRAIN . . . . . .=',1PG20.13,/,
     & 5X,'                                                     ',/,
     & 5X,'FOR SHELLS ONLY :                                    ',/,
     & 5X,'FLAG FOR REGULARIZATION OF MESH SENSITIVITY. . . . .=',I10,/,
     & 5X,'     = 1 : MESH REGULARIZATION NOT ACTIVATED         ',/,
     & 5X,'     = 2 : MESH REGULARIZATION ACTIVATED             ',/, 
     & 5X,'HARDENING EXPONENT FOR REGULARIZATION. . . . . . . .=',1PG20.13,/)
 1200 FORMAT(
     & 5X,' ----------------------------------------------------',//)
c----------- 
      END
