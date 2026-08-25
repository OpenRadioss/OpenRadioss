Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    hm_read_fail_nxt         ../starter/source/materials/fail/nxt/hm_read_fail_nxt.F
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FAIL_NXT(FAIL    ,TITLE    ,MAT_ID   ,FAIL_ID  ,IRUPT     ,
     .                  NTABLE  ,TABLE    ,LSUBMODEL,UNITAB   ,FAIL_TAG )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read NXT failure model parameters
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD 
      USE ELBUFTAG_MOD
      USE MESSAGE_MOD
      use my_alloc_mod
      use table_mod
      use func_table_copy_mod
      use names_and_titles_mod , only : nchartitle
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer            ,intent(in) :: fail_id       !< failure model id
      integer            ,intent(in) :: ntable        !< number of system function tables
      integer            ,intent(in) :: mat_id        !< material law id
      integer            ,intent(in) :: irupt         !< failure model type number
      type(unit_type_)   ,intent(in) :: unitab        !< table of input units
      type(submodel_data),intent(in) :: lsubmodel(*)  !< submodel table
      type(fail_param_)  ,intent(inout) :: fail       !< failure model data structure
      type (fail_tag_)   ,intent(inout) :: fail_tag   !< failure model tag for buffer allocation
      type(ttable) ,dimension(ntable) ,intent(in)    :: table      !< input table array
      character(len=nchartitle) ,intent(in):: title   !< material model title
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
      INTEGER :: IFAIL_SH
      INTEGER :: NFUNC
      INTEGER ,DIMENSION(2) :: IFUNC
      INTEGER :: IERROR
      my_real :: PTHKF
      my_real :: x1scale,x2scale
      my_real, dimension(1) :: x2vect,yscale
      LOGICAL    ::     IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C     READ INPUT DATA
C--------------------------------------------------      
      IFAIL_SH = 0
Card1
      CALL HM_GET_INTV ('fct_ID1'  ,ifunc(1)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV ('fct_ID2'  ,ifunc(2)    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV ('Ifail_sh' ,IFAIL_SH    ,IS_AVAILABLE,LSUBMODEL)
c-----------------------------
      IF (IFAIL_SH == 1) THEN
        PTHKF = EM06
      ELSE
        PTHKF = ONE
      ENDIF
c-----------------------------------------------------
      FAIL%KEYWORD = 'NXT' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 1
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 2
      FAIL%nvartmp = 2
      FAIL%NFUNC   = 0
      FAIL%NTABLE  = 0
      FAIL%ntable4d= 2
      FAIL%NMOD    = 1
      FAIL%PTHK    = PTHKF
c            
      call my_alloc(fail%uparam ,fail%nuparam,  "FAIL%UPARAM")
      call my_alloc(fail%iparam ,fail%niparam,  "FAIL%IPARAM")
      call my_alloc(fail%ifunc  ,fail%nfunc,    "FAIL%IFUNC")
      call my_alloc(fail%table  ,fail%ntable,   "FAIL%TABLE")
      if (allocated(fail%table4d)) deallocate(fail%table4d)
      allocate(fail%table4d(fail%ntable4d))
c
      ! Modes of failure
      FAIL_TAG%LF_DAMMX = FAIL_TAG%LF_DAMMX + FAIL%NMOD
      allocate(FAIL%MODE(FAIL%NMOD))
      FAIL%MODE(1) = "Instability factor"
c------------------------------------------------------      
      FAIL%UPARAM(1) = IFAIL_SH
c-----------------------------------------------------
      ! create local NXT from input functions
      nfunc = 1
      x1scale    = one
      x2scale    = one
      x2vect(1)  = zero
      yscale(1)  = one
!
      call func_table_copy(fail%table4d(1) ,title   ,mat_id   ,      
     1           nfunc    ,ifunc(1)   ,x2vect  ,x1scale ,      
     1           x2scale  ,yscale  ,ntable  ,table   ,      
     1           ierror  )
      if (ierror /= 0) then
        call ancmsg(msgid=487,msgtype=msgerror,anmode=aninfo_blind,i1=mat_id,c1=title,i2=ifunc(1))
      end if
 !
      call func_table_copy(fail%table4d(2) ,title   ,mat_id   ,      
     1           nfunc    ,ifunc(2)   ,x2vect  ,x1scale ,      
     1           x2scale  ,yscale  ,ntable  ,table   ,      
     1           ierror  )
      if (ierror /= 0) then
        call ancmsg(msgid=487,msgtype=msgerror,anmode=aninfo_blind,i1=mat_id,c1=title,i2=ifunc(2))
      end if
!--------------------------------------------------------------------------------------------------
      if(is_encrypted)then
        write(iout, 1000)
      else    
       if (ifail_sh == 0) then   
        write(iout, 1001) ifunc(1),ifunc(2)
       else
        write(iout, 1100) ifunc(1),ifunc(2)
       endif
      endif       
C-----------
 1000 FORMAT(
     & 5X,40H    CRYPTED DATA IN FAILURE MODEL             /,
     & 5X,40H    -----------------------------             /)
 1001 FORMAT(
     & 5X,40H     NXT FAILURE  CRITERIA                     /,
     & 5X,40H    ------------------------                  /,
     & 5X,'  NO SHELL ELEMENT DELETION AFTER FAILURE' /,
     & 5X,'S-R CURVE FORMING LIMIT DIAGRAM . . . . . . . =',I10/,
     & 5X,'3D  CURVE FORMING LIMIT DIAGRAM . . . . . . . =',I10//)
 1100 FORMAT(
     & 5X,40H     NXT FAILURE  CRITERIA                     /,
     & 5X,40H    ------------------------                  /,
     & 5X,'   SHELL ELEMENT DELETION AFTER FAILURE' /,
     & 5X,'S-R CURVE FORMING LIMIT DIAGRAM . . . . . . . =',I10/,
     & 5X,'3D  CURVE FORMING LIMIT DIAGRAM . . . . . . . =',I10//)
C-----------
      RETURN
      END








