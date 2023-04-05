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
Chd|  INT18_LAW151_NSV_SHIFT        source/interfaces/int18/int18_law151_nsv_shift.F
Chd|-- called by -----------
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
        SUBROUTINE INT18_LAW151_NSV_SHIFT(MODE,ITASK,MULTI_FVM,IPARI,INTBUF_TAB)
!$COMMENT
!       INT18_LAW151_NSV_SHIFT description
!       shift the secondary node ID 
!       for /INT18 + LAW151
!       
!       INT18_LAW151_NSV_SHIFT organization :
!       - at the beginning of inttri/intfop2, shift with +numnod
!       - at the end of inttri/intfop2, shift with -numnod
!$ENDCOMMENT
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------        
        USE INTBUFDEF_MOD 
        USE MULTI_FVM_MOD
        USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        CHARACTER(len=*), INTENT(in) :: MODE
        INTEGER, INTENT(in) :: ITASK
        TYPE(INTBUF_STRUCT_),DIMENSION(NINTER), INTENT(in) :: INTBUF_TAB      
        INTEGER, DIMENSION(NPARI,*), INTENT(in) :: IPARI
        TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: II,N,NN
        INTEGER :: NSN
        INTEGER :: NSNF,NSNL
C-----------------------------------------------
        DO NN=1,MULTI_FVM%NUMBER_INT18
            N = MULTI_FVM%INT18_LIST(NN)
            NSN = IPARI(5,N)    ! number of secondary nodes
            NSNF = 1 + ITASK * NSN / NTHREAD
            NSNL = (1 + ITASK) * NSN / NTHREAD 
            !   add/substract NUMNOD to NSV for phantom nodes
            !   if NSV > 0 --> local phantom node
            !   else if NSV < 0 --> reote node
            IF(MODE=='+') THEN
                DO II = NSNF,NSNL
                    IF(INTBUF_TAB(N)%NSV(II)>0) INTBUF_TAB(N)%NSV(II) = INTBUF_TAB(N)%NSV(II) + NUMNOD                
                ENDDO
            ELSE
                DO II = NSNF,NSNL
                    IF(INTBUF_TAB(N)%NSV(II)>0) INTBUF_TAB(N)%NSV(II) = INTBUF_TAB(N)%NSV(II) - NUMNOD                
                ENDDO
            ENDIF
        ENDDO

        RETURN
        END SUBROUTINE INT18_LAW151_NSV_SHIFT
C===============================================================================
