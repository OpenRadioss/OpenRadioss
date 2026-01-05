Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    int18_law151_nsv_shift_mod   ../common_source/interf/int18_law151_nsv_shift.F
!||--- called by ------------------------------------------------------
!||    inintr                       ../starter/source/interfaces/interf1/inintr.F
!||    intfop2                      ../engine/source/interfaces/interf/intfop2.F
!||    inttri                       ../engine/source/interfaces/intsort/inttri.F
!||====================================================================
      module int18_law151_nsv_shift_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief index shifting for law151
!! \details law151 has collocated numerical scheme node identifier is 1:NUMELS starting from NUMNOD+1 to NUMNOD+NUMELS
!||====================================================================
!||    int18_law151_nsv_shift   ../common_source/interf/int18_law151_nsv_shift.F
!||--- called by ------------------------------------------------------
!||    inintr                   ../starter/source/interfaces/interf1/inintr.F
!||    intfop2                  ../engine/source/interfaces/interf/intfop2.F
!||    inttri                   ../engine/source/interfaces/intsort/inttri.F
!||--- uses       -----------------------------------------------------
!||    groupdef_mod             ../common_source/modules/groupdef_mod.F
!||    intbufdef_mod            ../common_source/modules/interfaces/intbufdef_mod.F90
!||    multi_fvm_mod            ../common_source/modules/ale/multi_fvm_mod.F90
!||====================================================================
        subroutine int18_law151_nsv_shift(mode,itask,nthread,multi_fvm,ipari,intbuf_tab,npari,ninter,numnod,opt_int_id)
!$COMMENT
!       INT18_LAW151_NSV_SHIFT description
!       shift the secondary node ID
!       for /INT18 + LAW151
!
!       INT18_LAW151_NSV_SHIFT organization :
!       - at the beginning of inttri/intfop2, shift with +numnod
!       - at the end of inttri/intfop2, shift with -numnod
!$ENDCOMMENT
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use intbufdef_mod
          use multi_fvm_mod
          use groupdef_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit None
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: npari,ninter,numnod,nthread
          character(len=*), intent(in) :: mode
          integer, intent(in) :: itask
          type(intbuf_struct_),dimension(ninter), intent(inout) :: intbuf_tab
          integer, dimension(npari,*), intent(in) :: ipari
          type(multi_fvm_struct) :: multi_fvm
          integer,intent(in),optional :: opt_int_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: II,N,NN
          INTEGER :: NSN
          INTEGER :: NSNF,NSNL
          INTEGER :: INTER_ID
C-----------------------------------------------
          INTER_ID = 0
          IF(PRESENT(OPT_INT_ID))THEN
            INTER_ID = MAX(0,OPT_INT_ID)
          ENDIF
          DO NN=1,MULTI_FVM%NUMBER_INT18
            N = MULTI_FVM%INT18_LIST(NN)
            IF(INTER_ID > 0)THEN
              IF(INTER_ID /= N) CYCLE
            ENDIF
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
        end subroutine int18_law151_nsv_shift

!===============================================================================
      end module int18_law151_nsv_shift_mod
