!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      module hm_read_ebcs_cyclic_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
        subroutine hm_read_ebcs_cyclic(igrsurf, multi_fvm, id, titr, lsubmodel,  nsurf, ebcs, n2d, numnod, itab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use ebcs_mod
          use message_mod
          use multi_fvm_mod
          use groupdef_mod
          use submodel_mod
          use names_and_titles_mod , only : nchartitle, ncharkey
          use constant_mod , only : zero, one, three100
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
#include      "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: nsurf !< array sizes
          integer :: id
          type (multi_fvm_struct), intent(inout) :: multi_fvm
          type (surf_)   ,target,  dimension(nsurf)   :: igrsurf
          character(len=nchartitle), intent(in) :: titr
          type(submodel_data) :: lsubmodel(nsubmod)
          logical :: is_available,is_encrypted
          type(t_ebcs_cyclic), intent(inout) :: ebcs
          integer,intent(in) :: n2d
          integer,intent(in) :: numnod
          integer,intent(in) :: itab(numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: surf_iid1, surf_iid2 !< surfaces - internal identifiers
          integer :: surf_uid1, surf_uid2 !< surfaces - user identifiers          

          integer :: node_iid(1:6)        !< nodes - internal identifiers
          integer :: node_uid(1:6)        !< nodes - user identifiers       
          
          integer :: nseg1, nseg2         !< number of segment (surf1 and surf2
            
          integer :: surf
          integer, dimension(:), pointer :: ingr2usr
          integer, external :: ngr2usr

          INTEGER,EXTERNAL :: USR2SYS
          character(len=nchartitle) :: MESS
          DATA MESS/'CYCLIC EBCS DEFINITION                  '/
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ebcs%title = trim(titr)

          call hm_option_is_encrypted(is_encrypted)
          call hm_get_intv("entityid" , surf_uid1 , is_available,lsubmodel)
          call hm_get_intv("entityid2" , surf_uid2 , is_available,lsubmodel)
          
          call hm_get_intv("node_id1" , node_uid(1) , is_available,lsubmodel)
          call hm_get_intv("node_id2" , node_uid(2) , is_available,lsubmodel)
          call hm_get_intv("node_id3" , node_uid(3) , is_available,lsubmodel)
          call hm_get_intv("node_id4" , node_uid(4) , is_available,lsubmodel)
          call hm_get_intv("node_id5" , node_uid(5) , is_available,lsubmodel)
          call hm_get_intv("node_id6" , node_uid(6) , is_available,lsubmodel)                                                  

          if(N2D == 0)then
            if(node_uid(1) <= 0 .or. node_uid(2) <= 0 .or. node_uid(3) <= 0) then
              CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TRIM(TITR),&
                          C2="REFERENCE NODES ON SURFACE 1 MUST BE PROVIDED")
            endif   
            if(node_uid(4) <= 0 .or. node_uid(5) <= 0 .or. node_uid(6) <= 0) then
              CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TRIM(TITR),&
                          C2="REFERENCE NODES ON SURFACE 2 MUST BE PROVIDED")
            endif                      

          elseif(n2d > 0) then
            if(node_uid(1) <= 0 .or. node_uid(2) <= 0) then
              CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TRIM(TITR),&
                          C2="REFERENCE NODES ON SURFACE 1 MUST BE PROVIDED")
            endif   
            if(node_uid(4) <= 0 .or. node_uid(5) <= 0) then
              CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TRIM(TITR),&
                          C2="REFERENCE NODES ON SURFACE 2 MUST BE PROVIDED")
            endif 
            
            if(node_uid(3) /= 0 .or. node_uid(6) /= 0)then
              CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TRIM(TITR),&
                          C2="2D ANALYSIS : ONLY TWO REFERENCE NODES ARE REQUIRED. THIRD ONE IS IGNOREDl")
            endif
                       
          endif 

          ebcs%title = titr
          ebcs%has_ielem = .true.

          if(multi_fvm%is_used)then
            ebcs%is_multifluid = .true.
          end if

          ingr2usr => igrsurf(1:nsurf)%id
          surf_iid1 = 0
          surf_iid2 = 0          
          if (surf_uid1 /= 0) surf_iid1=ngr2usr(surf_uid1,ingr2usr,nsurf)
          if (surf_uid2 /= 0) surf_iid2=ngr2usr(surf_uid2,ingr2usr,nsurf)
          nseg1 = 0
          nseg2 = 0
          if (surf_iid1 /= 0) nseg1=igrsurf(surf_iid1)%nseg
          if (surf_iid2 /= 0) nseg2=igrsurf(surf_iid2)%nseg 
                   
          if(surf_uid1 == 0)then
            ierr=ierr+1
            write(istdo,"(6X,A)")" ** SURFACE 1 SHOULD BE INPUT"
            write(iout, "(6X,A)")" ** A SURFACE 1 SHOULD BE INPUT"
          else if(surf_iid1 == 0)then
            ierr=ierr+1
            write(istdo,*)" ** ERROR SURFACE 1 NOT FOUND, ID=",SURF
            write(iout,*) " ** ERROR SURFACE 1 NOT FOUND, ID=",SURF
          else if(nseg1 == 0)then
            ierr=ierr+1
            write(istdo,*)" ** ERROR EMPTY SURFACE 1",SURF
            write(iout,*) " ** ERROR EMPTY SURFACE 1",SURF
          end if
          
          if(surf_uid2 == 0)then
            ierr=ierr+1
            write(istdo,"(6X,A)")" ** SURFACE 2 MUST BE INPUT"
            write(iout, "(6X,A)")" ** A SURFACE 2 MUST BE INPUT"
          else if(surf_iid2 == 0)then
            ierr=ierr+1
            write(istdo,*)" ** ERROR SURFACE 2 NOT FOUND, ID=",SURF
            write(iout,*) " ** ERROR SURFACE 2 NOT FOUND, ID=",SURF
          else if(nseg2 == 0)then
            ierr=ierr+1
            write(istdo,*)" ** ERROR EMPTY SURFACE 2",SURF
            write(iout,*) " ** ERROR EMPTY SURFACE 2",SURF
          end if          

          if(nseg1 /= nseg2)then
              CALL ANCMSG(MSGID=1602,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TRIM(TITR),&
                          C2="NUMBER OF SEGMENT BETWEEN SURFACE 1 AND SURFACE 2 DO NOT MATCH")
          endif



          !CHECK NODES
          ! an error message will be automatically displayed if user identifier does not match an existing node.
          node_iid(1) = usr2sys(node_uid(1),itab,mess,id)
          node_iid(2) = usr2sys(node_uid(2),itab,mess,id)
          node_iid(4) = usr2sys(node_uid(4),itab,mess,id)
          node_iid(5) = usr2sys(node_uid(5),itab,mess,id)

          if(n2d == 0)then
            !third node onlyy for 3d
            node_iid(3) = usr2sys(node_uid(3),itab,mess,id)
            node_iid(6) = usr2sys(node_uid(6),itab,mess,id)
          else
            node_iid(3) = 0
            node_iid(6) = 0
          endif

          ebcs%nb_elem = 2*nseg1
          ebcs%surf_id2 = surf_iid2
          ebcs%node_id(1:6) = node_iid(1:6)

          write(iout,1001)id, trim(titr)
          
          !SURFACE-1
          write(iout,1118)surf_uid1
            if(n2d == 0)then
              write(iout,1200)node_uid(1:3)
            else
              write(iout,1300)node_uid(1:2)
            endif
          
          !SURFACE-2
          write(iout,1119)surf_uid2
            if(n2d == 0)then          
              write(iout,1200)node_uid(4:6)
            else
              write(iout,1300)node_uid(4:5)
            endif


!-----------
          return
!-----------
1001      format( //"CYCLIC EBCS NUMBER. . . . :",I8,1X,A)
1118      format(&
            "    SURFACE 1 . . . . . . . . . . . . . . . . ",I8)
1119      format(&
            "    SURFACE 2 . . . . . . . . . . . . . . . . ",I8)
1200      format( '        +REFERENCE NODES'   ,/,&
            "        |--NODE1 . . . . ",I10,/,&
            "        |--NODE2 . . . . ",I10,/,&
            "        |--NODE3 . . . . ",I10)
1300      format( '        +REFERENCE NODES'   ,/,&
            "        |--NODE1 . . . . ",I10,/,&
            "        |--NODE2 . . . . ",I10)

        end subroutine hm_read_ebcs_cyclic

      end module hm_read_ebcs_cyclic_mod
