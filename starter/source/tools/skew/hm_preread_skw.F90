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
!!||    lectur                 ../starter/source/starter/lectur.F
!||====================================================================
!||    hm_preread_skw_mod   ../starter/source/tools/skew/hm_preread_skw.F90
!||--- called by ------------------------------------------------------
!||    lectur               ../starter/source/starter/lectur.F
!||====================================================================
      module hm_preread_skw_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Pre-read of the skew definitions - temporary storage in Iskew_TMP and skew_TMP
!=======================================================================================================================
!!||    lectur                 ../starter/source/starter/lectur.F
!||====================================================================
!||    hm_preread_skw         ../starter/source/tools/skew/hm_preread_skw.F90
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    anodset                ../starter/source/output/analyse/analyse_node.c
!||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_get_string          ../starter/source/devtools/hm_reader/hm_get_string.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||    usr2sys                ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    format_mod             ../starter/share/modules1/format_mod.F90
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_preread_skw(skew      ,iskn   ,x      ,itab   ,itabm1    , &
                               &  lsubmodel ,unitab ,numnod ,numskw ,check_used, &
                               &  liskn     ,lskew  ,n2d    ,siskwn ,sskew     )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! -------------------------------------------------------------------, ---------------------------------------------------
          use unitab_mod
          use submodel_mod
          use message_mod
          use hm_option_read_mod
          use names_and_titles_mod , only : nchartitle, ncharkey, ncharfield
          use format_mod , only : lfield
          use precision_mod, only : WP
          use constant_mod, only : zero, em5, em20, one, ten
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type (unit_type_),                         intent(in) :: unitab                              !< Unit table
          integer,                                   intent(in) :: numnod                              !< Number of nodes
          integer,                                   intent(in) :: numskw                              !< Number of skew systems
          integer,                                   intent(in) :: liskn                               !< Max number of nodes per skew system
          integer,                                   intent(in) :: lskew                               !< Max number of skew systems
          integer,                                   intent(in) :: sskew                               !< Sum of skews
          integer,                                   intent(in) :: siskwn                              !< Skew system index
          integer,                                   intent(in) :: itab(numnod)                        !< Global to local node number conversion table
          integer,                                   intent(in) :: itabm1(2*numnod)                    !< Local to global node number conversion table
          integer,                                   intent(in) :: check_used                          !< Flag to check if node is used
          !integer,                                   intent(in) :: nsubmod                             !< Number of submodels
          integer,                                   intent(in) :: n2d                                 !< 2D model flag
          type (submodel_data),                      intent(in) :: lsubmodel(nsubmod)                  !< Submodel data
          integer,                                   intent(inout) :: iskn(liskn,siskwn/liskn)         !< skew system connectivity  
          real(kind=WP),                             intent(inout) :: skew(lskew,sskew/lskew)          !< skew system definition
          real(kind=WP),                             intent(inout) :: x(3, numnod)                     !< Coordinates of all nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,n,imov,j,n1,n2,n3,k,sub_id, &                                               !< local entities id's
                  &  idir,iflagunit,id,uid,cpt
          real(kind=WP) :: p(12), pnor1, pnor2, pnorm1, det1, det2, det3, det, pp                      !< local real variables
          character(len=nchartitle) :: titr                                                            !< Title string
          character(len=ncharfield) :: key                                                             !< key string
          character(len=ncharfield) :: dir                                                             !< Direction string
          logical :: is_available                                                                      !< Logical flag
          integer :: usr2sys                                                                           !< External function
          character(len=40), parameter :: mess = 'MOVING skew SYSTEM DEFINITION           '           !< message string
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          det1 = huge(det1)
          det2 = huge(det2)
          det3 = huge(det3)
!
          do i=1,liskn
            iskn(i,1)=0
          enddo
!
          do i=1,lskew
            skew(i,1)=zero
          enddo
          skew(1,1)=one
          skew(5,1)=one
          skew(9,1)=one
!
!--------------------------------------------------
! START BROWSING MODEL
!--------------------------------------------------
          call hm_option_start('/SKEW')
          i = 0
!
          do cpt=1,numskw
            i = i + 1
!--------------------------------------------------
! EXTRACT DATA OF /skew/... LINE
!--------------------------------------------------
            call hm_option_read_key(lsubmodel, &
                               &    option_id = id, &
                               &    unit_id = uid, &
                               &    submodel_id = sub_id, &
                               &    option_titr = titr, &
                               &    keyword2 = key)

            imov  = 0
!
            iflagunit = 0
            do j=1,unitab%nunits
              if (unitab%unit_id(j) == uid) then
                iflagunit = 1
                exit
              endif
            enddo
            if (uid/=0 .and. iflagunit==0) then
              call ancmsg(msgid=659, &
               &          anmode=aninfo, &
               &          msgtype=msgerror, &
               &          i2=uid, &
               &          i1=id, &
               &          c1='SKEW SYSTEM', &
               &          c2='SKEW SYSTEM', &
               &          c3=titr)
            endif
!
            if (key(1:3)=='FIX') then
!--------------------------------------------------
! EXTRACT DATA (REAL VALUES)
!--------------------------------------------------
              call hm_get_floatv('globaloriginx',p(10),is_available,lsubmodel,unitab)
              call hm_get_floatv('globaloriginy',p(11),is_available,lsubmodel,unitab)
              call hm_get_floatv('globaloriginz',p(12),is_available,lsubmodel,unitab)
!
              call hm_get_floatv('globalyaxisx',p(4),is_available,lsubmodel,unitab)
              call hm_get_floatv('globalyaxisy',p(5),is_available,lsubmodel,unitab)
              call hm_get_floatv('globalyaxisz',p(6),is_available,lsubmodel,unitab)
!
              call hm_get_floatv('globalzaxisx',p(7),is_available,lsubmodel,unitab)
              call hm_get_floatv('globalzaxisy',p(8),is_available,lsubmodel,unitab)
              call hm_get_floatv('globalzaxisz',p(9),is_available,lsubmodel,unitab)
            elseif (key(1:4)=='MOV2') then
              imov = 2
!--------------------------------------------------
! EXTRACT DATA  (INTEGER VALUES)
!--------------------------------------------------
              call hm_get_intv('originnodeid',n1,is_available,lsubmodel)
              call hm_get_intv('axisnodeid',n2,is_available,lsubmodel)
              call hm_get_intv('planenodeid',n3,is_available,lsubmodel)
            else
              imov = 1
              idir = 1
!--------------------------------------------------
! EXTRACT DATA (INTEGER VALUES)
!--------------------------------------------------
              call hm_get_intv('originnodeid',n1,is_available,lsubmodel)
              call hm_get_intv('axisnodeid',n2,is_available,lsubmodel)
              call hm_get_intv('planenodeid',n3,is_available,lsubmodel)
!--------------------------------------------------
! EXTRACT DATA (STRING)
!--------------------------------------------------
              call hm_get_string('DIR',dir,ncharfield,is_available)
              do k = 1,lfield
                if (dir(k:k) == 'X'.OR.dir(k:k) == 'x') idir = 1
                if (dir(k:k) == 'Y'.OR.dir(k:k) == 'y') idir = 2
                if (dir(k:k) == 'Z'.OR.dir(k:k) == 'z') idir = 3
              enddo
              iskn(6,i+1)=idir
  !
            endif
            iskn(4,i+1)=id
!----------------
!     skew MOV2
!----------------
            if (imov == 2) then
              n1=usr2sys(n1,itabm1,mess,id)
              n2=usr2sys(n2,itabm1,mess,id)
              n3=usr2sys(n3,itabm1,mess,id)
              call anodset(n1, check_used)
              call anodset(n2, check_used)
              call anodset(n3, check_used)
              iskn(1,i+1)=n1
              iskn(2,i+1)=n2
              iskn(3,i+1)=n3
              iskn(5,i+1)=imov
              p(7)=x(1,n2)-x(1,n1)
              p(8)=x(2,n2)-x(2,n1)
              p(9)=x(3,n2)-x(3,n1)
              p(1)=x(1,n3)-x(1,n1)
              p(2)=x(2,n3)-x(2,n1)
              p(3)=x(3,n3)-x(3,n1)
!-----------------
!       calculation of: Y = Z x X'
!-----------------
              p(4)=p(8)*p(3)-p(9)*p(2)
              p(5)=p(9)*p(1)-p(7)*p(3)
              p(6)=p(7)*p(2)-p(8)*p(1)
!-----------------
!       calculation of: X = Y x Z
!-----------------
              p(1)=p(5)*p(9)-p(6)*p(8)
              p(2)=p(6)*p(7)-p(4)*p(9)
              p(3)=p(4)*p(8)-p(5)*p(7)
!-----------------
!       calculation of the origin
!-----------------
              p(10) = x(1,n1)
              p(11) = x(2,n1)
              p(12) = x(3,n1)
!----------------
!       consistency checks
!----------------
              pnor1=sqrt(p(7)*p(7)+p(8)*p(8)+p(9)*p(9))
              if (pnor1 < em20) then
                call ancmsg(msgid=162, &
                     &      msgtype=msgerror, &
                     &      anmode=aninfo_blind_1, &
                     &      i2=itab(n1), &
                     &      i1=n,c1=titr, &
                     &      i3=itab(n2))
              endif
!       calculation of collinearity of vectors n1n2 and n1n3
              pnor2=sqrt(p(1)*p(1)+p(2)*p(2)+p(3)*p(3))
              if (pnor2 > em20) then
                pnorm1=one/(pnor1*pnor2)
                det1=abs((p(8)*p(3)-p(9)*p(2))*pnorm1)
                det2=abs((p(9)*p(1)-p(7)*p(3))*pnorm1)
                det3=abs((p(7)*p(2)-p(8)*p(1))*pnorm1)
                det=max(det1,det2,det3)
              else
                det=zero
              endif
              if (det < em5) then
                call ancmsg(msgid=163, &
                     &      msgtype=msgwarning, &
                     &      anmode=aninfo_blind_1, &
                     &      i1=id,c1=titr)
                if(abs(p(2)) < em5) then
                  p(4)=abs(p(1))+ten
                else
                  p(5)=ten
                endif
              endif
!----------------
!     moving skew (initial position calculation)
!----------------
            elseif (imov==1) then
              n1=usr2sys(n1,itabm1,mess,id)
              n2=usr2sys(n2,itabm1,mess,id)
              call anodset(n1, check_used)
              call anodset(n2, check_used)
              iskn(1,i+1)=n1
              iskn(2,i+1)=n2
              iskn(5,i+1)=imov
!-----------------
!     calculation of x' and y0' (idir=1) calculation of y' and z0' (idir=2) calculation of z' and x0' (idir=3)
!-----------------
              if(n2D==0)then
                if (idir == 1) then
                  p(1)=x(1,n2)-x(1,n1)
                  p(2)=x(2,n2)-x(2,n1)
                  p(3)=x(3,n2)-x(3,n1)
                elseif (idir == 2) then
                  p(4)=x(1,n2)-x(1,n1)
                  p(5)=x(2,n2)-x(2,n1)
                  p(6)=x(3,n2)-x(3,n1)
                elseif (idir == 3) then
                  p(7)=x(1,n2)-x(1,n1)
                  p(8)=x(2,n2)-x(2,n1)
                  p(9)=x(3,n2)-x(3,n1)
                endif
!
                n3=usr2sys(n3,itabm1,mess,id)
                call anodset(n3, check_used)
                iskn(3,i+1)=n3
!
                if (idir == 1) then
                  p(4)=x(1,n3)-x(1,n1)
                  p(5)=x(2,n3)-x(2,n1)
                  p(6)=x(3,n3)-x(3,n1)
                elseif (idir == 2) then
                  p(7)=x(1,n3)-x(1,n1)
                  p(8)=x(2,n3)-x(2,n1)
                  p(9)=x(3,n3)-x(3,n1)
                elseif (idir == 3) then
                  p(1)=x(1,n3)-x(1,n1)
                  p(2)=x(2,n3)-x(2,n1)
                  p(3)=x(3,n3)-x(3,n1)
                endif
              else
                p(1)=one
                p(2)=zero
                p(3)=zero
!
                p(4)=x(1,n2)-x(1,n1)
                p(5)=x(2,n2)-x(2,n1)
                p(6)=x(3,n2)-x(3,n1)
              endif
!
              p(10) = x(1,n1)
              p(11) = x(2,n1)
              p(12) = x(3,n1)
!----------------
!     consistency checks
!----------------
              pnor1 = zero
              if (idir == 1) pnor1=sqrt(p(1)*p(1)+p(2)*p(2)+p(3)*p(3))
              if (idir == 2) pnor1=sqrt(p(4)*p(4)+p(5)*p(5)+p(6)*p(6))
              if (idir == 3) pnor1=sqrt(p(7)*p(7)+p(8)*p(8)+p(9)*p(9))
              if (pnor1<em20) then
                call ancmsg(msgid=162, &
                  &         msgtype=msgerror, &
                  &         anmode=aninfo_blind_1, &
                  &         i2=itab(n1), &
                  &         i1=id,c1=titr, &
                  &         i3=itab(n2))
              endif
!     calculation of collinearity of vectors n1n2 and n1n3
              if (idir == 1) pnor2=sqrt(p(4)*p(4)+p(5)*p(5)+p(6)*p(6))
              if (idir == 2) pnor2=sqrt(p(7)*p(7)+p(8)*p(8)+p(9)*p(9))
              if (idir == 3) pnor2=sqrt(p(1)*p(1)+p(2)*p(2)+p(3)*p(3))
              if (pnor2>em20) then
                pnorm1=one/(pnor1*pnor2)
                if (idir == 1) then
                  det1=abs((p(1)*p(5)-p(2)*p(4))*pnorm1)
                  det2=abs((p(1)*p(6)-p(3)*p(4))*pnorm1)
                  det3=abs((p(2)*p(6)-p(3)*p(5))*pnorm1)
                elseif (idir == 2) then
                  det1=abs((p(4)*p(8)-p(5)*p(7))*pnorm1)
                  det2=abs((p(4)*p(9)-p(6)*p(7))*pnorm1)
                  det3=abs((p(5)*p(9)-p(6)*p(8))*pnorm1)
                elseif (idir == 3) then
                  det1=abs((p(7)*p(2)-p(8)*p(1))*pnorm1)
                  det2=abs((p(7)*p(3)-p(9)*p(1))*pnorm1)
                  det3=abs((p(8)*p(3)-p(9)*p(2))*pnorm1)
                endif
                det= max(det1,det2,det3)
              else
                det=zero
              endif
              if (det<em5) then
                call ancmsg(msgid=163, &
                      &     msgtype=msgwarning, &
                      &     anmode=aninfo_blind_1, &
                      &     i1=id, &
                      &     c1=titr)
                if (idir == 1) then
                  if(abs(p(2))>em5) then
                    p(4)=abs(p(1))+ten
                  else
                    p(5)=ten
                  endif
                elseif (idir == 2) then
                  if (abs(p(5))>em5) then
                    p(7)=abs(p(4))+ten
                  else
                    p(8)=ten
                  endif
                elseif (idir == 3) then
                  if (abs(p(8))>em5) then
                    p(1)=abs(p(7))+ten
                  else
                    p(2)=ten
                  endif
                endif
              endif
!-----------------
!     calculation of: Z'(idir=1) X'(idir=2) Y'(idir=3)
!-----------------
              if (idir == 1) then
                p(7)=p(2)*p(6)-p(3)*p(5)
                p(8)=p(3)*p(4)-p(1)*p(6)
                p(9)=p(1)*p(5)-p(2)*p(4)
              elseif (idir == 2) then
                p(1)=p(5)*p(9)-p(6)*p(8)
                p(2)=p(6)*p(7)-p(4)*p(9)
                p(3)=p(4)*p(8)-p(5)*p(7)
              elseif (idir == 3) then
                p(4)=p(8)*p(3)-p(9)*p(2)
                p(5)=p(9)*p(1)-p(7)*p(3)
                p(6)=p(7)*p(2)-p(8)*p(1)
              endif
!-----------------
!     calculation of: Y'(idir=1) Z'(idir=2) X'(idir=3) 
!-----------------
              if (idir == 1) then
                p(4)=p(8)*p(3)-p(9)*p(2)
                p(5)=p(9)*p(1)-p(7)*p(3)
                p(6)=p(7)*p(2)-p(8)*p(1)
              elseif (idir == 2) then
                p(7)=p(2)*p(6)-p(3)*p(5)
                p(8)=p(3)*p(4)-p(1)*p(6)
                p(9)=p(1)*p(5)-p(2)*p(4)
              elseif (idir == 3) then
                p(1)=p(5)*p(9)-p(6)*p(8)
                p(2)=p(6)*p(7)-p(4)*p(9)
                p(3)=p(4)*p(8)-p(5)*p(7)
              endif
            else
!----------------
!     skew FIX
!----------------
!
              iskn(1,i+1)=0
              iskn(2,i+1)=0
              iskn(3,i+1)=0
              iskn(5,i+1)=0
!
              if (p(4)==zero.and.p(6)==zero) p(5)=SIGN(one,p(5))
              if (p(7)==zero.and.p(8)==zero) p(9)=SIGN(one,p(9))
!-----------------
!     calculation of x'
!-----------------
              p(1)=p(5)*p(9)-p(6)*p(8)
              p(2)=p(6)*p(7)-p(4)*p(9)
              p(3)=p(4)*p(8)-p(5)*p(7)
!-----------------
!     calculation of y'
!-----------------
              p(4)=p(8)*p(3)-p(9)*p(2)
              p(5)=p(9)*p(1)-p(7)*p(3)
              p(6)=p(7)*p(2)-p(8)*p(1)
            endif
!-----------
!     normalization of the vectors
!-----------
            pp=sqrt(p(1)*p(1)+p(2)*p(2)+p(3)*p(3))
            p(1)=p(1)/pp
            p(2)=p(2)/pp
            p(3)=p(3)/pp
            pp=sqrt(p(4)*p(4)+p(5)*p(5)+p(6)*p(6))
            p(4)=p(4)/pp
            p(5)=p(5)/pp
            p(6)=p(6)/pp
            pp=sqrt(p(7)*p(7)+p(8)*p(8)+p(9)*p(9))
            p(7)=p(7)/pp
            p(8)=p(8)/pp
            p(9)=p(9)/pp
!
            do k=1,12
              skew(k,i+1)=p(k)
            enddo
!
          enddo
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine hm_preread_skw
      end module hm_preread_skw_mod
