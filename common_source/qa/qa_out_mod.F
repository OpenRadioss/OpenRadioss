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
      module QA_OUT_MOD

!#if CPP_mach == CPP_p4win64 
!      USE IFPORT
!#endif

      integer doqa, lunitqa, qaid, next_index
      integer lqa_storage
      parameter (lqa_storage=5000)
      integer,parameter :: qaprint_limit_default = 10000
      integer,parameter :: qaprint_limit_maxx = 500000
      integer qaprint_limit

      type tqa_value
        integer                   itype
        character*50              title
        integer                   valuei
        double precision          valuer
      end type tqa_value

      type(tqa_value), dimension(:), allocatable :: qa_storage

      character*12 qa_format
      common /QA_BLK/ doqa, lunitqa, qaid, qa_format

      save   /QA_BLK/

      data lunitqa /91/, doqa /0/, qaid /0/

      integer emax_index

C     Array that contains the predefined list of available qakeys
C     A WARNING is displayed if user asks for a non available qakey
      INTEGER NQAKEYLIST_AVAIL
#include "qaprint_c.inc"
C     Array that contain keywords given by the QAKEY env variable
C     At least one card is one char, so we do 1039 / 2 (one char one comma...)
      CHARACTER(len=64), DIMENSION(520) :: split_qakey_env

      contains

! ----------------------------------------------------------------------
!>    @purpose 
!>       open QA extract file
!>    @ingroup utl_qa
      subroutine qaopen (step)
#include "implicit_f.inc"
      integer preci
      integer i
      logical RD_extract_file
      character *100 env, idx_str
      character(len=*) :: step

      RD_extract_file=.false.

      call getenv ( 'DO_QA', env )
      doqa = 0

      if ( env /= 'ON' ) then
        return
      endif


      allocate(qa_storage(lqa_storage))
      do i=1,lqa_storage
        qa_storage(i)%itype=0
        qa_storage(i)%title=''
        qa_storage(i)%valuer=0.0_8
        qa_storage(i)%valuei=0
      end do

!     open the file
      open ( lunitqa, file='RD-qa.extract', err=9010 )

C     In engine step we append print in the extract file, in starter we
C     create the file from scratch

      if (step(1:LEN_TRIM(step)) == "engine") then

C       We get the next index value (in the case we are in engine, we want
C       to continue the id at the following, so read the RD.extract file
C       to get the value and set qaid for next qaprint

        next_index=0
 1001   read ( lunitqa, 2001, end=3001 ) next_index
 2001   format(i10)
        goto 1001
 3001   continue

#ifdef __GFORTRAN__
        ! Only Gfortran / Intel compiler works fine
        ! -----------------------------------------
        ! Gfortran cannot write after EOF
        ! Needs to do a Backspace to place just before EOF.
        BACKSPACE lunitqa
#endif

        qaid = next_index
      endif
      qa_format = '(g16.9)'
      preci = 9

        write( qa_format, 1200 ) 7+preci, preci
 1200   format('(g',i2,'.',i1,')')

      doqa = 1


C     Get possible defined env variable QAKEY
      call qagetqakeyenv()


      return

 9010 continue

      write(*,*) 'Error opening extract file'
      doqa = 2
      RETURN
      end subroutine qaopen

! ----------------------------------------------------------------------
!>    @purpose 
!>       get and store the possible values of the QAKEY env variable
!>    @ingroup utl_qa
      subroutine qagetqakeyenv ()
#include "implicit_f.inc"
        integer QAKEY_ENV_len, QAKEY_ENV_status, QAKEY_SPECIFIC_len, QAKEY_SPECIFIC_status, i, j, k
C       length is (64 chars + 1 comma)*16 occurrence - last comma (no comma at the end) = 1039
        character(LEN=1039)  QAKEY_ENV, QAKEY_SPECIFIC
        character(64) temp_qakey
        integer QAPRINT_LIMIT_ENV_len, QAPRINT_LIMIT_ENV_status
        character(LEN=12) QAPRINT_LIMIT_ENV

C     Initializing qaprint_limit with its default value
        qaprint_limit = qaprint_limit_default
C     If the env variable QAPRINT_LIMIT is defined, we replace the limit
        QAPRINT_LIMIT_ENV = ''
        call get_environment_variable("QAPRINT_LIMIT",QAPRINT_LIMIT_ENV,QAPRINT_LIMIT_ENV_len,QAPRINT_LIMIT_ENV_status,.TRUE.)
        if (QAPRINT_LIMIT_ENV /= '') then
          read(QAPRINT_LIMIT_ENV,'(i12)') qaprint_limit
        endif

        if (qaprint_limit > qaprint_limit_maxx) then
          qaprint_limit = qaprint_limit_maxx
        endif

C       WRITE(*,'(a,i12)') "TRACE qaprint_limit is ",qaprint_limit,"END"

        QAKEY_ENV = ''
        call get_environment_variable("QAKEY",QAKEY_ENV,QAKEY_ENV_len,QAKEY_ENV_status,.TRUE.)

        temp_qakey=''
        j=1
        k=1
        do i=1,LEN_TRIM(QAKEY_ENV)+1
            if (QAKEY_ENV(i:i) == ',' .OR. i == LEN_TRIM(QAKEY_ENV)+1) then
                if (temp_qakey /= '') then

                    ! Checking if the found keyword is allowed, if not => error reading input
                    IF (is_value_in_qakeylist_avail(temp_qakey)) THEN
                      IF (.NOT. MYQAKEY(temp_qakey)) THEN
                        split_qakey_env(j)=temp_qakey
                        j=j+1
                      ENDIF
                    ELSE
                      PRINT*,"ERROR : THE QAKEY ", temp_qakey(1:LEN_TRIM(temp_qakey)), " IS NOT AVAILABLE, ABORTING"
                      close ( lunitqa )
                      STOP
                    ENDIF
                    temp_qakey=''
                endif
                k=1
            else
                temp_qakey(k:k)=QAKEY_ENV(i:i)
                k=k+1
            endif  
        end do

        call get_environment_variable("QAKEY_SPECIFIC",QAKEY_SPECIFIC,QAKEY_SPECIFIC_len,QAKEY_SPECIFIC_status,.TRUE.)

        temp_qakey=''
        k=1
        do i=1,LEN_TRIM(QAKEY_SPECIFIC)+1
            if (QAKEY_SPECIFIC(i:i) == ',' .OR. i == LEN_TRIM(QAKEY_SPECIFIC)+1) then
                if (temp_qakey /= '') then

                    ! Checking if the found keyword is allowed, if not => error reading input
                    IF (is_value_in_qakeylist_avail(temp_qakey)) THEN
                      IF (.NOT. MYQAKEY(temp_qakey)) THEN
                        split_qakey_env(j)=temp_qakey
                        j=j+1
                      ENDIF
                    ELSE
                      PRINT*,"ERROR : THE QAKEY ", temp_qakey(1:LEN_TRIM(temp_qakey)), " IS NOT AVAILABLE, ABORTING"
                      close ( lunitqa )
                      STOP
                    ENDIF
                    temp_qakey=''
                endif
                k=1
            else
                temp_qakey(k:k)=QAKEY_SPECIFIC(i:i)
                k=k+1
            endif  
        end do

C       TRACE
C         do i=1,size(split_qakey_env)
C             PRINT*,"QAKEY TRACE : ",split_qakey_env(i)
C         enddo


      RETURN
      end subroutine qagetqakeyenv


! ----------------------------------------------------------------------
!>    @purpose 
!>       replace blank char with _ for a given string
      subroutine blank2underscore(textin,textout)
#include      "implicit_f.inc"
          character(len=*), intent(in) :: textin
          character(len=LEN_TRIM(textin)), intent(out)  ::  textout
          integer i

          textout=textin

          do i=1,LEN_TRIM(textout)
              if (textout(i:i) == ' ') then
                  textout(i:i)='_';
              endif
          enddo     

      end subroutine blank2underscore

! ----------------------------------------------------------------------
!>    @purpose 
!>       print one entry to QA extract file
!>    example of call for real print
!>      CALL QAPRINT('MY_LABEL',0,12345.6_8)  (2nd argument is 0, 3rd is the real value to be printed followed with '_8')
!>    example of call for integer print
!>      CALL QAPRINT('MY_LABEL',123456,0.0_8)  (2nd argument is the integer value to be printed, 3rd must be 0.0_8)
!>    @ingroup utl_qa
      subroutine qaprint ( name, idin, value )
#include "implicit_f.inc"

      character(len=*) :: name
      character(len=LEN_TRIM(name)) :: name2
      integer idin, id
      double precision value    !(2)
      real rvalue
      integer ivalue
      character *20 srvalue
      character *6  siid
      character*4 simg
      integer i, nvals, intstart, is, namelen
      logical chk
      integer ma4cmplx, idx
      data ma4cmplx/-999/
      character *40 qaprint_limit_char
      character(len=512) :: warning_msg
      
      qaprint_limit_char = ' '

      if ( doqa /= 1  ) return

      if (qaid == qaprint_limit) then
C       We don't go over a specified limit (default or manual set or limit threshold)
        return
      endif

      qaid = qaid + 1

C     If the value (double precision) variable is 0.0 we consider using the idin (integer)
C     The problem with doing that, is that a SPECIFIC VARIABLE PRINT with a real id and a value =0.0 will result in losing the suffix
C     as it will be used as value e.g. (CALL QAPRINT('STOP_STARTER',2,0.0_8)  =>  STOP_STARTER 2    and no more     STOP_STARTER_2  0  ) !!!
      if (value == 0.0) then
C       Forcing the id to 0, we don't want to add any name suffix in this case
        id = 0
      else
        id = abs(idin)
      endif

      if ( id > 99999 ) id = 90000 + mod(id,10000)


      if ( id == 0 ) then
        intstart = 6
        siid = ' '
      else
        write (siid,'(i6.6)') id
        intstart = 1
        if ( id <= 999 ) intstart = 3
        siid(intstart:intstart) = '_'
      endif

      simg = '<i> '
      is = 4

      rvalue = value  !(i)
      if ( rvalue == 0.0 ) then
!     normalize zero to limit p4 differences between platforms
         srvalue = ' 0.00000'
      else
        write(srvalue,qa_format) value    !(i)
      endif
      
C       Replace possible blank char in name (when it comes from array print)
      call blank2underscore(name,name2)

C       We don't go over 9999 lines, this is a default limitation
      if (qaid == qaprint_limit) then

        write(qaprint_limit_char, '(i12)') qaprint_limit
        qaprint_limit_char = ADJUSTL(qaprint_limit_char)
        write ( lunitqa, 1002 ) qaid, 'QAPRINT_IS_LIMITED_TO_',qaprint_limit_char(1:LEN_TRIM(qaprint_limit_char)),
     .'_LINES_(use_variable_QAPRINT_LIMIT_in_QA.files_to_change_limit)', siid(intstart:6), simg(is:4), 0

        PRINT*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      write ( warning_msg, 1003 ) 'WARNING : The QAPRINT max number of lines to be printed in the extract file has been reached (' 
     .,qaprint_limit_char(1:LEN_TRIM(qaprint_limit_char)),' lines).'
        PRINT*,warning_msg(1:LEN_TRIM(warning_msg))
        PRINT*,'To change this limit please setenv the variable QAPRINT_LIMIT'
        PRINT*,'in the related QA.filesxxx'
        PRINT*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

      else
C       If the value (double precision) variable is 0.0 we consider using the idin (integer) as value
        if (value == 0.0) then
          write ( lunitqa, 1000 ) qaid, name2(1:LEN_TRIM(name2)), siid(intstart:6), simg(is:4), idin
        else
          write ( lunitqa, 1001 ) qaid, name2(1:LEN_TRIM(name2)), siid(intstart:6), simg(is:4), srvalue(1:lastchar(srvalue))
        endif
      endif

 1000 format( i10, 2x, a, a, a, i12 )
 1001 format( i10, 2x, a, a, a, a )
 1002 format( i10, 2x, a, a, a, a, a, i12 )
 1003 format( a, a, a )

      is = 1

      call flush(lunitqa)
      RETURN
      end subroutine qaprint

! ----------------------------------------------------------------------
!>    @purpose 
!>       print one entry to QA extract file in an energy style (<value> <value max>)
!>    @ingroup utl_qa
      subroutine qaprint2 ( name, idin, value, value2 )
#include "implicit_f.inc"

      character *(*) name
      integer idin, id
      double precision value    !(2)
      double precision value2    !(2)
      real rvalue
      real rvalue2
      character *20 srvalue
      character *20 srvalue2
      character *6  siid
      character*4 simg
      integer i, nvals, intstart, is, namelen
      logical chk
      integer ma4cmplx, idx
      data ma4cmplx/-999/

      if ( doqa /= 1  ) return

      qaid = qaid + 1
      
      id = abs(idin)
      if ( id > 99999 ) id = 90000 + mod(id,10000)

      if ( id == 0 ) then
        intstart = 6
        siid = ' '
      else
        write (siid,'(i6.6)') id
        intstart = 1
        if ( id <= 999 ) intstart = 3
        siid(intstart:intstart) = '_'
      endif

      simg = '<i> '
      is = 4

      rvalue = value  !(i)
      if ( rvalue == 0.0 ) then
!     normalize zero to limit p4 differences between platforms
         srvalue = ' 0.00000'
      else
        write(srvalue,qa_format) value    !(i)
      endif

      rvalue2 = value2  !(i)
      if ( rvalue2 == 0.0 ) then
!     normalize zero to limit p4 differences between platforms
         srvalue2 = ' 0.00000'
      else
        write(srvalue2,qa_format) value2    !(i)
      endif
      
      write ( lunitqa, 1000 ) qaid, name(1:LEN_TRIM(name)), siid(intstart:6), simg(is:4), 
     .                                      srvalue(1:lastchar(srvalue)),
     .                                      srvalue2(1:lastchar(srvalue2))
 1000 format( i10, 2x, a, a, a, a, a )
      
      is = 1

      call flush(lunitqa)
      RETURN
      end subroutine qaprint2

! ----------------------------------------------------------------------
!>    @purpose 
!>      get QA status
!>    @ingroup utl_qa 
      subroutine qastatus (istatus)
#include "implicit_f.inc"
      integer istatus

      if ( doqa == 1) then
         istatus = 1
      else
         istatus = 0
      endif
      RETURN
      end subroutine qastatus

! ----------------------------------------------------------------------
!>    @purpose 
!>      write an integer value in standard values array qa_storage
!>    @ingroup utl_qa 
      subroutine qaseti (pos,title,i)
#include "implicit_f.inc"
      integer          pos
      character(*)     title
      integer          i
      integer          nlen

      qa_storage(pos)%itype=1
      write(qa_storage(pos)%title,'(A)')title(1:LEN_TRIM(title))
      qa_storage(pos)%valuei=i
      qa_storage(pos)%itype=1
      RETURN
      end subroutine qaseti

! ----------------------------------------------------------------------
!>    @purpose 
!>      write a real value in standard values array qa_storage
!>    @ingroup utl_qa 
      subroutine qasetr (pos,title,r)
#include "implicit_f.inc"
      integer          pos
      character(*)     title
      double precision r
      integer          nlen

      qa_storage(pos)%itype=2
      write(qa_storage(pos)%title,'(A)')title(1:LEN_TRIM(title))
      qa_storage(pos)%valuer=r
      qa_storage(pos)%itype=1
      RETURN
      end subroutine qasetr

! ----------------------------------------------------------------------
!>    @purpose 
!>      close QA extract file
!>    @ingroup utl_qa 
!>    Care when routine is called from Fortran (because of binding)
      subroutine qaclose ( ) bind ( C, name="qaclose_" )
#include "implicit_f.inc"
      integer i

      if ( doqa /= 1  ) return

C     Exclusive section in case of SPM (concurrence!) only the 1st thread executes this
!$OMP SINGLE

C     Look for EMAX index in struct
      emax_index = 0
      do i=1,lqa_storage
          if (qa_storage(i)%title == "EMAX") then
              emax_index = i
              exit
          end if
      end do

! dump value qa_storage values before closing ...
      do i=1,lqa_storage
        if (qa_storage(i)%itype>0) then
          if   (qa_storage(i)%title == 'IENERGY'
     .     .OR. qa_storage(i)%title == 'KENERGYT'
     .     .OR. qa_storage(i)%title == 'KENERGYR'
     .     .OR. qa_storage(i)%title == 'EXTWORK'
     .    ) then
            call qaprint2 ( qa_storage(i)%title, qa_storage(i)%valuei, qa_storage(i)%valuer, qa_storage(emax_index)%valuer)
          else
            if (i /= emax_index) then
C             A trick to pass an integer is to use idin (2nd parameter) and valuer = 0.0
              call qaprint ( qa_storage(i)%title, qa_storage(i)%valuei, qa_storage(i)%valuer)
            end if
          end if
        end if
      end do
! dump value qa_storage values before closing end.

      close ( lunitqa )

!$OMP END SINGLE
#ifdef __INTEL_LLVM_COMPILER
!$omp barrier
#endif
      return
      RETURN
      end subroutine qaclose

! ----------------------------------------------------------------------
!>    @purpose 
!>    Check if a given value is part of the values set by env variable
!>    Useful to make a condition on a qaprint
!>    Return true or false
!>    @ingroup utl_qa 
      function myqakey ( value ) result (tf)
#include "implicit_f.inc"
      integer i
      character (len=*) :: value
      character *64 value_without_leading_blank
      logical tf

      tf = .FALSE.
      if ( doqa /= 1  ) return
      
      do i=1,size(split_qakey_env)
C         Remove leading blank if any
          value_without_leading_blank=ADJUSTL(split_qakey_env(i))
          if (value_without_leading_blank(1:LEN_TRIM(value_without_leading_blank)) == value(1:LEN_TRIM(value))) then
                tf = .TRUE.
                exit
          endif
      enddo 

      end function myqakey

! ----------------------------------------------------------------------
!>    @purpose 
!>    Check if a given value is the predefined array fo available qakeys
!>    Useful to check if a qakey is allowed/available
!>    Return true or false (false = program must exit with an error)
!>    @ingroup utl_qa 
      function is_value_in_qakeylist_avail ( value ) result (tf)
#include "implicit_f.inc"
      integer i,len1,len2
      character *64 value, value_without_leading_blank
      logical tf

      tf = .FALSE.

      do i=1,size(QAKEYLIST_AVAIL)
C         Remove leading blank if any
          value_without_leading_blank=ADJUSTL(QAKEYLIST_AVAIL(i))
          len1=LEN_TRIM(value_without_leading_blank)
          len2=LEN_TRIM(value)
          if(len1 == len2)then
            if (value_without_leading_blank(1:len1) == value(1:len2)) then
                tf = .TRUE.
                exit
            endif
          endif
      enddo 

      end function is_value_in_qakeylist_avail

! ----------------------------------------------------------------------
      integer function lastchar(a)
#include "implicit_f.inc"
      character *(*) a

      integer l,lmax

c      lmax = len(a)
c      do while ( l <= lmax .and. a(l:l) > ' ' )
c        l = l + 1
c      enddo
c      lastchar = l
      lastchar=LEN_TRIM(a)
      return
      end function lastchar

!!!!!!!!!!!!!!!!!!!!!!!!!!
!     Not used function
!!!!!!!!!!!!!!!!!!!!!!!!!!

! ----------------------------------------------------------------------
!>    @purpose 
!>       print 'QASKIP' into extract and terminate the program
!>    @note
!>       'QASKIP' in the extract file will be identified by qa_script
!>       and will be regarded as a test to be skipped by QA
!>    @ingroup utl_qa 
C       subroutine qaskip()
C #include "implicit_f.inc"      
C       integer iqa
C       call qastatus(iqa)
C       if (iqa <= 0) return
C       call qaprint('QASKIP',0, 0.0D0)
C       call qaclose()
C       stop 0
C       RETURN
C       end subroutine qaskip

! ----------------------------------------------------------------------
!>    @purpose 
!>       qa print for integer array
!>    @ingroup utl_qa
C       subroutine qaprint_ia ( name,ivar,ndim,index,ilast )
C #include "implicit_f.inc"
C       integer ndim, index, ilast
C       integer ivar(ndim)
C       character*(*) name
C       character*48 tmpmsg
C       integer nlen
C       !integer lastchar, nlen
C       !external lastchar
C #define NQAVAR 10
C #define LIQAVAR 30
C #define LRQAVAR 40
C       integer i, ifirst, iqavar(3,NQAVAR)
C       double precision rqavar(4,NQAVAR), vari
C !
C       data ifirst /1/, iqavar /LIQAVAR*0/, rqavar /LRQAVAR*0.0d0/
C !
C       if (doqa /= 1) return
C !
C       if (ifirst == 1) then
C          do i = 1,NQAVAR
C             iqavar(1,i) = 0
C             iqavar(2,i) = 0
C             iqavar(3,i) = 0
C             rqavar(1,i) = 1.0d99
C             rqavar(2,i) =-1.0d99
C             rqavar(3,i) = 0.0d0
C             rqavar(4,i) = 0.0d0
C          enddo
C          ifirst = 0
C       endif
C !
C       do i = 1,ndim
C          vari = ivar(i)
C          if (vari < rqavar(1,index)) then
C             rqavar(1,index) = vari
C             iqavar(1,index) = iqavar(3,index) + i
C          endif
C          if (vari > rqavar(2,index)) then
C             rqavar(2,index) = vari
C             iqavar(2,index) = iqavar(3,index) + i
C          endif
C          rqavar(3,index) = rqavar(3,index) + vari
C          rqavar(4,index) = rqavar(4,index) + vari**2
C       enddo
C       iqavar(3,index) = iqavar(3,index) + ndim
C !
C       nlen = lastchar(name)
C !
C       if (ilast /= 0) then
C          if (iqavar(3,index) == 0) then
C             write(tmpmsg,'(A,A)') name(1:nlen),'-ndim'
C             call qaprint(tmpmsg,0, 1.0d0*iqavar(3,index))
C          else if (iqavar(3,index) == 1) then
C             write(tmpmsg,'(A,A)') name(1:nlen),'-var'
C             call qaprint(tmpmsg,0, 1.0d0*ivar(1))
C          else
C             write(tmpmsg,'(A,A)') name(1:nlen),'-ndim'
C             call qaprint(tmpmsg,0, 1.0d0*iqavar(3,index))
C             write(tmpmsg,'(A,A)') name(1:nlen),'-minvar'
C             call qaprint(tmpmsg,0, 1.0d0*rqavar(1,index))
C             write(tmpmsg,'(A,A)') name(1:nlen),'-iminvar'
C             call qaprint(tmpmsg,0, 1.0d0*iqavar(1,index))
C             write(tmpmsg,'(A,A)') name(1:nlen),'-maxvar'
C             call qaprint(tmpmsg,0, 1.0d0*rqavar(2,index))
C             write(tmpmsg,'(A,A)') name(1:nlen),'-imaxvar'
C             call qaprint(tmpmsg,0, 1.0d0*iqavar(2,index))
C             write(tmpmsg,'(A,A)') name(1:nlen),'-sumvar'
C             call qaprint(tmpmsg,0, 1.0d0*rqavar(3,index))
C             write(tmpmsg,'(A,A)') name(1:nlen),'-normvar'
C             call qaprint(tmpmsg,0, 1.0d0*rqavar(4,index))
C          endif
C          iqavar(1,index) = 0
C          iqavar(2,index) = 0
C          iqavar(3,index) = 0
C          rqavar(1,index) = 1.0d99
C          rqavar(2,index) =-1.0d99
C          rqavar(3,index) = 0.0d0
C          rqavar(4,index) = 0.0d0
C       endif
C !
C       return
C       RETURN
C       end subroutine qaprint_ia

! ----------------------------------------------------------------------
!>    @purpose 
!>       qa print for real array
!>    @ingroup utl_qa
C       subroutine qaprint_ra ( name,rvar,ndim,index,ilast )
C #include "implicit_f.inc"
C       integer ndim, index, ilast
C       double precision rvar(ndim)
C       character*(*) name
C       character*48 tmpmsg
C       integer nlen
C       !integer lastchar, nlen
C       !external lastchar
C #define NQAVAR  10
C #define LIQAVAR 30
C #define LRQAVAR 40
C       integer i, ifirst, iqavar(3,NQAVAR)
C       double precision rqavar(4,NQAVAR), vari
C !
C       data ifirst /1/, iqavar /LIQAVAR*0/, rqavar /LRQAVAR*0.0d0/
C !
C       if (doqa /= 1) return
C !
C       if (ifirst == 1) then
C          do i = 1,NQAVAR
C             rqavar(1,i) = 1.0d99
C             rqavar(2,i) =-1.0d99
C          enddo
C          ifirst = 0
C       endif
C !
C       do i = 1,ndim
C          vari = rvar(i)
C          if (vari < rqavar(1,index)) then
C             rqavar(1,index) = vari
C             iqavar(1,index) = iqavar(3,index) + i
C          endif
C          if (vari > rqavar(2,index)) then
C             rqavar(2,index) = vari
C             iqavar(2,index) = iqavar(3,index) + i
C          endif
C          rqavar(3,index) = rqavar(3,index) + vari
C          rqavar(4,index) = rqavar(4,index) + vari**2
C       enddo
C       iqavar(3,index) = iqavar(3,index) + ndim
C !
C       nlen = lastchar(name)
C !
C       if (ilast /= 0) then
C          if (iqavar(3,index) == 0) then
C             write(tmpmsg,'(A,A)') name(1:nlen),'-ndim'
C             call qaprint(tmpmsg,0, 1.0d0*iqavar(3,index))
C          else if (iqavar(3,index) == 1) then
C             write(tmpmsg,'(A,A)') name(1:nlen),'-var'
C             call qaprint(tmpmsg,0, 1.0d0*rvar(1))
C          else
C             write(tmpmsg,'(A,A)') name(1:nlen),'-ndim'
C             call qaprint(tmpmsg,0, 1.0d0*iqavar(3,index))
C             write(tmpmsg,'(A,A)') name(1:nlen),'-minvar'
C             call qaprint(tmpmsg,0, 1.0d0*rqavar(1,index))
C             write(tmpmsg,'(A,A)') name(1:nlen),'-iminvar'
C             call qaprint(tmpmsg,0, 1.0d0*iqavar(1,index))
C             write(tmpmsg,'(A,A)') name(1:nlen),'-maxvar'
C             call qaprint(tmpmsg,0, 1.0d0*rqavar(2,index))
C             write(tmpmsg,'(A,A)') name(1:nlen),'-imaxvar'
C             call qaprint(tmpmsg,0, 1.0d0*iqavar(2,index))
C             write(tmpmsg,'(A,A)') name(1:nlen),'-sumvar'
C             call qaprint(tmpmsg,0, 1.0d0*rqavar(3,index))
C             write(tmpmsg,'(A,A)') name(1:nlen),'-normvar'
C             call qaprint(tmpmsg,0, 1.0d0*rqavar(4,index))
C          endif
C          iqavar(1,index) = 0
C          iqavar(2,index) = 0
C          iqavar(3,index) = 0
C          rqavar(1,index) = 1.0d99
C          rqavar(2,index) =-1.0d99
C          rqavar(3,index) = 0.0d0
C          rqavar(4,index) = 0.0d0
C       endif
C !
C       return
C       RETURN
C       end subroutine qaprint_ra
      
!! ----------------------------------------------------------------------
!!>    @purpose 
!!>       print a complex value in magnitude and radian to QA extract file
!!>    @ingroup utl_qa
!      subroutine qaprint_c ( name, idin, value )
!#include "implicit_f.inc"
!
!      character *(*) name
!      integer idin, id
!      double precision value(2)
!!
!      double precision  rvalue
!      character *20 srvalue
!      character *6  siid
!      character*4 simg
!      integer i, nvals, intstart, is, lastchar, namelen
!      logical chk
!
!
!      if ( doqa /= 1  ) return
!
!      qaid = qaid + 1
!      
!      id = abs(idin)
!      if ( id > 99999 ) &
!        id = 90000 + mod(id,10000)
!
!
!      if ( id == 0 ) then
!        intstart = 6
!        siid = ' '
!      else
!        write (siid,'(i6.6)') id
!        intstart = 1
!        if ( id <= 999 ) intstart = 3
!        siid(intstart:intstart) = '_'
!      endif
!
!      simg = '<M> '
!
!        rvalue = ABS(CMPLX(value(1),value(2)))
!        if ( rvalue == 0.0D0 ) then
!!     normalize zero to limit p4 differences between platforms
!           srvalue = ' 0.00000'
!        else
!           write(srvalue,qa_format) rvalue
!        endif
!        
!        write ( lunitqa, 1000 ) qaid, name, &
!          siid(intstart:6), simg, srvalue(1:lastchar(srvalue))
! 1000   format( i10, 2x, a, a, a, a )
!
!        simg = '<A> '
!
!        if ( rvalue /= 0.0D0 ) then
!           rvalue = ATAN2(value(2),value(1))
!        end if
!        
!        if ( rvalue == 0.0D0 ) then
!!     normalize zero to limit p4 differences between platforms
!           srvalue = ' 0.00000'
!        else
!           write(srvalue,qa_format) rvalue
!        endif
!        
!        write ( lunitqa, 1000 ) qaid, name, &
!          siid(intstart:6), simg, srvalue(1:lastchar(srvalue))
!
!
!      call flush(lunitqa)
!
!      RETURN
!      end subroutine qaprint_c
! ----------------------------------------------------------------------





      end module QA_OUT_MOD
