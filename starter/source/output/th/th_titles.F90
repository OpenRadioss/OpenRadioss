!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      module th_titles_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \th_titles define index->title correspondance
        subroutine th_titles(nvarn1        ,nvarn1a       ,nvarn2           ,nvarnpinch        ,nvars1           ,&
          nvars2        ,nvars3        ,nvars4           ,nvars5            ,nvars6          ,&
          nvars7        ,nvars8        ,nvars9           ,nvarsnloc         ,&
          nvarp         ,nvarr         ,nvart            ,nvarns            ,nvarsph          ,&
          nvarin        ,nvarrw        ,nvarrb           ,nvarmv            ,nvarse           ,&
          nvarac        ,nvarjo        ,nvarmvent        ,nvarpa            ,nvarfx           ,&
          nvargau       ,nvarfr        ,nvarslip         ,nvarret           ,nvarclus         ,&
          nvarflow      ,nvarsurf      ,nvarc            ,nvarsens          ,&
          varn1_title   ,varn1a_title  ,varn2_title      ,&
          varnpinch_title,varp_title   ,varr_title       ,vart_title        ,&
          vars1_title   ,vars2_title   ,vars3_title      ,vars4_title       ,vars5_title      ,&
          vars6_title   ,vars7_title   ,vars8_title      ,vars9_title       ,varsnloc_title   ,&
          varc_title    ,&
          varns_title   ,varsph_title  ,varin_title      ,&
          varrw_title   ,varrb_title   ,varmv_title      ,varse_title       ,varac_title      ,&
          varjo_title   ,varmvent_title,varpa_title      ,varfx_title       ,vargau_title     ,&
          varfr_title   ,varslip_title ,varret_title     ,varclus_title     ,varflow_title    ,&
          varsurf_title ,varsens_title)
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Modules
          ! ----------------------------------------------------------------------------------------------------------------------
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Implicit none
          ! ----------------------------------------------------------------------------------------------------------------------
          implicit none
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Included files
          ! ----------------------------------------------------------------------------------------------------------------------
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Arguments
          ! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in) :: nvarn1
          integer,                                   intent(in) :: nvarn1a
          integer,                                   intent(in) :: nvarn2
          integer,                                   intent(in) :: nvarnpinch
          integer,                                   intent(in) :: nvars1
          integer,                                   intent(in) :: nvars2
          integer,                                   intent(in) :: nvars3
          integer,                                   intent(in) :: nvars4
          integer,                                   intent(in) :: nvars5
          integer,                                   intent(in) :: nvars6
          integer,                                   intent(in) :: nvars7
          integer,                                   intent(in) :: nvars8
          integer,                                   intent(in) :: nvars9
          integer,                                   intent(in) :: nvarsnloc
          integer,                                   intent(in) :: nvarp
          integer,                                   intent(in) :: nvarr
          integer,                                   intent(in) :: nvart
          integer,                                   intent(in) :: nvarns
          integer,                                   intent(in) :: nvarsph
          integer,                                   intent(in) :: nvarin
          integer,                                   intent(in) :: nvarrw
          integer,                                   intent(in) :: nvarrb
          integer,                                   intent(in) :: nvarmv
          integer,                                   intent(in) :: nvarse
          integer,                                   intent(in) :: nvarac
          integer,                                   intent(in) :: nvarjo
          integer,                                   intent(in) :: nvarmvent
          integer,                                   intent(in) :: nvarpa
          integer,                                   intent(in) :: nvarfx
          integer,                                   intent(in) :: nvargau
          integer,                                   intent(in) :: nvarfr
          integer,                                   intent(in) :: nvarslip
          integer,                                   intent(in) :: nvarret
          integer,                                   intent(in) :: nvarclus
          integer,                                   intent(in) :: nvarflow
          integer,                                   intent(in) :: nvarsurf
          integer,                                   intent(in) :: nvarc
          integer,                                   intent(in) :: nvarsens

          character(len=100),                        intent(out) :: varn1_title(nvarn1)
          character(len=100),                        intent(out) :: varn1a_title(nvarn1a)
          character(len=100),                        intent(out) :: varn2_title(nvarn2)
          character(len=100),                        intent(out) :: varnpinch_title(nvarnpinch)
          character(len=100),                        intent(out) :: varp_title(nvarp)
          character(len=100),                        intent(out) :: varr_title(nvarr)
          character(len=100),                        intent(out) :: vart_title(nvart)
          character(len=100),                        intent(out) :: vars1_title(nvars1)
          character(len=100),                        intent(out) :: vars2_title(nvars2)
          character(len=100),                        intent(out) :: vars3_title(nvars3)
          character(len=100),                        intent(out) :: vars4_title(nvars4)
          character(len=100),                        intent(out) :: vars5_title(nvars5)
          character(len=100),                        intent(out) :: vars6_title(nvars6)
          character(len=100),                        intent(out) :: vars7_title(nvars7)
          character(len=100),                        intent(out) :: vars8_title(nvars8)
          character(len=100),                        intent(out) :: vars9_title(nvars9)
          character(len=100),                        intent(out) :: varsnloc_title(nvarsnloc)
          character(len=100),                        intent(out) :: varc_title(nvarc)
          character(len=100),                        intent(out) :: varns_title(nvarns)
          character(len=100),                        intent(out) :: varsph_title(nvarsph)
          character(len=100),                        intent(out) :: varin_title(nvarin)
          character(len=100),                        intent(out) :: varrw_title(nvarrw)
          character(len=100),                        intent(out) :: varrb_title(nvarrb)
          character(len=100),                        intent(out) :: varmv_title(nvarmv)
          character(len=100),                        intent(out) :: varse_title(nvarse)
          character(len=100),                        intent(out) :: varac_title(nvarac)
          character(len=100),                        intent(out) :: varjo_title(nvarjo)
          character(len=100),                        intent(out) :: varmvent_title(nvarmvent)
          character(len=100),                        intent(out) :: varpa_title(nvarpa)
          character(len=100),                        intent(out) :: varfx_title(nvarfx)
          character(len=100),                        intent(out) :: vargau_title(nvargau)
          character(len=100),                        intent(out) :: varfr_title(nvarfr)
          character(len=100),                        intent(out) :: varslip_title(nvarslip)
          character(len=100),                        intent(out) :: varret_title(nvarret)
          character(len=100),                        intent(out) :: varclus_title(nvarclus)
          character(len=100),                        intent(out) :: varflow_title(nvarflow)
          character(len=100),                        intent(out) :: varsurf_title(nvarsurf)
          character(len=100),                        intent(out) :: varsens_title(nvarsens)
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   local variables
          ! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,l,ii,jj,kk
          character :: chi*3,chj*3
          character :: chk*1
          character :: chjj*3,chii*2,chjkk*4,chjs*1,chkk*3
          character :: var_tmp*10
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   External functions
          ! ----------------------------------------------------------------------------------------------------------------------
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Body
          ! ----------------------------------------------------------------------------------------------------------------------
          ! nodal
          varn1_title = (/&
            character(len=100) ::&
            'X-DISPLACEMENT',&
            'Y-DISPLACEMENT',&
            'Z-DISPLACEMENT',&
            'X-VELOCITY',&
            'Y-VELOCITY',&
            'Z-VELOCITY',&
            'X-ACCELERATION',&
            'Y-ACCELERATION',&
            'Z-ACCELERATION',&
            'X-ROT VELOCITY',&
            'Y-ROT VELOCITY',&
            'Z-ROT VELOCITY',&
            'X-ROT ACCELERATION',&
            'Y-ROT ACCELERATION',&
            'Z-ROT ACCELERATION',&
            'X-COORDINATE',&
            'Y-COORDINATE',&
            'Z-COORDINATE',&
            'TEMPERATURE'/)

          do i=1,200
            if (i <= 9) then
              write(chi,'(a2,i1.1)')'00',i
            else if (i <= 99) then
              write(chi,'(a1,i2.2)')'0',i
            else
              write(chi,'(i3.3)')i
            endif
            varn1a_title(3*(i-1)+1)  = 'INTERPLY '//chi//' RELATIVE DISPLACEMENT IN X DIRECTION'
            varn1a_title(3*(i-1)+2)  = 'INTERPLY '//chi//' RELATIVE DISPLACEMENT IN Y DIRECTION'
            varn1a_title(3*(i-1)+3)  = 'INTERPLY '//chi//' RELATIVE DISPLACEMENT IN Z DIRECTION'
          enddo

          varn2_title = (/&
            character(len=100) ::&
            'X REACTION IMPULSE ON NODE',&
            'Y REACTION IMPULSE ON NODE',&
            'Z REACTION IMPULSE ON NODE',&
            'XX REACTION IMPULSE ON NODE',&
            'YY REACTION IMPULSE ON NODE',&
            'ZZ REACTION IMPULSE ON NODE',&
            'X-ROT DISPLACEMENT',&
            'Y-ROT DISPLACEMENT',&
            'Z-ROT DISPLACEMENT'/)

          varnpinch_title = (/&
            character(len=100) ::&
            'X PINCHING ACCELERATION',&
            'Y PINCHING ACCELERATION',&
            'Z PINCHING ACCELERATION',&
            'X PINCHING VELOCITY',&
            'Y PINCHING VELOCITY',&
            'Z PINCHING VELOCITY',&
            'X PINCHING DISPLACEMENT',&
            'Y PINCHING DISPLACEMENT',&
            'Z PINCHING DISPLACEMENT'/)

! brick
          vars1_title = (/&
            character(len=100) ::&
            'ELEMENT FLAG',&
            'SIGMA-X',&
            'SIGMA-Y',&
            'SIGMA-Z',&
            'SIGMA-XY',&
            'SIGMA-YZ',&
            'SIGMA-XZ',&
            'INTERNAL ENERGY',&
            'DENSITY',&
            'BULK VISCOSITY',&
            'VOLUME',&
            'PLASTIC STRAIN',&
            'TEMPERATURE',&
            'STRAIN RATE',&
            'TENS. DAMAGE DIR 1',&
            'TENS. DAMAGE DIR 2',&
            'TENS. DAMAGE DIR 3',&
            'TSAI WU YIELD FUNC.           ',&
            'SUM OF DAMAGES',&
            'STRESS REINF. DIR-1',&
            'STRESS REINF. DIR-2',&
            'STRESS REINF. DIR-3',&
            'VOLUME OF OPEN CRACKS',&
            'CAP PARAMETER',&
            'PLASTIC PARAMETER',&
            'TURBULENT ENERGY',&
            'TURBULENT DISSIPATION',&
            'FIBER STRAIN',&
            'PHASE STATE',&
            'EQ. VOL. PLASTIC STRAIN',&
            'BURN FRACTION',&
            'PLASTIC WORK',&
            'STRESS IN FIBER',&
            'TENSILE DAMAGE IN DIR 23',&
            'AVERAGE LOCAL SIGMA-X',&
            'AVERAGE LOCAL SIGMA-Y',&
            'AVERAGE LOCAL SIGMA-Z',&
            'AVERAGE LOCAL SIGMA-XY',&
            'AVERAGE LOCAL SIGMA-YZ',&
            'AVERAGE LOCAL SIGMA-XZ',&
            'SIGMA-X 1ST GAUSS POINT',&
            'SIGMA-X 2ND GAUSS POINT',&
            'SIGMA-X 3RD GAUSS POINT',&
            'SIGMA-X 4TH GAUSS POINT',&
            'SIGMA-X 5TH GAUSS POINT',&
            'SIGMA-X 6TH GAUSS POINT',&
            'SIGMA-X 7TH GAUSS POINT',&
            'SIGMA-X 8TH GAUSS POINT',&
            'SIGMA-Y 1ST GAUSS POINT',&
            'SIGMA-Y 2ND GAUSS POINT',&
            'SIGMA-Y 3RD GAUSS POINT',&
            'SIGMA-Y 4TH GAUSS POINT',&
            'SIGMA-Y 5TH GAUSS POINT',&
            'SIGMA-Y 6TH GAUSS POINT',&
            'SIGMA-Y 7TH GAUSS POINT',&
            'SIGMA-Y 8TH GAUSS POINT',&
            'SIGMA-Z 1ST GAUSS POINT',&
            'SIGMA-Z 2ND GAUSS POINT',&
            'SIGMA-Z 3RD GAUSS POINT',&
            'SIGMA-Z 4TH GAUSS POINT',&
            'SIGMA-Z 5TH GAUSS POINT',&
            'SIGMA-Z 6TH GAUSS POINT',&
            'SIGMA-Z 7TH GAUSS POINT',&
            'SIGMA-Z 8TH GAUSS POINT',&
            'SIGMA-XY 1ST GAUSS POINT',&
            'SIGMA-XY 2ND GAUSS POINT',&
            'SIGMA-XY 3RD GAUSS POINT',&
            'SIGMA-XY 4TH GAUSS POINT',&
            'SIGMA-XY 5TH GAUSS POINT',&
            'SIGMA-XY 6TH GAUSS POINT',&
            'SIGMA-XY 7TH GAUSS POINT',&
            'SIGMA-XY 8TH GAUSS POINT',&
            'SIGMA-YZ 1ST GAUSS POINT',&
            'SIGMA-YZ 2ND GAUSS POINT',&
            'SIGMA-YZ 3RD GAUSS POINT',&
            'SIGMA-YZ 4TH GAUSS POINT',&
            'SIGMA-YZ 5TH GAUSS POINT',&
            'SIGMA-YZ 6TH GAUSS POINT',&
            'SIGMA-YZ 7TH GAUSS POINT',&
            'SIGMA-YZ 8TH GAUSS POINT',&
            'SIGMA-XZ 1ST GAUSS POINT',&
            'SIGMA-XZ 2ND GAUSS POINT',&
            'SIGMA-XZ 3RD GAUSS POINT',&
            'SIGMA-XZ 4TH GAUSS POINT',&
            'SIGMA-XZ 5TH GAUSS POINT',&
            'SIGMA-XZ 6TH GAUSS POINT',&
            'SIGMA-XZ 7TH GAUSS POINT',&
            'SIGMA-XZ 8TH GAUSS POINT',&
            'SIGMA-X-LOCAL 1ST GAUSS POINT',&
            'SIGMA-X-LOCAL 2ND GAUSS POINT',&
            'SIGMA-X-LOCAL 3RD GAUSS POINT',&
            'SIGMA-X-LOCAL 4TH GAUSS POINT',&
            'SIGMA-X-LOCAL 5TH GAUSS POINT',&
            'SIGMA-X-LOCAL 6TH GAUSS POINT',&
            'SIGMA-X-LOCAL 7TH GAUSS POINT',&
            'SIGMA-X-LOCAL 8TH GAUSS POINT',&
            'SIGMA-Y-LOCAL 1ST GAUSS POINT',&
            'SIGMA-Y-LOCAL 2ND GAUSS POINT',&
            'SIGMA-Y-LOCAL 3RD GAUSS POINT',&
            'SIGMA-Y-LOCAL 4TH GAUSS POINT',&
            'SIGMA-Y-LOCAL 5TH GAUSS POINT',&
            'SIGMA-Y-LOCAL 6TH GAUSS POINT',&
            'SIGMA-Y-LOCAL 7TH GAUSS POINT',&
            'SIGMA-Y-LOCAL 8TH GAUSS POINT',&
            'SIGMA-Z-LOCAL 1ST GAUSS POINT',&
            'SIGMA-Z-LOCAL 2ND GAUSS POINT',&
            'SIGMA-Z-LOCAL 3RD GAUSS POINT',&
            'SIGMA-Z-LOCAL 4TH GAUSS POINT',&
            'SIGMA-Z-LOCAL 5TH GAUSS POINT',&
            'SIGMA-Z-LOCAL 6TH GAUSS POINT',&
            'SIGMA-Z-LOCAL 7TH GAUSS POINT',&
            'SIGMA-Z-LOCAL 8TH GAUSS POINT',&
            'SIGMA-XY-LOCAL 1ST GAUSS POINT',&
            'SIGMA-XY-LOCAL 2ND GAUSS POINT',&
            'SIGMA-XY-LOCAL 3RD GAUSS POINT',&
            'SIGMA-XY-LOCAL 4TH GAUSS POINT',&
            'SIGMA-XY-LOCAL 5TH GAUSS POINT',&
            'SIGMA-XY-LOCAL 6TH GAUSS POINT',&
            'SIGMA-XY-LOCAL 7TH GAUSS POINT',&
            'SIGMA-XY-LOCAL 8TH GAUSS POINT',&
            'SIGMA-YZ-LOCAL 1ST GAUSS POINT',&
            'SIGMA-YZ-LOCAL 2ND GAUSS POINT',&
            'SIGMA-YZ-LOCAL 3RD GAUSS POINT',&
            'SIGMA-YZ-LOCAL 4TH GAUSS POINT',&
            'SIGMA-YZ-LOCAL 5TH GAUSS POINT',&
            'SIGMA-YZ-LOCAL 6TH GAUSS POINT',&
            'SIGMA-YZ-LOCAL 7TH GAUSS POINT',&
            'SIGMA-YZ-LOCAL 8TH GAUSS POINT',&
            'SIGMA-XZ-LOCAL 1ST GAUSS POINT',&
            'SIGMA-XZ-LOCAL 2ND GAUSS POINT',&
            'SIGMA-XZ-LOCAL 3RD GAUSS POINT',&
            'SIGMA-XZ-LOCAL 4TH GAUSS POINT',&
            'SIGMA-XZ-LOCAL 5TH GAUSS POINT',&
            'SIGMA-XZ-LOCAL 6TH GAUSS POINT',&
            'SIGMA-XZ-LOCAL 7TH GAUSS POINT',&
            'SIGMA-XZ-LOCAL 8TH GAUSS POINT',&
            'USER VARIABLE 1',&
            'USER VARIABLE 2',&
            'USER VARIABLE 3',&
            'USER VARIABLE 4',&
            'USER VARIABLE 5',&
            'USER VARIABLE 6',&
            'USER VARIABLE 7',&
            'USER VARIABLE 8',&
            'USER VARIABLE 9',&
            'USER VARIABLE 10',&
            'USER VARIABLE 11',&
            'USER VARIABLE 12',&
            'USER VARIABLE 13',&
            'USER VARIABLE 14',&
            'USER VARIABLE 15',&
            'USER VARIABLE 16',&
            'USER VARIABLE 17',&
            'USER VARIABLE 18',&
            'USER VARIABLE 19',&
            'USER VARIABLE 20',&
            'USER VARIABLE 21',&
            'USER VARIABLE 22',&
            'USER VARIABLE 23',&
            'USER VARIABLE 24',&
            'USER VARIABLE 25',&
            'USER VARIABLE 26',&
            'USER VARIABLE 27',&
            'USER VARIABLE 28',&
            'USER VARIABLE 29',&
            'USER VARIABLE 30',&
            'USER VARIABLE 31',&
            'USER VARIABLE 32',&
            'USER VARIABLE 33',&
            'USER VARIABLE 34',&
            'USER VARIABLE 35',&
            'USER VARIABLE 36',&
            'USER VARIABLE 37',&
            'USER VARIABLE 38',&
            'USER VARIABLE 39',&
            'USER VARIABLE 40',&
            'USER VARIABLE 41',&
            'USER VARIABLE 42',&
            'USER VARIABLE 43',&
            'USER VARIABLE 44',&
            'USER VARIABLE 45',&
            'USER VARIABLE 46',&
            'USER VARIABLE 47',&
            'USER VARIABLE 48',&
            'USER VARIABLE 49',&
            'USER VARIABLE 50',&
            'USER VARIABLE 51',&
            'USER VARIABLE 52',&
            'USER VARIABLE 53',&
            'USER VARIABLE 54',&
            'USER VARIABLE 55',&
            'USER VARIABLE 56',&
            'USER VARIABLE 57',&
            'USER VARIABLE 58',&
            'USER VARIABLE 59',&
            'USER VARIABLE 60'/)

          vars2_title = (/&
            character(len=100) ::&
            'SIGMA-X 111 GAUSS POINT',&
            'SIGMA-Y 111 GAUSS POINT',&
            'SIGMA-Z 111 GAUSS POINT',&
            'SIGMA-XY 111 GAUSS POINT',&
            'SIGMA-YZ 111 GAUSS POINT',&
            'SIGMA-XZ 111 GAUSS POINT',&
            'PLASTIC STRAIN 111 GAUSS POINT',&
            'SIGMA-X 211 GAUSS POINT',&
            'SIGMA-Y 211 GAUSS POINT',&
            'SIGMA-Z 211 GAUSS POINT',&
            'SIGMA-XY 211 GAUSS POINT',&
            'SIGMA-YZ 211 GAUSS POINT',&
            'SIGMA-XZ 211 GAUSS POINT',&
            'PLASTIC STRAIN 211 GAUSS POINT',&
            'SIGMA-X 311 GAUSS POINT',&
            'SIGMA-Y 311 GAUSS POINT',&
            'SIGMA-Z 311 GAUSS POINT',&
            'SIGMA-XY 311 GAUSS POINT',&
            'SIGMA-YZ 311 GAUSS POINT',&
            'SIGMA-XZ 311 GAUSS POINT',&
            'PLASTIC STRAIN 311 GAUSS POINT',&
            'SIGMA-X 121 GAUSS POINT',&
            'SIGMA-Y 121 GAUSS POINT',&
            'SIGMA-Z 121 GAUSS POINT',&
            'SIGMA-XY 121 GAUSS POINT',&
            'SIGMA-YZ 121 GAUSS POINT',&
            'SIGMA-XZ 121 GAUSS POINT',&
            'PLASTIC STRAIN 121 GAUSS POINT',&
            'SIGMA-X 221 GAUSS POINT',&
            'SIGMA-Y 221 GAUSS POINT',&
            'SIGMA-Z 221 GAUSS POINT',&
            'SIGMA-XY 221 GAUSS POINT',&
            'SIGMA-YZ 221 GAUSS POINT',&
            'SIGMA-XZ 221 GAUSS POINT',&
            'PLASTIC STRAIN 221 GAUSS POINT',&
            'SIGMA-X 321 GAUSS POINT',&
            'SIGMA-Y 321 GAUSS POINT',&
            'SIGMA-Z 321 GAUSS POINT',&
            'SIGMA-XY 321 GAUSS POINT',&
            'SIGMA-YZ 321 GAUSS POINT',&
            'SIGMA-XZ 321 GAUSS POINT',&
            'PLASTIC STRAIN 321 GAUSS POINT',&
            'SIGMA-X 131 GAUSS POINT',&
            'SIGMA-Y 131 GAUSS POINT',&
            'SIGMA-Z 131 GAUSS POINT',&
            'SIGMA-XY 131 GAUSS POINT',&
            'SIGMA-YZ 131 GAUSS POINT',&
            'SIGMA-XZ 131 GAUSS POINT',&
            'PLASTIC STRAIN 131 GAUSS POINT',&
            'SIGMA-X 231 GAUSS POINT',&
            'SIGMA-Y 231 GAUSS POINT',&
            'SIGMA-Z 231 GAUSS POINT',&
            'SIGMA-XY 231 GAUSS POINT',&
            'SIGMA-YZ 231 GAUSS POINT',&
            'SIGMA-XZ 231 GAUSS POINT',&
            'PLASTIC STRAIN 231 GAUSS POINT',&
            'SIGMA-X 331 GAUSS POINT',&
            'SIGMA-Y 331 GAUSS POINT',&
            'SIGMA-Z 331 GAUSS POINT',&
            'SIGMA-XY 331 GAUSS POINT',&
            'SIGMA-YZ 331 GAUSS POINT',&
            'SIGMA-XZ 331 GAUSS POINT',&
            'PLASTIC STRAIN 331 GAUSS POINT',&
            'SIGMA-X 141 GAUSS POINT',&
            'SIGMA-Y 141 GAUSS POINT',&
            'SIGMA-Z 141 GAUSS POINT',&
            'SIGMA-XY 141 GAUSS POINT',&
            'SIGMA-YZ 141 GAUSS POINT',&
            'SIGMA-XZ 141 GAUSS POINT',&
            'PLASTIC STRAIN 141 GAUSS POINT',&
            'SIGMA-X 241 GAUSS POINT',&
            'SIGMA-Y 241 GAUSS POINT',&
            'SIGMA-Z 241 GAUSS POINT',&
            'SIGMA-XY 241 GAUSS POINT',&
            'SIGMA-YZ 241 GAUSS POINT',&
            'SIGMA-XZ 241 GAUSS POINT',&
            'PLASTIC STRAIN 241 GAUSS POINT',&
            'SIGMA-X 341 GAUSS POINT',&
            'SIGMA-Y 341 GAUSS POINT',&
            'SIGMA-Z 341 GAUSS POINT',&
            'SIGMA-XY 341 GAUSS POINT',&
            'SIGMA-YZ 341 GAUSS POINT',&
            'SIGMA-XZ 341 GAUSS POINT',&
            'PLASTIC STRAIN 341 GAUSS POINT',&
            'SIGMA-X 151 GAUSS POINT',&
            'SIGMA-Y 151 GAUSS POINT',&
            'SIGMA-Z 151 GAUSS POINT',&
            'SIGMA-XY 151 GAUSS POINT',&
            'SIGMA-YZ 151 GAUSS POINT',&
            'SIGMA-XZ 151 GAUSS POINT',&
            'PLASTIC STRAIN 151 GAUSS POINT',&
            'SIGMA-X 251 GAUSS POINT',&
            'SIGMA-Y 251 GAUSS POINT',&
            'SIGMA-Z 251 GAUSS POINT',&
            'SIGMA-XY 251 GAUSS POINT',&
            'SIGMA-YZ 251 GAUSS POINT',&
            'SIGMA-XZ 251 GAUSS POINT',&
            'PLASTIC STRAIN 251 GAUSS POINT',&
            'SIGMA-X 351 GAUSS POINT',&
            'SIGMA-Y 351 GAUSS POINT',&
            'SIGMA-Z 351 GAUSS POINT',&
            'SIGMA-XY 351 GAUSS POINT',&
            'SIGMA-YZ 351 GAUSS POINT',&
            'SIGMA-XZ 351 GAUSS POINT',&
            'PLASTIC STRAIN 351 GAUSS POINT',&
            'SIGMA-X 161 GAUSS POINT',&
            'SIGMA-Y 161 GAUSS POINT',&
            'SIGMA-Z 161 GAUSS POINT',&
            'SIGMA-XY 161 GAUSS POINT',&
            'SIGMA-YZ 161 GAUSS POINT',&
            'SIGMA-XZ 161 GAUSS POINT',&
            'PLASTIC STRAIN 161 GAUSS POINT',&
            'SIGMA-X 261 GAUSS POINT',&
            'SIGMA-Y 261 GAUSS POINT',&
            'SIGMA-Z 261 GAUSS POINT',&
            'SIGMA-XY 261 GAUSS POINT',&
            'SIGMA-YZ 261 GAUSS POINT',&
            'SIGMA-XZ 261 GAUSS POINT',&
            'PLASTIC STRAIN 261 GAUSS POINT',&
            'SIGMA-X 361 GAUSS POINT',&
            'SIGMA-Y 361 GAUSS POINT',&
            'SIGMA-Z 361 GAUSS POINT',&
            'SIGMA-XY 361 GAUSS POINT',&
            'SIGMA-YZ 361 GAUSS POINT',&
            'SIGMA-XZ 361 GAUSS POINT',&
            'PLASTIC STRAIN 361 GAUSS POINT',&
            'SIGMA-X 171 GAUSS POINT',&
            'SIGMA-Y 171 GAUSS POINT',&
            'SIGMA-Z 171 GAUSS POINT',&
            'SIGMA-XY 171 GAUSS POINT',&
            'SIGMA-YZ 171 GAUSS POINT',&
            'SIGMA-XZ 171 GAUSS POINT',&
            'PLASTIC STRAIN 171 GAUSS POINT',&
            'SIGMA-X 271 GAUSS POINT',&
            'SIGMA-Y 271 GAUSS POINT',&
            'SIGMA-Z 271 GAUSS POINT',&
            'SIGMA-XY 271 GAUSS POINT',&
            'SIGMA-YZ 271 GAUSS POINT',&
            'SIGMA-XZ 271 GAUSS POINT',&
            'PLASTIC STRAIN 271 GAUSS POINT',&
            'SIGMA-X 371 GAUSS POINT',&
            'SIGMA-Y 371 GAUSS POINT',&
            'SIGMA-Z 371 GAUSS POINT',&
            'SIGMA-XY 371 GAUSS POINT',&
            'SIGMA-YZ 371 GAUSS POINT',&
            'SIGMA-XZ 371 GAUSS POINT',&
            'PLASTIC STRAIN 371 GAUSS POINT',&
            'SIGMA-X 181 GAUSS POINT',&
            'SIGMA-Y 181 GAUSS POINT',&
            'SIGMA-Z 181 GAUSS POINT',&
            'SIGMA-XY 181 GAUSS POINT',&
            'SIGMA-YZ 181 GAUSS POINT',&
            'SIGMA-XZ 181 GAUSS POINT',&
            'PLASTIC STRAIN 181 GAUSS POINT',&
            'SIGMA-X 281 GAUSS POINT',&
            'SIGMA-Y 281 GAUSS POINT',&
            'SIGMA-Z 281 GAUSS POINT',&
            'SIGMA-XY 281 GAUSS POINT',&
            'SIGMA-YZ 281 GAUSS POINT',&
            'SIGMA-XZ 281 GAUSS POINT',&
            'PLASTIC STRAIN 281 GAUSS POINT',&
            'SIGMA-X 381 GAUSS POINT',&
            'SIGMA-Y 381 GAUSS POINT',&
            'SIGMA-Z 381 GAUSS POINT',&
            'SIGMA-XY 381 GAUSS POINT',&
            'SIGMA-YZ 381 GAUSS POINT',&
            'SIGMA-XZ 381 GAUSS POINT',&
            'PLASTIC STRAIN 381 GAUSS POINT',&
            'SIGMA-X 191 GAUSS POINT',&
            'SIGMA-Y 191 GAUSS POINT',&
            'SIGMA-Z 191 GAUSS POINT',&
            'SIGMA-XY 191 GAUSS POINT',&
            'SIGMA-YZ 191 GAUSS POINT',&
            'SIGMA-XZ 191 GAUSS POINT',&
            'PLASTIC STRAIN 191 GAUSS POINT',&
            'SIGMA-X 291 GAUSS POINT',&
            'SIGMA-Y 291 GAUSS POINT',&
            'SIGMA-Z 291 GAUSS POINT',&
            'SIGMA-XY 291 GAUSS POINT',&
            'SIGMA-YZ 291 GAUSS POINT',&
            'SIGMA-XZ 291 GAUSS POINT',&
            'PLASTIC STRAIN 291 GAUSS POINT',&
            'SIGMA-X 391 GAUSS POINT',&
            'SIGMA-Y 391 GAUSS POINT',&
            'SIGMA-Z 391 GAUSS POINT',&
            'SIGMA-XY 391 GAUSS POINT',&
            'SIGMA-YZ 391 GAUSS POINT',&
            'SIGMA-XZ 391 GAUSS POINT',&
            'PLASTIC STRAIN 391 GAUSS POINT',&
            'SIGMA-X 112 GAUSS POINT',&
            'SIGMA-Y 112 GAUSS POINT',&
            'SIGMA-Z 112 GAUSS POINT',&
            'SIGMA-XY 112 GAUSS POINT',&
            'SIGMA-YZ 112 GAUSS POINT',&
            'SIGMA-XZ 112 GAUSS POINT',&
            'PLASTIC STRAIN 112 GAUSS POINT',&
            'SIGMA-X 212 GAUSS POINT',&
            'SIGMA-Y 212 GAUSS POINT',&
            'SIGMA-Z 212 GAUSS POINT',&
            'SIGMA-XY 212 GAUSS POINT',&
            'SIGMA-YZ 212 GAUSS POINT',&
            'SIGMA-XZ 212 GAUSS POINT',&
            'PLASTIC STRAIN 212 GAUSS POINT',&
            'SIGMA-X 312 GAUSS POINT',&
            'SIGMA-Y 312 GAUSS POINT',&
            'SIGMA-Z 312 GAUSS POINT',&
            'SIGMA-XY 312 GAUSS POINT',&
            'SIGMA-YZ 312 GAUSS POINT',&
            'SIGMA-XZ 312 GAUSS POINT',&
            'PLASTIC STRAIN 312 GAUSS POINT',&
            'SIGMA-X 122 GAUSS POINT',&
            'SIGMA-Y 122 GAUSS POINT',&
            'SIGMA-Z 122 GAUSS POINT',&
            'SIGMA-XY 122 GAUSS POINT',&
            'SIGMA-YZ 122 GAUSS POINT',&
            'SIGMA-XZ 122 GAUSS POINT',&
            'PLASTIC STRAIN 122 GAUSS POINT',&
            'SIGMA-X 222 GAUSS POINT',&
            'SIGMA-Y 222 GAUSS POINT',&
            'SIGMA-Z 222 GAUSS POINT',&
            'SIGMA-XY 222 GAUSS POINT',&
            'SIGMA-YZ 222 GAUSS POINT',&
            'SIGMA-XZ 222 GAUSS POINT',&
            'PLASTIC STRAIN 222 GAUSS POINT',&
            'SIGMA-X 322 GAUSS POINT',&
            'SIGMA-Y 322 GAUSS POINT',&
            'SIGMA-Z 322 GAUSS POINT',&
            'SIGMA-XY 322 GAUSS POINT',&
            'SIGMA-YZ 322 GAUSS POINT',&
            'SIGMA-XZ 322 GAUSS POINT',&
            'PLASTIC STRAIN 322 GAUSS POINT',&
            'SIGMA-X 132 GAUSS POINT',&
            'SIGMA-Y 132 GAUSS POINT',&
            'SIGMA-Z 132 GAUSS POINT',&
            'SIGMA-XY 132 GAUSS POINT',&
            'SIGMA-YZ 132 GAUSS POINT',&
            'SIGMA-XZ 132 GAUSS POINT',&
            'PLASTIC STRAIN 132 GAUSS POINT',&
            'SIGMA-X 232 GAUSS POINT',&
            'SIGMA-Y 232 GAUSS POINT',&
            'SIGMA-Z 232 GAUSS POINT',&
            'SIGMA-XY 232 GAUSS POINT',&
            'SIGMA-YZ 232 GAUSS POINT',&
            'SIGMA-XZ 232 GAUSS POINT',&
            'PLASTIC STRAIN 232 GAUSS POINT',&
            'SIGMA-X 332 GAUSS POINT',&
            'SIGMA-Y 332 GAUSS POINT',&
            'SIGMA-Z 332 GAUSS POINT',&
            'SIGMA-XY 332 GAUSS POINT',&
            'SIGMA-YZ 332 GAUSS POINT',&
            'SIGMA-XZ 332 GAUSS POINT',&
            'PLASTIC STRAIN 332 GAUSS POINT',&
            'SIGMA-X 142 GAUSS POINT',&
            'SIGMA-Y 142 GAUSS POINT',&
            'SIGMA-Z 142 GAUSS POINT',&
            'SIGMA-XY 142 GAUSS POINT',&
            'SIGMA-YZ 142 GAUSS POINT',&
            'SIGMA-XZ 142 GAUSS POINT',&
            'PLASTIC STRAIN 142 GAUSS POINT',&
            'SIGMA-X 242 GAUSS POINT',&
            'SIGMA-Y 242 GAUSS POINT',&
            'SIGMA-Z 242 GAUSS POINT',&
            'SIGMA-XY 242 GAUSS POINT',&
            'SIGMA-YZ 242 GAUSS POINT',&
            'SIGMA-XZ 242 GAUSS POINT',&
            'PLASTIC STRAIN 242 GAUSS POINT',&
            'SIGMA-X 342 GAUSS POINT',&
            'SIGMA-Y 342 GAUSS POINT',&
            'SIGMA-Z 342 GAUSS POINT',&
            'SIGMA-XY 342 GAUSS POINT',&
            'SIGMA-YZ 342 GAUSS POINT',&
            'SIGMA-XZ 342 GAUSS POINT',&
            'PLASTIC STRAIN 342 GAUSS POINT',&
            'SIGMA-X 152 GAUSS POINT',&
            'SIGMA-Y 152 GAUSS POINT',&
            'SIGMA-Z 152 GAUSS POINT',&
            'SIGMA-XY 152 GAUSS POINT',&
            'SIGMA-YZ 152 GAUSS POINT',&
            'SIGMA-XZ 152 GAUSS POINT',&
            'PLASTIC STRAIN 152 GAUSS POINT',&
            'SIGMA-X 252 GAUSS POINT',&
            'SIGMA-Y 252 GAUSS POINT',&
            'SIGMA-Z 252 GAUSS POINT',&
            'SIGMA-XY 252 GAUSS POINT',&
            'SIGMA-YZ 252 GAUSS POINT',&
            'SIGMA-XZ 252 GAUSS POINT',&
            'PLASTIC STRAIN 252 GAUSS POINT',&
            'SIGMA-X 352 GAUSS POINT',&
            'SIGMA-Y 352 GAUSS POINT',&
            'SIGMA-Z 352 GAUSS POINT',&
            'SIGMA-XY 352 GAUSS POINT',&
            'SIGMA-YZ 352 GAUSS POINT',&
            'SIGMA-XZ 352 GAUSS POINT',&
            'PLASTIC STRAIN 352 GAUSS POINT',&
            'SIGMA-X 162 GAUSS POINT',&
            'SIGMA-Y 162 GAUSS POINT',&
            'SIGMA-Z 162 GAUSS POINT',&
            'SIGMA-XY 162 GAUSS POINT',&
            'SIGMA-YZ 162 GAUSS POINT',&
            'SIGMA-XZ 162 GAUSS POINT',&
            'PLASTIC STRAIN 162 GAUSS POINT',&
            'SIGMA-X 262 GAUSS POINT',&
            'SIGMA-Y 262 GAUSS POINT',&
            'SIGMA-Z 261 GAUSS POINT',&
            'SIGMA-XY 262 GAUSS POINT',&
            'SIGMA-YZ 262 GAUSS POINT',&
            'SIGMA-XZ 262 GAUSS POINT',&
            'PLASTIC STRAIN 262 GAUSS POINT',&
            'SIGMA-X 362 GAUSS POINT',&
            'SIGMA-Y 362 GAUSS POINT',&
            'SIGMA-Z 362 GAUSS POINT',&
            'SIGMA-XY 362 GAUSS POINT',&
            'SIGMA-YZ 362 GAUSS POINT',&
            'SIGMA-XZ 362 GAUSS POINT',&
            'PLASTIC STRAIN 362 GAUSS POINT',&
            'SIGMA-X  172 GAUSS POINT',&
            'SIGMA-Y  172 GAUSS POINT',&
            'SIGMA-Z  172 GAUSS POINT',&
            'SIGMA-XY 172 GAUSS POINT',&
            'SIGMA-YZ 172 GAUSS POINT',&
            'SIGMA-XZ 172 GAUSS POINT',&
            'PLASTIC STRAIN 172 GAUSS POINT',&
            'SIGMA-X  272 GAUSS POINT',&
            'SIGMA-Y  272 GAUSS POINT',&
            'SIGMA-Z  272 GAUSS POINT',&
            'SIGMA-XY 272 GAUSS POINT',&
            'SIGMA-YZ 272 GAUSS POINT',&
            'SIGMA-XZ 272 GAUSS POINT',&
            'PLASTIC STRAIN 272 GAUSS POINT',&
            'SIGMA-X  372 GAUSS POINT',&
            'SIGMA-Y  372 GAUSS POINT',&
            'SIGMA-Z  372 GAUSS POINT',&
            'SIGMA-XY 372 GAUSS POINT',&
            'SIGMA-YZ 372 GAUSS POINT',&
            'SIGMA-XZ 372 GAUSS POINT',&
            'PLASTIC STRAIN 372 GAUSS POINT',&
            'SIGMA-X  182 GAUSS POINT',&
            'SIGMA-Y  182 GAUSS POINT',&
            'SIGMA-Z  182 GAUSS POINT',&
            'SIGMA-XY 182 GAUSS POINT',&
            'SIGMA-YZ 182 GAUSS POINT',&
            'SIGMA-XZ 182 GAUSS POINT',&
            'PLASTIC STRAIN 182 GAUSS POINT',&
            'SIGMA-X  282 GAUSS POINT',&
            'SIGMA-Y  282 GAUSS POINT',&
            'SIGMA-Z  282 GAUSS POINT',&
            'SIGMA-XY 282 GAUSS POINT',&
            'SIGMA-YZ 282 GAUSS POINT',&
            'SIGMA-XZ 282 GAUSS POINT',&
            'PLASTIC STRAIN 282 GAUSS POINT',&
            'SIGMA-X  382 GAUSS POINT',&
            'SIGMA-Y  382 GAUSS POINT',&
            'SIGMA-Z  382 GAUSS POINT',&
            'SIGMA-XY 382 GAUSS POINT',&
            'SIGMA-YZ 382 GAUSS POINT',&
            'SIGMA-XZ 382 GAUSS POINT',&
            'PLASTIC STRAIN 382 GAUSS POINT',&
            'SIGMA-X  192 GAUSS POINT',&
            'SIGMA-Y  192 GAUSS POINT',&
            'SIGMA-Z  192 GAUSS POINT',&
            'SIGMA-XY 192 GAUSS POINT',&
            'SIGMA-YZ 192 GAUSS POINT',&
            'SIGMA-XZ 192 GAUSS POINT',&
            'PLASTIC STRAIN 192 GAUSS POINT',&
            'SIGMA-X  292 GAUSS POINT',&
            'SIGMA-Y  292 GAUSS POINT',&
            'SIGMA-Z  292 GAUSS POINT',&
            'SIGMA-XY 292 GAUSS POINT',&
            'SIGMA-YZ 292 GAUSS POINT',&
            'SIGMA-XZ 292 GAUSS POINT',&
            'PLASTIC STRAIN 292 GAUSS POINT',&
            'SIGMA-X  392 GAUSS POINT',&
            'SIGMA-Y  392 GAUSS POINT',&
            'SIGMA-Z  392 GAUSS POINT',&
            'SIGMA-XY 392 GAUSS POINT',&
            'SIGMA-YZ 392 GAUSS POINT',&
            'SIGMA-XZ 392 GAUSS POINT',&
            'PLASTIC STRAIN 392 GAUSS POINT',&
            'SIGMA-X  113 GAUSS POINT',&
            'SIGMA-Y  113 GAUSS POINT',&
            'SIGMA-Z  113 GAUSS POINT',&
            'SIGMA-XY 113 GAUSS POINT',&
            'SIGMA-YZ 113 GAUSS POINT',&
            'SIGMA-XZ 113 GAUSS POINT',&
            'PLASTIC STRAIN 113 GAUSS POINT',&
            'SIGMA-X  213 GAUSS POINT',&
            'SIGMA-Y  213 GAUSS POINT',&
            'SIGMA-Z  213 GAUSS POINT',&
            'SIGMA-XY 213 GAUSS POINT',&
            'SIGMA-YZ 213 GAUSS POINT',&
            'SIGMA-XZ 213 GAUSS POINT',&
            'PLASTIC STRAIN 213 GAUSS POINT',&
            'SIGMA-X  313 GAUSS POINT',&
            'SIGMA-Y  313 GAUSS POINT',&
            'SIGMA-Z  313 GAUSS POINT',&
            'SIGMA-XY 313 GAUSS POINT',&
            'SIGMA-YZ 313 GAUSS POINT',&
            'SIGMA-XZ 313 GAUSS POINT',&
            'PLASTIC STRAIN 313 GAUSS POINT',&
            'SIGMA-X  123 GAUSS POINT',&
            'SIGMA-Y  123 GAUSS POINT',&
            'SIGMA-Z  123 GAUSS POINT',&
            'SIGMA-XY 123 GAUSS POINT',&
            'SIGMA-YZ 123 GAUSS POINT',&
            'SIGMA-XZ 123 GAUSS POINT',&
            'PLASTIC STRAIN 123 GAUSS POINT',&
            'SIGMA-X  223 GAUSS POINT',&
            'SIGMA-Y  223 GAUSS POINT',&
            'SIGMA-Z  223 GAUSS POINT',&
            'SIGMA-XY 223 GAUSS POINT',&
            'SIGMA-YZ 223 GAUSS POINT',&
            'SIGMA-XZ 223 GAUSS POINT',&
            'PLASTIC STRAIN 223 GAUSS POINT',&
            'SIGMA-X  323 GAUSS POINT',&
            'SIGMA-Y  323 GAUSS POINT',&
            'SIGMA-Z  323 GAUSS POINT',&
            'SIGMA-XY 323 GAUSS POINT',&
            'SIGMA-YZ 323 GAUSS POINT',&
            'SIGMA-XZ 323 GAUSS POINT',&
            'PLASTIC STRAIN 323 GAUSS POINT',&
            'SIGMA-X  133 GAUSS POINT',&
            'SIGMA-Y  133 GAUSS POINT',&
            'SIGMA-Z  133 GAUSS POINT',&
            'SIGMA-XY 133 GAUSS POINT',&
            'SIGMA-YZ 133 GAUSS POINT',&
            'SIGMA-XZ 133 GAUSS POINT',&
            'PLASTIC STRAIN 133 GAUSS POINT',&
            'SIGMA-X  233 GAUSS POINT',&
            'SIGMA-Y  233 GAUSS POINT',&
            'SIGMA-Z  233 GAUSS POINT',&
            'SIGMA-XY 233 GAUSS POINT',&
            'SIGMA-YZ 233 GAUSS POINT',&
            'SIGMA-XZ 233 GAUSS POINT',&
            'PLASTIC STRAIN 233 GAUSS POINT',&
            'SIGMA-X  333 GAUSS POINT',&
            'SIGMA-Y  333 GAUSS POINT',&
            'SIGMA-Z  333 GAUSS POINT',&
            'SIGMA-XY 333 GAUSS POINT',&
            'SIGMA-YZ 333 GAUSS POINT',&
            'SIGMA-XZ 333 GAUSS POINT',&
            'PLASTIC STRAIN 333 GAUSS POINT',&
            'SIGMA-X  143 GAUSS POINT',&
            'SIGMA-Y  143 GAUSS POINT',&
            'SIGMA-Z  143 GAUSS POINT',&
            'SIGMA-XY 143 GAUSS POINT',&
            'SIGMA-YZ 143 GAUSS POINT',&
            'SIGMA-XZ 143 GAUSS POINT',&
            'PLASTIC STRAIN 143 GAUSS POINT',&
            'SIGMA-X  243 GAUSS POINT',&
            'SIGMA-Y  243 GAUSS POINT',&
            'SIGMA-Z  243 GAUSS POINT',&
            'SIGMA-XY 243 GAUSS POINT',&
            'SIGMA-YZ 243 GAUSS POINT',&
            'SIGMA-XZ 243 GAUSS POINT',&
            'PLASTIC STRAIN 243 GAUSS POINT',&
            'SIGMA-X  343 GAUSS POINT',&
            'SIGMA-Y  343 GAUSS POINT',&
            'SIGMA-Z  343 GAUSS POINT',&
            'SIGMA-XY 343 GAUSS POINT',&
            'SIGMA-YZ 343 GAUSS POINT',&
            'SIGMA-XZ 343 GAUSS POINT',&
            'PLASTIC STRAIN 343 GAUSS POINT',&
            'SIGMA-X  153 GAUSS POINT',&
            'SIGMA-Y  153 GAUSS POINT',&
            'SIGMA-Z  153 GAUSS POINT',&
            'SIGMA-XY 153 GAUSS POINT',&
            'SIGMA-YZ 153 GAUSS POINT',&
            'SIGMA-XZ 153 GAUSS POINT',&
            'PLASTIC STRAIN 153 GAUSS POINT',&
            'SIGMA-X  253 GAUSS POINT',&
            'SIGMA-Y  253 GAUSS POINT',&
            'SIGMA-Z  253 GAUSS POINT',&
            'SIGMA-XY 253 GAUSS POINT',&
            'SIGMA-YZ 253 GAUSS POINT',&
            'SIGMA-XZ 253 GAUSS POINT',&
            'PLASTIC STRAIN 253 GAUSS POINT',&
            'SIGMA-X  353 GAUSS POINT',&
            'SIGMA-Y  353 GAUSS POINT',&
            'SIGMA-Z  353 GAUSS POINT',&
            'SIGMA-XY 353 GAUSS POINT',&
            'SIGMA-YZ 353 GAUSS POINT',&
            'SIGMA-XZ 353 GAUSS POINT',&
            'PLASTIC STRAIN 353 GAUSS POINT',&
            'SIGMA-X  163 GAUSS POINT',&
            'SIGMA-Y  163 GAUSS POINT',&
            'SIGMA-Z  163 GAUSS POINT',&
            'SIGMA-XY 163 GAUSS POINT',&
            'SIGMA-YZ 163 GAUSS POINT',&
            'SIGMA-XZ 163 GAUSS POINT',&
            'PLASTIC STRAIN 163 GAUSS POINT',&
            'SIGMA-X  263 GAUSS POINT',&
            'SIGMA-Y  263 GAUSS POINT',&
            'SIGMA-Z  263 GAUSS POINT',&
            'SIGMA-XY 263 GAUSS POINT',&
            'SIGMA-YZ 263 GAUSS POINT',&
            'SIGMA-XZ 263 GAUSS POINT',&
            'PLASTIC STRAIN 263 GAUSS POINT',&
            'SIGMA-X  363 GAUSS POINT',&
            'SIGMA-Y  363 GAUSS POINT',&
            'SIGMA-Z  363 GAUSS POINT',&
            'SIGMA-XY 363 GAUSS POINT',&
            'SIGMA-YZ 363 GAUSS POINT',&
            'SIGMA-XZ 363 GAUSS POINT',&
            'PLASTIC STRAIN 363 GAUSS POINT',&
            'SIGMA-X  173 GAUSS POINT',&
            'SIGMA-Y  173 GAUSS POINT',&
            'SIGMA-Z  173 GAUSS POINT',&
            'SIGMA-XY 173 GAUSS POINT',&
            'SIGMA-YZ 173 GAUSS POINT',&
            'SIGMA-XZ 173 GAUSS POINT',&
            'PLASTIC STRAIN 173 GAUSS POINT',&
            'SIGMA-X  273 GAUSS POINT',&
            'SIGMA-Y  273 GAUSS POINT',&
            'SIGMA-Z  273 GAUSS POINT',&
            'SIGMA-XY 273 GAUSS POINT',&
            'SIGMA-YZ 273 GAUSS POINT',&
            'SIGMA-XZ 273 GAUSS POINT',&
            'PLASTIC STRAIN 273 GAUSS POINT',&
            'SIGMA-X  373 GAUSS POINT',&
            'SIGMA-Y  373 GAUSS POINT',&
            'SIGMA-Z  373 GAUSS POINT',&
            'SIGMA-XY 373 GAUSS POINT',&
            'SIGMA-YZ 373 GAUSS POINT',&
            'SIGMA-XZ 373 GAUSS POINT',&
            'PLASTIC STRAIN 373 GAUSS POINT',&
            'SIGMA-X  183 GAUSS POINT',&
            'SIGMA-Y  183 GAUSS POINT',&
            'SIGMA-Z  183 GAUSS POINT',&
            'SIGMA-XY 183 GAUSS POINT',&
            'SIGMA-YZ 183 GAUSS POINT',&
            'SIGMA-XZ 183 GAUSS POINT',&
            'PLASTIC STRAIN 183 GAUSS POINT',&
            'SIGMA-X  283 GAUSS POINT',&
            'SIGMA-Y  283 GAUSS POINT',&
            'SIGMA-Z  283 GAUSS POINT',&
            'SIGMA-XY 283 GAUSS POINT',&
            'SIGMA-YZ 283 GAUSS POINT',&
            'SIGMA-XZ 283 GAUSS POINT',&
            'PLASTIC STRAIN 283 GAUSS POINT',&
            'SIGMA-X  383 GAUSS POINT',&
            'SIGMA-Y  383 GAUSS POINT',&
            'SIGMA-Z  383 GAUSS POINT',&
            'SIGMA-XY 383 GAUSS POINT',&
            'SIGMA-YZ 383 GAUSS POINT',&
            'SIGMA-XZ 383 GAUSS POINT',&
            'PLASTIC STRAIN 383 GAUSS POINT',&
            'SIGMA-X  193 GAUSS POINT',&
            'SIGMA-Y  193 GAUSS POINT',&
            'SIGMA-Z  193 GAUSS POINT',&
            'SIGMA-XY 193 GAUSS POINT',&
            'SIGMA-YZ 193 GAUSS POINT',&
            'SIGMA-XZ 193 GAUSS POINT',&
            'PLASTIC STRAIN 193 GAUSS POINT',&
            'SIGMA-X  293 GAUSS POINT',&
            'SIGMA-Y  293 GAUSS POINT',&
            'SIGMA-Z  293 GAUSS POINT',&
            'SIGMA-XY 293 GAUSS POINT',&
            'SIGMA-YZ 293 GAUSS POINT',&
            'SIGMA-XZ 293 GAUSS POINT',&
            'PLASTIC STRAIN 293 GAUSS POINT',&
            'SIGMA-X  393 GAUSS POINT',&
            'SIGMA-Y  393 GAUSS POINT',&
            'SIGMA-Z  393 GAUSS POINT',&
            'SIGMA-XY 393 GAUSS POINT',&
            'SIGMA-YZ 393 GAUSS POINT',&
            'SIGMA-XZ 393 GAUSS POINT',&
            'PLASTIC STRAIN 393 GAUSS POINT'/)

          vars3_title = (/&
            character(len=100) ::&
            'SIGMA-X   1U1  POINT',&
            'SIGMA-Y   1U1  POINT',&
            'SIGMA-Z   1U1  POINT',&
            'SIGMA-XY  1U1  POINT',&
            'SIGMA-YZ  1U1  POINT',&
            'SIGMA-ZX  1U1  POINT',&
            'PLASTIC STRAIN 1U1 POINT',&
            'SIGMA-X   2U1  POINT',&
            'SIGMA-Y   2U1  POINT',&
            'SIGMA-Z   2U1  POINT',&
            'SIGMA-XY  2U1  POINT',&
            'SIGMA-YZ  2U1  POINT',&
            'SIGMA-ZX  2U1  POINT',&
            'PLASTIC STRAIN 2U1 POINT',&
            'SIGMA-X   3U1  POINT',&
            'SIGMA-Y   3U1  POINT',&
            'SIGMA-Z   3U1  POINT',&
            'SIGMA-XY  3U1  POINT',&
            'SIGMA-YZ  3U1  POINT',&
            'SIGMA-ZX  3U1  POINT',&
            'PLASTIC STRAIN 3U1 POINT',&
            'SIGMA-X   1U2  POINT',&
            'SIGMA-Y   1U2  POINT',&
            'SIGMA-Z   1U2  POINT',&
            'SIGMA-XY  1U2  POINT',&
            'SIGMA-YZ  1U2  POINT',&
            'SIGMA-ZX  1U2  POINT',&
            'PLASTIC STRAIN 1U2 POINT',&
            'SIGMA-X   2U2  POINT',&
            'SIGMA-Y   2U2  POINT',&
            'SIGMA-Z   2U2  POINT',&
            'SIGMA-XY  2U2  POINT',&
            'SIGMA-YZ  2U2  POINT',&
            'SIGMA-ZX  2U2  POINT',&
            'PLASTIC STRAIN 2U2 POINT',&
            'SIGMA-X   3U2  POINT',&
            'SIGMA-Y   3U2  POINT',&
            'SIGMA-Z   3U2  POINT',&
            'SIGMA-XY  3U2  POINT',&
            'SIGMA-YZ  3U2  POINT',&
            'SIGMA-ZX  3U2  POINT',&
            'PLASTIC STRAIN 3U2 POINT',&
            'SIGMA-X   1U3  POINT',&
            'SIGMA-Y   1U3  POINT',&
            'SIGMA-Z   1U3  POINT',&
            'SIGMA-XY  1U3  POINT',&
            'SIGMA-YZ  1U3  POINT',&
            'SIGMA-ZX  1U3  POINT',&
            'PLASTIC STRAIN 1U3 POINT',&
            'SIGMA-X   2U3  POINT',&
            'SIGMA-Y   2U3  POINT',&
            'SIGMA-Z   2U3  POINT',&
            'SIGMA-XY  2U3  POINT',&
            'SIGMA-YZ  2U3  POINT',&
            'SIGMA-ZX  2U3  POINT',&
            'PLASTIC STRAIN 2U3 POINT',&
            'SIGMA-X   3U3  POINT',&
            'SIGMA-Y   3U3  POINT',&
            'SIGMA-Z   3U3  POINT',&
            'SIGMA-XY  3U3  POINT',&
            'SIGMA-YZ  3U3  POINT',&
            'SIGMA-ZX  3U3  POINT',&
            'PLASTIC STRAIN 3U3 POINT',&
            'SIGMA-X   1D1  POINT',&
            'SIGMA-Y   1D1  POINT',&
            'SIGMA-Z   1D1  POINT',&
            'SIGMA-XY  1D1  POINT',&
            'SIGMA-YZ  1D1  POINT',&
            'SIGMA-ZX  1D1  POINT',&
            'PLASTIC STRAIN 1D1 POINT',&
            'SIGMA-X   2D1  POINT',&
            'SIGMA-Y   2D1  POINT',&
            'SIGMA-Z   2D1  POINT',&
            'SIGMA-XY  2D1  POINT',&
            'SIGMA-YZ  2D1  POINT',&
            'SIGMA-ZX  2D1  POINT',&
            'PLASTIC STRAIN 2D1 POINT',&
            'SIGMA-X   3D1  POINT',&
            'SIGMA-Y   3D1  POINT',&
            'SIGMA-Z   3D1  POINT',&
            'SIGMA-XY  3D1  POINT',&
            'SIGMA-YZ  3D1  POINT',&
            'SIGMA-ZX  3D1  POINT',&
            'PLASTIC STRAIN 3D1 POINT',&
            'SIGMA-X   1D2  POINT',&
            'SIGMA-Y   1D2  POINT',&
            'SIGMA-Z   1D2  POINT',&
            'SIGMA-XY  1D2  POINT',&
            'SIGMA-YZ  1D2  POINT',&
            'SIGMA-ZX  1D2  POINT',&
            'PLASTIC STRAIN 1D2 POINT',&
            'SIGMA-X   2D2  POINT',&
            'SIGMA-Y   2D2  POINT',&
            'SIGMA-Z   2D2  POINT',&
            'SIGMA-XY  2D2  POINT',&
            'SIGMA-YZ  2D2  POINT',&
            'SIGMA-ZX  2D2  POINT',&
            'PLASTIC STRAIN 2D2 POINT',&
            'SIGMA-X   3D2  POINT',&
            'SIGMA-Y   3D2  POINT',&
            'SIGMA-Z   3D2  POINT',&
            'SIGMA-XY  3D2  POINT',&
            'SIGMA-YZ  3D2  POINT',&
            'SIGMA-ZX  3D2  POINT',&
            'PLASTIC STRAIN 3D2 POINT',&
            'SIGMA-X   1D3  POINT',&
            'SIGMA-Y   1D3  POINT',&
            'SIGMA-Z   1D3  POINT',&
            'SIGMA-XY  1D3  POINT',&
            'SIGMA-YZ  1D3  POINT',&
            'SIGMA-ZX  1D3  POINT',&
            'PLASTIC STRAIN 1D3 POINT',&
            'SIGMA-X   2D3  POINT',&
            'SIGMA-Y   2D3  POINT',&
            'SIGMA-Z   2D3  POINT',&
            'SIGMA-XY  2D3  POINT',&
            'SIGMA-YZ  2D3  POINT',&
            'SIGMA-ZX  2D3  POINT',&
            'PLASTIC STRAIN 2D3 POINT',&
            'SIGMA-X   3D3  POINT',&
            'SIGMA-Y   3D3  POINT',&
            'SIGMA-Z   3D3  POINT',&
            'SIGMA-XY  3D3  POINT',&
            'SIGMA-YZ  3D3  POINT',&
            'SIGMA-ZX  3D3  POINT',&
            'PLASTIC STRAIN 3D3 POINT',&
            '1ST USER VARIABLE  111 GAUSS POINT',&
            '2ND USER VARIABLE  111 GAUSS POINT',&
            '3RD USER VARIABLE  111 GAUSS POINT',&
            '4TH USER VARIABLE  111 GAUSS POINT',&
            '5TH USER VARIABLE  111 GAUSS POINT',&
            '6TH USER VARIABLE  111 GAUSS POINT',&
            '7TH USER VARIABLE  111 GAUSS POINT',&
            '8TH USER VARIABLE  111 GAUSS POINT',&
            '9TH USER VARIABLE  111 GAUSS POINT',&
            '1ST USER VARIABLE  211 GAUSS POINT',&
            '2ND USER VARIABLE  211 GAUSS POINT',&
            '3RD USER VARIABLE  211 GAUSS POINT',&
            '4TH USER VARIABLE  211 GAUSS POINT',&
            '5TH USER VARIABLE  211 GAUSS POINT',&
            '6TH USER VARIABLE  211 GAUSS POINT',&
            '7TH USER VARIABLE  211 GAUSS POINT',&
            '8TH USER VARIABLE  211 GAUSS POINT',&
            '9TH USER VARIABLE  211 GAUSS POINT',&
            '1ST USER VARIABLE  311 GAUSS POINT',&
            '2ND USER VARIABLE  311 GAUSS POINT',&
            '3RD USER VARIABLE  311 GAUSS POINT',&
            '4TH USER VARIABLE  311 GAUSS POINT',&
            '5TH USER VARIABLE  311 GAUSS POINT',&
            '6TH USER VARIABLE  311 GAUSS POINT',&
            '7TH USER VARIABLE  311 GAUSS POINT',&
            '8TH USER VARIABLE  311 GAUSS POINT',&
            '9TH USER VARIABLE  311 GAUSS POINT',&
            '1ST USER VARIABLE  121 GAUSS POINT',&
            '2ND USER VARIABLE  121 GAUSS POINT',&
            '3RD USER VARIABLE  121 GAUSS POINT',&
            '4TH USER VARIABLE  121 GAUSS POINT',&
            '5TH USER VARIABLE  121 GAUSS POINT',&
            '6TH USER VARIABLE  121 GAUSS POINT',&
            '7TH USER VARIABLE  121 GAUSS POINT',&
            '8TH USER VARIABLE  121 GAUSS POINT',&
            '9TH USER VARIABLE  121 GAUSS POINT',&
            '1ST USER VARIABLE  221 GAUSS POINT',&
            '2ND USER VARIABLE  221 GAUSS POINT',&
            '3RD USER VARIABLE  221 GAUSS POINT',&
            '4TH USER VARIABLE  221 GAUSS POINT',&
            '5TH USER VARIABLE  221 GAUSS POINT',&
            '6TH USER VARIABLE  221 GAUSS POINT',&
            '7TH USER VARIABLE  221 GAUSS POINT',&
            '8TH USER VARIABLE  221 GAUSS POINT',&
            '9TH USER VARIABLE  221 GAUSS POINT',&
            '1ST USER VARIABLE  321 GAUSS POINT',&
            '2ND USER VARIABLE  321 GAUSS POINT',&
            '3RD USER VARIABLE  321 GAUSS POINT',&
            '4TH USER VARIABLE  321 GAUSS POINT',&
            '5TH USER VARIABLE  321 GAUSS POINT',&
            '6TH USER VARIABLE  321 GAUSS POINT',&
            '7TH USER VARIABLE  321 GAUSS POINT',&
            '8TH USER VARIABLE  321 GAUSS POINT',&
            '9TH USER VARIABLE  321 GAUSS POINT',&
            '1ST USER VARIABLE  131 GAUSS POINT',&
            '2ND USER VARIABLE  131 GAUSS POINT',&
            '3RD USER VARIABLE  131 GAUSS POINT',&
            '4TH USER VARIABLE  131 GAUSS POINT',&
            '5TH USER VARIABLE  131 GAUSS POINT',&
            '6TH USER VARIABLE  131 GAUSS POINT',&
            '7TH USER VARIABLE  131 GAUSS POINT',&
            '8TH USER VARIABLE  131 GAUSS POINT',&
            '9TH USER VARIABLE  131 GAUSS POINT',&
            '1ST USER VARIABLE  231 GAUSS POINT',&
            '2ND USER VARIABLE  231 GAUSS POINT',&
            '3RD USER VARIABLE  231 GAUSS POINT',&
            '4TH USER VARIABLE  231 GAUSS POINT',&
            '5TH USER VARIABLE  231 GAUSS POINT',&
            '6TH USER VARIABLE  231 GAUSS POINT',&
            '7TH USER VARIABLE  231 GAUSS POINT',&
            '8TH USER VARIABLE  231 GAUSS POINT',&
            '9TH USER VARIABLE  231 GAUSS POINT',&
            '1ST USER VARIABLE  331 GAUSS POINT',&
            '2ND USER VARIABLE  331 GAUSS POINT',&
            '3RD USER VARIABLE  331 GAUSS POINT',&
            '4TH USER VARIABLE  331 GAUSS POINT',&
            '5TH USER VARIABLE  331 GAUSS POINT',&
            '6TH USER VARIABLE  331 GAUSS POINT',&
            '7TH USER VARIABLE  331 GAUSS POINT',&
            '8TH USER VARIABLE  331 GAUSS POINT',&
            '9TH USER VARIABLE  331 GAUSS POINT',&
            '1ST USER VARIABLE  141 GAUSS POINT',&
            '2ND USER VARIABLE  141 GAUSS POINT',&
            '3RD USER VARIABLE  141 GAUSS POINT',&
            '4TH USER VARIABLE  141 GAUSS POINT',&
            '5TH USER VARIABLE  141 GAUSS POINT',&
            '6TH USER VARIABLE  141 GAUSS POINT',&
            '7TH USER VARIABLE  141 GAUSS POINT',&
            '8TH USER VARIABLE  141 GAUSS POINT',&
            '9TH USER VARIABLE  141 GAUSS POINT',&
            '1ST USER VARIABLE  241 GAUSS POINT',&
            '2ND USER VARIABLE  241 GAUSS POINT',&
            '3RD USER VARIABLE  241 GAUSS POINT',&
            '4TH USER VARIABLE  241 GAUSS POINT',&
            '5TH USER VARIABLE  241 GAUSS POINT',&
            '6TH USER VARIABLE  241 GAUSS POINT',&
            '7TH USER VARIABLE  241 GAUSS POINT',&
            '8TH USER VARIABLE  241 GAUSS POINT',&
            '9TH USER VARIABLE  241 GAUSS POINT',&
            '1ST USER VARIABLE  341 GAUSS POINT',&
            '2ND USER VARIABLE  341 GAUSS POINT',&
            '3RD USER VARIABLE  341 GAUSS POINT',&
            '4TH USER VARIABLE  341 GAUSS POINT',&
            '5TH USER VARIABLE  341 GAUSS POINT',&
            '6TH USER VARIABLE  341 GAUSS POINT',&
            '7TH USER VARIABLE  341 GAUSS POINT',&
            '8TH USER VARIABLE  341 GAUSS POINT',&
            '9TH USER VARIABLE  341 GAUSS POINT',&
            '1ST USER VARIABLE  151 GAUSS POINT',&
            '2ND USER VARIABLE  151 GAUSS POINT',&
            '3RD USER VARIABLE  151 GAUSS POINT',&
            '4TH USER VARIABLE  151 GAUSS POINT',&
            '5TH USER VARIABLE  151 GAUSS POINT',&
            '6TH USER VARIABLE  151 GAUSS POINT',&
            '7TH USER VARIABLE  151 GAUSS POINT',&
            '8TH USER VARIABLE  151 GAUSS POINT',&
            '9TH USER VARIABLE  151 GAUSS POINT',&
            '1ST USER VARIABLE  251 GAUSS POINT',&
            '2ND USER VARIABLE  251 GAUSS POINT',&
            '3RD USER VARIABLE  251 GAUSS POINT',&
            '4TH USER VARIABLE  251 GAUSS POINT',&
            '5TH USER VARIABLE  251 GAUSS POINT',&
            '6TH USER VARIABLE  251 GAUSS POINT',&
            '7TH USER VARIABLE  251 GAUSS POINT',&
            '8TH USER VARIABLE  251 GAUSS POINT',&
            '9TH USER VARIABLE  251 GAUSS POINT',&
            '1ST USER VARIABLE  351 GAUSS POINT',&
            '2ND USER VARIABLE  351 GAUSS POINT',&
            '3RD USER VARIABLE  351 GAUSS POINT',&
            '4TH USER VARIABLE  351 GAUSS POINT',&
            '5TH USER VARIABLE  351 GAUSS POINT',&
            '6TH USER VARIABLE  351 GAUSS POINT',&
            '7TH USER VARIABLE  351 GAUSS POINT',&
            '8TH USER VARIABLE  351 GAUSS POINT',&
            '9TH USER VARIABLE  351 GAUSS POINT',&
            '1ST USER VARIABLE  161 GAUSS POINT',&
            '2ND USER VARIABLE  161 GAUSS POINT',&
            '3RD USER VARIABLE  161 GAUSS POINT',&
            '4TH USER VARIABLE  161 GAUSS POINT',&
            '5TH USER VARIABLE  161 GAUSS POINT',&
            '6TH USER VARIABLE  161 GAUSS POINT',&
            '7TH USER VARIABLE  161 GAUSS POINT',&
            '8TH USER VARIABLE  161 GAUSS POINT',&
            '9TH USER VARIABLE  161 GAUSS POINT',&
            '1ST USER VARIABLE  261 GAUSS POINT',&
            '2ND USER VARIABLE  261 GAUSS POINT',&
            '3RD USER VARIABLE  261 GAUSS POINT',&
            '4TH USER VARIABLE  261 GAUSS POINT',&
            '5TH USER VARIABLE  261 GAUSS POINT',&
            '6TH USER VARIABLE  261 GAUSS POINT',&
            '7TH USER VARIABLE  261 GAUSS POINT',&
            '8TH USER VARIABLE  261 GAUSS POINT',&
            '9TH USER VARIABLE  261 GAUSS POINT',&
            '1ST USER VARIABLE  361 GAUSS POINT',&
            '2ND USER VARIABLE  361 GAUSS POINT',&
            '3RD USER VARIABLE  361 GAUSS POINT',&
            '4TH USER VARIABLE  361 GAUSS POINT',&
            '5TH USER VARIABLE  361 GAUSS POINT',&
            '6TH USER VARIABLE  361 GAUSS POINT',&
            '7TH USER VARIABLE  361 GAUSS POINT',&
            '8TH USER VARIABLE  361 GAUSS POINT',&
            '9TH USER VARIABLE  361 GAUSS POINT',&
            '1ST USER VARIABLE  171 GAUSS POINT',&
            '2ND USER VARIABLE  171 GAUSS POINT',&
            '3RD USER VARIABLE  171 GAUSS POINT',&
            '4TH USER VARIABLE  171 GAUSS POINT',&
            '5TH USER VARIABLE  171 GAUSS POINT',&
            '6TH USER VARIABLE  171 GAUSS POINT',&
            '7TH USER VARIABLE  171 GAUSS POINT',&
            '8TH USER VARIABLE  171 GAUSS POINT',&
            '9TH USER VARIABLE  171 GAUSS POINT',&
            '1ST USER VARIABLE  271 GAUSS POINT',&
            '2ND USER VARIABLE  271 GAUSS POINT',&
            '3RD USER VARIABLE  271 GAUSS POINT',&
            '4TH USER VARIABLE  271 GAUSS POINT',&
            '5TH USER VARIABLE  271 GAUSS POINT',&
            '6TH USER VARIABLE  271 GAUSS POINT',&
            '7TH USER VARIABLE  271 GAUSS POINT',&
            '8TH USER VARIABLE  271 GAUSS POINT',&
            '9TH USER VARIABLE  271 GAUSS POINT',&
            '1ST USER VARIABLE  371 GAUSS POINT',&
            '2ND USER VARIABLE  371 GAUSS POINT',&
            '3RD USER VARIABLE  371 GAUSS POINT',&
            '4TH USER VARIABLE  371 GAUSS POINT',&
            '5TH USER VARIABLE  371 GAUSS POINT',&
            '6TH USER VARIABLE  371 GAUSS POINT',&
            '7TH USER VARIABLE  371 GAUSS POINT',&
            '8TH USER VARIABLE  371 GAUSS POINT',&
            '9TH USER VARIABLE  371 GAUSS POINT',&
            '1ST USER VARIABLE  181 GAUSS POINT',&
            '2ND USER VARIABLE  181 GAUSS POINT',&
            '3RD USER VARIABLE  181 GAUSS POINT',&
            '4TH USER VARIABLE  181 GAUSS POINT',&
            '5TH USER VARIABLE  181 GAUSS POINT',&
            '6TH USER VARIABLE  181 GAUSS POINT',&
            '7TH USER VARIABLE  181 GAUSS POINT',&
            '8TH USER VARIABLE  381 GAUSS POINT',&
            '9TH USER VARIABLE  381 GAUSS POINT',&
            '1ST USER VARIABLE  281 GAUSS POINT',&
            '2ND USER VARIABLE  281 GAUSS POINT',&
            '3RD USER VARIABLE  281 GAUSS POINT',&
            '4TH USER VARIABLE  281 GAUSS POINT',&
            '5TH USER VARIABLE  281 GAUSS POINT',&
            '6TH USER VARIABLE  281 GAUSS POINT',&
            '7TH USER VARIABLE  281 GAUSS POINT',&
            '8TH USER VARIABLE  281 GAUSS POINT',&
            '9TH USER VARIABLE  281 GAUSS POINT',&
            '1ST USER VARIABLE  381 GAUSS POINT',&
            '2ND USER VARIABLE  381 GAUSS POINT',&
            '3RD USER VARIABLE  381 GAUSS POINT',&
            '4TH USER VARIABLE  381 GAUSS POINT',&
            '5TH USER VARIABLE  381 GAUSS POINT',&
            '6TH USER VARIABLE  381 GAUSS POINT',&
            '7TH USER VARIABLE  381 GAUSS POINT',&
            '8TH USER VARIABLE  381 GAUSS POINT',&
            '9TH USER VARIABLE  381 GAUSS POINT',&
            '1ST USER VARIABLE  191 GAUSS POINT',&
            '2ND USER VARIABLE  191 GAUSS POINT',&
            '3RD USER VARIABLE  191 GAUSS POINT',&
            '4TH USER VARIABLE  191 GAUSS POINT',&
            '5TH USER VARIABLE  191 GAUSS POINT',&
            '6TH USER VARIABLE  191 GAUSS POINT',&
            '7TH USER VARIABLE  191 GAUSS POINT',&
            '8TH USER VARIABLE  191 GAUSS POINT',&
            '9TH USER VARIABLE  191 GAUSS POINT',&
            '1ST USER VARIABLE  191 GAUSS POINT',&
            '2ND USER VARIABLE  291 GAUSS POINT',&
            '3RD USER VARIABLE  291 GAUSS POINT',&
            '4TH USER VARIABLE  291 GAUSS POINT',&
            '5TH USER VARIABLE  291 GAUSS POINT',&
            '6TH USER VARIABLE  291 GAUSS POINT',&
            '7TH USER VARIABLE  291 GAUSS POINT',&
            '8TH USER VARIABLE  291 GAUSS POINT',&
            '9TH USER VARIABLE  291 GAUSS POINT',&
            '1ST USER VARIABLE  391 GAUSS POINT',&
            '2ND USER VARIABLE  391 GAUSS POINT',&
            '3RD USER VARIABLE  391 GAUSS POINT',&
            '4TH USER VARIABLE  391 GAUSS POINT',&
            '5TH USER VARIABLE  391 GAUSS POINT',&
            '6TH USER VARIABLE  391 GAUSS POINT',&
            '7TH USER VARIABLE  391 GAUSS POINT',&
            '8TH USER VARIABLE  391 GAUSS POINT',&
            '9TH USER VARIABLE  391 GAUSS POINT'/)

          vars4_title = (/&
            character(len=100) ::&
            '1ST USER VARIABLE  112 GAUSS POINT',&
            '2ND USER VARIABLE  112 GAUSS POINT',&
            '3RD USER VARIABLE  112 GAUSS POINT',&
            '4TH USER VARIABLE  112 GAUSS POINT',&
            '5TH USER VARIABLE  112 GAUSS POINT',&
            '6TH USER VARIABLE  112 GAUSS POINT',&
            '7TH USER VARIABLE  112 GAUSS POINT',&
            '8TH USER VARIABLE  112 GAUSS POINT',&
            '9TH USER VARIABLE  112 GAUSS POINT',&
            '1ST USER VARIABLE  212 GAUSS POINT',&
            '2ND USER VARIABLE  212 GAUSS POINT',&
            '3RD USER VARIABLE  212 GAUSS POINT',&
            '4TH USER VARIABLE  212 GAUSS POINT',&
            '5TH USER VARIABLE  212 GAUSS POINT',&
            '6TH USER VARIABLE  212 GAUSS POINT',&
            '7TH USER VARIABLE  212 GAUSS POINT',&
            '8TH USER VARIABLE  212 GAUSS POINT',&
            '9TH USER VARIABLE  212 GAUSS POINT',&
            '1ST USER VARIABLE  312 GAUSS POINT',&
            '2ND USER VARIABLE  312 GAUSS POINT',&
            '3RD USER VARIABLE  312 GAUSS POINT',&
            '4TH USER VARIABLE  312 GAUSS POINT',&
            '5TH USER VARIABLE  312 GAUSS POINT',&
            '6TH USER VARIABLE  312 GAUSS POINT',&
            '7TH USER VARIABLE  312 GAUSS POINT',&
            '8TH USER VARIABLE  312 GAUSS POINT',&
            '9TH USER VARIABLE  312 GAUSS POINT',&
            '1ST USER VARIABLE  122 GAUSS POINT',&
            '2ND USER VARIABLE  122 GAUSS POINT',&
            '3RD USER VARIABLE  122 GAUSS POINT',&
            '4TH USER VARIABLE  122 GAUSS POINT',&
            '5TH USER VARIABLE  122 GAUSS POINT',&
            '6TH USER VARIABLE  122 GAUSS POINT',&
            '7TH USER VARIABLE  122 GAUSS POINT',&
            '8TH USER VARIABLE  122 GAUSS POINT',&
            '9TH USER VARIABLE  122 GAUSS POINT',&
            '1ST USER VARIABLE  222 GAUSS POINT',&
            '2ND USER VARIABLE  222 GAUSS POINT',&
            '3RD USER VARIABLE  222 GAUSS POINT',&
            '4TH USER VARIABLE  222 GAUSS POINT',&
            '5TH USER VARIABLE  222 GAUSS POINT',&
            '6TH USER VARIABLE  222 GAUSS POINT',&
            '7TH USER VARIABLE  222 GAUSS POINT',&
            '8TH USER VARIABLE  222 GAUSS POINT',&
            '8TH USER VARIABLE  222 GAUSS POINT',&
            '1ST USER VARIABLE  322 GAUSS POINT',&
            '2ND USER VARIABLE  322 GAUSS POINT',&
            '3RD USER VARIABLE  322 GAUSS POINT',&
            '4TH USER VARIABLE  322 GAUSS POINT',&
            '5TH USER VARIABLE  322 GAUSS POINT',&
            '6TH USER VARIABLE  322 GAUSS POINT',&
            '7TH USER VARIABLE  322 GAUSS POINT',&
            '8TH USER VARIABLE  322 GAUSS POINT',&
            '9TH USER VARIABLE  322 GAUSS POINT',&
            '1ST USER VARIABLE  132 GAUSS POINT',&
            '2ND USER VARIABLE  132 GAUSS POINT',&
            '3RD USER VARIABLE  132 GAUSS POINT',&
            '4TH USER VARIABLE  132 GAUSS POINT',&
            '5TH USER VARIABLE  132 GAUSS POINT',&
            '6TH USER VARIABLE  132 GAUSS POINT',&
            '7TH USER VARIABLE  132 GAUSS POINT',&
            '8TH USER VARIABLE  132 GAUSS POINT',&
            '9TH USER VARIABLE  132 GAUSS POINT',&
            '1ST USER VARIABLE  232 GAUSS POINT',&
            '2ND USER VARIABLE  232 GAUSS POINT',&
            '3RD USER VARIABLE  232 GAUSS POINT',&
            '4TH USER VARIABLE  232 GAUSS POINT',&
            '5TH USER VARIABLE  232 GAUSS POINT',&
            '6TH USER VARIABLE  232 GAUSS POINT',&
            '7TH USER VARIABLE  232 GAUSS POINT',&
            '8TH USER VARIABLE  232 GAUSS POINT',&
            '9TH USER VARIABLE  232 GAUSS POINT',&
            '1ST USER VARIABLE  332 GAUSS POINT',&
            '2ND USER VARIABLE  332 GAUSS POINT',&
            '3RD USER VARIABLE  332 GAUSS POINT',&
            '4TH USER VARIABLE  332 GAUSS POINT',&
            '5TH USER VARIABLE  332 GAUSS POINT',&
            '6TH USER VARIABLE  332 GAUSS POINT',&
            '7TH USER VARIABLE  332 GAUSS POINT',&
            '8TH USER VARIABLE  332 GAUSS POINT',&
            '9TH USER VARIABLE  332 GAUSS POINT',&
            '1ST USER VARIABLE  142 GAUSS POINT',&
            '2ND USER VARIABLE  142 GAUSS POINT',&
            '3RD USER VARIABLE  142 GAUSS POINT',&
            '4TH USER VARIABLE  142 GAUSS POINT',&
            '5TH USER VARIABLE  142 GAUSS POINT',&
            '6TH USER VARIABLE  142 GAUSS POINT',&
            '7TH USER VARIABLE  142 GAUSS POINT',&
            '8TH USER VARIABLE  142 GAUSS POINT',&
            '9TH USER VARIABLE  142 GAUSS POINT',&
            '1ST USER VARIABLE  242 GAUSS POINT',&
            '2ND USER VARIABLE  242 GAUSS POINT',&
            '3RD USER VARIABLE  242 GAUSS POINT',&
            '4TH USER VARIABLE  242 GAUSS POINT',&
            '5TH USER VARIABLE  242 GAUSS POINT',&
            '6TH USER VARIABLE  242 GAUSS POINT',&
            '7TH USER VARIABLE  242 GAUSS POINT',&
            '8TH USER VARIABLE  242 GAUSS POINT',&
            '9TH USER VARIABLE  242 GAUSS POINT',&
            '1ST USER VARIABLE  342 GAUSS POINT',&
            '2ND USER VARIABLE  342 GAUSS POINT',&
            '3RD USER VARIABLE  342 GAUSS POINT',&
            '4TH USER VARIABLE  342 GAUSS POINT',&
            '5TH USER VARIABLE  342 GAUSS POINT',&
            '6TH USER VARIABLE  342 GAUSS POINT',&
            '7TH USER VARIABLE  342 GAUSS POINT',&
            '8TH USER VARIABLE  342 GAUSS POINT',&
            '9TH USER VARIABLE  342 GAUSS POINT',&
            '1ST USER VARIABLE  152 GAUSS POINT',&
            '2ND USER VARIABLE  152 GAUSS POINT',&
            '3RD USER VARIABLE  152 GAUSS POINT',&
            '4TH USER VARIABLE  152 GAUSS POINT',&
            '5TH USER VARIABLE  152 GAUSS POINT',&
            '6TH USER VARIABLE  152 GAUSS POINT',&
            '7TH USER VARIABLE  152 GAUSS POINT',&
            '8TH USER VARIABLE  152 GAUSS POINT',&
            '9TH USER VARIABLE  152 GAUSS POINT',&
            '1ST USER VARIABLE  252 GAUSS POINT',&
            '2ND USER VARIABLE  252 GAUSS POINT',&
            '3RD USER VARIABLE  252 GAUSS POINT',&
            '4TH USER VARIABLE  252 GAUSS POINT',&
            '5TH USER VARIABLE  252 GAUSS POINT',&
            '6TH USER VARIABLE  252 GAUSS POINT',&
            '7TH USER VARIABLE  252 GAUSS POINT',&
            '8TH USER VARIABLE  252 GAUSS POINT',&
            '9TH USER VARIABLE  252 GAUSS POINT',&
            '1ST USER VARIABLE  352 GAUSS POINT',&
            '2RD USER VARIABLE  352 GAUSS POINT',&
            '3TH USER VARIABLE  352 GAUSS POINT',&
            '4TH USER VARIABLE  352 GAUSS POINT',&
            '5TH USER VARIABLE  352 GAUSS POINT',&
            '6TH USER VARIABLE  352 GAUSS POINT',&
            '7TH USER VARIABLE  352 GAUSS POINT',&
            '8TH USER VARIABLE  352 GAUSS POINT',&
            '9TH USER VARIABLE  352 GAUSS POINT',&
            '1ST USER VARIABLE  162 GAUSS POINT',&
            '2ND USER VARIABLE  162 GAUSS POINT',&
            '3RD USER VARIABLE  162 GAUSS POINT',&
            '4TH USER VARIABLE  162 GAUSS POINT',&
            '5TH USER VARIABLE  162 GAUSS POINT',&
            '6TH USER VARIABLE  162 GAUSS POINT',&
            '7TH USER VARIABLE  162 GAUSS POINT',&
            '8TH USER VARIABLE  162 GAUSS POINT',&
            '9TH USER VARIABLE  162 GAUSS POINT',&
            '1ST USER VARIABLE  262 GAUSS POINT',&
            '2ND USER VARIABLE  262 GAUSS POINT',&
            '3RD USER VARIABLE  262 GAUSS POINT',&
            '4TH USER VARIABLE  262 GAUSS POINT',&
            '5TH USER VARIABLE  262 GAUSS POINT',&
            '6TH USER VARIABLE  262 GAUSS POINT',&
            '7TH USER VARIABLE  262 GAUSS POINT',&
            '8TH USER VARIABLE  262 GAUSS POINT',&
            '9TH USER VARIABLE  262 GAUSS POINT',&
            '1ST USER VARIABLE  362 GAUSS POINT',&
            '2ND USER VARIABLE  362 GAUSS POINT',&
            '3RD USER VARIABLE  362 GAUSS POINT',&
            '4TH USER VARIABLE  362 GAUSS POINT',&
            '5TH USER VARIABLE  362 GAUSS POINT',&
            '6TH USER VARIABLE  362 GAUSS POINT',&
            '7TH USER VARIABLE  362 GAUSS POINT',&
            '8TH USER VARIABLE  362 GAUSS POINT',&
            '9TH USER VARIABLE  362 GAUSS POINT',&
            '1ST USER VARIABLE  172 GAUSS POINT',&
            '2ND USER VARIABLE  172 GAUSS POINT',&
            '3RD USER VARIABLE  172 GAUSS POINT',&
            '4TH USER VARIABLE  172 GAUSS POINT',&
            '5TH USER VARIABLE  172 GAUSS POINT',&
            '6TH USER VARIABLE  172 GAUSS POINT',&
            '7TH USER VARIABLE  172 GAUSS POINT',&
            '8TH USER VARIABLE  172 GAUSS POINT',&
            '9TH USER VARIABLE  172 GAUSS POINT',&
            '1ST USER VARIABLE  272 GAUSS POINT',&
            '2ND USER VARIABLE  272 GAUSS POINT',&
            '3RD USER VARIABLE  272 GAUSS POINT',&
            '4TH USER VARIABLE  272 GAUSS POINT',&
            '5TH USER VARIABLE  272 GAUSS POINT',&
            '6TH USER VARIABLE  272 GAUSS POINT',&
            '7TH USER VARIABLE  272 GAUSS POINT',&
            '8TH USER VARIABLE  272 GAUSS POINT',&
            '9TH USER VARIABLE  272 GAUSS POINT',&
            '1ST USER VARIABLE  372 GAUSS POINT',&
            '2ND USER VARIABLE  372 GAUSS POINT',&
            '3RD USER VARIABLE  372 GAUSS POINT',&
            '4TH USER VARIABLE  372 GAUSS POINT',&
            '5TH USER VARIABLE  372 GAUSS POINT',&
            '6TH USER VARIABLE  372 GAUSS POINT',&
            '7TH USER VARIABLE  372 GAUSS POINT',&
            '8TH USER VARIABLE  372 GAUSS POINT',&
            '9TH USER VARIABLE  372 GAUSS POINT',&
            '1ST USER VARIABLE  182 GAUSS POINT',&
            '2ND USER VARIABLE  182 GAUSS POINT',&
            '3RD USER VARIABLE  182 GAUSS POINT',&
            '4TH USER VARIABLE  182 GAUSS POINT',&
            '5TH USER VARIABLE  182 GAUSS POINT',&
            '6TH USER VARIABLE  182 GAUSS POINT',&
            '7TH USER VARIABLE  182 GAUSS POINT',&
            '8TH USER VARIABLE  382 GAUSS POINT',&
            '9TH USER VARIABLE  382 GAUSS POINT',&
            '1ST USER VARIABLE  282 GAUSS POINT',&
            '2ND USER VARIABLE  282 GAUSS POINT',&
            '3RD USER VARIABLE  282 GAUSS POINT',&
            '4TH USER VARIABLE  282 GAUSS POINT',&
            '5TH USER VARIABLE  282 GAUSS POINT',&
            '6TH USER VARIABLE  282 GAUSS POINT',&
            '7TH USER VARIABLE  282 GAUSS POINT',&
            '8TH USER VARIABLE  282 GAUSS POINT',&
            '9TH USER VARIABLE  282 GAUSS POINT',&
            '1ST USER VARIABLE  382 GAUSS POINT',&
            '2ND USER VARIABLE  382 GAUSS POINT',&
            '3RD USER VARIABLE  382 GAUSS POINT',&
            '4TH USER VARIABLE  382 GAUSS POINT',&
            '5TH USER VARIABLE  382 GAUSS POINT',&
            '6TH USER VARIABLE  382 GAUSS POINT',&
            '7TH USER VARIABLE  382 GAUSS POINT',&
            '8TH USER VARIABLE  382 GAUSS POINT',&
            '9TH USER VARIABLE  382 GAUSS POINT',&
            '1ST USER VARIABLE  192 GAUSS POINT',&
            '2ND USER VARIABLE  192 GAUSS POINT',&
            '3RD USER VARIABLE  192 GAUSS POINT',&
            '4TH USER VARIABLE  192 GAUSS POINT',&
            '5TH USER VARIABLE  192 GAUSS POINT',&
            '6TH USER VARIABLE  192 GAUSS POINT',&
            '7TH USER VARIABLE  192 GAUSS POINT',&
            '8TH USER VARIABLE  192 GAUSS POINT',&
            '9TH USER VARIABLE  192 GAUSS POINT',&
            '1ST USER VARIABLE  292 GAUSS POINT',&
            '2ND USER VARIABLE  292 GAUSS POINT',&
            '3RD USER VARIABLE  292 GAUSS POINT',&
            '4TH USER VARIABLE  292 GAUSS POINT',&
            '5TH USER VARIABLE  292 GAUSS POINT',&
            '6TH USER VARIABLE  292 GAUSS POINT',&
            '7TH USER VARIABLE  292 GAUSS POINT',&
            '8TH USER VARIABLE  292 GAUSS POINT',&
            '9TH USER VARIABLE  292 GAUSS POINT',&
            '1ST USER VARIABLE  392 GAUSS POINT',&
            '2ND USER VARIABLE  392 GAUSS POINT',&
            '3RD USER VARIABLE  392 GAUSS POINT',&
            '4TH USER VARIABLE  392 GAUSS POINT',&
            '5TH USER VARIABLE  392 GAUSS POINT',&
            '6TH USER VARIABLE  392 GAUSS POINT',&
            '7TH USER VARIABLE  392 GAUSS POINT',&
            '8TH USER VARIABLE  392 GAUSS POINT',&
            '9TH USER VARIABLE  392 GAUSS POINT',&
            '1ST USER VARIABLE  113 GAUSS POINT',&
            '2ND USER VARIABLE  113 GAUSS POINT',&
            '3RD USER VARIABLE  113 GAUSS POINT',&
            '4TH USER VARIABLE  113 GAUSS POINT',&
            '5TH USER VARIABLE  113 GAUSS POINT',&
            '6TH USER VARIABLE  113 GAUSS POINT',&
            '7TH USER VARIABLE  113 GAUSS POINT',&
            '8TH USER VARIABLE  113 GAUSS POINT',&
            '9TH USER VARIABLE  113 GAUSS POINT',&
            '1ST USER VARIABLE  213 GAUSS POINT',&
            '2ND USER VARIABLE  213 GAUSS POINT',&
            '3RD USER VARIABLE  213 GAUSS POINT',&
            '4TH USER VARIABLE  213 GAUSS POINT',&
            '5TH USER VARIABLE  213 GAUSS POINT',&
            '6TH USER VARIABLE  213 GAUSS POINT',&
            '7TH USER VARIABLE  213 GAUSS POINT',&
            '8TH USER VARIABLE  213 GAUSS POINT',&
            '9TH USER VARIABLE  213 GAUSS POINT',&
            '1ST USER VARIABLE  313 GAUSS POINT',&
            '3ND USER VARIABLE  313 GAUSS POINT',&
            '3RD USER VARIABLE  313 GAUSS POINT',&
            '4TH USER VARIABLE  313 GAUSS POINT',&
            '5TH USER VARIABLE  313 GAUSS POINT',&
            '6TH USER VARIABLE  313 GAUSS POINT',&
            '7TH USER VARIABLE  313 GAUSS POINT',&
            '8TH USER VARIABLE  313 GAUSS POINT',&
            '9TH USER VARIABLE  313 GAUSS POINT',&
            '1ST USER VARIABLE  123 GAUSS POINT',&
            '2ND USER VARIABLE  123 GAUSS POINT',&
            '3RD USER VARIABLE  123 GAUSS POINT',&
            '4TH USER VARIABLE  123 GAUSS POINT',&
            '5TH USER VARIABLE  123 GAUSS POINT',&
            '6TH USER VARIABLE  123 GAUSS POINT',&
            '7TH USER VARIABLE  123 GAUSS POINT',&
            '8TH USER VARIABLE  123 GAUSS POINT',&
            '9TH USER VARIABLE  123 GAUSS POINT',&
            '1ST USER VARIABLE  223 GAUSS POINT',&
            '2ND USER VARIABLE  223 GAUSS POINT',&
            '3RD USER VARIABLE  223 GAUSS POINT',&
            '4TH USER VARIABLE  223 GAUSS POINT',&
            '5TH USER VARIABLE  223 GAUSS POINT',&
            '6TH USER VARIABLE  223 GAUSS POINT',&
            '7TH USER VARIABLE  223 GAUSS POINT',&
            '8TH USER VARIABLE  223 GAUSS POINT',&
            '9TH USER VARIABLE  223 GAUSS POINT',&
            '1ST USER VARIABLE  323 GAUSS POINT',&
            '2ND USER VARIABLE  323 GAUSS POINT',&
            '3RD USER VARIABLE  323 GAUSS POINT',&
            '4TH USER VARIABLE  323 GAUSS POINT',&
            '5TH USER VARIABLE  323 GAUSS POINT',&
            '6TH USER VARIABLE  323 GAUSS POINT',&
            '7TH USER VARIABLE  323 GAUSS POINT',&
            '8TH USER VARIABLE  323 GAUSS POINT',&
            '9TH USER VARIABLE  323 GAUSS POINT',&
            '1ST USER VARIABLE  133 GAUSS POINT',&
            '2ND USER VARIABLE  133 GAUSS POINT',&
            '3RD USER VARIABLE  133 GAUSS POINT',&
            '4TH USER VARIABLE  133 GAUSS POINT',&
            '5TH USER VARIABLE  133 GAUSS POINT',&
            '6TH USER VARIABLE  133 GAUSS POINT',&
            '7TH USER VARIABLE  133 GAUSS POINT',&
            '8TH USER VARIABLE  133 GAUSS POINT',&
            '9TH USER VARIABLE  133 GAUSS POINT',&
            '1ST USER VARIABLE  233 GAUSS POINT',&
            '2ND USER VARIABLE  233 GAUSS POINT',&
            '3RD USER VARIABLE  233 GAUSS POINT',&
            '4TH USER VARIABLE  233 GAUSS POINT',&
            '5TH USER VARIABLE  233 GAUSS POINT',&
            '6TH USER VARIABLE  233 GAUSS POINT',&
            '7TH USER VARIABLE  233 GAUSS POINT',&
            '8TH USER VARIABLE  233 GAUSS POINT',&
            '9TH USER VARIABLE  233 GAUSS POINT',&
            '1ST USER VARIABLE  333 GAUSS POINT',&
            '2ND USER VARIABLE  333 GAUSS POINT',&
            '3RD USER VARIABLE  333 GAUSS POINT',&
            '4TH USER VARIABLE  333 GAUSS POINT',&
            '5TH USER VARIABLE  333 GAUSS POINT',&
            '6TH USER VARIABLE  333 GAUSS POINT',&
            '7TH USER VARIABLE  333 GAUSS POINT',&
            '8TH USER VARIABLE  333 GAUSS POINT',&
            '9TH USER VARIABLE  333 GAUSS POINT',&
            '1ST USER VARIABLE  143 GAUSS POINT',&
            '2ND USER VARIABLE  143 GAUSS POINT',&
            '3RD USER VARIABLE  143 GAUSS POINT',&
            '4TH USER VARIABLE  143 GAUSS POINT',&
            '5TH USER VARIABLE  143 GAUSS POINT',&
            '6TH USER VARIABLE  143 GAUSS POINT',&
            '7TH USER VARIABLE  143 GAUSS POINT',&
            '8TH USER VARIABLE  143 GAUSS POINT',&
            '9TH USER VARIABLE  143 GAUSS POINT',&
            '1ST USER VARIABLE  243 GAUSS POINT',&
            '2ND USER VARIABLE  243 GAUSS POINT',&
            '3RD USER VARIABLE  243 GAUSS POINT',&
            '4TH USER VARIABLE  243 GAUSS POINT',&
            '5TH USER VARIABLE  243 GAUSS POINT',&
            '6TH USER VARIABLE  243 GAUSS POINT',&
            '7TH USER VARIABLE  243 GAUSS POINT',&
            '8TH USER VARIABLE  243 GAUSS POINT',&
            '9TH USER VARIABLE  243 GAUSS POINT',&
            '1ST USER VARIABLE  343 GAUSS POINT',&
            '2ND USER VARIABLE  343 GAUSS POINT',&
            '3RD USER VARIABLE  343 GAUSS POINT',&
            '4TH USER VARIABLE  343 GAUSS POINT',&
            '5TH USER VARIABLE  343 GAUSS POINT',&
            '6TH USER VARIABLE  343 GAUSS POINT',&
            '7TH USER VARIABLE  343 GAUSS POINT',&
            '8TH USER VARIABLE  343 GAUSS POINT',&
            '9TH USER VARIABLE  343 GAUSS POINT',&
            '1ST USER VARIABLE  153 GAUSS POINT',&
            '2ND USER VARIABLE  153 GAUSS POINT',&
            '3RD USER VARIABLE  153 GAUSS POINT',&
            '4TH USER VARIABLE  153 GAUSS POINT',&
            '5TH USER VARIABLE  153 GAUSS POINT',&
            '6TH USER VARIABLE  153 GAUSS POINT',&
            '7TH USER VARIABLE  153 GAUSS POINT',&
            '8TH USER VARIABLE  153 GAUSS POINT',&
            '9TH USER VARIABLE  153 GAUSS POINT',&
            '1ST USER VARIABLE  253 GAUSS POINT',&
            '2ND USER VARIABLE  253 GAUSS POINT',&
            '3RD USER VARIABLE  253 GAUSS POINT',&
            '4TH USER VARIABLE  253 GAUSS POINT',&
            '5TH USER VARIABLE  253 GAUSS POINT',&
            '6TH USER VARIABLE  253 GAUSS POINT',&
            '7TH USER VARIABLE  253 GAUSS POINT',&
            '8TH USER VARIABLE  253 GAUSS POINT',&
            '9TH USER VARIABLE  253 GAUSS POINT',&
            '1ST USER VARIABLE  353 GAUSS POINT',&
            '2ND USER VARIABLE  353 GAUSS POINT',&
            '3RD USER VARIABLE  353 GAUSS POINT',&
            '4TH USER VARIABLE  353 GAUSS POINT',&
            '5TH USER VARIABLE  353 GAUSS POINT',&
            '6TH USER VARIABLE  353 GAUSS POINT',&
            '7TH USER VARIABLE  353 GAUSS POINT',&
            '8TH USER VARIABLE  353 GAUSS POINT',&
            '9TH USER VARIABLE  353 GAUSS POINT',&
            '1ST USER VARIABLE  163 GAUSS POINT',&
            '2ND USER VARIABLE  163 GAUSS POINT',&
            '3RD USER VARIABLE  163 GAUSS POINT',&
            '4TH USER VARIABLE  163 GAUSS POINT',&
            '5TH USER VARIABLE  163 GAUSS POINT',&
            '6TH USER VARIABLE  163 GAUSS POINT',&
            '7TH USER VARIABLE  163 GAUSS POINT',&
            '8TH USER VARIABLE  163 GAUSS POINT',&
            '9TH USER VARIABLE  163 GAUSS POINT',&
            '1ST USER VARIABLE  263 GAUSS POINT',&
            '2ND USER VARIABLE  263 GAUSS POINT',&
            '3RD USER VARIABLE  263 GAUSS POINT',&
            '4TH USER VARIABLE  263 GAUSS POINT',&
            '5TH USER VARIABLE  263 GAUSS POINT',&
            '6TH USER VARIABLE  263 GAUSS POINT',&
            '7TH USER VARIABLE  263 GAUSS POINT',&
            '8TH USER VARIABLE  263 GAUSS POINT',&
            '9TH USER VARIABLE  263 GAUSS POINT',&
            '1ST USER VARIABLE  363 GAUSS POINT',&
            '2ND USER VARIABLE  363 GAUSS POINT',&
            '3RD USER VARIABLE  363 GAUSS POINT',&
            '4TH USER VARIABLE  363 GAUSS POINT',&
            '5TH USER VARIABLE  363 GAUSS POINT',&
            '6TH USER VARIABLE  363 GAUSS POINT',&
            '7TH USER VARIABLE  363 GAUSS POINT',&
            '8TH USER VARIABLE  363 GAUSS POINT',&
            '9TH USER VARIABLE  363 GAUSS POINT',&
            '1ST USER VARIABLE  173 GAUSS POINT',&
            '2ND USER VARIABLE  173 GAUSS POINT',&
            '3RD USER VARIABLE  173 GAUSS POINT',&
            '4TH USER VARIABLE  173 GAUSS POINT',&
            '5TH USER VARIABLE  173 GAUSS POINT',&
            '6TH USER VARIABLE  173 GAUSS POINT',&
            '7TH USER VARIABLE  173 GAUSS POINT',&
            '8TH USER VARIABLE  173 GAUSS POINT',&
            '9TH USER VARIABLE  173 GAUSS POINT',&
            '1ST USER VARIABLE  273 GAUSS POINT',&
            '2ND USER VARIABLE  273 GAUSS POINT',&
            '3RD USER VARIABLE  273 GAUSS POINT',&
            '4TH USER VARIABLE  273 GAUSS POINT',&
            '5TH USER VARIABLE  273 GAUSS POINT',&
            '6TH USER VARIABLE  273 GAUSS POINT',&
            '7TH USER VARIABLE  273 GAUSS POINT',&
            '8TH USER VARIABLE  273 GAUSS POINT',&
            '9TH USER VARIABLE  273 GAUSS POINT',&
            '1ST USER VARIABLE  373 GAUSS POINT',&
            '2ND USER VARIABLE  373 GAUSS POINT',&
            '3RD USER VARIABLE  373 GAUSS POINT',&
            '4TH USER VARIABLE  373 GAUSS POINT',&
            '5TH USER VARIABLE  373 GAUSS POINT',&
            '6TH USER VARIABLE  373 GAUSS POINT',&
            '7TH USER VARIABLE  373 GAUSS POINT',&
            '8TH USER VARIABLE  373 GAUSS POINT',&
            '9TH USER VARIABLE  373 GAUSS POINT',&
            '1ST USER VARIABLE  183 GAUSS POINT',&
            '2ND USER VARIABLE  183 GAUSS POINT',&
            '3RD USER VARIABLE  183 GAUSS POINT',&
            '4TH USER VARIABLE  183 GAUSS POINT',&
            '5TH USER VARIABLE  183 GAUSS POINT',&
            '6TH USER VARIABLE  183 GAUSS POINT',&
            '7TH USER VARIABLE  183 GAUSS POINT',&
            '8TH USER VARIABLE  183 GAUSS POINT',&
            '9TH USER VARIABLE  183 GAUSS POINT',&
            '1ST USER VARIABLE  283 GAUSS POINT',&
            '2ND USER VARIABLE  283 GAUSS POINT',&
            '3RD USER VARIABLE  283 GAUSS POINT',&
            '4TH USER VARIABLE  283 GAUSS POINT',&
            '5TH USER VARIABLE  283 GAUSS POINT',&
            '6TH USER VARIABLE  283 GAUSS POINT',&
            '7TH USER VARIABLE  283 GAUSS POINT',&
            '8TH USER VARIABLE  283 GAUSS POINT',&
            '9TH USER VARIABLE  283 GAUSS POINT',&
            '1ST USER VARIABLE  383 GAUSS POINT',&
            '2ND USER VARIABLE  383 GAUSS POINT',&
            '3RD USER VARIABLE  383 GAUSS POINT',&
            '4TH USER VARIABLE  383 GAUSS POINT',&
            '5TH USER VARIABLE  383 GAUSS POINT',&
            '6TH USER VARIABLE  383 GAUSS POINT',&
            '7TH USER VARIABLE  383 GAUSS POINT',&
            '8TH USER VARIABLE  383 GAUSS POINT',&
            '9TH USER VARIABLE  383 GAUSS POINT',&
            '1ST USER VARIABLE  193 GAUSS POINT',&
            '2ND USER VARIABLE  193 GAUSS POINT',&
            '3RD USER VARIABLE  193 GAUSS POINT',&
            '4TH USER VARIABLE  193 GAUSS POINT',&
            '5TH USER VARIABLE  193 GAUSS POINT',&
            '6TH USER VARIABLE  193 GAUSS POINT',&
            '7TH USER VARIABLE  193 GAUSS POINT',&
            '8TH USER VARIABLE  193 GAUSS POINT',&
            '9TH USER VARIABLE  193 GAUSS POINT',&
            '1ST USER VARIABLE  293 GAUSS POINT',&
            '2ND USER VARIABLE  293 GAUSS POINT',&
            '3RD USER VARIABLE  293 GAUSS POINT',&
            '4TH USER VARIABLE  293 GAUSS POINT',&
            '5TH USER VARIABLE  293 GAUSS POINT',&
            '6TH USER VARIABLE  293 GAUSS POINT',&
            '7TH USER VARIABLE  293 GAUSS POINT',&
            '8TH USER VARIABLE  293 GAUSS POINT',&
            '9TH USER VARIABLE  293 GAUSS POINT',&
            '1ST USER VARIABLE  393 GAUSS POINT',&
            '2ND USER VARIABLE  393 GAUSS POINT',&
            '3RD USER VARIABLE  393 GAUSS POINT',&
            '4TH USER VARIABLE  393 GAUSS POINT',&
            '5TH USER VARIABLE  393 GAUSS POINT',&
            '6TH USER VARIABLE  393 GAUSS POINT',&
            '7TH USER VARIABLE  393 GAUSS POINT',&
            '8TH USER VARIABLE  393 GAUSS POINT',&
            '9TH USER VARIABLE  393 GAUSS POINT',&
            'GLOBAL STRAIN XX',&
            'GLOBAL STRAIN YY',&
            'GLOBAL STRAIN ZZ',&
            'GLOBAL STRAIN XY',&
            'GLOBAL STRAIN ZX',&
            'GLOBAL STRAIN YZ'/)

          vars5_title = (/&
            character(len=100) ::&
            'SIGMA-X OF THE 1ST MATERIAL',&
            'SIGMA-Y OF THE 1ST MATERIAL',&
            'SIGMA-Z OF THE 1ST MATERIAL',&
            'SIGMA-XY OF THE 1ST MATERIAL',&
            'SIGMA-YZ OF THE 1ST MATERIAL',&
            'SIGMA-XZ OF THE 1ST MATERIAL',&
            'INTERNAL ENERGY OF THE 1ST MATERIAL',&
            'DENSITY OF THE 1ST MATERIAL',&
            'VOLUME OF THE 1ST MATERIAL',&
            'PLASTIC STRAIN OF THE 1ST MATERIAL',&
            'TEMPERATURE OF THE 1ST MATERIAL',&
            'SIGMA-X OF THE 2ND MATERIAL',&
            'SIGMA-Y OF THE 2ND MATERIAL',&
            'SIGMA-Z OF THE 2ND MATERIAL',&
            'SIGMA-XY OF THE 2ND MATERIAL',&
            'SIGMA-YZ OF THE 2ND MATERIAL',&
            'SIGMA-XZ OF THE 2ND MATERIAL',&
            'INTERNAL ENERGY OF THE 2ND MATERIAL',&
            'DENSITY OF THE 2ND MATERIAL',&
            'VOLUME OF THE 2ND MATERIAL',&
            'PLASTIC STRAIN OF THE 2ND MATERIAL',&
            'TEMPERATURE OF THE 2ND MATERIAL'/)

          do i=1,200
            if (i <= 9) then
              write(chi,'(a2,i1.1)')'00',i
            else if (i <= 99) then
              write(chi,'(a1,i2.2)')'0',i
            else
              write(chi,'(i3.3)')i
            endif
            varn1a_title(3*(i-1)+1)  = 'INTERPLY '//chi//' RELATIVE DISPLACEMENT IN X DIRECTION'
            varn1a_title(3*(i-1)+2)  = 'INTERPLY '//chi//' RELATIVE DISPLACEMENT IN Y DIRECTION'
            varn1a_title(3*(i-1)+3)  = 'INTERPLY '//chi//' RELATIVE DISPLACEMENT IN Z DIRECTION'
          enddo

          do j=1,200
            if (j <= 9) then
              write(chj,'(a2,i1.1)')'00',j
            else if (j <= 99) then
              write(chj,'(a1,i2.2)')'0',j
            else
              write(chj,'(i3.3)')j
            endif
            do i=1,9
              write(chi,'(i1.1)')i
              do k=1,9
                write(chk,'(i1.1)')k
                var_tmp=chi(1:1)//'-'//chj//'-'//chk
                vars6_title((j-1)*9*9*6+((i-1)*9+k-1)*6+1) = 'LOCAL 11 STRAIN INTEGRATION POINT '//var_tmp
                vars6_title((j-1)*9*9*6+((i-1)*9+k-1)*6+2) = 'LOCAL 12 STRAIN INTEGRATION POINT '//var_tmp
                vars6_title((j-1)*9*9*6+((i-1)*9+k-1)*6+3) = 'LOCAL 13 STRAIN INTEGRATION POINT '//var_tmp
                vars6_title((j-1)*9*9*6+((i-1)*9+k-1)*6+4) = 'LOCAL 22 STRAIN INTEGRATION POINT '//var_tmp
                vars6_title((j-1)*9*9*6+((i-1)*9+k-1)*6+5) = 'LOCAL 23 STRAIN INTEGRATION POINT '//var_tmp
                vars6_title((j-1)*9*9*6+((i-1)*9+k-1)*6+6) = 'LOCAL 33 STRAIN INTEGRATION POINT '//var_tmp
              enddo
            enddo
          enddo

          do j=1,200
            if (j <= 9) then
              write(chj,'(a2,i1.1)')'00',j
            else if (j <= 99) then
              write(chj,'(a1,i2.2)')'0',j
            else
              write(chj,'(i3.3)')j
            endif
            do i=1,9
              write(chi,'(i1.1)')i
              do k=1,9
                write(chk,'(i1.1)')k
                var_tmp=chi(1:1)//'-'//chj//'-'//chk
                vars7_title((j-1)*9*9*6+((i-1)*9+k-1)*6+1) = 'LOCAL 11 STRESS INTEGRATION POINT '//var_tmp
                vars7_title((j-1)*9*9*6+((i-1)*9+k-1)*6+2) = 'LOCAL 12 STRESS INTEGRATION POINT '//var_tmp
                vars7_title((j-1)*9*9*6+((i-1)*9+k-1)*6+3) = 'LOCAL 13 STRESS INTEGRATION POINT '//var_tmp
                vars7_title((j-1)*9*9*6+((i-1)*9+k-1)*6+4) = 'LOCAL 22 STRESS INTEGRATION POINT '//var_tmp
                vars7_title((j-1)*9*9*6+((i-1)*9+k-1)*6+5) = 'LOCAL 23 STRESS INTEGRATION POINT '//var_tmp
                vars7_title((j-1)*9*9*6+((i-1)*9+k-1)*6+6) = 'LOCAL 33 STRESS INTEGRATION POINT '//var_tmp
              enddo
            enddo
          enddo


          vars8_title(1)  = 'LOCAL AVERAGE STRAIN-X'
          vars8_title(2)  = 'LOCAL AVERAGE STRAIN-Y'
          vars8_title(3)  = 'LOCAL AVERAGE STRAIN-Z'
          vars8_title(4)  = 'LOCAL AVERAGE STRAIN-XY'
          vars8_title(5)  = 'LOCAL AVERAGE STRAIN-YZ'
          vars8_title(6)  = 'LOCAL AVERAGE STRAIN-ZX'
          vars8_title(7)  = 'STRAIN-X 1ST GAUSS POINT (TETRA)'
          vars8_title(8)  = 'STRAIN-X 2ND GAUSS POINT (TETRA)'
          vars8_title(9)  = 'STRAIN-X 3RD GAUSS POINT (TETRA)'
          vars8_title(10) = 'STRAIN-X 4TH GAUSS POINT (TETRA)'
          vars8_title(11) = 'STRAIN-Y 1ST GAUSS POINT (TETRA)'
          vars8_title(12) = 'STRAIN-Y 2ND GAUSS POINT (TETRA)'
          vars8_title(13) = 'STRAIN-Y 3RD GAUSS POINT (TETRA)'
          vars8_title(14) = 'STRAIN-Y 4TH GAUSS POINT (TETRA)'
          vars8_title(15) = 'STRAIN-Z 1ST GAUSS POINT (TETRA)'
          vars8_title(16) = 'STRAIN-Z 2ND GAUSS POINT (TETRA)'
          vars8_title(17) = 'STRAIN-Z 3RD GAUSS POINT (TETRA)'
          vars8_title(18) = 'STRAIN-Z 4TH GAUSS POINT (TETRA)'
          vars8_title(19) = 'STRAIN-XY 1ST GAUSS POINT (TETRA)'
          vars8_title(20) = 'STRAIN-XY 2ND GAUSS POINT (TETRA)'
          vars8_title(21) = 'STRAIN-XY 3RD GAUSS POINT (TETRA)'
          vars8_title(22) = 'STRAIN-XY 4TH GAUSS POINT (TETRA)'
          vars8_title(23) = 'STRAIN-YZ 1ST GAUSS POINT (TETRA)'
          vars8_title(24) = 'STRAIN-YZ 2ND GAUSS POINT (TETRA)'
          vars8_title(25) = 'STRAIN-YZ 3RD GAUSS POINT (TETRA)'
          vars8_title(26) = 'STRAIN-YZ 4TH GAUSS POINT (TETRA)'
          vars8_title(27) = 'STRAIN-ZX 1ST GAUSS POINT (TETRA)'
          vars8_title(28) = 'STRAIN-ZX 2ND GAUSS POINT (TETRA)'
          vars8_title(29) = 'STRAIN-ZX 3RD GAUSS POINT (TETRA)'
          vars8_title(30) = 'STRAIN-ZX 4TH GAUSS POINT (TETRA)'

          do k=1,3
            do j=1,9
              do i=1,3
                write(chi,'(i1.1)')i
                write(chjs,'(i1.1)')j
                write(chk,'(i1.1)')k
                write(chi,'(i1.1)')i
                write(chjs,'(i1.1)')j
                write(chk,'(i1.1)')k
                vars8_title(30+(k-1)*9*6*3+(j-1)*3*6+(i-1)*6+1)='GLOBAL STRAIN-X  INTEGRATION POINT '//&
                  chi(1:1)//'-'//chjs//'-'//chk
                vars8_title(30+(k-1)*9*6*3+(j-1)*3*6+(i-1)*6+2)='GLOBAL STRAIN-Y  INTEGRATION POINT '//&
                  chi(1:1)//'-'//chjs//'-'//chk
                vars8_title(30+(k-1)*9*6*3+(j-1)*3*6+(i-1)*6+3)='GLOBAL STRAIN-Z  INTEGRATION POINT '//&
                  chi(1:1)//'-'//chjs//'-'//chk
                vars8_title(30+(k-1)*9*6*3+(j-1)*3*6+(i-1)*6+4)='GLOBAL STRAIN-XY INTEGRATION POINT '//&
                  chi(1:1)//'-'//chjs//'-'//chk
                vars8_title(30+(k-1)*9*6*3+(j-1)*3*6+(i-1)*6+5)='GLOBAL STRAIN-YZ INTEGRATION POINT '//&
                  chi(1:1)//'-'//chjs//'-'//chk
                vars8_title(30+(k-1)*9*6*3+(j-1)*3*6+(i-1)*6+6)='GLOBAL STRAIN-ZX INTEGRATION POINT '//&
                  chi(1:1)//'-'//chjs//'-'//chk
              enddo
            enddo
          enddo

          vars9_title = (/&
            character(len=100) ::&
            'VELOCITY-X',&
            'VELOCITY-Y',&
            'VELOCITY-Z',&
            'SPEED OF SOUND',&
            'MACH NUMBER',&
            'YIELD SCALE FACTOR FROM FAILURE SURFACE'/)

          varsnloc_title = (/&
            character(len=100) ::&
            'NON LOCAL PLASTIC STRAIN',&
            'NON LOCAL PLASTIC STRAIN RATE'/)

! shells
          varc_title(1)  = 'STRESS DIRECTION 1'
          varc_title(2)  = 'STRESS DIRECTION 2'
          varc_title(3)  = 'SHEAR STRESS DIRECTION 12'
          varc_title(4)  = 'SHEAR STRESS DIRECTION 13'
          varc_title(5)  = 'SHEAR STRESS DIRECTION 23'
          varc_title(6)  = 'MOMENT M1'
          varc_title(7)  = 'MOMENT M2'
          varc_title(8)  = 'MOMENT M12'
          varc_title(9)  = 'MEMBRANE ENERGY'
          varc_title(10)  = 'BENDING  ENERGY'
          varc_title(11)  = 'ELEMENT FLAG'
          varc_title(12)  = 'THICKNESS'
          varc_title(13)  = 'MIN PLASTIC STRAIN'
          varc_title(14)  = 'MAX PLASTIC STRAIN'
          varc_title(15)  = 'MEMBRANE STRAIN E1'
          varc_title(16)  = 'MEMBRANE STRAIN E2'
          varc_title(17)  = 'MEMBRANE STRAIN E12'
          varc_title(18)  = 'SHEAR STRAIN SH1'
          varc_title(19)  = 'SHEAR STRAIN SH2'
          varc_title(20)  = 'CURVATURE K1'
          varc_title(21)  = 'CURVATURE K2'
          varc_title(22)  = 'CURVATURE K12'
          varc_title(23)  = 'STRAIN RATE'

          do i=1,60
            if (i <= 9) then
              write(chi,'(i1.1)')i
            else if (i <= 99) then
              write(chi,'(i2.2)')i
            endif
            varc_title(23+i)='USER VARIABLE '//chi
          enddo

          do i=1,20
            if (i <= 9) then
              write(chi,'(i1.1)')i
            else if (i <= 99) then
              write(chi,'(i2.2)')i
            endif
            do j=1,5
              write(chj,'(i1.1)')j
              varc_title(83+(i-1)*5+j)='USER VARIABLE '//chi//' INTEGR. POINT '//chj
            enddo
          enddo

          varc_title(184)='SIGMA-X FOR 1ST INTEGRATION POINT '
          varc_title(185)='SIGMA-Y FOR 1ST INTEGRATION POINT '
          varc_title(186)='SIGMA-XY FOR 1ST INTEGRATION POINT '
          varc_title(187)='SIGMA-YZ FOR 1ST INTEGRATION POINT '
          varc_title(188)='SIGMA-ZX FOR 1ST INTEGRATION POINT '
          varc_title(189)='SIGMA-X FOR 2ND INTEGRATION POINT '
          varc_title(190)='SIGMA-Y FOR 2ND INTEGRATION POINT '
          varc_title(191)='SIGMA-XY FOR 2ND INTEGRATION POINT '
          varc_title(192)='SIGMA-YZ FOR 2ND INTEGRATION POINT '
          varc_title(193)='SIGMA-ZX FOR 2ND INTEGRATION POINT '
          varc_title(194)='SIGMA-X FOR 3RD INTEGRATION POINT '
          varc_title(195)='SIGMA-Y FOR 3RD INTEGRATION POINT '
          varc_title(196)='SIGMA-XY FOR 3RD INTEGRATION POINT '
          varc_title(197)='SIGMA-YZ FOR 3RD INTEGRATION POINT '
          varc_title(198)='SIGMA-ZX FOR 3RD INTEGRATION POINT '
          varc_title(199)='SIGMA-X FOR 4TH INTEGRATION POINT '
          varc_title(200)='SIGMA-Y FOR 4TH INTEGRATION POINT '
          varc_title(201)='SIGMA-XY FOR 4TH INTEGRATION POINT '
          varc_title(202)='SIGMA-YZ FOR 4TH INTEGRATION POINT '
          varc_title(203)='SIGMA-ZX FOR 4TH INTEGRATION POINT '
          varc_title(204)='SIGMA-X FOR 5TH INTEGRATION POINT '
          varc_title(205)='SIGMA-Y FOR 5TH INTEGRATION POINT '
          varc_title(206)='SIGMA-XY FOR 5TH INTEGRATION POINT '
          varc_title(207)='SIGMA-YZ FOR 5TH INTEGRATION POINT '
          varc_title(208)='SIGMA-ZX FOR 5TH INTEGRATION POINT '


          do i=6,99
            j = 0
            if (i <= 9) then
              write(chi,'(i1.1)')i
              j = 1
            else if (i <= 99) then
              write(chi,'(i2.2)')i
              j = 2
            endif
            varc_title(208+(i-6)*5+1)='SIGMA-X FOR  '//chi(1:j)//'TH INTEGRATION POINT'
            varc_title(208+(i-6)*5+2)='SIGMA-Y FOR  '//chi(1:j)//'TH INTEGRATION POINT'
            varc_title(208+(i-6)*5+3)='SIGMA-XY FOR  '//chi(1:j)//'TH INTEGRATION POINT'
            varc_title(208+(i-6)*5+4)='SIGMA-YZ FOR  '//chi(1:j)//'TH INTEGRATION POINT'
            varc_title(208+(i-6)*5+5)='SIGMA-ZX FOR  '//chi(1:j)//'TH INTEGRATION POINT'
          enddo

          do i=1,20
            if (i <= 9) then
              write(chi,'(i1.1)')i
            else if (i <= 99) then
              write(chi,'(i2.2)')i
            endif
            do j=6,99
              if (j <= 9) then
                write(chj,'(i1.1)')j
                k=1
              else if (j <= 99) then
                write(chj,'(i2.2)')j
                k=2
              endif
              varc_title(678+(i-1)*94+j-5)='USER VARIABLE '//chi//' INTEGR. POINT '//chj(1:k)
            enddo
          enddo

          do i=21,60
            if (i <= 9) then
              write(chi,'(i1.1)')i
            else if (i <= 99) then
              write(chi,'(i2.2)')i
            endif
            do j=1,99
              k = 0
              if (j <= 9) then
                write(chj,'(i1.1)')j
                k=1
              else if (j <= 99) then
                write(chj,'(i2.2)')j
                k=2
              endif
              varc_title(2558+(i-21)*99+j)='USER VARIABLE '//chi//' INTEGR. POINT '//chj(1:k)
            enddo
          enddo

          do k=1,99
            if (k <= 9) then
              write(chkk,'(i1.1)')k
            else if (k <= 99) then
              write(chkk,'(i2.2)')k
            endif
            do j=1,4
              write(chj,'(i1.1)')j
              do i=1,60
                if (i <= 9) then
                  write(chi,'(i1.1)')i
                else if (i <= 99) then
                  write(chi,'(i2.2)')i
                endif
                varc_title(6518+i+(k-1)*240+(j-1)*60)='USER VARIABLE '//chi//' INTEGR. POINT '//chj//' LAYER '//chkk
              enddo
            enddo
          enddo


          varc_title(30279)='TOTAL NUMBER OF FAILED LAYERS'
          varc_title(30280)='PERCENTAGE OF FAILED LAYERS'
          varc_title(30281)='NUMBER OF LAYERS WHICH REACHED FAILURE LEVEL DIR. 1'
          varc_title(30282)='NUMBER OF LAYERS WHICH REACHED FAILURE LEVEL DIR. 2'
          varc_title(30283)='NUMBER OF LAYERS WHICH REACHED FAILURE LEVEL PLASTIC WORK'


          DO I=1,99
            j = 0
            if (i <= 9) then
              write(chi,'(i1.1)')i
              j = 1
            else if (i <= 99) then
              write(chi,'(i2.2)')i
              j = 2
            endif
            varc_title(30283+i)='PLASTIC WORK LAYER '//chi(1:j)
          enddo


          do i=1,100
            j = 0
            if (i <= 9) then
              write(chi,'(i1.1)')i
              j = 1
            else if (i <= 99) then
              write(chi,'(i2.2)')i
              j = 2
            else
              write(chi,'(i3.3)')i
              j = 3
            endif
            varc_title(30382+(i-1)*5+1)='VISCOUS SIGMA-X FOR INTEGR. POINT '//chi(1:j)
            varc_title(30382+(i-1)*5+2)='VISCOUS SIGMA-Y FOR INTEGR. POINT '//chi(1:j)
            varc_title(30382+(i-1)*5+3)='VISCOUS SIGMA-XY FOR INTEGR. POINT '//chi(1:j)
            varc_title(30382+(i-1)*5+4)='VISCOUS SIGMA-YZ FOR INTEGR. POINT '//chi(1:j)
            varc_title(30382+(i-1)*5+5)='VISCOUS SIGMA-ZX FOR INTEGR. POINT '//chi(1:j)
          enddo


          do kk = 1, 199
            ii = 30882 + (kk -1)*(6*4 + 11)
            l = 0
            if (kk <= 9) then
              write(chjj,'(i1.1)')kk
              l = 1
            else if (kk <= 99) then
              write(chjj,'(i2.2)')kk
              l = 2
            else
              write(chjj,'(i3.3)')kk
              l = 3
            endif

            do j = 1,4
              jj = j*1000 + kk
              write(chj ,'(i1.1)')j
              varc_title(ii + 1 ) = 'SIGMA-Z INTEGRATION POINT '//chj//'INTER LAYER '//chjj(1:l)
              varc_title(ii + 2 ) = 'SIGMA-YZ INTEGRATION POINT '//chj//'INTER LAYER '//chjj(1:l)
              varc_title(ii + 3 ) = 'SIGMA-XZ INTEGRATION POINT '//chj//'INTER LAYER '//chjj(1:l)
              varc_title(ii + 4 ) = 'EPS-Z INTEGRATION POINT '//chj//'INTER LAYER '//chjj(1:l)
              varc_title(ii + 5 ) ='EPS-YZ INTEGRATION POINT '//chj//'INTER LAYER '//chjj(1:l)
              varc_title(ii + 6 ) ='EPS-XZ INTEGRATION POINT '//chj//'INTER LAYER '//chjj(1:l)
              ii = ii + 6
            enddo
            varc_title(ii + 1 ) = 'AVERAGE SIGMA-Z INTER LAYER '//chjj(1:l)
            varc_title(ii + 2 ) = 'AVERAGE SIGMA-YZ INTER LAYER '//chjj(1:l)
            varc_title(ii + 3 ) = 'AVERAGE SIGMA-XZ INTER LAYER '//chjj(1:l)
            varc_title(ii + 4 ) = 'AVERAGE EPS-Z INTER LAYER '//chjj(1:l)
            varc_title(ii + 5 ) = 'AVERAGE EPS-YZ INTER LAYER '//chjj(1:l)
            varc_title(ii + 6 ) = 'AVERAGE EPS-XZ INTER LAYER '//chjj(1:l)
            ii =ii +6
            varc_title(ii + 1 ) = 'MAX DAMAGE INTER LAYER '//chjj(1:l)
            varc_title(ii + 2 ) = 'MAX DAMAGE 1 INTER LAYER '//chjj(1:l)
            varc_title(ii + 3 ) = 'MAX DAMAGE 2 INTER LAYER '//chjj(1:l)
            varc_title(ii + 4 ) = 'MAX DAMAGE 3 INTER LAYER '//chjj(1:l)
            ii = ii + 4
            varc_title(ii + 1 ) = 'INTERNAL ENERGY INTER LAYER '//chjj(1:l)
            ii = ii + 1
          enddo

          varc_title(37848)='PINCHING EPS-XZ'
          varc_title(37849)='PINCHING EPS-YZ'
          varc_title(37850)='PINCHING EPS-ZZ'
          varc_title(37851)='PINCHING SIGMA-Z'
          varc_title(37852)='PINCHING SIGMA-XZ'
          varc_title(37853)='PINCHING SIGMA-ZZ'
          varc_title(37854)='PINCHING THICKNESS'
          varc_title(37855)='NON LOCAL PLASTIC STRAIN'
          varc_title(37856)='NON LOCAL PLASTIC STRAIN RATE'



! trusses
          vart_title = (/&
            character(len=100) ::&
            'ELEMENT FLAG',&
            'NORMAL FORCE',&
            'INTERNAL ENERGY',&
            'AREA',&
            'INITIAL LENGTH',&
            'PLASTIC STRAIN'/)

!  beams
          varp_title(1)='ELEMENT FLAG'
          varp_title(2)='NORMAL FORCE'
          varp_title(3)='SHEAR FORCE 12'
          varp_title(4)='SHEAR FORCE 13'
          varp_title(5)='TORSIONAL MOMENT'
          varp_title(6)='BENDING MOMENT 12'
          varp_title(7)='BENDING MOMENT 13'
          varp_title(8)='INTERNAL ENERGY'
          varp_title(9)='STRESS XX'
          varp_title(10)='STRESS XY'
          varp_title(11)='STRESS ZX'

          do i=1,81
            if (i <= 9) then
              write(chi,'(i1.1)')i
            else if (i <= 99) then
              write(chi,'(i2.2)')i
            endif
            varp_title(11+3*(I-1)+1)='STRESS XX INTEGRATION POINT '//chi
            varp_title(11+3*(I-1)+2)='STRESS XY INTEGRATION POINT '//chi
            varp_title(11+3*(I-1)+3)='STRESS ZX INTEGRATION POINT '//chi
          enddo

          varp_title(255)='PLASTIC STRAIN'

          do i=1,81
            if (i <= 9) then
              write(chi,'(i1.1)')i
            else if (i <= 99) then
              write(chi,'(i2.2)')i
            endif
            varp_title(255+i)='PLASTIC STRAIN INTEGRATION POINT '//chi
          enddo

          varp_title(337)='STRAIN RATE'


!   springs
          varr_title = (/&
            character(len=100) ::&
            'ELEMENT FLAG',&
            'FORCE X',&
            'FORCE Y',&
            'FORCE Z',&
            'MOMENT X',&
            'MOMENT Y',&
            'MOMENT Z',&
            'ELONGATION X',&
            'ELONGATION Y',&
            'ELONGATION Z',&
            'ROTATION X',&
            'ROTATION Y',&
            'ROTATION Z',&
            'INTERNAL ENERGY',&
            'FORCE STRAND 1-2',&
            'FORCE STRAND 2-3',&
            'FORCE_X Node 1 (GLOGAL FRAME)',&
            'FORCE_Y Node 1 (GLOGAL FRAME)',&
            'FORCE_Z Node 1 (GLOGAL FRAME)',&
            'FORCE_X Node 2 (GLOGAL FRAME)',&
            'FORCE_Y Node 2 (GLOGAL FRAME)',&
            'FORCE_Z Node 2 (GLOGAL FRAME)',&
            'FORCE_X Node 1 (LOCAL  FRAME)',&
            'FORCE_Y Node 1 (LOCAL  FRAME)',&
            'FORCE_Z Node 1 (LOCAL  FRAME)',&
            'FORCE_X Node 2 (LOCAL  FRAME)',&
            'FORCE_Y Node 2 (LOCAL  FRAME)',&
            'FORCE_Z Node 2 (LOCAL  FRAME)',&
            'MOMENT_X Node 1 (GLOGAL FRAME)',&
            'MOMENT_Y Node 1 (GLOGAL FRAME)',&
            'MOMENT_Z Node 1 (GLOGAL FRAME)',&
            'MOMENT_X Node 2 (GLOGAL FRAME)',&
            'MOMENT_Y Node 2 (GLOGAL FRAME)',&
            'MOMENT_Z Node 2 (GLOGAL FRAME)',&
            'MOMENT_X Node 1 (LOCAL  FRAME)',&
            'MOMENT_Y Node 1 (LOCAL  FRAME)',&
            'MOMENT_Z Node 1 (LOCAL  FRAME)',&
            'MOMENT_X Node 2 (LOCAL  FRAME)',&
            'MOMENT_Y Node 2 (LOCAL  FRAME)',&
            'MOMENT_Z Node 2 (LOCAL  FRAME)',&
            'DEFORM_X Node 1 (GLOGAL FRAME)',&
            'DEFORM_Y Node 1 (GLOGAL FRAME)',&
            'DEFORM_Z Node 1 (GLOGAL FRAME)',&
            'DEFORM_X Node 2 (GLOGAL FRAME)',&
            'DEFORM_Y Node 2 (GLOGAL FRAME)',&
            'DEFORM_Z Node 2 (GLOGAL FRAME)',&
            'DEFORM_X Node 1 (LOCAL FRAME)',&
            'DEFORM_Y Node 1 (LOCAL FRAME)',&
            'DEFORM_Z Node 1 (LOCAL FRAME)',&
            'DEFORM_X Node 2 (LOCAL FRAME)',&
            'DEFORM_Y Node 2 (LOCAL FRAME)',&
            'DEFORM_Z Node 2 (LOCAL FRAME)',&
            'ROT_X    Node 1 (GLOBAL FRAME)',&
            'ROT_Y    Node 1 (GLOBAL FRAME)',&
            'ROT_Z    Node 1 (GLOBAL FRAME)',&
            'ROT_X    Node 2 (GLOBAL FRAME)',&
            'ROT_Y    Node 2 (GLOBAL FRAME)',&
            'ROT_Z    Node 2 (GLOBAL FRAME)',&
            'ROT_X    Node 1 (LOCAL FRAME)',&
            'ROT_Y    Node 1 (LOCAL FRAME)',&
            'ROT_Z    Node 1 (LOCAL FRAME)',&
            'ROT_X    Node 2 (LOCAL FRAME)',&
            'ROT_Y    Node 2 (LOCAL FRAME)',&
            'ROT_Z    Node 2 (LOCAL FRAME)',&
            'SPRING LENGTH',&
            'DAMAGE FACTOR'/)
! nstrand
          varns_title = (/&
            character(len=100) ::&
            'ELEMENT FLAG',&
            'FORCE X',&
            'ELONGATION X',&
            'INTERNAL ENERGY'/)
! sph
          varsph_title = (/&
            character(len=100) ::&
            'ELEMENT FLAG',&
            'SIGMA-X',&
            'SIGMA-Y',&
            'SIGMA-Z',&
            'SIGMA-XY',&
            'SIGMA-YZ',&
            'SIGMA-XZ',&
            'INTERNAL ENERGY',&
            'DENSITY',&
            'BULK VISCOSITY',&
            'VOLUME',&
            'PLASTIC STRAIN',&
            'TEMPERATURE',&
            'STRAIN RATE',&
            'TENS. DAMAGE DIR 1',&
            'TENS. DAMAGE DIR 2',&
            'TENS. DAMAGE DIR 3',&
            'TSAI WU YIELD FUNC.',&
            'SUM OF DAMAGES',&
            'STRESS REINF. DIR-1',&
            'STRESS REINF. DIR-2',&
            'STRESS REINF. DIR-3',&
            'VOLUME OF OPEN CRACKS',&
            'CAP PARAMETER',&
            'PLASTIC PARAMETER',&
            'TURBULENT ENERGY',&
            'TURBULENT DISSIPATION',&
            'FIBER STRAIN',&
            'PHASE STATE',&
            'EQ. VOL. PLASTIC STRAIN',&
            'BURN FRACTION',&
            'PLASTIC WORK',&
            'STRESS IN FIBER',&
            'TENSILE DAMAGE IN DIR 23',&
            'AVERAGE LOCAL SIGMA-X',&
            'AVERAGE LOCAL SIGMA-Y',&
            'AVERAGE LOCAL SIGMA-Z',&
            'AVERAGE LOCAL SIGMA-XY',&
            'AVERAGE LOCAL SIGMA-YZ',&
            'AVERAGE LOCAL SIGMA-XZ',&
            'DIAMETER'/)
          !   interfaces
          varin_title = (/&
            character(len=100) ::&
            'X-NORMAL IMPULSE',&
            'Y-NORMAL IMPULSE',&
            'Z-NORMAL IMPULSE',&
            'X-TANGENT IMPULSE',&
            'Y-TANGENT IMPULSE',&
            'Z-TANGENT IMPULSE',&
            'INTERFACE FORCES WORK',&
            'SUM_OF_ABSOLUTE_X_NORMAL_IMPULSES',&
            'SUM_OF_ABSOLUTE_Y_NORMAL_IMPULSES',&
            'SUM_OF_ABSOLUTE_Z_NORMAL_IMPULSES',&
            'SUM_OF_NORMAL_IMPULSE_NORMS',&
            'SUM_OF_ABSOLUTE_X_TOTAL_IMPULSES',&
            'SUM_OF_ABSOLUTE_Y_TOTAL_IMPULSES',&
            'SUM_OF_ABSOLUTE_Z_TOTAL_IMPULSES',&
            'SUM_OF_TOTAL_IMPULSE_NORMS',&
            'PVOL',&
            'PSURF',&
            'PMED',&
            'DELTAP',&
            'VOL',&
            'SURF',&
            'X TORQUS (IMPULSE)',&
            'Y TORQUS (IMPULSE)',&
            'Z TORQUS (IMPULSE)',&
            'FRICTIONAL ENERGY CONVERTED INTO HEAT',&
            'ELASTIC CONTACT ENERGY',&
            'FRICTIONAL CONTACT ENERGY',&
            'DAMPING CONTACT ENERGY',&
            'CONTACT AREA'/)
!   rigid wall
          varrw_title = (/&
            character(len=100) ::&
            'X-NORMAL IMPULSE',&
            'Y-NORMAL IMPULSE',&
            'Z-NORMAL IMPULSE',&
            'X-TANGENT IMPULSE',&
            'Y-TANGENT IMPULSE',&
            'Z-TANGENT IMPULSE'/)
!   rigid body
          varrb_title = (/&
            character(len=100) ::&
            'X-IMPULSE',&
            'Y-IMPULSE',&
            'Z-IMPULSE',&
            'X-MOMENT IMPULSE',&
            'Y-MOMENT IMPULSE',&
            'Z-MOMENT IMPULSE',&
            'ROTATION X',&
            'ROTATION Y',&
            'ROTATION Z',&
            'X-INTERFACE IMPULSE',&
            'Y-INTERFACE IMPULSE',&
            'Z-INTERFACE IMPULSE',&
            'X-INTERFACE MOMENT IMPULSE',&
            'Y-INTERFACE MOMENT IMPULSE',&
            'Z-INTERFACE MOMENT IMPULSE'/)
!   flexible body
          varfx_title = (/&
            character(len=100) ::&
            'INTERNAL ENERGY',&
            'KINETIC ENERGY',&
            'EXTERNAL WORK',&
            'DAMPING ENERGY'/)
!   accel
          varac_title = (/&
            character(len=100) ::&
            'X-ACCELERATION',&
            'Y-ACCELERATION',&
            'Z-ACCELERATION',&
            'X-INTEGRAL OF ACCELERATION',&
            'Y-INTEGRAL OF ACCELERATION',&
            'Z-INTEGRAL OF ACCELERATION'/)
!   section
          varse_title = (/&
            character(len=100) ::&
            'X-NORMAL IMPULSE',&
            'Y-NORMAL IMPULSE',&
            'Z-NORMAL IMPULSE',&
            'X-TANGENT IMPULSE',&
            'Y-TANGENT IMPULSE',&
            'Z-TANGENT IMPULSE',&
            '1-MOMENT IMPULSE',&
            '2-MOMENT IMPULSE',&
            '3-MOMENT IMPULSE',&
            'WORK F+M',&
            'FX-ERROR ',&
            'FY-ERROR ',&
            'FZ-ERROR ',&
            'F-ERROR (QUADRATIC) ',&
            'WORK M',&
            'MX-ERROR ',&
            'MY-ERROR ',&
            'MZ-ERROR ',&
            'M-ERROR (QUADRATIC)',&
            'TRA. KINETIC ENERGY (SECTION NODES)',&
            'ROT. KINETIC ENERGY (SECTION NODES)',&
            'MVX-ERROR',&
            'MVY-ERROR',&
            'MVZ-ERROR',&
            'ERROR TRA. KIN. ENERGY',&
            'MVRX-ERROR',&
            'MVRY-ERROR',&
            'MVRZ-ERROR',&
            'ERROR ROT. KIN. ENERGY',&
            'EXTERNAL WORK',&
            'X-MOMENT IMPULSE',&
            'Y-MOMENT IMPULSE',&
            'Z-MOMENT IMPULSE',&
            '1-IMPULSE',&
            '2-IMPULSE',&
            '3-IMPULSE',&
            'X-COORDINATE OF SECTION CENTER',&
            'Y-COORDINATE OF SECTION CENTER',&
            'Z-COORDINATE OF SECTION CENTER'/)
!   joint cyl
          varjo_title = (/&
            character(len=100) ::&
            'FORCE X',&
            'FORCE Y',&
            'FORCE Z',&
            'MOMENT X',&
            'MOMENT Y',&
            'MOMENT Z'/)
!   monitored volume
          VARMV_TITLE = (/&
            character(len=100) ::&
            'MASS',&
            'VOLUME',&
            'PRESSURE',&
            'AREA',&
            'TEMPERATURE',&
            'VENT AREA',&
            'VENT VELOCITY',&
            'COMM AREA',&
            'COMM VELOCITY',&
            'AVERAGED CP',&
            'AVERAGED CV',&
            'EQUIVALENT GAMA',&
            'TIME STEP',&
            'NUMBER OF FINITE VOLUMES',&
            'INJECTED MASS',&
            'INJECTED ENTHALPY',&
            'INTERNAL ENERGY',&
            'PRESSURE FORCE WORK',&
            'CRITERIA VALUE FOR UP SWITCH'/)
! vent
          varmvent_title = (/&
            character(len=100) ::&
            'MASS',&
            'VOLUME',&
            'PRESSURE',&
            'AREA',&
            'TEMPERATURE',&
            'VENT AREA',&
            'VENT VELOCITY',&
            'COMM AREA',&
            'COMM VELOCITY',&
            'AVERAGED CP',&
            'AVERAGED CV',&
            'EQUIVALENT GAMA',&
            'TIME STEP',&
            'NUMBER OF FINITE VOLUMES',&
            'INJECTED MASS',&
            'INJECTED ENTHALPY',&
            'INTERNAL ENERGY',&
            'PRESSURE FORCE WORK',&
            'CRITERIA VALUE FOR UP SWITCH',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            '',&
            'NON CLOSED VENT AREA A V1  ',&
            'NON CLOSED VENT AREA B V1  ',&
            'OUTGOING VEL V1  ',&
            'OUTGOING MASS V1 ',&
            'OUTGOING ENTHALPY V1   ',&
            'NON CLOSED VENT AREA A V2  ',&
            'NON CLOSED VENT AREA B V2  ',&
            'OUTGOING VEL V2 ',&
            'OUTGOING MASS V2  ',&
            'OUTGOING ENTHALPY V2   ',&
            'NON CLOSED VENT AREA A V3  ',&
            'NON CLOSED VENT AREA B V3  ',&
            'OUTGOING VEL V3  ',&
            'OUTGOING MASS V3  ',&
            'OUTGOING ENTHALPY V3    ',&
            'NON CLOSED VENT AREA A V4  ',&
            'NON CLOSED VENT AREA B V4  ',&
            'OUTGOING VEL V4  ',&
            'OUTGOING MASS V4  ',&
            'OUTGOING ENTHALPY V4    ',&
            'NON CLOSED VENT AREA A V5  ',&
            'NON CLOSED VENT AREA B V5  ',&
            'OUTGOING VEL V5  ',&
            'OUTGOING MASS V5  ',&
            'OUTGOING ENTHALPY V5     ',&
            'NON CLOSED VENT AREA A V6  ',&
            'NON CLOSED VENT AREA B V6  ',&
            'OUTGOING VEL V6  ',&
            'OUTGOING MASS V6  ',&
            'OUTGOING ENTHALPY V6    ',&
            'NON CLOSED VENT AREA A V7  ',&
            'NON CLOSED VENT AREA B V7  ',&
            'OUTGOING VEL V7  ',&
            'OUTGOING MASS V7  ',&
            'OUTGOING ENTHALPY V7    ',&
            'NON CLOSED VENT AREA A V8  ',&
            'NON CLOSED VENT AREA B V8  ',&
            'OUTGOING VEL V8  ',&
            'OUTGOING MASS V8  ',&
            'OUTGOING ENTHALPY V8    ',&
            'NON CLOSED VENT AREA A V9  ',&
            'NON CLOSED VENT AREA B V9  ',&
            'OUTGOING VEL V9  ',&
            'OUTGOING MASS V9  ',&
            'OUTGOING ENTHALPY V9   ',&
            'NON CLOSED VENT AREA A V10  ',&
            'NON CLOSED VENT AREA B V10  ',&
            'OUTGOING VEL V10  ',&
            'OUTGOING MASS V10  ',&
            'OUTGOING ENTHALPY V10 '/)
!   part
          varpa_title = (/&
            character(len=100) ::&
            'INTERNAL ENERGY',&
            'KINETIC ENERGY',&
            'X-MOMENTUM',&
            'Y-MOMENTUM',&
            'Z-MOMENTUM',&
            'MASS',&
            'HOURGLASS ENERGY',&
            'TURBULENT ENERGY',&
            'CENTER OF GRAVITY X-COORDINATE',&
            'CENTER OF GRAVITY Y-COORDINATE',&
            'CENTER OF GRAVITY Z-COORDINATE',&
            'XX-MOMENTUM',&
            'YY-MOMENTUM',&
            'ZZ-MOMENTUM',&
            'XX INERTIA',&
            'YY INERTIA',&
            'ZZ INERTIA',&
            'XY INERTIA',&
            'YZ INERTIA',&
            'ZX INERTIA',&
            'SHEAR INTERNAL ENERGY',&
            'TRANSLATIONAL RIGID BODY KINETIC ENERGY',&
            'ROTATIONAL RIGID BODY KINETIC ENERGY',&
            'ROTATIONAL KINETIC ENERGY',&
            'NUMBER OF DELETED ELEMENTS',&
            '',&
            '',&
            'STORED HEAT',&
            'AVERAGE X-VELOCITY',&
            'AVERAGE Y-VELOCITY',&
            'AVERAGE Z-VELOCITY'/)
! frame
          varfr_title = (/&
            character(len=100) ::&
            'ORIGIN X-COORDINATE',&
            'ORIGIN Y-COORDINATE',&
            'ORIGIN Z-COORDINATE',&
            'ORIENTATION MATRIX COMPONENT 1,1',&
            'ORIENTATION MATRIX COMPONENT 1,2',&
            'ORIENTATION MATRIX COMPONENT 1,3',&
            'ORIENTATION MATRIX COMPONENT 2,1',&
            'ORIENTATION MATRIX COMPONENT 2,2',&
            'ORIENTATION MATRIX COMPONENT 2,3',&
            'ORIENTATION MATRIX COMPONENT 3,1',&
            'ORIENTATION MATRIX COMPONENT 3,2',&
            'ORIENTATION MATRIX COMPONENT 3,3',&
            'TRANSLATIONAL VELOCITY X-COMPONENT',&
            'TRANSLATIONAL VELOCITY Y-COMPONENT',&
            'TRANSLATIONAL VELOCITY Z-COMPONENT',&
            'INSTANTANEOUS ROTATIONAL VELOCITY X-COMPONENT',&
            'INSTANTANEOUS ROTATIONAL VELOCITY Y-COMPONENT',&
            'INSTANTANEOUS ROTATIONAL VELOCITY Z-COMPONENT',&
            'TRANSLATIONAL ACCELERATION X-COMPONENT',&
            'TRANSLATIONAL ACCELERATION Y-COMPONENT',&
            'TRANSLATIONAL ACCELERATION Z-COMPONENT',&
            'ROTATIONAL ACCELERATION X-COMPONENT',&
            'ROTATIONAL ACCELERATION Y-COMPONENT',&
            'ROTATIONAL ACCELERATION Z-COMPONENT'/)
! gauge
          vargau_title = (/&
            character(len=100) ::&
            'PRESSURE',&
            'SPECIFIC ENERGY',&
            'DENSITY',&
            'TEMPERATURE',&
            'STAGNATION PRESSURE',&
            'X-STAGNATION PRESSURE',&
            'Y-STAGNATION PRESSURE',&
            'Z-STAGNATION PRESSURE'/)
! cluster
          varclus_title = (/&
            character(len=100) ::&
            'FORCE X',&
            'FORCE Y',&
            'FORCE Z',&
            'MOMENT X',&
            'MOMENT Y',&
            'MOMENT Z',&
            'SHEAR FORCE',&
            'NORMAL FORCE',&
            'BENDING MOMENT',&
            'TORSIONAL MOMENT',&
            'DAMAGE FACTOR'/)

          varflow_title = (/&
            character(len=100) ::&
            'CUMULATED MASS CROSSING THE SURFACE'/)
! surf
          varsurf_title = (/&
            character(len=100) ::&
            'SURFACE AREA',&
            'CUMULATED MASS CROSSING THE SURFACE (FVMBAG1,COMMU1)',&
            'FLOW VELOCITY ACROSS THE SURFACE (FVMBAG1,COMMU1)',&
            'AVERAGE EXTERNAL PRESSURE APPLIED ON THE SURFACE',&
            'SURFACE AREA WHERE EXTERNAL PRESSURE IS APPLIED'/)
! slipring
          varslip_title = (/&
            character(len=100) ::&
            'SEATBELT LENGTH CROSSING SLIPRING',&
            'NORMAL FORCE',&
            'AXIAL FORCE IN STRAND 1',&
            'AXIAL FORCE IN STRAND 2',&
            'ANGLE BETWEEN STRANDS',&
            'ORIENTATION ANGLE'/)
! retractor
          varret_title = (/&
            character(len=100) ::&
            'SEATBELT LENGTH PULLED OUT',&
            'RETRACTOR FORCE',&
            'LOCKED(1.0)/UNCLOKED(0.0)'/)
! sensors
          varsens_title= (/&
            character(len=100) ::&
            'SENSOR STATUS'/)

          ! ----------------------------------------------------------------------------------------------------------------------
        end subroutine th_titles
      end module th_titles_mod

