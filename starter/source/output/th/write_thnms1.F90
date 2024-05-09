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
      module write_thnms1_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \write th.nms1 file containing th index->name correspondance
        subroutine write_thnms1(nvarn1        ,nvarn1a       ,nvarn2           ,nvarnpinch        ,nvars1           ,&
          nvars2        ,nvars3        ,nvars4           ,nvars5            ,nvars6           ,&
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
          varsurf_title ,varsens_title,&
          varn1         ,varn1a        ,varn2            ,varnpinch         ,&
          varp          ,varr          ,vart             ,vars1             ,vars2            ,&
          vars3         ,vars4         ,vars5            ,vars6             ,vars7            ,&
          vars8         ,vars9         ,varsnloc         ,&
          varc          ,&
          varns         ,varsph        ,varin            ,&
          varrw         ,varrb         ,varmv            ,varse             ,varac            ,&
          varjo         ,varmvent      ,varpa            ,varfx             ,vargau           ,&
          varfr         ,varslip       ,varret           ,varclus           ,varflow          ,&
          varsurf       ,varsens)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use write_thnms1_titles_mod
          use write_thnms1_empty_titles_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
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

          character(len=100),                        intent(in) :: varn1_title(nvarn1)
          character(len=100),                        intent(in) :: varn1a_title(nvarn1a)
          character(len=100),                        intent(in) :: varn2_title(nvarn2)
          character(len=100),                        intent(in) :: varnpinch_title(nvarnpinch)
          character(len=100),                        intent(in) :: varp_title(nvarp)
          character(len=100),                        intent(in) :: varr_title(nvarr)
          character(len=100),                        intent(in) :: vart_title(nvart)
          character(len=100),                        intent(in) :: vars1_title(nvars1)
          character(len=100),                        intent(in) :: vars2_title(nvars2)
          character(len=100),                        intent(in) :: vars3_title(nvars3)
          character(len=100),                        intent(in) :: vars4_title(nvars4)
          character(len=100),                        intent(in) :: vars5_title(nvars5)
          character(len=100),                        intent(in) :: vars6_title(nvars6)
          character(len=100),                        intent(in) :: vars7_title(nvars7)
          character(len=100),                        intent(in) :: vars8_title(nvars8)
          character(len=100),                        intent(in) :: vars9_title(nvars9)
          character(len=100),                        intent(in) :: varsnloc_title(nvarsnloc)
          character(len=100),                        intent(in) :: varc_title(nvarc)
          character(len=100),                        intent(in) :: varns_title(nvarns)
          character(len=100),                        intent(in) :: varsph_title(nvarsph)
          character(len=100),                        intent(in) :: varin_title(nvarin)
          character(len=100),                        intent(in) :: varrw_title(nvarrw)
          character(len=100),                        intent(in) :: varrb_title(nvarrb)
          character(len=100),                        intent(in) :: varmv_title(nvarmv)
          character(len=100),                        intent(in) :: varse_title(nvarse)
          character(len=100),                        intent(in) :: varac_title(nvarac)
          character(len=100),                        intent(in) :: varjo_title(nvarjo)
          character(len=100),                        intent(in) :: varmvent_title(nvarmvent)
          character(len=100),                        intent(in) :: varpa_title(nvarpa)
          character(len=100),                        intent(in) :: varfx_title(nvarfx)
          character(len=100),                        intent(in) :: vargau_title(nvargau)
          character(len=100),                        intent(in) :: varfr_title(nvarfr)
          character(len=100),                        intent(in) :: varslip_title(nvarslip)
          character(len=100),                        intent(in) :: varret_title(nvarret)
          character(len=100),                        intent(in) :: varclus_title(nvarclus)
          character(len=100),                        intent(in) :: varflow_title(nvarflow)
          character(len=100),                        intent(in) :: varsurf_title(nvarsurf)
          character(len=100),                        intent(in) :: varsens_title(nvarsens)

          character(len=10),                        intent(in) :: varn1(nvarn1)
          character(len=10),                        intent(in) :: varn1a(nvarn1a)
          character(len=10),                        intent(in) :: varn2(nvarn2)
          character(len=10),                        intent(in) :: varnpinch(nvarnpinch)
          character(len=10),                        intent(in) :: varp(nvarp)
          character(len=10),                        intent(in) :: varr(nvarr)
          character(len=10),                        intent(in) :: vart(nvart)
          character(len=10),                        intent(in) :: vars1(nvars1)
          character(len=10),                        intent(in) :: vars2(nvars2)
          character(len=10),                        intent(in) :: vars3(nvars3)
          character(len=10),                        intent(in) :: vars4(nvars4)
          character(len=10),                        intent(in) :: vars5(nvars5)
          character(len=10),                        intent(in) :: vars6(nvars6)
          character(len=10),                        intent(in) :: vars7(nvars7)
          character(len=10),                        intent(in) :: vars8(nvars8)
          character(len=10),                        intent(in) :: vars9(nvars9)
          character(len=10),                        intent(in) :: varsnloc(nvarsnloc)
          character(len=10),                        intent(in) :: varc(nvarc)
          character(len=10),                        intent(in) :: varns(nvarns)
          character(len=10),                        intent(in) :: varsph(nvarsph)
          character(len=10),                        intent(in) :: varin(nvarin)
          character(len=10),                        intent(in) :: varrw(nvarrw)
          character(len=10),                        intent(in) :: varrb(nvarrb)
          character(len=10),                        intent(in) :: varmv(nvarmv)
          character(len=10),                        intent(in) :: varse(nvarse)
          character(len=10),                        intent(in) :: varac(nvarac)
          character(len=10),                        intent(in) :: varjo(nvarjo)
          character(len=10),                        intent(in) :: varmvent(nvarmvent)
          character(len=10),                        intent(in) :: varpa(nvarpa)
          character(len=10),                        intent(in) :: varfx(nvarfx)
          character(len=10),                        intent(in) :: vargau(nvargau)
          character(len=10),                        intent(in) :: varfr(nvarfr)
          character(len=10),                        intent(in) :: varslip(nvarslip)
          character(len=10),                        intent(in) :: varret(nvarret)
          character(len=10),                        intent(in) :: varclus(nvarclus)
          character(len=10),                        intent(in) :: varflow(nvarflow)
          character(len=10),                        intent(in) :: varsurf(nvarsurf)
          character(len=10),                        intent(in) :: varsens(nvarsens)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,l,k,m
          integer :: pos
          integer :: io
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          open(newunit=io, file="th.nms1", status="new", action="write")

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$'
          write(io, *) '$$               FILE NMS release 2024.0'
          write(io, *) '$$'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$    INDEX NAME      DESCRIPTION'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ TIME'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '            TIME      TIME'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ GLOBAL'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '          1 IE        INTERNAL ENERGY'
          write(io, *) '          2 KE        KINETIC ENERGY'
          write(io, *) '          3 XMOM      X-MOMENTUM'
          write(io, *) '          4 YMOM      Y-MOMENTUM'
          write(io, *) '          5 ZMOM      Z-MOMENTUM'
          write(io, *) '          6 MASS      MASS'
          write(io, *) '          7 DT        TIME STEP'
          write(io, *) '          8 RKE       ROTATION ENERGY'
          write(io, *) '          9 EFW       EXTERNAL WORK'
          write(io, *) '         10 SIE       SPRING ENERGY'
          write(io, *) '         11 CE        CONTACT ENERGY'
          write(io, *) '         12 HE        HOURGLASS ENERGY'
          write(io, *) '         13 CE_ELAST  ELASTIC CONTACT ENERGY'
          write(io, *) '         14 CE_FRIC   FRICTIONAL CONTACT ENERGY'
          write(io, *) '         15 CE_DAMP   DAMPING CONTACT ENERGY'


          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ NODAL'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarn1,varn1_title,varn1,0)
          call write_thnms1_titles(io,nvarn1a,varn1a_title,varn1a,nvarn1)
          call write_thnms1_titles(io,nvarn2,varn2_title,varn2,nvarn1+nvarn1a)
          call write_thnms1_titles(io,nvarnpinch,varnpinch_title,varnpinch,nvarn1+nvarn1a+nvarn2)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ BRICK'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvars1,vars1_title,vars1,0)
          call write_thnms1_titles(io,nvars2,vars2_title,vars2,nvars1)
          call write_thnms1_titles(io,nvars3,vars3_title,vars3,nvars1+nvars2)
          call write_thnms1_titles(io,nvars4,vars4_title,vars4,nvars1+nvars2+nvars3)
          call write_thnms1_titles(io,nvars5,vars5_title,vars5,nvars1+nvars2+nvars3+nvars4)
          call write_thnms1_titles(io,nvars6,vars6_title,vars6,nvars1+nvars2+nvars3+nvars4+nvars5)
          call write_thnms1_titles(io,nvars7,vars7_title,vars7,nvars1+nvars2+nvars3+nvars4+nvars5+97200)
          call write_thnms1_empty_titles(io,196047,239030)
          call write_thnms1_titles(io,nvars8,vars8_title,vars8,239030)
          call write_thnms1_titles(io,nvars9,vars9_title,vars9,239030+nvars8)
          call write_thnms1_titles(io,nvarsnloc,varsnloc_title,varsnloc,239030+nvars8+nvars9)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ SHELL'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarc,varc_title,varc,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ SH3N'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarc,varc_title,varc,0)


          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ TRUSS'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvart,vart_title,vart,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ BEAM'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarp,varp_title,varp,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ SPRING'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarr,varr_title,varr,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ NSTRAND'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarns,varns_title,varns,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ SPHCEL'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarsph,varsph_title,varsph,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ INTER'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarin,varin_title,varin,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ RWALL'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarrw,varrw_title,varrw,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ RBODY'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarrb,varrb_title,varrb,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ FXBODY'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarfx,varfx_title,varfx,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ ACCEL'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarac,varac_title,varac,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ SECTIO'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarse,varse_title,varse,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ CYL_JO'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarjo,varjo_title,varjo,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ MONVOL'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarmv,varmv_title,varmv,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ VENT'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarmvent,varmvent_title,varmvent,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ PART, MAT, PROP'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarpa,varpa_title,varpa,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ SUBSET'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarpa,varpa_title,varpa,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ FRAME'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarfr,varfr_title,varfr,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ GAUGE'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvargau,vargau_title,vargau,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ CLUSTER'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarclus,varclus_title,varclus,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ SPH_FLOW'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarflow,varflow_title,varflow,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ SURF'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarsurf,varsurf_title,varsurf,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ TRIA'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvars1,vars1_title,vars1,0)
          call write_thnms1_titles(io,nvars2,vars2_title,vars2,nvars1)
          call write_thnms1_titles(io,nvars3,vars3_title,vars3,nvars1+nvars2)
          call write_thnms1_titles(io,nvars4,vars4_title,vars4,nvars1+nvars2+nvars3)
          call write_thnms1_titles(io,nvars5,vars5_title,vars5,nvars1+nvars2+nvars3+nvars4)
          call write_thnms1_titles(io,nvars6,vars6_title,vars6,nvars1+nvars2+nvars3+nvars4+nvars5)
          call write_thnms1_titles(io,nvars7,vars7_title,vars7,nvars1+nvars2+nvars3+nvars4+nvars5+97200)
          call write_thnms1_empty_titles(io,196047,239030)
          call write_thnms1_titles(io,nvars8,vars8_title,vars8,239030)
          call write_thnms1_titles(io,nvars9,vars9_title,vars9,239030+nvars8)
          call write_thnms1_titles(io,nvarsnloc,varsnloc_title,varsnloc,239030+nvars8+nvars9)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ QUAD'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvars1,vars1_title,vars1,0)
          call write_thnms1_titles(io,nvars2,vars2_title,vars2,nvars1)
          call write_thnms1_titles(io,nvars3,vars3_title,vars3,nvars1+nvars2)
          call write_thnms1_titles(io,nvars4,vars4_title,vars4,nvars1+nvars2+nvars3)
          call write_thnms1_titles(io,nvars5,vars5_title,vars5,nvars1+nvars2+nvars3+nvars4)
          call write_thnms1_titles(io,nvars6,vars6_title,vars6,nvars1+nvars2+nvars3+nvars4+nvars5)
          call write_thnms1_titles(io,nvars7,vars7_title,vars7,nvars1+nvars2+nvars3+nvars4+nvars5+97200)
          call write_thnms1_empty_titles(io,196047,239030)
          call write_thnms1_titles(io,nvars8,vars8_title,vars8,239030)
          call write_thnms1_titles(io,nvars9,vars9_title,vars9,239030+nvars8)
          call write_thnms1_titles(io,nvarsnloc,varsnloc_title,varsnloc,239030+nvars8+nvars9)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ SLIPRING'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarslip,varslip_title,varslip,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ RETRACTOR'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarret,varret_title,varret,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(io, *) '$$ SENSOR'
          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
          call write_thnms1_titles(io,nvarsens,varsens_title,varsens,0)

          write(io, *) '$$ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'


          close(io)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine write_thnms1
      end module write_thnms1_mod
