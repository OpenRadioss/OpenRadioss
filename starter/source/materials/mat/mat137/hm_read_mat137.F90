!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
      module hm_read_mat137_mod
      contains
        subroutine hm_read_mat137(                                             &
          matparam ,nuvar    ,nfunc    ,parmat   ,unitab   ,mat_id   ,titr   , &
          mtag     ,nvartmp  ,lsubmodel,iout     ,ntable   ,table    )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
          use unitab_mod
          use submodel_mod
          use matparam_def_mod
          use elbuftag_mod
          use constant_mod
          use mat_table_copy_mod
          use hm_option_read_mod
          use message_mod
          use precision_mod, only: WP
          use table_mod
          use mat_table_copy_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          type(matparam_struct_) ,intent(inout) :: matparam  !< Material parameters structure
          integer,                intent(inout) :: nuvar     !< Number of user variables
          integer,                intent(inout) :: nfunc     !< Number of functions
          real(kind=WP), dimension(100),intent(inout) :: parmat !< Material parameter local array
          type (unit_type_),      intent(in)    :: unitab    !< Units table
          integer,                intent(in)    :: mat_id    !< Material identification number
          character(len=nchartitle),intent(in)  :: titr      !< Material title
          type(mlaw_tag_),        intent(inout) :: mtag      !< Material tags structure
          integer,                intent(inout) :: nvartmp   !< Number of temporary variables
          type(submodel_data), dimension(nsubmod), intent(in) :: lsubmodel !< Submodel data structure
          integer, intent(in)                   :: iout      !< Output file number
          integer, intent(in)                   :: ntable    !< Size of table data structure
          type(ttable),dimension(ntable),intent(in) :: table !< Table data structure        
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer :: i,fcte,fctnu,fctsy,fcthr,fctat,anopt
          real(kind=WP) :: rho0,x1scale,x2scale,x3scale,x4scale
          real(kind=WP),dimension(5) ::x2vect,x3vect,x4vect,fscale
          real(kind=WP) :: beta,tastrt,taend,tlstrt,tlend,eghost,nughost,      &
            aghost,epsini,t1phase,t2phase,dtemp,tcutoff
          integer :: ilaw
          logical :: is_available,is_encrypted
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
          is_encrypted = .false.
          is_available = .false.
          ilaw = 137
          !---------------------------------------------------------------------
          call hm_option_is_encrypted(is_encrypted)
          !---------------------------------------------------------------------
          !< Density
          call hm_get_floatv('Rho'            ,rho0    ,is_available,lsubmodel,unitab)
          !---------------------------------------------------------------------
          !< 1st line of material card
          call hm_get_intv  ("LSD_LCID4"      ,fcte    ,is_available,lsubmodel)
          call hm_get_intv  ("LSD_LCID5"      ,fctnu   ,is_available,lsubmodel)
          call hm_get_intv  ("LSD_LCID6"      ,fctsy   ,is_available,lsubmodel)
          call hm_get_intv  ("LSD_LCID7"      ,fcthr   ,is_available,lsubmodel)
          call hm_get_intv  ("LSD_LCID8"      ,fctat   ,is_available,lsubmodel)
          !---------------------------------------------------------------------
          !< 2nd line of material card
          call hm_get_floatv('LSD_MAT_BETA'   ,beta    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MATL270_TASTART',tastrt  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MATL270_TAEND'  ,taend   ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MATL270_TLSTART',tlstrt  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MATL270_TLEND'  ,tlend   ,is_available,lsubmodel,unitab)
          !---------------------------------------------------------------------
          !< 3rd line of material card
          call hm_get_floatv('MATL270_EGHOST' ,eghost  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MATL270_PGHOST' ,nughost ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MATL270_AGHOST' ,aghost  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MATL270_T1PHASE',t1phase ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MATL270_T2PHASE',t2phase ,is_available,lsubmodel,unitab)
          !---------------------------------------------------------------------
          !< 4th line of material card
          call hm_get_floatv('EPSINI' ,epsini ,is_available,lsubmodel,unitab)
          call hm_get_floatv('DTEMP'  ,dtemp  ,is_available,lsubmodel,unitab)
          call hm_get_intv  ('ANOPT'  ,anopt  ,is_available,lsubmodel)
          call hm_get_floatv('TCUTOFF',tcutoff,is_available,lsubmodel,unitab)
!
          !---------------------------------------------------------------------
          !< Parameters default values
          !---------------------------------------------------------------------
          if (anopt == 0) then 
            tcutoff = infinity
          elseif (anopt == 1) then
            tcutoff = taend
            if (tcutoff == zero) tcutoff = infinity
          elseif (anopt == 2) then
            if (tcutoff == zero) tcutoff = infinity
          endif
!
          !---------------------------------------------------------------------
          !< Filling buffer tables
          !---------------------------------------------------------------------
          !< Number of integer material parameters
          matparam%niparam = 1
          !< Number of real material parameters
          matparam%nuparam = 13
          !< Number of user variables
          nuvar = 8
          !< Number of functions
          nfunc = 0
          !< Number of tables and temporary variables
          matparam%ntable = 5
          nvartmp = 6
!
          !< Allocation of material parameters tables
          allocate(matparam%iparam(matparam%niparam))
          allocate(matparam%uparam(matparam%nuparam))
          allocate(matparam%table (matparam%ntable ))
!
          !< Material tables scaling factors and vectors
          x1scale = one
          x2scale = one
          x3scale = one
          x4scale = one
          x2vect(1:5) = one
          x3vect(1:5) = zero
          x4vect(1:5) = zero
          fscale(1:5) = one
!
          !< Assign table IDs
          ! -> Classic clipping yield surface
          matparam%table(1)%notable = fcte
          matparam%table(2)%notable = fctnu
          matparam%table(3)%notable = fctat
          matparam%table(4)%notable = fctsy
          matparam%table(5)%notable = fcthr
!
          !< Copy tables into matparam structure
          call mat_table_copy(matparam   ,x2vect   ,x3vect   ,x4vect   ,       &
            x1scale  ,x2scale  ,x3scale  ,x4scale  ,fscale   ,ntable   ,       &
            table    ,ilaw     )
!          
          !< Real material parameters
          matparam%young = maxval(matparam%table(1)%y1d(:))
          matparam%nu    = maxval(matparam%table(2)%y1d(:))
          matparam%shear = matparam%young/(two*(one+matparam%nu))
          matparam%bulk  = matparam%young/(three*(one-two*matparam%nu))
          matparam%therm%func_thexp  = fctat
          matparam%therm%scale_thexp = one
          matparam%iparam(1)  = anopt
          matparam%uparam(1)  = beta
          matparam%uparam(2)  = tastrt
          matparam%uparam(3)  = taend
          matparam%uparam(4)  = tlstrt
          matparam%uparam(5)  = tlend
          matparam%uparam(6)  = eghost
          matparam%uparam(7)  = nughost
          matparam%uparam(8)  = aghost
          matparam%uparam(9)  = epsini
          matparam%uparam(10) = t1phase
          matparam%uparam(11) = t2phase
          matparam%uparam(12) = dtemp
          matparam%uparam(13) = tcutoff
!
          !< PARMAT table
          parmat(1)  = matparam%bulk
          parmat(2)  = matparam%young
          parmat(3)  = matparam%nu
          parmat(16) = 2 
          parmat(17) = two*matparam%shear/                                     &
                       (matparam%bulk+four_over_3*matparam%shear)
!
          !< Initial and reference density
          matparam%rho0 = rho0
          matparam%rho  = rho0
!
          !< MTAG variable activation
          mtag%g_epsd = 1
          mtag%l_epsd = 1
          mtag%g_pla  = 1
          mtag%l_pla  = 1
          mtag%l_sigb = 6
          mtag%g_temp = 1
          mtag%l_temp = 1
          matparam%heat_flag = 1
          mtag%g_forth  = 1
          mtag%g_eintth = 1
          mtag%l_seq = 1
          mtag%g_seq = 1
!
          !< Properties compatibility
          call init_mat_keyword(matparam,"SOLID_ISOTROPIC")
          call init_mat_keyword(matparam,"SHELL_ISOTROPIC")
!
          !< Material model keywords
          call init_mat_keyword(matparam ,"INCREMENTAL"   )
          call init_mat_keyword(matparam ,"LARGE_STRAIN"  )
          call init_mat_keyword(matparam ,"HOOK"          )
          call init_mat_keyword(matparam ,"ISOTROPIC"     )
!
          !---------------------------------------------------------------------
          !< Listing output
          !---------------------------------------------------------------------
          write(iout,1001) trim(titr),mat_id,ilaw
          write(iout,1000)
          if (is_encrypted) then
            write(iout,'(5X,A,//)') 'CONFIDENTIAL DATA'
          else
            write(iout,1002) rho0
            write(iout,1003) fcte,fctnu,matparam%young,matparam%nu,eghost,     &
              nughost
            write(iout,1004) beta,fctsy,fcthr
            write(iout,1005) fctat,aghost,tastrt,taend,tlstrt,tlend,dtemp,     &
              anopt,tcutoff
            write(iout,1006)
          endif
!
          !---------------------------------------------------------------------
          !< Output formats
          !---------------------------------------------------------------------
1000  format(/                                                                 &
        5X,"========================================================",/        &
        5X,"    MATERIAL MODEL: COMPUTATIONAL MECHANICS WELDING     ",/,       &
        5X,"========================================================",/)
1001  format(/                                                                 &
        5X,A,/,                                                                &
        5X,"MATERIAL NUMBER . . . . . . . . . . . . . . . . . . . .=",I10/,    &
        5X,"MATERIAL LAW. . . . . . . . . . . . . . . . . . . . . .=",I10/)
1002  format(/                                                                 &
        5X,"INITIAL DENSITY . . . . . . . . . . . . . . . . . . . .=",1PG20.13/)
1003  format(/                                                                 &
        5X,"ELASTIC PARAMETERS:                                     ",/,       &
        5X,"-------------------                                     ",/,       &
        5X,"YOUNG MODULUS VS TEMP FUNCTION ID (FCT_ET). . . . . . .=",I10/,    &
        5X,"POISSON RATIO VS TEMP FUNCTION ID (FCT_NUT) . . . . . .=",I10/,    &
        5X,"MAXIMUM MEASURED YOUNG MODULUS. . . . . . . . . . . . .=",1PG20.13/&
        5X,"MAXIMUM MEASURED POISSON RATIO. . . . . . . . . . . . .=",1PG20.13/&
        5X,"GHOST MATERIAL YOUNG MODULUS (EGHOST) . . . . . . . . .=",1PG20.13/&
        5X,"GHOST MATERIAL POISSON RATIO (NUGHOST). . . . . . . . .=",1PG20.13/)
1004  format(/                                                                 &
        5X,"PLASTIC PARAMETERS:                                     ",/,       &
        5X,"--------------------                                    ",/,       &
        5X,"BETA PARAMETER . . . . . . . . . . . . . . . . . . . . .",1PG20.13/&
        5X,"YIELD STRESS VS TEMP FUNCTION ID (FCT_SY) . . . . . . .=",I10/,    &
        5X,"HARDENING MODULUS VS TEMP FUNCTION ID (FCT_HR). . . . .=",I10/,    &
        5X,"INITIAL PLASTIC STRAIN (EPSINI) . . . . . . . . . . . .=",1PG20.13)
1005  format(/                                                                 &
        5X,"THERMAL PARAMETERS:                                     ",/,       &
        5X,"--------------------                                    ",/,       &
        5X,"THERMAL EXPANSION COEFF VS TEMP FUNCTION ID (FCT_AT). .=",I10/,    &
        5X,"GHOST MATERIAL THERMAL EXPANSION COEFF (AGHOST) . . . .=",1PG20.13/&
        5X,"TEMPERATURE START OF ANNEALING (TASTRT) . . . . . . . .=",1PG20.13/&
        5X,"TEMPERATURE END OF ANNEALING (TAEND). . . . . . . . . .=",1PG20.13/&
        5X,"TEMPERATURE START OF MATERIAL BIRTH (TLSTRT). . . . . .=",1PG20.13/&
        5X,"TEMPERATURE END OF MATERIAL BIRTH (TLEND) . . . . . . .=",1PG20.13/&
        5X,"MAXIMUM TEMPERATURE INCREMENT (DTEMP) . . . . . . . . .=",1PG20.13/&
        5X,"ANNEALING OPTION FOR THERMAL EXPANSION (ANOPT). . . . .=",I10/     &
        5X,"    = 0: NO OPTION(DEFAULT)                             ",/,       &
        5X,"    = 1: UPPER LIMIT IS TAEND                           ",/,       &
        5X,"    = 2: UPPER LIMIT IS CUTOFF TEMPERATURE              ",/,       &
        5X,"TEMPERATURE CUTOFF FOR THERMAL EXPANSION (TCUTOFF). . .=",1PG20.13/)
1006 format(/                                                                  &
        5X,"========================================================",/)
!
        end subroutine hm_read_mat137
      end module hm_read_mat137_mod
