!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
      module hm_read_elasto_plastic_mod
        implicit none
      contains
        subroutine hm_read_elasto_plastic(                                     &
          matparam ,nvartmp  ,parmat   ,unitab   ,mat_id   ,titr     ,mtag    ,&
          lsubmodel,iout     ,nuvar    ,ilaw     ,israte   ,ntable   ,table   )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use matparam_def_mod
          use elbuftag_mod
          use constant_mod
          use mat_table_copy_mod
          use hm_option_read_mod
          use hm_read_elasticity_mod
          use hm_read_yield_criterion_mod
          use hm_read_work_hardening_mod
          use hm_read_srate_dependency_mod
          use hm_read_therm_softening_mod
          use hm_read_self_heating_mod
          use hm_read_kinematic_hardening_mod
          use table_mod
          use precision_mod, only : WP
          use mat_table_copy_mod
          use message_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          type(matparam_struct_) ,intent(inout)     :: matparam          !< Matparam data structure
          integer, intent(inout)                    :: nvartmp           !< Number of temporary variables
          real(kind=WP), intent(inout)              :: parmat(100)       !< Material parameter global table 1
          type(unit_type_),intent(in)               :: unitab            !< Units table
          integer, intent(in)                       :: mat_id            !< Material law user ID
          character(len=nchartitle),intent(in)      :: titr              !< Material law user title
          type(mlaw_tag_), intent(inout)            :: mtag              !< Material tag for internal variables in element buffer
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel !< Submodel data structure
          integer, intent(in)                       :: iout              !< Output file number
          integer, intent(inout)                    :: nuvar             !< Number of user variables
          integer, intent(in)                       :: ilaw              !< Material law number
          integer, intent(inout)                    :: israte            !< Strain rate filtering flag
          integer, intent(in)                       :: ntable            !< Size of table data structure
          type(ttable),dimension(ntable),intent(in) :: table             !< Table data structure
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          integer :: ielas,icrit,ihard,iratedep,itherm,iheat,ikine
          integer :: nupar_elas,nupar_crit,nupar_hard,nupar_ratedep,           &
            nupar_therm,nupar_heat,nupar_kine
          integer :: ntab_hard,nvartmp_hard,ntab_srate,nvartmp_srate,          &
            ntab_therm,nvartmp_therm,ntab_heat,nvartmp_heat
          integer :: vpflag,ires
          integer, dimension(100) :: itab_hard,itab_srate,itab_therm,itab_heat
          real(kind=WP), dimension(100) :: x2vect_hard,x3vect_hard,x4vect_hard,&
            fscale_hard,x2vect_srate,x3vect_srate,x4vect_srate,fscale_srate,   &
            x2vect_therm,x3vect_therm,x4vect_therm,fscale_therm,x2vect_heat,   &
            x3vect_heat,x4vect_heat,fscale_heat
          real(kind=WP), dimension(100) :: upar_elas,upar_crit,upar_hard,      &
            upar_ratedep,upar_therm,upar_heat,upar_kine
!
          integer :: ikey,nkeys,offset,i
          real(kind=WP) :: x1scale,x2scale,x3scale,x4scale
          real(kind=WP), dimension(100) :: x2vect,x3vect,x4vect,fscale
          real(kind=WP) :: rho0,chard
          logical :: is_encrypted,is_available
          character(len = ncharline) :: key_raw
          character(len = 20) :: key
          character(len = 20) :: type
!===============================================================================
!
          is_encrypted = .false.
          is_available = .false.
!
          !---------------------------------------------------------------------
          !< Initialize local variables
          !---------------------------------------------------------------------
          ! -> Key type
          ielas         = 0
          icrit         = 0
          ihard         = 0
          iratedep      = 0
          itherm        = 0
          iheat         = 0
          ikine         = 0
          ! -> Number of parameters
          nupar_elas    = 0
          nupar_crit    = 0
          nupar_hard    = 0
          nupar_ratedep = 0
          nupar_therm   = 0
          nupar_heat    = 0
          nupar_kine    = 0
          ! -> Number of tables
          ntab_hard     = 0
          ntab_srate    = 0
          ntab_therm    = 0
          ntab_heat     = 0
          ! -> Id of tables
          itab_hard     = 0
          itab_srate    = 0
          itab_therm    = 0
          itab_heat     = 0
          ! -> Number of temporary variables
          nvartmp_hard  = 0
          nvartmp_srate = 0
          nvartmp_therm = 0
          nvartmp_heat  = 0
          ! -> Iso-kinetic hardening parameter
          chard         = 0
          ! -> Scale factors for tables
          x2vect_hard   = zero
          x3vect_hard   = zero
          x4vect_hard   = zero
          fscale_hard   = zero
          x2vect_srate  = zero
          x3vect_srate  = zero
          x4vect_srate  = zero
          fscale_srate  = zero
          x2vect_therm  = zero
          x3vect_therm  = zero
          x4vect_therm  = zero
          fscale_therm  = zero
          x2vect_heat   = zero
          x3vect_heat   = zero
          x4vect_heat   = zero
          fscale_heat   = zero
          ! -> User parameters
          upar_elas     = zero
          upar_crit     = zero
          upar_hard     = zero
          upar_ratedep  = zero
          upar_therm    = zero
          upar_heat     = zero
          upar_kine     = zero
!
          !< Check encryption option
          call hm_option_is_encrypted(is_encrypted)
!          
          !=====================================================================
          !< Read and print elasto-plastic material common parameters
          !=====================================================================
          !< Density
          call hm_get_floatv('MAT_RHO'  ,rho0   ,is_available, lsubmodel, unitab)
          call hm_get_intv  ('MAT_IRES' ,ires   ,is_available, lsubmodel)
          if (ires == 0) ires = 2
!
          !< Material header printing
          write(iout,1000) trim(titr),mat_id,ilaw
          write(iout,1001)
          if (is_encrypted)then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1002) rho0,ires
          end if
!
          !=====================================================================
          !< Read and print elasto-plastic material bricks
          !=====================================================================
          !< Number of bricks
          call hm_get_intv('clausesmax',nkeys,is_available,lsubmodel)
          !< Loop over bricks/keys
          do ikey = 1,nkeys  
            !< Read keyset
            call hm_get_string_index('KEY_type',key_raw,ikey,ncharline,        &
              is_available)
            key = trim(adjustl(key_raw))
            if (key(1:20) == '') cycle
            !-------------------------------------------------------------------
            !< Select case over key
            !-------------------------------------------------------------------
            select case (key(1:4))
              !< Elasticity
              case ('ELAS')
                !< Check if elastic model is already defined for the material
                if (ielas /= 0) then 
                  call ancmsg(msgid=3123,                                      &                    
                              msgtype=msgerror,                                &
                              anmode=aninfo_blind_2,                           &
                              i1=mat_id,                                       &
                              c1=titr,                                         &
                              c2="ELAS")
                endif 
                type = key(6:len(key))
                call hm_read_elasticity(                                       &
                  ikey     ,type  ,ielas    ,nupar_elas,upar_elas,is_available,&
                  unitab,lsubmodel,matparam ,parmat    ,iout     ,is_encrypted)
              !< Yield criterion
              case ('CRIT')
                if (icrit /= 0) then 
                  call ancmsg(msgid=3123,                                      &                    
                              msgtype=msgerror,                                &
                              anmode=aninfo_blind_2,                           &
                              i1=mat_id,                                       &
                              c1=titr,                                         &
                              c2="CRIT")
                endif 
                type = key(6:len(key))
                call hm_read_yield_criterion(                                  &
                  ikey     ,type  ,icrit    ,nupar_crit,upar_crit,is_available,&
                  unitab,lsubmodel,iout     ,is_encrypted,mat_id ,titr        )
              !< Isotropic work-hardening
              case ('HARD')
                if (ihard /= 0) then 
                  call ancmsg(msgid=3123,                                      &                    
                              msgtype=msgerror,                                &
                              anmode=aninfo_blind_2,                           &
                              i1=mat_id,                                       &
                              c1=titr,                                         &
                              c2="HARD")
                endif 
                type = key(6:len(key))
                call hm_read_work_hardening(                                   &
                  ikey     ,type  ,ihard    ,nupar_hard,upar_hard,ntab_hard ,  &
                  itab_hard,x2vect_hard     ,x3vect_hard   ,x4vect_hard     ,  &
                  fscale_hard,nvartmp_hard  ,is_available  ,unitab,lsubmodel,  &
                  iout  ,is_encrypted       ,vpflag        ,israte          ,  &
                  parmat   )   
              !< Strain rate dependency
              case ('SRAT')
                if (iratedep /= 0) then 
                  call ancmsg(msgid=3123,                                      &                    
                              msgtype=msgerror,                                &
                              anmode=aninfo_blind_2,                           &
                              i1=mat_id,                                       &
                              c1=titr,                                         &
                              c2="SRATE")
                endif 
                type = key(7:len(key))
                call hm_read_srate_dependency(                                 &
                  ikey     ,type  ,iratedep ,nupar_ratedep,upar_ratedep,       &
                  is_available,unitab,lsubmodel,iout     ,is_encrypted ,       &
                  vpflag   ,israte ,parmat  ,ntab_srate  ,itab_srate   ,       &
                  x2vect_srate,x3vect_srate ,x4vect_srate,fscale_srate ,       &
                  nvartmp_srate)     
              !< Thermal softening
              case ('THER')
                if (itherm /= 0) then 
                  call ancmsg(msgid=3123,                                      &                    
                              msgtype=msgerror,                                &
                              anmode=aninfo_blind_2,                           &
                              i1=mat_id,                                       &
                              c1=titr,                                         &
                              c2="THERM")
                endif 
                type = key(7:len(key))
                call hm_read_therm_softening(                                  &
                  ikey     ,type  ,itherm   ,nupar_therm ,upar_therm   ,       &
                  is_available,unitab,lsubmodel,iout     ,is_encrypted ,       &
                  ntab_therm  ,itab_therm   ,x2vect_therm,x3vect_therm ,       &
                  x4vect_therm,fscale_therm ,nvartmp_therm,mtag        ) 
              !< Self-heating
              case ('HEAT')
                if (iheat /= 0) then 
                  call ancmsg(msgid=3123,                                      &                    
                              msgtype=msgerror,                                &
                              anmode=aninfo_blind_2,                           &
                              i1=mat_id,                                       &
                              c1=titr,                                         &
                              c2="HEAT")
                endif 
                type = key(6:len(key))
                call hm_read_self_heating(                                     &
                  ikey     ,type  ,iheat    ,nupar_heat  ,upar_heat    ,       &
                  is_available,unitab,lsubmodel,iout     ,is_encrypted ,       &
                  ntab_heat   ,itab_heat    ,x2vect_heat ,x3vect_heat  ,       &
                  x4vect_heat ,fscale_heat  ,nvartmp_heat,matparam     ,       &
                  mtag        )
              !< Kinematic hardening
              case ('KINE')
                if (ikine /= 0) then 
                  call ancmsg(msgid=3123,                                      &                    
                              msgtype=msgerror,                                &
                              anmode=aninfo_blind_2,                           &
                              i1=mat_id,                                       &
                              c1=titr,                                         &
                              c2="KINE")
                endif 
                type = key(6:len(key))
                call hm_read_kinematic_hardening(                              &
                  ikey     ,type  ,ikine    ,nupar_kine  ,upar_kine    ,       &
                  is_available,unitab,lsubmodel,iout     ,is_encrypted ,       &
                  mtag     ,chard  )  
                chard = min(max(chard,zero),one)               
            end select
          enddo
          !=====================================================================
!
          ! --------------------------------------------------------------------
          !< Error if no elastic law is defined
          ! --------------------------------------------------------------------
          if (ielas == 0) then 
            call ancmsg(msgid=3124,                                            &                    
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_2,                                 &
                        i1=mat_id,                                             &
                        c1=titr)
          endif
!
          ! --------------------------------------------------------------------
          !< Default Von Mises yield criterion if no yield criterion is defined
          ! --------------------------------------------------------------------          
          if (icrit == 0) then 
            icrit = 1
            call ancmsg(msgid=3125,                                            &                    
                        msgtype=msgwarning,                                    &
                        anmode=aninfo_blind_2,                                 &
                        i1=mat_id,                                             &
                        c1=titr)
          endif
!
          ! --------------------------------------------------------------------
          !< Default strain rate treatment
          ! --------------------------------------------------------------------
          !< Switch to filtered total strain rate
          if (vpflag == 0) then 
            vpflag = 2
            israte = 1
            parmat(4) = israte
            parmat(5) = 10000.0d0*unitab%fac_t_work       
          endif
!
          ! --------------------------------------------------------------------
          !< Filling buffer tables
          ! --------------------------------------------------------------------
          !< Number of integer material parameters
          matparam%niparam = 24
          !< Number of real material parameters
          matparam%nuparam = nupar_elas + nupar_crit +                         &
                             nupar_hard + nupar_ratedep +                      &
                             nupar_therm + nupar_heat + (nupar_kine + 1)
          !< Number of user variables
          if (ires == 1) then 
            nuvar = 7
          else
            nuvar = 1
          endif
          !< Number of tables and temporary variables
          matparam%ntable = ntab_hard + ntab_srate + ntab_therm + ntab_heat
          nvartmp = nvartmp_hard + nvartmp_srate + nvartmp_therm + nvartmp_heat
!
          !< Allocation of material parameters tables
          allocate(matparam%iparam(matparam%niparam))
          allocate(matparam%uparam(matparam%nuparam))
          allocate(matparam%table (matparam%ntable ))
!
          !< Integer material parameter
          ! -> Elastic parameters 
          matparam%iparam(1)  = ielas                               !< Elastic model type
          matparam%iparam(2)  = nupar_elas                          !< Address of elastic last real parameter
          ! -> Yield criterion parameters
          matparam%iparam(3)  = icrit                               !< Yield criterion type
          matparam%iparam(4)  = matparam%iparam(2) + nupar_crit     !< Address of yield criterion last real parameter
          ! -> Work hardening parameters
          matparam%iparam(5)  = ihard                               !< Work hardening model type
          matparam%iparam(6)  = ntab_hard                           !< Number of the last work hardening table
          matparam%iparam(7)  = matparam%iparam(4) + nupar_hard     !< Address of work hardening last real parameter
          matparam%iparam(8)  = nvartmp_hard                        !< Address of work hardening last temporary variable
          ! -> Strain rate dependency parameters
          matparam%iparam(9)  = iratedep                            !< Strain rate dependency model type
          matparam%iparam(10) = vpflag                              !< Viscoplastic flag
          matparam%iparam(11) = matparam%iparam(6) + ntab_srate     !< Number of the last strain rate dependency table
          matparam%iparam(12) = matparam%iparam(7) + nupar_ratedep  !< Address of strain rate dependency last real parameter
          matparam%iparam(13) = matparam%iparam(8) + nvartmp_srate  !< Address of strain rate dependency last temporary variable
          ! -> Thermal softening parameters
          matparam%iparam(14) = itherm                              !< Thermal softening model type
          matparam%iparam(15) = matparam%iparam(11) + ntab_therm    !< Number of the last thermal softening table
          matparam%iparam(16) = matparam%iparam(12) + nupar_therm   !< Address of thermal softening last real parameter
          matparam%iparam(17) = matparam%iparam(13) + nvartmp_therm !< Address of thermal softening last temporary variable
          ! -> Self-heating parameters
          matparam%iparam(18) = iheat                               !< Self-heating model type
          matparam%iparam(19) = matparam%iparam(15) + ntab_heat     !< Number of the last self-heating table
          matparam%iparam(20) = matparam%iparam(16) + nupar_heat    !< Address of self-heating last real parameter
          matparam%iparam(21) = matparam%iparam(17) + nvartmp_heat  !< Address of self-heating last temporary variable
          ! -> Kinematic hardening parameters
          matparam%iparam(22) = ikine                               !< Kinematic hardening model type
          matparam%iparam(23) = matparam%iparam(20) + nupar_kine + 1!< Address of kinematic hardening last real parameter
          ! -> Flag for return mapping algorithm
          matparam%iparam(24) = ires                                !< Return mapping flag
!
          !< Real material parameters
          ! -> Elastic parameters
          do i = 1,nupar_elas
            matparam%uparam(i) = upar_elas(i)
          enddo
          ! -> Yield criterion parameters
          offset = matparam%iparam(2)
          do i = 1,nupar_crit
            matparam%uparam(offset+i) = upar_crit(i)
          enddo
          ! -> Work hardening parameters
          offset = matparam%iparam(4)
          do i = 1,nupar_hard
            matparam%uparam(offset+i) = upar_hard(i)
          enddo
          ! -> Strain rate dependency parameters
          offset = matparam%iparam(7)
          do i = 1,nupar_ratedep
            matparam%uparam(offset+i) = upar_ratedep(i)
          enddo
          ! -> Thermal softening parameters
          offset = matparam%iparam(12)
          do i = 1,nupar_therm
            matparam%uparam(offset+i) = upar_therm(i)
          enddo
          ! -> Self-heating parameters
          offset = matparam%iparam(16)
          do i = 1,nupar_heat
            matparam%uparam(offset+i) = upar_heat(i)
          enddo
          ! -> Kinematic hardening parameters
          offset = matparam%iparam(20)
          matparam%uparam(offset+1) = chard
          do i = 1,nupar_kine
            matparam%uparam(offset+1+i) = upar_kine(i)
          enddo
!
          !< Material tables
          if (matparam%ntable > 0) then  
            ! -> Tabulated work-hardening   
            if (ntab_hard > 0) then 
              do i = 1, ntab_hard
                matparam%table(i)%notable = itab_hard(i)
                x1scale   = one
                x2scale   = one
                x3scale   = one
                x4scale   = one
                x2vect(i) = x2vect_hard(i)
                x3vect(i) = x3vect_hard(i)
                x4vect(i) = x4vect_hard(i)
                fscale(i) = fscale_hard(i)
              enddo
            endif
            ! -> Tabulated strain rate dependency
            if (ntab_srate > 0) then
              offset = matparam%iparam(6)
              do i = 1, ntab_srate
                matparam%table(offset+i)%notable = itab_srate(i)
                x1scale = one
                x2scale = one
                x3scale = one
                x4scale = one
                x2vect(offset+i) = x2vect_srate(i)
                x3vect(offset+i) = x3vect_srate(i)
                x4vect(offset+i) = x4vect_srate(i)
                fscale(offset+i) = fscale_srate(i)
              enddo
            endif
            ! -> Tabulated thermal softening
            if (ntab_therm > 0) then
              offset = matparam%iparam(11)
              do i = 1, ntab_therm
                matparam%table(offset+i)%notable = itab_therm(i)
                x1scale = one
                x2scale = one
                x3scale = one
                x4scale = one
                x2vect(offset+i) = x2vect_therm(i)
                x3vect(offset+i) = x3vect_therm(i)
                x4vect(offset+i) = x4vect_therm(i)
                fscale(offset+i) = fscale_therm(i)
              enddo
            endif
            ! -> Tabulated self-heating
            if (ntab_heat > 0) then
              offset = matparam%iparam(15)
              do i = 1, ntab_heat
                matparam%table(offset+i)%notable = itab_heat(i)
                x1scale = one
                x2scale = one
                x3scale = one
                x4scale = one
                x2vect(offset+i) = x2vect_heat(i)
                x3vect(offset+i) = x3vect_heat(i)
                x4vect(offset+i) = x4vect_heat(i)
                fscale(offset+i) = fscale_heat(i)
              enddo
            endif
            ! -> Copy tables to material structure
            call mat_table_copy(                                               &
              matparam ,x2vect    ,x3vect   ,x4vect   ,x1scale  ,x2scale  ,    &
              x3scale  ,x4scale   ,fscale   ,ntable   ,table    ,ilaw     )
          endif
!
          !< Initial and reference density
          matparam%rho0 = rho0
          matparam%rho  = rho0
!
          !< MTAG variable activation
          mtag%g_epsd = 1 !< Global equivalent strain rate
          mtag%l_epsd = 1 !< Local equivalent strain rate
          mtag%g_pla  = 1 !< Global plastic strain
          mtag%l_pla  = 1 !< Local plastic strain
          mtag%g_seq  = 1 !< Global equivalent stress
          mtag%l_seq  = 1 !< Local equivalent stress
!
          !< Properties compatibility
          call init_mat_keyword(matparam ,"SOLID_ISOTROPIC")
          call init_mat_keyword(matparam ,"SHELL_ISOTROPIC")
          call init_mat_keyword(matparam ,"SPH")
!
          !< Material model keywords
          call init_mat_keyword(matparam ,"ELASTO_PLASTIC")
          call init_mat_keyword(matparam ,"INCREMENTAL" )
          call init_mat_keyword(matparam ,"LARGE_STRAIN")
          call init_mat_keyword(matparam ,"HOOK")
!
          !< End of material definition printing
          write(iout,1003)
!
1001 format(/                                                                  &
          5X,"=======================================================",/       &
          5X,"           MATERIAL MODEL: ELASTO-PLASTIC              ",/,      &
          5X,"=======================================================",/)
1000 format(/                                                                  &
          5X,A,/,                                                              &
          5X,"MATERIAL NUMBER. . . . . . . . . . . . . . . . . . . .=",I10/,   &
          5X,"MATERIAL LAW . . . . . . . . . . . . . . . . . . . . .=",I10/)
1002 format(/                                                                  &
          5X,"INITIAL DENSITY. . . . . . . . . . . . . . . . . . . .=",1PG20.13/,&
          5X,"RETURN MAPPING FLAG. . . . . . . . . . . . . . . . . .=",I10/    &
          5X,"   = 1: NEXT INCREMENT CORRECT ERROR (NICE) 1 STEP ALGORITHM",/, &
          5X,"   = 2: CUTTING PLANE ITERATIVE ALGORITHM                   ",/, &
          5X,"   = 3: CLOSEST POINT PROJECTION METHOD (CPPM) IMPLICIT ALGORITHM",/)
1003 format(/                                                                  &
          5X,"=======================================================",/)
!
        end subroutine hm_read_elasto_plastic
      end module hm_read_elasto_plastic_mod