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
!||====================================================================
!||    hm_read_mat136_mod   ../starter/source/materials/mat/mat136/hm_read_mat136.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
!||====================================================================
      module hm_read_mat136_mod
      contains
!||====================================================================
!||    hm_read_mat136           ../starter/source/materials/mat/mat136/hm_read_mat136.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    mat_table_copy_mod       ../starter/source/materials/tools/mat_table_copy.F90
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_mat136(                                             &
          matparam ,nuvar    ,nfunc    ,parmat   ,unitab   ,mat_id   ,titr   , &
          mtag     ,nvartmp  ,lsubmodel,iout     )
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
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          real(kind=WP) :: ec, nuc, rho0, bulk, shear, lambda, mu,f_c, f_t,    &
            kfiss1, kfiss2, dmax1, dmax2, qp1, qp2, gamma, cm, cb, es, nueq,   &
            eeq, coating, lambda_b, mu_b, max_dens, rhos, rhoeq
          real(kind=WP), dimension(2) :: omega_x, omega_y, rho_x, rho_y, sigy
          integer :: ilaw
          logical :: is_available,is_encrypted
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
          is_encrypted = .false.
          is_available = .false.
          ilaw = 136
          !----------------------------------------------------------------------------------
          call hm_option_is_encrypted(is_encrypted)
          !----------------------------------------------------------------------------------
          !< Density
          call hm_get_floatv('MAT_RHO'          ,rho0      ,is_available,lsubmodel,unitab)
          !----------------------------------------------------------------------------------
          !< 1st line of material card
          call hm_get_floatv('MAT_EC'           ,ec        ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_NUC'          ,nuc       ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_ES'           ,es        ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_RHOS'         ,rhos      ,is_available,lsubmodel,unitab)
          !----------------------------------------------------------------------------------
          !< 2nd line of material card
          call hm_get_floatv('MAT_FT'           ,f_t       ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_FC'           ,f_c       ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_GAMMA'        ,gamma     ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_QP1'          ,qp1       ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_QP2'          ,qp2       ,is_available,lsubmodel,unitab)
          !----------------------------------------------------------------------------------
          !< 3rd line of material card
          call hm_get_floatv('MAT_SIGY_INF'     ,sigy(1)   ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_OMEGA_X_INF'  ,omega_x(1),is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_OMEGA_Y_INF'  ,omega_y(1),is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_RHO_X_INF'    ,rho_x(1)  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_RHO_Y_INF'    ,rho_y(1)  ,is_available,lsubmodel,unitab)
          !----------------------------------------------------------------------------------
          !< 4th line of material card
          call hm_get_floatv('MAT_SIGY_SUP'     ,sigy(2)   ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_OMEGA_X_SUP'  ,omega_x(2),is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_OMEGA_Y_SUP'  ,omega_y(2),is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_RHO_X_SUP'    ,rho_x(2)  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_RHO_Y_SUP'    ,rho_y(2)  ,is_available,lsubmodel,unitab)
          !----------------------------------------------------------------------------------
          !< 5th line of material card
          call hm_get_floatv('MAT_CM'           ,cm        ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_CB'           ,cb        ,is_available,lsubmodel,unitab)
!
          !----------------------------------------------------------------------------------
          !< Parameters default values
          !----------------------------------------------------------------------------------
!
          !----------------------------------------------------------------------------------
          !< Elastic constants
          !----------------------------------------------------------------------------------
          !< Lame constants
          max_dens = max(omega_x(1),omega_y(1),omega_x(2),omega_y(2))
          lambda = ec*nuc/(one - nuc*nuc) + two*es*max_dens
          mu     = ec/(two*(one + nuc))
          !< Shear modulus
          shear  = ec/(two*(one + nuc))
          !< Bulk modulus
          bulk   = ec/(three*(one - two*nuc))
          !< Normalized coating factor for the reinforcement
          coating = half*(one - max(abs(rho_x(1)),abs(rho_y(1)),abs(rho_x(2)),abs(rho_y(2))))
          !< Equivalent elastic constants for bending
          nueq = (nuc*ec/(twelve*(one - nuc*nuc)))/                                         &
             (((ec/(twelve*(one - nuc*nuc))))+two*es*max_dens*coating)
          eeq = ec*((nuc*(one - nueq*nueq))/(nueq*(one - nuc*nuc)))
          lambda_b = eeq*nueq/(one - nueq*nueq)
          mu_b     = eeq/(two*(one + nueq))
          !< Initial density correction for the reinforcement
          rhoeq = rho0*(one-max_dens) + rhos*(max_dens)
!
          !----------------------------------------------------------------------------------
          !< Damage constants
          !----------------------------------------------------------------------------------
          dmax1 = (one - qp1)/max((qp1 - gamma),em20)
          dmax2 = (one - qp2)/max((qp2 - gamma),em20)
          if (dmax1 == zero) dmax1 = one
          if (dmax2 == zero) dmax2 = one
!
          !----------------------------------------------------------------------------------
          !< Filling buffer tables
          !----------------------------------------------------------------------------------
          !< Number of integer material parameters
          matparam%niparam = 0
          !< Number of real material parameters
          matparam%nuparam = 23
          !< Number of user variables
          nuvar = 8
          !< Number of functions
          nfunc = 0
          !< Number of tables and temporary variables
          matparam%ntable = 0
          nvartmp = 0
!
          !< Allocation of material parameters tables
          allocate(matparam%iparam(matparam%niparam))
          allocate(matparam%uparam(matparam%nuparam))
          allocate(matparam%table (matparam%ntable ))
!
          !< Real material parameters
          matparam%young      = ec
          matparam%nu         = nuc
          matparam%shear      = shear
          matparam%bulk       = bulk
          matparam%uparam(1)  = lambda
          matparam%uparam(2)  = mu
          matparam%uparam(3)  = f_t
          matparam%uparam(4)  = f_c
          matparam%uparam(5)  = gamma
          matparam%uparam(6)  = dmax1
          matparam%uparam(7)  = dmax2
          matparam%uparam(8)  = sigy(1)
          matparam%uparam(9)  = omega_x(1)
          matparam%uparam(10) = omega_y(1)
          matparam%uparam(11) = rho_x(1)
          matparam%uparam(12) = rho_y(1)
          matparam%uparam(13) = sigy(2)
          matparam%uparam(14) = omega_x(2)
          matparam%uparam(15) = omega_y(2)
          matparam%uparam(16) = rho_x(2)
          matparam%uparam(17) = rho_y(2)
          matparam%uparam(18) = cm
          matparam%uparam(19) = cb
          matparam%uparam(20) = lambda_b
          matparam%uparam(21) = mu_b
          matparam%uparam(22) = eeq
          matparam%uparam(23) = nueq
!
          !< PARMAT table
          parmat(1)  = bulk
          parmat(2)  = ec
          parmat(3)  = nuc
          parmat(16) = 2 
          parmat(17) = two*shear/(bulk+four_over_3*shear)
!
          !< Initial and reference density
          matparam%rho0 = rhoeq
          matparam%rho  = rhoeq
!
          !< MTAG variable activation
          mtag%g_epsd = 1
          mtag%l_epsd = 1
          mtag%g_pla  = 1
          mtag%l_pla  = 1
          mtag%g_dmg  = 3
          mtag%l_dmg  = 3
          mtag%l_sigb = 5
!
          ! Number of output modes 
          matparam%nmod = 2
          allocate(matparam%mode(matparam%nmod))
          matparam%mode(1) = "Positive bending damage"
          matparam%mode(2) = "Negative bending damage"
!
          !< Properties compatibility
          call init_mat_keyword(matparam,"SHELL_ISOTROPIC")
!
          !< Material model keywords
          call init_mat_keyword(matparam ,"INCREMENTAL"   )
          call init_mat_keyword(matparam ,"LARGE_STRAIN"  )
          call init_mat_keyword(matparam ,"HOOK"          )
          call init_mat_keyword(matparam ,"ISOTROPIC"     )
!
          !----------------------------------------------------------------------------------
          !< Listing output
          !----------------------------------------------------------------------------------
          write(iout,1001) trim(titr),mat_id,ilaw
          write(iout,1000)
          if (is_encrypted) then
            write(iout,'(5X,A,//)') 'CONFIDENTIAL DATA'
          else
            write(iout,1002) rho0
            write(iout,1003) ec,nuc
            if (es > zero) then
              write(iout,1008) es, eeq, nueq
            endif
            if (rhos > zero) then
              write(iout,1009) rhos, rhoeq
            endif
            write(iout,1004) f_t,f_c,gamma,qp1,qp2,dmax1,dmax2
            write(iout,1005) sigy(1),omega_x(1),omega_y(1),rho_x(1),rho_y(1),      &
                             sigy(2),omega_x(2),omega_y(2),rho_x(2),rho_y(2)
            write(iout,1006) cm,cb
            write(iout,1007)
          endif
!
          !----------------------------------------------------------------------------------
          !< Output formats
          !----------------------------------------------------------------------------------
1000      format(/                                                                 &
            5X,"========================================================",/        &
            5X,"       MATERIAL MODEL: GLOBAL REINFORCED CONCRETE       ",/,       &
            5X,"========================================================",/)
1001      format(/                                                                 &
            5X,A,/,                                                                &
            5X,"MATERIAL NUMBER . . . . . . . . . . . . . . . . . . . .=",I10/,    &
            5X,"MATERIAL LAW. . . . . . . . . . . . . . . . . . . . . .=",I10/)
1002      format(/                                                                 &
            5X,"INITIAL DENSITY . . . . . . . . . . . . . . . . . . . .=",1PG20.13/)
1003      format(/                                                                 &
            5X,"CONCRETE ELASTIC PARAMETERS:                            ",/,       &
            5X,"-------------------                                     ",/,       &
            5X,"CONCRETE YOUNG MODULUS (E). . . . . . . . . . . . . . .=",1PG20.13/&
            5X,"CONCRETE POISSON RATIO (NU) . . . . . . . . . . . . . .=",1PG20.13/)
1004      format(/                                                                 &
            5X,"BENDING DAMAGE PARAMETERS:                              ",/,       &
            5X,"--------------------------                              ",/,       &
            5X,"CONCRETE TENSILE STRENGTH (FT). . . . . . . . . . . . .=",1PG20.13/&
            5X,"CONCRETE COMPRESSIVE STRENGTH (FC). . . . . . . . . . .=",1PG20.13/&
            5X,"DAMAGE SLOPE RATIO BEFORE/AFTER CRACKING (GAMMA). . . .=",1PG20.13/&
            5X,"SLOPE RATIO FOR POSITIVE BENDING (QP1). . . . . . . . .=",1PG20.13/&
            5X,"SLOPE RATIO FOR NEGATIVE BENDING (QP2). . . . . . . . .=",1PG20.13/&
            5X,"MAXIMUM DAMAGE COMPUTED FOR POS. BENDING (DMAX1). . . .=",1PG20.13/&
            5X,"MAXIMUM DAMAGE COMPUTED FOR NEG. BENDING (DMAX2). . . .=",1PG20.13/)
1005      format(/                                                                 &
            5X,"STEEL REINFORCEMENT GEOMETRIC PARAMETERS:               ",/,       &
            5X,"-----------------------------------------               ",/,       &
            5X,"                                                        ",/,       &
            5X,"REINFORCEMENT OF LOWER LAYER                            ",/,       &
            5X,"STEEL YIELD STRESS (SIGY) . . . . . . . . . . . . . . .=",1PG20.13/&
            5X,"DENSITY OF THE REINFORCEMENT IN X DIRECTION (OMEGA_X) .=",1PG20.13/&
            5X,"DENSITY OF THE REINFORCEMENT IN Y DIRECTION (OMEGA_Y) .=",1PG20.13/&
            5X,"POSITION IN THICKNESS OF REINF. IN X DIRECTION (RHO_X).=",1PG20.13/&
            5X,"POSITION IN THICKNESS OF REINF. IN Y DIRECTION (RHO_Y).=",1PG20.13/& 
            5X,"                                                        ",/,       &
            5X,"REINFORCEMENT OF UPPER LAYER                            ",/,       &
            5X,"STEEL YIELD STRESS (SIGY) . . . . . . . . . . . . . . .=",1PG20.13/&
            5X,"DENSITY OF THE REINFORCEMENT IN X DIRECTION (OMEGA_X) .=",1PG20.13/&
            5X,"DENSITY OF THE REINFORCEMENT IN  Y DIRECTION (OMEGA_Y).=",1PG20.13/&
            5X,"POSITION IN THICKNESS OF REINF. IN X DIRECTION (RHO_X).=",1PG20.13/&
            5X,"POSITION IN THICKNESS OF REINF. IN Y DIRECTION (RHO_Y).=",1PG20.13/)
1006      format(/                                                                 &
            5X,"STEEL REINFORCEMENT PLASTICITY PARAMETERS:              ",/,       &
            5X,"------------------------------------------              ",/,       &
            5X,"PRAGER HARDENING MODULUS FOR MEMBRANE FORCES (CM) . . .=",1PG20.13/&
            5X,"PRAGER HARDENING MODULUS FOR BENDING MOMENTS (CB) . . .=",1PG20.13/)
1008      format(/                                                                 &
            5X,"STEEL REINFORCEMENT ELASTIC PARAMETERS:                 ",/,       &
            5X,"---------------------------------------                 ",/,       &
            5X,"STEEL REINFORCEMENT YOUNG MODULUS (ES). . . . . . . . .=",1PG20.13/&
            5X,"EQUIVALENT YOUNG MODULUS FOR BENDING (EEQ). . . . . . .=",1PG20.13/&
            5X,"EQUIVALENT POISSON RATIO FOR BENDING (NUEQ) . . . . . .=",1PG20.13/)
1009      format(/                                                                 &
            5X,"STEEL REINFORCEMENT DENSITY PARAMETERS:                 ",/,       &
            5X,"---------------------------------------                 ",/,       &
            5X,"STEEL REINFORCEMENT DENSITY (RHOS). . . . . . . . . . .=",1PG20.13/&
            5X,"EQUIVALENT DENSITY (RHOEQ). . . . . . . . . . . . . . .=",1PG20.13/)
1007 format(/                                                                  &
            5X,"========================================================",/)
!
        end subroutine hm_read_mat136
      end module hm_read_mat136_mod
