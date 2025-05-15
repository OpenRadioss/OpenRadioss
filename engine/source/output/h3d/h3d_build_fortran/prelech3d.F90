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
      !||====================================================================
      !||    prelech3d_mod   ../engine/source/output/h3d/h3d_build_fortran/prelech3d.F90
      !||--- called by ------------------------------------------------------
      !||    lectur          ../engine/source/input/lectur.F
      !||====================================================================
      module prelech3d_mod
      contains
      !||====================================================================
      !||    prelech3d                ../engine/source/output/h3d/h3d_build_fortran/prelech3d.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                   ../engine/source/input/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    create_h3d_arg_keyword   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_arg_keyword.F
      !||    h3d_gene_keyword         ../engine/source/output/h3d/input_list/h3d_gene_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    elbufdef_mod             ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    h3d_inc_mod              ../engine/share/modules/h3d_inc_mod.F
      !||    h3d_mod                  ../engine/share/modules/h3d_mod.F
      !||    initbuf_mod              ../engine/share/resol/initbuf.F
      !||    matparam_def_mod         ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    message_mod              ../engine/share/message_module/message_mod.F
      !||    multi_fvm_mod            ../common_source/modules/ale/multi_fvm_mod.F90
      !||    names_and_titles_mod     ../common_source/modules/names_and_titles_mod.F
      !||    stack_mod                ../engine/share/modules/stack_mod.F
      !||    tri7box                  ../engine/share/modules/tri7box.F
      !||====================================================================
      subroutine prelech3d(                                                    &
        numgeo   ,npropgi  ,npropmi  ,nummat   ,numply   ,igeo     ,           &
        ipm      ,h3d_data ,multi_fvm,mds_output_table   ,mds_nmat ,           &
        max_depvar,      mds_ndepsvar,mat_param) 
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use initbuf_mod
        use message_mod
        use stack_mod
        use h3d_mod
        use multi_fvm_mod
        use elbufdef_mod
        use stack_mod
        use h3d_inc_mod
        use loads_mod
        use names_and_titles_mod, only: ncharline100,ncharkey
        use matparam_def_mod
        use tri7box
        use pblast_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none
#include "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        integer, intent(in) :: numgeo
        integer, intent(in) :: npropgi
        integer, intent(in) :: npropmi
        integer, intent(in) :: nummat
        integer, intent(in) :: numply
        integer, dimension(npropgi,numgeo)  :: igeo
        integer, dimension(npropmi,nummat)  :: ipm
        type (h3d_database), intent(inout)  :: h3d_data
        type (multi_fvm_struct), intent(in) :: multi_fvm
        integer, dimension(max_depvar,mds_nmat) :: mds_output_table
        integer, intent(in) :: mds_nmat
        integer, intent(in) :: max_depvar
        integer, dimension(*) :: mds_ndepsvar
        type (matparam_struct_) ,dimension(nummat) ,intent(inout) :: mat_param
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer i,j,k,l,cpt,id_input,is_char_key3,is_char_key4
        integer is_char_key5,is_char_key6,is_char_key7,is_char_key8
        integer is_empty_key3,is_empty_key4,is_empty_key5,is_empty_key6
        integer is_empty_key7,is_empty_key8,is_all,is_upper,is_lower
        integer is_def,is_ply,is_layer,is_ipt,is_ply_all,is_layer_all
        integer is_layer_lower,is_layer_upper,is_ipt_all,is_ipt_lower
        integer is_ipt_upper,is_ipt_memb,ply,layer,ipt,iuvar,is_uvar
        integer is_uvar_all,ir,is_ir,is_ir_all,is_ir_lower,is_ir_upper
        integer is,is_is,is_is_all,is_is_lower,is_is_upper
        integer it,is_it,is_it_all,is_it_lower,is_it_upper
        integer nb_key,cpt_key,is_inter,inter,is_inter_all
        integer is_mdsvar_def,is_mdsvar,is_model_npt,is_model_ply
        integer is_model_layer,is_id,id,is_id_all,is_mode,mode
        integer is_mode_all,nfail,is_memb,imat,nmod,npt
        integer ibid1,ibid2,ibid3,ibid4,is_mdsvar_all,imdsvar
        character(len=ncharkey) :: key2,key3,key4,key5,key6,key7,key8,         &
          key2_read
        character(len=ncharline100) :: key3_read,key3_glob
        integer :: nunits,sizetot
!===============================================================================
!
        !< Local variables initialization
        ibid1   = 0
        ibid2   = 0
        ibid3   = 0
        ibid4   = 0
        sizetot = 0
!
        !< Loop over the H3D inputs
        do id_input = 1,h3d_data%n_input_h3d
!
          nunits = 0
          key3_glob = ''
!
          !< H3D keywords
          key2 = h3d_data%input_list(id_input)%key2
          key3 = h3d_data%input_list(id_input)%key3
          key4 = h3d_data%input_list(id_input)%key4
          key5 = h3d_data%input_list(id_input)%key5
          key6 = h3d_data%input_list(id_input)%key6
          key7 = h3d_data%input_list(id_input)%key7
          key8 = h3d_data%input_list(id_input)%key8
!
          !< Flag initialization
          is_char_key3   = 1
          is_empty_key3  = 1
          is_char_key4   = 1
          is_empty_key4  = 1
          is_char_key5   = 1
          is_empty_key5  = 1
          is_char_key6   = 1
          is_empty_key6  = 1
          is_char_key7   = 1
          is_empty_key7  = 1
          is_char_key8   = 1
          is_empty_key8  = 1
          ply            = 0
          is_ply         = 0
          is_ply_all     = 0
          layer          = 0
          is_layer       = 0
          is_layer_lower = 0
          is_layer_upper = 0
          is_layer_all   = 0
          ipt            = 0
          is_ipt         = 0
          is_ipt_lower   = 0
          is_ipt_upper   = 0
          is_ipt_memb    = 0
          is_ipt_all     = 0
          iuvar          = 0
          is_uvar        = 0
          is_uvar_all    = 0
          inter          = 0
          is_inter       = 0
          is_inter_all   = 0
          imdsvar        = 0
          is_mdsvar      = 0
          is_mdsvar_all  = 0
          is_mdsvar_def  = 0
!
          !<-------------------------------------------------------------------
          !< SEARCH '=' character
          !<-------------------------------------------------------------------
          do i=1,ncharkey
            if ( key3(i:i) == '=' ) is_char_key3  = 0
            if ( key3(i:i) /= ' ' ) is_empty_key3 = 0
            if ( key4(i:i) == '=' ) is_char_key4  = 0
            if ( key4(i:i) /= ' ' ) is_empty_key4 = 0
            if ( key5(i:i) == '=' ) is_char_key5  = 0
            if ( key5(i:i) /= ' ' ) is_empty_key5 = 0
            if ( key6(i:i) == '=' ) is_char_key6  = 0
            if ( key6(i:i) /= ' ' ) is_empty_key6 = 0
            if ( key7(i:i) == '=' ) is_char_key7  = 0
            if ( key7(i:i) /= ' ' ) is_empty_key7 = 0
            if ( key8(i:i) == '=' ) is_char_key8  = 0
            if ( key8(i:i) /= ' ' ) is_empty_key8 = 0
          enddo
!
          !<--------------------------------------------------------------------
          !< Read PLY= I/ALL
          !<--------------------------------------------------------------------
          is_all   = 1
          is_lower = 0
          is_upper = 0
          is_def   = 0
          is_memb  = 0
          call create_h3d_arg_keyword(                                         &
                      key2       ,key3        ,key4      ,key5     ,key6      ,&
                      key7       ,key8        ,is_all    ,is_lower ,is_upper  ,&
                      'PLY'      ,3           ,is_ply    ,ply      ,           &
                      is_ply_all ,ibid1       ,ibid2     ,is_def   ,           &
                      ibid3      ,is_memb     ,ibid4     )
!
          !<--------------------------------------------------------------------
          !< Read LAYER= I/ALL/LOWER/UPPER
          !<--------------------------------------------------------------------
          is_all = 1
          is_lower = 1
          is_upper = 1
          is_def = 0
          is_memb = 0
          call create_h3d_arg_keyword(                                         &
                      key2       ,key3        ,key4      ,key5     ,key6      ,&
                      key7       ,key8        ,is_all    ,is_lower ,is_upper  ,&
                      'LAYER'    ,5           ,is_layer  ,layer    ,           &
                      is_layer_all,is_layer_lower,is_layer_upper   ,is_def    ,&
                      ibid1      ,is_memb     ,ibid2     )
!
          !<--------------------------------------------------------------------
          !< Read NPT= I/ALL/LOWER/UPPER/MEMB
          !<--------------------------------------------------------------------
          is_all = 1
          is_lower = 1
          is_upper = 1
          is_def = 0
          is_memb = 1
          call create_h3d_arg_keyword(                                         &
                      key2       ,key3        ,key4      ,key5     ,key6      ,&
                      key7       ,key8        ,is_all    ,is_lower ,is_upper  ,&
                      'NPT'      ,3           ,is_ipt    ,ipt      ,           &
                      is_ipt_all ,is_ipt_lower,is_ipt_upper,is_def ,           &
                      ibid1      ,is_memb     ,is_ipt_memb)
!
          !<--------------------------------------------------------------------
          !< Read UVAR= I/ALL/DEF
          !<--------------------------------------------------------------------
          is_all = 1
          is_lower = 0
          is_upper = 0
          is_def = 0
          is_memb = 0
          call create_h3d_arg_keyword(                                         &
                      key2       ,key3        ,key4      ,key5     ,key6      ,&
                      key7       ,key8        ,is_all    ,is_lower ,is_upper  ,&
                      'UVAR'     ,4           ,is_uvar   ,iuvar    ,           &
                      is_uvar_all,ibid1       ,ibid2     ,is_def   ,           &
                      ibid3      ,is_memb     ,ibid4     )
!
          !<--------------------------------------------------------------------
          !< Read MDS_VAR= I/ALL/DEF
          !<--------------------------------------------------------------------
          is_all = 1
          is_lower = 0
          is_upper = 0
          is_def = 1
          is_memb = 0
          call create_h3d_arg_keyword(                                         &
                      key2       ,key3        ,key4      ,key5     ,key6      ,&
                      key7       ,key8        ,is_all    ,is_lower ,is_upper  ,&
                      'MDS_VAR'  ,7           ,is_mdsvar ,imdsvar  ,           &
                      is_mdsvar_all,ibid1     ,ibid2     ,is_def   ,           &
                      is_mdsvar_def,is_memb   ,ibid3     )
!
          !<--------------------------------------------------------------------
          !< Read IR= I/ALL/LOWER/UPPER
          !<--------------------------------------------------------------------
          is_all = 1
          is_lower = 1
          is_upper = 1
          is_def = 0
          is_memb = 0
          call create_h3d_arg_keyword(                                         &
                      key2       ,key3        ,key4      ,key5     ,key6      ,&
                      key7       ,key8        ,is_all    ,is_lower ,is_upper  ,&
                      'IR'       ,2           ,is_ir     ,ir       ,           &
                      is_ir_all  ,is_ir_lower ,is_ir_upper,is_def  ,           &
                      ibid1      ,is_memb     ,ibid2     )
!
          !<--------------------------------------------------------------------
          !< Read IS= I/ALL/LOWER/UPPER
          !<--------------------------------------------------------------------
          is_all = 1
          is_lower = 1
          is_upper = 1
          is_def = 0
          is_memb = 0
          call create_h3d_arg_keyword(                                         &
                    key2       ,key3        ,key4        ,key5     ,key6      ,&
                    key7       ,key8        ,is_all      ,is_lower ,is_upper  ,&
                    'IS'       ,2           ,is_is       ,is       ,           &
                    is_is_all  ,is_is_lower ,is_is_upper ,is_def   ,           &
                    ibid1      ,is_memb     ,ibid2       )
!
          !<--------------------------------------------------------------------
          !< Read IT= I/ALL/LOWER/UPPER
          !<--------------------------------------------------------------------
          is_all = 1
          is_lower = 1
          is_upper = 1
          is_def = 0
          is_memb = 0
          call create_h3d_arg_keyword(                                         &
                    key2       ,key3        ,key4        ,key5     ,key6      ,&
                    key7       ,key8        ,is_all      ,is_lower ,is_upper  ,&
                    'IT'       ,2           ,is_it       ,it       ,           &
                    is_it_all  ,is_it_lower ,is_it_upper ,is_def   ,           &
                    ibid1      ,is_memb     ,ibid2       )
!
          !<--------------------------------------------------------------------
          !< Read INTER= I/ALL
          !<--------------------------------------------------------------------
          is_all = 1
          is_lower = 0
          is_upper = 0
          is_def = 0
          is_memb = 0
          call create_h3d_arg_keyword(                                         &
                    key2       ,key3        ,key4       ,key5      ,key6      ,&
                    key7       ,key8        ,is_all     ,is_lower  ,is_upper  ,&
                    'INTER'    ,5           ,is_inter   ,inter     ,           &
                    is_inter_all,ibid1      ,ibid2      ,is_def    ,           &
                    ibid3      ,is_memb     ,ibid4      )
!
          !<--------------------------------------------------------------------
          !< Read ID= I/ALL
          !<--------------------------------------------------------------------
          is_all = 1
          is_lower = 0
          is_upper = 0
          is_def = 0
          is_memb = 0
          call create_h3d_arg_keyword(                                         &
                    key2       ,key3        ,key4      ,key5       ,key6      ,&
                    key7       ,key8        ,is_all    ,is_lower   ,is_upper  ,&
                    'ID'       ,2           ,is_id     ,id         ,           &
                    is_id_all  ,ibid1       ,ibid2     ,is_def     ,           &
                    ibid3      ,is_memb     ,ibid4     )
!
          !<--------------------------------------------------------------------
          !< Read MODE= I/ALL
          !<--------------------------------------------------------------------
          is_all = 1
          is_lower = 0
          is_upper = 0
          is_def = 0
          is_memb = 0
          call create_h3d_arg_keyword(                                         &
                    key2       ,key3        ,key4      ,key5       ,key6      ,&
                    key7       ,key8        ,is_all    ,is_lower   ,is_upper  ,&
                    'MODE'     ,4           ,is_mode   ,mode       ,           &
                    is_mode_all,ibid1       ,ibid2     ,is_def     ,           &
                    ibid3      ,is_memb     ,ibid4     )
!
          cpt = 0
          if ( is_char_key3 == 1 .and. is_empty_key3 == 0 ) then
            do i=1,ncharkey
              if ( key3(i:i) /= ' ' ) then
                cpt = cpt + 1
                key3_glob(cpt:cpt) = key3(i:i)
              endif
            enddo
          endif
          if ( is_char_key4 == 1 .and. is_empty_key4 == 0 ) then
            cpt = cpt + 1
            key3_glob(cpt:cpt) = '/'
            do i=1,ncharkey
              if ( key4(i:i) /= ' ' ) then
                cpt = cpt + 1
                key3_glob(cpt:cpt) = key4(i:i)
              endif
            enddo
          endif
          if ( is_char_key5 == 1 .and. is_empty_key5 == 0 ) then
            cpt = cpt + 1
            key3_glob(cpt:cpt) = '/'
            do i=1,ncharkey
              if ( key5(i:i) /= ' ' ) then
                cpt = cpt + 1
                key3_glob(cpt:cpt) = key5(i:i)
              endif
            enddo
          endif
          if ( is_char_key6 == 1 .and. is_empty_key6 == 0 ) then
            cpt = cpt + 1
            key3_glob(cpt:cpt) = '/'
            do i=1,ncharkey
              if ( key6(i:i) /= ' ' ) then
                cpt = cpt + 1
                key3_glob(cpt:cpt) = key6(i:i)
              endif
            enddo
          endif
          if ( is_char_key7 == 1 .and. is_empty_key7 == 0 ) then
            cpt = cpt + 1
            key3_glob(cpt:cpt) = '/'
            do i=1,ncharkey
              if ( key7(i:i) /= ' ' ) then
                cpt = cpt + 1
                key3_glob(cpt:cpt) = key7(i:i)
              endif
            enddo
          endif
          if ( is_char_key8 == 1 .and. is_empty_key8 == 0 ) then
            cpt = cpt + 1
            key3_glob(cpt:cpt) = '/'
            do i=1,ncharkey
              if ( key8(i:i) /= ' ' ) then
                cpt = cpt + 1
                key3_glob(cpt:cpt) = key8(i:i)
              endif
            enddo
          endif
!
          is_model_npt   = 0
          is_model_layer = 0
          is_model_ply   = 0
          do k=1,numgeo
            if (igeo(11,k) == 9  .or. igeo(11,k) == 11 .or. igeo(11,k) == 16)  &
              is_model_npt = 1
            if (igeo(11,k) == 10 .or. igeo(11,k) == 11 .or. igeo(11,k) == 16)  &
              is_model_layer = 1
            if (igeo(11,k) == 17 .or. igeo(11,k) == 51 .or. igeo(11,k) == 52)  &
              is_model_ply = 1
          enddo
!
          cpt = 0
          do k=1,nummat
            if (ipm(2,k) == 200) then
              cpt = cpt + 1
            endif
          enddo
!
          !<--------------------------------------------------------------------
          !<  KEYWORD MANAGEMENT
          !<--------------------------------------------------------------------
          nb_key    = 1
          cpt_key   = 1
          key3_read = key3_glob
          key2_read = key2
          do while (cpt_key <= nb_key)
            call h3d_gene_keyword(                                             &
              key2_read,key2     ,key3_read,key3_glob,nb_key   ,cpt_key  ,     &
              multi_fvm,is_model_npt,is_model_layer,is_model_ply,is_mdsvar,    &
              is_mdsvar_def,is_ply_all,is_layer_all,is_ipt,is_layer,is_ply,    &
              is_id    )
!
            !< Element type or keyword
            if (key2 == 'SHELL' .or. key2 =='ELEM' .or. key2 =='SOLID' .or.    &
                key2 == 'BRICK' .or. key2 =='QUAD' .or. key2 =='BEAM ' .or.    &
                key2 == 'SPRING'.or. key2 =='TRUSS'.or. key2 =='SPH' ) then
!
              !<================================================================
              !< Number of output storage units (element, layer, ply, etc.)
              !<================================================================
              !< LAYER=...
              do k = 1,numgeo
                !< Shell composite properties
                if (igeo(11,k) == 10 .or. igeo(11,k) == 11 .or.              &
                    igeo(11,k) == 16) then
                  nunits = nunits + igeo(4,k)
                !< Thickshell properties
                elseif (igeo(11,k) == 20 .or. igeo(11,k) == 21 .or.          &
                        igeo(11,k) == 22) then
                  nunits = nunits + max(1,igeo(30,k))
                endif
              enddo
!
              !< NPT=...
              do k=1,numgeo
                !< Shell properties
                if (igeo(11,k) == 1 .or. igeo(11,k) == 9) then
                  nunits = nunits + igeo(4,k)
                !< Integrated beam property
                elseif (igeo(11,k) == 18) then
                  nunits = nunits + igeo(3,k)
                endif
              enddo
!
              !< PLY=...
              do k=1,numgeo
                !< Ply properties
                if (igeo(11,k) == 19) then
                  nunits = nunits + igeo(4,k)
                endif
              enddo
              !< Ply properties
              do k=1,numply
                nunits = nunits + ply_info(2,k)
              enddo
!
              !< IR=.../IS=.../IT=...
              do k = 1,numgeo
                !< Solid and thickshells properties
                if (igeo(11,k) == 6  .or. igeo(11,k) == 14 .or.                &
                    igeo(11,k) == 20 .or. igeo(11,k) == 21 .or.                &
                    igeo(11,k) == 22) then
                  nunits = nunits + igeo(4,k)
                !< Cohesive properties
                elseif (igeo(11,k) == 43) then
                  nunits = nunits + 4
                endif
              enddo
              !<================================================================
!
              !<================================================================
              !< Size of the output lists
              !<================================================================
              !< UVAR=ALL
              if (is_uvar_all == 1) then
                do k=1,nummat
                  sizetot = sizetot + nunits*ipm(8,k)
                enddo
!
              !< MDS_VAR=ALL
              elseif (is_mdsvar_all == 1) then
                if(key3_glob(1:3) == 'MDS' ) then
                  do k = 1,mds_nmat
                    sizetot = sizetot + nunits*mds_ndepsvar(k)
                  enddo
                endif
!
              !< MDS_VAR=DEF
              elseif (is_mdsvar_def == 1) then
                if(key3_glob(1:3) == 'MDS' ) then
                  do k=1,mds_nmat
                    do l=1,mds_ndepsvar(k)
                      sizetot = sizetot + nunits*mds_output_table(l,k)
                    enddo
                  enddo
                endif
! 
              !< /H3D/ELEM/TENS/BSTRESS/ID=
              elseif (key4(1:7) == 'BSTRESS') then
                if (is_id_all == 1) then
                  do k=1,nummat
                    if (ipm(2,k) == 36) then
                      sizetot = sizetot + nunits
                    elseif (ipm(2,k) == 78) then
                      sizetot = sizetot + 3*nunits
                    elseif (ipm(2,k) == 87) then
                      sizetot = sizetot + 4*nunits
                    endif
                  enddo
                else
                  sizetot = sizetot + nunits
                endif
!
              !< /H3D/ELEM/FAILURE output definition
              elseif (key3_glob(1:7) == 'FAILURE') then
                !< If mode output is specified by user
                if ((is_mode > 0).or.(is_mode_all > 0)) then 
                  do imat = 1,nummat
                    nfail = mat_param(imat)%nfail
                    if (nfail > 0) then 
                      do j = 1,nfail
                        nmod = mat_param(imat)%fail(j)%nmod
                        if (nmod > 0) then 
                          if (is_id_all > 0) then 
                            if (mat_param(imat)%fail(j)%fail_id > 0) then 
                              if (is_mode_all > 0) then
                                sizetot = sizetot + (nmod+1)*nunits
                              elseif (is_mode > 0) then 
                                sizetot = sizetot + nunits
                              endif
                            endif
                          elseif (is_id > 0) then
                            if (mat_param(imat)%fail(j)%fail_id == id) then 
                              if (is_mode_all > 0) then
                                sizetot = sizetot + (nmod+1)*nunits
                              elseif (is_mode > 0) then 
                                sizetot = sizetot + nunits
                              endif
                            endif
                          endif
                        endif
                      enddo
                    endif
                  enddo
                !< Otherwise, classic global damage variable only
                else 
                  do imat = 1,nummat
                    nfail = mat_param(imat)%nfail
                    if (nfail > 0) then 
                      do j = 1,nfail
                        if (is_id_all > 0) then
                          if (mat_param(imat)%fail(j)%fail_id > 0) then 
                            sizetot = sizetot + nunits
                          endif
                        elseif (is_id > 0) then
                          if (mat_param(imat)%fail(j)%fail_id == id) then 
                            sizetot = sizetot + nunits
                          endif
                        endif
                      enddo   
                    endif
                  enddo
                endif
!
              !< /H3D/ELEM/DAMG output definition
              elseif (key3_glob(1:4) == 'DAMG') then 
                ! -> modes output definition
                if ((is_mode > 0).or.(is_mode_all > 0)) then 
                  if (is_id > 0) then 
                    do imat = 1,nummat
                      if (mat_param(imat)%mat_id == id) then 
                        nmod = mat_param(imat)%nmod
                        if (is_mode_all > 0) then
                          sizetot = sizetot + (nmod+1)*nunits
                        elseif (is_mode > 0) then 
                          sizetot = sizetot + nunits
                        endif
                      endif
                    enddo
                  endif                      
                else
                  sizetot = sizetot + nunits
                endif
!
              !< All other keywords
              else
                sizetot = sizetot + nunits
              endif
!
            endif
          enddo    
        enddo  
! 
        !< Allocation of the output list
        if (h3d_data%n_input_h3d > 0) then 
          sizetot = max(sizetot,10000)
          allocate(h3d_data%output_list(sizetot))
          h3d_data%output_list(1:sizetot)%ok = 0
        else
          allocate(h3d_data%output_list(1))
        endif
!
        end subroutine prelech3d
        end module prelech3d_mod