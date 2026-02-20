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
! =================================================================================================
!! \brief Read materials cards
! --------------------------------------------------------------------------------------------------
!||====================================================================
!||    hm_read_mat_mod        ../starter/source/materials/mat/hm_read_mat.F90
!||--- called by ------------------------------------------------------
!||    read_material_models   ../starter/source/materials/read_material_models.F
!||====================================================================
      module hm_read_mat_mod
        implicit none
      contains
! --------------------------------------------------------------------------------------------------
!! \brief Read materials cards
! --------------------------------------------------------------------------------------------------
!||====================================================================
!||    hm_read_mat                 ../starter/source/materials/mat/hm_read_mat.F90
!||--- called by ------------------------------------------------------
!||    read_material_models        ../starter/source/materials/read_material_models.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                      ../starter/source/output/message/message.F
!||    fretitl                     ../starter/source/starter/freform.F
!||    hm_option_read_key          ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start             ../starter/source/devtools/hm_reader/hm_option_start.F
!||    hm_read_mat00               ../starter/source/materials/mat/mat000/hm_read_mat00.F
!||    hm_read_mat01               ../starter/source/materials/mat/mat001/hm_read_mat01.F
!||    hm_read_mat02_jc            ../starter/source/materials/mat/mat002/hm_read_mat02_jc.F90
!||    hm_read_mat02_predef        ../starter/source/materials/mat/mat002/hm_read_mat02_predef.F90
!||    hm_read_mat02_zerilli       ../starter/source/materials/mat/mat002/hm_read_mat02_zerilli.F90
!||    hm_read_mat03               ../starter/source/materials/mat/mat003/hm_read_mat03.F
!||    hm_read_mat04               ../starter/source/materials/mat/mat004/hm_read_mat04.F
!||    hm_read_mat05               ../starter/source/materials/mat/mat005/hm_read_mat05.F
!||    hm_read_mat06               ../starter/source/materials/mat/mat006/hm_read_mat06.F
!||    hm_read_mat06_keps          ../starter/source/materials/mat/mat006/hm_read_mat06_keps.F
!||    hm_read_mat10               ../starter/source/materials/mat/mat010/hm_read_mat10.F
!||    hm_read_mat100              ../starter/source/materials/mat/mat100/hm_read_mat100.F
!||    hm_read_mat101              ../starter/source/materials/mat/mat101/hm_read_mat101.F
!||    hm_read_mat102              ../starter/source/materials/mat/mat102/hm_read_mat102.F
!||    hm_read_mat103              ../starter/source/materials/mat/mat103/hm_read_mat103.F
!||    hm_read_mat104              ../starter/source/materials/mat/mat104/hm_read_mat104.F
!||    hm_read_mat105              ../starter/source/materials/mat/mat105/hm_read_mat105.F90
!||    hm_read_mat106              ../starter/source/materials/mat/mat106/hm_read_mat106.F90
!||    hm_read_mat107              ../starter/source/materials/mat/mat107/hm_read_mat107.F
!||    hm_read_mat108              ../starter/source/materials/mat/mat108/hm_read_mat108.F
!||    hm_read_mat109              ../starter/source/materials/mat/mat109/hm_read_mat109.F
!||    hm_read_mat11               ../starter/source/materials/mat/mat011/hm_read_mat11.F
!||    hm_read_mat110              ../starter/source/materials/mat/mat110/hm_read_mat110.F
!||    hm_read_mat111              ../starter/source/materials/mat/mat111/hm_read_mat111.F
!||    hm_read_mat112              ../starter/source/materials/mat/mat112/hm_read_mat112.F
!||    hm_read_mat113              ../starter/source/materials/mat/mat113/hm_read_mat113.F
!||    hm_read_mat114              ../starter/source/materials/mat/mat114/hm_read_mat114.F
!||    hm_read_mat115              ../starter/source/materials/mat/mat115/hm_read_mat115.F
!||    hm_read_mat116              ../starter/source/materials/mat/mat116/hm_read_mat116.F
!||    hm_read_mat117              ../starter/source/materials/mat/mat117/hm_read_mat117.F
!||    hm_read_mat119              ../starter/source/materials/mat/mat119/hm_read_mat119.F
!||    hm_read_mat11_k_eps         ../starter/source/materials/mat/mat011/hm_read_mat11_k_eps.F
!||    hm_read_mat12               ../starter/source/materials/mat/mat012/hm_read_mat12.F
!||    hm_read_mat120              ../starter/source/materials/mat/mat120/hm_read_mat120.F
!||    hm_read_mat121              ../starter/source/materials/mat/mat121/hm_read_mat121.F
!||    hm_read_mat122              ../starter/source/materials/mat/mat122/hm_read_mat122.F
!||    hm_read_mat123              ../starter/source/materials/mat/mat123/hm_read_mat123.F90
!||    hm_read_mat124              ../starter/source/materials/mat/mat124/hm_read_mat124.F
!||    hm_read_mat125              ../starter/source/materials/mat/mat125/hm_read_mat125.F90
!||    hm_read_mat126              ../starter/source/materials/mat/mat126/hm_read_mat126.F90
!||    hm_read_mat127              ../starter/source/materials/mat/mat127/hm_read_mat127.F90
!||    hm_read_mat128              ../starter/source/materials/mat/mat128/hm_read_mat128.F90
!||    hm_read_mat129              ../starter/source/materials/mat/mat129/hm_read_mat129.F90
!||    hm_read_mat13               ../starter/source/materials/mat/mat013/hm_read_mat13.F
!||    hm_read_mat130              ../starter/source/materials/mat/mat130/hm_read_mat130.F90
!||    hm_read_mat133              ../starter/source/materials/mat/mat133/hm_read_mat133.F90
!||    hm_read_mat134              ../starter/source/materials/mat/mat134/hm_read_mat134.F90
!||    hm_read_mat14               ../starter/source/materials/mat/mat014/hm_read_mat14.F
!||    hm_read_mat15               ../starter/source/materials/mat/mat015/hm_read_mat15.F
!||    hm_read_mat151              ../starter/source/materials/mat/mat151/hm_read_mat151.F
!||    hm_read_mat158              ../starter/source/materials/mat/mat158/hm_read_mat158.F
!||    hm_read_mat16               ../starter/source/materials/mat/mat016/hm_read_mat16.F
!||    hm_read_mat163              ../starter/source/materials/mat/mat163/hm_read_mat163.F90
!||    hm_read_mat169_arup         ../starter/source/materials/mat/mat169/hm_read_mat169.F90
!||    hm_read_mat18               ../starter/source/materials/mat/mat018/hm_read_mat18.F
!||    hm_read_mat19               ../starter/source/materials/mat/mat019/hm_read_mat19.F
!||    hm_read_mat190              ../starter/source/materials/mat/mat190/hm_read_mat190.F
!||    hm_read_mat20               ../starter/source/materials/mat/mat020/hm_read_mat20.F
!||    hm_read_mat21               ../starter/source/materials/mat/mat021/hm_read_mat21.F
!||    hm_read_mat22               ../starter/source/materials/mat/mat022/hm_read_mat22.F
!||    hm_read_mat23               ../starter/source/materials/mat/mat023/hm_read_mat23.F
!||    hm_read_mat24               ../starter/source/materials/mat/mat024/hm_read_mat24.F
!||    hm_read_mat25               ../starter/source/materials/mat/mat025/hm_read_mat25.F
!||    hm_read_mat26               ../starter/source/materials/mat/mat026/hm_read_mat26.F
!||    hm_read_mat27               ../starter/source/materials/mat/mat027/hm_read_mat27.F
!||    hm_read_mat28               ../starter/source/materials/mat/mat028/hm_read_mat28.F
!||    hm_read_mat29_31            ../starter/source/materials/mat/matuser/hm_read_mat_user29_31.F
!||    hm_read_mat32               ../starter/source/materials/mat/mat032/hm_read_mat32.F
!||    hm_read_mat33               ../starter/source/materials/mat/mat033/hm_read_mat33.F
!||    hm_read_mat34               ../starter/source/materials/mat/mat034/hm_read_mat34.F
!||    hm_read_mat35               ../starter/source/materials/mat/mat035/hm_read_mat35.F
!||    hm_read_mat36               ../starter/source/materials/mat/mat036/hm_read_mat36.F
!||    hm_read_mat37               ../starter/source/materials/mat/mat037/hm_read_mat37.F
!||    hm_read_mat38               ../starter/source/materials/mat/mat038/hm_read_mat38.F
!||    hm_read_mat40               ../starter/source/materials/mat/mat040/hm_read_mat40.F
!||    hm_read_mat41               ../starter/source/materials/mat/mat041/hm_read_mat41.F
!||    hm_read_mat42               ../starter/source/materials/mat/mat042/hm_read_mat42.F
!||    hm_read_mat43               ../starter/source/materials/mat/mat043/hm_read_mat43.F
!||    hm_read_mat44               ../starter/source/materials/mat/mat044/hm_read_mat44.F
!||    hm_read_mat46               ../starter/source/materials/mat/mat046/hm_read_mat46.F
!||    hm_read_mat48               ../starter/source/materials/mat/mat048/hm_read_mat48.F
!||    hm_read_mat49               ../starter/source/materials/mat/mat049/hm_read_mat49.F
!||    hm_read_mat50               ../starter/source/materials/mat/mat050/hm_read_mat50.F90
!||    hm_read_mat51               ../starter/source/materials/mat/mat051/hm_read_mat51.F
!||    hm_read_mat52               ../starter/source/materials/mat/mat052/hm_read_mat52.F
!||    hm_read_mat53               ../starter/source/materials/mat/mat053/hm_read_mat53.F
!||    hm_read_mat54               ../starter/source/materials/mat/mat054/hm_read_mat54.F
!||    hm_read_mat57               ../starter/source/materials/mat/mat057/hm_read_mat57.F90
!||    hm_read_mat58               ../starter/source/materials/mat/mat058/hm_read_mat58.F
!||    hm_read_mat59               ../starter/source/materials/mat/mat059/hm_read_mat59.F
!||    hm_read_mat60               ../starter/source/materials/mat/mat060/hm_read_mat60.F
!||    hm_read_mat62               ../starter/source/materials/mat/mat062/hm_read_mat62.F
!||    hm_read_mat63               ../starter/source/materials/mat/mat063/hm_read_mat63.F
!||    hm_read_mat64               ../starter/source/materials/mat/mat064/hm_read_mat64.F
!||    hm_read_mat65               ../starter/source/materials/mat/mat065/hm_read_mat65.F
!||    hm_read_mat66               ../starter/source/materials/mat/mat066/hm_read_mat66.F
!||    hm_read_mat68               ../starter/source/materials/mat/mat068/hm_read_mat68.F
!||    hm_read_mat69               ../starter/source/materials/mat/mat069/hm_read_mat69.F
!||    hm_read_mat70               ../starter/source/materials/mat/mat070/hm_read_mat70.F
!||    hm_read_mat71               ../starter/source/materials/mat/mat071/hm_read_mat71.F
!||    hm_read_mat72               ../starter/source/materials/mat/mat072/hm_read_mat72.F
!||    hm_read_mat73               ../starter/source/materials/mat/mat073/hm_read_mat73.F
!||    hm_read_mat74               ../starter/source/materials/mat/mat074/hm_read_mat74.F
!||    hm_read_mat75               ../starter/source/materials/mat/mat075/hm_read_mat75.F
!||    hm_read_mat76               ../starter/source/materials/mat/mat076/hm_read_mat76.F
!||    hm_read_mat77               ../starter/source/materials/mat/mat077/hm_read_mat77.F
!||    hm_read_mat78               ../starter/source/materials/mat/mat078/hm_read_mat78.F
!||    hm_read_mat79               ../starter/source/materials/mat/mat079/hm_read_mat79.F
!||    hm_read_mat80               ../starter/source/materials/mat/mat080/hm_read_mat80.F
!||    hm_read_mat81               ../starter/source/materials/mat/mat081/hm_read_mat81.F90
!||    hm_read_mat82               ../starter/source/materials/mat/mat082/hm_read_mat82.F
!||    hm_read_mat83               ../starter/source/materials/mat/mat083/hm_read_mat83.F
!||    hm_read_mat84               ../starter/source/materials/mat/mat084/hm_read_mat84.F
!||    hm_read_mat87               ../starter/source/materials/mat/mat087/hm_read_mat87.F90
!||    hm_read_mat88               ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||    hm_read_mat90               ../starter/source/materials/mat/mat090/hm_read_mat90.F
!||    hm_read_mat92               ../starter/source/materials/mat/mat092/hm_read_mat92.F
!||    hm_read_mat93               ../starter/source/materials/mat/mat093/hm_read_mat93.F
!||    hm_read_mat94               ../starter/source/materials/mat/mat094/hm_read_mat94.F
!||    hm_read_mat95               ../starter/source/materials/mat/mat095/hm_read_mat95.F
!||    hm_read_mat97               ../starter/source/materials/mat/mat097/hm_read_mat97.F
!||    hm_read_mat_99              ../starter/source/materials/mat/matuser/hm_read_mat_user_99.F
!||    hm_read_matgas              ../starter/source/materials/mat/matgas/hm_read_matgas.F
!||    init_mat_keyword            ../starter/source/materials/mat/init_mat_keyword.F
!||    vdouble                     ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                ../starter/share/modules1/elbuftag_mod.F
!||    file_descriptor_mod         ../starter/source/modules/file_descriptor_mod.F90
!||    hm_option_read_mod          ../starter/share/modules1/hm_option_read_mod.F
!||    hm_read_mat02_jc_mod        ../starter/source/materials/mat/mat002/hm_read_mat02_jc.F90
!||    hm_read_mat02_predef_mod    ../starter/source/materials/mat/mat002/hm_read_mat02_predef.F90
!||    hm_read_mat02_zerilli_mod   ../starter/source/materials/mat/mat002/hm_read_mat02_zerilli.F90
!||    hm_read_mat105_mod          ../starter/source/materials/mat/mat105/hm_read_mat105.F90
!||    hm_read_mat106_mod          ../starter/source/materials/mat/mat106/hm_read_mat106.F90
!||    hm_read_mat123_mod          ../starter/source/materials/mat/mat123/hm_read_mat123.F90
!||    hm_read_mat125_mod          ../starter/source/materials/mat/mat125/hm_read_mat125.F90
!||    hm_read_mat126_mod          ../starter/source/materials/mat/mat126/hm_read_mat126.F90
!||    hm_read_mat127_mod          ../starter/source/materials/mat/mat127/hm_read_mat127.F90
!||    hm_read_mat128_mod          ../starter/source/materials/mat/mat128/hm_read_mat128.F90
!||    hm_read_mat129_mod          ../starter/source/materials/mat/mat129/hm_read_mat129.F90
!||    hm_read_mat130_mod          ../starter/source/materials/mat/mat130/hm_read_mat130.F90
!||    hm_read_mat132_mod          ../starter/source/materials/mat/mat132/hm_read_mat132.F90
!||    hm_read_mat133_mod          ../starter/source/materials/mat/mat133/hm_read_mat133.F90
!||    hm_read_mat134_mod          ../starter/source/materials/mat/mat134/hm_read_mat134.F90
!||    hm_read_mat163_mod          ../starter/source/materials/mat/mat163/hm_read_mat163.F90
!||    hm_read_mat169_arup_mod     ../starter/source/materials/mat/mat169/hm_read_mat169.F90
!||    hm_read_mat50_mod           ../starter/source/materials/mat/mat050/hm_read_mat50.F90
!||    hm_read_mat57_mod           ../starter/source/materials/mat/mat057/hm_read_mat57.F90
!||    hm_read_mat81_mod           ../starter/source/materials/mat/mat081/hm_read_mat81.F90
!||    hm_read_mat87_mod           ../starter/source/materials/mat/mat087/hm_read_mat87.F90
!||    hm_read_mat88_mod           ../starter/source/materials/mat/mat088/hm_read_mat88.F90
!||    law_user                    ../starter/source/user_interface/law_user.F
!||    message_mod                 ../starter/share/message_module/message_mod.F
!||    reader_old_mod              ../starter/share/modules1/reader_old_mod.F90
!||    submodel_mod                ../starter/share/modules1/submodel_mod.F
!||    table_mod                   ../starter/share/modules1/table_mod.F
!||====================================================================
        subroutine hm_read_mat(                                      &
        &                    mat_param   ,mlaw_tag    ,eos_tag     ,bufmat    ,&
        &                    buflen      ,iadbuf      ,ipm         ,pm        ,&
        &                    multi_fvm   ,unitab      ,lsubmodel   ,table     ,&
        &                    sbufmat     ,npropmi     ,npropm      ,           &
        &                    ialelag     ,ntable      ,nummat      ,hm_nummat ,&
        &                    ltitr       ,userl_avail ,mat_number  )
! --------------------------------------------------------------------------------------------------
!                                                   Modules
! --------------------------------------------------------------------------------------------------
          use constant_mod
          use file_descriptor_mod
          use unitab_mod
          use elbuftag_mod
          use message_mod
          use law_user
          use multi_fvm_mod
          use submodel_mod
          use matparam_def_mod
          use hm_option_read_mod
          use table_mod
          use hm_read_mat02_jc_mod
          use hm_read_mat02_zerilli_mod
          use hm_read_mat02_predef_mod
          use hm_read_mat50_mod
          use hm_read_mat57_mod
          use hm_read_mat81_mod
          use hm_read_mat87_mod
          use hm_read_mat105_mod , only : hm_read_mat105
          use hm_read_mat88_mod
          use hm_read_mat106_mod
          use hm_read_mat123_mod
          use hm_read_mat125_mod
          use hm_read_mat126_mod
          use hm_read_mat127_mod
          use hm_read_mat128_mod
          use hm_read_mat129_mod
          use hm_read_mat130_mod
          use hm_read_mat132_mod , only : hm_read_mat132
          use hm_read_mat133_mod , only : hm_read_mat133
          use hm_read_mat134_mod
          use hm_read_mat163_mod
          use hm_read_mat169_arup_mod
          use names_and_titles_mod ,only : nchartitle, ncharline
          use reader_old_mod     ,only : key0
          use multimat_param_mod ,only : m51_ssp0max,m51_lc0max,m51_tcp_ref,m51_lset_iflg6,m20_discrete_fill
          use precision_mod, only : WP
! -------------------------------------------------------------------------------------------------------
          implicit none
! -------------------------------------------------------------------------------------------------------
!      Arguments
! -------------------------------------------------------------------------------------------------------
          integer, intent(in)                  :: sbufmat
          integer, intent(in)                  :: npropmi
          integer, intent(in)                  :: npropm
          integer, intent(in)                  :: ntable
          integer, intent(in)                  :: nummat
          integer, intent(in)                  :: hm_nummat
          integer, intent(in)                  :: ltitr
          integer, intent(in)                  :: userl_avail
          type (unit_type_),intent(in)         :: unitab
          type(submodel_data),intent(in)       :: lsubmodel(nsubmod)
          integer, intent(inout)               :: ialelag
          integer, intent(inout)               :: mat_number
          integer ,intent(inout) :: buflen,iadbuf
          integer ,dimension(npropmi,nummat), intent(inout) :: ipm
          !
          real(kind=WP) ,dimension(npropm ,nummat), intent(inout) :: pm
          real(kind=WP) ,dimension(sbufmat), intent(inout)        :: bufmat
          type(ttable) ,dimension(ntable) ,intent(in)       :: table

          type(mlaw_tag_), target, dimension(nummat),intent(inout)    :: mlaw_tag
          type(eos_tag_) , target, dimension(0:maxeos) ,intent(inout) :: eos_tag
          type(multi_fvm_struct),intent(inout)                        :: multi_fvm
          type(matparam_struct_) ,dimension(nummat) ,intent(inout)    :: mat_param
          target :: mat_param
! -----------------------------------------------------------------------------
!     Local variables
! -----------------------------------------------------------------------------
          integer :: i,j,mat_id,uid,ilaw,jale,jtur,jthe,&
          &imatvis,israte,iuser_law,nfunc,numtabl,nuparam,nuvar,nvartmp,&
          &maxuparam,maxfunc,maxtabl,iunit,iflagunit,k
          parameter (maxuparam = 1048576)
          parameter (maxfunc  = 128, maxtabl = 9)
          real(kind=WP) :: rho,young,nu,bulk,g,asrate,rbid
          integer ,dimension(maxfunc) :: ifunc
          integer ,dimension(maxtabl) :: itable
          real(kind=WP) ,dimension(:), allocatable :: uparam
          real(kind=WP) ,dimension(128) :: parmat
          character(len=nchartitle) :: titr
          character(len=ncharline) :: key
          character(len=ncharline) :: solverkeyword
          character :: mess*40
          character(len = ncharline) :: key2
!
          type(ulawbuf) :: userbuf
          type(matparam_struct_) , pointer :: matparam
          type(mlaw_tag_) ,pointer         :: mtag
!-----------------------------------------------
          data mess/'MATERIAL DEFINITION                     '/
!==============================================================================
          allocate( uparam(maxuparam) )

          ilaw          = 0
          iuser_law     = 0
          imatvis       = 0
          m51_lset_iflg6 = 0

          m51_ssp0max = zero
          m51_lc0max  = zero
          m51_tcp_ref = zero

          multi_fvm%is_used = .false.
          m20_discrete_fill = .false.  ! bimat : partial fill enabled by default
!
!--------------------------------------------------
! start browsing model materials
!--------------------------------------------------
!
          call hm_option_start('MATERIAL')
!
!--------------------------------------------
!     call material law reading routines
!--------------------------------------------
!
          do i=1,hm_nummat
!
            mat_number    = i
            solverkeyword = ''
            titr = ''
            key  = ''

            call hm_option_read_key(lsubmodel,&
            &option_id   = mat_id,&
            &option_titr = titr  ,&
            &unit_id     = uid   ,&
            &keyword2    = key   ,&
            &keyword3    = key2)

!--------------------------------------------------
!       check if uid exists
!--------------------------------------------------
            iflagunit = 0
            do iunit=1,unitab%nunits
              if (unitab%unit_id(iunit) == uid) then
                iflagunit = 1
                exit
              endif
            enddo
            if (uid > 0 .and. iflagunit == 0) then
              call ancmsg(msgid=659,anmode=aninfo,msgtype=msgerror, &
              &           i2=uid,i1=mat_id,                         &
              &           C1='MATERIAL',                            &
              &           C2='MATERIAL',                            &
              &           C3='TITR')
            endif
!----
            call fretitl(titr,ipm(npropmi-ltitr+1,i),ltitr)
            userbuf%id   = mat_id   !ipm(1,i)
            userbuf%name = titr(1:nchartitle)
!
            parmat(:) = zero
            uparam(:) = zero
            ifunc(:)  = 0
            itable(:) = 0
            jtur    = 0
            jale    = 0
            jthe    = 0
!---------------------------------------------------
!        local material strain rate flag :
!               israte =-1 => no strain rate computation
!               israte = 0 => strain rate computation, no filtering (default)
!               israte > 0 => strain rate filtering using fcut and exponential average
            israte  = 0              ! default value
!---------------------------------------------------
            imatvis = 0
            nfunc   = 0
            numtabl = 0
            nuvar   = 0
            nvartmp = 0
            nuparam = 0
            mtag => mlaw_tag(mat_number)
            matparam => mat_param(mat_number)
            matparam%title = ' '
            matparam%title = titr(1:len_trim(titr))
            matparam%mat_id = mat_id
            matparam%young0 = zero
!-----------------------------------------------------------------------
!
            if (len_trim(key2) == 0) then
!     in case of /mat/gas, there is a third keyword,hence, key does not end with \000 character
              key = key(1:len_trim(key))
            endif
            select case(key)
!-------
             case ('LAW0','VOID')
              ilaw = 0
              call hm_read_mat00(mtag   ,&
              &ipm(1,i), pm(1,i), unitab, mat_id, titr, lsubmodel,israte,&
              &matparam)
!-------
             case ('LAW1','LAW01','ELAST')
              ilaw = 1
              call hm_read_mat01(&
              &ipm(1,i), pm(1,i), unitab, mat_id, titr, lsubmodel,israte,&
              &matparam)
!-------
             case ('LAW2','LAW02','PLAS_JOHNS','JOHNS')
              ilaw  = 2
              call hm_read_mat02_jc(matparam,mtag     ,parmat   ,           &
                nuvar    ,unitab  ,mat_id   ,titr     ,lsubmodel,           &
                npropm   ,pm(1,i) ,npropmi  ,ipm(1,i) )
!-------
             case ('ZERIL','PLAS_ZERIL')
              ilaw  = 2
              call hm_read_mat02_zerilli(matparam,mtag,parmat   ,           &
                nuvar    ,unitab  ,mat_id   ,titr     ,lsubmodel,           &
                npropm   ,pm(1,i) ,npropmi  ,ipm(1,i) )
!-------
             case ('PLAS_PREDEF')
              ilaw  = 2
              call hm_read_mat02_predef(matparam,mtag     ,parmat   ,       &
                nuvar    ,unitab  ,mat_id   ,titr     ,              &
                npropm   ,pm(1,i) ,npropmi  ,ipm(1,i) )
!-------
             case ('LAW3','LAW03', 'HYDPLA')
              ilaw=3
              call hm_read_mat03(&
              &lsubmodel ,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i) ,&
              &mat_id    ,titr     ,israte    ,matparam)
!-------
             case ('LAW4','LAW04', 'HYD_JCOOK')
              ilaw=4
              call hm_read_mat04(&
              &lsubmodel ,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i)  ,&
              &mat_id    ,titr     ,israte    ,matparam )
!-------
             case ('JWL','LAW5','LAW05')
              ilaw = 5
              call hm_read_mat05(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,uid      ,matparam)
!-------
             case ('LAW06','LAW6', 'HYDRO','HYD_VISC')
              ilaw  = 6
              call hm_read_mat06(&
              &lsubmodel ,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i),&
              &mat_id    ,titr     ,israte    ,1      ,&
              &mtag      ,matparam )
!-------
             case ('K-EPS')
              ilaw = 6
              jtur=1
              pm(70,i) =jtur+em01
              call hm_read_mat06_keps(&
              &lsubmodel ,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i),&
              &mat_id    ,titr     ,israte    ,1      ,&
              &mtag      ,matparam )
!-------
             case ('LAW10','DPRAG1')
              ilaw = 10
              call hm_read_mat10(&
              &lsubmodel ,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i)  ,&
              &mat_id    ,titr     ,eos_tag  ,israte    ,matparam )
!-------
             case ('LAW11','BOUND')
              ilaw = 11
              call hm_read_mat11(&
              &lsubmodel ,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i),&
              &mat_id    ,titr     ,matparam )
!-------
             case ('B-K-EPS')
              ilaw = 11
              jtur=1
              pm(70,i) =jtur+em01
              call hm_read_mat11_k_eps(&
              &lsubmodel ,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i),&
              &mat_id    ,titr     ,matparam )
!-------
             case ('LAW12','3D_COMP')
              ilaw = 12
              call hm_read_mat12(&
              &lsubmodel ,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i),&
              &mat_id    ,titr     ,israte   ,matparam  )
!-------
             case ('LAW13','RIGID')
              ilaw = 13
              call hm_read_mat13(&
              &lsubmodel ,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i),&
              &mat_id    ,titr     ,matparam )
!-------
             case ('LAW14','COMPSO')
              ilaw  = 14
              call hm_read_mat14(&
              &lsubmodel ,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i),&
              &mat_id    ,titr     ,matparam )
!-------
             case ('LAW15','CHANG')
              ilaw  = 15
              call hm_read_mat15(&
              &lsubmodel ,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i),&
              &mat_id    ,titr     ,matparam ,israte    ,parmat )
!-------
             case ('LAW16','GRAY')
              ilaw  = 16
              call hm_read_mat16(&
              &mtag     ,pm(1,i)    ,mat_id   ,titr  ,ipm(1,i)   ,&
              &lsubmodel,unitab     ,matparam )
!-------
             case ('LAW18','THERM')
              ilaw  = 18
              call hm_read_mat18(&
              &nuparam  ,nuvar    ,nfunc    ,mat_id   ,titr     ,&
              &unitab   ,lsubmodel,mtag     ,pm(1,i)  ,ipm(1,i) ,&
              &jthe     ,matparam )
!-------
             case ('LAW19','FABRI')
              ilaw = 19
              call hm_read_mat19(matparam,mtag  ,pm(1,i)  ,parmat   ,&
              &nuvar    ,mat_id   ,titr     ,&
              &unitab   ,lsubmodel,israte   )
!-------
             case ('LAW20','BIMAT')
              ilaw  = 20
              call hm_read_mat20(&
              &ipm(1,i) ,pm(1,i)  ,unitab   ,mat_id   ,titr     ,&
              &lsubmodel,mtag     ,matparam )
!-------
             case ('LAW21','DPRAG')
              ilaw = 21
              call hm_read_mat21(&
              &lsubmodel,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i) ,&
              &mat_id   ,titr     ,matparam )
!-------
             case ('LAW22','DAMA')
              ilaw  = 22
              call hm_read_mat22(&
              &uparam ,maxuparam ,nuparam  ,nuvar     ,ifunc   ,&
              &maxfunc,nfunc     ,parmat   ,imatvis   ,0       ,&
              &unitab ,mat_id    ,titr     ,lsubmodel ,mtag    ,&
              &pm(1,i),ipm(1,i)  ,israte   ,matparam  )
!-------
             case ('LAW23','PLAS_DAM')
              ilaw  = 23
              call hm_read_mat23(&
              &uparam ,maxuparam ,nuparam  ,nuvar     ,ifunc   ,&
              &maxfunc,nfunc     ,parmat   ,imatvis   ,0       ,&
              &unitab ,mat_id    ,titr     ,lsubmodel ,mtag    ,&
              &pm(1,i),ipm(1,i)  ,israte   ,matparam  )
!-------
             case ('LAW24','CONC')
              ilaw = 24
              call hm_read_mat24(&
              &nuparam  ,nuvar    ,nfunc    ,ipm(1,i) ,pm(1,i)  ,&
              &mtag     ,mat_id   ,titr     ,unitab   ,lsubmodel,&
              &israte   ,matparam )
!-------
             case ('LAW25','COMPSH')
              ilaw = 25
              call hm_read_mat25(&
              &uparam   ,maxuparam,nuparam   ,unitab   ,lsubmodel,&
              &mtag     ,mat_id   ,titr      ,pm(1,i)  ,israte   ,&
              &parmat   ,matparam )
!-------
             case ('LAW26','SESAM')
              ilaw = 26
              call hm_read_mat26(&
              &mtag     ,pm(1,i) ,mat_id   ,titr   ,ipm(1,i) ,&
              &jthe     ,bufmat  ,buflen   ,iadbuf ,lsubmodel,&
              &unitab   ,matparam)
!-------
             case ('LAW27','PLAS_BRIT')
              ilaw = 27
              call hm_read_mat27(&
              &mtag     ,parmat   ,unitab   ,pm(1,i)  ,lsubmodel,&
              &israte   ,mat_id   ,titr     ,matparam )
!-------
             case ('LAW28','HONEYCOMB')
              ilaw = 28
              call hm_read_mat28(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,matparam  )
!-------
             case ('LAW32','HILL')
              ilaw = 32
              call hm_read_mat32(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,matparam )
!-------
             case ('LAW33','FOAM_PLAS')
              ilaw = 33
              call hm_read_mat33(&
              &uparam   ,maxuparam,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc    ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id   ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,matparam )
!-------
             case ('LAW34','BOLTZMAN')
              ilaw  = 34
              call hm_read_mat34(&
              &uparam   ,maxuparam,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc    ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id   ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,matparam )
!-------
             case ('LAW35','FOAM_VISC')
              ilaw = 35
              call hm_read_mat35(&
              &uparam   ,maxuparam,nuparam   ,nuvar    ,ifunc    ,&
              &maxfunc  ,nfunc    ,parmat    ,unitab   ,pm(1,i)  ,&
              &israte   ,mat_id   ,titr      ,lsubmodel,imatvis  ,&
              &mtag     ,matparam )
!-------
             case ('LAW36','PLAS_TAB')
              ilaw  = 36
              call hm_read_mat36(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,nvartmp  ,&
              &ifunc    ,maxfunc  ,nfunc    ,parmat   ,unitab   ,&
              &mat_id   ,mtag     ,titr     ,lsubmodel,pm(1,i)  ,&
              &israte   ,matparam )
!-------
             case ('LAW37','BIPHAS')
              ilaw   = 37
              israte = 1
              call hm_read_mat37(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,parmat    ,mat_id   ,matparam)
!-------
             case ('LAW38','VISC_TAB')
              ilaw  = 38
              call hm_read_mat38(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,ifunc    ,&
              &maxfunc  ,nfunc    ,parmat   ,unitab   ,mat_id   ,&
              &mtag     ,titr     ,lsubmodel,pm(1,i)  ,imatvis  ,&
              &matparam )
!-------
             case ('LAW40','KELVINMAX')
              ilaw  = 40
              call hm_read_mat40(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,&
              &maxfunc  ,nfunc    ,parmat   ,unitab   ,mat_id   ,&
              &mtag     ,titr     ,lsubmodel,pm(1,i)  ,imatvis  ,&
              &matparam )
!-------
             case ('LAW41', 'LEE_TARVER')
              ilaw = 41
              call hm_read_mat41(uparam, maxuparam, nuparam,&
              &nuvar, ifunc, maxfunc, nfunc, parmat,&
              &mat_id, titr, unitab, lsubmodel, pm(1, i),&
              &matparam, mtag )
!-------
             case ('LAW42','OGDEN')
              ilaw = 42
              call hm_read_mat42(&
              &matparam ,nuvar    ,maxfunc  ,nfunc    ,ifunc    ,&
              &parmat   ,imatvis  ,unitab   ,lsubmodel,mat_id   ,&
              &titr     ,pm(1,i)  )
!-------
             case ('LAW43','HILL_TAB')
              ilaw = 43
              call hm_read_mat43(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,matparam )
!-------
             case ('LAW44','COWPER')
              ilaw = 44
              call hm_read_mat44(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,nfunc    ,&
              &mtag     ,parmat   ,unitab   ,pm(1,i)  ,lsubmodel,&
              &israte   ,mat_id   ,titr     ,ifunc    ,maxfunc  ,&
              &nvartmp  ,matparam )
!-------
             case ('LAW46','LES_FLUID')
              ilaw = 46
              call hm_read_mat46(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,matparam )
!-------
             case ('LAW48','ZHAO')
              ilaw = 48
              call hm_read_mat48(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,parmat   ,&
              &mat_id   ,pm(1,i)  ,titr     ,unitab   ,lsubmodel,&
              &israte   ,nfunc    ,mtag     ,matparam )
!-------
             case ('LAW49','STEINB')
              ilaw=49
              call hm_read_mat49(&
              &lsubmodel ,mtag     ,unitab   ,ipm(1,i)  ,pm(1,i) ,&
              &mat_id    ,titr     ,israte    ,matparam)
!-------
             case ('LAW50','VISC_HONEY')
              ilaw  = 50
              call hm_read_mat50(                                      &
                matparam ,mtag     ,parmat   ,nuvar    ,nvartmp  ,  &
                ntable   ,table    ,mat_id   ,iout     ,titr     ,  &
                unitab   ,lsubmodel)
!-------
             case ('LAW51','MULTIMAT')
              ilaw = 51
              call hm_read_mat51(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,mat_param, mat_number ,ipm(1,i)  ,nvartmp ,&
              &nummat)
!-------
             case ('LAW52','GURSON')
              ilaw  = 52
              call hm_read_mat52(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,nfunc    ,&
              &maxfunc  ,ifunc    ,mtag     ,parmat   ,unitab   ,&
              &pm(1,i)  ,lsubmodel,israte   ,mat_id   ,titr     ,&
              &itable   ,maxtabl  ,numtabl  ,matparam )
!-------
             case ('LAW53','TSAI_TAB')
              ilaw = 53
              call hm_read_mat53(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,matparam )
!-------
             case('LAW54','PREDIT')
              ilaw = 54
              call hm_read_mat54(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,ifunc    ,&
              &maxfunc  ,nfunc    ,parmat   ,unitab   ,mat_id   ,&
              &mtag     ,titr     ,lsubmodel,pm(1,i)  ,matparam )
!-------
             case ('LAW57','BARLAT3','BARLAT')
              ilaw  = 57
              call hm_read_mat57(&
              &matparam ,nuvar    ,parmat   ,mat_id   ,titr     ,&
              &unitab   ,lsubmodel,mtag     ,iout     ,nvartmp  ,&
              &israte   ,ntable   ,table    )
!-------
             case ('LAW58','FABR_A')
              ilaw  = 58
              call hm_read_mat58(matparam ,nuvar    ,nfunc    ,&
              &maxfunc  ,ifunc    ,mtag     ,parmat   ,&
              &unitab   ,lsubmodel,mat_id   ,titr     )
              pm(23,i) = matparam%young
!-------
             case ('LAW59','CONNECT')
              ilaw  = 59
              call hm_read_mat59(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,matparam  )
!-------
             case ('LAW60','PLAS_T3')
              ilaw = 60
              call hm_read_mat60(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,matparam )
!-------
             case ('LAW62','VISC_HYP')
              ilaw = 62
              call hm_read_mat62(&
              &uparam   ,maxuparam ,nuparam  ,nuvar    ,nfunc    ,&
              &parmat   ,unitab    ,pm(1,i)  ,mat_id   ,titr     ,&
              &imatvis  ,lsubmodel ,matparam )
!-------
             case ('LAW63','HANSEL')
              ilaw = 63
              call hm_read_mat63(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,parmat   ,&
              &mat_id   ,pm(1,i)  ,titr     ,unitab   ,lsubmodel,&
              &mtag     ,matparam )
!-------
             case ('LAW64','UGINE_ALZ')
              ilaw = 64
              call hm_read_mat64(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,matparam )
!-------
             case ('LAW65','ELASTOMER')
              ilaw  = 65
              call hm_read_mat65(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,nfunc    ,&
              &maxfunc  ,ifunc    ,mtag     ,parmat   ,unitab   ,&
              &pm(1,i)  ,lsubmodel,israte   ,mat_id   ,titr     ,&
              &matparam )
!-------
             case ('LAW66')
              ilaw  = 66
              call hm_read_mat66(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,ifunc    ,&
              &maxfunc  ,nfunc    ,parmat   ,unitab   ,mat_id   ,&
              &mtag     ,titr     ,lsubmodel,pm(1,i)  ,matparam )
              israte = 1
!-------
             case ('LAW68','COSSER')
              ilaw = 68
              call hm_read_mat68(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,matparam )
!-------
             case ('LAW69')
              ilaw  = 69
              call hm_read_mat69(&
              &uparam ,maxuparam,nuparam  ,israte   ,imatvis  ,&
              &nuvar  ,ifunc    ,maxfunc  ,nfunc    ,parmat   ,&
              &unitab ,mat_id   ,titr     ,mtag     ,lsubmodel,&
              &pm(1,i),matparam )
!-------
             case ('LAW70','FOAM_TAB')
              ilaw = 70
              call hm_read_mat70(&
              &uparam   ,maxuparam,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc    ,maxfunc  ,nfunc    ,parmat   ,&
              &unitab   ,mat_id   ,titr     ,mtag     ,lsubmodel,&
              &pm(1,i)  ,matparam ,nvartmp  )
!-------
             case ('LAW71')
              ilaw  = 71
              call hm_read_mat71(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,nfunc    ,&
              &maxfunc  ,mtag     ,parmat   ,unitab   ,&
              &pm(1,i)  ,lsubmodel,mat_id   ,titr     ,imatvis  ,&
              &matparam )
!-------
             case ('LAW72','HILL_MMC')
              ilaw = 72
              call hm_read_mat72(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,matparam  )
!-------
             case ('LAW73')
              ilaw = 73
              call hm_read_mat73(&
              &uparam   ,maxuparam ,nuparam  ,israte   ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc    ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag     ,lsubmodel,&
              &itable   ,maxtabl  ,numtabl   ,pm(1,i)  ,ipm(1,i) ,&
              &matparam )
!-------
             case ('LAW74')
              ilaw = 74
              call hm_read_mat74(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &itable   ,maxtabl  ,numtabl   ,pm(1,i) ,ipm(1,i) ,&
              &matparam )
!-------
             case ('LAW75','POROUS')
              ilaw = 75
              call hm_read_mat75(&
              &uparam   ,maxuparam ,nuparam  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,matparam  )
!-------
             case ('LAW76','SAMP')
              ilaw  = 76
              call hm_read_mat76(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,ifunc    ,&
              &maxfunc  ,nfunc    ,parmat   ,unitab   ,mat_id   ,&
              &mtag     ,titr     ,lsubmodel,pm(1,i)  ,israte   ,&
              &matparam ,maxtabl  ,numtabl  ,itable   ,nvartmp  )
!-------
             case ('LAW77')
              ilaw  = 77
              ialelag = 1
              call hm_read_mat77(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,nfunc    ,&
              &maxfunc  ,ifunc    ,parmat   ,mat_id   ,pm(1,i)  ,&
              &israte   ,imatvis  ,titr     ,unitab   ,lsubmodel,&
              &matparam ,jale     )
!-------
             case ('LAW78','YUMODEL')
              ilaw = 78
              call hm_read_mat78(&
              &uparam   ,maxuparam ,nuparam  ,israte   ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc    ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag     ,lsubmodel,&
              &pm(1,i)  ,nvartmp   ,matparam )
!-------
             case ('LAW79','JOHN_HOLM')
              ilaw = 79
              call hm_read_mat79(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,nfunc    ,&
              &maxfunc  ,ifunc    ,parmat   ,mat_id   ,pm(1,i)  ,&
              &israte   ,mtag     ,titr     ,unitab   ,lsubmodel,&
              &matparam )
!-------
             case ('LAW80')
              ilaw = 80
              call hm_read_mat80(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,nfunc    ,&
              &maxfunc  ,ifunc    ,parmat   ,mat_id   ,pm(1,i)  ,&
              &israte   ,mtag     ,titr     ,unitab   ,lsubmodel,&
              &itable   ,maxtabl  ,numtabl  ,nvartmp  ,table    ,&
              &matparam )
!-------
             case ('LAW81','DPRAG_CAP')
              ilaw = 81
              call hm_read_mat81(&
              &matparam ,nuvar    ,ifunc    ,maxfunc  ,nfunc    ,&
              &parmat   ,mat_id   ,titr     ,unitab   ,lsubmodel,&
              &mtag     ,iout     ,nvartmp  )
!-------
             case ('LAW82')
              ilaw = 82
              call hm_read_mat82(&
              &uparam   ,maxuparam ,nuparam  ,nuvar    ,ifunc    ,&
              &maxfunc  ,nfunc    ,parmat   ,imatvis  ,unitab    ,&
              &mat_id   ,titr     ,lsubmodel,pm(1,i)  ,matparam  )
!-------
             case ('LAW83')
              ilaw = 83
              call hm_read_mat83(&
              &uparam   ,maxuparam  ,nuparam  ,mtag    ,pm(1,i)  ,&
              &nuvar    ,ifunc      ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id     ,titr     ,israte  ,lsubmodel,&
              &matparam )
!-------
             case ('LAW84')
              ilaw = 84
              call hm_read_mat84(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &itable   ,maxtabl   ,numtabl  ,pm(1,i) ,ipm(1,i) ,&
              &matparam )
!-------
             case ('LAW87','BARLAT2000')
              ilaw  = 87
              call hm_read_mat87(&
              &matparam ,nuvar    ,parmat   ,mat_id   ,titr     ,&
              &unitab   ,lsubmodel,mtag     ,iout     ,nvartmp  ,&
              &israte   ,ntable   ,table    ,maxfunc  )
!-------
             case ('LAW88','MLAW88','TABULATED_HYPERELASTIC')
              ilaw = 88
              call hm_read_mat88(&
              &matparam ,nvartmp  ,parmat   ,unitab   ,mat_id   ,&
              &titr     ,mtag     ,lsubmodel,iout     ,nuvar    ,&
              &ilaw     ,ntable   ,table    ,imatvis  ,israte   ,&
              &maxfunc  ,iunit    )
!-------
             case ('LAW90')
              ilaw = 90
              call hm_read_mat90(&
              &uparam  ,maxuparam  ,nuparam ,nuvar    ,ifunc ,&
              &maxfunc ,nfunc      ,parmat  ,unitab   ,mat_id ,&
              &titr    ,israte     ,pm(1,i) ,imatvis  ,lsubmodel,&
              &mtag    ,matparam   ,nvartmp )
!-------
             case ('LAW92')
              ilaw = 92
              call hm_read_mat92(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,matparam )
!-------
             case ('LAW93','ORTH_HILL','CONVERSE')
              ilaw = 93
              call hm_read_mat93(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,matparam ,nvartmp )
!-------
             case ('LAW94','YEOH')
              ilaw = 94
              call hm_read_mat94(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,matparam )
!-------
             case ('LAW95','BERGSTROM_BOYCE')
              ilaw  = 95
              call hm_read_mat95(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,nfunc    ,&
              &maxfunc  ,mtag     ,parmat   ,unitab   ,imatvis  ,&
              &pm(1,i)  ,lsubmodel,mat_id   ,titr     ,matparam )
!-------
             case ('LAW97','JWLB')
              ilaw = 97
              call hm_read_mat97(&
              &uparam   ,maxuparam ,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc     ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id    ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,matparam )
!-------
             case ('LAW100','MNF')
              ilaw  = 100
              call hm_read_mat100(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,ifunc     ,&
              &nfunc    ,maxfunc  ,mtag     ,parmat   ,unitab    ,&
              &imatvis  ,pm(1,i)  ,lsubmodel,mat_id   ,titr      ,&
              &matparam )
!-------
             case ('LAW101','PP')
              ilaw  = 101
              call hm_read_mat101(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,ifunc     ,&
              &maxfunc  ,nfunc    ,parmat   ,unitab   ,imatvis   ,&
              &pm(1,i)  ,lsubmodel,mat_id   ,titr     ,matparam  )
!-------
             case ('LAW102','DPRAG2')
              ilaw  = 102
              call hm_read_mat102(&
              &uparam   ,maxuparam,nuparam  ,israte  ,&
              &nuvar    ,nfunc    ,parmat   ,&
              &unitab   ,mat_id   ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i) ,matparam )
!-------
             case ('LAW103','HENSEL-SPI', 'FLAW103')
              ilaw  = 103
              call hm_read_mat103(&
              &uparam   ,maxuparam,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc    ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id   ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i) ,matparam )
!-------
             case ('LAW104','JOHNS_VOCE_DRUCKER')
              ilaw  = 104
              call hm_read_mat104(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,nfunc    ,&
              &maxfunc  ,ifunc    ,parmat   ,unitab   ,mat_id   ,&
              &pm(1,i)  ,titr     ,mtag     ,lsubmodel,matparam )
!-------
             case ('LAW105','POWDER_BURN','POWDER-BURN')
              ilaw  = 105
              call hm_read_mat105(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,nfunc    ,&
              &maxfunc  ,ifunc    ,parmat   ,unitab   ,mat_id   ,&
              &pm(1,i)  ,titr     ,mtag     ,lsubmodel,matparam ,&
              &npropm   )
!-------
             case ('LAW106','JCOOK_ALM')
              ilaw  = 106
              call hm_read_mat106(&
              &matparam ,nuvar    ,nfunc    ,parmat  ,unitab   ,&
              &mat_id   ,titr     ,mtag     ,nvartmp ,lsubmodel,&
              &ntable   ,table    ,iout     ,israte  )
!-------
             case ('LAW107','PAPER_LIGHT','PFEIFFER')
              ilaw = 107
              call hm_read_mat107(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,numtabl  ,&
              &mtag     ,parmat   ,unitab   ,pm(1,i)  ,lsubmodel,&
              &israte   ,mat_id   ,titr     ,itable   ,maxtabl  ,&
              &nvartmp  ,matparam )
!-------
             case ('LAW108','SPR_GENE')
              ilaw  = 108
              call hm_read_mat108(&
              &uparam   ,maxuparam,nuparam  ,nfunc    ,parmat   ,&
              &unitab   ,pm(1,i)  ,lsubmodel,israte   ,mat_id   ,&
              &titr     ,ifunc    ,maxfunc  ,mtag     ,matparam )
!-------
             case ('LAW109')
              ilaw  = 109
              call hm_read_mat109(&
              &uparam   ,maxuparam,nuparam   ,nuvar    ,nvartmp  ,&
              &itable   ,maxtabl  ,numtabl   ,parmat   ,unitab   ,&
              &mat_id   ,titr     ,rho       ,mtag     ,matparam ,&
              &lsubmodel )
              pm(1 ,i) = rho
              pm(89,i) = rho
!-------
             case ('LAW110','VEGTER')
              ilaw = 110
              call hm_read_mat110(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,numtabl  ,&
              &mtag     ,parmat   ,unitab   ,pm(1,i)  ,lsubmodel,&
              &israte   ,mat_id   ,titr     ,itable   ,maxtabl  ,&
              &nvartmp  ,matparam )
!-------
             case ('LAW111', 'MARLOW')
              ilaw = 111
              call hm_read_mat111(&
              &uparam   ,maxuparam,nuparam  ,israte  ,imatvis  ,&
              &nuvar    ,ifunc    ,maxfunc  ,nfunc   ,parmat   ,&
              &unitab   ,mat_id   ,titr     ,mtag    ,lsubmodel,&
              &pm(1,i)  ,ipm(1,i)  ,matparam )
!-------
             case ('LAW112','PAPER','XIA')
              ilaw = 112
              call hm_read_mat112(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,numtabl  ,&
              &mtag     ,parmat   ,unitab   ,pm(1,i)  ,lsubmodel,&
              &israte   ,mat_id   ,titr     ,itable   ,maxtabl  ,&
              &nvartmp  ,matparam )
!-------
             case ('LAW113','SPR_BEAM')
              ilaw  = 113
              call hm_read_mat113(&
              &uparam   ,maxuparam,nuparam  ,nfunc    ,parmat   ,&
              &unitab   ,pm(1,i)  ,lsubmodel,israte   ,mat_id   ,&
              &titr     ,ifunc    ,maxfunc  ,mtag     ,matparam )
!-------
             case ('LAW114','SPR_SEATBELT')
              ilaw  = 114
              call hm_read_mat114(&
              &uparam   ,maxuparam,nuparam  ,nfunc    ,parmat   ,&
              &unitab   ,pm(1,i)  ,lsubmodel,israte   ,mat_id   ,&
              &titr     ,ifunc    ,maxfunc  ,mtag     ,matparam )
!-------
             case ('LAW115','DESHFLECK')
              ilaw = 115
              call hm_read_mat115(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,numtabl  ,&
              &mtag     ,parmat   ,unitab   ,pm(1,i)  ,lsubmodel,&
              &israte   ,mat_id   ,titr     ,itable   ,maxtabl  ,&
              &nvartmp  ,matparam )
!-------
             case ('LAW116')
              ilaw  = 116
              call hm_read_mat116(&
              &mtag     ,uparam   ,maxuparam,nuparam  ,pm(1,i)  ,&
              &parmat   ,nuvar    ,ifunc    ,nfunc    ,maxfunc  ,&
              &unitab   ,mat_id   ,titr     ,lsubmodel,matparam )
!-------
             case ('LAW117')
              ilaw  = 117
              call hm_read_mat117(&
              &mtag     ,uparam   ,maxuparam,nuparam  ,pm(1,i)  ,&
              &parmat   ,nuvar    ,maxfunc  ,nfunc    ,ifunc    ,&
              &unitab   ,mat_id   ,titr     ,lsubmodel,matparam )
!-------
             case ('LAW119','SH_SEATBELT')
              ilaw = 119
              call hm_read_mat119(&
              &mtag     ,uparam   ,maxuparam,nuparam  ,pm(1,i)  ,&
              &matparam ,parmat   ,nuvar    ,mat_id   ,titr     ,&
              &maxtabl  ,numtabl  ,itable   ,unitab   ,lsubmodel,&
              &israte   )
!-------
             case ('LAW120','TAPO')
              ilaw  = 120
              call hm_read_mat120(&
              &mtag     ,uparam   ,maxuparam,maxtabl  ,nuparam  ,&
              &nuvar    ,nvartmp  ,numtabl  ,itable   ,parmat   ,&
              &matparam ,pm(1,i)  ,mat_id   ,titr     ,israte   ,&
              &unitab   ,lsubmodel)
!-------
             case ('LAW121','PLAS_RATE')
              ilaw  = 121
              call hm_read_mat121(&
              &uparam   ,maxuparam,nuparam  ,nuvar    ,maxfunc  ,&
              &nfunc    ,ifunc    ,mtag     ,parmat   ,unitab   ,&
              &pm(1,i)  ,lsubmodel,israte   ,mat_id   ,titr     ,&
              &matparam )
!-------
          case ('LAW122','MODIFIED_LADEVEZE')
            ilaw = 122
            call hm_read_mat122(&
            &uparam   ,maxuparam,nuparam  ,nuvar    ,maxfunc  ,&
            &nfunc    ,ifunc    ,mtag     ,parmat   ,unitab   ,&
            &pm(1,i)  ,lsubmodel,israte   ,mat_id   ,titr     ,&
            &matparam ,nvartmp  )

          case ('LAW123','LAMINATED_FRACTURE_DAIMLER_PINHO')
            ilaw = 123
            call hm_read_mat123(                              &
           &nuvar    ,maxfunc  ,npropm  , iout   ,            & 
           & mtag     ,parmat   ,unitab  ,ntable   ,table,    &
           &pm(1,i)  ,lsubmodel,israte   ,mat_id   ,titr ,    &
           &matparam ,nvartmp )     
!-------
             case ('LAW124','CDPM2')
              ilaw  = 124
              call hm_read_mat124(&
                uparam   ,maxuparam,nuparam  ,nuvar    ,mtag     ,&
                parmat   ,unitab   ,pm(1,i)  ,lsubmodel,israte   ,&
                mat_id   ,titr     ,matparam )
!-------
             case ('LAW125','LAMINATED_COMPOSITE')
              ilaw = 125
              call hm_read_mat125(&
              &nuvar    ,maxfunc  ,npropm  , iout     ,&
              &nfunc    ,ifunc    ,mtag     ,parmat   ,unitab   ,&
              &pm(1,i)  ,lsubmodel,israte   ,mat_id   ,titr     ,&
              &matparam ,nvartmp  )
!-------
             case ('LAW126','JOHNSON_HOLMQUIST_CONCRETE')
              ilaw = 126
              call hm_read_mat126(&
              &nuvar    ,mtag     ,matparam ,iout     ,parmat   ,&
              &unitab   ,lsubmodel,israte   ,mat_id   ,titr     )
!-------
             case ('LAW127','ENHANCED_COMPOSITE')
              ilaw = 127
              call hm_read_mat127(&
              &nuvar    ,maxfunc  ,npropm  , iout     ,&
              &nfunc    ,ifunc    ,mtag     ,parmat   ,unitab   ,&
              &pm(1,i)  ,lsubmodel,israte   ,mat_id   ,titr     ,&
              &matparam ,nvartmp  )
!-------
             case ('LAW128')
              ilaw = 128
              call hm_read_mat128(                                &
                matparam ,mtag     ,parmat   ,nuvar    ,nvartmp  ,  &
                ntable   ,table    ,mat_id   ,titr     ,iout     ,  &
                unitab   ,lsubmodel)
!-------
             case ('LAW129','THERMO_ELASTO_VISCOPLASTIC_CREEP')
              ilaw = 129
              call hm_read_mat129(mat_param(mat_number),          &
                mtag     ,parmat   ,nuvar    ,nvartmp  ,            &
                ntable   ,table    ,iout     ,unitab   ,lsubmodel)
!-------
             case ('LAW130','MODIFIED_HONEYCOMB')
              ilaw  = 130
              call hm_read_mat130(&
                mat_param(mat_number),nuvar  ,mtag     ,iout     ,&
                parmat   ,unitab   ,lsubmodel,israte   ,mat_id   ,&
                titr     ,table    ,ntable   ,nvartmp  ,imatvis  ,&
                iunit    )
!-------
             case('LAW132', 'LAMINATED_FRACTURE_DAIMLER_CAMANHO')
               ilaw = 132
               call hm_read_mat132(                              &
                nuvar    ,maxfunc  ,npropm  , iout   ,           & 
                mtag     ,parmat   ,unitab  ,ntable   ,table,    &
                pm(1,i)  ,lsubmodel,israte   ,mat_id   ,titr ,    &
                matparam ,nvartmp )   
!-------  
             case ('LAW133','GRANULAR')
              ilaw = 133
              call hm_read_mat133( &
                nuvar    ,mtag     , matparam ,iout     ,parmat   , &
                unitab   ,lsubmodel, mat_id   ,titr     ,nvartmp  , &
                ntable   ,table    , npropm   ,npropmi  , &
                pm(1,i)  ,ipm(1,i) )
!-------
             case ('LAW134','VISCOUS_FOAM')
              ilaw  = 134
              call hm_read_mat134(mtag     ,&
              &matparam  ,   parmat , nuvar  ,  unitab    ,lsubmodel,&
              &mat_id    ,titr       ,iout     )
!-------
             case ('LAW151','MULTIFLUID')
              ilaw  = 151
              multi_fvm%is_used = .true.
              call hm_read_mat151(mtag   ,pm(1, i) ,ipm(1, i),mat_id   ,titr   ,&
              &multi_fvm,unitab ,lsubmodel,matparam )
!-------
             case ('LAW158','FABR_NL')
              ilaw  = 158
              call hm_read_mat158(matparam ,nuvar    ,nfunc    ,&
              &maxfunc  ,ifunc    ,mtag     ,unitab   ,&
              &lsubmodel,mat_id   ,titr     )
!-------
             case ('LAW163','CRUSHABLE_FOAM')
              ilaw  = 163
              call hm_read_mat163(&
              &matparam ,nvartmp  ,parmat   ,unitab   ,mat_id   ,&
              &titr     ,mtag     ,lsubmodel,iout     ,nuvar    ,&
              &ilaw     ,ntable   ,table    )
!-------
             case ('LAW169','ARUP_ADHESIVE')
              ilaw  = 169
              call hm_read_mat169_arup(mtag     ,&
              &matparam  ,   parmat , nuvar  ,  unitab    ,lsubmodel,&
              &mat_id    ,titr      , pm(1,i)    ,iout     , npropm)
!-------
             case ('LAW190','FOAM_DUBOIS')
              ilaw  = 190
              call hm_read_mat190(&
              &nuvar    ,numtabl    ,&
              &maxtabl  ,itable   ,parmat   ,unitab     ,&
              &pm(1,i)  ,lsubmodel,mat_id   ,titr     ,matparam   ,&
              &nvartmp ,imatvis)
!-------
             case ('GAS')
              ilaw = 999
              call hm_read_matgas(pm(1, i), ipm(1, i),mat_id, titr, key2, unitab, lsubmodel)
!-------
             case ('LAW29','USER1')
              ilaw  = 29
              israte = 1
              pm(9,i) = ep20
              mtag%g_temp = 1
              mtag%l_temp = 1
              mtag%g_pla  = 1
              mtag%l_pla  = 1
              call hm_read_mat29_31(ilaw,key,&
              &userl_avail,&
              &uparam,maxuparam,nuparam,&
              &nuvar,ifunc,maxfunc,nfunc,&
              &parmat,&
              &lsubmodel,&
              &pm(1,i),matparam)
!-------
             case ('LAW30','USER2')
              ilaw  = 30
              israte = 1
              pm(9,i) = ep20
              mtag%g_temp = 1
              mtag%l_temp = 1
              mtag%g_pla  = 1
              mtag%l_pla  = 1
              call hm_read_mat29_31(ilaw,key,&
              &userl_avail,&
              &uparam,maxuparam,nuparam,&
              &nuvar,ifunc,maxfunc,nfunc,&
              &parmat,&
              &lsubmodel,&
              &pm(1,i),matparam)
!-------
             case ('LAW31','USER3')
              ilaw  = 31
              israte = 1
              pm(9,i) = ep20
              mtag%g_temp = 1
              mtag%l_temp = 1
              mtag%g_pla  = 1
              mtag%l_pla  = 1
              call hm_read_mat29_31(ilaw,key,&
              &userl_avail,&
              &uparam,maxuparam,nuparam,&
              &nuvar,ifunc,maxfunc,nfunc,&
              &parmat,&
              &lsubmodel,&
              &pm(1,i),matparam)

!-------
             case ('USER01','USER02','USER03','USER04','USER05','USER06',&
             &'USER07','USER08','USER09','USER10','USER11','USER12',&
             &'USER13','USER14','USER15','USER16','USER17','USER18',&
             &'USER19','USER20','USER21','USER22','USER23','USER24',&
             &'USER25','USER26','USER27','USER28','USER29','USER30',&
             &'USER31','USER32','USER33','USER34','USER35','USER36',&
             &'USER37','USER38','USER39','USER40','USER41','USER42',&
             &'USER43','USER44','USER45','USER46','USER47','USER48',&
             &'USER49','USER50','USER51','USER52','USER53','USER54',&
             &'USER55','USER56','USER57','USER58','USER59','USER60',&
             &'USER61','USER62','USER63','USER64','USER65','USER66',&
             &'USER67','USER68','USER69','USER70','USER71','USER72',&
             &'USER73','USER74','USER75','USER76','USER77','USER78',&
             &'USER79','USER80','USER81','USER82','USER83','USER84',&
             &'USER85','USER86','USER87','USER88','USER89','USER90',&
             &'USER91','USER92','USER93','USER94','USER95','USER96',&
             &'USER97','USER98','USER99')
!
              ilaw  = 99
!!            israte = 1
              pm(9,i) = ep20
              read(key(5:6), '(I2)') iuser_law
!
              call hm_read_mat_99(ilaw,iuser_law,key,&
              &userl_avail,&
              &uparam,maxuparam,nuparam,&
              &nuvar,ifunc,maxfunc,nfunc,&
              &parmat,userbuf,&
              &lsubmodel,&
              &pm(1,i),matparam)
              mtag%g_temp = 1
              mtag%l_temp = 1
              mtag%g_pla  = 1
              mtag%l_pla  = 1
!-------
#ifdef DNC
             case ('LAW200', 'MDS')
              read(key(5:6), '(I2)') iuser_law
              ilaw  = 200
!!            israte = 1
              mtag%g_temp = 1
              mtag%l_temp = 1
              mtag%g_pla  = 1
              mtag%l_pla  = 1
              call init_mat_keyword(matparam,"ORTHOTROPIC")
              ! Properties compatibility
              call init_mat_keyword(matparam,"SOLID_ISOTROPIC")
              call init_mat_keyword(matparam,"SHELL_ISOTROPIC")
!
              call hm_read_mat_mds(ilaw,i,mat_id,&
              &uparam,maxuparam,nuparam,&
              &nuvar,ifunc,maxfunc,nfunc,&
              &lsubmodel,&
              &pm(1,i) ,parmat)
#endif
            end select
!
            matparam%ilaw   = ilaw
!-----------------------------------------------------------------------
            ! set pmin default value to -inf for eos compatible materials

            if (matparam%compatibility_eos == 1 .and. pm(37,i) == zero) pm(37,i) = -ep20

!-----------------------------------------------------------------------
            if (ilaw == 99) then  ! write header info for user laws
              write(iout,2000) titr,mat_id,iuser_law
            endif
!--------------------------------------------
            matparam%ilaw   = ilaw
            matparam%mat_id = mat_id
!
            mtag%nuvar   = nuvar
            mtag%nvartmp = nvartmp
!--------------------------------------------
!           for user type laws (lecmuser)
!---------------------------------------------------------

            if (ilaw > 27 .and. ilaw /= 32 .and. ilaw /= 49&
            &.and. ilaw /= 151 .and. ilaw /= 999) then
              mtag%l_stra = 6        ! all user type laws calculate total strain
!
              bulk  = parmat(1)
              young = parmat(2)
              nu    = parmat(3)
              g     = half*young/(one + nu)
              if (ilaw==34) g=pm(22,i)
              if (ilaw==42) g=parmat(1)
              if (ilaw==62) g=parmat(2)/(one + nu)
              if (ilaw==69) g=parmat(1)
              if (ilaw==82) g=parmat(2)/(one + nu)
              pm(20,i) = young
              pm(21,i) = nu
              pm(22,i) = g
              pm(24,i) = young/(one - nu**2)
              pm(32,i) = bulk
              if (ilaw==71 ) pm(27,i)=sqrt(young/max(pm(1,i),em20))  ! sound speed
!---------
              ipm(7,i)   = iadbuf
              ipm(8,i)   = nuvar
              ipm(9,i)   = nuparam
              ipm(10,i)  = nfunc
              ipm(216,i) = imatvis
              ipm(226,i) = numtabl
!
!---------------------------------------------------------
              do j=1,nfunc
                ipm(10+j,i) = ifunc(j)
              enddo
!
              do j=1,numtabl
                ipm(226+j,i) = itable(j)
              enddo
!---------------------------------------------------------
!           fill uparam buffer
!---------------------------------------------------------
              if (nuparam > 0) then
                do j=1,nuparam
                  bufmat(iadbuf+j-1) = uparam(j)
                enddo
              end if
              iadbuf = iadbuf + nuparam
              buflen = buflen + nuparam
!
            else if (ilaw == 19) then
              mtag%l_stra = 6
              ipm(7,i) = iadbuf
              ipm(8,i) = nuvar
              ipm(9,i) = nuparam
              do j=1,nuparam
                bufmat(iadbuf+j-1) = uparam(j)
              enddo
              iadbuf = iadbuf + nuparam
              buflen = buflen + nuparam
!
            endif ! ilaw>=28
!-------    high stiffness for contact
            pm(107,i) = two*max(pm(32,i),pm(100,i))
            if (ilaw==1)  pm(107,i) = thirty*pm(107,i)
            if (ilaw==62) pm(107,i) = hundred*pm(107,i)
!
!---------------------------------------------------------
            israte = max(israte, nint(parmat(4)))    ! just in case ...
            asrate = two*pi * parmat(5)              ! asrate = 2*pi*fcut
            ipm(3,i) = israte
            if (asrate  == zero) asrate = ep20
            if (pm(9,i) == zero) pm(9,i) = asrate    ! old mat laws fill it directly
!---------------------------------------------------------
            ipm(1,i)   = mat_id
            ipm(2,i)   = ilaw
!
            pm(19,i)   = ilaw + em01     ! double stockage - a nettoyer
            pm(70,i)   = jtur + em01
            pm(71,i)   = jthe + em01
            pm(72,i)   = jale + em01
            !
            ipm(217,i) = iuser_law
            ! for solid elements time step computation.
            ! some laws fill directly ipm, pm, others use parmat  :
            if (ipm(252,i) == 0)  ipm(252,i) = nint(parmat(16))   ! iformdt = 0,1,2
            if (pm(105,i) == zero) pm(105,i) = parmat(17)         ! gfac factor
!---------------------------------------------------------
!
            if (matparam%rho   > zero) pm(1 ,i) = matparam%rho
            if (matparam%rho0  > zero) pm(89,i) = matparam%rho0
!
            if (matparam%rho  == zero) matparam%rho  = pm(1 ,i)
            if (matparam%rho0 == zero) matparam%rho0 = pm(89,i)
!
            if (matparam%young == zero) matparam%young = pm(20,i)
            if (matparam%nu    == zero) matparam%nu    = pm(21,i)
            if (matparam%shear == zero) matparam%shear = pm(22,i)
            if (matparam%bulk  == zero) matparam%bulk  = pm(32,i)
!
            if (pm(20,i) == zero) pm(20,i) = matparam%young
            if (pm(21,i) == zero) pm(21,i) = matparam%nu
            if (pm(22,i) == zero) pm(22,i) = matparam%shear
            if (pm(32,i) == zero) pm(32,i) = matparam%bulk
!
            ! to be defined
            ! if (matparam%stiff_contact == zero) matparam%stiff_contact = pm(?,i)
            ! if (matparam%stiff_hglass  == zero) matparam%stiff_hglass  = pm(?,i)
            ! if (matparam%stiff_tstep   == zero) matparam%stiff_tstep   = pm(?,i)
!---------------------------------------------------------
            if (matparam%therm%tref  == zero) matparam%therm%tref  = pm(79,i)
            if (matparam%therm%tini  == zero) matparam%therm%tini  = pm(79,i)
            if (matparam%therm%tmelt == zero) matparam%therm%tmelt = pm(80,i)
            if (matparam%therm%rhocp == zero) matparam%therm%rhocp = pm(69,i)
!
            if (pm(79,i) == zero) pm(79,i) = matparam%therm%tini
            if (pm(80,i) == zero) pm(80,i) = matparam%therm%tmelt
            if (pm(69,i) == zero) pm(69,i) = matparam%therm%rhocp
!---------------------------------------------------------
            if (matparam%young0==zero) matparam%young0= matparam%young  ! initial E0 will be updated in upd_mat (tabulated E)
            !  pm(100)=bulk
            !  for interface type 7  k=pm(32) ...
            !  for interface type 20 k=pm(100)...

            if (pm(100,i) == zero) pm(100,i) = pm(32,i) ! bulk used for interf 20 stiffness
!---------------------------------------------------------
            if (matparam%rho == zero .and. ilaw /= 37) then
              rho = matparam%rho0
              pm(1,i)      = rho
              matparam%rho = rho
            endif
!
            if (ilaw/=0   .and. ilaw/=20 .and. ilaw/=51 .and. ilaw/=151 .and.&
              ilaw/=108 .and. ilaw /= 999) then
              if (matparam%rho0 <= zero) then
                call ancmsg(msgid=683, msgtype=msgerror, anmode=aninfo,&
                &i1=mat_id,&
                &c1=titr,&
                &c2='DENSITY')
              endif
            endif
!---------------------------------------------------------
!
            ! force calculating strain rate for all
            mtag%g_epsd = 1                       ! global element strain rate, always calculated for output
            if (mtag%l_epsd == 0) mtag%l_epsd = 1 ! local strain rate (might depend on mat / fail model)
!!!!         if (israte >= 0) mtag%l_epsd = 1

            mtag%l_ssp = 1                         ! sound speed, always allocated

!---------------------------------------------------------
! for qeph (shell formulation)
            if (ipm(2,i) /= 999) then ! if ipm(2,) == 999 possible negative square root with pm(25)=cpe(gas)
              pm(12,i) = sqrt(max(zero, pm(22,i)))    ! gsr
              pm(13,i) = sqrt(max(zero, pm(24,i)))    ! a11sr
              pm(14,i) = sqrt(max(zero, pm(25,i)))    ! a12sr
              pm(190,i)= sqrt(max(zero, pm(21,i)))    ! nusr
            endif
!
!---------------  end loop over materials
          enddo   ! hm_nummat
!---------------
!-------------------------------------
!     search for duplicate ids
!-------------------------------------
          i=79
          j=0
          k=0
          rbid = zero
          call vdouble(ipm(1,1),npropmi,nummat-1,mess,0,rbid)
!------------------------------
!     precalcul sqrt
!------------------------------
          do i = 1, nummat-1
            if (ipm(2,i)==999) cycle !possible negative square root with pm(25)=cpe(gas)
            pm(12,i) = sqrt(max(zero, pm(22,i)))    ! gsr
            pm(13,i) = sqrt(max(zero, pm(24,i)))    ! a11sr
            pm(14,i) = sqrt(max(zero, pm(25,i)))    ! a12sr
            pm(190,i)= sqrt(max(zero, pm(21,i)))    ! nusr
          enddo

          do i = 1, nummat-1
            ilaw = ipm(2,i)
            if (ilaw /= 42) then
              pm(100,i) = pm(32,i)
            endif
          end do

          ! rho_max (inter18 automatic stiffness)
          do i = 1, nummat-1
            ilaw = ipm(2,i)
            if(ilaw/=20 .and. ilaw /=37 .and. ilaw/=51 .and. ilaw/=151)then
              !monomaterial laws
              pm(91,i)=pm(89,i)
            else
              !already done
              !law37  : see hm_read_mat37.f
              !law51  : see hm_read_mat51.f & fill_buffer_51.f
              !law151 : see m20dcod.f (user material ids are first converted into internal material ids in mat_param()%multimat%mid(1:nbmat)
            endif
          end do

          deallocate( uparam )
!------------------------------
          return
!------------------------------
2000      format(//&
          &5X,A,/,&
          &5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,&
          &5X,'USER MATERIAL LAW . . . . . . . . . . .=',I10/)
!------------------------------
        end subroutine hm_read_mat
!------------------------------
      end module hm_read_mat_mod
