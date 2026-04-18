Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||====================================================================
!||    hm_read_mat76                ../starter/source/materials/mat/mat076/hm_read_mat76.F
!||--- called by ------------------------------------------------------
!||    hm_read_mat                  ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                       ../starter/source/output/message/message.F
!||    hm_get_floatv                ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim            ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv                  ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted       ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword             ../starter/source/materials/mat/init_mat_keyword.F
!||    law76_func_comp              ../starter/source/materials/mat/mat076/law76_func_comp.F90
!||    mat_func_deintersect         ../starter/source/materials/tools/mat_func_deintersect.F90
!||    mat_table_table_copy         ../starter/source/materials/tools/mat_table_table_copy.F90
!||    table2d_deintersection       ../starter/source/materials/tools/table2d_deintersection.F90
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                 ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod           ../starter/share/modules1/hm_option_read_mod.F
!||    law76_func_comp_mod          ../starter/source/materials/mat/mat076/law76_func_comp.F90
!||    mat_func_deintersect_mod     ../starter/source/materials/tools/mat_func_deintersect.F90
!||    mat_table_table_copy_mod     ../starter/source/materials/tools/mat_table_table_copy.F90
!||    message_mod                  ../starter/share/message_module/message_mod.F
!||    submodel_mod                 ../starter/share/modules1/submodel_mod.F
!||    table2d_deintersection_mod   ../starter/source/materials/tools/table2d_deintersection.F90
!||    table_mod                    ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MAT76(MAT_PARAM,
     .                         MAXUPARAM,NUPARAM  ,UPARAM   ,NUVAR    ,NVARTMP ,
     .                         MTAG     ,PARMAT   ,UNITAB   ,LSUBMODEL,ISRATE   ,
     .                         IOUT     ,NTABLE   ,TABLE    ,NPROPM   ,PM       )                     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD      
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD          
      USE HM_OPTION_READ_MOD 
      USE TABLE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      use constant_mod , only : pi,zero,half,three_half,one,two,three,four_over_3
      use constant_mod , only : infinity
      use mat_table_table_copy_mod
      use mat_func_deintersect_mod
      use law76_func_comp_mod
      use table2d_deintersection_mod
      use precision_mod , only : WP
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
          implicit none
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)    :: MAXUPARAM
      INTEGER, INTENT(IN)    :: IOUT
      INTEGER, INTENT(IN)    :: NTABLE
      INTEGER, INTENT(INOUT) :: NUPARAM
      INTEGER, INTENT(INOUT) :: NUVAR
      INTEGER, INTENT(INOUT) :: NVARTMP
      INTEGER, INTENT(IN)    :: NPROPM
      INTEGER, INTENT(INOUT) :: ISRATE
      real(kind=WP), DIMENSION(NPROPM) ,INTENT(INOUT)     :: PM     
      real(kind=WP), DIMENSION(100)    ,INTENT(INOUT)     :: PARMAT
      real(kind=WP), DIMENSION(MAXUPARAM) ,INTENT(INOUT)  :: UPARAM
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE (SUBMODEL_DATA),INTENT(IN)      :: LSUBMODEL(*)
      TYPE (MLAW_TAG_)    ,INTENT(INOUT)   :: MTAG
      TYPE (MATPARAM_STRUCT_) ,INTENT(INOUT) :: MAT_PARAM
      type(ttable)  ,dimension(ntable) ,intent(in) :: table
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K
      INTEGER :: IFORM,ICONV,IQUAD,ICAS,ISRAT,ILAW,IERROR
      INTEGER :: NDIM
      INTEGER :: FUNC_TENS,FUNC_COMP,FUNC_SHEAR,FUNC_NU,FUNC_DMG
      INTEGER :: NPT,NPT_TENS,NPT_comp,NPT_SHEAR,NPTMAX,NEPSD
      INTEGER :: intersections
      real(kind=WP) :: E,NU,G,RHO0,RHOR,FCUT,NUP,C1,A1,A2,EPSR,EPSF,XFAC_UNIT
      real(kind=WP) :: x1scale,x2scale,x3scale,x4scale,xfac
      real(kind=WP) :: EPDT_MIN,EPDT_MAX,EPDC_MIN,EPDC_MAX,EPDS_MIN,EPDS_MAX
      real(kind=WP) :: TFAC(3),YFAC(2),FAC_UNIT(5) 
      real(kind=WP) ,dimension(:) ,allocatable :: x_comp,y_comp
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED,FOUND
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C-----------------------------------------------
      ILAW    = 76
Card1     
      CALL HM_GET_FLOATV('MAT_RHO'        ,RHO0        ,IS_AVAILABLE,LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho'      ,RHOR        ,IS_AVAILABLE,LSUBMODEL, UNITAB)
Card2      
      CALL HM_GET_FLOATV('MAT_E'          ,E           ,IS_AVAILABLE,LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'         ,NU          ,IS_AVAILABLE,LSUBMODEL, UNITAB)
Card3
      CALL HM_GET_INTV  ('FUN_D1'         ,FUNC_TENS   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_D2'         ,FUNC_COMP   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('FUN_D3'         ,FUNC_SHEAR  ,IS_AVAILABLE,LSUBMODEL)
Card4      
      CALL HM_GET_FLOATV('FScale11'       ,TFAC(1)     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale22'       ,TFAC(2)     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FScale33'       ,TFAC(3)     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('FACX'           ,XFAC        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card5
      CALL HM_GET_FLOATV('MAT_NUt'        ,NUP         ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('FUN_B5'         ,FUNC_NU     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_PScale'     ,YFAC(1)     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV  ('ISRATE'         ,ISRAT       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_asrate'     ,FCUT        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card6
      CALL HM_GET_FLOATV('MAT_Epsilon_F'  ,EPSF        ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Epsilon_0'      ,EPSR        ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
Card7 
      CALL HM_GET_INTV  ('FUN_A1'         ,FUNC_DMG    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('SCALE'          ,YFAC(2)     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
Card8
      CALL HM_GET_INTV  ('IFORM'          ,IFORM       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('MAT_Iflag'      ,IQUAD       ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Gflag'          ,ICONV       ,IS_AVAILABLE,LSUBMODEL)      
!-- unit
      CALL HM_GET_FLOATV_DIM('FScale11'   ,FAC_UNIT(1) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('FScale22'   ,FAC_UNIT(2) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('FScale33'   ,FAC_UNIT(3) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('FACX'       ,XFAC_UNIT   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('MAT_PScale' ,FAC_UNIT(4) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('SCALE'      ,FAC_UNIT(5) ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!-----------------------------------------------------------------------------------------                                          
c    input check
C------------
      IF (FCUT == ZERO) THEN
        FCUT = 500.0D0*UNITAB%FAC_T_WORK
      END IF 
      ISRAT  = 0
      ISRATE = 0
c
      IF (FUNC_TENS > 0 .AND. FUNC_COMP > 0 .AND. FUNC_SHEAR > 0) THEN 
        ICONV = 1
      ELSE
        ICONV = 0
      ENDIF
c
      IF (FUNC_TENS == 0) THEN
         CALL ANCMSG(MSGID=126, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .               I1=mat_param%mat_id, C1=mat_param%title, I2=FUNC_TENS)
      ENDIF
C
      IF (EPSF == ZERO) EPSF = INFINITY
      IF (EPSR == ZERO) EPSR = TWO*EPSF
      IF (IFORM == 1 .AND. IQUAD == 0) IQUAD = 1
C
c-----------------------------------------
c     icas      ifunt   | ifunc   | ifuncs
c       -1         1    |    1    |    1
c        0         1    |    0    |    0
c        1         1    |    1    |    0
c        2         1    |    0    |    1   
c-----------------------------------------
      ICAS = MIN(FUNC_COMP,1) + MIN(FUNC_SHEAR,1)
      IF (ICAS == 2) ICAS = -1
      IF (FUNC_COMP  > 0 .AND. ICAS == 1) ICAS = 1
      IF (FUNC_SHEAR > 0 .AND. ICAS == 1) ICAS = 2
      NUP = MAX(ZERO, MIN(NUP, HALF))
      IF (ICAS==0 .AND. NUP == ZERO .AND. FUNC_NU==0) NUP = HALF
c     
      IF (XFAC    == ZERO) XFAC    = XFAC_UNIT
      IF (TFAC(1) == ZERO) TFAC(1) = FAC_UNIT(1)
      IF (TFAC(2) == ZERO) TFAC(2) = FAC_UNIT(2)
      IF (TFAC(3) == ZERO) TFAC(3) = FAC_UNIT(3)
      IF (YFAC(1) == ZERO) YFAC(1) = FAC_UNIT(4)
      IF (YFAC(2) == ZERO) YFAC(2) = FAC_UNIT(5)
C
      G  = HALF*E/( ONE + NU)
      A1 = E*(ONE-NU) /((ONE + NU)*(ONE - TWO*NU))
      A2 = A1*NU/(ONE - NU)
      C1 = E/THREE/(ONE - TWO*NU)
!
      epdt_min = zero
      epdt_max = zero
      epdc_min = zero
      epdc_max = zero
      epds_min = zero
      epds_max = zero
      intersections = 0
c-----------------------------------------------
      mat_param%ntable = 5
      allocate (mat_param%table(mat_param%ntable))           ! allocate material table array
!
      mat_param%table(1)%notable  = func_tens
      mat_param%table(2)%notable  = func_comp
      mat_param%table(3)%notable  = func_shear
      mat_param%table(4)%notable  = func_nu
      mat_param%table(5)%notable  = func_dmg
!
! ----------------------------------------------------------------------------------------------------------------------
          ! create local function tables
! ----------------------------------------------------------------------------------------------------------------------
          if (func_tens > 0) then
            x1scale = one
            x2scale = one / xfac
            x3scale = one
            x4scale = one

            call mat_table_table_copy(                                          
     &         mat_param%table(1)  ,func_tens,mat_param%title,mat_param%mat_id , 
     &         x1scale  ,x2scale   ,x3scale  ,x4scale  ,                         
     &         tfac(1)  ,ntable    ,table    ,ierror     )
            npt_tens = size(mat_param%table(1)%x(1)%values)
            if (mat_param%table(1)%ndim == 2) then
              nepsd = size(mat_param%table(1)%x(2)%values)
              epdt_min = mat_param%table(1)%x(2)%values(1)
              epdt_max = mat_param%table(1)%x(2)%values(nepsd)
            end if
          end if
!
          if (func_comp > 0) then
            x1scale = one
            x2scale = one / xfac
            x3scale = one
            x4scale = one

            call mat_table_table_copy(                                          
     &         mat_param%table(2)  ,func_comp,mat_param%title,mat_param%mat_id , 
     &         x1scale  ,x2scale   ,x3scale  ,x4scale  ,                         
     &         tfac(2)  ,ntable    ,table    ,ierror     )
            npt_comp = size(mat_param%table(2)%x(1)%values)
            if (mat_param%table(2)%ndim == 2) then
              nepsd = size(mat_param%table(2)%x(2)%values)
              epdc_min = mat_param%table(2)%x(2)%values(1)
              epdc_max = mat_param%table(2)%x(2)%values(nepsd)
            end if
          end if
!
          if (func_shear > 0) then
            x1scale = one
            x2scale = one / xfac
            x3scale = one
            x4scale = one

            call mat_table_table_copy(                                          
     &         mat_param%table(3)  ,func_shear,mat_param%title,mat_param%mat_id ,
     &         x1scale  ,x2scale   ,x3scale  ,x4scale  ,                         
     &         tfac(3)  ,ntable    ,table    ,ierror     )
            npt_shear = size(mat_param%table(3)%x(1)%values)
            if (mat_param%table(3)%ndim == 2) then
              nepsd = size(mat_param%table(3)%x(2)%values)
              epds_min = mat_param%table(3)%x(2)%values(1)
              epds_max = mat_param%table(3)%x(2)%values(nepsd)
            end if
          end if
!
          if (func_nu > 0) then
            x1scale = one
            x2scale = one
            x3scale = one
            x4scale = one

            call mat_table_table_copy(                                          
     &         mat_param%table(4)  ,func_nu,mat_param%title,mat_param%mat_id , 
     &         x1scale  ,x2scale   ,x3scale  ,x4scale  ,                         
     &         yfac(1)  ,ntable    ,table    ,ierror     )
     
            nup = max(zero, min(half,mat_param%table(4)%y1d(1))) ! extract Poisson coef from tabulated function
          end if
!
          if (func_dmg > 0) then
            x1scale = one
            x2scale = one
            x3scale = one
            x4scale = one

            call mat_table_table_copy(                                         
     &         mat_param%table(5)  ,func_dmg ,mat_param%title,mat_param%mat_id ,
     &         x1scale  ,x2scale   ,x3scale  ,x4scale  ,                        
     &         yfac(2)  ,ntable    ,table    ,ierror     )
          end if
c-----------------------------------------------------------------------------
          !  Check yield stress values (must be strictly positive)
c-----------------------------------------------------------------------------
          do i=1,3
            if (mat_param%table(i)%notable > 0) then
              ndim = mat_param%table(i)%ndim
              ierror = 0
              if (ndim == 1) then
                if (minval(mat_param%table(i)%y1d) <= zero) ierror = 1
              else if (ndim == 2) then
                if (minval(mat_param%table(i)%y2d) <= zero) ierror = 1
              end if
              if (ierror == 1) then
                CALL ANCMSG(MSGID=2063, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     &                      i1=mat_param%mat_id, c1=mat_param%title, i2=I)                      
              end if
            end if
          end do
c-----------------------------------------------
          ! Check and correct intersections between succesive hardening curves for different strain rates
c-----------------------------------------------
          do i=1,3
            if (mat_param%table(i)%notable > 0) then
              ndim = mat_param%table(i)%ndim
              if (ndim == 2) then
                call table2d_deintersection(mat_param%table(i) ,intersections)
              end if
            end if
          end do          
c-----------------------------------------------
          if (icas == 2) then    ! create new static compression table from tension and shear data
            nptmax = npt_tens + npt_shear
            call law76_func_comp(mat_param%table(1),mat_param%table(3),mat_param%table(2),nptmax,nup)
                  
            ICAS  =-1
            ICONV = 1
          end if    
c-----------------------------------------------
          ! Check intersections between tension and compression curves
          ! Only static curves are compared and automatically corrected
c-----------------------------------------------
          if (mat_param%table(1)%notable > 0 .and. mat_param%table(2)%notable > 0) then
!
            call mat_func_deintersect(mat_param%table(1) ,mat_param%table(2), ierror, intersections)
!
            if (intersections > 0 .and. ierror == 1) then
              call ancmsg(MSGID=3135, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     &                    i1=mat_param%mat_id,c1=mat_param%title)
            else if (intersections > 0) then
              call ancmsg(MSGID=3136, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     &                    i1=mat_param%mat_id,c1=mat_param%title)
            end if
          end if
c-----------------------------------------------
      UPARAM(1)  = E
      UPARAM(2)  = E/(ONE - NU*NU)
      UPARAM(3)  = NU*UPARAM(2)
      UPARAM(4)  = G
      UPARAM(5)  = NU
      UPARAM(6)  = A1
      UPARAM(7)  = A2
      UPARAM(8)  = C1
      UPARAM(9)  = NUP
      UPARAM(10) = EPSF
      UPARAM(11) = EPSR

      UPARAM(13) = IFORM
      UPARAM(14) = IQUAD
      UPARAM(15) = ICONV
      UPARAM(16) = FCUT*PI*TWO   ! ASRATE
      UPARAM(17) = ICAS
      UPARAM(18) = ONE / XFAC
      UPARAM(19) = EPDT_MIN
      UPARAM(20) = EPDT_MAX
      UPARAM(21) = EPDC_MIN
      UPARAM(22) = EPDC_MAX
      UPARAM(23) = EPDS_MIN
      UPARAM(24) = EPDS_MAX
c
      NUPARAM = 24 
      NUVAR   = 7
      NVARTMP = 8
c --------------------------
      PARMAT(1)  = C1
      PARMAT(2)  = E
      PARMAT(3)  = NU
      PARMAT(4)  = ISRATE
      PARMAT(5)  = ZERO ! FCUT
      PARMAT(16) = 2   ! Formulation for solid elements time step computation.
      PARMAT(17) = (ONE - TWO*NU)/(ONE - NU) ! == TWO*G/(C1+FOUR_OVER_3*G)
c
      IF (RHOR == ZERO) RHOR=RHO0
      PM(1) = RHOR
      PM(89)= RHO0  
      !!-----------------------       
      MTAG%G_EPSD = 1
      MTAG%L_EPSD = 1
      MTAG%G_PLA  = 1
      MTAG%L_PLA  = 1
      MTAG%G_DMG  = 1
      MTAG%L_DMG  = 1
c
      IF (ICAS == 0) THEN 
        CALL INIT_MAT_KEYWORD(MAT_PARAM,"ELASTO_PLASTIC")
      ELSE
        CALL INIT_MAT_KEYWORD(MAT_PARAM ,"COMPRESSIBLE")
      ENDIF
      CALL INIT_MAT_KEYWORD(MAT_PARAM ,"INCREMENTAL" )
      CALL INIT_MAT_KEYWORD(MAT_PARAM ,"LARGE_STRAIN")
      CALL INIT_MAT_KEYWORD(MAT_PARAM ,"HOOK")
C
      ! Properties compatibility  
      CALL INIT_MAT_KEYWORD(MAT_PARAM,"SOLID_ISOTROPIC")  
      CALL INIT_MAT_KEYWORD(MAT_PARAM,"SHELL_ISOTROPIC")  
C-----------------------      
C
      WRITE(IOUT,1010) TRIM(mat_param%title),mat_param%mat_id,76
      WRITE(IOUT,1000)
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE    
        WRITE(IOUT,1020) RHO0
        WRITE(IOUT,1100) E,NU
        WRITE(IOUT,1200) FUNC_TENS,TFAC(1)
        WRITE(IOUT,1210) FUNC_COMP,TFAC(2)
        WRITE(IOUT,1220) FUNC_SHEAR,TFAC(3),XFAC
        WRITE(IOUT,1300) NUP,func_nu,YFAC(1),ISRATE,FCUT
        WRITE(IOUT,1400) EPSF,EPSR,func_dmg,YFAC(2)
        WRITE(IOUT,1500) IFORM,IQUAD,ICONV
        if (intersections > 0) then
          if (mat_param%table(1)%ndim == 1) then
            do i=1,3
              if (mat_param%table(i)%notable > 0) then
                select case (i)
                  case(1) 
                  write(iout,1610) zero
                  case(2) 
                  write(iout,1620) zero
                  case(3) 
                  write(iout,1630)zero
                end select
                do k=1,size(mat_param%table(i)%x(1)%values)
                  write(iout,2000) mat_param%table(i)%x(1)%values(k),mat_param%table(i)%y1d(k)
                end do
              end if
            end do
          else if (mat_param%table(1)%ndim == 2) then
            do i=1,3
              if (mat_param%table(i)%notable > 0) then
                do j=1,size(mat_param%table(i)%x(2)%values)  ! loop over strain rate values
                  select case (i)
                    case(1) 
                    write(iout,1610) mat_param%table(i)%x(2)%values(j)
                    case(2) 
                    write(iout,1620) mat_param%table(i)%x(2)%values(j)
                    case(3) 
                    write(iout,1630) mat_param%table(i)%x(2)%values(j)
                  end select
                  do k=1,size(mat_param%table(i)%x(1)%values)
                    write(iout,2000) mat_param%table(i)%x(1)%values(k),mat_param%table(i)%y2d(k,j)
                  end do
                end do
              end if
            end do
          end if
        end if  ! intersections
        write(iout,'(A)') ' '
      ENDIF
c-----------
      RETURN
c-----------------------------------------------------------------------
 1000 FORMAT(
     & 5X,' SEMI ANALYTICAL PLASTIC LAW 76  ',/,
     & 5X,' ------------------------------           ' ,//)
 1010 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . =',I10/,
     & 5X,'MATERIAL LAW . . . . . . . . . . . . . . . . =',I10/)
 1020 FORMAT(
     & 5X,'INITIAL DENSITY. . . . . . . . . . . . . . . =',1PG20.13/)
 1100 FORMAT(
     & 5X,'YOUNG''S MODULUS. . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'POISSON''S RATIO. . . . . . . . . . . . . . .=',1PG20.13/)

 1200 FORMAT(
     & 5X,'TENSION YIELD STRESS FUNCTION NUMBER. . . . .=',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . . . .=',1PG20.13)
 1210 FORMAT(
     & 5X,'COMPRESSION YIELD STRESS FUNCTION NUMBER. . .=',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . . . .=',1PG20.13)
     
 1220 FORMAT(
     & 5X,'SHEAR YIELD STRESS FUNCTION NUMBER. . . . . .=',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'STRAIN RATE SCALE FACTOR .  . . . . . . . . .=',1PG20.13)
   
 1300 FORMAT(              
     & 5X,'PLASTIC POISSON RATIO   . . . . . . . . . .  =',1PG20.13/
     & 5X,'PLASTIC POISSON RATIO FUNCTION NUMBER . . .  =',I10/
     & 5X,'YIELD SCALE FACTOR. . . . . . . . . . . . .  =',1PG20.13/   
     & 5X,'SMOOTH STRAIN RATE OPTION. . . . . . . . . . =',I10/
     & 5X,'STRAIN RATE CUTTING FREQUENCY . . . . . . . .=',1PG20.13/)
 1400 FORMAT(
     & 5X,'FAILURE PLASTIC STRAIN  . . . . . . . . . . .=',1PG20.13/
     & 5X,'RUPTURE PLASTIC STRAIN. . . . . . . . . . . .=',1PG20.13/
     & 5X,'DAMAGE  FUNCTION NUMBER . . . . . . . . . .  =',I10/,
     & 5X,'DAMAGE SCALE FACTOR. . . . . . . . . . . . . =',1PG20.13 )
 1500 FORMAT(              
     & 5X,'FORMULATION FLAG . . . . . . . . . . . . .   =', I10,/
     & 5X,'       = 0  Non associated formulation  ' ,/
     & 5X,'       = 1  VonMises associated formulation     ' ,/
     & 5X,' YIELD SURFACE FLAG . . . . . . . . . . . . .=', I10,/
     & 5X, '      = 0 Yield surface is linear in the Vonmises ',/
     & 5X, '      = 1 Yield surface is quadratic in the Vonmises',/
     & 5X, 'CONVEXITY CONDITION . . .  . . . .  . . .  . =',I10)
 1610 FORMAT(5X,'YIELD FUNCTION IN TENSION, STRAIN RATE     = ',1PG20.13)
 1620 FORMAT(5X,'YIELD FUNCTION IN COMPRESSION, STRAIN RATE = ',1PG20.13)
 1630 FORMAT(5X,'YIELD FUNCTION IN SHEAR, STRAIN RATE       = ',1PG20.13)
 2000 format(10x,2g20.13)
c-----------------------------------------------------------------------
      END
