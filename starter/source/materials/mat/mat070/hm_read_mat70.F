Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    hm_read_mat70                  ../starter/source/materials/mat/mat070/hm_read_mat70.F
!||--- called by ------------------------------------------------------
!||    hm_read_mat                    ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                         ../starter/source/output/message/message.F
!||    hm_get_float_array_index       ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_float_array_index_dim   ../starter/source/devtools/hm_reader/hm_get_float_array_index_dim.F
!||    hm_get_floatv                  ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_int_array_index         ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                    ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted         ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword               ../starter/source/materials/mat/init_mat_keyword.F
!||    simple_checksum                ../starter/source/tools/curve/simple_checksum.cpp
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                   ../starter/share/modules1/elbuftag_mod.F
!||    message_mod                    ../starter/share/message_module/message_mod.F
!||    simple_checksum_mod            ../starter/source/tools/curve/simple_checksum_mod.F90
!||    submodel_mod                   ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_MAT70(UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   , IMATVIS  ,
     .                         NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    , PARMAT   , 
     .                         UNITAB ,MAT_ID   ,TITR     ,MTAG     , LSUBMODEL,
     .                         PM     ,MATPARAM ,NVARTMP  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      use simple_checksum_mod
      use myqsort_d_mod
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   READ MAT LAW70 WITH HM READER ( TO BE COMPLETED )
C
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     PM              MATERIAL ARRAY(REAL)
C     UNITAB          UNITS ARRAY
C     MAT_ID          MATERIAL ID(INTEGER)
C     TITR            MATERIAL TITLE
C     LSUBMODEL       SUBMODEL STRUCTURE   
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD      
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real, INTENT(INOUT)                :: PM(NPROPM),PARMAT(100),UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT)                :: ISRATE,NFUNC,MAXFUNC,MAXUPARAM,
     .                                         NUPARAM,NUVAR,NVARTMP,IMATVIS
      INTEGER, INTENT(INOUT)                :: IFUNC(MAXFUNC)
      TYPE(MLAW_TAG_),INTENT(INOUT)         :: MTAG
      INTEGER,INTENT(IN)                    :: MAT_ID
      CHARACTER(LEN=NCHARTITLE) ,INTENT(IN)       :: TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)        :: LSUBMODEL(*)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRATEN,J,I,NRATEP,IUNLOAD,ITENS,IETANG,NL,ISORT
      integer :: i1,i2,sizeh,ierror,ifun
      my_real E,NU,G,C1,VISC, VISCV,EXPO,HYS,
     .        RHO0,RHOR,E0,EMAX,EPSMAX,FCUT,A1,A2,AA
      INTEGER ,DIMENSION(MAXFUNC) :: FID,PERM
      my_real ,DIMENSION(MAXFUNC) :: RATE,YFAC,YFAC_UNIT,SCALE,EPSP,RTMP,YTMP
      double precision :: h1,h2
      double precision :: chksum
      double precision :: hasht(3)
      double precision ,dimension(:) ,allocatable :: hash
      integer          ,dimension(:) ,allocatable :: perml
      LOGICAL IS_AVAILABLE,IS_ENCRYPTED         
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      IMATVIS = 2

      RATE(1:MAXFUNC) = ZERO
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
      
      !line-1
      CALL HM_GET_FLOATV('MAT_RHO'   ,RHO0     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('Refer_Rho' ,RHOR     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      !line-2
      CALL HM_GET_FLOATV('MAT_E0'    ,E0       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_NU'    ,NU       ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('E_Max'     ,EMAX     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_EPS'   ,EPSMAX   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('Itens'       ,ITENS    ,IS_AVAILABLE, LSUBMODEL)
      !line-3
      CALL HM_GET_FLOATV('MAT_asrate',FCUT     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_INTV('ISRATE'      ,ISRATE   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('NRATEP'      ,NRATEP   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('NRATEN'      ,NRATEN   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('MAT_Iflag'   ,IUNLOAD  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('MAT_SHAPE' ,EXPO     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('MAT_HYST'  ,HYS      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
!-------------------------------------------------------------------------------
      IF(RHOR==ZERO)RHOR=RHO0
      PM(1) =RHOR
      PM(89)=RHO0

!-------------------------------------------------------------------------------
      ! Loading functions
!-------------------------------------------------------------------------------
      IF(NRATEP == 0) THEN
          CALL ANCMSG(MSGID=866,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND,
     .                I1=MAT_ID,
     .                C1=TITR)
      ENDIF 
      !--loading function
      DO I=1,NRATEP
        CALL HM_GET_INT_ARRAY_INDEX('FUN_LOAD'           ,FID(I)      ,I,IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOAT_ARRAY_INDEX('STRAINRATE_LOAD'  ,EPSP(I)     ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('SCALE_LOAD'       ,SCALE(I)    ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('SCALE_LOAD'   ,YFAC_UNIT(I),I,IS_AVAILABLE, LSUBMODEL, UNITAB)         
        !units
        IF (SCALE(I) == ZERO) SCALE(I) = ONE * YFAC_UNIT(I)
      ENDDO  
c---------------------------------------------------------------------------
c     check if there are no input errors in function and strain rate definitions
c---------------------------------------------------------------------------
      if (nratep > 1) then
        allocate (perml(nratep))
        allocate (hash (nratep))
        ! create a checksum value for each function input line
        sizeh = 3
        do i=1,nratep
          hasht(1) = fid(i)
          hasht(2) = epsp(i)
          hasht(3) = scale(i)
          call simple_checksum(hasht,sizeh,chksum)
          hash(i) = chksum
        end do
!
        ! check duplicated input lines
!
        call myqsort_d(nratep,hash,perml,ierror)
        h1 = hash(1)
        ifun = 1
        i1 = perml(1)
        ifunc(1) = fid(i1)
        rate(1)  = epsp(i1)
        yfac(1)  = scale(i1)
        do i = 2,nratep
          h2 = hash(i)
          i1 = perml(i-1)
          i2 = perml(i)
          if (h1 == h2) then  ! skip double function line
            call ancmsg(MSGID=3101, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                  I1=mat_id,C1=titr, I2=ifunc(i1))
          else
            ifun = ifun+1
            ifunc(ifun) = fid(i2)
            rate(ifun)  = epsp(i2)
            yfac(ifun)  = scale(i2)
          end if
          h1 = h2
        end do
        nratep = ifun
      else
        ifunc(1) = fid(1)
        rate(1)  = epsp(1)
        yfac(1)  = scale(1)
      end if
c---------------------------------------------------------------------------
c     sort loading functions in increasing strain rate order
c---------------------------------------------------------------------------
      IF (NRATEP > 1) THEN
        DO I=1,NRATEP
          FID(I)  = IFUNC(I)
          RTMP(I) = RATE(I)
          YTMP(I) = YFAC(I)
        ENDDO  
        PERML(:) = 0
        CALL MYQSORT(NRATEP, RTMP, PERML, IERR)

        DO I=1,NRATEP
          J = PERML(I)
          IFUNC(I) = FID(J)
          RATE(I)  = RTMP(I)
          YFAC(I)  = YTMP(J)
        ENDDO  
c
        IF (RATE(1) /=  ZERO) THEN   ! static strain rate must be equal to 0
          RATE(1) = ZERO     
          CALL ANCMSG(MSGID=1721, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND,
     .                I1=MAT_ID,
     .                C1=TITR)       
        ENDIF 
      END IF                       
      if (allocated(perml)) deallocate (perml)
      if (allocated(hash))  deallocate (hash )
!-------------------------------------------------------------------------------
      ! Unloading functions
!-------------------------------------------------------------------------------
      IF (NRATEN > 0 ) THEN
        DO I=1,NRATEN
          CALL HM_GET_INT_ARRAY_INDEX('FUN_UNLOAD'         ,FID(I)      ,I,IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('STRAINRATE_UNLOAD',EPSP(I)     ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('SCALE_UNLOAD'     ,SCALE(I)    ,I,IS_AVAILABLE, LSUBMODEL, UNITAB)   
          CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('SCALE_UNLOAD' ,YFAC_UNIT(I),I,IS_AVAILABLE, LSUBMODEL, UNITAB)         
          !units
          IF (SCALE(I) == ZERO) SCALE(I) = ONE * YFAC_UNIT(I)        
        ENDDO 
      ELSE 
        IF (NRATEN == 0 .AND. IUNLOAD <= 2) THEN
             NRATEN = 1
             IFUNC(NRATEP+1) = IFUNC(1)
             RATE (NRATEP+1) = RATE(1)
             YFAC (NRATEP+1) = YFAC(1)
             IUNLOAD = 0
C             
             CALL ANCMSG(MSGID=1226, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                   I1=MAT_ID, C1=TITR)
         ENDIF
      ENDIF                     
c---------------------------------------------------------------------------
c     check if unloading functions are defined in increasing strain rate order
c---------------------------------------------------------------------------
      IF (NRATEN > 1) THEN
        IERR  = 0
        ISORT = 0
        DO I=1,NRATEN-1
          IF (EPSP(I) == EPSP(I+1)) THEN
            IERR  = 1
          ELSE IF (EPSP(I) > EPSP(I+1)) THEN
            ISORT = 1
          END IF
        ENDDO
        IF (IERR == 1) THEN
          CALL ANCMSG(MSGID=478,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND,
     .                I1=MAT_ID,
     .                C1=TITR)
        END IF
c
        IF (ISORT == 1) THEN  ! sort functions in correct order
          CALL MYQSORT(NRATEN, EPSP, PERM, IERR)

          DO I=1,NRATEN
            J = PERM(I)
            IFUNC(I + NRATEP) = FID(J)
            RATE (I + NRATEP) = EPSP(I)
            YFAC (I + NRATEP) = SCALE(J)
          ENDDO
        ELSE 
          DO I=1,NRATEN
            IFUNC(I + NRATEP) = FID(I)
            RATE (I + NRATEP) = EPSP(I)
            YFAC (I + NRATEP) = SCALE(I)
          ENDDO
        END IF
c
        IF (RTMP(1) /=  ZERO) THEN   ! static strain rate must be equal to 0
          RATE(1 + NRATEP) = ZERO     
          CALL ANCMSG(MSGID=1721, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND,
     .                I1=MAT_ID,
     .                C1=TITR)       
        ENDIF 
      else if (nraten == 1) then
        ifunc(nratep+1) = fid(1)
        rate (nratep+1) = epsp(1)
        yfac (nratep+1) = scale(1)
      END IF
c---------------------------------------------------------------------------
c
      IF (FCUT > ZERO) ISRATE = 1
      IF (FCUT == ZERO) FCUT = INFINITY
      NL = NRATEP + NRATEN
      IF (ITENS > 0) THEN
         NL = NRATEP + NRATEN + 1
         RATE(NL) = ZERO
         CALL HM_GET_INT_ARRAY_INDEX('FUN_A1'     ,IFUNC(NL) ,NL,IS_AVAILABLE, LSUBMODEL)
         CALL HM_GET_FLOAT_ARRAY_INDEX('FScale11' ,YFAC(NL)  ,NL,IS_AVAILABLE, LSUBMODEL,UNITAB)   
         IF (YFAC(NL) == ZERO) YFAC(NL) = ONE
      ENDIF 
      
      DO I=1,NL
        IF(IFUNC(I) == 0)THEN
          CALL ANCMSG(MSGID=126, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=IFUNC(I))
        ENDIF
      ENDDO
C
      IF (EMAX< E0) THEN
          CALL ANCMSG(MSGID=3028,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND,
     .                I1=MAT_ID,
     .                C1=TITR)
          EMAX = ZERO
      END IF      
      IF(EPSMAX == ZERO) EPSMAX = ONE
      UPARAM(1)=NL
      UPARAM(2)=E0
      AA = (EMAX-E0)/EPSMAX
      UPARAM(3)=AA
      UPARAM(4)=EPSMAX
      G = HALF*E0/(ONE + NU)
      UPARAM(5)=G
      UPARAM(6)=NU       
      UPARAM(7)= NRATEP
      UPARAM(8)= NRATEN
      DO I=1,NL
         UPARAM(I + 8)      = RATE(I)
         UPARAM(I + 8 + NL) = YFAC(I)
      END DO

      UPARAM(2*NL + 9)= IUNLOAD
      IF(EXPO == ZERO) EXPO = ONE
      IF(HYS  == ZERO) HYS = ONE
      UPARAM(2*NL + 10)= EXPO
      UPARAM(2*NL + 11) = HYS
      UPARAM(2*NL + 12) = EMAX
      UPARAM(2*NL + 13) = ITENS
      UPARAM(2*NL + 14) = 0  ! not used
C
      C1 = E0/THREE/(ONE - TWO*NU)
      NFUNC = NL 
      NUPARAM =16 + 2*NL  
      IF (IS_ENCRYPTED) UPARAM(16 + 2*NL) = 1
C
      PARMAT(1) = C1
      PARMAT(2) = E0
      PARMAT(3) = NU
      PARMAT(4) = ISRATE
      PARMAT(5) = FCUT
C     Formulation for solid elements time step computation.
      PARMAT(16) = 2
      PARMAT(17) = (ONE-TWO*NU)/(ONE-NU)
C
      NUVAR   = 10
      NVARTMP = 6
c-----------------
      CALL INIT_MAT_KEYWORD(MATPARAM,"TOTAL")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SMALL_STRAIN")
      IF (NU > 0.49) THEN
        CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
      ELSE
        CALL INIT_MAT_KEYWORD(MATPARAM,"COMPRESSIBLE")
      END IF
      CALL INIT_MAT_KEYWORD(MATPARAM,"HOOK")
      ! Properties compatibility  
      CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")
c------------------------------------
      WRITE(IOUT,1001) TRIM(TITR),MAT_ID,70    
      WRITE(IOUT,1000)
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1050)RHO0      
        WRITE(IOUT,1100)E0,NU,EMAX,EPSMAX
        WRITE(IOUT,1200)FCUT,ISRATE,NRATEP,NRATEN,IUNLOAD,EXPO,HYS,ITENS
        WRITE(IOUT,1300)(IFUNC(J),RATE(J),YFAC(J),J=1,NRATEP)
        WRITE(IOUT,1400)(IFUNC(J+NRATEP),RATE(J+NRATEP),YFAC(J+NRATEP),
     .                 J=1,NRATEN)
        IF(ITENS > 0) THEN
           WRITE(IOUT,1500)IFUNC(NL),YFAC(NL)        
        ENDIF
      ENDIF
c-----------
      RETURN
 1000 FORMAT(
     & 5X,40H  TABULATED NON LINEAR VISCO ELASTIC LAW,/,
     & 5X,40H  --------------------------------------,//)
 1001 FORMAT(/
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'MATERIAL LAW. . . . . . . . . . . . . .=',I10/)
 1100 FORMAT(
     & 5X,'INITIAL YOUNG''S MODULUS. . . . . . . .=',1PG20.13/,
     & 5X,'POISSON''S RATIO. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM YOUNG''S MODULUS. . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM STRAIN . . . . .. . . . . . . .=',1PG20.13)
 1200 FORMAT(
     & 5X,'STRAIN RATE COEFFICIENT . . . . . . . .=',1PG20.13/,
     & 5X,'FLAG FOR STRAIN RATE       . . . .. . .=',I10/,
     & 5X,'NUMBER OF LOAD STRESS FUNCTIONS    .. .=',I10/,
     & 5X,'NUMBER OF UNLOAD STRESS FUNCTIONS  .. .=',I10/,
     & 5X,'CHOICE OF UNLOADING FORMULATION  . . . =',I10/,
     & 5X,'SHAPE FACTOR FOR UNLOADING  . . . . . .=',1PG20.13/,
     & 5X,'HYSTERETIC UNLOADING FACTOR  . . . . . =',1PG20.13/, 
     & 5X,'FLAG  FOR TENSION BEHAVIOR . . . . . . =',I10/)
CC     & 5X,'FLAG CHOICE FOR YOUNG MODULUS . . . .  =',I10    )
 1300 FORMAT(
     & 5X,'LOAD YIELD STRESS FUNCTION NUMBER.. . .=',I10/,
     & 5X,'STRAIN RATE . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SCALE FACTOR. . . . . . . . . . . . . .=',1PG20.13/)

 1400 FORMAT(
     & 5X,'UNLOAD YIELD STRESS FUNCTION NUMBER.. .=',I10/,
     & 5X,'STRAIN RATE . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SCALE FACTOR. . . . . . . . . . . . . .=',1PG20.13/)
 1500 FORMAT(
     & 5X,'PRESSURE DEPENDENT YIELD FUNCTION . . .=',I10/
     & 5X,'PRESSURE SCALE FACTOR. . . . . . . . . =',1PG20.13)     
 1050 FORMAT(
     & 5X,'INITIAL DENSITY . . . . . . . . . . . .=',1PG20.13/)  

      END SUBROUTINE HM_READ_MAT70
