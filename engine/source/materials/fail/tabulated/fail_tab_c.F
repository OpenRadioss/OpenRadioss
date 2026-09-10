Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    fail_tab_c            ../engine/source/materials/fail/tabulated/fail_tab_c.F
!||--- called by ------------------------------------------------------
!||    mulawc                ../engine/source/materials/mat_share/mulawc.F90
!||    usermat_shell         ../engine/source/materials/mat_share/usermat_shell.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    interface_table_mod   ../engine/share/modules/table_mod.F
!||    message_mod           ../engine/share/message_module/message_mod.F
!||    table_mod             ../engine/share/modules/table_mod.F
!||====================================================================
      SUBROUTINE FAIL_TAB_C(FAIL    ,
     1           NEL      ,NUVAR    ,NVARTMP  ,UVAR     ,VARTMP   ,
     3           TIME     ,NGL      ,IPG      ,ILAY     ,IPT      ,
     4           SIGNXX   ,SIGNYY   ,SIGNXY   ,
     5           DPLA     ,EPSP     ,THK      ,ALDT     ,TEMP     ,
     6           DMG_FLAG ,DMG_SCALE,OFF      ,FOFF     ,
     7           DFMAX    ,TDEL     ,INLOC    )
C-----------------------------------------------
C    tabulated failure model
C-----------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "mvsiz_p.inc"
#include "units_c.inc"
#include "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C DPLA    | NEL     | F | R | PLASTIC STRAIN
C EPSP    | NEL     | F | R | STRAIN RATE
C THK     | NEL     | F | R | ELEMENT THICKNESS
C TEMP    | NEL     | F | R | ELEMENT TEMPERATURE
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C TDEL    | NEL     | F | W | FAILURE TIME
C DMG_FLAG|  1      | I | W | STRESS REDUCTION FLAG DUE TO DAMAGE
C DMG_SCALE| NEL    | F | W | STRESS REDUCTION FACTOR
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NVARTMP,NUVAR,IPG,ILAY,IPT,INLOC
      INTEGER ,DIMENSION(NEL)    ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME
      my_real ,DIMENSION(NEL) ,INTENT(IN)    :: OFF,THK,ALDT,DPLA,EPSP,
     .   TEMP,SIGNXX,SIGNYY,SIGNXY
      type (fail_param_) ,intent(in) :: fail    !< failure model data structure
CC-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,INTENT(OUT) ::DMG_FLAG
      INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: FOFF
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: DFMAX
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: TDEL,DMG_SCALE
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX,NINDXF,NINSTAB,IFAIL_SH,INST_FLAG,SIZE_FLAG
      INTEGER :: IFUN_EPSF,IFUN_INST,IFUN_EL,IFUN_TEMP,IFUN_DMG,IFUN_FAD
      INTEGER, DIMENSION(MVSIZ) :: INDX,INDXF,INDSTAB
      integer, DIMENSION(MVSIZ,3) :: IPOS
      my_real :: SHRF,BIAXF
      my_real, DIMENSION(MVSIZ) :: EPSF,EPSF_N,SIGM
      my_real  :: P,SVM,P_THINN,ECRIT,DCRIT,EL_REF,P_THICK,DD,DN
      my_real, DIMENSION(MVSIZ) :: DP
      INTEGER :: NINDX_2
      INTEGER, DIMENSION(MVSIZ) :: INDX_2
      my_real ,dimension(nel)   :: lambda,fade_expo,size_fac,y_loc,dydx
      my_real ,dimension(nel,3) :: xvec
C-----------------------------------------------
C Storage of initial thickness in UVAR(x,2) at time = 0.0
C 1 = DAMAGE
C 2 = initial shell thickness
C 3 = DCrit_NS --> instability starts
C 4 = percent from instability to failure
C 5 = initial characteristic el. length
C=======================================================================
      IFAIL_SH  = INT(FAIL%UPARAM(2))
      P_THICK   = FAIL%UPARAM(3)
      DCRIT     = FAIL%UPARAM(4)
      DD        = FAIL%UPARAM(5)
      DN        = FAIL%UPARAM(6)
      EL_REF    = FAIL%UPARAM(9)
      P_THINN   = FAIL%UPARAM(16)
      ECRIT     = FAIL%UPARAM(17)
      FADE_EXPO(:) = FAIL%UPARAM(18)
      DMG_FLAG  = INT(FAIL%UPARAM(19))
      INST_FLAG = INT(FAIL%UPARAM(20))
      SHRF      = FAIL%UPARAM(21)
      BIAXF     = FAIL%UPARAM(22)
      IF (SHRF == -ONE .and. BIAXF == ONE) THEN
        SIZE_FLAG = 0
      ELSE
        SIZE_FLAG = 1
      END IF
C-------------------------------------------------------------------
      ifun_epsf = 1    ! failure strain table
      ifun_inst = 2    ! instability strain table     
      ifun_el   = 3    ! element size function
      ifun_temp = 4    ! temperature scale function
      ifun_dmg  = 5    ! damage function
      ifun_fad  = 6    ! fading exponent function
C---------
      nindxf  = 0  
      nindx   = 0  
      ninstab = 0
      ! tag active elements for failure and instability checking
      do i=1,nel
        if (inloc > 0) uvar(i,5) = aldt(i)
        if (off(i) == one .and. foff(i) == 1) then
          nindx = nindx+1                                                 
          indx(nindx) = i
        endif
      enddo
      ! element size scale factor
      if (fail%table4d(ifun_el)%notable > 0) then
        lambda(1:nel) = uvar(1:nel,5) / el_ref
        call table_mat_vinterp(fail%table4d(ifun_el),nel,nel,vartmp(1:nel,1:1),lambda,size_fac,dydx)
      else
        size_fac(1:nel) = one
      end if
C-------------------------------------------------------------------
c     Failure strain value - function interpolation
C-------------------------------------------------------------------  
#include "vectorize.inc"
      DO J=1,NINDX                           
        I = INDX(J)  
        P    = THIRD*(SIGNXX(I) + SIGNYY(I))     
        SVM  = SQRT(SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)          
     .       - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I))        
        SIGM(I) = P / MAX(EM20,SVM)
      ENDDO
c---  failure strain interpolation
      do j=1,nindx                           
        i = indx(j)  
        xvec(j,1) = sigm(i)
        xvec(j,2) = epsp(i)
        xvec(j,3) = zero
        ipos(j,1) = vartmp(i,2)
        ipos(j,2) = vartmp(i,3)
        ipos(j,3) = 1
      end do
!
      call table_mat_vinterp(fail%table4d(ifun_epsf),nel,nindx,ipos,xvec,y_loc,dydx)
!
      do j=1,nindx                           
        i = indx(j)
        epsf(i)     = y_loc(j)
        vartmp(i,2) = ipos(j,1)
        vartmp(i,3) = ipos(j,2)
      end do

c     check elements with triaxiality between SHRF and BIAXF in case of SIZE_FLAG=1
      IF (SIZE_FLAG == 1) THEN   
#include "vectorize.inc"   
        DO J=1,NINDX                         
          I = INDX(J)
          IF (SIGM(I) > SHRF .and. SIGM(I) < BIAXF) THEN
            NINSTAB = NINSTAB + 1
            INDSTAB(NINSTAB) = I
          END IF
        ENDDO
      END IF                                                                                
c
      IF (fail%table4d(ifun_el)%notable > 0 .AND. INST_FLAG /= 2) THEN
!
        if (size_flag == 0) then          
!
#include   "vectorize.inc"
          do j=1,nindx                           
            i = indx(j)  
            epsf(i) = epsf(i) * size_fac(i)
          ENDDO
c
        ELSE  ! SIZE_FLAG = 1

#include   "vectorize.inc"
          do j=1,ninstab                           
            i = indstab(j)  
            epsf(i) = epsf(i) * size_fac(i)
          enddo
        END IF          
      ENDIF  

c----   instability function
c
      IF (fail%table4d(ifun_inst)%notable > 0) THEN
c
#include "vectorize.inc"
        do j=1,nindx                           
          i = indx(j)  
          xvec(j,1) = sigm(i)
          xvec(j,2) = epsp(i)
          ipos(j,1) = vartmp(i,4)
          ipos(j,2) = vartmp(i,5)
          ipos(j,3) = 1
        end do
!
        call table_mat_vinterp(fail%table4d(ifun_inst),nel,nindx,ipos,xvec,y_loc,dydx)
!
        do j=1,nindx                           
          i = indx(j)  
          epsf_n(i)   = y_loc(j)
          vartmp(i,4) = ipos(j,1)
          vartmp(i,5) = ipos(j,2)
        end do
!
c       Apply element size scale factor
        if (fail%table4d(ifun_el)%notable > 0 .and. inst_flag /= 1) then
          if (size_flag == 0) then          
#include "vectorize.inc"
            do j=1,nindx                           
              i = indx(j)  
              epsf_n(i) = epsf_n(i) * size_fac(i)  
            enddo
          else
#include "vectorize.inc"
            do j=1,ninstab                           
              i = indstab(j)  
              epsf_n(i) = epsf_n(i) * size_fac(i)  
            enddo
          end if
        endif            
c
      ELSEIF (ECRIT > ZERO) THEN
c
#include "vectorize.inc"
        DO J=1,NINDX                           
          I = INDX(J)  
          EPSF_N(I) = ECRIT
        ENDDO
      ELSE 
#include "vectorize.inc"
        DO J=1,NINDX                           
          I = INDX(J)  
          EPSF_N(I) = ZERO
        ENDDO
      ENDIF
c----   temperature scale function
      if (fail%table4d(ifun_temp)%notable > 0) THEN
!
#include "vectorize.inc"
        do j=1,nindx                           
          i = indx(j)  
          xvec(j,1) = temp(i)
          ipos(j,1) = vartmp(i,6)
        end do
!
        call table_mat_vinterp(fail%table4d(ifun_temp),nel,nindx,ipos,temp,y_loc,dydx)
!
#include "vectorize.inc" 
        do j=1,nindx                           
          i = indx(j)  
          epsf(i)   = epsf(i) * y_loc(j)
          vartmp(i,6) = ipos(j,1)
        end do
      
      end if ! ifun_temp
!
c----   Fading exponent
      if (fail%table4d(ifun_fad)%notable > 0) then
!#include "vectorize.inc"
        do j=1,nindx                           
          i = indx(j)            
          xvec(j,1) = lambda(i)
          ipos(j,1) = vartmp(i,7)
        enddo
!
        call table_mat_vinterp(fail%table4d(ifun_fad),nel,nindx,ipos,xvec,y_loc,dydx)
!
#include "vectorize.inc" 
        do j=1,nindx                           
          i = indx(j)  
          fade_expo(i) = y_loc(j)
          vartmp(i,7) = ipos(j,1)
        end do
!
      endif  ! ifun_fad
c-----------------------------------------------------------------------------
      NINDX_2 = 0
      IF (fail%table4d(ifun_dmg)%notable > 0 ) THEN
#include "vectorize.inc"
        DO J=1,NINDX                           
            I = INDX(J)  
            IF (UVAR(I,1) < DCRIT) THEN
                NINDX_2 = NINDX_2 + 1
                INDX_2(NINDX_2) = I
            ENDIF
        ENDDO
#include "vectorize.inc"
        do j=1,nindx_2                           
          i = indx_2(j)            
          xvec(j,1) = uvar(i,1)
          ipos(j,1) = vartmp(i,8)
        enddo
!
        call table_mat_vinterp(fail%table4d(ifun_dmg),nel,nindx_2,ipos,xvec,y_loc,dydx)
!
#include "vectorize.inc" 
        do j=1,nindx_2                           
          i = indx_2(j)  
          dp(i) = y_loc(j)
          vartmp(i,8) = ipos(j,1)
        end do
!
      ELSE
#include "vectorize.inc"
        DO J=1,NINDX                           
            I = INDX(J)  
            IF (UVAR(I,1) < DCRIT) THEN
                NINDX_2 = NINDX_2 + 1
                INDX_2(NINDX_2) = I
                IF (UVAR(I,1) == ZERO) THEN 
                    DP(I) = ONE
                ELSE
                    DP(I) = DN*UVAR(I,1)**(ONE-ONE/DN)
                ENDIF      
            ENDIF
        ENDDO
      ENDIF  ! ifun_dmg

#include "vectorize.inc"
      DO J=1,NINDX_2                        
            I = INDX_2(J)  
            IF (EPSF(I) > ZERO) UVAR(I,1) = UVAR(I,1)+DP(I)*DPLA(I)/EPSF(I)
            IF ((P_THINN*UVAR(I,2)) > THK(I)) THEN ! Failure due to thinning
              UVAR(I,1) = DCRIT     
              FOFF(I)   = 0
              TDEL(I)   = TIME
!              PTHKF     = EM06
            ENDIF
c-----
c           Damage accumulation
            IF (DMG_FLAG == 1 .AND. UVAR(I,1) <= DCRIT) THEN
              IF (EPSF_N(I) > ZERO .AND. SIGM(I) >= ZERO ) THEN 
                UVAR(I,3) = UVAR(I,3) + DP(I)*DPLA(I)/EPSF_N(I)
              ENDIF                
c
              IF (UVAR(I,3) >= ONE) THEN                ! softening starts
                IF (FADE_EXPO(i) /= ZERO) THEN 
                  UVAR(I,4) = UVAR(I,4) + DP(I)*DPLA(I)/(EPSF(I)-EPSF_N(I))
                  IF (UVAR(I,4) > DD) THEN 
                    DMG_SCALE(I) = ONE - ((UVAR(I,4)-DD)/(ONE-DD))**FADE_EXPO(i)
                    DMG_SCALE(I) = MAX(DMG_SCALE(I),ZERO)
                  ENDIF
                ENDIF
              ENDIF   
c            ELSEIF (DMG_FLAG == 1) THEN 
c              DMG_SCALE(I) = ZERO
            ENDIF ! end damage accumulation and softening
c-----            
            IF (UVAR(I,1) >= DCRIT) THEN
              NINDXF = NINDXF+1                                                 
              INDXF(NINDXF) = I                                                                                     
              TDEL(I)= TIME  
              IF (IFAIL_SH == 3) THEN
                FOFF(I) = -1
              ELSE
                FOFF(I) = 0
              ENDIF
            ENDIF                                                 
      ENDDO ! IEL
c
c--------------------------------------------      
c     Maximum Damage storing for output : 0 < DFMAX < 1
c
#include "vectorize.inc"
      DO J=1,NINDX                           
        I = INDX(J)  
        DFMAX(I)= MIN(ONE,MAX(DFMAX(I),UVAR(I,1)/DCRIT))
      ENDDO
c--------------------------------------------      
c     print
c--------------------------------------------      
      IF (NINDXF > 0) THEN  
        DO J=1,NINDXF                           
          I = INDXF(J)  
#include  "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,ILAY,IPT
          WRITE(ISTDO,2100) NGL(I),IPG,ILAY,IPT,TIME
#include  "lockoff.inc" 
        ENDDO
      ENDIF  
c------------------------
 2000 FORMAT(1X,'FAILURE (FTAB) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 2100 FORMAT(1X,'FAILURE (FTAB) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------------
      RETURN
      END
