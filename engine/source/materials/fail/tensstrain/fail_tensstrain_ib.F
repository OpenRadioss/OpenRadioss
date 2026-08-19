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
!||    fail_tensstrain_ib   ../engine/source/materials/fail/tensstrain/fail_tensstrain_ib.F
!||--- called by ------------------------------------------------------
!||    fail_beam18          ../engine/source/elements/beam/fail_beam18.F
!||--- calls      -----------------------------------------------------
!||    finter               ../engine/source/tools/curve/finter.F
!||====================================================================
      SUBROUTINE FAIL_TENSSTRAIN_IB(FAIL      ,NEL      ,NGL      ,
     .           NUVAR    ,NVARTMP  ,UVAR     ,VARTMP   ,
     .           TIME     ,EPSD     ,OFF      ,DFMAX    ,       
     .           TDEL     ,IOUT     ,ISTDO    ,
     .           EPSXX    ,AL       ,TSTAR    ,DAMSCL   ,         
     .           IPT      ,FOFF     ,ISMSTR   ,UELR     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
C-----------------------------------------------
c    tens strain failure model for integrated beams (TYPE 18) !irup=10
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER                     ,INTENT(IN)    :: NEL     ! size of element group
      INTEGER                     ,INTENT(IN)    :: IPT     ! current integration point
      INTEGER                     ,INTENT(IN)    :: IOUT    ! output file unit
      INTEGER                     ,INTENT(IN)    :: ISTDO   ! output file unit      
      INTEGER                     ,INTENT(IN)    :: ISMSTR
      INTEGER                     ,INTENT(IN)    :: NUVAR   ! number of user variables
      INTEGER                     ,INTENT(IN)    :: NVARTMP ! number of tmp user variables
      INTEGER ,DIMENSION(NEL)     ,INTENT(IN)    :: NGL     ! table of element identifiers
      INTEGER ,DIMENSION(NEL)     ,INTENT(INOUT) :: FOFF
      INTEGER ,DIMENSION(NEL,NVARTMP),INTENT(INOUT) :: VARTMP
      my_real                     ,INTENT(IN)    :: TIME    ! current time    
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: EPSD    ! strain rate
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: TSTAR
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: AL
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: EPSXX 
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: OFF     ! element deactivation flag
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DFMAX   ! maximum damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DAMSCL  !1-damage
      my_real ,DIMENSION(NEL,NUVAR),INTENT(INOUT):: UVAR   
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: TDEL    ! desactivation time
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: UELR  
      type (fail_param_) ,intent(inout) :: fail             !< failure model data structure  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDX,NINDXD,NINDXF,NINDX1,NINDX2,S_FLAG,STRDEF,STRFLAG,FAILIP
      INTEGER ,DIMENSION(NEL) :: INDX,INDXD,INDXF,INDX1,INDX2
      my_real :: R1,R2,EPSF1,EPSF2,EL_REF,EPST1,EPST2
      my_real ,DIMENSION(NEL) :: EPS_MAX,DAMAGE,EPSRATE,EPS11,EPSI,EPS_EQ
      my_real ,dimension(nel) :: lambda,xvec,dydx
      my_real ,dimension(nel) :: rfac,rfac2
C=======================================================================  
      STRDEF  = FAIL%IPARAM(1)
      S_FLAG  = FAIL%IPARAM(2)
      FAILIP  = FAIL%IPARAM(3)            ! 	number of failed integration point prior to solid element deletion. default = 1 (integer)
!
      EPST1   = FAIL%UPARAM(1)
      EPST2   = FAIL%UPARAM(2)
      EPSF1   = FAIL%UPARAM(3)            
      EPSF2   = FAIL%UPARAM(4)
      EL_REF  = FAIL%UPARAM(5)    
c
      DAMAGE(:NEL)  = ZERO
      EPS_MAX(:NEL) = ZERO
      NINDX   = 0
      NINDXD  = 0  
      NINDXF  = 0  
      NINDX1  = 0  
      NINDX2  = 0    
      STRFLAG = 0 
C-----------------------------------------------
c     Initialization
C-----------------------------------------------
      IF (UVAR(1,1) == ZERO) THEN 
        SELECT CASE (S_FLAG)
          CASE (2,3)
            IF (fail%table4d(2)%notable > 0)THEN
              LAMBDA(:) = AL(:) / EL_REF
              call table_mat_vinterp(fail%table4d(2),nel,nel,vartmp(1:nel,3:3),lambda,uvar(1:nel,1),dydx)
            ELSE
              UVAR(:,1) = ONE
            ENDIF
              
          CASE DEFAULT ! default - old formulation
            UVAR(1:NEL,1) = EPSF1
        END SELECT
      ENDIF
c----------------------------------------------
c     Max strain transformation following input definition
c-------------------     
      IF (STRDEF == 2 .AND. (ISMSTR == 0 .OR. ISMSTR == 4) ) THEN ! failure defined as engineering strain
c       transform true strain to engineering         
        DO I=1,NEL
          EPS11(I)   = EXP(EPSXX(I)) - ONE
          EPSRATE(I) = EXP(EPSD(I))  - ONE
        ENDDO        
c
      ELSE IF (STRDEF == 3 .AND. ISMSTR == 1) THEN   ! failure defined as true strain
c       transform engineering to true strain    
        DO I=1,NEL
          EPS11(I)   = LOG(EPSXX(I) + ONE)
          EPSRATE(I) = LOG(EPSD(I) + ONE)
        ENDDO        
      ELSE
        DO I=1,NEL
          EPS11(I)   = EPSXX(I) 
          EPSRATE(I) = EPSD(I) 
        ENDDO               
      END IF           

c-------------------------
      SELECT CASE (S_FLAG)
c-------------------------
        CASE (1)  ! Equivalent strain criterion  AND PRINCIPAL
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPS_EQ(I)  = ABS(EPS11(I))
              EPSI(I)    = EPS11(I)
            END IF
          ENDDO
        CASE (2)  ! Equivalent strain criterion  
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPS_EQ(I)  = ABS(EPS11(I))
            END IF
          ENDDO
        CASE (3)  ! Equivalent strain criterion  AND PRINCIPAL
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              EPSI(I)    = EPS11(I)
            END IF
          ENDDO
      END SELECT     
c-------------------  
!     Tag active elements 
      do i=1,nel
        if (off(i) == one .and. foff(i) == 1) then
          nindx = nindx + 1
          indx(nindx) = i  
        end if
      end do
c-----------------------------------------------------------------------------------
      SELECT CASE (S_FLAG)
        CASE (2)
C-------------------
c     Equivalent strain. New format: Element length and temperature
C-------------------
        if (fail%table4d(1)%notable > 0) then   ! strain rate
          do j=1,nindx
            i = indx(j)
            xvec(j) = epsrate(i)
          enddo
          call table_mat_vinterp(fail%table4d(1),nindx,nel,vartmp(1:nel,1:1),xvec,rfac,dydx)
          rfac(:) = max(rfac(:),em20)
        else
          rfac(:) = one
        endif
        if (fail%table4d(3)%notable > 0) then  ! temperature
          do j=1,nindx
            i = indx(j)
            xvec(j) = tstar(i)
          enddo
          call table_mat_vinterp(fail%table4d(3),nindx,nel,vartmp(1:nel,2:2),xvec,rfac2,dydx)
          rfac2(:) = max(rfac2(:),em20)
        else
          rfac2(:) = one
        endif
        !
        do j=1,nindx
          i = indx(j)
          r1 = epst1*rfac(j)*rfac2(j)*uvar(i,1)
          r2 = epst2*rfac(j)*rfac2(j)*uvar(i,1)  
          if (eps_eq(i) > r1) then
            if (dfmax(i) == zero) then
              nindxd = nindxd + 1  
              indxd(nindxd) = i
            endif   
            damage(i) = min(one, (eps_eq(i)-r1)/(r2-r1))
            dfmax(i)  = max(dfmax(i) ,damage(i))
          endif
          if (eps_eq(i) >= r2) then
            foff(i)  = 0
            tdel(i) = time 
            nindxf  = nindxf + 1  
            indxf(nindxf) = i
            dfmax(i) = one
            uelr(i)  = uelr(i) + one
            if (nint(uelr(i)) >= failip) then
              off(i) = four_over_5
            end if
          end if 
        end do
C-------------------
        CASE (3)
C-------------------
c     principal strain. with Element length and temperature
C-------------------
        if (fail%table4d(1)%notable > 0) then   ! strain rate
          do j=1,nindx
            i = indx(j)
            xvec(j) = epsrate(i)
          enddo
          call table_mat_vinterp(fail%table4d(1),nindx,nel,vartmp(1:nel,1:1),xvec,rfac,dydx)
          rfac(:) = max(rfac(:),em20)
        else
          rfac(:) = one
        endif
        if (fail%table4d(3)%notable > 0) then  ! temperature
          do j=1,nindx
            i = indx(j)
            xvec(j) = tstar(i)
          enddo
          call table_mat_vinterp(fail%table4d(3),nindx,nel,vartmp(1:nel,2:2),xvec,rfac2,dydx)
          rfac2(:) = max(rfac2(:),em20)
        else
          rfac2(:) = one
        endif
        !
        do j=1,nindx
          i = indx(j)
          r1 = epst1*rfac(j)*rfac2(j)*uvar(i,1)
          r2 = epst2*rfac(j)*rfac2(j)*uvar(i,1)  
          if (epsi(i) > r1) then
            if (dfmax(i) == zero) then
              nindxd = nindxd + 1  
              indxd(nindxd) = i
            endif   
            damage(i) = min(one, (epsi(i)-r1)/(r2-r1))
            dfmax(i)  = max(dfmax(i) ,damage(i))
          endif
          if (epsi(i) >= r2) then
            foff(i)  = 0
            tdel(i) = time 
            nindxf  = nindxf + 1  
            indxf(nindxf) = i
            dfmax(i)    = one
            uelr(i)  = uelr(i) + one
            if (nint(uelr(i)) >= failip) then
              off(i) = four_over_5
            end if           
          end if
        end do
C-------------------
c     Equivalent strain.  
C-------------------
        CASE DEFAULT  
        if (fail%table4d(1)%notable > 0) then   ! strain rate
          do j=1,nindx
            i = indx(j)
            xvec(j) = epsrate(i)
          enddo
          call table_mat_vinterp(fail%table4d(1),nindx,nel,vartmp(1:nel,1:1),xvec,rfac,dydx)
          rfac(:) = max(rfac(:),em20)
        else
          rfac(:) = one
        endif
!
        do j=1,nindx
          i = indx(j)
          r1 = epst1*rfac(j)
          r2 = epst2*rfac(j)
          if (eps_eq(i) > r1) then
            if (dfmax(i) == zero) then
              nindxd = nindxd + 1  
              indxd(nindxd) = i
            endif   
            damage(i) = min(one, (eps_eq(i)-r1)/(r2-r1))
            dfmax(i)  = max(dfmax(i) ,damage(i))
          endif
          if (eps_eq(i) >= r2) then
            off(i)  = four_over_5
            tdel(i) = time 
            nindxf  = nindxf + 1  
            indxf(nindxf) = i
            dfmax(i)    = one
            uelr(i)  = uelr(i) + one
            if (nint(uelr(i)) >= failip) then
              off(i) = four_over_5
            end if           
          end if
        end do
!
        if (epsf1 > zero .or. epsf2 > zero) then               
          do j=1,nindx
            i = indx(j) 
            if (eps_eq(i) > uvar(i,1) .and. uvar(i,1) /= zero) then
                  foff(i) = 0
                  tdel(i) = time 
                  nindx1  = nindx1 + 1  
                  indx1(nindx1) = i
                  uelr(i) = uelr(i) + one
                  if (nint(uelr(i)) >= failip) then
                    off(i) = four_over_5
                  endif 
            elseif (epsi(i) > epsf2 .and. epsf2 > zero) then
                  foff(i) = 0
                  tdel(i) = time 
                  nindx2  = nindx2 + 1  
                  indx2(nindx2) = i
                  eps_eq(i) = epsi(i)
                  uelr(i) = uelr(i) + one
                  if (nint(uelr(i)) >= failip) then
                    off(i) = four_over_5
                  endif 
            endif
            dfmax(i)   = max(dfmax(i),min( one, (eps_eq(i) / uvar(i,1))))
          end do
        end if
!
      END SELECT

C-------------------
      DO I=1,NEL
        DAMSCL(I) = ONE - DFMAX(I)
      END DO
c------------------------
c     print
c------------------------
      IF (NINDXD > 0) THEN    ! start damage
        DO J=1,NINDXD    
          I = INDXD(J)
          IF (S_FLAG == 3) THEN
#include  "lockon.inc"
            WRITE(IOUT, 2001) NGL(I),IPT,EPSI(I)
            WRITE(ISTDO,2001) NGL(I),IPT,EPSI(I)
            IF (OFF(i) == FOUR_OVER_5) THEN
              WRITE(IOUT, 1111) NGL(I),TIME
              WRITE(ISTDO,1111) NGL(I),TIME
            ENDIF 
#include  "lockoff.inc"
          ELSE
#include  "lockon.inc"
            WRITE(IOUT, 2002) NGL(I),IPT,EPS_EQ(I)
            WRITE(ISTDO,2002) NGL(I),IPT,EPS_EQ(I)
            IF (OFF(i) == FOUR_OVER_5) THEN
              WRITE(IOUT, 1111) NGL(I),TIME
              WRITE(ISTDO,1111) NGL(I),TIME
            ENDIF 
#include  "lockoff.inc"
          ENDIF
        END DO
      END IF              

c
      IF (NINDXF > 0) THEN    ! failure
        DO J=1,NINDXF    
          I = INDXF(J)
          IF (S_FLAG == 3) THEN
#include  "lockon.inc"
            WRITE(IOUT, 3001) NGL(I),IPT,EPSI(I)
            WRITE(ISTDO,3001) NGL(I),IPT,EPSI(I)
            IF (OFF(i) == FOUR_OVER_5) THEN
              WRITE(IOUT, 1111) NGL(I),TIME
              WRITE(ISTDO,1111) NGL(I),TIME
            ENDIF 
#include  "lockoff.inc"
          ELSE
            WRITE(IOUT, 3101) NGL(I),IPT,EPS_EQ(I)
            WRITE(ISTDO,3101) NGL(I),IPT,EPS_EQ(I)
            IF (OFF(i) == FOUR_OVER_5) THEN
              WRITE(IOUT, 1111) NGL(I),TIME
              WRITE(ISTDO,1111) NGL(I),TIME
            ENDIF 
          ENDIF
        END DO
      END IF              
c
      IF (NINDX1 > 0) THEN 
        DO J=1,NINDX1    
          I = INDX1(J)
#include  "lockon.inc"
          WRITE(IOUT, 4000) NGL(I),IPT,EPS_EQ(I),TIME
          WRITE(ISTDO,4100) NGL(I),IPT,EPS_EQ(I),TIME
          IF (OFF(i) == FOUR_OVER_5) THEN
            WRITE(IOUT, 1111) NGL(I),TIME
            WRITE(ISTDO,1111) NGL(I),TIME
          ENDIF 
#include  "lockoff.inc"
        END DO
      END IF              
c
      IF (NINDX2 > 0) THEN 
        DO J=1,NINDX2    
          I = INDX2(J)
#include  "lockon.inc"
          WRITE(IOUT, 5000) NGL(I),IPT,EPS_EQ(I),TIME
          WRITE(ISTDO,5100) NGL(I),IPT,EPS_EQ(I),TIME
          IF (OFF(i) == FOUR_OVER_5) THEN
            WRITE(IOUT, 1111) NGL(I),TIME
            WRITE(ISTDO,1111) NGL(I),TIME
          ENDIF 
#include  "lockoff.inc"
        END DO
      END IF              
c------------------------
             
c------------------
 1111 FORMAT(1X,'DELETED BEAM ELEMENT ',I10,1X,'AT TIME :',1PE12.4)
 2001 FORMAT(1X,'START DAMAGE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,  
     .       1X,'PRINCIPAL STRAIN=',G11.4)
 2002 FORMAT(1X,'START DAMAGE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,
     .       1X,'EQUIVALENT STRAIN=',G11.4)
C
 3001 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,  
     .       1X,'PRINCIPAL STRAIN=',G11.4)
 3101 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,
     .       1X,'EQUIVALENT STRAIN=',G11.4)
C
 4000 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,  
     .       1X,'EQUIVALENT STRAIN=',G11.4,1X,'AT TIME :',1PE12.4)
 4100 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,
     .       1X,'EQUIVALENT STRAIN=',G11.4,1X,'AT TIME :',1PE12.4)
C
 5000 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,  
     .       1X,'PRINCIPAL STRAIN=',G11.4,1X,'AT TIME :',1PE12.4)
 5100 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,',INTEGRATION PT',I2,
     .       1X,'PRINCIPAL STRAIN=',G11.4,1X,'AT TIME :',1PE12.4)
c------------------
      RETURN
      END
