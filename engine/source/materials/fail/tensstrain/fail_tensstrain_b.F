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
!||    fail_tensstrain_b   ../engine/source/materials/fail/tensstrain/fail_tensstrain_b.F
!||--- called by ------------------------------------------------------
!||    fail_beam3          ../engine/source/elements/beam/fail_beam3.F
!||--- calls      -----------------------------------------------------
!||    finter              ../engine/source/tools/curve/finter.F
!||====================================================================
      SUBROUTINE FAIL_TENSSTRAIN_B(FAIL ,
     .           NUVAR    ,NVARTMP  ,UVAR     ,VARTMP   ,
     .           NEL      ,NGL      ,ISMSTR   ,DMGSCL   ,
     .           TIME     ,EPSD     ,OFF      ,DFMAX    ,       
     .           TDEL     ,IOUT     ,ISTDO    ,
     .           EPSXX    ,AL       ,TSTAR    )
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
      INTEGER                     ,INTENT(IN)    :: NEL     !< size of element group
      INTEGER                     ,INTENT(IN)    :: IOUT    !< output file unit
      INTEGER                     ,INTENT(IN)    :: ISTDO   !< output file unit      
      INTEGER ,DIMENSION(NEL)     ,INTENT(IN)    :: NGL     !< table of element identifiers
      INTEGER                     ,INTENT(IN)    :: ISMSTR
      INTEGER                     ,INTENT(IN)    :: NUVAR
      INTEGER                     ,INTENT(IN)    :: NVARTMP
      INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
      my_real                     ,INTENT(IN)    :: TIME    !< current time
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: EPSD    !< strain rate
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: TSTAR      
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: AL
      my_real ,DIMENSION(NEL)     ,INTENT(IN)    :: EPSXX
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: OFF     !< element deactivation flag
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: DFMAX   !< maximum damage
      my_real ,DIMENSION(NEL)     ,INTENT(INOUT) :: TDEL    !< deactivation time
      my_real ,DIMENSION(NEL,NUVAR),INTENT(INOUT) :: UVAR   
      my_real, DIMENSION(NEL)     ,INTENT(INOUT) :: DMGSCL 
      type (fail_param_) ,intent(inout) :: fail             !< failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDX,NINDXD,NINDXF,NINDX1,NINDX2,S_FLAG,SCALE_FLAG,STRDEF,STRFLAG
      INTEGER ,DIMENSION(NEL) :: INDX,INDXD,INDXF,INDX1,INDX2
      my_real :: R1,R2,EPSF1,EPSF2,EL_REF,EPST1,EPST2
      my_real ,DIMENSION(NEL) ::  EPS_EQ,DAMAGE,EPSRATE,EPS11,EPSI
      my_real ,dimension(nel) :: lambda,xvec,dydx
      my_real ,dimension(nel) :: rfac,rfac2
C=======================================================================
      STRFLAG = 0 
      STRDEF  = FAIL%IPARAM(1)
      S_FLAG  = FAIL%IPARAM(2)
!
      EPST1   = FAIL%UPARAM(1)
      EPST2   = FAIL%UPARAM(2)
      EPSF1   = FAIL%UPARAM(3)            
      EPSF2   = FAIL%UPARAM(4)
      EL_REF  = FAIL%UPARAM(5)
c
      DAMAGE(:NEL)  = ZERO
      EPS_EQ(:NEL)  = ZERO
      NINDXD  = 0  
      NINDXF  = 0  
C-----------------------------------------------
c     Initialization
C-----------------------------------------------
      DO I=1,NEL
        UVAR(I,2) = UVAR(I,2) + EPSXX(I)        
      ENDDO
!              
      IF (UVAR(1,1) == ZERO) THEN 
        SELECT CASE (S_FLAG)
          CASE (2,3)
            IF (fail%table4d(2)%notable > 0)THEN
              LAMBDA(:) = AL(:) / EL_REF
              call table_mat_vinterp(fail%table4d(2),nel,nel,vartmp(1:nel,1:1),lambda,uvar(1:nel,1),dydx)
            ELSE
              UVAR(:,1) = ONE
            ENDIF
              
          CASE DEFAULT ! default - old formulation
            UVAR(1:NEL,1) = EPSF1
        END SELECT
      ENDIF
      NINDX  = 0
      NINDXD = 0  
      NINDXF = 0  
      NINDX1 = 0  
      NINDX2 = 0  
c----------------------------------------------
c     Max strain transformation following input definition
c-------------------     
      IF (STRDEF == 2 .AND. (ISMSTR == 0 .OR. ISMSTR == 4) ) THEN ! failure defined as engineering strain
c       transform true strain to engineering         
        DO I=1,NEL
          EPS11(I)   = EXP(UVAR(I,2)) - ONE
          EPSRATE(I) = EXP(EPSD(I)) - ONE
        ENDDO        
c
      ELSE IF (STRDEF == 3 .AND. ISMSTR == 1) THEN   ! failure defined as true strain
c       transform engineering to true strain    
        DO I=1,NEL
          EPS11(I)   = LOG(UVAR(I,2) + ONE)
          EPSRATE(I) = LOG(EPSD(I) + ONE)
        ENDDO        
      ELSE
        DO I=1,NEL
          EPS11(I)   = UVAR(I,2)
          EPSRATE(I) = EPSD(I) 
        ENDDO               
      END IF                   
!
      NINDX  = 0
      NINDXD = 0  
      NINDXF = 0  
      NINDX1 = 0  
      NINDX2 = 0  
!     Tag active elements 
      do i=1,nel
        if (off(i) == one) then
          nindx = nindx + 1
          indx(nindx) = i  
        end if
      end do
c-------------------------
      SELECT CASE (S_FLAG)
c-------------------------
        CASE (1)  ! Equivalent strain criterion  AND PRINCIPAL
          DO J=1,NINDX
            I = INDX(J) 
            EPS_EQ(I)  = ABS(EPS11(I))
            EPSI(I)    = EPS11(I)             
          ENDDO
        CASE (2)  ! Equivalent strain criterion  
          DO J=1,NINDX
            I = INDX(J) 
            EPS_EQ(I)  = ABS(EPS11(I))
          ENDDO
        CASE (3)  ! Equivalent strain criterion  AND PRINCIPAL
          DO J=1,NINDX
            I = INDX(J) 
            EPSI(I)    = EPS11(I)
          ENDDO
      END SELECT
!-----------------------------------------------------------------------------          
!     Failure criteria
!-----------------------------------------------------------------------------     

      SELECT CASE (S_FLAG)
        
      CASE (2)
C-------------------
c      Equivalent strain. New format: Element length and temperature
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
          call table_mat_vinterp(fail%table4d(3),nindx,nel,vartmp(1:nel,2:2),xvec,rfac,dydx)
          rfac2(:) = max(rfac(:),em20)
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
          endif
          if (eps_eq(i) >= r2) then
            off(i)  = four_over_5
            tdel(i) = time 
            nindxf  = nindxf + 1  
            indxf(nindxf) = i
          endif 
        end do
C-------------------
        CASE (3)
c       principal strain. with Element length and temperature
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
          call table_mat_vinterp(fail%table4d(3),nindx,nel,vartmp(1:nel,2:2),xvec,rfac,dydx)
          rfac2(:) = max(rfac(:),em20)
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
          endif
        end do

        CASE DEFAULT  
C-------------------
c        Equivalent strain.  
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
          endif
          if (eps_eq(i) >= r2) then
            off(i)  = four_over_5
            tdel(i) = time 
            nindxf  = nindxf + 1  
            indxf(nindxf) = i
          endif
        end do
        do j=1,nindx
          i = indx(j) 
          IF (EPSF1 > ZERO .OR. EPSF2 > ZERO) THEN               
            IF (EPS_EQ(I) > UVAR(I,1) .AND. UVAR(I,1) /= ZERO) THEN
                  OFF(I)  = FOUR_OVER_5
                  TDEL(I) = TIME 
                  NINDX1  = NINDX1 + 1  
                  INDX1(NINDX1) = I
            ELSEIF (EPSI(I) > EPSF2 .AND. EPSF2 > ZERO) THEN
                  OFF(I)  = FOUR_OVER_5
                  TDEL(I) = TIME 
                  NINDX2  = NINDX2 + 1  
                  INDX2(NINDX2) = I
                  EPS_EQ(I) = EPSI(I)
            ENDIF
            DFMAX(I)   = MAX(DFMAX(I),MIN( ONE, ( EPS_EQ(I) / UVAR(I,1))))
            SCALE_FLAG = 0
          ENDIF                                
        end do
      END SELECT

C-------------------
c---  Maximum Damage for output : 0 < DFMAX < 1
      DO I=1,NEL
        R1 = EPSF1
        R2 = EPSF2
        IF (R1 < R2) THEN    
          DMGSCL(I) = ONE - DAMAGE(I)
        ENDIF
        DFMAX(I) = MAX(DFMAX(I),DAMAGE(I))  ! Maximum Damage storing for output : 0 < DFMAX < 1
      ENDDO     
C-------------------       
c------------------------
c     print
c------------------------
      IF (NINDXD > 0) THEN    ! start damage
        DO J=1,NINDXD    
          I = INDXD(J)
          IF (S_FLAG == 3) THEN
#include  "lockon.inc"
            WRITE(IOUT, 2001) NGL(I),EPSI(I)
            WRITE(ISTDO,2001) NGL(I),EPSI(I)
#include  "lockoff.inc"
          ELSE
#include  "lockon.inc"
            WRITE(IOUT, 2002) NGL(I), EPS_EQ(I)
            WRITE(ISTDO,2002) NGL(I), EPS_EQ(I)
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
            WRITE(IOUT, 3001) NGL(I),EPSI(I)
            WRITE(ISTDO,3001) NGL(I),EPSI(I)
#include  "lockoff.inc"
          ELSE
            WRITE(IOUT, 3101) NGL(I), EPS_EQ(I)
            WRITE(ISTDO,3101) NGL(I), EPS_EQ(I)
          ENDIF
        END DO
      END IF              
c
      IF (NINDX1 > 0) THEN 
        DO J=1,NINDX1    
          I = INDX1(J)
#include  "lockon.inc"
          WRITE(IOUT, 4000) NGL(I), EPS_EQ(I),TIME
          WRITE(ISTDO,4100) NGL(I), EPS_EQ(I),TIME
#include  "lockoff.inc"
        END DO
      END IF              
c
      IF (NINDX2 > 0) THEN 
        DO J=1,NINDX2    
          I = INDX2(J)
#include  "lockon.inc"
          WRITE(IOUT, 5000) NGL(I), EPS_EQ(I),TIME
          WRITE(ISTDO,5100) NGL(I), EPS_EQ(I),TIME
#include  "lockoff.inc"
        END DO
      END IF              
c------------------
             
c------------------
 2001 FORMAT(1X,'START DAMAGE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,   
     .       1X,'PRINCIPAL STRAIN=',G11.4)
 2002 FORMAT(1X,'START DAMAGE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X, 
     .       'EQUIVALENT STRAIN=',G11.4)
C
 3001 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,   
     .       'PRINCIPAL STRAIN=',G11.4)
 3101 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X, 
     .       'EQUIVALENT STRAIN=',G11.4)
C
 4000 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,   
     .       'EQUIVALENT STRAIN=',G11.4,1X,'AT TIME :',1PE12.4)
 4100 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X, 
     .       'EQUIVALENT STRAIN=',G11.4,1X,'AT TIME :',1PE12.4)
C
 5000 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X,   
     .       'PRINCIPAL STRAIN=',G11.4,1X,'AT TIME :',1PE12.4)
 5100 FORMAT(1X,'FAILURE (TENSSTRAIN) OF BEAM ELEMENT ',I10,1X, 
     .      'PRINCIPAL STRAIN=',G11.4,1X,'AT TIME :',1PE12.4)
c------------------
      RETURN
      END
