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
!||    fail_nxt_c      ../engine/source/materials/fail/nxt/fail_nxt_c.F
!||--- called by ------------------------------------------------------
!||    mulawc          ../engine/source/materials/mat_share/mulawc.F90
!||    usermat_shell   ../engine/source/materials/mat_share/usermat_shell.F
!||--- calls      -----------------------------------------------------
!||    finter          ../engine/source/tools/curve/finter.F
!||====================================================================
      SUBROUTINE FAIL_NXT_C(FAIL,
     1     NEL       ,NUVAR     ,NVARTMP   ,UVAR      ,VARTMP    ,
     2     TIME      ,
     3     NGL       ,IPG       ,ILAY      ,IPT       ,HARDM     ,
     4     SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,  
     5     OFF       ,FOFF      ,DFMAX     ,TDEL      ,LF_DAMMX  )
C-----------------------------------------------
c     NXT Failure
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
      use my_alloc_mod
      use my_dealloc_mod, only : my_dealloc
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
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
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C TDEL    | NEL     | F | W | FAILURE TIME
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      integer ,intent(in) :: nel,nuvar,nvartmp,ipg,ilay,ipt,lf_dammx
      integer ,dimension(nel) ,intent(in) :: ngl
      my_real ,intent(in) :: time
      my_real ,dimension(nel) ,intent(in)    :: off,hardm,
     .   signxx,signyy,signxy,signyz,signzx
      type (fail_param_) ,intent(inout) :: fail    !< failure model data structure
c-----------------------------------------------
c   i n p u t   o u t p u t   a r g u m e n t s 
c-----------------------------------------------
      integer ,dimension(nel) ,intent(inout) :: foff
      integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp
      my_real ,dimension(nel) ,intent(out)   :: tdel
      my_real ,dimension(nel,nuvar) ,intent(inout) :: uvar
      my_real ,dimension(nel,lf_dammx),intent(inout) :: dfmax
c-----------------------------------------------
c   l o c a l   v a r i a b l e s
c-----------------------------------------------
      integer :: i,j,nindx,nindxf
      integer ,dimension(nel) :: indx,indxf,check
      my_real :: s1,s2,ss,s12
      my_real ,dimension(nel)  :: sig1,sig2,lambdanxt
      my_real ,dimension(nel)  :: sigsr,sig3d,dydx,tab_loc,sigsr_loc,sig3d_loc
C=======================================================================
      SIG1(1:nel) = ZERO       ! 1st principal normalized stress
      SIG2(1:nel) = ZERO       ! 2d principal normalized stress
      LAMBDANXT(1:nel) = ZERO  ! Instability factor
      NINDX  = 0
      NINDXF = 0                             
C-------- ZERO VALUE for deleted element       
      DO I=1,NEL
        IF (OFF(I) == ZERO) THEN 
          UVAR(I,1)= ZERO
          UVAR(I,2)= ZERO
        END IF
      ENDDO    
      ! Tag active elements
      DO I = 1,NEL
        IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
          NINDX = NINDX + 1
          INDX(NINDX) = I
        ENDIF
      ENDDO 
!
C-------------------------------------------------------
C     SIGMA1 and SIGMA2 computing: Principal stresses
C-------------------------------------------------------
      DO J=1,NINDX
        I = INDX(J)
          S12= SIGNXY(I)
          S1 = HALF*(SIGNXX(I) + SIGNYY(I))
          S2 = HALF*(SIGNXX(I) - SIGNYY(I))
          SIG1(I) = S1  + SQRT(S2**2 + S12**2)
          SS   = S1 - SQRT(S2**2 + S12**2)
          SIG2(I) = SS
          IF(SS >= SIG1(I))THEN
            SIG2(I) = SIG1(I)
            SIG1(I) = SS
          ENDIF
          CHECK(i) = 1
          IF (SIG2(I)/=ZERO) THEN
            SS = SIG1(I)/SIG2(I)
            IF ((SS < -ONE .AND. SS < ZERO) .OR. (SIG1(I) < ZERO .AND. SIG2(I) < ZERO)) THEN
              CHECK(i) = 0
            ENDIF
          ENDIF
C ---     Normalization of principal stresses vs hardening modulus
          S1 = SIG1(I)
          S2 = SIG2(I)
          SIG1(I) = SIG1(I)/MAX(HARDM(I),EM20)
          SIG2(I) = SIG2(I)/MAX(HARDM(I),EM20)
      END DO
! ---   compute failure parameters from input curve SR and 3D
      do j = 1,nindx
        i = indx(j)
        tab_loc(j) = sig2(i)
      end do

      call table_mat_vinterp(fail%table4d(1),nel,nindx,vartmp(1:nel,1:1),tab_loc,sigsr,dydx)
      call table_mat_vinterp(fail%table4d(2),nel,nindx,vartmp(1:nel,2:2),tab_loc,sig3d,dydx)
      do j = 1,nindx
        i = indx(j)
        sigsr(i) = sigsr_loc(j)
        sig3d(i) = sig3d_loc(j)
      end do! --- The instability factor
      DO J=1,NINDX
        I = INDX(J)
        IF (CHECK(I) == 1 ) THEN           
            IF(SIGSR(I) < ABS(SIG2(I)).AND.SIG3D(I) < ABS(SIG2(I))) THEN
               LAMBDANXT(I) = TWO
            ELSEIF(SIGSR(I) < ABS(SIG2(I))) THEN
              SIGSR(I) = ABS(SIG2(I))  
              LAMBDANXT(I) = ONE + ((SIG1(I) - SIGSR(I))/(SIG3D(I) - SIGSR(I)))
            ELSE 
             LAMBDANXT(I) = ONE + ((SIG1(I) - SIGSR(I))/(SIG3D(I) - SIGSR(I)))
            ENDIF
            LAMBDANXT(I) = MAX(LAMBDANXT(I),ZERO)
            LAMBDANXT(I) = MIN(LAMBDANXT(I),TWO)
        ENDIF
      ENDDO
c-----------------------
      DO J=1,NINDX
        I = INDX(J)
        IF (LAMBDANXT(I) >= TWO) THEN
          NINDXF = NINDXF + 1
          INDXF(NINDXF) = I  
          FOFF(I) = 0
          TDEL(I) = TIME
        ENDIF
      ENDDO
c-----------------------
      IF (NINDXF > 0) THEN
        DO J=1,NINDXF
          I = INDXF(J)
#include  "lockon.inc"
           WRITE(IOUT, 2000) NGL(I),IPT,LAMBDANXT(I)
           WRITE(ISTDO,2100) NGL(I),IPT,TIME,LAMBDANXT(I)
#include  "lockoff.inc"
         END DO
      ENDIF   ! NINDXF            
c-----------------------
c     SAVE USER VARIABLES
c-----------------------
      DO I=1,NEL                          
        DFMAX(I,2) = MAX(DFMAX(I,2),LAMBDANXT(I))                                     
        UVAR(I,1)  = SIG1(I)                                                      
        UVAR(I,2)  = SIG2(I)                                                      
        DFMAX(I,1) = MIN(ONE, DFMAX(I,2)/TWO) ! Maximum Dmg for output : 0 < DFMAX < 1  
      ENDDO                                                                      
C-----------------------------------------------
 2000 FORMAT(1X,'(NXT FAILURE) FOR SHELL ELEMENT ',I10,1X,
     .'LAYER',I10,':',/
     . 1X,'WITH FAILURE FACTOR :',1PE20.13)
 2100 FORMAT(1X,'(NXT FAILURE) FOR SHELL ELEMENT',I10,1X,
     .'LAYER',I10,':',/,
     . 1X,'AT TIME :',1PE20.13/,
     . 1X,'WITH FAILURE FACTOR :',1PE20.13)
C-----------------------------------------------
        RETURN
        END
