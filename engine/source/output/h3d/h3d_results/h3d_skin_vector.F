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
!||    h3d_skin_vector        ../engine/source/output/h3d/h3d_results/h3d_skin_vector.F
!||--- called by ------------------------------------------------------
!||    genh3d                 ../engine/source/output/h3d/h3d_results/genh3d.F
!||--- calls      -----------------------------------------------------
!||    finter                 ../engine/source/tools/curve/finter.F
!||    finter_smooth          ../engine/source/tools/curve/finter_smooth.F
!||    get_u_numsens          ../engine/source/user_interface/usensor.F
!||    get_u_sens_fpar        ../engine/source/user_interface/usensor.F
!||    get_u_sens_ipar        ../engine/source/user_interface/usensor.F
!||    get_u_sens_value       ../engine/source/user_interface/usensor.F
!||    press_seg3             ../engine/source/loads/general/load_pcyl/press_seg3.F
!||    set_u_sens_value       ../engine/source/user_interface/usensor.F
!||--- uses       -----------------------------------------------------
!||    h3d_inc_mod            ../engine/share/modules/h3d_inc_mod.F
!||    h3d_mod                ../engine/share/modules/h3d_mod.F
!||    loads_mod              ../common_source/modules/loads/loads_mod.F90
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||    pblast_mod             ../common_source/modules/loads/pblast_mod.F90
!||    pinchtype_mod          ../common_source/modules/pinchtype_mod.F
!||    sensor_mod             ../common_source/modules/sensor_mod.F90
!||    table_mod              ../engine/share/modules/table_mod.F
!||====================================================================
      SUBROUTINE H3D_SKIN_VECTOR(SKIN_VECTOR,NODAL_IPART,NSENSOR,
     .                   IS_WRITTEN_SKIN  ,H3D_PART,INFO1   ,KEYWORD ,
     .                   IB ,ILOADP,LLOADP,FAC ,NPC,TF ,SENSOR_TAB,
     .                   TAGNCONT,LOADP_HYD_INTER,FORC,XFRAME,X ,V ,
     .                   IMAPSKP,LOADS ,TABLE,IFRAME,DIS,PBLAST)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE H3D_MOD
      USE PINCHTYPE_MOD 
      USE PBLAST_MOD
      USE SENSOR_MOD
      USE H3D_INC_MOD
      USE LOADS_MOD
      USE TABLE_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
      USE PBLAST_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      INTEGER  GET_U_NUMSENS,GET_U_SENS_FPAR,GET_U_SENS_IPAR,
     .         GET_U_SENS_VALUE,SET_U_SENS_VALUE
      EXTERNAL GET_U_NUMSENS,GET_U_SENS_FPAR,GET_U_SENS_IPAR,
     .         GET_U_SENS_VALUE,SET_U_SENS_VALUE
C-----------------------------------------------,
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ,INTENT(IN) :: NSENSOR
      CHARACTER(LEN=NCHARLINE100):: KEYWORD
      my_real
     .   SKIN_VECTOR(3,*),TF(*),X(3,*),V(3,*),DIS(3,NUMNOD)
      TYPE (H3D_DATABASE) :: H3D_DATA
      INTEGER , DIMENSION(NUMSKINP0), INTENT(IN) ::  IMAPSKP
      INTEGER
     .   H3D_PART(*),IS_WRITTEN_SKIN(*),INFO1,NPC(*)
      INTEGER LLOADP(SLLOADP)
      INTEGER ILOADP(SIZLOADP,*),IB(NIBCLD,*)
      INTEGER TAGNCONT(NLOADP_HYD_INTER,NUMNOD),
     .        LOADP_HYD_INTER(NLOADP_HYD),NODAL_IPART(*)
      my_real
     .   FAC(LFACLOAD,NLOADP),XFRAME(NXFRAME,*),FORC(LFACCLD,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
      TYPE (LOADS_)   , INTENT(IN) :: LOADS
      INTEGER ,       DIMENSION(LISKN,NUMFRAM+1)  ,INTENT(IN) :: IFRAME
      TYPE (TTABLE)  ,DIMENSION(NTABLE)   ,INTENT(IN) :: TABLE
      TYPE(PBLAST_),INTENT(IN) :: PBLAST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NL, N1, ISK, N2, N3, N4, N5,NSKIN,  
     .        IAD ,NP ,IFUNC ,NPRES,NSKIN0,NSKIN1,N1FRAM,DIR_HSP,I,N
      INTEGER K1, K2, K3, ISENS,K,LL,IERR,
     .        N_OLD, ISMOOTH,IDEL,NINTERP ,NPL,TAGN1,TAGN2,TAGN3,TAGN4,
     .        FUN_CX,FUN_VEL,DIR_VEL,IFRA2, IANIM,IJK,UP_BOUND,
     .        IZ_UPDATE,ABAC_ID,ISIZ_SEG,IERR1,
     .        Phi_I, ID, USER_ID, ITA_SHIFT,NDT,NDT0,
     .        NITER,ITER,IMODEL,IL,IS,SEGCONT,FUN_HSP,IFRA1,NP0
      my_real
     .   NX, NY, NZ, AXI, AA, A0, VV, FX, FY, FZ, AX, DYDX, TS,
     .   SIXTH,X_OLD, F1, F2,XSENS,FCX,FCY,FCYPINCH,FP,
     .   FCX1,FCY1,FCX2,FCY2,VX,VY,VZ,VEL,VSEG,NORM
      my_real FINTER, PS, Zx,Zy,Zz,FINTER_SMOOTH
      my_real
     .   RXI,RYI,RZI,SXI,SYI,SZI
      my_real COORMEAN,YMEAN,ZMEAN,PVEL,NSIGN,DNORM,
     .        Xdet,Ydet,Zdet,Tdet,Wtnt,PMIN,Dx,Dy,Dz,NORMD, P,
     .        T0INF_LOC, TA_SHIFT, TT_STAR
      INTEGER :: IFUN,IFRA,M1,M2,NDIM,NPOINT, IIOUT,SHIFT,FUNCTYPE
       my_real :: A11,A12,A21,A22,B1,B2,DET,LEN,DIRX,DIRY,DIRZ,
     .     BETA,GAMMA,R,S,RMAX,XFACR,XFACT,YFAC,SEGP,PRESS,DISP
      my_real, DIMENSION(3) :: P0,DIR,A,B,C,D,M
      
      EXTERNAL FINTER,FINTER_SMOOTH
C=======================================================================
      IF (KEYWORD /= 'VECT/PEXT') RETURN
      NSKIN = NUMSKIN - NUMSKINP
      IS_WRITTEN_SKIN(NSKIN+1:NUMSKIN) = 0
            SKIN_VECTOR(1:3,NSKIN+1:NUMSKIN)=ZERO
        NP0= 0
        NSKIN0 = NSKIN
C---- fill SKIN_SCALAR(*) w/ IS_WRITTEN_SKIN(*)=1
C-----Force (pressure) first 
      N_OLD  = 0
      X_OLD  = ZERO
       DO NL=1,NCONLD-NPLOADPINCH
         N1      = IB(1,NL)
         N2      = IB(2,NL)
         N3      = IB(3,NL)
         N4      = IB(4,NL)
         N5      = IB(5,NL)
         IDEL    = IB(8,NL)
         FUNCTYPE= IB(9,NL)
         FCY     = FORC(1,NL)
         FCX     = FORC(2,NL)
         IF (N1==0.OR.N2==0.OR.N3==0.OR.N4==-1) CYCLE
C--------default zero        
         NP0 = NP0 + 1
         NSKIN = NSKIN0+ IMAPSKP(NP0)
         IF (NODAL_IPART(N1)>0) THEN
             IF (H3D_PART(NODAL_IPART(N1))==1) IS_WRITTEN_SKIN(NSKIN)=1
         END IF
         ISENS   = 0
         XSENS   = ONE
         DO K=1,NSENSOR
           IF(IB(6,NL)== SENSOR_TAB(K)%SENS_ID) ISENS=K
         ENDDO
         IF(ISENS==0)THEN
            TS=TT
         ELSE
            TS = TT- SENSOR_TAB(ISENS)%TSTART
            IF(TS < ZERO) CYCLE
         ENDIF
         IF(IDEL > 0 ) CYCLE  ! SEGMENT DELETED
         IF(FUNCTYPE == 1)THEN
           IF(N_OLD/=N5.OR.X_OLD/=TS) THEN
             ISMOOTH = 0
             IF (N5 > 0) ISMOOTH = NPC(2*NFUNCT+N5+1)
             IF (ISMOOTH == 0) THEN
               F1 = FINTER(N5,TS*FCX,NPC,TF,DYDX)
             ELSE IF(ISMOOTH > 0) THEN
               F1 = FINTER_SMOOTH(N5,TS*FCX,NPC,TF,DYDX)
             ELSE 
             ENDIF ! IF (ISMOOTH == 0)
           ENDIF
           N_OLD = N5
           X_OLD = TS
         ELSEIF(FUNCTYPE == 2)THEN
          IF(N_OLD/=N5) THEN
            DISP = (DIS(3,N1)+DIS(3,N2)+DIS(3,N3)+DIS(3,N4))/4.0
            ISMOOTH = 0
            IF (N5 > 0) ISMOOTH = NPC(2*NFUNCT+N5+1)
            IF (ISMOOTH == 0) THEN
              F1 = FINTER(N5,DISP*FCX,NPC,TF,DYDX)
            ELSE IF(ISMOOTH > 0) THEN
              F1 = FINTER_SMOOTH(N5,DISP*FCX,NPC,TF,DYDX)
            ELSE 
            ENDIF ! IF (ISMOOTH == 0)
          ENDIF
          N_OLD = N5
          X_OLD = DISP
         ELSEIF(FUNCTYPE == 3)THEN
          IF(N_OLD/=N5) THEN
            VEL = (V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4))/4.0
            ISMOOTH = 0
            IF (N5 > 0) ISMOOTH = NPC(2*NFUNCT+N5+1)
            IF (ISMOOTH == 0) THEN
              F1 = FINTER(N5,VEL*FCX,NPC,TF,DYDX)
            ELSE IF(ISMOOTH > 0) THEN
              F1 = FINTER_SMOOTH(N5,VEL*FCX,NPC,TF,DYDX)
            ELSE 
            ENDIF ! IF (ISMOOTH == 0)
          ENDIF
          N_OLD = N5
          X_OLD = VEL
         ENDIF
         AA = FCY*F1*XSENS
         SKIN_VECTOR(3,NSKIN)=SKIN_VECTOR(3,NSKIN) + AA
       END DO 
C----------load_pressure     
       SHIFT =   NLOADP_F+PBLAST%NLOADP_B
       DO NP=1+SHIFT,NLOADP_HYD+SHIFT
         ISIZ_SEG = ILOADP(1,NP)/4 
         IFUNC = ILOADP(3,NP)
         IAD = ILOADP(4,NP)
         NINTERP = ILOADP(5,NP)
         ISENS =  ILOADP(7,NP) 
         FCY = FAC(1,NP)
         FCX = FAC(2,NP)
C--------default zero        
         DO N=1, ISIZ_SEG
           N1 = LLOADP(IAD+4*(N-1))
           N2 = LLOADP(IAD+4*(N-1)+1)
           N3 = LLOADP(IAD+4*(N-1)+2)
           N4 = LLOADP(IAD+4*(N-1)+3)
           IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
           NSKIN = NSKIN0+ IMAPSKP(NP0+N)
           IF (NODAL_IPART(N1)>0) THEN
               IF (H3D_PART(NODAL_IPART(N1))==1) IS_WRITTEN_SKIN(NSKIN)=1
           END IF
         ENDDO
C
           IF(ISENS==0)THEN
              TS=TT
           ELSE
              TS = TT-SENSOR_TAB(ISENS)%TSTART
           ENDIF    
         DO N=1, ISIZ_SEG
           N1 = LLOADP(IAD+4*(N-1))
           N2 = LLOADP(IAD+4*(N-1)+1)
           N3 = LLOADP(IAD+4*(N-1)+2)
           N4 = LLOADP(IAD+4*(N-1)+3)
           IF (N1==0.OR.N2==0.OR.N3==0) CYCLE

           NP0 = NP0 + 1
           IF(TS<ZERO) CYCLE
           NSKIN = NSKIN0+ IMAPSKP(NP0)
           F1 = FINTER(IFUNC,TS*FCX,NPC,TF,DYDX)
           AA = FCY*F1  
C----------------
C       Check if segment is in contact 
C----------------
             SEGCONT = 0

             TAGN1 = 0
             TAGN2 = 0
             TAGN3 = 0
             TAGN4 = 0
             FP = ONE
             IF(NINTERP > 0 ) THEN           
                NPL = LOADP_HYD_INTER(NP)
                IF(N4/=0) THEN
                   IF(TAGNCONT(NPL,N1)==1.AND.TAGNCONT(NPL,N2)==1.AND.
     .                TAGNCONT(NPL,N3)==1.AND.TAGNCONT(NPL,N4)==1) THEN
                      SEGCONT = 1
                   ELSE
                      TAGN1 = TAGNCONT(NPL,N1)
                      TAGN2 = TAGNCONT(NPL,N2)
                      TAGN3 = TAGNCONT(NPL,N3)
                      TAGN4 = TAGNCONT(NPL,N4)
            FP = (TAGN1+TAGN2+TAGN3+TAGN4)/4
                   ENDIF
                ELSE
                   IF(TAGNCONT(NPL,N1)==1.AND.TAGNCONT(NPL,N2)==1.AND.
     .                TAGNCONT(NPL,N3)==1) THEN
                      SEGCONT = 1
                   ELSE
                      TAGN1 = TAGNCONT(NPL,N1)
                      TAGN2 = TAGNCONT(NPL,N2)
                      TAGN3 = TAGNCONT(NPL,N3)
            FP = (TAGN1+TAGN2+TAGN3)/3
                   ENDIF
                ENDIF    
                IF (FP==ZERO) FP = ONE
             ENDIF
           IF (SEGCONT==1) AA = ZERO
              AA=AA*FP
              SKIN_VECTOR(3,NSKIN)=SKIN_VECTOR(3,NSKIN) + AA
         END DO !N=1, NPRES/4
       END DO !NP=1,NLOADP_HYD
C---------pfluid     
       DO NL=1,NLOADP_F
C--------default zero        
         ISIZ_SEG = ILOADP(1,NL)/4 
         IAD = ILOADP(4,NL)
         DO N=1, ISIZ_SEG
           N1 = LLOADP(IAD+4*(N-1))
           N2 = LLOADP(IAD+4*(N-1)+1)
           N3 = LLOADP(IAD+4*(N-1)+2)
           N4 = LLOADP(IAD+4*(N-1)+3)
           IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
           NSKIN = NSKIN0+ IMAPSKP(NP0+N)
           IF (NODAL_IPART(N1)>0) THEN
               IF (H3D_PART(NODAL_IPART(N1))==1) IS_WRITTEN_SKIN(NSKIN)=1
           END IF
         ENDDO
         FUN_HSP=ILOADP(7,NL)
         DIR_HSP=ILOADP(8,NL)
         IFRA1=ILOADP(9,NL)
         FCY = FAC(1,NL)
         FCX = FAC(2,NL)
         FUN_CX=ILOADP(10,NL)
         FCY1 = FAC(3,NL)
         FCX1 = FAC(4,NL)
         FUN_VEL=ILOADP(11,NL)
         FCY2 = FAC(5,NL)
         FCX2 = FAC(6,NL)
         ! To avoid a check bound issue when the velocity options are not set in the input, 
         ! the DIR_VEL variable is bounded to a minimal value of 1
         DIR_VEL=MAX(ILOADP(12,NL),1)
         IFRA2=ILOADP(13,NL)
         ISENS=0
         XSENS = ONE
         DO K=1,NSENSOR
           IF(ILOADP(6,NL)== SENSOR_TAB(K)%SENS_ID) ISENS=K
         ENDDO
         IF(ISENS==0)THEN
            TS=TT
         ELSE                        
            TS = TT-SENSOR_TAB(ISENS)%TSTART
         ENDIF
         DO I = 1,ISIZ_SEG
           N1=LLOADP(ILOADP(4,NL)+4*(I-1))
           N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
           N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
           N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3)
           IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
           NP0 = NP0 + 1
           IF(TS < ZERO) CYCLE     
           NSKIN = NSKIN0+ IMAPSKP(NP0)
C        
           AA = ZERO
           VEL = ZERO
           PVEL=ZERO
C------  ----------  
C        
           IF(N4/=0 .AND. N1/=N2 .AND. N1/=N3 .AND. N1/=N4 .AND.
     .                      N2/=N3 .AND. N2/=N4 .AND. N3/=N4 )THEN
C        
             K1=3*DIR_HSP-2
             K2=3*DIR_HSP-1
             K3=3*DIR_HSP
             ! hydrostatic pressure
             IF(FUN_HSP /=0)THEN
                COORMEAN = (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))/FOUR)+
     .                     (XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))/FOUR)+
     .                     (XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))/FOUR)
                AA       = FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX)
             ENDIF
             NX= (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
             NY= (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
             NZ= (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
             NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
             AA = AA * HALF * NORM
C vel pressure
             K1=3*DIR_VEL-2
             K2=3*DIR_VEL-1
             K3=3*DIR_VEL
c         
             NSIGN = (NX * XFRAME(K1,IFRA2) + 
     .                NY * XFRAME(K2,IFRA2) + 
     .                NZ * XFRAME(K3,IFRA2)) 
             IF(NSIGN/=ZERO) NSIGN = SIGN(ONE,NSIGN)
C         
             VSEG= (XFRAME(K1,IFRA2)*
     .               (V(1,N1) + V(1,N2) + V(1,N3) + V(1,N4)) /FOUR)+
     .              (XFRAME(K2,IFRA2)*
     .               (V(2,N1) + V(2,N2) + V(2,N3) + V(2,N4)) /FOUR)+
     .              (XFRAME(K3,IFRA2)*
     .               (V(3,N1) + V(3,N2) + V(3,N3) + V(3,N4)) /FOUR)
             
             IF(FUN_VEL /=0)THEN
                VEL =  FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX)- VSEG
             ELSE
                VEL =  - VSEG
             ENDIF
             IF(FUN_CX /=0)   
     .         PVEL = (  (-(NX/NORM)*VEL*XFRAME(K1,IFRA2)-
     .                 (NY/NORM)*VEL*XFRAME(K2,IFRA2)-
     .                 (NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2  )* FCY1*
     .              FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/TWO
C         
           ELSE
            IF(N1 == N2)THEN
              N2 = N3
              N3 = N4
              N4 = 0
            ELSEIF(N1 == N3)THEN
              N3 = N4
              N4 = 0
            ELSEIF(N1 == N4)THEN
              N4 = 0
            ELSEIF(N2 == N3)THEN
              N3 = N4
              N4 = 0
            ELSEIF(N2 == N4)THEN
              N2 = N3
              N3 = N4
              N4 = 0
            ELSEIF(N3 == N4)THEN
              N4 = 0
            ENDIF
            IF (N4==0) N4=N3
C           true triangles.
            IF(FUN_HSP /=0)THEN
               K1=3*DIR_HSP-2
               K2=3*DIR_HSP-1
               K3=3*DIR_HSP
               ! hydrostatic pressure
               COORMEAN = (XFRAME(K1,IFRA1)*(X(1,N1)+X(1,N2)+X(1,N3))/THREE)+
     .                    (XFRAME(K2,IFRA1)*(X(2,N1)+X(2,N2)+X(2,N3))/THREE)+
     .                    (XFRAME(K3,IFRA1)*(X(3,N1)+X(3,N2)+X(3,N3))/THREE)
               AA       =  FCY*FINTER(FUN_HSP,(COORMEAN-XFRAME(9+DIR_HSP,IFRA1))*FCX,NPC,TF,DYDX)
            ENDIF
            NX= (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
            NY= (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
            NZ= (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
            NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
            AA = AA * HALF * NORM
C vel pressure
             K1=3*DIR_VEL-2
             K2=3*DIR_VEL-1
             K3=3*DIR_VEL
c           
             NSIGN = (NX * XFRAME(K1,IFRA2) + 
     .                NY * XFRAME(K2,IFRA2) + 
     .                NZ * XFRAME(K3,IFRA2)) 
             IF(NSIGN/=ZERO) NSIGN = SIGN(ONE,NSIGN)
C           
             VSEG= (XFRAME(K1,IFRA2)*
     .               (V(1,N1) + V(1,N2) + V(1,N3)) /THREE)+
     .              (XFRAME(K2,IFRA2)*
     .               (V(2,N1) + V(2,N2) + V(2,N3)) /THREE)+
     .              (XFRAME(K3,IFRA2)*
     .               (V(3,N1) + V(3,N2) + V(3,N3)) /THREE)
             
             IF(FUN_VEL /=0)THEN
                VEL =  FCY2*FINTER(FUN_VEL,TT*FCX2,NPC,TF,DYDX)- VSEG
             ELSE
                VEL =  - VSEG
             ENDIF
             IF(FUN_CX /=0)   
     .         PVEL = (  (-(NX/NORM)*VEL*XFRAME(K1,IFRA2)-
     .                 (NY/NORM)*VEL*XFRAME(K2,IFRA2)-
     .                 (NZ/NORM)*VEL*XFRAME(K3,IFRA2))**2  )* FCY1*
     .              FINTER(FUN_CX,TT*FCX1,NPC,TF,DYDX)/TWO
           ENDIF
              AA=-AA+PVEL*NSIGN
              SKIN_VECTOR(3,NSKIN)=SKIN_VECTOR(3,NSKIN) + AA
         END DO 
       END DO 
C---------pblast     
       DO NL=1+NLOADP_F,NLOADP_F+PBLAST%NLOADP_B
C--------default zero        
         ISIZ_SEG  = ILOADP(1,NL)/4 
         IAD = ILOADP(4,NL)
         DO N=1, ISIZ_SEG
           N1 = LLOADP(IAD+4*(N-1))
           N2 = LLOADP(IAD+4*(N-1)+1)
           N3 = LLOADP(IAD+4*(N-1)+2)
           N4 = LLOADP(IAD+4*(N-1)+3)
           IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
           NSKIN = NSKIN0+ IMAPSKP(NP0+N)
           IF (NODAL_IPART(N1)>0) THEN
               IF (H3D_PART(NODAL_IPART(N1))==1) IS_WRITTEN_SKIN(NSKIN)=1
           END IF
         ENDDO
         IL             = NL-NLOADP_F
         TDET           = FAC(01,NL)
         ID             = ILOADP(08,NL) !user_id
         !---------------------------------------------
         !   LOOP ON SEGMENTS (4N or 3N)
         !---------------------------------------------      
         DO I = 1,ISIZ_SEG
           N1=LLOADP(ILOADP(4,NL)+4*(I-1))
           N2=LLOADP(ILOADP(4,NL)+4*(I-1)+1)
           N3=LLOADP(ILOADP(4,NL)+4*(I-1)+2)
           N4=LLOADP(ILOADP(4,NL)+4*(I-1)+3) 
           IF (N1==0.OR.N2==0.OR.N3==0) CYCLE           
           NP0 = NP0 + 1
           IF(TT<TDET)CYCLE
           NSKIN = NSKIN0+ IMAPSKP(NP0)
           P = PBLAST%PBLAST_TAB(IL)%PRES(I)                                                           
           AA= -P
           SKIN_VECTOR(3,NSKIN)=SKIN_VECTOR(3,NSKIN) + AA
         ENDDO
       END DO 
C---------/LOAD/PCYL     
       DO NL=1,LOADS%NLOAD_CYL
C--------default zero        
         ISIZ_SEG  = LOADS%LOAD_CYL(NL)%NSEG 
         DO N=1, ISIZ_SEG
           N1 = LOADS%LOAD_CYL(NL)%SEGNOD(N,1)
           N2 = LOADS%LOAD_CYL(NL)%SEGNOD(N,2)
           N3 = LOADS%LOAD_CYL(NL)%SEGNOD(N,3)
           N4 = LOADS%LOAD_CYL(NL)%SEGNOD(N,4)
           IF (N1==0.OR.N2==0.OR.N3==0) CYCLE
           NSKIN = NSKIN0+ IMAPSKP(NP0+N)
           IF (NODAL_IPART(N1)>0) THEN
              IF (H3D_PART(NODAL_IPART(N1))==1) IS_WRITTEN_SKIN(NSKIN)=1
           END IF
         ENDDO
         ISENS = LOADS%LOAD_CYL(NL)%ISENS
         IIOUT = 0
         IF (ISENS > 0) THEN
           IF (SENSOR_TAB(ISENS)%STATUS == 0) THEN
              NP0 = NP0 + ISIZ_SEG
              CYCLE
           END IF
         END IF
         IFRA = LOADS%LOAD_CYL(NL)%IFRAME + 1
         XFACR= LOADS%LOAD_CYL(NL)%XSCALE_R
         XFACT= LOADS%LOAD_CYL(NL)%XSCALE_T
         YFAC = LOADS%LOAD_CYL(NL)%YSCALE
         IFUN = LOADS%LOAD_CYL(NL)%ITABLE
         NDIM = TABLE(IFUN)%NDIM
         NPOINT = SIZE(TABLE(IFUN)%X(1)%VALUES)
         RMAX = TABLE(IFUN)%X(1)%VALUES(NPOINT)
         M1   = IFRAME(1,IFRA)
         M2   = IFRAME(2,IFRA)
         DIRX = X(1,M1) - X(1,M2)
         DIRY = X(2,M1) - X(2,M2)
         DIRZ = X(3,M1) - X(3,M2)
         LEN  = SQRT(DIRX**2 + DIRY**2 + DIRZ**2)
         ! SEGP beam axis
         DIR(1) = DIRX / LEN
         DIR(2) = DIRY / LEN
         DIR(3) = DIRZ / LEN
         P0(1)  = X(1,M2)
         P0(2)  = X(2,M2)
         P0(3)  = X(3,M2)
         !---------------------------------------------
         !   LOOP ON SEGMENTS (4N or 3N)
         !---------------------------------------------      
         DO N = 1,ISIZ_SEG
           N1 = LOADS%LOAD_CYL(NL)%SEGNOD(N,1)
           N2 = LOADS%LOAD_CYL(NL)%SEGNOD(N,2)
           N3 = LOADS%LOAD_CYL(NL)%SEGNOD(N,3)
           N4 = LOADS%LOAD_CYL(NL)%SEGNOD(N,4)
           PRESS = ZERO
           A(1) = X(1,N1)
           A(2) = X(2,N1)
           A(3) = X(3,N1)
           B(1) = X(1,N2)
           B(2) = X(2,N2)
           B(3) = X(3,N2)
           C(1) = X(1,N3)
           C(2) = X(2,N3)
           C(3) = X(3,N3)
           NP0 = NP0 + 1
           IF (N4 == 0) THEN    ! 3 node segment
             CALL PRESS_SEG3(A       ,B      ,C      ,P0      ,DIR    , 
     .                       IFUN    ,TABLE  ,XFACR  ,XFACT   ,SEGP   )
             PRESS = ABS(SEGP) * YFAC
c          
           ELSE                ! 4 node segment
             D(1) = X(1,N4)
             D(2) = X(2,N4)
             D(3) = X(3,N4)
             M(1) = (X(1,N1) + X(1,N2) + X(1,N3) + X(1,N4)) * FOURTH
             M(2) = (X(2,N1) + X(2,N2) + X(2,N3) + X(2,N4)) * FOURTH
             M(3) = (X(3,N1) + X(3,N2) + X(3,N3) + X(3,N4)) * FOURTH
c            1st internal triangle 
             CALL PRESS_SEG3(A       ,B      ,M      ,P0      ,DIR    , 
     .                       IFUN    ,TABLE  ,XFACR  ,XFACT   ,SEGP   )
             PRESS = PRESS + SEGP * FOURTH
c            2nd internal triangle 
             CALL PRESS_SEG3(B       ,C      ,M      ,P0      ,DIR    , 
     .                       IFUN    ,TABLE  ,XFACR  ,XFACT   ,SEGP   )
             PRESS = PRESS + SEGP * FOURTH
c            3rd internal triangle 
             CALL PRESS_SEG3(C       ,D      ,M      ,P0      ,DIR    , 
     .                       IFUN    ,TABLE  ,XFACR  ,XFACT   ,SEGP   )
             PRESS = PRESS + SEGP * FOURTH
c            4th internal triangle 
             CALL PRESS_SEG3(D       ,A      ,M      ,P0      ,DIR    , 
     .                       IFUN    ,TABLE  ,XFACR  ,XFACT   ,SEGP   )
             PRESS = PRESS + SEGP * FOURTH
             PRESS = ABS(PRESS) * YFAC
           END IF  !  seg 4 node
           NSKIN = NSKIN0+ IMAPSKP(NP0)
           SKIN_VECTOR(3,NSKIN)=SKIN_VECTOR(3,NSKIN) +PRESS
         ENDDO!next N
       END DO 
C
      RETURN
      END
