Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|====================================================================
Chd|  SMS_CJOINT_0                  source/ams/sms_cjoint.F       
Chd|-- called by -----------
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_TELESC_0                  source/ams/sms_cjoint.F       
Chd|        SPMD_SD_CJ_0                  source/mpi/kinematic_conditions/spmd_sd_cj_0.F
Chd|====================================================================
      SUBROUTINE SMS_CJOINT_0(A    ,AR    ,V ,VR,X    ,
     2                      FSAV   ,LJOINT,MS,IN,IADCJ,
     3                      FR_CJ  ,CJWORK,TAG_LNK_SMS,DIAG_SMS,ITASK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LJOINT(*), FR_CJ(*), IADCJ(NSPMD+1,*),
     .        TAG_LNK_SMS(*), ITASK
      my_real
     .   A(3,NUMNOD), AR(3,NUMNOD), V(3,NUMNOD), VR(3,NUMNOD), X(3,NUMNOD), FSAV(NTHVKI,*),
     .   MS(*), IN(*), CJWORK(18,*), DIAG_SMS(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, NN, KIND(NJOINT), ICSIZE
C-----------------------------------------------
C
      IF(ISPMD.EQ.0) THEN
        
        K=1
        DO N=1,NJOINT
          KIND(N) = K
          K=K+LJOINT(K)+1
        END DO

!$OMP DO
        DO N=1,NJOINT
          IF(TAG_LNK_SMS(N)==0) CYCLE
          K = KIND(N)
          NN=NINTER+NRWALL+NRBODY+NSECT+N
          CALL SMS_TELESC_0(A,AR,V,VR,X,FSAV(1,NN),LJOINT(K),MS,IN,
     .                      CJWORK(1,N),DIAG_SMS)
        END DO
!$OMP END DO

      ENDIF

      IF(NSPMD.GT.1)THEN
C
        CALL MY_BARRIER
C
        IF(ITASK==0)THEN
          ICSIZE=0
          DO N=1,NJOINT
            IF(TAG_LNK_SMS(N)/=0)
     .        ICSIZE=ICSIZE+IADCJ(NSPMD+1,N)-IADCJ(1,N)
          END DO
          CALL SPMD_SD_CJ_0(AR   ,V     ,VR         ,LJOINT,FR_CJ,
     2                      IADCJ,ICSIZE,TAG_LNK_SMS)
        END IF
      END IF

      RETURN
      END
Chd|====================================================================
Chd|  SMS_CJOINT_1                  source/ams/sms_cjoint.F       
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_TELESC_1                  source/ams/sms_cjoint.F       
Chd|        SPMD_SD_CJ_1                  source/mpi/kinematic_conditions/spmd_sd_cj_1.F
Chd|====================================================================
      SUBROUTINE SMS_CJOINT_1(A     ,MS         ,LJOINT ,IADCJ ,FR_CJ ,
     .                        CJWORK,IDOWN,TAG_LNK_SMS,ITASK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LJOINT(*), FR_CJ(*), IADCJ(NSPMD+1,*), IDOWN, 
     .        TAG_LNK_SMS(*), ITASK
C     REAL ou REAL*8
      my_real
     .   A(3,*), MS(*), CJWORK(18,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, KIND(NJOINT), ICSIZE
C-----------------------------------------------
C
      IF(ISPMD.EQ.0) THEN
        
        K=1
        DO N=1,NJOINT
          KIND(N) = K
          K=K+LJOINT(K)+1
        END DO

!$OMP DO
        DO N=1,NJOINT
          IF(TAG_LNK_SMS(N)==0) CYCLE
          K = KIND(N)
          CALL SMS_TELESC_1(A,MS,LJOINT(K),CJWORK(1,N),IDOWN)
        END DO
!$OMP END DO

      ENDIF

      IF(NSPMD.GT.1)THEN
C
        CALL MY_BARRIER
C
        IF(ITASK==0)THEN
          ICSIZE=0
          DO N=1,NJOINT
            IF(TAG_LNK_SMS(N)/=0)
     .        ICSIZE=ICSIZE+IADCJ(NSPMD+1,N)-IADCJ(1,N)
          END DO
          CALL SPMD_SD_CJ_1(A,LJOINT,FR_CJ,IADCJ,ICSIZE,TAG_LNK_SMS)
        END IF
      END IF

      RETURN
      END
Chd|====================================================================
Chd|  SMS_CJOINT_2                  source/ams/sms_cjoint.F       
Chd|-- called by -----------
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_TELESC_2                  source/ams/sms_cjoint.F       
Chd|        SPMD_SD_CJ_1                  source/mpi/kinematic_conditions/spmd_sd_cj_1.F
Chd|====================================================================
      SUBROUTINE SMS_CJOINT_2(A    ,AR    ,V ,VR,X    ,
     2                      LJOINT,MS,IN,IADCJ,FR_CJ,
     3                      CJWORK,TAG_LNK_SMS,ITASK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LJOINT(*), FR_CJ(*), IADCJ(NSPMD+1,*),
     .        TAG_LNK_SMS(*), ITASK
C     REAL ou REAL*8
      my_real
     .   A(3,*), AR(3,*), V(3,*), VR(3,*), X(3,*), 
     .   MS(*), IN(*), CJWORK(18,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, KIND(NJOINT), ICSIZE
C-----------------------------------------------
C
      IF(ISPMD.EQ.0) THEN
        
        K=1
        DO N=1,NJOINT
          KIND(N) = K
          K=K+LJOINT(K)+1
        END DO

!$OMP DO
        DO N=1,NJOINT
          IF(TAG_LNK_SMS(N)==0) CYCLE
          K = KIND(N)
          CALL SMS_TELESC_2(A,AR,V,VR,X,LJOINT(K),MS,IN,
     .                      CJWORK(1,N))
        END DO
!$OMP END DO

      ENDIF

      IF(NSPMD.GT.1)THEN
C
        CALL MY_BARRIER
C
        IF(ITASK==0)THEN
          ICSIZE=0
          DO N=1,NJOINT
            IF(TAG_LNK_SMS(N)/=0)
     .        ICSIZE=ICSIZE+IADCJ(NSPMD+1,N)-IADCJ(1,N)
          END DO
          CALL SPMD_SD_CJ_1(A,LJOINT,FR_CJ,IADCJ,ICSIZE,TAG_LNK_SMS)
        END IF
      END IF

      RETURN
      END
Chd|====================================================================
Chd|  SMS_TELESC_0                  source/ams/sms_cjoint.F       
Chd|-- called by -----------
Chd|        SMS_CJOINT_0                  source/ams/sms_cjoint.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_TELESC_0(A,AR,V,VR,X,FS,NOD,MS,IN,
     .                        CJWORK,DIAG_SMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(0:*), IFLAG
C     REAL ou REAL*8
      my_real
     .   A(3,*), AR(3,*), V(3,*), VR(3,*), X(3,*), FS(*), MS(*),
     .   IN(*), CJWORK(*), DIAG_SMS(*)
CMasParINCLUDE 'telesc.intmap.inc'
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSN, NA, NB, I, N
C     REAL ou REAL*8
      my_real
     .   MASSE, INER, N1, N2, N3, S, AX, AY, AZ, AXX, AYY, AZZ, VX,
     .   VY, VZ, VXX, VYY, VZZ, XCDG, YCDG, ZCDG, XX, YY, ZZ, RR, A0,
     .   DMASSE, VG(3), USDT, V0, DT05
C-----------------------------------------------
      NSN =NOD(0)
C----------------------------
C     DIRECTION LIBRE
C----------------------------
      NA=NOD(1)
      NB=NOD(2)
      N1=X(1,NB)-X(1,NA)
      N2=X(2,NB)-X(2,NA)
      N3=X(3,NB)-X(3,NA)
      S=SQRT(N1**2+N2**2+N3**2)
      N1=N1/S
      N2=N2/S
      N3=N3/S
C
      MASSE=ZERO
      INER=ZERO
C
      AX= ZERO
      AY= ZERO
      AZ= ZERO
C
      AXX= ZERO
      AYY= ZERO
      AZZ= ZERO
C
      XCDG=ZERO
      YCDG=ZERO
      ZCDG=ZERO      
C----------------------------
C     CALCUL DU CDG + MASSE
C----------------------------
      DO 100 I=1,NSN
      N = NOD(I)
      MASSE= MASSE+MS(N)
      XCDG=XCDG+X(1,N)*MS(N)
      YCDG=YCDG+X(2,N)*MS(N)
      ZCDG=ZCDG+X(3,N)*MS(N)
  100 CONTINUE
C
      IF (MASSE.GT.ZERO) THEN
        XCDG=XCDG/MASSE
        YCDG=YCDG/MASSE
        ZCDG=ZCDG/MASSE
      ENDIF
C----------------------------
C     CALCUL MOMENTS,INERTIE(PTS ALIGNES SUR N)
C----------------------------
      DO I=1,NSN
      N = NOD(I)
C
      XX=X(1,N)-XCDG
      YY=X(2,N)-YCDG
      ZZ=X(3,N)-ZCDG
C
      RR=N1*XX+N2*YY+N3*ZZ
      XX=N1*RR
      YY=N2*RR
      ZZ=N3*RR
C
      INER=INER+RR**2*DIAG_SMS(N)+IN(N)
C     INER=INER+RR**2*MS(N)+IN(N)
C
C     Forces
      AX= AX+A(1,N)
      AY= AY+A(2,N)
      AZ= AZ+A(3,N)
C
C     Moments
      AXX= AXX+AR(1,N)+YY*A(3,N)-ZZ*A(2,N)
      AYY= AYY+AR(2,N)+ZZ*A(1,N)-XX*A(3,N)
      AZZ= AZZ+AR(3,N)+XX*A(2,N)-YY*A(1,N)
C
      END DO
C
      A0=N1*AX+N2*AY+N3*AZ
      AX=AX-N1*A0
      AY=AY-N2*A0
      AZ=AZ-N3*A0
      A0=N1*AXX+N2*AYY+N3*AZZ
      AXX=AXX-N1*A0
      AYY=AYY-N2*A0
      AZZ=AZZ-N3*A0
C
      FS(1)=FS(1)+AX*DT12
      FS(2)=FS(2)+AY*DT12
      FS(3)=FS(3)+AZ*DT12
      FS(4)=FS(4)+AXX*DT12
      FS(5)=FS(5)+AYY*DT12
      FS(6)=FS(6)+AZZ*DT12
C
      IF (INER.GT.ZERO) THEN
        AXX=AXX/INER
        AYY=AYY/INER
        AZZ=AZZ/INER
      ENDIF
C----------------------------
      CJWORK(1)=N1
      CJWORK(2)=N2
      CJWORK(3)=N3
C
      CJWORK(4)=AXX 
      CJWORK(5)=AYY 
      CJWORK(6)=AZZ 
C
      CJWORK(7)=XCDG
      CJWORK(8)=YCDG
      CJWORK(9)=ZCDG      
C
      CJWORK(10)=MASSE
      CJWORK(11)=INER
C----------------------------
C     mvt de corps rigide autour de CDG (masse MASSE, inertie INER):
C     Extract V(t-1/2) and VR(t-1/2)
C----------------------------
      VX= ZERO
      VY= ZERO
      VZ= ZERO
C
      VXX= ZERO
      VYY= ZERO
      VZZ= ZERO
C
      DO I=1,NSN
      N = NOD(I)
C
      VX= VX+V(1,N)*MS(N)
      VY= VY+V(2,N)*MS(N)
      VZ= VZ+V(3,N)*MS(N)
C
      END DO
C
      A0=N1*VX+N2*VY+N3*VZ
      VX=VX-N1*A0
      VY=VY-N2*A0
      VZ=VZ-N3*A0
C
      IF (MASSE.GT.ZERO) THEN
	VX=VX/MASSE
	VY=VY/MASSE
	VZ=VZ/MASSE
      ENDIF
C
      DT05=HALF*DT1
      DO I=1,NSN
      N = NOD(I)
C
      XX=X(1,N)-XCDG-(V(1,N)-VX)*DT05
      YY=X(2,N)-YCDG-(V(2,N)-VY)*DT05
      ZZ=X(3,N)-ZCDG-(V(3,N)-VZ)*DT05
C
      RR=N1*XX+N2*YY+N3*ZZ
      XX=N1*RR
      YY=N2*RR
      ZZ=N3*RR
C
      VXX= VXX+VR(1,N)*IN(N)+YY*V(3,N)*MS(N)-ZZ*V(2,N)*MS(N)
      VYY= VYY+VR(2,N)*IN(N)+ZZ*V(1,N)*MS(N)-XX*V(3,N)*MS(N)
      VZZ= VZZ+VR(3,N)*IN(N)+XX*V(2,N)*MS(N)-YY*V(1,N)*MS(N)
C
      END DO
C
      A0=N1*VXX+N2*VYY+N3*VZZ
      VXX=VXX-N1*A0
      VYY=VYY-N2*A0
      VZZ=VZZ-N3*A0
C
      IF (INER.GT.ZERO) THEN
	VXX=VXX/INER
	VYY=VYY/INER
	VZZ=VZZ/INER
      ENDIF
C
C store sum(diag_i)
      DMASSE=ZERO
      DO I=1,NSN
      N = NOD(I)
      DMASSE= DMASSE+DIAG_SMS(N)
      END DO
C
      CJWORK(12)=VX
      CJWORK(13)=VY
      CJWORK(14)=VZ      
C
      CJWORK(15)=VXX
      CJWORK(16)=VYY
      CJWORK(17)=VZZ     
C
      CJWORK(18)=DMASSE
C----------------------------
C     CALCUL ACCELERATIONS DE ROTATION
C----------------------------
      VG(1)=VXX+AXX*DT12
      VG(2)=VYY+AYY*DT12
      VG(3)=VZZ+AZZ*DT12
C
      USDT = ONE/DT12
C
      DO I=1,NSN
      N = NOD(I)
C
      A0=N1*AR(1,N)+N2*AR(2,N)+N3*AR(3,N)
      V0=N1*VR(1,N)+N2*VR(2,N)+N3*VR(3,N)
      AR(1,N)= IN(N)*(VG(1)-(VR(1,N)-N1*V0)) * USDT + N1*A0
      AR(2,N)= IN(N)*(VG(2)-(VR(2,N)-N2*V0)) * USDT + N2*A0
      AR(3,N)= IN(N)*(VG(3)-(VR(3,N)-N3*V0)) * USDT + N3*A0
C
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_TELESC_1                  source/ams/sms_cjoint.F       
Chd|-- called by -----------
Chd|        SMS_CJOINT_1                  source/ams/sms_cjoint.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_TELESC_1(A,DMS,NOD,CJWORK,IDOWN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(0:*), IDOWN
C     REAL ou REAL*8
      my_real
     .   A(3,*), DMS(*), CJWORK(*)
CMasParINCLUDE 'telesc.intmap.inc'
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSN, NA, NB, I, N
C     REAL ou REAL*8
      my_real
     .   AX, AY, AZ, A0, N1, N2, N3, DMASSE
C-----------------------------------------------
      NSN =NOD(0)
C----------------------------
      N1=CJWORK(1)
      N2=CJWORK(2)
      N3=CJWORK(3)
      DMASSE=CJWORK(18)
C----------------------------
      SELECT CASE(IDOWN)

      CASE(0)
C----------------------------
C     Remontee
C----------------------------
C      
      AX= ZERO
      AY= ZERO
      AZ= ZERO
C
C     MONTE FORCES(PTS ALIGNES SUR N)
      DO I=1,NSN
        N = NOD(I)
C
        AX= AX+A(1,N)
        AY= AY+A(2,N)
        AZ= AZ+A(3,N)
C
      END DO
C
      A0=N1*AX+N2*AY+N3*AZ
      AX=AX-N1*A0
      AY=AY-N2*A0
      AZ=AZ-N3*A0
C
C transmet force au 1er nd
      N = NOD(1)
      A0=N1*A(1,N)+N2*A(2,N)+N3*A(3,N)
      A(1,N)=AX+N1*A0
      A(2,N)=AY+N2*A0
      A(3,N)=AZ+N3*A0
      DO I=2,NSN
        N = NOD(I)
        A0=N1*A(1,N)+N2*A(2,N)+N3*A(3,N)
        A(1,N)=N1*A0
        A(2,N)=N2*A0
        A(3,N)=N3*A0
      END DO
C----------------------------
C     Redescente
C----------------------------
C
      CASE(1)
C

      N = NOD(1)
C
      A0=N1*A(1,N)+N2*A(2,N)+N3*A(3,N)
      AX= DMS(N)*(A(1,N)-N1*A0)
      AY= DMS(N)*(A(2,N)-N2*A0)
      AZ= DMS(N)*(A(3,N)-N3*A0)
C
      IF (DMASSE.GT.ZERO) THEN
        AX=AX/DMASSE
        AY=AY/DMASSE
        AZ=AZ/DMASSE
      ENDIF
C
      DO I=1,NSN
        N = NOD(I)
C
        A0=N1*A(1,N)+N2*A(2,N)+N3*A(3,N)
        A(1,N)=AX+N1*A0
        A(2,N)=AY+N2*A0
        A(3,N)=AZ+N3*A0
      END DO

      END SELECT 

      RETURN
      END
Chd|====================================================================
Chd|  SMS_TELESC_2                  source/ams/sms_cjoint.F       
Chd|-- called by -----------
Chd|        SMS_CJOINT_2                  source/ams/sms_cjoint.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_TELESC_2(A,AR,V,VR,X,NOD,MS,IN,
     .                        CJWORK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(0:*)
C     REAL ou REAL*8
      my_real
     .   A(3,*), AR(3,*), V(3,*), VR(3,*), X(3,*), MS(*),
     .   IN(*), CJWORK(*)
CMasParINCLUDE 'telesc.intmap.inc'
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSN, NA, NB, I, N
C     REAL ou REAL*8
      my_real
     .   N1, N2, N3, AX, AY, AZ, AXX, AYY, AZZ, 
     .   XCDG, YCDG, ZCDG, XX, YY, ZZ, RR, A0, 
     .   VX, VY, VZ, VXX, VYY, VZZ, V0,
     .   VG(3), V1X2, V2X1, V2X3, V3X2, V3X1, V1X3, USDT, VX1, VX2, VX3
C-----------------------------------------------
      NSN =NOD(0)
C----------------------------
      N1=CJWORK(1)
      N2=CJWORK(2)
      N3=CJWORK(3)
C
      AXX= CJWORK(4)
      AYY= CJWORK(5)
      AZZ= CJWORK(6)
C
      XCDG=CJWORK(7)
      YCDG=CJWORK(8)
      ZCDG=CJWORK(9)     
C
      VX=CJWORK(12)
      VY=CJWORK(13)
      VZ=CJWORK(14)	 
C
      VXX=CJWORK(15)
      VYY=CJWORK(16)
      VZZ=CJWORK(17)	 
C
      VG(1)=VXX+AXX*DT12
      VG(2)=VYY+AYY*DT12
      VG(3)=VZZ+AZZ*DT12
C----------------------------
      N = NOD(1)
      AX=A(1,N)
      AY=A(2,N)
      AZ=A(3,N)
C
      A0=N1*A(1,N)+N2*A(2,N)+N3*A(3,N)
      AX=AX-N1*A0
      AY=AY-N2*A0
      AZ=AZ-N3*A0
C----------------------------
C     CALCUL ACCELERATIONS
C----------------------------
      USDT = ONE/DT12
C
      DO I=1,NSN
      N = NOD(I)
C
      XX=X(1,N)-XCDG
      YY=X(2,N)-YCDG
      ZZ=X(3,N)-ZCDG
C
      RR=N1*XX+N2*YY+N3*ZZ
      XX=N1*RR
      YY=N2*RR
      ZZ=N3*RR
C
      V1X2=VG(1)*YY
      V2X1=VG(2)*XX
      V2X3=VG(2)*ZZ
      V3X2=VG(3)*YY
      V3X1=VG(3)*XX
      V1X3=VG(1)*ZZ
C
      VX1=V2X3-V3X2
      VX2=V3X1-V1X3
      VX3=V1X2-V2X1
C
      A0=N1*A(1,N)+N2*A(2,N)+N3*A(3,N)
      V0=N1*V(1,N)+N2*V(2,N)+N3*V(3,N)
      A(1,N)=AX
     .    +(VX+VX1+HALF*DT2*(VG(2)*VX3-VG(3)*VX2)-(V(1,N)-N1*V0))*USDT
     .    +N1*A0
      A(2,N)=AY
     .    +(VY+VX2+HALF*DT2*(VG(3)*VX1-VG(1)*VX3)-(V(2,N)-N2*V0))*USDT
     .    +N2*A0
      A(3,N)=AZ
     .    +(VZ+VX3+HALF*DT2*(VG(1)*VX2-VG(2)*VX1)-(V(3,N)-N3*V0))*USDT
     .    +N3*A0
C
      END DO
C
      RETURN
      END
