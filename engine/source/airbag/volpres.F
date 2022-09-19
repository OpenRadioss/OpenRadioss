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
Chd|  VOLPRE                        source/airbag/volpres.F       
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE VOLPRE(IVOLU ,RVOLU   ,NJET    ,IBAGJET ,RBAGJET  ,
     2                LSENSOR ,SENSOR  ,X       ,V       ,A        ,
     3                N       ,NPC     ,TF      ,NN      ,SURF_NODES,
     4                FEXT    ,H3D_DATA,SURF_ELTYP,SURF_ELEM)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IVOLU(*),NJET,IBAGJET(NIBJET,*),LSENSOR(NSENSI,*),
     .        NPC(*),NN,SURF_NODES(NN,4),SURF_ELTYP(NN),SURF_ELEM(NN)
C     REAL
      my_real
     .   RVOLU(*),RBAGJET(NRBJET,*),SENSOR(NSENSR,*),
     .   X(3,*), V(3,*), A(3,*), N(3,*),TF(*),FEXT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,NJ1,NJ2,NJ3,IPT,IPA,IPZ,
     .        N1,N2,N3,N4,IJ,ISENS,K,
     .        ITYP
C     REAL
      my_real
     .   PRES,FX,FY,FZ,P,PEXT,DP,Q,PJ,PJ0,DYDX,XX,YY,ZZ,
     .   THETA,NX1,NX2,NY1,NY2,NZ1,NZ2,AN1,AN,
     .   XM,YM,ZM,NX3,NY3,NZ3,
     .   PS1,PS2,PS3,AN2,AN3,DET,MU,FACR,
     .   API,DIST,TSTART,TS,TMPO,FPT,FPA,FPZ,SCALT,SCALA,SCALD
      my_real
     .   FINTER
C=======================================================================
      API=HUNDRED80/PI
C-----------------------------------------------
      PRES   =RVOLU(12)
      Q      =RVOLU(23)
      PEXT   =RVOLU(3)
      SCALT  =RVOLU(26)
      SCALA  =RVOLU(29)
      SCALD  =RVOLU(30)
      DP     =Q+PRES-PEXT
      DO I=1,NN
C
        IF(SURF_ELTYP(I)==3)THEN
          P=DP*FOURTH
          II=SURF_ELEM(I)
C
          FX=P*N(1,II)
          FY=P*N(2,II)
          FZ=P*N(3,II)
!
          N1 = SURF_NODES(I,1)
          N2 = SURF_NODES(I,2)
          N3 = SURF_NODES(I,3)
          N4 = SURF_NODES(I,4)
C
          A(1,N1)=A(1,N1)+FX
          A(2,N1)=A(2,N1)+FY
          A(3,N1)=A(3,N1)+FZ
C
          A(1,N2)=A(1,N2)+FX
          A(2,N2)=A(2,N2)+FY
          A(3,N2)=A(3,N2)+FZ
C
          A(1,N3)=A(1,N3)+FX
          A(2,N3)=A(2,N3)+FY
          A(3,N3)=A(3,N3)+FZ
C
          A(1,N4)=A(1,N4)+FX
          A(2,N4)=A(2,N4)+FY
          A(3,N4)=A(3,N4)+FZ
          IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .       ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
            FEXT(1,N1) = FEXT(1,N1)+FX
            FEXT(2,N1) = FEXT(2,N1)+FY
            FEXT(3,N1) = FEXT(3,N1)+FZ
            FEXT(1,N2) = FEXT(1,N2)+FX
            FEXT(2,N2) = FEXT(2,N2)+FY
            FEXT(3,N2) = FEXT(3,N2)+FZ
            FEXT(1,N3) = FEXT(1,N3)+FX
            FEXT(2,N3) = FEXT(2,N3)+FY
            FEXT(3,N3) = FEXT(3,N3)+FZ
            FEXT(1,N4) = FEXT(1,N4)+FX
            FEXT(2,N4) = FEXT(2,N4)+FY
            FEXT(3,N4) = FEXT(3,N4)+FZ
          ENDIF
        ELSEIF(SURF_ELTYP(I)==7)THEN
          P=DP/3.
          II=SURF_ELEM(I) + NUMELC
C
          FX=P*N(1,II)
          FY=P*N(2,II)
          FZ=P*N(3,II)
!
          N1 = SURF_NODES(I,1)
          N2 = SURF_NODES(I,2)
          N3 = SURF_NODES(I,3)
C
          A(1,N1)=A(1,N1)+FX
          A(2,N1)=A(2,N1)+FY
          A(3,N1)=A(3,N1)+FZ
C
          A(1,N2)=A(1,N2)+FX
          A(2,N2)=A(2,N2)+FY
          A(3,N2)=A(3,N2)+FZ
C
          A(1,N3)=A(1,N3)+FX
          A(2,N3)=A(2,N3)+FY
          A(3,N3)=A(3,N3)+FZ
          IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .       ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
            FEXT(1,N1) = FEXT(1,N1)+FX
            FEXT(2,N1) = FEXT(2,N1)+FY
            FEXT(3,N1) = FEXT(3,N1)+FZ
            FEXT(1,N2) = FEXT(1,N2)+FX
            FEXT(2,N2) = FEXT(2,N2)+FY
            FEXT(3,N2) = FEXT(3,N2)+FZ
            FEXT(1,N3) = FEXT(1,N3)+FX
            FEXT(2,N3) = FEXT(2,N3)+FY
            FEXT(3,N3) = FEXT(3,N3)+FZ
          ENDIF
        ELSE
          P=DP*FOURTH
          II=I+NUMELC+NUMELTG
C
          FX=P*N(1,II)
          FY=P*N(2,II)
          FZ=P*N(3,II)
!
          N1 = SURF_NODES(I,1)
          N2 = SURF_NODES(I,2)
          N3 = SURF_NODES(I,3)
          N4 = SURF_NODES(I,4)
C
          A(1,N1)=A(1,N1)+FX
          A(2,N1)=A(2,N1)+FY
          A(3,N1)=A(3,N1)+FZ
C
          A(1,N2)=A(1,N2)+FX
          A(2,N2)=A(2,N2)+FY
          A(3,N2)=A(3,N2)+FZ
C
          A(1,N3)=A(1,N3)+FX
          A(2,N3)=A(2,N3)+FY
          A(3,N3)=A(3,N3)+FZ
C
          A(1,N4)=A(1,N4)+FX
          A(2,N4)=A(2,N4)+FY
          A(3,N4)=A(3,N4)+FZ
          IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .       ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
            FEXT(1,N1) = FEXT(1,N1)+FX
            FEXT(2,N1) = FEXT(2,N1)+FY
            FEXT(3,N1) = FEXT(3,N1)+FZ
            FEXT(1,N2) = FEXT(1,N2)+FX
            FEXT(2,N2) = FEXT(2,N2)+FY
            FEXT(3,N2) = FEXT(3,N2)+FZ
            FEXT(1,N3) = FEXT(1,N3)+FX
            FEXT(2,N3) = FEXT(2,N3)+FY
            FEXT(3,N3) = FEXT(3,N3)+FZ
            FEXT(1,N4) = FEXT(1,N4)+FX
            FEXT(2,N4) = FEXT(2,N4)+FY
            FEXT(3,N4) = FEXT(3,N4)+FZ
          ENDIF
        ENDIF
      ENDDO
C
      ITYP=IVOLU(2)
      IF(ITYP/=4.AND.ITYP/=5.AND.ITYP/=7.AND.ITYP/=9)RETURN
C-------------------------------------------
C     INJECTEURS
C-------------------------------------------
      DO IJ =1,NJET
       NJ1  = IBAGJET( 5,IJ)
       NJ2  = IBAGJET( 6,IJ)
       NJ3  = IBAGJET( 7,IJ)
       IPT  = IBAGJET( 8,IJ)
       IPA  = IBAGJET( 9,IJ)
       IPZ  = IBAGJET(10,IJ)
       FPT  = RBAGJET(12,IJ)
       FPA  = RBAGJET(13,IJ)
       FPZ  = RBAGJET(14,IJ)
C 
       ISENS=IBAGJET(4,IJ)
       IF(ISENS==0)THEN
         TSTART=ZERO
       ELSE
         TSTART=SENSOR(2,ISENS)
       ENDIF
C
       IF(NJ1/=0.AND.TT>=TSTART)THEN
        TS=TT-TSTART
        PJ0=FPT*FINTER(IPT,TS*SCALT,NPC,TF,DYDX)
        DO I=1,NN
        IF(SURF_ELTYP(I)==3)THEN
          PJ=FOURTH*PJ0
C
          N1 = SURF_NODES(I,1)
          N2 = SURF_NODES(I,2)
          N3 = SURF_NODES(I,3)
          N4 = SURF_NODES(I,4)
C
          II=SURF_ELEM(I)
C
          XX = FOURTH*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))
          YY = FOURTH*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))
          ZZ = FOURTH*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))	
C
          XM=HALF*(X(1,NJ1)+X(1,NJ3))
          YM=HALF*(X(2,NJ1)+X(2,NJ3))
          ZM=HALF*(X(3,NJ1)+X(3,NJ3))

C
          NX1 = XX-XM
          NY1 = YY-YM
          NZ1 = ZZ-ZM
C
C         decomposition de (M,P) sur (M,N2) et (M,N3)
          NX2 = X(1,NJ2)-XM
          NY2 = X(2,NJ2)-YM
          NZ2 = X(3,NJ2)-ZM
C
          NX3 = X(1,NJ3)-XM
          NY3 = X(2,NJ3)-YM
          NZ3 = X(3,NJ3)-ZM
C
          PS1 = NX1*NX2+NY1*NY2+NZ1*NZ2
          PS2 = NX2*NX3+NY2*NY3+NZ2*NZ3
          PS3 = NX1*NX3+NY1*NY3+NZ1*NZ3
          AN2 = NX2*NX2+NY2*NY2+NZ2*NZ2
          AN3 = NX3*NX3+NY3*NY3+NZ3*NZ3
          DET = PS2*PS2-AN2*AN3
C         LAMBDA = (PS3*PS2-PS1*AN3)/SIGN(MAX(EM30,ABS(DET)),DET)
          MU     = (PS2*PS1-PS3*AN2)/SIGN(MAX(EM30,ABS(DET)),DET)
C
          FACR =MIN(ONE,MAX(-ONE,MU))
          NX1=NX1-FACR*NX3
          NY1=NY1-FACR*NY3
          NZ1=NZ1-FACR*NZ3
C
          AN1 = (NX1**2+NY1**2+NZ1**2)
          AN = MAX(EM30,SQRT((N(1,II)**2+N(2,II)**2+N(3,II)**2)*AN1))
          PJ=PJ*MAX(ZERO,(NX1*N(1,II)+NY1*N(2,II)+NZ1*N(3,II))/AN)
          AN = MAX(EM30,SQRT((NX2**2+NY2**2+NZ2**2)*AN1))
          TMPO = (NX1*NX2+NY1*NY2+NZ1*NZ2)/AN
          TMPO = SIGN(MIN(ONE,ABS(TMPO)),TMPO)
          THETA=API*ACOS(TMPO)
          PJ=PJ*FPA*FINTER(IPA,THETA*SCALA,NPC,TF,DYDX)
          DIST = SQRT(AN1)
          IF(IPZ/=0)PJ=PJ*FPZ*FINTER(IPZ,DIST*SCALD,NPC,TF,DYDX)
C
          FX=PJ*N(1,II)
          FY=PJ*N(2,II)
          FZ=PJ*N(3,II)
C
          A(1,N1)=A(1,N1)+FX
          A(2,N1)=A(2,N1)+FY
          A(3,N1)=A(3,N1)+FZ
C
          A(1,N2)=A(1,N2)+FX
          A(2,N2)=A(2,N2)+FY
          A(3,N2)=A(3,N2)+FZ
C
          A(1,N3)=A(1,N3)+FX
          A(2,N3)=A(2,N3)+FY
          A(3,N3)=A(3,N3)+FZ
C
          A(1,N4)=A(1,N4)+FX
          A(2,N4)=A(2,N4)+FY
          A(3,N4)=A(3,N4)+FZ
C
          IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .       ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
            FEXT(1,N1) = FEXT(1,N1)+FX
            FEXT(2,N1) = FEXT(2,N1)+FY
            FEXT(3,N1) = FEXT(3,N1)+FZ
            FEXT(1,N2) = FEXT(1,N2)+FX
            FEXT(2,N2) = FEXT(2,N2)+FY
            FEXT(3,N2) = FEXT(3,N2)+FZ
            FEXT(1,N3) = FEXT(1,N3)+FX
            FEXT(2,N3) = FEXT(2,N3)+FY
            FEXT(3,N3) = FEXT(3,N3)+FZ
            FEXT(1,N4) = FEXT(1,N4)+FX
            FEXT(2,N4) = FEXT(2,N4)+FY
            FEXT(3,N4) = FEXT(3,N4)+FZ
          ENDIF
C
          TFEXT=TFEXT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                    +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                    +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
        ELSEIF(SURF_ELTYP(I)==7)THEN
          PJ=PJ0*THIRD
C
          N1 = SURF_NODES(I,1)
          N2 = SURF_NODES(I,2)
          N3 = SURF_NODES(I,3)
C
          II=SURF_ELEM(I) + NUMELC
C
          XX = (X(1,N1)+X(1,N2)+X(1,N3)) * THIRD
          YY = (X(2,N1)+X(2,N2)+X(2,N3)) * THIRD
          ZZ = (X(3,N1)+X(3,N2)+X(3,N3)) * THIRD	
C
          XM=0.5*(X(1,NJ1)+X(1,NJ3))
          YM=0.5*(X(2,NJ1)+X(2,NJ3))
          ZM=0.5*(X(3,NJ1)+X(3,NJ3))
C
          NX1 = XX-XM
          NY1 = YY-YM
          NZ1 = ZZ-ZM
C
C         decomposition de (M,P) sur (M,N2) et (M,N3)
          NX2 = X(1,NJ2)-XM
          NY2 = X(2,NJ2)-YM
          NZ2 = X(3,NJ2)-ZM
C
          NX3 = X(1,NJ3)-XM
          NY3 = X(2,NJ3)-YM
          NZ3 = X(3,NJ3)-ZM
C
          PS1 = NX1*NX2+NY1*NY2+NZ1*NZ2
          PS2 = NX2*NX3+NY2*NY3+NZ2*NZ3
          PS3 = NX1*NX3+NY1*NY3+NZ1*NZ3
          AN2 = NX2*NX2+NY2*NY2+NZ2*NZ2
          AN3 = NX3*NX3+NY3*NY3+NZ3*NZ3
          DET = PS2*PS2-AN2*AN3
C         LAMBDA = (PS3*PS2-PS1*AN3)/SIGN(MAX(EM30,ABS(DET)),DET)
          MU     = (PS2*PS1-PS3*AN2)/SIGN(MAX(EM30,ABS(DET)),DET)
C
          FACR =MIN(ONE,MAX(-ONE,MU))
          NX1=NX1-FACR*NX3
          NY1=NY1-FACR*NY3
          NZ1=NZ1-FACR*NZ3
C
          AN1 = (NX1**2+NY1**2+NZ1**2)
          AN = MAX(EM30,SQRT((N(1,II)**2+N(2,II)**2+N(3,II)**2)*AN1))
          PJ=PJ*MAX(ZERO,(NX1*N(1,II)+NY1*N(2,II)+NZ1*N(3,II))/AN)
          AN = MAX(EM30,SQRT((NX2**2+NY2**2+NZ2**2)*AN1))
          TMPO = (NX1*NX2+NY1*NY2+NZ1*NZ2)/AN
          TMPO = SIGN(MIN(ONE,ABS(TMPO)),TMPO)
          THETA=API*ACOS(TMPO)
          PJ=PJ*FPA*FINTER(IPA,THETA*SCALA,NPC,TF,DYDX)
          DIST = SQRT(AN1)
          IF(IPZ/=0)PJ=PJ*FPZ*FINTER(IPZ,DIST*SCALD,NPC,TF,DYDX)
C
          FX=PJ*N(1,II)
          FY=PJ*N(2,II)
          FZ=PJ*N(3,II)
C
          A(1,N1)=A(1,N1)+FX
          A(2,N1)=A(2,N1)+FY
          A(3,N1)=A(3,N1)+FZ
C
          A(1,N2)=A(1,N2)+FX
          A(2,N2)=A(2,N2)+FY
          A(3,N2)=A(3,N2)+FZ
C
          A(1,N3)=A(1,N3)+FX
          A(2,N3)=A(2,N3)+FY
          A(3,N3)=A(3,N3)+FZ
C
          IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .       ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
            FEXT(1,N1) = FEXT(1,N1)+FX
            FEXT(2,N1) = FEXT(2,N1)+FY
            FEXT(3,N1) = FEXT(3,N1)+FZ
            FEXT(1,N2) = FEXT(1,N2)+FX
            FEXT(2,N2) = FEXT(2,N2)+FY
            FEXT(3,N2) = FEXT(3,N2)+FZ
            FEXT(1,N3) = FEXT(1,N3)+FX
            FEXT(2,N3) = FEXT(2,N3)+FY
            FEXT(3,N3) = FEXT(3,N3)+FZ
          ENDIF
C
          TFEXT=TFEXT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3))
     +                    +FY*(V(2,N1)+V(2,N2)+V(2,N3))
     +                    +FZ*(V(3,N1)+V(3,N2)+V(3,N3)))
        ELSE
          PJ=FOURTH*PJ0
C
!          N1 = ISX(1,I)
!          N2 = ISX(2,I)
!          N3 = ISX(3,I)
!          N4 = ISX(4,I)
          N1 = SURF_NODES(I,1)
          N2 = SURF_NODES(I,2)
          N3 = SURF_NODES(I,3)
          N4 = SURF_NODES(I,4)
C
          II=I+NUMELC+NUMELTG
C
          XX = FOURTH*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))
          YY = FOURTH*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))
          ZZ = FOURTH*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))	
C
          XM=HALF*(X(1,NJ1)+X(1,NJ3))
          YM=HALF*(X(2,NJ1)+X(2,NJ3))
          ZM=HALF*(X(3,NJ1)+X(3,NJ3))	
C
          NX1 = XX-XM
          NY1 = YY-YM
          NZ1 = ZZ-ZM
C
C         decomposition de (M,P) sur (M,N2) et (M,N3)
          NX2 = X(1,NJ2)-XM
          NY2 = X(2,NJ2)-YM
          NZ2 = X(3,NJ2)-ZM
C
          NX3 = X(1,NJ3)-XM
          NY3 = X(2,NJ3)-YM
          NZ3 = X(3,NJ3)-ZM
C
          PS1 = NX1*NX2+NY1*NY2+NZ1*NZ2
          PS2 = NX2*NX3+NY2*NY3+NZ2*NZ3
          PS3 = NX1*NX3+NY1*NY3+NZ1*NZ3
          AN2 = NX2*NX2+NY2*NY2+NZ2*NZ2
          AN3 = NX3*NX3+NY3*NY3+NZ3*NZ3
          DET = PS2*PS2-AN2*AN3
C         LAMBDA = (PS3*PS2-PS1*AN3)/SIGN(MAX(EM30,ABS(DET)),DET)
          MU     = (PS2*PS1-PS3*AN2)/SIGN(MAX(EM30,ABS(DET)),DET)
C
          FACR =MIN(ONE,MAX(-ONE,MU))
          NX1=NX1-FACR*NX3
          NY1=NY1-FACR*NY3
          NZ1=NZ1-FACR*NZ3
C
          AN1 = (NX1**2+NY1**2+NZ1**2)
          AN = MAX(EM30,SQRT((N(1,II)**2+N(2,II)**2+N(3,II)**2)*AN1))
          PJ=PJ*MAX(ZERO,(NX1*N(1,II)+NY1*N(2,II)+NZ1*N(3,II))/AN)
          AN = MAX(EM30,SQRT((NX2**2+NY2**2+NZ2**2)*AN1))
          TMPO = (NX1*NX2+NY1*NY2+NZ1*NZ2)/AN
          TMPO = SIGN(MIN(ONE,ABS(TMPO)),TMPO)
          THETA=API*ACOS(TMPO)
          PJ=PJ*FPA*FINTER(IPA,THETA*SCALA,NPC,TF,DYDX)
          DIST = SQRT(AN1)
          IF(IPZ/=0)PJ=PJ*FPZ*FINTER(IPZ,DIST*SCALD,NPC,TF,DYDX)
C
          FX=PJ*N(1,II)
          FY=PJ*N(2,II)
          FZ=PJ*N(3,II)
C
          A(1,N1)=A(1,N1)+FX
          A(2,N1)=A(2,N1)+FY
          A(3,N1)=A(3,N1)+FZ
C
          A(1,N2)=A(1,N2)+FX
          A(2,N2)=A(2,N2)+FY
          A(3,N2)=A(3,N2)+FZ
C
          A(1,N3)=A(1,N3)+FX
          A(2,N3)=A(2,N3)+FY
          A(3,N3)=A(3,N3)+FZ
C
          A(1,N4)=A(1,N4)+FX
          A(2,N4)=A(2,N4)+FY
          A(3,N4)=A(3,N4)+FZ
C
          IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .       ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT >0) THEN
            FEXT(1,N1) = FEXT(1,N1)+FX
            FEXT(2,N1) = FEXT(2,N1)+FY
            FEXT(3,N1) = FEXT(3,N1)+FZ
            FEXT(1,N2) = FEXT(1,N2)+FX
            FEXT(2,N2) = FEXT(2,N2)+FY
            FEXT(3,N2) = FEXT(3,N2)+FZ
            FEXT(1,N3) = FEXT(1,N3)+FX
            FEXT(2,N3) = FEXT(2,N3)+FY
            FEXT(3,N3) = FEXT(3,N3)+FZ
            FEXT(1,N4) = FEXT(1,N4)+FX
            FEXT(2,N4) = FEXT(2,N4)+FY
            FEXT(3,N4) = FEXT(3,N4)+FZ
          ENDIF
C
          TFEXT=TFEXT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                    +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                    +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
        ENDIF
       ENDDO
C
       ENDIF
      ENDDO
C
      RETURN
      END
