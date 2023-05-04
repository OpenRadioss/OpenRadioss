Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  VOLPREP                       source/airbag/volpresp.F      
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE VOLPREP(IVOLU  ,RVOLU   ,NJET    ,IBAGJET ,RBAGJET   ,
     2                   NSENSOR,SENSOR_TAB,X       ,V       ,A         ,
     3                   N      ,NPC     ,TF      ,NN      ,SURF_NODES,
     4                   IADMV  ,FSKY    ,FSKYV   ,FEXT    ,H3D_DATA  ,
     5                   SURF_ELTYP,SURF_ELEM)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD 
      USE SENSOR_MOD
C-----------------------------------------------
C     AIRBAGS INPUT FORMAT 4.4
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER IVOLU(*),NJET,IBAGJET(NIBJET,*),
     .        NPC(*),NN,IADMV(4,*),SURF_NODES(NN,4),SURF_ELTYP(NN),
     .        SURF_ELEM(NN)
C     REAL
      my_real
     .   RVOLU(*),RBAGJET(NRBJET,*),
     .   X(3,*), V(3,*), A(3,*), N(3,*),TF(*),
     .   FSKY(8,LSKY),FSKYV(LSKY,8),FEXT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
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
C-----------------------------------------------
      API=HUNDRED80/PI
C-----------------------------------------------
      PRES   =RVOLU(12)
      Q      =RVOLU(23)
      PEXT   =RVOLU(3)
      DP     =Q+PRES-PEXT
      SCALT  =RVOLU(26)   
      SCALA  =RVOLU(29)   
      SCALD  =RVOLU(30)   
C cas vectoriel : ne pas oublier le cas scalaire
      IF (IVECTOR==1) THEN
       DO I=1,NN
C
        N1 = SURF_NODES(I,1)
        N2 = SURF_NODES(I,2)
        N3 = SURF_NODES(I,3)
        IF(SURF_ELTYP(I)/=7) N4 = SURF_NODES(I,4)
C
        IF(SURF_ELTYP(I)==3)THEN
          P=DP*FOURTH
          II=SURF_ELEM(I)
C
          FX=P*N(1,II)
          FY=P*N(2,II)
          FZ=P*N(3,II)
C
          FSKYV(IADMV(1,I),1)=FX
          FSKYV(IADMV(1,I),2)=FY
          FSKYV(IADMV(1,I),3)=FZ
C
          FSKYV(IADMV(2,I),1)=FX
          FSKYV(IADMV(2,I),2)=FY
          FSKYV(IADMV(2,I),3)=FZ
C
          FSKYV(IADMV(3,I),1)=FX
          FSKYV(IADMV(3,I),2)=FY
          FSKYV(IADMV(3,I),3)=FZ
C
          FSKYV(IADMV(4,I),1)=FX
          FSKYV(IADMV(4,I),2)=FY
          FSKYV(IADMV(4,I),3)=FZ
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
          P=DP*THIRD
          II=SURF_ELEM(I) + NUMELC
C
          FX=P*N(1,II)
          FY=P*N(2,II)
          FZ=P*N(3,II)
C
          FSKYV(IADMV(1,I),1)=FX
          FSKYV(IADMV(1,I),2)=FY
          FSKYV(IADMV(1,I),3)=FZ
C
          FSKYV(IADMV(2,I),1)=FX
          FSKYV(IADMV(2,I),2)=FY
          FSKYV(IADMV(2,I),3)=FZ
C
          FSKYV(IADMV(3,I),1)=FX
          FSKYV(IADMV(3,I),2)=FY
          FSKYV(IADMV(3,I),3)=FZ
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
C
          FSKYV(IADMV(1,I),1)=FX
          FSKYV(IADMV(1,I),2)=FY
          FSKYV(IADMV(1,I),3)=FZ
C
          FSKYV(IADMV(2,I),1)=FX
          FSKYV(IADMV(2,I),2)=FY
          FSKYV(IADMV(2,I),3)=FZ
C
          FSKYV(IADMV(3,I),1)=FX
          FSKYV(IADMV(3,I),2)=FY
          FSKYV(IADMV(3,I),3)=FZ
C
          FSKYV(IADMV(4,I),1)=FX
          FSKYV(IADMV(4,I),2)=FY
          FSKYV(IADMV(4,I),3)=FZ
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
      ELSE
C scalaire
       DO I=1,NN
C
        N1 = SURF_NODES(I,1)
        N2 = SURF_NODES(I,2)
        N3 = SURF_NODES(I,3)
        IF(SURF_ELTYP(I)/=7) N4 = SURF_NODES(I,4)
C
        IF(SURF_ELTYP(I)==3)THEN
          P=DP*FOURTH
          II=SURF_ELEM(I)
C
          FX=P*N(1,II)
          FY=P*N(2,II)
          FZ=P*N(3,II)
C
          FSKY(1,IADMV(1,I))=FX
          FSKY(2,IADMV(1,I))=FY
          FSKY(3,IADMV(1,I))=FZ
C
          FSKY(1,IADMV(2,I))=FX
          FSKY(2,IADMV(2,I))=FY
          FSKY(3,IADMV(2,I))=FZ
C
          FSKY(1,IADMV(3,I))=FX
          FSKY(2,IADMV(3,I))=FY
          FSKY(3,IADMV(3,I))=FZ
C
          FSKY(1,IADMV(4,I))=FX
          FSKY(2,IADMV(4,I))=FY
          FSKY(3,IADMV(4,I))=FZ
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
          P=DP*THIRD
          II=SURF_ELEM(I) + NUMELC
C
          FX=P*N(1,II)
          FY=P*N(2,II)
          FZ=P*N(3,II)
C
          FSKY(1,IADMV(1,I))=FX
          FSKY(2,IADMV(1,I))=FY
          FSKY(3,IADMV(1,I))=FZ
C
          FSKY(1,IADMV(2,I))=FX
          FSKY(2,IADMV(2,I))=FY
          FSKY(3,IADMV(2,I))=FZ
C
          FSKY(1,IADMV(3,I))=FX
          FSKY(2,IADMV(3,I))=FY
          FSKY(3,IADMV(3,I))=FZ
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
C
          FSKY(1,IADMV(1,I))=FX
          FSKY(2,IADMV(1,I))=FY
          FSKY(3,IADMV(1,I))=FZ
C
          FSKY(1,IADMV(2,I))=FX
          FSKY(2,IADMV(2,I))=FY
          FSKY(3,IADMV(2,I))=FZ
C
          FSKY(1,IADMV(3,I))=FX
          FSKY(2,IADMV(3,I))=FY
          FSKY(3,IADMV(3,I))=FZ
C
          FSKY(1,IADMV(4,I))=FX
          FSKY(2,IADMV(4,I))=FY
          FSKY(3,IADMV(4,I))=FZ
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
      ENDIF
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
         TSTART=SENSOR_TAB(ISENS)%TSTART
       ENDIF
C
       IF(NJ1/=0.AND.TT>=TSTART)THEN
        TS=TT-TSTART
        PJ0=FPT*FINTER(IPT,TS*SCALT,NPC,TF,DYDX)
C vecteur
        IF (IVECTOR==1) THEN
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
          FSKYV(IADMV(1,I),1)=FSKYV(IADMV(1,I),1)+FX
          FSKYV(IADMV(1,I),2)=FSKYV(IADMV(1,I),2)+FY
          FSKYV(IADMV(1,I),3)=FSKYV(IADMV(1,I),3)+FZ
C
          FSKYV(IADMV(2,I),1)=FSKYV(IADMV(2,I),1)+FX
          FSKYV(IADMV(2,I),2)=FSKYV(IADMV(2,I),2)+FY
          FSKYV(IADMV(2,I),3)=FSKYV(IADMV(2,I),3)+FZ
C
          FSKYV(IADMV(3,I),1)=FSKYV(IADMV(3,I),1)+FX
          FSKYV(IADMV(3,I),2)=FSKYV(IADMV(3,I),2)+FY
          FSKYV(IADMV(3,I),3)=FSKYV(IADMV(3,I),3)+FZ
C
          FSKYV(IADMV(4,I),1)=FSKYV(IADMV(4,I),1)+FX
          FSKYV(IADMV(4,I),2)=FSKYV(IADMV(4,I),2)+FY
          FSKYV(IADMV(4,I),3)=FSKYV(IADMV(4,I),3)+FZ
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
          XX = (X(1,N1)+X(1,N2)+X(1,N3))*THIRD
          YY = (X(2,N1)+X(2,N2)+X(2,N3))*THIRD
          ZZ = (X(3,N1)+X(3,N2)+X(3,N3))*THIRD
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
          FSKYV(IADMV(1,I),1)=FSKYV(IADMV(1,I),1)+FX
          FSKYV(IADMV(1,I),2)=FSKYV(IADMV(1,I),2)+FY
          FSKYV(IADMV(1,I),3)=FSKYV(IADMV(1,I),3)+FZ
C
          FSKYV(IADMV(2,I),1)=FSKYV(IADMV(2,I),1)+FX
          FSKYV(IADMV(2,I),2)=FSKYV(IADMV(2,I),2)+FY
          FSKYV(IADMV(2,I),3)=FSKYV(IADMV(2,I),3)+FZ
C
          FSKYV(IADMV(3,I),1)=FSKYV(IADMV(3,I),1)+FX
          FSKYV(IADMV(3,I),2)=FSKYV(IADMV(3,I),2)+FY
          FSKYV(IADMV(3,I),3)=FSKYV(IADMV(3,I),3)+FZ
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
          FSKYV(IADMV(1,I),1)=FSKYV(IADMV(1,I),1)+FX
          FSKYV(IADMV(1,I),2)=FSKYV(IADMV(1,I),2)+FY
          FSKYV(IADMV(1,I),3)=FSKYV(IADMV(1,I),3)+FZ
C
          FSKYV(IADMV(2,I),1)=FSKYV(IADMV(2,I),1)+FX
          FSKYV(IADMV(2,I),2)=FSKYV(IADMV(2,I),2)+FY
          FSKYV(IADMV(2,I),3)=FSKYV(IADMV(2,I),3)+FZ
C
          FSKYV(IADMV(3,I),1)=FSKYV(IADMV(3,I),1)+FX
          FSKYV(IADMV(3,I),2)=FSKYV(IADMV(3,I),2)+FY
          FSKYV(IADMV(3,I),3)=FSKYV(IADMV(3,I),3)+FZ
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
          TFEXT=TFEXT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                    +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                    +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
         ENDIF
         ENDDO
C scalaire
        ELSE
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
          FSKY(1,IADMV(1,I))=FSKY(1,IADMV(1,I))+FX
          FSKY(2,IADMV(1,I))=FSKY(2,IADMV(1,I))+FY
          FSKY(3,IADMV(1,I))=FSKY(3,IADMV(1,I))+FZ
C
          FSKY(1,IADMV(2,I))=FSKY(1,IADMV(2,I))+FX
          FSKY(2,IADMV(2,I))=FSKY(2,IADMV(2,I))+FY
          FSKY(3,IADMV(2,I))=FSKY(3,IADMV(2,I))+FZ
C
          FSKY(1,IADMV(3,I))=FSKY(1,IADMV(3,I))+FX
          FSKY(2,IADMV(3,I))=FSKY(2,IADMV(3,I))+FY
          FSKY(3,IADMV(3,I))=FSKY(3,IADMV(3,I))+FZ
C
          FSKY(1,IADMV(4,I))=FSKY(1,IADMV(4,I))+FX
          FSKY(2,IADMV(4,I))=FSKY(2,IADMV(4,I))+FY
          FSKY(3,IADMV(4,I))=FSKY(3,IADMV(4,I))+FZ
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
          XX = (X(1,N1)+X(1,N2)+X(1,N3))*THIRD
          YY = (X(2,N1)+X(2,N2)+X(2,N3))*THIRD
          ZZ = (X(3,N1)+X(3,N2)+X(3,N3))*THIRD
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
          FSKY(1,IADMV(1,I))=FSKY(1,IADMV(1,I))+FX
          FSKY(2,IADMV(1,I))=FSKY(2,IADMV(1,I))+FY
          FSKY(3,IADMV(1,I))=FSKY(3,IADMV(1,I))+FZ
C
          FSKY(1,IADMV(2,I))=FSKY(1,IADMV(2,I))+FX
          FSKY(2,IADMV(2,I))=FSKY(2,IADMV(2,I))+FY
          FSKY(3,IADMV(2,I))=FSKY(3,IADMV(2,I))+FZ
C
          FSKY(1,IADMV(3,I))=FSKY(1,IADMV(3,I))+FX
          FSKY(2,IADMV(3,I))=FSKY(2,IADMV(3,I))+FY
          FSKY(3,IADMV(3,I))=FSKY(3,IADMV(3,I))+FZ
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
          FSKY(1,IADMV(1,I))=FSKY(1,IADMV(1,I))+FX
          FSKY(2,IADMV(1,I))=FSKY(2,IADMV(1,I))+FY
          FSKY(3,IADMV(1,I))=FSKY(3,IADMV(1,I))+FZ
C
          FSKY(1,IADMV(2,I))=FSKY(1,IADMV(2,I))+FX
          FSKY(2,IADMV(2,I))=FSKY(2,IADMV(2,I))+FY
          FSKY(3,IADMV(2,I))=FSKY(3,IADMV(2,I))+FZ
C
          FSKY(1,IADMV(3,I))=FSKY(1,IADMV(3,I))+FX
          FSKY(2,IADMV(3,I))=FSKY(2,IADMV(3,I))+FY
          FSKY(3,IADMV(3,I))=FSKY(3,IADMV(3,I))+FZ
C
          FSKY(1,IADMV(4,I))=FSKY(1,IADMV(4,I))+FX
          FSKY(2,IADMV(4,I))=FSKY(2,IADMV(4,I))+FY
          FSKY(3,IADMV(4,I))=FSKY(3,IADMV(4,I))+FZ
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
        ENDIF
C
       ENDIF
      ENDDO
C
      RETURN
      END
