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
Chd|  LOAD_PRESSURE                 source/loads/general/load_pressure/load_pressure.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TH_SURF_MOD                   ../common_source/modules/interfaces/th_surf_mod.F
Chd|====================================================================
      SUBROUTINE LOAD_PRESSURE (ILOADP    ,LOADP     ,LLOADP   ,NPC     ,TF        ,
     2                          A         ,V         ,X       ,SKEW     ,SENSOR_TAB,
     3                          IADC      ,FSKY      ,FEXT    ,TAGNCONT ,NSENSOR   ,
     4                          LOADP_HYD_INTER  , H3D_DATA ,
     5                          NPRESLOAD  ,LOADP_TAGDEL,TH_SURF,FSAVSURF,NSEG_LOADP)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD
      USE SENSOR_MOD
      USE TH_SURF_MOD , ONLY : TH_SURF_
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "parit_c.inc"
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
      INTEGER, INTENT(IN) :: NPRESLOAD,NSENSOR
      INTEGER NPC(*),LLOADP(SLLOADP)
      INTEGER ILOADP(SIZLOADP,*),
     .        TAGNCONT(NLOADP_HYD_INTER,NUMNOD),
     .        LOADP_HYD_INTER(NLOADP_HYD)
      INTEGER  IADC(*) 
      my_real
     .   LOADP(SLOADP,*), TF(*), A(3,*), V(3,*), 
     .   X(3,*), SKEW(LSKEW,*),FSKY(8,LSKY), FEXT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      INTEGER, INTENT(IN) :: LOADP_TAGDEL(NPRESLOAD)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
      TYPE (TH_SURF_) , INTENT(IN) :: TH_SURF
      my_real, INTENT(INOUT) :: FSAVSURF(5,NSURF)
      INTEGER, INTENT(INOUT) :: NSEG_LOADP(NSURF)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1, ISK, N2, N3, N4, ISENS,
     .        IAD ,NP ,IFUNC ,NPRES ,NINTERP ,IDIR ,INORM ,NPL ,IANIM,IFLOAD,
     .        SEGCONT ,N ,IADN , IJK ,UP_BOUND,TAGN1,TAGN2,TAGN3,TAGN4,NUMPRESLOAD,
     .        IDEL, NSEGPL, NS, KSURF,NIDXLOAD
      my_real
     .   NX, NY, NZ,  AA, FX, FY, FZ, DYDX, TS,
     .   TFEXTT,F1, FCX,FCY,NORM,AREA 
      my_real FINTER
      EXTERNAL FINTER
C=======================================================================
C---------------------------------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------------------------------
C     This subroutine is applying pressure load to
C      a defined surface on dynamic zones which is not in contact
C---------------------------------------------------------------------
      TFEXTT = ZERO

      IANIM  = ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .         ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT
C
      NUMPRESLOAD = 0
      NSEGPL = TH_SURF%NSEGLOADPF+TH_SURF%NSEGLOADPB
      NIDXLOAD = NLOADP_F+NLOADP_B
      IF(IPARIT==0) THEN
C code SPMD Parith/OFF or SMP 
       DO  NP= 1,NLOADP_HYD

           NPRES = ILOADP(1,NIDXLOAD+NP)
           IFUNC = ILOADP(3,NIDXLOAD+NP)
           IAD = ILOADP(4,NIDXLOAD+NP)
           NINTERP = ILOADP(5,NIDXLOAD+NP)
           IDIR = ILOADP(6,NIDXLOAD+NP)   
           ISENS =  ILOADP(7,NIDXLOAD+NP) 
           ISK = ILOADP(8,NIDXLOAD+NP)    
           INORM = ILOADP(9,NIDXLOAD+NP)
           IFLOAD = ILOADP(10,NIDXLOAD+NP)
           FCY = LOADP(1,NIDXLOAD+NP)
           FCX = LOADP(2,NIDXLOAD+NP)
C
           IF(ISENS==0)THEN
              TS=TT
           ELSE
              TS = TT-SENSOR_TAB(ISENS)%TSTART
              IF(TS<ZERO)CYCLE
           ENDIF    


           F1 = FINTER(IFUNC,TS*FCX,NPC,TF,DYDX)
           AA = FCY*F1  

           DO N=1, NPRES/4
C
              N1 = LLOADP(IAD+4*(N-1))
              N2 = LLOADP(IAD+4*(N-1)+1)
              N3 = LLOADP(IAD+4*(N-1)+2)
              N4 = LLOADP(IAD+4*(N-1)+3)
              NUMPRESLOAD = NUMPRESLOAD + 1
              NSEGPL = NSEGPL + 1

              IDEL = LOADP_TAGDEL(NUMPRESLOAD)

              IF(IDEL > 0 ) CYCLE  ! SEGMENT DELETED

C----------------
C       Check if segment is in contact 
C----------------
             SEGCONT = 0

             TAGN1 = 0
             TAGN2 = 0
             TAGN3 = 0
             TAGN4 = 0       

             IF(NINTERP > 0 ) THEN           
                NPL = LOADP_HYD_INTER(NP)
                IF(N4/=0) THEN
                   SEGCONT = TAGNCONT(NPL,N1) + TAGNCONT(NPL,N2) +
     .                     TAGNCONT(NPL,N3)+TAGNCONT(NPL,N4)
                   IF(SEGCONT >= 2 .AND.IFLOAD==1) THEN
                      SEGCONT = 1
                   ELSEIF(SEGCONT <= 1.AND.IFLOAD==2) THEN
                      SEGCONT = 1
                   ELSE
                      SEGCONT = 0   
                   ENDIF
                ELSE
                   SEGCONT = TAGNCONT(NPL,N1) + TAGNCONT(NPL,N2) +
     .                     TAGNCONT(NPL,N3)
                   IF(SEGCONT >= 2 .AND.IFLOAD==1) THEN
                      SEGCONT = 1
                   ELSEIF(SEGCONT <= 1.AND.IFLOAD==2) THEN
                      SEGCONT = 1
                   ELSE
                      SEGCONT = 0
                   ENDIF
                ENDIF    
             ENDIF

             IF(SEGCONT == 0) THEN

               IF(N4/=0)THEN

                  NX = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
                  NY = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
                  NZ = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
                  IF(INORM == 1) THEN
                     FX = AA*NX*ONE_OVER_8
                     FY = AA*NY*ONE_OVER_8
                     FZ = AA*NZ*ONE_OVER_8
                  ELSEIF(INORM==2) THEN
                     NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
                     IF(ISK == 0) THEN
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_8*NORM
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_8*NORM
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_8*NORM
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ELSE 
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_8*NORM*SKEW(1,ISK)
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_8*NORM*SKEW(2,ISK)
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_8*NORM*SKEW(3,ISK)
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ENDIF
                  ELSEIF(INORM==3) THEN
   
                     IF(ISK == 0) THEN
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_8*NX
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_8*NY
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_8*NZ
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ELSE 
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_8*SKEW(1,ISK)*NX
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_8*SKEW(2,ISK)*NY
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_8*SKEW(3,ISK)*NZ
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ENDIF
                  ENDIF
C  
                  IF(TAGN1 == 0) THEN
                    A(1,N1)=A(1,N1)+FX
                    A(2,N1)=A(2,N1)+FY
                    A(3,N1)=A(3,N1)+FZ
                    IF(IANIM >0 .AND.IMPL_S==0) THEN
                       FEXT(1,N1) = FEXT(1,N1)+FX
                       FEXT(2,N1) = FEXT(2,N1)+FY
                       FEXT(3,N1) = FEXT(3,N1)+FZ
                    ENDIF 
                  ENDIF
C
                  IF(TAGN2 == 0) THEN
                     A(1,N2)=A(1,N2)+FX
                     A(2,N2)=A(2,N2)+FY
                     A(3,N2)=A(3,N2)+FZ
                     IF(IANIM >0 .AND.IMPL_S==0) THEN
                        FEXT(1,N2) = FEXT(1,N2)+FX
                        FEXT(2,N2) = FEXT(2,N2)+FY
                        FEXT(3,N2) = FEXT(3,N2)+FZ
                     ENDIF 
                  ENDIF
C
                  IF(TAGN3 == 0) THEN                  
                    A(1,N3)=A(1,N3)+FX
                    A(2,N3)=A(2,N3)+FY
                    A(3,N3)=A(3,N3)+FZ
                    IF(IANIM >0 .AND.IMPL_S==0) THEN
                       FEXT(1,N3) = FEXT(1,N3)+FX
                       FEXT(2,N3) = FEXT(2,N3)+FY
                       FEXT(3,N3) = FEXT(3,N3)+FZ
                    ENDIF
                  ENDIF
C
                  IF(TAGN4 == 0) THEN
                     A(1,N4)=A(1,N4)+FX
                     A(2,N4)=A(2,N4)+FY
                     A(3,N4)=A(3,N4)+FZ
                     IF(IANIM >0 .AND.IMPL_S==0) THEN
                       FEXT(1,N4) = FEXT(1,N4)+FX
                       FEXT(2,N4) = FEXT(2,N4)+FY
                       FEXT(3,N4) = FEXT(3,N4)+FZ
                     ENDIF
                 ENDIF
C
                 IF(TH_SURF%LOADP_FLAG >0 ) THEN
                    AREA = HALF*SQRT(NX*NX+NY*NY+NZ*NZ)
                    DO NS=TH_SURF%LOADP_KSEGS(NSEGPL) +1,TH_SURF%LOADP_KSEGS(NSEGPL+1)
                       KSURF = TH_SURF%LOADP_SEGS(NS)
                       NSEG_LOADP(KSURF) = NSEG_LOADP(KSURF) + 1
                       FSAVSURF(4,KSURF)= FSAVSURF(4,KSURF) + AA
                       FSAVSURF(5,KSURF)= FSAVSURF(5,KSURF) + AREA
                    ENDDO
                  ENDIF
C
                  TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                         +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                         +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
C
               ELSE
                ! true triangles.
                  NX = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
                  NY = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
                  NZ = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))

                  IF(INORM == 1) THEN
                     FX = AA*NX*ONE_OVER_6
                     FY = AA*NY*ONE_OVER_6
                     FZ = AA*NZ*ONE_OVER_6
                  ELSEIF(INORM==2) THEN
                     NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
                     IF(ISK == 0) THEN
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_6*NORM
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_6*NORM
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_6*NORM
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ELSE 
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_6*NORM*SKEW(1,ISK)
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_6*NORM*SKEW(2,ISK)
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_6*NORM*SKEW(3,ISK)
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ENDIF
                  ELSEIF(INORM==3) THEN
   
                     IF(ISK == 0) THEN
                        IF(IDIR == 1 ) THEN
                           IF(NX /= ZERO) FX = AA*ONE_OVER_6*NX
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           IF(NY /= ZERO) FY = AA*ONE_OVER_6*NY
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           IF(NZ /= ZERO) FZ = AA*ONE_OVER_6*NZ
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ELSE 
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_6*SKEW(1,ISK)*NX
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_6*SKEW(2,ISK)*NY
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_6*SKEW(3,ISK)*NZ
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ENDIF
                  ENDIF
C  
                  IF(TAGN1 == 0) THEN
                    A(1,N1)=A(1,N1)+FX
                    A(2,N1)=A(2,N1)+FY
                    A(3,N1)=A(3,N1)+FZ
                    IF(IANIM >0 .AND.IMPL_S==0) THEN
                       FEXT(1,N1) = FEXT(1,N1)+FX
                       FEXT(2,N1) = FEXT(2,N1)+FY
                       FEXT(3,N1) = FEXT(3,N1)+FZ
                    ENDIF

                  ENDIF
C
                  IF(TAGN2 == 0) THEN
                     A(1,N2)=A(1,N2)+FX
                     A(2,N2)=A(2,N2)+FY
                     A(3,N2)=A(3,N2)+FZ
                     IF(IANIM >0 .AND.IMPL_S==0) THEN
                        FEXT(1,N2) = FEXT(1,N2)+FX
                        FEXT(2,N2) = FEXT(2,N2)+FY
                        FEXT(3,N2) = FEXT(3,N2)+FZ
                     ENDIF
                  ENDIF
C
                  IF(TAGN3 == 0) THEN                  
                    A(1,N3)=A(1,N3)+FX
                    A(2,N3)=A(2,N3)+FY
                    A(3,N3)=A(3,N3)+FZ
                    IF(IANIM >0 .AND.IMPL_S==0) THEN
                       FEXT(1,N3) = FEXT(1,N3)+FX
                       FEXT(2,N3) = FEXT(2,N3)+FY
                       FEXT(3,N3) = FEXT(3,N3)+FZ
                    ENDIF
                  ENDIF
C
                 IF(TH_SURF%LOADP_FLAG >0 ) THEN
                    AREA = HALF*SQRT(NX*NX+NY*NY+NZ*NZ)
                    DO NS=TH_SURF%LOADP_KSEGS(NSEGPL) +1,TH_SURF%LOADP_KSEGS(NSEGPL+1)
                       KSURF = TH_SURF%LOADP_SEGS(NS)
                       NSEG_LOADP(KSURF) = NSEG_LOADP(KSURF) + 1
                       FSAVSURF(4,KSURF)= FSAVSURF(4,KSURF) + AA
                       FSAVSURF(5,KSURF)= FSAVSURF(5,KSURF) + AREA
                    ENDDO
                  ENDIF
C
                  TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)) 
     +                              + FY*(V(2,N1)+V(2,N2)+V(2,N3))
     +                              + FZ*(V(3,N1)+V(3,N2)+V(3,N3)))
               ENDIF
C
          ENDIF
C
        ENDDO
C
       ENDDO
C
#include "atomic.inc"
       TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
C
      ELSE
C-------------------------
C code SPMD Parith/ON
C-------------------------

       DO  NP= 1,NLOADP_HYD

           NPRES = ILOADP(1,NIDXLOAD+NP)
           IFUNC = ILOADP(3,NIDXLOAD+NP)
           IAD = ILOADP(4,NIDXLOAD+NP)
           NINTERP = ILOADP(5,NIDXLOAD+NP)
           IDIR = ILOADP(6,NIDXLOAD+NP)   
           ISENS =  ILOADP(7,NIDXLOAD+NP) 
           ISK = ILOADP(8,NIDXLOAD+NP)    
           INORM = ILOADP(9,NIDXLOAD+NP)
           IFLOAD = ILOADP(10,NIDXLOAD+NP)
           FCY = LOADP(1,NIDXLOAD+NP)
           FCX = LOADP(2,NIDXLOAD+NP)
C
           IF(ISENS==0)THEN
              TS=TT
           ELSE
              TS = TT-SENSOR_TAB(ISENS)%TSTART
              IF(TS<ZERO)CYCLE
           ENDIF    

         ! -------------
         ! flush fsky array to 0.
          DO N = 1,NPRES/4
             N1=LLOADP(IAD+4*(N-1))
             N2=LLOADP(IAD+4*(N-1)+1)
             N3=LLOADP(IAD+4*(N-1)+2)
             N4=LLOADP(IAD+4*(N-1)+3)

             IF(N4/=0 .AND. N1/=N2 .AND. N1/=N3 .AND. N1/=N4 .AND.
     .                      N2/=N3 .AND. N2/=N4 .AND. N3/=N4 )THEN
                UP_BOUND=4
             ELSE
                UP_BOUND=3
             ENDIF
             DO IJK=1,UP_BOUND
                IADN = IADC(IAD + 4*(N-1)+(IJK-1))
                FSKY(1:3,IADN) = ZERO
             ENDDO
          ENDDO
          ! -------------


           F1 = FINTER(IFUNC,TS*FCX,NPC,TF,DYDX)
           AA = FCY*F1  

           DO N=1, NPRES/4
              N1 = LLOADP(IAD+4*(N-1))
              N2 = LLOADP(IAD+4*(N-1)+1)
              N3 = LLOADP(IAD+4*(N-1)+2)
              N4 = LLOADP(IAD+4*(N-1)+3)

              NUMPRESLOAD = NUMPRESLOAD + 1
              NSEGPL = NSEGPL + 1

              IDEL = LOADP_TAGDEL(NUMPRESLOAD)

              IF(IDEL > 0 ) CYCLE  ! SEGMENT DELETED

C----------------
C       Check if segment is in contact 
C----------------
             SEGCONT = 0

             TAGN1 = 0
             TAGN2 = 0
             TAGN3 = 0
             TAGN4 = 0 

             IF(NINTERP > 0 ) THEN           
                NPL = LOADP_HYD_INTER(NP)
                IF(N4/=0) THEN
                   SEGCONT = TAGNCONT(NPL,N1) + TAGNCONT(NPL,N2) +
     .                     TAGNCONT(NPL,N3)+TAGNCONT(NPL,N4)
                   IF(SEGCONT >= 2 .AND.IFLOAD==1) THEN
                      SEGCONT = 1
                   ELSEIF(SEGCONT <= 1.AND.IFLOAD==2) THEN
                      SEGCONT = 1
                   ELSE
                      SEGCONT = 0
                   ENDIF
                ELSE
                   SEGCONT = TAGNCONT(NPL,N1) + TAGNCONT(NPL,N2) +
     .                     TAGNCONT(NPL,N3)
                   IF(SEGCONT >= 2 .AND.IFLOAD==1) THEN
                      SEGCONT = 1
                   ELSEIF(SEGCONT <= 1.AND.IFLOAD==2) THEN
                      SEGCONT = 1
                   ELSE
                      SEGCONT = 0
                   ENDIF
                ENDIF    
             ENDIF
            
             IF(SEGCONT == 0) THEN       

               IF(N4/=0)THEN

                  NX = (X(2,N3)-X(2,N1))*(X(3,N4)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N4)-X(2,N2))
                  NY = (X(3,N3)-X(3,N1))*(X(1,N4)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N4)-X(3,N2))
                  NZ = (X(1,N3)-X(1,N1))*(X(2,N4)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N4)-X(1,N2))
                  IF(INORM == 1) THEN
                     FX = AA*NX*ONE_OVER_8
                     FY = AA*NY*ONE_OVER_8
                     FZ = AA*NZ*ONE_OVER_8
                  ELSEIF(INORM==2) THEN
                     NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
                     IF(ISK == 0) THEN
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_8*NORM
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_8*NORM
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_8*NORM
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ELSE 
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_8*NORM*SKEW(1,ISK)
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_8*NORM*SKEW(2,ISK)
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_8*NORM*SKEW(3,ISK)
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ENDIF
                  ELSEIF(INORM==3) THEN
   
                     IF(ISK == 0) THEN
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_8*NX
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_8*NY
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_8*NZ
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ELSE 
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_8*SKEW(1,ISK)*NX
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_8*SKEW(2,ISK)*NY
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_8*SKEW(3,ISK)*NZ
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ENDIF
                  ENDIF
C   
                  IF(TAGN1 == 0) THEN
                    IADN = IADC(IAD+4*(N-1))
                    FSKY(1,IADN) = FX
                    FSKY(2,IADN) = FY
                    FSKY(3,IADN) = FZ
                    IF(IANIM >0 .AND.IMPL_S==0) THEN
                       FEXT(1,N1) = FEXT(1,N1) + FX
                       FEXT(2,N1) = FEXT(2,N1) + FY
                       FEXT(3,N1) = FEXT(3,N1) + FZ
                    ENDIF
                  ENDIF
C
                  IF(TAGN2 == 0) THEN
                    IADN = IADC(IAD+4*(N-1)+1)
                    FSKY(1,IADN) = FX
                    FSKY(2,IADN) = FY
                    FSKY(3,IADN) = FZ
                    IF(IANIM >0 .AND.IMPL_S==0) THEN
                       FEXT(1,N2) = FEXT(1,N2) + FX
                       FEXT(2,N2) = FEXT(2,N2) + FY
                       FEXT(3,N2) = FEXT(3,N2) + FZ
                    ENDIF
                  ENDIF
C
                  IF(TAGN3 == 0) THEN
                    IADN = IADC(IAD+4*(N-1)+2)
                    FSKY(1,IADN) = FX
                    FSKY(2,IADN) = FY
                    FSKY(3,IADN) = FZ
                    IF(IANIM >0 .AND.IMPL_S==0) THEN
                       FEXT(1,N3) = FEXT(1,N3) + FX
                       FEXT(2,N3) = FEXT(2,N3) + FY
                       FEXT(3,N3) = FEXT(3,N3) + FZ
                    ENDIF 
                  ENDIF
C
                  IF(TAGN4 == 0) THEN
                    IADN = IADC(IAD+4*(N-1)+3)
                    FSKY(1,IADN) = FX
                    FSKY(2,IADN) = FY
                    FSKY(3,IADN) = FZ
                    IF(IANIM >0 .AND.IMPL_S==0) THEN
                       FEXT(1,N4) = FEXT(1,N4) + FX
                       FEXT(2,N4) = FEXT(2,N4) + FY
                       FEXT(3,N4) = FEXT(3,N4) + FZ
                    ENDIF
                  ENDIF
C
                 IF(TH_SURF%LOADP_FLAG >0 ) THEN
                    AREA = HALF*SQRT(NX*NX+NY*NY+NZ*NZ)
                    DO NS=TH_SURF%LOADP_KSEGS(NSEGPL) +1,TH_SURF%LOADP_KSEGS(NSEGPL+1)
                       KSURF = TH_SURF%LOADP_SEGS(NS)
                       NSEG_LOADP(KSURF) = NSEG_LOADP(KSURF) + 1
                       FSAVSURF(4,KSURF)= FSAVSURF(4,KSURF) + AA
                       FSAVSURF(5,KSURF)= FSAVSURF(5,KSURF) + AREA
                    ENDDO
                  ENDIF
                  TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)+V(1,N4))
     +                         +FY*(V(2,N1)+V(2,N2)+V(2,N3)+V(2,N4))
     +                         +FZ*(V(3,N1)+V(3,N2)+V(3,N3)+V(3,N4)))
C
               ELSE

                ! true triangles.
                  NX = (X(2,N3)-X(2,N1))*(X(3,N3)-X(3,N2)) - (X(3,N3)-X(3,N1))*(X(2,N3)-X(2,N2))
                  NY = (X(3,N3)-X(3,N1))*(X(1,N3)-X(1,N2)) - (X(1,N3)-X(1,N1))*(X(3,N3)-X(3,N2))
                  NZ = (X(1,N3)-X(1,N1))*(X(2,N3)-X(2,N2)) - (X(2,N3)-X(2,N1))*(X(1,N3)-X(1,N2))

                  IF(INORM == 1) THEN
                     FX = AA*NX*ONE_OVER_6
                     FY = AA*NY*ONE_OVER_6
                     FZ = AA*NZ*ONE_OVER_6
                  ELSEIF(INORM==2) THEN
                     NORM = SQRT(NX*NX+NY*NY+NZ*NZ)
                     IF(ISK == 0) THEN
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_6*NORM
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_6*NORM
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_6*NORM
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ELSE 
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_6*NORM*SKEW(1,ISK)
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_6*NORM*SKEW(2,ISK)
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_6*NORM*SKEW(3,ISK)
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ENDIF
                  ELSEIF(INORM==3) THEN
   
                     IF(ISK == 0) THEN
                        IF(IDIR == 1 ) THEN
                           IF(NX /= ZERO) FX = AA*ONE_OVER_6*NX
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           IF(NY /= ZERO) FY = AA*ONE_OVER_6*NY
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           IF(NZ /= ZERO) FZ = AA*ONE_OVER_6*NZ
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ELSE 
                        IF(IDIR == 1 ) THEN
                           FX = AA*ONE_OVER_6*SKEW(1,ISK)*NX
                           FY = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==2) THEN
                           FY = AA*ONE_OVER_6*SKEW(2,ISK)*NY
                           FX = ZERO
                           FZ = ZERO
                        ELSEIF(IDIR==3) THEN
                           FZ = AA*ONE_OVER_6*SKEW(3,ISK)*NZ
                           FX = ZERO
                           FY = ZERO
                        ENDIF
                     ENDIF
                  ENDIF
C  
                  IF(TAGN1 == 0) THEN
                    IADN = IADC(IAD+4*(N-1))
                    FSKY(1,IADN) = FX
                    FSKY(2,IADN) = FY
                    FSKY(3,IADN) = FZ
                    IF(IANIM >0 .AND.IMPL_S==0) THEN
                       FEXT(1,N1) = FEXT(1,N1) + FX
                       FEXT(2,N1) = FEXT(2,N1) + FY
                       FEXT(3,N1) = FEXT(3,N1) + FZ
                    ENDIF
                  ENDIF
C
                  IF(TAGN2 == 0) THEN
                    IADN = IADC(IAD+4*(N-1)+1)
                    FSKY(1,IADN) = FX
                    FSKY(2,IADN) = FY
                    FSKY(3,IADN) = FZ
                    IF(IANIM >0 .AND.IMPL_S==0) THEN
                       FEXT(1,N2) = FEXT(1,N2) + FX
                       FEXT(2,N2) = FEXT(2,N2) + FY
                       FEXT(3,N2) = FEXT(3,N2) + FZ
                    ENDIF
                  ENDIF
C
                  IF(TAGN3 == 0) THEN
                    IADN = IADC(IAD+4*(N-1)+2)
                    FSKY(1,IADN) = FX
                    FSKY(2,IADN) = FY
                    FSKY(3,IADN) = FZ
                    IF(IANIM >0 .AND.IMPL_S==0) THEN
                       FEXT(1,N3) = FEXT(1,N3) + FX
                       FEXT(2,N3) = FEXT(2,N3) + FY
                       FEXT(3,N3) = FEXT(3,N3) + FZ
                    ENDIF
                  ENDIF

C
                 IF(TH_SURF%LOADP_FLAG >0 ) THEN
                    AREA = HALF*SQRT(NX*NX+NY*NY+NZ*NZ)
                    DO NS=TH_SURF%LOADP_KSEGS(NSEGPL) +1,TH_SURF%LOADP_KSEGS(NSEGPL+1)
                       KSURF = TH_SURF%LOADP_SEGS(NS)
                       NSEG_LOADP(KSURF) = NSEG_LOADP(KSURF) + 1
                       FSAVSURF(4,KSURF)= FSAVSURF(4,KSURF) + AA
                       FSAVSURF(5,KSURF)= FSAVSURF(5,KSURF) + AREA
                    ENDDO
                  ENDIF

                  TFEXTT=TFEXTT+DT1*(FX*(V(1,N1)+V(1,N2)+V(1,N3)) 
     +                              + FY*(V(2,N1)+V(2,N2)+V(2,N3))
     +                              + FZ*(V(3,N1)+V(3,N2)+V(3,N3)))

               ENDIF
C
          ENDIF
C
        ENDDO
C
       ENDDO
C
#include "atomic.inc"
        TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
C
      ENDIF
      RETURN
      END
