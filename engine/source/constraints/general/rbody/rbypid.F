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
Chd|  RBYPID                        source/constraints/general/rbody/rbypid.F
Chd|-- called by -----------
Chd|        RBYONF                        source/constraints/general/rbody/rbyonf.F
Chd|        RBYSENS                       source/constraints/general/rbody/rbyonf.F
Chd|-- calls ---------------
Chd|        RBYACT                        source/constraints/general/rbody/rbyact.F
Chd|        SPMD_CHKW                     source/mpi/generic/spmd_chkw.F
Chd|        SPMD_EXCH_FR6                 source/mpi/kinematic_conditions/spmd_exch_fr6.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_PART_COM                 source/mpi/interfaces/spmd_th.F
Chd|        SPMD_WIOUT                    source/mpi/generic/spmd_wiout.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE RBYPID(IPARG   ,IPARI       ,MS    ,IN     ,
     .                  IXS     ,IXQ  ,IXC   ,IXT   ,IXP    ,
     .                  IXR     ,SKEW ,ITAB  ,ITABM1,ISKWN  ,
     .                  NPBY    ,ONOF ,ITAG  ,LPBY  ,
     .                  X       ,V    ,VR    ,RBY   ,
     .                  IXTG    ,NPBYI,RBYI  ,LPBYI ,IACTS  ,
     .                  FR_RBY2 ,NRB  ,ONFELT,WEIGHT,PARTSAV,
     .                  IPARTC  ,NSN  ,ELBUF_TAB,PRI_OFF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com07_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "cong1_c.inc"
#include      "cong2_c.inc"
#include      "scrfs_c.inc"
#include      "stati_c.inc"
#include      "statr_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), IPARI(*), IXS(NIXS,*), IXQ(NIXQ,*),
     .   IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*), IXR(NIXR,*),
     .   ITAB(*), ITABM1(*),IXTG(NIXTG,*),NRB, NSN,
     .   ISKWN(LISKN,*), NPBY(*),ITAG(*),LPBY(*),NPBYI(*) ,LPBYI(*),
     .   WEIGHT(*), FR_RBY2(3,*), IPARTC(*)
      INTEGER ONOF,IACTS, ONFELT, IWIOUT
      INTEGER, INTENT(IN) :: PRI_OFF
C     REAL
      my_real
     .   SKEW(LSKEW,*),MS(*),IN(*),PARTSAV(NPSAV,*),
     .   X(3,*),V(3,*),VR(3,*),RBY(*),RBYI(NRBY,*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, NG, ITY, NEL, NFT, IAD, IGOF, N, NI, LSKYRBKG,
     .   M, ISPH, NALL,MLW, K, PMAIN, TAG, L,
     .   MX,ICOMM(NSPMD+2),ISTRAIN,NPT,IHBE, ID
C     REAL
      my_real
     .   XMOM, YMOM, ZMOM,II1,II2,II3,II4,II5,II6,II7,II8,II9,
     .   IG1,IG2,IG3,IG4,IG5,IG6,IG7,IG8,IG9,
     .   XXMOM, YYMOM, ZZMOM, WA1, WA2, WA3, 
     .   TSUM(100),
     .   FSKYRBK(NSKYRBK0*10+1),
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN),
     .   F5(NSN), F6(NSN), OFF_OLD
      DOUBLE PRECISION RBF6(6,6)
      my_real,
     .   DIMENSION(:), POINTER :: OFFG
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C======================================================================|
      M    = NPBY(1)
C
      IF(NSPMD > 1)THEN
C FR_RBY2(3,NRB) => proc main ; ICOMM : Array of flag for necessary comm pmain => p
        PMAIN = ABS(FR_RBY2(3,NRB))
        TAG = 1
        IF(M < 0) TAG = 0
        CALL SPMD_PART_COM(TAG,PMAIN,ICOMM)
        IF(M < 0) GOTO 100
C for use of ICOMM in SPMD_EXCH_FR6
C FR_RBY2 can not be used directly
        ICOMM(NSPMD+1) = 0
        ICOMM(NSPMD+2) = PMAIN
      ELSE
        PMAIN = 1
      ENDIF
C
      ISPH = NPBY(5)
      ID = NPBY(6)
C
C-----------------------------------------------
      IF(ONOF == 0)THEN
C-----------------------
C     DEACTIVATION OF RB
C-----------------------
        IN(M) = RBY(13)
        MS(M) = RBY(15)
      ELSEIF(ONOF == 1)THEN
C-----------------------
C     REACTIVATION OF RB
C-----------------------
       IF(N2D==0) THEN
C        3D ANALYSIS
         XMOM = V(1,M)*MS(M)
         YMOM = V(2,M)*MS(M)
         ZMOM = V(3,M)*MS(M)
C
         XXMOM = VR(1,M)*IN(M)
         YYMOM = VR(2,M)*IN(M)
         ZZMOM = VR(3,M)*IN(M)
       ELSEIF(N2D==1) THEN
C        2D ANALYSIS : Axisymmetry
         XMOM = ZERO
         YMOM = V(2,M)*MS(M)
         ZMOM = V(3,M)*MS(M)
C
         XXMOM = ZERO
         YYMOM = ZERO
         ZZMOM = VR(3,M)*IN(M)   
       ELSEIF(N2D==2) THEN
C        2D ANALYSIS : Plane strain
         XMOM = ZERO
         YMOM = V(2,M)*MS(M)
         ZMOM = V(3,M)*MS(M)
C
         XXMOM = VR(1,M)*IN(M)
         YYMOM = ZERO
         ZZMOM = ZERO    
       ENDIF
C
       CALL RBYACT(RBY   ,M     ,LPBY  ,NSN   ,MS    ,
     .             IN    ,X     ,ITAB  ,SKEW  ,ISPH  ,
     .             ITAG(1+NUMNOD),NPBYI,RBYI  ,LPBYI ,
     .             PMAIN,ICOMM,WEIGHT,ID    )
C----------------------------------------------
C     MOMENTUM +
C     RESET OF MASSES AND INERTIAS OF SECNDS NODES
C----------------------------------------------
       IF(N2D==0) THEN
C       3D ANALYSIS
        DO I=1,NSN
          N = LPBY(I)
          IF(ITAG(NUMNOD+N) > 0.AND.WEIGHT(N) == 1)THEN
C main node of secondary rbody
            NI = ITAG(NUMNOD+N)
            F1(I) =  V(1,N)*MS(N)
            F2(I) =  V(2,N)*MS(N)
            F3(I) =  V(3,N)*MS(N)
c           XMOM = XMOM + V(1,N)*MS(N)
c           YMOM = YMOM + V(2,N)*MS(N)
c           ZMOM = ZMOM + V(3,N)*MS(N)
C Inertia matrix -> global frame
            II1=RBYI(10,NI)*RBYI(1,NI)
            II2=RBYI(10,NI)*RBYI(2,NI)
            II3=RBYI(10,NI)*RBYI(3,NI)
            II4=RBYI(11,NI)*RBYI(4,NI)
            II5=RBYI(11,NI)*RBYI(5,NI)
            II6=RBYI(11,NI)*RBYI(6,NI)
            II7=RBYI(12,NI)*RBYI(7,NI)
            II8=RBYI(12,NI)*RBYI(8,NI)
            II9=RBYI(12,NI)*RBYI(9,NI)
C
            IG1=RBYI(1,NI)*II1+RBYI(4,NI)*II4+RBYI(7,NI)*II7
            IG2=RBYI(1,NI)*II2+RBYI(4,NI)*II5+RBYI(7,NI)*II8
            IG3=RBYI(1,NI)*II3+RBYI(4,NI)*II6+RBYI(7,NI)*II9
            IG4=RBYI(2,NI)*II1+RBYI(5,NI)*II4+RBYI(8,NI)*II7
            IG5=RBYI(2,NI)*II2+RBYI(5,NI)*II5+RBYI(8,NI)*II8
            IG6=RBYI(2,NI)*II3+RBYI(5,NI)*II6+RBYI(8,NI)*II9
            IG7=RBYI(3,NI)*II1+RBYI(6,NI)*II4+RBYI(9,NI)*II7
            IG8=RBYI(3,NI)*II2+RBYI(6,NI)*II5+RBYI(9,NI)*II8
            IG9=RBYI(3,NI)*II3+RBYI(6,NI)*II6+RBYI(9,NI)*II9
C
            F4(I) = VR(1,N)*IG1 + VR(2,N)*IG2 + VR(3,N)*IG3
     .             +(X(2,N)-X(2,M))*V(3,N)*MS(N)
     .             -(X(3,N)-X(3,M))*V(2,N)*MS(N)
            F5(I) = VR(1,N)*IG4 + VR(2,N)*IG5 + VR(3,N)*IG6
     .             +(X(3,N)-X(3,M))*V(1,N)*MS(N)
     .             -(X(1,N)-X(1,M))*V(3,N)*MS(N)
            F6(I) = VR(1,N)*IG7 + VR(2,N)*IG8 + VR(3,N)*IG9
     .             +(X(1,N)-X(1,M))*V(2,N)*MS(N)
     .             -(X(2,N)-X(2,M))*V(1,N)*MS(N)
c            XXMOM = XXMOM + VR(1,N)*IG1 + VR(2,N)*IG2 + VR(3,N)*IG3
c     .        +(X(2,N)-X(2,M))*V(3,N)*MS(N)
c     .        -(X(3,N)-X(3,M))*V(2,N)*MS(N)
c            YYMOM = YYMOM + VR(1,N)*IG4 + VR(2,N)*IG5 + VR(3,N)*IG6
c     .        +(X(3,N)-X(3,M))*V(1,N)*MS(N)
c     .        -(X(1,N)-X(1,M))*V(3,N)*MS(N)
c            ZZMOM = ZZMOM + VR(1,N)*IG7 + VR(2,N)*IG8 + VR(3,N)*IG9
c     .        +(X(1,N)-X(1,M))*V(2,N)*MS(N)
c     .        -(X(2,N)-X(2,M))*V(1,N)*MS(N)
          ELSEIF(ITAG(NUMNOD+N) == 0.AND.WEIGHT(N) == 1)THEN
C node neither main nor secondary of secondary rbody
            F1(I) = V(1,N)*MS(N)
            F2(I) = V(2,N)*MS(N)
            F3(I) = V(3,N)*MS(N)
c            XMOM = XMOM + V(1,N)*MS(N)
c            YMOM = YMOM + V(2,N)*MS(N)
c            ZMOM = ZMOM + V(3,N)*MS(N)
C
            F4(I) = VR(1,N)*IN(N)
     .	           +(X(2,N)-X(2,M))*V(3,N)*MS(N)
     .	           -(X(3,N)-X(3,M))*V(2,N)*MS(N)
            F5(I) = VR(2,N)*IN(N)
     .             +(X(3,N)-X(3,M))*V(1,N)*MS(N)
     .             -(X(1,N)-X(1,M))*V(3,N)*MS(N)
            F6(I) = VR(3,N)*IN(N)
     .             +(X(1,N)-X(1,M))*V(2,N)*MS(N)
     .             -(X(2,N)-X(2,M))*V(1,N)*MS(N)
c            XXMOM = XXMOM + VR(1,N)*IN(N)
c     .        +(X(2,N)-X(2,M))*V(3,N)*MS(N)
c     .        -(X(3,N)-X(3,M))*V(2,N)*MS(N)
c            YYMOM = YYMOM + VR(2,N)*IN(N)
c     .        +(X(3,N)-X(3,M))*V(1,N)*MS(N)
c     .        -(X(1,N)-X(1,M))*V(3,N)*MS(N)
c            ZZMOM = ZZMOM + VR(3,N)*IN(N)
c     .        +(X(1,N)-X(1,M))*V(2,N)*MS(N)
c     .        -(X(2,N)-X(2,M))*V(1,N)*MS(N)
          ELSE
            F1(I) = ZERO
            F2(I) = ZERO
            F3(I) = ZERO
            F4(I) = ZERO
            F5(I) = ZERO
            F6(I) = ZERO
          ENDIF
C
        ENDDO
       ELSEIF(N2D==1) THEN
C       2D ANALYSIS : Axisymmetry
        DO I=1,NSN
          N = LPBY(I)
          IF(ITAG(NUMNOD+N) > 0.AND.WEIGHT(N) == 1)THEN
C main node of secondary rbody
            NI = ITAG(NUMNOD+N)
            F1(I) =  V(1,N)*MS(N)
            F2(I) =  V(2,N)*MS(N)
            F3(I) =  V(3,N)*MS(N)
C Inertia matrix -> global frame
            IG1=RBYI(10,NI)
            IG5=RBYI(11,NI)
            IG9=RBYI(12,NI)
C
            F4(I) = VR(1,N)*IG1 
     .             +(X(2,N)-X(2,M))*V(3,N)*MS(N)
     .             -(X(3,N)-X(3,M))*V(2,N)*MS(N)
            F5(I) = VR(2,N)*IG5 + 
     .             +(X(3,N)-X(3,M))*V(1,N)*MS(N)
     .             -(X(1,N)-X(1,M))*V(3,N)*MS(N)
            F6(I) = VR(3,N)*IG9
     .             +(X(1,N)-X(1,M))*V(2,N)*MS(N)
     .             -(X(2,N)-X(2,M))*V(1,N)*MS(N)

          ELSEIF(ITAG(NUMNOD+N) == 0.AND.WEIGHT(N) == 1)THEN
C node neither main nor secondary of secondary rbody
            F1(I) = V(1,N)*MS(N)
            F2(I) = V(2,N)*MS(N)
            F3(I) = V(3,N)*MS(N)
C
            F4(I) = VR(1,N)*IN(N)
     .	           +(X(2,N)-X(2,M))*V(3,N)*MS(N)
     .	           -(X(3,N)-X(3,M))*V(2,N)*MS(N)
            F5(I) = VR(2,N)*IN(N)
     .             +(X(3,N)-X(3,M))*V(1,N)*MS(N)
     .             -(X(1,N)-X(1,M))*V(3,N)*MS(N)
            F6(I) = VR(3,N)*IN(N)
     .             +(X(1,N)-X(1,M))*V(2,N)*MS(N)
     .             -(X(2,N)-X(2,M))*V(1,N)*MS(N)
          ELSE
            F1(I) = ZERO
            F2(I) = ZERO
            F3(I) = ZERO
            F4(I) = ZERO
            F5(I) = ZERO
            F6(I) = ZERO
          ENDIF
C
        ENDDO
       ELSEIF(N2D==2) THEN
C       2D ANALYSIS : Plane symmetry
        DO I=1,NSN
          N = LPBY(I)
          IF(ITAG(NUMNOD+N) > 0.AND.WEIGHT(N) == 1)THEN
C main node of secondary rbody
            NI = ITAG(NUMNOD+N)
            F1(I) =  ZERO
            F2(I) =  V(2,N)*MS(N)
            F3(I) =  V(3,N)*MS(N)
C Inertia matrix -> global frame
            II1=RBYI(10,NI)*RBYI(1,NI)
            II5=RBYI(11,NI)*RBYI(5,NI)
            II6=RBYI(11,NI)*RBYI(6,NI)
            II8=RBYI(12,NI)*RBYI(8,NI)
            II9=RBYI(12,NI)*RBYI(9,NI)
C
            IG1=RBYI(1,NI)*II1
            IG5=RBYI(5,NI)*II5+RBYI(8,NI)*II8
            IG6=RBYI(5,NI)*II6+RBYI(8,NI)*II9
            IG8=RBYI(6,NI)*II5+RBYI(9,NI)*II8
            IG9=RBYI(6,NI)*II6+RBYI(9,NI)*II9
C
            F4(I) = VR(1,N)*IG1+(X(2,N)-X(2,M))*V(3,N)*MS(N)
     .             -(X(3,N)-X(3,M))*V(2,N)*MS(N)
            F5(I) = ZERO
            F6(I) = ZERO
            F5(I) = VR(2,N)*IG5 + VR(3,N)*IG6
            F6(I) = VR(2,N)*IG8 + VR(3,N)*IG9
          ELSEIF(ITAG(NUMNOD+N) == 0.AND.WEIGHT(N) == 1)THEN
C node neither main nor secondary of secondary rbody
            F1(I) = ZERO
            F2(I) = V(2,N)*MS(N)
            F3(I) = V(3,N)*MS(N)
            F4(I) = VR(1,N)*IN(N)+(X(2,N)-X(2,M))*V(3,N)*MS(N)
     .             -(X(3,N)-X(3,M))*V(2,N)*MS(N)
            F5(I) = ZERO
            F6(I) = ZERO
            F5(I) = VR(2,N)*IN(N)
            F6(I) = VR(3,N)*IN(N)
          ELSE
            F1(I) = ZERO
            F2(I) = ZERO
            F3(I) = ZERO
            F4(I) = ZERO
            F5(I) = ZERO
            F6(I) = ZERO
          ENDIF
C
        ENDDO
      ENDIF
C
C
C Parith/ON treatment before exchange
C 
C
         DO K = 1, 6
          RBF6(1,K) = ZERO
          RBF6(2,K) = ZERO
          RBF6(3,K) = ZERO
          RBF6(4,K) = ZERO
          RBF6(5,K) = ZERO
          RBF6(6,K) = ZERO
        END DO

        CALL SUM_6_FLOAT(1  ,NSN  ,F1, RBF6(1,1), 6)
        CALL SUM_6_FLOAT(1  ,NSN  ,F2, RBF6(2,1), 6)
        CALL SUM_6_FLOAT(1  ,NSN  ,F3, RBF6(3,1), 6)
        CALL SUM_6_FLOAT(1  ,NSN  ,F4, RBF6(4,1), 6)
        CALL SUM_6_FLOAT(1  ,NSN  ,F5, RBF6(5,1), 6)
        CALL SUM_6_FLOAT(1  ,NSN  ,F6, RBF6(6,1), 6)

        
        IF(NSPMD > 1) THEN
          CALL SPMD_EXCH_FR6(ICOMM,RBF6,6*6)
        ENDIF

        XMOM = XMOM+
     +         RBF6(1,1)+RBF6(1,2)+RBF6(1,3)+
     +         RBF6(1,4)+RBF6(1,5)+RBF6(1,6)
        YMOM = YMOM+
     +         RBF6(2,1)+RBF6(2,2)+RBF6(2,3)+
     +         RBF6(2,4)+RBF6(2,5)+RBF6(2,6)
        ZMOM = ZMOM+
     +         RBF6(3,1)+RBF6(3,2)+RBF6(3,3)+
     +         RBF6(3,4)+RBF6(3,5)+RBF6(3,6)
        XXMOM= XXMOM+
     +         RBF6(4,1)+RBF6(4,2)+RBF6(4,3)+
     +         RBF6(4,4)+RBF6(4,5)+RBF6(4,6)
        YYMOM= YYMOM+
     +         RBF6(5,1)+RBF6(5,2)+RBF6(5,3)+
     +         RBF6(5,4)+RBF6(5,5)+RBF6(5,6)
        ZZMOM= ZZMOM+
     +         RBF6(6,1)+RBF6(6,2)+RBF6(6,3)+
     +         RBF6(6,4)+RBF6(6,5)+RBF6(6,6)

C
        V(1,M) = XMOM / MS(M)
        V(2,M) = YMOM / MS(M)
        V(3,M) = ZMOM / MS(M)
C
        WA1=XXMOM
        WA2=YYMOM
        WA3=ZZMOM
        XXMOM=RBY(1)*WA1+RBY(2)*WA2+RBY(3)*WA3
        YYMOM=RBY(4)*WA1+RBY(5)*WA2+RBY(6)*WA3
        ZZMOM=RBY(7)*WA1+RBY(8)*WA2+RBY(9)*WA3
        WA1 = XXMOM / RBY(10)
        WA2 = YYMOM / RBY(11)
        WA3 = ZZMOM / RBY(12)
        IF(N2D==0) THEN
           VR(1,M)=RBY(1)*WA1+RBY(4)*WA2+RBY(7)*WA3
           VR(2,M)=RBY(2)*WA1+RBY(5)*WA2+RBY(8)*WA3
           VR(3,M)=RBY(3)*WA1+RBY(6)*WA2+RBY(9)*WA3
        ELSEIF(N2D==1) THEN
           VR(1,M)=ZERO
           VR(2,M)=ZERO
           VR(3,M)=RBY(9)*WA3
        ELSEIF(N2D==2) THEN
          VR(1,M)=RBY(1)*WA1+RBY(4)*WA2+RBY(7)*WA3
          VR(2,M)=ZERO
          VR(3,M)=ZERO
        ENDIF

      ENDIF
C
      IF(ONFELT == 0.OR.ONFELT == 1)THEN
C-----------------------
C     Tag of secondary nodes
C-----------------------
      DO I=1,NSN
       ITAG(LPBY(I))=1
      ENDDO
C-----------------------
C     OFF SET TO -OFF
C-----------------------
      DO NG=1,NGROUP
       MLW=IPARG(1,NG)
       ITY=IPARG(5,NG)
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       IAD=IPARG(4,NG) - 1
       GBUF => ELBUF_TAB(NG)%GBUF
C-----------------------
C     1. Solid elements
C-----------------------
       IF(ITY == 1.AND.MLW /= 0)THEN       ! void material, off not used
         OFFG => ELBUF_TAB(NG)%GBUF%OFF
         DO I=1,NEL
           II=I+NFT
           NALL = ITAG(IXS(2,II)) * ITAG(IXS(3,II)) *
     +            ITAG(IXS(4,II)) * ITAG(IXS(5,II)) *
     +            ITAG(IXS(6,II)) * ITAG(IXS(7,II)) *
     +            ITAG(IXS(8,II)) * ITAG(IXS(9,II))
           IF(NALL /= 0)THEN
             OFF_OLD = OFFG(I)
             IF (ONFELT == 1) THEN
               OFFG(I) = ABS(OFFG(I))
               IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .            WRITE(IOUT,*)' BRICK ACTIVATION:',IXS(11,II)
             ELSEIF(ONFELT == 0)THEN
               OFFG(I) = -ABS(OFFG(I))
               IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .            WRITE(IOUT,*)' BRICK DEACTIVATION:',IXS(11,II)
             ENDIF
           ENDIF
         ENDDO
C----------------------------------------
C       Test for elimination of the group
C----------------------------------------
         IGOF = 1
         DO I = 1,NEL
           II=I+NFT
           IF (OFFG(I) > ZERO) IGOF=0
         ENDDO
         IPARG(8,NG) = IGOF
C-----------------------
C     2. Quad elements
C-----------------------
       ELSEIF(ITY == 2.AND.MLW /= 0)THEN       ! void material, off not used
         OFFG => ELBUF_TAB(NG)%GBUF%OFF
         DO I=1,NEL
           II=I+NFT
           NALL = ITAG(IXQ(2,II)) * ITAG(IXQ(3,II)) *
     +            ITAG(IXQ(4,II)) * ITAG(IXQ(5,II)) 
           IF(NALL /= 0)THEN
             OFF_OLD = OFFG(I)
             IF (ONFELT == 1) THEN
               OFFG(I) = ABS(OFFG(I))
               IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .            WRITE(IOUT,*)' QUAD ACTIVATION:',IXQ(7,II)
             ELSEIF(ONFELT == 0)THEN
               OFFG(I) = -ABS(OFFG(I))
               IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .            WRITE(IOUT,*)' QUAD DEACTIVATION:',IXQ(7,II)
             ENDIF
           ENDIF
         ENDDO
C----------------------------------------
C       Test for elimination of the group
C----------------------------------------
         IGOF = 1
         DO I = 1,NEL
           II=I+NFT
           IF (OFFG(I) > ZERO) IGOF=0
         ENDDO
         IPARG(8,NG) = IGOF
C-----------------------
C     3. SHell elements
C-----------------------
       ELSEIF(ITY == 3.AND.MLW /= 0)THEN       ! void material, off not used
         OFFG => ELBUF_TAB(NG)%GBUF%OFF
         ISTRAIN = IPARG(44,NG)
         NPT    =  IABS(IPARG(6,NG))
         IHBE   =  IPARG(23,NG)
         DO I=1,NEL
           II=I+NFT
           NALL = ITAG(IXC(2,II)) * ITAG(IXC(3,II)) *
     +            ITAG(IXC(4,II)) * ITAG(IXC(5,II))
           IF(NALL /= 0)THEN
             OFF_OLD = OFFG(I)
             IF(ONFELT == 1)THEN
               IF (OFFG(I) < ZERO)THEN
                 OFFG(I) = -OFFG(I)
                 MX = IPARTC(II)
                 PARTSAV(24,MX) = PARTSAV(24,MX)
     .                          - GBUF%EINT(I) - GBUF%EINT(I+NEL)
               ENDIF
               IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .            WRITE(IOUT,*)' SHELL ACTIVATION:',IXC(7,II)
             ELSEIF(ONFELT == 0)THEN
               IF (OFFG(I) > ZERO) THEN
                 OFFG(I) = -OFFG(I)
                 MX = IPARTC(II)
                 PARTSAV(24,MX) = PARTSAV(24,MX)
     .                          + GBUF%EINT(I) + GBUF%EINT(I+NEL)
               ENDIF
               IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .            WRITE(IOUT,*)' SHELL DEACTIVATION:',IXC(7,II)
             ENDIF
           ENDIF
         ENDDO
C----------------------------------------
C       Test for elimination of the group
C----------------------------------------
         IGOF = 1
         DO I = 1,NEL
           IF (OFFG(I) > ZERO) IGOF=0
         ENDDO
         IPARG(8,NG) = IGOF
C-----------------------
C     4. Truss elements
C-----------------------
       ELSEIF(ITY == 4.AND.(IACTS == 1.OR.CODVERS>=44))THEN
         OFFG => ELBUF_TAB(NG)%GBUF%OFF
         DO I=1,NEL
           II=I+NFT
           NALL = ITAG(IXT(2,II)) * ITAG(IXT(3,II))
           IF(NALL /= 0)THEN
             OFF_OLD = OFFG(I)
             IF(ONFELT == 1)THEN
               OFFG(I) = ABS(OFFG(I))
               IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .            WRITE(IOUT,*)' TRUSS ACTIVATION:',IXT(5,II)
             ELSEIF(ONFELT == 0)THEN
               OFFG(I) = -ABS(OFFG(I))
               IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .            WRITE(IOUT,*)' TRUSS DEACTIVATION:',IXT(5,II)
             ENDIF
           ENDIF
         ENDDO
C----------------------------------------
C       Test for elimination of the group
C----------------------------------------
C Imcompatible with gap option in truss property
C        IGOF = 1
C        DO I = 1,NEL
C         IF(ELBUF(IAD + I) /= ZERO) IGOF=0
C        ENDDO
C        IPARG(8,NG) = IGOF
C-----------------------
C     5. Beam elements
C-----------------------
       ELSEIF(ITY == 5.AND.(IACTS == 1.OR.CODVERS>=44))THEN
         OFFG => ELBUF_TAB(NG)%GBUF%OFF
         DO I=1,NEL
           II=I+NFT
           NALL = ITAG(IXP(2,II)) * ITAG(IXP(3,II))
           IF(NALL /= 0)THEN
             OFF_OLD = OFFG(I)
             IF(ONFELT == 1)THEN
               OFFG(I) = ABS(OFFG(I))
               IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .            WRITE(IOUT,*)' BEAM ACTIVATION:',IXP(6,II)
             ELSEIF(ONFELT == 0)THEN
               OFFG(I) = -ABS(OFFG(I))
               IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .            WRITE(IOUT,*)' BEAM DEACTIVATION:',IXP(6,II)
             ENDIF
           ENDIF
         ENDDO
C----------------------------------------
C        Test for elimination of the group
C----------------------------------------
         IGOF = 1
         DO I = 1,NEL
           IF (OFFG(I) > ZERO) IGOF=0
         ENDDO
         IPARG(8,NG) = IGOF
C-----------------------
C     6. Spring elements
C-----------------------
       ELSEIF(ITY == 6.AND.MLW /= 3.AND.
     .        (IACTS == 1.OR.CODVERS>=44))THEN
         OFFG => ELBUF_TAB(NG)%GBUF%OFF
         DO I=1,NEL
           II=I+NFT
           NALL = ITAG(IXR(2,II)) * ITAG(IXR(3,II))
           IF(NALL /= 0)THEN
             OFF_OLD = OFFG(I)
             IF(ONFELT == 1)THEN
               IF (OFFG(I) /= -TEN)
C                spring is active
     .           OFFG(I)= ABS(OFFG(I))
                 IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .              WRITE(IOUT,*)' SPRING ACTIVATION:',IXR(NIXR,II)
             ELSEIF(ONFELT == 0)THEN
               IF (OFFG(I) /= -TEN)
C                spring is active
     .           OFFG(I) = -ABS(OFFG(I))
                 IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .              WRITE(IOUT,*)' SPRING DEACTIVATION:',IXR(NIXR,II)
             ENDIF
           ENDIF
         ENDDO
C----------------------------------------
C        Test for elimination of the group
C----------------------------------------
         IGOF = 1
         DO I = 1,NEL
           IF(OFFG(I) /= ZERO) IGOF=0
         ENDDO
         IPARG(8,NG) = IGOF
C-----------------------
C     7. SH3N elements
C-----------------------
       ELSEIF (ITY == 7 .AND. MLW /= 0) THEN       ! void material, off not used
         OFFG => ELBUF_TAB(NG)%GBUF%OFF
         ISTRAIN = IPARG(44,NG)
         NPT    =  IABS(IPARG(6,NG))
         DO I=1,NEL
           II=I+NFT
           NALL = ITAG(IXTG(2,II)) * ITAG(IXTG(3,II)) *
     +            ITAG(IXTG(4,II))
           IF(NALL /= 0)THEN
             OFF_OLD = OFFG(I)
             IF (ONFELT == 1) THEN
               OFFG(I) = ABS(OFFG(I))
               IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .            WRITE(IOUT,*)' SH_3N ACTIVATION:',IXTG(6,II)
             ELSEIF(ONFELT == 0)THEN
               OFFG(I) = -ABS(OFFG(I))
               IF ((PRI_OFF==0).OR.(OFF_OLD*OFFG(I)<ZERO))
     .            WRITE(IOUT,*)' SH_3N DEACTIVATION:',IXTG(6,II)
             ENDIF
           ENDIF
         ENDDO
C----------------------------------------
C        Test for elimination of the group
C----------------------------------------
        IGOF = 1
        DO I = 1,NEL
          IF (OFFG(I) > ZERO) IGOF=0
        ENDDO
        IPARG(8,NG) = IGOF
C----------------------------------------
       ENDIF
      ENDDO
C-----------------------
C     Rest of tag of secondary nodes
C-----------------------
      DO I=1,NSN
       ITAG(LPBY(I))=0
      ENDDO

      ENDIF ! IF(ONFELT == 0.OR.ONFELT == 1)THEN
C
 100  CONTINUE
      IF(NSPMD > 1) THEN
C Treatment needed to get active and inative elements in the right order 
        IWIOUT = 0
        IF (ISPMD /= 0) CALL SPMD_CHKW(IWIOUT,IOUT)
        CALL SPMD_GLOB_ISUM9(IWIOUT,1)
        CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
        IF (IWIOUT > 0) THEN
          CALL SPMD_WIOUT(IOUT,IWIOUT)
          IWIOUT = 0
        ENDIF
      ENDIF
C-----------
      RETURN
      END
