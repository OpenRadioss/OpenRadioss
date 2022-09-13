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
Chd|  RBE2V                         source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2V1                        source/constraints/general/rbe2/rbe2v.F
Chd|        RBE2VL1                       source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE RBE2V(IRBE2 ,LRBE2 ,X    ,A     ,AR    ,
     1                 V     ,VR    ,SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*)
C     REAL
      my_real
     .   X(3,*), A(3,*), AR(3,*),V(3,*), VR(3,*),SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, M, NS ,NML, IAD,JJ,IROT,IADS,
     .        JT(3,NRBE2),JR(3,NRBE2),NM,NN,K,ISK,NSN,IRAD
C     REAL
C======================================================================|
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
      DO N=NRBE2,1,-1
        IAD = IRBE2(1,N)
        M  = IRBE2(3,N)
        NSN = IRBE2(5,N)
        ISK = IRBE2(7,N)
        IRAD = IRBE2(11,N)
	IF (ISK>1) THEN
           CALL RBE2VL1(NSN   ,LRBE2(IAD+1),X     ,A     ,AR    ,
     1                  V     ,VR    ,JT(1,N),JR(1,N),M     ,
     2                  SKEW(1,ISK),IRAD  )
        ELSE
           CALL RBE2V1(NSN   ,LRBE2(IAD+1),X     ,A     ,AR    ,
     1                  V     ,VR    ,JT(1,N),JR(1,N),M  ,IRAD  )
        END IF
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2V0                        source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBE2V0(NSL   ,ISL   ,X     ,A     ,AR    ,
     1                  V     ,VR    ,JT    ,JR    ,M     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ISL(*),JT(3),JR(3),M
C     REAL
      my_real
     .   X(3,*), A(3,*), AR(3,*), V(3,*), VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS
C     REAL
      my_real
     .     XS, YS, ZS,VRM(3)
C======================================================================|
        DO J = 1,3
	 IF (JT(J)/=0) THEN
          DO I=1,NSL
           NS = ISL(I)
C	   V(J,NS)= V(J,M)
	   A(J,NS)= A(J,M)
	  ENDDO
         ENDIF
        ENDDO
       IF ((JR(1)+JR(2)+JR(3))>0) THEN
        DO J = 1,3
	 IF (JR(J)/=0) THEN
          DO I=1,NSL
           NS = ISL(I)
C	   VR(J,NS)= VR(J,M)
	   AR(J,NS)= AR(J,M)
	  ENDDO
         ENDIF
	 VRM(J)= AR(J,M)*JR(J)
        ENDDO
        DO I=1,NSL
         NS = ISL(I)
         XS=X(1,NS)-X(1,M)
         YS=X(2,NS)-X(2,M)
         ZS=X(3,NS)-X(3,M)
         A(1,NS)=A(1,NS)+VRM(2)*ZS-VRM(3)*YS
         A(2,NS)=A(2,NS)-VRM(1)*ZS+VRM(3)*XS
         A(3,NS)=A(3,NS)+VRM(1)*YS-VRM(2)*XS
        ENDDO
       END IF
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2VL                        source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBE2VL(NSN   ,ISL   ,X     ,A     ,AR    ,
     1                  V     ,VR    ,JT    ,JR    ,M     ,
     2                  SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,ISL(*),JT(3),JR(3),M
C     REAL
      my_real
     .   X(3,*), A(3,*), AR(3,*), V(3,*), VR(3,*),SKEW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS
C     REAL
      my_real
     .   XS, YS, ZS,RX, RY,RZ,LRX, LRY,LRZ,RVX,RVY,RVZ,
     .   DVX,DVY,DVZ,VVX,VVY,VVZ,LXS(NSN), LYS(NSN), LZS(NSN)
C======================================================================|
      DO I=1,NSN
        NS = ISL(I)
        DVX  =A(1,NS)-A(1,M)
        DVY  =A(2,NS)-A(2,M)
        DVZ  =A(3,NS)-A(3,M)
        VVX  =JT(1)*(SKEW(1)*DVX+SKEW(2)*DVY+SKEW(3)*DVZ)
        VVY  =JT(2)*(SKEW(4)*DVX+SKEW(5)*DVY+SKEW(6)*DVZ)
        VVZ  =JT(3)*(SKEW(7)*DVX+SKEW(8)*DVY+SKEW(9)*DVZ)
        A(1,NS) =A(1,NS)-VVX*SKEW(1)-VVY*SKEW(4)-VVZ*SKEW(7)
        A(2,NS) =A(2,NS)-VVX*SKEW(2)-VVY*SKEW(5)-VVZ*SKEW(8)
        A(3,NS) =A(3,NS)-VVX*SKEW(3)-VVY*SKEW(6)-VVZ*SKEW(9)
      ENDDO
      IF ((JR(1)+JR(2)+JR(3))>0) THEN
       DO I=1,NSN
        NS = ISL(I)
        XS=X(1,NS)-X(1,M)
        YS=X(2,NS)-X(2,M)
        ZS=X(3,NS)-X(3,M)
        LXS(I)=SKEW(1)*XS+SKEW(2)*YS+SKEW(3)*ZS
        LYS(I)=SKEW(4)*XS+SKEW(5)*YS+SKEW(6)*ZS
        LZS(I)=SKEW(7)*XS+SKEW(8)*YS+SKEW(9)*ZS
       ENDDO
       DO I=1,NSN
        NS = ISL(I)
        DVX  =AR(1,NS)-AR(1,M)
        DVY  =AR(2,NS)-AR(2,M)
        DVZ  =AR(3,NS)-AR(3,M)
        VVX  =JR(1)*(SKEW(1)*DVX+SKEW(2)*DVY+SKEW(3)*DVZ)
        VVY  =JR(2)*(SKEW(4)*DVX+SKEW(5)*DVY+SKEW(6)*DVZ)
        VVZ  =JR(3)*(SKEW(7)*DVX+SKEW(8)*DVY+SKEW(9)*DVZ)
        AR(1,NS) =AR(1,NS)-VVX*SKEW(1)-VVY*SKEW(4)-VVZ*SKEW(7)
        AR(2,NS) =AR(2,NS)-VVX*SKEW(2)-VVY*SKEW(5)-VVZ*SKEW(8)
        AR(3,NS) =AR(3,NS)-VVX*SKEW(3)-VVY*SKEW(6)-VVZ*SKEW(9)
        RX=AR(1,M)
        RY=AR(2,M)
        RZ=AR(3,M)
        LRX =JR(1)*(SKEW(1)*RX+SKEW(2)*RY+SKEW(3)*RZ)
        LRY =JR(2)*(SKEW(4)*RX+SKEW(5)*RY+SKEW(6)*RZ)
        LRZ =JR(3)*(SKEW(7)*RX+SKEW(8)*RY+SKEW(9)*RZ)
        RVX=LRY*LZS(I)-LRZ*LYS(I)
        RVY=-LRX*LZS(I)+LRZ*LXS(I)
        RVZ=LRX*LYS(I)-LRY*LXS(I)
        A(1,NS) =A(1,NS)+RVX*SKEW(1)+RVY*SKEW(4)+RVZ*SKEW(7)
        A(2,NS) =A(2,NS)+RVX*SKEW(2)+RVY*SKEW(5)+RVZ*SKEW(8)
        A(3,NS) =A(3,NS)+RVX*SKEW(3)+RVY*SKEW(6)+RVZ*SKEW(9)
       ENDDO
      END IF
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_IMPD                     source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        RECUKIN                       source/implicit/recudis.F     
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2D0                        source/constraints/general/rbe2/rbe2v.F
Chd|        RBE2DL2                       source/constraints/general/rbe2/rbe2v.F
Chd|        RBE2DL3                       source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE RBE2_IMPD(IRBE2 ,LRBE2 ,X    ,D     ,DR    ,SKEW )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*)
C     REAL
      my_real
     .   X(3,*), D(3,*), DR(3,*),SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, M, NS ,NML, IAD,JJ,ISK,
     .        JT(3,NRBE2),JR(3,NRBE2),NM,NN,K,NSL,IRAD
C     REAL
C======================================================================|
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
      DO N=NRBE2,1,-1
        IAD = IRBE2(1,N)
        M  = IRBE2(3,N)
        NSL = IRBE2(5,N)
        ISK = IRBE2(7,N)
        IRAD= IRBE2(11,N)
	  IF (ISK>1) THEN
	    IF( IMP_LR == 0 ) THEN
            CALL RBE2DL2(NSL   ,LRBE2(IAD+1),X     ,D     ,DR    ,
     1                 JT(1,N),JR(1,N),M     ,SKEW(1,ISK),IRAD  )
          ELSE
            CALL RBE2DL3(NSL   ,LRBE2(IAD+1),X     ,D     ,DR    ,
     1                 JT(1,N),JR(1,N),M     ,SKEW(1,ISK),IRAD  )
          END IF
        ELSE
           CALL RBE2D0(NSL   ,LRBE2(IAD+1),X     ,D     ,DR    ,
     1                 JT(1,N),JR(1,N),M   ,IRAD   )
        END IF
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2D0                        source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        RBE2_IMPD                     source/constraints/general/rbe2/rbe2v.F
Chd|-- calls ---------------
Chd|        VELROT                        source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE RBE2D0(NSL   ,ISL   ,X     ,V     ,VR    ,
     1                  JT    ,JR    ,M     ,IRAD  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ISL(*),JT(3),JR(3),M,IRAD
C     REAL
      my_real
     .   X(3,*), V(3,*), VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS
C     REAL
      my_real
     .     XS, YS, ZS,VRM(3),LSM(3),VS(3)
C======================================================================|
        DO J = 1,3
	 IF (JT(J)/=0) THEN
          DO I=1,NSL
           NS = ISL(I)
	   V(J,NS)= V(J,M)
	  ENDDO
         ENDIF
        ENDDO
       IF ((JR(1)+JR(2)+JR(3))>0) THEN
        DO J = 1,3
	 IF (JR(J)/=0) THEN
          DO I=1,NSL
           NS = ISL(I)
	   VR(J,NS)= VR(J,M)
	  ENDDO
         ENDIF
        ENDDO
       END IF
C
       IF (IRAD==0) THEN
        DO J = 1,3
	 VRM(J)= VR(J,M)
        ENDDO
        DO I=1,NSL
         NS = ISL(I)
         XS=X(1,NS)-X(1,M)
         YS=X(2,NS)-X(2,M)
         ZS=X(3,NS)-X(3,M)
         IF( IMP_LR == 0)THEN
           IF (JT(1)/=0) V(1,NS)=V(1,NS)+VRM(2)*ZS-VRM(3)*YS
           IF (JT(2)/=0) V(2,NS)=V(2,NS)-VRM(1)*ZS+VRM(3)*XS
           IF (JT(3)/=0) V(3,NS)=V(3,NS)+VRM(1)*YS-VRM(2)*XS
         ELSE
           LSM(1) = XS
           LSM(2) = YS
           LSM(3) = ZS
           CALL VELROT(VRM, LSM,VS)
           IF (JT(1)/=0) V(1,NS)=V(1,NS) + VS(1)
           IF (JT(2)/=0) V(2,NS)=V(2,NS) + VS(2)
           IF (JT(3)/=0) V(3,NS)=V(3,NS) + VS(3)
         END IF
        ENDDO
       ELSEIF ((JR(1)+JR(2)+JR(3))>0) THEN
        DO J = 1,3
	 VRM(J)= VR(J,M)*JR(J)
        ENDDO
        DO I=1,NSL
         NS = ISL(I)
         XS=X(1,NS)-X(1,M)
         YS=X(2,NS)-X(2,M)
         ZS=X(3,NS)-X(3,M)
         IF( IMP_LR == 0 ) THEN
           V(1,NS)=V(1,NS)+VRM(2)*ZS-VRM(3)*YS
           V(2,NS)=V(2,NS)-VRM(1)*ZS+VRM(3)*XS
           V(3,NS)=V(3,NS)+VRM(1)*YS-VRM(2)*XS
         ELSE
          LSM(1) = XS
          LSM(2) = YS
          LSM(3) = ZS
          CALL VELROT(VRM, LSM,VS)
          V(1,NS)=V(1,NS)+ VS(1)
          V(2,NS)=V(2,NS)+ VS(2)
          V(3,NS)=V(3,NS)+ VS(3)
         END IF
        ENDDO
       END IF
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2DL                        source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBE2DL(NSN   ,ISL   ,X     ,V     ,VR    ,
     1                  JT    ,JR    ,M     ,SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,ISL(*),JT(3),JR(3),M
C     REAL
      my_real
     .   X(3,*), V(3,*), VR(3,*),SKEW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NS
C     REAL
      my_real
     .   XS, YS, ZS,RX, RY,RZ,LRX, LRY,LRZ,RVX,RVY,RVZ,
     .   DVX,DVY,DVZ,VVX,VVY,VVZ,LXS(NSN), LYS(NSN), LZS(NSN)
C======================================================================|
      DO I=1,NSN
        NS = ISL(I)
        DVX  =V(1,NS)-V(1,M)
        DVY  =V(2,NS)-V(2,M)
        DVZ  =V(3,NS)-V(3,M)
        VVX  =JT(1)*(SKEW(1)*DVX+SKEW(2)*DVY+SKEW(3)*DVZ)
        VVY  =JT(2)*(SKEW(4)*DVX+SKEW(5)*DVY+SKEW(6)*DVZ)
        VVZ  =JT(3)*(SKEW(7)*DVX+SKEW(8)*DVY+SKEW(9)*DVZ)
        V(1,NS) =V(1,NS)-VVX*SKEW(1)-VVY*SKEW(4)-VVZ*SKEW(7)
        V(2,NS) =V(2,NS)-VVX*SKEW(2)-VVY*SKEW(5)-VVZ*SKEW(8)
        V(3,NS) =V(3,NS)-VVX*SKEW(3)-VVY*SKEW(6)-VVZ*SKEW(9)
      ENDDO
      IF ((JR(1)+JR(2)+JR(3))>0) THEN
       DO I=1,NSN
        NS = ISL(I)
        XS=X(1,NS)-X(1,M)
        YS=X(2,NS)-X(2,M)
        ZS=X(3,NS)-X(3,M)
        LXS(I)=SKEW(1)*XS+SKEW(2)*YS+SKEW(3)*ZS
        LYS(I)=SKEW(4)*XS+SKEW(5)*YS+SKEW(6)*ZS
        LZS(I)=SKEW(7)*XS+SKEW(8)*YS+SKEW(9)*ZS
       ENDDO
       DO I=1,NSN
        NS = ISL(I)
        DVX  =VR(1,NS)-VR(1,M)
        DVY  =VR(2,NS)-VR(2,M)
        DVZ  =VR(3,NS)-VR(3,M)
        VVX  =JR(1)*(SKEW(1)*DVX+SKEW(2)*DVY+SKEW(3)*DVZ)
        VVY  =JR(2)*(SKEW(4)*DVX+SKEW(5)*DVY+SKEW(6)*DVZ)
        VVZ  =JR(3)*(SKEW(7)*DVX+SKEW(8)*DVY+SKEW(9)*DVZ)
        VR(1,NS) =VR(1,NS)-VVX*SKEW(1)-VVY*SKEW(4)-VVZ*SKEW(7)
        VR(2,NS) =VR(2,NS)-VVX*SKEW(2)-VVY*SKEW(5)-VVZ*SKEW(8)
        VR(3,NS) =VR(3,NS)-VVX*SKEW(3)-VVY*SKEW(6)-VVZ*SKEW(9)
        RX=VR(1,M)
        RY=VR(2,M)
        RZ=VR(3,M)
        LRX =JR(1)*(SKEW(1)*RX+SKEW(2)*RY+SKEW(3)*RZ)
        LRY =JR(2)*(SKEW(4)*RX+SKEW(5)*RY+SKEW(6)*RZ)
        LRZ =JR(3)*(SKEW(7)*RX+SKEW(8)*RY+SKEW(9)*RZ)
        RVX=LRY*LZS(I)-LRZ*LYS(I)
        RVY=-LRX*LZS(I)+LRZ*LXS(I)
        RVZ=LRX*LYS(I)-LRY*LXS(I)
        V(1,NS) =V(1,NS)+RVX*SKEW(1)+RVY*SKEW(4)+RVZ*SKEW(7)
        V(2,NS) =V(2,NS)+RVX*SKEW(2)+RVY*SKEW(5)+RVZ*SKEW(8)
        V(3,NS) =V(3,NS)+RVX*SKEW(3)+RVY*SKEW(6)+RVZ*SKEW(9)
       ENDDO
      END IF
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2DL2                       source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        RBE2_IMPD                     source/constraints/general/rbe2/rbe2v.F
Chd|-- calls ---------------
Chd|        CDI_BCN1                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2D_BCL                     source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE RBE2DL2(NSN   ,ISL   ,X     ,V     ,VR    ,
     1                   JT    ,JR    ,M     ,SKEW  ,IRAD  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,ISL(*),JT(3),JR(3),M,IRAD
C     REAL
      my_real
     .   X(3,*), V(3,*), VR(3,*),SKEW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NS ,NT,NR,ICT,ICR
C     REAL
      my_real
     .   XS, YS, ZS,RX, RY,RZ,LRX, LRY,LRZ,RVX,RVY,RVZ,
     .   DVX,DVY,DVZ,VVX,VVY,VVZ,KTR(3,3)
C======================================================================|
        NT=JT(1)+JT(2)+JT(3)
        NR=JR(1)+JR(2)+JR(3)
        ICT=JT(1)*4 +JT(2)*2 +JT(3)
        ICR=JR(1)*4 +JR(2)*2 +JR(3)
      IF (NT>0.AND.NT<3) THEN
       CALL RBE2D_BCL(ICT  ,NSN   ,ISL   ,M     ,V     ,
     1               SKEW  )
      ELSEIF (NT==3) THEN
       DO I=1,NSN
        NS = ISL(I)
        V(1,NS)=V(1,M)
        V(2,NS)=V(2,M)
        V(3,NS)=V(3,M)
       ENDDO
      ENDIF
C
      IF (NR>0) THEN
       IF (NR<3) THEN
        CALL RBE2D_BCL(ICR  ,NSN   ,ISL   ,M     ,VR    ,
     1                 SKEW )
       ELSEIF (NR==3) THEN
        DO I=1,NSN
         NS = ISL(I)
         VR(1,NS)=VR(1,M)
         VR(2,NS)=VR(2,M)
         VR(3,NS)=VR(3,M)
        ENDDO
       ENDIF
      END IF
C
      IF (IRAD==0.OR.NR>0) THEN
       DO I=1,NSN
        NS = ISL(I)
        RX=VR(1,M)
        RY=VR(2,M)
        RZ=VR(3,M)
        XS=X(1,NS)-X(1,M)
        YS=X(2,NS)-X(2,M)
        ZS=X(3,NS)-X(3,M)
        CALL CDI_BCN1(XS,YS,ZS,JT,JR,SKEW,KTR,IRAD)
        V(1,NS) =V(1,NS)+KTR(1,1)*RX+KTR(1,2)*RY+KTR(1,3)*RZ
        V(2,NS) =V(2,NS)+KTR(2,1)*RX+KTR(2,2)*RY+KTR(2,3)*RZ
        V(3,NS) =V(3,NS)+KTR(3,1)*RX+KTR(3,2)*RY+KTR(3,3)*RZ
       ENDDO
      END IF
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2D_BCL                     source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        RBE2DL2                       source/constraints/general/rbe2/rbe2v.F
Chd|        RBE2VL1                       source/constraints/general/rbe2/rbe2v.F
Chd|        RBE2_FRD                      source/constraints/general/rbe2/rbe2v.F
Chd|-- calls ---------------
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2D_BCL(ICT  ,NSN   ,ISL   ,M     ,V     ,
     1                    SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICT,NSN   ,ISL(*),M
      my_real
     .   SKEW(LSKEW),V(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,J1,L,NS
      my_real
     .   VQ(3,3),EJ(3),EJ1(3),VLM(3),VV(3),VLS(3),S
C----------------------------------------
      DO I=1,3
       VQ(1,I)= SKEW(I)
       VQ(2,I)= SKEW(I+3)
       VQ(3,I)= SKEW(I+6)
      ENDDO
        VV(1) =V(1,M)
        VV(2) =V(2,M)
        VV(3) =V(3,M)
        VLM(1)  =SKEW(1)*VV(1)+SKEW(2)*VV(2)+SKEW(3)*VV(3)
        VLM(2)  =SKEW(4)*VV(1)+SKEW(5)*VV(2)+SKEW(6)*VV(3)
        VLM(3)  =SKEW(7)*VV(1)+SKEW(8)*VV(2)+SKEW(9)*VV(3)
C-------------------100---------------------
      IF (ICT == 4 ) THEN
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
          CALL L_DIR(EJ,J)
	  J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
C-------------------010---------------------
       ELSEIF (ICT == 2) THEN
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
	  J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
C-------------------001---------------------
       ELSEIF (ICT == 1) THEN
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
	  J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
C-------------------011---------------------
       ELSEIF (ICT == 3) THEN
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(4)
          EJ1(2)=SKEW(5)
          EJ1(3)=SKEW(6)
          CALL L_DIR(EJ1,J1)
          IF (J1==J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
          ENDIF
          VLS(3)=VLM(3)
          VLS(2)=VLM(2)
          CALL DIR_RBE2(J    ,J1    ,K     )
	  IF (ABS(VQ(1,K))<EM20) THEN
	   S= ZERO
	  ELSE
	   S= ONE/VQ(1,K)
	  ENDIF
C-------------------101---------------------
       ELSEIF (ICT == 5) THEN
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1==J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
          ENDIF
          VLS(3)=VLM(3)
          VLS(1)=VLM(1)
          CALL DIR_RBE2(J    ,J1    ,K     )
	  IF (ABS(VQ(2,K))<EM20) THEN
	   S= ZERO
	  ELSE
	   S= ONE/VQ(2,K)
	  ENDIF
C-------------------110---------------------
       ELSEIF (ICT == 6) THEN
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1==J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
          ENDIF
          VLS(2)=VLM(2)
          VLS(1)=VLM(1)
          CALL DIR_RBE2(J    ,J1    ,K     )
	  IF (ABS(VQ(3,K))<EM20) THEN
	   S= ZERO
	  ELSE
	   S= ONE/VQ(3,K)
	  ENDIF
       ENDIF
       DO I=1,NSN
        NS = ISL(I)
C-------------------100---------------------
         IF (ICT == 4 ) THEN
          V(J,NS) = VLM(1)/SKEW(J)-EJ(J1)*V(J1,NS)-EJ(K)*V(K,NS)
C-------------------010---------------------
         ELSEIF (ICT == 2) THEN
          V(J,NS) = VLM(2)/SKEW(3+J)-EJ(J1)*V(J1,NS)-EJ(K)*V(K,NS)
C-------------------001---------------------
         ELSEIF (ICT == 1) THEN
          V(J,NS) = VLM(3)/SKEW(6+J)-EJ(J1)*V(J1,NS)-EJ(K)*V(K,NS)
C-------------------011---------------------
         ELSEIF (ICT == 3) THEN
          VLS(1)=(V(K,NS)-VLS(3)*VQ(3,K)-VLS(2)*VQ(2,K))*S
          V(1,NS) =VLS(1)*SKEW(1)+VLS(2)*SKEW(4)+VLS(3)*SKEW(7)
          V(2,NS) =VLS(1)*SKEW(2)+VLS(2)*SKEW(5)+VLS(3)*SKEW(8)
          V(3,NS) =VLS(1)*SKEW(3)+VLS(2)*SKEW(6)+VLS(3)*SKEW(9)
C-------------------101---------------------
         ELSEIF (ICT == 5) THEN
          VLS(2)=(V(K,NS)-VLS(3)*VQ(3,K)-VLS(1)*VQ(1,K))*S
          V(1,NS) =VLS(1)*SKEW(1)+VLS(2)*SKEW(4)+VLS(3)*SKEW(7)
          V(2,NS) =VLS(1)*SKEW(2)+VLS(2)*SKEW(5)+VLS(3)*SKEW(8)
          V(3,NS) =VLS(1)*SKEW(3)+VLS(2)*SKEW(6)+VLS(3)*SKEW(9)
C-------------------110---------------------
         ELSEIF (ICT == 6) THEN
          VLS(3)=(V(K,NS)-VLS(2)*VQ(2,K)-VLS(1)*VQ(1,K))*S
          V(1,NS) =VLS(1)*SKEW(1)+VLS(2)*SKEW(4)+VLS(3)*SKEW(7)
          V(2,NS) =VLS(1)*SKEW(2)+VLS(2)*SKEW(5)+VLS(3)*SKEW(8)
          V(3,NS) =VLS(1)*SKEW(3)+VLS(2)*SKEW(6)+VLS(3)*SKEW(9)
         ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        BCL_IMPD                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_FI2                        source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPD2D                      source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDF2D                     source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDFR2                     source/constraints/general/bcs/bc_imp0.F
Chd|        BC_UPDK2D                     source/constraints/general/bcs/bc_imp0.F
Chd|        CDI_BCN                       source/constraints/general/rbe2/rbe2_imp0.F
Chd|        CDI_BCN1                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        FVBC_COMPA0                   source/constraints/general/impvel/fv_imp0.F
Chd|        FV_UPDKD2                     source/constraints/general/bcs/bc_imp0.F
Chd|        GETBCL_J                      source/constraints/general/impvel/fv_imp0.F
Chd|        GFVBC2_IND                    source/constraints/general/impvel/fv_imp0.F
Chd|        RBE2D_BCL                     source/constraints/general/rbe2/rbe2v.F
Chd|        RBE2FLSN                      source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2FLSNFR                    source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2IMPBSN                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_BCL                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        SELECT_DOF                    source/constraints/general/rbe2/rbe2v.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DIR_RBE2(J    ,J1    ,K     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER K,J,J1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      K = J + 1
      IF (K>3) K = K - 3
      IF (J1==0) THEN
       J1 = J + 2
       IF (J1>3) J1 = J1 - 3
      ELSEIF (K==J1) THEN
       K = J + 2
       IF (K>3) K = K - 3
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2V1                        source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        RBE2V                         source/constraints/general/rbe2/rbe2v.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBE2V1(NSL   ,ISL   ,X     ,A     ,AR    ,
     1                  V     ,VR    ,JT    ,JR    ,M     ,
     2                  IRAD  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ISL(*),JT(3),JR(3),M,IRAD
C     REAL
      my_real
     .   X(3,*), A(3,*), AR(3,*), V(3,*), VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NS
C     REAL
      my_real
     .   V1X2, V2X1, V2X3, V3X2, V3X1, V1X3,USDT,DT05,
     .   VX1, VX2, VX3,VG(3)
C======================================================================|
C-------change As=Am to Vs=Vm------
        USDT = ONE/DT12
        DO J = 1,3
	 IF (JT(J)/=0) THEN
          DO I=1,NSL
           NS = ISL(I)
	   A(J,NS)= A(J,M)+(V(J,M)-V(J,NS))*USDT
	  ENDDO
         ENDIF
        ENDDO
C
       IF ((JR(1)+JR(2)+JR(3))>0.OR.IRAD==0) THEN
        DO J = 1,3
         VG(J)=VR(J,M)+AR(J,M)*DT12
	 IF (JR(J)/=0) THEN
          DO I=1,NSL
           NS = ISL(I)
           AR(J,NS)= (VG(J)-VR(J,NS)) * USDT
	  ENDDO
         ENDIF
        ENDDO
       END IF
C
       IF (IRAD==0) THEN
        DT05 = HALF*DT2
        DO I=1,NSL
         NS = ISL(I)
         V1X2=VG(1)*(X(2,NS)-X(2,M))
         V2X1=VG(2)*(X(1,NS)-X(1,M))
         V2X3=VG(2)*(X(3,NS)-X(3,M))
         V3X2=VG(3)*(X(2,NS)-X(2,M))
         V3X1=VG(3)*(X(1,NS)-X(1,M))
         V1X3=VG(1)*(X(3,NS)-X(3,M))
C
         VX1=V2X3-V3X2
         VX2=V3X1-V1X3
         VX3=V1X2-V2X1
	 IF (JT(1)/=0)
     .   A(1,NS)= A(1,NS)+(VX1+DT05*(VG(2)*VX3-VG(3)*VX2))*USDT
	 IF (JT(2)/=0)
     .   A(2,NS)= A(2,NS)+(VX2+DT05*(VG(3)*VX1-VG(1)*VX3))*USDT
	 IF (JT(3)/=0)
     .   A(3,NS)= A(3,NS)+(VX3+DT05*(VG(1)*VX2-VG(2)*VX1))*USDT
        ENDDO
       ELSEIF ((JR(1)+JR(2)+JR(3))>0) THEN
        DT05 = HALF*DT2
        DO J = 1,3
         VG(J)=JR(J)*VG(J)
        ENDDO
        DO I=1,NSL
         NS = ISL(I)
C
         V1X2=VG(1)*(X(2,NS)-X(2,M))
         V2X1=VG(2)*(X(1,NS)-X(1,M))
         V2X3=VG(2)*(X(3,NS)-X(3,M))
         V3X2=VG(3)*(X(2,NS)-X(2,M))
         V3X1=VG(3)*(X(1,NS)-X(1,M))
         V1X3=VG(1)*(X(3,NS)-X(3,M))
C
         VX1=V2X3-V3X2
         VX2=V3X1-V1X3
         VX3=V1X2-V2X1
         A(1,NS)= A(1,NS)+(VX1+DT05*(VG(2)*VX3-VG(3)*VX2))*USDT
         A(2,NS)= A(2,NS)+(VX2+DT05*(VG(3)*VX1-VG(1)*VX3))*USDT
         A(3,NS)= A(3,NS)+(VX3+DT05*(VG(1)*VX2-VG(2)*VX1))*USDT
        ENDDO
       END IF
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2VL1                       source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        RBE2V                         source/constraints/general/rbe2/rbe2v.F
Chd|-- calls ---------------
Chd|        CDI_BCN1                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2D_BCL                     source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE RBE2VL1(NSL   ,ISL   ,X     ,A     ,AR    ,
     1                   V     ,VR    ,JT    ,JR    ,M     ,
     2                   SKEW  ,IRAD  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ISL(*),JT(3),JR(3),M,IRAD
C     REAL
      my_real
     .   X(3,*), A(3,*), AR(3,*), V(3,*), VR(3,*),SKEW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS,ICR
C     REAL
      my_real
     .   XS, YS, ZS,RX, RY,RZ,LRX, LRY,LRZ,RVX,RVY,RVZ,
     .   DVX,DVY,DVZ,VVX,VVY,VVZ,LXS(NSL),LYS(NSL),LZS(NSL)
      my_real
     .   V1X2, V2X1, V2X3, V3X2, V3X1, V1X3,USDT,DT05,
     .   VX1, VX2, VX3,VRG(3),VRL(3),USDT1(3),KTR(3,3)
C----we modify A,AR(*,NS) so that V,VR(*,NS) follow the constraint equations     
C======================================================================|
        USDT = ONE/DT12
      IF ((JT(1)+JT(2)+JT(3))==3) THEN
       DO J = 1,3
       DO I=1,NSL
        NS = ISL(I)
        A(J,NS)= A(J,M)+(V(J,M)-V(J,NS))*USDT
       ENDDO
       ENDDO
      ELSE
       DO I=1,NSL
        NS = ISL(I)
        DVX  =A(1,NS)-(A(1,M)+(V(1,M)-V(1,NS))*USDT)
        DVY  =A(2,NS)-(A(2,M)+(V(2,M)-V(2,NS))*USDT)
        DVZ  =A(3,NS)-(A(3,M)+(V(3,M)-V(3,NS))*USDT)
        VVX  =JT(1)*(SKEW(1)*DVX+SKEW(2)*DVY+SKEW(3)*DVZ)
        VVY  =JT(2)*(SKEW(4)*DVX+SKEW(5)*DVY+SKEW(6)*DVZ)
        VVZ  =JT(3)*(SKEW(7)*DVX+SKEW(8)*DVY+SKEW(9)*DVZ)
        A(1,NS) =A(1,NS)-VVX*SKEW(1)-VVY*SKEW(4)-VVZ*SKEW(7)
        A(2,NS) =A(2,NS)-VVX*SKEW(2)-VVY*SKEW(5)-VVZ*SKEW(8)
        A(3,NS) =A(3,NS)-VVX*SKEW(3)-VVY*SKEW(6)-VVZ*SKEW(9)
       ENDDO
      END IF !((JT(1)+JT(2)+JT(3))==3) THEN
C
      IF ((JR(1)+JR(2)+JR(3)) >0.OR.IRAD ==0 ) THEN
       IF ((JR(1)+JR(2)+JR(3)) >0 ) THEN
        ICR=JR(1)*4 +JR(2)*2 +JR(3)
C---------save VR(*,M), VR(*,NS) before they are modified        
        DO J = 1,3
         VRG(J)=VR(J,M)
        ENDDO
        DO I=1,NSL
         NS = ISL(I)
         LXS(I)=VR(1,NS)
         LYS(I)=VR(2,NS)
         LZS(I)=VR(3,NS)
         VR(1:3,NS)=VR(1:3,NS)+AR(1:3,NS)*DT12
        END DO !I=1,NSL
C-----VR(t+dt)        
        VR(1:3,M)=VRG(1:3)+AR(1:3,M)*DT12
        IF ((JR(1)+JR(2)+JR(3)) < 3) THEN
         CALL RBE2D_BCL(ICR  ,NSL   ,ISL   ,M     ,VR    ,
     1                 SKEW )
        ELSE
         DO I=1,NSL
          NS = ISL(I)
          VR(1,NS)=VR(1,M)
          VR(2,NS)=VR(2,M)
          VR(3,NS)=VR(3,M)
         ENDDO
        ENDIF
        DO I=1,NSL
         NS = ISL(I)
         AR(1,NS)=(VR(1,NS)-LXS(I)) * USDT
         AR(2,NS)=(VR(2,NS)-LYS(I)) * USDT
         AR(3,NS)=(VR(3,NS)-LZS(I)) * USDT
         VR(1,NS)= LXS(I)
         VR(2,NS)= LYS(I)
         VR(3,NS)= LZS(I)
        END DO !I=1,NSL
        VR(1:3,M)=VRG(1:3)
       END IF !IF ((JR(1)+JR(2)+JR(3)) >0 )
C       
       DO J = 1,3
         VRG(J)=VR(J,M)+AR(J,M)*DT12
       ENDDO
       DT05 = HALF*DT2
       IF (IRAD==0) THEN
        DO J = 1,3
         USDT1(J)=USDT*JT(J)
        ENDDO
       ELSE
        DO J = 1,3
         USDT1(J)=USDT
        ENDDO
       END IF
       DO I=1,NSL
        NS = ISL(I)
        RX=VRG(1)
        RY=VRG(2)
        RZ=VRG(3)
        XS=X(1,NS)-X(1,M)
        YS=X(2,NS)-X(2,M)
        ZS=X(3,NS)-X(3,M)
        CALL CDI_BCN1(XS,YS,ZS,JT,JR,SKEW,KTR,IRAD)
        VX1 =KTR(1,1)*RX+KTR(1,2)*RY+KTR(1,3)*RZ
        VX2 =KTR(2,1)*RX+KTR(2,2)*RY+KTR(2,3)*RZ
        VX3 =KTR(3,1)*RX+KTR(3,2)*RY+KTR(3,3)*RZ
        A(1,NS) =A(1,NS)+(VX1+DT05*(VRG(2)*VX3-VRG(3)*VX2))*USDT1(1)
        A(2,NS) =A(2,NS)+(VX2+DT05*(VRG(3)*VX1-VRG(1)*VX3))*USDT1(2)
        A(3,NS) =A(3,NS)+(VX3+DT05*(VRG(1)*VX2-VRG(2)*VX1))*USDT1(3)
       ENDDO
      END IF
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_FRD                      source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|-- calls ---------------
Chd|        CDI_BCN1                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2D_BCL                     source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE RBE2_FRD(NS    ,M     ,X     ,V     ,VR    ,
     1                    JT    ,JR    ,SKEW0 ,ISK   ,IRAD  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NS,JT(3),JR(3),M,IRAD,ISK
C     REAL
      my_real
     .   X(3,*), V(3,*), VR(3,*),SKEW0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NT,NR,ICT,ICR,NSN,ISL(1),K
C     REAL
      my_real
     .   XS, YS, ZS,RX, RY,RZ,LRX, LRY,LRZ,RVX,RVY,RVZ,
     .   DVX,DVY,DVZ,VVX,VVY,VVZ,KTR(3,3),SKEW(LSKEW)
C======================================================================|
        NT=JT(1)+JT(2)+JT(3)
        NR=JR(1)+JR(2)+JR(3)
        ICT=JT(1)*4 +JT(2)*2 +JT(3)
        ICR=JR(1)*4 +JR(2)*2 +JR(3)
	IF (ISK>1) THEN
	 DO K=1,LSKEW
	  SKEW(K)=SKEW0(K)
	 ENDDO
	ELSE
	 DO K=1,LSKEW
	  SKEW(K)=ZERO
	 ENDDO
	  SKEW(1)=ONE
	  SKEW(5)=ONE
	  SKEW(9)=ONE
        ENDIF
      IF (NT>0.AND.NT<3) THEN
       NSN=1
       ISL(1)=NS
       CALL RBE2D_BCL(ICT  ,NSN   ,ISL   ,M     ,V     ,
     1               SKEW  )
      ELSEIF (NT==3) THEN
        V(1,NS)=V(1,M)
        V(2,NS)=V(2,M)
        V(3,NS)=V(3,M)
      ENDIF
C
      IF (NR>0) THEN
       IF (NR<3) THEN
        NSN=1
        ISL(1)=NS
        CALL RBE2D_BCL(ICR  ,NSN   ,ISL   ,M     ,VR    ,
     1                 SKEW )
       ELSEIF (NR==3) THEN
         VR(1,NS)=VR(1,M)
         VR(2,NS)=VR(2,M)
         VR(3,NS)=VR(3,M)
       END IF
      END IF
C
      IF (IRAD==0.OR.NR>0) THEN
        RX=VR(1,M)
        RY=VR(2,M)
        RZ=VR(3,M)
        XS=X(1,NS)-X(1,M)
        YS=X(2,NS)-X(2,M)
        ZS=X(3,NS)-X(3,M)
        CALL CDI_BCN1(XS,YS,ZS,JT,JR,SKEW,KTR,IRAD)
        V(1,NS) =V(1,NS)+KTR(1,1)*RX+KTR(1,2)*RY+KTR(1,3)*RZ
        V(2,NS) =V(2,NS)+KTR(2,1)*RX+KTR(2,2)*RY+KTR(2,3)*RZ
        V(3,NS) =V(3,NS)+KTR(3,1)*RX+KTR(3,2)*RY+KTR(3,3)*RZ
      END IF
C---
      RETURN
      END

Chd|====================================================================
Chd|  VELROT                        source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        FV_IMP                        source/constraints/general/impvel/fv_imp0.F
Chd|        I2RECU0                       source/interfaces/interf/i2_imp2.F
Chd|        I2RECU2                       source/interfaces/interf/i2_imp2.F
Chd|        RBE2D0                        source/constraints/general/rbe2/rbe2v.F
Chd|        RBE2DL3                       source/constraints/general/rbe2/rbe2v.F
Chd|        RBY_IMP7                      source/constraints/general/rbody/rby_impd.F
Chd|        SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        CROSSPRODUCT                  source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE VELROT(VRM,LSM,VS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C PURPOSE: calculate displacement increment of secnd node by displacement increment of main node.
C          the general ideal is that one local coordinate is created by W and the
C          vector (LSM) pointing from main node to secnd node. The local coordinate
C          can be expressed as
C          Z = W,where W=(Wx, Wy, Wz)
C          X = W CROSSPRODUCT LSM
C          Y = Z CROSSPRODUCT X
C          the problem can be describled as LSM rotate around local z axis, then the rotated
C          vector is transformed back to global coordinate.
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   VRM(*), LSM(*), VS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

      INTEGER I , J , K
      my_real
     * ANGELV, RZ(3,3), LOCALZ(3), L, LOCALX(3), LSMUNI(3),TRANS(3,3),
     * LOCALY(3), LSMLOCAL(3), LSMLTR(3), LSMGTR(3)

      DO I = 1 , 3
        VS(I) = ZERO
      END DO

      L = ZERO
      DO I = 1 ,3
        L = L + LSM(I)*LSM(I)
      END DO
      L = SQRT(L)
      IF( L < EM20) THEN
C SECND NODE COINCIDE WITH main NODE, NO VELICITY IS CAUSED BY ROTATION
        RETURN
      END IF

      ANGELV = ZERO
      DO I = 1 , 3
        ANGELV = ANGELV + VRM(I)*VRM(I)
      END DO
      ANGELV = SQRT(ANGELV)
      IF( ANGELV <= EM20) THEN
C NO ROTATION VELOCITY
        RETURN
      END IF

      DO I = 1 , 3
        LSMUNI(I) = LSM(I)/L
      END DO

      DO I = 1 ,3
        LOCALZ(I ) = VRM(I)/ANGELV
      END DO

      CALL CROSSPRODUCT(LOCALZ,LSMUNI,LOCALX)
      L = 0
      DO I = 1 ,3
        L = L + LOCALX(I)*LOCALX(I)
      END DO
      L = SQRT(L)
      IF( L <= EM20 )THEN
C ROTATION AXIS COINCIDIE WITH THE VECTOR POINTING FROM main TO SECND.
C SO NO VELOCITY WILL BE CAUSED BY THIS ROTATION
        RETURN
      END IF
      DO I = 1 , 3
        LOCALX(I) = LOCALX(I)/L
      END DO

      CALL CROSSPRODUCT(LOCALZ,LOCALX,LOCALY)

      DO I = 1 ,3
        TRANS(1,I) = LOCALX(I)
        TRANS(2,I) = LOCALY(I)
        TRANS(3,I) = LOCALZ(I)
      END DO


      DO I = 1 ,3
        LSMLOCAL(I) = ZERO
      END DO

      DO I = 1 ,3
        DO J = 1 , 3
          LSMLOCAL(I) = LSMLOCAL(I) + TRANS(I,J)*LSM(J)
        END DO
      END DO

      DO I = 1 , 3
        DO J = 1 , 3
          RZ(I,J) = ZERO
        END DO
      END DO

      RZ(1,1) = COS(ANGELV)
      RZ(1,2) = SIN(ANGELV)
      RZ(2,1) = -SIN(ANGELV)
      RZ(2,2) = COS(ANGELV)
      RZ(3,3) = ONE

      DO I = 1,3
        LSMLTR(I) = ZERO
      END DO

      DO I = 1 , 3
        DO J = 1 , 3
          LSMLTR(I) = LSMLTR(I) + RZ(J,I)*LSMLOCAL(J)
        END DO
      END DO

      DO I = 1 , 3
        LSMGTR(I) = ZERO
      END DO

      DO I = 1, 3
        DO J = 1 , 3
          LSMGTR(I) = LSMGTR(I) + TRANS(J,I)*LSMLTR(J)
        END DO
      END DO

      DO I = 1 , 3
        VS(I) = LSMGTR(I) - LSM(I)
      END DO

      RETURN

      END SUBROUTINE !VELROT


Chd|====================================================================
Chd|  CROSSPRODUCT                  source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        VELROT                        source/constraints/general/rbe2/rbe2v.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CROSSPRODUCT(X,Y,Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C PURPOSE:
C         CALCULATE CROSSPRODUCT Z = X (X) Y

      my_real
     * X(*),Y(*),Z(*)

      Z(1) = X(2)*Y(3) - Y(2)*X(3)
      Z(2) = -X(1)*Y(3) + Y(1)*X(3)
      Z(3) = X(1)*Y(2) - Y(1)*X(2)

      RETURN

      END ! SUBROUTINE CROSSPRODUCT

Chd|====================================================================
Chd|  RBE2DL3                       source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        RBE2_IMPD                     source/constraints/general/rbe2/rbe2v.F
Chd|-- calls ---------------
Chd|        SELECT_DOF                    source/constraints/general/rbe2/rbe2v.F
Chd|        UPDATE_GLOBV                  source/constraints/general/rbe2/rbe2v.F
Chd|        VELROT                        source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE RBE2DL3(NSN   ,ISL   ,X     ,V     ,VR    ,
     1                   JT    ,JR    ,M     ,SKEW  ,IRAD  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,ISL(*),JT(3),JR(3),M,IRAD
C     REAL
      my_real
     .   X(3,*), V(3,*), VR(3,*),SKEW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NS ,NT,NR,ICT,ICR , J , K,
     .        TDOF1,TDOF2,TDOF3,RDOF1,RDOF2,RDOF3
C     REAL
      my_real
     .   RX, RY,RZ,LRX, LRY,LRZ,RVX,RVY,RVZ,
     .   DVX,DVY,DVZ,VVX,VVY,VVZ,KTR(3,3), VLM(3),LOCALSM(3),GL0BLSM(3),
     .   VLS(3), VRLM(3), VRLS(3),VSROT(3),VRTEMP(3)

C======================================================================|


      NT=JT(1)+JT(2)+JT(3)
      NR=JR(1)+JR(2)+JR(3)
      ICT=JT(1)*4 +JT(2)*2 +JT(3)
      ICR=JR(1)*4 +JR(2)*2 +JR(3)

      CALL SELECT_DOF(ICT, SKEW, TDOF1, TDOF2, TDOF3)
      CALL SELECT_DOF(ICR, SKEW, RDOF1, RDOF2, RDOF3)

C transform velocity of main node from global coordinate to local coordinate
      DO I = 1 , 3
        VLM(I) = ZERO
        VRLM(I) = ZERO
      END DO

      DO I = 1, 3
        DO J = 1 , 3
          VLM(I) = VLM(I) + SKEW((I-1)*3+J)*V(J,M)
          VRLM(I) = VRLM(I) + SKEW((I-1)*3+J)*VR(J,M)
        END DO
      END DO

      DO I = 1 , NSN

        NS = ISL(I)

        DO J = 1 , 3
          GL0BLSM(J) = X(J,NS) - X(J,M)
        END DO

C       transform vector pointing from main node to secnd node to local coordinate
        DO J = 1 , 3
          LOCALSM(J) = ZERO
          DO K = 1 , 3
            LOCALSM(J) = LOCALSM(J) + SKEW((J-1)*3+K)*GL0BLSM(K)
          END DO
        END DO

        DO J = 1 , 3
          VLS(J) = VLM(J) * JT(J)
          VRLS(J) = VRLM(J) * JR(J)
        END DO

        IF( IRAD == 0) THEN ! NASTRAN FORMAT
           CALL VELROT(VRLM, LOCALSM,VSROT)
           IF( JT(1) /= 0 ) VLS(1) = VLS(1) + VSROT(1)
           IF( JT(2) /= 0 ) VLS(2) = VLS(2) + VSROT(2)
           IF( JT(3) /= 0 ) VLS(3) = VLS(3) + VSROT(3)
        ELSE IF( (JR(1)+JR(2)+JR(3))>0) THEN ! RIGID LINK
          DO J = 1, 3
            VRTEMP(J) = VRLM(J) * JR(J)
          END DO
          CALL VELROT(VRTEMP, LOCALSM,VSROT)
          DO J = 1 , 3
            VLS(J) = VLS(J) + VSROT(J)
          END DO

        END IF ! IRAD == 0

C       update dependent DOF(s) by independent DOF(s)

        CALL UPDATE_GLOBV(ICT,NS,VLS,V,SKEW,TDOF1,
     .                     TDOF2,TDOF3)

        CALL UPDATE_GLOBV(ICR,NS,VRLS,VR,SKEW,RDOF1,
     .                     RDOF2,RDOF3)

      END DO ! I = 1 , NSN

C---
      RETURN
      END ! SUBROUTINE RBE2DL3

Chd|====================================================================
Chd|  SELECT_DOF                    source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        RBE2DL3                       source/constraints/general/rbe2/rbe2v.F
Chd|-- calls ---------------
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE SELECT_DOF(ICT, SKEW, J, J1, K)
#include "implicit_f.inc"

C
      INTEGER ICT, J, J1, K
      my_real
     .       SKEW(*)

C WORK VARIABLE

      my_real
     .        EJ(3), EJ1(3)

C-------------------100---------------------
      IF (ICT == 4 ) THEN
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
          CALL L_DIR(EJ,J)
	    J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
C-------------------010---------------------
       ELSEIF (ICT == 2) THEN
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
C-------------------001---------------------
       ELSEIF (ICT == 1) THEN
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
	    J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
C-------------------011---------------------
       ELSEIF (ICT == 3) THEN
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(4)
          EJ1(2)=SKEW(5)
          EJ1(3)=SKEW(6)
          CALL L_DIR(EJ1,J1)
          IF (J1==J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
C-------------------101---------------------
       ELSEIF (ICT == 5) THEN
         EJ(1)=SKEW(7)
         EJ(2)=SKEW(8)
         EJ(3)=SKEW(9)
         CALL L_DIR(EJ,J)
         EJ1(1)=SKEW(1)
         EJ1(2)=SKEW(2)
         EJ1(3)=SKEW(3)
         CALL L_DIR(EJ1,J1)
         IF (J1==J) THEN
          EJ1(J)=ZERO
          CALL L_DIR(EJ1,J1)
         ENDIF
         CALL DIR_RBE2(J    ,J1    ,K     )
C-------------------110---------------------
       ELSEIF (ICT == 6) THEN
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1==J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
       ENDIF

      END !SUBROUTINE SELECT_DOF


Chd|====================================================================
Chd|  UPDATE_GLOBV                  source/constraints/general/rbe2/rbe2v.F
Chd|-- called by -----------
Chd|        RBE2DL3                       source/constraints/general/rbe2/rbe2v.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPDATE_GLOBV(ICT,NS,VLS,V,SKEW,J, J1, K)
#include "implicit_f.inc"
#include "param_c.inc"
      INTEGER ICT, NS, J, J1, K
      my_real
     .       VLS(3), V(3,*),SKEW(LSKEW)

C  WORK VARIABLE
      INTEGER M,N,I
      my_real
     .       SKEWINV(3,3), ARRAY1(2), ARRAY2(2,2), TEMP, SKEW1(3,3)

      DO I=1,3
       SKEWINV(I,1)= SKEW(I)
       SKEWINV(I,2)= SKEW(I+3)
       SKEWINV(I,3)= SKEW(I+6)
      ENDDO

      DO M = 1, 3
        DO N = 1 ,3
          SKEW1(M,N) = SKEW((M-1)*3+N)
        END DO
      END DO

C-------------------100---------------------
      IF (ICT == 4 ) THEN
        V(J,NS) = ONE/SKEW1(1,J)*(VLS(1)-SKEW1(1,J1)*V(J1,NS)
     .            -SKEW1(1,K)*V(K,NS ))
C-------------------010---------------------
      ELSEIF (ICT == 2) THEN
        V(J,NS) = ONE/SKEW1(2,J)*(VLS(2)-SKEW1(2,J1)*V(J1,NS)
     .            -SKEW1(2,K)*V(K,NS ))
C-------------------001---------------------
      ELSEIF (ICT == 1) THEN
        V(J,NS) = ONE/SKEW1(3,J)*(VLS(3)-SKEW1(3,J1)*V(J1,NS)
     .            -SKEW1(3,K)*V(K,NS ))
C-------------------011---------------------
      ELSEIF (ICT == 3) THEN

        ARRAY1(1) = VLS(2) - SKEW1(2,K) * V(K,NS)
        ARRAY1(2) = VLS(3) - SKEW1(3,K) * V(K,NS)

        TEMP = SKEW1(2,J)*SKEW1(3,J1) - SKEW1(2,J1)*SKEW1(3,J)
        ARRAY2(1,1) = SKEW1(3,J1)/TEMP
        ARRAY2(1,2) = -SKEW1(2,J1)/TEMP
        ARRAY2(2,1) = -SKEW1(3,J)/TEMP
        ARRAY2(2,2) = SKEW1(2,J)/TEMP

        V(J,NS)  = ARRAY2(1,1) * ARRAY1(1) + ARRAY2(1,2) * ARRAY1(2)
        V(J1,NS) = ARRAY2(2,1) * ARRAY1(1) + ARRAY2(2,2) * ARRAY1(2)

C-------------------101---------------------
      ELSEIF (ICT == 5) THEN

        ARRAY1(1) = VLS(1) - SKEW1(1,K) * V(K,NS)
        ARRAY1(2) = VLS(3) - SKEW1(3,K) * V(K,NS)

        TEMP = SKEW1(1,J)*SKEW1(3,J1) - SKEW1(1,J1)*SKEW1(3,J)
        ARRAY2(1,1) = SKEW1(3,J1)/TEMP
        ARRAY2(1,2) = -SKEW1(1,J1)/TEMP
        ARRAY2(2,1) = -SKEW1(3,J)/TEMP
        ARRAY2(2,2) = SKEW1(1,J)/TEMP

        V(J,NS)  = ARRAY2(1,1) * ARRAY1(1) + ARRAY2(1,2) * ARRAY1(2)
        V(J1,NS) = ARRAY2(2,1) * ARRAY1(1) + ARRAY2(2,2) * ARRAY1(2)

C-------------------110---------------------
      ELSEIF (ICT == 6) THEN
        ARRAY1(1) = VLS(2) - SKEW1(2,K) * V(K,NS)
        ARRAY1(2) = VLS(1) - SKEW1(1,K) * V(K,NS)

        TEMP = SKEW1(2,J)*SKEW1(1,J1) - SKEW1(2,J1)*SKEW1(1,J)
        ARRAY2(1,1) = SKEW1(1,J1)/TEMP
        ARRAY2(1,2) = -SKEW1(2,J1)/TEMP
        ARRAY2(2,1) = -SKEW1(1,J)/TEMP
        ARRAY2(2,2) = SKEW1(2,J)/TEMP

        V(J,NS)  = ARRAY2(1,1) * ARRAY1(1) + ARRAY2(1,2) * ARRAY1(2)
        V(J1,NS) = ARRAY2(2,1) * ARRAY1(1) + ARRAY2(2,2) * ARRAY1(2)

C-------------------110---------------------
      ELSEIF( ICT == 7 ) THEN
        DO M = 1 , 3
          V(M,NS) = ZERO
          DO N = 1 , 3
            V(M,NS) = V(M,NS) + SKEWINV(M,N)*VLS(N)
          END DO
        END DO
      ENDIF

      RETURN

      END ! SUBROUTINE CONDENSE

