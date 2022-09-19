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
Chd|  I2FOR3N                       source/interfaces/interf/i2for3.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2FOR3N(NSN    ,NMN    ,A      ,IRECT  ,CRST   ,
     2                   MSR    ,NSV    ,IRTL   ,MS     ,WEIGHT ,
     3                   STIFN  ,MMASS  ,IDEL2  ,SMASS  ,X      ,
     4                   V      ,FSAV   ,FNCONT ,H3D_DATA, CSTS_BIS,
     5                   FNCONTP,FTCONTP )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*), IDEL2
C     REAL
      my_real
     .    X(*),V(*),A(*),MS(*),CRST(2,*),STIFN(*),MMASS(*),SMASS(*),
     .    FSAV(*),FNCONT(3,*),CSTS_BIS(2,*),FNCONTP(3,*),FTCONTP(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, I3, J3, I2, J2, I1, J1, II, L, JJ
C     REAL
      my_real
     .   H(4),XMSJ, SS, ST, XMSI, FXI, FYI, FZI,SP,SM,TP,TM,H2(4)
C-----------------------------------------------
      NIR=2
      IF(N2D==0)NIR=4
C
Cel spmd pre-traitement sur noeuds main + sauvegarde de la masse
Cel sauvegarde de la masse initiale
C
      IF(IMPL_S>0) THEN
      DO II=1,NSN
       I=NSV(II)
       IF(I>0)THEN
        L=IRTL(II)
C
        I3=3*I
        I2=I3-1
        I1=I2-1
C
        XMSI=MS(I)*WEIGHT(I)
        FXI=A(I1)*WEIGHT(I)
        FYI=A(I2)*WEIGHT(I)
        FZI=A(I3)*WEIGHT(I)
C
        SS=CRST(1,II)
        ST=CRST(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + ST)
        TM=FOURTH*(ONE - ST)
        H(1)=TM*SM
        H(2)=TM*SP
        H(3)=TP*SP
        H(4)=TP*SM
C
C       Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
        SS=CSTS_BIS(1,II)
        ST=CSTS_BIS(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + ST)
        TM=FOURTH*(ONE - ST)
        H2(1)=TM*SM
        H2(2)=TM*SP
        H2(3)=TP*SP
        H2(4)=TP*SM
C
        IF(IRECT(3,L)==IRECT(4,L)) THEN
	 NIR = 3
         H(3)=ONE-H(1)-H(2)
         H(4)=ZERO
         H2(3)=ONE-H2(1)-H2(2)
         H2(4)=ZERO
        ELSE
         NIR=4
        ENDIF
C
        DO JJ=1,NIR
         J=IRECT(JJ,L)
         J3=3*J
         J2=J3-1
         J1=J2-1
         A(J1)=A(J1)+FXI*H(JJ)
         A(J2)=A(J2)+FYI*H(JJ)
         A(J3)=A(J3)+FZI*H(JJ)
         MS(J)=MS(J)+XMSI*H2(JJ)
         STIFN(J)=STIFN(J)+ABS(STIFN(I)*H(JJ)*WEIGHT(I))
        ENDDO
C
C---    output of tied contact forces
 	CALL I2FORCES(X      ,V	,A	 ,MS	  ,I	  ,
     .	              IRECT(1,L),H	,NIR     ,FSAV    ,FNCONT ,
     .                FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C      
        IF(IRODDL==0)THEN
          IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
          MS(I)=ZERO
          STIFN(I)=EM20
          A(I1)=ZERO
          A(I2)=ZERO
          A(I3)=ZERO
        ENDIF

       ENDIF
C----
      ENDDO
      ELSE
      DO II=1,NSN
       I=NSV(II)
       IF(I>0)THEN
        L=IRTL(II)
C
        I3=3*I
        I2=I3-1
        I1=I2-1
C
        XMSI=MS(I)*WEIGHT(I)
        FXI=A(I1)*WEIGHT(I)
        FYI=A(I2)*WEIGHT(I)
        FZI=A(I3)*WEIGHT(I)
C
        SS=CRST(1,II)
        ST=CRST(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + ST)
        TM=FOURTH*(ONE - ST)
        H(1)=TM*SM
        H(2)=TM*SP
        H(3)=TP*SP
        H(4)=TP*SM
C
C       Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
        SS=CSTS_BIS(1,II)
        ST=CSTS_BIS(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + ST)
        TM=FOURTH*(ONE - ST)
        H2(1)=TM*SM
        H2(2)=TM*SP
        H2(3)=TP*SP
        H2(4)=TP*SM
C
        DO JJ=1,NIR
         J=IRECT(JJ,L)
         J3=3*J
         J2=J3-1
         J1=J2-1
         A(J1)=A(J1)+FXI*H(JJ)
         A(J2)=A(J2)+FYI*H(JJ)
         A(J3)=A(J3)+FZI*H(JJ)
         MS(J)=MS(J)+XMSI*H2(JJ)
         STIFN(J)=STIFN(J)+ABS(STIFN(I)*H(JJ)*WEIGHT(I))
        ENDDO
C
C---    output or tied contact forces
 	CALL I2FORCES(X      ,V	,A	 ,MS	  ,I	  ,
     .	              IRECT(1,L),H	,NIR     ,FSAV    ,FNCONT ,
     .                FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C
        IF(IRODDL==0)THEN
          IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
          MS(I)=ZERO
          STIFN(I)=EM20
          A(I1)=ZERO
          A(I2)=ZERO
          A(I3)=ZERO
        ENDIF
C----
       ENDIF
      ENDDO
      ENDIF
C
      RETURN
      END
Cel|         INTTI12F                        /interf/intti12.F
Chd|====================================================================
Chd|  I2FOR3                        source/interfaces/interf/i2for3.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2FOR3(NSN     ,NMN     ,A     ,IRECT  ,CRST    ,
     2                  MSR     ,NSV     ,IRTL  ,MS     ,WEIGHT  ,
     3                  STIFN   ,MMASS   ,X     ,V      ,FSAV    ,
     4                  FNCONT  ,H3D_DATA, CSTS_BIS,FNCONTP ,FTCONTP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*)
C     REAL
      my_real
     .   X(*),V(*),A(*),MS(*),CRST(2,*),STIFN(*),MMASS(*),FSAV(*),
     .   FNCONT(3,*),CSTS_BIS(2,*),FNCONTP(3,*) ,FTCONTP(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, I3, J3, I2, J2, I1, J1, II, L, JJ
C     REAL
      my_real
     .   H(4),
     .   XMSJ,SS,ST,XMSI,FXI,FYI,FZI,SP,SM,TP,TM,H2(4)
C=======================================================================
      NIR=2
      IF(N2D==0)NIR=4
C
Cel spmd pre-traitement sur noeuds main + sauvegarde de la masse
      IF (IMACH==3) THEN
       ELSE
Cel sauvegarde de la masse initiale
        DO II=1,NMN
          J=MSR(II)
          MMASS(II)=MS(J)
        ENDDO
       ENDIF
      IF(IMPL_S>0) THEN
        DO II=1,NSN
          I=NSV(II)
          L=IRTL(II)
          I3=3*I
          I2=I3-1
          I1=I2-1
C
          XMSI=MS(I)*WEIGHT(I)
          FXI=A(I1)*WEIGHT(I)
          FYI=A(I2)*WEIGHT(I)
          FZI=A(I3)*WEIGHT(I)
C
          SS=CRST(1,II)
          ST=CRST(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + ST)
          TM=FOURTH*(ONE - ST)
          IF(IRECT(3,L)==IRECT(4,L)) THEN
	   NIR = 3
           H(1)=TM*SM
           H(2)=TM*SP
           H(3)=ONE-H(1)-H(2)
          ELSE
           NIR=4
           H(1)=TM*SM
           H(2)=TM*SP
           H(3)=TP*SP
           H(4)=TP*SM
          ENDIF

C        Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
          SS=CSTS_BIS(1,II)
          ST=CSTS_BIS(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + ST)
          TM=FOURTH*(ONE - ST)
          IF(IRECT(3,L)==IRECT(4,L)) THEN
	   NIR = 3
           H2(1)=TM*SM
           H2(2)=TM*SP
           H2(3)=ONE-H2(1)-H2(2)
          ELSE
           NIR=4
           H2(1)=TM*SM
           H2(2)=TM*SP
           H2(3)=TP*SP
           H2(4)=TP*SM
          ENDIF
C
          DO JJ=1,NIR
           J=IRECT(JJ,L)
           J3=3*J
           J2=J3-1
           J1=J2-1
           A(J1)=A(J1)+FXI*H(JJ)
           A(J2)=A(J2)+FYI*H(JJ)
           A(J3)=A(J3)+FZI*H(JJ)
           MS(J)=MS(J)+XMSI*H2(JJ)
           STIFN(J)=STIFN(J)+ABS(STIFN(I)*H(JJ)*WEIGHT(I))
          ENDDO
C
C---      output of tied contact forces
 	  CALL I2FORCES(X      ,V	,A	 ,MS	  ,I	  ,
     .	               IRECT(1,L),H	,NIR     ,FSAV    ,FNCONT ,
     .                 FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C
          IF(IRODDL==0)THEN
            STIFN(I)=EM20
            A(I1)=ZERO
            A(I2)=ZERO
            A(I3)=ZERO
          ENDIF
C----
        ENDDO
      ELSE
C
        DO II=1,NSN
          I=NSV(II)
          L=IRTL(II)
C
          I3=3*I
          I2=I3-1
          I1=I2-1
C
          XMSI=MS(I)*WEIGHT(I)
          FXI=A(I1)*WEIGHT(I)
          FYI=A(I2)*WEIGHT(I)
          FZI=A(I3)*WEIGHT(I)
C
          SS=CRST(1,II)
          ST=CRST(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + ST)
          TM=FOURTH*(ONE - ST)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=TP*SP
          H(4)=TP*SM

C        Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
          SS=CSTS_BIS(1,II)
          ST=CSTS_BIS(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + ST)
          TM=FOURTH*(ONE - ST)
          H2(1)=TM*SM
          H2(2)=TM*SP
          H2(3)=TP*SP
          H2(4)=TP*SM
C
          DO JJ=1,NIR
           J=IRECT(JJ,L)
           J3=3*J
           J2=J3-1
           J1=J2-1
           A(J1)=A(J1)+FXI*H(JJ)
           A(J2)=A(J2)+FYI*H(JJ)
           A(J3)=A(J3)+FZI*H(JJ)
           MS(J)=MS(J)+XMSI*H2(JJ)
           STIFN(J)=STIFN(J)+ABS(STIFN(I)*H(JJ)*WEIGHT(I))
          ENDDO
C
C---      output of tied contact forces
 	  CALL I2FORCES(X      ,V	,A	 ,MS	  ,I	  ,
     .	               IRECT(1,L),H	,NIR     ,FSAV    ,FNCONT ,
     .                 FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C
          IF(IRODDL==0)THEN
            STIFN(I)=EM20
            A(I1)=ZERO
            A(I2)=ZERO
            A(I3)=ZERO
          ENDIF
C----
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  I2FOR3O                       source/interfaces/interf/i2for3.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2FOR3O(NSN      ,NMN     ,A        ,IRECT  ,CRST    ,
     2                   MSR      ,NSV     ,IRTL     ,MS     ,WEIGHT  ,
     3                   STIFN    ,MMASS   ,X        ,V      ,FSAV    ,
     4                   FNCONT   ,CSTS_BIS,H3D_DATA ,FNCONTP,FTCONTP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*)
C     REAL
      my_real
     .   X(*),V(*),A(*),CRST(2,*),MS(*), STIFN(*), MMASS(*),FSAV(*),
     .   FNCONT(3,*), CSTS_BIS(2,*),FNCONTP(3,*),FTCONTP(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, I3, J3, I2, J2, I1, J1, II, L, JJ
C     REAL
      my_real
     .   H(4),XMSJ, SS, ST, XMSI, FXI, FYI, FZI,SP,SM,TP,TM, H2(4)
C-----------------------------------------------
      NIR=2
      IF(N2D==0)NIR=4
C
Cel spmd pre-traitement sur noeuds main + sauvegarde de la masse
      IF (IMACH==3) THEN
      ELSE
Cel sauvegarde de la masse initiale
        DO II=1,NMN
          J=MSR(II)
          MMASS(II)=MS(J)
        ENDDO
      ENDIF
C
      IF(IMPL_S>0) THEN
        DO II=1,NSN
         I=NSV(II)
          L=IRTL(II)
          I3=3*I
          I2=I3-1
          I1=I2-1
C
          XMSI=MS(I)*WEIGHT(I)
          FXI=A(I1)*WEIGHT(I)
          FYI=A(I2)*WEIGHT(I)
          FZI=A(I3)*WEIGHT(I)
C
          SS=CRST(1,II)
          ST=CRST(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + ST)
          TM=FOURTH*(ONE - ST)
          IF(IRECT(3,L)==IRECT(4,L)) THEN
	   NIR = 3
           H(1)=TM*SM
           H(2)=TM*SP
           H(3)=ONE-H(1)-H(2)
          ELSE
           NIR=4
           H(1)=TM*SM
           H(2)=TM*SP
           H(3)=TP*SP
           H(4)=TP*SM
          ENDIF

C        Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
          SS=CSTS_BIS(1,II)
          ST=CSTS_BIS(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + ST)
          TM=FOURTH*(ONE - ST)
          IF(IRECT(3,L)==IRECT(4,L)) THEN
	   NIR = 3
           H2(1)=TM*SM
           H2(2)=TM*SP
           H2(3)=ONE-H2(1)-H2(2)
          ELSE
           NIR=4
           H2(1)=TM*SM
           H2(2)=TM*SP
           H2(3)=TP*SP
           H2(4)=TP*SM
          ENDIF
C
          DO JJ=1,NIR
           J=IRECT(JJ,L)
           J3=3*J
           J2=J3-1
           J1=J2-1
           A(J1)=A(J1)+FXI*H(JJ)
           A(J2)=A(J2)+FYI*H(JJ)
           A(J3)=A(J3)+FZI*H(JJ)
           MS(J)=MS(J)+XMSI*H2(JJ)
           STIFN(J)=STIFN(J)+ABS(STIFN(I)*H(JJ)*WEIGHT(I))
          ENDDO
c
C---      output of tied contact forces
 	  CALL I2FORCES(X      ,V	,A	 ,MS	  ,I	  ,
     .	              IRECT(1,L),H	,NIR     ,FSAV    ,FNCONT ,
     .                FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
c
          STIFN(I)=EM20
          A(I1)=ZERO
          A(I2)=ZERO
          A(I3)=ZERO
C----
        ENDDO
      ELSE
        DO II=1,NSN
          I=NSV(II)
          L=IRTL(II)
C
          I3=3*I
          I2=I3-1
          I1=I2-1
C
          XMSI=MS(I)*WEIGHT(I)
          FXI=A(I1)*WEIGHT(I)
          FYI=A(I2)*WEIGHT(I)
          FZI=A(I3)*WEIGHT(I)
C
          SS=CRST(1,II)
          ST=CRST(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + ST)
          TM=FOURTH*(ONE - ST)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=TP*SP
          H(4)=TP*SM

C        Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
          SS=CSTS_BIS(1,II)
          ST=CSTS_BIS(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + ST)
          TM=FOURTH*(ONE - ST)
          H2(1)=TM*SM
          H2(2)=TM*SP
          H2(3)=TP*SP
          H2(4)=TP*SM
C
          DO JJ=1,NIR
           J=IRECT(JJ,L)
           J3=3*J
           J2=J3-1
           J1=J2-1
           A(J1)=A(J1)+FXI*H(JJ)
           A(J2)=A(J2)+FYI*H(JJ)
           A(J3)=A(J3)+FZI*H(JJ)
           MS(J)=MS(J)+XMSI*H2(JJ)
           STIFN(J)=STIFN(J)+ABS(STIFN(I)*H(JJ)*WEIGHT(I))
          ENDDO
C
C---      output of tied contact forces
 	  CALL I2FORCES(X      ,V	,A	 ,MS	  ,I	  ,
     .	              IRECT(1,L),H	,NIR     ,FSAV    ,FNCONT ,
     .                FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C
          STIFN(I)=EM20
          A(I1)=ZERO
          A(I2)=ZERO
          A(I3)=ZERO
C----
C
        ENDDO
      ENDIF
C-----
      RETURN
      END

Chd|====================================================================
Chd|  I2MOM3N                       source/interfaces/interf/i2for3.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2MOM3N(NSN ,NMN     ,AR      ,IRECT    ,CRST    ,
     2               MSR     ,NSV     ,IRTL    ,IN       ,MS      ,
     3               A       ,X       ,WEIGHT  ,STIFR    ,STIFN   ,
     4               IDEL2   ,SMASS   ,SINER   ,NMAS     ,ADI     ,
     4               MINER   ,H3D_DATA,CSTS_BIS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*), IDEL2
C     REAL
      my_real
     .   A(3,*), AR(3,*),CRST(2,*), MS(*),
     .   X(3,*),IN(*),STIFR(*),STIFN(*), SMASS(*), SINER(*),
     .   NMAS(*),ADI(*),MINER(*),CSTS_BIS(2,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, I3, J3, I2, J2, I1, J1, II, L, JJ, W,NIR
C     REAL
      my_real
     .   H(4), XMSJ, SS, ST, XMSI, FXI, FYI, FZI, MXI, MYI, MZI,INS,
     .   X0,X1,X2,X3,X4,Y0,Y1,Y2,Y3,Y4,Z0,Z1,Z2,Z3,Z4,AA,
     .   XC0,YC0,ZC0,SP,SM,TP,TM,XC,YC,ZC,
     .   STF,AI,H2(4)
C-----------------------------------------------
C     NMAS(II) initialise a MS(J) dans resol_init
      IF(ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
        DO II=1,NMN
          J=MSR(II)
          ADI(J) = ADI(J)*NMAS(II)
        ENDDO
      ENDIF
C
      IF(IMPL_S>0) THEN
      DO II=1,NSN
       I=NSV(II)
       IF(I>0)THEN
        L=IRTL(II)
C
        SS=CRST(1,II)
        ST=CRST(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + ST)
        TM=FOURTH*(ONE - ST)
        H(1)=TM*SM
        H(2)=TM*SP
        H(3)=TP*SP
        H(4)=TP*SM
C
C       Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
        SS=CSTS_BIS(1,II)
        ST=CSTS_BIS(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + ST)
        TM=FOURTH*(ONE - ST)
        H2(1)=TM*SM
        H2(2)=TM*SP
        H2(3)=TP*SP
        H2(4)=TP*SM
C
        IF (IRECT(3,L)==IRECT(4,L)) THEN
         NIR=3
         H(3)=ONE-H(1)-H(2)
         H(4)=ZERO
         H2(3)=ONE-H2(1)-H2(2)
         H2(4)=ZERO
        ELSE
         NIR=4
        ENDIF
C
        XC=ZERO
        YC=ZERO
        ZC=ZERO
        DO JJ=1,NIR
          J=IRECT(JJ,L)
          XC=XC+X(1,J)*H(JJ)
          YC=YC+X(2,J)*H(JJ)
          ZC=ZC+X(3,J)*H(JJ)
        ENDDO
C
        X0 = X(1,I)
        Y0 = X(2,I)
        Z0 = X(3,I)
C
        XC0=X0-XC
        YC0=Y0-YC
        ZC0=Z0-ZC
C
        AA = XC0*XC0 + YC0*YC0 + ZC0*ZC0
        INS = IN(I) + AA * MS(I)
        STF = STIFR(I) + AA * STIFN(I)
C
        FXI=A(1,I)
        FYI=A(2,I)
        FZI=A(3,I)
C
        MXI = AR(1,I) + YC0 * FZI - ZC0 * FYI
        MYI = AR(2,I) + ZC0 * FXI - XC0 * FZI
        MZI = AR(3,I) + XC0 * FYI - YC0 * FXI
C
        W = WEIGHT(I)
        AI=AA * MS(I) * W
        IF (ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
          DO JJ=1,NIR
            J=IRECT(JJ,L)
            ADI(J)=ADI(J)+AI*H(JJ)
          END DO
        END IF
        DO JJ=1,NIR
         J=IRECT(JJ,L)
         IF (MINER(J)>EM20) THEN
          AR(1,J)=AR(1,J)+MXI*H(JJ)*W
          AR(2,J)=AR(2,J)+MYI*H(JJ)*W
          AR(3,J)=AR(3,J)+MZI*H(JJ)*W
          IN(J)=IN(J)+INS*H2(JJ)*W
          STIFR(J)=STIFR(J)+ABS(STF*H(JJ)*W)
         END IF
        ENDDO
        STIFR(I)=EM20
        IF(IDEL2/=0.AND.IN(I)/=ZERO)SINER(II)=IN(I)
        IN(I)=ZERO
        STIFN(I)=EM20
        IF(IDEL2/=0.AND.MS(I)/=ZERO)SMASS(II)=MS(I)
        MS(I)=ZERO
        A(1,I)=ZERO
        A(2,I)=ZERO
        A(3,I)=ZERO
       ENDIF
C
      ENDDO
      ELSE
      DO II=1,NSN
       I=NSV(II)
       IF(I>0)THEN
        L=IRTL(II)
C
        SS=CRST(1,II)
        ST=CRST(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + ST)
        TM=FOURTH*(ONE - ST)
        H(1)=TM*SM
        H(2)=TM*SP
        H(3)=TP*SP
        H(4)=TP*SM
C
        SS=CRST(1,II)
        ST=CRST(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + ST)
        TM=FOURTH*(ONE - ST)
        H2(1)=TM*SM
        H2(2)=TM*SP
        H2(3)=TP*SP
        H2(4)=TP*SM
C
        X0 = X(1,I)
        Y0 = X(2,I)
        Z0 = X(3,I)
C
        X1 = X(1,IRECT(1,L))
        Y1 = X(2,IRECT(1,L))
        Z1 = X(3,IRECT(1,L))
        X2 = X(1,IRECT(2,L))
        Y2 = X(2,IRECT(2,L))
        Z2 = X(3,IRECT(2,L))
        X3 = X(1,IRECT(3,L))
        Y3 = X(2,IRECT(3,L))
        Z3 = X(3,IRECT(3,L))
        X4 = X(1,IRECT(4,L))
        Y4 = X(2,IRECT(4,L))
        Z4 = X(3,IRECT(4,L))
C
        XC = X1 * H(1) + X2 * H(2) + X3 * H(3) + X4 * H(4)
        YC = Y1 * H(1) + Y2 * H(2) + Y3 * H(3) + Y4 * H(4)
        ZC = Z1 * H(1) + Z2 * H(2) + Z3 * H(3) + Z4 * H(4)
C
        XC0=X0-XC
        YC0=Y0-YC
        ZC0=Z0-ZC
C
        AA = XC0*XC0 + YC0*YC0 + ZC0*ZC0
        INS = IN(I) + AA * MS(I)
        STF = STIFR(I) + AA * STIFN(I)
C
        FXI=A(1,I)
        FYI=A(2,I)
        FZI=A(3,I)
C
        MXI = AR(1,I) + YC0 * FZI - ZC0 * FYI
        MYI = AR(2,I) + ZC0 * FXI - XC0 * FZI
        MZI = AR(3,I) + XC0 * FYI - YC0 * FXI
C
        W = WEIGHT(I)
        AI=AA * MS(I) * W
        IF (ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
          DO JJ=1,4
            J=IRECT(JJ,L)
            ADI(J)=ADI(J)+AI*H(JJ)
          END DO
        END IF
        DO JJ=1,4
         J=IRECT(JJ,L)
         IF (MINER(J)>EM20) THEN
           AR(1,J)=AR(1,J)+MXI*H(JJ)*W
           AR(2,J)=AR(2,J)+MYI*H(JJ)*W
           AR(3,J)=AR(3,J)+MZI*H(JJ)*W
           IN(J)=IN(J)+INS*H2(JJ)*W
           STIFR(J)=STIFR(J)+ABS(STF*H(JJ)*W)
         END IF
        ENDDO
        STIFR(I)=EM20
        IF(IDEL2/=0.AND.IN(I)/=0.)SINER(II)=IN(I)
        IN(I)=ZERO
        STIFN(I)=EM20
        IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
        MS(I)=ZERO
        A(1,I)=ZERO
        A(2,I)=ZERO
        A(3,I)=ZERO
       ENDIF
C
      ENDDO
      ENDIF
C
C Traitement specifique pour ADI
C
      IF(ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          J=MSR(II)
          ADI(J) = ADI(J)/MAX(EM20,NMAS(II))
        ENDDO
      ENDIF
C
      RETURN
      END
C==========================================================================
Chd|====================================================================
Chd|  I2MOM3                        source/interfaces/interf/i2for3.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2MOM3(NSN,NMN,AR ,IRECT ,CRST,
     2                  MSR,NSV,IRTL,IN    ,MS  ,
     3                  A  ,X  ,WEIGHT,STIFR,STIFN,
     4                  MINER,CSTS_BIS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*)
C     REAL
      my_real
     .   A(3,*), AR(3,*), CRST(2,*), MS(*),
     .   X(3,*), IN(*), STIFR(*), STIFN(*),MINER(*),CSTS_BIS(2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, I3, J3, I2, J2, I1, J1, II, L, JJ, W,NIR
C     REAL
      my_real
     .   H(4), XMSJ, SS, ST, XMSI, FXI, FYI, FZI, MXI, MYI, MZI,INS,
     .   X0,X1,X2,X3,X4,Y0,Y1,Y2,Y3,Y4,Z0,Z1,Z2,Z3,Z4,AA,STF,
     .   XC0,YC0,ZC0,SP,SM,TP,TM,XC,YC,ZC,H2(4)
C-----------------------------------------------
      IF(IMPL_S>0) THEN
      DO II=1,NSN
       I=NSV(II)
       IF(I>0)THEN
        L=IRTL(II)
C
        SS=CRST(1,II)
        ST=CRST(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + ST)
        TM=FOURTH*(ONE - ST)
        IF (IRECT(3,L)==IRECT(4,L)) THEN
         NIR=3
         H(1)=TM*SM
         H(2)=TM*SP
         H(3)=ONE-H(1)-H(2)
        ELSE
         NIR=4
         H(1)=TM*SM
         H(2)=TM*SP
         H(3)=TP*SP
         H(4)=TP*SM
        ENDIF

C       Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
        SS=CSTS_BIS(1,II)
        ST=CSTS_BIS(2,II)
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + ST)
        TM=FOURTH*(ONE - ST)
        IF (IRECT(3,L)==IRECT(4,L)) THEN
         NIR=3
         H2(1)=TM*SM
         H2(2)=TM*SP
         H2(3)=ONE-H2(1)-H2(2)
        ELSE
         NIR=4
         H2(1)=TM*SM
         H2(2)=TM*SP
         H2(3)=TP*SP
         H2(4)=TP*SM
        ENDIF
C
        XC=ZERO
        YC=ZERO
        ZC=ZERO
        DO JJ=1,NIR
          J=IRECT(JJ,L)
          XC=XC+X(1,J)*H(JJ)
          YC=YC+X(2,J)*H(JJ)
          ZC=ZC+X(3,J)*H(JJ)
        ENDDO
C
        X0 = X(1,I)
        Y0 = X(2,I)
        Z0 = X(3,I)
C
        XC0=X0-XC
        YC0=Y0-YC
        ZC0=Z0-ZC
C
        AA = XC0*XC0 + YC0*YC0 + ZC0*ZC0
        INS = IN(I) + AA * MS(I)
        STF = STIFR(I) + AA * STIFN(I)
C
        FXI=A(1,I)
        FYI=A(2,I)
        FZI=A(3,I)
C
        MXI = AR(1,I) + YC0 * FZI - ZC0 * FYI
        MYI = AR(2,I) + ZC0 * FXI - XC0 * FZI
        MZI = AR(3,I) + XC0 * FYI - YC0 * FXI
C
        W = WEIGHT(I)
        DO JJ=1,NIR
         J=IRECT(JJ,L)
         IF (MINER(J)>EM20) THEN
          AR(1,J)=AR(1,J)+MXI*H(JJ)*W
          AR(2,J)=AR(2,J)+MYI*H(JJ)*W
          AR(3,J)=AR(3,J)+MZI*H(JJ)*W
          IF(TT==ZERO) IN(J)=IN(J)+INS*H2(JJ)*W
          STIFR(J)=STIFR(J)+ABS(STF*H(JJ)*W)
         END IF
        ENDDO
        STIFR(I)=EM20
        IN(I)=ZERO
        STIFN(I)=EM20
        A(1,I)=ZERO
        A(2,I)=ZERO
        A(3,I)=ZERO
       ENDIF
C
      ENDDO
      ELSE
      DO II=1,NSN
      I=NSV(II)
      L=IRTL(II)
C
      SS=CRST(1,II)
      ST=CRST(2,II)
      SP=ONE + SS
      SM=ONE - SS
      TP=FOURTH*(ONE + ST)
      TM=FOURTH*(ONE - ST)
      H(1)=TM*SM
      H(2)=TM*SP
      H(3)=TP*SP
      H(4)=TP*SM

C     Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
      SS=CSTS_BIS(1,II)
      ST=CSTS_BIS(2,II)
      SP=ONE + SS
      SM=ONE - SS
      TP=FOURTH*(ONE + ST)
      TM=FOURTH*(ONE - ST)
      H2(1)=TM*SM
      H2(2)=TM*SP
      H2(3)=TP*SP
      H2(4)=TP*SM
C
      X0 = X(1,I)
      Y0 = X(2,I)
      Z0 = X(3,I)
C
      X1 = X(1,IRECT(1,L))
      Y1 = X(2,IRECT(1,L))
      Z1 = X(3,IRECT(1,L))
      X2 = X(1,IRECT(2,L))
      Y2 = X(2,IRECT(2,L))
      Z2 = X(3,IRECT(2,L))
      X3 = X(1,IRECT(3,L))
      Y3 = X(2,IRECT(3,L))
      Z3 = X(3,IRECT(3,L))
      X4 = X(1,IRECT(4,L))
      Y4 = X(2,IRECT(4,L))
      Z4 = X(3,IRECT(4,L))
C
      XC = X1 * H(1) + X2 * H(2) + X3 * H(3) + X4 * H(4)
      YC = Y1 * H(1) + Y2 * H(2) + Y3 * H(3) + Y4 * H(4)
      ZC = Z1 * H(1) + Z2 * H(2) + Z3 * H(3) + Z4 * H(4)
C
      XC0=X0-XC
      YC0=Y0-YC
      ZC0=Z0-ZC
C
      AA = XC0*XC0 + YC0*YC0 + ZC0*ZC0
      INS = IN(I) + AA * MS(I)
      STF = STIFR(I) + AA * STIFN(I)
C
      FXI=A(1,I)
      FYI=A(2,I)
      FZI=A(3,I)
C
      MXI = AR(1,I) + YC0 * FZI - ZC0 * FYI
      MYI = AR(2,I) + ZC0 * FXI - XC0 * FZI
      MZI = AR(3,I) + XC0 * FYI - YC0 * FXI
C
      W = WEIGHT(I)
      DO JJ=1,4
       J=IRECT(JJ,L)
        IF (MINER(J)>EM20) THEN
         AR(1,J)=AR(1,J)+MXI*H(JJ)*W
         AR(2,J)=AR(2,J)+MYI*H(JJ)*W
         AR(3,J)=AR(3,J)+MZI*H(JJ)*W
         IF(TT==ZERO) IN(J)=IN(J)+INS*H2(JJ)*W
         STIFR(J)=STIFR(J)+ABS(STF*H(JJ)*W)
        END IF
      ENDDO
      STIFR(I)=EM20
      IN(I)=ZERO
      STIFN(I)=EM20
      A(1,I)=ZERO
      A(2,I)=ZERO
      A(3,I)=ZERO
C
      ENDDO
      ENDIF
C
      RETURN
      END
C==========================================================================
Chd|====================================================================
Chd|  I2FOMO3                       source/interfaces/interf/i2for3.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2FOMO3(NSN     ,NMN     ,A      ,IRECT  ,DPARA   ,
     2                   MSR     ,NSV     ,IRTL   ,MS     ,WEIGHT  ,
     3                   AR      ,IN      ,X      ,STIFN  ,STIFR   ,
     4                   ILEV    ,DMAST   ,ADM    ,MMASS  ,IDEL2   ,
     5                   SMASS   ,SINER   ,V      ,CRST   ,FSAV    ,
     6                   FNCONT  ,H3D_DATA,FNCONTP,FTCONTP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,ILEV,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*), IDEL2
C     REAL
      my_real
     .   X(3,*),V(3,*),A(3,*),AR(3,*), MMASS(*), CRST(2,*),
     .   DPARA(7,*), MS(*), IN(*),STIFN(*),STIFR(*),DMAST,ADM(*),
     .   SMASS(*), SINER(*),FSAV(*), FNCONT(3,*), FNCONTP(3,*),
     .   FTCONTP(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, J1,J2,J3,J4, II, L, JJ
C     REAL
      my_real
     .   H(4),XM(4),YM(4),ZM(4),
     .   XMSJ, SS, ST, XMSI, FXS, FYS, FZS,SP,SM,TP,TM,FACM,
     .   MX,MY,MZ,DET,FX0,FY0,FZ0,INS,
     .   X0,X1,X2,X3,X4,XS,Y0,Y1,Y2,Y3,Y4,YS,Z0,Z1,Z2,Z3,Z4,ZS,
     .   X12,X22,X32,X42,Y12,Y22,Y32,Y42,Z12,Z22,Z32,Z42,
     .   XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,S,T,
     .   A1,A2,A3,B1,B2,B3,C1,C2,C3,MR,MRX,MRY,MRZ,INX,INY,INZ,STF
C=======================================================================
      NIR=4
C
C Traitement specifique pour DMAS
C
      IF(ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS >0.AND.ILEV==1) THEN
        DO II=1,NMN
          J=MSR(II)
          ADM(J) = ADM(J)*MMASS(II)
        ENDDO
      ENDIF

C------------------------------
C     FORCES ET MOMENTS DES NOEUDS SECONDS 
C     TRANSMIS AUX NOEUDS MAINS SOUS 
C     FORME DE FORCES 
C
C     MASSES ET INERTIES DES NOEUDS SECONDS 
C     TRANSMISES AUX NOEUDS MAINS SOUS 
C     FORME DE MASSES 
C------------------------------
      IF(IMPL_S>0) THEN
      DO II=1,NSN
       I=NSV(II)
       IF(I>0)THEN
        L=IRTL(II)
C
        S = CRST(1,II)
        T = CRST(2,II)
        SP=ONE + S
        SM=ONE - S
        TP=FOURTH*(ONE + T)
        TM=FOURTH*(ONE - T)
C
        H(1)=ONE/NIR
        H(2)=ONE/NIR
        H(3)=ONE/NIR
        H(4)=ONE/NIR
C
        NIR=4
        DO JJ=1,NIR
         J=IRECT(JJ,L)
         XM(JJ)=X(1,J)
         YM(JJ)=X(2,J)
         ZM(JJ)=X(3,J)
        ENDDO
        IF(IRECT(3,L)==IRECT(4,L)) THEN
         NIR=3
         XM(4)=ZERO
         YM(4)=ZERO
         ZM(4)=ZERO
        ENDIF
        FACM = ONE / NIR
C----------------------------------------------------
C       VITESSE DE ROTATION MOYENNE DU SEGMENT MAIN
C----------------------------------------------------
        X0=FACM*(XM(1)+XM(2)+XM(3)+XM(4))
        Y0=FACM*(YM(1)+YM(2)+YM(3)+YM(4))
        Z0=FACM*(ZM(1)+ZM(2)+ZM(3)+ZM(4))	
        DO J=1,NIR
         XM(J)=XM(J)-X0
         YM(J)=YM(J)-Y0
         ZM(J)=ZM(J)-Z0
        ENDDO
        XS=X(1,I)-X0
        YS=X(2,I)-Y0
        ZS=X(3,I)-Z0
        XX=0
        YY=0
        ZZ=0
        XY=0
        YZ=0
        ZX=0
        DO J=1,NIR
         XX=XX+ XM(J)*XM(J)
         YY=YY+ YM(J)*YM(J)
         ZZ=ZZ+ ZM(J)*ZM(J)
         XY=XY+ XM(J)*YM(J)
         YZ=YZ+ YM(J)*ZM(J)
         ZX=ZX+ ZM(J)*XM(J)
        ENDDO
        ZZZ=XX+YY
        XXX=YY+ZZ
        YYY=ZZ+XX
        XY2=XY*XY
        YZ2=YZ*YZ
        ZX2=ZX*ZX
        DET= XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2 - TWO*XY*YZ*ZX
        DET=ONE/DET
        B1=ZZZ*YYY-YZ2
        B2=XXX*ZZZ-ZX2
        B3=YYY*XXX-XY2
        C3=ZZZ*XY+YZ*ZX
        C1=XXX*YZ+ZX*XY
        C2=YYY*ZX+XY*YZ
C
        DPARA(1,II)=DET
        DPARA(2,II)=B1
        DPARA(3,II)=B2
        DPARA(4,II)=B3
        DPARA(5,II)=C1
        DPARA(6,II)=C2
        DPARA(7,II)=C3
C
        XMSI=MS(I)*WEIGHT(I)
        FXS=A(1,I)*WEIGHT(I)
        FYS=A(2,I)*WEIGHT(I)
        FZS=A(3,I)*WEIGHT(I)
        INS=IN(I)*WEIGHT(I)
        MX=AR(1,I)*WEIGHT(I) + YS*FZS - ZS*FYS
        MY=AR(2,I)*WEIGHT(I) + ZS*FXS - XS*FZS
        MZ=AR(3,I)*WEIGHT(I) + XS*FYS - YS*FXS
C
        A1=DET*(MX*B1+MY*C3+MZ*C2)
        A2=DET*(MY*B2+MZ*C1+MX*C3)
        A3=DET*(MZ*B3+MX*C2+MY*C1)
C
        FX0=FXS*FACM
        FY0=FYS*FACM
        FZ0=FZS*FACM
C------------------------------------------------------
C     FORCES TRANSMISES AUX NOEUDS MAINS
C------------------------------------------------------
        DO JJ=1,NIR
         J=IRECT(JJ,L)
         A(1,J)=A(1,J) + FX0 + A2*ZM(JJ) - A3*YM(JJ)
         A(2,J)=A(2,J) + FY0 + A3*XM(JJ) - A1*ZM(JJ)
         A(3,J)=A(3,J) + FZ0 + A1*YM(JJ) - A2*XM(JJ)
        ENDDO
C------------------------------------------------------
C     INERTIES => MASSES
C------------------------------------------------------
C
        INX=INS + XMSI*(XS*XS+YS*YS+ZS*ZS)
        MRX = (B1+C3+C2)
        MRY = (B2+C1+C3)
        MRZ = (B3+C2+C1)
        MR=DET*INX*MAX(MRX,MRY,MRZ)
C
C------------------------------------------------------
C     MASSES TRANSMISES AUX NOEUDS MAINS
C------------------------------------------------------
        IF(ILEV==1)THEN
          XMSI=MAX(FACM*XMSI,MR)
        ELSEIF(ILEV==3)THEN
          XMSI=MAX(FACM*XMSI,MR)
        ENDIF
        DMAST = DMAST + NIR*XMSI - MS(I)
        IF (ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS >0) THEN
         DO JJ=1,NIR
          J=IRECT(JJ,L)
          ADM(J) = ADM(J) + XMSI - FACM*MS(I)
         ENDDO
        ENDIF
        STF = ( FACM*STIFN(I)
     .  + DET*MAX(MRX,MRY,MRZ)*(STIFR(I)+STIFN(I)*(XS*XS+YS*YS+ZS*ZS))
     .  )*WEIGHT(I)
        DO JJ=1,NIR
         J=IRECT(JJ,L)
         MS(J)=MS(J)+XMSI
         STIFN(J)=STIFN(J) + STF
        ENDDO
C
        STIFN(I)=EM20
        IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
        MS(I)=ZERO
        STIFR(I)=EM20
        IF(IDEL2/=0.AND.IN(I)/=0.)SINER(II)=IN(I)
        IN(I)=ZERO
       ENDIF
C----
       IF(I>0)THEN
C---     output of tied contact forces
 	 CALL I2FORCES(X      ,V	,A	 ,MS	  ,I	  ,
     .	               IRECT(1,L),H	,NIR     ,FSAV    ,FNCONT ,
     .                 FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
       ENDIF
C----
      ENDDO

      ELSE
      DO II=1,NSN
       I=NSV(II)
       IF(I>0)THEN
        L=IRTL(II)
C
        S = CRST(1,II)
        T = CRST(2,II)
        SP=ONE + S
        SM=ONE - S
        TP=FOURTH*(ONE + T)
        TM=FOURTH*(ONE - T)
C
        H(1)=ONE/NIR
        H(2)=ONE/NIR
        H(3)=ONE/NIR
        H(4)=ONE/NIR
C
        J1=IRECT(1,L)
        J2=IRECT(2,L)
        J3=IRECT(3,L)
        J4=IRECT(4,L)
        X1=X(1,J1)
        Y1=X(2,J1)
        Z1=X(3,J1)
        X2=X(1,J2)
        Y2=X(2,J2)
        Z2=X(3,J2)
        X3=X(1,J3)
        Y3=X(2,J3)
        Z3=X(3,J3)
        X4=X(1,J4)
        Y4=X(2,J4)
        Z4=X(3,J4)
        X0=FOURTH*(X1+X2+X3+X4)
        Y0=FOURTH*(Y1+Y2+Y3+Y4)
        Z0=FOURTH*(Z1+Z2+Z3+Z4)
        X1=X1-X0
        Y1=Y1-Y0
        Z1=Z1-Z0
        X2=X2-X0
        Y2=Y2-Y0
        Z2=Z2-Z0
        X3=X3-X0
        Y3=Y3-Y0
        Z3=Z3-Z0
        X4=X4-X0
        Y4=Y4-Y0
        Z4=Z4-Z0
        XS=X(1,I)-X0
        YS=X(2,I)-Y0
        ZS=X(3,I)-Z0
C
        X12=X1*X1
        X22=X2*X2
        X32=X3*X3
        X42=X4*X4
        Y12=Y1*Y1
        Y22=Y2*Y2
        Y32=Y3*Y3
        Y42=Y4*Y4
        Z12=Z1*Z1
        Z22=Z2*Z2
        Z32=Z3*Z3
        Z42=Z4*Z4
        XX=X12 + X22 + X32 + X42
        YY=Y12 + Y22 + Y32 + Y42
        ZZ=Z12 + Z22 + Z32 + Z42
        XY=X1*Y1 + X2*Y2 + X3*Y3 + X4*Y4
        YZ=Y1*Z1 + Y2*Z2 + Y3*Z3 + Y4*Z4
        ZX=Z1*X1 + Z2*X2 + Z3*X3 + Z4*X4
        ZZZ=XX+YY
        XXX=YY+ZZ
        YYY=ZZ+XX
        XY2=XY*XY
        YZ2=YZ*YZ
        ZX2=ZX*ZX
        DET= XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2 - TWO*XY*YZ*ZX
        DET=ONE/DET
        B1=ZZZ*YYY-YZ2
        B2=XXX*ZZZ-ZX2
        B3=YYY*XXX-XY2
        C3=ZZZ*XY+YZ*ZX
        C1=XXX*YZ+ZX*XY
        C2=YYY*ZX+XY*YZ
C
        DPARA(1,II)=DET
        DPARA(2,II)=B1
        DPARA(3,II)=B2
        DPARA(4,II)=B3
        DPARA(5,II)=C1
        DPARA(6,II)=C2
        DPARA(7,II)=C3
C
        XMSI=MS(I)*WEIGHT(I)
        FXS=A(1,I)*WEIGHT(I)
        FYS=A(2,I)*WEIGHT(I)
        FZS=A(3,I)*WEIGHT(I)
        INS=IN(I)*WEIGHT(I)
        MX=AR(1,I)*WEIGHT(I) + YS*FZS - ZS*FYS
        MY=AR(2,I)*WEIGHT(I) + ZS*FXS - XS*FZS
        MZ=AR(3,I)*WEIGHT(I) + XS*FYS - YS*FXS
C
        A1=DET*(MX*B1+MY*C3+MZ*C2)
        A2=DET*(MY*B2+MZ*C1+MX*C3)
        A3=DET*(MZ*B3+MX*C2+MY*C1)
C
        FX0=FXS*FOURTH
        FY0=FYS*FOURTH
        FZ0=FZS*FOURTH
C------------------------------------------------------
C     FORCES TRANSMISES AUX NOEUDS MAINS
C------------------------------------------------------
        A(1,J1)=A(1,J1) + FX0 + A2*Z1 - A3*Y1
        A(2,J1)=A(2,J1) + FY0 + A3*X1 - A1*Z1
        A(3,J1)=A(3,J1) + FZ0 + A1*Y1 - A2*X1
        A(1,J2)=A(1,J2) + FX0 + A2*Z2 - A3*Y2
        A(2,J2)=A(2,J2) + FY0 + A3*X2 - A1*Z2
        A(3,J2)=A(3,J2) + FZ0 + A1*Y2 - A2*X2
        A(1,J3)=A(1,J3) + FX0 + A2*Z3 - A3*Y3
        A(2,J3)=A(2,J3) + FY0 + A3*X3 - A1*Z3
        A(3,J3)=A(3,J3) + FZ0 + A1*Y3 - A2*X3
        A(1,J4)=A(1,J4) + FX0 + A2*Z4 - A3*Y4
        A(2,J4)=A(2,J4) + FY0 + A3*X4 - A1*Z4
        A(3,J4)=A(3,J4) + FZ0 + A1*Y4 - A2*X4
C------------------------------------------------------
C     INERTIES => MASSES
C------------------------------------------------------
C
        INX=INS + XMSI*(XS*XS+YS*YS+ZS*ZS)
        MRX = (B1+C3+C2)
        MRY = (B2+C1+C3)
        MRZ = (B3+C2+C1)
        MR=DET*INX*MAX(MRX,MRY,MRZ)
C
C------------------------------------------------------
C     MASSES TRANSMISES AUX NOEUDS MAINS
C------------------------------------------------------
        IF(ILEV==1)THEN
          XMSI=FOURTH*XMSI+MR
        ELSEIF(ILEV==3)THEN
          XMSI=MAX(FOURTH*XMSI,MR)
        ENDIF
        DMAST = DMAST + FOUR*XMSI - MS(I)
        IF (ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS >0) THEN
         ADM(J1) = ADM(J1) + XMSI - FOURTH*MS(I)
         ADM(J2) = ADM(J2) + XMSI - FOURTH*MS(I)
         ADM(J3) = ADM(J3) + XMSI - FOURTH*MS(I)
         ADM(J4) = ADM(J4) + XMSI - FOURTH*MS(I)
        ENDIF
        MS(J1)=MS(J1)+XMSI
        MS(J2)=MS(J2)+XMSI
        MS(J3)=MS(J3)+XMSI
        MS(J4)=MS(J4)+XMSI
        STF = ( FOURTH*STIFN(I)
     .  + DET*MAX(MRX,MRY,MRZ)*(STIFR(I)+STIFN(I)*(XS*XS+YS*YS+ZS*ZS))
     .  )*WEIGHT(I)
        STIFN(J1)=STIFN(J1) + STF
        STIFN(J2)=STIFN(J2) + STF
        STIFN(J3)=STIFN(J3) + STF
        STIFN(J4)=STIFN(J4) + STF
C
        STIFN(I)=EM20
        IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
        MS(I)=ZERO
        STIFR(I)=EM20
        IF(IDEL2/=0.AND.IN(I)/=0.)SINER(II)=IN(I)
        IN(I)=ZERO
       ENDIF
C----
       IF(I>0)THEN
C---     output of tied contact forces
 	 CALL I2FORCES(X      ,V	,A	 ,MS	  ,I	  ,
     .	               IRECT(1,L),H	,NIR     ,FSAV    ,FNCONT ,
     .                 FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
       ENDIF
C----
      ENDDO
      ENDIF
C
C
C Traitement specifique pour ADM
C
      IF(ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS >0.AND.ILEV==1) THEN
#include "vectorize.inc"
        DO II=1,NMN
          J=MSR(II)
          ADM(J) = ADM(J)/MMASS(II)
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  I2MZERO                       source/interfaces/interf/i2for3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2MZERO(NMN,MSR,AR,IN,STIFR,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, MSR(*),WEIGHT(*)
C     REAL
      my_real
     .   AR(3,*),IN(*),STIFR(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, J
C-----------------------------------------------
#include "vectorize.inc"
      DO II=1,NMN
        J=MSR(II)
        AR(1,J)=AR(1,J)*WEIGHT(J)
        AR(2,J)=AR(2,J)*WEIGHT(J)
        AR(3,J)=AR(3,J)*WEIGHT(J)
        IN(J)=IN(J)*WEIGHT(J)
        STIFR(J)=STIFR(J)*WEIGHT(J)
      ENDDO
C
      RETURN
      END
