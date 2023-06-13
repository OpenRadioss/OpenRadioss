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
Celdev023 routine duale de i2for3 pour parith/on spmd
Chd|====================================================================
Chd|  I2FOR3PN                      source/interfaces/interf/i2for3p.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        I2FORCES_2D                   source/interfaces/interf/i2forces_2D.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2FOR3PN(NSN    ,NMN    ,A      ,CRST   ,NSV    ,
     2                    MS     ,WEIGHT ,STIFN  ,MMASS  ,FSKYI2 ,
     3                    IADI2  ,I0     ,NIR    ,I2SIZE ,IDEL2  ,
     4                    SMASS  ,IRECT  ,X      ,V      ,FSAV   ,
     5                    FNCONT  ,IRTL  ,H3D_DATA, CSTS_BIS,FNCONTP ,
     6                    FTCONTP)
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
      INTEGER NSN, NMN, I0, NIR, I2SIZE, IDEL2,
     .   IRECT(4,*),IADI2(NIR,*), NSV(*),  WEIGHT(*), IRTL(*)
C     REAL
      my_real
     .   X(*),V(*),A(*), CRST(2,*), MS(*), STIFN(*), MMASS(*),
     .   FSKYI2(I2SIZE,*), SMASS(*),FSAV(*),FNCONT(3,*),CSTS_BIS(2,*),
     .   FNCONTP(3,*)   ,FTCONTP(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I1, I2, I3, II, NN, L, J, JJ
C     REAL
      my_real
     .   SS, ST, XMSI, FXI, FYI, FZI,SP,SM,TP,TM,
     .   H(4),H2(4)
C-----------------------------------------------
      IF (NIR==2) THEN
#include "vectorize.inc"
        DO II=1,NSN
          I=NSV(II)
          IF(I>0)THEN
            L=IRTL(II)
            I3=3*I
            I2=I3-1
            I1=I2-1
Cel traitement 1er noeud secnd
            IF (WEIGHT(I)==1) THEN
C
              XMSI=MS(I)
              FXI=A(I1)
              FYI=A(I2)
              FZI=A(I3)
C
              SS=CRST(1,II)
              ST=CRST(2,II)
              SP=ONE + SS
              SM=ONE - SS
              TP=FOURTH*(ONE + ST)
              TM=FOURTH*(ONE - ST)
              H(1)=TM*SM
              H(2)=TM*SP
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
              I0 = I0 + 1
              NN = IADI2(1,I0)
              FSKYI2(1,NN) = FXI*H(1)
              FSKYI2(2,NN) = FYI*H(1)
              FSKYI2(3,NN) = FZI*H(1)
              FSKYI2(4,NN) = XMSI*H2(1)
              FSKYI2(5,NN) = ABS(STIFN(I)*H(1))
C
              NN = IADI2(2,I0)
              FSKYI2(1,NN) = FXI*H(2)
              FSKYI2(2,NN) = FYI*H(2)
              FSKYI2(3,NN) = FZI*H(2)
              FSKYI2(4,NN) = XMSI*H2(2)
              FSKYI2(5,NN) = ABS(STIFN(I)*H(2))
            ENDIF
C---      output of tied contact forces
            CALL I2FORCES_2D(X   ,A     ,I    ,
     .                     IRECT(1,L),H ,NIR     ,FSAV    ,FNCONT ,
     .                     FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
c
            STIFN(I)=EM20
            IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
            MS(I)=ZERO
            A(I1)=ZERO
            A(I2)=ZERO
            A(I3)=ZERO
C stokage ZERO pour noeuds delete par idel2
          ELSEIF(WEIGHT(-I)==1) THEN
            I0 = I0 + 1
            NN = IADI2(1,I0)
            FSKYI2(1,NN) = ZERO
            FSKYI2(2,NN) = ZERO
            FSKYI2(3,NN) = ZERO
            FSKYI2(4,NN) = ZERO
            FSKYI2(5,NN) = ZERO
            NN = IADI2(2,I0)
            FSKYI2(1,NN) = ZERO
            FSKYI2(2,NN) = ZERO
            FSKYI2(3,NN) = ZERO
            FSKYI2(4,NN) = ZERO
            FSKYI2(5,NN) = ZERO
          ENDIF
        ENDDO
Cel NIR = 4
      ELSE
#include "vectorize.inc"
        DO II=1,NSN
          I=NSV(II)
          IF(I>0)THEN
            L=IRTL(II)
            I3=3*I
            I2=I3-1
            I1=I2-1
Cel traitement 1er noeud secnd
            IF (WEIGHT(I)==1) THEN
C
              XMSI=MS(I)
              FXI=A(I1)
              FYI=A(I2)
              FZI=A(I3)
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
              I0 = I0 + 1
              NN = IADI2(1,I0)
              FSKYI2(1,NN) = FXI*H(1)
              FSKYI2(2,NN) = FYI*H(1)
              FSKYI2(3,NN) = FZI*H(1)
              FSKYI2(4,NN) = XMSI*H2(1)
              FSKYI2(5,NN) = ABS(STIFN(I)*H(1))
C
              NN = IADI2(2,I0)
              FSKYI2(1,NN) = FXI*H(2)
              FSKYI2(2,NN) = FYI*H(2)
              FSKYI2(3,NN) = FZI*H(2)
              FSKYI2(4,NN) = XMSI*H2(2)
              FSKYI2(5,NN) = ABS(STIFN(I)*H(2))
C
              NN = IADI2(3,I0)
              FSKYI2(1,NN) = FXI*H(3)
              FSKYI2(2,NN) = FYI*H(3)
              FSKYI2(3,NN) = FZI*H(3)
              FSKYI2(4,NN) = XMSI*H2(3)
              FSKYI2(5,NN) = ABS(STIFN(I)*H(3))
C
              NN = IADI2(4,I0)
              FSKYI2(1,NN) = FXI*H(4)
              FSKYI2(2,NN) = FYI*H(4)
              FSKYI2(3,NN) = FZI*H(4)
              FSKYI2(4,NN) = XMSI*H2(4)
              FSKYI2(5,NN) = ABS(STIFN(I)*H(4))
            ENDIF
C
C---      output of tied contact forces
            CALL I2FORCES(X      ,V ,A   ,MS    ,I    ,
     .                    IRECT(1,L),H  ,NIR     ,FSAV    ,FNCONT ,
     .                    FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C
            IF(IRODDL==0)THEN
              STIFN(I)=EM20
              IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
              MS(I)=ZERO
              A(I1)=ZERO
              A(I2)=ZERO
              A(I3)=ZERO
            ENDIF

C stokage ZERO pour noeuds delete par idel2
          ELSEIF(WEIGHT(-I)==1) THEN
            I0 = I0 + 1
            NN = IADI2(1,I0)
            FSKYI2(1,NN) = ZERO
            FSKYI2(2,NN) = ZERO
            FSKYI2(3,NN) = ZERO
            FSKYI2(4,NN) = ZERO
            FSKYI2(5,NN) = ZERO
            NN = IADI2(2,I0)
            FSKYI2(1,NN) = ZERO
            FSKYI2(2,NN) = ZERO
            FSKYI2(3,NN) = ZERO
            FSKYI2(4,NN) = ZERO
            FSKYI2(5,NN) = ZERO
            NN = IADI2(3,I0)
            FSKYI2(1,NN) = ZERO
            FSKYI2(2,NN) = ZERO
            FSKYI2(3,NN) = ZERO
            FSKYI2(4,NN) = ZERO
            FSKYI2(5,NN) = ZERO
            NN = IADI2(4,I0)
            FSKYI2(1,NN) = ZERO
            FSKYI2(2,NN) = ZERO
            FSKYI2(3,NN) = ZERO
            FSKYI2(4,NN) = ZERO
            FSKYI2(5,NN) = ZERO
          ENDIF
C----
C----
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  I2FOR3P                       source/interfaces/interf/i2for3p.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2FOR3P(NSN    ,NMN    ,A      ,CRST   ,MSR    ,
     2                   NSV    ,MS     ,WEIGHT ,STIFN  ,MMASS  ,
     3                   FSKYI2 ,IADI2  ,I0     ,NIR    ,I2SIZE ,
     4                   IRECT  ,X      ,V      ,FSAV   ,FNCONT    ,
     5                   IRTL   ,H3D_DATA, CSTS_BIS,FNCONTP,FTCONTP)
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
      INTEGER NSN, NMN, I0, NIR, I2SIZE,
     .        IRECT(4,*),IADI2(NIR,*),
     .   MSR(*), NSV(*), WEIGHT(*), IRTL(*)
C     REAL
      my_real
     .   X(*),V(*),A(*),CRST(2,*),MS(*),STIFN(*), MMASS(*),FSAV(*),
     .   FSKYI2(I2SIZE,*),FNCONT(3,*), CSTS_BIS(2,*),
     .   FNCONTP(3,*),FTCONTP(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, I1, I2, I3, II, JJ, NN, L
C     REAL
      my_real
     .   H(4),
     .   SS, ST, XMSI, FXI, FYI, FZI,SP,SM,TP,TM,H2(4)
C=======================================================================
      L = 0
Cel sauvegarde de la masse initiale
      DO II=1,NMN
        J=MSR(II)
        MMASS(II)=MS(J)
      ENDDO
C
      IF (NIR==2) THEN
#include "vectorize.inc"
        DO II=1,NSN
          I=NSV(II)
          IF(I>0) THEN
        L=IRTL(II)
        I3=3*I
        I2=I3-1
        I1=I2-1
Cel traitement 1er noeud secnd
          IF (WEIGHT(I)==1) THEN
C
        SS=CRST(1,II)
        ST=CRST(2,II)
C
        XMSI=MS(I)
        FXI=A(I1)
        FYI=A(I2)
        FZI=A(I3)
C
        SP=ONE + SS
        SM=ONE - SS
        TP=FOURTH*(ONE + ST)
        TM=FOURTH*(ONE - ST)
        H(1)=TM*SM
        H(2)=TM*SP
C
        I0 = I0 + 1
        NN = IADI2(1,I0)
        FSKYI2(1,NN) = FXI*H(1)
        FSKYI2(2,NN) = FYI*H(1)
        FSKYI2(3,NN) = FZI*H(1)
        FSKYI2(4,NN) = XMSI*H(1)
        FSKYI2(5,NN) = ABS(STIFN(I)*H(1))
C
        NN = IADI2(2,I0)
        FSKYI2(1,NN) = FXI*H(2)
        FSKYI2(2,NN) = FYI*H(2)
        FSKYI2(3,NN) = FZI*H(2)
        FSKYI2(4,NN) = XMSI*H(2)
        FSKYI2(5,NN) = ABS(STIFN(I)*H(2))
          ENDIF
        STIFN(I)=EM20
        A(I1)=ZERO
        A(I2)=ZERO
        A(I3)=ZERO
        ELSEIF(WEIGHT(-I)==1) THEN
        I0 = I0 + 1
        NN = IADI2(1,I0)
        FSKYI2(1,NN) = ZERO
        FSKYI2(2,NN) = ZERO
        FSKYI2(3,NN) = ZERO
        FSKYI2(4,NN) = ZERO
        FSKYI2(5,NN) = ZERO
        NN = IADI2(2,I0)
        FSKYI2(1,NN) = ZERO
        FSKYI2(2,NN) = ZERO
        FSKYI2(3,NN) = ZERO
        FSKYI2(4,NN) = ZERO
        FSKYI2(5,NN) = ZERO
        END IF
        ENDDO
Cel NIR = 4
      ELSE
#include "vectorize.inc"
        DO II=1,NSN
          I=NSV(II)
          IF(I>0)THEN
        L=IRTL(II)
        I3=3*I
        I2=I3-1
        I1=I2-1
Cel traitement 1er noeud secnd
        IF (WEIGHT(I)==1) THEN
C
        XMSI=MS(I)
        FXI=A(I1)
        FYI=A(I2)
        FZI=A(I3)
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
        I0 = I0 + 1
        NN = IADI2(1,I0)
        FSKYI2(1,NN) = FXI*H(1)
        FSKYI2(2,NN) = FYI*H(1)
        FSKYI2(3,NN) = FZI*H(1)
        FSKYI2(4,NN) = XMSI*H2(1)
        FSKYI2(5,NN) = ABS(STIFN(I)*H(1))
C
        NN = IADI2(2,I0)
        FSKYI2(1,NN) = FXI*H(2)
        FSKYI2(2,NN) = FYI*H(2)
        FSKYI2(3,NN) = FZI*H(2)
        FSKYI2(4,NN) = XMSI*H2(2)
        FSKYI2(5,NN) = ABS(STIFN(I)*H(2))
C
        NN = IADI2(3,I0)
        FSKYI2(1,NN) = FXI*H(3)
        FSKYI2(2,NN) = FYI*H(3)
        FSKYI2(3,NN) = FZI*H(3)
        FSKYI2(4,NN) = XMSI*H2(3)
        FSKYI2(5,NN) = ABS(STIFN(I)*H(3))
C
        NN = IADI2(4,I0)
        FSKYI2(1,NN) = FXI*H(4)
        FSKYI2(2,NN) = FYI*H(4)
        FSKYI2(3,NN) = FZI*H(4)
        FSKYI2(4,NN) = XMSI*H2(4)
        FSKYI2(5,NN) = ABS(STIFN(I)*H(4))
            ENDIF
C
C---     output of tied contact forces
        CALL I2FORCES(X      ,V  ,A   ,MS    ,I    ,
     .                    IRECT(1,L),H ,NIR     ,FSAV    ,FNCONT ,
     .                    FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C
        IF(IRODDL==0)THEN
         STIFN(I)=EM20
         A(I1)=ZERO
         A(I2)=ZERO
         A(I3)=ZERO
          END IF
C
C stokage ZERO pour noeuds delete par idel2
        ELSEIF(WEIGHT(-I)==1) THEN
        I0 = I0 + 1
        NN = IADI2(1,I0)
        FSKYI2(1,NN) = ZERO
        FSKYI2(2,NN) = ZERO
        FSKYI2(3,NN) = ZERO
        FSKYI2(4,NN) = ZERO
        FSKYI2(5,NN) = ZERO
        NN = IADI2(2,I0)
        FSKYI2(1,NN) = ZERO
        FSKYI2(2,NN) = ZERO
        FSKYI2(3,NN) = ZERO
        FSKYI2(4,NN) = ZERO
        FSKYI2(5,NN) = ZERO
        NN = IADI2(3,I0)
        FSKYI2(1,NN) = ZERO
        FSKYI2(2,NN) = ZERO
        FSKYI2(3,NN) = ZERO
        FSKYI2(4,NN) = ZERO
        FSKYI2(5,NN) = ZERO
        NN = IADI2(4,I0)
        FSKYI2(1,NN) = ZERO
        FSKYI2(2,NN) = ZERO
        FSKYI2(3,NN) = ZERO
        FSKYI2(4,NN) = ZERO
        FSKYI2(5,NN) = ZERO
        ENDIF
C----
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  I2FOR3PO                      source/interfaces/interf/i2for3p.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F
Chd|====================================================================
      SUBROUTINE I2FOR3PO(NSN      ,NMN    ,A       ,CRST   ,MSR     ,
     2                    NSV      ,MS     ,WEIGHT  ,STIFN  ,MMASS   ,
     3                    FSKYI2   ,IADI2  ,I0      ,NIR    ,I2SIZE  ,
     4                    IRECT    ,X      ,V      ,FSAV   ,FNCONT    ,
     5                    IRTL     ,H3D_DATA, CSTS_BIS,FNCONTP,FTCONTP)
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
      INTEGER NSN, NMN, I0, NIR, I2SIZE,
     .        IRECT(4,*),IADI2(NIR,*),
     .   MSR(*), NSV(*), WEIGHT(*), IRTL(*)
C     REAL
      my_real
     .   X(*),V(*),A(*), CRST(2,*), MS(*), STIFN(*), MMASS(*),FSAV(*),
     .   FSKYI2(I2SIZE,*),FNCONT(3,*),CSTS_BIS(2,*),
     .   FNCONTP(3,*),FTCONTP(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, I1, I2, I3, II, JJ, NN, L
C     REAL
      my_real
     .   SS, ST, XMSI, FXI, FYI, FZI,SP,SM,TP,TM,
     .   H(4),H2(4)
C-----------------------------------------------
Cel sauvegarde de la masse initiale
      DO II=1,NMN
        J=MSR(II)
        MMASS(II)=MS(J)
      ENDDO
C
      IF (NIR==2) THEN
#include "vectorize.inc"
        DO II=1,NSN
          I=NSV(II)
          I3=3*I
          I2=I3-1
          I1=I2-1
Cel traitement 1er noeud secnd
          IF (WEIGHT(I)==1) THEN
C
            SS=CRST(1,II)
            ST=CRST(2,II)
C
            XMSI=MS(I)
            FXI=A(I1)
            FYI=A(I2)
            FZI=A(I3)
C
            SP=ONE + SS
            SM=ONE - SS
            TP=FOURTH*(ONE + ST)
            TM=FOURTH*(ONE - ST)
            H(1)=TM*SM
            H(2)=TM*SP
C
            I0 = I0 + 1
            NN = IADI2(1,I0)
            FSKYI2(1,NN) = FXI*H(1)
            FSKYI2(2,NN) = FYI*H(1)
            FSKYI2(3,NN) = FZI*H(1)
            FSKYI2(4,NN) = XMSI*H(1)
            FSKYI2(5,NN) = ABS(STIFN(I)*H(1))
C
            NN = IADI2(2,I0)
            FSKYI2(1,NN) = FXI*H(2)
            FSKYI2(2,NN) = FYI*H(2)
            FSKYI2(3,NN) = FZI*H(2)
            FSKYI2(4,NN) = XMSI*H(2)
            FSKYI2(5,NN) = ABS(STIFN(I)*H(2))
          ENDIF
          STIFN(I)=EM20
          A(I1)=ZERO
          A(I2)=ZERO
          A(I3)=ZERO
        ENDDO
Cel NIR = 4
      ELSE
#include "vectorize.inc"
        DO II=1,NSN
          I=NSV(II)
          L = IRTL(II)
          I3=3*I
          I2=I3-1
          I1=I2-1
Cel traitement 1er noeud secnd
          IF (WEIGHT(I)==1) THEN
C
            XMSI=MS(I)
            FXI=A(I1)
            FYI=A(I2)
            FZI=A(I3)
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
            I0 = I0 + 1
            NN = IADI2(1,I0)
            FSKYI2(1,NN) = FXI*H(1)
            FSKYI2(2,NN) = FYI*H(1)
            FSKYI2(3,NN) = FZI*H(1)
            FSKYI2(4,NN) = XMSI*H2(1)
            FSKYI2(5,NN) = ABS(STIFN(I)*H(1))
C
            NN = IADI2(2,I0)
            FSKYI2(1,NN) = FXI*H(2)
            FSKYI2(2,NN) = FYI*H(2)
            FSKYI2(3,NN) = FZI*H(2)
            FSKYI2(4,NN) = XMSI*H2(2)
            FSKYI2(5,NN) = ABS(STIFN(I)*H(2))
C
            NN = IADI2(3,I0)
            FSKYI2(1,NN) = FXI*H(3)
            FSKYI2(2,NN) = FYI*H(3)
            FSKYI2(3,NN) = FZI*H(3)
            FSKYI2(4,NN) = XMSI*H2(3)
            FSKYI2(5,NN) = ABS(STIFN(I)*H(3))
C
            NN = IADI2(4,I0)
            FSKYI2(1,NN) = FXI*H(4)
            FSKYI2(2,NN) = FYI*H(4)
            FSKYI2(3,NN) = FZI*H(4)
            FSKYI2(4,NN) = XMSI*H2(4)
            FSKYI2(5,NN) = ABS(STIFN(I)*H(4))
          ENDIF
C
C---    output of tied contact forces
          CALL I2FORCES(X      ,V ,A   ,MS    ,I    ,
     .                  IRECT(1,L),H  ,NIR     ,FSAV    ,FNCONT ,
     .                  FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C
          STIFN(I)=EM20
          A(I1)=ZERO
          A(I2)=ZERO
          A(I3)=ZERO
C----
        ENDDO
      ENDIF
C
      RETURN
      END

Chd|====================================================================
Chd|  I2MOM3PN                      source/interfaces/interf/i2for3p.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F
Chd|====================================================================
      SUBROUTINE I2MOM3PN(NSN  ,NMN  ,AR    ,IRECT,CRST  ,
     2                    MSR  ,NSV  ,IRTL  ,IN   ,MS    ,
     3                    A    ,X    ,WEIGHT,STIFR,FSKYI2,
     4                    STIFN,IADI2,I0    ,NIR  ,I2SIZE,
     5                    IDEL2,SMASS,SINER ,MINER,ADI   ,
     6                    H3D_DATA,CSTS_BIS)
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
      INTEGER NSN, NMN, I0    ,NIR  ,I2SIZE, IDEL2,
     .        IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*),
     .        IADI2(NIR,*)
C     REAL
      my_real
     .        A(3,*), AR(3,*),CRST(2,*), MS(*),
     .        X(3,*),IN(*),STIFR(*), FSKYI2(I2SIZE,*), STIFN(*),
     .        SMASS(*), SINER(*), MINER(*), ADI(*), CSTS_BIS(2,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, L, NN
C     REAL
      my_real
     .   SS, ST, XMSI, FXI, FYI, FZI, MXI, MYI, MZI,INS,
     .   X0,X1,X2,X3,X4,Y0,Y1,Y2,Y3,Y4,Z0,Z1,Z2,Z3,Z4,AA,
     .   XC0,YC0,ZC0,SP,SM,TP,TM,XC,YC,ZC,
     .   STF,AI,INMX,H(4),H2(4)
C-----------------------------------------------
C     MINER(II) initialise a MS(J) dans resol_init
      IF(ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
        DO II=1,NMN
          J=MSR(II)
          ADI(J) = ADI(J)*MINER(II)
        ENDDO
      ENDIF
#include "vectorize.inc"
      DO II=1,NMN
        J=MSR(II)
        IN(J)=MAX(EM20,IN(J))
      ENDDO
C
      IF (NIR == 2) THEN
#include "vectorize.inc"
        DO II=1,NSN
          I=NSV(II)
          IF(I>0)THEN
            IF (WEIGHT(I)==1) THEN
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

C        Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
              SS=CSTS_BIS(1,II)
              ST=CSTS_BIS(2,II)
              SP=ONE + SS
              SM=ONE - SS
              TP=FOURTH*(ONE + ST)
              TM=FOURTH*(ONE - ST)
              H2(1)=TM*SM
              H2(2)=TM*SP
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
C
              XC = X1 * H(1) + X2 * H(2)
              YC = Y1 * H(1) + Y2 * H(2)
              ZC = Z1 * H(1) + Z2 * H(2)
C
              XC0=X0-XC
              YC0=Y0-YC
              ZC0=Z0-ZC
C
              AA = XC0*XC0 + YC0*YC0 + ZC0*ZC0
              INS = IN(I) + AA * MS(I)
              STF = STIFR(I) + AA * STIFN(I)
C
              IF (ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
                AI=AA * MS(I)
                ADI(IRECT(1,L))=ADI(IRECT(1,L))+AI*H(1)
                ADI(IRECT(2,L))=ADI(IRECT(2,L))+AI*H(2)
              END IF
C
              FXI=A(1,I)
              FYI=A(2,I)
              FZI=A(3,I)
C
              MXI = AR(1,I) + YC0 * FZI - ZC0 * FYI
              MYI = AR(2,I) + ZC0 * FXI - XC0 * FZI
              MZI = AR(3,I) + XC0 * FYI - YC0 * FXI
C
              I0 = I0 + 1
              DO J = 1,NIR
                IF (IN(IRECT(J,L)) > EM20) THEN
                  NN = IADI2(J,I0)
                  FSKYI2(6,NN) = MXI*H(J)
                  FSKYI2(7,NN) = MYI*H(J)
                  FSKYI2(8,NN) = MZI*H(J)
                  FSKYI2(9,NN) = INS*H2(J)
                  FSKYI2(10,NN)= ABS(STF*H(J))
C
                ELSE
                  NN = IADI2(J,I0)
                  FSKYI2(6,NN) = ZERO
                  FSKYI2(7,NN) = ZERO
                  FSKYI2(8,NN) = ZERO
                  FSKYI2(9,NN) = ZERO
                  FSKYI2(10,NN)= ZERO
C
                END IF
              ENDDO
            ENDIF
            STIFR(I)=EM20
            IF(IDEL2/=0.AND.IN(I)/=0.)SINER(II)=IN(I)
            IN(I)=ZERO
            STIFN(I)=EM20
            IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
            MS(I)=ZERO
            A(1,I)=ZERO
            A(2,I)=ZERO
            A(3,I)=ZERO
C stokage ZERO pour noeuds delete par idel2
          ELSEIF(WEIGHT(-I)==1) THEN
            I0 = I0 + 1
            NN = IADI2(1,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN) = ZERO
            NN = IADI2(2,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN) = ZERO
          ENDIF
        ENDDO
      ELSE
#include "vectorize.inc"
        DO II=1,NSN
          I=NSV(II)
          IF(I>0)THEN
            IF (WEIGHT(I)==1) THEN
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
              IF (ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
                AI=AA * MS(I)
                ADI(IRECT(1,L))=ADI(IRECT(1,L))+AI*H(1)
                ADI(IRECT(2,L))=ADI(IRECT(2,L))+AI*H(2)
                ADI(IRECT(3,L))=ADI(IRECT(3,L))+AI*H(3)
                ADI(IRECT(4,L))=ADI(IRECT(4,L))+AI*H(4)
              END IF
C
              FXI=A(1,I)
              FYI=A(2,I)
              FZI=A(3,I)
C
              MXI = AR(1,I) + YC0 * FZI - ZC0 * FYI
              MYI = AR(2,I) + ZC0 * FXI - XC0 * FZI
              MZI = AR(3,I) + XC0 * FYI - YC0 * FXI
C
              I0 = I0 + 1
              DO J = 1,NIR
                IF (IN(IRECT(J,L)) > EM20) THEN
                  NN = IADI2(J,I0)
                  FSKYI2(6,NN) = MXI*H(J)
                  FSKYI2(7,NN) = MYI*H(J)
                  FSKYI2(8,NN) = MZI*H(J)
                  FSKYI2(9,NN) = INS*H2(J)
                  FSKYI2(10,NN)= ABS(STF*H(J))
C
                ELSE
                  NN = IADI2(J,I0)
                  FSKYI2(6,NN) = ZERO
                  FSKYI2(7,NN) = ZERO
                  FSKYI2(8,NN) = ZERO
                  FSKYI2(9,NN) = ZERO
                  FSKYI2(10,NN)= ZERO
C
                END IF
              ENDDO
            ENDIF
            STIFR(I)=EM20
            IF(IDEL2/=0.AND.IN(I)/=0.)SINER(II)=IN(I)
            IN(I)=ZERO
            STIFN(I)=EM20
            IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
            MS(I)=ZERO
            A(1,I)=ZERO
            A(2,I)=ZERO
            A(3,I)=ZERO
C stokage ZERO pour noeuds delete par idel2
          ELSEIF(WEIGHT(-I)==1) THEN
            I0 = I0 + 1
            NN = IADI2(1,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN) = ZERO
            NN = IADI2(2,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN) = ZERO
            NN = IADI2(3,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN) = ZERO
            NN = IADI2(4,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN) = ZERO
          ENDIF
        ENDDO
C
      ENDIF
C
C Traitement specifique pour ADI
C
      IF(ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          J=MSR(II)
          ADI(J) = ADI(J)/MAX(EM20,MINER(II))
        ENDDO
      ENDIF
C
      RETURN
      END
Celdev023 routine duale de i2mom3 pour parith/on spmd
Chd|====================================================================
Chd|  I2MOM3P                       source/interfaces/interf/i2for3p.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2MOM3P(NSN  ,NMN,AR    ,IRECT ,CRST  ,
     2                   MSR  ,NSV,IRTL  ,IN    ,MS    ,
     3                   A    ,X  ,WEIGHT,STIFR ,FSKYI2,
     4                   IADI2,I0 ,NIR   ,I2SIZE,STIFN ,
     4                   CSTS_BIS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN, I0    ,NIR  ,I2SIZE,
     .        IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*),
     .        IADI2(NIR,*)
C     REAL
      my_real
     .   A(3,*), AR(3,*),CRST(2,*), MS(*),
     .   X(3,*),IN(*),STIFR(*), FSKYI2(I2SIZE,*), STIFN(*), CSTS_BIS(2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, L, NN
C     REAL
      my_real
     .   SS, ST, XMSI, FXI, FYI, FZI, MXI, MYI, MZI,INS,
     .   X0,X1,X2,X3,X4,Y0,Y1,Y2,Y3,Y4,Z0,Z1,Z2,Z3,Z4,AA,INMX,
     .   XC0,YC0,ZC0,SP,SM,TP,TM,XC,YC,ZC,H1, H2,H3,H4,STF,
     .   H21,H22,H23,H24
C-----------------------------------------------
#include "vectorize.inc"
      DO II=1,NMN
        J=MSR(II)
        IN(J)=MAX(EM20,IN(J))
      ENDDO
C
#include "vectorize.inc"
      DO II=1,NSN
        I=NSV(II)
        IF (WEIGHT(I)==1) THEN
          L=IRTL(II)
C
          SS=CRST(1,II)
          ST=CRST(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + ST)
          TM=FOURTH*(ONE - ST)
          H1=TM*SM
          H2=TM*SP
          H3=TP*SP
          H4=TP*SM

C        Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
          SS=CSTS_BIS(1,II)
          ST=CSTS_BIS(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + ST)
          TM=FOURTH*(ONE - ST)
          H21=TM*SM
          H22=TM*SP
          H23=TP*SP
          H24=TP*SM
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
          XC = X1 * H1 + X2 * H2 + X3 * H3 + X4 * H4
          YC = Y1 * H1 + Y2 * H2 + Y3 * H3 + Y4 * H4
          ZC = Z1 * H1 + Z2 * H2 + Z3 * H3 + Z4 * H4
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
          I0 = I0 + 1
          INMX = ZERO
          DO J = 1,NIR
            INMX = MAX(INMX,IN(IRECT(J,L)))
          ENDDO
          IF (INMX > EM20) THEN
            NN = IADI2(1,I0)
            FSKYI2(6,NN) = MXI*H1
            FSKYI2(7,NN) = MYI*H1
            FSKYI2(8,NN) = MZI*H1
            FSKYI2(9,NN) = INS*H21
            FSKYI2(10,NN)= STF*H1
C
            NN = IADI2(2,I0)
            FSKYI2(6,NN) = MXI*H2
            FSKYI2(7,NN) = MYI*H2
            FSKYI2(8,NN) = MZI*H2
            FSKYI2(9,NN) = INS*H22
            FSKYI2(10,NN)= STF*H2
C
            NN = IADI2(3,I0)
            FSKYI2(6,NN) = MXI*H3
            FSKYI2(7,NN) = MYI*H3
            FSKYI2(8,NN) = MZI*H3
            FSKYI2(9,NN) = INS*H23
            FSKYI2(10,NN)= STF*H3
C
            NN = IADI2(4,I0)
            FSKYI2(6,NN) = MXI*H4
            FSKYI2(7,NN) = MYI*H4
            FSKYI2(8,NN) = MZI*H4
            FSKYI2(9,NN) = INS*H24
            FSKYI2(10,NN)= STF*H4
          ELSE
            NN = IADI2(1,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN)= ZERO
C
            NN = IADI2(2,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN)= ZERO
C
            NN = IADI2(3,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN)= ZERO
C
            NN = IADI2(4,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN)= ZERO
          END IF
        ENDIF
        STIFR(I)=EM20
        IN(I)=ZERO
        STIFN(I)=EM20
        A(1,I)=ZERO
        A(2,I)=ZERO
        A(3,I)=ZERO
C
      ENDDO
C
C
      RETURN
      END
Celdev023 routine duale de i2fomo3 pour parith/on spmd
C==========================================================================
Chd|====================================================================
Chd|  I2FOMO3P                      source/interfaces/interf/i2for3p.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F
Chd|====================================================================
      SUBROUTINE I2FOMO3P(
     1             NSN      ,NMN     ,A      ,IRECT   ,DPARA   ,
     2             MSR      ,NSV     ,IRTL   ,MS      ,WEIGHT  ,
     3             AR       ,IN      ,X      ,STIFN   ,STIFR   ,
     4             FSKYI2   ,IADI2   ,ILEV   ,DMAST   ,ADM     ,
     5             MMASS    ,I0      ,NIR    ,I2SIZE  ,IDEL2   ,
     6             SMASS    ,SINER   ,V      ,CRST    ,FSAV    ,
     7             FNCONT   ,H3D_DATA,FNCONTP,FTCONTP )
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
      INTEGER NSN, NMN, ILEV, I0, NIR, I2SIZE, IDEL2,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*),
     .   IADI2(NIR,*)
C     REAL
      my_real
     .   A(3,*),AR(3,*), X(3,*),V(*), FSKYI2(I2SIZE,*),MMASS(*),
     .   DPARA(7,*), MS(*), IN(*),STIFN(*),STIFR(*),DMAST,ADM(*),
     .   SMASS(*), SINER(*),FSAV(*), CRST(2,*),FNCONT(3,*),
     .   FNCONTP(3,*),FTCONTP(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, J1,J2,J3,J4, II, L, JJ, NN,NISKY2
C     REAL
      my_real
     .   H(4),
     .   S,T,SS, ST, XMSI, FXS, FYS, FZS,SP,SM,TP,TM,
     .   MX,MY,MZ,DET,FX0,FY0,FZ0,INS,
     .   X0,X1,X2,X3,X4,XS,Y0,Y1,Y2,Y3,Y4,YS,Z0,Z1,Z2,Z3,Z4,ZS,
     .   X12,X22,X32,X42,Y12,Y22,Y32,Y42,Z12,Z22,Z32,Z42,
     .   XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .   A1,A2,A3,B1,B2,B3,C1,C2,C3,MR,MRX,MRY,MRZ,INX,INY,INZ,STF
C=======================================================================
C Traitement specifique pour DMAS
C
C MMASS(II) initialise a MS(J) a t=0 dans starter
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
C
#include "vectorize.inc"
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
          DET= XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2
     .                                         - TWO*XY*YZ*ZX
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
          IF (WEIGHT(I)==1) THEN
            XMSI=MS(I)
            FXS=A(1,I)
            FYS=A(2,I)
            FZS=A(3,I)
            INS=IN(I)
            MX=AR(1,I) + YS*FZS - ZS*FYS
            MY=AR(2,I) + ZS*FXS - XS*FZS
            MZ=AR(3,I) + XS*FYS - YS*FXS
C
            A1=DET*(MX*B1+MY*C3+MZ*C2)
            A2=DET*(MY*B2+MZ*C1+MX*C3)
            A3=DET*(MZ*B3+MX*C2+MY*C1)
C
            FX0=FXS*FOURTH
            FY0=FYS*FOURTH
            FZ0=FZS*FOURTH
C
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
C     MASSES & FORCES TRANSMISES AUX NOEUDS MAINS
C------------------------------------------------------
            IF(ILEV==1)THEN
              XMSI=FOURTH*XMSI+MR
            ELSEIF(ILEV==3)THEN
              XMSI=MAX(FOURTH*XMSI,MR)
            ENDIF
C
            STF = FOURTH*STIFN(I)
     .      + DET*MAX(MRX,MRY,MRZ)*(STIFR(I)+STIFN(I)*(XS*XS+YS*YS+ZS*ZS))
            I0 = I0 + 1
            NN = IADI2(1,I0)
            FSKYI2(1,NN) = FX0 + A2*Z1 - A3*Y1
            FSKYI2(2,NN) = FY0 + A3*X1 - A1*Z1
            FSKYI2(3,NN) = FZ0 + A1*Y1 - A2*X1
            FSKYI2(4,NN) = XMSI
            FSKYI2(5,NN) = STF
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN)= ZERO
            NN = IADI2(2,I0)
            FSKYI2(1,NN) = FX0 + A2*Z2 - A3*Y2
            FSKYI2(2,NN) = FY0 + A3*X2 - A1*Z2
            FSKYI2(3,NN) = FZ0 + A1*Y2 - A2*X2
            FSKYI2(4,NN) = XMSI
            FSKYI2(5,NN) = STF
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN)= ZERO
            NN = IADI2(3,I0)
            FSKYI2(1,NN) = FX0 + A2*Z3 - A3*Y3
            FSKYI2(2,NN) = FY0 + A3*X3 - A1*Z3
            FSKYI2(3,NN) = FZ0 + A1*Y3 - A2*X3
            FSKYI2(4,NN) = XMSI
            FSKYI2(5,NN) = STF
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN)= ZERO
            NN = IADI2(4,I0)
            FSKYI2(1,NN) =  FX0 + A2*Z4 - A3*Y4
            FSKYI2(2,NN) =  FY0 + A3*X4 - A1*Z4
            FSKYI2(3,NN) =  FZ0 + A1*Y4 - A2*X4
            FSKYI2(4,NN) = XMSI
            FSKYI2(5,NN) = STF
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN)= ZERO
C
            DMAST = DMAST + 4.*XMSI - MS(I)
            IF (ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS >0) THEN
              ADM(J1) = ADM(J1) + XMSI - FOURTH*MS(I)
              ADM(J2) = ADM(J2) + XMSI - FOURTH*MS(I)
              ADM(J3) = ADM(J3) + XMSI - FOURTH*MS(I)
              ADM(J4) = ADM(J4) + XMSI - FOURTH*MS(I)
            ENDIF
          ENDIF
          STIFN(I)=EM20
          IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
          MS(I)=ZERO
          STIFR(I)=EM20
          IF(IDEL2/=0.AND.IN(I)/=0.)SINER(II)=IN(I)
          IN(I)=ZERO
C
C---    output of tied contact forces
          CALL I2FORCES(X      ,V ,A   ,MS    ,I    ,
     .                  IRECT(1,L),H  ,NIR     ,FSAV    ,FNCONT ,
     .                  FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C----
C stokage ZERO pour noeuds delete par idel2
        ELSEIF(WEIGHT(-I)==1) THEN
          I0 = I0 + 1
          NN = IADI2(1,I0)
          FSKYI2(1,NN) = ZERO
          FSKYI2(2,NN) = ZERO
          FSKYI2(3,NN) = ZERO
          FSKYI2(4,NN) = ZERO
          FSKYI2(5,NN) = ZERO
          FSKYI2(6,NN) = ZERO
          FSKYI2(7,NN) = ZERO
          FSKYI2(8,NN) = ZERO
          FSKYI2(9,NN) = ZERO
          FSKYI2(10,NN)= ZERO
          NN = IADI2(2,I0)
          FSKYI2(1,NN) = ZERO
          FSKYI2(2,NN) = ZERO
          FSKYI2(3,NN) = ZERO
          FSKYI2(4,NN) = ZERO
          FSKYI2(5,NN) = ZERO
          FSKYI2(6,NN) = ZERO
          FSKYI2(7,NN) = ZERO
          FSKYI2(8,NN) = ZERO
          FSKYI2(9,NN) = ZERO
          FSKYI2(10,NN)= ZERO
          NN = IADI2(3,I0)
          FSKYI2(1,NN) = ZERO
          FSKYI2(2,NN) = ZERO
          FSKYI2(3,NN) = ZERO
          FSKYI2(4,NN) = ZERO
          FSKYI2(5,NN) = ZERO
          FSKYI2(6,NN) = ZERO
          FSKYI2(7,NN) = ZERO
          FSKYI2(8,NN) = ZERO
          FSKYI2(9,NN) = ZERO
          FSKYI2(10,NN)= ZERO
          NN = IADI2(4,I0)
          FSKYI2(1,NN) = ZERO
          FSKYI2(2,NN) = ZERO
          FSKYI2(3,NN) = ZERO
          FSKYI2(4,NN) = ZERO
          FSKYI2(5,NN) = ZERO
          FSKYI2(6,NN) = ZERO
          FSKYI2(7,NN) = ZERO
          FSKYI2(8,NN) = ZERO
          FSKYI2(9,NN) = ZERO
          FSKYI2(10,NN)= ZERO
        ENDIF
      ENDDO
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
Chd|  I2MZEROP                      source/interfaces/interf/i2for3p.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2MZEROP(FSKYI2,I0 ,NIR   ,I2SIZE,IADI2,
     2                    NSN   ,NSV,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I0,NIR,I2SIZE,NSN,
     .        IADI2(NIR,*),NSV(*),WEIGHT(*)
C     REAL
      my_real
     .   FSKYI2(I2SIZE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,I,NN
C-----------------------------------------------
C
#include "vectorize.inc"
      DO II=1,NSN
        I=NSV(II)
        IF (WEIGHT(I)==1) THEN
          I0 = I0 + 1
          NN = IADI2(1,I0)
          FSKYI2(6,NN) = ZERO
          FSKYI2(7,NN) = ZERO
          FSKYI2(8,NN) = ZERO
          FSKYI2(9,NN) = ZERO
          FSKYI2(10,NN)= ZERO
          NN = IADI2(2,I0)
          FSKYI2(6,NN) = ZERO
          FSKYI2(7,NN) = ZERO
          FSKYI2(8,NN) = ZERO
          FSKYI2(9,NN) = ZERO
          FSKYI2(10,NN)= ZERO
          NN = IADI2(3,I0)
          FSKYI2(6,NN) = ZERO
          FSKYI2(7,NN) = ZERO
          FSKYI2(8,NN) = ZERO
          FSKYI2(9,NN) = ZERO
          FSKYI2(10,NN)= ZERO
          NN = IADI2(4,I0)
          FSKYI2(6,NN) = ZERO
          FSKYI2(7,NN) = ZERO
          FSKYI2(8,NN) = ZERO
          FSKYI2(9,NN) = ZERO
          FSKYI2(10,NN)= ZERO
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I2SKIP                        source/interfaces/interf/i2for3p.F
Chd|-- called by -----------
Chd|        INTTI1                        source/interfaces/interf/intti1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2SKIP(NSN  ,NSV   ,WEIGHT,I0    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, I0, NSV(*), WEIGHT(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II
C     REAL
C-----------------------------------------------
#include "vectorize.inc"
      DO II=1,NSN
        I=NSV(II)
        IF(I>0)THEN
          IF (WEIGHT(I)==1) THEN
            I0=I0+1
          END IF
        ELSEIF(I<0)THEN
          IF (WEIGHT(-I)==1) THEN
            I0=I0+1
          END IF
        END IF
      END DO
C
      RETURN
      END
