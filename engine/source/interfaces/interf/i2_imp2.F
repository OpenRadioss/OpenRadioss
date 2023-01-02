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
Chd|  INT2_IMP2                     source/interfaces/interf/i2_imp2.F
Chd|-- called by -----------
Chd|        I2_IMPD                       source/interfaces/interf/i2_impd.F
Chd|-- calls ---------------
Chd|        I2RECU0                       source/interfaces/interf/i2_imp2.F
Chd|        I2RECU1                       source/interfaces/interf/i2_imp2.F
Chd|        I2RECU2                       source/interfaces/interf/i2_imp2.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INT2_IMP2(IPARI,INTBUF_TAB,X    ,
     .                   MS  ,IN   ,WEIGHT  ,NDOF ,D    ,DR  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*),NDOF(*), WEIGHT(*)
C     REAL
      my_real
     .   X(*),MS(*),IN(*),D(3,*),DR(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .   NSN,NMN,NRTS,NRTM,ILEV
C-----------------------------------------------
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      ILEV  =IPARI(20)
C
C  version spmd avec plus d'un proc nsn = nsn_loc & nmn = nmn_loc
c      IF (IMACH==3.AND.NSPMD>1) THEN
c        NSN = IPARI(16)
c        NMN = IPARI(18)
c      ENDIF
C
      IF(ILEV==1)THEN
        if( IMP_LR > 0)THEN
          CALL I2RECU2(NSN       ,NMN       ,INTBUF_TAB%IRECTM,
     1      INTBUF_TAB%DPARA,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     .      INTBUF_TAB%IRTLM,
     2      MS        ,X         ,WEIGHT    ,NDOF      ,
     3      D         ,DR   )
        ELSE
          CALL I2RECU1(NSN       ,NMN       ,INTBUF_TAB%IRECTM,
     1      INTBUF_TAB%DPARA,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     .      INTBUF_TAB%IRTLM,
     2      MS        ,X         ,WEIGHT    ,NDOF      ,
     3      D         ,DR   )
        END IF
      ELSE
        CALL I2RECU0(NSN       ,NMN       ,INTBUF_TAB%IRECTM,
     1    INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     .    INTBUF_TAB%IRTLM,
     2    MS        ,X         ,WEIGHT    ,NDOF      ,
     3    D         ,DR   )
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  I2RECU0                       source/interfaces/interf/i2_imp2.F
Chd|-- called by -----------
Chd|        INT2_IMP2                     source/interfaces/interf/i2_imp2.F
Chd|-- calls ---------------
Chd|        VELROT                        source/constraints/general/rbe2/rbe2v.F
Chd|        ZERO1                         source/system/zero.F          
Chd|====================================================================
      SUBROUTINE I2RECU0(NSN,NMN,IRECT,CRST,MSR ,
     1                  NSV,IRTL,MS    ,X  ,WEIGHT,
     2                  NDOF, D  ,DR  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,NDOF(*),
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*),WEIGHT(*)
C     REAL
      my_real
     .    CRST(2,*), D(3,*),MS(*),DR(3,*),X(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, J1, J2, J3, J4, K, JD, II, L, JJ,
     .        I1,ID,NL,NJ,ND
C     REAL
      my_real
     .   H(4), SS, TT, SP,SM,TP,TM,DR1(3),XS,YS,ZS,XS0,YS0,ZS0,NUN,
     .   DS(3), LSM(3)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
C-------pour simplement etre coherent avec INTTI2, on ne distinque pas 3n,4n
      NUN=-ONE
      DO II=1,NSN
        I=NSV(II)
        L=IRTL(II)
C
       SS=CRST(1,II)
       TT=CRST(2,II)
        SS = MIN(ONE,SS)
        TT = MIN(ONE,TT)
        SS = MAX(NUN,SS)
        TT = MAX(NUN,TT)
       SP=ONE + SS
       SM=ONE - SS
       IF (IRECT(3,L)==IRECT(4,L)) THEN
         NIR=3
         TP=THIRD*(ONE + TT)
         TM=THIRD*(ONE - TT)
         H(1)=TM*SM
         H(2)=TM*SP
         H(3)=ONE-H(1)-H(2)
       ELSE
         NIR=4
        TP=FOURTH*(ONE + TT)
        TM=FOURTH*(ONE - TT)
        H(1)=TM*SM
        H(2)=TM*SP
        H(3)=TP*SP
        H(4)=TP*SM
       ENDIF
        ND = 0
       DO J=1,NIR
         NJ=IRECT(J,L)
         ND = MAX(ND,NDOF(NJ))
       ENDDO
C-------NDOF(M)> 3 comme rigid body---
        IF (ND==6) THEN
         XS0=ZERO
         YS0=ZERO
         ZS0=ZERO
         DO J=1,NIR
          NJ=IRECT(J,L)
          XS0=XS0+X(1,NJ)*H(J)
          YS0=YS0+X(2,NJ)*H(J)
          ZS0=ZS0+X(3,NJ)*H(J)
         ENDDO 
         XS=X(1,I)-XS0
         YS=X(2,I)-YS0
         ZS=X(3,I)-ZS0
        ENDIF 
       DO K =1,3
        D(K,I)=ZERO
       ENDDO
       DO J=1,NIR
        NJ=IRECT(J,L)
        DO K =1,3
         D(K,I)=D(K,I)+H(J)*D(K,NJ)
        ENDDO
       ENDDO 
       IF (ND==6) THEN 
        DO K =1,3
          DR(K,I)=ZERO
        ENDDO
        DO J=1,NIR
         NJ=IRECT(J,L)
         DO K =1,3
          DR(K,I)=DR(K,I)+H(J)*DR(K,NJ)
         ENDDO
        ENDDO
        IF( IMP_LR > 0)THEN
          CALL ZERO1(DS,3)
          LSM(1) = XS
          LSM(2) = YS
          LSM(3) = ZS
          CALL VELROT(DR(1,I),LSM,DS)
          DO K = 1 , 3
            D(K,I) = D(K,I) + DS(K)
          END DO
        ELSE  
          D(1,I)=D(1,I)+ZS*DR(2,I)-YS*DR(3,I)
          D(2,I)=D(2,I)-ZS*DR(1,I)+XS*DR(3,I)
          D(3,I)=D(3,I)+YS*DR(1,I)-XS*DR(2,I)
        END IF
       ENDIF  
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I2RECU1                       source/interfaces/interf/i2_imp2.F
Chd|-- called by -----------
Chd|        INT2_IMP2                     source/interfaces/interf/i2_imp2.F
Chd|-- calls ---------------
Chd|        I2MATC                        source/interfaces/interf/i2_imp1.F
Chd|====================================================================
      SUBROUTINE I2RECU1(NSN,NMN,IRECT,DPARA,MSR ,
     1                  NSV,IRTL,MS    ,X  ,WEIGHT,
     2                  NDOF, D  ,DR  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,NDOF(*),
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*),WEIGHT(*)
C     REAL
      my_real
     .    DPARA(7,*), D(3,*),MS(*),DR(3,*),X(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,  K,  II, L, NIR(NSN),NJ
C     REAL
      my_real
     .   RJ(3,3,4,NSN),RJT(3,3,4,NSN)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
      CALL I2MATC(NSN,IRECT,DPARA,NSV,IRTL,X  ,
     1                            NIR,RJ ,RJT  )
      DO II=1,NSN
        I=NSV(II)
        L=IRTL(II)
       DO K =1,3
        D(K,I)=ZERO
       ENDDO
       IF (NDOF(I)>3) THEN
        DO K =1,3
         DR(K,I)=ZERO
        ENDDO
       ENDIF 
       DO J=1,NIR(II)
        NJ=IRECT(J,L)
C-------recupere salve dis : in function of main's translation--
         DO K=1,3
          D(K,I)=D(K,I)+RJT(K,1,J,II)*D(1,NJ)+
     .           RJT(K,2,J,II)*D(2,NJ)+RJT(K,3,J,II)*D(3,NJ)
         ENDDO 
         IF (NDOF(I)>3) THEN
          DO K=1,3
           DR(K,I)=DR(K,I)+RJ(K,1,J,II)*D(1,NJ)+
     .           RJ(K,2,J,II)*D(2,NJ)+RJ(K,3,J,II)*D(3,NJ)
          ENDDO 
         ENDIF 
       ENDDO 
      ENDDO
C
      RETURN
      END      
Chd|====================================================================
Chd|  I2RECU2                       source/interfaces/interf/i2_imp2.F
Chd|-- called by -----------
Chd|        INT2_IMP2                     source/interfaces/interf/i2_imp2.F
Chd|-- calls ---------------
Chd|        I2MATC                        source/interfaces/interf/i2_imp1.F
Chd|        VELROT                        source/constraints/general/rbe2/rbe2v.F
Chd|====================================================================
      SUBROUTINE I2RECU2(NSN,NMN,IRECT,DPARA,MSR ,
     1                  NSV,IRTL,MS    ,X  ,WEIGHT,
     2                  NDOF, D  ,DR  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN,NDOF(*),
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*),WEIGHT(*)
C     REAL
      my_real
     .    DPARA(7,*), D(3,*),MS(*),DR(3,*),X(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,  K,  II, L, NIR(NSN),NJ,NIRI
C     REAL
      my_real
     .   RJ(3,3,4,NSN),RJT(3,3,4,NSN),FACM,X0,Y0,Z0,
     .   LSM(3),DR2(3),DS(3),XM(4),YM(4),ZM(4),DT(3)
C------------------------------------
C     VITESSES DES NOEUDS SECONDS
C------------------------------------
      CALL I2MATC(NSN,IRECT,DPARA,NSV,IRTL,X  ,
     1                            NIR,RJ ,RJT  )
      DO II=1,NSN
        I=NSV(II)
        L=IRTL(II)
       NIRI=4
       DO J=1,NIRI
        NJ=IRECT(J,L)
        XM(J)=X(1,NJ)
        YM(J)=X(2,NJ)
        ZM(J)=X(3,NJ)
       ENDDO 
       IF(IRECT(3,L)==IRECT(4,L)) THEN
        NIRI=3
        XM(4)=ZERO
        YM(4)=ZERO
        ZM(4)=ZERO
       ENDIF
       FACM = ONE / NIRI
C----------------------------------------------------
C       VITESSE DE ROTATION MOYENNE DU SEGMENT MAIN
C----------------------------------------------------
        X0=FACM*(XM(1)+XM(2)+XM(3)+XM(4))
        Y0=FACM*(YM(1)+YM(2)+YM(3)+YM(4))
        Z0=FACM*(ZM(1)+ZM(2)+ZM(3)+ZM(4))	   
        LSM(1)=X(1,I)-X0
        LSM(2)=X(2,I)-Y0
        LSM(3)=X(3,I)-Z0             
       DO K =1,3
        D(K,I)=ZERO
       ENDDO
       IF (NDOF(I)>3) THEN
        DO K =1,3
         DR(K,I)=ZERO
        ENDDO
       ENDIF 
       call ZERO1(DR2,3)
       call ZERO1(DT,3)
       DO J=1,NIR(II)
        NJ=IRECT(J,L)
C-------recupere salve dis : in function of main's translation--
          DO K=1,3
           DR2(K)=DR2(K)+RJ(K,1,J,II)*D(1,NJ)+
     .           RJ(K,2,J,II)*D(2,NJ)+RJ(K,3,J,II)*D(3,NJ)
           DT(K) = DT(K) + FACM*D(K,NJ)
          END DO
          
         IF (NDOF(I)>3) THEN
          DO K=1,3
           DR(K,I)=DR(K,I)+RJ(K,1,J,II)*D(1,NJ)+
     .           RJ(K,2,J,II)*D(2,NJ)+RJ(K,3,J,II)*D(3,NJ)
          ENDDO           
         ENDIF          
       ENDDO
       CALL VELROT(DR2,LSM,DS)
       DO K=1,3
        D(K,I)=D(K,I)+ DS(K)+ DT(K)
       ENDDO
       
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I2_FRRD1                      source/interfaces/interf/i2_imp2.F
Chd|-- called by -----------
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2_FRRD1(X   ,IRECT,DPARA ,NSV ,IRTL ,
     1                    D   ,II   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .   IRECT(4,*), NSV(*), IRTL(*),II
C     REAL
      my_real
     .   D(3,*),X(3,*),DPARA(7,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, J1,J2,J3,J4,  L, JJ
C     REAL
      my_real
     .   VMX, VMY, VMZ,MGX,MGY,MGZ,VRX,VRY,VRZ,
     .   X0,X1,X2,X3,X4,XS,Y0,Y1,Y2,Y3,Y4,YS,Z0,Z1,
     .   Z2,Z3,Z4,ZS,A1,A2,A3,B1,B2,B3,C1,C2,C3,DET
C-----------------------------------------------
       I=NSV(II)
        L=IRTL(II)
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
        DET=DPARA(1,II)
        B1=DPARA(2,II)
        B2=DPARA(3,II)
        B3=DPARA(4,II)
        C1=DPARA(5,II)
        C2=DPARA(6,II)
        C3=DPARA(7,II)
C
        VMX=FOURTH*(D(1,J1)+D(1,J2)+D(1,J3)+D(1,J4))
        VMY=FOURTH*(D(2,J1)+D(2,J2)+D(2,J3)+D(2,J4))
        VMZ=FOURTH*(D(3,J1)+D(3,J2)+D(3,J3)+D(3,J4))
C
        MGX = Y1*D(3,J1) + Y2*D(3,J2) + Y3*D(3,J3) + Y4*D(3,J4) 
     .      - Z1*D(2,J1) - Z2*D(2,J2) - Z3*D(2,J3) - Z4*D(2,J4)
        MGY = Z1*D(1,J1) + Z2*D(1,J2) + Z3*D(1,J3) + Z4*D(1,J4) 
     .      - X1*D(3,J1) - X2*D(3,J2) - X3*D(3,J3) - X4*D(3,J4)
        MGZ = X1*D(2,J1) + X2*D(2,J2) + X3*D(2,J3) + X4*D(2,J4) 
     .      - Y1*D(1,J1) - Y2*D(1,J2) - Y3*D(1,J3) - Y4*D(1,J4)
        VRX=DET*(MGX*B1+MGY*C3+MGZ*C2)
        VRY=DET*(MGY*B2+MGZ*C1+MGX*C3)
        VRZ=DET*(MGZ*B3+MGX*C2+MGY*C1)
        D(1,I)=VMX + VRY*ZS - VRZ*YS
        D(2,I)=VMY + VRZ*XS - VRX*ZS
        D(3,I)=VMZ + VRX*YS - VRY*XS
C
      RETURN
      END
Chd|====================================================================
Chd|  I2_FRRD0                      source/interfaces/interf/i2_imp2.F
Chd|-- called by -----------
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2_FRRD0(X   ,IRECT,CRST  ,NSV  ,IRTL ,
     1                    D   ,DR   ,II    ,NDOF )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .   IRECT(4,*), NSV(*), IRTL(*), II,NDOF(*)
C     REAL
      my_real
     .   X(3,*),D(3,*),DR(3,*), CRST(2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, J1, J2, J3, J4, K, JD,  L, JJ,
     .        I1,ID,NL,NJ,ND
C     REAL
      my_real
     .   H(4), SS, TT, SP,SM,TP,TM,DR1(3),XS,YS,ZS,XS0,YS0,ZS0
C-----------------------------------------------
      NIR=4
        I=NSV(II)
        L=IRTL(II)
C
       SS=CRST(1,II)
       TT=CRST(2,II)
       SP=ONE + SS
       SM=ONE - SS
       TP=FOURTH*(ONE + TT)
       TM=FOURTH*(ONE - TT)
       H(1)=TM*SM
       H(2)=TM*SP
       H(3)=TP*SP
       H(4)=TP*SM
       ND = 0
       DO J=1,NIR
        NJ=IRECT(J,L)
        ND = MAX(ND,NDOF(NJ))
       ENDDO
C-------NDOF(M)> 3 comme rigid body---
        IF (ND==6) THEN
         XS0=ZERO
         YS0=ZERO
         ZS0=ZERO
         DO J=1,NIR
          NJ=IRECT(J,L)
          XS0=XS0+X(1,NJ)*H(J)
          YS0=YS0+X(2,NJ)*H(J)
          ZS0=ZS0+X(3,NJ)*H(J)
         ENDDO 
         XS=X(1,I)-XS0
         YS=X(2,I)-YS0
         ZS=X(3,I)-ZS0
        ENDIF 
C-------Update K(main node),B---
       DO K =1,3
        D(K,I)=ZERO
       ENDDO
       DO J=1,NIR
        NJ=IRECT(J,L)
        DO K =1,3
         D(K,I)=D(K,I)+H(J)*D(K,NJ)
        ENDDO
       ENDDO 
       IF (ND==6) THEN 
        DO K =1,3
          DR1(K)=ZERO
        ENDDO
        DO J=1,NIR
         DO K =1,3
          DR1(K)=DR1(K)+H(J)*DR(K,NJ)
         ENDDO
        ENDDO 
        D(1,I)=D(1,I)+ZS*DR1(2)-YS*DR1(3)
        D(2,I)=D(2,I)-ZS*DR1(1)+XS*DR1(3)
        D(3,I)=D(3,I)+YS*DR1(1)-XS*DR1(2)
       ENDIF  
C
      RETURN
      END
