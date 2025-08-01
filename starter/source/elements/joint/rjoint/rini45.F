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
!||    rini45                 ../starter/source/elements/joint/rjoint/rini45.F
!||--- called by ------------------------------------------------------
!||    rinit3                 ../starter/source/elements/spring/rinit3.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    get_skew45             ../starter/source/elements/joint/rjoint/rini45.F
!||    get_u_func             ../starter/source/user_interface/uaccess.F
!||    get_u_func_deri        ../starter/source/user_interface/uaccess.F
!||    get_u_geo              ../starter/source/user_interface/uaccess.F
!||    get_u_pnu              ../starter/source/user_interface/uaccess.F
!||    init_skew45            ../starter/source/elements/joint/rjoint/rini45.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE RINI45(NEL   ,IOUT   ,IPROP , IX, X, XL    ,
     3                  MASS   ,XINER ,STIFN  , STIFR ,VISCM ,
     4                  VISCR,UVAR ,NUVAR,IXR, IXR_KJ,ID,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-------------------------------------------------------------------------
C     This subroutine initialize springs using user properties.
C-------------------------------------------------------------------------
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C----------+---------+---+---+--------------------------------------------
C IX       | 4*NEL   | I | R | SPRING CONNECTIVITY
C                            | IX(1,I) NODE 1 ID
C                            | IX(2,I) NODE 2 ID
C                            | IX(3,I) OPTIONAL NODE 3 ID
C                            | IX(4,I) SPRING ID
C----------+---------+---+---+--------------------------------------------
C MASS     |   NEL   | F | W | ELEMENT MASS
C XINER    |   NEL   | F | W | ELEMENT INERTIA (SPHERICAL)
C STIFM    |   NEL   | F | W | ELEMENT STIFNESS (TIME STEP)
C STIFR    |   NEL   | F | W | ELEMENT ROTATION STIFNESS (TIME STEP)
C VISCM    |   NEL   | F | W | ELEMENT VISCOSITY (TIME STEP)
C VISCR    |   NEL   | F | W | ELEMENT ROTATION VISCOSITY (TIME STEP)
C----------+---------+---+---+--------------------------------------------
C UVAR     |NUVAR*NEL| F | W | USER ELEMENT VARIABLES
C NUVAR    |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C----------+---------+---+---+--------------------------------------------
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
#include      "vect01_c.inc"
#include      "param_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER NEL,IOUT,IPROP,NUVAR,IX(4,MVSIZ),IXR(NIXR,*),
     .        IXR_KJ(5,*)
      my_real 
     .        MASS(NEL) ,XINER(NEL) ,STIFN(NEL),XL(MVSIZ,3) ,
     .        STIFR(NEL),VISCM(NEL) ,VISCR(NEL),UVAR(NUVAR,*),
     .        X(3,*)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDSK1,IDSK2,JTYP,SKFLG,IFKNX,IFKNY,IFKNZ,
     .        IFKRX,IFKRY,IFKRZ,IFCNX,IFCNY,IFCNZ,IFCRX,IFCRY,IFCRZ,
     .        GET_U_PNU,GET_SKEW45,KFUNC,IL,IFM
      my_real KXX,KYY,KZZ,KRX,KRY,KRZ,KNN,KR,X1,Y1,Z1,LEN2,
     .        K1,K2,K3,K4,K5,K6,C1,C2,C3,C4,C5,C6,KTT,KRR,CTT,CRR,
     .        CXX,CYY,CZZ,CRX,CRY,CRZ, DERI,XF,GET_U_FUNC,FM,
     .        EX(LSKEW),GET_U_GEO,SCF,GET_U_FUNC_DERI,DERIMAX,KFM,
     .        THETA0(3),VECT1(LSKEW),VECT2(LSKEW),STOP_ANGL_MIN(3),
     .        STOP_ANGL_MAX(3)
C-----------------------------------------------
      EXTERNAL  GET_U_GEO,GET_SKEW45,GET_U_FUNC_DERI
      PARAMETER (KFUNC=29)      
C=======================================================================
      JTYP = NINT(GET_U_GEO(1,IPROP))
C      
      KXX  = GET_U_GEO(4,IPROP)
      KYY  = GET_U_GEO(5,IPROP)
      KZZ  = GET_U_GEO(6,IPROP)
      KRX  = GET_U_GEO(7,IPROP)
      KRY  = GET_U_GEO(8,IPROP)
      KRZ  = GET_U_GEO(9,IPROP)
      KNN  = GET_U_GEO(10,IPROP)
      SCF  = GET_U_GEO(11,IPROP)
      DO I=1,3
        STOP_ANGL_MIN(I) = GET_U_GEO(35+2*(I-1),IPROP)
        STOP_ANGL_MAX(I) = GET_U_GEO(36+2*(I-1),IPROP)
      ENDDO
      IFKNX = GET_U_PNU(1,IPROP,KFUNC) 
      IFKNY = GET_U_PNU(2,IPROP,KFUNC) 
      IFKNZ = GET_U_PNU(3,IPROP,KFUNC) 
      IFKRX = GET_U_PNU(4,IPROP,KFUNC) 
      IFKRY = GET_U_PNU(5,IPROP,KFUNC) 
      IFKRZ = GET_U_PNU(6,IPROP,KFUNC)
C----
      K1 = KXX
      K2 = KYY
      K3 = KZZ
      K4 = KRX
      K5 = KRY
      K6 = KRZ
      IF (IFKNX/=0) THEN
        XF = GET_U_FUNC(IFKNX,ZERO,DERI)
        K1  = MAX(KXX*DERI, EM20)
      ENDIF
      IF (IFKNY/=0) THEN
        XF = GET_U_FUNC(IFKNY,ZERO,DERI)
        K2  = MAX(KYY*DERI, EM20)
      ENDIF
      IF (IFKNZ/=0) THEN
        XF = GET_U_FUNC(IFKNZ,ZERO,DERI)
        K3  = MAX(KZZ*DERI, EM20)
      ENDIF
      IF (IFKRX/=0) THEN
        XF = GET_U_FUNC(IFKRX,ZERO,DERI)
        K4  = MAX(KRX*DERI, EM20)
      ENDIF
      IF (IFKRY/=0) THEN
        XF = GET_U_FUNC(IFKRY,ZERO,DERI)
        K5  = MAX(KRY*DERI, EM20) 
      ENDIF
      IF (IFKRZ/=0) THEN
        XF = GET_U_FUNC(IFKRZ,ZERO,DERI)
        K6  = MAX(KRZ*DERI, EM20)
      ENDIF
      CXX = GET_U_GEO(21,IPROP)
      CYY = GET_U_GEO(22,IPROP)
      CZZ = GET_U_GEO(23,IPROP)
      CRX = GET_U_GEO(24,IPROP)
      CRY = GET_U_GEO(25,IPROP)
      CRZ = GET_U_GEO(26,IPROP)
C
      IFCNX = GET_U_PNU(7,IPROP,KFUNC) 
      IFCNY = GET_U_PNU(8,IPROP,KFUNC) 
      IFCNZ = GET_U_PNU(9,IPROP,KFUNC) 
      IFCRX = GET_U_PNU(10,IPROP,KFUNC) 
      IFCRY = GET_U_PNU(11,IPROP,KFUNC) 
      IFCRZ = GET_U_PNU(12,IPROP,KFUNC)
C
      C1 = CXX
      C2 = CYY
      C3 = CZZ
      C4 = CRX
      C5 = CRY
      C6 = CRZ
      IF (IFCNX/=0) THEN
        XF = GET_U_FUNC(IFCNX,ZERO,DERI)
        C1  = MAX(CXX*DERI, EM20)
      ENDIF
      IF (IFCNY/=0) THEN
        XF = GET_U_FUNC(IFCNY,ZERO,DERI)
        C2  = MAX(CYY*DERI, EM20)
      ENDIF
      IF (IFCNZ/=0) THEN
        XF = GET_U_FUNC(IFCNZ,ZERO,DERI)
        C3  = MAX(CZZ*DERI, EM20)
      ENDIF
      IF (IFCRX/=0) THEN
        XF = GET_U_FUNC(IFCRX,ZERO,DERI)
        C4  = MAX(CRX*DERI, EM20)
      ENDIF
      IF (IFCRY/=0) THEN
        XF = GET_U_FUNC(IFCRY,ZERO,DERI)
        C5  = MAX(CRY*DERI, EM20) 
      ENDIF
      IF (IFCRZ/=0) THEN
        XF = GET_U_FUNC(IFCRZ,ZERO,DERI)
        C6  = MAX(CRZ*DERI, EM20)
      ENDIF
      DO I=1,6
        IFM = GET_U_PNU(12+I,IPROP,KFUNC)
        KFM  = GET_U_GEO(40+I,IPROP)
        FM = GET_U_GEO(46+I,IPROP)
        IF (IFM/=0) THEN
          DERIMAX = FM*GET_U_FUNC_DERI(IFM) 
                IF (DERIMAX>KFM) THEN
            CALL ANCMSG(MSGID=979,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  R1=KFM,
     .                  R2=DERIMAX)
          ENDIF 
        ENDIF
      END DO

        KTT = MAX(K1,K2,K3)
        KRR = MAX(K4,K5,K6)
        CTT = MAX(C1,C2,C3)
        CRR = MAX(C4,C5,C6)
C--------------------------------------
C       INITIAL ROTATION AND LOCAL FRAME FROM SKEWS
C--------------------------------------
        CALL INIT_SKEW45(JTYP,IPROP,IDSK1,IDSK2,VECT1,VECT2,ID,TITR)
C
C--------------------------------------
C       ELEMENT INITIALIZATION
C--------------------------------------
        DO I=1,NEL
          MASS(I)   = ZERO
          XINER(I)  = ZERO
C    
          IL = LFT+I-1+NFT
C
C-------  computation of local frame
          IERR=IERR+GET_SKEW45(IOUT,JTYP,EX,IXR,IXR_KJ,IL,X,ID,TITR,
     .                         IDSK1,IDSK2,VECT1,VECT2,THETA0,
     .                         STOP_ANGL_MIN,STOP_ANGL_MAX)
C 
          X1 = XL(I,1)
          Y1 = XL(I,2)
          Z1 = XL(I,3)    
          XL(I,1)=EX(1)*X1+EX(2)*Y1+EX(3)*Z1
          XL(I,2)=EX(4)*X1+EX(5)*Y1+EX(6)*Z1
          XL(I,3)=EX(7)*X1+EX(8)*Y1+EX(9)*Z1
C            
          UVAR(1,I) = XL(I,1)
          UVAR(2,I) = XL(I,2)
          UVAR(3,I) = XL(I,3)
          LEN2=XL(I,1)*XL(I,1)+XL(I,2)*XL(I,2)+XL(I,3)*XL(I,3)
C
          IF (IDSK2>0) THEN
            UVAR(7,I) = THETA0(1)
            UVAR(8,I) = THETA0(2)
            UVAR(9,I) = THETA0(3)
          ELSE
            UVAR(7,I) = ZERO
            UVAR(8,I) = ZERO
            UVAR(9,I) = ZERO
          ENDIF
C
          UVAR(22,I) = EX(1)
          UVAR(23,I) = EX(2)
          UVAR(24,I) = EX(3)
          UVAR(25,I) = EX(4)
          UVAR(26,I) = EX(5)
          UVAR(27,I) = EX(6)
          UVAR(28,I) = EX(7)
          UVAR(29,I) = EX(8)
          UVAR(30,I) = EX(9)
C            
C-------  stockage des raideurs
          KR = KNN*MAX(SCF,LEN2)
          UVAR(17,I)= KNN
          UVAR(18,I)= KR
          UVAR(19,I)= KXX
          UVAR(20,I)= KYY
          UVAR(21,I)= KZZ        
C 
          IF(JTYP>=2.AND.JTYP<=4) THEN
            UVAR(31,I)= KRX
            UVAR(32,I)= KR
            UVAR(33,I)= KR
          ELSEIF(JTYP==5) THEN
            UVAR(31,I)= KR
            UVAR(32,I)= KRY
            UVAR(33,I)= KRZ
            UVAR(10,I)= EX(4)
            UVAR(11,I)= EX(5)
            UVAR(12,I)= EX(6)
            UVAR(13,I)= EX(7)
            UVAR(14,I)= EX(8)
            UVAR(15,I)= EX(9)            
          ELSEIF(JTYP>=6.AND.JTYP<=8) THEN
            UVAR(31,I)= KR
            UVAR(32,I)= KR
            UVAR(33,I)= KR
          ELSE
            UVAR(31,I)= KRX
            UVAR(32,I)= KRY
            UVAR(33,I)= KRZ
          ENDIF
C
          UVAR(34,I)= ZERO
          UVAR(35,I)= ZERO
          UVAR(36,I)= ZERO
          UVAR(37,I)= ZERO
          UVAR(38,I)= ZERO
          UVAR(39,I)= ZERO    
C
          STIFN(I) = KTT
          STIFR(I) = KRR+KTT*LEN2
          VISCM(I) = CTT
          VISCR(I) = CRR

        ENDDO
C
      RETURN
      END
      
!||====================================================================
!||    get_skew45             ../starter/source/elements/joint/rjoint/rini45.F
!||--- called by ------------------------------------------------------
!||    rini45                 ../starter/source/elements/joint/rjoint/rini45.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    prod_atb               ../starter/source/elements/joint/rjoint/rini33.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      INTEGER FUNCTION GET_SKEW45(IOUT,JTYP,EX,IXR,IXR_KJ,IEL,X,ID,TITR,
     .                            IDSK1,IDSK2,VECT1,VECT2,THETA0,
     .                            STOP_ANGL_MIN,STOP_ANGL_MAX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER IOUT,IXR(NIXR,*),IEL,JTYP,IXR_KJ(5,*)
      my_real EX(LSKEW),X(3,*)
      INTEGER ID
      INTEGER, INTENT(INOUT) :: IDSK1,IDSK2
      my_real, INTENT(INOUT) :: VECT1(LSKEW),VECT2(LSKEW)
      my_real, INTENT(INOUT) :: THETA0(3)
      my_real, INTENT(INOUT) :: STOP_ANGL_MIN(3),STOP_ANGL_MAX(3)
      CHARACTER(LEN=NCHARTITLE) :: TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NNOD,N1,N2,N3,N4,IERR1,IELUSR,NN(6),HH
      INTEGER NUMEL_KJ,ID_KJ,NNOD2,NNOD_REQ(9),NB_ROT
      my_real PP1,PP2,PP3,PP4,LEN,SCAL,EXMAX
      my_real VECT1_UPT(LSKEW),VECT2_UPT(LSKEW),Q(LSKEW),NR,T(3),
     .        COSK,SINK,SI2,E,KSI,UMI,ERR,SCAL_SIGN
      DATA  UMI/-1.0/
C=======================================================================
C
      IERR1 = 0
      NNOD = 0
      NN = 0
      N1 = 0
      N2 = 0
      N3 = 0
      N4 = 0
      NUMEL_KJ = IXR_KJ(1,NUMELR+1)      
      IELUSR = IXR(NIXR,IEL)
C---- Nb de noeuds min par type de joint
      NNOD_REQ(1) = 2
      NNOD_REQ(2) = 3
      NNOD_REQ(3) = 3
      NNOD_REQ(4) = 3
      NNOD_REQ(5) = 4
      NNOD_REQ(6) = 3
      NNOD_REQ(7) = 3
      NNOD_REQ(8) = 2
      NNOD_REQ(9) = 4                                   
C---- Check des vrais noeuds      
      DO J=1,3
        IF (IXR(1+J,IEL)/=0) THEN    
              NNOD = NNOD + 1
              NN(NNOD) = IXR(1+J,IEL)
          ENDIF   
      END DO
C---- Check des noeuds additionnels
      DO J=1,NUMEL_KJ
        IF (IXR_KJ(4,J)==IELUSR) ID_KJ = J
      END DO
      
      IF (ID_KJ>0) THEN                
      DO J=1,3
        IF (IXR_KJ(J,ID_KJ)/=0) THEN    
              NNOD = NNOD + 1
              NN(NNOD) = IXR_KJ(J,ID_KJ)
          ENDIF   
      END DO
      ENDIF
      
      NNOD2 = NNOD   
C      
      LEN = SQRT((X(1,NN(1))-X(1,NN(2)))**2+(X(2,NN(1))-X(2,NN(2)))**2   
     .          +(X(3,NN(1))-X(3,NN(2)))**2)
      IF ((NNOD==2).AND.(LEN>EM10)) NNOD2=3
C
      IF ((NNOD2<NNOD_REQ(JTYP)).AND.(IDSK1 == 0)) THEN
C-----------------------------------------------      
C---    Error - no skew can be defined 
C-----------------------------------------------
             CALL ANCMSG(MSGID=936,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IELUSR,
     .                   I3=JTYP,
     .                   I4=NNOD_REQ(JTYP)-NNOD2)
      ELSEIF ((NNOD==2).AND.((JTYP==1).OR.(JTYP==8)).AND.(IDSK1==0)) THEN
C-----------------------------------------------      
C---    Global frame is used
C-----------------------------------------------
             EX(1)= ONE
             EX(2)= ZERO
             EX(3)= ZERO
             EX(4)= ZERO
             EX(5)= ONE
             EX(6)= ZERO
             EX(7)= ZERO
             EX(8)= ZERO
             EX(9)= ONE
             PP1 = ONE
             PP2 = ONE
             PP3 = ONE
      ELSEIF ((NNOD==2).AND.(LEN>EM10)) THEN
C-----------------------------------------------      
C---    joint with 1 axis - axis defined with 2nd node
C-----------------------------------------------
             N1 = NN(1)
             N2 = NN(2)        
C--     computation of x
             EX(1)=X(1,N2)-X(1,N1)
             EX(2)=X(2,N2)-X(2,N1)
             EX(3)=X(3,N2)-X(3,N1)
             PP1=SQRT(EX(1)*EX(1)+EX(2)*EX(2)+EX(3)*EX(3))
               EXMAX =ZERO
               DO J=1,3
                 IF (ABS(EX(J))>= EXMAX) THEN
                   EXMAX = ABS(EX(J))
                   HH = J
                 ENDIF
               END DO
C--     computation of y
             IF (HH<3) THEN
               EX(4)= -EX(2)
               EX(5)= EX(1)
               EX(6)= ZERO
               ELSE
               EX(4)= ZERO
               EX(5)= EX(3)
               EX(6)= -EX(2)       
               ENDIF  
             PP2=SQRT(EX(4)*EX(4)+EX(5)*EX(5)+EX(6)*EX(6))       
C--     computation of z = x ^ y       
             EX(7)=EX(2)*EX(6)-EX(3)*EX(5)
             EX(8)=EX(3)*EX(4)-EX(1)*EX(6)
             EX(9)=EX(1)*EX(5)-EX(2)*EX(4)
             PP3=SQRT(EX(7)*EX(7)+EX(8)*EX(8)+EX(9)*EX(9))       
      ELSEIF (NNOD==3) THEN
C-----------------------------------------------      
C---    joint with 1 axis - axis defined with 3rd node
C-----------------------------------------------
             N1 = NN(1)
             N2 = NN(3)        
C--     computation of x
             EX(1)=X(1,N2)-X(1,N1)
             EX(2)=X(2,N2)-X(2,N1)
             EX(3)=X(3,N2)-X(3,N1)
             PP1=SQRT(EX(1)*EX(1)+EX(2)*EX(2)+EX(3)*EX(3))
             EXMAX =ZERO
             DO J=1,3
               IF (ABS(EX(J)) >= EXMAX) THEN
                 EXMAX = ABS(EX(J))
                 HH = J
               ENDIF
             END DO
C--     computation of y
             IF (HH<3) THEN
               EX(4)= -EX(2)
               EX(5)= EX(1)
               EX(6)= ZERO
               ELSE
               EX(4)= ZERO
               EX(5)= EX(3)
               EX(6)= -EX(2)       
               ENDIF  
             PP2=SQRT(EX(4)*EX(4)+EX(5)*EX(5)+EX(6)*EX(6))       
C--     computation of z = x ^ y       
             EX(7)=EX(2)*EX(6)-EX(3)*EX(5)
             EX(8)=EX(3)*EX(4)-EX(1)*EX(6)
             EX(9)=EX(1)*EX(5)-EX(2)*EX(4)
             PP3=SQRT(EX(7)*EX(7)+EX(8)*EX(8)+EX(9)*EX(9))
C--     control of axis's length
             IF (PP1<=EM10) THEN
                CALL ANCMSG(MSGID=935,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IELUSR)
             ENDIF              
      ELSEIF ((NNOD>=4).AND.(JTYP/=5)) THEN
C-----------------------------------------------      
C---    General case : skew computed with 3rd and 4th node
C-----------------------------------------------
             N1 = NN(1)
             N2 = NN(3)
             N3 = NN(4)        
C--     computation of x
             EX(1)=X(1,N2)-X(1,N1)
             EX(2)=X(2,N2)-X(2,N1)
             EX(3)=X(3,N2)-X(3,N1)
             PP1=SQRT(EX(1)*EX(1)+EX(2)*EX(2)+EX(3)*EX(3))        
C--     computation of y
             EX(4)=X(1,N3)-X(1,N1)
             EX(5)=X(2,N3)-X(2,N1)
             EX(6)=X(3,N3)-X(3,N1)       
             PP2=SQRT(EX(4)*EX(4)+EX(5)*EX(5)+EX(6)*EX(6))       
C--     computation of z = x ^ y       
             EX(7)=EX(2)*EX(6)-EX(3)*EX(5)
             EX(8)=EX(3)*EX(4)-EX(1)*EX(6)
             EX(9)=EX(1)*EX(5)-EX(2)*EX(4)     
             PP3=SQRT(EX(7)*EX(7)+EX(8)*EX(8)+EX(9)*EX(9))
C--     control of orthogonality
             SCAL =ABS(EX(1)*EX(4)+EX(2)*EX(5)+EX(3)*EX(6))/(PP1*PP2)    
             IF (ABS(SCAL)>=0.98) THEN
                CALL ANCMSG(MSGID=1009,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IELUSR)
             ELSE
                EX(4)=EX(8)*EX(3)-EX(9)*EX(2)
                EX(5)=EX(9)*EX(1)-EX(7)*EX(3)
                EX(6)=EX(7)*EX(2)-EX(8)*EX(1)
                PP2=SQRT(EX(4)*EX(4)+EX(5)*EX(5)+EX(6)*EX(6))           
             ENDIF
C--     control of N4
             IF ((N4==N1).OR.(N4==N2).OR.(N4==N3)) THEN
                CALL ANCMSG(MSGID=681,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=IELUSR)
             ENDIF       
C--     control of axis length
             PP4 = SQRT((X(1,N3)-X(1,N2))**2+(X(2,N3)-X(2,N2))**2   
     .                 +(X(3,N3)-X(3,N2))**2)
             IF ((PP1<=EM10).OR.(PP2<=EM10).OR.(PP4<=EM10)) THEN
                CALL ANCMSG(MSGID=934,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IELUSR)
             ENDIF
      ELSEIF ((NNOD>=4).AND.(JTYP==5)) THEN
C-----------------------------------------------      
C---    Universal Joint : local skew
C-----------------------------------------------
             N1 = NN(1)
             N2 = NN(3)
             N3 = NN(4)        
C--     computation of y
             EX(4)=X(1,N2)-X(1,N1)
             EX(5)=X(2,N2)-X(2,N1)
             EX(6)=X(3,N2)-X(3,N1)
             PP2=SQRT(EX(4)*EX(4)+EX(5)*EX(5)+EX(6)*EX(6))        
C--     computation of z
             EX(7)=X(1,N3)-X(1,N1)
             EX(8)=X(2,N3)-X(2,N1)
             EX(9)=X(3,N3)-X(3,N1)       
             PP3=SQRT(EX(7)*EX(7)+EX(8)*EX(8)+EX(9)*EX(9))       
C--     computation of x = y ^ z       
             EX(1)=EX(5)*EX(9)-EX(6)*EX(8)
             EX(2)=EX(6)*EX(7)-EX(4)*EX(9)
             EX(3)=EX(4)*EX(8)-EX(5)*EX(7)     
             PP1=SQRT(EX(1)*EX(1)+EX(2)*EX(2)+EX(3)*EX(3))
C--     control of orthogonality
             SCAL =ABS(EX(7)*EX(4)+EX(8)*EX(5)+EX(9)*EX(6))/(PP1+PP2)
             IF (ABS(SCAL)>=0.98) THEN
                CALL ANCMSG(MSGID=1009,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IELUSR)    
             ELSEIF (SCAL>=1e-4) THEN
C--     Orthogonalization of the skew
                CALL ANCMSG(MSGID=940,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IELUSR)
C--     computation of z = x ^ y
                  EX(7)=EX(2)*EX(6)-EX(3)*EX(5)
                EX(8)=EX(3)*EX(4)-EX(1)*EX(6)
                EX(9)=EX(1)*EX(5)-EX(2)*EX(4)     
                PP3=SQRT(EX(7)*EX(7)+EX(8)*EX(8)+EX(9)*EX(9))
             ENDIF       
C--     control of axis length
             PP4 = SQRT((X(1,N3)-X(1,N2))**2+(X(2,N3)-X(2,N2))**2   
     .                 +(X(3,N3)-X(3,N2))**2)
             IF ((PP1<=EM10).OR.(PP2<=EM10).OR.(PP4<=EM10)) THEN
                CALL ANCMSG(MSGID=934,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IELUSR)
               ENDIF  
      ELSEIF (IDSK1 > 0) THEN
C-----------------------------------------------      
C---    skew1 is used
C-----------------------------------------------
             EX(1:9)= VECT1(1:9)
             PP1 = ONE
             PP2 = ONE
             PP3 = ONE
      ELSE
             CALL ANCMSG(MSGID=937,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IELUSR)
      ENDIF
C-----------------------------------------------
C-- norm
C-----------------------------------------------
      EX(1)=EX(1)/PP1
      EX(2)=EX(2)/PP1
      EX(3)=EX(3)/PP1
      EX(4)=EX(4)/PP2
      EX(5)=EX(5)/PP2
      EX(6)=EX(6)/PP2
      EX(7)=EX(7)/PP3
      EX(8)=EX(8)/PP3
      EX(9)=EX(9)/PP3
C-----------------------------------------------
C--   Initialisation of rotations
C-----------------------------------------------      
C
       IF ((IDSK1 > 0).AND.(IDSK2 > 2)) THEN
C
         NB_ROT = 3
         IF ((JTYP==2).OR.(JTYP==3).OR.(JTYP==4)) THEN
C--        only one init angle allowed on X axis
           NB_ROT = 1
C          check of alignement of skew1
           SCAL_SIGN = SIGN(ONE,EX(1)*VECT1(1)+EX(2)*VECT1(2)+EX(3)*VECT1(3)) 
           SCAL = ABS(EX(1)*VECT1(1)+EX(2)*VECT1(2)+EX(3)*VECT1(3))      
           IF (SCAL.LT.0.98) THEN
C--          misalignement - error 
             CALL ANCMSG(MSGID=3076,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IELUSR,
     .                   C2='SKEW1')
           ELSE
             IF ((ONE-SCAL).GT.EM05) THEN
C--            misalignement - warning       
               CALL ANCMSG(MSGID=3067,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IELUSR,
     .                   C2='SKEW1')  
             ENDIF  
C--          realignement of skew1
             VECT1_UPT(1:3) = SCAL_SIGN*EX(1:3)
C--          z1' = x1'^y1       
             VECT1_UPT(7)=VECT1_UPT(2)*VECT1(6)-VECT1_UPT(3)*VECT1(5)
             VECT1_UPT(8)=VECT1_UPT(3)*VECT1(4)-VECT1_UPT(1)*VECT1(6)
             VECT1_UPT(9)=VECT1_UPT(1)*VECT1(5)-VECT1_UPT(2)*VECT1(4)
C--          y1' = z1'^x1'      
             VECT1_UPT(4)=VECT1_UPT(8)*VECT1_UPT(3)-VECT1_UPT(9)*VECT1_UPT(2)
             VECT1_UPT(5)=VECT1_UPT(9)*VECT1_UPT(1)-VECT1_UPT(7)*VECT1_UPT(3)
             VECT1_UPT(6)=VECT1_UPT(7)*VECT1_UPT(2)-VECT1_UPT(8)*VECT1_UPT(1)
             VECT1(1:9) = VECT1_UPT(1:9)             
           ENDIF
           SCAL_SIGN = SIGN(ONE,EX(1)*VECT2(1)+EX(2)*VECT2(2)+EX(3)*VECT2(3))   
           SCAL = ABS(EX(1)*VECT2(1)+EX(2)*VECT2(2)+EX(3)*VECT2(3))       
           IF (SCAL.LT.0.98) THEN
C--          misalignement - error 
             CALL ANCMSG(MSGID=3076,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_1,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IELUSR,
     .                   C2='SKEW2')
           ELSE
             IF ((ONE-SCAL).GT.EM05) THEN
C--            misalignement - warning       
               CALL ANCMSG(MSGID=3067,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IELUSR,
     .                   C2='SKEW2')       
             ENDIF     
C--          realignement of skew2
             VECT2_UPT(1:3) = SCAL_SIGN*EX(1:3)
C--          z1' = x1'^y1       
             VECT2_UPT(7)=VECT2_UPT(2)*VECT2(6)-VECT2_UPT(3)*VECT2(5)
             VECT2_UPT(8)=VECT2_UPT(3)*VECT2(4)-VECT2_UPT(1)*VECT2(6)
             VECT2_UPT(9)=VECT2_UPT(1)*VECT2(5)-VECT2_UPT(2)*VECT2(4)
C--          y1' = z1'^x1'      
             VECT2_UPT(4)=VECT2_UPT(8)*VECT2_UPT(3)-VECT2_UPT(9)*VECT2_UPT(2)
             VECT2_UPT(5)=VECT2_UPT(9)*VECT2_UPT(1)-VECT2_UPT(7)*VECT2_UPT(3)
             VECT2_UPT(6)=VECT2_UPT(7)*VECT2_UPT(2)-VECT2_UPT(8)*VECT2_UPT(1)  
             VECT2(1:9) = VECT2_UPT(1:9)
           ENDIF
         ENDIF    
C         
C--      computation of rotation from skew1 to skew2
         CALL PROD_ATB(VECT1,VECT2,Q)
C
         E = Q(1)+Q(5)+Q(9)
         COSK = HALF * (E - ONE)
         COSK = MIN(COSK,ONE)
         COSK = MAX(COSK,-ONE)
         KSI = ACOS(COSK)
         SINK   = SIN(KSI)
         IF(SINK==ZERO) THEN
           SI2 = ZERO
         ELSE
           SI2  = HALF / SINK
         ENDIF
C
         T(1) = (Q(6) - Q(8)) * SI2
         T(2) = (Q(7) - Q(3)) * SI2
         T(3) = (Q(2) - Q(4)) * SI2
         NR = SQRT(T(1)*T(1)+T(2)*T(2)+T(3)*T(3))
         IF (NR/=ZERO) NR = ONE/NR
         T(1) = T(1)*NR
         T(2) = T(2)*NR
         T(3) = T(3)*NR

C        Vector of initial rotation (in local frame)
         THETA0(1) = T(1)*KSI
         THETA0(2) = T(2)*KSI
         THETA0(3) = T(3)*KSI
C
C--      Check of initial angles vs stopping angles
         DO I=1,NB_ROT
           IF (THETA0(I)<0) THEN
             IF ((THETA0(I)<STOP_ANGL_MIN(I)).AND.(STOP_ANGL_MIN(I)/=ZERO)) THEN
               CALL ANCMSG(MSGID=1133,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 R1=THETA0(I),
     .                 R2=STOP_ANGL_MIN(I))
             ENDIF
           ELSE
             IF ((THETA0(I)>STOP_ANGL_MAX(I)).AND.(STOP_ANGL_MAX(I)/=ZERO)) THEN
               CALL ANCMSG(MSGID=1133,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 R1=THETA0(I),
     .                 R2=STOP_ANGL_MAX(I))
             ENDIF
           ENDIF
         ENDDO  
C
       ENDIF    
C-----------------------------------------------
      GET_SKEW45 = IERR1
   
      RETURN      
      END

!||====================================================================
!||    init_skew45            ../starter/source/elements/joint/rjoint/rini45.F
!||--- called by ------------------------------------------------------
!||    rini45                 ../starter/source/elements/joint/rjoint/rini45.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    get_u_geo              ../starter/source/user_interface/uaccess.F
!||    get_u_skew             ../starter/source/user_interface/uaccess.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE INIT_SKEW45(JTYP,IPROP,IDSK1,IDSK2,VECT1,VECT2,ID,TITR)
C-------------------------------------------------------------------------
C     This subroutine compute the initial angles from skews
C-------------------------------------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER JTYP,IPROP,ID,IDSK1,IDSK2
      my_real, INTENT(INOUT) :: VECT1(LSKEW),VECT2(LSKEW)
      CHARACTER(LEN=NCHARTITLE) :: TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,GET_U_SKEW,N1,N2,N3,ISK
      my_real 
     .        Q(LSKEW),GET_U_GEO,NR,T(3),COSK,SINK,
     .        SI2,E,KSI,UMI,ERR,STOP_ANGL
      DATA  UMI/-1.0/
C-----------------------------------------------
      EXTERNAL  GET_U_GEO,GET_U_SKEW 
C-----------------------------------------------

      IDSK1 = NINT(GET_U_GEO(53,IPROP))
      IDSK2 = NINT(GET_U_GEO(54,IPROP))

C Check skew1 ------
      IF (IDSK1>0) THEN
        ISK = GET_U_SKEW(IDSK1,N1,N2,N3,VECT1)
        IF (ISK==0) THEN
          CALL ANCMSG(MSGID=926,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IDSK1)
        ENDIF
      ENDIF

C Check skew2 ------
      IF (IDSK2>0) THEN
        ISK = GET_U_SKEW(IDSK2,N1,N2,N3,VECT2)
        IF (ISK==0) THEN
            CALL ANCMSG(MSGID=926,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IDSK2)
        ENDIF
        IF ((JTYP>4).AND.(JTYP<9)) THEN
C--       init angle not allowed for joints with no rotations - idsk2 ignored
          CALL ANCMSG(MSGID=1134,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=JTYP,
     .                 I3=IDSK2)
          IDSK2 = 0
        ENDIF
      ENDIF
C
      RETURN      
      END
