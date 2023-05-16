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
Chd|  LAW76_UPD                     source/materials/mat/mat076/law76_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FUNC_COMP                     source/materials/mat/mat076/law76_upd.F
Chd|        TABLE2D_INTERSECT             source/tools/curve/table2d_intersect.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW76_UPD(IOUT   ,TITR    ,MAT_ID   ,NUPARAM ,MATPARAM ,
     .                     UPARAM ,NUMTABL ,ITABLE   ,TABLE   ,NFUNC    ,
     .                     IFUNC  ,NPC     ,PLD      )
C----------------------------------------------- 
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD      
      USE MATPARAM_DEF_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IOUT,MAT_ID,NUMTABL,NFUNC,NUPARAM
      INTEGER ,DIMENSION(NUMTABL) :: ITABLE
      INTEGER ,DIMENSION(NFUNC)   :: IFUNC
      INTEGER :: NPC(*)
      my_real UPARAM(NUPARAM)
      my_real PLD(*)
      CHARACTER*nchartitle  :: TITR
      TYPE(MATPARAM_STRUCT_)  ,TARGET :: MATPARAM
      TYPE(TTABLE), DIMENSION(NTABLE) ,INTENT(INOUT) ,TARGET ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NDIM,NPT,NEPSP,FUNC_ID,FUNC_T,FUNC_C,FUNC_S,ICAS,ICONV,
     .           NPT_TRAC,NPT_COMP,NPT_SHEAR,NPTMAX,IFUN_NUP,IFX,IFY,STAT
      my_real :: XFAC,EPDT_MIN,EPDT_MAX,EPDC_MIN,EPDC_MAX,EPDS_MIN,EPDS_MAX,
     .           NUP,XINT,YINT
      my_real ,DIMENSION(:)  ,ALLOCATABLE :: X_COMP,Y_COMP
      TYPE(TTABLE), DIMENSION(:) ,POINTER ::  TABLE_MAT
C=======================================================================
      FUNC_T = ITABLE(1)
      FUNC_C = ITABLE(2)
      FUNC_S = ITABLE(3)
c
      NUP    = UPARAM(9)
      ICONV  = UPARAM(15)
      ICAS   = UPARAM(17)
      XFAC   = UPARAM(18)
c-----------------------------------------------------------------------------
c     Check yield stresses values (must be strictly positive)
c-----------------------------------------------------------------------------
      DO I = 1,NUMTABL
        FUNC_ID = ITABLE(I)
        IF (FUNC_ID > 0) THEN
          IF (TABLE(FUNC_ID)%Y%VALUES(1) <= 0) THEN
            CALL ANCMSG(MSGID=2063, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=I)                      
          ELSE IF (MINVAL(TABLE(FUNC_ID)%Y%VALUES) <= ZERO) THEN 
            ! Non strictly positive value was found
            CALL ANCMSG(MSGID=2049, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                I2=I)            
          ENDIF
        ENDIF
      ENDDO
c---------------------------------------------------------------
c    check max and min strain rates by direction in yield tables
c    check if yield curves for different strain rates do not intersect
c---------------------------------------------------------------
      NDIM = TABLE(FUNC_T)%NDIM
      IF (NDIM == 2) THEN
        NPT      = SIZE(TABLE(FUNC_T)%X(1)%VALUES)
        NEPSP    = SIZE(TABLE(FUNC_T)%X(2)%VALUES)
        EPDT_MIN = TABLE(FUNC_T)%X(2)%VALUES(1)*XFAC
        EPDT_MAX = TABLE(FUNC_T)%X(2)%VALUES(NEPSP)*XFAC
        UPARAM(19) = EPDT_MIN
        UPARAM(20) = EPDT_MAX
c
        DO I = 2,NEPSP
          DO J = I+1,NEPSP
            CALL TABLE2D_INTERSECT(TABLE(FUNC_T) ,I ,J ,NPT  ,
     .                             XFAC  ,XINT ,YINT)
            IF (XINT > ZERO .and. YINT > ZERO) THEN
              CALL ANCMSG(MSGID=3007, MSGTYPE=MSGWARNING, ANMODE=ANINFO,        
     .                    I1 = MAT_ID,                                               
     .                    I2 = TABLE(FUNC_T)%NOTABLE,                                               
     .                    C1 = TITR  ,
     .                    R1 = TABLE(FUNC_T)%X(2)%VALUES(I)*XFAC,                                               
     .                    R2 = TABLE(FUNC_T)%X(2)%VALUES(J)*XFAC,                                               
     .                    R3 = XINT,
     .                    R4 = YINT)
            END IF
          END DO
        END DO
      END IF
c---
      IF (FUNC_C > 0) THEN
        NDIM = TABLE(FUNC_C)%NDIM
        IF (NDIM == 2) THEN
          NPT      = SIZE(TABLE(FUNC_C)%X(1)%VALUES)
          NEPSP    = SIZE(TABLE(FUNC_C)%X(2)%VALUES)
          EPDC_MIN = TABLE(FUNC_C)%X(2)%VALUES(1)*XFAC
          EPDC_MAX = TABLE(FUNC_C)%X(2)%VALUES(NEPSP)*XFAC
          UPARAM(21) = EPDC_MIN
          UPARAM(22) = EPDC_MAX
c
          DO I = 2,NEPSP
            DO J = I+1,NEPSP
              CALL TABLE2D_INTERSECT(TABLE(FUNC_C) ,I ,J ,NPT  ,
     .                               XFAC  ,XINT ,YINT)
              IF (XINT > ZERO .and. YINT > ZERO) THEN
                CALL ANCMSG(MSGID=3007, MSGTYPE=MSGWARNING, ANMODE=ANINFO,        
     .                      I1 = MAT_ID,                                             
     .                      I2 = TABLE(FUNC_T)%NOTABLE,                                             
     .                      C1 = TITR  ,
     .                      R1 = TABLE(FUNC_T)%X(2)%VALUES(I)*XFAC,                                             
     .                      R2 = TABLE(FUNC_T)%X(2)%VALUES(J)*XFAC,                                             
     .                      R3 = XINT,
     .                      R4 = YINT)
              END IF
            END DO
          END DO
        END IF
      END IF
c---
      IF (FUNC_S > 0) THEN
        NDIM = TABLE(FUNC_S)%NDIM
        IF (NDIM == 2) THEN
          NPT      = SIZE(TABLE(FUNC_S)%X(1)%VALUES)
          NEPSP    = SIZE(TABLE(FUNC_S)%X(2)%VALUES)
          EPDS_MIN = TABLE(FUNC_S)%X(2)%VALUES(1)*XFAC
          EPDS_MAX = TABLE(FUNC_S)%X(2)%VALUES(NEPSP)*XFAC
          UPARAM(23) = EPDS_MIN
          UPARAM(24) = EPDS_MAX
c
          DO I = 2,NEPSP
            DO J = I+1,NEPSP
              CALL TABLE2D_INTERSECT(TABLE(FUNC_S) ,I ,J ,NPT  ,
     .                               XFAC  ,XINT ,YINT)
              IF (XINT > ZERO .and. YINT > ZERO) THEN
                CALL ANCMSG(MSGID=3007, MSGTYPE=MSGWARNING, ANMODE=ANINFO,      
     .                      I1 = MAT_ID,                                             
     .                      I2 = TABLE(FUNC_T)%NOTABLE,                                             
     .                      C1 = TITR  ,
     .                      R1 = TABLE(FUNC_T)%X(2)%VALUES(I)*XFAC,                                             
     .                      R2 = TABLE(FUNC_T)%X(2)%VALUES(J)*XFAC,                                             
     .                      R3 = XINT,
     .                      R4 = YINT)
              END IF
            END DO
          END DO
        END IF
      END IF
c--------------------------------------------------------------------------
c     copy global function tables to local private storage for material law
c--------------------------------------------------------------------------
      ALLOCATE (MATPARAM%TABLE(NUMTABL))
      TABLE_MAT =>  MATPARAM%TABLE(1:NUMTABL)      
      TABLE_MAT(1:NUMTABL)%NOTABLE = 0
c
c     copy tension table
c
      IF (FUNC_T > 0) THEN
        TABLE_MAT(1)%NOTABLE = FUNC_T
        NDIM = TABLE(FUNC_T)%NDIM
        TABLE_MAT(1)%NDIM  = NDIM
        ALLOCATE (TABLE_MAT(1)%X(NDIM)    ,STAT=stat)      
        ALLOCATE (TABLE_MAT(1)%Y          ,STAT=stat)
c        
        DO I = 1,NDIM
          NPT = SIZE(TABLE(FUNC_T)%X(I)%VALUES)
          ALLOCATE (TABLE_MAT(1)%X(I)%VALUES(NPT) ,STAT=stat)
          TABLE_MAT(1)%X(I)%VALUES(1:NPT) = TABLE(FUNC_T)%X(I)%VALUES(1:NPT)
        END DO
c
        NPT = SIZE(TABLE(FUNC_T)%Y%VALUES)
        ALLOCATE (TABLE_MAT(1)%Y%VALUES(NPT) ,STAT=stat)
        TABLE_MAT(1)%Y%VALUES(1:NPT) = TABLE(FUNC_T)%Y%VALUES(1:NPT)
      END IF
c      
c     copy compression table
c
      IF (FUNC_C > 0) THEN

        TABLE_MAT(2)%NOTABLE = FUNC_C
        NDIM = TABLE(FUNC_C)%NDIM
        TABLE_MAT(2)%NDIM  = NDIM
        ALLOCATE (TABLE_MAT(2)%X(NDIM)    ,STAT=stat)      
        ALLOCATE (TABLE_MAT(2)%Y          ,STAT=stat)
c
        DO I = 1,NDIM
          NPT = SIZE(TABLE(FUNC_C)%X(I)%VALUES)
          ALLOCATE (TABLE_MAT(2)%X(I)%VALUES(NPT) ,STAT=stat)
          TABLE_MAT(2)%X(I)%VALUES(1:NPT) = TABLE(FUNC_C)%X(I)%VALUES(1:NPT)
        END DO      
c
        NPT = SIZE(TABLE(FUNC_C)%Y%VALUES)
        ALLOCATE (TABLE_MAT(2)%Y%VALUES(NPT)  ,STAT=stat)
        TABLE_MAT(2)%Y%VALUES(1:NPT) = TABLE(FUNC_C)%Y%VALUES(1:NPT)
      END IF      
c
c     copy shear table
c
      IF (FUNC_S > 0) THEN

        TABLE_MAT(3)%NOTABLE = FUNC_S
        NDIM = TABLE(FUNC_S)%NDIM
        TABLE_MAT(3)%NDIM  = NDIM
        ALLOCATE (TABLE_MAT(3)%X(NDIM)    ,STAT=stat)      
        ALLOCATE (TABLE_MAT(3)%Y          ,STAT=stat)
c
        DO I = 1,NDIM
          NPT = SIZE(TABLE(FUNC_S)%X(I)%VALUES)
          ALLOCATE (TABLE_MAT(3)%X(I)%VALUES(NPT) ,STAT=stat)
          TABLE_MAT(3)%X(I)%VALUES(1:NPT) = TABLE(FUNC_S)%X(I)%VALUES(1:NPT)
        END DO      
c
        NPT = SIZE(TABLE(FUNC_S)%Y%VALUES)
        ALLOCATE (TABLE_MAT(3)%Y%VALUES(NPT)  ,STAT=stat)
        TABLE_MAT(3)%Y%VALUES(1:NPT) = TABLE(FUNC_S)%Y%VALUES(1:NPT)
      END IF      
c--------------------------------------------------------
c     Initialize plastic Poisson ratio if needed
c--------------------------------------------------------
      IFUN_NUP = IFUNC(1)
      IF (IFUN_NUP > 0) THEN
        IFX = NPC(IFUN_NUP)
        IFY = NPC(IFUN_NUP + 1)
        NUP = PLD(IFY)
        NUP = MAX(ZERO, MIN(HALF, NUP))
        UPARAM(9) =  NUP
      END IF
c--------------------------------------------------------
      IF (ICAS == 2) THEN      
        !     create new compression table
        NDIM = 1
        TABLE_MAT(2)%NOTABLE = 2
        TABLE_MAT(2)%NDIM = NDIM
c
        NPT_TRAC = SIZE(TABLE(FUNC_T )%X(1)%VALUES)
        NPT_SHEAR= SIZE(TABLE(FUNC_S )%X(1)%VALUES)
        NPTMAX   = NPT_TRAC+NPT_SHEAR

        ALLOCATE(X_COMP(NPTMAX), STAT=stat)
        ALLOCATE(Y_COMP(NPTMAX), STAT=stat)
        X_COMP(1:NPTMAX) = ZERO
        Y_COMP(1:NPTMAX) = ZERO

        CALL FUNC_COMP(TABLE_MAT,NTABLE  ,NPTMAX  ,NPT_TRAC ,NPT_SHEAR , 
     .                 NPT_COMP ,X_COMP  ,Y_COMP  ,NUP      )


        NPT = NPT_COMP
        ALLOCATE (TABLE_MAT(2)%X(NDIM)          ,STAT=stat)      
        ALLOCATE (TABLE_MAT(2)%X(1)%VALUES(NPT) ,STAT=stat)
        ALLOCATE (TABLE_MAT(2)%Y                ,STAT=stat)
        ALLOCATE (TABLE_MAT(2)%Y%VALUES(NPT)    ,STAT=stat)
c
        DO I = 1,NPT
          TABLE_MAT(2)%X(1)%VALUES(I) = X_COMP(I)
          TABLE_MAT(2)%Y%VALUES(I)    = Y_COMP(I)
        END DO
              
        ICAS  =-1
        ICONV = 1

        DEALLOCATE(X_COMP,Y_COMP)

      END IF      
c--------------------------------------------------------
      UPARAM(15) = ICONV 
      UPARAM(17) = ICAS  
c--------------------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  FUNC_COMP                     source/materials/mat/mat076/law76_upd.F
Chd|-- called by -----------
Chd|        LAW76_UPD                     source/materials/mat/mat076/law76_upd.F
Chd|-- calls ---------------
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE FUNC_COMP(TABLE   ,NTABLE  ,NPTMAX  ,NPT_TRAC ,NPT_SHEAR ,
     .                     NPT_COMP,X_COMP  ,Y_COMP  ,NUP      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NTABLE,NPTMAX, NPT_TRAC,NPT_SHEAR,NPT_COMP
      TYPE(TTABLE), DIMENSION(NTABLE) ::  TABLE
      my_real :: NUP
      my_real ,DIMENSION(NPTMAX) :: X_COMP,Y_COMP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,J,NT,NS,FUNC_TRAC,FUNC_SHEAR
 
      my_real XI, XJ , NUM, DEN ,ALPHAMAX ,BETAMIN, SCALE_X_S,
     .        AA,BB,CC,DELTA,X1,X2,XA,XB,Y1,Y2,YC1,YC2,NUPC,NUP1,NUP2
      my_real X_T_SHEAR(NPT_SHEAR)
      my_real ,DIMENSION(NPTMAX) :: SLOPE_TRAC,SLOPE_SHEA,Y_T, Y_S,ALPHA,BETA
C=======================================================================
      FUNC_TRAC  = 1
      FUNC_SHEAR = 3
      NT = TABLE(FUNC_TRAC)%NDIM
      NS = TABLE(FUNC_SHEAR)%NDIM
      SCALE_X_S = SQRT(THREE)/(ONE+NUP)
      DO I = 1, NPT_SHEAR
         X_T_SHEAR(I) = SCALE_X_S* TABLE(FUNC_SHEAR)%X(1)%VALUES(I) 
      ENDDO

      I = 1
      J = 1 
      K = 1
      DO WHILE (I <= NPT_TRAC .OR. J <= NPT_SHEAR)  
        IF (I <= NPT_TRAC .AND. J <= NPT_SHEAR)THEN
          XI = TABLE(FUNC_TRAC )%X(1)%VALUES(I) 
          XJ = X_T_SHEAR(J) 
          IF (XI < XJ   ) THEN    
             X_COMP(K) = XI
             Y_T(K)    = TABLE(FUNC_TRAC )%Y%VALUES(I)
             IF (J ==1) THEN
              Y_S(K)    = TABLE(FUNC_SHEAR)%Y%VALUES(1) +
     .                   (XI - X_T_SHEAR(1) )*
     .                   (TABLE(FUNC_SHEAR)%Y%VALUES(2)   - TABLE(FUNC_SHEAR)%Y%VALUES(1))/
     .                   (X_T_SHEAR(2)- X_T_SHEAR(1)) 
             ELSE
              Y_S(K)    = TABLE(FUNC_SHEAR)%Y%VALUES(J-1) +
     .                   (XI - X_T_SHEAR(J-1) )*
     .                   (TABLE(FUNC_SHEAR)%Y%VALUES(J)   - TABLE(FUNC_SHEAR)%Y%VALUES(J-1))/
     .                   (X_T_SHEAR(J)- X_T_SHEAR(J-1)) 
             ENDIF
             I = I + 1
             K = K + 1     
          ELSEIF (XJ < XI    ) THEN                            
             X_COMP(K) = XJ
             Y_S(K)    = TABLE(FUNC_SHEAR )%Y%VALUES(J)
             IF (I ==1) THEN
              Y_T(K)    = TABLE(FUNC_TRAC)%Y%VALUES(1) +
     .                   (XJ - TABLE(FUNC_TRAC)%X(1)%VALUES(1) )*
     .                   (TABLE(FUNC_TRAC)%Y%VALUES(2)   - TABLE(FUNC_TRAC)%Y%VALUES(1))/
     .                   (TABLE(FUNC_TRAC)%X(1)%VALUES(2)- TABLE(FUNC_TRAC)%X(1)%VALUES(1)) 
             ELSE
              Y_T(K)    = TABLE(FUNC_TRAC)%Y%VALUES(I-1) +
     .                   (XJ - TABLE(FUNC_TRAC)%X(1)%VALUES(I-1) )*
     .                   (TABLE(FUNC_TRAC)%Y%VALUES(I)   - TABLE(FUNC_TRAC)%Y%VALUES(I-1))/
     .                   (TABLE(FUNC_TRAC)%X(1)%VALUES(I)- TABLE(FUNC_TRAC)%X(1)%VALUES(I-1)) 
             ENDIF

             J = J + 1
             K = K + 1                             
          ELSEIF (XI == XJ ) THEN
             X_COMP(K) = XI
             Y_T(K)    = TABLE(FUNC_TRAC  )%Y%VALUES(I)
             Y_S(K)    = TABLE(FUNC_SHEAR )%Y%VALUES(J)
             I = I + 1
             J = J + 1
             K = K + 1                             
          ENDIF   
        ELSEIF (I > NPT_TRAC .AND. J <= NPT_SHEAR)THEN
             XJ=X_T_SHEAR(J) 
             X_COMP(K) = XJ
             Y_S(K)    = TABLE(FUNC_SHEAR )%Y%VALUES(J)
             Y_T(K)    = TABLE(FUNC_TRAC)%Y%VALUES(I-2) +
     .                  (XJ - TABLE(FUNC_TRAC)%X(1)%VALUES(I-2) )*
     .                  (TABLE(FUNC_TRAC)%Y%VALUES(I-1)  - TABLE(FUNC_TRAC)%Y%VALUES(I-2))/
     .                  (TABLE(FUNC_TRAC)%X(1)%VALUES(I-1)- TABLE(FUNC_TRAC)%X(1)%VALUES(I-2)) 
             J = J + 1
             K = K + 1                             
        ELSEIF (I <= NPT_TRAC .AND. J > NPT_SHEAR)THEN
             XI=TABLE(FUNC_TRAC )%X(1)%VALUES(I) 
             X_COMP(K) = XI
             Y_T(K)    = TABLE(FUNC_TRAC )%Y%VALUES(I)
             Y_S(K)    = TABLE(FUNC_SHEAR)%Y%VALUES(J-2) +
     .                  (XI - X_T_SHEAR(J-2) )*
     .                  (TABLE(FUNC_SHEAR)%Y%VALUES(J-1)  - TABLE(FUNC_SHEAR)%Y%VALUES(J-2))/
     .                  (X_T_SHEAR(J-1)- X_T_SHEAR(J-2)) 
             I = I + 1
             K = K + 1     
        ELSE
           EXIT     
        ENDIF
      END DO   
      NPT_COMP = K - 1

      ALPHAMAX = ONE
      DO K= 2, NPT_COMP
         SLOPE_TRAC(K) = (Y_T(K)-Y_T(K-1)) / (X_COMP(K)-X_COMP(K-1))
         SLOPE_SHEA(K) = (Y_S(K)-Y_S(K-1)) / (X_COMP(K)-X_COMP(K-1)) 
         IF( SLOPE_TRAC(K)>ZERO .AND. SLOPE_SHEA(K)>ZERO)THEN
           ALPHA (K)   = SQRT(THREE)*HALF *(SLOPE_TRAC(K)/SLOPE_SHEA(K)) * (Y_S(K)/Y_T(K))**2
           ALPHAMAX = MAX (ALPHAMAX,ALPHA(K))
         ENDIF
      END DO 
      DO K= 1, NPT_COMP
         NUM = SQRT(THREE) *ALPHAMAX *Y_T(K)*Y_S(K)
         DEN = TWO * ALPHAMAX * Y_T(K) - SQRT(THREE) * Y_S(K)
         Y_COMP(K) = NUM / MAX(EM20, DEN)
      END DO 
c-----------
      !!
c-----------
      RETURN
      END
cc

