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
Chd|  RUSER32                       source/elements/spring/ruser32.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        GET_U_PNU                     source/user_interface/upidmid.F
Chd|        GET_U_SENS                    source/user_interface/usensor.F
Chd|====================================================================
      SUBROUTINE RUSER32(
     1             NEL     ,IOUT    ,IPROP  ,UVAR   ,NUVAR  ,
     2             FX      ,FY      ,FZ     ,XMOM   ,YMOM   ,
     3             ZMOM    ,E       ,OFF    ,STIFM  ,STIFR  ,
     4             VISCM   ,VISCR   ,MASS   ,XINER  ,DT     ,
     5             XL      ,VX      ,RY1    ,RZ1    ,RX     ,
     6             RY2     ,RZ2     ,FR_WAVE,SENSOR_TAB)
C-----------------------------------------------  
C   M o d u l e s
C-----------------------------------------------  
      USE SENSOR_MOD
C-------------------------------------------------------------------------
C     This subroutine compute springs forces and moments.
C-------------------------------------------------------------------------
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C----------+---------+---+---+--------------------------------------------
C XL       |   NEL   | F | R | ELEMENT LENGTH
C----------+---------+---+---+--------------------------------------------
C UVAR     |NUVAR*NEL| F |R/W| USER ELEMENT VARIABLES
C NUVAR    |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C----------+---------+---+---+--------------------------------------------
C The user routine does not need to return elements mass in vector MASS : 
C this vector is not used by RADIOSS since version 4.1E.
C The mass which is used by RADIOSS for time step (and output) is the 
C initial mass which was returned by user routine RINI29 into starter.
C-------------------------------------------------------------------------
C FUNCTION 
C-------------------------------------------------------------------------
C INTEGER II = GET_U_PNU(I,IP,KK)
C         IFUNCI = GET_U_PNU(I,IP,KFUNC)
C         IPROPI = GET_U_PNU(I,IP,KFUNC)
C         IMATI  = GET_U_PNU(I,IP,KMAT)
C         I     :     VARIABLE INDEX(1 for first variable,...)
C         IP    :     PROPERTY NUMBER
C         KK    :     PARAMETER KFUNC,KMAT,KPROP
C         THIS FUNCTION RETURN THE USER STORED FUNCTION(IF KK=KFUNC), 
C         MATERIAL(IF KK=KMAT) OR PROPERTY(IF KK=KPROP) NUMBER. 
C         SEE LECG29 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C INTEGER IFUNCI = GET_U_MNU(I,IM,KFUNC)
C         I     :     VARIABLE INDEX(1 for first function)
C         IM    :     MATERIAL NUMBER
C         KFUNC :     ONLY FUNCTION ARE YET AVAILABLE.
C         THIS FUNCTION RETURN THE USER STORED FUNCTION NUMBER(function 
C         refered by users materials).
C         SEE LECM29 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C my_real PARAMI = GET_U_GEO(I,IP)
C         I     :     PARAMETER INDEX(1 for first parameter,...)
C         IP    :     PROPERTY NUMBER
C         THIS FUNCTION RETURN THE USER GEOMETRY PARAMETERS 
C         NOTE: IF(IP==IPROP) UPARAG(I) == GET_U_GEO(I,IPROP)
C         see lecg30 for storage
C-------------------------------------------------------------------------
C my_real PARAMI = GET_U_MAT(I,IM)
C         I     :     PARAMETER INDEX(1 for first parameter,...)
C         IM    :     MATERIAL NUMBER
C         THIS FUNCTION RETURN THE USER MATERIAL PARAMETERS 
C         NOTE: GET_U_MAT(0,IMAT) RETURN THE DENSITY
C         see lecm29,30,31 for storage
C-------------------------------------------------------------------------
C INTEGER PID = GET_U_PID(IP)
C         IP    :     PROPERTY NUMBER
C         THIS FUNCTION RETURN THE USER PROPERTY ID CORRESPONDING TO
C         USER PROPERTY NUMBER IP. 
C-------------------------------------------------------------------------
C INTEGER MID = GET_U_MID(IM)
C         IM   :     MATERIAL NUMBER
C         THIS FUNCTION RETURN THE USER MATERIAL ID CORRESPONDING TO
C         USER MATERIAL NUMBER IM. 
C-------------------------------------------------------------------------
C my_real Y = GET_U_FUNC(IFUNC,X,DXDY)
C         IFUNC :     function number obtained by 
C                     IFUNC = GET_U_MNU(I,IM,KFUNC) or IFUNC = GET_U_PNU(I,IP,KFUNC)
C         X     :     X value
C         DXDY  :     slope dX/dY
C         THIS FUNCTION RETURN Y(X)
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "impl1_c.inc"
#include      "com04_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER IOUT,NEL,NUVAR,IPROP,
     .        GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU,
     .        KFUNC,KMAT,KPROP
      my_real
     .   UVAR(NUVAR,*),DT ,
     .   FX(*), FY(*), FZ(*), E(*), VX(*),MASS(*) ,XINER(*),
     .   RY1(*), RZ1(*), OFF(*), XMOM(*), YMOM(*),
     .   ZMOM(*), RX(*), RY2(*), RZ2(*),XL(*),
     .   STIFM(*) ,STIFR(*) , VISCM(*) ,VISCR(*) ,FR_WAVE(*) ,
     .   GET_U_MAT, GET_U_GEO, GET_U_FUNC, GET_U_SENS
      EXTERNAL GET_U_MNU,GET_U_PNU,GET_U_MID,GET_U_PID,
     .         GET_U_MAT,GET_U_GEO, GET_U_FUNC
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=33)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFUNC1,IFUNC2,ISENS,ITYP,IACT,ILOCK
      my_real 
     .   STIF0,STIF1,DSCAL,FSCAL,TSCAL,X,DXDY,DX,TACTI,F0,FF,D1,
     .   DXDY2,FF2
C=======================================================================
      STIF0  = GET_U_GEO(2,IPROP)       
      STIF1  = GET_U_GEO(3,IPROP)       
      TSCAL  = GET_U_GEO(7,IPROP)    
      DSCAL  = GET_U_GEO(8,IPROP)    
      FSCAL  = GET_U_GEO(9,IPROP)    
      D1     = GET_U_GEO(11,IPROP)
      ISENS  = NINT(GET_U_GEO(5,IPROP)) 
      ITYP   = NINT(GET_U_GEO(6,IPROP))
      ILOCK  = NINT(GET_U_GEO(10,IPROP))
      TACTI  = SENSOR_TAB(ISENS)%STATUS   !GET_U_SENS(ISENS)
C
      IF (TACTI == ZERO .AND. ISENS /= ZERO) THEN
        IACT=0
        DO I=1,NEL
          IF (UVAR(2,I) == ONE) THEN
            UVAR(2,I) = ZERO
          ENDIF
          FX(I)    = FX(I) + STIF0 * DT * VX(I)
          UVAR(4,I) = STIF0
          STIFM(I) = STIF0
        ENDDO
      ELSE
        IACT=1
        DO I=1,NEL
          IF (UVAR(2,I) == ZERO) THEN
            UVAR(1,I) = ZERO
            UVAR(2,I) = ONE    
          ENDIF
          UVAR(1,I) = UVAR(1,I) + DT * VX(I)
          FX(I)     = FX(I) + STIF0 * DT * VX(I)
          UVAR(4,I) = STIF0
          STIFM(I)  = STIF0
        ENDDO
      ENDIF
C	  	
      IF (IACT == 0) THEN
      ELSEIF (ITYP == 1) THEN
        F0  = GET_U_GEO(4,IPROP)
        DO I=1,NEL
          X  = UVAR(1,I)
          FF = F0 + STIF1 * X
          IF (FX(I) >  FF .AND. ILOCK == 2) UVAR(3,I) = ONE
          IF (FF > ZERO  .AND. UVAR(3,I) == ZERO) THEN 
            FX(I) = MAX(FF,FX(I))
            IF (IMPL_S > ZERO) THEN
              FF2 = F0 + STIF1 * (X-DT * VX(I))
              IF (FF2 > FF) THEN
                UVAR(4,I) = MIN(STIF0,STIF1)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ELSEIF (ITYP == 2) THEN
        IFUNC1   = GET_U_PNU(1,IPROP,KFUNC) 
        DO I=1,NEL
          X  = UVAR(1,I) 
          FF = FSCAL*GET_U_FUNC(IFUNC1,X*DSCAL,DXDY)
          IF (X < D1 .AND. D1 /= ZERO) UVAR(3,I) = ONE
          IF (FX(I) > FF .AND. ILOCK == 2) UVAR(3,I) = ONE
          IF (FF > ZERO .AND. UVAR(3,I) == ZERO) THEN
            FX(I) = MAX(FF,FX(I))
            IF (IMPL_S > ZERO) THEN
              FF2   = FSCAL*GET_U_FUNC(IFUNC1,(X-DT * VX(I))*DSCAL,DXDY2)
              IF (FF2 > FF) THEN
                UVAR(4,I) = MIN(STIF0,ABS(DXDY))
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ELSEIF (ITYP == 3) THEN
        IFUNC2   = GET_U_PNU(2,IPROP,KFUNC) 
        F0 = FSCAL*GET_U_FUNC(IFUNC2,TACTI*TSCAL,DXDY)
        DO I=1,NEL
          X = UVAR(1,I) 
          IF (X < D1 .AND. D1 /= ZERO) UVAR(3,I) = ONE
          IF (FX(I) > F0 .AND. ILOCK == 2) UVAR(3,I) = ONE
          IF (F0 > ZERO .AND. UVAR(3,I) == ZERO) THEN
            FX(I) = MAX(F0,FX(I))
          ENDIF
        ENDDO
      ELSEIF (ITYP == 4) THEN
        IFUNC1   = GET_U_PNU(1,IPROP,KFUNC) 
        IFUNC2   = GET_U_PNU(2,IPROP,KFUNC) 
        F0 = FSCAL*GET_U_FUNC(IFUNC2,TACTI*TSCAL,DXDY)
        DO I=1,NEL
          X  = UVAR(1,I) 
          FF = F0*GET_U_FUNC(IFUNC1,X*DSCAL,DXDY)
          IF (X < D1 .AND. D1 /= ZERO) UVAR(3,I) = ONE
          IF (FX(I) > FF .AND. ILOCK == 2) UVAR(3,I) = ONE
          IF (FF > ZERO .AND. UVAR(3,I) == ZERO) THEN
            FX(I) = MAX(FF,FX(I))
            IF (IMPL_S > ZERO) THEN
              FF2   = GET_U_FUNC(IFUNC1,(X-DT * VX(I))*DSCAL,DXDY2)            
              IF (FF2 > FF) THEN
                UVAR(4,I) = MIN(STIF0,ABS(DXDY))
              ENDIF
            ENDIF  
          ENDIF
        ENDDO
      ENDIF
C-------------------------------
      DO I=1,NEL 
        STIFR(I) = ZERO   
        VISCM(I) = ZERO   
        VISCR(I) = ZERO   
        XINER(I) = ZERO   
      ENDDO
C-----------
      RETURN
      END
