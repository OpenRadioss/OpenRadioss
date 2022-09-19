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
Chd|  RUSER46                       source/elements/spring/ruser46.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        GET_U_PNU                     source/user_interface/upidmid.F
Chd|        GET_U_TIME                    source/user_interface/uaccess.F
Chd|====================================================================
      SUBROUTINE RUSER46(NEL,IOUT   ,IPROP  ,UVAR   ,NUVAR  ,
     2             FX      ,FY      ,FZ     ,XMOM   ,YMOM   ,
     3             ZMOM    ,E       ,OFF    ,STIFM  ,STIFR  ,
     4             VISCM   ,VISCR   ,MASS   ,XINER  ,DT     ,
     5             XL      ,VX      ,RY1    ,RZ1    ,RX     ,
     6             RY2     ,RZ2     ,FR_WAVE)
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER IOUT,NEL,NUVAR,IPROP,
     .        GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU,
     .        KFUNC,KMAT,KPROP,GET_SPRING_ELNUM
      my_real
     .   UVAR(NUVAR,*),DT ,
     .   FX(*), FY(*), FZ(*), E(*), VX(*),MASS(*) ,XINER(*),
     .   RY1(*), RZ1(*), OFF(*), XMOM(*), YMOM(*),
     .   ZMOM(*), RX(*), RY2(*), RZ2(*),XL(*),
     .   STIFM(*) ,STIFR(*) , VISCM(*) ,VISCR(*) ,FR_WAVE(*) ,
     .   GET_U_MAT, GET_U_GEO, GET_U_FUNC, GET_U_TIME
      EXTERNAL GET_U_MNU,GET_U_PNU,GET_U_MID,GET_U_PID,
     .         GET_U_MAT,GET_U_GEO, GET_U_FUNC, GET_U_TIME,
     .         GET_SPRING_ELNUM
      PARAMETER (KFUNC=29)
!      PARAMETER (KMAT=31)
!      PARAMETER (KPROP=33)
C=======================================================================
C
C     EXAMPLE 2 : Elastoplastic truss defined with 1 user property.
C                 
C               
C=======================================================================
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFUNC1,IFUNC2,IFUNC3,IFUNC4,IFUNC5,EPSI
      my_real
     .        ELASTIF,X,DXDY,XLIM1,XLIM2,FMX,FMN,DX,DAMP,
     .        FUX1,FUX2,FUX3,FUX4,DEBUG,SLOPE
      my_real
     .        TT,NEWLEN,DELTALEN,DELTALENDOT,SCALET,SCALEX,SCALEV,SCALEF
C-----------------------------------------------
C
      ELASTIF= GET_U_GEO(2,IPROP)
      XLIM1  = GET_U_GEO(3,IPROP) !vmax
      XLIM2  = GET_U_GEO(4,IPROP) !fmax
      DAMP   = GET_U_GEO(6,IPROP)
      SCALET = GET_U_GEO(8,IPROP)
      SCALEX = GET_U_GEO(9,IPROP)
      SCALEV = GET_U_GEO(10,IPROP)
      SCALEF = GET_U_GEO(11,IPROP)

      EPSI   = INT(GET_U_GEO(7,IPROP))
      IFUNC1= GET_U_PNU(1,IPROP,KFUNC)
      IFUNC2= GET_U_PNU(2,IPROP,KFUNC)
      IFUNC3= GET_U_PNU(3,IPROP,KFUNC)
      IFUNC4= GET_U_PNU(4,IPROP,KFUNC)
      IFUNC5= GET_U_PNU(5,IPROP,KFUNC)
C 
      TT= GET_U_TIME()
      TT = TT / SCALET
C 
      DELTALENDOT = ZERO
C
C      DEBUG = GET_U_FUNC(IFUNC5,TT,DXDY)
C Loop over elements
      DO I=1,NEL
          DX = DT * VX(I)
          X = UVAR(1,I) + DX
          UVAR(1,I) = X
          NEWLEN = UVAR(3,I) + UVAR(1,I)!UVAR(3,I)=initial length of the spring at time=0
C
          IF (EPSI == 0) THEN
             DELTALEN   = NEWLEN/UVAR(3,I) - ONE
                IF (DT == ZERO) THEN
                   DELTALEN = ZERO
                ELSE
                   DELTALENDOT = NEWLEN/UVAR(3,I)*VX(I)/XLIM1
                ENDIF
          ELSE
             DELTALEN = X
             DELTALENDOT = VX(I)
          ENDIF
C
C Active force vs. time
          FUX1 = GET_U_FUNC(IFUNC1,TT,DXDY)
C Active force vs. elongation
          IF(EPSI/= 0)DELTALEN = DELTALEN / SCALEX
          FUX2 = GET_U_FUNC(IFUNC2,DELTALEN,DXDY)
C Active force vs. velocity
C          FUX3 = GET_U_FUNC(IFUNC3,VX(I),DXDY)
          DELTALENDOT = DELTALENDOT / SCALEV
          FUX3 = GET_U_FUNC(IFUNC3,DELTALENDOT,DXDY)
C Passive force vs. elongation
          FUX4 = GET_U_FUNC(IFUNC4,DELTALEN,DXDY)
          FUX4 = FUX4 * SCALEF
C Check if velocity > limit1 ...if YES, then use XLIM1 = max_vel
C Sum FX + damping*velocity
          IF (VX(I)>XLIM1)THEN
             FX(I) = XLIM2 * FUX1 * FUX2 * FUX3 + FUX4 + DAMP*XLIM1
          ELSEIF (VX(I)<-XLIM1)THEN
             FX(I) = XLIM2 * FUX1 * FUX2 * FUX3 + FUX4 - DAMP*XLIM1
          ELSE
             FX(I) = XLIM2 * FUX1 * FUX2 * FUX3 + FUX4 + DAMP*VX(I)
          ENDIF
C
C        ***       TIMESTEP       ***
C=======================================================================
C       TIME STEP
C=======================================================================
C          STIFM(I) = ELASTIF / XL(I)
          IF(DELTALEN /= ZERO)THEN
            SLOPE = ABS(FX(I)-UVAR(4,I))/ABS(DX)
C            WRITE(IOUT,*) 'Slope: FR_WAVE = ZERO'
          ELSE
            SLOPE = ELASTIF
          ENDIF
C

C
          STIFM(I) = SLOPE
          STIFR(I) = ZERO
          VISCM(I) = ZERO
          VISCR(I) = ZERO
          XINER(I) = ZERO
          UVAR(4,I)= FX(I)
      ENDDO
C-------------------------------
      RETURN
      END
