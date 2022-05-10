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
Chd|  RUSER35                       source/elements/spring/ruser35.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        GET_U_PNU                     source/user_interface/upidmid.F
Chd|====================================================================
      SUBROUTINE RUSER35(
     1             NEL     ,IOUT    ,IPROP   ,UVAR   ,NUVAR  ,
     2             FX      ,FY      ,FZ      ,XMOM   ,YMOM   ,
     3             ZMOM    ,E       ,OFF     ,STIFM  ,STIFR  ,
     4             VISCM   ,VISCR   ,MASS    ,XINER  ,DT     ,
     5             XL      ,VX      ,RY1     ,RZ1    ,RX     ,
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
C utilisation de la version 4.1d ==> on recupere la masse
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
     .        KFUNC,KMAT,KPROP
      my_real
     .   UVAR(NUVAR,*),DT ,
     .   FX(*), FY(*), FZ(*), E(*), VX(*),MASS(*) ,XINER(*),
     .   RY1(*), RZ1(*), OFF(*), XMOM(*), YMOM(*),
     .   ZMOM(*), RX(*), RY2(*), RZ2(*),XL(*),
     .   STIFM(*) ,STIFR(*) , VISCM(*) ,VISCR(*) ,FR_WAVE(*) ,
     .   GET_U_MAT, GET_U_GEO, GET_U_FUNC
      EXTERNAL GET_U_MNU,GET_U_PNU,GET_U_MID,GET_U_PID,
     .         GET_U_MAT,GET_U_GEO, GET_U_FUNC
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=33)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFUNC1,IFUNC2,IFUNC3,IFUNC4,ILOAD
      my_real 
     .        ELASTIF,X,DXDY,XLIM1,XLIM2,FMX,FMN,DX,AMAS,D1,D2,FSCAL
C-----------------------------------------------
C
      AMAS   = GET_U_GEO(1,IPROP)
      ELASTIF= GET_U_GEO(2,IPROP)
      XLIM1  = GET_U_GEO(3,IPROP)
      XLIM2  = GET_U_GEO(4,IPROP)
      D1     = GET_U_GEO(5,IPROP)
      D2     = GET_U_GEO(6,IPROP)
      FSCAL  = GET_U_GEO(8,IPROP)
      ILOAD  = NINT(GET_U_GEO(7,IPROP))
      IFUNC1= GET_U_PNU(1,IPROP,KFUNC) 
      IFUNC2= GET_U_PNU(2,IPROP,KFUNC) 
      IFUNC3= GET_U_PNU(3,IPROP,KFUNC) 
      IFUNC4= GET_U_PNU(4,IPROP,KFUNC) 
C
      DO I=1,NEL
        MASS(I) = AMAS
        DX = DT * VX(I) / XL(I)
        X = UVAR(1,I) + DX
c
        IF (UVAR(3,I) == ZERO) THEN
          FMX = FSCAL*GET_U_FUNC(IFUNC1,X,DXDY)
          FMN = FSCAL*GET_U_FUNC(IFUNC2,X,DXDY)
        ELSE            
          FMX = UVAR(2,I)*FSCAL*GET_U_FUNC(IFUNC3,X,DXDY)
          FMN = UVAR(2,I)*FSCAL*GET_U_FUNC(IFUNC4,X,DXDY)            
        ENDIF
c
        IF (UVAR(3,I) >= UN) THEN
          FR_WAVE(I)=UN
c  UVAR(2,I) = endommagement sur fonction yield et stiffness 
c              par facteur 0 <= D1 < 1.              
          IF (ILOAD == 0 .OR. DX > ZERO) UVAR(2,I)=UVAR(2,I)*(UN-D1)
        ELSEIF (FR_WAVE(I) == UN) THEN
          IF (ILOAD == 0 .OR. DX > ZERO) UVAR(3,I)=UVAR(3,I)+D2    
c FR_WAVE est laisse a 1.
        ENDIF
C
        IF (X >= XLIM1) THEN
          FR_WAVE(I)=UN
          IF (UVAR(3,I) >= UN .AND. OFF(I) == UN) THEN
Cqz  change I to global indice
            WRITE(iout,*)'SPRING',I, 'REACHES LIMIT IN X : ',X
            OFF(I)=ZERO
          ENDIF
        ELSE
          FR_WAVE(I)=ZERO
        ENDIF
c
        UVAR(1,I) = X 
        FX(I) = FX(I) + ELASTIF * DX * UVAR(2,I)
        FX(I) = MIN(FX(I),FMX)
        FX(I) = MAX(FX(I),FMN)
        FX(I) = FX(I) * OFF(I)
C----------------
C         TIME STEP
C----------------
        STIFM(I) = ELASTIF / XL(I)
        STIFR(I) = ZERO
        VISCM(I) = ZERO
        VISCR(I) = ZERO
        XINER(I) = ZERO 
      ENDDO
C-------------------------------
      RETURN
      END
