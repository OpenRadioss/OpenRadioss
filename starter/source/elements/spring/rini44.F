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
Chd|  RINI44                        source/elements/spring/rini44.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|====================================================================
      SUBROUTINE RINI44(NEL   ,IOUT   ,IPROP ,
     3                  IX    ,XL     ,MASS  ,XINER  ,STIFM ,
     4                  STIFR ,VISCM  ,VISCR ,UVAR   ,NUVAR )
C-------------------------------------------------------------------------
C     This subroutine initialize springs using user properties.
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C----------+---------+---+---+--------------------------------------------
C IX       | 3*NEL   | I | R | SPRING CONNECTIVITY
C                            | IX(1,I) NODE 1 ID
C                            | IX(2,I) NODE 2 ID
C                            | IX(3,I) OPTIONNAL NODE 3 ID
C                            | IX(4,I) SPRING ID
C XL       |   NEL   | F | R | ELEMENT LENGTH
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
C-------------------------------------------------------------------------
C FUNCTION 
C-------------------------------------------------------------------------
C INTEGER II = GET_U_PNU(I,IP,KK)
C         IFUNCI = GET_U_PNU(I,IP,KFUNC)
C         IPROPI = GET_U_PNU(I,IP,KPROP)
C         IMATI = GET_U_PNU(I,IP,KMAT)
C         I     :     VARIABLE INDEX(1 for first variable,...)
C         IP    :     PROPERTY NUMBER
C         KK    :     PARAMETER KFUNC,KMAT,KPROP
C         THIS FUNCTION RETURN THE USER STORED FUNCTION(IF KK=KFUNC), 
C         MATERIAL(IF KK=KMAT) OR PROPERTY(IF KK=KPROP) NUMBERS. 
C         SEE LECG29 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C INTEGER IFUNCI = GET_U_MNU(I,IM,KFUNC)
C         I     :     VARIABLE INDEX(1 for first function)
C         IM    :     MATERIAL NUMBER
C         KFUNC :     ONLY FUNCTION ARE YET AVAILABLE.
C         THIS FUNCTION RETURN THE USER STORED FUNCTION NUMBERS(function 
C         refered by users materials).
C         SEE LECM29 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C my_real PARAMI = GET_U_GEO(I,IP)
C         I     :     PARAMETER INDEX(1 for first parameter,...)
C         IP    :     PROPERTY NUMBER
C         THIS FUNCTION RETURN THE USER GEOMETRY PARAMETERS 
C-------------------------------------------------------------------------
C my_real PARAMI = GET_U_MAT(I,IM)
C         I     :     PARAMETER INDEX(1 for first parameter,...)
C         IM    :     MATERIAL NUMBER
C         THIS FUNCTION RETURN THE USER MATERIAL PARAMETERS 
C         NOTE: GET_U_MAT(0,IMAT) RETURN THE DENSITY
C-------------------------------------------------------------------------
C INTEGER MID = GET_U_PID(IP)
C         IP    :     PROPERTY NUMBER
C         THIS FUNCTION RETURN THE USER PROPERTY ID CORRESPONDING TO
C         USER PROPERTY NUMBER IP. 
C-------------------------------------------------------------------------
C INTEGER PID = GET_U_MID(IM)
C         IM   :     MATERIAL NUMBER
C         THIS FUNCTION RETURN THE USER MATERIAL ID CORRESPONDING TO
C         USER MATERIAL NUMBER IM. 
C-------------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER IOUT,NUVAR,NEL,IPROP,   
     .        IX(4,NEL) ,
     .        GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU,
     .        KFUNC,KMAT,KPROP
      my_real 
     .        XL(NEL) ,MASS(NEL) ,XINER(NEL) ,STIFM(NEL) ,
     .        STIFR(NEL),VISCM(NEL) ,VISCR(NEL),UVAR(NUVAR,*),
     .        GET_U_MAT,GET_U_GEO
      EXTERNAL GET_U_PNU,GET_U_MNU,GET_U_MAT,GET_U_GEO,GET_U_PID,
     .        GET_U_MID
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=47)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real 
     .        AMAS,INER,K11,K44,K55,K66,K5B,K6C,
     .        XLIMG,XLIM,XXLIM,YY1LIM,YY2LIM,ZZ1LIM,ZZ2LIM,
     .        NCF,IDAMPING
      INTEGER I
C=================================================================
        AMAS  = GET_U_GEO(8,IPROP)
        INER  = GET_U_GEO(9,IPROP)
        K11   = GET_U_GEO(10,IPROP)
        K44   = GET_U_GEO(11,IPROP)
        K55   = GET_U_GEO(12,IPROP)
        K66   = GET_U_GEO(13,IPROP)
        K5B   = GET_U_GEO(14,IPROP)
        K6C   = GET_U_GEO(15,IPROP)
!  strain rate filtering number of cycles:
        NCF   = GET_U_GEO(35,IPROP)
!  flag for damping activation
        IDAMPING = GET_U_GEO(36,IPROP)
C--------------------------------------
C       ELEMENT CHECK
C--------------------------------------
        DO I=1,NEL
          IF (XL(I) == ZERO)THEN
            WRITE(IOUT,*)' **ERROR ZERO LENGTH SPRING :'
          ENDIF
        ENDDO
C--------------------------------------
C       ELEMENT INITIALIZATION
C--------------------------------------
        DO I=1,NEL
          MASS(I)   = AMAS * XL(I)
          XINER(I)  = INER * XL(I)
C EPS
          UVAR(1,I)  = ZERO
          UVAR(2,I)  = ZERO
          UVAR(3,I)  = ZERO
          UVAR(4,I)  = ZERO
          UVAR(5,I)  = ZERO
          UVAR(6,I)  = ZERO
C JPOS
          UVAR(7,I)  = ZERO
          UVAR(8,I)  = ZERO
          UVAR(9,I)  = ZERO
          UVAR(10,I) = ZERO
          UVAR(11,I) = ZERO
          UVAR(12,I) = ZERO
          UVAR(13,I) = ZERO
          UVAR(14,I) = ZERO
          UVAR(15,I) = ZERO
          UVAR(16,I) = ZERO
          UVAR(17,I) = ZERO
          UVAR(18,I) = ZERO
C K
          UVAR(19,I) = K11 / XL(I)
          UVAR(20,I) = K44 / XL(I)
          UVAR(21,I) = K55 / XL(I) / XL(I) / XL(I)
          UVAR(22,I) = K66 / XL(I) / XL(I) / XL(I)
          UVAR(23,I) = K5B / XL(I) / XL(I) / XL(I)
          UVAR(24,I) = K6C / XL(I) / XL(I) / XL(I)
C         FOR NODAL AND ELEMENT TIME STEP COMPUTATION
          UVAR(25,I) = MAX(UVAR(19,I),
     .                     UVAR(21,I)+ABS(UVAR(23,I)),
     .                     UVAR(22,I)+ABS(UVAR(24,I)))
          UVAR(26,I) = MAX(UVAR(20,I),K55 / XL(I),K66 / XL(I))
          STIFM(I) = UVAR(25,I)
          STIFR(I) = UVAR(26,I)
          VISCM(I) = ZERO
          VISCR(I) = ZERO
          UVAR(27,I) = MASS(I)
          UVAR(28,I) = XINER(I)
          UVAR(30,I) = UN/XL(I)
!
!  add damping (linear + function):
!
          IF (IDAMPING > ZERO) THEN
            UVAR(31,I) = ZERO
            UVAR(32,I) = ZERO
            UVAR(33,I) = ZERO
            UVAR(34,I) = ZERO
            UVAR(35,I) = ZERO
            UVAR(36,I) = ZERO
          ENDIF ! IF (IDAMPING > ZERO)
! + strain rate filtering
          IF (NCF > ZERO) THEN
            UVAR(37,I) = ZERO
            UVAR(38,I) = ZERO
            UVAR(39,I) = ZERO
            UVAR(40,I) = ZERO
            UVAR(41,I) = ZERO
            UVAR(42,I) = ZERO
          ENDIF ! IF (NCF > ZERO)
        ENDDO
C-----------
      RETURN
      END
