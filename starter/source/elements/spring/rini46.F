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
!||    rini46      ../starter/source/elements/spring/rini46.F
!||--- called by ------------------------------------------------------
!||    rinit3      ../starter/source/elements/spring/rinit3.F
!||--- calls      -----------------------------------------------------
!||    get_u_geo   ../starter/source/user_interface/uaccess.F
!||    get_u_mat   ../starter/source/user_interface/uaccess.F
!||    get_u_mid   ../starter/source/user_interface/uaccess.F
!||    get_u_mnu   ../starter/source/user_interface/uaccess.F
!||    get_u_pid   ../starter/source/user_interface/uaccess.F
!||    get_u_pnu   ../starter/source/user_interface/uaccess.F
!||====================================================================
      SUBROUTINE RINI46(NEL   ,IOUT   ,IPROP ,
     3                  IX    ,XL     ,MASS  ,XINER  ,STIFM ,
     4                  STIFR ,VISCM  ,VISCR ,UVAR   ,NUVAR )
C-------------------------------------------------------------------------
C     This subroutine initialize springs using user properties.
C-------------------------------------------------------------------------
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C----------+---------+---+---+--------------------------------------------
C IX       | 3*NEL   | I | R | SPRING CONNECTIVITY
C                            | IX(1,I) NODE 1 ID
C                            | IX(2,I) NODE 2 ID
C                            | IX(3,I) OPTIONAL NODE 3 ID
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
C----------+---------+---+---+--------------------------------------------
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
C         referred by users materials).
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
     .         GET_U_MID
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=33)
C=======================================================================
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .        AMAS,AMAS2,ELASTIF
      INTEGER I,IDENS
C-----------------------------------------------
        AMAS     = GET_U_GEO(1,IPROP)
        AMAS2    = AMAS
        ELASTIF  = GET_U_GEO(2,IPROP)
        IDENS    = INT(GET_U_GEO(12,IPROP))

C
C       MEAN VALUES
C
C--------------------------------------
C       ELEMENT CHECK
C--------------------------------------
        DO I=1,NEL
          IF(XL(I)== ZERO)THEN
            WRITE(IOUT,*)' **ERROR: ZERO LENGTH SPRING :'
          ENDIF
        ENDDO
C--------------------------------------
C       ELEMENT INITIALIZATION
C--------------------------------------
        DO I=1,NEL
          IF (IDENS == 0 )THEN
             AMAS2 = AMAS* XL(I)
          ENDIF
          MASS(I)   = AMAS2
          XINER(I)  = ZERO
C
C Initialization of UVAR(1,I) with the delta length of the spring
C
          UVAR(1,I) = ZERO
          UVAR(2,I) = ONE
C
C Initialization of UVAR(3,I) with the initial length of the spring at time=0
C
          UVAR(3,I) = XL(I)
C
C Initialization of UVAR(4,I) with Zero (will be the Force of previous cycle)
C
          UVAR(4,I) = ZERO
C
C FOR NODAL AND ELEMENT TIME STEP COMPUTATION
C         STIFM(I) = ELASTIF / XL(I)
          STIFM(I) = ELASTIF
          STIFR(I) = ZERO
          VISCM(I) = ZERO
          VISCR(I) = ZERO
        ENDDO
C
      RETURN
      END
