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
Chd|  XINI31                        source/elements/xelem/xini31.F
Chd|-- called by -----------
Chd|        XINIT3                        source/elements/xelem/xinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE XINI31(NX    ,NAX1D  ,NAX2D ,NAX3D ,
     2                  XEL   ,VEL    ,VREL  ,
     2                  IOUT  ,IPROP  ,IMAT  ,
     3                  IX    ,IDS    ,MASS  ,
     4                  XINER  ,STIFM ,STIFR ,VISCM  ,VISCR ,
     5                  UVAR   ,NUVAR ,UVARN ,NUVARN ,DTE )
C-------------------------------------------------------------------------
C     This subroutine initialize multipurpose element 
C     (when element uses user property TYPE31).
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C IMAT     |  1      | I | R | MATERIAL NUMBER
C----------+---------+---+---+--------------------------------------------
C XEL      | 3*NX    | F | R | NODES COORDINATES
C VEL      | 3*NX    | F | R | NODES VELOCITIES
C VREL     | 3*NX    | F | R | NODES ROTATIONAL VELOCITIES
C----------+---------+---+---+--------------------------------------------
C NX       |    1    | I | R | NUMBER OF NODES (CONSTANT IN THE GROUP)
C----------+---------+---+---+--------------------------------------------
C NAX1D    |    1    | I | W | NUMBER OF EDGES TO BE DRAWN INTO ANIM
C NAX2D    |    1    | I | W | NUMBER OF FACETS TO BE DRAWN INTO ANIM
C NAX3D    |    1    | I | W | NUMBER OF SOLIDS TO BE DRAWN INTO ANIM
C----------+---------+---+---+--------------------------------------------
C IX       |   NX    | I | R | ELEMENT CONNECTIVITY
C                            | IX(J) (1<=J<=NX) : NODE J USER ID
C IDS      |    1    | I | R | ELEMENT USER IDENTIFIER
C----------+---------+---+---+--------------------------------------------
C MASS     |   NX    | F | W | NODAL MASS
C XINER    |   NX    | F | W | NODAL INERTIA (SPHERICAL)
C STIFM    |   NX    | F | W | NODAL STIFNESS (TIME STEP)
C STIFR    |   NX    | F | W | NODAL ROTATION STIFNESS (TIME STEP)
C VISCM    |   NX    | F | W | NODAL VISCOSITY (TIME STEP)
C VISCR    |   NX    | F | W | NODAL ROTATION VISCOSITY (TIME STEP)
C----------+---------+---+---+--------------------------------------------
C UVAR     |NUVAR    | F | W | USER ELEMENT VARIABLES
C NUVAR    |    1    | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVARN    |NUVARN*NX| F | W | USER ELEMENT VARIABLES
C NUVARN   |    1    | I | R | NUMBER OF USER ELEMENT VARIABLES PER NODE.
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
C         SEE LECG31 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C INTEGER IFUNCI = GET_U_MNU(I,IM,KFUNC)
C         I     :     VARIABLE INDEX(1 for first function)
C         IM    :     MATERIAL NUMBER
C         KFUNC :     ONLY FUNCTION ARE YET AVAILABLE.
C         THIS FUNCTION RETURN THE USER STORED FUNCTION NUMBERS(function 
C         refered by users materials).
C         SEE LECM31 FOR CORRESPONDING ID STORAGE.
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
       USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER IOUT,NUVAR,NUVARN,IPROP,IMAT,
     .        NX ,NAX1D ,NAX2D ,NAX3D , IX(NX), IDS,
     .        GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU,
     .        KFUNC,KMAT,KPROP
      my_real
     .        XEL(3,NX),VEL(3,NX),VREL(3,NX),
     .        MASS(NX) ,XINER(NX) ,STIFM(NX) ,
     .        STIFR(NX),VISCM(NX) ,VISCR(NX) ,UVAR(NUVAR) ,
     .        UVARN(NUVARN*NX), DTE,
     .        GET_U_MAT,GET_U_GEO,GET_U_FUNC
c jbm024
      EXTERNAL GET_U_PNU,GET_U_MNU,GET_U_MAT,GET_U_GEO,GET_U_PID,
     .         GET_U_MID,GET_U_FUNC
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=33)
C=================================================================
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CALL ANCMSG(MSGID=1613,ANMODE=ANSTOP,
     .                       MSGTYPE=MSGERROR)
      END
