Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  XFORC31                       source/elements/xelem/xforc31.F
Chd|-- called by -----------
Chd|        XFORC3                        source/elements/xelem/xforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE XFORC31(NX     ,
     2             XEL     ,VEL     ,VREL   ,UIX    ,UID    ,
     3             IOUT    ,IPROP   ,IMAT   ,OFF    ,KEINT  ,
     4             EINT   ,MASS    ,XINER   ,STIFM  ,STIFR  ,
     5             VISCM  ,VISCR   ,FORC    ,TORQ   ,
     6             NUVAR   ,UVAR    ,NUVARN ,UVARN  ,
     7             DT      ,DTE     )
C-------------------------------------------------------------------------
C     This subroutine computes multipurpose element forces and moments 
C     (when element uses user property TYPE31).
C-------------------------------------------------------------------------
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C NX       |    1    | I | R | NUMBER OF NODES (CONSTANT IN THE GROUP)
C----------+---------+---+---+--------------------------------------------
C XEL      | 3*NX    | F | R | NODES COORDINATES
C VEL      | 3*NX    | F | R | NODES VELOCITIES
C VREL     | 3*NX    | F | R | NODES ROTATIONAL VELOCITIES
C----------+---------+---+---+--------------------------------------------
C UIX      |   NX    | I | R | ELEMENT CONNECTIVITY
C                            | IX(J) (1<=J<=NX) : NODE J USER ID
C UID      |    1    | I | R | ELEMENT USER IDENTIFIER
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C IMAT     |  1      | I | R | MATERIAL NUMBER
C----------+---------+---+---+--------------------------------------------
C OFF      |  1      | F | W | ELEMENT STATE (ON=1./OFF=0.)
C KEINT    |  1      | I | W | ELEMENT INTERNAL ENERGY FLAG
C                        | 0 | HAS TO BE COMPUTED BY RADIOSS OUT OF USER ROUTINE.
C                        | 1 | IS RETURNED BY THIS USER ROUTINE.
C EINT     |  1      | F | W | ELEMENT INTERNAL ENERGY IF KEINT=1
C----------+---------+---+---+--------------------------------------------
C MASS     |   NX    | F | W | NODAL MASS
C XINER    |   NX    | F | W | NODAL INERTIA (SPHERICAL)
C STIFM    |   NX    | F | W | NODAL STIFNESS (TIME STEP)
C STIFR    |   NX    | F | W | NODAL ROTATION STIFNESS (TIME STEP)
C VISCM    |   NX    | F | W | NODAL VISCOSITY (TIME STEP)
C VISCR    |   NX    | F | W | NODAL ROTATION VISCOSITY (TIME STEP)
C----------+---------+---+---+--------------------------------------------
C FORC     | 3*NX    | F | W | NODAL FORCES
C TORQ     | 3*NX    | F | W | NODAL TORQUS
C----------+---------+---+---+--------------------------------------------
C NUVAR    |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR     |NUVAR    | F |R/W| USER ELEMENT VARIABLES 
C                            |               (FIX SIZE ZONE)
C NUVARN   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES PER NODE
C UVARN    |NUVARN*NX| F |R/W| USER ELEMENT VARIABLES PER NODE 
C                            |               (NX DEPENDENT SIZE ZONE)
C----------+---------+---+---+--------------------------------------------
C DT       |  1      | F | R | PREVIOUS TIME INCREMENT :
C                            |    CURRENT TIME POINT IS T = TOLD+DT
C----------+---------+---+---+--------------------------------------------
C DTE      |  1      | F | W | ELEMENT TIME STEP
C----------+---------+---+---+--------------------------------------------
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
C         SEE LECG31 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C INTEGER IFUNCI = GET_U_MNU(I,IM,KFUNC)
C         I     :     VARIABLE INDEX(1 for first function)
C         IM    :     MATERIAL NUMBER
C         KFUNC :     ONLY FUNCTION ARE YET AVAILABLE.
C         THIS FUNCTION RETURN THE USER STORED FUNCTION NUMBER(function 
C         referred by users materials).
C         SEE LECM31 FOR CORRESPONDING ID STORAGE.
C-------------------------------------------------------------------------
C my_real PARAMI = GET_U_GEO(I,IP)
C         I     :     PARAMETER INDEX(1 for first parameter,...)
C         IP    :     PROPERTY NUMBER
C         THIS FUNCTION RETURN THE USER GEOMETRY PARAMETERS 
C         NOTE: IF(IP==IPROP) UPARAG(I) == GET_U_GEO(I,IPROP)
C-------------------------------------------------------------------------
C my_real PARAMI = GET_U_MAT(I,IM)
C         I     :     PARAMETER INDEX(1 for first parameter,...)
C         IM    :     MATERIAL NUMBER
C         THIS FUNCTION RETURN THE USER MATERIAL PARAMETERS 
C         NOTE: GET_U_MAT(0,IMAT) RETURN THE DENSITY
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
      INTEGER IOUT,NUVAR,NUVARN,IPROP,IMAT,
     .        NX ,UIX(NX) ,UID, KEINT,
     .        GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU,
     .        KFUNC,KMAT,KPROP
      my_real
     .   OFF, EINT, XEL(3,NX),  VEL(3,NX) ,VREL(3,NX),
     .   MASS(NX) ,  XINER(NX) ,STIFM(NX) ,
     .   STIFR(NX),  VISCM(NX) ,VISCR(NX) ,
     .   FORC(3,NX), TORQ(3,NX),
     .   UVAR(NUVAR),UVARN(NUVARN*NX),DT ,DTE ,
     .   GET_U_MAT, GET_U_GEO, GET_U_FUNC
      EXTERNAL GET_U_MNU,GET_U_PNU,GET_U_MID,GET_U_PID,
     .         GET_U_MAT,GET_U_GEO, GET_U_FUNC
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=33)
C=======================================================================
      RETURN
      END
