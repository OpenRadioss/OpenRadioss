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
Chd|  XANIM29                       source/output/anim/generate/xanim29.F
Chd|-- called by -----------
Chd|        ANIMX                         source/output/anim/generate/animx.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE XANIM29(NX ,UIX     ,UID     ,XEL     ,
     2              IOUT    ,IPROP   ,IMAT    ,
     3              OFF     ,MASSELE ,EINTELE ,
     4              NEDGE   ,NFACET  ,NSOLID  ,
     5              IXEDGE  ,IXFACET ,IXSOLID ,
     6              XMASSED ,XMASSFA ,XMASSSO ,
     7              XFUNCED ,XFUNCFA ,XFUNCSO ,
     8              NUVAR   ,UVAR    ,NUVARN  ,UVARN  )
C-------------------------------------------------------------------------
C     This subroutine returns edges, facets and solids to be written into 
C     ANIMATION FILES.
C-------------------------------------------------------------------------
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C NX       |    1    | I | R | NUMBER OF NODES (CONSTANT IN THE GROUP)
C----------+---------+---+---+--------------------------------------------
C UID      |    1    | I | R | ELEMENT USER IDENTIFIER
C UIX      |   NX    | I | R | ELEMENT CONNECTIVITY
C                            | IX(J) (1<=J<=NX) : NODE J USER ID
C----------+---------+---+---+--------------------------------------------
C XEL      | 3*NX    | F | R | NODES COORDINATES
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IPROP    |  1      | I | R | PROPERTY NUMBER
C IMAT     |  1      | I | R | MATERIAL NUMBER
C----------+---------+---+---+--------------------------------------------
C OFF      |  1      | F | R | ELEMENT STATE (ON=1./OFF=0.)
C MASSELE  |  1      | F | R | ELEMENT (INITIAL) MASS
C EINTELE  |  1      | F | R | ELEMENT INTERNAL ENERGY
C----------+---------+---+---+--------------------------------------------
C NUVAR    |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR     |NUVAR    | F |R/W| USER ELEMENT VARIABLES 
C                            |               (FIX SIZE ZONE)
C NUVARN   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES PER NODE
C UVARN    |NUVARN*NX| F |R/W| USER ELEMENT VARIABLES PER NODE 
C                            |               (NX DEPENDENT SIZE ZONE)
C----------+---------+---+---+--------------------------------------------
C-------------------------------------------------------------------------
C FUNCTION 
C-------------------------------------------------------------------------
C INTEGER II = GET_U_PNU(I,IP,KK)
C         IPROPI = GET_U_PNU(I,IP,KFUNC)
C         IMATI  = GET_U_PNU(I,IP,KMAT)
C         I     :     VARIABLE INDEX(1 for first variable,...)
C         IP    :     PROPERTY NUMBER
C         KK    :     PARAMETER KFUNC,KMAT,KPROP
C         THIS FUNCTION RETURN THE USER STORED MATERIAL(IF KK=KMAT) 
C         OR PROPERTY(IF KK=KPROP) NUMBER. 
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER IOUT,NUVAR,NUVARN,IPROP,IMAT,
     .        NX ,UIX(NX) ,UID,
     .        NEDGE, NFACET, NSOLID,
     .        IXEDGE(2,*), IXFACET(4,*), IXSOLID(8,*),
     .        GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU,
     .        KMAT,KPROP
      my_real
     .   XEL(3,NX) , OFF, MASSELE, EINTELE,
     .   XMASSED(*), XMASSFA(*), XMASSSO(*),
     .   XFUNCED(10,*), XFUNCFA(10,*), XFUNCSO(10,*),
     .   UVAR(NUVAR),UVARN(NUVARN*NX),
     .   GET_U_MAT, GET_U_GEO
      EXTERNAL GET_U_MNU,GET_U_PNU,GET_U_MID,GET_U_PID,
     .         GET_U_MAT,GET_U_GEO
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=33)
C=======================================================================
      RETURN
      END
