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
CCgw|============================================================       
Chd|====================================================================
Chd|  INITBUF                       source/output/anim/initbuf.F  
Chd|-- called by -----------
Chd|        DFUNCS                        source/output/anim/dfuncs.F   
Chd|        DMASANIS                      source/output/anim/dmasanis.F 
Chd|        TENSORS                       source/output/anim/tensors.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INITBUF (IPARG    ,NG      ,
     2   MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,
     3   NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4   JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5   JPOR    ,JCVT    ,JCLOSE  ,JPLA    ,
     6   IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7   ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE)
C-----------------------------------------------                        
C   I m p l i c i t   T y p e s                                         
C-----------------------------------------------                        
#include      "implicit_f.inc"
C-----------------------------------------------                        
C   C o m m o n   B l o c k s                                           
C-----------------------------------------------                        
#include      "param_c.inc"
C-----------------------------------------------                        
C   D u m m y   A r g u m e n t s                                       
C-----------------------------------------------                        
      INTEGER, INTENT (IN)  :: IPARG(NPARG,*),NG
      INTEGER, INTENT (OUT) :: MTN,LLT,NFT,IAD,ITY,NPT,JALE,ISMSTR,
     .   JEUL,JTUR,JTHE,JLAG,JMULT,JHBE,JIVF,JPOR,JPLA,JCLOSE,
     .   IREP,IINT,IGTYP,JCVT,ISROT,ISRAT,ISORTH,ISORTHG,ICSEN,IFAILURE
C-----------------------------------------------                        
C   S o u r c e  L i n e s                                              
C====================================================================== 
      MTN     = IPARG(1,NG)
      LLT     = IPARG(2,NG)
      NFT     = IPARG(3,NG)
      IAD     = IPARG(4,NG)
      ITY     = IPARG(5,NG)
      NPT     = IPARG(6,NG)
      JALE    = IPARG(7,NG)

      ISMSTR  = IPARG(9,NG)
      IF(ITY==1.OR.ITY==2)THEN
        JEUL    = IPARG(11,NG)
      ELSE             ! attention dkt9_s3 utilise ce flag
        JEUL    = 0
      END IF
      JTUR    = IPARG(12,NG)
      JTHE    = IPARG(13,NG)
      JLAG    = IPARG(14,NG)
      JMULT   = IPARG(20,NG)
      JHBE    = IPARG(23,NG)
      JIVF    = IPARG(24,NG)
      JPOR    = IPARG(27,NG)
      JPLA    = IPARG(29,NG)
      JCLOSE  = IPARG(33,NG)
      IREP    = IPARG(35,NG)
      IINT    = IPARG(36,NG)
      JCVT    = IPARG(37,NG)
      IGTYP   = IPARG(38,NG)
      ICSEN   = IPARG(39,NG)
      ISRAT   = IPARG(40,NG)
      ISROT   = IPARG(41,NG)
      ISORTH  = IPARG(42,NG)
      ISORTHG = ISORTH
      IFAILURE = IPARG(43,NG)
C----                                                                   
      RETURN
      END
