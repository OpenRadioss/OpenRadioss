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
Chd|  TAGINT_R2R                    source/coupling/rad2rad/tagint_r2r.F
Chd|-- called by -----------
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE TAGINT_R2R(G1,G2,GRS,GRM,ID_INTER,
     2           TYPE2,VAL,TAG,I,COMPT,PASSE,FLAG,IGRPP_R2R,
     3           IGRNOD ,IGRSURF ,IGRSLIN, IGRBRIC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD      
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "r2r_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER G1,G2,TAG,TYPE2,VAL,I,
     .   GRS,GRM,ID_INTER,COMPT,PASSE,NBTOT,FLAG,IGRPP_R2R(2,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,TAG2,NF1,NF2,NF1T,NF2T,W
      INTEGER N_SURFP,N_SURFT,N_NP,N_NT,N_NS
      INTEGER N_SURFP2,N_SURFT2,I1,I2,N_GRBP,N_GRBT
      INTEGER N_LINP,N_LINT,N_LINP2,N_LINT2      
C=======================================================================
C-->  Main domain : domain in which contact between domains is treated - where void elts are generated
C-->  Second. domain : domain in which contact between domains is not treated 
C=======================================================================
C-->  TAG = -1 : external interface for current domain
C-->  TAG =  0 : internal interface for current domain
C-->  TAG =  1 : interf. between 2 domains - general case
C-->  TAG =  2 : interf. between 2 domains - main side is only main side of contact interface
C-->  TAG =  3 : interf. between 2 domains - main side is only second. side of contact interface
C-->  TAG =  4 : interf. type2 between 2 domains
C=======================================================================      
C-->  N_np = nb of nodes only in the main domain
C-->  N_nt = N_np + nb of common nodes between domains
C-->  N_ns = nb of nodes in second. domain
C-->  N_surfp = nb of segments only in main domain
C-->  N_surft = Nb_surfp + nb of common segments between domains	
C=======================================================================
              
      G1 = 0
      G2 = 0
      TAG = 1
      TAG2 = 1 
       
C--------------------------------------------------------------------C
C------------FLAG = 0 --> surface / nodes interfaces ----------------C
C--------------------------------------------------------------------C

      IF (FLAG.EQ.0) THEN
       
C-->  determination of group of secondary nodes and surface of main elements <---
      DO J=1,NGRNOD	  
	IF (IGRNOD(J)%ID.EQ.GRS) G1 = J
      END DO
      DO J=1,NSURF	  
	IF (IGRSURF(J)%ID.EQ.GRM) G2 = J
      END DO
      
C-->  error - error message will be printed in the reading of the interfaces <--- 	      
      IF ((G1.EQ.0).OR.(G2.EQ.0)) GOTO 149
      	         
      IF (IGRNOD(G1)%R2R_ALL.EQ.0) TAG2 = 0
      IF ((ISURF_R2R(1,G2)+ISURF_R2R(2,G2)).EQ.0) TAG2 = 0

      IF (IDDOM.NE.0) THEN
C-->  treatment of the main side   <---	     
	VAL = 1
	W = 1
        N_NP = IGRNOD(G1)%NENTITY-IGRNOD(G1)%R2R_SHARE+IGRPP_R2R(1,G1)
	N_NT = IGRNOD(G1)%R2R_ALL-IGRPP_R2R(2,G1)
	N_NS = IGRNOD(G1)%R2R_SHARE-IGRPP_R2R(1,G1)
        N_SURFP = ISURF_R2R(2,G2)+ISURF_R2R(4,G2) 
        N_SURFT = ISURF_R2R(1,G2)-ISURF_R2R(5,G2)  		    		   	   
      ELSE
C-->  treatment of the second. side   <---	     
	VAL = 0
	W=0
	N_NP = IGRNOD(G1)%NENTITY-IGRNOD(G1)%R2R_ALL+IGRPP_R2R(2,G1)
	N_NT = IGRNOD(G1)%R2R_SHARE-IGRPP_R2R(1,G1)
	N_NS = IGRNOD(G1)%R2R_ALL-IGRPP_R2R(2,G1)
        N_SURFP = IGRSURF(G2)%NSEG-ISURF_R2R(1,G2)+ISURF_R2R(5,G2)
        N_SURFT = IGRSURF(G2)%NSEG-ISURF_R2R(2,G2)-ISURF_R2R(4,G2)
      ENDIF
            
C-->  determination of the tag   <--- 
      IF (TYPE2.EQ.1) N_NP = N_NT	       	      	   	   
      IF ((N_NP.EQ.0).AND.(N_SURFP.GT.0)) TAG=2
      IF ((N_NP.NE.0).AND.(N_SURFP.EQ.0)) TAG=3
      IF ((N_NS.EQ.IGRNOD(G1)%NENTITY).AND.(N_SURFP.EQ.0)) TAG=-W
      IF (FLG_SWALE.EQ.0) THEN
        IF ((N_NT.EQ.IGRNOD(G1)%NENTITY).AND.(N_SURFT.EQ.IGRSURF(G2)%NSEG)) TAG=W-1
      ENDIF
      
C-->  Interfaces TYPE2  <---      
      IF (TYPE2.EQ.1) THEN      
        IF (((N_NT.GT.0).AND.(N_NS.GT.0)).OR.(TAG.GT.0)) THEN
           TAG = 4	        
           VAL = -100
	ENDIF  
      ENDIF
      
C-->  detection of splitted interfaces for printout of warnings <--- 
      IF ((TAG.GT.0).AND.(TAG2.GT.0).AND.(TAG.LT.4)) THEN
        W = ISURF_R2R(1,G2)+ISURF_R2R(2,G2)-ISURF_R2R(5,G2)
        IF (((IGRNOD(G1)%R2R_ALL-IGRPP_R2R(2,G1)).NE.IGRNOD(G1)%NENTITY)
     .                                           .OR.(W.NE.IGRSURF(G2)%NSEG)) THEN
           TAGINT_WARN(1)=TAGINT_WARN(1)+1
	   TAGINT_WARN(1+TAGINT_WARN(1)) = ID_INTER
	ENDIF 
      ENDIF
      
C--------------------------------------------------------------------C
C------------FLAG = 1 --> surface / surface interfaces --------------C
C--------------------------------------------------------------------C
      
      ELSEIF (FLAG.EQ.1) THEN
	      	    	    
      DO J=1,NSURF
	IF (IGRSURF(J)%ID.EQ.GRS) G1 = J
      END DO
      DO J=1,NSURF
	IF (IGRSURF(J)%ID.EQ.GRM) G2 = J
      END DO
      
C-->  error - error message will be printed in the reading of the interfaces <---  	      
      IF ((G1.EQ.0).OR.(G2.EQ.0)) GOTO 149
      
      I1 = IGRSURF(G1)%NSEG
      I2 = IGRSURF(G2)%NSEG
      TAG = 1
      TAG2 = 1
      IF ((ISURF_R2R(1,G1)).EQ.0) TAG2=0
      IF ((ISURF_R2R(1,G2)).EQ.0) TAG2=0
      
      IF (IDDOM.NE.0) THEN      
C-->  treatment of the main side   <---
        W = 1 
        VAL = 1                
        N_SURFP=ISURF_R2R(2,G1)+ISURF_R2R(4,G1) 
        N_SURFP2=ISURF_R2R(2,G2)+ISURF_R2R(4,G2)        
        N_SURFT=ISURF_R2R(1,G1)-ISURF_R2R(5,G1) 
        N_SURFT2=ISURF_R2R(1,G2)-ISURF_R2R(5,G2)            
      ELSE
C-->  treatment of the second. side   <---
        W = 0
        VAL = 0                 
        N_SURFP=IGRSURF(G1)%NSEG-ISURF_R2R(1,G1)+ISURF_R2R(5,G1)
        N_SURFP2=IGRSURF(G2)%NSEG-ISURF_R2R(1,G2)+ISURF_R2R(5,G2)
        N_SURFT=IGRSURF(G1)%NSEG-ISURF_R2R(2,G1)-ISURF_R2R(4,G1)
        N_SURFT2=IGRSURF(G2)%NSEG-ISURF_R2R(2,G2)-ISURF_R2R(4,G2)
      ENDIF
      
C-->  determination of the tag   <---	       	      	   	   
      IF ((N_SURFP.EQ.0).AND.(N_SURFP2.GT.0)) TAG=2
      IF ((N_SURFP.GT.0).AND.(N_SURFP2.EQ.0)) TAG=3
      IF ((N_SURFP.EQ.0).AND.(N_SURFP2.EQ.0)) TAG=-W
      IF (FLG_SWALE.EQ.0) THEN
        IF ((N_SURFT.EQ.I1).AND.(N_SURFT2.EQ.I2)) TAG=W-1
      ENDIF
      
C-->  detection of splitted interfaces for printout of warnings <--- 
      IF ((TAG.GT.0).AND.(TAG2.GT.0)) THEN
        IF ((N_SURFT.NE.I1).OR.(N_SURFT2.NE.I2)) THEN
           TAGINT_WARN(1)=TAGINT_WARN(1)+1
	   TAGINT_WARN(1+TAGINT_WARN(1)) = ID_INTER
	ENDIF 
      ENDIF
                 
C--------------------------------------------------------------------C
C------------FLAG = 2 --> line / line interfaces --------------------C
C--------------------------------------------------------------------C
      
      ELSEIF (FLAG.EQ.2) THEN
	      	      	    
      DO J=1,NSLIN	  
	IF (IGRSLIN(J)%ID.EQ.GRS) G1 = J
      END DO
      DO J=1,NSLIN	  
	IF (IGRSLIN(J)%ID.EQ.GRM) G2 = J
      END DO
      
C-->  error - error message will be printed in the reading of the interfaces <--- 	      
      IF ((G1.EQ.0).OR.(G2.EQ.0)) GOTO 149
      
      I1 = IGRSLIN(G1)%NSEG
      I2 = IGRSLIN(G2)%NSEG
      TAG = 1
      TAG2 = 1
      IF ((ISLIN_R2R(1,G1)).EQ.0) TAG2=0
      IF ((ISLIN_R2R(1,G2)).EQ.0) TAG2=0

      IF (IDDOM.NE.0) THEN      
C-->  treatment of the main side   <---
        W = 1 
        VAL = 1                
        N_LINP=ISLIN_R2R(2,G1)
        N_LINP2=ISLIN_R2R(2,G2)       
        N_LINT=ISLIN_R2R(1,G1)
        N_LINT2=ISLIN_R2R(1,G2)            
      ELSE
C-->  treatment of the second. side   <---
        W = 0
        VAL = 0                 
        N_LINP=IGRSLIN(G1)%NSEG-ISLIN_R2R(1,G1)
        N_LINP2=IGRSLIN(G2)%NSEG-ISLIN_R2R(1,G2)
        N_LINT=IGRSLIN(G1)%NSEG-ISLIN_R2R(2,G1)
        N_LINT2=IGRSLIN(G2)%NSEG-ISLIN_R2R(2,G2)
      ENDIF
      
C-->  determination of the tag   <---	       	      	   	   
      IF ((N_LINP.EQ.0).AND.(N_LINP2.GT.0)) TAG=2
      IF ((N_LINP.GT.0).AND.(N_LINP2.EQ.0)) TAG=3
      IF ((N_LINP.EQ.0).AND.(N_LINP2.EQ.0)) TAG=-W
      IF (FLG_SWALE.EQ.0) THEN
        IF ((N_LINT.EQ.I1).AND.(N_LINT2.EQ.I2)) TAG=W-1
      ENDIF
      
C-->  detection of splitted interfaces for printout of warnings <---
      IF ((TAG.GT.0).AND.(TAG2.GT.0)) THEN
        IF ((N_LINT.NE.I1).OR.(N_LINT2.NE.I2)) THEN
           TAGINT_WARN(1)=TAGINT_WARN(1)+1
	   TAGINT_WARN(1+TAGINT_WARN(1)) = ID_INTER
	ENDIF 
      ENDIF

C--------------------------------------------------------------------C			
C------------FLAG = 3 --> TYPE18 contact with GR_BRIC ---------------C
C--------------------------------------------------------------------C

      ELSEIF (FLAG.EQ.3) THEN
	      	    	    
      DO J=1,NGRBRIC
	IF (IGRBRIC(J)%ID.EQ.GRS) G1 = J
      END DO
      DO J=1,NSURF
	IF (IGRSURF(J)%ID.EQ.GRM) G2 = J
      END DO
      
C-->  error - error message will be printed in the reading of the interfaces <-- 	      
      IF ((G1.EQ.0).OR.(G2.EQ.0)) GOTO 149
      
      I1 = IGRBRIC(G1)%NENTITY
      I2 = IGRSURF(G2)%NSEG
      TAG = 1
      TAG2 = 1
      IF ((IGRBRIC_R2R(1,G1)).EQ.0) TAG2=0
      IF ((ISURF_R2R(1,G2)).EQ.0) TAG2=0
      
      IF (IDDOM.NE.0) THEN      
C-->  treatment of the main side   <---
        W = 1 
        VAL = 1                
        N_GRBP=IGRBRIC_R2R(2,G1)+IGRBRIC_R2R(4,G1) 
        N_SURFP2=ISURF_R2R(2,G2)+ISURF_R2R(4,G2)        
        N_GRBT=IGRBRIC_R2R(1,G1)-IGRBRIC_R2R(5,G1) 
        N_SURFT2=ISURF_R2R(1,G2)-ISURF_R2R(5,G2)            
      ELSE
C-->  treatment of the second. side   <---
        W = 0
        VAL = 0                 
        N_GRBP=IGRBRIC(G1)%NENTITY-IGRBRIC_R2R(1,G1)+IGRBRIC_R2R(5,G1)
        N_SURFP2=IGRSURF(G2)%NSEG-ISURF_R2R(1,G2)+ISURF_R2R(5,G2)
        N_GRBT=IGRBRIC(G1)%NENTITY-IGRBRIC_R2R(2,G1)-IGRBRIC_R2R(4,G1)
        N_SURFT2=IGRSURF(G2)%NSEG-ISURF_R2R(2,G2)-ISURF_R2R(4,G2)
      ENDIF
      
C-->  determination of the tag   <---	       	      	   	   
      IF ((N_GRBP.EQ.0).AND.(N_SURFP2.GT.0)) TAG=2
      IF ((N_GRBP.GT.0).AND.(N_SURFP2.EQ.0)) TAG=3
      IF ((N_GRBP.EQ.0).AND.(N_SURFP2.EQ.0)) TAG=-W
      IF (FLG_SWALE.EQ.0) THEN
        IF ((N_GRBT.EQ.I1).AND.(N_SURFT2.EQ.I2)) TAG=W-1
      ENDIF
      
C-->  detection of splitted interfaces for printout of warnings <---
      IF ((TAG.GT.0).AND.(TAG2.GT.0)) THEN
        IF ((N_GRBT.NE.I1).OR.(N_SURFT2.NE.I2)) THEN
           TAGINT_WARN(1)=TAGINT_WARN(1)+1
	   TAGINT_WARN(1+TAGINT_WARN(1)) = ID_INTER
	ENDIF 
      ENDIF
                                    
      ENDIF
      
C--------------------------------------------------------------------C			
C--------TAG OF CONTACT INTERFACES IN TAGINT FOR SPLIT---------------C
C--------------------------------------------------------------------C
      
C-->  if one side is empty interface is not kept   <---
      IF ((TAG2.GT.0).AND.(TAG.GT.-1)) GOTO 149
      	
      GOTO 150
      
C-->  tag and couting for contact interfaces that will be kept  <---      
149   TAGINT(I) = ID_INTER
      COMPT = COMPT+1
      
150   CONTINUE
            	                       
C-----------
      RETURN
      END      
