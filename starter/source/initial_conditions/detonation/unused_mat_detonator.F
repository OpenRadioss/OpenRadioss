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
      FUNCTION UNUSED_MAT_DETONATOR(MDET,NUMMAT,LISTMAT)
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
        INTEGER               :: UNUSED_MAT_DETONATOR  
	INTEGER,intent(in)    :: LISTMAT(NPROPMI,*)     !material used in the model : {LISTMAT(1,I)},I=1..NUMMAT      
	INTEGER,intent(inout) :: MDET                   !detonator flag
	INTEGER,intent(in)    :: NUMMAT                 !Number of Material cards defined in user file  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
	INTEGER JJ, I, MID, MLN
C-----------------------------------------------
	  UNUSED_MAT_DETONATOR=0
	  JJ=0
	  IF (MDET /= 0) THEN
  	    DO I=1,NUMMAT
	      MID= LISTMAT(1,I)
              MLN= LISTMAT(2,I)
	      IF (MDET == MID) THEN !MDET refer to an used material       
                IF(MLN /= 5 .AND. MLN /= 51 .AND. MLN /= 97)THEN
                 UNUSED_MAT_DETONATOR=2
                 RETURN
                ENDIF   
		MDET=I !Internal ID                                       
		EXIT
	      ELSEIF(MID == 0)THEN  !end of list reached
		JJ=JJ+1
		EXIT
	      ELSE                  !MDET does not correspond to this current material
	        JJ=JJ+1
	      END IF
	    END DO
	    !!JJ=I => All material were compared to Mdet and do not correspond to it.
	    IF (JJ==I) THEN
	      UNUSED_MAT_DETONATOR=1
	    END IF	      	  
	  END IF
      END FUNCTION
C-----------------------------------------------
