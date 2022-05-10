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
Chd|  SHSUB                         source/elements/shell/subcycling/shsub.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SHSUB(LSHSUB  ,IXS     ,IXS10   ,IXS20   ,IXS16   ,
     .                 IXC     ,IXT     ,IXP     ,IXR     ,IXTG    )
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
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LSHSUB(*),IXS(NIXS,*),IXS10(6,*),IXS16(8,*),IXS20(12,*),
     .   IXC(NIXC,*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IXTG(NIXTG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C=======================================================================
C
C---  Solids
C
      DO I = 1, NUMELS                                               
        DO J=2,9                                    
          IF (LSHSUB(IXS(J,I)) < 2)  
     .        LSHSUB(IXS(J,I))=LSHSUB(IXS(J,I))+2
        ENDDO                                                  
      ENDDO                                                   
C
      DO I = 1, NUMELS10                                               
        DO J=1,6                                                      
          IF(IXS10(J,I).NE.0)THEN                                   
            IF (LSHSUB(IXS10(J,I) ) < 2)                             
     .          LSHSUB(IXS10(J,I)) = LSHSUB(IXS10(J,I))+2                
          ENDIF                                                       
        ENDDO                                                         
      ENDDO                                                   
C
      DO I = 1, NUMELS20                                               
        DO J=1,12                      
          IF(IXS20(J,I).NE.0)THEN                                   
            IF (LSHSUB(IXS20(J,I)) < 2)                            
     .          LSHSUB(IXS20(J,I)) = LSHSUB(IXS20(J,I))+2               
          ENDIF                                                       
        ENDDO                                                         
      ENDDO                                                   
C
      DO I = 1, NUMELS16                                               
        DO J=1,8                      
          IF(IXS16(J,I).NE.0)THEN                                   
            IF (LSHSUB(IXS16(J,I)) < 2)                            
     .          LSHSUB(IXS16(J,I)) = LSHSUB(IXS16(J,I))+2               
          ENDIF                                                       
        ENDDO                                                         
      ENDDO                                                   
C
C---  Shells
C
      DO I = 1, NUMELC                                               
        DO J=2,5                                               
          IF (LSHSUB(IXC(J,I)) == 0) LSHSUB(IXC(J,I)) = 1
        ENDDO                                                  
      ENDDO                                                   
C
C---  Truss
C
      DO I = 1, NUMELT                                               
        DO J=2,3                                               
          IF (LSHSUB(IXP(J,I)) < 2)
     .        LSHSUB(IXP(J,I))= LSHSUB(IXP(J,I)) + 2
        ENDDO                                                  
      ENDDO                                                   
C
C---  Beams
C
      DO I = 1, NUMELP                                               
        DO J=2,4                                              
            IF (LSHSUB(IXP(J,I)) < 2)
     .          LSHSUB(IXP(J,I)) = LSHSUB(IXP(J,I)) + 2
        ENDDO                                                  
      ENDDO                                                   
C
C---  Springs
C
      DO I = 1, NUMELR                                               
        DO J=2,4
          IF (IXR(J,I) /= 0) THEN                                             
            IF (LSHSUB(IXR(J,I)) < 2)
     .          LSHSUB(IXR(J,I)) = LSHSUB(IXR(J,I)) + 2
          END IF
        ENDDO                                                  
      ENDDO                                                   
C
C---  Triangle Shells
C
      DO I = 1, NUMELTG                                               
        DO J=2,4                                               
          IF (LSHSUB(IXTG(J,I)) == 0) LSHSUB(IXTG(J,I)) = 1
        ENDDO                                                  
      ENDDO                                                   
C
C-----------
C
      DO I=1,NUMNOD
        IF (LSHSUB(I) == 3)THEN
          NSHFRONT=NSHFRONT+1
          LSHSUB(NSHFRONT)=I
        ENDIF
      ENDDO
C-----------
      RETURN
      END
