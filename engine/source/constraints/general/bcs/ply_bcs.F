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
Chd|  PLY_BCS                       source/constraints/general/bcs/ply_bcs.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PLY_BCS1V                     source/constraints/general/bcs/ply_bcs.F
Chd|====================================================================
      SUBROUTINE PLY_BCS(NODFT,NODLT  ,ICODT ,ISKEW,
     2                 SKEW , INOD,MS_PLY,IBC_PLY)

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,ICODT(*),ISKEW(*),INOD(*),IBC_PLY(*)
      my_real
     .    SKEW(LSKEW,*),MS_PLY(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, K, ISK, LCOD, NINDX,
     .        INDX(1024),L,J
      my_real
     .   XDUM
C-----------------------------------------------
      DO  I = NODFT,NODLT,1024
        NINDX = 0
        DO  N=I,MIN(NODLT,I+1023)
          IF (ICODT(N).NE.0)THEN
            NINDX = NINDX + 1
            INDX(NINDX) = N
           ENDIF
        ENDDO
C     
        CALL PLY_BCS1V(NINDX,INDX,INOD,ISKEW,ICODT,
     .             SKEW   ,MS_PLY,IBC_PLY )
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  PLY_BCS1V                     source/constraints/general/bcs/ply_bcs.F
Chd|-- called by -----------
Chd|        PLY_BCS                       source/constraints/general/bcs/ply_bcs.F
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE PLY_BCS1V(NINDX,INDX,INOD,ISKEW,ICODT,B ,
     .                     MS_PLY,IBC_PLY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
         USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDX, INDX(*), ISKEW(*), ICODT(*),INOD(*),
     .       IBC_PLY(*)
C     REAL
      my_real
     .  B(LSKEW,*), MS_PLY(NPLYXFE,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, K, L, ISK, LCOD,J,LG,IBC,ISUP,IINF
C     REAL
      my_real
     .   AA, VV,A1,A2,A3,V1,V2,V3
C-----------------------------------------------
#include "vectorize.inc"
      
      DO K = 1, NINDX
        LG = INDX(K)
        ISK =ISKEW(LG)
        LCOD=ICODT(LG)
        L = INOD(LG)
        IBC = IBC_PLY(LG)
        IF(L > 0 .AND. IBC == 0) THEN                
         IF(ISK == 0) THEN
C------------------
C     REPERE GLOBAL
C------------------
           IF(LCOD == 1)THEN
              DO J=1,NPLYMAX
                PLY(J)%V(3,L)=ZERO
                PLY(J)%A(3,L)=ZERO
                PLY(J)%U(3,L)=ZERO
              ENDDO   
           ELSEIF(LCOD == 2)THEN
             DO J=1,NPLYMAX
               PLY(J)%V(2,L)=ZERO
               PLY(J)%A(2,L)=ZERO
               PLY(J)%U(2,L)=ZERO
             ENDDO
           ELSEIF(LCOD == 3)THEN
             DO J=1,NPLYMAX
               PLY(J)%V(2,L)=ZERO
               PLY(J)%V(3,L)=ZERO      
               PLY(J)%A(2,L)=ZERO
               PLY(J)%A(3,L)=ZERO
               PLY(J)%U(2,L)=ZERO
               PLY(J)%U(3,L)=ZERO       
            ENDDO   
          ELSEIF(LCOD == 4)THEN
            DO J=1,NPLYMAX
                PLY(J)%V(1,L)=ZERO
                PLY(J)%A(1,L)=ZERO
                PLY(J)%U(1,L)=ZERO
             ENDDO
           ELSEIF(LCOD == 5)THEN
             DO J=1,NPLYMAX
               PLY(J)%V(1,L)=ZERO
               PLY(J)%V(3,L)=ZERO
               PLY(J)%A(1,L)=ZERO
               PLY(J)%A(3,L)=ZERO  
               PLY(J)%U(1,L)=ZERO
               PLY(J)%U(3,L)=ZERO    
              ENDDO  
           ELSEIF(LCOD == 6)THEN
              DO J=1,NPLYMAX
                PLY(J)%V(1,L)=ZERO
                PLY(J)%V(2,L)=ZERO
                PLY(J)%A(1,L)=ZERO
                PLY(J)%A(2,L)=ZERO
                PLY(J)%U(1,L)=ZERO
                PLY(J)%U(2,L)=ZERO      
              ENDDO   
           ELSEIF(LCOD == 7)THEN
             DO J=1,NPLYMAX
               PLY(J)%V(1,L)=ZERO
               PLY(J)%V(2,L)=ZERO
               PLY(J)%V(3,L)=ZERO
               
               PLY(J)%A(1,L)=ZERO
               PLY(J)%A(2,L)=ZERO
               PLY(J)%A(3,L)=ZERO
               
               PLY(J)%U(1,L)=ZERO
               PLY(J)%U(2,L)=ZERO
               PLY(J)%U(3,L)=ZERO     
             ENDDO
           ENDIF
C
         ELSE ! iskw
C-------------------
C     REPERE OBLIQUE
C-------------------
C 
           IF(LCOD == 1)THEN
              DO J=1,NPLYMAX 
               A1 = PLY(J)%A(1,L)
               A2 = PLY(J)%A(2,L)
               A3 = PLY(J)%A(3,L)
               V1 = PLY(J)%V(1,L)
               V2 = PLY(J)%V(2,L)
               V3 = PLY(J)%V(3,L)
               AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
               VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
               PLY(J)%A(1,L) = PLY(J)%A(1,L) - B(7,ISK)*AA
               PLY(J)%A(2,L) = PLY(J)%A(2,L) - B(8,ISK)*AA
               PLY(J)%A(3,L) = PLY(J)%A(3,L) - B(9,ISK)*AA
               PLY(J)%V(1,L) = PLY(J)%V(1,L) - B(7,ISK)*VV
               PLY(J)%V(2,L) = PLY(J)%V(2,L) - B(8,ISK)*VV
               PLY(J)%V(3,L) = PLY(J)%V(3,L) - B(9,ISK)*VV
             ENDDO  
           ELSEIF(LCOD == 2)THEN
             DO J=1,NPLYMAX 
               A1 = PLY(J)%A(1,L)
               A2 = PLY(J)%A(2,L)
               A3 = PLY(J)%A(3,L)
               V1 = PLY(J)%V(1,L)
               V2 = PLY(J)%V(2,L)
               V3 = PLY(J)%V(3,L)
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               PLY(J)%A(1,L)=PLY(J)%A(1,L) - B(4,ISK)*AA
               PLY(J)%A(2,L)=PLY(J)%A(2,L) - B(5,ISK)*AA
               PLY(J)%A(3,L)=PLY(J)%A(3,L) - B(6,ISK)*AA
               PLY(J)%V(1,L)=PLY(J)%V(1,L) - B(4,ISK)*VV
               PLY(J)%V(2,L)=PLY(J)%V(2,L) - B(5,ISK)*VV
               PLY(J)%V(3,L)=PLY(J)%V(3,L) - B(6,ISK)*VV
             ENDDO  
           ELSEIF(LCOD == 3)THEN
             DO J=1,NPLYMAX
               A1 = PLY(J)%A(1,L)
               A2 = PLY(J)%A(2,L)
               A3 = PLY(J)%A(3,L)
               V1 = PLY(J)%V(1,L)
               V2 = PLY(J)%V(2,L)
               V3 = PLY(J)%V(3,L)
               AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
               VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
               
               PLY(J)%A(1,L)=PLY(J)%A(1,L) - B(7,ISK)*AA
               PLY(J)%A(2,L)=PLY(J)%A(2,L) - B(8,ISK)*AA
               PLY(J)%A(3,L)=PLY(J)%A(3,L) - B(9,ISK)*AA
               PLY(J)%V(1,L)=PLY(J)%V(1,L) - B(7,ISK)*VV
               PLY(J)%V(2,L)=PLY(J)%V(2,L) - B(8,ISK)*VV
               PLY(J)%V(3,L)=PLY(J)%V(3,L) - B(9,ISK)*VV
               
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               
               PLY(J)%A(1,L)=PLY(J)%A(1,L) - B(4,ISK)*AA
               PLY(J)%A(2,L)=PLY(J)%A(2,L) - B(5,ISK)*AA
               PLY(J)%A(3,L)=PLY(J)%A(3,L) - B(6,ISK)*AA
               PLY(J)%V(1,L)=PLY(J)%V(1,L) - B(4,ISK)*VV
               PLY(J)%V(2,L)=PLY(J)%V(2,L) - B(5,ISK)*VV
               PLY(J)%V(3,L)=PLY(J)%V(3,L) - B(6,ISK)*VV
             ENDDO
           ELSEIF(LCOD == 4)THEN
             DO  J=1,NPLYMAX
               A1 = PLY(J)%A(1,L)
               A2 = PLY(J)%A(2,L)
               A3 = PLY(J)%A(3,L)
               V1 = PLY(J)%V(1,L)
               V2 = PLY(J)%V(2,L)
               V3 = PLY(J)%V(3,L)
               
               AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
               VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
               PLY(J)%A(1,L)=PLY(J)%A(1,L) - B(1,ISK)*AA
               PLY(J)%A(2,L)=PLY(J)%A(2,L) - B(2,ISK)*AA
               PLY(J)%A(3,L)=PLY(J)%A(3,L) - B(3,ISK)*AA
               PLY(J)%V(1,L)=PLY(J)%V(1,L) - B(1,ISK)*VV
               PLY(J)%V(2,L)=PLY(J)%V(2,L) - B(2,ISK)*VV
               PLY(J)%V(3,L)=PLY(J)%V(3,L) - B(3,ISK)*VV
             ENDDO  
           ELSEIF(LCOD == 5)THEN
             DO  J=1,NPLYMAX
              A1 = PLY(J)%A(1,L)
              A2 = PLY(J)%A(2,L)
              A3 = PLY(J)%A(3,L)
              V1 = PLY(J)%V(1,L)
              V2 = PLY(J)%V(2,L)
              V3 = PLY(J)%V(3,L)
              AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
              VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
              PLY(J)%A(1,L)=PLY(J)%A(1,L) -  B(7,ISK)*AA
              PLY(J)%A(2,L)=PLY(J)%A(2,L) -  B(8,ISK)*AA
              PLY(J)%A(3,L)=PLY(J)%A(3,L) -  B(9,ISK)*AA
              PLY(J)%V(1,L)=PLY(J)%V(1,L) -  B(7,ISK)*VV
              PLY(J)%V(2,L)=PLY(J)%V(2,L) -  B(8,ISK)*VV
              PLY(J)%V(3,L)=PLY(J)%V(3,L) -  B(9,ISK)*VV
              AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
              VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
              PLY(J)%A(1,L)=PLY(J)%A(1,L) -  B(1,ISK)*AA
              PLY(J)%A(2,L)=PLY(J)%A(2,L) -  B(2,ISK)*AA
              PLY(J)%A(3,L)=PLY(J)%A(3,L) -  B(3,ISK)*AA
              PLY(J)%V(1,L)=PLY(J)%V(1,L) -  B(1,ISK)*VV
              PLY(J)%V(2,L)=PLY(J)%V(2,L) -  B(2,ISK)*VV
              PLY(J)%V(3,L)=PLY(J)%V(3,L) -  B(3,ISK)*VV
             ENDDO   
            ELSEIF(LCOD == 6)THEN
              DO  J=1,NPLYMAX
               A1 = PLY(J)%A(1,L)
               A2 = PLY(J)%A(2,L)
               A3 = PLY(J)%A(3,L)
               V1 = PLY(J)%V(1,L)
               V2 = PLY(J)%V(2,L)
               V3 = PLY(J)%V(3,L)
               AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
               VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
               PLY(J)%A(1,L)=PLY(J)%A(1,L)  -  B(1,ISK)*AA
               PLY(J)%A(2,L)=PLY(J)%A(2,L)  -  B(2,ISK)*AA
               PLY(J)%A(3,L)=PLY(J)%A(3,L)  -  B(3,ISK)*AA
               PLY(J)%V(1,L)=PLY(J)%V(1,L)  -  B(1,ISK)*VV
               PLY(J)%V(2,L)=PLY(J)%V(2,L)  -  B(2,ISK)*VV
               PLY(J)%V(3,L)=PLY(J)%V(3,L)  -  B(3,ISK)*VV
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               PLY(J)%A(1,L)=PLY(J)%A(1,L)-B(4,ISK)*AA
               PLY(J)%A(2,L)=PLY(J)%A(2,L)-B(5,ISK)*AA
               PLY(J)%A(3,L)=PLY(J)%A(3,L)-B(6,ISK)*AA
               PLY(J)%V(1,L)=PLY(J)%V(1,L)-B(4,ISK)*VV
               PLY(J)%V(2,L)=PLY(J)%V(2,L)-B(5,ISK)*VV
               PLY(J)%V(3,L)=PLY(J)%V(3,L)-B(6,ISK)*VV
            ENDDO  
          ELSEIF(LCOD == 7)THEN
           DO J=1,NPLYMAX
              PLY(J)%A(1,L)=ZERO
              PLY(J)%A(2,L)=ZERO
              PLY(J)%A(3,L)=ZERO
              PLY(J)%V(1,L)=ZERO
              PLY(J)%V(2,L)=ZERO
              PLY(J)%V(3,L)=ZERO  
             ENDDO  
           ENDIF
C
         END IF ! iskew
C
C --- sup or inf  ply or the both
C
       ELSEIF( L > 0 ) THEN ! ibc > 0
           J = 1
           DO WHILE (MS_PLY(L,J) == ZERO)
             J = J +1
           ENDDO 
           IINF = J 
           J = NPLYMAX
           DO WHILE (MS_PLY(L,J) == ZERO) 
             J = J - 1
           ENDDO 
           ISUP = J
          IF(ISK == 0) THEN
C------------------
C     REPERE GLOBAL
C------------------          
C          
             IF(IBC ==2 ) THEN 
               IF(LCOD == 1)THEN
                    PLY(ISUP)%V(3,L)=ZERO
                    PLY(ISUP)%A(3,L)=ZERO
                    PLY(ISUP)%U(3,L)=ZERO
                    PLY(IINF)%V(3,L)=ZERO
                    PLY(IINF)%A(3,L)=ZERO
                    PLY(IINF)%U(3,L)=ZERO
               ELSEIF(LCOD == 2)THEN

                   PLY(IINF)%V(2,L)=ZERO
                   PLY(IINF)%A(2,L)=ZERO
                   PLY(IINF)%U(2,L)=ZERO
C                  
                   PLY(ISUP)%V(2,L)=ZERO
                   PLY(ISUP)%A(2,L)=ZERO
                   PLY(ISUP)%U(2,L)=ZERO                  
               ELSEIF(LCOD == 3)THEN
C               
                   PLY(IINF)%V(2,L)=ZERO
                   PLY(IINF)%V(3,L)=ZERO      
                   PLY(IINF)%A(2,L)=ZERO
                   PLY(IINF)%A(3,L)=ZERO
                   PLY(IINF)%U(2,L)=ZERO
                   PLY(IINF)%U(3,L)=ZERO 
C               
                   PLY(ISUP)%V(2,L)=ZERO
                   PLY(ISUP)%V(3,L)=ZERO      
                   PLY(ISUP)%A(2,L)=ZERO
                   PLY(ISUP)%A(3,L)=ZERO
                   PLY(ISUP)%U(2,L)=ZERO
                   PLY(ISUP)%U(3,L)=ZERO        
               ELSEIF(LCOD == 4)THEN
                    PLY(ISUP)%V(1,L)=ZERO
                    PLY(ISUP)%A(1,L)=ZERO
                    PLY(ISUP)%U(1,L)=ZERO
C                    
                    PLY(IINF)%V(1,L)=ZERO
                    PLY(IINF)%A(1,L)=ZERO
                    PLY(IINF)%U(1,L)=ZERO
               ELSEIF(LCOD == 5)THEN
                   PLY(IINF)%V(1,L)=ZERO
                   PLY(IINF)%V(3,L)=ZERO
                   PLY(IINF)%A(1,L)=ZERO
                   PLY(IINF)%A(3,L)=ZERO  
                   PLY(IINF)%U(1,L)=ZERO
                   PLY(IINF)%U(3,L)=ZERO 
C 
                   PLY(ISUP)%V(1,L)=ZERO
                   PLY(ISUP)%V(3,L)=ZERO
                   PLY(ISUP)%A(1,L)=ZERO
                   PLY(ISUP)%A(3,L)=ZERO  
                   PLY(ISUP)%U(1,L)=ZERO
                   PLY(ISUP)%U(3,L)=ZERO 
               ELSEIF(LCOD == 6)THEN
                    PLY(ISUP)%V(1,L)=ZERO
                    PLY(ISUP)%V(2,L)=ZERO
                    PLY(ISUP)%A(1,L)=ZERO
                    PLY(ISUP)%A(2,L)=ZERO
                    PLY(ISUP)%U(1,L)=ZERO
                    PLY(ISUP)%U(2,L)=ZERO
C                     
                    PLY(IINF)%V(1,L)=ZERO
                    PLY(IINF)%V(2,L)=ZERO
                    PLY(IINF)%A(1,L)=ZERO
                    PLY(IINF)%A(2,L)=ZERO
                    PLY(IINF)%U(1,L)=ZERO
                    PLY(IINF)%U(2,L)=ZERO 
               ELSEIF(LCOD == 7)THEN

                   PLY(ISUP)%V(1,L)=ZERO
                   PLY(ISUP)%V(2,L)=ZERO
                   PLY(ISUP)%V(3,L)=ZERO
                   PLY(ISUP)%A(1,L)=ZERO
                   PLY(ISUP)%A(2,L)=ZERO
                   PLY(ISUP)%A(3,L)=ZERO
                   PLY(ISUP)%U(1,L)=ZERO
                   PLY(ISUP)%U(2,L)=ZERO
                   PLY(ISUP)%U(3,L)=ZERO
C                      
                   PLY(IINF)%V(1,L)=ZERO
                   PLY(IINF)%V(2,L)=ZERO
                   PLY(IINF)%V(3,L)=ZERO
                   PLY(IINF)%A(1,L)=ZERO
                   PLY(IINF)%A(2,L)=ZERO
                   PLY(IINF)%A(3,L)=ZERO
                   PLY(IINF)%U(1,L)=ZERO
                   PLY(IINF)%U(2,L)=ZERO
                   PLY(IINF)%U(3,L)=ZERO    

               ENDIF
C sup       
            ELSEIF(IBC == 1) THEN ! ibc
               IF(LCOD == 1)THEN
                    PLY(ISUP)%V(3,L)=ZERO
                    PLY(ISUP)%A(3,L)=ZERO
                    PLY(ISUP)%U(3,L)=ZERO
               ELSEIF(LCOD == 2)THEN
                   PLY(ISUP)%V(2,L)=ZERO
                   PLY(ISUP)%A(2,L)=ZERO
                   PLY(ISUP)%U(2,L)=ZERO                  
               ELSEIF(LCOD == 3)THEN 
                   PLY(ISUP)%V(2,L)=ZERO
                   PLY(ISUP)%V(3,L)=ZERO      
                   PLY(ISUP)%A(2,L)=ZERO
                   PLY(ISUP)%A(3,L)=ZERO
                   PLY(ISUP)%U(2,L)=ZERO
                   PLY(ISUP)%U(3,L)=ZERO        
               ELSEIF(LCOD == 4)THEN
                    PLY(ISUP)%V(1,L)=ZERO
                    PLY(ISUP)%A(1,L)=ZERO
                    PLY(ISUP)%U(1,L)=ZERO
               ELSEIF(LCOD == 5)THEN
                   PLY(ISUP)%V(1,L)=ZERO
                   PLY(ISUP)%V(3,L)=ZERO
                   PLY(ISUP)%A(1,L)=ZERO
                   PLY(ISUP)%A(3,L)=ZERO  
                   PLY(ISUP)%U(1,L)=ZERO
                   PLY(ISUP)%U(3,L)=ZERO 
               ELSEIF(LCOD == 6)THEN
                    PLY(ISUP)%V(1,L)=ZERO
                    PLY(ISUP)%V(2,L)=ZERO
                    PLY(ISUP)%A(1,L)=ZERO
                    PLY(ISUP)%A(2,L)=ZERO
                    PLY(ISUP)%U(1,L)=ZERO
                    PLY(ISUP)%U(2,L)=ZERO
               ELSEIF(LCOD == 7)THEN
                   PLY(ISUP)%V(1,L)=ZERO
                   PLY(ISUP)%V(2,L)=ZERO
                   PLY(ISUP)%V(3,L)=ZERO
                   PLY(ISUP)%A(1,L)=ZERO
                   PLY(ISUP)%A(2,L)=ZERO
                   PLY(ISUP)%A(3,L)=ZERO
                   PLY(ISUP)%U(1,L)=ZERO
                   PLY(ISUP)%U(2,L)=ZERO
                   PLY(ISUP)%U(3,L)=ZERO 
               ENDIF
C inf           
            ELSE ! ibc
               IF(LCOD == 1)THEN
                    PLY(IINF)%V(3,L)=ZERO
                    PLY(IINF)%A(3,L)=ZERO
                    PLY(IINF)%U(3,L)=ZERO
               ELSEIF(LCOD == 2)THEN

                   PLY(IINF)%V(2,L)=ZERO
                   PLY(IINF)%A(2,L)=ZERO
                   PLY(IINF)%U(2,L)=ZERO
               ELSEIF(LCOD == 3)THEN
C               
                   PLY(IINF)%V(2,L)=ZERO
                   PLY(IINF)%V(3,L)=ZERO      
                   PLY(IINF)%A(2,L)=ZERO
                   PLY(IINF)%A(3,L)=ZERO
                   PLY(IINF)%U(2,L)=ZERO
                   PLY(IINF)%U(3,L)=ZERO 
               ELSEIF(LCOD == 4)THEN
C                    
                    PLY(IINF)%V(1,L)=ZERO
                    PLY(IINF)%A(1,L)=ZERO
                    PLY(IINF)%U(1,L)=ZERO
               ELSEIF(LCOD == 5)THEN
                   PLY(IINF)%V(1,L)=ZERO
                   PLY(IINF)%V(3,L)=ZERO
                   PLY(IINF)%A(1,L)=ZERO
                   PLY(IINF)%A(3,L)=ZERO  
                   PLY(IINF)%U(1,L)=ZERO
                   PLY(IINF)%U(3,L)=ZERO
               ELSEIF(LCOD == 6)THEN
                    PLY(IINF)%V(1,L)=ZERO
                    PLY(IINF)%V(2,L)=ZERO
                    PLY(IINF)%A(1,L)=ZERO
                    PLY(IINF)%A(2,L)=ZERO
                    PLY(IINF)%U(1,L)=ZERO
                    PLY(IINF)%U(2,L)=ZERO 
               ELSEIF(LCOD == 7)THEN
                   PLY(IINF)%V(1,L)=ZERO
                   PLY(IINF)%V(2,L)=ZERO
                   PLY(IINF)%V(3,L)=ZERO
                   PLY(IINF)%A(1,L)=ZERO
                   PLY(IINF)%A(2,L)=ZERO
                   PLY(IINF)%A(3,L)=ZERO
                   PLY(IINF)%U(1,L)=ZERO
                   PLY(IINF)%U(2,L)=ZERO
                   PLY(IINF)%U(3,L)=ZERO 
               ENDIF 
            ENDIF    ! ibc 
C 
         ELSE  ! iskew
C-------------------
C     REPERE OBLIQUE
C-------------------
C         
         IF(IBC == 2) THEN
            IF(LCOD == 1)THEN
 
               A1 = PLY(IINF)%A(1,L)
               A2 = PLY(IINF)%A(2,L)
               A3 = PLY(IINF)%A(3,L)
               V1 = PLY(IINF)%V(1,L)
               V2 = PLY(IINF)%V(2,L)
               V3 = PLY(IINF)%V(3,L)
               AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
               VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
               PLY(IINF)%A(1,L) = PLY(IINF)%A(1,L) - B(7,ISK)*AA
               PLY(IINF)%A(2,L) = PLY(IINF)%A(2,L) - B(8,ISK)*AA
               PLY(IINF)%A(3,L) = PLY(IINF)%A(3,L) - B(9,ISK)*AA
               PLY(IINF)%V(1,L) = PLY(IINF)%V(1,L) - B(7,ISK)*VV
               PLY(IINF)%V(2,L) = PLY(IINF)%V(2,L) - B(8,ISK)*VV
               PLY(IINF)%V(3,L) = PLY(IINF)%V(3,L) - B(9,ISK)*VV
C
               A1 = PLY(ISUP)%A(1,L)
               A2 = PLY(ISUP)%A(2,L)
               A3 = PLY(ISUP)%A(3,L)
               V1 = PLY(ISUP)%V(1,L)
               V2 = PLY(ISUP)%V(2,L)
               V3 = PLY(ISUP)%V(3,L)
               AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
               VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
               PLY(ISUP)%A(1,L) = PLY(ISUP)%A(1,L) - B(7,ISK)*AA
               PLY(ISUP)%A(2,L) = PLY(ISUP)%A(2,L) - B(8,ISK)*AA
               PLY(ISUP)%A(3,L) = PLY(ISUP)%A(3,L) - B(9,ISK)*AA
               PLY(ISUP)%V(1,L) = PLY(ISUP)%V(1,L) - B(7,ISK)*VV
               PLY(ISUP)%V(2,L) = PLY(ISUP)%V(2,L) - B(8,ISK)*VV
               PLY(ISUP)%V(3,L) = PLY(ISUP)%V(3,L) - B(9,ISK)*VV
           ELSEIF(LCOD == 2)THEN
               A1 = PLY(IINF)%A(1,L)
               A2 = PLY(IINF)%A(2,L)
               A3 = PLY(IINF)%A(3,L)
               V1 = PLY(IINF)%V(1,L)
               V2 = PLY(IINF)%V(2,L)
               V3 = PLY(IINF)%V(3,L)
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L) - B(4,ISK)*AA
               PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L) - B(5,ISK)*AA
               PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L) - B(6,ISK)*AA
               PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L) - B(4,ISK)*VV
               PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L) - B(5,ISK)*VV
               PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L) - B(6,ISK)*VV
C 
               A1 = PLY(ISUP)%A(1,L)
               A2 = PLY(ISUP)%A(2,L)
               A3 = PLY(ISUP)%A(3,L)
               V1 = PLY(ISUP)%V(1,L)
               V2 = PLY(ISUP)%V(2,L)
               V3 = PLY(ISUP)%V(3,L)
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L) - B(4,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L) - B(5,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L) - B(6,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L) - B(4,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L) - B(5,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L) - B(6,ISK)*VV              
           ELSEIF(LCOD == 3)THEN

               A1 = PLY(IINF)%A(1,L)
               A2 = PLY(IINF)%A(2,L)
               A3 = PLY(IINF)%A(3,L)
               V1 = PLY(IINF)%V(1,L)
               V2 = PLY(IINF)%V(2,L)
               V3 = PLY(IINF)%V(3,L)
               AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
               VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
               
               PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L) - B(7,ISK)*AA
               PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L) - B(8,ISK)*AA
               PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L) - B(9,ISK)*AA
               PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L) - B(7,ISK)*VV
               PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L) - B(8,ISK)*VV
               PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L) - B(9,ISK)*VV
               
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               
               PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L) - B(4,ISK)*AA
               PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L) - B(5,ISK)*AA
               PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L) - B(6,ISK)*AA
               PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L) - B(4,ISK)*VV
               PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L) - B(5,ISK)*VV
               PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L) - B(6,ISK)*VV
C
               A1 = PLY(ISUP)%A(1,L)
               A2 = PLY(ISUP)%A(2,L)
               A3 = PLY(ISUP)%A(3,L)
               V1 = PLY(ISUP)%V(1,L)
               V2 = PLY(ISUP)%V(2,L)
               V3 = PLY(ISUP)%V(3,L)
               AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
               VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
               
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L) - B(7,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L) - B(8,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L) - B(9,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L) - B(7,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L) - B(8,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L) - B(9,ISK)*VV
               
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L) - B(4,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L) - B(5,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L) - B(6,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L) - B(4,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L) - B(5,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L) - B(6,ISK)*VV
         
           ELSEIF(LCOD == 4)THEN
               A1 = PLY(IINF)%A(1,L)
               A2 = PLY(IINF)%A(2,L)
               A3 = PLY(IINF)%A(3,L)
               V1 = PLY(IINF)%V(1,L)
               V2 = PLY(IINF)%V(2,L)
               V3 = PLY(IINF)%V(3,L)
               
               AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
               VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
               PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L) - B(1,ISK)*AA
               PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L) - B(2,ISK)*AA
               PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L) - B(3,ISK)*AA
               PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L) - B(1,ISK)*VV
               PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L) - B(2,ISK)*VV
               PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L) - B(3,ISK)*VV
C
               A1 = PLY(ISUP)%A(1,L)
               A2 = PLY(ISUP)%A(2,L)
               A3 = PLY(ISUP)%A(3,L)
               V1 = PLY(ISUP)%V(1,L)
               V2 = PLY(ISUP)%V(2,L)
               V3 = PLY(ISUP)%V(3,L)
               
               AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
               VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L) - B(1,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L) - B(2,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L) - B(3,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L) - B(1,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L) - B(2,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L) - B(3,ISK)*VV               
           ELSEIF(LCOD == 5)THEN
              A1 = PLY(IINF)%A(1,L)
              A2 = PLY(IINF)%A(2,L)
              A3 = PLY(IINF)%A(3,L)
              V1 = PLY(IINF)%V(1,L)
              V2 = PLY(IINF)%V(2,L)
              V3 = PLY(IINF)%V(3,L)
              AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
              VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
              PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L) -  B(7,ISK)*AA
              PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L) -  B(8,ISK)*AA
              PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L) -  B(9,ISK)*AA
              PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L) -  B(7,ISK)*VV
              PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L) -  B(8,ISK)*VV
              PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L) -  B(9,ISK)*VV
              AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
              VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
              PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L) -  B(1,ISK)*AA
              PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L) -  B(2,ISK)*AA
              PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L) -  B(3,ISK)*AA
              PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L) -  B(1,ISK)*VV
              PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L) -  B(2,ISK)*VV
              PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L) -  B(3,ISK)*VV
C
              A1 = PLY(ISUP)%A(1,L)
              A2 = PLY(ISUP)%A(2,L)
              A3 = PLY(ISUP)%A(3,L)
              V1 = PLY(ISUP)%V(1,L)
              V2 = PLY(ISUP)%V(2,L)
              V3 = PLY(ISUP)%V(3,L)
              AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
              VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
              PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L) -  B(7,ISK)*AA
              PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L) -  B(8,ISK)*AA
              PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L) -  B(9,ISK)*AA
              PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L) -  B(7,ISK)*VV
              PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L) -  B(8,ISK)*VV
              PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L) -  B(9,ISK)*VV
              AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
              VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
              PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L) -  B(1,ISK)*AA
              PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L) -  B(2,ISK)*AA
              PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L) -  B(3,ISK)*AA
              PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L) -  B(1,ISK)*VV
              PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L) -  B(2,ISK)*VV
              PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L) -  B(3,ISK)*VV
                 
            ELSEIF(LCOD == 6)THEN

               A1 = PLY(IINF)%A(1,L)
               A2 = PLY(IINF)%A(2,L)
               A3 = PLY(IINF)%A(3,L)
               V1 = PLY(IINF)%V(1,L)
               V2 = PLY(IINF)%V(2,L)
               V3 = PLY(IINF)%V(3,L)
               AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
               VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
               PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L)  -  B(1,ISK)*AA
               PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L)  -  B(2,ISK)*AA
               PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L)  -  B(3,ISK)*AA
               PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L)  -  B(1,ISK)*VV
               PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L)  -  B(2,ISK)*VV
               PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L)  -  B(3,ISK)*VV
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L)-B(4,ISK)*AA
               PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L)-B(5,ISK)*AA
               PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L)-B(6,ISK)*AA
               PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L)-B(4,ISK)*VV
               PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L)-B(5,ISK)*VV
               PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L)-B(6,ISK)*VV
C             
               A1 = PLY(ISUP)%A(1,L)
               A2 = PLY(ISUP)%A(2,L)
               A3 = PLY(ISUP)%A(3,L)
               V1 = PLY(ISUP)%V(1,L)
               V2 = PLY(ISUP)%V(2,L)
               V3 = PLY(ISUP)%V(3,L)
               AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
               VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L)  -  B(1,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L)  -  B(2,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L)  -  B(3,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L)  -  B(1,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L)  -  B(2,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L)  -  B(3,ISK)*VV
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L)-B(4,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L)-B(5,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L)-B(6,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L)-B(4,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L)-B(5,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L)-B(6,ISK)*VV
          ELSEIF(LCOD == 7)THEN
 
              PLY(IINF)%A(1,L)=ZERO
              PLY(IINF)%A(2,L)=ZERO
              PLY(IINF)%A(3,L)=ZERO
              PLY(IINF)%V(1,L)=ZERO
              PLY(IINF)%V(2,L)=ZERO
              PLY(IINF)%V(3,L)=ZERO
C                
              PLY(ISUP)%A(1,L)=ZERO
              PLY(ISUP)%A(2,L)=ZERO
              PLY(ISUP)%A(3,L)=ZERO
              PLY(ISUP)%V(1,L)=ZERO
              PLY(ISUP)%V(2,L)=ZERO
              PLY(ISUP)%V(3,L)=ZERO  
           ENDIF ! lcod
C sup           
         ELSEIF(IBC == 1) THEn
         
             IF(LCOD == 1)THEN
C
               A1 = PLY(ISUP)%A(1,L)
               A2 = PLY(ISUP)%A(2,L)
               A3 = PLY(ISUP)%A(3,L)
               V1 = PLY(ISUP)%V(1,L)
               V2 = PLY(ISUP)%V(2,L)
               V3 = PLY(ISUP)%V(3,L)
               AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
               VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
               PLY(ISUP)%A(1,L) = PLY(ISUP)%A(1,L) - B(7,ISK)*AA
               PLY(ISUP)%A(2,L) = PLY(ISUP)%A(2,L) - B(8,ISK)*AA
               PLY(ISUP)%A(3,L) = PLY(ISUP)%A(3,L) - B(9,ISK)*AA
               PLY(ISUP)%V(1,L) = PLY(ISUP)%V(1,L) - B(7,ISK)*VV
               PLY(ISUP)%V(2,L) = PLY(ISUP)%V(2,L) - B(8,ISK)*VV
               PLY(ISUP)%V(3,L) = PLY(ISUP)%V(3,L) - B(9,ISK)*VV
           ELSEIF(LCOD == 2)THEN
C 
               A1 = PLY(ISUP)%A(1,L)
               A2 = PLY(ISUP)%A(2,L)
               A3 = PLY(ISUP)%A(3,L)
               V1 = PLY(ISUP)%V(1,L)
               V2 = PLY(ISUP)%V(2,L)
               V3 = PLY(ISUP)%V(3,L)
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L) - B(4,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L) - B(5,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L) - B(6,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L) - B(4,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L) - B(5,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L) - B(6,ISK)*VV              
           ELSEIF(LCOD == 3)THEN
               A1 = PLY(ISUP)%A(1,L)
               A2 = PLY(ISUP)%A(2,L)
               A3 = PLY(ISUP)%A(3,L)
               V1 = PLY(ISUP)%V(1,L)
               V2 = PLY(ISUP)%V(2,L)
               V3 = PLY(ISUP)%V(3,L)
               AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
               VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
               
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L) - B(7,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L) - B(8,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L) - B(9,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L) - B(7,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L) - B(8,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L) - B(9,ISK)*VV
               
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L) - B(4,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L) - B(5,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L) - B(6,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L) - B(4,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L) - B(5,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L) - B(6,ISK)*VV
         
           ELSEIF(LCOD == 4)THEN
               A1 = PLY(ISUP)%A(1,L)
               A2 = PLY(ISUP)%A(2,L)
               A3 = PLY(ISUP)%A(3,L)
               V1 = PLY(ISUP)%V(1,L)
               V2 = PLY(ISUP)%V(2,L)
               V3 = PLY(ISUP)%V(3,L)
               
               AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
               VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L) - B(1,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L) - B(2,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L) - B(3,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L) - B(1,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L) - B(2,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L) - B(3,ISK)*VV               
           ELSEIF(LCOD == 5)THEN
C
               A1 = PLY(ISUP)%A(1,L)
               A2 = PLY(ISUP)%A(2,L)
               A3 = PLY(ISUP)%A(3,L)
               V1 = PLY(ISUP)%V(1,L)
               V2 = PLY(ISUP)%V(2,L)
               V3 = PLY(ISUP)%V(3,L)
               
               AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
               VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L) - B(1,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L) - B(2,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L) - B(3,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L) - B(1,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L) - B(2,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L) - B(3,ISK)*VV  
            ELSEIF(LCOD == 6)THEN
C             
               A1 = PLY(ISUP)%A(1,L)
               A2 = PLY(ISUP)%A(2,L)
               A3 = PLY(ISUP)%A(3,L)
               V1 = PLY(ISUP)%V(1,L)
               V2 = PLY(ISUP)%V(2,L)
               V3 = PLY(ISUP)%V(3,L)
               AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
               VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L)  -  B(1,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L)  -  B(2,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L)  -  B(3,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L)  -  B(1,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L)  -  B(2,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L)  -  B(3,ISK)*VV
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               PLY(ISUP)%A(1,L)=PLY(ISUP)%A(1,L)-B(4,ISK)*AA
               PLY(ISUP)%A(2,L)=PLY(ISUP)%A(2,L)-B(5,ISK)*AA
               PLY(ISUP)%A(3,L)=PLY(ISUP)%A(3,L)-B(6,ISK)*AA
               PLY(ISUP)%V(1,L)=PLY(ISUP)%V(1,L)-B(4,ISK)*VV
               PLY(ISUP)%V(2,L)=PLY(ISUP)%V(2,L)-B(5,ISK)*VV
               PLY(ISUP)%V(3,L)=PLY(ISUP)%V(3,L)-B(6,ISK)*VV
          ELSEIF(LCOD == 7)THEN
C                
              PLY(ISUP)%A(1,L)=ZERO
              PLY(ISUP)%A(2,L)=ZERO
              PLY(ISUP)%A(3,L)=ZERO
              PLY(ISUP)%V(1,L)=ZERO
              PLY(ISUP)%V(2,L)=ZERO
              PLY(ISUP)%V(3,L)=ZERO  
           ENDIF ! lcod        
C inf          
         ELSE
            IF(LCOD == 1)THEN
 
               A1 = PLY(IINF)%A(1,L)
               A2 = PLY(IINF)%A(2,L)
               A3 = PLY(IINF)%A(3,L)
               V1 = PLY(IINF)%V(1,L)
               V2 = PLY(IINF)%V(2,L)
               V3 = PLY(IINF)%V(3,L)
               AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
               VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
               PLY(IINF)%A(1,L) = PLY(IINF)%A(1,L) - B(7,ISK)*AA
               PLY(IINF)%A(2,L) = PLY(IINF)%A(2,L) - B(8,ISK)*AA
               PLY(IINF)%A(3,L) = PLY(IINF)%A(3,L) - B(9,ISK)*AA
               PLY(IINF)%V(1,L) = PLY(IINF)%V(1,L) - B(7,ISK)*VV
               PLY(IINF)%V(2,L) = PLY(IINF)%V(2,L) - B(8,ISK)*VV
               PLY(IINF)%V(3,L) = PLY(IINF)%V(3,L) - B(9,ISK)*VV
           ELSEIF(LCOD == 2)THEN
               A1 = PLY(IINF)%A(1,L)
               A2 = PLY(IINF)%A(2,L)
               A3 = PLY(IINF)%A(3,L)
               V1 = PLY(IINF)%V(1,L)
               V2 = PLY(IINF)%V(2,L)
               V3 = PLY(IINF)%V(3,L)
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L) - B(4,ISK)*AA
               PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L) - B(5,ISK)*AA
               PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L) - B(6,ISK)*AA
               PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L) - B(4,ISK)*VV
               PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L) - B(5,ISK)*VV
               PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L) - B(6,ISK)*VV
C          
           ELSEIF(LCOD == 3)THEN

               A1 = PLY(IINF)%A(1,L)
               A2 = PLY(IINF)%A(2,L)
               A3 = PLY(IINF)%A(3,L)
               V1 = PLY(IINF)%V(1,L)
               V2 = PLY(IINF)%V(2,L)
               V3 = PLY(IINF)%V(3,L)
               AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
               VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
               
               PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L) - B(7,ISK)*AA
               PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L) - B(8,ISK)*AA
               PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L) - B(9,ISK)*AA
               PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L) - B(7,ISK)*VV
               PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L) - B(8,ISK)*VV
               PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L) - B(9,ISK)*VV
               
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               
               PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L) - B(4,ISK)*AA
               PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L) - B(5,ISK)*AA
               PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L) - B(6,ISK)*AA
               PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L) - B(4,ISK)*VV
               PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L) - B(5,ISK)*VV
               PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L) - B(6,ISK)*VV
C         
           ELSEIF(LCOD == 4)THEN
               A1 = PLY(IINF)%A(1,L)
               A2 = PLY(IINF)%A(2,L)
               A3 = PLY(IINF)%A(3,L)
               V1 = PLY(IINF)%V(1,L)
               V2 = PLY(IINF)%V(2,L)
               V3 = PLY(IINF)%V(3,L)
               
               AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
               VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
               PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L) - B(1,ISK)*AA
               PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L) - B(2,ISK)*AA
               PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L) - B(3,ISK)*AA
               PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L) - B(1,ISK)*VV
               PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L) - B(2,ISK)*VV
               PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L) - B(3,ISK)*VV              
           ELSEIF(LCOD == 5)THEN
              A1 = PLY(IINF)%A(1,L)
              A2 = PLY(IINF)%A(2,L)
              A3 = PLY(IINF)%A(3,L)
              V1 = PLY(IINF)%V(1,L)
              V2 = PLY(IINF)%V(2,L)
              V3 = PLY(IINF)%V(3,L)
              AA  =B(7,ISK)*A1 + B(8,ISK)*A2 + B(9,ISK)*A3
              VV  =B(7,ISK)*V1 + B(8,ISK)*V2 + B(9,ISK)*V3
              PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L) -  B(7,ISK)*AA
              PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L) -  B(8,ISK)*AA
              PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L) -  B(9,ISK)*AA
              PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L) -  B(7,ISK)*VV
              PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L) -  B(8,ISK)*VV
              PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L) -  B(9,ISK)*VV
              AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
              VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
              PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L) -  B(1,ISK)*AA
              PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L) -  B(2,ISK)*AA
              PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L) -  B(3,ISK)*AA
              PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L) -  B(1,ISK)*VV
              PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L) -  B(2,ISK)*VV
              PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L) -  B(3,ISK)*VV
            ELSEIF(LCOD == 6)THEN

               A1 = PLY(IINF)%A(1,L)
               A2 = PLY(IINF)%A(2,L)
               A3 = PLY(IINF)%A(3,L)
               V1 = PLY(IINF)%V(1,L)
               V2 = PLY(IINF)%V(2,L)
               V3 = PLY(IINF)%V(3,L)
               AA  =B(1,ISK)*A1 + B(2,ISK)*A2 + B(3,ISK)*A3
               VV  =B(1,ISK)*V1 + B(2,ISK)*V2 + B(3,ISK)*V3
               PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L)  -  B(1,ISK)*AA
               PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L)  -  B(2,ISK)*AA
               PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L)  -  B(3,ISK)*AA
               PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L)  -  B(1,ISK)*VV
               PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L)  -  B(2,ISK)*VV
               PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L)  -  B(3,ISK)*VV
               AA  =B(4,ISK)*A1 + B(5,ISK)*A2 + B(6,ISK)*A3
               VV  =B(4,ISK)*V1 + B(5,ISK)*V2 + B(6,ISK)*V3
               PLY(IINF)%A(1,L)=PLY(IINF)%A(1,L)-B(4,ISK)*AA
               PLY(IINF)%A(2,L)=PLY(IINF)%A(2,L)-B(5,ISK)*AA
               PLY(IINF)%A(3,L)=PLY(IINF)%A(3,L)-B(6,ISK)*AA
               PLY(IINF)%V(1,L)=PLY(IINF)%V(1,L)-B(4,ISK)*VV
               PLY(IINF)%V(2,L)=PLY(IINF)%V(2,L)-B(5,ISK)*VV
               PLY(IINF)%V(3,L)=PLY(IINF)%V(3,L)-B(6,ISK)*VV
C             
          ELSEIF(LCOD == 7)THEN
 
              PLY(IINF)%A(1,L)=ZERO
              PLY(IINF)%A(2,L)=ZERO
              PLY(IINF)%A(3,L)=ZERO
              PLY(IINF)%V(1,L)=ZERO
              PLY(IINF)%V(2,L)=ZERO
              PLY(IINF)%V(3,L)=ZERO 
           ENDIF ! lcod                           
         ENDIF  
C
        END IF ! iskew        
       ENDIF ! ibc 
      ENDDO
C  
      RETURN
      END
