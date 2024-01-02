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
Chd|  INSURF_DX                     source/interfaces/interf1/insurf_dx.F
Chd|-- called by -----------
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INSURF_DX(NRT       ,MSN   ,IR    ,IRECT ,NOINT  ,
     .                  SURF_NODES,ITAB  ,MSV   ,ID    ,TITR   ,
     .                  NTAG      ,S_MSV  ,SIRECT, X, STIFF_STAT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C     LECTURE DES SURFACES ET DECOMPTE DES SEGMENTS
C           ENTREE : 
C                    NRT NOMBRE DE RENSEIGNEMENTS A LIRE
C           SORTIE : 
C                    IRECT 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT,IR,MSN
      INTEGER,INTENT(IN) :: S_MSV,SIRECT
      INTEGER IRECT(4,SIRECT/4), ITAB(NUMNOD), MSV(S_MSV),SURF_NODES(NRT,4)
      INTEGER ID
      my_real, INTENT(INOUT) :: X(3,NUMNOD)
      CHARACTER*nchartitle,
     .   TITR
      INTEGER, DIMENSION(2*NUMNOD+1), INTENT(INOUT) ::  NTAG
      my_real, INTENT(INOUT) :: STIFF_STAT(3)
      INTEGER,INTENT(IN) :: NOINT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,K4
      my_real :: SURF, S2n(3), D1(3), D2(3), SURF_MAX, SURF_MIN, SURF_MEAN, STVAL,VEL
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      SURF_MIN = EP20
      SURF_MAX = ZERO
      SURF_MEAN = ZERO
      DO I=1,NRT                                                     
        IF(IRECT(3,I)==IRECT(4,I)) THEN
          D1(1)=X(1,IRECT(1,I))-X(1,IRECT(3,I))
          D1(2)=X(2,IRECT(1,I))-X(2,IRECT(3,I))
          D1(3)=X(3,IRECT(1,I))-X(3,IRECT(3,I))                    
          D2(1)=X(1,IRECT(2,I))-X(1,IRECT(1,I))
          D2(2)=X(2,IRECT(2,I))-X(2,IRECT(1,I))
          D2(3)=X(3,IRECT(2,I))-X(3,IRECT(1,I))
          S2n(1) =   D1(2)*D2(3)-D1(3)*D2(2)
          S2n(2) = - D1(1)*D2(3)+D1(3)*D2(1)
          S2n(3) =   D1(1)*D2(2)-D1(2)*D2(1)
          SURF = S2n(1)*S2n(1) + S2n(2)*S2n(2) + S2n(3)*S2n(3)
          SURF = SQRT(FOURTH*SURF)        
        ELSE
          D1(1)=X(1,IRECT(1,I))-X(1,IRECT(3,I))
          D1(2)=X(2,IRECT(1,I))-X(2,IRECT(3,I))
          D1(3)=X(3,IRECT(1,I))-X(3,IRECT(3,I))                    
          D2(1)=X(1,IRECT(2,I))-X(1,IRECT(4,I))
          D2(2)=X(2,IRECT(2,I))-X(2,IRECT(4,I))
          D2(3)=X(3,IRECT(2,I))-X(3,IRECT(4,I))
          S2n(1) =   D1(2)*D2(3)-D1(3)*D2(2)
          S2n(2) = - D1(1)*D2(3)+D1(3)*D2(1)
          S2n(3) =   D1(1)*D2(2)-D1(2)*D2(1)
          SURF = S2n(1)*S2n(1) + S2n(2)*S2n(2) + S2n(3)*S2n(3)
          SURF = SQRT(FOURTH*SURF)
        ENDIF 
        
        SURF_MAX = MAX(SURF_MAX,SURF)
        SURF_MIN = MIN(SURF_MIN,SURF)
        SURF_MEAN = SURF_MEAN + SURF/REAL(NRT)
                                                      
      ENDDO   

      STVAL = STIFF_STAT(1)*STIFF_STAT(2)*SURF_MEAN/STIFF_STAT(3)
      
      STIFF_STAT(1) = -STVAL
      
      
      WRITE(IOUT,1000)NOINT
      WRITE(IOUT,3021)STVAL, STIFF_STAT(2),  SURF_MEAN, SURF_MIN, SURF_MAX
      IF(IPRI>=1) THEN                                              
        WRITE(IOUT,'(/,A,/)')' SEGMENTS USED FOR SURFACE DEFINITION'  
        K=1                                                           
        K4=4                                                          
        IF(N2D/=0)K4=2                                              
        DO I=1,NRT                                                    
          WRITE(IOUT,FMT=FMW_4I)(ITAB(IRECT(K,I)),K=1,K4)             
        ENDDO                                               
      ENDIF 
                                                    
C------------------------------------------------------------
      RETURN
1000  FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)      
3021  FORMAT(
     .    '    COMPUTED STIFFNESS VALUE. . . . . . . . . . ',1PG20.13,/,
     .    '                      . . .USING DENSITY. . . . ',1PG20.13,/,     
     .    '                      . . .USING MEAN AREA. . . ',1PG20.13,/,          
     .    '                      . . . . . . . . .MIN. . . ',1PG20.13,/,        
     .    '                      . . . . . . . . .MAX  . . ',1PG20.13)                    

      
      END
