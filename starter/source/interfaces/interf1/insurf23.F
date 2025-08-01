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
!||    insurf23     ../starter/source/interfaces/interf1/insurf23.F
!||--- called by ------------------------------------------------------
!||    lecins       ../starter/source/interfaces/interf1/lecins.F
!||    lecint       ../starter/source/interfaces/interf1/lecint.F
!||--- calls      -----------------------------------------------------
!||    count3       ../starter/source/interfaces/interf1/count3.F
!||--- uses       -----------------------------------------------------
!||    format_mod   ../starter/share/modules1/format_mod.F90
!||====================================================================
      SUBROUTINE INSURF23(NRT       ,MSN   ,IR    ,IRECT ,NOINT  ,
     .                    SURF_NODES,ITAB  ,MSV   ,NRTNEW,X      ,
     .                    NTAG      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE FORMAT_MOD , ONLY : FMW_4I
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     READING SURFACE AND NUMBERING SEGMENTS
C           IN :  NRT NOMBRE DE RENSEIGNEMENTS A LIRE
C           OUT : IRECT
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NOINT, IR, MSN, NRTNEW
      INTEGER IRECT(4,*), ITAB(*), MSV(*),SURF_NODES(NRT,4)
      INTEGER, DIMENSION(2*NUMNOD+1), INTENT(INOUT) ::  NTAG
      my_real X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N23, IT, J, NSGOLD,ITG,K,IR1,K4,N1,N2,N3,N4
      my_real X13,Y13,Z13,X24,Y24,Z24,D1,D2
C=======================================================================
      NRTNEW=0

      I=1                                                   
      DO J=1,NRT
        N1=SURF_NODES(J,1)
        N2=SURF_NODES(J,2)
        N3=SURF_NODES(J,3)
        N4=SURF_NODES(J,4)
        NRTNEW=NRTNEW+1
        IRECT(1,NRTNEW)=N1
        IRECT(2,NRTNEW)=N2
        IRECT(3,NRTNEW)=N3
        IRECT(4,NRTNEW)=N4
      ENDDO                                                 
C-------          
      IF(IPRI>=1) THEN                                              
        WRITE(IOUT,'(/,A,/)')' SEGMENTS USED FOR SURFACE DEFINITION'  
        K=1                                                           
        K4=4                                                          
        IF(N2D/=0)K4=2                                              
        DO I=1,NRTNEW                                                    
          WRITE(IOUT,FMT=FMW_4I)(ITAB(IRECT(K,I)),K=1,K4)             
        ENDDO                                                         
      ENDIF                                                           
      IF (NRTNEW/=0) CALL COUNT3(IRECT,MSV,MSN,NRTNEW,NTAG)                         
C------------------------------------------------------------
      RETURN
      END
