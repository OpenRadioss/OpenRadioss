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
Chd|  INSURFIGEO                    source/interfaces/interf1/insurfigeo.F
Chd|-- called by -----------
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|-- calls ---------------
Chd|        COUNTIGEO                     source/interfaces/interf1/countigeo.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INSURFIGEO(NRT_IGE ,OFFSET_SEG   ,MSN_IGE,OFFSET_NODE,IADTABIGE,IR    ,IRECT  ,NOINT  ,
     .                 SURF_NODES_IGE,ITAB  ,MSV   ,ID,TITR,
     .                 NIGE,RIGE,XIGE,VIGE,NIGE_TMP,RIGE_TMP,XIGE_TMP,VIGE_TMP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------------------------------
C     LECTURE DES SURFACES ET DECOMPTE DES SEGMENTS
C           ENTREE : 
C                    NRT_IGE NOMBRE DE RENSEIGNEMENTS A LIRE
C           SORTIE : 
C                    IRECT 
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
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT_IGE,OFFSET_SEG,NOINT,IR,MSN_IGE,OFFSET_NODE,IADTABIGE
      INTEGER IRECT(4,*),
     .        ITAB(*), MSV(*),NIGE_TMP(*),NIGE(*),SURF_NODES_IGE(NRT_IGE,4)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      my_real
     .   RIGE(3,*),XIGE(3,*),VIGE(3,*),
     .   RIGE_TMP(3,*),XIGE_TMP(3,*),VIGE_TMP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N23, IT, J, NSGOLD,ITG,K,L,IR1,K4,JSEG
      INTEGER OLDIRECT,OLDIRECT1
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C=======================================================================   
CC dans INTBUF_TAB%NIGE, on ecrit tout les points fictifs slave puis les points fictifs master
c NIGE est offsete a l'appel de la routine

      DO J=1,MSN_IGE
        NIGE(J)=NIGE_TMP(J+IADTABIGE) 
        RIGE(:,J)=RIGE_TMP(:,J+IADTABIGE)       
        XIGE(:,J)=XIGE_TMP(:,J+IADTABIGE)                                     
        VIGE(:,J)=VIGE_TMP(:,J+IADTABIGE)  
        XIGE(:,J)=XIGE_TMP(:,J+IADTABIGE) 
      ENDDO
      DO J=OFFSET_SEG+1,OFFSET_SEG+NRT_IGE                                            
        DO K=1,4                                          
          IRECT(K,J)=SURF_NODES_IGE(J,K)+OFFSET_NODE
        ENDDO    
      ENDDO                                                 
C-------    
      IF(IR.NE.0.AND.IR.NE.2)THEN              
        DO J=OFFSET_SEG+1,OFFSET_SEG+NRT_IGE                 
          IR1=IRECT(1,J)           
          IRECT(1,J)=IRECT(2,J)    
          IRECT(2,J)=IR1           
          IR1=IRECT(3,J)           
          IRECT(3,J)=IRECT(4,J)    
          IRECT(4,J)=IR1            
        ENDDO                      
      ENDIF                        
      N23=4                        
C-------          
      IF(IPRI.GE.1) THEN                                              
        WRITE(IOUT,'(/,A,/)')' SEGMENTS USED FOR ISOGEOMETRIC SURFACE DEFINITION'  
        K=1
        K4=4
        DO I=OFFSET_SEG+1,OFFSET_SEG+NRT_IGE  
          WRITE(IOUT,FMT=FMW_4I)(IRECT(K,I),K=1,K4)  !  ECRITURE NON DEFINITIVE
        ENDDO                                                         
      ENDIF                                                           
      IF (NRT_IGE.NE.0) CALL COUNTIGEO(IRECT,MSV,MSN_IGE,NRT_IGE,OFFSET_SEG)                    
C------------------------------------------------------------
      RETURN
      END
