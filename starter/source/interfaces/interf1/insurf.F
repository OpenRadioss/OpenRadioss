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
Chd|  INSURF                        source/interfaces/interf1/insurf.F
Chd|-- called by -----------
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        COUNT3                        source/interfaces/interf1/count3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INSURF(NRT       ,MSN   ,IR    ,IRECT ,NOINT  ,
     .                  SURF_NODES,ITAB  ,MSV   ,ID    ,TITR   ,
     .                  NTAG      ,S_MSV  ,SIRECT, X, TYPE18)
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
      INTEGER NRT, NOINT,IR,MSN
      INTEGER,INTENT(IN) :: S_MSV,SIRECT
      INTEGER IRECT(4,SIRECT/4), ITAB(NUMNOD), MSV(S_MSV),SURF_NODES(NRT,4)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      INTEGER, DIMENSION(2*NUMNOD+1), INTENT(INOUT) ::  NTAG
      LOGICAL, INTENT(INOUT) :: TYPE18
      my_real,INTENT(IN) :: X(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N23, IT, J, NSGOLD,ITG,K,L,IR1,K4
      INTEGER OLDIRECT,OLDIRECT1
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C=======================================================================
      I=1                                                   
      L=1                                                   
      DO J=1,NRT                                            
          DO K=1,4                                          
            IRECT(K,J)=SURF_NODES(J,K)
            L=L+1                                           
          ENDDO                                             
          L=L+2                                             
      ENDDO                                                 
C     SEGMENTS 3 NOEUDS MISE DE N4 = N3                     
      DO I=1,NRT                                            
        IF(IRECT(4,I)==0) IRECT(4,I)=IRECT(3,I)           
        IF(IRECT(1,I)==IRECT(4,I)) THEN
           OLDIRECT=IRECT(4,I)
           IRECT(4,I)=IRECT(3,I)                            
C           WRITE(IOUT,*)'       REPLACED BY :',            
C            IRECT(1,I),IRECT(2,I),IRECT(3,I),IRECT(4,I).   
           CALL ANCMSG(MSGID=106,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IRECT(1,I),
     .                 I3=IRECT(2,I),
     .                 I4=IRECT(3,I),
     .                 I5=OLDIRECT,
     .                 I6=IRECT(1,I),
     .                 I7=IRECT(2,I),
     .                 I8=IRECT(3,I),
     .                 I9=IRECT(4,I))
        ELSEIF(IRECT(2,I)==IRECT(3,I)) THEN               
C           IWARN = IWARN + 1                               
C           WRITE(IOUT,*)' WARNING INTERFACE SEGMENT :',    
C            IRECT(1,I),IRECT(2,I),IRECT(3,I),IRECT(4,I).   
           OLDIRECT=IRECT(3,I)
           IRECT(3,I)=IRECT(4,I)                            
C           WRITE(IOUT,*)'       REPLACED BY :',            
C            IRECT(1,I),IRECT(2,I),IRECT(3,I),IRECT(4,I).   
           CALL ANCMSG(MSGID=106,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IRECT(1,I),
     .                 I3=IRECT(2,I),
     .                 I4=OLDIRECT,
     .                 I5=IRECT(4,I),
     .                 I6=IRECT(1,I),
     .                 I7=IRECT(2,I),
     .                 I8=IRECT(3,I),
     .                 I9=IRECT(4,I))
        ELSEIF(IRECT(1,I)==IRECT(2,I)) THEN               
C           IWARN = IWARN + 1                               
C           WRITE(IOUT,*)' WARNING INTERFACE SEGMENT :',    
C            IRECT(1,I),IRECT(2,I),IRECT(3,I),IRECT(4,I).   
           OLDIRECT=IRECT(2,I)
           OLDIRECT1=IRECT(3,I)
           IRECT(2,I)=IRECT(3,I)                            
           IRECT(3,I)=IRECT(4,I)                            
C           WRITE(IOUT,*)'       REPLACED BY :',            
C            IRECT(1,I),IRECT(2,I),IRECT(3,I),IRECT(4,I).   
           CALL ANCMSG(MSGID=106,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IRECT(1,I),
     .                 I3=OLDIRECT,
     .                 I4=OLDIRECT1,
     .                 I5=IRECT(4,I),
     .                 I6=IRECT(1,I),
     .                 I7=IRECT(2,I),
     .                 I8=IRECT(3,I),
     .                 I9=IRECT(4,I))
        ENDIF                                               
      ENDDO                                                 
C-------    
      IF(IR/=0.AND.IR/=2)THEN              
        DO J=1,NRT                 
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
      IF(IPRI>=1 .AND. .NOT.TYPE18) THEN                                              
        WRITE(IOUT,'(/,A,/)')' SEGMENTS USED FOR SURFACE DEFINITION'  
        K=1                                                           
        K4=4                                                          
        IF(N2D/=0)K4=2                                              
        DO I=1,NRT                                                    
          WRITE(IOUT,FMT=FMW_4I)(ITAB(IRECT(K,I)),K=1,K4)             
        ENDDO                                                         
      ENDIF                                                          
      IF (NRT/=0) CALL COUNT3(IRECT,MSV,MSN,NRT,NTAG)                      
C------------------------------------------------------------
      RETURN
      END
