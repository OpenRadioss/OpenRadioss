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
Chd|  UNIT_CODE                     source/general_controls/computation/unit_code.F
Chd|-- called by -----------
Chd|        HM_READ_UNIT                  source/general_controls/computation/hm_read_unit.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE UNIT_CODE(LENGTH,FIELD,KEY,FAC,IERR1,ID)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENGTH,IERR1,ID
      CHARACTER FIELD*(*),KEY*(*)               !precondition:LEN(KEY)>=4 ('-check bounds' error otherwise)
      my_real FAC
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,J1,J2,J3,N,IREEL
      CHARACTER CFAC*20, CUNIT
C=======================================================================
      IREEL = 0
      READ(FIELD,ERR=100,FMT=FMT_F) FAC
      IREEL=1
* La valeur entree est un reel
100   CONTINUE
* La valeur entree est un code (ou une erreur)
      IF (IREEL == 0) THEN  
         CFAC='  '                                         
         IERR1= 1
         I   = 1                                   
         J   = 0                                   
         J1  = 1                                           
         J2  = 1                                           
         J3  = 1                                           
C        Skip leading spaces
         DO WHILE (I <= LENGTH)
           IF (FIELD(I:I) /= ' ') EXIT
           I=I+1
         ENDDO
C        Read, skip trailing spaces
         DO WHILE (I <= LENGTH)
           IF (FIELD(I:I) == ' ') EXIT
           J=J+1
           I=I+1                                           
         ENDDO
         J3 = I-1
         IF (J == 1) THEN    
           CUNIT=FIELD(J3:J3)
         ELSEIF (J == 2) THEN  
           J1 = J3-1  
           CUNIT=FIELD(J3:J3)
           CFAC(1:2)=FIELD(J1:J1)                           
         ELSEIF (J == 3) THEN  
           J1 = J3-2  
           J2 = J3-1  
           CUNIT=FIELD(J3:J3)
           CFAC(1:2)=FIELD(J1:J2)
         ELSE          
           IERR1=0
         ENDIF 
C---
        SELECT CASE (CFAC(1:2))
        CASE ('y ')
            FAC=EM20*EM04                                  
        CASE ('z ')                    
            FAC=EM20*EM01                                  
        CASE ('a ')               
            FAC=EM18                                       
        CASE ('p ')        
            FAC=EM12                                       
        CASE ('f ')   
            FAC=EM15                                       
        CASE ('n ')        
            FAC=EM09                                       
        CASE ('µ ','mu', 'u ')      
            FAC=EM06                                       
        CASE ('m ')
            FAC=EM03                                       
        CASE ('c ')
            FAC=EM02                                       
        CASE ('d ')
            FAC=EM01                                       
        CASE ('  ')
            FAC=ONE                                         
        CASE ('da')
            FAC=TEN                                        
        CASE ('h ')
            FAC=HUNDRED                                       
        CASE ('k ')
            FAC=EP03                                         
        CASE ('K ')
            FAC=EP03                                      
        CASE ('M ')
            FAC=EP06                                       
        CASE ('G ')
            FAC=EP09                                       
        CASE ('T ')
            FAC=EP12                                       
        CASE ('P ')
            FAC=EP15                                       
        CASE ('E ')
            FAC=EP18                                       
        CASE ('Z ')
            FAC=EP20*TEN                                   
        CASE ('Y ')
            FAC=EP20*EP04                                  
        CASE DEFAULT       
            IERR1=0                                       
        END SELECT                                           
*       L'unite SI de masse est le kg et pas le g
        IF (KEY(1:4) == 'MASS') FAC=FAC*EM03   
C---
        IF ((              KEY(1:4) == 'MASS'  .AND.CUNIT /= 'g').OR.   
     .      (KEY(1:MIN(LEN(KEY),6)) == 'LENGTH'.AND.CUNIT /= 'm').OR.   
     .      (              KEY(1:4) == 'TIME'  .AND.CUNIT /= 's').OR.   
     .      (IERR1 == 0) 
     .      ) THEN                               
           CALL ANCMSG(MSGID=573,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C2=KEY,
     .                 C1=FIELD(J1:J3))
        ENDIF                                              
C-----
      ENDIF                                     
C-----
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
C-----
      RETURN
      END
