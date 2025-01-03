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
      !||    inter_dcod_function    ../starter/source/interfaces/reader/inter_dcod_function.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_inter_type07   ../starter/source/interfaces/int07/hm_read_inter_type07.F
      !||    hm_read_inter_type21   ../starter/source/interfaces/int21/hm_read_inter_type21.F
      !||    hm_read_inter_type25   ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    fretitl2               ../starter/source/starter/freform.F
      !||--- uses       -----------------------------------------------------
      !||    intstamp_mod           ../starter/share/modules1/intstamp_mod.F
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    table_mod              ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE INTER_DCOD_FUNCTION(NTYP,NI,IPARI,NPC1,NOM_OPT,NPC,PLD)
C-----------------------------------------------
C     DECODE USER NUMBERS  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTSTAMP_MOD
      USE TABLE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTYP, NI
      INTEGER NPC1(*),IPARI(*),NOM_OPT(LNOPT1,*),NPC(*)
      my_real PLD(*)
      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,OK,FCOND,FCOND0,PN1,PN2,JJ
      my_real CMAX,CMIN 
      CHARACTER(LEN=NCHARTITLE) :: TITR
C     DATA IUN/1/
C
C=======================================================================
C
      ID  = NOM_OPT(1,NI)
      CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,NI),LTITR)
C
C---------------------------------------------------------
C     FUNCTIONS IN INTERFACES
C---------------------------------------------------------
                                                  
c
C---------------------------------------------------------
      IF (NTYP == 7.OR.NTYP==21) THEN

C         Inter type 7 , 21 : function friction/temperature
C---------------------------------------------------------
          OK = 0
          IF (IPARI(50) > 0) THEN           
            DO J=1,NFUNCT                     
              IF(IPARI(50) == NPC1(J)) THEN 
                 IPARI(50)=J 
                 OK = 1               
                 EXIT
              ENDIF                           
            ENDDO    
            IF (OK == 0) THEN                         
               CALL ANCMSG(MSGID=127,            
     .                  MSGTYPE=MSGERROR,        
     .                  ANMODE=ANINFO_BLIND_1,   
     .                  I1=ID,                   
     .                  C1=TITR,                 
     .                  I2=IPARI(50))          
             ENDIF
          ENDIF

C         Inter type 7 , 21 : function conduvity/Pressure
C---------------------------------------------------------
          OK = 0
          IF (IPARI(42) > 0) THEN           
            DO J=1,NFUNCT                     
              IF(IPARI(42) == NPC1(J)) THEN 
                 IPARI(42)=J                
                 OK = 1               
                 EXIT                    
              ENDIF                           
            ENDDO   
            IF (OK == 0) THEN                                                   
                CALL ANCMSG(MSGID=127,            
     .                  MSGTYPE=MSGERROR,        
     .                  ANMODE=ANINFO_BLIND_1,   
     .                  I1=ID,                   
     .                  C1=TITR,                 
     .                  I2=IPARI(42))          
             ENDIF
           ENDIF

C         Inter type 21 : function conduvity/Distance
C---------------------------------------------------------
          OK = 0
          FCOND0 = IPARI(53)
          IF (IPARI(53) > 0) THEN           
            DO J=1,NFUNCT                     
              IF(IPARI(53) == NPC1(J)) THEN 
                 IPARI(53)=J                
                 OK = 1               
                 EXIT                    
              ENDIF                           
            ENDDO   
            IF (OK == 0) THEN                                                   
                CALL ANCMSG(MSGID=127,            
     .                  MSGTYPE=MSGERROR,        
     .                  ANMODE=ANINFO_BLIND_1,   
     .                  I1=ID,                   
     .                  C1=TITR,                 
     .                  I2=IPARI(53))          
             ENDIF
C
             FCOND = IPARI(53) 
 
              PN1 = NPC(FCOND)
              PN2 = NPC(FCOND+1)
              CMAX =  PLD(PN1+1)
              CMIN =  PLD(PN1+1)
              DO JJ = PN1+2,PN2-2,2
               IF(PLD(JJ+1) > CMAX.AND.PLD(JJ)<=ONE.AND.PLD(JJ)>=ZERO) THEN
                   CMAX=PLD(JJ+1)
               ENDIF
               IF(PLD(JJ+1) < CMIN.AND.PLD(JJ)<=ONE.AND.PLD(JJ)>=ZERO) THEN
                  CMIN=PLD(JJ+1)
               ENDIF

              ENDDO
               IF(CMIN < ZERO.OR.CMAX > ONE) THEN
                CALL ANCMSG(MSGID=1811,            
     .                  MSGTYPE=MSGERROR,        
     .                  ANMODE=ANINFO_BLIND_1,   
     .                  I1=ID,                   
     .                  C1=TITR,                 
     .                  I2=FCOND0)  
                ENDIF


           ENDIF
       ENDIF

c
C---------------------------------------------------------
      IF (NTYP == 25.AND.IPARI(47) > 0) THEN


C         Function conduvity/Pressure
C---------------------------------------------------------
          OK = 0
          IF (IPARI(92) > 0) THEN           
            DO J=1,NFUNCT                     
              IF(IPARI(92) == NPC1(J)) THEN 
                 IPARI(92)=J                
                 OK = 1               
                 EXIT                    
              ENDIF                           
            ENDDO   
            IF (OK == 0) THEN                                                   
                CALL ANCMSG(MSGID=127,            
     .                  MSGTYPE=MSGERROR,        
     .                  ANMODE=ANINFO_BLIND_1,   
     .                  I1=ID,                   
     .                  C1=TITR,                 
     .                  I2=IPARI(92))          
             ENDIF
           ENDIF

C         function friction/temperature
C---------------------------------------------------------
          OK = 0
          IF (IPARI(50) > 0) THEN           
            DO J=1,NFUNCT                     
              IF(IPARI(50) == NPC1(J)) THEN 
                 IPARI(50)=J 
                 OK = 1               
                 EXIT
              ENDIF                           
            ENDDO    
            IF (OK == 0) THEN                         
               CALL ANCMSG(MSGID=127,            
     .                  MSGTYPE=MSGERROR,        
     .                  ANMODE=ANINFO_BLIND_1,   
     .                  I1=ID,                   
     .                  C1=TITR,                 
     .                  I2=IPARI(50))          
             ENDIF
          ENDIF


C         Inter type 25 : function conduvity/Distance
C---------------------------------------------------------
          OK = 0
          FCOND0 = IPARI(93)
          IF (IPARI(93) > 0) THEN           
            DO J=1,NFUNCT                     
              IF(IPARI(93) == NPC1(J)) THEN 
                 IPARI(93)=J                
                 OK = 1               
                 EXIT                    
              ENDIF                           
            ENDDO   
            IF (OK == 0) THEN                                                   
                CALL ANCMSG(MSGID=127,            
     .                  MSGTYPE=MSGERROR,        
     .                  ANMODE=ANINFO_BLIND_1,   
     .                  I1=ID,                   
     .                  C1=TITR,                 
     .                  I2=IPARI(93))          
             ENDIF
C
             FCOND = IPARI(93) 
 
              PN1 = NPC(FCOND)
              PN2 = NPC(FCOND+1)
              CMAX =  PLD(PN1+1)
              CMIN =  PLD(PN1+1)
              DO JJ = PN1+2,PN2-2,2
               IF(PLD(JJ+1) > CMAX.AND.PLD(JJ)<=ONE.AND.PLD(JJ)>=ZERO) THEN
                   CMAX=PLD(JJ+1)
               ENDIF
               IF(PLD(JJ+1) < CMIN.AND.PLD(JJ)<=ONE.AND.PLD(JJ)>=ZERO) THEN
                  CMIN=PLD(JJ+1)
               ENDIF

              ENDDO
               IF(CMIN < ZERO.OR.CMAX > ONE) THEN
                CALL ANCMSG(MSGID=1811,            
     .                  MSGTYPE=MSGERROR,        
     .                  ANMODE=ANINFO_BLIND_1,   
     .                  I1=ID,                   
     .                  C1=TITR,                 
     .                  I2=FCOND0)  
                ENDIF


           ENDIF
       ENDIF

      RETURN
C-----
      END
C

