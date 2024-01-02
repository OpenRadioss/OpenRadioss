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
Chd|  I7PWR3                        source/interfaces/inter3d1/i7pwr3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I7PWR3(ITAB,INACTI,CAND_E,CAND_N,STFN,
     1                  STF ,X     ,NSV   ,IWPENE,CAND_P,
     2                  CAND_EN,CAND_NN,TAG,NOINT,GAPV ,
     3                  NTY    ,ITIED  ,FPENMAX,ID,TITR,
     4                  IDDLEVEL,IREMNODE,KREMNODE,REMNODE,ISTOK,
     5                  IX1,IX2,IX3,IX4,NSVG,
     6                  X1 ,X2 ,X3 ,X4 ,Y1  ,
     7                  Y2 ,Y3 ,Y4 ,Z1 ,Z2  ,
     8                  Z3 ,Z4 ,XI ,YI ,ZI  ,
     9                  N1 ,N2 ,N3 ,PENE)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "vect07_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),CAND_E(*),CAND_N(*),CAND_EN(*),CAND_NN(*),
     .   KREMNODE(*),REMNODE(*)
      INTEGER NSV(*),TAG(*),IWPENE,INACTI,NOINT,NTY,ITIED, ISTOK
      my_real
     .   STF(*),STFN(*),X(3,*),CAND_P(*),GAPV(*), FPENMAX
      INTEGER ID,IDDLEVEL,IREMNODE
      CHARACTER*nchartitle,
     .   TITR
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: IX1,IX2,IX3,IX4,NSVG
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: N1,N2,N3
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: PENE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,JWARN,J,K,L,TAGNOD
      my_real
     .     PENEOLD, PENMAX, PENE0
      my_real
     .  DN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      JWARN = 0
      DO I=LFT,LLT                                                                                                           
        TAGNOD = 0                                                                                                           
        IF(IREMNODE == 3)THEN                                                                                                
          K = KREMNODE(CAND_E(I))+1                                                                                          
          L = KREMNODE(CAND_E(I)+1)                                                                                          
          DO J=K,L                                                                                                           
            IF( REMNODE(J) == NSVG(I) ) TAGNOD = 1                                                                           
          ENDDO                                                                                                              
        ENDIF                                                                                                                
        IF(IPRI>=1 .AND. PENE(I)>ZERO .AND. TAGNOD == 0)THEN                                                            
         IF(IX1(I)<=NUMNOD) THEN                                                                                           
          WRITE(IOUT,FMT=FMW_5I)ITAB(NSVG(I)),ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I))                            
         ELSE                                                                                                                
          WRITE(IOUT,FMT=FMW_5I)NSVG(I),IX1(I),IX2(I),IX3(I),IX4(I)                                                          
         ENDIF                                                                                                               
        ELSEIF(IPRI>=6 .AND. TAGNOD == 0)THEN                                                                              
         IF(IX1(I)<=NUMNOD) THEN                                                                                           
          WRITE(IOUT,FMT=FMW_5I)ITAB(NSVG(I)),ITAB(IX1(I)),ITAB(IX2(I)),ITAB(IX3(I)),ITAB(IX4(I))                            
         ELSE                                                                                                                
          WRITE(IOUT,FMT=FMW_5I)NSVG(I),IX1(I),IX2(I),IX3(I),IX4(I)                                                          
         ENDIF                                                                                                               
        ENDIF                                                                                                                
        IF(PENE(I)>ZERO .AND. TAGNOD == 0)THEN                                                                            
          TAG(NSVG(I))=TAG(NSVG(I))+1                                                                                        
          DN=N1(I)*N1(I)+N2(I)*N2(I)+N3(I)*N3(I)                                                                             
          IF(DN<=EM30) THEN                                                                                                
            IF(IX1(I)<=NUMNOD) THEN                                                                                        
             WRITE(IOUT,1100)PENE(I),ITAB(NSVG(I))                                                                           
             IF(NTY/=24.AND.(NTY/=10.OR.ITIED==0))THEN                                                                 
               IF(INACTI/=1.AND.INACTI/=2.AND.FPENMAX==ZERO) THEN                                                        

                 IF (INACTI==0) THEN                                                                                       
                   CALL ANCMSG(MSGID=612,                                                                                    
     .                         MSGTYPE=MSGERROR,                                                                             
     .                         ANMODE=ANINFO_BLIND_1,                                                                        
     .                         I1=ID,                                                                                        
     .                         C1=TITR,                                                                                      
     .                         I2=INACTI,                                                                                    
     .                         I3=ITAB(NSVG(I)))                                                                             
                ELSE                                                                                                         
                   CALL ANCMSG(MSGID=611,                                                                                    
     .                         MSGTYPE=MSGERROR,                                                                             
     .                         ANMODE=ANINFO_BLIND_1,                                                                        
     .                         I1=ID,                                                                                        
     .                         C1=TITR,                                                                                      
     .                         I2=INACTI,                                                                                    
     .                         I3=ITAB(NSVG(I)))                                                                             
                ENDIF                                                                                                        
               ENDIF                                                                                                         
             END IF                                                                                                          
            ELSE                                                                                                             
             WRITE(IOUT,1100)PENE(I),NSVG(I)                                                                                 
             IF(NTY/=24.AND.(NTY/=10.OR.ITIED==0))THEN                                                                 
               IF(INACTI/=1.AND.INACTI/=2.AND.FPENMAX==ZERO) THEN                                                        

                 IF (INACTI==0) THEN                                                                                       
                   CALL ANCMSG(MSGID=612,                                                                                    
     .                         MSGTYPE=MSGERROR,                                                                             
     .                         ANMODE=ANINFO_BLIND_1,                                                                        
     .                         I1=ID,                                                                                        
     .                         C1=TITR,                                                                                      
     .                         I2=INACTI,                                                                                    
     .                         I3=NSVG(I))                                                                                   
                ELSE                                                                                                         
                   CALL ANCMSG(MSGID=611,                                                                                    
     .                         MSGTYPE=MSGERROR,                                                                             
     .                         ANMODE=ANINFO_BLIND_1,                                                                        
     .                         I1=ID,                                                                                        
     .                         C1=TITR,                                                                                      
     .                         I2=INACTI,                                                                                    
     .                         I3=NSVG(I))                                                                                   
                ENDIF                                                                                                        
               ENDIF                                                                                                         
             END IF                                                                                                          
            ENDIF                                                                                                            
          ELSE                                                                                                               
            PENE0 = PENE(I)                                                                                                  
            PENE(I) = PENE(I) + EM8*PENE(I)                                                                                  
            IF(IPRI>=5) THEN                                                                                               
             IF(IX1(I)<=NUMNOD) THEN                                                                                       
              CALL ANCMSG(MSGID=1164,                                                                                        
     .                         MSGTYPE=MSGWARNING,                                                                           
     .                         ANMODE=ANINFO_BLIND_1,                                                                        
     .                         I1=ITAB(NSVG(I)),                                                                             
     .                         I2=ITAB(IX1(I)),                                                                              
     .                         I3=ITAB(IX2(I)),                                                                              
     .                         I4=ITAB(IX3(I)),                                                                              
     .                         I5=ITAB(IX4(I)),                                                                              
     .                         R1=PENE0,                                                                                     
     .                         PRMOD=MSG_CUMU)                                                                               
             ELSE                                                                                                            
              CALL ANCMSG(MSGID=1164,                                                                                        
     .                         MSGTYPE=MSGWARNING,                                                                           
     .                         ANMODE=ANINFO_BLIND_1,                                                                        
     .                         I1=NSVG(I),                                                                                   
     .                         I2=IX1(I),                                                                                    
     .                         I3=IX2(I),                                                                                    
     .                         I4=IX3(I),                                                                                    
     .                         I5=IX4(I),                                                                                    
     .                         R1=PENE0,                                                                                     
     .                         PRMOD=MSG_CUMU)                                                                               
             ENDIF                                                                                                           
            ENDIF                                                                                                            
          ENDIF                                                                                                              
          PENMAX = FPENMAX*GAPV(I)                                                                                           
          IF(.NOT.((INACTI==5.OR.INACTI==6).AND.(FPENMAX /= ZERO .AND. PENE(I) > PENMAX)))ISTOK=ISTOK+1                      
          IF(FPENMAX /= ZERO .AND. PENE(I) > PENMAX) THEN                                                                    
              !DESACTIVATION DES NOEUDS                                                                                      
              WRITE(IOUT,'(A,1PG20.13,A)')' MAX INITIAL PENETRATION ',PENMAX,' IS REACHED'                                   
              WRITE(IOUT,'(A)')'NODE STIFFNESS IS SET TO ZERO'                                                               
              STFN(CAND_N(I)) = ZERO                                                                                         
          ELSE IF(INACTI==1) THEN                                                                                          
              !DESACTIVATION DES NOEUDS                                                                                      
              WRITE(IOUT,'(A)')'NODE STIFFNESS IS SET TO ZERO'                                                               
              STFN(CAND_N(I)) = ZERO                                                                                         
          ELSE IF(INACTI==2) THEN                                                                                          
            !DESACTIVATION DES ELEMENTS                                                                                      
            WRITE(IOUT,'(A)')'ELEMENT STIFFNESS IS SET TO ZERO'                                                              
            STF(CAND_E(I)) = ZERO                                                                                            
          ELSE IF(INACTI==3) THEN                                                                                          
            !CHANGE LES COORDONNEES DES NOEUDS SECONDARY                                                                         
            WRITE(IOUT,'(A)')'NODE COORD IS CHANGED AS PROPOSED'                                                             
            PENEOLD = SQRT( (X(1,NSV(CAND_N(I)))-XI(I))**2 +(X(2,NSV(CAND_N(I)))-YI(I))**2 +(X(3,NSV(CAND_N(I)))-ZI(I))**2   )
            IF(PENE(I)>PENEOLD) THEN                                                                                      
              X(1,NSV(CAND_N(I))) = XI(I)+PENE(I)*N1(I)                                                                      
              X(2,NSV(CAND_N(I))) = YI(I)+PENE(I)*N2(I)                                                                      
              X(3,NSV(CAND_N(I))) = ZI(I)+PENE(I)*N3(I)                                                                      
            ENDIF                                                                                                            
          ELSE IF(INACTI==4) THEN                                                                                          
            !CHANGE LES COORDONNEES DES NOEUDS MAIN                                                                        
            WRITE(IOUT,'(A)')'SEG. COORD IS CHANGED AS PROPOSED'                                                             
            PENEOLD = SQRT( (X(1,IX1(I))-X1(I))**2 +(X(2,IX1(I))-Y1(I))**2 +(X(3,IX1(I))-Z1(I))**2 )                         
            IF(PENE(I)>PENEOLD) THEN                                                                                      
              X(1,IX1(I)) = X1(I)-PENE(I)*N1(I)                                                                              
              X(2,IX1(I)) = Y1(I)-PENE(I)*N2(I)                                                                              
              X(3,IX1(I)) = Z1(I)-PENE(I)*N3(I)                                                                              
            ENDIF                                                                                                            
            PENEOLD = SQRT( (X(1,IX2(I))-X2(I))**2 +(X(2,IX2(I))-Y2(I))**2 +(X(3,IX2(I))-Z2(I))**2 )                         
            IF(PENE(I)>PENEOLD) THEN                                                                                      
              X(1,IX2(I)) = X2(I)-PENE(I)*N1(I)                                                                              
              X(2,IX2(I)) = Y2(I)-PENE(I)*N2(I)                                                                              
              X(3,IX2(I)) = Z2(I)-PENE(I)*N3(I)                                                                              
            ENDIF                                                                                                            
            PENEOLD = SQRT( (X(1,IX3(I))-X3(I))**2 +(X(2,IX3(I))-Y3(I))**2 +(X(3,IX3(I))-Z3(I))**2 )                         
            IF(PENE(I)>PENEOLD) THEN                                                                                      
              X(1,IX3(I)) = X3(I)-PENE(I)*N1(I)                                                                              
              X(2,IX3(I)) = Y3(I)-PENE(I)*N2(I)                                                                              
              X(3,IX3(I)) = Z3(I)-PENE(I)*N3(I)                                                                              
            ENDIF                                                                                                            
            PENEOLD = SQRT( (X(1,IX4(I))-X4(I))**2 +(X(2,IX4(I))-Y4(I))**2 +(X(3,IX4(I))-Z4(I))**2 )                         
            IF(PENE(I)>PENEOLD) THEN                                                                                      
              X(1,IX4(I)) = X4(I)-PENE(I)*N1(I)                                                                              
              X(2,IX4(I)) = Y4(I)-PENE(I)*N2(I)                                                                              
              X(3,IX4(I)) = Z4(I)-PENE(I)*N3(I)                                                                              
            ENDIF                                                                                                            
          ELSE IF(INACTI==5) THEN                                                                                          
             !REDUCTION DU GAP                                                                                               
             JWARN = 1                                                                                                       
             CAND_P(ISTOK) = PENE(I)                                                                                         
             CAND_NN(ISTOK) = CAND_N(I)                                                                                      
             CAND_EN(ISTOK) = CAND_E(I)                                                                                      
           ELSE IF(INACTI==6) THEN                                                                                         
             !INACTI==6                                                                                                    
             !REDUCTION DU GAP                                                                                               
             JWARN = 1                                                                                                       
             PENE(I)=PENE(I)+ZEP05*(GAPV(I)-PENE(I))                                                                         
             CAND_P(ISTOK) = PENE(I)                                                                                         
             CAND_NN(ISTOK) = CAND_N(I)                                                                                      
             CAND_EN(ISTOK) = CAND_E(I)                                                                                      
           END IF                                                                                                            
           IWPENE=IWPENE+1                                                                                                   
        ENDIF                                                                                                                

      ENDDO!NEXT I                                                                                                           
 
      IF (JWARN /= 0) WRITE(IOUT,'(A)')'REDUCE INITIAL GAP'
      
 1000 FORMAT(2X,'** INITIAL PENETRATION =',1PG20.13,' POSSIBLE NEW COORDINATES OF SECONDARY NODE')
 1100 FORMAT(2X,'** INITIAL PENETRATION =',E14.7   ,' IMPOSSIBLE TO CALCULATE NEW COORDINATES OF SECONDARY NODE',I8)

C-----------------------------------------------
      RETURN
      END

