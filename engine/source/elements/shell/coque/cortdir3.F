Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
C   27/12/01 nouvelle routine
Chd|====================================================================
Chd|  CORTDIR3                      source/elements/shell/coque/cortdir3.F
Chd|-- called by -----------
Chd|        C3COORK3                      source/elements/sh3n/coque3n/c3coork3.F
Chd|        CBACOOR                       source/elements/shell/coqueba/cbacoor.F
Chd|        CBACOORK                      source/elements/shell/coqueba/cbacoork.F
Chd|        CDK6COOR3                     source/elements/sh3n/coquedk6/cdk6coor3.F
Chd|        CDKCOOR3                      source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        CEVEC3                        source/elements/shell/coque/cevec3.F
Chd|        CNVEC3                        source/elements/shell/coque/cnvec3.F
Chd|        CZCOORK3                      source/elements/shell/coquez/czcoork3.F
Chd|        CZCORC1                       source/elements/shell/coquez/czcorc.F
Chd|        DYNAIN_C_STRSG                source/output/dynain/dynain_c_strsg.F
Chd|        GET_Q4LSYS                    source/output/sta/sta_c_get_q4lsys.F
Chd|        GET_T3LSYS                    source/output/sta/sta_c_get_t3lsys.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CORTDIR3(ELBUF_STR,DIR_A  ,DIR_B  ,JFT    ,JLT    ,
     .                    NLAY     ,IREP   ,RX     ,RY     ,RZ     ,
     .                    SX       ,SY     ,SZ     ,E1X    ,E1Y    ,
     .                    E1Z      ,E2X    ,E2Y    ,E2Z    ,NEL    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NLAY,IREP,NEL,IDRAPE,IT
C     REAL
      my_real
     .   RX(*),RY(*),RZ(*),SX(*),SY(*),SZ(*),
     .   DIR_A(*),DIR_B(*),E1X(*),E1Y(*),E1Z(*),E2X(*),E2Y(*),E2Z(*)
cc     .   DIR_A(*),DIR_B(*),E1X(*),E1Y(*),E1Z(*),E2X(*),E2Y(*),E2Z(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ILAW,NPTT,K,IAD
C     REAL
      my_real 
     .   V1,V2,V3,VR,VS,AA,BB,SUMA
      my_real, 
     .   DIMENSION(:) , POINTER :: DIR1, DIR2
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(L_BUFEL_DIR_) ,POINTER :: LBUF_DIR     
C=======================================================================
      IDRAPE = ELBUF_STR%IDRAPE
      IF(IDRAPE == 0) THEN
       IF (IREP == 1) THEN
        DO N=1,NLAY
          DIR1 => ELBUF_STR%BUFLY(N)%DIRA
          J = (N-1)*NEL*2
!    or    J = 2*(N-1)
          DO I=JFT,JLT
            AA = DIR1(I)                          
            BB = DIR1(I + NEL)                         
            V1 = AA*RX(I) + BB*SX(I)                
            V2 = AA*RY(I) + BB*SY(I)                
            V3 = AA*RZ(I) + BB*SZ(I)                
            VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
            VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
            SUMA=SQRT(VR*VR + VS*VS)
            DIR_A(J+I)     = VR/SUMA  		  
            DIR_A(J+I+NEL) = VS/SUMA     
!    or      DIR_A(I,J+1) = VR/SUMA                    
!            DIR_A(I,J+2) = VS/SUMA
          ENDDO                                     
        ENDDO                                       
      ELSEIF (IREP == 2) THEN                       
        DO N=1,NLAY                                  
          DIR1 => ELBUF_STR%BUFLY(N)%DIRA
          DIR2 => ELBUF_STR%BUFLY(N)%DIRB
          J = (N-1)*NEL*2
!    or    J = 2*(N-1)
          DO I=JFT,JLT                              
C---        Axe I                                   
            AA = DIR1(I)                        
            BB = DIR1(I + NEL)                       
            V1 = AA*RX(I) + BB*SX(I)                
            V2 = AA*RY(I) + BB*SY(I)                
            V3 = AA*RZ(I) + BB*SZ(I)                
            VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
            VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
            SUMA = MAX( SQRT(VR*VR + VS*VS), EM20)  
            DIR_A(J+I)     = VR/SUMA		     
            DIR_A(J+I+NEL) = VS/SUMA		     
!    or      DIR_A(I,J+1) = VR/SUMA                    
!            DIR_A(I,J+2) = VS/SUMA
C---        Axe II                                  
            AA = DIR2(I)                         
            BB = DIR2(I + NEL)                         
            V1 = AA*RX(I) + BB*SX(I)                
            V2 = AA*RY(I) + BB*SY(I)                
            V3 = AA*RZ(I) + BB*SZ(I)                
            VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
            VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
            SUMA = MAX( SQRT(VR*VR + VS*VS), EM20)  
            DIR_B(J+I)     = VR/SUMA		     
            DIR_B(J+I+NEL) = VS/SUMA		     
!    or      DIR_B(I,J+1) = VR/SUMA                    
!            DIR_B(I,J+2) = VS/SUMA
          ENDDO                                     
        ENDDO                                       
      ELSEIF (IREP == 3) THEN
C   mixing law58 with other user laws with IREP = 0 within PID51
        DO N=1,NLAY
          ILAW = ELBUF_STR%BUFLY(N)%ILAW
          IF (ILAW == 58) THEN
            DIR1 => ELBUF_STR%BUFLY(N)%DIRA
            DIR2 => ELBUF_STR%BUFLY(N)%DIRB
            J = (N-1)*NEL*2
!    or      J = 2*(N-1)
            DO I=JFT,JLT                              
C---        Axe I                                   
              AA = DIR1(I)                        
              BB = DIR1(I + NEL)                       
              V1 = AA*RX(I) + BB*SX(I)                
              V2 = AA*RY(I) + BB*SY(I)                
              V3 = AA*RZ(I) + BB*SZ(I)                
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
              SUMA = MAX( SQRT(VR*VR + VS*VS), EM20)  
              DIR_A(J+I)     = VR/SUMA		       
              DIR_A(J+I+NEL) = VS/SUMA		       
!     or       DIR_A(I,J+1) = VR/SUMA                    
!              DIR_A(I,J+2) = VS/SUMA
C---        Axe II                                  
              AA = DIR2(I)                         
              BB = DIR2(I + NEL)                         
              V1 = AA*RX(I) + BB*SX(I)                
              V2 = AA*RY(I) + BB*SY(I)                
              V3 = AA*RZ(I) + BB*SZ(I)                
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
              SUMA = MAX( SQRT(VR*VR + VS*VS), EM20)  
              DIR_B(J+I)     = VR/SUMA		       
              DIR_B(J+I+NEL) = VS/SUMA		       
!    or        DIR_B(I,J+1) = VR/SUMA                    
!              DIR_B(I,J+2) = VS/SUMA
            ENDDO
          ELSE  ! IREP = 0 within PID51
            DIR1 => ELBUF_STR%BUFLY(N)%DIRA
!            J = 2*(N-1)
            J = (N-1)*NEL*2
            DO I=JFT,JLT
              DIR_A(J+I)     = DIR1(I)
              DIR_A(J+I+NEL) = DIR1(I+NEL)
!    or        DIR_A(I,J+1) = VR/SUMA                    
!              DIR_A(I,J+2) = VS/SUMA
            ENDDO
          ENDIF
        ENDDO
      ELSEIF (IREP == 4) THEN
C   mixing law58 with other user laws with IREP = 1 within PID51
        DO N=1,NLAY
          ILAW = ELBUF_STR%BUFLY(N)%ILAW
          IF (ILAW == 58) THEN
            DIR1 => ELBUF_STR%BUFLY(N)%DIRA
            DIR2 => ELBUF_STR%BUFLY(N)%DIRB
            J = (N-1)*NEL*2
!    or      J = 2*(N-1)
            DO I=JFT,JLT                              
C---        Axe I                                   
              AA = DIR1(I)                        
              BB = DIR1(I + NEL)                       
              V1 = AA*RX(I) + BB*SX(I)                
              V2 = AA*RY(I) + BB*SY(I)                
              V3 = AA*RZ(I) + BB*SZ(I)                
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
              SUMA = MAX( SQRT(VR*VR + VS*VS), EM20)  
              DIR_A(J+I)     = VR/SUMA		       
              DIR_A(J+I+NEL) = VS/SUMA		       
!    or        DIR_A(I,J+1) = VR/SUMA                    
!              DIR_A(I,J+2) = VS/SUMA
C---        Axe II                                  
              AA = DIR2(I)                         
              BB = DIR2(I + NEL)                         
              V1 = AA*RX(I) + BB*SX(I)                
              V2 = AA*RY(I) + BB*SY(I)                
              V3 = AA*RZ(I) + BB*SZ(I)                
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
              SUMA = MAX( SQRT(VR*VR + VS*VS), EM20)  
              DIR_B(J+I)     = VR/SUMA		       
              DIR_B(J+I+NEL) = VS/SUMA		       
!     or       DIR_B(I,J+1) = VR/SUMA                    
!              DIR_B(I,J+2) = VS/SUMA
            ENDDO
          ELSE  ! IREP = 1 within PID51
            DIR1 => ELBUF_STR%BUFLY(N)%DIRA
            J = (N-1)*NEL*2
!    or      J = 2*(N-1)
            DO I=JFT,JLT
              AA = DIR1(I)                          
              BB = DIR1(I + NEL)                         
              V1 = AA*RX(I) + BB*SX(I)                
              V2 = AA*RY(I) + BB*SY(I)                
              V3 = AA*RZ(I) + BB*SZ(I)                
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
              SUMA=SQRT(VR*VR + VS*VS)
              DIR_A(J+I)     = VR/SUMA		       
              DIR_A(J+I+NEL) = VS/SUMA	
!    or        DIR_A(I,J+1) = VR/SUMA                    
!              DIR_A(I,J+2) = VS/SUMA
            ENDDO
          ENDIF
        ENDDO
       ENDIF
      ELSE ! idrape
       IF (IREP == 1) THEN
       IAD = 0
        DO N=1,NLAY
          NPTT = ELBUF_STR%BUFLY(N)%NPTT
          DO IT = 1,NPTT
            LBUF_DIR => ELBUF_STR%BUFLY(N)%LBUF_DIR(IT) 
            DIR1 => LBUF_DIR%DIRA
            J = IAD + (IT-1)*NEL*2
!    or    J = 2*(N-1)
           DO I=JFT,JLT
            AA = DIR1(I)                          
            BB = DIR1(I + NEL)                         
            V1 = AA*RX(I) + BB*SX(I)                
            V2 = AA*RY(I) + BB*SY(I)                
            V3 = AA*RZ(I) + BB*SZ(I)                
            VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
            VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
            SUMA=SQRT(VR*VR + VS*VS)
            DIR_A(J+I)     = VR/SUMA  		  
            DIR_A(J+I+NEL) = VS/SUMA     
!    or      DIR_A(I,J+1) = VR/SUMA                    
!            DIR_A(I,J+2) = VS/SUMA
          ENDDO                                     
         ENDDO  ! NPTT
         IAD = IAD + NPTT*NEL*2
        ENDDO ! NLAY                                     
      ELSEIF (IREP == 2) THEN  
        IAD = 0                     
        DO N=1,NLAY 
          NPTT = ELBUF_STR%BUFLY(N)%NPTT
          DO IT = 1,NPTT
            LBUF_DIR => ELBUF_STR%BUFLY(N)%LBUF_DIR(IT) 
            DIR1 => LBUF_DIR%DIRA
            DIR2 => LBUF_DIR%DIRB
            J =  IAD + (IT-1)*NEL*2
!    or    J = 2*(N-1)
            DO I=JFT,JLT                              
C---        Axe I                                   
             AA = DIR1(I)                        
             BB = DIR1(I + NEL)                       
             V1 = AA*RX(I) + BB*SX(I)                
             V2 = AA*RY(I) + BB*SY(I)                
             V3 = AA*RZ(I) + BB*SZ(I)                
             VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
             VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
             SUMA = MAX( SQRT(VR*VR + VS*VS), EM20)  
             DIR_A(J+I)     = VR/SUMA		     
             DIR_A(J+I+NEL) = VS/SUMA		     
!    or      DIR_A(I,J+1) = VR/SUMA                    
!            DIR_A(I,J+2) = VS/SUMA
C---        Axe II                                  
             AA = DIR2(I)                         
             BB = DIR2(I + NEL)                         
             V1 = AA*RX(I) + BB*SX(I)                
             V2 = AA*RY(I) + BB*SY(I)                
             V3 = AA*RZ(I) + BB*SZ(I)                
             VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
             VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
             SUMA = MAX( SQRT(VR*VR + VS*VS), EM20)  
             DIR_B(J+I)     = VR/SUMA		     
             DIR_B(J+I+NEL) = VS/SUMA		     
!    or      DIR_B(I,J+1) = VR/SUMA                    
!            DIR_B(I,J+2) = VS/SUMA
            ENDDO
          ENDDO ! NPTT  
            IAD = IAD + NPTT*NEL*2                                     
        ENDDO ! NLAY                                      
      ELSEIF (IREP == 3) THEN
C   mixing law58 with other user laws with IREP = 0 within PID51
        IAD = 0
        DO N=1,NLAY
          ILAW = ELBUF_STR%BUFLY(N)%ILAW
          IF (ILAW == 58) THEN
           NPTT = ELBUF_STR%BUFLY(N)%NPTT
           DO IT = 1,NPTT
            LBUF_DIR => ELBUF_STR%BUFLY(N)%LBUF_DIR(IT) 
            DIR1 => LBUF_DIR%DIRA
            DIR2 => LBUF_DIR%DIRB
            J = IAD + (IT-1)*NEL*2
!    or      J = 2*(N-1)
            DO I=JFT,JLT                              
C---        Axe I                                   
              AA = DIR1(I)                        
              BB = DIR1(I + NEL)                       
              V1 = AA*RX(I) + BB*SX(I)                
              V2 = AA*RY(I) + BB*SY(I)                
              V3 = AA*RZ(I) + BB*SZ(I)                
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
              SUMA = MAX( SQRT(VR*VR + VS*VS), EM20)  
              DIR_A(J+I)     = VR/SUMA		       
              DIR_A(J+I+NEL) = VS/SUMA		       
!     or       DIR_A(I,J+1) = VR/SUMA                    
!              DIR_A(I,J+2) = VS/SUMA
C---        Axe II                                  
              AA = DIR2(I)                         
              BB = DIR2(I + NEL)                         
              V1 = AA*RX(I) + BB*SX(I)                
              V2 = AA*RY(I) + BB*SY(I)                
              V3 = AA*RZ(I) + BB*SZ(I)                
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
              SUMA = MAX( SQRT(VR*VR + VS*VS), EM20)  
              DIR_B(J+I)     = VR/SUMA		       
              DIR_B(J+I+NEL) = VS/SUMA		       
!    or        DIR_B(I,J+1) = VR/SUMA                    
!              DIR_B(I,J+2) = VS/SUMA
            ENDDO
           ENDDO ! NPTT
           IAD = IAD + NPTT*2*NEL 
          ELSE  ! IREP = 0 within PID51
          NPTT = ELBUF_STR%BUFLY(N)%NPTT
          DO IT = 1,NPTT
            LBUF_DIR => ELBUF_STR%BUFLY(N)%LBUF_DIR(IT) 
            DIR1 => LBUF_DIR%DIRA
!            J = 2*(N-1)
            J = IAD + (IT-1)*NEL*2
            DO I=JFT,JLT
              DIR_A(J+I)     = DIR1(I)
              DIR_A(J+I+NEL) = DIR1(I+NEL)
!    or        DIR_A(I,J+1) = VR/SUMA                    
!              DIR_A(I,J+2) = VS/SUMA
            ENDDO
           ENDDO ! NPTT
           IAD = IAD + 2*NEL*NPTT 
          ENDIF
        ENDDO ! NALY
      ELSEIF (IREP == 4) THEN
C   mixing law58 with other user laws with IREP = 1 within PID51
        IAD = 0
        DO N=1,NLAY
          ILAW = ELBUF_STR%BUFLY(N)%ILAW
          IF (ILAW == 58) THEN
           NPTT = ELBUF_STR%BUFLY(N)%NPTT
           DO IT = 1,NPTT
            LBUF_DIR => ELBUF_STR%BUFLY(N)%LBUF_DIR(IT) 
            DIR1 => LBUF_DIR%DIRA
            DIR2 => LBUF_DIR%DIRB
            J = IAD + (IT - 1)*NEL*2
!    or      J = 2*(N-1)
            DO I=JFT,JLT                              
C---        Axe I                                   
              AA = DIR1(I)                        
              BB = DIR1(I + NEL)                       
              V1 = AA*RX(I) + BB*SX(I)                
              V2 = AA*RY(I) + BB*SY(I)                
              V3 = AA*RZ(I) + BB*SZ(I)                
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
              SUMA = MAX( SQRT(VR*VR + VS*VS), EM20)  
              DIR_A(J+I)     = VR/SUMA		       
              DIR_A(J+I+NEL) = VS/SUMA		       
!    or        DIR_A(I,J+1) = VR/SUMA                    
!              DIR_A(I,J+2) = VS/SUMA
C---        Axe II                                  
              AA = DIR2(I)                         
              BB = DIR2(I + NEL)                         
              V1 = AA*RX(I) + BB*SX(I)                
              V2 = AA*RY(I) + BB*SY(I)                
              V3 = AA*RZ(I) + BB*SZ(I)                
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
              SUMA = MAX( SQRT(VR*VR + VS*VS), EM20)  
              DIR_B(J+I)     = VR/SUMA		       
              DIR_B(J+I+NEL) = VS/SUMA		       
!     or       DIR_B(I,J+1) = VR/SUMA                    
!              DIR_B(I,J+2) = VS/SUMA
            ENDDO
           ENDDO ! NPTT
           IAD = IAD + NPTT*2*NEL
          ELSE  ! IREP = 1 within PID51
           NPTT = ELBUF_STR%BUFLY(N)%NPTT
           DO IT = 1,NPTT
            LBUF_DIR => ELBUF_STR%BUFLY(N)%LBUF_DIR(IT) 
            DIR1 => LBUF_DIR%DIRA
            J = IAD + (IT-1)*NEL*2
!    or      J = 2*(N-1)
             DO I=JFT,JLT
              AA = DIR1(I)                          
              BB = DIR1(I + NEL)                         
              V1 = AA*RX(I) + BB*SX(I)                
              V2 = AA*RY(I) + BB*SY(I)                
              V3 = AA*RZ(I) + BB*SZ(I)                
              VR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)    
              VS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)    
              SUMA=SQRT(VR*VR + VS*VS)
              DIR_A(J+I)     = VR/SUMA		       
              DIR_A(J+I+NEL) = VS/SUMA	
!    or        DIR_A(I,J+1) = VR/SUMA                    
!              DIR_A(I,J+2) = VS/SUMA
            ENDDO
           ENDDO ! NPTT
           IAD = IAD + 2*NPTT*NEL 
          ENDIF
        ENDDO ! NLAY
      ENDIF      
      
      
      ENDIF ! IDRAPE 
C----------------------------
      RETURN
      END
