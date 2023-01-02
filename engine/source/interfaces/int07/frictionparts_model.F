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
Chd|====================================================================
Chd|  FRICTIONPARTS_MODEL_ORTHO     source/interfaces/int07/frictionparts_model.F
Chd|-- called by -----------
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE FRICTIONPARTS_MODEL_ORTHO(
     1         INTFRIC     , JLT       ,IPARTFRICSI  ,IPARTFRICMI  ,ADPARTS_FRIC  ,
     2         NSET        , TABCOUPLEPARTS_FRIC,NPARTFRIC ,TABPARTS_FRIC ,TABCOEF_FRIC    ,             
     3         FRIC        , VISCF      , FROT_P     ,FRIC_COEFS   ,  FRICC     ,
     4         VISCFFRIC   , NTY        , MFROT      ,IORTHFRIC    , FRIC_COEFS2,
     5         FRICC2      ,VISCFFRIC2  , IFRICORTH  ,NFORTH       , NFISOT      , 
     6         INDEXORTH     ,INDEXISOT   ,JLT_TIED  ,IREP_FRICMI   , DIR_FRICMI   , 
     7         IX3         ,IX4           ,X1        ,Y1           , Z1          ,
     8         X2          ,Y2            ,Z2        ,X3           ,Y3           ,
     9         Z3          ,X4            ,Y4        ,Z4           ,CE_LOC       ,
     A         DIR1        ,DIR2          )
                          
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INTFRIC,JLT,NFRIC_P ,NSET ,NPARTFRIC ,NTY ,MFROT ,IORTHFRIC ,NFORTH ,
     .        NFISOT ,JLT_TIED
      INTEGER IPARTFRICSI(MVSIZ), IPARTFRICMI(MVSIZ), ADPARTS_FRIC(NPARTFRIC+1),
     .        TABCOUPLEPARTS_FRIC(NSET ),TABPARTS_FRIC(NPARTFRIC) ,INDEXORTH(MVSIZ),
     .        INDEXISOT(MVSIZ),IFRICORTH(NSET),IREP_FRICMI(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),
     .        CE_LOC(MVSIZ)

      my_real
     .   VISCF ,FRIC ,
     .   FRIC_COEFS(MVSIZ,10),TABCOEF_FRIC (*),FRICC(MVSIZ), VISCFFRIC(MVSIZ),
     .   FROT_P(10),FRICC2(MVSIZ), VISCFFRIC2(MVSIZ),FRIC_COEFS2(MVSIZ,10),DIR1(MVSIZ,3),
     .   DIR2(MVSIZ,3),DIR_FRICMI(MVSIZ,2),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,K ,IPS ,IPM ,IP ,IPMID ,ADRI ,ADRF ,ADRCOEF ,IPSL ,IPML ,IPI ,IPF , 
     .       L , IREP ,IORTH,NI,NN ,LENC
      my_real ADR ,E1X ,E1Y ,E1Z ,E2X ,E2Y ,E2Z ,E3X ,E3Y ,E3Z ,RX ,RY ,RZ ,SX ,SY ,SZ ,
     .        SUMA ,S1 ,S2 ,AA ,BB ,V1 ,V2 ,V3 ,VR ,VS  
C
C-----------------------------------------------
      NFORTH = 0
      NFISOT = 0

C--------Default coefficients define in friction interface
      DO I=1,JLT-JLT_TIED
         FRICC(I) = TABCOEF_FRIC(1)
         VISCFFRIC(I) = TABCOEF_FRIC(2)
         FRICC2(I) = ZERO
         VISCFFRIC2(I) = ZERO
      ENDDO
      IF(MFROT ==0 ) THEN   
        LENC =2  
      ELSE
        LENC = 8
      ENDIF 
      IF(MFROT/=0) THEN
         DO I=1,JLT-JLT_TIED
            DO J=1,6
               FRIC_COEFS(I,J) = TABCOEF_FRIC(J+2)
               FRIC_COEFS2(I,J) = ZERO
            ENDDO
         ENDDO
      ENDIF  

      IF(NTY == 24.OR.NTY == 25) THEN
         DO I=1,JLT-JLT_TIED
           VISCFFRIC(I) = ZERO
         ENDDO
      ENDIF
C
      DO I=1,JLT-JLT_TIED
         IPSL = IPARTFRICSI(I)  
         IPML = IPARTFRICMI(I)
c
         IF(IPSL /= 0) THEN
            IPS = TABPARTS_FRIC(IPSL)  ! PART of secnd node
         ELSE
            IPS = 0
         ENDIF
         IF(IPML /= 0) THEN
            IPM = TABPARTS_FRIC(IPML)  ! PART of main node
         ELSE
            IPM = 0
         ENDIF
c
         IF(IPS/=0.AND.IPM/=0) THEN
           IF(IPSL > IPML ) THEN
              IP = IPS
              IPS = IPM
              IPM = IP
c
              IP = IPSL
              IPSL = IPML
              IPML = IP              
            ENDIF

            ADRI = ADPARTS_FRIC(IPSL)     !Adress of First local part associated to IP
c
            IF (ADRI /= 0) THEN
               ADRF = ADPARTS_FRIC(IPSL+1)-1 !Adress of Last part associated to IPS
C
C--------Looking for Adress of parts couple (IPS,IPM) in TABPARTSFRIC
               ADRCOEF = 0
C
               IF(ADRI == ADRF ) THEN
                  IPI = TABCOUPLEPARTS_FRIC(ADRI) 
                  IF(IPI == IPM) THEN
                    ADRCOEF = ADRI 
                  ELSE
                    ADRCOEF = 0
                  ENDIF                
               ELSE
                  DO WHILE ((ADRF-ADRI) >= 1)
                    ADR = (ADRF-ADRI)*HALF
                    K = ADRI + NINT(ADR)
                    IPMID = TABCOUPLEPARTS_FRIC(K) 
                    IPF = TABCOUPLEPARTS_FRIC(ADRF) 
                    IPI = TABCOUPLEPARTS_FRIC(ADRI) 
                    IF(IPMID == IPM) THEN
                      ADRCOEF = K 
                      EXIT
                    ELSEIF(IPI == IPM) THEN
                      ADRCOEF = ADRI 
                      EXIT
                    ELSEIF(IPF == IPM) THEN
                      ADRCOEF = ADRF 
                      EXIT
                    ELSEIF (IPMID < IPM) THEN
                      ADRI = K + 1
                    ELSEIF (IPMID > IPM) THEN
                      ADRF = K - 1    
                    ENDIF  
                  ENDDO
               ENDIF

C-----Selecting corresponding friction coefs ------------------
               IF(ADRCOEF /= 0) THEN
c
                  IORTH  = IFRICORTH(ADRCOEF)
                  L= CE_LOC(I)
                  IREP = IREP_FRICMI(I)  
c
                  IF(IORTH > 0 .AND.IREP /=10 ) THEN
                     NFORTH = NFORTH +1 
                     INDEXORTH(NFORTH) = I 
c                    
                     FRICC(I) = TABCOEF_FRIC(LENC+2*LENC*(ADRCOEF-1)+1)
                     FRICC2(I)= TABCOEF_FRIC(2*LENC*ADRCOEF+1)
                     IF(NTY == 24.OR.NTY == 25 ) THEN
                        VISCFFRIC(I) = ZERO
                        VISCFFRIC2(I) = ZERO
                     ELSE
                        VISCFFRIC(I) = TABCOEF_FRIC(LENC+2*LENC*(ADRCOEF-1)+2)
                        VISCFFRIC2(I) = TABCOEF_FRIC(2*LENC*ADRCOEF+2)
                     ENDIF
                     IF (MFROT > 0) THEN    
                       DO J=1,6
                          FRIC_COEFS(I,J) = TABCOEF_FRIC(LENC+2*LENC*ADRCOEF+J+2)
                          FRIC_COEFS2(I,J)  = TABCOEF_FRIC(2*LENC*ADRCOEF+J+2)
                        ENDDO
                     ENDIF
                  ELSE
                     NFISOT = NFISOT +1 
                     INDEXISOT(NFISOT) = I 
C
                     FRICC(I) = TABCOEF_FRIC(LENC+2*LENC*(ADRCOEF-1)+1)
                     IF(NTY == 24.OR.NTY == 25 ) THEN
                        VISCFFRIC(I) = ZERO
                     ELSE
                        VISCFFRIC(I) = TABCOEF_FRIC(LENC+2*LENC*(ADRCOEF-1)+2)
                     ENDIF
                     IF (MFROT > 0) THEN    
                        DO J=1,6
                           FRIC_COEFS(I,J) = TABCOEF_FRIC(2*LENC*ADRCOEF+J+2)
                        ENDDO
                     ENDIF
                   ENDIF
                ELSE ! ADRCOEF
                   NFISOT = NFISOT +1 
                   INDEXISOT(NFISOT) = I 
                ENDIF
c

             ELSE ! ADRI ==0

                NFISOT = NFISOT +1 
                INDEXISOT(NFISOT) = I 
             ENDIF

         ELSE

             NFISOT = NFISOT +1 
             INDEXISOT(NFISOT) = I 
         ENDIF
      
        ENDDO

C----Orthotropic Friction : global orthotropic axes computation
        IF(NFORTH > 0 ) THEN
           DO K=1,NFORTH
               I = INDEXORTH(K ) 
               L= CE_LOC(I)      
C---    isoparametric (material) axes
               IF (IX3(I) /= IX4(I)) THEN
C---      shell 4N
                 E1X= X2(I) + X3(I) - X1(I) - X4(I) 
                 E1Y= Y2(I) + Y3(I) - Y1(I) - Y4(I) 
                 E1Z= Z2(I) + Z3(I) - Z1(I) - Z4(I) 
                 E2X= X3(I) + X4(I) - X1(I) - X2(I) 
                 E2Y= Y3(I) + Y4(I) - Y1(I) - Y2(I) 
                 E2Z= Z3(I) + Z4(I) - Z1(I) - Z2(I) 
      
               ELSE
C---      shell 3N
                 E1X= X2(I) - X1(I) 
                 E1Y= Y2(I) - Y1(I) 
                 E1Z= Z2(I) - Z1(I) 
                 E2X= X3(I) - X1(I) 
                 E2Y= Y3(I) - Y1(I) 
                 E2Z= Z3(I) - Z1(I) 
               ENDIF
               RX = E1X
               RY = E1Y
               RZ = E1Z
               SX = E2X
               SY = E2Y
               SZ = E2Z
c
               E3X = E1Y*E2Z-E1Z*E2Y
               E3Y = E1Z*E2X-E1X*E2Z
               E3Z = E1X*E2Y-E1Y*E2X  
  
               SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z  
               SUMA   = ONE/MAX(SQRT(SUMA),EM20)                    
               E3X = E3X*SUMA                              
               E3Y = E3Y*SUMA                              
               E3Z = E3Z*SUMA                              
C
               S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z
               S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z
               SUMA   = SQRT(S1/S2)                
               E1X    = E1X  + (E2Y *E3Z-E2Z*E3Y)*SUMA
               E1Y    = E1Y  + (E2Z *E3X-E2X*E3Z)*SUMA
               E1Z    = E1Z  + (E2X *E3Y-E2Y*E3X)*SUMA
C
               SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z  
               SUMA   = ONE/MAX(SQRT(SUMA),EM20)                    
               E1X    = E1X*SUMA                              
               E1Y    = E1Y*SUMA                              
               E1Z    = E1Z*SUMA                              
C
               E2X    = E3Y * E1Z - E3Z * E1Y
               E2Y    = E3Z * E1X - E3X * E1Z
               E2Z    = E3X * E1Y - E3Y * E1X

C---    Projection of orthotropic axes on global system
               AA = DIR_FRICMI(I,1)
               BB = DIR_FRICMI(I,2)   
               IREP = IREP_FRICMI(I) 

               IF(IREP == 1) THEN
                 V1 = AA*RX + BB*SX         
                 V2 = AA*RY + BB*SY         
                 V3 = AA*RZ + BB*SZ         
                 VR = V1*E1X+ V2*E1Y + V3*E1Z
                 VS = V1*E2X+ V2*E2Y + V3*E2Z
                 SUMA=MAX(SQRT(VR*VR + VS*VS) , EM20)                
                 AA = VR/SUMA                                                   
                 BB = VS/SUMA
               ENDIF

               DIR1(I,1) = AA*E1X+BB*E2X
               DIR1(I,2) = AA*E1Y+BB*E2Y
               DIR1(I,3) = AA*E1Z+BB*E2Z

               DIR2(I,1) = AA*E2X-BB*E1X
               DIR2(I,2) = AA*E2Y-BB*E1Y
               DIR2(I,3) = AA*E2Z-BB*E1Z
                   
           ENDDO
          ENDIF     

C       
      RETURN
      END


Chd|====================================================================
Chd|  FRICTIONPARTS_MODEL_ISOT      source/interfaces/int07/frictionparts_model.F
Chd|-- called by -----------
Chd|        I11MAINF                      source/interfaces/int11/i11mainf.F
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FRICTIONPARTS_MODEL_ISOT(
     1         INTFRIC     , JLT       ,IPARTFRICSI  ,IPARTFRICMI  ,ADPARTS_FRIC  ,
     2         NSET        , TABCOUPLEPARTS_FRIC,NPARTFRIC ,TABPARTS_FRIC ,TABCOEF_FRIC    ,             
     3         FRIC        , VISCF      , FROT_P     ,FRIC_COEFS   ,FRICC       ,
     4         VISCFFRIC   , NTY        , MFROT      ,IORTHFRIC    ,IFRIC       ,
     5         JLT_TIED    , TINT       ,TEMPI       ,NPC          ,TF          ,
     6         TEMP        , H1         ,H2          ,H3           ,H4          ,
     7         IX1         , IX2        ,IX3         ,IX4          ,IFORM       )
                          
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INTFRIC,JLT,NFRIC_P ,NSET ,NPARTFRIC ,NTY ,MFROT ,IORTHFRIC ,IFRIC ,
     .        JLT_TIED ,IFORM
      INTEGER IPARTFRICSI(*), IPARTFRICMI(*), ADPARTS_FRIC(NPARTFRIC+1),
     .        TABCOUPLEPARTS_FRIC(NSET),TABPARTS_FRIC(NPARTFRIC) ,NPC(*) ,
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ)
      my_real
     .   VISCF ,FRIC , TINT, 
     .   FRIC_COEFS(MVSIZ,10),TABCOEF_FRIC (12*(NSET+1)),FRICC(*), VISCFFRIC(*),
     .   TF(*) , TEMP(*) ,TEMPI(MVSIZ) ,H1(MVSIZ) ,H2(MVSIZ) ,H3(MVSIZ) ,H4(MVSIZ) ,
     .   FROT_P(10)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,K ,IPS ,IPM ,IP ,IPMID ,ADRI ,ADRF ,ADRCOEF ,IPSL ,IPML ,IPI ,IPF ,
     .    LENC 
      my_real ADR  ,THI ,TM ,DYDX
      my_real 
     .   FINTER,TOTAL 
      EXTERNAL FINTER
C
C-----------------------------------------------
      IF(MFROT ==0 ) THEN   
        LENC =2  
      ELSE
        LENC = 8
      ENDIF

      IF (INTFRIC == 0) THEN

         DO I=1,JLT-JLT_TIED
           IF(MFROT/=0) FRIC_COEFS(I,1:10) = FROT_P(1:10)
           VISCFFRIC(I) = VISCF
           FRICC(I) = FRIC
         ENDDO 

         IF (IFRIC > 0) THEN ! Friction coef = f(Temp)
            IF( IFORM == 0) THEN 
              DO I=1,JLT-JLT_TIED
                 THI = (TEMPI(I)+TINT)/2
                 FRICC(I)  =  FRICC(I)*FINTER(IFRIC,THI,NPC,TF,DYDX)   
              ENDDO
            ELSE
              DO I=1,JLT-JLT_TIED
                 TM = H1(I)*TEMP(IX1(I)) + H2(I)*TEMP(IX2(I)) 
     .              + H3(I)*TEMP(IX3(I)) + H4(I)*TEMP(IX4(I))
                 THI = (TEMPI(I)+TM)/2
                 FRICC(I)  =  FRICC(I)*FINTER(IFRIC,THI,NPC,TF,DYDX)  
               ENDDO
           ENDIF

         ENDIF


      ELSE
C--------Default coefficients define in friction interface
        DO I=1,JLT-JLT_TIED
           FRICC(I) = TABCOEF_FRIC(1)
           VISCFFRIC(I) = TABCOEF_FRIC(2)
           IF(MFROT/=0) THEN
             DO J=1,6
                FRIC_COEFS(I,J) = TABCOEF_FRIC(J+2)
             ENDDO
           ENDIF
        ENDDO
        IF(NTY == 24.OR.NTY == 25) THEN
          DO I=1,JLT-JLT_TIED
           VISCFFRIC(I) = ZERO
          ENDDO
        ENDIF
C
        DO I=1,JLT-JLT_TIED
          IPSL = IPARTFRICSI(I)  
          IPML = IPARTFRICMI(I)
c
          IF(IPSL /= 0) THEN
             IPS = TABPARTS_FRIC(IPSL)  ! PART of secnd node
          ELSE
             IPS = 0
          ENDIF
          IF(IPML /= 0) THEN
             IPM = TABPARTS_FRIC(IPML)  ! PART of main node
          ELSE
             IPM = 0
          ENDIF
c
          IF(IPS/=0.AND.IPM/=0) THEN
            IF(IPSL > IPML ) THEN
               IP = IPS
               IPS = IPM
               IPM = IP
c
               IP = IPSL
               IPSL = IPML
               IPML = IP              
             ENDIF

             ADRI = ADPARTS_FRIC(IPSL)     !Adress of First local part associated to IPS
c
             IF (ADRI /= 0) THEN
               ADRF = ADPARTS_FRIC(IPSL+1)-1 !Adress of Last part associated to IPS
C
C--------Looking for Adress of parts couple (IPS,IPM) in TABPARTSFRIC
               ADRCOEF = 0
C
               IF(ADRI == ADRF ) THEN
                  IPI = TABCOUPLEPARTS_FRIC(ADRI) 
                  IF(IPI == IPM) THEN
                    ADRCOEF = ADRI 
                  ELSE
                    ADRCOEF = 0
                  ENDIF                   
               ELSE
                  DO WHILE ((ADRF-ADRI) >= 1)
                    ADR = (ADRF-ADRI)*HALF
                    K = ADRI + NINT(ADR)
                    IPMID = TABCOUPLEPARTS_FRIC(K) 
                    IPF = TABCOUPLEPARTS_FRIC(ADRF) 
                    IPI = TABCOUPLEPARTS_FRIC(ADRI) 
                    IF(IPMID == IPM) THEN
                      ADRCOEF = K 
                      EXIT
                    ELSEIF(IPI == IPM) THEN
                      ADRCOEF = ADRI 
                      EXIT
                    ELSEIF(IPF == IPM) THEN
                      ADRCOEF = ADRF 
                      EXIT
                    ELSEIF (IPMID < IPM) THEN
                      ADRI = K + 1
                    ELSEIF (IPMID > IPM) THEN
                      ADRF = K - 1    
                    ENDIF  
                  ENDDO
               ENDIF

C-----Selecting corresponding friction coefs ------------------

               IF(IORTHFRIC==0) THEN
                  IF(ADRCOEF /= 0) THEN
                    FRICC(I) = TABCOEF_FRIC(LENC*ADRCOEF+1)
                    VISCFFRIC(I) = TABCOEF_FRIC(LENC*ADRCOEF+2)
                    IF(MFROT > 0 ) THEN   
                       DO J=1,6
                          FRIC_COEFS(I,J) = TABCOEF_FRIC(LENC*ADRCOEF+J+2)
                       ENDDO
                    ENDIF
                  ENDIF
               ELSE
                  IF(ADRCOEF /= 0) THEN
                    FRICC(I) = TABCOEF_FRIC(LENC+2*LENC*(ADRCOEF-1)+1)
                    VISCFFRIC(I) = TABCOEF_FRIC(LENC+2*LENC*(ADRCOEF-1)+2) 
                    IF(MFROT > 0 ) THEN                        
                      DO J=1,6
                         FRIC_COEFS(I,J) = TABCOEF_FRIC(LENC+2*LENC*ADRCOEF+J+2)
                      ENDDO
                     ENDIF
                  ENDIF
               ENDIF
             ENDIF
           ENDIF
     
        ENDDO
      ENDIF
      

C       
      RETURN
      END



