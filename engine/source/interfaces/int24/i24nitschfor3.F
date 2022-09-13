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
Chd|  I24NITSCHFOR3                 source/interfaces/int24/i24nitschfor3.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
       SUBROUTINE  I24NITSCHFOR3 (IPARI ,INTBUF_TAB ,IPARIT ,STRESSMEAN,
     2                           INTLIST ,NBINTC    ,X      ,IADS      ,
     3                           FORNEQS ,FORNEQSKY,ITAB    ,IXS       ,
     4                           IADS10  ,IADS20  ,IADS16   ,NFACNIT   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARIT, NBINTC, NFACNIT
      INTEGER IPARI(NPARI,*) ,INTLIST(*) ,IADS(8,*) ,ITAB(*),IXS(NIXS,*),
     .          IADS10(6,*),IADS20(12,*),IADS16(8,*)
      my_real STRESSMEAN(6,*) ,X(3,*) ,FORNEQS(3,*) ,FORNEQSKY(3*NFACNIT,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER,ALLOCATABLE,DIMENSION(:) ::  ITAG
      INTEGER I ,J ,NI ,NIN ,NTY ,NSN ,NRTS ,SN ,IE ,NF ,N1 ,N2 ,N3 ,N4 ,
     .        N ,K1 ,K2 ,K3 ,K4 ,INTNITSCHE ,ADS1 ,ADS2 ,ADS3 ,ADS4 ,IE10,
     .        NS1 ,NS2 ,NS3 ,NS4
      my_real 
     .        SX1 ,SY1 ,SZ1 ,SX2 ,SY2 ,SZ2 ,SX3 ,SY3 ,
     .       SZ3 ,AREAS ,XNS ,YNS ,ZNS ,FOR ,FORX ,FORY ,FORZ,
     .       SIGNX ,SIGNY ,SIGNZ   
C-----------------------------------------------
C Equivalent nodal force computation using element mean stress:
C               mean of contribution of all surrounded elements 
C               FORNEQS = SUM(PHI*STRESSMEAN*Facet_Normal*Facet_Area)
C-----------------------------------------------
      ALLOCATE(ITAG(NUMNOD))
      ITAG(1:NUMNOD)=0
      IF(IPARIT==0)THEN
       DO NI=1,NBINTC
	 NIN = INTLIST(NI)
         NTY   = IPARI(7,NIN)
         NSN   = IPARI(5,NIN)
         NRTS  = IPARI(3,NIN)
         INTNITSCHE   = IPARI(86,NIN)
         IF(NTY==24 .AND.INTNITSCHE > 0) THEN
           DO I=1,NRTS
              IE = INTBUF_TAB(NIN)%IELNRTS(I)

              NS1 = INTBUF_TAB(NIN)%IRECTS(4*(I-1)+1)    
              NS2 = INTBUF_TAB(NIN)%IRECTS(4*(I-1)+2)                           
              NS3 = INTBUF_TAB(NIN)%IRECTS(4*(I-1)+3)                           
              NS4 = INTBUF_TAB(NIN)%IRECTS(4*(I-1)+4)  

              N1 = INTBUF_TAB(NIN)%NSV(NS1)                             
              N2 = INTBUF_TAB(NIN)%NSV(NS2)                             
              N3 = INTBUF_TAB(NIN)%NSV(NS3)                             
              N4 = INTBUF_TAB(NIN)%NSV(NS4)     

              ADS1 = INTBUF_TAB(NIN)%ADRECTS(4*(I-1)+1)
              ADS2 = INTBUF_TAB(NIN)%ADRECTS(4*(I-1)+2)
              ADS3 = INTBUF_TAB(NIN)%ADRECTS(4*(I-1)+3)
              ADS4 = INTBUF_TAB(NIN)%ADRECTS(4*(I-1)+4)

              IF(IE > 0) THEN

                IF(N4 /=N3) THEN
                   SX1 = X(1,N3) - X(1,N1)
                   SY1 = X(2,N3) - X(2,N1)
                   SZ1 = X(3,N3) - X(3,N1)
                   SX2 = X(1,N4) - X(1,N2)
                   SY2 = X(2,N4) - X(2,N2)
                   SZ2 = X(3,N4) - X(3,N2)
                   SX3  = SY1*SZ2 - SZ1*SY2
                   SY3  = SZ1*SX2 - SX1*SZ2
                   SZ3  = SX1*SY2 - SY1*SX2
c                  AREAS= HALF*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)

C
                   SIGNX  = STRESSMEAN(1,IE)*SX3 + STRESSMEAN(4,IE)*SY3 +STRESSMEAN(6,IE)*SZ3 
                   SIGNY  = STRESSMEAN(4,IE)*SX3 + STRESSMEAN(2,IE)*SY3 +STRESSMEAN(5,IE)*SZ3 
                   SIGNZ  = STRESSMEAN(6,IE)*SX3 + STRESSMEAN(5,IE)*SY3 +STRESSMEAN(3,IE)*SZ3 


C
               
                   FORX = ONE_OVER_16*SIGNX
                   FORY = ONE_OVER_16*SIGNY
                   FORZ = ONE_OVER_16*SIGNZ

C
                   IF(ITAG(N1)==0.AND.ADS1 < 10) THEN
                     FORNEQS(1,N1) = FORNEQS(1,N1) + FORX
                     FORNEQS(2,N1) = FORNEQS(2,N1) + FORY
                     FORNEQS(3,N1) = FORNEQS(3,N1) + FORZ
                   ELSEIF(ITAG(N1)==0) THEN
                     FORNEQS(1,N1) = FORNEQS(1,N1) + HALF*FORX
                     FORNEQS(2,N1) = FORNEQS(2,N1) + HALF*FORY
                     FORNEQS(3,N1) = FORNEQS(3,N1) + HALF*FORZ
                   ENDIF
                   IF(ITAG(N2)==0.AND.ADS2 < 10) THEN  
                     FORNEQS(1,N2) = FORNEQS(1,N2) + FORX
                     FORNEQS(2,N2) = FORNEQS(2,N2) + FORY
                     FORNEQS(3,N2) = FORNEQS(3,N2) + FORZ
                   ELSEIF(ITAG(N2)==0) THEN
                     FORNEQS(1,N2) = FORNEQS(1,N2) + HALF*FORX
                     FORNEQS(2,N2) = FORNEQS(2,N2) + HALF*FORY
                     FORNEQS(3,N2) = FORNEQS(3,N2) + HALF*FORZ
                   ENDIF  
                   IF(ITAG(N3)==0.AND.ADS3 < 10) THEN  
                     FORNEQS(1,N3) = FORNEQS(1,N3) + FORX
                     FORNEQS(2,N3) = FORNEQS(2,N3) + FORY
                     FORNEQS(3,N3) = FORNEQS(3,N3) + FORZ
                   ELSEIF(ITAG(N3)==0) THEN
                     FORNEQS(1,N3) = FORNEQS(1,N3) + HALF*FORX
                     FORNEQS(2,N3) = FORNEQS(2,N3) + HALF*FORY
                     FORNEQS(3,N3) = FORNEQS(3,N3) + HALF*FORZ
                   ENDIF 
                   IF (ITAG(N4)==0.AND.ADS1 < 10) THEN
                     FORNEQS(1,N4) = FORNEQS(1,N4) + FORX
                     FORNEQS(2,N4) = FORNEQS(2,N4) + FORY
                     FORNEQS(3,N4) = FORNEQS(3,N4) + FORZ
                   ELSEIF(ITAG(N4)==0) THEN
                     FORNEQS(1,N4) = FORNEQS(1,N4) + HALF*FORX
                     FORNEQS(2,N4) = FORNEQS(2,N4) + HALF*FORY
                     FORNEQS(3,N4) = FORNEQS(3,N4) + HALF*FORZ
                   ENDIF 

                ELSE

                   SX1 = X(1,N2) - X(1,N1)
                   SY1 = X(2,N2) - X(2,N1)
                   SZ1 = X(3,N2) - X(3,N1)
                   SX2 = X(1,N3) - X(1,N1)
                   SY2 = X(2,N3) - X(2,N1)
                   SZ2 = X(3,N3) - X(3,N1)
                   SX3  = SY1*SZ2 - SZ1*SY2
                   SY3  = SZ1*SX2 - SX1*SZ2
                   SZ3  = SX1*SY2 - SY1*SX2
c                  AREAS= SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
C
                   SIGNX  = STRESSMEAN(1,IE)*SX3 + STRESSMEAN(4,IE)*SY3 +STRESSMEAN(6,IE)*SZ3 
                   SIGNY  = STRESSMEAN(4,IE)*SX3 + STRESSMEAN(2,IE)*SY3 +STRESSMEAN(5,IE)*SZ3 
                   SIGNZ  = STRESSMEAN(6,IE)*SX3 + STRESSMEAN(5,IE)*SY3 +STRESSMEAN(3,IE)*SZ3 
C
               
                   FORX = ONE_OVER_8*SIGNX
                   FORY = ONE_OVER_8*SIGNY
                   FORZ = ONE_OVER_8*SIGNZ
C
                   IF(ITAG(N1)==0.AND.ADS1 < 10) THEN
                      FORNEQS(1,N1) = FORNEQS(1,N1) + FORX
                      FORNEQS(2,N1) = FORNEQS(2,N1) + FORY
                      FORNEQS(3,N1) = FORNEQS(3,N1) + FORZ
                  ELSEIF(ITAG(N1)==0) THEN
                      FORNEQS(1,N1) = FORNEQS(1,N1) + THIRD*FORX
                      FORNEQS(2,N1) = FORNEQS(2,N1) + THIRD*FORY
                      FORNEQS(3,N1) = FORNEQS(3,N1) + THIRD*FORZ
                   ENDIF 
                   IF(ITAG(N2)==0.AND.ADS2 < 10) THEN  
                     FORNEQS(1,N2) = FORNEQS(1,N2) + FORX
                     FORNEQS(2,N2) = FORNEQS(2,N2) + FORY
                     FORNEQS(3,N2) = FORNEQS(3,N2) + FORZ
                   ELSEIF(ITAG(N2)==0) THEN
                     FORNEQS(1,N2) = FORNEQS(1,N2) + THIRD*FORX
                     FORNEQS(2,N2) = FORNEQS(2,N2) + THIRD*FORY
                     FORNEQS(3,N2) = FORNEQS(3,N2) + THIRD*FORZ
                   ENDIF 
                   IF(ITAG(N3)==0.AND.ADS3 < 10) THEN  
                     FORNEQS(1,N3) = FORNEQS(1,N3) + FORX
                     FORNEQS(2,N3) = FORNEQS(2,N3) + FORY
                     FORNEQS(3,N3) = FORNEQS(3,N3) + FORZ
                   ELSEIF(ITAG(N3)==0) THEN
                     FORNEQS(1,N3) = FORNEQS(1,N3) + THIRD*FORX
                     FORNEQS(2,N3) = FORNEQS(2,N3) + THIRD*FORY
                     FORNEQS(3,N3) = FORNEQS(3,N3) + THIRD*FORZ
                   ENDIF 
               ENDIF

              ENDIF

            ENDDO
            DO N=1,NSN
                SN = INTBUF_TAB(NIN)%NSV(N)
                ITAG(SN) = 1
            ENDDO
          ENDIF
         ENDDO

      ELSE !IPARIT

       DO NI=1,NBINTC
	 NIN = INTLIST(NI)
         NTY   = IPARI(7,NIN)
         NSN   = IPARI(5,NIN)
         NRTS  = IPARI(3,NIN)
         INTNITSCHE   = IPARI(86,NIN)
         IF(NTY==24 .AND.INTNITSCHE > 0) THEN
           DO I=1,NRTS
              IE = INTBUF_TAB(NIN)%IELNRTS(I)
              NF = INTBUF_TAB(NIN)%FACNRTS(I)
              NS1 = INTBUF_TAB(NIN)%IRECTS(4*(I-1)+1)    
              NS2 = INTBUF_TAB(NIN)%IRECTS(4*(I-1)+2)                           
              NS3 = INTBUF_TAB(NIN)%IRECTS(4*(I-1)+3)                           
              NS4 = INTBUF_TAB(NIN)%IRECTS(4*(I-1)+4) 

              N1 = INTBUF_TAB(NIN)%NSV(NS1)    
              N2 = INTBUF_TAB(NIN)%NSV(NS2)                             
              N3 = INTBUF_TAB(NIN)%NSV(NS3)                             
              N4 = INTBUF_TAB(NIN)%NSV(NS4)  

              ADS1 = INTBUF_TAB(NIN)%ADRECTS(4*(I-1)+1)
              ADS2 = INTBUF_TAB(NIN)%ADRECTS(4*(I-1)+2)
              ADS3 = INTBUF_TAB(NIN)%ADRECTS(4*(I-1)+3)
              ADS4 = INTBUF_TAB(NIN)%ADRECTS(4*(I-1)+4)
   

              IF(IE > 0) THEN
                
                IF(N4 /=N3) THEN
                   SX1 = X(1,N3) - X(1,N1)
                   SY1 = X(2,N3) - X(2,N1)
                   SZ1 = X(3,N3) - X(3,N1)
                   SX2 = X(1,N4) - X(1,N2)
                   SY2 = X(2,N4) - X(2,N2)
                   SZ2 = X(3,N4) - X(3,N2)
                   SX3  = SY1*SZ2 - SZ1*SY2
                   SY3  = SZ1*SX2 - SX1*SZ2
                   SZ3  = SX1*SY2 - SY1*SX2
c                  AREAS= SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
C
                   SIGNX  = STRESSMEAN(1,IE)*SX3 + STRESSMEAN(4,IE)*SY3 +STRESSMEAN(6,IE)*SZ3 
                   SIGNY  = STRESSMEAN(4,IE)*SX3 + STRESSMEAN(2,IE)*SY3 +STRESSMEAN(5,IE)*SZ3 
                   SIGNZ  = STRESSMEAN(6,IE)*SX3 + STRESSMEAN(5,IE)*SY3 +STRESSMEAN(3,IE)*SZ3 
C
               
                   FORX = ONE_OVER_16*SIGNX
                   FORY = ONE_OVER_16*SIGNY
                   FORZ = ONE_OVER_16*SIGNZ
C

                   IF(ADS1 < 10) THEN
                      K1 = IADS(ADS1,IE)
                   ELSEIF(ADS1 < 40) THEN
                      K1 = IADS20(ADS1-20,IE)
                   ELSEIF(ADS1 < 50) THEN
                      K1 = IADS16(ADS1-40,IE)
                   ENDIF

                   IF(ADS2 < 10) THEN
                     K2 = IADS(ADS2,IE)
                   ELSEIF(ADS1 < 40) THEN
                     K2 = IADS20(ADS2-20,IE)
                   ELSEIF(ADS1 < 50) THEN
                     K2 = IADS16(ADS2-40,IE)
                   ENDIF

                   IF(ADS3 < 10) THEN
                     K3 = IADS(ADS3,IE)
                   ELSEIF(ADS3 < 40) THEN
                     K3 = IADS20(ADS3-20,IE)
                   ELSEIF(ADS3 < 50) THEN
                     K3 = IADS16(ADS3-40,IE)
                   ENDIF

                   IF(ADS4 < 10) THEN
                      K4 = IADS(ADS4,IE)
                   ELSEIF(ADS3 < 40) THEN
                      K4 = IADS20(ADS4-20,IE)
                   ELSEIF(ADS1 < 50) THEN
                      K4 = IADS16(ADS4-40,IE)
                   ENDIF


                   IF(ADS1 < 10) THEN
                     FORNEQSKY(3*(NF-1)+1,K1) = FORX
                     FORNEQSKY(3*(NF-1)+2,K1) = FORY
                     FORNEQSKY(3*(NF-1)+3,K1) = FORZ
                   ELSE
                     FORNEQSKY(3*(NF-1)+1,K1) = HALF*FORX
                     FORNEQSKY(3*(NF-1)+2,K1) = HALF*FORY
                     FORNEQSKY(3*(NF-1)+3,K1) = HALF*FORZ
                   ENDIF

                   IF(ADS2 < 10) THEN
                     FORNEQSKY(3*(NF-1)+1,K2) = FORX
                     FORNEQSKY(3*(NF-1)+2,K2) = FORY
                     FORNEQSKY(3*(NF-1)+3,K2) = FORZ
                   ELSE
                     FORNEQSKY(3*(NF-1)+1,K2) = HALF*FORX
                     FORNEQSKY(3*(NF-1)+2,K2) = HALF*FORY
                     FORNEQSKY(3*(NF-1)+3,K2) = HALF*FORZ
                   ENDIF

                   IF(ADS3 < 10) THEN
                     FORNEQSKY(3*(NF-1)+1,K3) = FORX
                     FORNEQSKY(3*(NF-1)+2,K3) = FORY
                     FORNEQSKY(3*(NF-1)+3,K3) = FORZ
                   ELSE
                     FORNEQSKY(3*(NF-1)+1,K3) = HALF*FORX
                     FORNEQSKY(3*(NF-1)+2,K3) = HALF*FORY
                     FORNEQSKY(3*(NF-1)+3,K3) = HALF*FORZ
                   ENDIF
 
                   IF(ADS4 < 10) THEN
                     FORNEQSKY(3*(NF-1)+1,K4) = FORX
                     FORNEQSKY(3*(NF-1)+2,K4) = FORY
                     FORNEQSKY(3*(NF-1)+3,K4) = FORZ
                   ELSE
                     FORNEQSKY(3*(NF-1)+1,K4) = HALF*FORX
                     FORNEQSKY(3*(NF-1)+2,K4) = HALF*FORY
                     FORNEQSKY(3*(NF-1)+3,K4) = HALF*FORZ
                   ENDIF

                ELSE

                   SX1 = X(1,N2) - X(1,N1)
                   SY1 = X(2,N2) - X(2,N1)
                   SZ1 = X(3,N2) - X(3,N1)
                   SX2 = X(1,N3) - X(1,N1)
                   SY2 = X(2,N3) - X(2,N1)
                   SZ2 = X(3,N3) - X(3,N1)
                   SX3  = SY1*SZ2 - SZ1*SY2
                   SY3  = SZ1*SX2 - SX1*SZ2
                   SZ3  = SX1*SY2 - SY1*SX2
c                  AREAS= SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3) 
C
                   SIGNX  = STRESSMEAN(1,IE)*SX3 + STRESSMEAN(4,IE)*SY3 +STRESSMEAN(6,IE)*SZ3 
                   SIGNY  = STRESSMEAN(4,IE)*SX3 + STRESSMEAN(2,IE)*SY3 +STRESSMEAN(5,IE)*SZ3 
                   SIGNZ  = STRESSMEAN(6,IE)*SX3 + STRESSMEAN(5,IE)*SY3 +STRESSMEAN(3,IE)*SZ3 
C
               
                   FORX = ONE_OVER_8*SIGNX
                   FORY = ONE_OVER_8*SIGNY
                   FORZ = ONE_OVER_8*SIGNZ
C
                   IF(IE > NUMELS8)  IE10 = IE - NUMELS8

                   IF(ADS1 < 10) THEN
                     K1 = IADS(ADS1,IE)
                   ELSE
                     K1 = IADS10(ADS1-10,IE10)
                   ENDIF

                   IF(ADS2 < 10) THEN
                     K2 = IADS(ADS2,IE)
                   ELSE
                     K2 = IADS10(ADS2-10,IE10)
                   ENDIF

                   IF(ADS3 < 10) THEN
                     K3 = IADS(ADS3,IE)
                   ELSE
                     K3 = IADS10(ADS3-10,IE10)
                   ENDIF


                   IF(ADS1 < 10) THEN
                     FORNEQSKY(3*(NF-1)+1,K1) = FORX
                     FORNEQSKY(3*(NF-1)+2,K1) = FORY
                     FORNEQSKY(3*(NF-1)+3,K1) = FORZ
                   ELSE
                     FORNEQSKY(3*(NF-1)+1,K1) = THIRD*FORX
                     FORNEQSKY(3*(NF-1)+2,K1) = THIRD*FORY
                     FORNEQSKY(3*(NF-1)+3,K1) = THIRD*FORZ
                   ENDIF

                   IF(ADS2 < 10) THEN
                     FORNEQSKY(3*(NF-1)+1,K2) = FORX
                     FORNEQSKY(3*(NF-1)+2,K2) = FORY
                     FORNEQSKY(3*(NF-1)+3,K2) = FORZ
                   ELSE
                     FORNEQSKY(3*(NF-1)+1,K2) = THIRD*FORX
                     FORNEQSKY(3*(NF-1)+2,K2) = THIRD*FORY
                     FORNEQSKY(3*(NF-1)+3,K2) = THIRD*FORZ
                   ENDIF

                   IF(ADS3 < 10) THEN
                     FORNEQSKY(3*(NF-1)+1,K3) = FORX
                     FORNEQSKY(3*(NF-1)+2,K3) = FORY
                     FORNEQSKY(3*(NF-1)+3,K3) = FORZ
                   ELSE
                     FORNEQSKY(3*(NF-1)+1,K3) = THIRD*FORX
                     FORNEQSKY(3*(NF-1)+2,K3) = THIRD*FORY
                     FORNEQSKY(3*(NF-1)+3,K3) = THIRD*FORZ
                   ENDIF
                ENDIF
C
              ENDIF

       
            ENDDO

            DO N=1,NSN
                SN = INTBUF_TAB(NIN)%NSV(N)
                ITAG(SN) = 1
            ENDDO
          ENDIF
         ENDDO

      ENDIF


C-----------------------------------------------
      END SUBROUTINE I24NITSCHFOR3
    
