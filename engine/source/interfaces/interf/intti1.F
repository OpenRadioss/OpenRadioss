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
Chd|  INTTI1                        source/interfaces/interf/intti1.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ASSPARI2                      source/assembly/asspar4.F     
Chd|        I2SKIP                        source/interfaces/interf/i2for3p.F
Chd|        INT2POFF                      source/interfaces/interf/int2poff.F
Chd|        INT2POFFH                     source/interfaces/interf/int2poff.F
Chd|        INT2RUPT                      source/interfaces/interf/int2rupt.F
Chd|        INTTI12F                      source/interfaces/interf/intti12.F
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|        SPMD_EXCH_A_INT2              source/mpi/forces/spmd_exch_a_int2.F
Chd|        SPMD_EXCH_A_INT2H             source/mpi/forces/spmd_exch_a_int2h.F
Chd|        SPMD_EXCH_A_INT2H_AMS         source/mpi/forces/spmd_exch_a_int2h_ams.F
Chd|        SPMD_EXCH_A_INT2_AMS          source/mpi/forces/spmd_exch_a_int2_ams.F
Chd|        SPMD_EXCH_A_INT2_PON          source/mpi/forces/spmd_exch_a_int2_pon.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INTTI1(
     1   IPARI    ,X        ,V        ,A       ,
     2   VR       ,AR       ,WA       ,MS      ,IN       ,WEIGHT  ,
     3   STIFN    ,STIFR    ,KHIE     ,ITAB    ,FR_I2M   ,IAD_I2M ,
     4   ADDCNI2  ,PROCNI2  ,IADI2    ,I2MSCH  ,DMAST    ,ADM     ,
     5   SKEW     ,I2SIZE   ,FR_NBCCI2,ADI     ,IGEO     ,BUFGEO  ,
     6   FSAV     ,NPF      ,TF       ,FNCONT  ,IAD_ELEM ,FR_ELEM ,
     7   NODNX_SMS,DMINT2   ,PDAMA2   ,NB_FRI2M,FR_LOCI2M,
     8   DT2T     ,NELTST   ,ITYPTST  ,INTBUF_TAB,TEMP   ,MCP     ,
     9   FTHE     ,CONDN    ,
     A   H3D_DATA ,T2FAC_SMS,FNCONTP  ,FTCONTP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr18_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), WEIGHT(*), FR_I2M(*), IAD_I2M(*),
     .        ITAB(*),KHIE,ADDCNI2(*),PROCNI2(*),IADI2(*),IGEO(*),
     .        FR_NBCCI2(2,*),NPF(*),IAD_ELEM(2,*),FR_ELEM(*),
     .        NODNX_SMS(*),NB_FRI2M,FR_LOCI2M(*)
      INTEGER I2MSCH,ILAGM,I2SIZE,NELTST,ITYPTST
C     REAL
      my_real
     .   X(3,*), V(3,*), A(3,*), WA(*), MS(*),IN(*),
     .   AR(3,*),VR(3,*),STIFN(*),STIFR(*),DMAST,ADM(*),SKEW(*),
     .   ADI(*),BUFGEO(*),FSAV(NTHVKI,*),TF(*), FNCONT(3,*),
     .   DMINT2(*),PDAMA2(*),DT2T,TEMP(*),FTHE(*),CONDN(*),MCP(*),
     .   T2FAC_SMS(*),
     .   FNCONTP(3,*),FTCONTP(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NTY, JI, JB, NMN, NINT, LWA, I2OK,K,ITIED,
     .        LCOMI2M, I0, NIR, SIZE, LENS, LENR, I, J, ILEV,
     .        TAGNOD(NUMNOD), K10, K11, K12, NSN, KSN,I25PENA,
     .        I2SIZETH,INTTH2, SIZE_MINER_POFF,II
C     REAL
      my_real
     .   FSKYI2(I2SIZE,LCNI2),FTHESKYI2(LCNI2),CONDNSKYI2(LCNI2)
      my_real,
     . DIMENSION(:,:),ALLOCATABLE ::SAV_FOR_PENA
      my_real,
     . DIMENSION(:),ALLOCATABLE ::MS_PENA,SAV_MINER_POFF
C=======================================================================
      I25PENA=0
      SIZE_MINER_POFF = 0
C
      IF (IPARIT == 0)THEN
        DO N=1,NINTER                                    
          NTY  = IPARI(7,N)                               
          ILEV = IPARI(20,N)
          IF (NTY == 2) THEN
            IF (ILEV == 25) THEN
              I25PENA=1
            ELSEIF (ILEV == 26) THEN
              I25PENA=2
            ELSEIF (ILEV == 27 .or. ILEV == 28) THEN
              I25PENA=2
            ENDIF
            IF (IRODDL > 0) SIZE_MINER_POFF = NUMNOD
          ENDIF
        ENDDO
C
        IF (I25PENA == 2) THEN
          ALLOCATE(SAV_FOR_PENA(8,NUMNOD))
          SAV_FOR_PENA(1:8,1:NUMNOD) = ZERO
          ALLOCATE(MS_PENA(NUMNOD))
          MS_PENA(1:NUMNOD) = MS(1:NUMNOD)
        ELSEIF (I25PENA == 1) THEN
          ALLOCATE(SAV_FOR_PENA(4,NUMNOD))
          SAV_FOR_PENA(1:4,1:NUMNOD) = ZERO
          ALLOCATE(MS_PENA(NUMNOD))
          MS_PENA(1:NUMNOD) = MS(1:NUMNOD)
        ELSE
          ALLOCATE(SAV_FOR_PENA(8,0))
          ALLOCATE(MS_PENA(0))
        ENDIF
C
C-- For parithoff inertia of main node must be saved
        ALLOCATE(SAV_MINER_POFF(SIZE_MINER_POFF))
        IF (SIZE_MINER_POFF>0) SAV_MINER_POFF(1:NUMNOD) = IN(1:NUMNOD)
C
      ENDIF
C
C      Calcul flag de rupture pour interface type 2 user
       DO N=1,NINTER                                    
         NTY  = IPARI(7,N)                               
         ILEV = IPARI(20,N)
         IF (NTY == 2 .AND. ILEV >= 10 .AND. ILEV < 23) THEN             
           JI  =IPARI(1,N)                                 
           JB  =IPARI(2,N)    
           CALL INT2RUPT(                              
     .          IPARI(1,N),MS        ,IN         ,
     .          X         ,V        ,A        ,STIFN     ,IGEO       ,
     .          WEIGHT    ,FSAV(1,N),ILEV     ,NPF       ,TF         ,
     .          ITAB      ,FNCONT   ,PDAMA2   ,INTBUF_TAB(N),H3D_DATA,
     .          FNCONTP   ,FTCONTP )         
         ENDIF                                           
       ENDDO                                            
C
Cel correction pb poff si main dans plusieurs interfaces type2
      IF (IMACH == 3 .AND. IPARIT == 0) THEN                     
        IF (NHIN2 == 0) THEN                                  
          DO N=1,NINTER                                       
            NTY   =IPARI(7,N)                                 
            IF (IPARI(26,N) == KHIE) THEN                       
              JI=IPARI(1,N)                                   
              JB=IPARI(2,N)                                   
              NMN =IPARI(6,N)                                 
              NINT=N                                          
              ILAGM = IPARI(33,N)                             
              IF (NTY == 2 .AND. ILAGM == 0)THEN  
                CALL INT2POFF(                                
     .   	         IPARI(1,N) ,X        ,V      ,  
     .   	         A          ,VR       ,AR     ,MS    ,IN    ,          
     .               WEIGHT     ,STIFN    ,STIFR  ,MCP   ,CONDN , 
     .               FTHE       ,INTBUF_TAB(N)    )               
              ENDIF                                           
            ENDIF                                             
          ENDDO                                               
        ELSE                                                  
          TAGNOD=0                                            
          DO N=1,NINTER                                       
            NTY = IPARI(7,N)                                 
            IF (IPARI(26,N) == KHIE) THEN                      
              JI=IPARI(1,N)                                   
              JB=IPARI(2,N)                                   
              NMN =IPARI(6,N)                                 
              NINT=N                                          
              ILAGM = IPARI(33,N)                             
              IF (NTY == 2 .AND. ILAGM == 0) THEN 
                CALL INT2POFFH(                               
     .   	      IPARI(1,N),X    ,V    ,  
     .   	      A	  ,VR	,AR   ,MS   ,IN    ,          
     .   	      WEIGHT    ,STIFN     ,STIFR   ,TAGNOD,INTBUF_TAB(N))        
              ENDIF                                           
            ENDIF                                             
          ENDDO                                               
        ENDIF                                                 
       ELSEIF (IMACH == 3 .AND. IPARIT /= 0) THEN               
c       ELSEIF(IMACH == 3.AND.IPARIT /= 0.AND.NHIN2 /= 0)THEN 
        DO I=1,LCNI2                                          
          DO J=1,I2SIZE                                       
            FSKYI2(J,I)=ZERO                                  
          END DO                                              
        END DO 
      ENDIF
C
      IF(IPARIT /= 0.AND.INTHEAT /= 0)THEN 
        DO I=1,LCNI2                                          
           FTHESKYI2(I)=ZERO                                  
        END DO  
        IF(IDT_THERM == 1) THEN
          DO I=1,LCNI2                                          
           CONDNSKYI2(I)=ZERO  
          ENDDO                                
        ENDIF                                              
      ENDIF                                                     
C
      I2OK = 0          
      I2MSCH=0          
      I0 = 0            
      NIR=2 
      IF(N2D == 0)NIR=4 
      KSN=1     
      INTTH2 = 0       
C---
      DO N=1,NINTER                                                     
        NTY = IPARI(7,N)                                               
        IF (IPARI(26,N) == KHIE) THEN                                     
          JI=IPARI(1,N)                                                 
          JB=IPARI(2,N)                                                 
          NSN =IPARI(5,N)
          NMN =IPARI(6,N)                                               
          ILEV=IPARI(20,N)                                               
          NINT=N                                                        
          ILAGM = IPARI(33,N)                                           
          IF(NTY == 2 .AND. ILAGM == 0)THEN                             
            I2OK=1
            IF (ILEV == 0.OR.ILEV == 1.OR.ILEV == 3.OR.ILEV == 27.OR.ILEV == 28) I2MSCH = 1
            IF (ILEV==25.OR.ILEV==26.OR.ILEV==27.OR.ILEV==28) I7KGLO=1 
        !       Optimization :
        !       If NSN=0, some variables are loaded in INTTI2F (NRTS,...)
        !       if the number of TYPE2 interface is important (>3000) and if the number of
        !       spmd domain is quite important (NSPMD>50), the initialisation time is important :
        !       for_array_copy_in and other initialisations represent up to 5% of total CPUTIME
        !       and break the scalability of the code
            IF((NSN>0)) THEN
            CALL INTTI2F(                                               
     1       IPARI(1,N),X       ,V        ,A      , 
     2       VR        ,AR       ,MS       ,IN      ,WEIGHT   ,STIFN  , 
     3       STIFR     ,FSKYI2   ,IADI2    ,I2MSCH  ,DMAST    ,ADM    , 
     4       I0        ,NIR      ,I2SIZE   ,ADI     ,IGEO     ,BUFGEO , 
     5       FSAV(1,N) ,FNCONT   ,NODNX_SMS,DMINT2(KSN)  ,SAV_FOR_PENA,
     6       MS_PENA   ,DT2T     ,NELTST   ,ITYPTST ,INTBUF_TAB(N),TEMP,
     7       FTHE      ,FTHESKYI2,CONDN    ,CONDNSKYI2,ITAB,
     8       SAV_MINER_POFF     ,H3D_DATA,T2FAC_SMS,FNCONTP ,
     A       FTCONTP)
            ELSE
        !       WARNING : if NSN==0 and ILEV==2 or 4, ones needs to save the mass 
                ILEV = IPARI(20,N)
                IF(ILEV==2.OR.ILEV==4) THEN
                        DO II=1,NMN
                                J=INTBUF_TAB(N)%MSR(II)
                                INTBUF_TAB(N)%NMAS(II) = MS(J)
                        ENDDO
                ENDIF
          ENDIF
            IF (ILEV==25 .or. ILEV==26 .or. ILEV==27 .or. ILEV==28) KSN=KSN+4*NSN
          ELSEIF(NTY == 12)THEN                                           
            IF(IMACH /= 3.OR.ISPMD == 0)THEN                         
             CALL INTTI12F(                                          
     .        IPARI(1,N),INTBUF_TAB(N)      ,X      ,V     ,         
     .        A         ,MS       ,ITAB     ,WEIGHT ,STIFN,WA,SKEW ) 
            ENDIF                                                    
          ENDIF                                                      
        ELSEIF(IMACH == 3.AND.IPARIT > 0)THEN                       
          ILAGM = IPARI(33,N)                                        
          IF(IPARI(26,N) /= KHIE.AND.NTY == 2.AND.ILAGM == 0)THEN    
            K10=IPARI(1,N)                                           
            K11=K10+4*IPARI(3,N)                                     
            K12=K11+4*IPARI(4,N)                                     
            CALL I2SKIP(IPARI(5,N) ,INTBUF_TAB(N)%NSV ,WEIGHT ,I0 ) 
          ENDIF                                                      
        ENDIF 
        IF(NTY == 2 .AND. IPARI(47,N)> 0)THEN  
           INTTH2 = 1
        ENDIF                           
      ENDDO 
      IF (INTTH2 == 1) THEN
        I2SIZETH = I2SIZE + 1
        IF(IDT_THERM == 1) I2SIZETH = I2SIZETH + 1
      ELSE
          I2SIZETH = I2SIZE  
      ENDIF                                                           
C------------------------------------------------------------
      IF (IMACH == 3 .AND. I2OK == 1) THEN
C version spmd avec plus d'1 proc : sommer A et AR sur les noeuds main
        IF (IPARIT == 0.AND.NSPMD > 1) THEN
          IF(NHIN2 == 0) THEN
            LCOMI2M = IAD_I2M(NSPMD+1)
            IF(IDTMINS/=0)THEN
              CALL SPMD_EXCH_A_INT2_AMS(
     .            A    ,AR   ,MS      ,IN     ,STIFN,
     .            STIFR,FR_I2M,IAD_I2M,LCOMI2M,I2SIZETH,
     .            NB_FRI2M,FR_LOCI2M,INTTH2,FTHE,CONDN,
     .            FNCONT,FNCONTP,FTCONTP,H3D_DATA)
            ELSE
              CALL SPMD_EXCH_A_INT2(
     .            A    ,AR   ,MS      ,IN     ,STIFN,
     .            STIFR,FR_I2M,IAD_I2M,LCOMI2M,I2SIZETH,
     .            INTTH2,FTHE ,CONDN  ,FNCONT ,FNCONTP ,
     .            FTCONTP,H3D_DATA )
             ENDIF
          ELSE
            LCOMI2M = IAD_I2M(NSPMD+1)
            IF(IDTMINS/=0)THEN
              CALL SPMD_EXCH_A_INT2H_AMS(
     .         A    ,AR   ,MS      ,IN     ,STIFN,
     .         STIFR,FR_I2M,IAD_I2M,LCOMI2M,I2SIZETH,
     .         NB_FRI2M,FR_LOCI2M,TAGNOD,INTTH2,FTHE,
     .         CONDN,FNCONT,FNCONTP,FTCONTP,H3D_DATA )
            ELSE
              CALL SPMD_EXCH_A_INT2H(
     .         A    ,AR   ,MS      ,IN     ,STIFN,
     .         STIFR,FR_I2M,IAD_I2M,LCOMI2M,I2SIZETH,
     .         TAGNOD,INTTH2,FTHE  ,CONDN  ,FNCONT  ,
     .         FNCONTP,FTCONTP,H3D_DATA )
            ENDIF
          END IF
        ELSEIF (IPARIT > 0) THEN
C         version spmd p/on
          IF (NSPMD > 1) THEN
            LENS = FR_NBCCI2(1,NSPMD+1)
            LENR = FR_NBCCI2(2,NSPMD+1)
            LCOMI2M = IAD_I2M(NSPMD+1)
            CALL SPMD_EXCH_A_INT2_PON(
     1         FR_I2M  ,IAD_I2M,ADDCNI2,PROCNI2,FR_NBCCI2,
     2         I2SIZETH,LENR   ,LENS   ,FSKYI2 ,INTTH2   ,
     3         FTHESKYI2,CONDNSKYI2 ,I2SIZE,LCOMI2M,FNCONT,
     4         FNCONTP,FTCONTP,H3D_DATA )
          END IF
C
C Routine assemblage parith/ON
C
C Rare case where type2 interfaces are defined with no more active secnd nodes
        IF(I2NSNT > 0)
     *    CALL ASSPARI2(
     1       A    ,AR    ,STIFN ,STIFR  ,MS               ,
     2       IN   ,FSKYI2,I2SIZE,ADDCNI2,ADDCNI2(NUMNOD+2),
     3      FTHESKYI2, FTHE ,CONDNSKYI2,CONDN)
        ENDIF
      ENDIF
C
      IF (I25PENA > 0 .AND. I2NSNT>0)THEN
        DO I=1,NUMNOD
          A(1,I)=A(1,I)+SAV_FOR_PENA(1,I)
          A(2,I)=A(2,I)+SAV_FOR_PENA(2,I)
          A(3,I)=A(3,I)+SAV_FOR_PENA(3,I)
          STIFN(I) = STIFN(I) + SAV_FOR_PENA(4,I)
        ENDDO
        IF (I25PENA == 2 .and. IRODDL == 1)THEN
          DO I=1,NUMNOD
            AR(1,I)=AR(1,I)+SAV_FOR_PENA(5,I)
            AR(2,I)=AR(2,I)+SAV_FOR_PENA(6,I)
            AR(3,I)=AR(3,I)+SAV_FOR_PENA(7,I)
            STIFR(I) = STIFR(I) + SAV_FOR_PENA(8,I)
          ENDDO
        ENDIF
        DEALLOCATE(SAV_FOR_PENA)
        DEALLOCATE(MS_PENA)
      ENDIF
C

C
      RETURN
      END
