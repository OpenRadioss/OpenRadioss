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
Chd|  SMS_AUTO_DT                   source/ams/sms_auto_dt.F      
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        NOD_SMS_DT                    source/ams/sms_auto_dt.F      
Chd|        NOD_SMS_DT_SOL                source/ams/sms_auto_dt.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SMS_AUTO_DT(DTELEM,NATIV_SMS,
     1  IXS     ,IXQ      ,IXC     ,IXT     ,IXP       ,
     2  IXR     ,IXTG     ,IXS10   ,IXS16   ,IXS20     ,
     3  IPARTS  ,IPARTQ   ,IPARTC  ,IPARTT  ,IPARTP    ,
     4  IPARTR  ,IPARTUR  ,IPARTG  ,IPARTX  ,IPART     ,
     5  IPARG   ,ELBUF_TAB,IGEO    ,IDDLEVEL,TAGPRT_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------                     
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "param_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
#include      "sphcom.inc"
#include      "rnur_c.inc"
#include      "units_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NATIV_SMS(*),IPART(LIPART1,*),
     .   IXS(NIXS,*),IXS10(6,*) ,IXS16(8,*) ,IXS20(12,*),
     .   IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*),IXTG(NIXTG,*),IPARTS(*),IPARTQ(*),IPARTC(*),
     .   IPARTT(*),IPARTP(*),IPARTR(*),IPARTUR(*),IPARTG(*),IPARTX(*),
     .   IPARG(NPARG,NGROUP),IGEO(NPROPGI,*),IDDLEVEL, TAGPRT_SMS(*)
      my_real
     .   DTELEM(*)
C
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NEL,IAD,NFT,ITY,ISOL,NG,NUMEL,
     .   COMPT_PART(NPART),COMPT_TOT_PART(NPART),COMPT(7),COMPT_TOT,
     .   NUME(7),DT_INDEX(7)
      CHARACTER FILNAM*109, KEYA*80, KEYA2*80
      CHARACTER*12 :: TYPE_ELEM(7)
      my_real
     .   RATIO
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
C
C     Selection of elements for AMS based on time step
C     must be launched 2 times (iddlevel = 0/1) 
C     because GBUF%ISMS is cleared after the end of first pass
C
C-----------------------------------------------
C
C-----------------------------------------------
C     Remplissage de ISMS pour les elements selectionnes par PART
C-----------------------------------------------
C
      IF(ISMS_SELEC==1 .OR. ISMS_SELEC==2 .OR. ISMS_SELEC==4)THEN

        DO NG=1,NGROUP
C
          NEL = IPARG(2,NG)
          NFT = IPARG(3,NG)
          IAD = IPARG(4,NG)
          ITY = IPARG(5,NG)
          ISOL = IPARG(28,NG)
          GBUF => ELBUF_TAB(NG)%GBUF
C
          IF(ITY==1)THEN
            DO I=1,NEL
              IF(TAGPRT_SMS(IPARTS(NFT+I))/=0) GBUF%ISMS(I)=1
            END DO
          ELSEIF(ITY==3)THEN
            DO I=1,NEL
              IF(TAGPRT_SMS(IPARTC(NFT+I))/=0) GBUF%ISMS(I)=1
            END DO
          ELSEIF(ITY==7)THEN
            DO I=1,NEL
              IF(TAGPRT_SMS(IPARTG(NFT+I))/=0) GBUF%ISMS(I)=1
            END DO
          ELSEIF(ITY==4)THEN
            DO I=1,NEL
              IF(TAGPRT_SMS(IPARTT(NFT+I))/=0) GBUF%ISMS(I)=1
            END DO
          ELSEIF(ITY==5)THEN
            DO I=1,NEL
              IF(TAGPRT_SMS(IPARTP(NFT+I))/=0) GBUF%ISMS(I)=1
            END DO
          ELSEIF(ITY==6)THEN
            DO I=1,NEL
              IF(TAGPRT_SMS(IPARTR(NFT+I))/=0) GBUF%ISMS(I)=1
            END DO
          END IF
        END DO
C                  
        IF(ISMS_SELEC==1 .OR. ISMS_SELEC==2) RETURN
C     
      END IF
C
C-----------------------------------------------
C     Calcul des index dans dtelem pour chaque type d'element
C-----------------------------------------------
C
      NUMEL= NUMELC+NUMELS+NUMELT+NUMELQ+NUMELP+NUMELR+NUMELUR+NUMELTG
     .      +NUMELX+NUMSPH+NUMELIG3D
C
      NUME(1) = NUMELS
      NUME(2) = NUMELQ
      NUME(3) = NUMELC
      NUME(4) = NUMELT
      NUME(5) = NUMELP
      NUME(6) = NUMELR
      NUME(7) = NUMELTG 	
      DT_INDEX(1)  = 0
      DO I=2,7
        DT_INDEX(I)  = DT_INDEX(I-1)  + NUME(I-1)
      END DO
C
      COMPT(1:7) = 0
      COMPT_PART(1:NPART) = 0
      COMPT_TOT_PART(1:NPART) = 0
C
      TYPE_ELEM(1)='SOLID'
      TYPE_ELEM(3)='SHELL-4NODES'
      TYPE_ELEM(4)='TRUSS'
      TYPE_ELEM(5)='BEAM'
      TYPE_ELEM(6)='SPRING'
      TYPE_ELEM(7)='SHELL-3NODES'
C
C--------------------------------------------------
C     Identification des elements a switcher en AMS	
C--------------------------------------------------
C
      DO NG=1,NGROUP
C
        NEL = IPARG(2,NG)
        NFT = IPARG(3,NG)
        IAD = IPARG(4,NG)
        ITY = IPARG(5,NG)
        ISOL = IPARG(28,NG)
        GBUF => ELBUF_TAB(NG)%GBUF
C
        IF (ITY == 1) THEN
C
          CALL NOD_SMS_DT(NUMNOD,GBUF%ISMS,NEL,NFT,NIXS ,1 ,8 ,
     .        IXS ,NATIV_SMS,DTELEM,ITY,DT_INDEX(ITY),
     .        IPARTS,COMPT(ITY),COMPT_PART,COMPT_TOT_PART,IGEO,IPART,TAGPRT_SMS)
C
C    Tag of additional nodes for solides with more than 8 nodes
C
          IF (ISOL==10) CALL NOD_SMS_DT_SOL(NUMNOD,GBUF%ISMS,NEL,NFT,6 ,NUMELS8,
     .        IXS10 ,NATIV_SMS)
          IF (ISOL==20) CALL NOD_SMS_DT_SOL(NUMNOD,GBUF%ISMS,NEL,NFT,12,NUMELS8+NUMELS10,
     .        IXS20 ,NATIV_SMS)
          IF (ISOL==16) CALL NOD_SMS_DT_SOL(NUMNOD,GBUF%ISMS,NEL,NFT,8 ,NUMELS8+NUMELS10+NUMELS20,
     .        IXS16 ,NATIV_SMS)
C
        ELSEIF (ITY==3) THEN
C                                        
          CALL NOD_SMS_DT(NUMNOD,GBUF%ISMS,NEL,NFT,NIXC ,1 ,4 ,
     .        IXC ,NATIV_SMS,DTELEM,ITY,DT_INDEX(ITY),
     .        IPARTC,COMPT(ITY),COMPT_PART,COMPT_TOT_PART,IGEO,IPART,TAGPRT_SMS)
C
        ELSEIF (ITY==7) THEN
C
          CALL NOD_SMS_DT(NUMNOD,GBUF%ISMS,NEL,NFT,NIXTG,1 ,3 ,
     .        IXTG,NATIV_SMS,DTELEM,ITY,DT_INDEX(ITY),
     .        IPARTG,COMPT(ITY),COMPT_PART,COMPT_TOT_PART,IGEO,IPART,TAGPRT_SMS)
C
        ELSEIF (ITY==4) THEN
C
          CALL NOD_SMS_DT(NUMNOD,GBUF%ISMS,NEL,NFT,NIXT ,1 ,2 ,
     .        IXT ,NATIV_SMS,DTELEM,ITY,DT_INDEX(ITY),
     .        IPARTT,COMPT(ITY),COMPT_PART,COMPT_TOT_PART,IGEO,IPART,TAGPRT_SMS)
C
        ELSEIF (ITY==5) THEN
C
          CALL NOD_SMS_DT(NUMNOD,GBUF%ISMS,NEL,NFT,NIXP ,1 ,2 ,
     .        IXP ,NATIV_SMS,DTELEM,ITY,DT_INDEX(ITY),
     .        IPARTP,COMPT(ITY),COMPT_PART,COMPT_TOT_PART,IGEO,IPART,TAGPRT_SMS)
C
        ELSEIF (ITY==6) THEN
C
          CALL NOD_SMS_DT(NUMNOD,GBUF%ISMS,NEL,NFT,NIXR ,1 ,2 ,
     .        IXR ,NATIV_SMS,DTELEM,ITY,DT_INDEX(ITY),
     .        IPARTR,COMPT(ITY),COMPT_PART,COMPT_TOT_PART,IGEO,IPART,TAGPRT_SMS)
C
        ENDIF
C
      ENDDO
C
      COMPT_TOT = 0
      DO I=1,7
        COMPT_TOT = COMPT_TOT + COMPT(I)
      ENDDO
C
C------------------------------------------------------------------
C     Printout of selected elements - only at first call of initia	
C------------------------------------------------------------------
C
      IF (IDDLEVEL == 0) THEN
C
        WRITE(IOUT,1000)
        WRITE(IOUT,1001)
        IF (COMPT_TOT>0) THEN
          WRITE(IOUT,2000) COMPT_TOT
          RATIO = 100*REAL(COMPT_TOT)/REAL(MAX(1,NUMEL))
          WRITE(IOUT,2001) RATIO
          WRITE(IOUT,2010)
          WRITE(IOUT,2011)
          DO I=1,7
            IF (COMPT(I)>0) WRITE(IOUT,'(7X,A12,I10)') TYPE_ELEM(I),COMPT(I)
          END DO
          WRITE(IOUT,2020)
          WRITE(IOUT,2011)
          WRITE(IOUT,2021)
          DO I=1,NPART
            IF ((COMPT_PART(I)>0).AND.(COMPT_TOT_PART(I)>0)) THEN
              RATIO = 100*REAL(COMPT_PART(I))/REAL(COMPT_TOT_PART(I))
              WRITE(IOUT,'(4X,I10,19X,I10,12X,F16.1)') IPART(4,I),COMPT_PART(I),RATIO
            ENDIF
          ENDDO
        ELSE
          WRITE(IOUT,3000)
        ENDIF
C
      ENDIF
C                  
C------------------------------------------- 
      RETURN
C
 1000 FORMAT(//,9X,'AUTOMATIC SELECTION OF ELEMENTS FOR AMS')
 1001 FORMAT(9X,'---------------------------------------',/)

 2000 FORMAT(3X,'TOTAL NUMBER OF ELEMENTS SELECTED ',I10)
 2001 FORMAT(3X,'% OF ELEMENTS SELECTED IN THE MODEL',F16.1)
 2010 FORMAT(/,3X,'REPARTITION OF SELECTED ELEMENTS BY TYPES')
 2011 FORMAT(3X,'-----------------------------------------')
 2020 FORMAT(/,3X,'REPARTITION OF SELECTED ELEMENTS BY PARTS')
 2021 FORMAT(7X,'PART ID',6X,'NB OF ELEMENTS SELECTED',6X,'% OF ELEMENTS SELECTED')

 3000 FORMAT(2X,'NO ELEMENTS SELECTED FOR AMS ACTIVATION')

      END

Chd|====================================================================
Chd|  NOD_SMS_DT                    source/ams/sms_auto_dt.F      
Chd|-- called by -----------
Chd|        SMS_AUTO_DT                   source/ams/sms_auto_dt.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NOD_SMS_DT(
     1 NUMNOD  ,TAGEL_SMS,NEL,NFT,NIX ,MIX ,LIX_INPUT,
     2 IX      ,TAGN_SMS,DTELEM,ITY,DT_INDEX,
     3 IPART   ,COMPT,COMPT_PART,COMPT_TOT_PART,IGEO,IPARTG,TAGPRT_SMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "sms_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NUMNOD   , NIX  ,MIX, LIX_INPUT,
     .         IX(NIX,*), TAGN_SMS(*),NEL,
     .         IPART(*),COMPT_PART(*),COMPT_TOT_PART(*),COMPT,ID_PART,
     .         TAGEL_SMS(*),NFT,ITY,DT_INDEX,IGEO(NPROPGI,*),IPARTG(LIPART1,*),TAGPRT_SMS(*)
      my_real
     .   DTELEM(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, TAG(NUMNOD), ID_ELT, IGTYP, IGG, LIX
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C   
      DO J=1,NEL
        ID_ELT = NFT + J
        ID_PART = IPART(ID_ELT)
        IGG = IPARTG(2,ID_PART)
        IGTYP =  IGEO(11,IGG)
        IF (TAGPRT_SMS(ID_PART)==0) COMPT_TOT_PART(ID_PART) = COMPT_TOT_PART(ID_PART) + 1
C
        IF(DTELEM(DT_INDEX+ID_ELT)>DT_SMS_SWITCH) CYCLE
C
C---   Third node must be taken into account for spring pulley
        LIX = LIX_INPUT
        IF ((ITY==6).AND.(IGTYP==12)) LIX = 3
C
        DO K=1,LIX
          I = IX(MIX+K,ID_ELT)
          IF(I/=0) TAG(I)=0
        ENDDO
        DO K=1,LIX
          I = IX(MIX+K,ID_ELT)
          IF(I/=0)THEN
            IF(TAG(I)==0)THEN
              TAGN_SMS(I)=1
              TAG(I)=1
            END IF		
          END IF		
        ENDDO
        IF (TAGPRT_SMS(ID_PART)==0) THEN
          COMPT = COMPT + 1
          COMPT_PART(ID_PART) = COMPT_PART(ID_PART) + 1
        ENDIF

C---   Tag per element
        TAGEL_SMS(J) = 1
C                          
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  NOD_SMS_DT_SOL                source/ams/sms_auto_dt.F      
Chd|-- called by -----------
Chd|        SMS_AUTO_DT                   source/ams/sms_auto_dt.F      
Chd|-- calls ---------------
Chd|        SMS_MOD                       share/modules1/sms_mod.F      
Chd|====================================================================
      SUBROUTINE NOD_SMS_DT_SOL(
     1 NUMNOD  ,TAGEL_SMS ,NEL  ,NFT     ,NIX ,OFFSET,
     2 IXS  ,TAGN_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SMS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "sms_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NUMNOD   , NIX, OFFSET,
     .         IXS(NIX,*), TAGN_SMS(*),NEL,TAGEL_SMS(*),NFT
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, TAG(NUMNOD), ID_ELT, ID_ELT_B
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C   
      DO J=1,NEL
        ID_ELT = NFT + J
        ID_ELT_B = NFT + J - OFFSET
C
        IF (TAGEL_SMS(J) == 1) THEN
C
C-- > Tag of additional nodes for selected solides with more than 8 nodes      
C
          DO K=1,NIX
            I = IXS(K,ID_ELT_B)
            IF(I/=0) TAG(I)=0
          ENDDO
          DO K=1,NIX
            I = IXS(K,ID_ELT_B)
            IF(I/=0)THEN
              IF(TAG(I)==0)THEN
                TAGN_SMS(I)=1
                TAG(I)=1
              END IF		
            END IF		
          ENDDO
C
        ENDIF
C                          
      ENDDO
C
      RETURN
      END
