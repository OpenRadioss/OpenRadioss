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
!||    hm_read_inject2            ../starter/source/properties/injector/hm_read_inject2.F
!||--- called by ------------------------------------------------------
!||    hm_read_properties         ../starter/source/properties/hm_read_properties.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                     ../starter/source/output/message/message.F
!||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_floatv              ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim          ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    nintri                     ../starter/source/system/nintrr.F
!||    usr2sys                    ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod               ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod         ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                ../starter/share/message_module/message_mod.F
!||    submodel_mod               ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_INJECT2(GEO       ,IGEO     ,PROP_TAG ,IGTYP    ,PROP_ID        ,
     .                           IDTITL    ,UNITAB   ,LSUBMODEL,IPM      ,PM             ,
     .                           NPC       ,PLD)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD  
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IGEO(*)
      INTEGER ,INTENT(IN) :: IGTYP,PROP_ID,IPM(NPROPMI,NUMMAT),NPC(*)
      my_real, INTENT(IN) :: PLD(*),PM(NPROPM,NUMMAT)
      my_real, INTENT(INOUT) ::  GEO(*)
      CHARACTER(LEN=NCHARTITLE)::IDTITL
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IG,I,J,K,NGASES,IFLOW,IJET,NOD1,NOD2,NOD3,
     .        ICURPT
      INTEGER MAT_ID(2,100),F_IDMASS(2,100),F_IDTEMP(2,100),
     .        F_IDMF(2,100),MW_MIXTURE,MW_MIXTURE_OK,
     .        NB_POINTS,NB_POINTS_1,NB_POINTS_OLD,
     .        IFUN_REF,IFUN_TMP,IFUN_TMP_USR
      my_real
     .        FSMASS(100),FSTEMP(100),ASTIME,
     .        MOLFR(100),
     .        CPAI_MIX,CPBI_MIX,CPCI_MIX,CPDI_MIX,CPEI_MIX,CPFI_MIX,
     .        MF_TOT,MOL_TOT,MASS_TOT,MASS_INI,MOL_INI,MWI_MIXTURE,
     .        STP_GAMA_MIX,STP_TEMP,MASS_MOL,INIT_MASS,CPI_MIX,
     .        R_IGC1, FAC_M, FAC_T
      CHARACTER MESS*40
      DATA MESS/'INJECTOR PROPERTY SET                   '/
      DATA STP_TEMP/293.15/
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NINTRI,USR2SYS
C=======================================================================
C
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C----------------------
C FROM LECGEO - GENERAL
C----------------------
      IGEO( 1)=PROP_ID
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+0.1
C----------------------

      IG=IGEO(1)
      IGEO(22)=2 !I_INJECT : Injectors (1:'INJECT1' or 2:'INJECT2')

C Initialisation
      MAT_ID=0
      F_IDMASS=0
      F_IDTEMP=0
      F_IDMF=0
      MOLFR=0
      FSMASS=ZERO
      FSTEMP=ZERO
      ASTIME=ZERO
      CPAI_MIX=0
      CPBI_MIX=0
      CPCI_MIX=0
      CPDI_MIX=0
      CPEI_MIX=0
      CPFI_MIX=0
      FAC_T=ONE
      FAC_M=ONE
C Lecture carte 1
      CALL HM_GET_INTV('NIP',NGASES,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('IFLOW',IFLOW,IS_AVAILABLE,LSUBMODEL)
C Verification
      IF (NGASES<1.OR.100<NGASES) THEN
         CALL ANCMSG(MSGID=696,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=PROP_ID,
     .               C1=IDTITL,
     .               I2=NGASES,
     .               I3=100)
      END IF
      IF (IFLOW/=0.AND.IFLOW/=1) THEN
         CALL ANCMSG(MSGID=697,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=PROP_ID,
     .               C1=IDTITL)
      END IF
C Lecture carte 2
      CALL HM_GET_INTV('FUN_A1',F_IDMASS(1,1),IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B1',F_IDTEMP(1,1),IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('F_SCALE_Y',FSMASS(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('F_SHIFT_Y',FSTEMP(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('A_SCALE_X',ASTIME,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('F_SCALE_Y',FAC_M,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('A_SCALE_X',FAC_T,IS_AVAILABLE,LSUBMODEL,UNITAB)


      IF(FSMASS(1) == ZERO)FSMASS(1)=ONE*FAC_M                                                     
      IF(FSTEMP(1) == ZERO)FSTEMP(1)=ONE*FAC_T                                                     
      IF(F_IDMASS(1,1) == 0) THEN                                                                 
          CALL ANCMSG(MSGID=1115,                                                                 
     .                MSGTYPE=MSGERROR,                                                           
     .                ANMODE=ANINFO_BLIND_1,                                                      
     .                 I1=PROP_ID,                                                                
     .                 C1=IDTITL )                                                                
      ELSE                                                                                        
        DO J=1,NFUNCT                                                                             
          IF(NPC(NFUNCT+1+J) == F_IDMASS(1,1)) F_IDMASS(2,1)=J                                    
          IF(NPC(NFUNCT+1+J) == F_IDTEMP(1,1)) F_IDTEMP(2,1)=J                                    
        ENDDO                                                                                     
        IF(F_IDMASS(2,1) == 0)THEN                                                                
          CALL ANCMSG(MSGID=708,                                                                  
     .                MSGTYPE=MSGERROR,                                                           
     .                ANMODE=ANINFO_BLIND_1,                                                      
     .                I1=PROP_ID,                                                                 
     .                C1=IDTITL,                                                                  
     .                I2=F_IDMASS(1,1))                                                           
        ENDIF                                                                                     
        IF(F_IDTEMP(1,1)/=0.AND.F_IDTEMP(2,1) == 0)THEN                                         
           CALL ANCMSG(MSGID=708,                                                                 
     .                 MSGTYPE=MSGERROR,                                                          
     .                 ANMODE=ANINFO_BLIND_1,                                                     
     .                 I1=PROP_ID,                                                                
     .                 C1=IDTITL,                                                                 
     .                 I2=F_IDTEMP(1,1))                                                          
        ENDIF                                                                                     
        DO I=1,NGASES                                                                             

          CALL HM_GET_INT_ARRAY_INDEX('materialIds',MAT_ID(1,I),I,IS_AVAILABLE,LSUBMODEL)         
          CALL HM_GET_FLOAT_ARRAY_INDEX('CM1',MOLFR(I),I,IS_AVAILABLE,LSUBMODEL,UNITAB)           
          CALL HM_GET_INT_ARRAY_INDEX('ABG_Imass',F_IDMF(1,I),I,IS_AVAILABLE,LSUBMODEL)           

          IF (MOLFR(I) < ZERO) THEN                                                               
            CALL ANCMSG(MSGID=729,                                                                
     .                  MSGTYPE=MSGERROR,                                                         
     .                  ANMODE=ANINFO_BLIND_1,                                                    
     .                  I1=PROP_ID,                                                               
     .                  C1=IDTITL)                                                                
          END IF                                                                                  
          IF (F_IDMF(1,I)/=0) MOLFR(I)=ONE                                                       
C         Verification existence materiau et functions                                            
C ---
C ---     Materiaux                                                                               
          MAT_ID(2,I) = NINTRI(MAT_ID(1,I),IPM,NPROPMI,NUMMAT,1)                                  
          IF(MAT_ID(2,I) == 0) THEN                                                               
            CALL ANCMSG(MSGID=698,                                                                
     .                  MSGTYPE=MSGERROR,                                                         
     .                  ANMODE=ANINFO,                                                            
     .                  I1=PROP_ID,                                                               
     .                  C1=IDTITL,                                                                
     .                  I2=MAT_ID(1,I))                                                           
          ELSEIF(IPM(2,MAT_ID(2,I))/=999) THEN                                                  
            CALL ANCMSG(MSGID=857,                                                                
     .                  MSGTYPE=MSGERROR,                                                         
     .                  ANMODE=ANINFO_BLIND_1,                                                    
     .                  I1=PROP_ID,                                                               
     .                  C1=IDTITL,                                                                
     .                  I2=MAT_ID(1,I))                                                           
          END IF                                                                                  
C ---     Fonctions                                                                               
          DO J=1,NFUNCT                                                                           
            IF(NPC(NFUNCT+1+J) == F_IDMF(1,I)) F_IDMF(2,I)=J                                      
          ENDDO                                                                                   
          IF(F_IDMF(1,I)/=0.AND.F_IDMF(2,I) == 0)THEN                                           
            CALL ANCMSG(MSGID=708,                                                                
     .                  MSGTYPE=MSGERROR,                                                         
     .                  ANMODE=ANINFO_BLIND_1,                                                    
     .                  I1=PROP_ID,                                                               
     .                  C1=IDTITL,                                                                
     .                  I2=F_IDMF(1,I))                                                           
          ENDIF                                                                                   
        ENDDO ! I=1,NGASES                                                                        
      ENDIF                                                                                       

C Conversion d unites ----------------------
      IF(ASTIME == ZERO)ASTIME=ONE*FAC_T
      R_IGC1=PM(27,MAT_ID(2,1))
C     Verification de la croissance des fonctions de masse
      DO I=1,NGASES
        IFUN_TMP=0
        IFUN_TMP_USR=0
        IF (IGEO(22) == 1) THEN
          IF (F_IDMASS(2,I)/=0) THEN
            IFUN_TMP=F_IDMASS(2,I)
            IFUN_TMP_USR=F_IDMASS(1,I)
          END IF
        ELSE IF (IGEO(22) == 2) THEN
          IF (F_IDMF(2,I)/=0) THEN
            IFUN_TMP=F_IDMF(2,I)
            IFUN_TMP_USR=F_IDMF(1,I)
          END IF
        END IF
        IF (IFUN_TMP/=0) THEN
          IF ((NPC(IFUN_TMP+1)-NPC(IFUN_TMP)) >= 4) THEN
            IF (IFLOW == 0) THEN
              DO J = NPC(IFUN_TMP),NPC(IFUN_TMP+1)-3,2
                IF (PLD(J+1) > PLD(J+3)) THEN
                  CALL ANCMSG(MSGID=720,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=PROP_ID,
     .                        C1=IDTITL,
     .                        I2=IFUN_TMP_USR,I3=I)
                END IF
              END DO
            ELSE IF (IFLOW == 1) THEN
              DO J = NPC(IFUN_TMP),NPC(IFUN_TMP+1)-1,2
                IF (PLD(J+1) < ZERO) THEN
                  CALL ANCMSG(MSGID=721,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=PROP_ID,
     .                        C1=IDTITL,
     .                        I2=IFUN_TMP_USR,I3=I)
                END IF
              END DO
            END IF
          END IF 
        END IF
      END DO

      MW_MIXTURE=ZERO                                                     
      MW_MIXTURE_OK=0                                                     
C     Verification du nombre de points des fonctions de fraction molaire  
      NB_POINTS_1=-1                                                      
      DO I=1,NGASES                                                       
        IF (F_IDMF(2,I)/=0) THEN                                        
          IF (NB_POINTS_1==-1)                                          
     .        NB_POINTS_1=NPC(F_IDMF(2,I))-NPC(F_IDMF(2,I)+1)             
          NB_POINTS=NPC(F_IDMF(2,I))-NPC(F_IDMF(2,I)+1)                   
          IF (NB_POINTS/=NB_POINTS_1) THEN                              
            CALL ANCMSG(MSGID=713,                                        
     .                  MSGTYPE=MSGERROR,                                 
     .                  ANMODE=ANINFO,                                    
     .                  I1=PROP_ID,                                            
     .                  C1=IDTITL)                                          
          END IF                                                          
        END IF                                                            
      END DO                                                              
C     Verification des abscisses des fonctions de fraction molaire        
      IFUN_REF=-1                                                         
      DO I=1,NGASES                                                       
        IF (F_IDMF(2,I)/=0) THEN                                        
          IF (IFUN_REF==-1)                                             
     .        IFUN_REF=F_IDMF(2,I)                                        
          DO J=NPC(F_IDMF(2,I)),NPC(F_IDMF(2,I)+1)-1,2                    
            ICURPT=J-NPC(F_IDMF(2,I))                                     
            IF (PLD(J)/=PLD(NPC(IFUN_REF)+ICURPT)) THEN                 
              CALL ANCMSG(MSGID=715,                                      
     .                    MSGTYPE=MSGERROR,                               
     .                    ANMODE=ANINFO,                                  
     .                    I1=PROP_ID,                                          
     .                    C1=IDTITL)                                        
            END IF                                                        
          END DO                                                          
        END IF                                                            
      END DO                                                              
C     Verification somme des fractions molaires egale a 1                 
      IF (IFUN_REF/=-1) THEN                                            
C     Dans le cas ou il y a au moins une fonction                         
        DO J=NPC(IFUN_REF),NPC(IFUN_REF+1)-1,2                            
          ICURPT=J-NPC(IFUN_REF)                                          
          MF_TOT=ZERO                                                     
          DO I=1,NGASES                                                   
            IF (F_IDMF(2,I)==0) THEN                                    
              MF_TOT=MF_TOT+MOLFR(I)                                      
              IF (MOLFR(I)<ZERO) THEN                                  
                CALL ANCMSG(MSGID=728,                                    
     .                      MSGTYPE=MSGERROR,                             
     .                      ANMODE=ANINFO_BLIND_1,                        
     .                      I1=PROP_ID,                                        
     .                      C1=IDTITL,                                      
     .                      I2=ICURPT/2+1,I3=I)                           
              END IF                                                      
            ELSE                                                          
              IF (PLD(NPC(F_IDMF(2,I))+ICURPT+1)<ZERO) THEN            
                CALL ANCMSG(MSGID=728,                                    
     .                      MSGTYPE=MSGERROR,                             
     .                      ANMODE=ANINFO_BLIND_1,                        
     .                      I1=PROP_ID,                                        
     .                      C1=IDTITL,                                      
     .                      I2=ICURPT/2+1,I3=I)                           
              END IF                                                      
              MF_TOT=MF_TOT+PLD(NPC(F_IDMF(2,I))+ICURPT+1)                
     .                     *MOLFR(I)                                      
            END IF                                                        
          END DO                                                          
          IF (ABS(MF_TOT-ONE)>EM03) THEN                                
            CALL ANCMSG(MSGID=716,                                        
     .                  MSGTYPE=MSGERROR,                                 
     .                  ANMODE=ANINFO_BLIND_1,                            
     .                  I1=PROP_ID,                                            
     .                  C1=IDTITL,                                          
     .                  I2=ICURPT/2+1)                                    
          END IF                                                          
        END DO  
      ELSE
      !Dans le cas ou il n y a que des fractions molaires
        MF_TOT=ZERO
        DO I=1,NGASES
          MF_TOT=MF_TOT+MOLFR(I)
        END DO
      !Si la somme est zero on ne peut rien faire, MF_TOT est positif
        IF (MF_TOT<EM03) THEN
          CALL ANCMSG(MSGID=717,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=PROP_ID,
     .                C1=IDTITL)
        ELSE IF (ABS(MF_TOT-ONE)>EM03) THEN
      !Sinon on normalise par rapport a la somme
          DO I=1,NGASES
            MOLFR(I)=MOLFR(I)/MF_TOT
          END DO
          CALL ANCMSG(MSGID=741,
     .                MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .                I1=PROP_ID,
     .                C1=IDTITL)
        END IF    
      END IF                                                          
          
C Calcul des caracteristiques initiales du melange
      MWI_MIXTURE=ZERO
      CPAI_MIX=ZERO
      CPBI_MIX=ZERO
      CPCI_MIX=ZERO
      CPDI_MIX=ZERO
      CPEI_MIX=ZERO
      CPFI_MIX=ZERO
      MASS_INI=ZERO
      MOL_INI=ZERO
      MASS_TOT=ZERO
      MOL_TOT=ZERO
      INIT_MASS=ZERO

      DO I=1,NGASES                                        
       IF (F_IDMF(2,I) == 0) THEN                          
         MOL_INI=MOLFR(I)                                  
       ELSE                                                
         MOL_INI=MOLFR(I)*PLD(NPC(F_IDMF(2,I))+3)          
       END IF                                              
       MOL_TOT=MOL_TOT+MOL_INI                             
      END DO                                               
      DO I=1,NGASES                                        
       IF (MOL_TOT == ZERO) THEN                           
         INIT_MASS    = EM09 / UNITAB%FAC_M_WORK                  
       ELSE                                                
         IF (F_IDMF(2,I) == 0) THEN                        
           MOL_INI=MOLFR(I)                                
         ELSE                                              
           MOL_INI=MOLFR(I)*PLD(NPC(F_IDMF(2,I))+3)        
         END IF                                            
         INIT_MASS    = MOL_INI*PM(20,MAT_ID(2,I))         
       END IF                                              
       MASS_TOT = MASS_TOT + INIT_MASS                     
       CPAI_MIX = CPAI_MIX + INIT_MASS*PM(21,MAT_ID(2,I))  
       CPBI_MIX = CPBI_MIX + INIT_MASS*PM(22,MAT_ID(2,I))  
       CPCI_MIX = CPCI_MIX + INIT_MASS*PM(23,MAT_ID(2,I))  
       CPDI_MIX = CPDI_MIX + INIT_MASS*PM(24,MAT_ID(2,I))  
       CPEI_MIX = CPEI_MIX + INIT_MASS*PM(25,MAT_ID(2,I))  
       CPFI_MIX = CPFI_MIX + INIT_MASS*PM(26,MAT_ID(2,I))  
      END DO                                               
      MWI_MIXTURE=MASS_TOT/MOL_TOT                         
      CPAI_MIX = CPAI_MIX / MASS_TOT                       
      CPBI_MIX = CPBI_MIX / MASS_TOT                       
      CPCI_MIX = CPCI_MIX / MASS_TOT                       
      CPDI_MIX = CPDI_MIX / MASS_TOT                       
      CPEI_MIX = CPEI_MIX / MASS_TOT                       
      CPFI_MIX = CPFI_MIX / MASS_TOT                       

C ------------------------------------------
      IGEO(23)=NGASES
      IGEO(24)=IFLOW
      IGEO(25)=F_IDMASS(2,1)             
      IGEO(26)=F_IDTEMP(2,1)             
      DO I=1,NGASES                      
        IGEO(100+(I-1)*2+1)=MAT_ID(2,I)  
        IGEO(100+(I-1)*2+2)=F_IDMF(2,I)  
      END DO                             
C ------------------------------------------
      GEO(201)=ASTIME
      GEO(202)=MWI_MIXTURE
      GEO(203)=CPAI_MIX
      GEO(204)=CPBI_MIX
      GEO(205)=CPCI_MIX
      GEO(206)=CPDI_MIX
      GEO(207)=CPEI_MIX
      GEO(208)=CPFI_MIX
      CPI_MIX =CPAI_MIX
     .         +CPBI_MIX*STP_TEMP
     .         +CPCI_MIX*STP_TEMP*STP_TEMP
     .         +CPDI_MIX*STP_TEMP*STP_TEMP*STP_TEMP
     .         +CPEI_MIX/(STP_TEMP*STP_TEMP)
     .         +CPFI_MIX*STP_TEMP*STP_TEMP*STP_TEMP*STP_TEMP
      STP_GAMA_MIX=CPI_MIX/(CPI_MIX-R_IGC1/MWI_MIXTURE)

      GEO(209)=FSMASS(1)           
      GEO(210)=FSTEMP(1)           
      GEO(211)=MW_MIXTURE          
      DO I=1,NGASES                
        GEO(211+(I-1)+1)=MOLFR(I)  
      END DO                       
C ------------------------------------------
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,1000)PROP_ID
      ELSE
        WRITE(IOUT,1130)IG,IFLOW,F_IDMASS(1,1),F_IDTEMP(1,1),  
     .     FSMASS(1),FSTEMP(1),ASTIME                          
        WRITE(IOUT,1110)NGASES                                 
        WRITE(IOUT,1115)MWI_MIXTURE,STP_GAMA_MIX,              
     .                  CPAI_MIX,CPBI_MIX,CPCI_MIX,            
     .                  CPDI_MIX,CPEI_MIX,CPFI_MIX             
        DO I=1,NGASES                                          
        WRITE(IOUT,1140)MAT_ID(1,I),MOLFR(I),F_IDMF(1,I)       
        END DO                                                 
        WRITE(IOUT,'(//)')                                     
      ENDIF
C
C----------------------
C FROM LECGEO - GENERAL
C----------------------
      IF(GEO(39)/=ZERO.AND.IGEO( 9)== 0) IGEO( 9)=NINT(GEO(39))
      IF(GEO(171)/=ZERO.AND.IGEO(10)== 0) IGEO(10)=NINT(GEO(171))
C----------------------

      RETURN
 1000    FORMAT(
     &    5X,'INJECTOR PROPERTY SET (/PROP/INJECT2)'/,
     &    5X,'--------------------------------------',/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10,/,
     &    5X,'CONFIDENTIAL DATA'//)
c1100 FORMAT(
c    & 5X,'INJECTOR PROPERTY SET  (/PROP/INJECT2)'/,
c    & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10,/,
c    & 5X,'INCOMING MASS FLAG FOR FUNCTIONS. . . .=',I10,/,
c    & 5X,'(0:MASS/TIME, 1:MASS FLOW/TIME)',/,
c    & 5X,'ABSCISSA SCALE FACTOR',/,
c    & 5X,'     FOR TIME BASED FUNCTIONS . . . . .=',1PG20.13,/)
 1110 FORMAT(
     & 5X,'MIXTURE DEFINTION'/,
     & 5X,'NUMBER OF GASES . . . . . . . . . . . .=',I10,/)
 1115 FORMAT(
     & 5X,'INITIAL CHARACTERISTICS OF MIXTURE',/,
     & 5X,'----------------------------------',/,
     & 5X,'MOLECULAR WEIGHT. . . . . . . . . . . .=',1PG20.13,/,
     & 5X,'STP GAMMA . . . . . . . . . . . . . . .=',1PG20.13,/,
     & 5X,'COEFFICIENT CPA . . . . . . . . . . . .=',1PG20.13,/,
     & 5X,'COEFFICIENT CPB . . . . . . . . . . . .=',1PG20.13,/,
     & 5X,'COEFFICIENT CPC . . . . . . . . . . . .=',1PG20.13,/,
     & 5X,'COEFFICIENT CPD . . . . . . . . . . . .=',1PG20.13,/,
     & 5X,'COEFFICIENT CPE . . . . . . . . . . . .=',1PG20.13,/,
     & 5X,'COEFFICIENT CPF . . . . . . . . . . . .=',1PG20.13,/)
 1130 FORMAT(
     & 5X,'INJECTOR PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10,/,
     & 5X,'INCOMING MASS FLAG FOR FUNCTIONS. . . .=',I10,/,
     & 5X,'(0:MASS/TIME, 1:MASS FLOW/TIME)',/,
     & 5X,'TIME FUNCTION FOR INCOMING MASS . . . .=',I10,/,
     & 5X,'TIME FUNCTION FOR INCOMING GAS TEMP . .=',I10,/,
     & 5X,'SCALE FACTOR FOR INCOMING MASS. . . . .=',1PG20.13,/,
     & 5X,'SCALE FACTOR FOR INCOMING GAS TEMP. . .=',1PG20.13,/,
     & 5X,'ABSCISSA SCALE FACTOR',/,
     & 5X,'     FOR TIME BASED FUNCTIONS . . . . .=',1PG20.13,/)
 1140 FORMAT(
     & 10X,'GAS NUMBER. . . . . . . . . . . . . . .=',I10,/,
     & 10X,'MOLAR FRACTION. . . . . . . . . . . . .=',1PG20.13,/,
     & 10X,'TIME FUNCTION FOR MOLAR FRACTION. . . .=',I10,/)
      RETURN
      END
