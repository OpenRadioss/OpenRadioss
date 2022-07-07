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
Chd|  PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_SPLIT                     source/coupling/rad2rad/r2r_split.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        GRSIZEN                       source/system/nintrr.F        
Chd|        GRSIZE_ELE                    source/system/nintrr.F        
Chd|        GRSIZE_ELE_TRANS              source/system/nintrr.F        
Chd|        GRSIZE_R2R                    source/coupling/rad2rad/routines_r2r.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE PRELECSEC(
     1     SNSTRF  ,SSECBUF   ,ITABM1  ,FLAG_R2R ,NOM_OPT ,
     2     IGRBRIC ,IGRQUAD   ,IGRSH4N ,IGRSH3N  ,IGRTRUSS,
     3     IGRBEAM ,IGRSPRING ,IGRNOD, LSUBMODEL , SEATBELT_SHELL_TO_SPRING,
     4     NB_SEATBELT_SHELLS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "r2r_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(INOUT) :: SNSTRF,SSECBUF
      INTEGER ,INTENT(IN)    :: ITABM1(*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      INTEGER ,INTENT(IN)    :: NB_SEATBELT_SHELLS
      INTEGER ,INTENT(IN)    :: SEATBELT_SHELL_TO_SPRING(NUMELC,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,KK,IBID,ISAV,IGU,IGUS,IGUQ,IGUC,IGUT,IGUP,IGUR,IGUTG,
     .   NNOD,NBINTER,NSEGQ,NSEGS,NSEGC,NSEGT,NSEGP,NSEGR,NSEGTG,NFRAM,
     .   ID,UID,L,ISTYP,
     .   FLAG_R2R,N1,N2,N3,POS_SEC_R2R,NSEG0,NSEG,
     .   COMPT,NG
      CHARACTER TITR*nchartitle
      CHARACTER KEY2*ncharkey,MESS*40  
      LOGICAL IS_AVAILABLE   
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  GRSIZEN,USR2SYS,GRSIZE_R2R,GRSIZE_ELE,GRSIZE_ELE_TRANS
      EXTERNAL GRSIZEN,USR2SYS,GRSIZE_R2R,GRSIZE_ELE,GRSIZE_ELE_TRANS
C=======================================================================
      NFRAM = 0
      SNSTRF  = 30
      SSECBUF = 20
      L = 7
      COMPT = 0
      NG = 0
      
      IF (FLAG_R2R == 1) THEN          
        ALLOCATE(TAGSEC(NSECT))
        TAGSEC = 0
      ENDIF    
      

      CALL HM_OPTION_START('/SECT')
      
      DO I=1,NSECT

        NG=NG+1
C----------Multidomaines --> on ignore les sections non tagees----
        IF (FLAG_R2R == 0) THEN
            IF( NSUBDOM > 0 ) THEN
                IF( TAGSEC(NG) == 0 ) CALL HM_SZ_R2R(TAGSEC,NG,LSUBMODEL)
            ENDIF
        ENDIF
C-----------------------------------------------------------------

        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR, UNIT_ID=UID, KEYWORD2=KEY2)
        NOM_OPT(1,I)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
                
        CALL HM_GET_INTV('Axis_Origin_Node_N1', N1, IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('Axis_Node_N2', N2, IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('Axis_Node_N3', N3, IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('ISAVE', ISAV, IS_AVAILABLE, LSUBMODEL)   

        IF(KEY2(1:5) == 'PARAL' .OR. KEY2(1:6) == 'CIRCLE') THEN
          ISTYP = 1
        ELSE
          ISTYP = 0
          CALL HM_GET_INTV('Grnod_ID', IGU, IS_AVAILABLE, LSUBMODEL)  
          CALL HM_GET_INTV('System_Id', NFRAM, IS_AVAILABLE, LSUBMODEL)
        ENDIF

        IGUQ=0                       
        CALL HM_GET_INTV('grbrick_id', IGUS, IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('grshel_id', IGUC, IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('grtrus_id', IGUT, IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('grbeam_id', IGUP, IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('grsprg_id', IGUR, IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('grtria_id', IGUTG, IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('Niter', NBINTER, IS_AVAILABLE, LSUBMODEL)  
C---    
        IF (FLAG_R2R == 1) THEN
C----------tag des noeuds du skew de la saection---------------
	  IF (N1 /= 0) N1=USR2SYS(N1,ITABM1,MESS,ID)
          IF (N2 /= 0) N2=USR2SYS(N2,ITABM1,MESS,ID)
          IF (N3 /= 0) N3=USR2SYS(N3,ITABM1,MESS,ID)
	  IF(TAGNO(N1+NPART) < 2) TAGNO(N1+NPART) = 2
	  IF(TAGNO(N2+NPART) < 2) TAGNO(N2+NPART) = 2
	  IF(TAGNO(N3+NPART) < 2) TAGNO(N3+NPART) = 2
        ELSEIF (FLAG_R2R == 2) THEN
C----------determination de la position de la section par rapport  l'interface multidomaines---------------        
C----------groupe d'elements
           NSEG  = GRSIZE_ELE(IGUS,IGRBRIC,NGRBRIC)
           NSEGC = GRSIZE_R2R(IGUS,IGRBRIC,NGRBRIC,9)
           NSEG0 = GRSIZE_R2R(IGUS,IGRBRIC,NGRBRIC,8)
C
           NSEG  = NSEG  + GRSIZE_ELE(IGUQ,IGRQUAD,NGRQUAD)
           NSEGC = NSEGC + GRSIZE_R2R(IGUQ,IGRQUAD,NGRQUAD,9)  
           NSEG0 = NSEG0 + GRSIZE_R2R(IGUQ,IGRQUAD,NGRQUAD,8)
C
           NSEG  = NSEG  + GRSIZE_ELE(IGUC,IGRSH4N,NGRSHEL)
           NSEGC = NSEGC + GRSIZE_R2R(IGUC,IGRSH4N,NGRSHEL,9)           
           NSEG0 = NSEG0 + GRSIZE_R2R(IGUC,IGRSH4N,NGRSHEL,8)
C
           NSEG  = NSEG  + GRSIZE_ELE(IGUT,IGRTRUSS,NGRTRUS)
           NSEGC = NSEGC + GRSIZE_R2R(IGUT,IGRTRUSS,NGRTRUS,9)            
           NSEG0 = NSEG0 + GRSIZE_R2R(IGUT,IGRTRUSS,NGRTRUS,8)
C
           NSEG  = NSEG  + GRSIZE_ELE(IGUP,IGRBEAM,NGRBEAM)
           NSEGC = NSEGC + GRSIZE_R2R(IGUP,IGRBEAM,NGRBEAM,9)
           NSEG0 = NSEG0 + GRSIZE_R2R(IGUP,IGRBEAM,NGRBEAM,8)                        
C
           NSEG  = NSEG  + GRSIZE_ELE(IGUR,IGRSPRING,NGRSPRI)
           NSEGC = NSEGC + GRSIZE_R2R(IGUR,IGRSPRING,NGRSPRI,9)
           NSEG0 = NSEG0 + GRSIZE_R2R(IGUR,IGRSPRING,NGRSPRI,8)                        
C
           NSEG  = NSEG  + GRSIZE_ELE(IGUTG,IGRSH3N,NGRSH3N)
           NSEGC = NSEGC + GRSIZE_R2R(IGUTG,IGRSH3N,NGRSH3N,9)
           NSEG0 = NSEG0 + GRSIZE_R2R(IGUTG,IGRSH3N,NGRSH3N,8)                         
C                                                                                   
           TAGSEC(I)=ID
           COMPT = COMPT + 1
C-----------Critre de tri
           IF ((NSEGC > 0).OR.((NSEG.NE.NSEG0).AND.(NSEG > 0))) THEN
C-------------Section sur l'interface multidomains
              CALL ANCMSG(MSGID=1006,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID)         
           ELSEIF ((NSEG == 0).AND.(NSEG0 > 0)) THEN
C-------------Section externe : non conserve
             TAGSEC(I)=0
             COMPT = COMPT - 1                          
           ENDIF                                                                                                               
        ELSE           
C---    
        IF (NFRAM == 0 .AND. ISTYP == 0) THEN
	         NNOD  = GRSIZEN(IGU,IGRNOD,NGRNOD)
       	ELSE
          NNOD = 20 * GRSIZE_ELE(IGUS,IGRBRIC,NGRBRIC)	   
          NNOD = NNOD + 4 * GRSIZE_ELE(IGUQ,IGRQUAD,NGRQUAD)	   
          NNOD = NNOD + 4 * GRSIZE_ELE(IGUC,IGRSH4N,NGRSHEL)	   
          NNOD = NNOD + 2 * GRSIZE_ELE(IGUT,IGRTRUSS,NGRTRUS)	   
          NNOD = NNOD + 2 * GRSIZE_ELE(IGUP,IGRBEAM,NGRBEAM)	   
          NNOD = NNOD + 2 * GRSIZE_ELE(IGUR,IGRSPRING,NGRSPRI)	   
          NNOD = NNOD + 3 * GRSIZE_ELE(IGUTG,IGRSH3N,NGRSH3N)
	ENDIF
        NSEGS = GRSIZE_ELE(IGUS,IGRBRIC,NGRBRIC)        
        NSEGQ = GRSIZE_ELE(IGUQ,IGRQUAD,NGRQUAD)        
        NSEGC = GRSIZE_ELE(IGUC,IGRSH4N,NGRSHEL)        
        NSEGT = GRSIZE_ELE(IGUT,IGRTRUSS,NGRTRUS)        
        NSEGP = GRSIZE_ELE(IGUP,IGRBEAM,NGRBEAM) 
        NSEGR = GRSIZE_ELE(IGUR,IGRSPRING,NGRSPRI)
        IF (NB_SEATBELT_SHELLS .NE.0)
     .    NSEGR = NSEGR + GRSIZE_ELE_TRANS(IGUC,IGRSH4N,NGRSHEL,SEATBELT_SHELL_TO_SPRING)
        NSEGTG = GRSIZE_ELE(IGUTG,IGRSH3N,NGRSH3N)      
C---    
        SNSTRF = SNSTRF +30 + NBINTER + NNOD + 2*(NSEGS+NSEGQ+NSEGC+NSEGT+NSEGP+NSEGR+NSEGTG)
        SSECBUF=SSECBUF+10
        IF(ISAV >= 100) SSECBUF=SSECBUF+12*NNOD
        IF(ISAV >= 101) SSECBUF=SSECBUF+12*NNOD
        IF(ISAV >= 102) SSECBUF=SSECBUF+6*NNOD 
        ENDIF            
      ENDDO
C      
      IF (FLAG_R2R == 2) NSECT = COMPT
            
C-----------
      RETURN
C-----------
 999  CALL FREERR(3)
      RETURN
C-----------
      END
C-----------
