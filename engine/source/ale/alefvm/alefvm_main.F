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
Chd|  ALEFVM_MAIN                   source/ale/alefvm/alefvm_main.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ALEFVM_SCHEME                 source/ale/alefvm/alefvm_scheme.F
Chd|        ALEFVM_SFINT3                 source/ale/alefvm/alefvm_sfint3.F
Chd|        ALEFVM_SFINT3_INT22           source/ale/alefvm/alefvm_sfint3_int22.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE ALEFVM_MAIN(
     1         PM        , GEO       ,X         ,A        ,AR       ,V       ,
     2         MS        , WA        ,ELBUF_TAB ,BUFMAT   ,PARTSAV  ,TF      ,
     3         VEUL      , FV        ,FSKY      ,VR       ,
     4         SKEW      , W         ,
     5         D         , DT2T      ,DT2SAVE   ,ALE_CONNECT    ,
     6         IPARG     , NPC       ,IXS       ,IXQ      ,IADS     ,IADQ    ,
     7         NALE      , ISKEW     ,
     8         NELTST    , ITYPTST   ,IPART     ,IPARTS   ,IPARTQ   ,ITASK   ,
     A         NODFT     , NODLT     ,NBRCVOIS  ,
     B         NBSDVOIS  , LNRCVOIS  ,LNSDVOIS  ,NERCVOIS ,NESDVOIS ,LERCVOIS,
     C         LESDVOIS  , ISIZXV    ,IAD_ELEM  ,FR_ELEM  ,FSKYM    ,MSNF    ,
     D         IPARI     , SEGVAR    ,ITAB      ,ISKWN    ,
     E         FSAV      ,
     F         WEIGHT    , NPSEGCOM  ,LSEGCOM   ,IPM      ,IGEO     ,
     G         ITABM1    ,NV46     )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C 'alefvm' is related to a collocated scheme (built from FVM and based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandoned, and is not an official option.
C There is no other use for this scheme which is automatically enabled when /INTER/TYPE22 is defined (INT22>0 => IALEFVM=1).
C
C This subroutine is treating an uncut cell.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE INTBUFDEF_MOD
      USE ALEFVM_MOD
      USE I22TRI_MOD
      USE SEGVAR_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXQ(NIXQ,*),
     .   NPC(*), IPARG(NPARG,*),
     .   IADS(8,*),IADQ(4,*),NALE(*),ISKEW(*), 
     .   IGEO(NPROPGI,*),
     .   IPART(LIPART1,*) ,IPARTS(*) ,IPARTQ(*),IPM(NPROPMI,*),NODFT,
     .   NELTST ,ITYPTST, ITASK, 
     .   NBRCVOIS(*),NBSDVOIS(*), LNRCVOIS(*), LNSDVOIS(*),
     .   NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),
     .   NPSEGCOM(*),LSEGCOM(*),
     .   IAD_ELEM(*),FR_ELEM(*), IPARI(*),ITAB(*),ISKWN(*),
     .   WEIGHT(*), ITABM1(*),
     .   ISIZXV, NV46,
     .   NODLT
           
      my_real
     .   X(3,*)    ,V(3,*)   ,MS(*)   ,PM(NPROPM,*),SKEW(LSKEW,*),
     .   GEO(NPROPG,*),BUFMAT(*) ,W(3,*)    ,VEUL(*),
     .   TF(*) ,
     .   FV(*)     ,A(3,*)    ,AR(3,*)     ,
     .   PARTSAV(*) ,D(3,*),WA(*),
     .   FSKY(*), FSKYM(*),
     .   DT2T, DT2SAVE,
     .   MSNF(*),FSAV(NTHVKI,*),
     .   VR(3,*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(t_segvar) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NG, NF1,
     .   ISOLNOD,   NEL,
     .   ISTRA
      INTEGER IPLA
      INTEGER IALEFVM_FLG, IMAT,NSG,NVC
      my_real
     .   BID
      
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_)  ,POINTER  :: LBUF
            
      INTEGER :: NIN,NBF,NBL,tNB
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------       
      IF(ALEFVM_Param%IEnabled==0)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------  

      !---------------------------------------------------------!
      ! 2.  INIT/PARAMETERS                                     !
      !---------------------------------------------------------!
      NIN            = 1
      BID            = ZERO
      !---------------------------------------------------------!
      ! 3.  INIT/MULTITHREADING                                 !
      !---------------------------------------------------------!
      NBF = 1+ITASK*NB/NTHREAD
      NBL = (ITASK+1)*NB/NTHREAD
      NBL = MIN(NBL,NB)
      tNB = NBL-NBF+1

      CALL MY_BARRIER
      
      IF(INT22/=0)THEN
         CALL ALEFVM_SFINT3_INT22
     .                     (                                                            
     1                      IXS     , NV46    ,  IALEFVM_FLG, ITASK,          
     2                      IPM     , IPARG   ,  NG      , NALE       , A    ,          
     3                      X       , V       ,  BID     , BID        , MS   ,          
     4                      BID     , BID     ,  W       , NBF        , NBL  ,
     5                      tNB     , NIN                                  
     .                     )  
  
      ENDIF


      !---------------------------------------------                                        
      ! ALEFVM : FINITE VOLUME FOR MOMENTUM                                                 
      !--------------------------------------------- 

       ALEFVM_Buffer%VERTEX(1:4,1:NUMNOD) = ZERO ! for alefvm_expand_momentum.F => vel on nodes           

       IF(ALEFVM_Param%Ienabled > 0) THEN                                                                 
         CALL MY_BARRIER 
                                                                                    
         DO NG=ITASK+1,NGROUP,NTHREAD                                                       
           CALL INITBUF (                                                                   
     1                   IPARG    ,NG      ,                                                
     2                   MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,                      
     3                   NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,                      
     4                   JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,                      
     5                   NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,IPLA    ,                      
     6                   IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,                      
     7                   ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS                           
     .                  )                                                                   
           GBUF => ELBUF_TAB(NG)%GBUF                                                      
           IF (IPARG(8,NG) == 1) CYCLE                                                      
           IF (JLAG == 1 .OR. ITY>2) CYCLE                                               
           NSG           = IPARG(10,NG)                                                     
           NVC           = IPARG(19,NG)                                                     
           ISOLNOD       = IPARG(28,NG)                                                     
           ISTRA         = IPARG(44,NG)                                                     
           JSPH          = 0                                                                
           ISPH2SOL      = 0                                                                
           IPARTSPH      = IPARG(69,NG)                                                     
           LFT           = 1                                                                
           LLT           = NEL                                                              
           NF1           = NFT+1                                                            
           IF(ITY == 1 .AND. ISOLNOD == 4)THEN                                              
             !CALL S4FORC3()                                                                
                                                                                            
           ELSEIF(ITY == 1 .AND. ISOLNOD /= 4)THEN                                                    
             LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)                                                      
             IMAT        = IXS(1,NFT+LFT) !all elem in group does have same material id.    
             IALEFVM_FLG = IPM(251,IMAT)                                                    
             IF (IALEFVM_FLG <= 1) CYCLE                                                  
             CALL ALEFVM_SFINT3(                                                            
     1                          IXS     , NV46    ,  ALE_CONNECT   , IALEFVM_FLG, ITASK,          
     2                          IPM     , IPARG   ,  NG      , NALE       , A    ,          
     3                          X       , V       ,  GBUF%RHO, GBUF%VOL   , MS   ,          
     4                          GBUF%MOM, GBUF%SIG,  W       , GBUF%TAG22 ,NEL   )
             CALL ALEFVM_SCHEME(                                                            
     1                          IXS     , NV46    ,  IALEFVM_FLG, ITASK      ,          
     2                          GBUF%MOM, GBUF%VOL,  GBUF%RHO, V          , VEUL       ,          
     3                          IPM     , MSNF    ,  X       , NALE       , GBUF%TAG22 ,
     4                          LBUF%SSP,GBUF%SIG ,  NEL     )
           ELSEIF (ITY == 2 .AND. JMULT == 0) THEN                                          
             !CALL QFORC2()                                                                 
           ELSEIF(ITY == 2 .AND. JMULT /= 0)THEN                                            
             !CALL BFORC2()                                                                 
           ENDIF                                                                            
         ENDDO!next NG                                                                      
       ENDIF!IF (ILAW11 /= 0) THEN                                                          

       CALL MY_BARRIER
       

 
       DO N=NODFT,NODLT
         IF(ALEFVM_Buffer%VERTEX(4,N) == ZERO .OR. NALE(N)==0)CYCLE
         V(1,N)      = ALEFVM_Buffer%VERTEX(1,N) 
         V(2,N)      = ALEFVM_Buffer%VERTEX(2,N) 
         V(3,N)      = ALEFVM_Buffer%VERTEX(3,N)            
       ENDDO
            
#include "vectorize.inc"
        !---TRANSLATIONS---!
       DO N=NODFT,NODLT
         IF(MSNF(N)<=ZERO)CYCLE
         IF(NALE(N)==0)CYCLE
         V(1,N)    = V(1,N) / MSNF(N)
         V(2,N)    = V(2,N) / MSNF(N)
         V(3,N)    = V(3,N) / MSNF(N)
       ENDDO
       IF (IRODDL/=0) THEN
#include "vectorize.inc"
         !---TROTATIONS---!
         DO N=NODFT,NODLT
           IF(NALE(N)==0)CYCLE
           VR(1,N) = ZERO
           VR(2,N) = ZERO
           VR(3,N) = ZERO      
         ENDDO
       ENDIF

       CALL MY_BARRIER
       ALEFVM_Buffer%VERTEX(4,1:NUMNOD) = ZERO ! was needed for alefvm_expand_momentum.F reset here for gravity on next cycle

      RETURN
      END
