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
Chd|  DOUBLE_FLOT_IEEE              source/elements/shell/coque/cinmas.F
Chd|-- called by -----------
Chd|        CINMAS                        source/elements/shell/coque/cinmas.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DOUBLE_FLOT_IEEE(JFT  ,JLT  ,I8 ,R8, I8F)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      integer*8 I8(*),I8F(3,*)
      my_real
     .   R8(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c___________________________________________________          
      double precision
     .    R8_LOCAL,R8_DEUXP43,AA
      INTEGER*8 I8_DEUXP43
      DATA I8_DEUXP43 /'80000000000'x/
      DATA R8_DEUXP43 /'42A0000000000000'x/
      INTEGER I
c___________________________________________________          
C-----------------------------------------------
C
      DO I=JFT,JLT
c___________________________________________________          
          I8F(1,I)   = R8(I)
          AA         = I8F(1,I)
          R8_LOCAL   = (R8(I)    - AA) * R8_DEUXP43
          I8F(2,I)   = R8_LOCAL
          AA         = I8F(2,I)
          R8_LOCAL   = (R8_LOCAL - AA) * R8_DEUXP43
          I8F(3,I)   = R8_LOCAL + HALF
      ENDDO
c___________________________________________________          
      RETURN
      END
C
Chd|====================================================================
Chd|  CINMAS                        source/elements/shell/coque/cinmas.F
Chd|-- called by -----------
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DOUBLE_FLOT_IEEE              source/elements/shell/coque/cinmas.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE CINMAS(
     .          X       ,XREFC   ,IX      ,GEO      ,PM       ,
     .          MS      ,TINER   ,THKE    ,IHBE     ,PARTSAV  ,     
     .          V       ,IPART   ,MSC     ,INC      ,AREA     ,         
     .          I8MI    ,IGEO    ,ETNOD   ,IMID     ,IPROP    ,           
     .          NSHNOD  ,STC     ,SH4TREE ,MCP      ,MCPS     ,         
     .          TEMP    ,MS_LAYER,ZI_LAYER,MS_LAYERC,ZI_LAYERC,
     .          MSZ2C   ,ZPLY    ,ISUBSTACK,NLAY    ,ELBUF_STR,
     .          STACK   ,THKI    ,RNOISE   ,DRAPE   ,
     .          PERTURB ,IX1     ,IX2      ,IX3     ,IX4      ,
     .          IDRAPE  ,INDX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
      USE STACK_MOD
      USE DRAPE_MOD
C----------------------------------------------
C     INITIALISATION DES MASSES ET INERTIES NODALES
C----------------------------------------------
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "remesh_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "scr22_c.inc"
#include      "vect01_c.inc"
#include      "drape_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IX(NIXC,*),IPART(*),IHBE,IMID,IPROP,
     .        IGEO(NPROPGI,*),NSHNOD(*), SH4TREE(KSH4TREE,*),
     .        ISUBSTACK,NLAY,PERTURB(NPERTURB),IDRAPE
      INTEGER, DIMENSION(MVSIZ), INTENT(IN)  :: IX1,IX2,IX3,IX4
      INTEGER*8 I8MI(6,*) 
      INTEGER, DIMENSION(SCDRAPE) :: INDX
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: AREA
      my_real
     .   X(3,*), GEO(NPROPG,*), PM(NPROPM,*), MS(*), TINER(*),THKE(*),
     .   V(3,*),PARTSAV(20,*),MSC(*),INC(*),
     .   ETNOD(*), STC(*),MCP(*),MCPS(*),TEMP(*),
     .   MS_LAYER(NUMNOD,*), ZI_LAYER(NUMNOD,*),XREFC(4,3,*),
     .   MS_LAYERC(NUMELC,*),ZI_LAYERC(NUMELC,*),MSZ2C(*),ZPLY(*),THKI(*),
     .   RNOISE(NPERTURB,*)
      my_real
     .   ZSHIFT
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_) , DIMENSION(NUMELC_DRAPE+NUMELTG_DRAPE), TARGET :: DRAPE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,K,N,IP,I1,I2,I3,I4,MATLY,IPTHK,IPMAT,IPPOS,ID,   
     .   ERRM,IDPROP,IPID,IPPID,ISTACK(MVSIZ,NPT),ISLV,IPID_LY,
     .   IPANG,NTHK,IGTYP,IGMAT,ILAY,NPTT,IT,IINT,IPGMAT,MAX_NPTT,
     .   NSLICE,IPOS,IPT,IPT_ALL,IE,NLAY_MAX
      INTEGER*8 I8EMS(3,MVSIZ) 
      CHARACTER*nchartitle, TITR
      my_real 
     .   FAC,MST,DMS,THICKT,THKLY,LAYPOS,MSE,INS,MZI,EMS_NPTT,ET,ZJ,
     .   XX,YY,ZZ,XY,YZ,ZX,MSL,XIL,MS_NPTT,POS_0,THINNING
      my_real 
     .   EMS_LAYER(MVSIZ,NLAY)
      my_real, DIMENSION(:), ALLOCATABLE  :: POSLY,RATIO_THKLY, THK_IT,POS_IT
      my_real, DIMENSION(MVSIZ) :: RHO,RHOCP,EMS,EMSCP,XI,THK
C

      TYPE (DRAPE_PLY_),  POINTER :: DRAPE_PLY
      my_real A_GAUSS(9,9),W_GAUSS(9,9)
C-----------------------------------------------
      DATA A_GAUSS /
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/
      DATA W_GAUSS /
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.555555555555556,0.888888888888889,0.555555555555556,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.347854845137454,0.652145154862546,0.652145154862546,
     4 0.347854845137454,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.236926885056189,0.478628670499366,0.568888888888889,
     5 0.478628670499366,0.236926885056189,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.171324492379170,0.360761573048139,0.467913934572691,
     6 0.467913934572691,0.360761573048139,0.171324492379170,
     6 0.               ,0.               ,0.               ,
     7 0.129484966168870,0.279705391489277,0.381830050505119,
     7 0.417959183673469,0.381830050505119,0.279705391489277,
     7 0.129484966168870,0.               ,0.               ,
     8 0.101228536290376,0.222381034453374,0.313706645877887,
     8 0.362683783378362,0.362683783378362,0.313706645877887,
     8 0.222381034453374,0.101228536290376,0.               ,
     9 0.081274388361574,0.180648160694857,0.260610696402935,
     9 0.312347077040003,0.330239355001260,0.312347077040003,
     9 0.260610696402935,0.180648160694857,0.081274388361574/
C=======================================================================
      IGTYP=NINT(GEO(12,IPROP)) 
      IGMAT = IGEO(98,IPROP)
        IPANG  =  1   !  NPT
        IPTHK  =  IPANG + NLAY   !  NPT
        IPPOS  =  IPTHK + NLAY   !  NPT
 
C    
       MAX_NPTT   = 1   
       IF(IGTYP == 51 .OR. IGTYP == 52) THEN
          DO ILAY=1,NLAY   !  NPT  
              MAX_NPTT = MAX(MAX_NPTT ,ELBUF_STR%BUFLY(ILAY)%NPTT)
          ENDDO
          ALLOCATE(THK_IT(MAX_NPTT),POS_IT(MAX_NPTT))
       ELSE
          ALLOCATE(THK_IT(0),POS_IT(0))  
       ENDIF 
       NLAY_MAX = MAX(NLAY, NPT)
       ALLOCATE(POSLY(NLAY_MAX*MAX_NPTT),RATIO_THKLY(NLAY_MAX*MAX_NPTT)) 
c-----------------------------
      IF ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT < 0 ) THEN
c-----------------------------
        DO I=LFT,LLT
          IF (NDRAPE == 0) THEN
            IF (THKE(I)== ZERO) THEN          
              THK(I) = STACK%GEO(1,ISUBSTACK)
              THKE(I)= THK(I)
            ELSE
              THK(I)=THKE(I)
            ENDIF
            THKI(I) = STACK%GEO(1,ISUBSTACK)
          ELSEIF (NDRAPE > 0) THEN
            THK(I) = THKE(I)
            THKI(I)= THK(I)
          ENDIF ! IF (NDRAPE == 0)
c      
           IF (NPERTURB /= 0) THEN
            DO J=1,NPERTURB
             IF (PERTURB(J) == 1 .AND. RNOISE(J,I+NFT) /= ZERO) THEN
               THK(I)  = THK(I)  * RNOISE(J,I+NFT)
               THKE(I) = THKE(I) * RNOISE(J,I+NFT)
               THKI(I) = THKI(I) * RNOISE(J,I+NFT)
             ENDIF
            ENDDO
           ENDIF
c      
           RHO(I)   = PM(89,IMID)
           RHOCP(I) = PM(69,IMID) 
           IF(THK(I) == ZERO.AND.IGTYP/=0)THEN
              CALL ANCMSG(MSGID=495,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=IX(7,NFT+I))
           ENDIF  
        ENDDO
c-----------------------------
      ELSEIF (IGTYP == 52 .OR. 
     .      ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
c-----------------------------
        DO I=LFT,LLT
          IF (NDRAPE == 0) THEN
            IF (THKE(I)== ZERO) THEN          
              THK(I) = STACK%GEO(1,ISUBSTACK)
              THKE(I)= THK(I)
            ELSE
              THK(I)=THKE(I)
            ENDIF
            THKI(I) = STACK%GEO(1,ISUBSTACK)
          ELSEIF (NDRAPE > 0) THEN
            THK(I) = THKE(I)
            THKI(I)= THK(I)
          ENDIF ! IF (NDRAPE == 0)
           RHO(I)   = STACK%PM(1,ISUBSTACK)
           RHOCP(I) = STACK%PM(15,ISUBSTACK)* RHO(I)/STACK%PM(14,ISUBSTACK)
c      
           IF (NPERTURB /= 0) THEN
             DO J=1,NPERTURB
               IF (PERTURB(J) == 1 .AND. RNOISE(J,I+NFT) /= ZERO) THEN
                 THK(I)  = THK(I)  * RNOISE(J,I+NFT)
                 THKE(I) = THKE(I) * RNOISE(J,I+NFT)
                 THKI(I) = THKI(I) * RNOISE(J,I+NFT)
               ENDIF
             ENDDO
           ENDIF
c      
           IF(THK(I) == ZERO.AND.IGTYP/=0)THEN
              CALL ANCMSG(MSGID=495,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=IX(7,NFT+I))
           ENDIF  
         ENDDO
c-----------------------------
      ELSE  ! IGTYP : std shell property
c-----------------------------
          DO I=LFT,LLT
             
            IF (THKE(I) == ZERO) THEN          
              THK(I) = GEO(1,IPROP)
              THKE(I)= THK(I)
            ELSE
              THK(I)=THKE(I)
            ENDIF 
c                            
            THKI(I) = GEO(1,IPROP)
c      
            IF (NPERTURB /= 0) THEN
             DO J=1,NPERTURB
                IF (PERTURB(J) == 1 .AND. RNOISE(J,I+NFT) /= ZERO) THEN
                  THK(I)  = THK(I)  * RNOISE(J,I+NFT)
                  THKE(I) = THKE(I) * RNOISE(J,I+NFT)
                  THKI(I) = THKI(I) * RNOISE(J,I+NFT)
                ENDIF
              ENDDO
            ENDIF
c
            RHO(I)   = PM(89,IMID)
            RHOCP(I) = PM(69,IMID)
            IF(THK(I) == ZERO.AND.IGTYP/=0)THEN
               CALL ANCMSG(MSGID=495,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=IX(7,NFT+I))
            ENDIF  
          ENDDO
        ENDIF 
C----------------------------------------------
C     MASSES ELEMENTS / 4
C----------------------------------------------
      IF (IGTYP == 11 .OR. IGTYP == 16) THEN
c--------------------------------------------
        IPTHK = 300
        IPMAT = 100
        DO I=LFT,LLT
          EMS(I)=ZERO
          DO N=1,NPT
            I1=IPTHK+N
            I2=IPMAT+N
            THICKT = GEO(200,IPROP)
            THKLY  = GEO(I1,IPROP)*THICKT
            MATLY  = IGEO(I2,IPROP)
            EMS(I) = EMS(I) + PM(89,MATLY)*THKLY*AREA(I)*FOURTH
          ENDDO
          MST = RHO(I)*THK(I)*AREA(I)*FOURTH
          DMS = ABS(EMS(I)-MST)/MST
C! for igty=11 ---> IGMAT=-1 (old G-mat), 1 for new g-mat
C! for igtyp=16 --->IGMAT= 0
C! for igtyp=17 --->IGMAT= 0
          IF(IGMAT <= 0) THEN
             IF (DMS > EM02) ERRM = 1
             IF (DMS > EM02) THEN
                IDPROP = IGEO(1,IPROP)
                CALL FRETITL2(TITR,
     .                    IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
                 CALL ANCMSG(MSGID=519,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=IDPROP,
     .                  C1=TITR,I2=IX(NIXC,NFT+I),
     .                  R1=EMS(I),R2=MST)
              ENDIF 
           ENDIF            
        ENDDO
c--------------------------------------------
      ELSEIF(IGTYP == 17 ) THEN
c--------------------------------------------
        IPPID   = 2 
        IPMAT   = IPPID + NPT
        IPANG  =  1
        IPTHK  =  IPANG + NPT
        IPPOS  =  IPTHK + NPT
        NTHK   =  IPPOS + NPT 
C       
        IF(IDRAPE == 0) THEN
           DO I=LFT,LLT
             EMS(I)=ZERO
             MZI = ZERO
             DO N=1,NPT
               I1 = IPPID + N
               I2 = IPMAT + N
               I3 = IPTHK + N
               I4 = IPPOS + N
               THICKT = STACK%GEO(1 ,ISUBSTACK)
               THKLY  = STACK%GEO(I3 ,ISUBSTACK)*THICKT
               MATLY  = STACK%IGEO(I2,ISUBSTACK)
               EMS(I) = EMS(I) + PM(89,MATLY)*THKLY*AREA(I)*FOURTH
               EMS_LAYER(I,N) =  PM(89,MATLY)*THKLY*AREA(I)*FOURTH
               MZI = MZI + EMS_LAYER(I,N)*STACK%GEO(I4 ,ISUBSTACK)
               IPID   = STACK%IGEO(I1, ISUBSTACK)
               ISTACK(I,N) = IGEO(102,IPID)
             ENDDO
             MST = RHO(I)*THK(I)*AREA(I)*FOURTH
             DMS = ABS(EMS(I)-MST)/MST
             IF(DMS  > EM02) ERRM = 1
             IF(IGMAT <= 0) THEN  
                  IF(DMS  >  EM02) THEN
                    IDPROP = IGEO(1,IPROP)
                    CALL FRETITL2(TITR,
     .                        IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
                    CALL ANCMSG(MSGID=519,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=IDPROP,
     .                      C1=TITR,I2=IX(NIXC,NFT+I),
     .                      R1=EMS(I),R2=MST)
                  ENDIF
             ENDIF     
            !!
             DO N=1,NPT
               I4 = IPPOS + N
               IF(ABS(STACK%GEO(I4 ,ISUBSTACK)) < EM15)
     .                         STACK%GEO(I4 ,ISUBSTACK)=ZERO
             ENDDO         
             IF(ABS(MZI) > EM02) THEN
               DO N=1,NPT
                 I4 = IPPOS + N
                 DMS = MZI/EMS(I)
                 STACK%GEO(I4 ,ISUBSTACK) = STACK%GEO(I4 ,ISUBSTACK) - DMS
               ENDDO
             ENDIF                      
           ENDDO 
        ELSE ! idrape > 0
          DO I=LFT,LLT
            EMS(I)=ZERO
            MZI = ZERO
            IE = INDX(NFT+I)
            IF(IE == 0) THEN
               DO N=1,NPT
                 I1 = IPPID + N
                 I2 = IPMAT + N
                 I3 = IPTHK + N
                 I4 = IPPOS + N 
                 THICKT = STACK%GEO(1 ,ISUBSTACK)
                 THKLY  = STACK%GEO(I3 ,ISUBSTACK)*THICKT
                 MATLY  = STACK%IGEO(I2,ISUBSTACK)
                 EMS(I) = EMS(I) + PM(89,MATLY)*THKLY*AREA(I)*FOURTH
                 EMS_LAYER(I,N) =  PM(89,MATLY)*THKLY*AREA(I)*FOURTH
                 MZI = MZI + EMS_LAYER(I,N)*STACK%GEO(I4 ,ISUBSTACK)
                 IPID   = STACK%IGEO(I1, ISUBSTACK)
                 ISTACK(I,N) = IGEO(102,IPID)
               ENDDO
             ELSE
               DO N=1,NPT                                                          
                 I1 = IPPID + N                                                    
                 I2 = IPMAT + N
                 I3 = IPTHK + N
                 I4 = IPPOS + N
                 IP = DRAPE(IE)%INDX_PLY(N) 
                 IF(IP > 0 ) THEN                            
                    DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                    THINNING = DRAPE_PLY%RDRAPE(1,1) ! one slice by layer   
                    THICKT = STACK%GEO(1 ,ISUBSTACK)                        
                    THKLY  = STACK%GEO(I3 ,ISUBSTACK)*THICKT ! initial      
                    THKLY  = THKLY*THINNING                                 
                    MATLY  = STACK%IGEO(I2,ISUBSTACK)                       
                    EMS(I) = EMS(I) + PM(89,MATLY)*THKLY*AREA(I)*FOURTH     
                    EMS_LAYER(I,N) =  PM(89,MATLY)*THKLY*AREA(I)*FOURTH     
                    RATIO_THKLY(N) = THKLY/THK(I)                           
                    IF (N == 1) THEN                                        
                      POSLY(N) = -HALF + HALF*RATIO_THKLY(N)               
                    ELSE                                                    
                       POSLY(N) = POSLY(N-1)                                
     .                          + HALF*(RATIO_THKLY(N)+RATIO_THKLY(N-1))           
                    ENDIF ! IF (N == 1)                                     
                    MZI = MZI + EMS_LAYER(I,N)*POSLY(N)                     
                    IPID   = STACK%IGEO(I1, ISUBSTACK)
                    ISTACK(I,N) = IGEO(102,IPID)
                 ELSE ! IP=0   
                    THICKT = STACK%GEO(1 ,ISUBSTACK)                        
                    THKLY  = STACK%GEO(I3 ,ISUBSTACK)*THICKT ! initial 
                    MATLY  = STACK%IGEO(I2,ISUBSTACK)                       
                    EMS(I) = EMS(I) + PM(89,MATLY)*THKLY*AREA(I)*FOURTH     
                    EMS_LAYER(I,N) =  PM(89,MATLY)*THKLY*AREA(I)*FOURTH     
                    RATIO_THKLY(N) = THKLY/THK(I)                           
                    IF (N == 1) THEN                                        
                      POSLY(N) = -HALF + HALF*RATIO_THKLY(N)               
                    ELSE                                                    
                       POSLY(N) = POSLY(N-1)                                
     .                          + HALF*(RATIO_THKLY(N)+RATIO_THKLY(N-1))     
                    ENDIF ! IF (N == 1)                                     
                    MZI = MZI + EMS_LAYER(I,N)*POSLY(N)                     
                    IPID   = STACK%IGEO(I1, ISUBSTACK)
                    ISTACK(I,N) = IGEO(102,IPID)
                 ENDIF
               ENDDO
             ENDIF ! IE
             MST = RHO(I)*THK(I)*AREA(I)*FOURTH                   
             DMS = ABS(EMS(I)-MST)/MST                            
             IF(DMS  > EM02) ERRM = 1                            
             IF(IGMAT <= 0) THEN                                 
                 IF(DMS  >  EM02) THEN                           
                   IDPROP = IGEO(1,IPROP)                        
                   CALL FRETITL2(TITR,                           
     .                      IGEO(NPROPGI-LTITR+1,IPROP),LTITR)   
                   CALL ANCMSG(MSGID=519,                        
     .                    MSGTYPE=MSGWARNING,                    
     .                    ANMODE=ANINFO_BLIND_2,                 
     .                    I1=IDPROP,                             
     .                    C1=TITR,I2=IX(NIXC,NFT+I),             
     .                    R1=EMS(I),R2=MST)                      
                 ENDIF                                           
             ENDIF                                               
C correction la position de la surface neutre au CDG
            DO N=1,NPT
              I4 = IPPOS + N
              IF(ABS(STACK%GEO(I4 ,ISUBSTACK)) < EM15)
     .                       STACK%GEO(I4 ,ISUBSTACK)=ZERO
            ENDDO         
            IF(ABS(MZI) > EM02) THEN
              DO N=1,NPT
                I4 = IPPOS + N
                DMS = MZI/EMS(I)
                STACK%GEO(I4 ,ISUBSTACK) = STACK%GEO(I4 ,ISUBSTACK) - DMS
              ENDDO
            ENDIF                      
          ENDDO 
        ENDIF ! IDRAPE   
c--------------------------------------------
      ELSEIF (IGTYP == 51 .OR. IGTYP == 52 ) THEN
c--------------------------------------------

        IPANG  =  1
        IPPID   = 2
        IPMAT   = IPPID + NLAY   !  NPT
        IPTHK  =  IPANG + NLAY   !  NPT
        IPPOS  =  IPTHK + NLAY   !  NPT
        NTHK   =  IPPOS + NLAY   !  NPT
C           
        IPOS = IGEO(99,IPROP)                   
        THICKT = STACK%GEO(1,ISUBSTACK)
        ZSHIFT = GEO(199,IPROP)
        IF(IPOS == 0 )THEN                           
          ZSHIFT = -HALF                             
        ELSEIF(IPOS == 2 )THEN                        
          ZSHIFT =  -  ZSHIFT /MAX(THICKT,EM20)       
        ELSEIF(IPOS == 3) THEN                        
          ZSHIFT = -ONE                               
        ELSEIF(IPOS == 4) THEN                        
          ZSHIFT = ZERO                               
        ENDIF  
        IF(IDRAPE == 0 ) THEN                                  
          DO I=LFT,LLT
            EMS(I)=ZERO
            MZI = ZERO
            IPT_ALL = 0
            DO ILAY=1,NLAY   !  NPT  
              NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
              I1 = IPPID + ILAY
              I2 = IPMAT + ILAY
              I3 = IPTHK + ILAY
              I4 = IPPOS + ILAY
              IPID = STACK%IGEO(I1,ISUBSTACK)  ! or IPID = IX(NIXC-1,NFT+I)
              IINT = IGEO(47,IPROP)
              THICKT  = STACK%GEO(1,ISUBSTACK)
              IF(IINT == 1) THEN
                   THKLY    = STACK%GEO(I3,ISUBSTACK)*THICKT
                   LAYPOS   = STACK%GEO(I4,ISUBSTACK)
                   IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                   MATLY   = STACK%IGEO(I2,ISUBSTACK)
                   DO IT=1,NPTT
                      IPT = IPT_ALL + IT                                                                    
                      THK_IT(IT) = THKLY/NPTT  ! uniform distribution of NPTT through layer
                      RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)              
                      IF (IPT  == 1)  THEN                                                                 
                         POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT)  ! integr. point "IT" position ratio
                       ELSE                                                                                 
                         POSLY(IPT) = POSLY(IPT-1)           ! integr. point "IT" position ratio            
     .                     + HALF*(RATIO_THKLY(IPT) + RATIO_THKLY(IPT-1))                            
                       ENDIF ! IF (IPT == 1)
                       POS_IT(IT) = POSLY(IPT)*THK(I) 
                    ENDDO 
              ELSEIF(IINT == 2) THEN
                   THKLY    = STACK%GEO(I3,ISUBSTACK)*THICKT
                   LAYPOS   = STACK%GEO(I4,ISUBSTACK)
                   IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                   MATLY   = STACK%IGEO(I2,ISUBSTACK)
                   DO IT=1,NPTT
                      IPT = IPT_ALL + IT                                                 
                      THK_IT(IT) = HALF*THKLY*W_GAUSS(IT,NPTT)  ! Gauss distribution of NPTT through layer
                      RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)
                      IF (IPT  == 1 ) THEN                                                                 
                         POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT) ! integr. point "IT" position ratio  
                       ELSE                                                                                 
                         POSLY(IPT) = POSLY(IPT-1)           ! integr. point "IT" position ratio            
     .                     + HALF*(RATIO_THKLY(IPT) + RATIO_THKLY(IPT-1))                 
                       ENDIF ! IF (IPT  == 1)  
                       POS_IT(IT) = POSLY(IPT)*THK(I) 
                    ENDDO 
             ENDIF  ! iint
             DO IT=1,NPTT
                  EMS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)*FOURTH
                  EMS(I) = EMS(I) + EMS_NPTT
                  MZI = MZI + EMS_NPTT * POS_IT(IT)/THK(I)
             ENDDO 
             IPT_ALL = IPT_ALL + NPTT
            ENDDO ! NLAY
            !!
            MST = RHO(I)*THK(I)*AREA(I)*FOURTH
            DMS = ABS(EMS(I)-MST)/MST
            IF (DMS  > EM02) ERRM = 1
            IF(IGTYP == 51 .AND. IGMAT < 0) THEN 
             IF (DMS  > EM02) THEN
                IDPROP = IGEO(1,IPROP)
               CALL FRETITL2(TITR,
     .                      IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
               CALL ANCMSG(MSGID=519,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=IDPROP,
     .                    C1=TITR,I2=IX(NIXC,NFT+I),
     .                    R1=EMS(I),R2=MST)
             ENDIF
            ENDIF 
          ENDDO ! LFT,LLT
C------
          IF( JTHE > 0 ) THEN
             DO I=LFT,LLT
                EMSCP(I)=RHOCP(I)*THK(I)*AREA(I)*FOURTH  
             ENDDO
          ENDIF
        ELSE ! idrape > 0
          DO I=LFT,LLT
            EMS(I)=ZERO
            MZI = ZERO
            IPT_ALL = 0
            IE = INDX(NFT+I)
            IF(IE == 0) THEN
             DO ILAY=1,NLAY   !  NPT  
              NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
              I1 = IPPID + ILAY
              I2 = IPMAT + ILAY
              I3 = IPTHK + ILAY
              I4 = IPPOS + ILAY
              IPID = STACK%IGEO(I1,ISUBSTACK)  ! or IPID = IX(NIXC-1,NFT+I)
              IINT = IGEO(47,IPROP)
              THICKT  = STACK%GEO(1,ISUBSTACK)
              IF(IINT == 1) THEN
                   THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT
                   LAYPOS  = STACK%GEO(I4,ISUBSTACK)
                   IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                   MATLY   = STACK%IGEO(I2,ISUBSTACK)
                   DO IT=1,NPTT
                      IPT = IPT_ALL + IT                                                                    
                      THK_IT(IT) = THKLY/NPTT  ! uniform distribution of NPTT through layer
                      RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)              
                      IF (IPT  == 1)  THEN                                                                 
                         POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT)  ! integr. point "IT" position ratio
                       ELSE                                                                                 
                         POSLY(IPT) = POSLY(IPT-1)           ! integr. point "IT" position ratio            
     .                     + HALF*(RATIO_THKLY(IPT) + RATIO_THKLY(IPT-1))                            
                       ENDIF ! IF (IPT == 1)
                       POS_IT(IT) = POSLY(IPT)*THK(I)                                                      
                    ENDDO 
              ELSEIF(IINT == 2) THEN
                   THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT
                   LAYPOS   = STACK%GEO(I4,ISUBSTACK)
                   IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                   MATLY   = STACK%IGEO(I2,ISUBSTACK)
                   DO IT=1,NPTT
                      IPT = IPT_ALL + IT                                                 
                      THK_IT(IT) = HALF*THKLY*W_GAUSS(IT,NPTT)  ! Gauss distribution of NPTT through layer
                      RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)
                      IF (IPT  == 1 ) THEN                                                                 
                         POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT) ! integr. point "IT" position ratio  
                       ELSE                                                                                 
                         POSLY(IPT) = POSLY(IPT-1)           ! integr. point "IT" position ratio            
     .                     + HALF*(RATIO_THKLY(IPT) + RATIO_THKLY(IPT-1))                 
                       ENDIF ! IF (IPT  == 1)  
                       POS_IT(IT) = POSLY(IPT)*THK(I) 
                    ENDDO 
             ENDIF  ! iint
             DO IT=1,NPTT
                  EMS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)*FOURTH
                  EMS(I) = EMS(I) + EMS_NPTT
                  MZI = MZI + EMS_NPTT * POS_IT(IT)/THK(I)
             ENDDO 
             IPT_ALL = IPT_ALL + NPTT
            ENDDO ! NLAY
         ELSE ! IE
            DO ILAY=1,NLAY   !  NPT
              IP = DRAPE(IE)%INDX_PLY(ILAY)
              NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT ! = NSLICE
              IF(IP > 0 ) THEN                             
                DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                NSLICE = DRAPE_PLY%NSLICE 
                I1 = IPPID + ILAY
                I2 = IPMAT + ILAY
                I3 = IPTHK + ILAY
                I4 = IPPOS + ILAY
                IPID = STACK%IGEO(I1,ISUBSTACK)  ! or IPID = IX(NIXC-1,NFT+I)
                IINT = IGEO(47,IPROP)
                THICKT  = STACK%GEO(1,ISUBSTACK)
                IF(IINT == 1) THEN
                     THKLY   = STACK%GEO(I3 ,ISUBSTACK)*THICKT ! initial
                     IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                     MATLY   = STACK%IGEO(I2,ISUBSTACK) 
                     DO IT=1,NPTT
                       IPT = IPT_ALL + IT
                       THINNING = DRAPE_PLY%RDRAPE(IT,1)
                       THK_IT(IT)   = THKLY*THINNING/NPTT
                       RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)
                       IF (IPT  == 1) THEN
                         POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT)
                       ELSE
                         POSLY(IPT) = POSLY(IPT-1)            
     .                     + HALF*(RATIO_THKLY(IPT)+RATIO_THKLY(IPT-1))
                       ENDIF ! IF (IPT ==  1)
                       POS_IT(IT)   = POSLY(IPT)*THK(I)
                     ENDDO  ! nptt
                ELSEIF(IINT == 2) THEN
                     THKLY   = STACK%GEO(I3 ,ISUBSTACK)*THICKT ! initial
                     IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                     MATLY   = STACK%IGEO(I2,ISUBSTACK) 
                     DO IT=1,NPTT
                       IPT = IPT_ALL + IT
                       THINNING     = DRAPE_PLY%RDRAPE(IT,1)
                       THK_IT(IT)   = HALF*THKLY*W_GAUSS(IT,NPTT)*THINNING
                       RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)
                       IF (IPT == 1) THEN
                         POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT)
                       ELSE
                         POSLY(IPT) = POSLY(IPT-1)            
     .                     + HALF*(RATIO_THKLY(IPT)+RATIO_THKLY(IPT-1))
                       ENDIF ! IF (IPT  == 1)
                       POS_IT(IT)   = POSLY(IPT)*THK(I)
                     ENDDO  ! nptt      
                 ENDIF  ! iint
                 
                 DO IT=1,NPTT
                    EMS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)*FOURTH
                    EMS(I) = EMS(I) + EMS_NPTT
                    MZI = MZI + EMS_NPTT * POS_IT(IT)/THK(I)
                 ENDDO  
              ELSE ! Ip not draped
                NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                I1 = IPPID + ILAY
                I2 = IPMAT + ILAY
                I3 = IPTHK + ILAY
                I4 = IPPOS + ILAY
                IPID = STACK%IGEO(I1,ISUBSTACK)  ! or IPID = IX(NIXC-1,NFT+I)
                IINT = IGEO(47,IPROP)
                THICKT  = STACK%GEO(1,ISUBSTACK)
                IF(IINT == 1) THEN
                     THKLY   = STACK%GEO(I3 ,ISUBSTACK)*THICKT ! initial
                     IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                     MATLY   = STACK%IGEO(I2,ISUBSTACK) 
                     DO IT=1,NPTT
                       IPT = IPT_ALL + IT
                       THK_IT(IT)   = THKLY/NPTT
                       RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)
                       IF (IPT  == 1) THEN
                         POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT)
                       ELSE
                         POSLY(IPT) = POSLY(IPT-1)            
     .                     + HALF*(RATIO_THKLY(IPT)+RATIO_THKLY(IPT-1))
                       ENDIF ! IF (IPT ==  1)
                       POS_IT(IT)   = POSLY(IPT)*THK(I)
                     ENDDO  ! nptt
                ELSEIF(IINT == 2) THEN
                     THKLY   = STACK%GEO(I3 ,ISUBSTACK)*THICKT ! initial
                     IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                     MATLY   = STACK%IGEO(I2,ISUBSTACK) 
                     DO IT=1,NPTT
                       IPT = IPT_ALL + IT
                       THK_IT(IT)   = HALF*THKLY*W_GAUSS(IT,NPTT)
                       RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)
                       IF (IPT == 1) THEN
                         POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT)
                       ELSE
                         POSLY(IPT) = POSLY(IPT-1)            
     .                     + HALF*(RATIO_THKLY(IPT)+RATIO_THKLY(IPT-1))
                       ENDIF ! IF (IPT  == 1)
                       POS_IT(IT)   = POSLY(IPT)*THK(I)
                     ENDDO  ! nptt      
                 ENDIF  ! iint
                 DO IT=1,NPTT
                    EMS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)*FOURTH
                    EMS(I) = EMS(I) + EMS_NPTT
                    MZI = MZI + EMS_NPTT * POS_IT(IT)/THK(I)
                    
                 ENDDO
              ENDIF     ! IP
              IPT_ALL = IPT_ALL + NPTT
            ENDDO ! NLAY
          ENDIF ! IE 
       
          MST = RHO(I)*THK(I)*AREA(I)*FOURTH                  
          DMS = ABS(EMS(I)-MST)/MST                           
          IF (DMS  > EM02) ERRM = 1                           
          IF(IGTYP == 51 .AND. IGMAT < 0) THEN                
           IF (DMS  > EM02) THEN                              
              IDPROP = IGEO(1,IPROP)                          
             CALL FRETITL2(TITR,                              
     .                    IGEO(NPROPGI-LTITR+1,IPROP),LTITR)  
             CALL ANCMSG(MSGID=519,                           
     .                  MSGTYPE=MSGWARNING,                   
     .                  ANMODE=ANINFO_BLIND_2,                
     .                  I1=IDPROP,                            
     .                  C1=TITR,I2=IX(NIXC,NFT+I),            
     .                  R1=EMS(I),R2=MST)                     
           ENDIF                                              
          ENDIF                                               
C                              
        ENDDO                                                          
C------
         IF( JTHE > 0 ) THEN                           
            DO I=LFT,LLT                               
               EMSCP(I)=RHOCP(I)*THK(I)*AREA(I)*FOURTH   
            ENDDO                                      
         ENDIF                                         
        ENDIF ! idrape  
c--------------------------------------------
      ELSE  ! IGTYP
c--------------------------------------------
        DO I=LFT,LLT
          EMS(I)=RHO(I)*THK(I)*AREA(I)*FOURTH
        ENDDO
C
        IF( JTHE > 0 ) THEN
          DO I=LFT,LLT
            EMSCP(I) = RHOCP(I)*THK(I)*AREA(I)*FOURTH
          ENDDO
        ENDIF  
C
      ENDIF  ! IGTYP
C----------------------------------------------
C     INITIALISATION DES MASSES NODALES
c     elt mass (parith/on spmd + adaptive meshing) 
C----------------------------------------------
      IF(JTHE == 0 ) THEN
        DO I=LFT,LLT
          MSC(I)=EMS(I)       
        ENDDO
      ELSE
        DO I=LFT,LLT            
          MSC(I)=EMS(I)         
          MCPS(I) = EMSCP(I)    
        ENDDO                   
      ENDIF 
c
      IF(ISHXFEM_PLY > 0 )THEN
         IPPID   = 2
         IPMAT   = IPPID + NPT
         IPANG  =  1
         IPTHK  =  IPANG + NPT
         IPPOS  =  IPTHK + NPT
         NTHK   =  IPPOS + NPT 
         DO I=LFT,LLT
            II = NFT + I
            DO J=1,NPT
                IP = ISTACK(I,J)
                MS_LAYERC(II,IP)= EMS_LAYER(I,J)
C
                I4 = IPPOS + J  
                THICKT = STACK%GEO(1 ,ISUBSTACK) 
                ISLV = IGEO(32,IPROP) 
                IF(ISLV == 0)THEN
                  ZI_LAYERC(II,IP) = THICKT*STACK%GEO(I4 ,ISUBSTACK)
                  ZPLY(IP) = ZI_LAYERC(II,IP) 
                ENDIF  
cc                ZJ = ZI_LAYERC(II,IP)
                 ZJ = THICKT*STACK%GEO(I4 ,ISUBSTACK)
                 MSZ2C(II) = MSZ2C(II)  +  EMS_LAYER(I,J)*ZJ*ZJ
             ENDDO 
         ENDDO 
      ENDIF
C
      IF(JTHE > 0 ) THEN                                          
        IF(NINTEMP > 0 ) THEN                                   
          DO I= LFT,LLT                                          
            IF(TEMP(IX1(I))== ZERO) TEMP(IX1(I)) = PM(79,IMID)   
            IF(TEMP(IX2(I))== ZERO) TEMP(IX2(I)) = PM(79,IMID)   
            IF(TEMP(IX3(I))== ZERO) TEMP(IX3(I)) = PM(79,IMID)   
            IF(TEMP(IX4(I))== ZERO) TEMP(IX4(I)) = PM(79,IMID)   
          ENDDO                                                  
        ELSE                                                     
          DO I=LFT,LLT                                           
            TEMP(IX1(I))=  PM(79,IMID)                           
            TEMP(IX2(I))=  PM(79,IMID)                           
            TEMP(IX3(I))=  PM(79,IMID)                           
            TEMP(IX4(I))=  PM(79,IMID)                           
          ENDDO                                                  
        ENDIF                                                    
      ENDIF                                                      
C----------------------------------------------
C     INERTIES ELEMENTS /4
C----------------------------------------------
      IF(INER_9_12 /= ZERO)THEN
        FAC=INER_9_12
      ELSEIF(IHBE>=11)THEN
        FAC=TWELVE
      ELSE
        FAC=NINE
      ENDIF
      IF (IGTYP == 11 .OR. IGTYP == 16) THEN
        IPTHK = 300
        IPPOS = 400
        DO I=LFT,LLT
          XI(I)=ZERO
          DO N=1,NPT
            I1=IPTHK+N
            I3=IPPOS+N
            THICKT= GEO(200,IPROP)
            THKLY = GEO(I1,IPROP)*THICKT
            LAYPOS = GEO(I3,IPROP)*THICKT
            I2=IPMAT+N
            MATLY = IGEO(I2,IPROP)
            MSL   = PM(89,MATLY)*THKLY*AREA(I)*FOURTH
            XIL   = MSL*(AREA(I)/FAC+THKLY*THKLY*ONE_OVER_12+LAYPOS*LAYPOS)
            XI(I) = XI(I) + XIL
          ENDDO
        ENDDO
      ELSEIF(IGTYP == 17) THEN
        IPPID  = 2
        IPMAT   = IPPID + NPT
        IPANG  =  1
        IPTHK  =  IPANG + NPT
        IPPOS  =  IPTHK + NPT
        NTHK   =  IPPOS + NPT          
C         
        IF(IDRAPE == 0) THEN
           DO I=LFT,LLT
             XI(I)=ZERO
             DO N=1,NPT
               I1 = IPPID + N
               I2 = IPMAT + N
               I3 = IPTHK + N
               I4 = IPPOS + N
               THICKT= STACK%GEO(1,ISUBSTACK)
               THKLY = STACK%GEO(I3 ,ISUBSTACK )*THICKT
               LAYPOS = STACK%GEO(I4,ISUBSTACK)*THICKT
               MATLY = STACK%IGEO(I2 ,ISUBSTACK)
               MSL   = PM(89,MATLY)*THKLY*AREA(I)*FOURTH
               XIL   = MSL*(AREA(I)/FAC+THKLY*THKLY*ONE_OVER_12+LAYPOS*LAYPOS)
               XI(I) = XI(I) + XIL
             ENDDO  
           ENDDO   
         ELSE ! IDRAPE > 0
           DO I=LFT,LLT
             XI(I)=ZERO
             IE = INDX(NFT + I) 
             IF(IE == 0) THEN
               DO N=1,NPT
                 I1 = IPPID + N
                 I2 = IPMAT + N
                 I3 = IPTHK + N
                 I4 = IPPOS + N
                 THICKT= STACK%GEO(1,ISUBSTACK)
                 THKLY = STACK%GEO(I3 ,ISUBSTACK )*THICKT
                 LAYPOS = STACK%GEO(I4,ISUBSTACK)*THICKT
                 MATLY = STACK%IGEO(I2 ,ISUBSTACK)
                 MSL   = PM(89,MATLY)*THKLY*AREA(I)*FOURTH
                 XIL   = MSL*(AREA(I)/FAC+THKLY*THKLY*ONE_OVER_12+LAYPOS*LAYPOS)
                 XI(I) = XI(I) + XIL
               ENDDO  
             ELSE
               DO N=1,NPT                           
                 IP = DRAPE(IE)%INDX_PLY(N) 
                 IF(IP > 0) THEN                            
                   DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                   I1 = IPPID + N
                   I2 = IPMAT + N
                   I3 = IPTHK + N
                   I4 = IPPOS + N
                   THINNING = DRAPE_PLY%RDRAPE(1,1)                      
                   THICKT= STACK%GEO(1,ISUBSTACK)                        
                   THKLY  = STACK%GEO(I3 ,ISUBSTACK)*THICKT ! initial    
                   THKLY  = THKLY*THINNING                               
                   MATLY  = STACK%IGEO(I2,ISUBSTACK)                     
                   RATIO_THKLY(N) = THKLY/THK(I)                         
                   IF (N == 1) THEN                                      
                     POSLY(N) = -HALF + HALF*RATIO_THKLY(N)              
                   ELSE                                                  
                    POSLY(N) = POSLY(N-1)                               
     .                 + HALF*(RATIO_THKLY(N)+RATIO_THKLY(N-1))          
                   ENDIF ! IF (N == 1)                                   
                   LAYPOS = POSLY(N)*THK(I)                              
                   MSL   = PM(89,MATLY)*THKLY*AREA(I)*FOURTH
                   XIL   = MSL*(AREA(I)/FAC+THKLY*THKLY*ONE_OVER_12+LAYPOS*LAYPOS)
                   XI(I) = XI(I) + XIL
                 ELSE !IP ==0
                   I1 = IPPID + N
                   I2 = IPMAT + N
                   I3 = IPTHK + N
                   I4 = IPPOS + N                    
                   THICKT= STACK%GEO(1,ISUBSTACK)                        
                   THKLY  = STACK%GEO(I3 ,ISUBSTACK)*THICKT ! initial  
                   MATLY  = STACK%IGEO(I2,ISUBSTACK)                     
                   RATIO_THKLY(N) = THKLY/THK(I)                         
                   IF (N == 1) THEN                                      
                     POSLY(N) = -HALF + HALF*RATIO_THKLY(N)              
                   ELSE                                                  
                    POSLY(N) = POSLY(N-1)                               
     .                 + HALF*(RATIO_THKLY(N)+RATIO_THKLY(N-1))          
                   ENDIF ! IF (N == 1)                                   
                   LAYPOS = POSLY(N)*THK(I)                              
                   MSL   = PM(89,MATLY)*THKLY*AREA(I)*FOURTH
                   XIL   = MSL*(AREA(I)/FAC+THKLY*THKLY*ONE_OVER_12+LAYPOS*LAYPOS)
                   XI(I) = XI(I) + XIL         
                 ENDIF  
               ENDDO  
             ENDIF  
           ENDDO 
         ENDIF       
      ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN
        IPANG  =  1
        IPPID  =  2
        IPMAT  =  IPPID + NLAY   !  NPT
        IPTHK  =  IPANG + NLAY   !  NPT
        IPPOS  =  IPTHK + NLAY   !  NPT
        NTHK   =  IPPOS + NLAY   !  NPT 
        IPOS = IGEO(99,IPROP)                   
        THICKT  = STACK%GEO(1,ISUBSTACK)
        ZSHIFT = GEO(199,IPROP)
        IF(IPOS == 0 )THEN                           
          ZSHIFT = - HALF                             
        ELSEIF(IPOS == 2 )THEN                        
          ZSHIFT =  -  ZSHIFT /MAX(THICKT,EM20)       
        ELSEIF(IPOS == 3) THEN                        
          ZSHIFT = -ONE                               
        ELSEIF(IPOS == 4) THEN                        
          ZSHIFT = ZERO                               
        ENDIF          
C       
        IF(IDRAPE == 0) THEN  
           DO I=LFT,LLT
             XI(I)=ZERO
             IPT_ALL = 0
             DO ILAY=1, NLAY   !  NPT                                                                              
               NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT                                                                   
               I1 = IPPID + ILAY                                                                                   
               I2 = IPMAT + ILAY                                                                                   
               I3 = IPTHK + ILAY                                                                                   
               I4 = IPPOS + ILAY                                                                                   
               IPID_LY = STACK%IGEO(I1,ISUBSTACK)                                                                  
               MATLY   = STACK%IGEO(I2,ISUBSTACK)                                                                  
               IINT = IGEO(47,IPROP)                                                                               
               IF(IINT == 1) THEN                                                                                  
                   THICKT  = STACK%GEO(1,ISUBSTACK)                                                                
                   THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT                                                        
                   LAYPOS   = STACK%GEO(I4 ,ISUBSTACK)*THICKT                                                      
                   DO IT=1,NPTT                                                                                    
                     IPT = IPT_ALL + IT                                                                            
                     THK_IT(IT) = THKLY/NPTT  ! uniform distribution of NPTT through layer                         
                     RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)                                                          
                     IF (IPT  == 1 ) THEN                                                                          
                        POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT) ! integr. point "IT" position ratio            
                     ELSE                                                                                          
                        POSLY(IPT) = POSLY(IPT-1)           ! integr. point "IT" position ratio                    
     .                    + HALF*(RATIO_THKLY(IPT) + RATIO_THKLY(IPT-1))                                           
                     ENDIF ! IF (IPT == 1)                                                                         
                     POS_IT(IT) = POSLY(IPT)*THK(I)                                                                
                  ENDDO                                                                                            
               ELSEIF(IINT == 2) THEN                                                                              
                  THICKT  = STACK%GEO(1,ISUBSTACK)                                                                 
                  THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT                                                         
                  LAYPOS   = STACK%GEO(I4 ,ISUBSTACK)*THICKT                                                       
                  DO IT=1,NPTT                                                                                     
                     IPT = IPT_ALL + IT                                                                            
                     THK_IT(IT)   =  HALF*THKLY*W_GAUSS(IT,NPTT)                                                   
                     RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)                                                          
                     IF (IPT == 1) THEN                                                                            
                       POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT)                                                 
                     ELSE                                                                                          
                       POSLY(IPT) = POSLY(IPT-1)                                                                   
     .                   + HALF*(RATIO_THKLY(IPT)+RATIO_THKLY(IPT-1))                                              
                     ENDIF ! IF (IPT 1)                                                                            
                     POS_IT(IT)   = POSLY(IPT)*THK(I)                                                              
                  ENDDO  ! nptt                                                                                    
               ENDIF    ! iint                                                                                     
C
               DO IT=1,NPTT                                                                                        
                 MS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)*FOURTH                                                  
                 XIL = MS_NPTT*(AREA(I)/FAC+THK_IT(IT)*THK_IT(IT)*ONE_OVER_12+                                     
     .                          POS_IT(IT)*POS_IT(IT))                                                             
                 XI(I) = XI(I) + XIL                                                                               
               ENDDO                                                                                               
               IPT_ALL = IPT_ALL + NPTT                                                                            
              ENDDO  ! NLAY                                                                                      
            ENDDO      ! LFT,LLT
C
          ELSE ! IDRAPE > 0
           DO I=LFT,LLT
             XI(I)=ZERO
             IPT_ALL = 0
             IE = INDX(NFT + I)
             IF(IE == 0) THEN
               DO ILAY=1, NLAY   !  NPT
                 NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                 I1 = IPPID + ILAY
                 I2 = IPMAT + ILAY
                 I3 = IPTHK + ILAY
                 I4 = IPPOS + ILAY
                 IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                 MATLY   = STACK%IGEO(I2,ISUBSTACK)
                 IINT = IGEO(47,IPROP)
                 IF(IINT == 1) THEN                                             
                     THICKT  = STACK%GEO(1,ISUBSTACK)                                                          
                     THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT                                                  
                     LAYPOS   = STACK%GEO(I4 ,ISUBSTACK)*THICKT                                                 
                     DO IT=1,NPTT                                                                              
                       IPT = IPT_ALL + IT                                                                      
                       THK_IT(IT) = THKLY/NPTT  ! uniform distribution of NPTT through layer
                       RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)                
                       IF (IPT  == 1 ) THEN                                                                   
                          POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT) ! integr. point "IT" position ratio     
                       ELSE                                                                                    
                          POSLY(IPT) = POSLY(IPT-1)           ! integr. point "IT" position ratio             
     .                      + HALF*(RATIO_THKLY(IPT) + RATIO_THKLY(IPT-1))                                    
                       ENDIF ! IF (IPT == 1) 
                       POS_IT(IT) = POSLY(IPT)*THK(I)  
                    ENDDO                                                          
                 ELSEIF(IINT == 2) THEN
                    THICKT  = STACK%GEO(1,ISUBSTACK)                         
                    THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT                 
                    LAYPOS   = STACK%GEO(I4 ,ISUBSTACK)*THICKT               
                    DO IT=1,NPTT                                                                                   
                       IPT = IPT_ALL + IT                                    
                       THK_IT(IT)   =  HALF*THKLY*W_GAUSS(IT,NPTT)           
                       RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)                                                  
                       IF (IPT == 1) THEN                                                                     
                         POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT)                                      
                       ELSE                                                                                    
                         POSLY(IPT) = POSLY(IPT-1)                                                       
     .                     + HALF*(RATIO_THKLY(IPT)+RATIO_THKLY(IPT-1))                                    
                       ENDIF ! IF (IPT 1)                                                                  
                       POS_IT(IT)   = POSLY(IPT)*THK(I)                                                         
                    ENDDO  ! nptt                                             
                 ENDIF    ! iint 
C
                 DO IT=1,NPTT
                   MS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)*FOURTH
                   XIL = MS_NPTT*(AREA(I)/FAC+THK_IT(IT)*THK_IT(IT)*ONE_OVER_12+
     .                            POS_IT(IT)*POS_IT(IT))
                   XI(I) = XI(I) + XIL 
                 ENDDO
                 IPT_ALL = IPT_ALL + NPTT
                ENDDO  ! NLAY
              ELSE ! IE
                DO ILAY=1, NLAY   !  NPT
                  NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                  IP = DRAPE(IE)%INDX_PLY(ILAY)
                  IF(IP > 0) THEN
                    DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                    NSLICE = DRAPE_PLY%NSLICE  ! NPTT
                    I1 = IPPID + ILAY
                    I2 = IPMAT + ILAY
                    I3 = IPTHK + ILAY
                    I4 = IPPOS + ILAY
                    IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                    MATLY   = STACK%IGEO(I2,ISUBSTACK)
                    IINT = IGEO(47,IPROP)
                    IF(IINT == 1) THEN                                                                
                       THICKT = STACK%GEO(1,ISUBSTACK)                                                            
                       THKLY   = STACK%GEO(I3 ,ISUBSTACK)*THICKT ! initial                                        
                       IPID_LY = STACK%IGEO(I1,ISUBSTACK)                                                         
                       MATLY   = STACK%IGEO(I2,ISUBSTACK)                                                         
                       DO IT=1,NPTT                                                                               
                         IPT = IPT_ALL + IT                                                                       
                         THINNING = DRAPE_PLY%RDRAPE(IT,1)                                       
                         THK_IT(IT)   = THKLY*THINNING/NPTT                                                            
                         RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)                                                   
                         IF (IPT == 1) THEN                                                                      
                           POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT)                                          
                         ELSE                                                                                     
                           POSLY(IPT) = POSLY(IPT-1)                                                        
     .                              + HALF*(RATIO_THKLY(IPT)+RATIO_THKLY(IPT-1))               
                         ENDIF ! IF (IPT == 1)                                                                   
                         POS_IT(IT)   = POSLY(IPT)*THK(I)
                       ENDDO  ! nptt                                                                             
                    ELSEIF(IINT == 2) THEN
                       THICKT = STACK%GEO(1,ISUBSTACK)                              
                       THKLY  = STACK%GEO(I3 ,ISUBSTACK)*THICKT ! initial           
                       DO IT=1,NPTT                                                                                  
                         IPT = IPT_ALL + IT                                                                    
                         THINNING = DRAPE_PLY%RDRAPE(IT,1)                                     
                         THK_IT(IT)   =  HALF*THKLY*W_GAUSS(IT,NPTT)*THINNING      
                         RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)                                                 
                         IF (IPT == 1) THEN                                                                    
                           POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT)                                     
                         ELSE                                                                                   
                           POSLY(IPT) = POSLY(IPT-1)                                                      
     .                                + HALF*(RATIO_THKLY(IPT)+RATIO_THKLY(IPT-1))                                   
                         ENDIF ! IF (IPT 1)                                                                 
                         POS_IT(IT)   = POSLY(IPT)*THK(I)
                        ENDDO  ! nptt                                               
                    ENDIF    ! iint 
                    DO IT=1,NPTT
                      MS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)*FOURTH
                      XIL = MS_NPTT*(AREA(I)/FAC+THK_IT(IT)*THK_IT(IT)*ONE_OVER_12+
     .                                POS_IT(IT)*POS_IT(IT))
                      XI(I) = XI(I) + XIL
                    ENDDO
                  ELSE ! IP == 0
                    NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                    I1 = IPPID + ILAY
                    I2 = IPMAT + ILAY
                    I3 = IPTHK + ILAY
                    I4 = IPPOS + ILAY
                    IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                    MATLY   = STACK%IGEO(I2,ISUBSTACK)
                    IINT = IGEO(47,IPROP) 
                    IF(IINT == 1) THEN                                                                              
                       THICKT = STACK%GEO(1,ISUBSTACK)                                                            
                       THKLY   = STACK%GEO(I3 ,ISUBSTACK)*THICKT ! initial                                        
                       IPID_LY = STACK%IGEO(I1,ISUBSTACK)                                                         
                       MATLY   = STACK%IGEO(I2,ISUBSTACK)                                                         
                       DO IT=1,NPTT                                                                               
                         IPT = IPT_ALL + IT                    
                         THK_IT(IT)   = THKLY/NPTT                                                            
                         RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)                                                   
                         IF (IPT == 1) THEN                                                                      
                           POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT)                                          
                         ELSE                                                                                     
                           POSLY(IPT) = POSLY(IPT-1)                                                        
     .                              + HALF*(RATIO_THKLY(IPT)+RATIO_THKLY(IPT-1))               
                         ENDIF ! IF (IPT == 1)                                                                   
                         POS_IT(IT)   = POSLY(IPT)*THK(I)
                       ENDDO  ! nptt                                                                             
                    ELSEIF(IINT == 2) THEN
                       THICKT = STACK%GEO(1,ISUBSTACK)                              
                       THKLY  = STACK%GEO(I3 ,ISUBSTACK)*THICKT ! initial           
                       DO IT=1,NPTT                                                                                  
                         IPT = IPT_ALL + IT          
                         THK_IT(IT)   =  HALF*THKLY*W_GAUSS(IT,NPTT)     
                         RATIO_THKLY(IPT) = THK_IT(IT)/THK(I)                                                 
                         IF (IPT == 1) THEN                                                                    
                           POSLY(IPT) = ZSHIFT + HALF*RATIO_THKLY(IPT)                                     
                         ELSE                                                                                   
                           POSLY(IPT) = POSLY(IPT-1)                                                      
     .                                + HALF*(RATIO_THKLY(IPT)+RATIO_THKLY(IPT-1))                                   
                         ENDIF ! IF (IPT 1)                                                                 
                         POS_IT(IT)   = POSLY(IPT)*THK(I)                                                        
                        ENDDO  ! nptt                                               
                    ENDIF    ! iint 
                    DO IT=1,NPTT
                      MS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)*FOURTH
                      XIL = MS_NPTT*(AREA(I)/FAC+THK_IT(IT)*THK_IT(IT)*ONE_OVER_12+
     .                                POS_IT(IT)*POS_IT(IT))
                      XI(I) = XI(I) + XIL
                    ENDDO                    
                    ENDIF ! IP
                    IPT_ALL = IPT_ALL + NPTT   
                   ENDDO  ! NLAY
              ENDIF ! IE 
            ENDDO      ! LFT,LLT
          ENDIF  ! idrape  
      ELSE
        DO I=LFT,LLT
          XI(I)=EMS(I)*(AREA(I)/FAC+THK(I)*THK(I)*ONE_OVER_12)
        ENDDO
      ENDIF
C---  check stability condition
      IF (IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17 
     .                                 .OR. IGTYP == 51)THEN
       IF(IGMAT <= 0) THEN ! 
         DO I=LFT,LLT
          INS = EMS(I)*(AREA(I)/FAC+THK(I)*THK(I)*ONE_OVER_12)
          DMS = ABS(XI(I)-INS)/INS
          IF(DMS > EM02) THEN
            IDPROP = IGEO(1,IPROP)
            CALL FRETITL2(TITR,
     .                    IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
            CALL ANCMSG(MSGID=520,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=IDPROP,
     .                  C1=TITR,I2=IX(NIXC,NFT+I))
          ENDIF 
         ENDDO
        ENDIF
      ENDIF
C----------------------------------------------
C     initialisation inertie parith/on spmd
      IF (IMACH/=3) THEN
       IF(NADMESH == 0)THEN
        IF(IPARI0 == 3)THEN
          CALL DOUBLE_FLOT_IEEE(LFT,LLT,XI,XI,I8EMS)
          DO I=LFT,LLT
            I8MI(4,IX1(I))=I8MI(4,IX1(I)) + I8EMS(1,I)
            I8MI(5,IX1(I))=I8MI(5,IX1(I)) + I8EMS(2,I)
            I8MI(6,IX1(I))=I8MI(6,IX1(I)) + I8EMS(3,I)
            I8MI(4,IX2(I))=I8MI(4,IX2(I)) + I8EMS(1,I)
            I8MI(5,IX2(I))=I8MI(5,IX2(I)) + I8EMS(2,I)
            I8MI(6,IX2(I))=I8MI(6,IX2(I)) + I8EMS(3,I)
            I8MI(4,IX3(I))=I8MI(4,IX3(I)) + I8EMS(1,I)
            I8MI(5,IX3(I))=I8MI(5,IX3(I)) + I8EMS(2,I)
            I8MI(6,IX3(I))=I8MI(6,IX3(I)) + I8EMS(3,I)
            I8MI(4,IX4(I))=I8MI(4,IX4(I)) + I8EMS(1,I)
            I8MI(5,IX4(I))=I8MI(5,IX4(I)) + I8EMS(2,I)
            I8MI(6,IX4(I))=I8MI(6,IX4(I)) + I8EMS(3,I)
          ENDDO
        ELSE
         DO I=LFT,LLT
           TINER(IX1(I))=TINER(IX1(I))+XI(I)
           TINER(IX2(I))=TINER(IX2(I))+XI(I)
           TINER(IX3(I))=TINER(IX3(I))+XI(I)
           TINER(IX4(I))=TINER(IX4(I))+XI(I)
         ENDDO
        ENDIF
       ELSE
        IF(IPARI0 == 3)THEN
          CALL DOUBLE_FLOT_IEEE(LFT,LLT,XI,XI,I8EMS)
          DO I=LFT,LLT
           N=NFT+I
           IF(SH4TREE(3,N) >= 0)THEN
            I8MI(4,IX1(I))=I8MI(4,IX1(I)) + I8EMS(1,I)
            I8MI(5,IX1(I))=I8MI(5,IX1(I)) + I8EMS(2,I)
            I8MI(6,IX1(I))=I8MI(6,IX1(I)) + I8EMS(3,I)
            I8MI(4,IX2(I))=I8MI(4,IX2(I)) + I8EMS(1,I)
            I8MI(5,IX2(I))=I8MI(5,IX2(I)) + I8EMS(2,I)
            I8MI(6,IX2(I))=I8MI(6,IX2(I)) + I8EMS(3,I)
            I8MI(4,IX3(I))=I8MI(4,IX3(I)) + I8EMS(1,I)
            I8MI(5,IX3(I))=I8MI(5,IX3(I)) + I8EMS(2,I)
            I8MI(6,IX3(I))=I8MI(6,IX3(I)) + I8EMS(3,I)
            I8MI(4,IX4(I))=I8MI(4,IX4(I)) + I8EMS(1,I)
            I8MI(5,IX4(I))=I8MI(5,IX4(I)) + I8EMS(2,I)
            I8MI(6,IX4(I))=I8MI(6,IX4(I)) + I8EMS(3,I)
           END IF
          ENDDO
        ELSE
         IF(ISTATCND==0)THEN
           DO I=LFT,LLT
            N=NFT+I
            IF(SH4TREE(3,N) >= 0)THEN
             TINER(IX1(I))=TINER(IX1(I))+XI(I)
             TINER(IX2(I))=TINER(IX2(I))+XI(I)
             TINER(IX3(I))=TINER(IX3(I))+XI(I)
             TINER(IX4(I))=TINER(IX4(I))+XI(I)
            END IF
           ENDDO
         ELSE
           DO I=LFT,LLT
            N=NFT+I
            IF(SH4TREE(3,N) == 0 .OR. SH4TREE(3,N) == -1)THEN
             TINER(IX1(I))=TINER(IX1(I))+XI(I)
             TINER(IX2(I))=TINER(IX2(I))+XI(I)
             TINER(IX3(I))=TINER(IX3(I))+XI(I)
             TINER(IX4(I))=TINER(IX4(I))+XI(I)
            END IF
           ENDDO
         END IF
        ENDIF
       ENDIF
      ENDIF
C-----------------------
      DO I=LFT,LLT
        INC(I)=XI(I)
      ENDDO
C-----------------------
      IF(NADMESH==0)THEN
       IF (NXREF == 0) THEN
         DO I=LFT,LLT
          I1 = IX1(I)
          I2 = IX2(I)
          I3 = IX3(I)
          I4 = IX4(I)
          IP = IPART(I)
C

          PARTSAV(1,IP)=PARTSAV(1,IP) + FOUR*EMS(I)
          PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*
     .         (X(1,I1)+X(1,I2)+X(1,I3)+X(1,I4))
          PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*
     .         (X(2,I1)+X(2,I2)+X(2,I3)+X(2,I4))
          PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*
     .         (X(3,I1)+X(3,I2)+X(3,I3)+X(3,I4))
          XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2)
     .         +X(1,I3)*X(1,I3)+X(1,I4)*X(1,I4))
          XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2)
     .         +X(1,I3)*X(2,I3)+X(1,I4)*X(2,I4))
          YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2)
     .         +X(2,I3)*X(2,I3)+X(2,I4)*X(2,I4))
          YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2)
     .         +X(2,I3)*X(3,I3)+X(2,I4)*X(3,I4))
          ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2)
     .         +X(3,I3)*X(3,I3)+X(3,I4)*X(3,I4))
          ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2)
     .         +X(3,I3)*X(1,I3)+X(3,I4)*X(1,I4))
C
          PARTSAV(5,IP) =PARTSAV(5,IP) + FOUR*XI(I) + EMS(I) * (YY+ZZ)
          PARTSAV(6,IP) =PARTSAV(6,IP) + FOUR*XI(I) + EMS(I) * (ZZ+XX)
          PARTSAV(7,IP) =PARTSAV(7,IP) + FOUR*XI(I) + EMS(I) * (XX+YY)
          PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
          PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
          PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX
C
          PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*
     .         (V(1,I1)+V(1,I2)+V(1,I3)+V(1,I4))
          PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*
     .         (V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4))
          PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*
     .         (V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4))
C
          PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS(I) *
     .       (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .       +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .       +V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .       +V(1,I4)*V(1,I4)+V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4))
         END DO
       ELSE
         DO I=LFT,LLT
          I1 = IX1(I)
          I2 = IX2(I)
          I3 = IX3(I)
          I4 = IX4(I)
          IP = IPART(I)
C
          PARTSAV(1,IP)=PARTSAV(1,IP) + FOUR*EMS(I)
          PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*
     .           (XREFC(1,1,I)+XREFC(2,1,I)+XREFC(3,1,I)+XREFC(4,1,I))
          PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*
     .           (XREFC(1,2,I)+XREFC(2,2,I)+XREFC(3,2,I)+XREFC(4,2,I))
          PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*
     .           (XREFC(1,3,I)+XREFC(2,3,I)+XREFC(3,3,I)+XREFC(4,3,I))
          XX = XREFC(1,1,I)**2 + XREFC(2,1,I)**2
     .       + XREFC(3,1,I)**2 + XREFC(4,1,I)
          XY = XREFC(1,1,I)*XREFC(1,2,I) + XREFC(2,1,I)*XREFC(2,2,I)
     .       + XREFC(3,1,I)*XREFC(3,2,I) + XREFC(4,1,I)*XREFC(4,2,I)
          YY = XREFC(1,2,I)**2 + XREFC(2,2,I)**2
     .       + XREFC(3,2,I)**2 + XREFC(4,2,I)
          YZ = XREFC(1,2,I)*XREFC(1,3,I) + XREFC(2,2,I)*XREFC(2,3,I)
     .       + XREFC(3,2,I)*XREFC(3,3,I) + XREFC(4,2,I)*XREFC(4,3,I)
          ZZ = XREFC(1,3,I)**2 + XREFC(2,3,I)**2
     .       + XREFC(3,3,I)**2 + XREFC(4,3,I)
          ZX = XREFC(1,3,I)*XREFC(1,1,I) + XREFC(2,3,I)*XREFC(2,1,I)
     .       + XREFC(3,3,I)*XREFC(3,1,I) + XREFC(4,3,I)*XREFC(4,1,I)
C
          PARTSAV(5,IP) =PARTSAV(5,IP) + FOUR*XI(I) + EMS(I)*(YY+ZZ)
          PARTSAV(6,IP) =PARTSAV(6,IP) + FOUR*XI(I) + EMS(I)*(ZZ+XX)
          PARTSAV(7,IP) =PARTSAV(7,IP) + FOUR*XI(I) + EMS(I)*(XX+YY)
          PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
          PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
          PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX
C
          PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*
     .         (V(1,I1)+V(1,I2)+V(1,I3)+V(1,I4))
          PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*
     .         (V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4))
          PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*
     .         (V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4))
C
          PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS(I) *
     .       (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .       +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .       +V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .       +V(1,I4)*V(1,I4)+V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4))
         END DO
       ENDIF
      ELSEIF(ISTATCND==0) THEN
       IF (NXREF == 0) THEN
         DO I=LFT,LLT
          N=NFT+I
          IF(SH4TREE(3,N) >= 0)THEN
           I1 = IX1(I)
           I2 = IX2(I)
           I3 = IX3(I)
           I4 = IX4(I)
           IP = IPART(I)
C
           PARTSAV(1,IP)=PARTSAV(1,IP) + FOUR*EMS(I)
           PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*
     .          (X(1,I1)+X(1,I2)+X(1,I3)+X(1,I4))
           PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*
     .          (X(2,I1)+X(2,I2)+X(2,I3)+X(2,I4))
           PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*
     .          (X(3,I1)+X(3,I2)+X(3,I3)+X(3,I4))
           XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2)
     .          +X(1,I3)*X(1,I3)+X(1,I4)*X(1,I4))
           XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2)
     .          +X(1,I3)*X(2,I3)+X(1,I4)*X(2,I4))
           YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2)
     .          +X(2,I3)*X(2,I3)+X(2,I4)*X(2,I4))
           YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2)
     .          +X(2,I3)*X(3,I3)+X(2,I4)*X(3,I4))
           ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2)
     .          +X(3,I3)*X(3,I3)+X(3,I4)*X(3,I4))
           ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2)
     .          +X(3,I3)*X(1,I3)+X(3,I4)*X(1,I4))
C
           PARTSAV(5,IP) =PARTSAV(5,IP) + FOUR*XI(I) + EMS(I)*(YY+ZZ)
           PARTSAV(6,IP) =PARTSAV(6,IP) + FOUR*XI(I) + EMS(I)*(ZZ+XX)
           PARTSAV(7,IP) =PARTSAV(7,IP) + FOUR*XI(I) + EMS(I)*(XX+YY)
           PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
           PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
           PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX
C
           PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*
     .          (V(1,I1)+V(1,I2)+V(1,I3)+V(1,I4))
           PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*
     .          (V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4))
           PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*
     .          (V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4))
C
           PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS(I) *
     .        (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .        +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .        +V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .        +V(1,I4)*V(1,I4)+V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4))
          END IF
         END DO
       ELSE
         DO I=LFT,LLT
          N=NFT+I
          IF(SH4TREE(3,N) >= 0)THEN
           I1 = IX1(I)
           I2 = IX2(I)
           I3 = IX3(I)
           I4 = IX4(I)
           IP = IPART(I)
C
           PARTSAV(1,IP)=PARTSAV(1,IP) + FOUR*EMS(I)
           PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*
     .           (XREFC(1,1,I)+XREFC(2,1,I)+XREFC(3,1,I)+XREFC(4,1,I))
           PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*                        
     .            (XREFC(1,2,I)+XREFC(2,2,I)+XREFC(3,2,I)+XREFC(4,2,I)) 
           PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*                        
     .            (XREFC(1,3,I)+XREFC(2,3,I)+XREFC(3,3,I)+XREFC(4,3,I)) 
           XX = XREFC(1,1,I)**2 + XREFC(2,1,I)**2                       
     .        + XREFC(3,1,I)**2 + XREFC(4,1,I)                          
           XY = XREFC(1,1,I)*XREFC(1,2,I) + XREFC(2,1,I)*XREFC(2,2,I)   
     .        + XREFC(3,1,I)*XREFC(3,2,I) + XREFC(4,1,I)*XREFC(4,2,I)   
           YY = XREFC(1,2,I)**2 + XREFC(2,2,I)**2                       
     .        + XREFC(3,2,I)**2 + XREFC(4,2,I)                          
           YZ = XREFC(1,2,I)*XREFC(1,3,I) + XREFC(2,2,I)*XREFC(2,3,I)   
     .        + XREFC(3,2,I)*XREFC(3,3,I) + XREFC(4,2,I)*XREFC(4,3,I)   
           ZZ = XREFC(1,3,I)**2 + XREFC(2,3,I)**2                       
     .        + XREFC(3,3,I)**2 + XREFC(4,3,I)                          
           ZX = XREFC(1,3,I)*XREFC(1,1,I) + XREFC(2,3,I)*XREFC(2,1,I)   
     .        + XREFC(3,3,I)*XREFC(3,1,I) + XREFC(4,3,I)*XREFC(4,1,I)   
C
           PARTSAV(5,IP) =PARTSAV(5,IP) + FOUR*XI(I) + EMS(I)*(YY+ZZ)
           PARTSAV(6,IP) =PARTSAV(6,IP) + FOUR*XI(I) + EMS(I)*(ZZ+XX)
           PARTSAV(7,IP) =PARTSAV(7,IP) + FOUR*XI(I) + EMS(I)*(XX+YY)
           PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
           PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
           PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX
C
           PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*
     .          (V(1,I1)+V(1,I2)+V(1,I3)+V(1,I4))
           PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*
     .          (V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4))
           PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*
     .          (V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4))
C
           PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS(I) *
     .        (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .        +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .        +V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .        +V(1,I4)*V(1,I4)+V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4))
          END IF
         END DO
       ENDIF
      ELSE
       IF (NXREF == 0) THEN
         DO I=LFT,LLT
          N=NFT+I
          IF(SH4TREE(3,N) == 0 .OR. SH4TREE(3,N) == -1)THEN
           I1 = IX1(I)
           I2 = IX2(I)
           I3 = IX3(I)
           I4 = IX4(I)
           IP = IPART(I)
C
           PARTSAV(1,IP)=PARTSAV(1,IP) + FOUR*EMS(I)
           PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*
     .          (X(1,I1)+X(1,I2)+X(1,I3)+X(1,I4))
           PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*
     .          (X(2,I1)+X(2,I2)+X(2,I3)+X(2,I4))
           PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*
     .          (X(3,I1)+X(3,I2)+X(3,I3)+X(3,I4))
           XX = (X(1,I1)*X(1,I1)+X(1,I2)*X(1,I2)
     .          +X(1,I3)*X(1,I3)+X(1,I4)*X(1,I4))
           XY = (X(1,I1)*X(2,I1)+X(1,I2)*X(2,I2)
     .          +X(1,I3)*X(2,I3)+X(1,I4)*X(2,I4))
           YY = (X(2,I1)*X(2,I1)+X(2,I2)*X(2,I2)
     .          +X(2,I3)*X(2,I3)+X(2,I4)*X(2,I4))
           YZ = (X(2,I1)*X(3,I1)+X(2,I2)*X(3,I2)
     .          +X(2,I3)*X(3,I3)+X(2,I4)*X(3,I4))
           ZZ = (X(3,I1)*X(3,I1)+X(3,I2)*X(3,I2)
     .          +X(3,I3)*X(3,I3)+X(3,I4)*X(3,I4))
           ZX = (X(3,I1)*X(1,I1)+X(3,I2)*X(1,I2)
     .          +X(3,I3)*X(1,I3)+X(3,I4)*X(1,I4))
C
           PARTSAV(5,IP) =PARTSAV(5,IP) + FOUR*XI(I) + EMS(I)*(YY+ZZ)
           PARTSAV(6,IP) =PARTSAV(6,IP) + FOUR*XI(I) + EMS(I)*(ZZ+XX)
           PARTSAV(7,IP) =PARTSAV(7,IP) + FOUR*XI(I) + EMS(I)*(XX+YY)
           PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
           PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
           PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX
C
           PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*
     .          (V(1,I1)+V(1,I2)+V(1,I3)+V(1,I4))
           PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*
     .          (V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4))
           PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*
     .          (V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4))
C
           PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS(I) *
     .        (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .        +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .        +V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .        +V(1,I4)*V(1,I4)+V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4))
          END IF
         END DO
       ELSE
         DO I=LFT,LLT
          N=NFT+I
          IF(SH4TREE(3,N) == 0 .OR. SH4TREE(3,N) == -1)THEN
           I1 = IX1(I)
           I2 = IX2(I)
           I3 = IX3(I)
           I4 = IX4(I)
           IP = IPART(I)
C
           PARTSAV(1,IP)=PARTSAV(1,IP) + FOUR*EMS(I)
           PARTSAV(2,IP)=PARTSAV(2,IP) + EMS(I)*
     .           (XREFC(1,1,I)+XREFC(2,1,I)+XREFC(3,1,I)+XREFC(4,1,I))
           PARTSAV(3,IP)=PARTSAV(3,IP) + EMS(I)*
     .            (XREFC(1,2,I)+XREFC(2,2,I)+XREFC(3,2,I)+XREFC(4,2,I))
           PARTSAV(4,IP)=PARTSAV(4,IP) + EMS(I)*
     .            (XREFC(1,3,I)+XREFC(2,3,I)+XREFC(3,3,I)+XREFC(4,3,I))
           XX = XREFC(1,1,I)**2 + XREFC(2,1,I)**2
     .        + XREFC(3,1,I)**2 + XREFC(4,1,I)
           XY = XREFC(1,1,I)*XREFC(1,2,I) + XREFC(2,1,I)*XREFC(2,2,I)
     .        + XREFC(3,1,I)*XREFC(3,2,I) + XREFC(4,1,I)*XREFC(4,2,I)
           YY = XREFC(1,2,I)**2 + XREFC(2,2,I)**2
     .        + XREFC(3,2,I)**2 + XREFC(4,2,I)
           YZ = XREFC(1,2,I)*XREFC(1,3,I) + XREFC(2,2,I)*XREFC(2,3,I)
     .        + XREFC(3,2,I)*XREFC(3,3,I) + XREFC(4,2,I)*XREFC(4,3,I)
           ZZ = XREFC(1,3,I)**2 + XREFC(2,3,I)**2
     .        + XREFC(3,3,I)**2 + XREFC(4,3,I)
           ZX = XREFC(1,3,I)*XREFC(1,1,I) + XREFC(2,3,I)*XREFC(2,1,I)
     .        + XREFC(3,3,I)*XREFC(3,1,I) + XREFC(4,3,I)*XREFC(4,1,I)
C
           PARTSAV(5,IP) =PARTSAV(5,IP) + FOUR*XI(I) + EMS(I)*(YY+ZZ)
           PARTSAV(6,IP) =PARTSAV(6,IP) + FOUR*XI(I) + EMS(I)*(ZZ+XX)
           PARTSAV(7,IP) =PARTSAV(7,IP) + FOUR*XI(I) + EMS(I)*(XX+YY)
           PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS(I) * XY
           PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS(I) * YZ
           PARTSAV(10,IP)=PARTSAV(10,IP) - EMS(I) * ZX
C
           PARTSAV(11,IP)=PARTSAV(11,IP) + EMS(I)*
     .          (V(1,I1)+V(1,I2)+V(1,I3)+V(1,I4))
           PARTSAV(12,IP)=PARTSAV(12,IP) + EMS(I)*
     .          (V(2,I1)+V(2,I2)+V(2,I3)+V(2,I4))
           PARTSAV(13,IP)=PARTSAV(13,IP) + EMS(I)*
     .          (V(3,I1)+V(3,I2)+V(3,I3)+V(3,I4))
C
           PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS(I) *
     .        (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1)
     .        +V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2)
     .        +V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3)
     .        +V(1,I4)*V(1,I4)+V(2,I4)*V(2,I4)+V(3,I4)*V(3,I4))
          END IF
         END DO
       ENDIF
      END IF
      
C----------------------------------------------
C     INITIALISATION DES RIGIDITES NODALES POUR INTERFACES
C----------------------------------------------
      IF (IGTYP == 52 .OR. 
     .  ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0)) THEN
           IF(I7STIFS/=0)THEN
            IF (IMACH/=3) THEN
             DO I=LFT,LLT
               ET= STACK%PM(2,ISUBSTACK)*THK(I)
               ETNOD(IX1(I))=ETNOD(IX1(I))+ET
               ETNOD(IX2(I))=ETNOD(IX2(I))+ET
               ETNOD(IX3(I))=ETNOD(IX3(I))+ET
               ETNOD(IX4(I))=ETNOD(IX4(I))+ET
               NSHNOD(IX1(I))=NSHNOD(IX1(I))+1
               NSHNOD(IX2(I))=NSHNOD(IX2(I))+1
               NSHNOD(IX3(I))=NSHNOD(IX3(I))+1
               NSHNOD(IX4(I))=NSHNOD(IX4(I))+1
             ENDDO
            ELSE
             DO I=LFT,LLT
               ET= STACK%PM(2,ISUBSTACK)*THK(I)
               STC(I)=ET
               NSHNOD(IX1(I))=NSHNOD(IX1(I))+1
               NSHNOD(IX2(I))=NSHNOD(IX2(I))+1
               NSHNOD(IX3(I))=NSHNOD(IX3(I))+1
               NSHNOD(IX4(I))=NSHNOD(IX4(I))+1
             ENDDO
            ENDIF
           ENDIF
      ELSEIF(IGTYP == 11 .AND. IGMAT >  0) THEN
          IPGMAT = 700
          IF(I7STIFS/=0)THEN
            IF (IMACH/=3) THEN
             DO I=LFT,LLT
              ET=GEO(IPGMAT +2 ,IPROP)*THK(I)
              ETNOD(IX1(I))=ETNOD(IX1(I))+ET
              ETNOD(IX2(I))=ETNOD(IX2(I))+ET
              ETNOD(IX3(I))=ETNOD(IX3(I))+ET
              ETNOD(IX4(I))=ETNOD(IX4(I))+ET
              NSHNOD(IX1(I))=NSHNOD(IX1(I))+1
              NSHNOD(IX2(I))=NSHNOD(IX2(I))+1
              NSHNOD(IX3(I))=NSHNOD(IX3(I))+1
              NSHNOD(IX4(I))=NSHNOD(IX4(I))+1
            ENDDO
           ELSE
            DO I=LFT,LLT
              ET=GEO(IPGMAT +2 ,IPROP)*THK(I)
              STC(I)=ET
              NSHNOD(IX1(I))=NSHNOD(IX1(I))+1
              NSHNOD(IX2(I))=NSHNOD(IX2(I))+1
              NSHNOD(IX3(I))=NSHNOD(IX3(I))+1
              NSHNOD(IX4(I))=NSHNOD(IX4(I))+1
            ENDDO
           ENDIF
          ENDIF
      ELSE ! igmat  = 0
         IF(I7STIFS/=0)THEN
          IF (IMACH/=3) THEN
           DO I=LFT,LLT
             ET=PM(20,IMID)*THK(I)
             ETNOD(IX1(I))=ETNOD(IX1(I))+ET
             ETNOD(IX2(I))=ETNOD(IX2(I))+ET
             ETNOD(IX3(I))=ETNOD(IX3(I))+ET
             ETNOD(IX4(I))=ETNOD(IX4(I))+ET
             NSHNOD(IX1(I))=NSHNOD(IX1(I))+1
             NSHNOD(IX2(I))=NSHNOD(IX2(I))+1
             NSHNOD(IX3(I))=NSHNOD(IX3(I))+1
             NSHNOD(IX4(I))=NSHNOD(IX4(I))+1
           ENDDO
          ELSE
           DO I=LFT,LLT
             ET=PM(20,IMID)*THK(I)
             STC(I)=ET
             NSHNOD(IX1(I))=NSHNOD(IX1(I))+1
             NSHNOD(IX2(I))=NSHNOD(IX2(I))+1
             NSHNOD(IX3(I))=NSHNOD(IX3(I))+1
             NSHNOD(IX4(I))=NSHNOD(IX4(I))+1
           ENDDO
          ENDIF
         ENDIF
      ENDIF ! igmat      
      DEALLOCATE(POSLY,RATIO_THKLY,THK_IT,POS_IT ) 
C-----------
      RETURN
      END
