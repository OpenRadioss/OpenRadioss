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
Chd|  LECSTACK_PLY                  source/properties/composite_options/stack/lecstack_ply.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        BIDON2                        source/system/machine.F       
Chd|        FRETITL                       source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_READ_STACK                 source/stack/hm_read_stack.F  
Chd|        LCGEO19                       source/elements/shell/coque/lcgeo19.F
Chd|        VDOUBLE                       source/system/sysfus.F        
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECSTACK_PLY(GEO_STACK ,X           ,IX           ,PM          ,ITABM1  ,
     .                       ISKN       ,IGEO_STACK  ,IPM          ,NPC         ,PLD     ,
     .                       UNITAB     ,RTRANS      ,LSUBMODEL    ,IPART       ,IDRAPEID,
     .                       PLY_INFO   ,STACK_INFO  ,NUMGEO_STACK ,NPROP_STACK)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE STACK_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IX(*),ITABM1(*),ISKN(LISKN,*),
     .        IGEO_STACK(NPROPGI,NUMSTACK + NUMPLY),IPM(NPROPMI,NUMMAT),NPC(*),
     .        IPART(LIPART1,*),IDRAPEID(*),PLY_INFO(2,NUMPLY),
     .        NPROP_STACK,NUMGEO_STACK(NUMGEO+NUMSTACK)
      my_real GEO_STACK(NPROPG,NUMSTACK+NUMPLY), X(*), PM(NPROPM,NUMMAT),PLD(*),RTRANS(NTRANSF,*)
      TYPE(STACK_INFO_ ) , DIMENSION (1:NPROP_STACK) :: STACK_INFO
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL lFOUND
      CHARACTER IDTITL*nchartitle, MESS*40, TITR1*nchartitle
      INTEGER I, IG,IGTYP,J,IP,ISTRAIN,I8PT,ISK,ITU,IRB,IHON,IHBE,IPLAST,ITHK,K,N,IDS, IUNIT,UID,ISORTH
      INTEGER NSTACK,ISTACK,NUMS,IFLAGUNIT,JPID,N1,SUB_ID,PID1,JPID1,JPID2,NISUB,II
      my_real ANGL,RBID,FAC_L
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'PID DEFINITION                          '/
C----------------------
C     GEO(3) : ISMSTR
C     GEO(5) : DT (ISMSTR=3 expect for solid elems)
C     GEO(7) : VX  shell/solids ortho - reference vector
C     GEO(8) : VY
C     GEO(9) : VZ
C     GEO(11): ISTRAIN (shell)
C     GEO(12): IGTYP      ->    IGEO(11)
C     GEO(35): ITHK
C     GEO(37): ISHEAR
C     GEO(38): FSHEAR
C     GEO(39): IPLAST
C     GEO(40): IG    v50  ->    IGEO(1)
C     GEO(20:34) : Milieu poreux (bricks)
C     GEO(129): HCLOS (bricks)
C     GEO(130): HTEST (bricks)
C     GEO(131:170): FREE
C     GEO(171): IHBE
C     GEO(212): ANGLE BETWEEN two orthotropy directions (DIR1,DIR2) for 
C               the PID52 with LAW58
C-------------------
C    IGEO(1)  : IG
C    IGEO(2)  : ISK
C    IGEO(3)  : ISEN
C    IGEO(4)  : NIP
C    IGEO(5)  : ISMSTR
C    IGEO(6)  : IREP
C    IGEO(7)  : ITHK
C    IGEO(8)  : ISHEAR
C    IGEO(9)  : IPLAST
C    IGEO(10) : IHBE
C    IGEO(11) : IGTYP
C    IGEO(12) :
C    IGEO(13) : ICPRE
C    IGEO(14) : ICSTR
C    IGEO(15) : IINT
C    IGEO(16) : IFRAM
C    IGEO(17) : ISORTH
C    IGEO(18) : ISH3N
C    IGEO(19) : ICXFEM
C    IGEO(20) : ISROT
C    IGEO(40) : IAD_KNOT
C    IGEO(41) : POLYNOMIAL DEGREE in 1st direction
C    IGEO(42) : POLYNOMIAL DEGREE in 2nd direction
C    IGEO(43) : POLYNOMIAL DEGREE in 3rd direction
C    IGEO(44) : NUMBER OF CONTROL POINTS in 1st direction
C    IGEO(45) : NUMBER OF CONTROL POINTS in 2nd direction
C    IGEO(46) : NUMBER OF CONTROL POINTS in 3rd direction
C    IGEO(47) : INTEGRATION FORMULATION FLAG for PID51 (UNIFORM / GAUSS distribution)
C    IGEO(48) : DRAPE IDENTIFICATION NUMBER
C    IGEO(49) : =1 ORTHOTROPY ANGLE DEFINED AT ELEMENT LEVEL  ( /PROP/TYPE19/51/52 )
C               =2 ORTHOTROPY ANGLE DEFINED AT STACK LEVEL ( /PROP/TYPE19/51/52 )
C=======================================================================
      WRITE(IOUT,1000)
C----------------------
      SUB_ID = 0    
      RBID=ZERO
c----------            
      CALL HM_OPTION_START('/STACK')
      DO I=1,NUMSTACK 
        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=IG, UNIT_ID=UID, OPTION_TITR=IDTITL)
        ISORTH = 0
        IFLAGUNIT = 0

        DO IUNIT=1,NUNITS
          IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO

        IF (UID /= 0 .AND. IFLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=IG, I2=UID,
     .                C1='STACK', C2='STACK', C3=IDTITL)
        ENDIF    

!       Stack + ply are belong to /PROP/PCOMPP 
        IGTYP = 52  ! belong to /PROP/PCOMP - TYPE52              
C
C new shell property (stack based with multi NPT through one layer 
C---------------
C      GENERIC SHELL
C----------------------
C
C----------------------------------------------------------------
C      COMPOSITE LAYERED SHELL (NEW)
C      LAYERS WITH : -VARIABLE THICKNESS
C                    -VARIABLE MATERIAL  (BUT LAW 25 OR 27 ONLY)
C                    -VARIABLE NUMBER OF INTEGRATION POINTS THROUGH ONE LAYER
C---------------------------------------------------------------- 
          CALL FRETITL(IDTITL,IGEO_STACK(NPROPGI-LTITR+1,I),LTITR)    
          NUMS = NUMGEO_STACK(NUMGEO + I)
          CALL HM_READ_STACK(
     .           GEO_STACK(1,I) ,IGEO_STACK(1,I) ,PM       ,IPM      ,ISKN     ,
     .           IG             ,RTRANS          ,SUB_ID   ,STACK_INFO(NUMS)   ,
     .           IDTITL         ,UNITAB          ,LSUBMODEL)
     
C--------   Variables stored in element buffer        
c----   Shells
C should be done for pccomp
C-------------------------------
C  temporary double storage : GEO() / IGEO() : may be optimized & deleted later 
!!! ---------------------
        IGEO_STACK(17,I)=ISORTH
        IF(GEO_STACK(39,I) /= ZERO .AND. IGEO_STACK(9,I) == 0) IGEO_STACK( 9,I)=NINT(GEO_STACK(39,I))
        IF(GEO_STACK(171,I) /= ZERO .AND. IGEO_STACK(10,I) == 0) IGEO_STACK(10,I)=NINT(GEO_STACK(171,I))    
C      
      END DO!next I
C
C-------------------------------
C  Objet /PLY
C-------------------------------
      I = NUMSTACK
      CALL HM_OPTION_START('/PLY')
      DO II = 1, NUMPLY  
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = IG, UNIT_ID = UID, OPTION_TITR = IDTITL)
         ISORTH = 0
         IFLAGUNIT = 0
         DO IUNIT=1,NUNITS
            IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN
               IFLAGUNIT = 1
               EXIT
            ENDIF
         ENDDO
c     call BIDON2 to avoid optimization issue on FAC_L variable from compiler (issue
c     observed after global code compilation with -openmp flag
        CALL BIDON2(FAC_L)
        IF (UID /=0 .AND. IFLAGUNIT == 0) THEN
           CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                 I1=IG, I2=UID, 
     .                 C1='PLY',C2='PLY',C3=IDTITL)
        ENDIF
        IGTYP = 19
        I = I + 1   
        IHBE = 0
        IGEO_STACK( 1,I) = IG
        ISTACK = 1
C
        CALL FRETITL(IDTITL,IGEO_STACK(NPROPGI-LTITR+1,I),LTITR)        
C
        CALL LCGEO19(GEO_STACK(1,I), IGEO_STACK(1,I), PM, IPM, UNITAB, IUNIT, ISTACK,IDRAPEID, LSUBMODEL)
        IF(IGEO_STACK(4,I) > 10) THEN
           CALL ANCMSG(MSGID=1146,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=IG,C1=IDTITL)
           CALL ARRET(2)
        ENDIF
        PLY_INFO(1,II) = IG
        PLY_INFO(2,II) = IGEO_STACK(4,I)
        IGEO_STACK(1,I) =IG     
      ENDDO
C
C-------------------------------cy
      NPLYMAX = MAX(NPLYMAX,NUMPLY)
C------------------------------
      DO I = 1, NUMSTACK
        GEO_STACK(100,I) = SQRT(GEO_STACK(38,I))      ! SHFSR
      END DO
C------------------------------
C
      DO I = 1,NUMSTACK
        IGTYP=IGEO_STACK(11,I)
        NUMS= NUMGEO_STACK(NUMGEO + I)
        IF(IGTYP == 52) THEN
          N1 = IGEO_STACK(4,I)
          DO J =1 , N1
C ply of stack JPID              
            JPID = STACK_INFO(NUMS)%PID(J)
            IF(JPID > 0)THEN
              lFOUND = .FALSE.
              DO K=1,NUMPLY
                IF (IGEO_STACK(1,NUMSTACK + K) == JPID) THEN
                    STACK_INFO(NUMS)%PID(J) = NUMSTACK + K
C tag if the ply is in the  stack  
                    IDS = IGEO_STACK(42,NUMSTACK  + K)
                    IGEO_STACK(42 ,NUMSTACK + K) = I
                    IF(IDS > 0 .AND. IDS /= I) THEN 
                       CALL FRETITL2(TITR1,IGEO_STACK(NPROPGI-LTITR+1,NUMSTACK+K),LTITR)
                       CALL ANCMSG(MSGID=1148,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,
     .                    I1=IGEO_STACK(1,NUMSTACK + K), I2= IGEO_STACK(1,IDS), I3= IGEO_STACK(1,I),
     .                    C1=TITR1, C2='PLY')
                    ENDIF
                  lFOUND = .TRUE.
                  EXIT
                ENDIF
              ENDDO
              IF(.NOT.lFOUND)THEN
                CALL FRETITL2(TITR1,IGEO_STACK(NPROPGI-LTITR+1,I),LTITR)
                CALL ANCMSG(MSGID=1149,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,
     .                      I1=IGEO_STACK(1,I), I2=JPID,
     .                      C1=TITR1, C2='STACK')
              ENDIF
            ENDIF!(JPID > 0)
          END DO!next J
C interface substack
          NISUB = IGEO_STACK(44,I) 
          IF (NISUB > 0) THEN
            DO J =1 , NISUB
              JPID1 = STACK_INFO(NUMS)%ISUB( 3*(J-1) + 1 )
              JPID2 = STACK_INFO(NUMS)%ISUB( 3*(J-1) + 2 )
              IF (JPID1 > 0 .OR. JPID2 > 0) THEN
                DO K=1,NUMPLY 
                  NSTACK = 0
                  lFOUND=.FALSE.
                  IF (IGEO_STACK(1,NUMSTACK + K) == JPID1) THEN
                    STACK_INFO(NUMS)%ISUB (3*(J-1) + 1) = NUMSTACK  + K
                    lFOUND=.TRUE.
                    EXIT !next J
                  ELSEIF (IGEO_STACK(1,NUMSTACK + K) == JPID2) THEN 
                    STACK_INFO(NUMS)%ISUB (3*(J-1) + 2) = NUMSTACK  + K
                    lFOUND=.TRUE.
                    EXIT !next J
                  ENDIF
                ENDDO
                IF(.NOT.lFOUND)THEN
                  CALL FRETITL2(TITR1,IGEO_STACK(NPROPGI-LTITR+1,I),LTITR)
                  CALL ANCMSG(MSGID=1149,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,
     .                        I1=IGEO_STACK(1,I), I2=JPID1,
     .                        C1=TITR1, C2='STACK')
                  CALL FRETITL2(TITR1,IGEO_STACK(NPROPGI-LTITR+1,I),LTITR)
                  CALL ANCMSG(MSGID=1149,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,
     .                        I1=IGEO_STACK(1,I), I2=JPID2,
     .                        C1=TITR1, C2='STACK')
                ENDIF
              ENDIF ! IF (JPID1 > 0 .OR. JPID2 > 0)
            ENDDO !next J 
          ENDIF ! IF (NISUB > 0)
C
          DO J=1,N1
            JPID = STACK_INFO(NUMS)%PID(J)
            STACK_INFO(NUMS)%THK(J)  = GEO_STACK(1,JPID) 
            STACK_INFO(NUMS)%DIR(J)  = GEO_STACK(212,JPID) ! angle (DIR1,DIR2) - for compatibility of law58 with PID51)
            STACK_INFO(NUMS)%MID(J)  = IGEO_STACK(101,JPID)
          ENDDO
! 
       ENDIF 
      ENDDO ! DO I = 1, NUMSTACK
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      IDS = 79
      I = 0
      J = 0
c      CALL ANCNTS(IDS,I)
      CALL VDOUBLE(IGEO_STACK(1,1),NPROPGI,NUMSTACK,MESS,0,RBID)
      CALL VDOUBLE(IGEO_STACK(1,NUMSTACK+1),NPROPGI,NUMPLY,MESS,0,RBID)
C
C-----------
      RETURN
C-----------
 1000 FORMAT(//
     & 5X,'    STACK OBJECT FOR PLY-BASED SHELL ELEMENT  SETS'/,
     & 5X,'    ----------------------------------------------'//) 
      END
