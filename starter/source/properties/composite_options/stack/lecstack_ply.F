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
      SUBROUTINE LECSTACK_PLY(GEO   ,X     ,IX         ,PM    ,ITABM1  ,
     .                       ISKN   ,IGEO  ,IPM        ,NPC   ,PLD     ,
     .                       UNITAB ,RTRANS,LSUBMODEL  ,IPART ,IDRAPEID,
     .                       PLY_INFO ,STACK_INFO,  NUMGEO_STACK,NPROP_STACK)
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
      USE ALE_MOD
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
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IX(*),ITABM1(*),ISKN(LISKN,*),
     .        IGEO(NPROPGI,NUMGEO),IPM(NPROPMI,NUMMAT),NPC(*),
     .        IPART(LIPART1,*),IDRAPEID(*),PLY_INFO(2,NUMPLY),
     .        NPROP_STACK,NUMGEO_STACK(NUMGEO+NUMSTACK)
      my_real
     .   GEO(NPROPG,NUMGEO), X(*), PM(NPROPM,NUMMAT),
     .   PLD(*),RTRANS(NTRANSF,*)
      TYPE(STACK_INFO_ ) , DIMENSION (1:NPROP_STACK) :: STACK_INFO
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER LAW_ID*4
      INTEGER I, IG, IGTYP, ISMSTR, NIP, J, IR1X, IR1Y, IR1Z, IREP,
     .   IR2X, IR2Y, IR2Z, ISHEAR, IRX, IROT, IMODE, IP, ISTRAIN,I8PT,
     .   ISK,ITU,IRB,IHON,IHBE,IPLAST,ITHK,IDF,IHBEOUTP,K,N,
     .   IGFLU, IDS, NSHELL, NSHSUP, NSHINF, FLGBADI, NBADI,IUNIT,UID,
     .   NSST_D, NSST_DS, NPSH, ICPRE, ICSTR ,NPTS,ISEN,ISORTH,
     .   PID1,IPID1, IHGFLU, IHBE_OLD,NSTACK,IGMAT,I3,ISTACK,NUMS
      INTEGER ISH3N,IFLAGUNIT,ICXFEM, JPID,N1,IPOS,I_INJECT,ISROT,MLAWLY,MID,SUB_ID,
     .        PROP_SHELL,PROP_TSHEL,PROP_SOLID,PROP_SPH,PROP_BEAM,
     .        IAD_KNOT,IPDIR
     
      INTEGER JPID1,JPID2,NISUB,IPISUB,II
      my_real
     .    FN, FT, DX, ANGL,PUN,HTEST,HCLOS,CVIS,RBID,VX,VY,VZ,
     .    FAC_L,FAC_T,FAC_M, TMIN,TMAX,DT,THICKT,IERREL,DN_P,ZSHIFT
      CHARACTER IDTITL*nchartitle, MESS*40,KEY*ncharkey, KEY2*ncharkey,
     .          STRING*ncharfield,TITR1*nchartitle,
     .          CHROT*7
      DATA NSHELL /0/, NSHSUP /0/, NSHINF /0/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'PID DEFINITION                          '/
      DATA PUN/0.1/
      DATA IDF /1/
C----------------------
C     GEO(3) : ISMSTR
C     GEO(5) : DT (ISMSTR=3 SAUF SOLIDES)
C     GEO(7) : VX  coques/solides ortho - vecteur de reference
C     GEO(8) : VY
C     GEO(9) : VZ
C     GEO(11): ISTRAIN (COQUE)
C     GEO(12): IGTYP      ->    IGEO(11)
C     GEO(35): ITHK
C     GEO(37): ISHEAR
C     GEO(38): FSHEAR
C     GEO(39): IPLAST
C     GEO(40): IG    v50  ->    IGEO(1)
C     GEO(20:34) : Milieu poreux (briques)
C     GEO(129): HCLOS (briques)
C     GEO(130): HTEST (briques)
C     GEO(131:170): LIBRE
C     GEO(171): IHBE
C     GEO(212): ANGLE BETWEEN two orthotropy directions (DIR1,DIR2) for 
C               the PID52 with LAW58
C-------------------
C    nouveau stockage:
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
      NBADI  = 0
      NSST_D = 0
      NSST_DS= 0
      NPSH   = 0
      CVIS =ZERO
      IAD_KNOT = 0
      SUB_ID = 0
      
C
c----------      
C       
      CALL HM_OPTION_START('/STACK')
      DO 600 I=1,NUMSTACK 
        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = IG, UNIT_ID = UID, OPTION_TITR = IDTITL)
        IGFLU = 0
        IF(ALE%GLOBAL%ICAA == 1)IGFLU=1
        ISORTH = 0
        IHGFLU = 0
        IFLAGUNIT = 0
!
        DO IUNIT=1,NUNITS
          IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
C
        IF (UID/=0.AND.IFLAGUNIT==0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=IG,C1='STACK',
     .                 C2='STACK',
     .                 C3=IDTITL)
        ENDIF
!!     
!       Stack + ply are belong to /PROP/PCOMPP 
!!
        IGTYP = 52  ! belong to /PROP/PCOMP - TYPE52              
C
C new shell property (stack based with multi NPT through one layer 
C---------------
C      COQUE GENERIQUE
C----------------------
C
C----------------------------------------------------------------
C      COMPOSITE LAYERED SHELL (NEW)
C      LAYERS WITH : -VARIABLE THICKNESS
C                    -VARIABLE MATERIAL  (BUT LAW 25 OR 27 ONLY)
C                    -VARIABLE NUMBER OF INTEGRATION POINTS THROUGH ONE LAYER
C---------------------------------------------------------------- 
          NUMS = NUMGEO_STACK(NUMGEO + I)
!!          CALL LCGEO51(GEO(1,I),IGEO(1,I),PM,IPM,ISKN,UNITAB,IUNIT,
!!     .                RTRANS,LSUBMODEL,SUB_ID, ISTACK,STACK_INFO(NUMS) )
          CALL HM_READ_STACK(
     .           GEO(1,I) ,IGEO(1,I) ,PM       ,IPM      ,ISKN     ,
     .           IG       ,RTRANS   ,SUB_ID   ,STACK_INFO(NUMS)   ,
     .           IDTITL   ,UNITAB    ,LSUBMODEL)
     
C--------   Variables stored in element buffer        
c----   Shells
C should be done for pccomp
C-------------------------------
C  Double stockage temporaire : GEO() / IGEO() : a supprimer a terme 
!!! ---------------------
C
        IGEO(17,I)=ISORTH
        IF(GEO(39,I)/=ZERO.AND.IGEO( 9,I)== 0)IGEO( 9,I)=NINT(GEO(39,I))
        IF(GEO(171,I)/=ZERO.AND.IGEO(10,I)== 0)
     .     IGEO(10,I)=NINT(GEO(171,I))    
C      
 600  CONTINUE
C
C-------------------------------
C  Objet /PLY
C-------------------------------
      I = NUMSTACK
      CALL HM_OPTION_START('/PLY')
      DO II = 1, NUMPLY  
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = IG, UNIT_ID = UID, OPTION_TITR = IDTITL)
         IGFLU = 0
         IF(ALE%GLOBAL%ICAA == 1)I GFLU = 1
         ISORTH = 0
         IHGFLU = 0
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
        IF (UID/=0.AND.IFLAGUNIT==0) THEN
           CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .          I2=UID, I1=IG,C1='PLY',
     .          C2='PLY',
     .          C3=IDTITL)
        ENDIF
        IGTYP = 19
        I = I + 1   
        IHBE=0
        ISMSTR=0
        ISROT=0
        IGMAT =0      
        IGEO( 1,I)=IG
        ISTACK = 1
C
        CALL FRETITL(IDTITL,IGEO(NPROPGI-LTITR+1,I),LTITR)        
C
        CALL LCGEO19(GEO(1,I), IGEO(1,I), PM, IPM, UNITAB, IUNIT, ISTACK,
     .       IDRAPEID, LSUBMODEL)
        IF (IGEO(4,I)>10) THEN
           CALL ANCMSG(MSGID=1146,
     .          MSGTYPE=MSGERROR,
     .          ANMODE=ANINFO,
     .          I1=IG,
     .          C1=IDTITL)
           CALL ARRET(2)
        ENDIF
        PLY_INFO(1,II) = IG
        PLY_INFO(2,II) = IGEO(4,I)
        IGEO(1,I) =IG     
      ENDDO
C
C-------------------------------
      NPLYMAX = MAX(NPLYMAX,NUMPLY)
C------------------------------
C Precalcul SQRT
C------------------------------
      DO I = 1, NUMSTACK
        GEO(100,I) = SQRT(GEO(38,I))      ! SHFSR
      END DO
C------------------------------
C
      DO I = 1,NUMSTACK
        IGTYP=IGEO(11,I)
        NUMS= NUMGEO_STACK(NUMGEO + I)
        IF (IGTYP == 52) THEN
          N1 = IGEO(4,I)
          DO 100 J =1 , N1
C ply of stack JPID              
            JPID = STACK_INFO(NUMS)%PID(J)
            IF (JPID > 0) THEN
              DO K=1,NUMPLY
                IF (IGEO(1,NUMSTACK + K) == JPID) THEN
                    STACK_INFO(NUMS)%PID(J) = NUMSTACK + K
C tag if the ply is in the  stack  
                    IDS = IGEO(42,NUMSTACK  + K)
                    IGEO(42 ,NUMSTACK + K) = I
                    IF(IDS > 0 .AND. IDS /= I) THEN 
                       CALL FRETITL2(TITR1,
     .                       IGEO(NPROPGI-LTITR+1,NUMSTACK+I),LTITR)
                       CALL ANCMSG(MSGID=1148,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=IGEO(1,NUMSTACK + K),C1=TITR1,
     .                    C2='PLY',
     .                    I2= IGEO(1,IDS),
     .                    I3= IGEO(1,I))
                    ENDIF
                  GOTO 100
                ENDIF
              ENDDO
              CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,I),LTITR)
              CALL ANCMSG(MSGID=1149,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=IGEO(1,I),C1=TITR1,
     .                    C2='STACK',
     .                    I2=JPID)
            ENDIF
 100      CONTINUE
C interface substack
          NISUB = IGEO(44,I) 
          IF (NISUB > 0) THEN
            DO 110 J =1 , NISUB
              JPID1 = STACK_INFO(NUMS)%ISUB( 3*(J-1) + 1 )
              JPID2 = STACK_INFO(NUMS)%ISUB( 3*(J-1) + 2 )
              IF (JPID1 > 0 .OR. JPID2 > 0) THEN
                DO K=1,NUMPLY 
                  NSTACK = 0
                  IF (IGEO(1,NUMSTACK + K) == JPID1) THEN
                    STACK_INFO(NUMS)%ISUB (3*(J-1) + 1) = NUMSTACK  + K
                    GOTO 110
                  ELSEIF (IGEO(1,NUMSTACK + K) == JPID2) THEN 
                    STACK_INFO(NUMS)%ISUB (3*(J-1) + 2) = NUMSTACK  + K
                    GOTO 110
                  ENDIF
                ENDDO
                CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,I),LTITR)
                CALL ANCMSG(MSGID=1149,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=IGEO(1,I),C1=TITR1,
     .                      C2='STACK',
     .                      I2=JPID1)
                CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,I),LTITR)
                CALL ANCMSG(MSGID=1149,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=IGEO(1,I),C1=TITR1,
     .                      C2='STACK',
     .                      I2=JPID2)
              ENDIF ! IF (JPID1 > 0 .OR. JPID2 > 0)
 110        CONTINUE 
          ENDIF ! IF (NISUB > 0)
C
          DO J=1,N1
            JPID = STACK_INFO(NUMS)%PID(J)
            STACK_INFO(NUMS)%THK(J)  = GEO(1,JPID)
            STACK_INFO(NUMS)%ANG(J)  = STACK_INFO(NUMS)%ANG(J) + GEO(2,JPID)
            STACK_INFO(NUMS)%DIR(J)  = GEO(212,JPID) ! angle (DIR1,DIR2) - for compatibility of law58 with PID51)
            STACK_INFO(NUMS)%MID(J)  = IGEO(101,JPID)
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
      CALL VDOUBLE(IGEO(1,1),NPROPGI,NUMSTACK,MESS,0,RBID)
      CALL VDOUBLE(IGEO(1,NUMSTACK+1),NPROPGI,NUMPLY,MESS,0,RBID)
C
C-----------
      RETURN
C-----------
 1000 FORMAT(//
     & 5X,'    STACK OBJECT FOR PLY-BASED SHELL ELEMENT  SETS'/,
     & 5X,'    -------------'//) 
      END
