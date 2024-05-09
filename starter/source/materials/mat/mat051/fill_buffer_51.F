Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  FILL_BUFFER_51                source/materials/mat/mat051/fill_buffer_51.F
Chd|-- called by -----------
Chd|        FILL_BUFFER_51_0              source/materials/mat/mat051/fill_buffer_51_0.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        NINTRI                        source/system/nintrr.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FILL_BUFFER_51( IPM, PM, UPARAM, BUFMAT, USER_ID, TITR, INTERNAL_ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is filling law51 material buffer UPARAM(:)
C from material identifiers which were provided in input
C format related to IFLG=12
C
C It is done here and not in lecm51 because we need all
C material cards and eos card to be treated before.
C
C you need yield criteria and eos to be defined for each submaterial.
C
C submaterial order from user is not the same as the one in buffer (UPARAM).
C bijective app is in UPARAM(277:280) to get corresponding ids
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "com04_c.inc"
#include "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,TARGET :: IPM(NPROPMI,NUMMAT)
      INTEGER,INTENT(IN) :: USER_ID, INTERNAL_ID
      my_real,TARGET :: PM(NPROPM,NUMMAT),BUFMAT(*)
      my_real        :: UPARAM(*)
      CHARACTER*nchartitle, INTENT(IN) :: TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER          :: IDX_AV, IDX_RHO, IDX_C1, IDX_C2, IDX_C3(3), IDX_C4, IDX_C5, IDX_G
      INTEGER          :: IDX_E0, IDX_C0, IDX_PM, IDX_IPLA, IDX_EINF, IDX_VISC
      INTEGER          :: IDX_YIELD(4)
      INTEGER          :: MID(4),MID_VALID(4),IEXP,NPAR,IADBUF,IDX_PSH_TAB
      INTEGER          :: IMID, MLN, EOS_TYPE, I, J, NJWL, COUNT_VALID_MAT, COUNT_NONEXPLO, ID,  NBMAT, TAG(4),IPLA,NITER
      INTEGER,EXTERNAL :: NINTRI

      my_real                        :: AV(4),E1_INF,E2_INF,E3_INF,E4_INF,PEXT,RATIO,TMP1,TMP2,PSH_TAB(4),RHO_MAX
      my_real, POINTER, DIMENSION(:) :: PM_
      my_real, EXTERNAL              :: IE_BOUND
      my_real, DIMENSION(:), POINTER :: UPARAM_

      CHARACTER*nchartitle :: chain1

      !====================================POLYNOMIAL EOS
      my_real :: RHO,C0,C1,C2,C3,C4,C5,E0,PSH,P0,DPDMU,SSP

      !====================================LAW05 - PARAMETERS
      my_real :: VDET,PCJ,VCJ,B1,B2,R1,R2,W,
     .           PM4,AV4,RHO40,E04,C04,C14,
     .           TMELT4,THETL4,SPH4,T40,XKA4,XKB4,SSP4,
     .           EADD,TBEGIN,TEND,REACTION_RATE,A_MIL,M_MIL,N_MIL,REACTION_RATE2,ALPHA_UNIT
      INTEGER :: IBFRAC,QOPT,NEXPLO,IMIN

      !====================================LAW03 - PARAMETERS
      my_real :: YOUNG,ANU,G,BULK,PMIN,CA,CB,CN,EPSM,SIGM,GG

      !====================================LAW04 - PARAMETERS
      my_real :: CC,EPS0,M,TMELT,TMAX,CS,SPH,T0

      !====================================LAW06 - PARAMETERS
      my_real :: VISC

      !====================================LAW10 - PARAMETERS
      my_real :: A0,A1,A2,AMX,PSTAR

C-----------------------------------------------
C   P r e c o n d i t i o n
C-----------------------------------------------
C   already checked before subroutine call
C-----------------------------------------------
C   I n i t i a l i z a t i o n
C-----------------------------------------------
      IDX_AV       = 003
      IDX_RHO      = 008
      IDX_C1       = 011
      IDX_C2       = 014
      IDX_C3(1:3)  = (/018,020,021/)
      IDX_C4       = 021
      IDX_C5       = 024
      IDX_G        = 027
      IDX_E0       = 031
      IDX_C0       = 034
      IDX_PM       = 038
      IDX_EINF     = 056
      IDX_IPLA     = 063
      IDX_VISC     = 080
      IDX_YIELD(1) = 100
      IDX_YIELD(2) = 150
      IDX_YIELD(3) = 200
      IDX_YIELD(4) = 250
      NEXPLO=0

      !filled in lecm51
      MID(1:4) = NINT(UPARAM(9:12))
      AV(1:4)  = UPARAM(13:16)
      UPARAM(9:280)=ZERO
      UPARAM(31) = 12
      MID_VALID(1:4)=0
      COUNT_VALID_MAT = 0
      COUNT_NONEXPLO = 0

      !pointers
      NULLIFY(PM_)

      !PSTAR INIT
      UPARAM(123)=-INFINITY
      UPARAM(173)=-INFINITY
      UPARAM(223)=-INFINITY

      !Ei_INF
      UPARAM(57) = -INFINITY
      UPARAM(58) = -INFINITY
      UPARAM(59) = -INFINITY
      UPARAM(60) = ZERO !-INFINITY

      IEXP     = 0
      NJWL     = 0
      TAG(1:4) = 0
      IPLA     = 0

      G  = ZERO
      GG = ZERO

      PSH_TAB(1:4)=ZERO
      IDX_PSH_TAB = 0
      PEXT = ZERO
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        DO I=1,4
          IF(MID(I) == 0)EXIT !if defined
          IMID     = NINTRI(MID(I),IPM,NPROPMI,NUMMAT,1) !internal ID
          MLN      = 0
          ID       = 0
          EOS_TYPE = 0
          IF(IMID /= 0)THEN
            MLN      = IPM(2,IMID)
            ID       = IPM(1,IMID)
            EOS_TYPE = IPM(4,IMID)
          ENDIF
          IF(MLN == 5)NJWL=NJWL+1
          IF(IMID == 0)THEN
              chain1='NON EXISTING SUBMATERIAL IDENTIFIER:          '
              write(chain1(37:46),'(i10)')MID(I)
              IF(MID(I) > 0) THEN
                CALL ANCMSG(MSGID=99,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=USER_ID,
     .                    C1=TITR,
     .                    C2=chain1)
              ELSE
                !already checked in hm_read_mat51 (MID(I)=0 or MID(I) <0)
              ENDIF
          ELSE
            IF(MLN == 3 .OR. MLN == 4 .OR. MLN == 5 .OR. MLN == 6 .OR. MLN == 10 .OR. MLN == 102)THEN
              IF(MLN /= 5)THEN
                IF(EOS_TYPE == 18 .OR. EOS_TYPE == 1 .OR. EOS_TYPE == 7 .OR. EOS_TYPE == 10)THEN
                ELSE
                chain1='SUBMATERIAL COMPATIBLE EOS:POLYNOMIAL, IDEAL-GAS, STIFFENED-GAS'
                CALL ANCMSG(MSGID=99,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=ID,
     .                      C1=TITR,
     .                      C2=chain1)
                ENDIF
              ELSE
                IEXP=1
                NEXPLO=NEXPLO+1
              ENDIF
              IF(IMID > 0)THEN
                COUNT_VALID_MAT = COUNT_VALID_MAT + 1    !take into account only line with MID>0
                MID_VALID(COUNT_VALID_MAT) = IMID
                !--UPARAM(276:280) is BIJECTION TO RETRIEVE PHASE ORDER FROM USER
                IF(MLN /= 5)THEN
                  COUNT_NONEXPLO=COUNT_NONEXPLO + 1
                  UPARAM(276+COUNT_VALID_MAT)=MINLOC(TAG(1:4),1)
                  TAG(COUNT_NONEXPLO)=1
                ELSE
                  UPARAM(276+COUNT_VALID_MAT)=4
                  TAG(4)=1
                ENDIF
              ENDIF
            ELSE
              chain1='SUBMATERIAL CAN ONLY BE DEFINED FROM LAWS 3,4,5,6,10 OR 102 '
              CALL ANCMSG(MSGID=99,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    C2=chain1)
            ENDIF
           ENDIF
       ENDDO !next I

       ! fill missing phases if user defines less 1,2,or 3 ; (ie, user did not defined all 4)
       !  example UPARAM(276+4)=0 if user defines only 3 submaterial, then output related to phase 4 has wrond index : memory corruption)
       ! (/1,2,4,0/) -> (/1,2,4,3/)
       DO I=COUNT_VALID_MAT+1,4
         IMIN = MINLOC(TAG(1:4),1)
         IF(TAG(IMIN)==0)THEN
            TAG(I)=1
            UPARAM(276+I)=IMIN
          ELSE
            EXIT
         ENDIF
       ENDDO

        NBMAT = COUNT_VALID_MAT
        UPARAM(55)=1 !IEXP

       IF(NEXPLO>1)THEN
          chain1='ONLY ONE EXPLOSIVE SUBMATERIAL CAN BE DEFINED'
          CALL ANCMSG(MSGID=99,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                C2=chain1)
       ENDIF

!ALREADY CHECKED IN LECM51
!   chain1='AT LEAST ONE SUBMATERIAL MUST BE DEFINED'


       IF(NBMAT>4)THEN
          chain1='LAW51 IS COMPATIBLE WITH UP TO 4 SUBMATERIAL ONLY'
          CALL ANCMSG(MSGID=99,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                C2=chain1)
       ENDIF

       IPM(5,INTERNAL_ID)=COUNT_VALID_MAT  !storing NUMBER OF SUBMATERIALS
       PM(27,INTERNAL_ID) = ZERO

       RHO_MAX=ZERO
       DO I=1,COUNT_VALID_MAT
         IMID     = NINTRI(MID(I),IPM,NPROPMI,NUMMAT,1)
         IPM(20+I,INTERNAL_ID)=IMID        !storing internal material IDs
         IPM(50+I,INTERNAL_ID)=UPARAM(276+I)        !storing bijective order of submaterial
         MLN      = IPM(2,IMID)
         EOS_TYPE = IPM(4,IMID)
         PM_      => PM(1:,IMID)
         RHO_MAX=MAX(RHO_MAX,PM(1,IMID))
         IF(MLN /= 5)THEN
           SELECT CASE(EOS_TYPE)
               !EOS PARAMETERS DEPENDS ON 'EOS_TYPE'
               CASE(1,18)                  !18:/EOS/LINEAR
                 RHO    = PM_(1)           !1 :/EOS/POLYNOMIAL
                 E0     = PM_(23)
                 C0     = PM_(104)   !C0-PSH
                 C1     = PM_(32)
                 C2     = PM_(33)
                 C3     = PM_(34)
                 C4     = PM_(35)
                 C5     = PM_(36)
                 PSH    = PM_(88)
                 T0     = PM_(35)
               CASE(7)                     !07:/EOS/IDEAL-GAS
                 RHO    = PM_(1)
                 E0     = PM_(23)
                 C0     = -PM_(88)
                 C1     = ZERO
                 C2     = ZERO
                 C3     = ZERO
                 C4     = PM_(32)-ONE
                 C5     = PM_(32)-ONE
                 PSH    = PM_(88)
                 T0     = PM_(35)
               CASE(10)                    !10:/EOS/STIFF-GAS
                 RHO    = PM_(1)
                 E0     = PM_(23)
                 C0     = -PM_(34)*PM_(35)-PM_(88)  !-gamma*P_star  - PSH
                 C1     = ZERO
                 C2     = ZERO
                 C3     = ZERO
                 C4     = PM_(34)-ONE
                 C5     = PM_(34)-ONE
                 PSH    = PM_(88)
                 T0     = PM_(35)
               CASE DEFAULT
            END SELECT
            !----update PSH
            IDX_PSH_TAB = IDX_PSH_TAB + 1
            PSH_TAB(IDX_PSH_TAB)=PSH
            !no yield
            IF(MLN == 6)THEN
              VISC = PM_(24) !use submaterial viscosity (if defined with /MAT/LAW6)
            ELSE
              VISC = UPARAM(1) !use global viscosity otherwise (if defined with /MAT/LAW51)
            ENDIF
            PMIN   = PM_(37)
            SPH    = PM_(69)
            P0     = C0+C4*E0
            G      =  PM_(22)
            DPDMU  = (C1+C5*E0) + C4*(P0)
            SSP    = SQRT( (DPDMU + TWO_THIRD*G) / RHO )
            PM(27,INTERNAL_ID) = MAX( PM(27,INTERNAL_ID), SSP  )
            !---WRITING LAW51 SUBMATERIAL BUFFER FOR EOS
            J = UPARAM(276+I)
            UPARAM(IDX_AV      +J) = AV(I)
            UPARAM(IDX_RHO     +J) = RHO
            UPARAM(IDX_C0      +J) = C0
            UPARAM(IDX_C1      +J) = C1
            UPARAM(IDX_C2      +J) = C2
            UPARAM(IDX_C3(J))      = C3
            UPARAM(IDX_C4      +J) = C4
            UPARAM(IDX_C5      +J) = C5
            UPARAM(IDX_E0      +J) = E0
            UPARAM(IDX_YIELD(J)+13)= T0
            UPARAM(IDX_PM      +J) = PMIN
            UPARAM(IDX_YIELD(J)+12)= SPH
            UPARAM(IDX_YIELD(J)+24) = SSP
            UPARAM(IDX_YIELD(J)+26) = RHO*SSP*SSP
            UPARAM(IDX_VISC+J) = VISC
         ENDIF !(MLN/=5)

         SELECT CASE(MLN)
           CASE (5)
              !---READING MATERIAL LAW5 BUFFER
              VDET           = PM_(38)
              PCJ            = PM_(39)
              B1             = PM_(33)
              B2             = PM_(34)
              R1             = PM_(35)
              R2             = PM_(36)
              W              = PM_(45)
              IBFRAC         = NINT(PM_(41))
              QOPT           = NINT(PM_(42))
              PSH            = PM_(88)
              PM4            = -PSH
              AV4            = AV(I)
              RHO40          = PM_(1)
              E04            = PM_(23)
              C04            = PM_(43)-PM_(88)
              C14            = PM_(44)
              TMELT4         = INFINITY
              THETL4         = INFINITY
              SPH4           = ONE
              T40            = THREE100
              XKA4           = EM20
              XKB4           = ZERO
              SSP4           = VDET
              EADD           = PM_(160)
              TBEGIN         = PM_(161)
              TEND           = PM_(162)
              REACTION_RATE  = PM_(163)
              A_MIL          = PM_(164)
              M_MIL          = PM_(165)
              N_MIL          = PM_(166)
              REACTION_RATE2 = PM_(167)
              ALPHA_UNIT     = PM_(168)
              !---WRITING LAW51 SUBMATERIAL BUFFER
              UPARAM(42) = VDET
              UPARAM(43) = PCJ
              IF(PCJ > EM20)THEN
                UPARAM(44) = RHO40 * VDET**2 / PCJ
              ELSE
                UPARAM(44) = INFINITY
              END IF
              UPARAM(45)  = B1
              VCJ   = ONE  - ONE/UPARAM(44)
              UPARAM(46)  = AV4
              UPARAM(47)  = RHO40
              IF(UPARAM(47)==ZERO) UPARAM(47) = EM20
              UPARAM(48)  = E04
              UPARAM(49)  = C04
              UPARAM(50)  = C14
              UPARAM(51)  = B2
              UPARAM(52)  = R1
              UPARAM(53)  = R2
              UPARAM(54)  = W
              UPARAM(55)  = IEXP
              IF(PM4==ZERO)PM4=-INFINITY
              UPARAM(56)  = PM4
              UPARAM(68)  = IBFRAC
              UPARAM(258) = TMELT4
              UPARAM(259) = THETL4
              UPARAM(262) = SPH4
              UPARAM(263) = T40
              UPARAM(264) = XKA4
              UPARAM(265) = XKB4
              UPARAM(273) = SSP4
              UPARAM(274) = ZERO
              UPARAM(275) = RHO40*SSP4*SSP4
              UPARAM(276) = ZERO

              IDX_PSH_TAB = IDX_PSH_TAB + 1
              PSH_TAB(IDX_PSH_TAB) = PSH
              PM(27,INTERNAL_ID) = MAX( PM(27,INTERNAL_ID), SSP4  )

              !MSG - specific case of law5 when used with law 51 (new input format iform=12)
              IF(C14 <= ZERO)THEN
                chain1='BULK MODULUS OF LAW5 (JWL) MUST BE PROVIDED FOR UNREACTED EXPLOSIVE'
                CALL ANCMSG(MSGID=99,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      I1=USER_ID,
     .                      C1=TITR,
     .                      C2=chain1)
              ENDIF
           CASE(3,4)
             !yield criteria
             YOUNG  =  PM_(20)
             ANU    =  ZEP2 !PM_(21)
             G      =  PM_(22)
             BULK   =  PM_(32)
             PMIN   =  PM_(37)
             CA     =  PM_(38)
             CB     =  PM_(39)
             CN     =  PM_(40)
             EPSM   =  PM_(41)
             SIGM   =  PM_(42)
             CC     =  PM_(43)
             EPS0   =  PM_(44)
             M      =  PM_(45)
             TMELT  =  PM_(46)
             TMAX   =  PM_(47)
             CS     =  PM_(48)
             SPH    =  PM_(69)
             T0     =  PM_(79)
             GG     =  TWO*G
             SSP    = SQRT( (DPDMU + TWO_THIRD*G) / RHO )
             !---WRITING LAW51 SUBMATERIAL BUFFER
             J = UPARAM(276+I)
             UPARAM(IDX_IPLA    +J)  = 1
             IPLA                    = 1
             UPARAM(IDX_G       +J)  = GG
             UPARAM(IDX_YIELD(J)+01) = G
             UPARAM(IDX_YIELD(J)+02) = CA
             UPARAM(IDX_YIELD(J)+03) = CB
             UPARAM(IDX_YIELD(J)+04) = CN
             !specific law4 subcase
             UPARAM(IDX_YIELD(J)+05:IDX_YIELD(J)+13)=ZERO
             IF(MLN == 4)THEN
             UPARAM(IDX_YIELD(J)+05) = CC
             UPARAM(IDX_YIELD(J)+06) = EPS0
             UPARAM(IDX_YIELD(J)+07) = M
             UPARAM(IDX_YIELD(J)+08) = TMELT
             UPARAM(IDX_YIELD(J)+09) = TMAX
             UPARAM(IDX_YIELD(J)+12) = SPH
             UPARAM(IDX_YIELD(J)+13) = T0
             ENDIF
             UPARAM(IDX_YIELD(J)+10) = EPSM
             UPARAM(IDX_YIELD(J)+11) = SIGM
             UPARAM(IDX_YIELD(J)+14) = ZERO
             UPARAM(IDX_YIELD(J)+15) = ZERO
             UPARAM(IDX_YIELD(J)+16) = ZERO
             UPARAM(IDX_YIELD(J)+17) = ZERO
             UPARAM(IDX_YIELD(J)+18) = ZERO
             UPARAM(IDX_YIELD(J)+19) = ZERO
             UPARAM(IDX_YIELD(J)+20) = ZERO
             UPARAM(IDX_YIELD(J)+21) = ZERO
             UPARAM(IDX_YIELD(J)+22) = ANU
             UPARAM(IDX_YIELD(J)+23) = -INFINITY
             UPARAM(IDX_YIELD(J)+24) = SSP
             UPARAM(IDX_YIELD(J)+25) = ZERO
             UPARAM(IDX_YIELD(J)+26) = RHO*SSP*SSP

           CASE(6)
             !no yield criteria
             T0    = THREE100
             G     = ZERO
             GG    = ZERO
             EPSM  = ZERO
             SIGM  = ZERO
             CA    = ZERO
             CB    = ZERO
             CN    = ZERO
             CC    = ZERO
             EPS0  = ZERO
             M     = ZERO
             TMELT = ZERO
             TMAX  = ZERO
             SPH   = ZERO
             ANU   = ZERO
             AMX   = ZERO
             PSTAR = ZERO
             YOUNG = ZERO
             A0    = ZERO
             A1    = ZERO
             A2    = ZERO
             AMX   = ZERO
             SSP   = SQRT( (DPDMU + ZERO) / RHO )
             !---WRITING LAW51 SUBMATERIAL BUFFER
             J = UPARAM(276+I)
             UPARAM(IDX_IPLA    +J)  = 0
             UPARAM(IDX_G       +J)  = GG
             UPARAM(IDX_YIELD(J)+01) = G
             UPARAM(IDX_YIELD(J)+02) = YOUNG
             UPARAM(IDX_YIELD(J)+05) = CC
             UPARAM(IDX_YIELD(J)+06) = EPS0
             UPARAM(IDX_YIELD(J)+07) = M
             UPARAM(IDX_YIELD(J)+08) = TMELT
             UPARAM(IDX_YIELD(J)+09) = TMAX
             UPARAM(IDX_YIELD(J)+12) = SPH
             UPARAM(IDX_YIELD(J)+13) = T0
             UPARAM(IDX_YIELD(J)+14) = ZERO
             UPARAM(IDX_YIELD(J)+15) = ZERO
             UPARAM(IDX_YIELD(J)+16) = A0
             UPARAM(IDX_YIELD(J)+17) = A1
             UPARAM(IDX_YIELD(J)+18) = A2
             UPARAM(IDX_YIELD(J)+19) = AMX
             UPARAM(IDX_YIELD(J)+20) = ZERO
             UPARAM(IDX_YIELD(J)+21) = ZERO
             UPARAM(IDX_YIELD(J)+22) = ANU
             UPARAM(IDX_YIELD(J)+23) = PSTAR
             UPARAM(IDX_YIELD(J)+24) = SSP
             UPARAM(IDX_YIELD(J)+25) = ZERO
             UPARAM(IDX_YIELD(J)+26) = RHO*SSP*SSP

           CASE(10,102)
             !yield criteria
             IF(MLN == 10)THEN
               YOUNG  =  PM_(20)
               ANU    =  PM_(21)
               G      =  PM_(22)
               BULK   =  PM_(32)
               PMIN   =  PM_(37)
               A0     =  PM_(38)
               A1     =  PM_(39)
               A2     =  PM_(40)
               AMX    =  PM_(41)
               PSTAR  =  PM_(44)
             ELSEIF(MLN == 102)THEN
               NPAR   = IPM(9,IMID)
               IADBUF = IPM(7,IMID)
               IADBUF = MAX(1,IADBUF)
               UPARAM_ => BUFMAT(IADBUF:IADBUF+NPAR)
               YOUNG  =  UPARAM_(10)
               ANU    =  UPARAM_(11)
               G      =  UPARAM_(08)
               BULK   =  PM_(32)
               IF(BULK == ZERO)BULK=THIRD*YOUNG/(ONE-TWO*ANU)
               PMIN   =  PM_(37)
               A0     =  UPARAM_(04)
               A1     =  UPARAM_(05)
               A2     =  UPARAM_(06)
               AMX    =  UPARAM_(07)
               PSTAR  =  UPARAM_(03)
             ENDIF
             GG     =  TWO*G
             SSP    = SQRT( (DPDMU + TWO_THIRD*G) / RHO )
             !---WRITING LAW51 SUBMATERIAL BUFFER
             J = UPARAM(276+I)
             UPARAM(IDX_IPLA    +J)  = 2
             IPLA                    = 1
             UPARAM(IDX_G       +J)  = GG
             UPARAM(IDX_YIELD(J)+01) = G
             UPARAM(IDX_YIELD(J)+02) = YOUNG
             UPARAM(IDX_YIELD(J)+14) = ZERO
             UPARAM(IDX_YIELD(J)+15) = ZERO
             UPARAM(IDX_YIELD(J)+16) = A0
             UPARAM(IDX_YIELD(J)+17) = A1
             UPARAM(IDX_YIELD(J)+18) = A2
             UPARAM(IDX_YIELD(J)+19) = AMX
             UPARAM(IDX_YIELD(J)+20) = ZERO
             UPARAM(IDX_YIELD(J)+21) = ZERO
             UPARAM(IDX_YIELD(J)+22) = ANU
             UPARAM(IDX_YIELD(J)+23) = PSTAR
             UPARAM(IDX_YIELD(J)+24) = SSP
             UPARAM(IDX_YIELD(J)+25) = ZERO
             UPARAM(IDX_YIELD(J)+26) = RHO*SSP*SSP

           CASE DEFAULT
             !not expected, pre-condition test done in lecm51, error message if MLN is not relevant

         END SELECT

       ENDDO! next SUBMAT

       PM(91,INTERNAL_ID)=RHO_MAX

       !CHECK CONSISTENCY OF PSH PARAMETERS
       IF(IDX_PSH_TAB > 0)THEN
         TMP1=MINVAL(PSH_TAB(1:IDX_PSH_TAB))
         TMP2=MAXVAL(PSH_TAB(1:IDX_PSH_TAB))
         IF(TMP1 == TMP2)THEN
           PEXT = TMP1
         ELSE
           chain1='SUBMATERIAL EOS MUST HAVE CONSISTENT PSH PARAMETERS'
           CALL ANCMSG(MSGID=99,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=USER_ID,C1=TITR,C2=chain1)
         ENDIF
         UPARAM(8) = PEXT
       ENDIF
C-----------------------------------------------
C   Default
C-----------------------------------------------
      !ABCS
      IF(UPARAM(38)==ZERO)      UPARAM(38)=ONE
      !SPH
      IF(UPARAM(112)==ZERO)    UPARAM(112)=ONE
      IF(UPARAM(162)==ZERO)    UPARAM(162)=ONE
      IF(UPARAM(212)==ZERO)    UPARAM(212)=ONE
      IF(UPARAM(262)==ZERO)    UPARAM(262)=ONE
      !JCOOK EXPONENT
      IF(UPARAM(104)==ZERO)    UPARAM(104)=ONE
      IF(UPARAM(154)==ZERO)    UPARAM(154)=ONE
      IF(UPARAM(204)==ZERO)    UPARAM(204)=ONE
      !IF(UPARAM(254)==ZERO)    UPARAM(254)=ONE
      !INITIAL TEMPERATURE
      IF(UPARAM(113)==ZERO)    UPARAM(113)=THREE100
      IF(UPARAM(163)==ZERO)    UPARAM(163)=THREE100
      IF(UPARAM(213)==ZERO)    UPARAM(213)=THREE100
      IF(UPARAM(263)==ZERO)    UPARAM(263)=THREE100
      !MAXIMUM PLASTIC STRAIN
      IF(UPARAM(110)==ZERO)    UPARAM(110)=INFINITY
      IF(UPARAM(160)==ZERO)    UPARAM(160)=INFINITY
      IF(UPARAM(210)==ZERO)    UPARAM(210)=INFINITY
      !IF(UPARAM(260)==ZERO)    UPARAM(260)=INFINITY
      !MAXIMUM STRESS
      IF(UPARAM(111)==ZERO)    UPARAM(111)=INFINITY
      IF(UPARAM(161)==ZERO)    UPARAM(161)=INFINITY
      IF(UPARAM(211)==ZERO)    UPARAM(211)=INFINITY
      !IF(UPARAM(261)==ZERO)    UPARAM(261)=INFINITY
      !MELTING TEMPERATURE
      IF(UPARAM(108)==ZERO)    UPARAM(108)=INFINITY
      IF(UPARAM(158)==ZERO)    UPARAM(158)=INFINITY
      IF(UPARAM(208)==ZERO)    UPARAM(208)=INFINITY
      IF(UPARAM(258)==ZERO)    UPARAM(258)=INFINITY
      !LIMIT TEMPERATURE
      IF(UPARAM(109)==ZERO)    UPARAM(109)=INFINITY
      IF(UPARAM(159)==ZERO)    UPARAM(159)=INFINITY
      IF(UPARAM(209)==ZERO)    UPARAM(209)=INFINITY
      IF(UPARAM(259)==ZERO)    UPARAM(259)=INFINITY
      !THERMAL CONDUCTIVITY
      IF(UPARAM(114)==ZERO)    UPARAM(114)=EM20
      IF(UPARAM(164)==ZERO)    UPARAM(164)=EM20
      IF(UPARAM(214)==ZERO)    UPARAM(214)=EM20
      IF(UPARAM(264)==ZERO)    UPARAM(264)=EM20
      !JCOOK EPS_DOT_REF
      IF(UPARAM(106)==ZERO)    UPARAM(106)=ONE
      IF(UPARAM(156)==ZERO)    UPARAM(156)=ONE
      IF(UPARAM(206)==ZERO)    UPARAM(206)=ONE
      !IF(UPARAM(256)==ZERO)    UPARAM(256)=ONE
      !DPRAG MUMAX
      IF(UPARAM(119)==ZERO)    UPARAM(119)=INFINITY
      IF(UPARAM(169)==ZERO)    UPARAM(169)=INFINITY
      IF(UPARAM(219)==ZERO)    UPARAM(219)=INFINITY
      !IF(UPARAM(269)==ZERO)    UPARAM(269)=INFINITY
      !DPRAG POISSON RATIO
      IF(UPARAM(122)==ZERO)    UPARAM(122)=ZEP2
      IF(UPARAM(172)==ZERO)    UPARAM(172)=ZEP2
      IF(UPARAM(222)==ZERO)    UPARAM(222)=ZEP2
      !IF(UPARAM(272)==ZERO)    UPARAM(272)=ZEP2
      !Drucker-Prager unload modulus
      IF(UPARAM(121) == ZERO)  UPARAM(121) = UPARAM(12)
      IF(UPARAM(171) == ZERO)  UPARAM(171) = UPARAM(13)
      IF(UPARAM(221) == ZERO)  UPARAM(221) = UPARAM(14)

      !E_INF
      PEXT   = ZERO
      E1_INF = IE_BOUND(PEXT,UPARAM(39),UPARAM(35),UPARAM(12),UPARAM(15),UPARAM(18),UPARAM(22),UPARAM(25),UPARAM(32))
      E2_INF = IE_BOUND(PEXT,UPARAM(40),UPARAM(36),UPARAM(13),UPARAM(16),UPARAM(20),UPARAM(23),UPARAM(26),UPARAM(33))
      E3_INF = IE_BOUND(PEXT,UPARAM(41),UPARAM(37),UPARAM(14),UPARAM(17),UPARAM(21),UPARAM(24),UPARAM(27),UPARAM(34))
      E4_INF = ZERO
      UPARAM(57) = E1_INF
      UPARAM(58) = E2_INF
      UPARAM(59) = E3_INF
      UPARAM(60) = E4_INF



      !GLOBAL PARAMETERS
       !IF(UPARAM(30)==ZERO)    UPARAM(30)=ZEP2
       UPARAM(62) = EM03
       UPARAM(69) = UPARAM(9)*UPARAM(4) + UPARAM(10)*UPARAM(5) + UPARAM(11)*UPARAM(6) + UPARAM(47)*UPARAM(46)
       !UPARAM(31) = 1
       UPARAM(72) = INFINITY
       IF(UPARAM(43) <= EM20) UPARAM(44)=INFINITY
       IF(UPARAM(47)==ZERO) UPARAM(47) = EM20
       IF(UPARAM(56)==ZERO) UPARAM(56)=-INFINITY
       UPARAM(63) = IPLA

       RATIO=UPARAM(74)
       IF(RATIO <= ZERO)THEN
         RATIO = 0.25D00   !ONE is for previous formulation (permitted large volume change)
         UPARAM(74)=RATIO
       ENDIF

       NITER=UPARAM(73)
       IF(NITER == 0)THEN
         NITER=10
         UPARAM(73)=NITER
       ENDIF
C-----------------------------------------------

      RETURN
      END SUBROUTINE
