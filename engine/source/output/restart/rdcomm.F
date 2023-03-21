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
Chd|  RDCOMI                        source/output/restart/rdcomm.F
Chd|-- called by -----------
Chd|        RDRESA                        source/output/restart/rdresa.F
Chd|-- calls ---------------
Chd|        READ_ARRAY                    ../common_source/comm/read_array.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|====================================================================
      SUBROUTINE RDCOMI(LMXVINT,LVARINT,TABVINT,MULTI_FVM,H3D_DATA,DYNAIN_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD 
      USE H3D_MOD 
      USE PINCHTYPE_MOD
      USE OUTMAX_MOD
      USE ANIM_MOD
      USE OUTPUTS_MOD
      USE DRAPE_MOD
      USE STATE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER LMXVINT,LVARINT,TABVINT(*)
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (DYNAIN_DATABASE), INTENT(INOUT) :: DYNAIN_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "lagmult.inc"
#include      "r4r8_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "com10_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "cong1_c.inc"
#include      "task_c.inc"
#include      "scrnoi_c.inc"
#include      "scrfs_c.inc"
#include      "scrcut_c.inc"
#include      "warn_c.inc"
#include      "couple_c.inc"
#include      "conv_c.inc"
#include      "scr18_c.inc"
#include      "scr02_c.inc"
#include      "check.inc"
#include      "stati_c.inc"
#include      "timeri_c.inc"
#include      "scr06_c.inc"
#include      "scrupw_c.inc"
#include      "scr17_c.inc"
#include      "rad2r_c.inc"
#include      "spmd_c.inc"
#include      "fxbcom.inc"
#include      "eigcom.inc"
#include      "chara_c.inc"
#include      "fluid_scr_c.inc"
#include      "flowcom.inc"
#include      "impl1_c.inc"
#include      "tabsiz_c.inc"
#include      "remesh_c.inc"
#include      "com_xfem1.inc"
#include      "sysunit.inc"
#include      "intstamp_c.inc"
#include      "thknod_c.inc"
#include      "sms_c.inc"
#include      "submodel.inc"
#include      "inter18.inc"
#include      "inter22.inc"
#include      "alefvm.inc"
#include      "drape_c.inc"
#include      "ige3d_c.inc"
#include      "thermal_c.inc"
#include      "alesolver.inc"
#include      "alemuscl.inc"
#include      "ale_param.inc"
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
          INTEGER LWASPH44C, I, NEXT,IDUM1,ISUSED
C-----------------------------------------------
C     LECTURE DES VARIABLES ENTIERES.
C-----------------------------------------------
      N2D     =TABVINT(1)
      NCPRI   =TABVINT(2)
      !IUSERWI =TABVINT(3)
      IALE    =TABVINT(4)
      NGROUP  =TABVINT(5)
      NCYCLE  =TABVINT(6)
      IRUN    =TABVINT(7)
      IGER    =TABVINT(8)
      LBUFEL  =TABVINT(9)
      IRODDL  =TABVINT(10)
      IEULER  =TABVINT(11)
      ITHERM  =TABVINT(12)
      IHSH    =TABVINT(13)
      ITESTV  =TABVINT(14)
      ITURB   =TABVINT(15)
      ILAG    =TABVINT(16)
      ISECUT  =TABVINT(17)
      IDAMP   =TABVINT(18)
      IRXDP   =TABVINT(19)
      NMULT   =TABVINT(20)
      INTEG8  =TABVINT(21)
      ISIGI   =TABVINT(22)
      NSPMD =TABVINT(23)
      LENWA   =TABVINT(24)
      ISGIFL  =TABVINT(25)
      ISUB    =TABVINT(26)
      NNODS   =TABVINT(27)
      NCNOIS  =TABVINT(28)
      LCNE0   =TABVINT(29)
      IPARI0  =TABVINT(30)
      IMAXIMP =TABVINT(31)
      NNOISER =TABVINT(32)
      NSPGROUP=TABVINT(33)
      IRESMD  =TABVINT(34)
      IFRWV   =TABVINT(35)
C-----
        ITESTV = IABS(ITESTV)
        IF (IR4R8==2) ITESTV = -ITESTV
C       ITESTV = IABS(ITESTV)
C-----
C   -------- Tailles des tableaux entiers --------
      SICODE    = TABVINT(37)
      SLSHSUB   = TABVINT(38)
      SISKEW    = TABVINT(39)
      SISKWN    = TABVINT(40)
      SIFRAME   = TABVINT(41)
      SNETH     = TABVINT(42)
      SIBCSLAG  = TABVINT(43)
      SIPART    = TABVINT(44)
      SNOM_OPT  = TABVINT(45)
      SNPC      = TABVINT(46)
C Empty field           = TABVINT(47)
      SIXTG     = TABVINT(48)
      SIXTG1    = TABVINT(49)
      SIXS      = TABVINT(50)
      SIXQ      = TABVINT(51)
      SIXC      = TABVINT(52)
      SIXT      = TABVINT(53)
      SIXP      = TABVINT(54)
      SIXR      = TABVINT(55)
      SITAB     = TABVINT(56)
      SITABM1   = TABVINT(57)
      SGJBUFI   = TABVINT(58)
      SNALE     = TABVINT(59)
      SIELVS    = TABVINT(60)
      SIFILL    = TABVINT(62)
      SIMS      = TABVINT(63)
!! to empty+++
      SISUBS    = TABVINT(64)
      SIGRN     = TABVINT(65)
      SISURF    = TABVINT(66)
      SISLIN    = TABVINT(67)
      SIBUFSSG  = TABVINT(68)
!! to empty---
      SKXX      = TABVINT(69)
      SIXX      = TABVINT(70)
      SKXSP     = TABVINT(71)
      SIXSP     = TABVINT(72)
      SNOD2SP   = TABVINT(73)
      SISPSYM   = TABVINT(74)
      SISPCOND  = TABVINT(75)
      SISPHIO   = TABVINT(76)
      SLPRTSPH  = TABVINT(77)
      SLONFSPH  = TABVINT(78)
      SIBCL     = TABVINT(79)
      SIBFV     = TABVINT(80)
      SIECRAN   = TABVINT(81)
      SILAS     = TABVINT(82)
      SIEBCS    = TABVINT(83)
      SLACCELM  = TABVINT(84)

      SNNLINK   = TABVINT(86)
      SLNLINK   = TABVINT(87)
      !SIUSER   = TABVINT(88) - unused
      SIPARG    = TABVINT(89)
      SDD_IAD   = TABVINT(90)
      SIGRV     = TABVINT(91)
      SLGRAV    = TABVINT(92)
      SIEXLNK   = TABVINT(93)
      SIBVEL    = TABVINT(94)
      SLBVEL    = TABVINT(95)
      SIACTIV   = TABVINT(96)
      SIBMPC    = TABVINT(97)
      SKINET    = TABVINT(98)
      SIPARI    = TABVINT(99)
      SINBUF    = 0 !TABVINT(100)
      SNPRW     = TABVINT(101)
      ! TABVINT(102) obsolete option removed
      SICONX    = TABVINT(103)
      SMADPRT   = TABVINT(104)
      SMADSH4   = TABVINT(105)
      SMADSH3   = TABVINT(106)
      SMADSOL   = TABVINT(107)
      SMADNOD   = TABVINT(108)
      SMADFAIL  = TABVINT(109)
      SNPBY     = TABVINT(110)
      SNPBYL    = TABVINT(111)
      SLPBY     = TABVINT(112)
      SLPBYL    = TABVINT(113)
      SLRIVET   = TABVINT(114)
      SNSTRF    = TABVINT(115)
      SLJOINT   = TABVINT(116)
      SNODPOR   = TABVINT(117)
      SMONVOL   = TABVINT(118)
      SLAGBUF   = TABVINT(119)
      SITHGRP   = TABVINT(120)
      SIAD_ELEM = TABVINT(121)
      SIAD_RBY  = TABVINT(122)
      SIAD_RBY2 = TABVINT(123)
      SIAD_I2M  = TABVINT(124)
      SIADCJ    = TABVINT(125)
      SIAD_RBM  = TABVINT(126)
      SIAD_RBM2 = TABVINT(127)
      SIAD_SEC  = TABVINT(128)
      SIAD_CUT  = TABVINT(129)
      SFR_ELEM  = TABVINT(130)
      SFR_RBY   = TABVINT(131)
      SFR_WALL  = TABVINT(132)
      SFR_RBY2  = TABVINT(133)
      SFR_I2M   = TABVINT(134)
      SFR_MV    = TABVINT(135)
      SFR_LL    = TABVINT(136)
      SFR_CJ    = TABVINT(137)
      SFR_RBM   = TABVINT(138)
      SFR_RBM2  = TABVINT(139)
      SFR_SEC   = TABVINT(140)
      SFR_CUT   = TABVINT(141)
      SRG_CUT   = TABVINT(142)
      SFR_MAD   = TABVINT(143)
      SFR_I18   = TABVINT(144)
      SWEIGHT	= TABVINT(145)
      SNEWFRONT = TABVINT(146)
      SNODGLOB  = TABVINT(147)
      SNBRCVOIS = TABVINT(148)
      SNBSDVOIS = TABVINT(149)
      SLNRCVOIS = TABVINT(150)
      SLNSDVOIS = TABVINT(151)
      SNERCVOIS = TABVINT(152)
      SNESDVOIS = TABVINT(153)
      SLERCVOIS = TABVINT(154)
      SLESDVOIS = TABVINT(155)
      !TABVINT(156) obsolete option removed
      !TABVINT(157) obsolete option removed
      SNPSEGCOM = TABVINT(158)
      SLSEGCOM  = TABVINT(159)
      SNPORGEO  = TABVINT(160)
      SLNODPOR  = TABVINT(161)
C-----
C   tailles des tableau flottant
C
      SX        = TABVINT(162)
      SD        = TABVINT(163)
      SV        = TABVINT(164)
      SVR       = TABVINT(165)
      SDR       = TABVINT(166)
      STHKE     = TABVINT(167)
      SDAMPR    = TABVINT(168)
      SDAMP     = TABVINT(169)
      SMS       = TABVINT(170)
      SIN       = TABVINT(171)
      STF       = TABVINT(172)
      SPM       = TABVINT(173)
      SSKEW     = TABVINT(174)
      SXFRAME   = TABVINT(175)
      SGEO      = TABVINT(176)
      SEANI     = TABVINT(177)
      SBUFMAT   = TABVINT(178)
      SBUFGEO   = TABVINT(179)
      SBUFSF    = TABVINT(180)
      SRBMPC    = TABVINT(181)
      SGJBUFR   = TABVINT(182)
      SW        = TABVINT(183)
      SVEUL     = TABVINT(184)
      SFILL     = TABVINT(185)
      SDFILL    = TABVINT(186)
      SALPH     = TABVINT(187)
      SWB       = TABVINT(188)
      SDSAVE    = TABVINT(189)
      SDRSAVE	= TABVINT(190)
      SASAVE    = TABVINT(191)
      SARSAVE   = TABVINT(192)
      SSTSAVE   = TABVINT(193)
      SSTRSAVE  = TABVINT(194)
      SFSAVSAV  = TABVINT(195)
      SSPBUF    = TABVINT(196)
      SVSPHIO   = TABVINT(200)
      SSPHVELN  = TABVINT(201)
      SMSNF     = TABVINT(202)
      SFORC     = TABVINT(203)
      SVEL      = TABVINT(204)
      SFSAV     = TABVINT(205)
      SFZERO    = TABVINT(206)
      SXLAS     = TABVINT(207)
      SREBCS    = TABVINT(208)
      SACCELM   = TABVINT(209)

      SFBVEL    = TABVINT(211)
      SGRAV     = TABVINT(212)
      SFR_WAVE  = TABVINT(213)
      SPARTS0   = TABVINT(214)
      !SUSER     = TABVINT(215) unused
      SELBUF    = TABVINT(216)
      SBUFIN    = 0 !TABVINT(217)
      SRWBUF    = TABVINT(218)
      SRWSAV    = TABVINT(219)
      ! TABVINT(220) obsolete option removed
      SRBY      = TABVINT(221)
      SRBYL     = TABVINT(222)
      SRIVET    = TABVINT(223)
      SSECBUF   = TABVINT(224)
      SVOLMON   = TABVINT(225)
      SLAMBDA   = TABVINT(226)
      SRCONX    = TABVINT(227)
      SFASOLFR  = TABVINT(228)
      SDD_R2R   = TABVINT(229)
      SLPRW     = TABVINT(230)
      !TABVINT(231) obsolete option removed
      SIEXMAD   = TABVINT(232)
      SIGEO     = TABVINT(233)
      SIPM      = TABVINT(234)
      SKIPPON   = TABVINT(235)
      SSEGQUADFR= TABVINT(236)
C-----1
      NUMMAT =TABVINT(238)
      NUMNOD =TABVINT(239)
      NUMSKW =TABVINT(240)
      NUMBCS =TABVINT(241)
      NANALY =TABVINT(242)
      NUMELQ =TABVINT(243)
      NUMELS =TABVINT(244)
      NUMELC =TABVINT(245)
      NUMELT =TABVINT(246)
      NUMGEO =TABVINT(247)
      NFUNCT =TABVINT(248)
      NCONLD =TABVINT(249)
      NINVEL =TABVINT(250)
      NLASER =TABVINT(251)
      NINTER =TABVINT(252)
      NRWALL =TABVINT(253)
      NRBODY =TABVINT(254)
      NODMAS =TABVINT(255)
      NFXVEL =TABVINT(256)
      NRIVET =TABVINT(257)
      NUMELR =TABVINT(258)
      NUMELP =TABVINT(259)
      !obsolete option removed : TABVINT(260)=0
      NSECT  =TABVINT(261)
      NRBAG  =TABVINT(262)
      NJOINT =TABVINT(263)
      NWALE_RST = TABVINT(264)
      NWALE=NWALE_RST
!------------------------------!
      NUMELTG=TABVINT(265)
      NSLAG  =TABVINT(266)
      NFACX  =TABVINT(267)
      NUMPOR =TABVINT(268)
      NACCELM=TABVINT(269)
      NSENSOR=TABVINT(270)
      NPRETEN=TABVINT(271)
      NVOLU  =TABVINT(272)
      NPART  =TABVINT(273)
      NSURF =TABVINT(274)
      NSUBS =TABVINT(275)
      NGRAV  =TABVINT(276)
      NRBY2  =TABVINT(277)
      NGRNOD =TABVINT(278)
      NGRBRIC=TABVINT(279)
      NGRQUAD=TABVINT(280)
      NGRSHEL=TABVINT(281)
      NGRSH3N=TABVINT(282)
      NGRBEAM=TABVINT(283)
      NGRTRUS=TABVINT(284)
      NGRSPRI=TABVINT(285)
      NLINK  =TABVINT(286)
      INVSTR =TABVINT(287)
      NSLIN  =TABVINT(288)
      NUMELX =TABVINT(289)
      NCONX  =TABVINT(290)
      ISUMNX =TABVINT(291)
      NANIM1D=TABVINT(292)
      NR2RLNK=TABVINT(293)
C-----
      IPAT  =TABVINT(294)
      IANIM =TABVINT(295)
      IOUTP =TABVINT(296)
      NHIN2 =TABVINT(297)
      IDELI7=TABVINT(298)
C-----2
      NSNOD  =TABVINT(299)
      NSELQ  =TABVINT(300)
      NSELS  =TABVINT(301)
      NSELC  =TABVINT(302)
      NSELT  =TABVINT(303)
      NSELP  =TABVINT(304)
      NSELR  =TABVINT(305)
      NSRBY  =TABVINT(306)
      NSMAT  =TABVINT(307)
      NSELTG =TABVINT(308)
      NTHGRP =TABVINT(309)
      NTHGRP0=TABVINT(310)
      NSELU  =TABVINT(311)
C-----
      NPROPM =TABVINT(312)
      NVSIZ  =TABVINT(313)
      NPROPG =TABVINT(314)
      NPARG  =TABVINT(315)
      LVEUL  =TABVINT(316)
      NIXFR1 =TABVINT(325)
      NIXFR2 =TABVINT(326)
      NPARI  =TABVINT(327)
      LWAMP  =TABVINT(328)
      LWANMP =TABVINT(329)
      NISX   =TABVINT(330)
      LIGRN1  =TABVINT(331)
      LISUB1 =TABVINT(332)
      NIMV   =TABVINT(333)
      NNPBY  =TABVINT(334)
      NRBY   =TABVINT(335)
      NIFV   =TABVINT(336)
      NTHVKI =TABVINT(337)
      NPSAV  =TABVINT(338)
      NRCNX  =TABVINT(339)
      NR2R   =TABVINT(340)
      NUMFRAM=TABVINT(379)
      NUMSPH  =TABVINT(380)
      NSELSP  =TABVINT(381)
      NSPCOND =TABVINT(382)
      NSPHSYM =TABVINT(384)
      MAXPJET =TABVINT(385)
      NISP    =TABVINT(386)
      KVOISPH =TABVINT(387)
      NSPBUF  =TABVINT(388)
      NISPCOND=TABVINT(389)
      INTBAG=TABVINT(425)
      IBAGSURF=TABVINT(426)
      NRVOLU  =TABVINT(427)
      IF(NRVOLU==0)NRVOLU=24
      NUMELS10  =TABVINT(428)
      NUMELS20  =TABVINT(429)
      IDAMPG=TABVINT(430)
      ICLOSE=TABVINT(431)
      NSENSI = TABVINT(432)
      NSENSR = TABVINT(433)
      IF(NSENSI==0) NSENSI = 15
      IF(NSENSR==0) NSENSR = 20
      NIBVEL=TABVINT(434)
      LWASPH=TABVINT(435)
      IF(LWASPH==0)LWASPH=16*NUMSPH
C     from version 4.4c :
      LWASPH44C=17*NUMSPH
      IF(LWASPH<LWASPH44C)LWASPH=LWASPH44C
C-----
      NUMELS16  =TABVINT(436)
      NUMELS8   =TABVINT(437)
C test pour compatibilite avec anciens restarts
      IF(NUMELS8==0)NUMELS8 =
     .             NUMELS - (NUMELS10+NUMELS20+NUMELS16)
      NRIVF = TABVINT(438)
      IF (NRIVF==0) NRIVF = 1
      NXFRAME=TABVINT(439)
      NSPHIO   =TABVINT(440)
      NISPHIO  =TABVINT(441)
      LAG_NCF =TABVINT(442)
      LAG_NKF =TABVINT(443)
      LAG_NHF =TABVINT(444)
      LAG_NCL =TABVINT(445)
      LAG_NKL =TABVINT(446)
      LAG_NHL =TABVINT(447)
      NBCSLAG =TABVINT(448)
      LACTIV  =TABVINT(449)
      NACTIV  =TABVINT(450)
      NDAMP   =TABVINT(451)
C-----
      NICBAG=TABVINT(452)
C     versions 4.4X, X <= D :
      IF(NICBAG==0)NICBAG=3
      NRCBAG=TABVINT(453)
C     versions 4.4X, X <= D :
      IF(NRCBAG==0)NRCBAG=3
C     not available for version <= 4.4D :
      LICBAG  =TABVINT(454)
      LRCBAG  =TABVINT(455)
      NIBJET  =TABVINT(456)
      NRBJET  =TABVINT(457)
      NIBHOL  =TABVINT(458)
      NRBHOL  =TABVINT(459)
      LIBAGJET=TABVINT(460)
      LRBAGJET=TABVINT(461)
      LIBAGHOL=TABVINT(462)
      LRBAGHOL=TABVINT(463)
      NGJOINT =TABVINT(464)
      NUMMPC  =TABVINT(465)
      LVSPHIO =TABVINT(466)
      SITHGRPA = TABVINT(467)
      SITHGRPB = TABVINT(468)
      SITHGRPC = TABVINT(469)
      SITHGRPD = TABVINT(470)
      SITHGRPE = TABVINT(471)
      SITHGRPF = TABVINT(472)
      SITHGRPG = TABVINT(473)
      SITHGRPH = TABVINT(474)
      SITHGRPI = TABVINT(475)
C
      NTHGRP01(1) = TABVINT(476)
      NTHGRP01(2) = TABVINT(477)
      NTHGRP01(3) = TABVINT(478)
      NTHGRP01(4) = TABVINT(479)
      NTHGRP01(5) = TABVINT(480)
      NTHGRP01(6) = TABVINT(481)
      NTHGRP01(7) = TABVINT(482)
      NTHGRP01(8) = TABVINT(483)
      NTHGRP01(9) = TABVINT(484)
C
      NTHGRP1(1)  = TABVINT(485)
      NTHGRP1(2)  = TABVINT(486)
      NTHGRP1(3)  = TABVINT(487)
      NTHGRP1(4)  = TABVINT(488)
      NTHGRP1(5)  = TABVINT(489)
      NTHGRP1(6)  = TABVINT(490)
      NTHGRP1(7)  = TABVINT(491)
      NTHGRP1(8)  = TABVINT(492)
      NTHGRP1(9)  = TABVINT(493)
      
      NEXT = 493

      !output data must be read in case of mcheck/=0 (dynamic size)
      !otherwise LVARINT is increased to skip related range in TABVINT
      CALL READ_ARRAY(OUTP_V ,TABVINT, NEXT, MCHECK)
      CALL READ_ARRAY(OUTP_SS,TABVINT, NEXT, MCHECK)
      CALL READ_ARRAY(OUTP_ST,TABVINT, NEXT, MCHECK)      
      CALL READ_ARRAY(OUTP_CS,TABVINT, NEXT, MCHECK)
      CALL READ_ARRAY(OUTP_CT,TABVINT, NEXT, MCHECK)      
      CALL READ_ARRAY(OUTP_TS,TABVINT, NEXT, MCHECK)
      CALL READ_ARRAY(OUTP_PS,TABVINT, NEXT, MCHECK)      
      CALL READ_ARRAY(OUTP_PT,TABVINT, NEXT, MCHECK)
      CALL READ_ARRAY(OUTP_RS,TABVINT, NEXT, MCHECK)      
      CALL READ_ARRAY(OUTP_RT,TABVINT, NEXT, MCHECK)
      CALL READ_ARRAY(ANIM_V ,TABVINT, NEXT, MCHECK)      
      CALL READ_ARRAY(ANIM_T ,TABVINT, NEXT, MCHECK)
      CALL READ_ARRAY(ANIM_E ,TABVINT, NEXT, MCHECK)      
      CALL READ_ARRAY(ANIM_CE,TABVINT, NEXT, MCHECK)
      CALL READ_ARRAY(ANIM_SE,TABVINT, NEXT, MCHECK)      
      CALL READ_ARRAY(ANIM_FE,TABVINT, NEXT, MCHECK)
      CALL READ_ARRAY(ANIM_CT,TABVINT, NEXT, MCHECK)      
      CALL READ_ARRAY(ANIM_ST,TABVINT, NEXT, MCHECK)
      CALL READ_ARRAY(ANIM_FT,TABVINT, NEXT, MCHECK) 
      
      IF(MCHECK/=0)THEN
        NEXT=NEXT+1
        ANIM_M    = TABVINT(NEXT)
        NEXT=NEXT+1        
        ANIM_K    = TABVINT(NEXT)
        NEXT=NEXT+1        
        ANIM_U    = TABVINT(NEXT)
        NEXT=NEXT+1
        ANIM_MAT  = TABVINT(NEXT)
        NEXT=NEXT+1        
        NCUTS     = TABVINT(NEXT)
        NEXT=NEXT+1        
        IEPSDOT   = TABVINT(NEXT)
        NEXT=NEXT+1        
        FMT_ANI   = TABVINT(NEXT)
        NEXT=NEXT+1        
        ANIM_VERS = TABVINT(NEXT)
        NEXT=NEXT+1        
        IZIP      = TABVINT(NEXT)        
      ELSE
        NEXT=NEXT+9
      ENDIF
           
      NEXT=NEXT+1 
      NRBYLAG =TABVINT(NEXT)
      NEXT = NEXT+1
      NFVLAG  =TABVINT(NEXT)
      NEXT = NEXT+1
      NNPRW   =TABVINT(NEXT)
      NEXT = NEXT+1
      NRWLAG  =TABVINT(NEXT)
      NEXT = NEXT+1
      NHMAX   =TABVINT(NEXT)
      NEXT = NEXT+1
      LAGMOD  =TABVINT(NEXT)
      NEXT = NEXT+1
      LAGOPT  =TABVINT(NEXT)
      NEXT = NEXT+1
      NSEGFLU   =TABVINT(NEXT)
      NEXT = NEXT+1
      NVCONV    =TABVINT(NEXT)
C-----
      DO I=1,LCONV
        CODV(I)=TABVINT(NEXT+I)
      ENDDO
C-----
      NEXT=NEXT+LCONV+1
      NPEBC=TABVINT(NEXT)
      NEXT = NEXT+1
      NEBCS=TABVINT(NEXT)
      NEXT = NEXT+1
      NRBYKIN  =TABVINT(NEXT)
      NEXT = NEXT+1
      NBCSKIN  =TABVINT(NEXT)
      NEXT = NEXT+1
      NUMELS8A =TABVINT(NEXT)
      NEXT = NEXT+1
      NUMELTG6 =TABVINT(NEXT)
C--------------------------------------
      IF(MCHECK/=0)THEN
        DO I=1,12
         IDTMIN(I)= TABVINT(NEXT + I)
        ENDDO
        IDTGR(11) = TABVINT(NEXT + 13 )
        NODADT    = TABVINT(NEXT + 14 )
        NPROC     = TABVINT(NEXT + 15 )
C No more used        TABVINT(NEXT + 16 )
        TH_VERS   = TABVINT(NEXT + 17 )
        ISTATG    = TABVINT(NEXT + 18 )
        ISTAT     = TABVINT(NEXT + 19 )
        NTH       = TABVINT(NEXT + 20 )
        NANIM     = TABVINT(NEXT + 21 )
C No more used        TABVINT(NEXT + 22 )
        IMON      = TABVINT(NEXT + 23 )
        IDUM1     = TABVINT(NEXT + 24 )
        DO I =1,10
         DEBUG(I) = TABVINT(NEXT + 24 + I )
        ENDDO
        ITRACE    = TABVINT(NEXT + 35 )
        INCOMP    = TABVINT(NEXT + 36 )

        IALESUB   = TABVINT(NEXT + 38 )
        IMPL      = TABVINT(NEXT + 39 )
        NITMX     = TABVINT(NEXT + 40 )
        UPWM      = TABVINT(NEXT + 41 )
        IDDW      = TABVINT(NEXT + 43 )
        IPREAD    = TABVINT(NEXT + 44 )
        NSFLSW    = TABVINT(NEXT + 45 )
        NTFLSW    = TABVINT(NEXT + 46 )
       ! IUSERW    = TABVINT(NEXT + 47 )
       ! NUSERW    = TABVINT(NEXT + 48 )
        IRAD2R    = TABVINT(NEXT + 49 )
        NNOISE    = TABVINT(NEXT + 50 )
        RNOI      = TABVINT(NEXT + 51 )
        NOISEV    = TABVINT(NEXT + 52 )
        NOISEA    = TABVINT(NEXT + 53 )
        NOISEP    = TABVINT(NEXT + 54 )
c link
        NRLINK    = TABVINT(NEXT + 55 )
        NALELK    = TABVINT(NEXT + 56 )
        LLLINK    = TABVINT(NEXT + 57 )
        !!!LLINAL=TABVINT(LVARINT + 58 )   
      ENDIF

      NEXT=NEXT+58
      CALL READ_ARRAY(ANIM_N ,TABVINT, NEXT, MCHECK)
C
      NEXT = NEXT+1
      IOUTP_FMT = TABVINT(NEXT)
      NEXT = NEXT+1
      NPROPGI = TABVINT(NEXT)
      NEXT = NEXT+1
      NPROPMI = TABVINT(NEXT)
      NEXT = NEXT+1
      NEXMAD =TABVINT(NEXT)
      NEXT = NEXT+1
      NMADPRT =TABVINT(NEXT)
      NEXT = NEXT+1
      NMADSH4 =TABVINT(NEXT)
      NEXT = NEXT+1
      NMADSH3 =TABVINT(NEXT)
      NEXT = NEXT+1
      NMADSOL =TABVINT(NEXT)
      NEXT = NEXT+1
      NMADNOD =TABVINT(NEXT)
      NEXT = NEXT+1
      ISHFRAM =TABVINT(NEXT)
C--------------------------------------
      NEXT = NEXT+1
      NSKYRW0 = TABVINT(NEXT)
      NEXT = NEXT+1
      NSKYRBK0 = TABVINT(NEXT)
      NEXT = NEXT+1
      NBDDNOD = TABVINT(NEXT)
      NEXT = NEXT+1
      NBDDNRB = TABVINT(NEXT)
      NEXT = NEXT+1
      MULTIMAX = TABVINT(NEXT)
      NEXT = NEXT+1
      NBDDI2M = TABVINT(NEXT)
      NEXT = NEXT+1
      NISKYI2 = TABVINT(NEXT)
      NEXT = NEXT+1
      LCNI2 = TABVINT(NEXT)
      NEXT = NEXT+1
      I2NSNT = TABVINT(NEXT)
      NEXT = NEXT+1
      NSKYMV0 = TABVINT(NEXT)
      NEXT = NEXT+1
      NSKYMVC0 = TABVINT(NEXT)
      NEXT = NEXT+1
      NSKYLL0 = TABVINT(NEXT)
      NEXT = NEXT+1
      NBDDNCJ = TABVINT(NEXT)
      NEXT = NEXT+1
      NBDDNRBM = TABVINT(NEXT)
      NEXT = NEXT+1
      NSKYRBM0 = TABVINT(NEXT)
      NEXT = NEXT+1
      NNODTCUT = TABVINT(NEXT)
      NEXT = NEXT+1
      NNODLCUT = TABVINT(NEXT)
      NEXT = NEXT+1
      NBDDRBE3M = TABVINT(NEXT)
      NEXT = NEXT+1
      NSKYRBE30 = TABVINT(NEXT)
C
      NEXT = NEXT+1
      NPARIR = TABVINT(NEXT)
      NEXT = NEXT+1
      NUMNODG = TABVINT(NEXT)
      NEXT = NEXT+1
      NUMNODM = TABVINT(NEXT)
      NEXT = NEXT+1
      NUMELQG =  TABVINT(NEXT)
      NEXT = NEXT+1
      NUMELCG =  TABVINT(NEXT)
      NEXT = NEXT+1
      NUMELTGG =  TABVINT(NEXT)
      NEXT = NEXT+1
C       empty field
      NEXT = NEXT+1
      NUMELSG =  TABVINT(NEXT)
      NEXT = NEXT+1
      NUMELPG =  TABVINT(NEXT)
      NEXT = NEXT+1
      NUMELRG =  TABVINT(NEXT)
      NEXT = NEXT+1
      NUMELTRG =  TABVINT(NEXT)
      NEXT = NEXT+1
      NB1DG =  TABVINT(NEXT)
      NEXT = NEXT+1
      NSKEWAG =  TABVINT(NEXT)
      NEXT = NEXT+1
      SECTIOM =  TABVINT(NEXT)
      NEXT = NEXT+1
      NUMELS16G =  TABVINT(NEXT)
      NEXT = NEXT+1
      NUM16SHIFT = TABVINT(NEXT)
      NEXT = NEXT+1
      OUTYY_FMT = TABVINT(NEXT)
      NEXT = NEXT+1
      NFXBODY=TABVINT(NEXT)
      NEXT = NEXT+1
      LENNOD=TABVINT(NEXT)
      NEXT = NEXT+1
      LENMOD=TABVINT(NEXT)
      NEXT = NEXT+1
      LENGLM=TABVINT(NEXT)
      NEXT = NEXT+1
      LENCP=TABVINT(NEXT)
      NEXT = NEXT+1
      LENLM=TABVINT(NEXT)
      NEXT = NEXT+1
      LENFLS=TABVINT(NEXT)
      NEXT = NEXT+1
      LENDLS=TABVINT(NEXT)
      NEXT = NEXT+1
      LENVAR=TABVINT(NEXT)
      NEXT = NEXT+1
      LENRPM=TABVINT(NEXT)
      NEXT = NEXT+1
      LENMCD=TABVINT(NEXT)
      NEXT = NEXT+1
      LENELM=TABVINT(NEXT)
      NEXT = NEXT+1
      LENSIG=TABVINT(NEXT)
      NEXT = NEXT+1
      LENGRVI=TABVINT(NEXT)
      NEXT = NEXT+1
      LENGRVR=TABVINT(NEXT)
      NEXT = NEXT+1
      NBIPM=TABVINT(NEXT)
      NEXT = NEXT+1
      NSFXBY=TABVINT(NEXT)
      NEXT = NEXT+1
      NEIG=TABVINT(NEXT)
      NEXT=NEXT+1
      NEIPM=TABVINT(NEXT)
      NEXT=NEXT+1
      NERPM=TABVINT(NEXT)
      NEXT=NEXT+1
      LEIBUF=TABVINT(NEXT)
      NEXT = NEXT+1
      NINTSUB = TABVINT(NEXT)
      NEXT = NEXT+1
      MSHSUB   = TABVINT(NEXT)
      NEXT = NEXT+1
      NSHFRONT = TABVINT(NEXT)
C--------------------------------------
      NEXT = NEXT+1
      IROOTYY = TABVINT(NEXT)
      IF(MCHECK/=0)THEN
       INVERS     = TABVINT(NEXT + 1)
       NCRST      = TABVINT(NEXT + 2)
       IRLTYP     = TABVINT(NEXT + 3)
       IRETYP     = TABVINT(NEXT + 4)
       MULTIREST  = TABVINT(NEXT + 5)
       IRPREV     = TABVINT(NEXT + 6)
      ENDIF
      NEXT = NEXT + 6
      NEXT = NEXT+1
      TRIMAT=TABVINT(NEXT)
      NEXT = NEXT+1
      SPHSHIFT = TABVINT(NEXT)
      NEXT = NEXT+1
      NUMSPHG = TABVINT(NEXT)
      NEXT = NEXT+1
      NUMELXG=TABVINT(NEXT)
      NEXT = NEXT+1
      NITHGR=TABVINT(NEXT)
      NEXT = NEXT+1
      NPEBCR=TABVINT(NEXT)
      NEXT = NEXT+1
      NBI18=TABVINT(NEXT)
      NEXT = NEXT+1
      NSKYI18=TABVINT(NEXT)
C partie commun spmd cfd
      NEXT = NEXT+1
      NSVOIS=TABVINT(NEXT)
      NEXT = NEXT+1
      NQVOIS=TABVINT(NEXT)
      NEXT = NEXT+1
      NTGVOIS = TABVINT(NEXT)
      NEXT = NEXT + 1
      NRCVVOIS=TABVINT(NEXT)
      NEXT = NEXT+1
      NSNDVOIS=TABVINT(NEXT)
      NEXT = NEXT+1
      NERVOIS=TABVINT(NEXT)
      NEXT = NEXT+1
      NESVOIS=TABVINT(NEXT)
      NEXT = NEXT+1
      ILAW11=TABVINT(NEXT)
      NEXT = NEXT+1
      NSEGCOM=TABVINT(NEXT)
      NEXT = NEXT+1
      NUMPORL=TABVINT(NEXT)
      NEXT = NEXT+1
      ICAA=TABVINT(NEXT)
      NEXT = NEXT+1
      ISPHBUC=TABVINT(NEXT)
      NEXT = NEXT+1
      NRDAMP=TABVINT(NEXT)
      IF(NRDAMP==0) NRDAMP=4
      NEXT=NEXT+1
      NSPROC=TABVINT(NEXT)
      NEXT=NEXT+1
      NDSOLV=TABVINT(NEXT)
      NEXT=NEXT+1
      NSBMAX=TABVINT(NEXT)
      NEXT=NEXT+1
      NSVMAXT=TABVINT(NEXT)
      NEXT=NEXT+1
      NSMOD=TABVINT(NEXT)
      NEXT=NEXT+1
      NFLOW=TABVINT(NEXT)
      NEXT=NEXT+1
      LIFLOW=TABVINT(NEXT)
      NEXT=NEXT+1
      LRFLOW=TABVINT(NEXT)
      NEXT=NEXT+1
      NIFLOW=TABVINT(NEXT)
      NEXT=NEXT+1
      NIIOFLOW=TABVINT(NEXT)
      NEXT=NEXT+1
      NRFLOW=TABVINT(NEXT)
      NEXT=NEXT+1
      NRIOFLOW=TABVINT(NEXT)
      NEXT=NEXT+1
      ICONDP=TABVINT(NEXT)
      NEXT=NEXT+1
      IMPL_S0=TABVINT(NEXT)
      NEXT=NEXT+1
      SITHBUF=TABVINT(NEXT)
      NEXT=NEXT+1
      SITHBUFA=TABVINT(NEXT)
      NEXT=NEXT+1
      SITHBUFB=TABVINT(NEXT)
      NEXT=NEXT+1
      SITHBUFC=TABVINT(NEXT)
      NEXT=NEXT+1
      SITHBUFD=TABVINT(NEXT)
      NEXT=NEXT+1
      SITHBUFE=TABVINT(NEXT)
      NEXT=NEXT+1
      SITHBUFF=TABVINT(NEXT)
      NEXT=NEXT+1
      SITHBUFG=TABVINT(NEXT)
      NEXT=NEXT+1
      SITHBUFH=TABVINT(NEXT)
      NEXT=NEXT+1
      SITHBUFI=TABVINT(NEXT)
      NEXT=NEXT+1
      NFASOLFR=TABVINT(NEXT)
      NEXT=NEXT+1
      NSEGQUADFR=TABVINT(NEXT)
      NEXT=NEXT+1
      SFR_LAGF=TABVINT(NEXT)
      NEXT=NEXT+1
      SLLAGF=TABVINT(NEXT)
      NEXT=NEXT+1
      NVENTTOT=TABVINT(NEXT)
      NEXT   = NEXT + 1
      NUMELC0= TABVINT(NEXT)
      NEXT   = NEXT + 1
      NUMELTG0= TABVINT(NEXT)
      NEXT   = NEXT + 1
      NUMNOD0= TABVINT(NEXT)
      NEXT   = NEXT + 1
      NADMESH = TABVINT(NEXT)
      NEXT   = NEXT + 1
      LEVELMAX = TABVINT(NEXT)
      NEXT   = NEXT + 1
      IADMRULE = TABVINT(NEXT)
      NEXT   = NEXT + 1
      KSH4TREE= TABVINT(NEXT)
      NEXT   = NEXT + 1
      KSH3TREE= TABVINT(NEXT)
      NEXT   = NEXT + 1
      KPADMESH= TABVINT(NEXT)
      NEXT   = NEXT + 1
      KCONTACT= TABVINT(NEXT)
      NEXT   = NEXT + 1
      KIPADMESH= TABVINT(NEXT)
      NEXT   = NEXT + 1
      SICONTACT= TABVINT(NEXT)
      NEXT   = NEXT + 1
      SRCONTACT= TABVINT(NEXT)
      NEXT   = NEXT + 1
      SISKWP= TABVINT(NEXT)
      NEXT   = NEXT + 1
      SNSKWP= TABVINT(NEXT)
      NEXT   = NEXT + 1
      SISENSP= TABVINT(NEXT)
      NEXT   = NEXT + 1
      SNSENSP= TABVINT(NEXT)
      NEXT   = NEXT + 1
      SIACCP= TABVINT(NEXT)
      NEXT   = NEXT + 1
      SNACCP= TABVINT(NEXT)
      NEXT   = NEXT + 1
      IISROTS = TABVINT(NEXT)
C
C .sta files
      NEXT     = NEXT + 1
      MX_STAT0 = TABVINT(NEXT)
      NEXT   = NEXT + 1
      ISTATF = TABVINT(NEXT)
      IF(MCHECK/=0)THEN
        DO I=1, MX_STAT0
         NEXT   = NEXT + 1
         STAT_C(I) = TABVINT(NEXT)
        ENDDO
      ELSE
        NEXT   = NEXT + MX_STAT0
      END IF
C .dyain files
      NEXT   = NEXT + 1
      DYNAIN_DATA%IDYNAINF = TABVINT(NEXT)
      IF(MCHECK/=0)THEN
        DO I=1, DYNAIN_DATA%MX_DYNAIN
         NEXT   = NEXT + 1
         DYNAIN_DATA%DYNAIN_C(I) = TABVINT(NEXT)
        ENDDO
      ELSE
        NEXT   = NEXT + DYNAIN_DATA%MX_DYNAIN
      END IF
      NEXT   = NEXT + 1
      DYNAIN_DATA%DYNAIN_CHECK = TABVINT(NEXT)
C  heat transfer + int7
      NEXT=NEXT+1
      INTHEAT = TABVINT(NEXT)
C  xfem
      NEXT=NEXT+1
      NUMELCRK=TABVINT(NEXT)
      NEXT=NEXT+1
      NLEVSET=TABVINT(NEXT)
C---------------------------------------
C heat transfer
      NEXT=NEXT+1
      ITHERM_FE=TABVINT(NEXT)
C Static condensation
      NEXT=NEXT+1
      ISTATCND=TABVINT(NEXT)
C Defense Lic Flag
      NEXT   = NEXT + 1
      LIDFS  = TABVINT(NEXT)
      NEXT   = NEXT + 1
      LIRNUR = TABVINT(NEXT)
C Unit info
      NEXT   = NEXT + 1
      LUNIT  = TABVINT(NEXT)
      NEXT   = NEXT + 1
      NUNIT0 = TABVINT(NEXT)
      NEXT   = NEXT + 1
      NUNITS = TABVINT(NEXT)
C
      NEXT   = NEXT + 1
      LSH4TRIM  = TABVINT(NEXT)
      NEXT   = NEXT + 1
      LSH3TRIM = TABVINT(NEXT)
C
      NEXT   = NEXT + 1
      IADMERRT = TABVINT(NEXT)
C
      NEXT   = NEXT + 1
      IMPOSE_DR = TABVINT(NEXT)
C
      NEXT   = NEXT + 1
      NINTSTAMP = TABVINT(NEXT)
C
      NEXT   = NEXT + 1
      ITHKNOD = TABVINT(NEXT)
C
C  rigid material
C
      NEXT = NEXT + 1
      IRIGID_MAT = TABVINT(NEXT)
C
      NEXT = NEXT + 1
      NRBYM = TABVINT(NEXT)
C
      NEXT = NEXT + 1
      NGSLNRBYM = TABVINT(NEXT)
C
      NEXT = NEXT + 1
      NFRBYM = TABVINT(NEXT)
C
      NEXT = NEXT + 1
      NIRBYM = TABVINT(NEXT)
c
      NEXT = NEXT + 1
      NSKYRBMK0 = TABVINT(NEXT)
C-----------
      NEXT = NEXT + 1
      NIFT = TABVINT(NEXT)
      NEXT = NEXT + 1
      NICONV = TABVINT(NEXT)
      NEXT   = NEXT + 1
      LFACTHER = TABVINT(NEXT)
      NEXT   = NEXT + 1
      NUMCONV = TABVINT(NEXT)
      NEXT   = NEXT + 1
      NFXTEMP = TABVINT(NEXT)
C--------------------------------------
C stapming : modification of warning messages in engine
      NEXT = NEXT + 1
      ISTAMPING = TABVINT(NEXT)
C-----RBE3
      NEXT   = NEXT + 1
      NRBE3 = TABVINT(NEXT)
      NEXT   = NEXT + 1
      SIRBE3 = TABVINT(NEXT)
      NEXT   = NEXT + 1
      SLRBE3 = TABVINT(NEXT)
      NEXT   = NEXT + 1
      SFRBE3 = TABVINT(NEXT)
      NEXT   = NEXT + 1
      SIAD_RBE3M = TABVINT(NEXT)
      NEXT   = NEXT + 1
      SFR_RBE3M = TABVINT(NEXT)
C-------------------------------------------------------------------
C    spmd rigid material
      NEXT = NEXT + 1
      SFR_RBYM =TABVINT(NEXT)
      NEXT = NEXT + 1
      SFR_RBYM2 =TABVINT(NEXT)
C Tableaux Noise
      NEXT = NEXT + 1
      SINOISE = TABVINT(NEXT)
      NEXT = NEXT + 1
      SFNOISE = TABVINT(NEXT)
      NEXT = NEXT + 1
      NNOISE_SAV = TABVINT(NEXT)
C--------------------------------------
C  shell xfem
      NEXT   = NEXT + 1
      IPLYXFEM = TABVINT(NEXT)
      NEXT    = NEXT + 1
      NPLYMAX = TABVINT(NEXT)
      NEXT    = NEXT + 1
      IPLYBCS = TABVINT(NEXT)
      NEXT    = NEXT + 1
      NPLYXFE = TABVINT(NEXT)
      NEXT    = NEXT + 1
      EPLYXFE = TABVINT(NEXT)
C--------------------------------------
      NEXT    = NEXT + 1
      NGRPART = TABVINT(NEXT)
C--------------------------------------
C Madymo array
      NEXT    = NEXT + 1
      SMADIDX = TABVINT(NEXT)
C--------------------------------------
C Madcl nodes
      NEXT = NEXT + 1
      MADCLNODS = TABVINT(NEXT)
C
      NEXT = NEXT + 1
      IDTMINS_OLD = TABVINT(NEXT)
      NEXT = NEXT + 1
      IDTGRS_OLD = TABVINT(NEXT)
C--------------------------------------
      NEXT = NEXT + 1
      IREAC = TABVINT(NEXT)
C--------------------------------------
      NEXT = NEXT + 1
      SNOM_SECT = TABVINT(NEXT)
C--------------------------------------
      NEXT = NEXT + 1
      IGRELEM = TABVINT(NEXT)
C--------------------------------------
      NEXT = NEXT + 1
      NTHPART = TABVINT(NEXT)
C--------------------------------------
      NEXT = NEXT + 1
      IREST_MSELT = TABVINT(NEXT)
C--------------------------------------
      NEXT    = NEXT + 1
      NTHREAD0= TABVINT(NEXT)
C--------------------------------------
      NEXT    = NEXT + 1
      IDROT   = TABVINT(NEXT)
      NEXT = NEXT + 1
      NIRADIA = TABVINT(NEXT)
      NEXT   = NEXT + 1
      NUMRADIA = TABVINT(NEXT)
C--------------------------------------
      NEXT    = NEXT+1
      NITFLUX = TABVINT(NEXT) 
      NEXT    = NEXT+1
      NFXFLUX = TABVINT(NEXT)  
C--------------------------------------
      NEXT = NEXT + 1
      IF(MCHECK/=0)NSMSPCG = TABVINT(NEXT)
C--------------------------------------
      NEXT   = NEXT + 1
      NTABLE = TABVINT(NEXT)
C----Rbe2----
      NEXT = NEXT+1
      NBDDNRBE2 = TABVINT(NEXT)
      NEXT   = NEXT + 1
      NRBE2 = TABVINT(NEXT)
      NEXT   = NEXT + 1
      SIRBE2 = TABVINT(NEXT)
      NEXT   = NEXT + 1

      SLRBE2 = TABVINT(NEXT)
      NEXT   = NEXT + 1
      SIAD_RBE2 = TABVINT(NEXT)
      NEXT   = NEXT + 1
      SFR_RBE2 = TABVINT(NEXT)
      NEXT   = NEXT + 1
      NHRBE2 = TABVINT(NEXT)
C
C --- simplified ale option + lagrange
C
      NEXT   = NEXT + 1
      IALELAG = TABVINT(NEXT)
C
      NEXT   = NEXT + 1
      NRBE2G=TABVINT(NEXT)
C
      NEXT   = NEXT + 1
      NRBE3G=TABVINT(NEXT)
C
C--------------------------------------
C ... shell 4-N  +  shell 3-N (X-FEM)
      NEXT=NEXT+1
      ICRACK3D=TABVINT(NEXT)
C ply xfem (parit on)
      NEXT=NEXT+1
      LCNEPXFEM =TABVINT(NEXT)
      NEXT   = NEXT + 1
      SITHVAR = TABVINT(NEXT)
      NEXT=NEXT+1
      NPLYNODG = TABVINT(NEXT)
C --- Need NXREF Option for Modif files / SPMD
      NEXT  = NEXT + 1
      NXREF = TABVINT(NEXT)
C
      NEXT = NEXT + 1
      IDTMINS_INT_OLD = TABVINT(NEXT)
C
      NEXT = NEXT +1
      SLCFIELD = TABVINT(NEXT)
      NEXT = NEXT + 1
      NLOAD = TABVINT(NEXT)
      NEXT = NEXT + 1
      IDAMP_RDOF = TABVINT(NEXT)
      NEXT = NEXT + 1
      LSKEW = TABVINT(NEXT)
      NEXT = NEXT + 1
      LISKN = TABVINT(NEXT)
      NEXT = NEXT + 1
      LFXVELR = TABVINT(NEXT)
C
      NEXT = NEXT + 1
      NLOADC = TABVINT(NEXT)
      NEXT = NEXT + 1
      SLLOADP = TABVINT(NEXT)
      NEXT = NEXT + 1
      NLOADP = TABVINT(NEXT)
      NEXT = NEXT + 1
      NLOADP_HYD = TABVINT(NEXT)
      NEXT = NEXT + 1
      NINTLOADP = TABVINT(NEXT)
      NEXT = NEXT + 1
      NINTLOADP21 = TABVINT(NEXT)
C xfem for layered shell (cracks)
      NEXT=NEXT+1
      LCNECRKXFEM=TABVINT(NEXT)
      NEXT     = NEXT + 1
      NCRKXFE  = TABVINT(NEXT)
      NEXT     = NEXT + 1
      ECRKXFE  = TABVINT(NEXT)
      NEXT     = NEXT + 1
      NLEVMAX  = TABVINT(NEXT)
      NEXT     = NEXT + 1
      NCRKNODG = TABVINT(NEXT)
      NEXT     = NEXT + 1
      IENRNOD  = TABVINT(NEXT)
      NEXT     = NEXT + 1
      SIBUFSSG_IO = TABVINT(NEXT)
      NEXT = NEXT + 1
      NIBSPH   = TABVINT(NEXT)
      NEXT = NEXT + 1
      SLINALE  = TABVINT(NEXT)      
      NEXT = NEXT + 1
      NSUBMOD  = TABVINT(NEXT)
      NEXT = NEXT + 1
      NALENOVS = TABVINT(NEXT)
C
      NEXT     = NEXT + 1
      LNOPT1 = TABVINT(NEXT)
      NEXT = NEXT + 1
      LILSET1   = TABVINT(NEXT)
      NEXT = NEXT + 1
      LISLIN1  = TABVINT(NEXT)
      NEXT = NEXT + 1
      LTITR = TABVINT(NEXT)
      NEXT = NEXT + 1
      LISURF1 = TABVINT(NEXT)
      NEXT = NEXT + 1
      LIPART1 = TABVINT(NEXT)
      NEXT   = NEXT + 1
      SIZFIELD = TABVINT(NEXT)
      NEXT   = NEXT + 1
      SIZLOADP = TABVINT(NEXT)
      NEXT   = NEXT + 1
      I2NSN25 = TABVINT(NEXT)
      NEXT = NEXT + 1
      LVOISPH = TABVINT(NEXT)
      NEXT = NEXT + 1
      IF(MCHECK /=  0 ) ANIM_PLY = TABVINT(NEXT)
      NEXT = NEXT + 1
      SIBORDNODE = TABVINT(NEXT)
      NEXT = NEXT + 1
      SIEDGESH = TABVINT(NEXT)
C multilayer xfem shell
      NEXT = NEXT + 1
      ECRKXFEC = TABVINT(NEXT)
      NEXT = NEXT + 1
      ECRKXFETG = TABVINT(NEXT)
      NEXT = NEXT + 1
      NUMEDGES = TABVINT(NEXT)
c inter22
      NEXT     = NEXT + 1
      INT22    = TABVINT(NEXT)
      NEXT     = NEXT + 1
      I22_ALEUL= TABVINT(NEXT)
      NEXT     = NEXT + 1
      NSUB22   = TABVINT(NEXT)
      NEXT     = NEXT + 1
      IOUTP22  = TABVINT(NEXT)
      NEXT     = NEXT + 1
      I22LAW37 = TABVINT(NEXT)      
      NEXT     = NEXT + 1
      I22LAW51 = TABVINT(NEXT)
c
      NEXT = NEXT + 1
      ICRASH = TABVINT(NEXT)
C
      NEXT      = NEXT + 1
      SIAD_EDGE = TABVINT(NEXT)
      NEXT      = NEXT + 1
      SFR_EDGE  = TABVINT(NEXT)
      NEXT      = NEXT + 1
      SFR_NBEDGE= TABVINT(NEXT)
C
      NEXT    = NEXT + 1
      NBGAUGE = TABVINT(NEXT)
      NEXT    = NEXT + 1
      LLACCELM = TABVINT(NEXT)
      NEXT    = NEXT + 1
      LLGAUGE = TABVINT(NEXT)
c
      NEXT   = NEXT + 1
      SIGAUP= TABVINT(NEXT)
      NEXT   = NEXT + 1
      SNGAUP= TABVINT(NEXT)
C      
      NEXT=NEXT+1
      SDD_R2R_ELEM=TABVINT(NEXT)
      NEXT=NEXT+1
      R2R_SIU=TABVINT(NEXT)
C heat trasfert for interface Type21 : main temperature is variable
      NEXT   = NEXT + 1
      FTEMPVAR21 = TABVINT(NEXT)
C
      NEXT   = NEXT + 1
      NUMNODXFE=TABVINT(NEXT)
      NEXT   = NEXT + 1
      SNODGLOBXFE=TABVINT(NEXT)
c
      NEXT   = NEXT + 1
      NSPHSOL= TABVINT(NEXT)
      NEXT   = NEXT + 1
      FIRST_SPHSOL= TABVINT(NEXT)
C
      NEXT   = NEXT + 1
      IDDOM=TABVINT(NEXT)
      NEXT   = NEXT + 1
      NUMMAT0=TABVINT(NEXT)
      NEXT   = NEXT + 1
      NUMGEO0=TABVINT(NEXT)                  
C 
      NEXT       = NEXT + 1
      INTPLYXFEM = TABVINT(NEXT)
C      
      NEXT       = NEXT + 1
      I22LEN     = TABVINT(NEXT)      
C 
      NEXT       = NEXT + 1
      NCLUSTER   = TABVINT(NEXT)      
C
      NEXT       = NEXT + 1
      NXLAYMAX   = TABVINT(NEXT)
C
      NEXT       = NEXT+1
      SRTHBUF    = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      NUMELIG3D  = TABVINT(NEXT)
      NEXT       = NEXT + 1
      SKXIG3D    = TABVINT(NEXT)
      NEXT       = NEXT + 1
      SIXIG3D    = TABVINT(NEXT)
      NEXT       = NEXT + 1
      SKNOT      = TABVINT(NEXT)
      NEXT       = NEXT + 1
      SKNOTLOCPC = TABVINT(NEXT)
      NEXT       = NEXT + 1
      SKNOTLOCEL = TABVINT(NEXT)
      NEXT       = NEXT + 1
      NUMFAKENODIGEO = TABVINT(NEXT)
      NEXT       = NEXT + 1
      DEG_MAX    = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      NXEL       = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      NFILSOL    = TABVINT(NEXT)
C
      NEXT=NEXT+1
      SFSAV6     = TABVINT(NEXT)
C
      NEXT=NEXT+1
      STABSEN     = TABVINT(NEXT)
C Stack info
      NEXT    = NEXT + 1
      IPART_STACK = TABVINT(NEXT)
      NEXT    = NEXT + 1
      NPT_STACK  = TABVINT(NEXT)    
      NEXT     = NEXT + 1
      NS_STACK  = TABVINT(NEXT)  
C
      NEXT           = NEXT + 1
      IALEFVM        = TABVINT(NEXT)           
      NEXT           = NEXT + 1
      I_ALE_SOLVER   = TABVINT(NEXT)  
C MUSCL keyword, compression parameter beta
      NEXT           = NEXT + 1
      IALEMUSCL      = TABVINT(NEXT)
      IALEMUSCL      = IALEMUSCL * (1-I_MUSCL_OFF) !/ALE/MUSCL/OFF : freform.F
      NEXT           = NEXT + 1
      I_BETA         = TABVINT(NEXT)
      NEXT           = NEXT + 1
      I_LAW         = TABVINT(NEXT)
C ITETRA=3
      NEXT=NEXT+1
      ISFEM =TABVINT(NEXT)
C /LOAD/PBLAST
      NEXT       = NEXT + 1
      NLOADP_F   = TABVINT(NEXT)
      NEXT       = NEXT + 1
      NLOADP_B   = TABVINT(NEXT)      
C
      NEXT     = NEXT + 1
      IF(MCHECK/=0) IDTMIN(51)= TABVINT(NEXT)
C     /DT/FVMBAG
      NEXT     = NEXT + 1
      IF(MCHECK/=0) IDTMIN(52)= TABVINT(NEXT)
C
      NEXT     = NEXT + 1
      NBR_GPMP  = TABVINT(NEXT)
C
      NEXT     = NEXT + 1
      TAILLE  = TABVINT(NEXT) 
C
      NEXT       = NEXT + 1
      NINTER25   = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      NUMNOR     = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      NBDDNORT   = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      NBDDNOR_MAX = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      NBCCFR25   = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      NBCCNOR    = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      NBDDEDGT   = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      NBDDEDG_MAX = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      NSNT25     = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      NRTMX25    = TABVINT(NEXT)
C--------------------------------------
C drape option
      NEXT       = NEXT + 1
      NDRAPE     = TABVINT(NEXT)
C Element activation
      NEXT       = NEXT + 1
      LRACTIV    = TABVINT(NEXT)
C ID_PLY -> animation file
      NEXT       = NEXT + 1
      NUMPLY     = TABVINT(NEXT)
C ID_PLY -> animation file
      NEXT       = NEXT + 1
      NUMSTACK     = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      TABMP_L     = TABVINT(NEXT)
C SOL2SPH flag
      NEXT       = NEXT + 1
      SOL2SPH_FLAG   = TABVINT(NEXT)
      NEXT       = NEXT + 1
      ITSOL2SPH  = TABVINT(NEXT)
C ISFINT flag
      NEXT       = NEXT + 1
      IF(ISFINT==0)THEN
        !no engine keyword (/ALE/SOLVER) affecting ISFINT, then use starter setting
        ISFINT   = TABVINT(NEXT)      
      ENDIF  
C ICNDS10 dyna condensation for S10
      NEXT       = NEXT + 1
      NS10E      = TABVINT(NEXT)      
      NEXT       = NEXT + 1
      LCNCND     = TABVINT(NEXT)      
      NEXT       = NEXT + 1
      SIAD_CNDM  = TABVINT(NEXT)      
      NEXT       = NEXT + 1
      SFR_CNDM   = TABVINT(NEXT)      
C---- old local sys of shell3n      
      NEXT = NEXT+1
      ISH3NFRAM =TABVINT(NEXT)
      NEXT = NEXT+1
      STAB_MAT = TABVINT(NEXT)
C SMS auto switch
      NEXT       = NEXT + 1
      ISMS_SELEC = TABVINT(NEXT)  
      NEXT = NEXT + 1
      ISUSED = TABVINT(NEXT)
      MULTI_FVM%IS_USED = .FALSE.
      IF (ISUSED == 1) THEN
         MULTI_FVM%IS_USED = .TRUE.
         MULTI_FVM%BETA = I_BETA
      ENDIF
      NEXT = NEXT + 1
      ISUSED = TABVINT(NEXT)
      MULTI_FVM%NBMAT = ISUSED
C---- old Isolid17      
      NEXT = NEXT+1
      IS17_OLD =TABVINT(NEXT)
C IH3D -> H3D count       
      NEXT       = NEXT + 1
      H3D_DATA%IH3D = TABVINT(NEXT)   
C        
      NEXT       = NEXT + 1
      NUMELS10G  = TABVINT(NEXT)   
C        
      NEXT       = NEXT + 1                      
      NUMELS20G  = TABVINT(NEXT)   
!
      NEXT             = NEXT + 1                      
      I_DT_NODA_ALE_ON = TABVINT(NEXT) 
c interface adhesion
      NEXT    = NEXT + 1
      INTERADHESION = TABVINT(NEXT)
C Friction interface  
      NEXT             = NEXT + 1
      NINTERFRIC   = TABVINT(NEXT)
!
      NEXT    = NEXT + 1                      
      LENSUBS = TABVINT(NEXT)
!
      NEXT    = NEXT + 1                      
      LENIGRNOD = TABVINT(NEXT)
!
      NEXT    = NEXT + 1                      
      LENIGRBRIC = TABVINT(NEXT)
!
      NEXT    = NEXT + 1                      
      LENIGRQUAD = TABVINT(NEXT)
!
      NEXT    = NEXT + 1                      
      LENIGRSH4N = TABVINT(NEXT)
!
      NEXT    = NEXT + 1                      
      LENIGRSH3N = TABVINT(NEXT)
!
      NEXT    = NEXT + 1                      
      LENIGRTRUS = TABVINT(NEXT)
!
      NEXT    = NEXT + 1                      
      LENIGRBEAM = TABVINT(NEXT)
!
      NEXT    = NEXT + 1                      
      LENIGRSPRI = TABVINT(NEXT)
!
      NEXT    = NEXT + 1                      
      LENIGRPART = TABVINT(NEXT)
!
      NEXT    = NEXT + 1                      
      LENISURF = TABVINT(NEXT)
!
      NEXT    = NEXT + 1                      
      LENISLIN = TABVINT(NEXT)
!
      IF(MCHECK/=0)THEN
        NEXT    = NEXT + 1 
        IDTS6   =  TABVINT(NEXT)                    
        NEXT    = NEXT + 1                      
        IDTHEPH =  TABVINT(NEXT)                    
        NEXT    = NEXT + 1                      
        IDT1SOL =  TABVINT(NEXT)                    
        NEXT    = NEXT + 1                      
        IDT1TET10=  TABVINT(NEXT)  
        NEXT    = NEXT + 1                      
        IDT1SH  =  TABVINT(NEXT)  
         
      ELSE
        NEXT    = NEXT + 5                 
      END IF
!
!     H3D -> number of inter skid lines    
      NEXT       = NEXT + 1
      NINTSKIDOLD= TABVINT(NEXT) 
!     H3D ANIM-> Is maximum contact force /contact pressure computed    
      NEXT       = NEXT + 1
      IFCONTMAX  = TABVINT(NEXT) 
      NEXT       = NEXT + 1
      IFCONTPMAX = TABVINT(NEXT) 
      NEXT       = NEXT + 1
      IFCONT2MAX = TABVINT(NEXT) 
      NEXT       = NEXT + 1
      IFCONTP2MAX= TABVINT(NEXT) 
      NEXT       = NEXT + 1
      IFCONT2MIN = TABVINT(NEXT) 
      NEXT       = NEXT + 1
      IFCONTP2MIN= TABVINT(NEXT) 
      NEXT       = NEXT + 1
      S_EFRIC    = TABVINT(NEXT) 
      NEXT       = NEXT + 1
      S_EFRICINT = TABVINT(NEXT) 
      NEXT       = NEXT + 1
      S_EFRICG   = TABVINT(NEXT) 
      NEXT       = NEXT + 1
      S_EFRICINTG = TABVINT(NEXT) 
      NEXT       = NEXT + 1
      NINEFRIC   = TABVINT(NEXT)
      NEXT       = NEXT + 1
      NINEFRIC_STAMP = TABVINT(NEXT)  
C  NITSCHE METHOD
      NEXT=NEXT+1
      NITSCHE = TABVINT(NEXT)
C  Pinching
      NEXT = NEXT+1
      NPINCH = TABVINT(NEXT)
      NEXT = NEXT+1
      NPLOADPINCH = TABVINT(NEXT)

!
      NEXT=NEXT+1
      IF(I_SUPG_ON_OFF==0) I_SUPG_ON_OFF = TABVINT(NEXT)

      NEXT       = NEXT + 1
      NLEDGE     = TABVINT(NEXT) 
!
      NEXT       = NEXT + 1
      INISPRI    = TABVINT(NEXT)
!
      NEXT       = NEXT + 1
      NSTOPSENS_OLD = TABVINT(NEXT)
      NEXT       = NEXT + 1
      NSTATSENS_OLD = TABVINT(NEXT)
      NEXT       = NEXT + 1
      NOUTPSENS_OLD = TABVINT(NEXT)
      NEXT       = NEXT + 1
      IANISENS_OLD = TABVINT(NEXT)
!
      NEXT       = NEXT + 1
      NIBCLD = TABVINT(NEXT)
      NEXT       = NEXT + 1
      LFACCLD = TABVINT(NEXT)
!
      NEXT       = NEXT + 1
      NIGRV      = TABVINT(NEXT)
      NEXT       = NEXT + 1
      LFACGRV    = TABVINT(NEXT)
!
C  BCS/CYCLIC 
      NEXT       = NEXT + 1
      NBCSCYC    = TABVINT(NEXT)
      NEXT       = NEXT + 1
      SIBCSCYC   = TABVINT(NEXT)
      NEXT       = NEXT + 1
      SLBCSCYC   = TABVINT(NEXT)
      
      NEXT       = NEXT + 1
      INTER18_AUTOPARAM = TABVINT(NEXT)
      NEXT       = NEXT + 1
      IF(TABVINT(NEXT) == 1)INTER18_IS_VARIABLE_GAP_DEFINED = .TRUE.

      IF ((IALE > 0 .AND. NWALE_RST == 6) .OR. INTER18_AUTOPARAM == 1 .OR. INTER18_IS_VARIABLE_GAP_DEFINED) THEN
         NEXT = NEXT + 1
         NE_SLERCVOIS = TABVINT(NEXT)
         NEXT = NEXT + 1
         NE_SLESDVOIS = TABVINT(NEXT)
         NEXT = NEXT + 1
         NE_NSVOIS = TABVINT(NEXT)
         NEXT = NEXT + 1
         NE_NQVOIS = TABVINT(NEXT)
         NEXT = NEXT + 1
         NE_NTGVOIS = TABVINT(NEXT)
      ENDIF

      NEXT       = NEXT + 1
      PDEL = TABVINT(NEXT)

C-----Seatbelts - slipring / retractors
      NEXT       = NEXT + 1
      NSLIPRING  = TABVINT(NEXT)
      NEXT       = NEXT + 1
      NRETRACTOR = TABVINT(NEXT)
      NEXT       = NEXT + 1
      NSLIPRING_G  = TABVINT(NEXT)
      NEXT       = NEXT + 1
      NRETRACTOR_G = TABVINT(NEXT)
      NEXT       = NEXT + 1
      N_ANCHOR_REMOTE  = TABVINT(NEXT)
      NEXT       = NEXT + 1
      N_ANCHOR_REMOTE_SEND  = TABVINT(NEXT)
      NEXT       = NEXT + 1
      N_SEATBELT  = TABVINT(NEXT)
      NEXT       = NEXT + 1
      N_SEATBELT_2D  = TABVINT(NEXT)
C
      NEXT       = NEXT + 1
      LASTANIMCYCLE  = TABVINT(NEXT)

      NEXT       = NEXT + 1
      LASTH3DCYCLE  = TABVINT(NEXT)

      NEXT       = NEXT + 1
      LASTDYNCYCLE  = TABVINT(NEXT)

      NEXT       = NEXT + 1
      LASTSTATCYCLE  = TABVINT(NEXT)
C
C---- Starter input version
      NEXT       = NEXT + 1
      ST_INVERS  = TABVINT(NEXT)
C
C---- Number of RBODY before merge
      NEXT       = NEXT + 1
      NRBODY0    = TABVINT(NEXT)
C---- MAX (/H3D) 
      NEXT       = NEXT + 1
      LMAX_DIS   = TABVINT(NEXT)
      NEXT       = NEXT + 1
      LMAX_VEL   = TABVINT(NEXT)
      NEXT       = NEXT + 1
      LMAX_NSIG  = TABVINT(NEXT)
      NEXT       = NEXT + 1
      LMAX_NSTRA = TABVINT(NEXT)
C---- Time history strain flag
      NEXT       = NEXT + 1
      TH_STRAIN  = TABVINT(NEXT)
C  draped shell element
      NEXT       = NEXT + 1
      NUMELC_DRAPE= TABVINT(NEXT)
      NEXT       = NEXT + 1
      NUMELTG_DRAPE= TABVINT(NEXT)
C--------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  RDCOMR                        source/output/restart/rdcomm.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|====================================================================
      SUBROUTINE RDCOMR(LMXVREA, LVARREA, TABVREA,DYNAIN_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
       USE ANIM_MOD      
       USE OUTPUTS_MOD        
       USE STATE_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER LMXVREA,LVARREA
      my_real TABVREA(LMXVREA)
      TYPE (DYNAIN_DATABASE), INTENT(INOUT) :: DYNAIN_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com07_c.inc"
#include      "com08_c.inc"
#include      "cong2_c.inc"
#include      "couple_c.inc"
#include      "lagmult.inc"
#include      "remesh_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr18_c.inc"
#include      "scr19_c.inc"
#include      "scrnoi_c.inc"
#include      "scrupw_c.inc"
#include      "sms_c.inc"
#include      "sphcom.inc"
#include      "sysunit.inc"
#include      "statr_c.inc"
#include      "inter22.inc"
#include      "mmale51_c.inc"
#include      "thermal_c.inc"
#include      "itet2_c.inc"
C-----------------------------------------------
#include      "ddspmd_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,RVARINT,J
C-----------------------------------------------
C      warning to radioss developers:
C      IF(LVARREA>LMXVREA)THEN
C        WRITE(*,*) ' ** RADIOSS INTERNAL ERROR'
C        CALL ARRET(7)
C      ENDIF
C-----
      TT     =TABVREA(1)
      DT1    =TABVREA(2)
      DT2    =TABVREA(3)
      DT12   =TABVREA(4)
      DT2OLD =TABVREA(5)
      TSTOP  =TABVREA(6)
      !TABVREA(7) no longer used
      THIS   =TABVREA(8)
C-----
      !TABVREA(9) no longer used
      DTHIS  =TABVREA(10)
      DTFAC  =TABVREA(11)
      DTMIN  =TABVREA(12)
      VOLMIN =TABVREA(13)
      TFEXT  =TABVREA(14)
      REINT  =TABVREA(15)
      !TABVREA(16) no longer used
      !TABVREA(17) no longer used
      DTANIM =TABVREA(18)
      TANIM  =TABVREA(19)
      UREINT =TABVREA(20)
      ECONTV =TABVREA(21)
      EHOUR  =TABVREA(22)
      DTOUTP =TABVREA(23)
      TOUTP  =TABVREA(24)
      T1S    =TABVREA(25)
      DT2S   =TABVREA(26)
      EPOR   =TABVREA(27)
      ! unused USREINT=TABVREA(28)
      R2RFX1 =TABVREA(29)
      R2RFX2 =TABVREA(30)
      DAMPA  =TABVREA(31)
      DAMPB  =TABVREA(32)
      DAMPW  =TABVREA(33)
C-----
      ALPHA  =TABVREA(34)
      GAMMA  =TABVREA(35)
      VGX    =TABVREA(36)
      VGY    =TABVREA(37)
      VGZ    =TABVREA(38)
C-----
       SPASORT=TABVREA(39)
       SPATRUE=TABVREA(40)
       DO I=1,9
        DTHIS1(I) = TABVREA(40+I)
        THIS1(I)  = TABVREA(49+I)
       ENDDO
       MASS0 = TABVREA(59)
       ENTOT0 = TABVREA(60)
       LAGM_TOL =TABVREA(61)
       LAG_ALPH =TABVREA(62)
       LAG_ALPHS=TABVREA(63)

C-----
C     read/write next real variable at TABVREA(LVR44A+1) if LVARREA > LVR44A,
C                                      else, set its value to zero.
C           (whatever the real common to store the variable).
C--------------------------------------
       IF(MCHECK/=0)THEN
         DO I=1,12
          DTFAC1(I) = TABVREA(63 + I)
          DTMIN1(I) = TABVREA(63 + 12 +I)
         ENDDO
         BETA   = TABVREA(88)
         BETATE = TABVREA(89)
         PERIOD = TABVREA(90)
         DMTMXS = TABVREA(91)
         DMNMXS = TABVREA(92)
         DMNMXS = TABVREA(93)
         DTIN   = TABVREA(94)
         DTMX   = TABVREA(95)
         MADUL  = TABVREA(96)
         MADUT  = TABVREA(97)
         MADUM  = TABVREA(98)
         DTFSUB = TABVREA(99)
         DTMSUB = TABVREA(100)
         EPS    = TABVREA(101)
         EPS2   = TABVREA(102)
         CUPWM  = TABVREA(103)
c noise
         TNOISE = TABVREA(104)
         DTNOISE= TABVREA(105)
       ENDIF


      DWMAD = TABVREA(106)
      FAC_MASS   = TABVREA(107)
      FAC_LENGTH = TABVREA(108)
      FAC_TIME   = TABVREA(109)
      T1SH   = TABVREA(110)
      UPWSM = TABVREA(111)

      RVARINT = 111

      RVARINT = RVARINT + 1
      TPSREF = TABVREA(RVARINT)
C THERMAL ANALYSIS
      RVARINT = RVARINT + 1
      HEAT_STORED = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      HEAT_MECA   = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      HEAT_CONV   = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      HEAT_RADIA  = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      HEAT_FFLUX  = TABVREA(RVARINT)
C   TABLEAU SOL1TNL
      DO I=0, MAXLAW
        RVARINT = RVARINT + 1
        SOL1TNL(I,1) = TABVREA(RVARINT)
        RVARINT = RVARINT + 1
        SOL1TNL(I,2) = TABVREA(RVARINT)
      ENDDO
C   TABLEAU SOL8TNL
      DO I=0, MAXLAW
        RVARINT = RVARINT + 1
        SOL8TNL(I,1) = TABVREA(RVARINT)
        RVARINT = RVARINT + 1
        SOL8TNL(I,2) = TABVREA(RVARINT)
      ENDDO

C   TABLEAU TET4TNL
      DO I=0, MAXLAW
        RVARINT = RVARINT + 1
        TET4TNL(I,1) = TABVREA(RVARINT)
      ENDDO
C   TABLEAU TET10TNL
      DO I=0, MAXLAW
        RVARINT = RVARINT + 1
        TET10TNL(I,1) = TABVREA(RVARINT)
      ENDDO

C   TABLEAU SHTNL
      DO I=0, MAXLAW
        DO J=0,3
          RVARINT = RVARINT + 1
          SHTNL(I,J,1) = TABVREA(RVARINT)
          RVARINT = RVARINT + 1
          SHTNL(I,J,2) = TABVREA(RVARINT)
          RVARINT = RVARINT + 1
          SHTNL(I,J,3) = TABVREA(RVARINT)
        ENDDO
      ENDDO
C   TABLEAU TRITNL
      DO I=0, MAXLAW
        DO J=0,3
          RVARINT = RVARINT + 1
          TRITNL(I,J,1) = TABVREA(RVARINT)
          RVARINT = RVARINT + 1
          TRITNL(I,J,2) = TABVREA(RVARINT)
          RVARINT = RVARINT + 1
          TRITNL(I,J,3) = TABVREA(RVARINT)
        ENDDO
      ENDDO
C   TABLEAU SOLTELT
      DO I=1,8
        RVARINT = RVARINT + 1
        SOLTELT(I) = TABVREA(RVARINT)
      ENDDO

      RVARINT = RVARINT + 1
      TET4TELT(1) = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      TET10TELT(1) = TABVREA(RVARINT)

C   TABLEAU SHTELT
      DO I=1,10
        RVARINT = RVARINT + 1
        SHTELT(I) = TABVREA(RVARINT)
      ENDDO
C   TABLEAU TRITELT
      DO I=1,10
        RVARINT = RVARINT + 1
        TRITELT(I) = TABVREA(RVARINT)
      ENDDO
C
C .sta files
      RVARINT = RVARINT + 1
      DTSTAT  = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      TSTAT   = TABVREA(RVARINT)
C
C .dynain files
      RVARINT = RVARINT + 1
      DYNAIN_DATA%DTDYNAIN  = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      DYNAIN_DATA%TDYNAIN   = TABVREA(RVARINT)
C
      RVARINT = RVARINT + 1
      TADMESH = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      DTADMESH= TABVREA(RVARINT)
C
      RVARINT = RVARINT + 1
      DTFACS_OLD = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      DTMINS_OLD = TABVREA(RVARINT)
C
      RVARINT = RVARINT + 1
      IF(MCHECK/=0) TOL_SMS = TABVREA(RVARINT)
C abf files
      DO I=1,10
        RVARINT = RVARINT + 1
        TABFIS(I) = TABVREA(RVARINT)
        RVARINT = RVARINT + 1
        DTABF(I) = TABVREA(RVARINT)
        RVARINT = RVARINT + 1
        DTABFWR(I) = TABVREA(RVARINT)
        RVARINT = RVARINT + 1
        TABFWR(I) = TABVREA(RVARINT)
      ENDDO
C
      RVARINT       = RVARINT + 1
      RATIO22       = TABVREA(RVARINT)
C
      RVARINT       = RVARINT + 1
      M51_IFLG6     = TABVREA(RVARINT)      
C
      RVARINT       = RVARINT + 1
      MS_ND         = TABVREA(RVARINT)      
      RVARINT       = RVARINT + 1
      DMSI2         = TABVREA(RVARINT)      
      RVARINT       = RVARINT + 1
      MSI20         = TABVREA(RVARINT)      
C      
      RVARINT = RVARINT + 1
      IF(MCHECK/=0) DTFAC1(51)= TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      IF(MCHECK/=0) DTMIN1(51)= TABVREA(RVARINT)
C     /DT/FVMBAG
      RVARINT = RVARINT + 1
      IF(MCHECK/=0) DTFAC1(52)= TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      IF(MCHECK/=0) DTMIN1(52)= TABVREA(RVARINT)
C
      RVARINT = RVARINT + 1
      IF(MCHECK/=0) DTFAC1(102)= TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      IF(MCHECK/=0) DTMIN1(102)= TABVREA(RVARINT)
C      
      RVARINT = RVARINT + 1
      IF(MCHECK/=0) DTFACS_INT_OLD= TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      IF(MCHECK/=0) DTMINS_INT_OLD= TABVREA(RVARINT)
C      
C    /ADYREL :BETATE_0, EINT_0,ENCIN_0,PIMAX ,PCMAX,PINT ,PCIN ,FIL_
      RVARINT = RVARINT + 1
      BETATE_0 = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      EINT_0 = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      ENCIN_0 = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      PIMAX = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      PCMAX = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      PINT = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      PCIN = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      FIL_IE(1:4)=TABVREA(RVARINT:RVARINT+3)
      RVARINT = RVARINT + 4
      FIL_KE(1:4)=TABVREA(RVARINT:RVARINT+3)
      RVARINT = RVARINT + 4
      NFIRST  = TABVREA(RVARINT)
C     
      RVARINT = RVARINT + 1
      IF(MCHECK/=0) VGY0= TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      IF(MCHECK/=0) VGZ0= TABVREA(RVARINT)
C
      RVARINT = RVARINT + 1
      ECONTD= TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      ECONT_CUMU= TABVREA(RVARINT)

C  INITIAL MASS IN STARTER
      RVARINT = RVARINT + 1
      MASS0_START = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      PERCENT_ADDMASS_OLD = TABVREA(RVARINT)
C  ANIM FILES FINAL TIME
      RVARINT = RVARINT + 1
      TANIM_STOP = TABVREA(RVARINT)     
C  INITIAL COORDS OF SPH RESERVE 
      RVARINT = RVARINT + 1
      XI_RES = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      YI_RES = TABVREA(RVARINT)
      RVARINT = RVARINT + 1
      ZI_RES = TABVREA(RVARINT)
           
      RETURN
      END
