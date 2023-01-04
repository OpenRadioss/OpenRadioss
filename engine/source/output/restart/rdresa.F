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
Chd|  RDRESA                        source/output/restart/rdresa.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CUR_FIL_C                     source/output/tools/sortie_c.c
Chd|        RDCOMI                        source/output/restart/rdcomm.F
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE RDRESA(ITSTV1 , LHEADER, LVARINT, LVARREA, PRADIOS,
     .                  PRESFIL,MULTI_FVM,H3D_DATA,
     .                  PINCH_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE PINCHTYPE_MOD
      USE H3D_MOD
C-----------------------------------------------
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
#include      "r4r8_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com05_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr13_c.inc"
#include      "scr14_c.inc"
#include      "scr20_c.inc"
#include      "fluid_scr_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "couple_c.inc"
#include      "check.inc"
#include      "task_c.inc"
#include      "tabsiz_c.inc"
#include      "scrupw_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITSTV1, LHEADER, LVARINT, LVARREA, PRADIOS,
     .        PRESFIL
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(PINCH) :: PINCH_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   I
      INTEGER   LMXHEAD,LMXVINT
      PARAMETER (LMXHEAD=100)
      PARAMETER (LMXVINT=4000)     
      INTEGER   TABHEAD(LMXHEAD),TABVINT(LMXVINT)
      my_real
     .        PROBINT
C-----------------------------------------------
      DO I=1,LMXVINT
        TABVINT(I)=0
      ENDDO
C-----------------------------------------------
C     LECTURE TEXTE
C-----------------------------------------------
      CALL CUR_FIL_C(0)
      CALL READ_I_C(TEXT,LCOM05)
C-----------------------------------------------
C     LECTURE DE L'ENTETE
C-----------------------------------------------
        CALL READ_I_C(LHEADER,1)
C       warning to radioss developers:
C       IF(LHEADER>LMXHEAD)THEN
C         WRITE(*,*) ' ** RADIOSS INTERNAL ERROR'
C         CALL ARRET(7)
C       ENDIF
        CALL READ_I_C(TABHEAD,LHEADER)
C       RADIOSS FILE R/W
      PRADIOS=TABHEAD(1)
      IF(PRADIOS/=IRADIOS)RETURN
C       RESTART FILE
      PRESFIL=TABHEAD(2)
      IF(PRESFIL/=IRESFIL)RETURN
C       ITESTV
      ITESTV =TABHEAD(3)
      ITESTV = IABS(ITESTV)
C itestv +/- suivant ir4r8 courant
      IF (IR4R8==2) ITESTV = -ITESTV
      IF(ITESTV/=ITSTV1.AND.DEBUG(2)<2)RETURN
C-----
C     PREVIOUS RUN NUMBER
      PIRUN  =TABHEAD(4)
C     CODE OF PREVIOUS RUN (-1:UNKNOWN,0:STARTER,1:ENGINE,2:MODIF)
      PCODRUN=TABHEAD(5)
C     CODVERS FOR PREVIOUS RUN
      PCODVER=TABHEAD(6)
C     MINOR VERSION CODE OF PREVIOUS RUN
      PMINVER=TABHEAD(7)
C     SOURCE VERSION CODE OF PREVIOUS RUN
      PSRCVER=TABHEAD(8)
C     INPUT VERSION OF PREVIOUS RUN
      PINVERS=TABHEAD(9)
C     STARTER CODVERS
      SCODVER=TABHEAD(10)
C     STARTER MINOR VERSION CODE.
      SMINVER=TABHEAD(11)
C     STARTER SOURCE VERSION CODE.
      SSRCVER=TABHEAD(12)
C     STARTER INPUT VERSION
      INVSTR =TABHEAD(13)
C
      LVARINT= TABHEAD(14)
      LVARREA= TABHEAD(15)
C ------------------------------------------

      IF(LVARINT>LMXVINT)THEN
        CALL ANCMSG(MSGID=31,ANMODE=ANINFO,I1=LVARINT,I2=LMXVINT)
        CALL ARRET(2)
      ENDIF

C-----------------------------------------------
C     LECTURE DES VARIABLES ENTIERES.
C-----------------------------------------------
C      warning to radioss developers:
C      IF(LVARINT>LMXVINT)THEN
C        WRITE(*,*) ' ** RADIOSS INTERNAL ERROR'
C        CALL ARRET(7)
C      ENDIF
C-----
      CALL READ_I_C(TABVINT,LVARINT)
      CALL RDCOMI(LMXVINT,LVARINT,TABVINT,MULTI_FVM,H3D_DATA)

C
      IF(ANIM_PLY > 0 .AND.IPLYXFEM == 0) THEN
          WRITE(ISTDO,*) '**WARNING** /ANIM/SHELL/PLY '
          WRITE(ISTDO,*) 
     .  'IS AVAILABLE JUST WITH NEW DELAMINATION SHELL  FORMULATION'
          WRITE(IOUT,*) '**WARNING** /ANIM/SHELL/PLY '
          WRITE(IOUT,*) 
     .  'IS AVAILABLE JUST WITH NEW DELAMINATION SHELL  FORMULATION'     
         ANIM_PLY = 0  
      ENDIF
C
      RETURN
      END

Chd|====================================================================
Chd|  INIRESA                       source/output/restart/rdresa.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|====================================================================
      SUBROUTINE INIRESA(H3D_DATA,PINCH_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
      USE PINCHTYPE_MOD
C-----------------------------------------------
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
#include      "r4r8_p.inc"
#include      "com01_c.inc"
#include      "com02_c.inc"
#include      "com04_c.inc"
#include      "com05_c.inc"
#include      "com09_c.inc"
#include      "com10_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "lon_c.inc"
#include      "scr13_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "cong1_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "rnur_c.inc"
#include      "scrnoi_c.inc"
#include      "scrfs_c.inc"
#include      "scrcut_c.inc"
#include      "scr_fac_c.inc"    
#include      "warn_c.inc"
#include      "couple_c.inc"
#include      "sphcom.inc"
#include      "conv_c.inc"
#include      "check.inc"
#include      "scr20_c.inc"
#include      "scr18_c.inc"
#include      "spmd_c.inc"
#include      "tabsiz_c.inc"
#include      "scr17_c.inc"
#include      "sms_c.inc"
#include      "thermal_c.inc" 
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(PINCH) :: PINCH_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   I,NVAR,M16STAK,NPE,NIPMAX,NPSAV_OLD
      my_real
     .          PROBINT
C-----------------------------------------------
C
      PINCH_DATA%SAPINCH = 0
C ipari0 defini dans le starter
      IF(IPARIT==-1)IPARIT=IPARI0
C
      IF(ICRACK3D > 0)THEN
        IF(IPARIT /= 1) IPARIT = IPARI0
      ENDIF
C
      NUMELUR=SIXUR/NIXUR
      IF(IMACH==3.AND.IPARI0==0.AND.IPARIT==1) THEN
        IF(ISPMD==0)THEN
C Parith/on necessite Parith/ON anterieur en SPMD (structures de donnees additionnelles)
          WRITE(ISTDO,*)' **WARNING PARALLEL ARITHMETIC DISABLED'
          WRITE(IOUT,*)' **WARNING PREVIOUS RUN WITHOUT PARITH/ON'
          WRITE(IOUT,*)' **PARALLEL ARITHMETIC IS NO MORE ACTIVABLE'
        ENDIF
        IPARIT=0
      ENDIF

C calcul de NISKY0 dans ts les cas
      NISKY0 = 8*NUMELS   + 4*NUMELQ    + 4*NUMELC   + 2*NUMELT  +
     .         2*NUMELP   + 3*NUMELR    + 3*NUMELTG  + 4*NUMELUR +
     .         6*NUMELS10 + 12*NUMELS20 + 8*NUMELS16 + 3*NUMELTG6
      LCNEL = NISKY0
      IF(IMACH==3) THEN
C rajout des surfaces de MV comme des elts pour parith/on SPMD
          NISKY0 = NISKY0 + 4*NSKYMV0
C rajout des pressure loads calculee comme des elts pour parith/on SPMD
          NISKY0 = NISKY0 + 4*NCONLD
C rajout des flux thermiques
          NISKY0 = NISKY0 + 4*NUMCONV + 4*NUMRADIA + 4*NFXFLUX
          NISKY0 = NISKY0 + SLLOADP
      END IF
C
C Parith/ON en SPMD
      IF(NUMSPH/=0.AND.IMACH/=3) IPARIT=0
C
      IF(IMACH==3.AND.IPARIT>=1) IPARIT=1
      IF( LWAMP /= 0  .AND. LWANMP/=0)THEN
        LWAMP    = MAX(LWAMP,2*MVSIZ*100)
        LENWA    = MAX( NTHREAD * LWAMP,LWANMP)
      ENDIF
      SWA = LENWA
      SFV = NFUNCT
      SICODT=NUMNOD+2*NUMNOD*MAX(IALE,IEULER,IALELAG)
      SICODR=NUMNOD*IRODDL
C-------------------------------------------
C     IPARITH
C-------------------------------------------
C AR=AM(M73) PLACE RECUPERABLE SI IRODDL=0 A CONDITION DE VERIFIER 
C LES ROUTINES DE FORCES CONCENTEES ET DE VITESSES IMPOSEES
C
      NPSAV_OLD = NPSAV
      IF(NPSAV==0)NPSAV_OLD = 23
      NPSAV = 28

      IF(IPARIT==3)THEN
        SA  = 3*NUMNOD+9*NUMNOD*NTHREAD
        SAR = 3*NUMNOD+9*NUMNOD*NTHREAD
        SSTIFN = NUMNOD+3*NUMNOD*NTHREAD
C   j ai un doute sur SVISCN - et du flag KDTINT
        SVISCN = NUMNOD+3*NUMNOD*NTHREAD
        SSTIFR = NUMNOD*IRODDL+3*NUMNOD*NTHREAD*IRODDL
C non supported option w/IPARIT=3
        SDMSPH = 0
        SPARTSAV = NPSAV*(NPART+NTHPART)*NTHREAD
        SFSKY = 0
        SFSKYM = 0
        SFSKYD = 0
        SISKY    = 0

        PROBINT=0.5
        LSKYI_SMS = MAX(NINT(5*MAX(4*NUMNOD,IMAXIMP)*PROBINT)
     +           +4*(NUMSPH+NSKYI18)+LCNI2,ISUMNX)

        SADSKY   = 0
        SPROCNE  = 0
        SADDCNI2 = 0
        SPROCNI2 = 0
        SIADSDP  = 0
        SIADRCP  = 0
        SIADS    = 0
        SIADWAL  = 0
        SIADRBK  = 0
        SIADI2   = 0
        SIADMV2  = 0
        SIADMV3  = 0
        SIADLL   = 0
        SIADRBM  = 0
        SIADI18  = 0
        SIADRBMK = 0
        SADDCNCND = 0
        SPROCNCND = 0
        SIADCND   = 0
      ELSEIF(IPARIT/=0)THEN
        SA  = 3*NUMNOD
        SAR = 3*NUMNOD
        SSTIFN = NUMNOD
C   j ai un doute sur SVISCN - et du flag KDTINT
        SVISCN = NUMNOD
        SSTIFR = NUMNOD
        IF(SOL2SPH_FLAG/=0) THEN
          SDMSPH = NUMNOD
        ELSE
          SDMSPH = 0
        END IF
        SPARTSAV = NPSAV*(NPART+NTHPART)*NTHREAD
C LCNE0 = NISKY0+voisins (pre calcule dans starter)
        LSKY = LCNE0
        SFSKY = 8*LCNE0
C pxfem
        LSKYPXFEM = LCNEPXFEM          
C xfem for layered shell (cracks)
        LSKYCRKXFEM = LCNECRKXFEM
C FSKY MASSE
        IF(IALE+IEULER+ITHERM+IALELAG/=0) THEN
           SFSKYM = LCNE0
        ELSE
           SFSKYM = 0
        ENDIF
C FSKY DMASS
        IF(SOL2SPH_FLAG/=0) THEN
           SFSKYD = LCNE0
        ELSE
           SFSKYD = 0
        ENDIF
Cel PROBINT egalement initialisee dans STARTER (lectur)
        PROBINT=0.5

        LSKYI = MAX(NINT(5*MAX(4*NUMNOD,IMAXIMP)*PROBINT)
     +         +4*(NUMSPH+NSKYI18),ISUMNX)
        SFSKYI = NFSKYI*LSKYI
        SISKY    = LSKYI

        LSKYI_SMS = LSKYI+LCNI2

        SADSKY   = NUMNOD + 1
C  INDEX VECTOR
        SADSKY = SADSKY + NUMNOD
        SPROCNE  = LCNE0
        SPROCNE_PXFE  = LCNEPXFEM
        SPROCNE_CRKXFE  = LCNECRKXFEM
C  si I2NSNT>0 (tableau addresse connectivite interface type 2)
        IF (I2NSNT>0) THEN
           SADDCNI2 = NUMNOD+1
C Rajout numnod pour index machine vectorielle
           IF(IVECTOR==1) SADDCNI2 = SADDCNI2 + NUMNOD
        ELSE
           SADDCNI2 = 0
        ENDIF
C  LCNI2 tableau de connectivite
        SPROCNI2 = LCNI2
        SIADSDP  = NSPMD+1
        SIADRCP  = NSPMD+1
C IADS (correspond bien au NISKY0 : connections locales)
C IADWAL : NSKYRW0
C IADRBK : NSKYRBK0
C IADI2  : NSKYI2
C IADMV2 : NSKYMV0
C IADMV3 : NSKYIMVC0
C IADLL  : NSKYLL0
C IADRBM : NSKYRBM0
C IADI18 : NSKYI18
        SIADS    = NISKY0
        SIADWAL  = NSKYRW0
        SIADRBK  = NSKYRBK0
        SIADI2   = NISKYI2
        SIADMV2  = NSKYMV0
        SIADMV3  = NSKYMVC0
        SIADLL   = NSKYLL0
        SIADRBM  = NSKYRBM0
        SIADI18  = NSKYI18
        SIADRBMK   = NSKYRBMK0
      ELSE
        SA  = 3*NUMNOD*NTHREAD
        SAR = 3*NUMNOD*NTHREAD
        SSTIFN = NUMNOD*NTHREAD
C pinching
        IF(NPINCH > 0) THEN
          PINCH_DATA%SAPINCH = NTHREAD*NUMNOD
        ELSE
          PINCH_DATA%SAPINCH = 0
        ENDIF
C 
        IF(SOL2SPH_FLAG/=0) THEN
          SDMSPH = NUMNOD*NTHREAD 
        ELSE
          SDMSPH = 0
        END IF
C   j ai un doute sur SVISCN - et du flag KDTINT
        SVISCN = NUMNOD*NTHREAD 
        SSTIFR = NUMNOD*IRODDL*NTHREAD
        SPARTSAV = NPSAV*(NPART+NTHPART)*NTHREAD
        SFSKY = 0
        SFSKYM = 0
        SFSKYD = 0
        SISKY	 = 0

        PROBINT=0.5
        LSKYI_SMS = MAX(NINT(5*MAX(4*NUMNOD,IMAXIMP)*PROBINT)
     +           +4*(NUMSPH+NSKYI18)+LCNI2,ISUMNX)

        SADSKY   = 0
        SPROCNE  = 0
        SADDCNI2 = 0
        SPROCNI2 = 0
        SIADSDP  = 0
        SIADRCP  = 0
        SIADS	 = 0
        SIADWAL  = 0
        SIADRBK  = 0
        SIADI2   = 0
        SIADMV2  = 0
        SIADMV3  = 0
        SIADLL   = 0
        SIADRBM  = 0
        SIADI18  = 0
        SPROCNE_PXFE = 0
        SPROCNE_CRKXFE = 0
      ENDIF
      SUWA = NUSERW
C      
       IF (NS10E >0 .AND. IPARI0 /= 0) THEN
        SADDCNCND = NUMNOD + 1
        SPROCNCND = LCNCND
        SIADCND   = 2*NS10E
       ELSE
        SADDCNCND = 0
        SPROCNCND = 0
        SIADCND   = 0
       END IF

C Si quasi compressible variables non necessaires
      IF(INCOMP==0)THEN

        SVAL2=
     +  MAX(IALE,ITHERM,IEULER,IALELAG)*(NUMELS+NUMELQ+NUMELTG+NSVOIS+NQVOIS+NTGVOIS)
     *  *MAX(1,NMULT)

C tableau temporaire phi et valeur segsurf
         SPHI = MAX(IALE,ITHERM,IEULER,IALELAG)*( NUMELS + NUMELQ + NUMELTG +
     *                                            NSVOIS + NQVOIS + NTGVOIS + NSEGFLU )
      ELSE
        SVAL2 = 0
        SPHI = 0
      ENDIF
      SR = 0
      SESTIF = 0
C-------------------------------------------
C     LINK
C-------------------------------------------
      SILINK=4*NRLINK
      IF(IMACH==3) THEN
         SFR_RL = (NSPMD+2)*NRLINK
      ELSE
         SFR_RL = 0
      ENDIF

      SLLINK=LLLINK
      IF(IMACH==3.AND.IPARIT/=0) THEN
         SIADRL = LLLINK
      ELSE
         SIADRL = 0
      ENDIF
      !SLINALE= SLINALE + LLINAL
C-------------------------------------------
C     IMPLICIT
C-------------------------------------------
C---------------------------------------------------------
C     FLUX ET ROTATION
C---------------------------------------------------------
      SCRFLSW = 9*NSFLSW
      SFLSW   = 6*NSFLSW
      SNEFLSW = 1*NSFLSW
      SNNFLSW = 8*NTFLSW
C---------------------------------------------------------
C     ANIMATION (VECTEURS CONTACT,FORCES INTERNES,FORCES EXTERNES)
C     CUTS NOISE 
C---------------------------------------------------------
      SFANI = 3*NUMNOD*(
     .                MIN(1,ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT)
     .              + MIN(1,ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT)
     .              + MIN(1,ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT) )
     .          + 6*(NSECT+NRBODY+NRWALL+NUMELUR)
     .          + 6*NUMNOD*MIN(1,ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT)  
     .          + 3*NUMNOD*MIN(1,ANIM_V(13)+H3D_DATA%N_VECT_CONT2)
     .          + 6*NUMNOD*MIN(1,ANIM_V(27)+H3D_DATA%N_VECT_PCONT2)  
      SFANI_CELL = (NUMELS)*(ANIM_SE(10)+ANIM_SE(4960)+ANIM_SE(4961)) !ALE : 3D-Vorticity        
      SFANI_CELL = SFANI_CELL+(NUMELS+NUMELQ)*3*(H3D_DATA%N_VECT_CONT) !ALE : FSI reaction force  /H3D/ELEM/VECT/CONT

      SXCUT = 7*NCUTS
      SANIN = NUMNOD*( 
     .                MIN(1,ANIM_N(1)+OUTP_N(1)+H3D_DATA%N_SCAL_DT)
     .              + MIN(1,ANIM_N(2)+OUTP_N(2)+H3D_DATA%N_SCAL_DMAS)
     .              + MIN(1,ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER))
     .          + NUMELR*(ANIM_FE(11)+ANIM_FE(12)+ANIM_FE(13))
     .              + 2 * NUMNOD * MIN(1,ANIM_N(15)*ANIM_N(16)+H3D_DATA%N_SCAL_DAMA2)
      STANI = 6*(NUMELC+NUMELTG)*IEPSDOT 
     .          + 15*(NUMELT+NUMELP+NUMELR)*(MAX(ANIM_FT(1),H3D_DATA%UND_FORC))
C
      SSECFCUM = 7*NUMNOD*NSECT*ISECUT
C     Work Array private to sph : WAPSH(LWASPH)
      SWASPH = LWASPH
C
      M16STAK = 0
      IF(NUMELS16/=0)THEN
        NPE = 16
        NIPMAX = 81
        M16STAK = (11*NIPMAX+15*NPE+3*NIPMAX*NPE)*MVSIZ
      ENDIF
      IF(NUMELS20/=0)THEN
        NPE = 20
        NIPMAX = 81
        M16STAK = MAX(M16STAK,(10*NIPMAX+15*NPE+3*NIPMAX*NPE)*MVSIZ)
      ENDIF
      NPSAV = NPSAV_OLD
C     NPSAV remis a la bonne valeur dans rdresb
      IF(NUMELS8A>0)THEN
        NIPMAX = 729
        M16STAK = MAX(M16STAK,12*NIPMAX*MVSIZ)
      ENDIF
      SW16 = M16STAK * NTHREAD
      SICUT= 44*NCUTS     
C---------------------------------------------------------
C     tableaux spmd additionnels alloues dans ma en v40
C---------------------------------------------------------
C---------------------------------------------------------
C     tableaux spmd additionnels alloues dans am en v40
C---------------------------------------------------------
      IF(IMACH==3) THEN
        SDRETRIO = 5*NINTER
      ELSE
        SDRETRIO = 0
      ENDIF
      SLBVRS = 21*NIBVEL
C---------------------------------------------------------
C Stockages facultatifs
C---------------------------------------------------------
      IF(NNOISE==0)THEN
         NNOISE=NNOISER*RNOI
         NVAR=NCNOIS*RNOI
      ELSE
         IF((NOISEV+NOISEA+NOISEP)==0) NOISEV = 1
         NVAR=3*NOISEV+3*NOISEA+NOISEP
      ENDIF
      IF01=1
      MF01=1
      IFIF=IF01
      MFIF=MF01
      SINOISE = 0
      SFNOISE = 0
      IF(NNOISE>0)THEN
       IFIF = (2*NNOISE+10)
       MFIF = (6*NVAR*NNOISE+1)
       SINOISE = 2*NNOISE+10
       SFNOISE = 6*NVAR*NNOISE+1
      ENDIF
      NNOISE_SAV = NNOISE

C---------------------------------------------------------
C
      RETURN
      END
C
