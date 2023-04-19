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
Chd|  SPMD_ANIM_PLY_INIT            source/spmd/spmd_anim_ply_init.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules1/plyxfem_mod.F  
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE SPMD_ANIM_PLY_INIT (IGEO, GEO  ,IPARG  ,IXC  ,IXTG ,
     .                          IPARTC,IPARTQ,IPARTTG,STACK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .  GEO(NPROPG,*)
      INTEGER IXC(NIXC,*),IPARTQ(*),IPARTTG(*),IXTG(NIXTG,*),
     .        IPARG(NPARG,*), IPARTC(*),IGEO(NPROPGI,*)
C     
      TYPE (STACK_PLY) :: STACK
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER IE,NG, ITY, LFT, LLT, NPT, N, I, J,
     .        IPRT, NEL, IAD, NPAR, NFT, IMID,IALEL,MTN,
     .        JJ, K, SH_IH, IHBE,BUF,ISHPLYXFEM,IPID,IPT,
     .        IPLY,IPPID,JPID,TAG_ARRAY(NUMNOD),TAG_SHELLS(NUMELC),
     .        NCOUNT,SHCOUNT,MATER(NPART),ISUBSTACK

C-----------------------------------------------
      DO I=1,NPART
        MATER(I)=0
      ENDDO

        DO NG = 1, NGROUP
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG)
          ITY   =IPARG(5,NG)
          IF(ITY==2)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTQ(N)) = 1
            ENDDO
          ELSEIF(ITY==3)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTC(N)) = 1
            ENDDO
          ELSEIF(ITY==7)THEN
            DO I = 1, NEL
              N = I + NFT
              MATER(IPARTTG(N)) = 1
            ENDDO
          ENDIF
      ENDDO

c

C     INITIALISATION

      ALLOCATE(PLYNOD(NPLYMAX))

      DO I=1,NPLYMAX
        PLYNOD(I)%PLYNUMNODS = 0
      ENDDO

      ALLOCATE(PLYSHELL(NPLYMAX))
      DO I=1,NPLYMAX
        PLYSHELL(I)%PLYNUMSHELL = 0
      ENDDO
C-----------------------------------------------
C     NORMALE
C-----------------------------------------------
      IE = 0
C-----------------------------------------------
      IPPID = 100
C-----------------------------------------------
C     PART
C----------------------------------------------- 
       ALLOCATE (IDPID_PLY(NPLYMAX))
       IDPID_PLY= 0
      DO K=1,NPLYMAX
        TAG_ARRAY=0
        TAG_SHELLS=0
        SHCOUNT=0
        DO 500 IPRT=1,NPART
          IF(MATER(IPRT) == 0)GOTO 500
          DO 490 NG=1,NGROUP
             MTN   =IPARG(1,NG)
             NEL   =IPARG(2,NG)
             NFT   =IPARG(3,NG)
             IAD   =IPARG(4,NG)
             ITY   =IPARG(5,NG)
             ISHPLYXFEM = IPARG(50,NG)
             LFT=1
             LLT=NEL

             IF(ISHPLYXFEM > 0) THEN
C-----------------------------------------------
C       COQUES
C-----------------------------------------------
               IF(ITY==3)THEN
                 NPT   =IPARG(6,NG)
                 IHBE = IPARG(23,NG)
                 ISUBSTACK = IPARG(71,NG)
                 IPPID = 2
                 DO 130 I=LFT,LLT
                   N = I + NFT
                   IF(IPARTC(N)/=IPRT)GOTO 130
                    IPID = IXC(6,N)
                    DO IPT=1,NPT
                      JPID = STACK%IGEO(IPPID + IPT, ISUBSTACK)
                      IPLY = IGEO(102  ,JPID)
                      IDPID_PLY(IPLY) =IGEO(1  ,JPID) 
                      IF (IPLY == K) THEN
                         TAG_ARRAY(IXC(2,N))=1
                         TAG_ARRAY(IXC(3,N))=1
                         TAG_ARRAY(IXC(4,N))=1
                         TAG_ARRAY(IXC(5,N))=1

                         TAG_SHELLS(N) = IPT
                         PLYSHELL(K)%PLYNUMSHELL =
     *                                PLYSHELL(K)%PLYNUMSHELL+1
                      ENDIF

                    ENDDO
 130             CONTINUE

C-----------------------------------------------
C       COQUES 3 NOEUDS
C-----------------------------------------------
               ELSEIF(ITY==7)THEN
C
               ENDIF
             ENDIF
 490      CONTINUE
C
 500    CONTINUE
C
C Ply Nodes Post treatments
C
        DO I=1,NUMNOD
           IF (TAG_ARRAY(I)==1)
     *        PLYNOD(K)%PLYNUMNODS = PLYNOD(K)%PLYNUMNODS + 1
        ENDDO
        ALLOCATE (PLYNOD(K)%NODES(PLYNOD(K)%PLYNUMNODS))

        NCOUNT = 0
        DO I=1,NUMNOD
           IF (TAG_ARRAY(I)==1) THEN
             NCOUNT=NCOUNT+1
             PLYNOD(K)%NODES(NCOUNT)=I
           ENDIF
        ENDDO
C
C Ply Shells Post treatments
C
        ALLOCATE (PLYSHELL(K)%SHID(PLYSHELL(K)%PLYNUMSHELL))
        ALLOCATE (PLYSHELL(K)%SHELLIPT(PLYSHELL(K)%PLYNUMSHELL))

C debugs    print*,'PLYSHELL : ',PLYSHELL(K)%plynumshell,NUMELC

        SHCOUNT=0
        DO I=1, NUMELC
           IF (TAG_SHELLS(I) > 0)THEN
               SHCOUNT=SHCOUNT+1
               PLYSHELL(K)%SHID(SHCOUNT)=I
               PLYSHELL(K)%SHELLIPT(SHCOUNT)=TAG_SHELLS(I)
           ENDIF
        ENDDO

      ENDDO

C-------------------------------
C Numeros globaux des shell ply
C-------------------------------
      NCOUNT = 0
      DO K=1,NPLYMAX
        ALLOCATE(PLYSHELL(K)%SHELLID(PLYSHELL(K)%PLYNUMSHELL))
        DO I=1,PLYSHELL(K)%PLYNUMSHELL
           NCOUNT=NCOUNT+1
           PLYSHELL(K)%SHELLID(I)=NCOUNT
        ENDDO
      ENDDO

C-------------------------------
C Numeros globaux des noeuds ply
C-------------------------------
      NCOUNT = 0
      DO K=1,NPLYMAX
        ALLOCATE(PLYNOD(K)%PLYNODID(PLYNOD(K)%PLYNUMNODS))
        DO I=1,PLYNOD(K)%PLYNUMNODS
           NCOUNT=NCOUNT+1
           PLYNOD(K)%PLYNODID(I)=NCOUNT
        ENDDO
      ENDDO

C-------------------------------
C Plyparts
C-------------------------------
      ALLOCATE (INDX_PLY(NPLYMAX))
      NPLYPART = 0
      INDX_PLY = 0
      DO K=1,NPLYMAX
        IF ( PLYSHELL(K)%PLYNUMSHELL >0) THEN
          NPLYPART = NPLYPART + 1
          INDX_PLY(NPLYPART) = K
          IDPID_PLY(NPLYPART) = IDPID_PLY(K)
        ENDIF
      ENDDO

C      print*,'NPLYPART:',NPLYPART,'NPLYMAX:',NPLYMAX

      NPLYNODG = 0
      DO K=1,NPLYMAX
        NPLYNODG = NPLYNODG + PLYNOD(K)%PLYNUMNODS
      ENDDO


C      print*,'plynodg=',nplynodg
C      print*,'---------------- nodes -----------------------'
C      DO K=1,NPLYMAX
C        print*,'=== ply:',K,'===='
C        DO I=1,PLYNOD(K)%PLYNUMNODS
C          print*,'ND:',PLYNOD(K)%NODES(I),PLYNOD(K)%PLYNODID(I)
C        ENDDO
C      ENDDO



      RETURN
      END
Chd|====================================================================
Chd|  GET_PLY_NOD                   source/spmd/spmd_anim_ply_init.F
Chd|-- called by -----------
Chd|        W_ANIM_PLY                    source/restart/ddsplit/w_anim_ply.F
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules1/plyxfem_mod.F  
Chd|====================================================================
      INTEGER FUNCTION GET_PLY_NOD(IPLY,NODID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPLY,NODID
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,BINF,BSUP,MILIEU,SIZE,EMPL,TAILLE
C-----------------------------------------------
      SIZE=PLYNOD(IPLY)%PLYNUMNODS

      BINF=1
      BSUP=SIZE

      IF (PLYNOD(IPLY)%NODES(BINF)==NODID)THEN
        EMPL=BINF
        GOTO 100
      ENDIF

      IF (PLYNOD(IPLY)%NODES(BSUP)==NODID)THEN
        EMPL=BSUP
        GOTO 100
      ENDIF

      TAILLE = BSUP-BINF+1
      DO WHILE ( TAILLE > 5)
         MILIEU = (BSUP-BINF)/2 + BINF

         IF (PLYNOD(IPLY)%NODES(MILIEU) == NODID) THEN
           EMPL = MILIEU
           GOTO 100
         ENDIF
         IF ( PLYNOD(IPLY)%NODES(MILIEU) < NODID) THEN
           BINF = MILIEU
         ELSE
           BSUP = MILIEU
         ENDIF
         TAILLE = BSUP-BINF+1
      ENDDO

      DO I=BINF,BSUP
        IF  (PLYNOD(IPLY)%NODES(I) == NODID) THEN
           EMPL = I
           GOTO 100
        ENDIF
      ENDDO

 100  CONTINUE

      GET_PLY_NOD = EMPL

      RETURN
      END


Chd|====================================================================
Chd|  GET_PLY_NODID                 source/spmd/spmd_anim_ply_init.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules1/plyxfem_mod.F  
Chd|====================================================================
      INTEGER FUNCTION GET_PLY_NODID(IPLY,NODID,OFFSET)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPLY,NODID,OFFSET
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,BINF,BSUP,MILIEU,SIZE,EMPL,TAILLE
C-----------------------------------------------
      SIZE=PLYNOD(IPLY)%PLYNUMNODS

      BINF=1
      BSUP=SIZE

      IF (PLYNOD(IPLY)%NODES(BINF)==NODID)THEN
        EMPL=BINF
        GOTO 100
      ENDIF

      IF (PLYNOD(IPLY)%NODES(BSUP)==NODID)THEN
        EMPL=BSUP
        GOTO 100
      ENDIF

      TAILLE = BSUP-BINF+1
      DO WHILE ( TAILLE > 5)
         MILIEU = (BSUP-BINF)/2 + BINF

         IF (PLYNOD(IPLY)%NODES(MILIEU) == NODID) THEN
           EMPL = MILIEU
           GOTO 100
         ENDIF
         IF ( PLYNOD(IPLY)%NODES(MILIEU) < NODID) THEN
           BINF = MILIEU
         ELSE
           BSUP = MILIEU
         ENDIF
         TAILLE = BSUP-BINF+1
      ENDDO

      DO I=BINF,BSUP
        IF  (PLYNOD(IPLY)%NODES(I) == NODID) THEN
           EMPL = I
           GOTO 100
        ENDIF
      ENDDO

 100  CONTINUE

      GET_PLY_NODID = PLYNOD(IPLY)%PLYNODID(EMPL)+OFFSET

      RETURN
      END
