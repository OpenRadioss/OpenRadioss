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
C.../COM01/
      COMMON /COM01/ N2D,    NCPRI,  IUSERWI, IALE,    NGROUP,
     .               NCYCLE, IRUN,   IGER,    LBUFEL,  IRODDL,
     .               IEULER, IMULTI_FVM, ITHERM, IHSH,    ITESTV,  ITURB,
     .               ILAG,   ISECUT, IDAMP,   IRXDP,   NMULT,
     .               INTEG8, ISIGI,  NSPMD, LENWA,   ISGIFL,
     .               ISUB,   NNODS,  NCNOIS,  LCNE0,   IPARI0,
     .               IMAXIMP,NNOISER,NSPGROUP,IRESMD,  IFRWV,
     .               INTBAG ,ICLOSE ,LICBAG  ,LRCBAG,LIBAGJET,
     .              LRBAGJET,LIBAGHOL,LRBAGHOL,ISHFRAM,MSHSUB,
     .              NSHFRONT,TRIMAT,IUSHELL,IUSOLID,NUSHELL,
     .              NUSOLID, NVSHELL,INISHVAR, NSPROC, NDSOLV,
     .              NSBMAX,  NSVMAXT, NFLOW, ICONDP,NVSOLID1,
     .              NVSOLID2,NFASOLFR,KCONTACT,ITHERMC,INTHEAT,
     .              IORTSHEL,NORTSHEL,ICRACK,IUSRSHEL,ITHERM_FE,
     .           IMPOSE_DR,IRIGID_MAT,ISTAMPING,IINTTHICK,
     .              IPLYXFEM,IPLYBCS,IREAC,IGRELEM,IREST_MSELT,
     .              NTHREAD,IALELAG,INVOL,IIMPLICIT,IRODDL0,
     .              NVSOLID3,IMASADD,IDAMP_RDOF,KMP_SET,KMPSTSIZ,
     .              ICRASH,STACKSIZ,FTEMPVAR21,NVSOLID4,INTPLYXFEM,
     .              UNITMAX,NVSHELL1,NFILSOL,IPART_STACK,TEST_POIDS,
     .              IPRIVATE,IPERTURB,IPART_PCOMPP,ITHKSHEL,ISH3NFRAM,
     .              IUFACYLD,NVSHELL2,NVSOLID5,NUBEAM,NVBEAM,NVTRUSS,
     .              IS17OLD ,NSEGQUADFR, NUMBCSN,INTERADHESION,NITSCHE,
     .              INISPRI ,INISHVAR1,NVSPRI,NVSOLID6

      INTEGER        N2D,    NCPRI,  IUSERWI, IALE,    NGROUP,
     .               NCYCLE, IRUN,   IGER,    LBUFEL,  IRODDL,
     .               IEULER, IMULTI_FVM, ITHERM, IHSH,    ITESTV,  ITURB,
     .               ILAG,   ISECUT, IDAMP,   IRXDP,  NMULT, 
     .               INTEG8, ISIGI,  NSPMD, LENWA,   ISGIFL,
     .               ISUB,   NNODS,  NCNOIS,  LCNE0,   IPARI0,
     .               IMAXIMP,NNOISER,NSPGROUP,IRESMD,  IFRWV,
     .               INTBAG ,ICLOSE ,LICBAG  ,LRCBAG,LIBAGJET,
     .              LRBAGJET,LIBAGHOL,LRBAGHOL,ISHFRAM,MSHSUB,
     .              NSHFRONT,TRIMAT,IUSHELL,IUSOLID,NUSHELL,
     .              NUSOLID, NVSHELL,INISHVAR, NSPROC, NDSOLV,
     .              NSBMAX,  NSVMAXT, NFLOW, ICONDP,NVSOLID1,
     .              NVSOLID2,NFASOLFR,KCONTACT,ITHERMC,INTHEAT,
     .              IORTSHEL,NORTSHEL,ICRACK,IUSRSHEL,ITHERM_FE,
     .           IMPOSE_DR,IRIGID_MAT,ISTAMPING,IINTTHICK,
     .              IPLYXFEM,IPLYBCS,IREAC,IGRELEM,IREST_MSELT,
     .              NTHREAD,IALELAG,INVOL,IIMPLICIT,IRODDL0,
     .              NVSOLID3,IMASADD,IDAMP_RDOF,KMP_SET,KMPSTSIZ,
     .              ICRASH,STACKSIZ,FTEMPVAR21,NVSOLID4,INTPLYXFEM,
     .              UNITMAX,NVSHELL1,NFILSOL,IPART_STACK,TEST_POIDS,
     .              IPRIVATE,IPERTURB,IPART_PCOMPP,ITHKSHEL,ISH3NFRAM,
     .              IUFACYLD,NVSHELL2,NVSOLID5,NUBEAM,NVBEAM,NVTRUSS,
     .              IS17OLD ,NSEGQUADFR, NUMBCSN,INTERADHESION,NITSCHE,
     .              INISPRI ,INISHVAR1,NVSPRI,NVSOLID6
