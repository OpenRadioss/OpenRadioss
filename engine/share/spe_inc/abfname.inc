Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
/* include C pour donner un nom abf specifique a chaque platteforme */

#if CPP_mach==CPP_macosx64
char * ABFPLATTTF = "macosx64";

#elif CPP_mach==CPP_p4linux964
#ifdef __aarch64__
char * ABFPLATTTF = "linuxa64";
#elif 1
char * ABFPLATTTF = "linux64";
#endif 
#elif CPP_mach==CPP_p4linux932
char * ABFPLATTTF = "linux32";

#elif CPP_mach==CPP_p4win32
char * ABFPLATTTF = "win32.exe";

#elif CPP_mach==CPP_p4win64
char * ABFPLATTTF = "win64.exe";

#elif 1
char * ABFPLATTTF = "linux";
#endif

