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

