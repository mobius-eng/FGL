#define _INFO_(MSG)
#define _INFO_IF_(COND, MSG)
#define _DEBUG_(MSG)
#define _DEBUG_IF_(COND, MSG)
#define _ERROR_(MSG)
#define _ERROR_IF_(COND, MSG)
      
#ifdef LOGINFO
#define LOGDEBUG
#undef _INFO_
#undef _INFO_IF_
#define _INFO_(MSG) CALL print_log(MSG, 'INFO', .TRUE.)
#define _INFO_IF_(COND, MSG) IF (COND) CALL print_log(MSG, 'INFO', .TRUE.)
#endif

#ifdef LOGDEBUG
#define LOGERROR
#undef _DEBUG_
#undef _DEBUG_IF_
#define _DEBUG_(MSG) CALL print_log(MSG, 'DEBUG', .TRUE.)
#define _DEBUG_IF_(COND, MSG) IF (COND) CALL print_log(MSG, 'INFO', .TRUE.)
#endif

#ifdef LOGERROR
#undef _ERROR_
#undef _ERROR_IF_
#define _ERROR_(MSG) CALL print_log(MSG, 'ERROR', .TRUE.)
#define _ERROR_IF_(COND, MSG) IF (COND) CALL print_log(MSG, 'ERROR', .TRUE.)
#endif
