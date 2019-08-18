!*********************
! dee2018-12-01  
! Класс приема и передачи майслотов
!*********************

ms_SECURITY_ATTRIBUTES group,type
nLength                 long
lpSecurityDescriptor    long
bInheritHandle          bool
                      end
ms_OVERLAPPED   Group,Type
Internal         UNSIGNED
InternalHigh     UNSIGNED
Offset           UNSIGNED
OffsetHigh       UNSIGNED
hEvent           UNSIGNED
               End

ms_MAILSLOT_WAIT_FOREVER EQUATE(-1)
ms_MAILSLOT_NO_MESSAGE   EQUATE(-1)
ms_GENERIC_READ          equate(80000000h)
ms_GENERIC_WRITE         equate(40000000h)
ms_FILE_SHARE_READ       equate(00000001h)
ms_FILE_SHARE_WRITE      equate(00000002h)
ms_OPEN_EXISTING         equate(3)
ms_INVALID_HANDLE_VALUE  equate(-1)
ms_FILE_ATTRIBUTE_NORMAL EQUATE(00000080h)
ms_GPTR                  EQUATE(0040h)

MailSlot        Class,Type,Module('MailSlot.clw'),Link('MailSlot.clw')
S                    &CString                                !строка
Len                  Long                                    !длина строки
!*************************************************************************
ms_cbRead            long,protected
ms_CA                group(ms_SECURITY_ATTRIBUTES),PRE(CA),protected.
ms_fReturnCode       BOOL,protected
ms_cbMessages        ulong,protected
ms_cbMsgNumber       ulong,protected
ms_lpMaxMessageSize  ulong,protected
ms_lpReadTimeout     ulong,protected
ms_cbWritten         long,protected
ms_NameEvent         cstring(50),protected
ms_hEvent            long,protected
ms_lpszMailslotName  cstring(255),protected
ms_hMailslot         long,protected
ms_OV                group(MS_OVERLAPPED),pre(ms_OV),protected.
ms_lhMem             long,protected
ms_lnPtr             long,protected
ms_achID             string(512),protected
ms_cAllMessages      long,protected
!*************************************************************************
Construct            Procedure()
Destruct             Procedure()
Empty                Procedure(Long=0)                       !очистить строку и заполнить пробелами
Cat                  Procedure(String),*MailSlot,Proc        !присоеденить к строке (конкатинация)
Cat                  Procedure(*CString),*MailSlot,Proc      !присоеденить к строке (конкатинация)
Len                  Procedure(),Long                        !длина строки
Str                  Procedure(),*String                     !вернуть string строку
Cstr                 Procedure(),*CString                    !вернуть Cstring строку
Set                  Procedure(String)                       !начальное значение строки
SetEx                Procedure(long)                         !задать размер строки без начального значения
Set                  Procedure(*CString)                     !начальное значение строки
Sub                  Procedure(Long,<Long>),*String          !вернуть часть строки .Sub(нач.позиция, кон. позиция)
Slice                Procedure(Long,Long),*String            !вернуть часть строки .Slice(нач.позиция, кон. позиция) *функция не проверяет выход за границы
Kill                 Procedure()
!********************************* (Client) ******************************
SendInit             Procedure(String _MailslotName,<String _HostName>),long  !инициализация отправки
SendMess             Procedure(String _Mess),long                             !отправить строку
SendClose            Procedure(),long,Proc                                    !закрыть соединение
!********************************* (Server) ******************************
ReceiveInit          Procedure(String _MailslotName,string _UniqueID,<String _HostName>),long !инициализация приема
ReceiveClose         Procedure(),long,Proc                                    !закрыть сервер
ReceiveQuery         Procedure(),long                                         !запросить сообщения
ReceiveGetItem       Procedure(),VIRTUAL                                      !получить строку сообщения
!*************************************************************************
                end