!*********************
! dee2018-12-01  
! ����� ������ � �������� ���������
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
S                    &CString                                !������
Len                  Long                                    !����� ������
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
Empty                Procedure(Long=0)                       !�������� ������ � ��������� ���������
Cat                  Procedure(String),*MailSlot,Proc        !������������ � ������ (������������)
Cat                  Procedure(*CString),*MailSlot,Proc      !������������ � ������ (������������)
Len                  Procedure(),Long                        !����� ������
Str                  Procedure(),*String                     !������� string ������
Cstr                 Procedure(),*CString                    !������� Cstring ������
Set                  Procedure(String)                       !��������� �������� ������
SetEx                Procedure(long)                         !������ ������ ������ ��� ���������� ��������
Set                  Procedure(*CString)                     !��������� �������� ������
Sub                  Procedure(Long,<Long>),*String          !������� ����� ������ .Sub(���.�������, ���. �������)
Slice                Procedure(Long,Long),*String            !������� ����� ������ .Slice(���.�������, ���. �������) *������� �� ��������� ����� �� �������
Kill                 Procedure()
!********************************* (Client) ******************************
SendInit             Procedure(String _MailslotName,<String _HostName>),long  !������������� ��������
SendMess             Procedure(String _Mess),long                             !��������� ������
SendClose            Procedure(),long,Proc                                    !������� ����������
!********************************* (Server) ******************************
ReceiveInit          Procedure(String _MailslotName,string _UniqueID,<String _HostName>),long !������������� ������
ReceiveClose         Procedure(),long,Proc                                    !������� ������
ReceiveQuery         Procedure(),long                                         !��������� ���������
ReceiveGetItem       Procedure(),VIRTUAL                                      !�������� ������ ���������
!*************************************************************************
                end