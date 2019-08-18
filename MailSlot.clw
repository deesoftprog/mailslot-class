!*********************
! dee2018-12-01  
! Класс приема и передачи майслотов
!*********************
      MEMBER
      Map
         Module('Window API')
             !--Mailslot
             wapi_CreateMailslot(*CSTRING _lpName,long _nMaxMessageSize,long _lReadTimeout,long _pSECURITY_ATTRIBUTES),long,PASCAL,RAW,name('CreateMailslotA')
             wapi_GetMailslotInfo(long _hMailslot,|
                                  *ulong _lpMaxMessageSize,|
                                  *ulong _lpNextSize,|
                                  *ulong _lpMessageCount,|
                                  *ulong _lpReadTimeout|
                                  ),BOOL,PASCAL,name('GetMailslotInfo')
             wapi_SetMailslotInfo(long _hMailslot,long _lReadTimeout),BOOL,PASCAL,name('SetMailslotInfo')
             wapi_CloseHandle(long _Handle),BOOL,PROC,PASCAL,name('CloseHandle')
             wapi_CreateFile(*cstring szFileName, |
                             long dwDesiredAccess, |
                             long dwShareMode, |
                             long lpSecurityAttributes, |
                             long dwCreationDisposition, |
                             long dwFlagsAndAttributes, |
                             long hTemplateFile),|
                             long, |
                             pascal,raw,name('CreateFileA')
             wapi_WriteFile(long hFile, |
                           long lpBuffer, |
                           long dwBytes, |
                           *long dwBytesWritten, |
                           long lpOverlapped),|
                           bool,raw,pascal,proc,name('WriteFile')
             wapi_ReadFile(long hFile, | 
                           long lpBuffer, | 
                           long dwBytes, | 
                           *long dwBytesRead, | 
                           long lpOverlapped), |
                           bool,raw,pascal,name('ReadFile')
             wapi_CreateEventA(long _pSECURITY_ATTRIBUTES,BOOL _bManualReset,BOOL _bInitialState,*cstring _lpName),long,PASCAL,RAW,name('CreateEventA')

             wapi_GlobalAlloc(long uFlags, long dwBytes),long,pascal,name('GlobalAlloc')
             wapi_GlobalFree(long hMem),long,pascal,proc,name('GlobalFree')
             wapi_MoveMemory(long DestinationPtr,long SourcePtr,long dwLength),bool,raw,proc,pascal,name('RtlMoveMemory')
             wapi_GlobalLock(long hMem),long,pascal,proc,name('GlobalLock')
             wapi_GlobalUnlock(long hMem),bool,pascal,proc,name('GlobalUnlock')
             
             Cstr_Memmove(Ulong Dest,Ulong Src,Ulong Size),Name('_memmove')
             Cstr_MultiByteToWideChar(Unsigned Codepage,Ulong dwFlags,ULong LpMultuByteStr, |
                 Long cbMultiByte, ULong LpWideCharStr,Long cchWideChar),RAW,Ulong,PASCAL
             Cstr_WideCharToMultiByte(Unsigned Codepage,Ulong dwFlags,ULong LpWideCharStr,Long cchWideChar, |
                 ULong lpMultuByteStr, Long cbMultiByte, Ulong LpDefalutChar,Ulong lpUsedDefalutChar), |
                 Raw,Ulong,Pascal 
             wapi_GetLastError(),ulong,PASCAL,name('GetLastError') 

         End
         MODULE('C Standart Library')
             CSL_memcpy(LONG,LONG,LONG),LONG,PROC,NAME('_memcpy')
         END
      End
      Include('MailSlot.inc'),Once
!*************************************************************************
MailSlot.Construct Procedure()
                  Code
                  Self.S &= New CString(2)
                  Self.S = ''
                  Self.Len=0
                  Return
!*************************************************************************
MailSlot.Destruct Procedure()
                  Code
                  If ~(Self.S &= Null)
                     Dispose(Self.S)
                     Self.S &= Null 
                     Self.Len=0
                  End
                  Return
!*************************************************************************              
MailSlot.Empty    Procedure(Long L)
                  Code
                  If L=0
                     Self.Len=0
                  Else
                     Self.Set(All(' ',L))
                  End
                  Return
!*************************************************************************
MailSlot.Cat      Procedure(String P)
L                 Long
NewStr            &CString
                  Code
                  L=Len(P)
                  NewStr &=  New CString(Self.Len+L+1)
                  Cstr_MemMove(Address(NewStr),Address(Self.S),Self.Len)
                  Cstr_MemMove(Address(NewStr)+Self.Len,Address(P),L)
                  NewStr[Self.Len+L+1]='<0>'
                  Dispose(Self.S)
                  Self.S &= NewStr
                  Self.Len+=L
                  Return Self
!*************************************************************************
MailSlot.Cat      Procedure(*CString P)
L                 Long
NewStr            &CString
                  Code
                  L=Len(P)
                  NewStr &=  New CString(Self.Len+L+1)
                  Cstr_MemMove(Address(NewStr),Address(Self.S),Self.Len)
                  Cstr_MemMove(Address(NewStr)+Self.Len,Address(P),L)
                  NewStr[Self.Len+L+1]='<0>'
                  Dispose(Self.S)
                  Self.S &= NewStr
                  Self.Len+=L
                  Return Self
!*************************************************************************
MailSlot.Len      Procedure()
                  Code
                  Return(Self.Len)
!*************************************************************************
MailSlot.Str      Procedure
                  Code
                  Return Self.S[1 : Self.Len]
!*************************************************************************
MailSlot.CStr     Procedure()
                  Code
                  Return Self.S
!*************************************************************************
MailSlot.Set      Procedure(String S)
                  Code
                  Self.Len=Len(S)
                  Dispose(Self.S)
                  Self.S &= New CString(Self.Len+1)
                  Cstr_Memmove(Address(Self.S),Address(S),Self.Len)
                  Self.S[Self.Len+1]='<0>'
                  Return
!*************************************************************************                  
MailSlot.SetEx    Procedure(long L)
                  Code
                  Self.Len=L
                  Dispose(Self.S)
                  Self.S &= New CString(L+1)
                  Self.S[Self.Len+1]='<0>'
                  Return
!*************************************************************************
MailSlot.Set      Procedure(*CString CS)
L                 Long
NewStr            &String
                  Code
                  Self.Len=Len(CS)
                  Dispose(Self.S)
                  Self.S &= New CString(Self.Len+1)
                  Self.S=CS
                  Return
!*************************************************************************
MailSlot.Sub      Procedure(Long From,<Long L>)
                  Code
                  If ~Omitted(L)
                     IF From > Self.Len 
                        Return Self.S[1:0] 
                     END   
                     IF L <= Self.Len 
                        Return Self.S[From : L]
                     Else
                        Return Self.S[From : Self.Len] 
                     End   
                  Else
                     Return Self.S[From : Self.Len] 
                  End
!*************************************************************************
MailSlot.Slice    Procedure(Long F,Long T)
                  Code
                  Return Self.S[F : T]
!*************************************************************************
MailSlot.Kill     Procedure()
                  Code
                  Dispose(Self)
                  Return
!*************************************************************************
MailSlot.ReceiveGetItem Procedure()
                  Code
                  Return
!*************************************************************************          
MailSlot.SendInit Procedure(String _MailslotName,<String _HostName>)
L_HostName string(50)
  CODE
  if omitted(_HostName)
     L_HostName= '.'
  else
     L_HostName= _HostName
  end
  self.ms_lpszMailslotName = '\\'&|
       clip(L_HostName) &|
       '\mailslot\' &|
       clip(_MailslotName)
                       
  !---откроем канал
  self.ms_hMailslot = wapi_CreateFile(self.ms_lpszMailslotName, |
                              ms_GENERIC_WRITE,|
                              ms_FILE_SHARE_READ, | 
                              0, | 
                              ms_OPEN_EXISTING, | 
                              ms_FILE_ATTRIBUTE_NORMAL, | 
                              0)
  
  if self.ms_hMailslot = ms_INVALID_HANDLE_VALUE
     return wapi_GetLastError()
  end                       
  return 0
!*************************************************************************
MailSlot.SendMess Procedure(String _Mess)
  code
  if self.ms_hMailslot = ms_INVALID_HANDLE_VALUE
     return -1
  end
  if clip(_Mess)=''
     return 0
  end
  self.set(_Mess)
  cc# = wapi_WriteFile(self.ms_hMailslot, |
                       ADDRESS(self.s), |
                       self.len + 1,|
                       self.ms_cbWritten, |
                       0)
  if self.ms_cbWritten=0
     return -wapi_GetLastError()
  end
  return self.ms_cbWritten
!*************************************************************************
MailSlot.SendClose Procedure
  code
  if self.ms_hMailslot = ms_INVALID_HANDLE_VALUE
     return -1
  end
  if self.ms_hMailslot
     wapi_CloseHandle(self.ms_hMailslot)
  end  
  return 0
!*************************************************************************
MailSlot.ReceiveClose Procedure
  code
  return self.SendClose()
!*************************************************************************
MailSlot.ReceiveInit Procedure(String _MailslotName,string _UniqueID,<String _HostName>)
L_HostName string(50)
  CODE
  if omitted(_HostName)
     L_HostName= '.'
  else
     L_HostName= _HostName
  end
  if clip(_UniqueID)=''
     self.ms_NameEvent= RANDOM(100000,999999)
  else
     self.ms_NameEvent= _UniqueID
  end
  self.ms_lpszMailslotName = '\\'&|
       clip(L_HostName) &|
       '\mailslot\' &|
       clip(_MailslotName)
       
  self.ms_hMailslot = wapi_CreateMailslot(self.ms_lpszMailslotName, 0,|
                                          ms_MAILSLOT_WAIT_FOREVER, 0)
  
  if self.ms_hMailslot = ms_INVALID_HANDLE_VALUE
     return wapi_GetLastError()
  end      
  return 0
!*************************************************************************
MailSlot.ReceiveQuery Procedure
  code
  if self.ms_hMailslot = ms_INVALID_HANDLE_VALUE
     return -1
  end  
  self.ms_hEvent = wapi_CreateEventA(0,0,0,self.ms_NameEvent)
  if self.ms_hEvent=0
     return('')
  end
  self.S=''
  self.ms_OV.Offset = 0
  self.ms_OV.OffsetHigh = 0
  self.ms_OV.hEvent = self.ms_hEvent
  
  !---проверим есть ли сообщение  
  self.ms_fReturnCode = wapi_GetMailslotInfo(self.ms_hMailslot, |
                                     self.ms_lpMaxMessageSize, |
                                     self.ms_cbMessages, |
                                     self.ms_cbMsgNumber, |
                                     self.ms_lpReadTimeout)
  if self.ms_fReturnCode=0
     return -2
  end  
  if self.ms_cbMessages = ms_MAILSLOT_NO_MESSAGE
     return ''
  end
  self.ms_cAllMessages = self.ms_cbMsgNumber
  
  !---отыщем все сообщения
  loop WHILE self.ms_cbMsgNumber <> 0
    
       !---выделим Glo память для сообщения
       self.ms_lhMem = wapi_GlobalAlloc(ms_GPTR, |
                                     len(self.ms_achID)*|
                                     size(self.ms_cbMsgNumber) + |
                                     self.ms_cbMsgNumber)
       if self.ms_lhMem=0
          return -3
       end

       !---читаем слот
       self.ms_fReturnCode = wapi_ReadFile(self.ms_hMailslot, |
                                           self.ms_lhMem, |
                                           self.ms_cbMessages, |
                                           self.ms_cbRead, |
                                           ADDRESS(self.ms_OV))
       if ~self.ms_fReturnCode
          cc# = wapi_GlobalFree(self.ms_lhMem)
          return -4
       end

       !---подготовим буфер
       self.SetEx(len(self.ms_achID)*size(self.ms_cbMsgNumber) + |
                           self.ms_cbMsgNumber)
       !---получим значение в буфере                    
       CSL_memcpy(address(self.S),self.ms_lhMem,  |
                           len(self.ms_achID)*size(self.ms_cbMsgNumber) + |
                           self.ms_cbMsgNumber)
    
       cc# = wapi_GlobalFree(self.ms_lhMem)
       
       self.ReceiveGetItem()  !virtual metod
       
       !---проверим есть ли другое сообщение  
       self.ms_fReturnCode = wapi_GetMailslotInfo(self.ms_hMailslot, |
                                     self.ms_lpMaxMessageSize, |
                                     self.ms_cbMessages, |
                                     self.ms_cbMsgNumber, |
                                     self.ms_lpReadTimeout) 

       if self.ms_fReturnCode=0
          return -2
       end  
       if self.ms_cbMessages = ms_MAILSLOT_NO_MESSAGE
          break
       end
  end
  return self.ms_cAllMessages
!*************************************************************************
!*************************************************************************
!*************************************************************************