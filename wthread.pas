unit wthread;
// модуль для работы с доп.потоками Delphi&Lazarus (win&wince&*nix)
// позволяет "общаться" дополнительному и основному потокам посредством очереди сообщений
// (c) wadman 2013-2017, версия от 10.07.2017
//
// использование:
// 1. Создать наследника с объявленными обработчиками сообщений
//  const WM_TEST_PROC = WM_THREAD_BASE + 1;
//  TMyThread = class(TWThread)
//     procedure WMTestProc(var Msg: TThreadMessage); message WM_TEST_PROC;
//  данные процедуры будут выполняться в доп.потоке путем отправки связанного с ними сообщения в поток
//  см. PostToThreadMessage
// 2. Присвоить форме обработчика(ов) события OnThreadReceiveMessage (OnTimeOut - при необходимости)
//
// Для обмена строками рекомендуется использовать функции NewString и FreeString
{$IFDEF FPC}
{$mode delphi}{$H+}
    {$DEFINE WTHREAD_LIBRARY}
{$ELSE}
    {$DEFINE WTHREAD_WINDOWS}
    {$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF UNIX}
    {$LINKLIB libc.so}
{$ENDIF}

// для передачи строк между потоками используется выделенная память
{$DEFINE ALLOC_STRING}

{$I wthread.inc}

interface

uses
    SysUtils,
    {$IFDEF WTHREAD_DEBUG_LOG}
     wlog,
    {$ENDIF}
    Classes
    {$IFNDEF FPC}
    ,Messages
    {$ENDIF}
    ,contnrs
    {$IFDEF WINDOWS}
    ,Windows
    {$ENDIF}
    ,SyncObjs
    ;

const
    INTERNAL_WAIT_TIMEOUT   = 10;

    WM_ANY_MESSAGE      = 0;

    WM_USER             = $400;
    WM_WTHREAD_BASE     = WM_USER + $110;
    WM_WTHREAD_MAX      = WM_USER + $300;

type
    {$IFDEF FPC}
    MessageWord         = DWord;
    {$IFNDEF WINCE}
    THandle             = TEventHandle;
    {$ENDIF}
    {$ELSE}
    MessageWord         = Word;
    {$ENDIF}

    TWThread = class;
    TWEvent = class;
    TEventArray = array of TWEvent;
    THandleArray = array of THandle;

    PThreadMessage = ^TThreadMessage;
    TThreadMessage = record
        Message: MessageWord;
        WParam: Word;
        LParam: NativeInt;
        Sender: TWThread;
    end;

    // event of receiving messages from the thread
    TWThreadReceiveMessage = procedure(Sender: TWThread; var Msg: TThreadMessage) of object;
    // timeout event
    TWThreadTimeOut = procedure(Sender: TWThread) of object;

    TWWaitResult = (
        wwrNone         = 0,
        wwrSignaled     = 1,
        wwrTimeout      = 2,
        wwrAbandoned    = 4,
        wwrError        = 8,
        wwrIOCompletion = 16,
        wwrEvent        = 32
    );

    // Event for correct work in wince

    { TWEvent }

    TWEvent = class(TEvent)
    private
        function GetHandle: THandle;
    protected
        FWHandle: THandle;
        FParent: TWEvent;
        FLastResult: TWWaitResult;
        procedure SetLastResult(const ALastResult: TWWaitResult);
        procedure SetParent(const AParent: TWEvent);
    public
        constructor Create; overload;
        constructor Create(EventAttributes: PSecurityAttributes; AManualReset, InitialState: Boolean; const Name: string); overload;
        destructor Destroy; override;
        procedure SetEvent;
        procedure ResetEvent;
        function WaitFor(const TimeOut: Cardinal): TWWaitResult; overload;
        class function WaitForMultiple(const Events: TEventArray; const Timeout: Cardinal; const WaitAll: Boolean;
            out SignaledEvent: TWEvent): TWWaitResult;
        property Handle: THandle read GetHandle;
        property LastResult: TWWaitResult read FLastResult;
    end;

    { TWThread }

    TWThread = class(TThread)
    private
        FAffinityMask: Cardinal;
        FOwnerWThread: TWThread;
        {$IFDEF WTHREAD_LIBRARY}
        FQueue: TList;
        FSection: TCriticalSection;
        {$ELSE}
        FQueueReady: boolean;
        FQueueMessages: array of TThreadMessage;
        FToolWindow: THandle;
        {$ENDIF}
        FOnThreadReceiveMessage: TWThreadReceiveMessage;
        FOnTimeOut: TWThreadTimeOut;
        FTimeOutIsDirect: boolean;
        FThreadName: string;
        {$IFDEF WTHREAD_DEBUG_LOG}
        FUseDebugLog: boolean;
        function intPostToLog(const Text: string): boolean; overload;
        function intPostToLog(const Text: string; const Level: TLogLevel): boolean; overload;
        {$ENDIF}
        function GetMsFromDateTime(const Value: TDateTime): Cardinal;
        procedure SetTimeOut(const Value: Cardinal);
        procedure FreeQueue;
        function GetAffinityMask: Cardinal;
        procedure SetAffinityMask(const Value: Cardinal);
        procedure SetOnThreadReceiveMessage(const Value: TWThreadReceiveMessage);
        procedure SetOnTimeOut(const Value: TWThreadTimeOut);
        function GetTerminated: boolean;
        {$IFDEF WTHREAD_LIBRARY}
        procedure CreateGUIThread;
        procedure FreeGUIThread;
        {$ENDIF}
    protected
        FHandleEvent: TWEvent;
        FTimeOut: Cardinal;
        {$IFDEF WTHREAD_WINDOWS}
        FWindowHandle: THandle;
        procedure WWindowProc(var Msg: TMessage);
        procedure GetWindow;
        procedure FreeWindow;
        {$ENDIF}
        procedure ResetTimeout;
        procedure DoThreadReceiveMessage(const Msg: Word; const WParam: Word; const LParam: NativeInt);
        procedure DoTimeout;
        procedure Execute; override;
        function GetTimeOut: Cardinal; virtual;
            // sending a message from this thread to another thread (see. OwnerWThread)
        procedure PostMessageToWThread(const Msg: Word; const WParam: Word; const LParam: NativeInt);
            // procedure for clearing memory in messages that are stored in the queue before stopping the thread
        procedure FreeMessage(const Msg: Word; const WParam: Word; const LParam: NativeInt); virtual;
            // см TimeOutIsDirect, при true - перекрыть следующую процедуру
        procedure DirectTimeOut; virtual;
        // procedures that are called in the context of the thread (at the beginning and at the end)
        procedure InitThread; virtual;
        procedure DoneThread; virtual;
    public
        constructor Create(const AOwnerThread: TWThread; const AThreadName: string
        {$IFDEF WTHREAD_DEBUG_LOG}
                ; AUseDebugLog: Boolean
        {$ENDIF}
            ); overload;
        constructor Create(const AOwnerThread: TWThread; const AThreadName: string; CreateSuspended
        {$IFDEF WTHREAD_DEBUG_LOG}
                , AUseDebugLog
        {$ENDIF}
                : Boolean); overload;
        constructor Create(CreateSuspended: boolean); overload;
        constructor Create; overload;
        procedure AfterConstruction; override;
            // checking for messages in the queue
        function HaveMessages(const Msg: Word = WM_ANY_MESSAGE): boolean;
            // checking for execution in this thread
        function ItsMe: boolean;
        procedure WaitMs(const Ms: cardinal);
        class function ProcessorCount: integer;
        destructor Destroy; override;
            // send a message to this thread
        function PostToThreadMessage(const Msg: Word; const WParam: Word; const LParam: NativeInt): Boolean;
            // send a message from this thread for processing in another or main thread (see OnThreadReceiveMessage)
        procedure PostMessageFromThread(const Msg: Word; const WParam: Word; const LParam: NativeInt);
            // stop thread
        procedure Terminate;
            // event of receiving a message from this thread
        property OnThreadReceiveMessage: TWThreadReceiveMessage read FOnThreadReceiveMessage write SetOnThreadReceiveMessage;
            // thread to which messages are sent by the PostMessageToWThread procedure
        property OwnerWThread: TWThread read FOwnerWThread; // write FOwnerWThread;
            // event occurring when the timeout interval is exceeded
        property OnTimeOut: TWThreadTimeOut read FOnTimeOut write SetOnTimeOut;
            // wait interval, default is infinite
        property TimeOut: Cardinal read GetTimeOut write SetTimeOut default INFINITE;
            // where the timeout handler should be called: in this thread or the main thread
        property TimeOutIsDirect: boolean read FTimeOutIsDirect write FTimeOutIsDirect default false;
            // affinity mask is a bit mask indicating what processor(s) a thread should be run
        property AffinityMask: Cardinal read GetAffinityMask write SetAffinityMask;
        {$IFDEF WTHREAD_DEBUG_LOG}
        property UseDebugLog: boolean read FUseDebugLog write FUseDebugLog default false;
        {$ENDIF}
        property ThreadName: string read FThreadName;
        property Terminated: boolean read GetTerminated;
    end;

    // preparation of a string for transfer between streams with allocation of memory
function NewString(const Text: string): NativeInt;

    // freeing memory and returning a string
function FreeString(var P: NativeInt): String;

implementation

{$IFDEF FPC}
uses
    {$IFDEF UNIX}
    UTF8Process,
    {$ENDIF}
    LCLIntf
    ;
{$ENDIF}

const
    WM_WINTERNAL_BASE   = WM_USER+$100;
    WM_WTIMEOUT         = WM_WINTERNAL_BASE+1;
    {$IFDEF WTHREAD_WINDOWS}
    WM_WTHREAD_READY    = WM_WINTERNAL_BASE+2;
    {$ENDIF}

    SizeOfChar          = SizeOf(Char);

    // константы из DateUtils
    OneMillisecond      = 1 / MSecsPerDay;

{$IFDEF ALLOC_STRING}
{$IFDEF WTHREAD_WINDOWS}
// для передачи строки между программами
function GlobalNewString(const Text: string): NativeInt;
var l: Integer;
    p: pointer;
begin
    l := Length(Text)*SizeOfChar;
    if L > SizeOfChar then begin
        Result := GlobalAlloc(GHND, l+SizeOfChar);
        if LongBool(Result) then begin
            {$HINTS OFF}
            p := GlobalLock(Result);
            {$HINTS ON}
            Move(Pointer(Text)^, Pointer(p)^, l);
            GlobalUnlock(Result);
        end
    end else
        Result := 0;
end;

// для передачи строки между программами
function GlobalFreeString(var P: NativeInt): String;
var ps: pointer;
begin
    if LongBool(P) then begin
        {$HINTS OFF}
        ps := GlobalLock(P);
        {$HINTS ON}
        SetLength(Result, Length(PChar(ps)));
        Move(Pointer(ps)^, Pointer(Result)^, Length(Result)*SizeOfChar);
        GlobalUnlock(P);
        P := GlobalFree(P);
    end;
end;
{$ENDIF}

// для передачи строки в пределах одной программы
function FreeString(var P: NativeInt): String;
begin
    {$HINTS OFF}
    if LongBool(P) then begin
        SetLength(Result, Length(PChar(P)));
        Move(Pointer(P)^, Pointer(Result)^, Length(Result)*SizeOfChar);
        {$IFDEF WTHREAD_WINDOWS}
        P := LocalFree(HLOCAL(P));
        {$ELSE}
        {$IFDEF FPC}
        P := FreeMem(Pointer(P));
        {$ELSE}
        FreeMem(Pointer(P));
        P := 0;
        {$ENDIF}
        {$ENDIF}
    end;
end;

// для передачи строки в пределах одной программы
function NewString(const Text: string): NativeInt;
var l: Integer;
begin
    l := Length(Text)*SizeOfChar;
    if L >= SizeOfChar then begin
        {$IFDEF WTHREAD_WINDOWS}
        Result := LocalAlloc(LPTR, l+SizeOfChar);
        {$ELSE}
        Pointer(Result) := AllocMem(l+SizeOfChar);
        {$ENDIF}
        if LongBool(Result) then
            Move(Pointer(Text)^, Pointer(Result)^, l);
        {$HINTS ON}
    end else
        Result := 0;
end;
{$ELSE}
function NewString(const Text: string): NativeInt;
begin
    Result := 0;
    string(Result) := Text;
end;

function FreeString(var P: NativeInt): String;
begin
    Result := '';
    NativeInt(Result) := P;
end;
{$ENDIF}

{$IFDEF UNIX}
function sched_getaffinity(pid: PtrUInt; cpusetsize: LongInt; cpuset: Pointer): LongInt; cdecl; external;
function sched_setaffinity(pid: PtrUInt; cpusetsize: LongInt; cpuset: Pointer): LongInt; cdecl; external;
function pthread_setaffinity_np(pid: PtrUInt; cpusetsize: LongInt; cpuset: Pointer): LongInt; cdecl; external;
function pthread_getaffinity_np(pid: PtrUInt; cpusetsize: LongInt; cpuset: Pointer) : LongInt; cdecl; external;
{$ENDIF}

constructor TWEvent.Create;
begin
    Create(nil, true, false, '');
end;

constructor TWEvent.Create(EventAttributes: PSecurityAttributes; AManualReset, InitialState: Boolean; const Name: string);
begin
    {$IFDEF WINCE}
    FWHandle := CreateEvent(EventAttributes, AManualReset, InitialState, Name);
    {$ELSE}
    inherited Create(EventAttributes, AManualReset, InitialState, Name);
    {$ENDIF}
    FLastResult := wwrNone;
end;

destructor TWEvent.Destroy;
begin
    {$IFDEF WINCE}
    CloseHandle(FWHandle);
    {$ELSE}
    inherited;
    {$ENDIF}
end;

function TWEvent.GetHandle: THandle;
begin
    {$IFDEF WINCE}
    result := FWHandle;
    {$ELSE}
    result := inherited Handle;
    {$ENDIF}
end;

procedure TWEvent.SetLastResult(const ALastResult: TWWaitResult);
begin
    if ALastResult <> wwrTimeout then begin
        FLastResult := ALastResult;
        if Assigned(FParent) then begin
            FParent.SetEvent;
        end;
    end;
end;

procedure TWEvent.SetParent(const AParent: TWEvent);
begin
    FParent := AParent;
end;

procedure TWEvent.ResetEvent;
begin
    {$IFDEF WINCE}
    Windows.ResetEvent(FWHandle);
    {$ELSE}
    inherited ResetEvent;
    {$ENDIF}
    FLastResult := wwrNone;
end;

procedure TWEvent.SetEvent;
begin
    SetLastResult(wwrSignaled);
    {$IFDEF WINCE}
    Windows.SetEvent(FWHandle);
    {$ELSE}
    inherited SetEvent;
    {$ENDIF}
end;

function TWEvent.WaitFor(const TimeOut: Cardinal): TWWaitResult;
{$IFDEF WINCE}
var dw: DWord;
{$ENDIF}
begin
{$IFDEF WINCE}
    dw := WaitForSingleObject(FWHandle, TimeOut);
    case dw of
        WAIT_ABANDONED: result := wwrAbandoned;
        WAIT_OBJECT_0: result := wwrSignaled;
        WAIT_TIMEOUT: result := wwrTimeout;
        WAIT_IO_COMPLETION: result := wwrIOCompletion;
    else
        Result := wwrError;
{$ELSE}
    case inherited WaitFor(TimeOut) of
        wrSignaled: result := wwrSignaled;
        wrTimeout: result := wwrTimeout;
        wrAbandoned: result := wwrAbandoned;
        wrError: result := wwrError;
    else
        result := wwrIOCompletion;
    end;
{$ENDIF}
    SetLastResult(result);
end;

class function TWEvent.WaitForMultiple(const Events: TEventArray; const Timeout: Cardinal; const WaitAll: Boolean;
    out SignaledEvent: TWEvent): TWWaitResult;

type
    TBooleanArray = array of Boolean;

var OwnEvent: TWEvent;
    FiredEvents: TBooleanArray;
    fired: integer;
    ticks: DWORD;
    Len: integer;
    VTimeout: Int64;

    procedure SetParents(const AParent: TWEvent);
    var i: integer;
    begin
        for i := Low(Events) to High(Events) do begin
            Events[i].SetParent(AParent);
            FiredEvents[i] := false;
        end;
    end;

    function GetFiredCount: integer;
    var i: integer;
    begin
        result := 0;
        for i := Low(Events) to High(Events) do if not FiredEvents[i] then begin
            if Events[i].WaitFor(0) in [wwrSignaled, wwrIOCompletion] then begin
                SignaledEvent := Events[i];
                FiredEvents[i] := true;
            end;
        end;
        for i := Low(FiredEvents) to High(FiredEvents) do
            if FiredEvents[i] then inc(Result);
    end;

begin
    SignaledEvent := nil;
    Result := wwrNone;
    Len := Length(Events);
    if Len = 0 then exit;
    SetLength(FiredEvents, Len);
    VTimeout := Timeout;
    fired := 0;
    try
        OwnEvent := TWEvent.Create(nil, false, false, 'OwnerEvent');
        SetParents(OwnEvent);
        if VTimeout <> INFINITE then begin
            ticks := GetTickCount64;
        end;
        while ((fired <> Len) and WaitAll) or ((fired = 0) and (not WaitAll)) do begin
            Result := OwnEvent.WaitFor(VTimeout);
            fired := GetFiredCount;
            if VTimeout <> INFINITE then begin
                VTimeout := VTimeout - (GetTickCount64 - ticks);
                ticks := GetTickCount64;
                if (VTimeout <= 0) then begin
                    break;
                end;
            end;
        end;
        SetParents(nil);
    finally
        OwnEvent.Free;
    end;
    if Assigned(SignaledEvent) and (((fired = Len) and WaitAll) or ((fired <> 0) and (not WaitAll))) then begin
        Result := SignaledEvent.LastResult;
    end else begin
        Result := wwrTimeout;
    end;
end;

{$IFDEF WTHREAD_LIBRARY}
type

    { TGUIThread }

    TGUIThread = class(TThread)
    private
        FMessageEvent: TWEvent;
        FQueue: TList;
        FSection: TCriticalSection;
        FCurrentMessage: TThreadMessage;
        procedure FreeQueue;
        function GetMessageCount: integer;
    protected
        procedure Execute; override;
        procedure CallGUIThread;
    public
        constructor Create; overload;
        destructor Destroy; override;
        procedure PostMessage(const Sender: TWThread; const Message: DWord; const WParam: Word; const LParam: NativeInt);
        procedure Terminate;
        property MessageCount: integer read GetMessageCount;
    end;

var
   FGUIThread: TGUIThread;

{ TGUIThread }

procedure TGUIThread.FreeQueue;
var PMsg: PThreadMessage;
begin
    while FQueue.Count > 0 do begin
        PMsg := FQueue[0];
        FQueue.Delete(0);
        //FOwner.FreeMessage(PMsg^.Message, PMsg^.WParam, PMsg^.LParam);
        FreeMem(PMsg);
    end;
end;

function TGUIThread.GetMessageCount: integer;
begin
    result := FQueue.Count;
end;

procedure TGUIThread.Execute;
var Message: PThreadMessage;
    wr: TWWaitResult;
begin
    while not Terminated do begin
        wr := FMessageEvent.WaitFor(INFINITE);
        if (not Terminated) and (wr = wwrSignaled) then begin
            while FQueue.Count > 0 do begin
                FSection.Enter;
                Message := FQueue[0];
                FQueue.Delete(0);
                FSection.Leave;
                FCurrentMessage := Message^;
                FreeMem(Message);
                if  ((FCurrentMessage.Message = WM_WTIMEOUT) or ((FCurrentMessage.Message >= WM_WTHREAD_BASE)
                    and(FCurrentMessage.Message <= WM_WTHREAD_MAX)))and(not Terminated) then
                        Synchronize(CallGUIThread);
            end;
        end;
    end;
    FreeQueue;
end;

procedure TGUIThread.CallGUIThread;
begin
    if Assigned(FCurrentMessage.Sender) then begin
        if FCurrentMessage.Message = WM_WTIMEOUT then begin
            FCurrentMessage
              .Sender
              .DoTimeout;
        end else begin
            FCurrentMessage
              .Sender
              .DoThreadReceiveMessage(FCurrentMessage.Message, FCurrentMessage.WParam, FCurrentMessage.LParam);
        end;
    end;
end;

constructor TGUIThread.Create;
begin
    inherited Create(False);
    FMessageEvent := TWEvent.Create(nil, false, false, 'GUIThreadEvent');
    FQueue := TList.Create;
    FSection := TCriticalSection.Create;
    FreeOnTerminate := true;
end;

destructor TGUIThread.Destroy;
begin
    FSection.Free;
    FQueue.Free;
    FMessageEvent.Free;
    inherited Destroy;
end;

procedure TGUIThread.PostMessage(const Sender: TWThread; const Message: DWord; const WParam: Word; const LParam: NativeInt);
var Msg: PThreadMessage;
begin
    if Terminated then exit;
    GetMem(Msg, SizeOf(TThreadMessage));
    Msg^.Message := Message;
    Msg^.WParam := WParam;
    Msg^.LParam := LParam;
    Msg^.Sender := Sender;
    FSection.Enter;
    FQueue.Add(Msg);
    FSection.Leave;
    FMessageEvent.SetEvent;
end;

procedure TGUIThread.Terminate;
begin
    inherited Terminate;
    FMessageEvent.SetEvent;
end;

{$ENDIF}

{ TWThread }

constructor TWThread.Create(const AOwnerThread: TWThread; const AThreadName: string; CreateSuspended
        {$IFDEF WTHREAD_DEBUG_LOG}
        , AUseDebugLog
        {$ENDIF}
        : Boolean);
begin
    inherited Create(CreateSuspended);
    FAffinityMask := 0;
    FOwnerWThread := AOwnerThread;
    {$IFDEF WTHREAD_DEBUG_LOG}
    FUseDebugLog := AUseDebugLog;
    {$ENDIF}
    if Length(AThreadName) = 0 then
        FThreadName := 'Thread'
    else
        FThreadName := AThreadName;
    FHandleEvent := TWEvent.Create(nil, false, false, FThreadName+'HandleEvent');
    {$IFDEF WTHREAD_LIBRARY}
    FQueue := TList.Create;
    FSection := TCriticalSection.Create;
    {$ELSE}
    GetWindow;
    FWindowHandle := 0;
    FQueueReady := False;
    {$ENDIF}
    FTimeOut := INFINITE;
    FTimeOutIsDirect := False;
end;

constructor TWThread.Create(const AOwnerThread: TWThread; const AThreadName: string
    {$IFDEF WTHREAD_DEBUG_LOG}
    ; AUseDebugLog: Boolean
    {$ENDIF}
    );
begin
    Create(AOwnerThread, AThreadName, false
        {$IFDEF WTHREAD_DEBUG_LOG}
        , AUseDebugLog
        {$ENDIF}
        );
end;

constructor TWThread.Create(CreateSuspended: boolean);
begin
    Create(nil, '', CreateSuspended
    {$IFDEF WTHREAD_DEBUG_LOG}
    , false
    {$ENDIF}
    );
end;

constructor TWThread.Create;
begin
    Create(false);
end;

procedure TWThread.AfterConstruction;
begin
    inherited AfterConstruction;
//    AffinityMask := AffinityMask;
end;

class function TWThread.ProcessorCount: integer;
{$IFDEF MSWINDOWS}
var sys: SYSTEM_INFO;
begin
    GetSystemInfo(sys);
    result := sys.dwNumberOfProcessors
{$ELSE}
begin
    {$IFDEF UNIX}
    result := GetSystemThreadCount;
    {$ELSE}
    result := inherited ProcessorCount;
    {$ENDIF}
{$ENDIF}
end;

destructor TWThread.Destroy;
begin
    {$IFDEF WTHREAD_LIBRARY}
    FreeGUIThread;
    FQueue.Free;
    FSection.Free;
    {$ELSE}
    SetLength(FQueueMessages, 0);
    FreeWindow;
    //SleepEx(1, false);
    //result := WaitForSingleObject(Handle, 10000) <> WAIT_TIMEOUT;
    {$ENDIF}
    FHandleEvent.Free;
    inherited;
end;

procedure TWThread.DirectTimeOut;
begin
    // override
end;

// procedure that executes in the context of this thread before running the message queue.
procedure TWThread.InitThread;
begin
    // empty
end;

function TWThread.ItsMe: boolean;
begin
{$IFDEF WINDOWS}
    result := GetCurrentThreadId = ThreadID;
{$ELSE}
    result := GetCurrentThreadId = ThreadID;
{$ENDIF}
end;

procedure TWThread.WaitMs(const Ms: cardinal);
var curms: integer;
begin
    curms := ms;
    while (not Terminated) and (curms > 0) do begin
        Sleep(INTERNAL_WAIT_TIMEOUT);
        dec(curms, INTERNAL_WAIT_TIMEOUT);
    end;
end;

function TWThread.HaveMessages(const Msg: Word = WM_ANY_MESSAGE): boolean;
{$IFDEF WTHREAD_LIBRARY}
var i: integer;
begin
    result := FQueue.Count > 0;
    if Msg = WM_ANY_MESSAGE then
        exit
    else begin
        if result then begin
            result := false;
            for i := 0 to FQueue.Count-1 do
                if PThreadMessage(FQueue[i])^.Message = Msg then begin
                    result := true;
                    break;
                end;
        end;
    end
{$ELSE}
var
    m: TMsg;
begin
    result := PeekMessage(m, 0, Msg, Msg, PM_NOREMOVE);
{$ENDIF}
end;

// procedure that executes in the context of this thread after the message queue ends
procedure TWThread.DoneThread;
begin
    // empty
end;

procedure TWThread.DoThreadReceiveMessage(const Msg: Word; const WParam: Word; const LParam: NativeInt);
var ThreadMsg: TThreadMessage;
begin
    if Assigned(FOnThreadReceiveMessage) then begin
        ThreadMsg.Message := Msg;
        ThreadMsg.WParam := WParam;
        ThreadMsg.LParam := LParam;
        FOnThreadReceiveMessage(Self, ThreadMsg);
    end;
end;

procedure TWThread.DoTimeout;
begin
    if Assigned(FOnTimeOut) then
        FOnTimeOut(Self);
end;

// sending message to this thread
function TWThread.PostToThreadMessage(const Msg: Word; const WParam: Word;
    const LParam: NativeInt): Boolean;
{$IFDEF WTHREAD_LIBRARY}
var PMsg: PThreadMessage;
begin
    result := false;
    if Terminated then exit;
    GetMem(PMsg, SizeOf(TThreadMessage));
    PMsg^.Message := Msg;
    PMsg^.WParam := WParam;
    PMsg^.LParam := LParam;
    FSection.Enter;
    FQueue.Add(PMsg);
    FSection.Leave;
    {$IFDEF WTHREAD_DEBUG_LOG}
    intPostToLog(Format('send to me msg: %d (0x%0:x).', [Msg]), WLL_EXTRA);
    {$ENDIF}
    FHandleEvent.SetEvent;
    result := true;
{$ELSE}
begin
    result := false;
    if Terminated then exit;
    if FQueueReady then begin
        // если очередь сообщений создана и поток инициализирован, то сразу отправляем
        result := (not Suspended)and(PostThreadMessage(ThreadID, Msg, wParam, lParam));
        {$IFDEF WTHREAD_DEBUG_LOG}
        intPostToLog(Format('send to me msg: %d (0x%0:x).', [Msg]), WLL_EXTRA);
        {$ENDIF}
    end else begin
        // иначе кэшируем
        SetLength(FQueueMessages, Length(FQueueMessages)+1);
        FQueueMessages[High(FQueueMessages)].Message := Msg;
        FQueueMessages[High(FQueueMessages)].WParam := WParam;
        FQueueMessages[High(FQueueMessages)].LParam := LParam;
        {$IFDEF WTHREAD_DEBUG_LOG}
        intPostToLog(Format('cached to me msg: %d (0x%0:x).', [Msg]), WLL_EXTRA);
        {$ENDIF}
        result := True;
    end;
{$ENDIF}
end;

{$IFDEF WTHREAD_DEBUG_LOG}
{$B-}
function TWThread.intPostToLog(const Text: string): boolean;
begin
    result := intPostToLog(Text, WLL_NORMAL);
end;

function TWThread.intPostToLog(const Text: string; const Level: TLogLevel): boolean;
begin
    result := FUseDebugLog and PostToLog(Format('%s(%d) : %s', [FThreadName, Handle, Text]), Level);
end;
{$ENDIF}

function TWThread.GetAffinityMask: Cardinal;
{$IFDEF UNIX}
var
    cpuset: PtrUInt;
begin
    //res := pthread_getaffinity_np(ThreadID, 4, @cpuset);
    if pthread_getaffinity_np(ThreadID, SizeOf(cpuset), @cpuset) = 0 then begin
        FAffinityMask := cpuset;
    end else begin
        FAffinityMask := 0;
    end;
{$ElSE}
begin
{$ENDIF}
    result := FAffinityMask;
end;

function TWThread.GetMsFromDateTime(const Value: TDateTime): Cardinal;
begin
    Result := Round((Now - Value) / OneMillisecond);
end;

procedure TWThread.SetAffinityMask(const Value: Cardinal);
{$IFDEF UNIX}
var
    cpuset: PtrUInt;
{$ENDIF}
begin
    if Value <> 0 then begin
{$IFDEF WINDOWS}
        {$IFDEF WINCE}
        {$Message WARN 'WTHREAD: No AffinityMask support for WinCE.'}
        FAffinityMask := 0;
        {$ELSE}
        if not LongBool(SetThreadAffinityMask(Handle, Value)) then
            FAffinityMask := 0
        else
            FAffinityMask := Value;
        {$ENDIF}
{$ENDIF}
{$IFDEF UNIX}
        cpuset := Value;
        if pthread_setaffinity_np(ThreadID, SizeOf(cpuset), @cpuset) = 0 then
            FAffinityMask := Value
        else
            FAffinityMask := 0;
{$ENDIF}
    end;
end;

procedure TWThread.SetOnThreadReceiveMessage(const Value: TWThreadReceiveMessage);
begin
    FOnThreadReceiveMessage := Value;
{$IFDEF WTHREAD_LIBRARY}
    if (Assigned(Value) and (not Assigned(FGUIThread))) then begin
        CreateGUIThread;
    end else if (not Assigned(Value)) and (Assigned(FGUIThread)) then begin
        FreeGUIThread;
    end;
{$ENDIF}
end;

procedure TWThread.SetOnTimeOut(const Value: TWThreadTimeOut);
begin
    FOnTimeOut := Value;
{$IFDEF WTHREAD_LIBRARY}
    if (Assigned(Value) and (not Assigned(FGUIThread))) then begin
        CreateGUIThread;
    end else if (not Assigned(Value)) and (Assigned(FGUIThread)) then begin
        FreeGUIThread;
    end;
{$ENDIF}
end;

procedure TWThread.SetTimeOut(const Value: Cardinal);
begin
    if Terminated then exit;
{$IFDEF WTHREAD_LIBRARY}
    if FTimeOut <> Value then begin
        if (Value = 0) then
            FTimeOut := INFINITE
        else
            FTimeOut := Value;
        ResetTimeout;
    end;
{$ELSE}
    if FTimeOut <> Value then begin
        if (Value = 0) then
            FTimeOut := INFINITE
        else
            FTimeOut := Value;
        ResetTimeout;
    end;
{$ENDIF}
end;

procedure TWThread.FreeQueue;
{$IFDEF WTHREAD_LIBRARY}
var PMsg: PThreadMessage;
begin
    FSection.Enter;
    while FQueue.Count > 0 do begin
        PMsg := FQueue[0];
        FQueue.Delete(0);
        FreeMessage(PMsg^.Message, PMsg^.WParam, PMsg^.LParam);
        FreeMem(PMsg);
    end;
    FSection.Leave;
{$ELSE}
begin
{$ENDIF}
end;

procedure TWThread.Execute;
var
    internalTimeout, ms: Cardinal;
    busy: TDateTime;
{$IFDEF WTHREAD_LIBRARY}
    WR: TWWaitResult;
    Message: PThreadMessage;
begin
    {$IFDEF WTHREAD_DEBUG_LOG}
    intPostToLog('started.');
    {$ENDIF}
    InitThread;
    internalTimeout := TimeOut;
    busy := 0;
    {$IFDEF WTHREAD_DEBUG_LOG}
    intPostToLog('main loop started.', WLL_EXTRA);
    {$ENDIF}
    while not Terminated do begin
        WR := FHandleEvent.WaitFor(internalTimeout);
        busy := Now;
        if not Terminated then
            case WR of
                wwrSignaled: begin
                    {$IFDEF WTHREAD_DEBUG_LOG}
                    intPostToLog(Format('got signal (queue cnt: %d).', [FQueue.Count]), WLL_EXTRA);
                    {$ENDIF}
                    while FQueue.Count > 0 do begin
                        FSection.Enter;
                        Message := FQueue[0];
                        FQueue.Delete(0);
                        FSection.Leave;
                        if (Message^.message >= WM_WTHREAD_BASE)
                            and (Message^.Message <= WM_WTHREAD_MAX) then begin
                                {$IFDEF WTHREAD_DEBUG_LOG}
                                intPostToLog(Format('dispatch msg %d (0x%0:x).', [Message^.Message]), WLL_EXTRA);
                                {$ENDIF}
                                try
                                    Dispatch(message^);
                                except on E: Exception do
                                    {$IFDEF WTHREAD_DEBUG_LOG}
                                    intPostToLog(Format('Exception dispatch msg %d (0x%0:x): %s', [Message.Message, E.ToString]), WLL_EXTRA);
                                    {$ENDIF}
                                end;
                            end else if (Message^.message >= WM_WINTERNAL_BASE)
                                and (Message^.Message < WM_WTHREAD_BASE) then begin // внутренние сообщения
                            end;
                        FreeMem(Message);
                    end;
                end;
                wwrTimeout: begin
                    internalTimeout := TimeOut;
                    if FTimeOutIsDirect then begin
                        {$IFDEF WTHREAD_DEBUG_LOG}
                        intPostToLog('DirectTimeOut.', WLL_EXTRA);
                        {$ENDIF}
                        try
                            DirectTimeOut;
                        except on E: Exception do
                            //raise E;
                            {$IFDEF WTHREAD_DEBUG_LOG}
                            intPostToLog(Format('Exception on DirectTimeout: %s', [E.ToString]), WLL_EXTRA);
                            {$ENDIF}
                        end;
                    end else begin
                        {$IFDEF WTHREAD_DEBUG_LOG}
                        intPostToLog('Sent TimeOut.', WLL_EXTRA);
                        {$ENDIF}
                        PostMessageFromThread(WM_WTIMEOUT, 0, 0);
                        // сообщение не занимает много времени, на точность не влияет
                        //busy := 0;
                    end;
                end;
            end;
        // корректируем интервал таймаута на время выполнения кода выше
        if (busy <> 0) then begin
            ms := GetMsFromDateTime(busy);
            if (ms > TimeOut) then
                internalTimeout := 0
            else
                internalTimeout := TimeOut - ms;
        end else begin
            internalTimeout := TimeOut;
        end;
    end;
{$ELSE}
    message: TThreadMessage;
    HandlesWaitFor: array[0..0] of THandle;
    dwHandleSignaled: DWORD;
    msg: TMsg;

begin
    // следующая строка должна быть всегда первой в процедуре, т.к. запускает очередь сообщений для потока
    {$HINTS OFF}
    PeekMessage(msg, THandle(-1), WM_USER, WM_USER, PM_NOREMOVE);
    {$HINTS ON}
    {$IFDEF WTHREAD_DEBUG_LOG}
    intPostToLog('main loop started.', WLL_EXTRA);
    {$ENDIF}
    internalTimeout := TimeOut;
    busy := 0;
    InitThread;
    HandlesWaitFor[0] := FHandleEvent.Handle;
    PostMessageFromThread(WM_WTHREAD_READY, 0, 0);

    while not Terminated do begin
        if (not PeekMessage(msg, 0, 0, 0, PM_REMOVE)) then begin
            dwHandleSignaled := MsgWaitForMultipleObjects(1, HandlesWaitFor, false, internalTimeout, QS_ALLEVENTS);
            busy := Now;
            if not Terminated then case dwHandleSignaled of
                WAIT_FAILED: begin
                    raise Exception.CreateFmt('%s: WAIT_FAILED', [ThreadName]);
                    Terminate;
                    break;
                end;
                WAIT_OBJECT_0: begin // выставлено событие остановки потока
                    if not FreeOnTerminate then
                        Terminate;
                    break;
                end;
                WAIT_OBJECT_0+1: begin
                    Continue;
                end;
                WAIT_TIMEOUT: begin
                    if FTimeOutIsDirect then begin
                        {$IFDEF WTHREAD_DEBUG_LOG}
                        intPostToLog('DirectTimeOut.', WLL_EXTRA);
                        {$ENDIF}
                        try
                            DirectTimeOut;
                        except on E: Exception do
                            //raise E;
                            {$IFDEF WTHREAD_DEBUG_LOG}
                            intPostToLog(Format('Exception on DirectTimeout: %s', [E.ToString]), WLL_EXTRA);
                            {$ENDIF}
                        end;
                    end else begin
                        {$IFDEF WTHREAD_DEBUG_LOG}
                        intPostToLog('Sent TimeOut.', WLL_EXTRA);
                        {$ENDIF}
                        PostMessageFromThread(WM_WTIMEOUT, 0, 0);
                        // сообщение не занимает много времени, на точность не влияет
                        busy := 0;
                    end;
                    // выставляем новый интервал за минусом времени выполнения процедуры таймаута
                    if (busy <> 0) then begin
                        ms := GetMsFromDateTime(busy);
                        if (ms > TimeOut) then
                            internalTimeout := 0
                        else
                            internalTimeout := TimeOut - ms;
                    end else begin
                        internalTimeout := TimeOut;
                    end;
                    Continue;
                end;
            end;
        end;

        if Terminated then break;

        if LongBool(msg.hwnd) then begin
            TranslateMessage(msg);
            DispatchMessage(msg);
        end else case msg.message of
            WM_WTIMEOUT: begin
                // изменен таймаут
                FTimeOut := msg.wParam;
                busy := 0;
            end;
            WM_WTHREAD_BASE..WM_WTHREAD_MAX: begin
                message.Message := Msg.message;
                message.WParam := Msg.wParam;
                message.LParam := Msg.lParam;
                {$IFDEF WTHREAD_DEBUG_LOG}
                intPostToLog(Format('dispatch msg %d (0x%0:x).', [Message.Message]), WLL_EXTRA);
                {$ENDIF}
                // ищем и вызываем procedure of object message
                try
                    Dispatch(message);
                except on E: Exception do
                    {$IFDEF WTHREAD_DEBUG_LOG}
                    intPostToLog(Format('Exception dispatch msg %d (0x%0:x): %s', [Message.Message, E.ToString]), WLL_EXTRA);
                    {$ENDIF}
                end;
            end;
        end;

        // корректируем интервал таймаута на время выполнения кода выше
        if (busy <> 0) then begin
            ms := GetMsFromDateTime(busy);
            if (ms > TimeOut) then
                internalTimeout := 0
            else
                internalTimeout := TimeOut - ms;
        end else begin
            internalTimeout := TimeOut;
        end;
    end;
{$ENDIF}
{$IFDEF WTHREAD_DEBUG_LOG}
    intPostToLog('main loop stoped.', WLL_EXTRA);
{$ENDIF}
    FreeQueue;
    DoneThread;
{$IFDEF WTHREAD_DEBUG_LOG}
    intPostToLog('stoped.', WLL_EXTRA);
{$ENDIF}
end;

// отправка сообщения из этого потока для вызова обработчика OnThreadReceiveMessage
procedure TWThread.PostMessageFromThread(const Msg: Word; const WParam: Word; const LParam: NativeInt);
begin
    if Terminated then exit;
{$IFDEF WTHREAD_LIBRARY}
    if Assigned(FGUIThread) then begin
        TGUIThread(FGUIThread).PostMessage(Self, Msg, WParam, LParam);
    {$IFDEF WTHREAD_DEBUG_LOG}
        intPostToLog(Format('send to receiver msg: %d (0x%0:x).', [Msg]), WLL_EXTRA);
    {$ENDIF}
    end
{$ELSE}
    if FToolWindow <> 0 then begin
        PostMessage(FToolWindow, Msg, WParam, LParam);
        {$IFDEF WTHREAD_DEBUG_LOG}
        intPostToLog(Format('send to receiver msg: %d (0x%0:x).', [Msg]), WLL_EXTRA);
        {$ENDIF}
    end
{$ENDIF}
    {$IFDEF WTHREAD_DEBUG_LOG}
    else
        intPostToLog(Format('cant send to receiver msg: %d (0x%0:x).', [Msg]), WLL_EXTRA);
    {$ENDIF}
end;

procedure TWThread.PostMessageToWThread(const Msg: Word; const WParam: Word; const LParam: NativeInt);
begin
    if Terminated then exit;
    if Assigned(FOwnerWThread) then begin
        FOwnerWThread.PostToThreadMessage(Msg, WParam, LParam);
   {$IFDEF WTHREAD_DEBUG_LOG}
        intPostToLog(Format('send to %s(%d) msg: %d (0x%2:x).', [FOwnerWThread.FThreadName, FOwnerWThread.Handle, Msg]), WLL_EXTRA);
    end else begin
        intPostToLog(Format('cant send to other wthread msg: %d (0x%0:x).', [Msg]), WLL_EXTRA);
    {$ENDIF}
    end;
end;

procedure TWThread.Terminate;
begin
{$IFDEF WTHREAD_DEBUG_LOG}
    intPostToLog('stop signal.');
{$ENDIF}
    inherited Terminate;
    FHandleEvent.SetEvent;
end;

function TWThread.GetTerminated: boolean;
begin
    Result := inherited Terminated;
end;

function TWThread.GetTimeOut: Cardinal;
begin
    Result := FTimeOut;
end;

{$IFDEF WTHREAD_LIBRARY}
procedure TWThread.CreateGUIThread;
begin
    if (not Assigned(FGUIThread)) and (not Terminated) then begin
        FGUIThread := TGUIThread.Create;
    end;
end;

procedure TWThread.FreeGUIThread;
begin
    //if Assigned(FGUIThread) then begin
    //    TGUIThread(FGUIThread).Terminate;
    //    FGUIThread := nil;
    //end;
end;

{$ENDIF}

procedure TWThread.ResetTimeout;
begin
    if Terminated then exit;
{$IFDEF WTHREAD_LIBRARY}
    if not Suspended then
        FHandleEvent.SetEvent;
{$ELSE}
    if (not Suspended)and(not ItsMe) then
        PostThreadMessage(ThreadID, WM_WTIMEOUT, FTimeout, 0);
{$ENDIF}
end;

procedure TWThread.FreeMessage(const Msg: Word; const WParam: Word; const LParam: NativeInt);
begin
    // эту процедуру необходимо перекрыть для освобождения памяти, которая была выделена и
    // передана через очередь в этот или из этого потока (через GUIThread)
    // исп. в FreeQueue
end;

{$IFDEF WTHREAD_WINDOWS}

procedure TWThread.WWindowProc(var Msg: TMessage);
var i: integer;
begin
    if Terminated then exit;
    case Msg.Msg of
        WM_WTIMEOUT: begin
            DoTimeout;
            Msg.Result := 1;
        end;
        WM_WTHREAD_READY: begin
            FQueueReady := true;
            // поток готов, отправляем ему все закэшированные сообщения
            if Length(FQueueMessages) > 0 then for i := Low(FQueueMessages) to High(FQueueMessages) do
                PostToThreadMessage(FQueueMessages[i].Message, FQueueMessages[i].WParam, FQueueMessages[i].LParam);
            SetLength(FQueueMessages, 0);
            Msg.Result := 1;
        end;
        WM_WTHREAD_BASE..WM_WTHREAD_MAX: begin
            DoThreadReceiveMessage(Msg.Msg, Msg.WParam, Msg.LParam);
            Msg.Result := 1;
        end
    else  if LongBool(FToolWindow) then
        Msg.Result := DefWindowProc(FToolWindow, Msg.Msg, Msg.wParam, Msg.lParam);
    end;
end;

procedure TWThread.GetWindow;
begin
    FToolWindow := AllocateHWnd(WWindowProc);
end;

procedure TWThread.FreeWindow;
begin
    DeallocateHWnd(FToolWindow);
    FToolWindow := 0;
end;

{$ENDIF}

initialization

finalization
{$IFDEF WTHREAD_LIBRARY}
    if Assigned(FGUIThread) then begin
        FGUIThread.Terminate;
        FGUIThread := nil;
    end;
{$ENDIF}

end.
