unit wcthread;
// component for work with a thread, Delphi&Lazarus (win&wince&*nix)
//
// (c) wadman 2016-2017, from 10.07.2017


interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

uses Classes, SysUtils, wthread, Variants;

type
    {$IFNDEF FPC}
    PtrInt          = Integer;
    {$ENDIF}
    TWCThread       = class;
    TOwnedList      = class;
    TTask           = class;

    TTaskExecute    = procedure(const Sender: TTask; const Msg: Word; var Param: Variant) of object;
    TTaskFinish     = procedure(const Sender: TTask; const Msg: Word; const Param: Variant) of object;
    TTaskProgress   = procedure(const Sender: TTask; const Msg: Word; const Value: Word) of object;
    TTaskMessage    = procedure(const Sender: TTask; const Msg: Word; const Param: Variant) of object;
    TWCThreadNotify = procedure(const Sender: TWCthread) of object;

    TTaskState      = (tsWait, tsRunning, tsRunningDestroy, tsFinished, tsDestroying, tsReadyToFree);

    { TTask }

    // Task for the TWChread, like a message for the thread
    TTask = class(TComponent)
    private
        FOnExecute: TTaskExecute;
        FOnFinish: TTaskFinish;
        FOnMessage: TTaskMessage;
        FOnProgress: TTaskProgress;
        FParent: TWCThread;
        FCaller: TWCThread;
        FTerminated: boolean;
        FState: TTaskState;
        function GetIsFinished: boolean;
        function GetIsRunning: boolean;
        procedure SetParent(const Value: TWCThread);
        function GetTerminated: boolean;
    public
        function GetParentComponent: TComponent; override;
        function HasParent: Boolean; override;
    protected
        procedure PreDestroy;
        procedure SetParentComponent(AParent: TComponent); override;
        procedure DoExecute(const AMsg: Word; var AParameter: Variant);
        procedure DoMessage(const AMsg: Word; const AParameter: Variant);
        procedure DoFinish(const AMsg: Word; const AParameter: Variant);
        procedure DoProgress(const Msg: Word; const Value: Word);
        function ItsMe: boolean;
    public
        destructor Destroy; override;
        // Start this task on a different thread, like a PostMessage
        procedure Start(const ACaller: TWCThread; const AMsg: Word; const AParam: Variant); overload;
        // Start this task on a different thread, like a PostMessage
        procedure Start(const AMsg: Word; const AParam: Variant); overload;
        // Start this task on a different thread, like a PostMessage
        procedure Start(const AParam: Variant); overload;
        // Start this task on a different thread, like a PostMessage
        procedure Start(const ACaller: TWCThread); overload;
        // Start this task on a different thread, like a PostMessage
        procedure Start; overload;
        // Stop task (sets a Terminated, in another thread (OnExecute)
        // should be checked Sender.Terminated)
        procedure Stop;
        // Post from OnExecute to VLC/LCL a task progress
        procedure PostProgress(const AValue: Word); overload;
        // Post from OnExecute to VLC/LCL a task progress
        procedure PostProgress(const AMsg, AValue: Word); overload;
        // Post a custom message from thread
        procedure PostMessage(const AMsg: Word; const AParameter: Variant); overload;
        // wait ms with 10 ms interval
        procedure WaitMs(const Ms: cardinal);
        // When this flag is set, u need to stop OnExecute
        property Terminated: boolean read GetTerminated;
        property Parent: TWCThread read FParent write SetParent;
        // State of current task
        property State: TTaskState read FState;
        // Task is running
        property IsRunning: boolean read GetIsRunning;
        // Task is finished
        property IsFinished: boolean read GetIsFinished;
    published
        // This event is executed in another thread
        property OnExecute: TTaskExecute read FOnExecute write FOnExecute;
        // This event is executed in main thread, see TTask.PostProgress
        property OnProgress: TTaskProgress read FOnProgress write FOnProgress;
        // This event for custom messages from a task, see TTask.PostMessage
        property OnMessage: TTaskMessage read FOnMessage write FOnMessage;
        // This event is executed in main thread, when task is done by any reason
        property OnFinish: TTaskFinish read FOnFinish write FOnFinish;
        property Tag;
    end;

    // TWCThread component, (c) wadman
    // Add a tasks and fill OnExecute event for every

    { TWCThread }

    TWCThread = class(TComponent)
    private
        FAutoStart: boolean;
        FParam: Variant;
        FOnAllTasksFinished: TWCThreadNotify;
        function GetRunningTask: TTask;
        function GetThreadPriority: TThreadPriority;
        procedure SetParam(AValue: Variant);
        procedure SetThreadPriority(const Value: TThreadPriority);
        function GetAffinityMask: Cardinal;
        procedure SetAffinityMask(const Value: Cardinal);
        function GetTask(index: integer): TTask;
        function GetTerminated: boolean;
        procedure SetAutoStart(const Value: boolean);
        procedure DoAllTasksFinished;
    protected
        FTerminated: boolean;
        FWThread: TWThread;
        FPriority: TThreadPriority;
        FAffinityMask: Cardinal;
        FTasks: TOwnedList;
        procedure OnReceive(Sender: TWThread; var Msg: TThreadMessage);
        procedure PostMessageFromThread(const Msg: Word; const WParam: Word; const LParam: NativeInt);
        function PostToThreadMessage(const Msg: Word; const WParam: Word; const LParam: NativeInt): Boolean;
        function StartThread: boolean;
        procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
        property Terminated: boolean read GetTerminated;
        procedure Loaded; override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        // This procedure is performed in the destruction of the form
        // Abnormal ternimation
        procedure FinishAllTasks(const WaitMs: Cardinal = 1000);
        // Waiting for completion of all tasks in flow
        procedure WaitAllTasks(const WaitMs: Cardinal = 1000);
        property Task[index: integer]: TTask read GetTask; default;
        // Currently running task
        property RunningTask: TTask read GetRunningTask;
        class function ProcessorCount: integer;
    published
        // Cpu affinity mask, 0 = auto
        property AffinityMask: Cardinal read GetAffinityMask write SetAffinityMask default 0;
        // Start thread in a idle state when component is created at runtime.
        // False = thread started when a first task executed
        property AutoStart: boolean read FAutoStart write SetAutoStart default true;
        // Thread priority
        property Priority: TThreadPriority read GetThreadPriority write SetThreadPriority default tpNormal;
        // Custom param for all tasks
        property Param: Variant read FParam write SetParam;
        property Tasks: TOwnedList read FTasks;
        property OnAllTasksFinished: TWCThreadNotify read FOnAllTasksFinished write FOnAllTasksFinished;
    end;

    { TOwnedList }

    TOwnedList = class(TList)
    private
        FOwner: TWCThread;
    protected
        procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    public
        constructor Create(AOwner: TWCThread); overload;
        property Owner: TWCThread read FOwner;
    end;

implementation

{$IFDEF MSWINDOWS}
uses Windows;
{$ENDIF}

const
    WM_TASK_START       = WM_WTHREAD_BASE + 1;
    WM_TASK_FINISH      = WM_WTHREAD_BASE + 2;
    WM_TASK_PROGRESS    = WM_WTHREAD_BASE + 3;
    WM_TASK_MESSAGE     = WM_WTHREAD_BASE + 4;

    WAIT_TASK_INTERVAL  = 50;

type

    { TTaskThread }

    TTaskThread = class(TWThread)
    protected
        //procedure FreeMessage(const Msg: Word; const WParam: Word; const LParam: NativeInt); override;
    public
        procedure WMTaskStart(var Msg: TThreadMessage); message WM_TASK_START;
        procedure WMTaskFinish(var Msg: TThreadMessage); message WM_TASK_FINISH;
        procedure WMTaskProgress(var Msg: TThreadMessage); message WM_TASK_PROGRESS;
        procedure WMTaskMessage(var Msg: TThreadMessage); message WM_TASK_MESSAGE;
    end;

    PWCThreadMessageRecord = ^TWCThreadMessageRecord;
    TWCThreadMessageRecord = record
        callThread: TWCThread;
        task: TTask;
        Msg: Word;
        Parameter: Variant;
    end;

{$Hints off}
function _NewMessage(const AOwner: TWCThread; const ATask: TTask; const AMsg: Word; const AParameter: Variant): PtrInt;
var r: PWCThreadMessageRecord;
begin
    r := AllocMem(SizeOf(TWCThreadMessageRecord));
    if Assigned(r) then begin
        Pointer(result) := r;
        r^.callThread := AOwner;
        r^.task := ATask;
        r^.Msg := AMsg;
        r^.Parameter := AParameter;
    end else begin
        result := 0;
    end;
end;

procedure _FreeMessage(var AMessage: PWCThreadMessageRecord);
begin
    if Assigned(AMessage) then begin
        VarClear(AMessage^.Parameter);
        FreeMemory(AMessage);
        AMessage := nil;
    end;
end;

function _GetMessage(const AMessage: PtrInt): PWCThreadMessageRecord;
begin
    result := PWCThreadMessageRecord(AMessage);
end;
{$Hints on}

{ TWCThread }

constructor TWCThread.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FPriority := tpNormal;
    FTasks := TOwnedList.Create(Self);
    FAutoStart := true;
end;

destructor TWCThread.Destroy;
begin
    FTerminated := true;
    if Assigned(FWThread) then begin
        FWThread.Terminate;
        FWThread.WaitFor;
        FWThread.Free;
        FWThread := nil;
    end;
    FTasks.Free;
    inherited;
end;

procedure TWCThread.DoAllTasksFinished;
begin
    if Assigned(FOnAllTasksFinished) and (not Terminated) then
        FOnAllTasksFinished(Self);
end;

procedure TWCThread.FinishAllTasks(const WaitMs: Cardinal = 1000);
var t: integer;
    task: TTask;
begin
    if Assigned(FWThread) then begin
        task := RunningTask;
        if Assigned(task) then begin
            task.PreDestroy;
            t := WaitMs;
            while (task.State <> tsReadyToFree) and (t > 0) do begin
                Sleep(WAIT_TASK_INTERVAL);
                dec(t, WAIT_TASK_INTERVAL);
            end;
        end;
    end;
end;

procedure TWCThread.WaitAllTasks(const WaitMs: Cardinal);
var t: integer;
    task: TTask;
begin
    if Assigned(FWThread) then begin
        t := WaitMs;
        repeat
            task := RunningTask;
            if Assigned(task) then begin
                Sleep(WAIT_TASK_INTERVAL);
                dec(t, WAIT_TASK_INTERVAL);
            end;
        until Assigned(task) or (t > 0);
    end;
end;

function TWCThread.GetAffinityMask: Cardinal;
begin
    Result := FAffinityMask;
    //if Assigned(FWThread) then begin
    //    Result := FWThread.AffinityMask;
    //end else begin
    //    Result := FAffinityMask;
    //end;
end;

{$HINTS OFF}
procedure TWCThread.GetChildren(Proc: TGetChildProc; Root: TComponent);
var i: Integer;
begin
    if Terminated then exit;
    for i := 0 to Tasks.Count - 1 do
        Proc(Task[i]);
end;

{$HINTS ON}

function TWCThread.GetTask(index: integer): TTask;
begin
    Result := (TObject(FTasks[index]) as TTask);
end;

function TWCThread.GetTerminated: boolean;
begin
    result := FTerminated;
end;

function TWCThread.GetThreadPriority: TThreadPriority;
begin
    if Assigned(FWThread) then begin
        Result := FWThread.Priority;
    end else begin
        Result := FPriority;
    end;
end;

procedure TWCThread.Loaded;
begin
    inherited;
    if AutoStart then begin
        FAutoStart := StartThread;
    end;
end;

function TWCThread.GetRunningTask: TTask;
var i: integer;
begin
    result := nil;
    for i := 0 to Tasks.Count-1 do
        if Task[i].IsRunning then begin
            result := Task[i];
            break;
        end;
end;

procedure TWCThread.SetParam(AValue: Variant);
begin
    if Assigned(RunningTask) and (not FWThread.ItsMe) then
        raise Exception.Create('Can''t change Param from a other threads while the task is running.');
    if FParam = AValue then Exit;
    FParam := AValue;
end;

procedure TWCThread.OnReceive(Sender: TWThread; var Msg: TThreadMessage);
var message: PWCThreadMessageRecord;
begin
    message := _GetMessage(Msg.LParam);
    case Msg.Message of
        WM_TASK_FINISH: begin
            if Assigned(message) then begin
                message^.task.DoFinish(message^.Msg, message^.Parameter);
            end;
            if not FWThread.HaveMessages(WM_TASK_START) then
                DoAllTasksFinished;
        end;
        WM_TASK_PROGRESS: begin
            message^.task.DoProgress(message^.Msg, message^.Parameter);
        end;
        WM_TASK_MESSAGE: begin
            message^.task.DoMessage(message^.Msg, message^.Parameter);
        end;
    end;
    _FreeMessage(message);
end;

procedure TWCThread.PostMessageFromThread(const Msg: Word; const WParam: Word;
    const LParam: NativeInt);
begin
    if Terminated then exit;
    if Assigned(FWThread) then
        FWThread.PostMessageFromThread(Msg, WParam, LParam);
end;

function TWCThread.PostToThreadMessage(const Msg: Word; const WParam: Word;
    const LParam: NativeInt): Boolean;
begin
    result := false;
    if Terminated then exit;
    result := (Assigned(FWThread) or (StartThread)) and FWThread.PostToThreadMessage(Msg, WParam, LParam);
end;

class function TWCThread.ProcessorCount: integer;
begin
    result := TWThread.ProcessorCount;
end;

procedure TWCThread.SetAffinityMask(const Value: Cardinal);
begin
    FAffinityMask := Value;
    if Assigned(FWThread) then begin
        FWThread.AffinityMask := Value;
    end;
end;

procedure TWCThread.SetAutoStart(const Value: boolean);
begin
    FAutoStart := Value;
    if AutoStart and (not (csDesigning in ComponentState)) then
        FAutoStart := StartThread;
end;

procedure TWCThread.SetThreadPriority(const Value: TThreadPriority);
begin
    FPriority := Value;
    if Assigned(FWThread) then begin
        FWThread.Priority := Value;
    end;
end;

function TWCThread.StartThread: boolean;
begin
    result := false;
    if Terminated then exit;
    FWThread := TTaskThread.Create(nil, Name);
    FWThread.OnThreadReceiveMessage := OnReceive;
    //FWThread.FreeOnTerminate := true;
    FWThread.Priority := FPriority;
    if FAffinityMask <> 0 then
        FWThread.AffinityMask := FAffinityMask;
    result := Assigned(FWThread);
end;

{ TTaskThread }

{procedure TTaskThread.FreeMessage(const Msg: Word; const WParam: Word;
    const LParam: NativeInt);
var pmessage: PWCThreadMessageRecord;
begin
    case Msg of
        WM_TASK_START,
        WM_TASK_FINISH,
        WM_TASK_MESSAGE,
        WM_TASK_PROGRESS: begin
            pmessage := _GetMessage(LParam);
            _FreeMessage(pmessage);
        end
    else
        inherited FreeMessage(Msg, WParam, LParam);
    end;
end;}

procedure TTaskThread.WMTaskFinish(var Msg: TThreadMessage);
var message: PWCThreadMessageRecord;
begin
    message := _GetMessage(Msg.LParam);
    message^.task.DoFinish(message^.Msg, message^.Parameter);
    _FreeMessage(message);
end;

procedure TTaskThread.WMTaskMessage(var Msg: TThreadMessage);
var message: PWCThreadMessageRecord;
begin
    message := _GetMessage(Msg.LParam);
    message^.task.DoMessage(message^.Msg, message^.Parameter);
    _FreeMessage(message);
end;

procedure TTaskThread.WMTaskProgress(var Msg: TThreadMessage);
var message: PWCThreadMessageRecord;
begin
    message := _GetMessage(Msg.LParam);
    message^.task.DoProgress(message^.Msg, message^.Parameter);
    _FreeMessage(message);
end;

procedure TTaskThread.WMTaskStart(var Msg: TThreadMessage);
var message: PWCThreadMessageRecord;
    param: Variant;
begin
    message := _GetMessage(Msg.LParam);
    param := message^.Parameter;
    message^.task.DoExecute(message^.Msg, param);
    if message^.task.State in [tsRunningDestroy, tsDestroying] then begin
        message^.task.FState := tsReadyToFree;
        _FreeMessage(message);
    end else begin
        {$Hints off}
        message^.task.FState := tsFinished;
        message^.Parameter := param;
        if Assigned(message^.callThread) then begin
            message^.callThread.PostToThreadMessage(WM_TASK_FINISH, 0, PtrInt(message));
            PostMessageFromThread(WM_TASK_FINISH, 0, 0);
        end else begin
            PostMessageFromThread(WM_TASK_FINISH, 0, PtrInt(message));
        end;
        {$Hints on}
    end;
end;

{ TTask }

destructor TTask.Destroy;
begin
    PreDestroy;
    Parent := nil;
    inherited;
end;

procedure TTask.DoExecute(const AMsg: Word; var AParameter: Variant);
begin
    FState := tsRunning;
    if Assigned(FOnExecute) then
        FOnExecute(Self, AMsg, AParameter);
end;

procedure TTask.DoFinish(const AMsg: Word; const AParameter: Variant);
begin
    if (not Terminated) and Assigned(FOnFinish) then begin
        FState := tsFinished;
        FOnFinish(Self, AMsg, AParameter);
    end;
    if State in [tsRunningDestroy, tsDestroying] then
        FState := tsReadyToFree;
end;

function TTask.ItsMe: boolean;
begin
    result := Parent.FWThread.ItsMe;
end;

procedure TTask.DoProgress(const Msg: Word; const Value: Word);
begin
    if (not Terminated) and Assigned(FOnProgress) then
        FOnProgress(Self, Msg, Value);
end;

procedure TTask.DoMessage(const AMsg: Word; const AParameter: Variant);
begin
    if (not Terminated) and Assigned(FOnMessage) then
        FOnMessage(Self, AMsg, AParameter);
end;

function TTask.GetParentComponent: TComponent;
begin
    result := FParent;
end;

function TTask.GetTerminated: boolean;
begin
    Result := (State in [tsRunningDestroy, tsDestroying, tsReadyToFree]) or FTerminated;
end;

function TTask.HasParent: Boolean;
begin
    result := Assigned(FParent);
end;

procedure TTask.PostProgress(const AValue: Word);
begin
    PostProgress(0, AValue);
end;

procedure TTask.PostProgress(const AMsg, AValue: Word);
begin
    if (not Terminated) then begin
        if Assigned(FCaller) then begin
            FCaller.PostToThreadMessage(WM_TASK_PROGRESS, 0, _NewMessage(nil, Self, AMsg, AValue));
        end else begin
            FParent.PostMessageFromThread(WM_TASK_PROGRESS, 0, _NewMessage(nil, Self, AMsg, AValue));
        end;
    end;
end;

procedure TTask.PostMessage(const AMsg: Word; const AParameter: Variant);
begin
    if (not Terminated) then begin
        if Assigned(FCaller) then begin
            FCaller.PostToThreadMessage(WM_TASK_MESSAGE, 0, _NewMessage(nil, Self, AMsg, AParameter));
        end else begin
            FParent.PostMessageFromThread(WM_TASK_MESSAGE, 0, _NewMessage(nil, Self, AMsg, AParameter));
        end;
    end;
end;

procedure TTask.WaitMs(const Ms: cardinal);
var curms: integer;
begin
    curms := ms;
    while (not Terminated) and LongBool(curms) do begin
        Sleep(INTERNAL_WAIT_TIMEOUT);
        dec(curms, INTERNAL_WAIT_TIMEOUT);
    end;
end;

procedure TTask.PreDestroy;
begin
    if FState = tsRunning then
        FState := tsRunningDestroy
    else
        FState := tsDestroying;
    FTerminated := true;
end;

procedure TTask.SetParent(const Value: TWCThread);
begin
    if FParent <> Value then begin
        if Assigned(FParent) then
            FParent.FTasks.Remove(Self);
        FParent := Value;
        if Assigned(FParent) then
            FParent.FTasks.Add(Self);
    end;
end;

function TTask.GetIsFinished: boolean;
begin
    result := FState = tsFinished;
end;

function TTask.GetIsRunning: boolean;
begin
    result := FState in [tsRunning, tsRunningDestroy];
end;

procedure TTask.SetParentComponent(AParent: TComponent);
begin
    if AParent is TWCThread then
        SetParent(AParent as TWCThread)
    else
        SetParent(nil);
end;

procedure TTask.Start;
begin
    Start(nil, 0, null);
end;

procedure TTask.Start(const ACaller: TWCThread);
begin
    Start(ACaller, 0, null);
end;

procedure TTask.Start(const ACaller: TWCThread; const AMsg: Word; const AParam: Variant);
begin
    if Terminated then
        raise Exception.CreateFmt('%s cannot start while destroying.', [Name]);
    FTerminated := false;
    FCaller := ACaller;
    FParent.PostToThreadMessage(WM_TASK_START, 0, _NewMessage(ACaller, Self, AMsg, AParam));
end;

procedure TTask.Start(const AMsg: Word; const AParam: Variant);
begin
    Start(nil, AMsg, AParam);
end;

procedure TTask.Start(const AParam: Variant);
begin
    Start(nil, 0, AParam);
end;

procedure TTask.Stop;
begin
    FTerminated := true;
end;

{ TOwnedList }

procedure TOwnedList.Notify(Ptr: Pointer; Action: TListNotification);
begin
    inherited Notify(Ptr, Action);
end;

constructor TOwnedList.Create(AOwner: TWCThread);
begin
    inherited Create;
    FOwner := AOwner;
end;

end.
