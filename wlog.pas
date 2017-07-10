unit wlog;
// wlog (c) wadman, 2015-2017, 11.04.2017
// multithread safe logging

interface

{$i wthread.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

// log level
type
    TLogLevel = (
        WLL_MINIMUM     = 1,
        WLL_NORMAL      = 2,
        WLL_MAXIMUM     = 3,
        WLL_EXTRA       = 4
    );

var // log file name, default is *.log
    WLogFileName: string;
    // Default logging enabled
    WLogEnabled: boolean;
    // Default clear log when a programm started
    WLogClearOnStart: boolean;
    // Log level, see TLogLevel (default WLL_NORMAL, ie MAXIMUM and EXTRA will be ignored)
    WLogLevel: TLogLevel;

function PostToLog(const Text: string): boolean; overload; // with WLL_NORMAL
function PostToLog(const Text: string; const Level: TLogLevel): boolean; overload;
function PostToLog(const ADateTime: TDateTime; const Text: string): boolean; overload; // with WLL_NORMAL
function PostToLog(const ADateTime: TDateTime; const Text: string; const Level: TLogLevel): boolean; overload;
procedure EraseLog;

implementation

uses SysUtils, WThread;

const
    WM_LOG      = WM_WTHREAD_BASE + 1;
    WM_ERASE    = WM_WTHREAD_BASE + 2;

type
    PLogRecord = ^TLogRecord;
    TLogRecord = record
        DT: TDateTime;
        PString: NativeInt;
    end;

    TLogThread = class(TWThread)
    private
        procedure WMLog(var Msg: TThreadMessage); message WM_LOG;
        procedure WMErase(var Msg: TThreadMessage); message WM_ERASE;
    end;

var
    LogThread: TLogThread;
    FirstLine: boolean;
    log: TextFile;
    FileOpened: boolean;

function PostToLog(const Text: string): boolean; overload; // with WLL_NORMAL
begin
    result := PostToLog(Now, Text, WLL_NORMAL);
end;

function PostToLog(const Text: string; const Level: TLogLevel): boolean; overload;
begin
    result := PostToLog(Now, Text, Level);
end;

function PostToLog(const ADateTime: TDateTime; const Text: string): boolean;
begin
    result := PostToLog(ADateTime, Text, WLL_NORMAL);
end;

function PostToLog(const ADateTime: TDateTime; const Text: string; const Level: TLogLevel): boolean;
var p: PLogRecord;
begin
    if (SmallInt(Level) <= SmallInt(WLogLevel)) then begin
        result := WLogEnabled and Assigned(LogThread) and LongBool(Length(Text));
        if result then begin
            p := AllocMem(SizeOf(TLogRecord));
            p^.DT := ADateTime;
            p^.PString := NewString(Text);
{$HINTS OFF}
            result := LogThread.PostToThreadMessage(WM_LOG, 0, NativeInt(p));
{$HINTS ON}
            if not result then begin
                FreeString(p^.PString);
                FreeMem(p);
            end;
        end;
    end else
        Result := true;
end;

procedure EraseLog;
begin
    LogThread.PostToThreadMessage(WM_ERASE, 0, 0);
end;

function InitLogs: boolean;
begin
    WLogFileName := ChangeFileExt(ParamStr(0), '.log');
    LogThread := TLogThread.Create(nil, 'LogThread',
{$IFDEF WTHREAD_DEBUG_LOG}
        false,
{$ENDIF}
    false
    );
    result := Assigned(LogThread);
    if result then begin
        LogThread.FreeOnTerminate := true;
    end;
end;

procedure DoneLogs;
begin
    if Assigned(LogThread) then begin
        LogThread.Terminate;
    end;
end;

function OpenLog: boolean;
begin
    result := false;
    try
        AssignFile(log, WLogFileName);
        if (FileExists(WLogFileName))and(not WLogClearOnStart) then begin
            Append(log);
            if FirstLine then begin
                WriteLn(log);
                FirstLine := false;
            end;
        end else begin
            WLogClearOnStart := false;
            Rewrite(log);
        end;
        result := true;
    finally

    end;
end;

function CloseLog: boolean;
begin
    result := false;
    try
        CloseFile(log);
        result := true;
    finally

    end;
end;

function AddToLog(const DT: TDateTime; const Text: string): boolean;

    function ClearText(const AText: string): String;
    var i: integer;
    begin
        result := AText;
        for I := 1 to Length(result) do
            if Ord(result[i]) < 32 then
                result[i] := '.';
    end;

begin
    result := false;
    try
        WriteLn(log, Format('%s : %s', [FormatDateTime('dd.mm.yyyy hh:nn:ss.zzz', DT), ClearText(Text)]));
        result := true;
    finally
    end;
end;

{ TLogThread }

procedure TLogThread.WMLog(var Msg: TThreadMessage);
var p: PLogRecord;
begin
{$HINTS OFF}
    p := PLogRecord(Msg.LParam);
{$HINTS ON}
    if Length(WLogFileName) > 3 then begin
        if (not FileOpened) then
            FileOpened := OpenLog;
        if FileOpened then
            AddToLog(p^.DT, FreeString(p^.PString));
        if (not LogThread.HaveMessages) or Terminated then
            FileOpened := not CloseLog;
    end else if FileOpened then
        CloseLog;
    FreeMem(p);
end;

procedure TLogThread.WMErase(var Msg: TThreadMessage);
var f: File;
begin
    if FileOpened then
        FileOpened := not CloseLog;
    try
        if (not FileOpened) and (FileExists(WLogFileName)) then begin
            AssignFile(f, WLogFileName);
            Erase(f);
        end;
    finally

    end;
end;

initialization
    FileOpened := false;
    WLogLevel := WLL_NORMAL;
    WLogEnabled := InitLogs;
    FirstLine := true;
    WLogClearOnStart := false;

finalization
    DoneLogs;

end.
