unit main;

{$mode delphi}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Variants,
    wcthread, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

    { TForm1 }

    TForm1 = class(TForm)
        butDemoTimer: TButton;
        Memo1: TMemo;
        TaskDemoTimer: TTask;
        WCThread1: TWCThread;
        procedure butDemoTimerClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure TaskDemoTimerExecute(const Sender: TTask; const AMsg: Word; var Param: Variant);
        procedure TaskDemoTimerFinish(const Sender: TTask; const Msg: Word; const Param: Variant);
        procedure TaskDemoTimerMessage(const Sender: TTask; const Msg: Word; const Parameter: Variant);
        procedure TaskDemoTimerProgress(const Sender: TTask; const AMsg: Word; const Value: Word);
    private
        procedure AddLog(const AText: string);
    public
    end;

var
    Form1: TForm1;

resourcestring
    rsFinished = 'finished';
    rsGotMessageDD = 'Got message %d, (%d)';
    rsGotMessageDS = 'Got message %d, (%s)';
    rsSS = '%s %s';
    rsProgress = 'progress';
    rsSSD = '%s %s %d';
    rsSSLog = '%s : %s';
    rsDateTime = 'hh:nn:ss.zzz';
    rsOldAffinityMaskD = 'Old AffinityMask %d';
    rsNewAffinityMaskD = 'New AffinityMask %d';
    rsProcessorCountD = 'Processor count: %d';
    rsStart = 'start';
    rsWCThreadDemo = 'WCThreadDemo';
    rsDemoTimerCaption = 'DemoTimer';

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.TaskDemoTimerExecute(const Sender: TTask; const AMsg: Word; var Param: Variant);
var i: integer;
begin
    // another thread
    for I := 0 to Param-1 do begin
        // check is task terminated?
        if Sender.Terminated then exit;
        // hard work
        Sleep(1000);
        // send progress to vcl/lcl
        Sender.PostProgress(i+1);
        // some time send custom message to vcl/lcl
        if i mod 2 = 0 then
            Sender.PostMessage(1, 1)
        else
            Sender.PostMessage(2, 'message');
    end;
end;

procedure TForm1.TaskDemoTimerFinish(const Sender: TTask; const Msg: Word; const Param: Variant);
begin
    // task is finished
    AddLog(Format(rsSS, [Sender.Name, rsFinished]));
end;

procedure TForm1.TaskDemoTimerMessage(const Sender: TTask; const Msg: Word; const Parameter: Variant);
var w: word;
    s: string;
begin
    // custom message from another thread
    case Msg of
        1: begin
            w := Parameter;
            AddLog(Format(rsGotMessageDD, [Msg, w]));
        end;
        2: begin
            s := Parameter;
            AddLog(Format(rsGotMessageDS, [Msg, s]));
        end;
    end;
end;

procedure TForm1.TaskDemoTimerProgress(const Sender: TTask; const AMsg: Word; const Value: Word);
begin
    // progress from another thread
    AddLog(Format(rsSSD, [Sender.Name, rsProgress, Value]));
end;

procedure TForm1.AddLog(const AText: string);
begin
    // log message to memo
    Memo1.Lines.Add(Format(rsSSLog, [FormatDateTime(rsDateTime, Now), AText]));
end;

procedure TForm1.butDemoTimerClick(Sender: TObject);
begin
    if TaskDemoTimer.IsRunning then begin
        // stop task
        TaskDemoTimer.Stop;
    end else begin
        Memo1.Lines.Clear;
        AddLog(Format(rsOldAffinityMaskD, [WCThread1.AffinityMask]));
        WCThread1.AffinityMask := 2;
        AddLog(Format(rsNewAffinityMaskD, [WCThread1.AffinityMask]));
        AddLog(Format(rsProcessorCountD, [WCThread1.ProcessorCount]));
        // start task with a custom parameter on another thread
        TaskDemoTimer.Start(20);
        AddLog(Format(rsSS, [TaskDemoTimer.Name, rsStart]));
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    Caption := rsWCThreadDemo;
    butDemoTimer.Caption := rsDemoTimerCaption;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    // must be called
    WCThread1.FinishAllTasks;
end;

end.

