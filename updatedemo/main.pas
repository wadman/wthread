unit main;

{$mode delphi}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ComCtrls, wcthread, IniFiles, Math,
{$IfDef FPC}
    LCLIntf,
{$EndIf}
    Variants;

const
    TaskMessage     = 10;
    TaskProgress    = 11;

    TaskAllOk       = 1;
    TaskNotAllOk    = 2;
    TaskError       = 3;
    TaskCancelled   = 4;

type
    TButtonAction   = (baNone, baCancel, baRun, baClose);

    { TfrmRun }

    TfrmRun = class(TForm)
        butAction: TButton;
        labInfo: TLabel;
        progressBar: TProgressBar;
        TaskCloseFile: TTask;
        TaskWriteFile: TTask;
        TaskUpdate: TTask;
        threadUpdate: TWCThread;
        threadWrite: TWCThread;
        procedure butActionClick(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure TaskCloseFileExecute(const Sender: TTask; const Msg: Word; var Param: Variant);
        procedure TaskUpdateExecute(const Sender: TTask; const Msg: Word; var Param: Variant);
        procedure TaskUpdateFinish(const Sender: TTask; const Msg: Word; const Param: Variant);
        procedure TaskUpdateMessage(const Sender: TTask; const Msg: Word; const Parameter: Variant);
        procedure TaskWriteFileExecute(const Sender: TTask; const Msg: Word; var Param: Variant);
    private
        FActivated: boolean;
        FButtonAction: TButtonAction;
        FIniFile: TIniFile;
        function GetInfo: String;
        procedure SetButtonAction(const AValue: TButtonAction);
        procedure SetInfo(const AValue: String);
    public
        procedure Run;
        property ButtonAction: TButtonAction read FButtonAction write SetButtonAction;
        property Info: String read GetInfo write SetInfo;
        property IniFile: TIniFile read FIniFile;
        property Activated: boolean read FActivated write FActivated;
    end;

const
    CM_SYSTEM   = 0; // use system copy
    CM_1THREAD  = 1; // buildin copy in one thread
    CM_2THREAD  = 2; // buildin copy in two threads

var
    frmRun: TfrmRun;
    AlwaysOverwrite: boolean;
    cnt: int64;
    copyMode: integer = CM_2THREAD;

const
    MAX_BUF_SIZE: int64 = 16*1024*1024;

resourcestring
    rsCheckingForUpdate = 'Checking for update...';
    rsMain = 'Main';
    rsThereIsNoIniFileInTh = 'There is no ini-file in the command line.';
    rsUpdating = 'Updating...';
    rsSomeFilesAreNotCopie = 'Some files are not copied.%sPress Run to run the old version.';
    rsUpdateErrorPressRunT = 'Update error.%sPress Run to run the old version.';
    rsRun = 'Run';
    rsCancel = 'Cancel';
    rsClose = 'Close';
    rsRunning = 'Running...';
    rsErrorToExecuteTheMai = 'Error to execute the main file.';
    rsParameters = 'Parameters';
    rsCount = 'Count';
    rsFileD = 'File%d';
    rsFrom = 'From';
    rsTo = 'To';
    rsName = 'Name';
    rsOverwite = 'Overwite';
    rsOpen = 'open';
    rsSS = '%s %s';
    rsEmpty = '';

implementation

uses LazUTF8;

{$R *.lfm}

{ TfrmRun }

procedure TfrmRun.FormCreate(Sender: TObject);
begin
    Activated := false;
    AlwaysOverwrite := false;
end;

procedure TfrmRun.butActionClick(Sender: TObject);
begin
    case ButtonAction of
        baRun: begin
            Run;
            Close;
        end;
        baCancel: begin
            if TaskUpdate.IsRunning then
                TaskUpdate.Stop;
        end;
        baClose: begin
            Close;
        end;
    end;
end;

procedure TfrmRun.FormActivate(Sender: TObject);
var cap: string;
begin
    if Activated then exit;
    Activated := true;
    if FileExists(ParamStr(1)) then begin
        FIniFile := TIniFile.Create(ExpandUNCFileNameUTF8(ParamStr(1)));
        cap := IniFile.ReadString(rsMain, rsName, rsEmpty);
        if Length(cap) > 0 then Caption := cap;
        AlwaysOverwrite := IniFile.ReadBool(rsMain, rsOverwite, false);
        Info := rsCheckingForUpdate;
        ButtonAction := baCancel;
        // for speed check
        cnt := GetTickCount64;
        TaskUpdate.Start(PtrUInt(IniFile));
    end else begin
        Info := rsThereIsNoIniFileInTh;
        ButtonAction := baClose;
    end;
end;

procedure TfrmRun.FormDestroy(Sender: TObject);
begin
    if Assigned(IniFile) then begin
        FIniFile.Free;
        FIniFile := nil;
    end;
    threadUpdate.FinishAllTasks;
    threadWrite.FinishAllTasks;
end;

procedure TfrmRun.TaskCloseFileExecute(const Sender: TTask; const Msg: Word;
    var Param: Variant);
var h: THANDLE;
begin
    h := Param;
    if h > 0 then begin
        FileClose(h);
    end;
end;

procedure TfrmRun.TaskUpdateExecute(const Sender: TTask; const Msg: Word; var Param: Variant);
var ini: TIniFile;
    total, copied: int64;
    i, filesCount, filesCopied: integer;
    fromFileName, toFileName: ansistring;

    procedure UpdateProgress;
    begin
        Sender.PostMessage(TaskProgress, VarArrayOf([total div 1024, copied div 1024]));
    end;

    function CheckFileSize(const fileName: string): int64;
    begin
        result := FileSize(fileName);
    end;

    function CopyFile1(const fromName, toName: string): boolean;
    var bytesCopied: LongInt;
        Source, Dest: Integer;
        bufSize: int64;
        buffer: Pointer;
    begin
        if (AlwaysOverwrite) or (not FileExists(toName)) or (FileAge(toName) <> FileAge(fromName)) then begin
            result := false;
            try
                bufSize := Min(FileSize(fromName), MAX_BUF_SIZE);
                Source := FileOpen(fromName, fmShareDenyWrite); { open source file }
                if Source > 0 then
                try
                    Dest := FileCreate(toName); { create output file; overwrite existing }
                    if Dest > 0 then
                    try
                        buffer := GetMem(bufSize);
                        repeat
                            bytesCopied := FileRead(Source, buffer^, bufSize); { read chunk }
                            if bytesCopied > 0 then { if we read anything... }
                                FileWrite(Dest, buffer^, bytesCopied); { ...write chunk }
                            copied := copied + bytesCopied;
                            UpdateProgress;
                        until (BytesCopied < bufSize) and (not Sender.Terminated); { until we run out of chunks }
                        result := true;
                    finally
                        FileClose(Dest); { close the destination file }
                        FreeMem(buffer);
                    end;
                finally
                    FileClose(Source); { close the source file }
                end;
            finally
            end;
        end else begin
            result := true;
            copied := copied + FileSize(fromName);
        end;
        UpdateProgress;
    end;

    function CopyFile2(const fromName, toName: string): boolean;
    var bytesCopied: int64;
        Source, Dest: Integer;
        bufSize: int64;
        buffer: pointer;
    begin
        if (AlwaysOverwrite) or (not FileExists(toName)) or (FileAge(toName) <> FileAge(fromName)) then begin
            result := false;
            try
                bufSize := Min(FileSize(fromName), MAX_BUF_SIZE);
                Source := FileOpen(fromName, fmShareDenyWrite); { open source file }
                if Source > 0 then
                try
                    Dest := FileCreate(toName, fmShareExclusive); { create output file; overwrite existing }
                    if Dest > 0 then
                    try
                        repeat
                            GetMem(buffer, bufSize);
                            bytesCopied := FileRead(Source, buffer^, bufSize); { read chunk }
                            if bytesCopied > 0 then begin
                                TaskWriteFile.Start(VarArrayOf([Dest, PtrInt(buffer), bytesCopied]));
                            end else begin
                                FreeMem(buffer);
                            end;
                            copied := copied + bytesCopied;
                            UpdateProgress;
                        until (BytesCopied < bufSize) and (not Sender.Terminated); { until we run out of chunks }
                        result := true;
                    finally
                        //FileClose(Dest); { close the destination file }
                        TaskCloseFile.Start(dest);
                    end;
                finally
                    FileClose(Source); { close the source file }
                end;
            finally
            end;
        end else begin
            result := true;
            copied := copied + FileSize(fromName);
        end;
        UpdateProgress;
    end;

begin
    total := 0;
    copied := 0;
    ini := TObject(PtrUInt(Param)) as TIniFile;
    filesCopied := 0;
    filesCount := ini.ReadInteger(rsMain, rsCount, 0);
    for i := 0 to filesCount-1 do begin
        total := total + CheckFileSize(ini.ReadString(Format(rsFileD, [i + 1]), rsFrom, rsEmpty));
    end;
    Sender.PostMessage(TaskMessage, rsUpdating);
    UpdateProgress;
    if total > 0 then for i := 0 to filesCount-1 do if not Sender.Terminated then begin
        fromFileName := ini.ReadString(Format(rsFileD, [i+1]), rsFrom, rsEmpty);
        toFileName := ini.ReadString(Format(rsFileD, [i + 1]), rsTo, rsEmpty);
        if (Length(fromFileName) > 0) and (Length(toFileName) > 0) then case CopyMode of
            CM_SYSTEM: begin
                if (AlwaysOverwrite) or (not FileExists(toFileName)) or (FileAge(toFileName) <> FileAge(fromFileName)) then begin
                    if CopyFile(pchar(fromFileName), pchar(toFileName), false) then begin
                        inc(filesCopied);
                    end;
                    copied := copied + FileSize(fromFileName);
                    UpdateProgress;
                end;
            end;
            CM_1THREAD: begin
                if CopyFile1(fromFileName, toFileName) then begin
                    inc(filesCopied);
                end;
            end;
            CM_2THREAD: begin
                if CopyFile2(fromFileName, toFileName) then begin
                    inc(filesCopied);
                end;
            end;
        end;
    end;
    if filesCopied = 0 then
        Param := TaskError
    else if filesCopied < filesCount then
        Param := TaskNotAllOk
    else if filesCopied = filesCount then
        Param := TaskAllOk;
    if Sender.Terminated then begin
        Param := TaskCancelled;
        TaskWriteFile.Stop;
    end;
end;

procedure TfrmRun.TaskUpdateFinish(const Sender: TTask; const Msg: Word; const Param: Variant);
begin
    progressBar.Visible := false;
    if Param = TaskAllOk then begin
        // ok
        ButtonAction := baNone;
        Run;
        Close;
    end else if Param = TaskNotAllOk then begin
        // not all ok
        Info := Format(rsSomeFilesAreNotCopie, [LineEnding]);
        ButtonAction := baRun;
    end else if Param = TaskCancelled then begin
        // cancelled by user
        Close;
    end else begin
        // all not ok
        Info := Format(rsUpdateErrorPressRunT, [LineEnding]);
        ButtonAction := baRun;
    end;
end;

procedure TfrmRun.TaskUpdateMessage(const Sender: TTask; const Msg: Word; const Parameter: Variant);
begin
    case Msg of
        TaskMessage: begin
            Info := Parameter;
        end;
        TaskProgress: begin
            progressBar.Visible := true;
            progressBar.Max := Parameter[0];
            progressBar.Position := Parameter[1];
        end;
    end;
end;

procedure TfrmRun.TaskWriteFileExecute(const Sender: TTask; const Msg: Word; var Param: Variant);
var p: Pointer;
    h: THandle;
    s: int64;
begin
    h := Param[0];
    p := Pointer(PtrInt(Param[1]));
    s := Param[2];
    if h > 0 then begin
        FileWrite(h, p^, s);
    end;
    FreeMem(p);
end;

procedure TfrmRun.SetButtonAction(const AValue: TButtonAction);
begin
    if FButtonAction = AValue then Exit;
    FButtonAction := AValue;
    case ButtonAction of
        baNone: begin
            butAction.Visible := false;
        end;
        baRun: begin
            butAction.Caption := rsRun;
            butAction.Visible := true;
        end;
        baCancel: begin
            butAction.Caption := rsCancel;
            butAction.Visible := true;
        end;
        baClose: begin
            butAction.Caption := rsClose;
            butAction.Visible := true;
        end;
    end;
end;

function TfrmRun.GetInfo: String;
begin
    result := labInfo.Caption;
end;

procedure TfrmRun.SetInfo(const AValue: String);
begin
    labInfo.Caption := AValue;
end;

procedure TfrmRun.Run;
var runable,
    params: string;
    res: boolean;
begin
    threadWrite.WaitAllTasks(5000);
    cnt := GetTickCount64 - cnt;
    Cursor := crHourGlass;
    Info := rsRunning;
    runable := IniFile.ReadString(rsMain, rsRun, rsEmpty);
    params := IniFile.ReadString(rsMain, rsParameters, rsEmpty);
    res := false;
    if (Length(runable) > 0)and(FileExists(runable)) then begin
        {$IfDef FPC}
        res := OpenURL(Format(rsSS, [PChar(runable), PChar(params)]));
        {$Else}
        res := ShellExecute(Handle, rsOpen, PChar(runable), PChar(params), PChar(ExtractFilePath(runable)), SW_NORMAL);
        {$EndIf}
    end;
    if not res then
        MessageDlg(rsErrorToExecuteTheMai, mtError, [mbOk], 0);
end;

end.

