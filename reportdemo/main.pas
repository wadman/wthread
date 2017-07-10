unit main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
    DbCtrls, StdCtrls, wcthread, LR_Class, LR_BarC, LR_e_img, db, memds;

type

    { TfrmMain }

    TfrmMain = class(TForm)
        butGenerate: TButton;
        butClose: TButton;
        butTest: TButton;
        DataSource1: TDataSource;
        DBGrid1: TDBGrid;
        DBNavigator1: TDBNavigator;
        frBarCodeObject1: TfrBarCodeObject;
        frImageExport1: TfrImageExport;
        MemDataset1: TMemDataset;
        report: TfrReport;
        TaskGenerateReports: TTask;
        WCThread1: TWCThread;
        procedure butCloseClick(Sender: TObject);
        procedure butGenerateClick(Sender: TObject);
        procedure butTestClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure reportGetValue(const ParName: String; var ParValue: Variant);
        procedure TaskGenerateReportsExecute(const Sender: TTask; const Msg: Word; var Param: Variant);
    private
    public

    end;

var
    frmMain: TfrmMain;
    curSerialString: string;

resourcestring
    rsDataSetFileName = 'serials';
    rsJpg = '%s.jpg';
    rsSERIAL = 'SERIAL';
    rsSERIAL2 = '[SERIAL]';
    rsDataSetIsEmpty = 'DataSet is empty.';
    rsFormCaption = 'Report Background Generate';
    rsGenerateCaption = 'Generate';
    rsTestCaption = 'Test...';
    rsCloseCaption = 'Close';

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
    Caption := rsFormCaption;
    butGenerate.Caption := rsGenerateCaption;
    butTest.Caption := rsTestCaption;
    butClose.Caption := rsCloseCaption;
    if FileExists(rsDataSetFileName) then begin
        MemDataset1.LoadFromFile(rsDataSetFileName);
    end else begin
        MemDataset1.CreateTable;
    end;
    DataSource1.DataSet.Open;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
    WCThread1.FinishAllTasks;
    MemDataset1.SaveToFile(rsDataSetFileName);
    DataSource1.DataSet.Close;
end;

procedure TfrmMain.reportGetValue(const ParName: String;
    var ParValue: Variant);
begin
    if (ParName = rsSERIAL) or (ParName = rsSERIAL2) then begin
        if TaskGenerateReports.IsRunning then
            // backgroud task
            ParValue := curSerialString
        else
            // foregrand task
            ParValue := DataSource1.DataSet.Fields[0].AsString;
    end;
end;

procedure TfrmMain.TaskGenerateReportsExecute(const Sender: TTask; const Msg: Word; var Param: Variant);
begin
    curSerialString := Param;
    if report.PrepareReport then begin
        // print emulation
        //report.ExportTo(TfrImageExportFilter, Format(rsJpg, [curSerialString]));
        report.SavePreparedReport(curSerialString+'.frp');
    end;
end;

procedure TfrmMain.butCloseClick(Sender: TObject);
begin
    Close;
end;

procedure TfrmMain.butGenerateClick(Sender: TObject);
var s: string;
    oldDs: TDataSet;
begin
    if DataSource1.DataSet.RecordCount > 0 then begin
        oldDs := DataSource1.DataSet;
        oldDs.DisableControls;
        //oldDs.First;
        while not oldDs.EOF do begin
            s := trim(oldDs.Fields[0].AsString);
            if Length(s) > 0 then begin
                // flow message filling
                TaskGenerateReports.Start(s);
                //FReportThread.PostToThreadMessage(WM_GENERATE_REPORT, 0, NewString(s));
            end;
            oldDs.Next;
        end;
        //oldDs.First;
        oldDs.EnableControls;
    end else begin
        MessageDlg(rsDataSetIsEmpty, mtError, [mbOk], 0);
    end;
end;

procedure TfrmMain.butTestClick(Sender: TObject);
begin
    if not Assigned(WCThread1.RunningTask) then
        // there is no running task, can show
        report.ShowReport;
end;

end.

