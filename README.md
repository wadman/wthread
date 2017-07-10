# wthread
Component for work with a thread, Delphi&Lazarus (win&wince&*nix)
(c) wadman 2016-2017, from 10.07.2017

This is my component to work with additional threads for Delphi & Lazarus.
Using them is very easy.
Drop a TWCThread on the form and add a few tasks.
In the task you need to write a method OnExecute which will be executed in a different thread.
Each task is a message flow. All tasks will run in the start order.

Components tested on Ubuntu 14 and 16 LTS, Windows 7, WinCE, Lazarus 1.6/1.7/1.8/1.9, Delphi 7/XE2.

Example project:
```
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, wcthread, StdCtrls;

type
  TForm1 = class(TForm)
    WCThread1: TWCThread;
    TaskCheckLicense: TTask;
    Label1: TLabel;
    procedure TaskCheckLicenseExecute(const Sender: TTask; const Msg: Word; var Param: Variant);
    procedure FormCreate(Sender: TObject);
    procedure TaskCheckLicenseFinish(const Sender: TTask; const Msg: Word; const Param: Variant);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Label1.Caption := 'Check license...';
  // Running a task in another thread
  TaskCheckLicense.Start(5000); // see TaskCheckLicenseExecute(... Param)
end;

procedure TForm1.TaskCheckLicenseExecute(const Sender: TTask; const Msg: Word; var Param: Variant);
begin
  // Check license, its another thread
  Sender.WaitMs(Param); // wait some secs for the hard work
  Param := 'License is valid.';  // Input and output parameter.  It is transferred from Start and returns to Finish.
end;

procedure TForm1.TaskCheckLicenseFinish(const Sender: TTask; const Msg: Word; const Param: Variant);
begin
  Label1.Caption := Param;
end;

end.
```
