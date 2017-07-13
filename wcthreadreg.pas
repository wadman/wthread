unit wcthreadreg;

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

uses
    Forms,
    Classes,
    {$IFDEF FPC}
    ProjectIntf,
    LazIDEIntf,
    FormEditingIntf,
    PropEdits,
    ComponentEditors,
    {$ELSE}
    DesignEditors,
    DesignIntf,
    DesignWindows,
    {$ENDIF}
    wcthread,
    Controls,
    ExtCtrls,
    ComCtrls,
    ActnList,
    Buttons,
    ImgList,
    SysUtils;

type
    TWCThreadEditor = class(TComponentEditor)
        procedure ExecuteVerb(Index: Integer); override;
        function GetVerb(Index: Integer): string; override;
        function GetVerbCount: Integer; override;
    end;

type

    { TfrmTaskEditor }
    {$IFDEF FPC}
    {$ELSE}
    TForm = TDesignWindow;
    {$ENDIF}

    TfrmTaskEditor = class(TForm)
        panTop: TPanel;
        lvTasks: TListView;
        ActionList1: TActionList;
        SpeedButton1: TSpeedButton;
        actAdd: TAction;
        actDelete: TAction;
        SpeedButton2: TSpeedButton;
        ImageList1: TImageList;
        procedure actAddExecute(Sender: TObject);
        procedure actDeleteExecute(Sender: TObject);
        procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
        procedure FormDestroy(Sender: TObject);
        procedure lvTasksEdited(Sender: TObject; Item: TListItem; var S: string);
        procedure lvTasksSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
        procedure FormClose(Sender: TObject; var AAction: TCloseAction);
        procedure lvTasksDblClick(Sender: TObject);
    private
        {$IFDEF FPC}
        FDesigner: TComponentEditorDesigner;
        {$ENDIF}
        FSelectionError: Boolean;
        FComponent: TWCThread;
        {$IFDEF FPC}
        function GetDesigner: TComponentEditorDesigner;
        procedure SetDesigner(AValue: TComponentEditorDesigner);
        {$ENDIF}
        procedure SetComponent(const Value: TWCThread);
        function GetSelectedTask: TTask;
    protected
        procedure UpdateList;
        procedure SetSelection;
        procedure Modified;
        procedure OnModified(Sender: TObject);
    public
        {$IFDEF FPC}
        property Designer: TComponentEditorDesigner read FDesigner write SetDesigner;
        {$ELSE}
        procedure ItemsModified(const Designer: IDesigner); override;
        {$ENDIF}
        property Component: TWCThread read FComponent write SetComponent;
        property SelectedTask: TTask read GetSelectedTask;
	end;

procedure Register;

implementation

{$IFDEF FPC}
{$i lazver.inc}
{$R *.lfm}
{$ElSE}
{$R *.dfm}
{$ENDIF}

type

    {$IFDEF FPC}
    { TLazarusHook }

    TLazarusHook = class
        FNeedUpdate: boolean;
        procedure Init;
        procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
        procedure OnComponentRenamed(AComponent: TComponent);
        procedure OnPersistentDeleting(APersistent: TPersistent);
        procedure OnPersistentDeleted;
        {$ifdef ver16}
        procedure OnModified(Sender: TObject; PropName: ShortString);
        {$else}
        procedure OnModified(Sender: TObject);
        {$endif}
        procedure Done;
    end;
    {$ELSE}
    PtrInt = NativeInt;
    {$ENDIF}

    TTasksEditor = class(TPropertyEditor)
        procedure Edit; override;
        function GetAttributes: TPropertyAttributes; override;
        function GetValue: string; override;
    end;

var
    frmTaskEditor: TfrmTaskEditor;

procedure ShowTasksEditor({$IFDEF FPC}ADesigner: TComponentEditorDesigner; {$ELSE}const ADesigner: IDesigner;{$ENDIF} const Component: TWCThread);
begin
    try
        if not Assigned(frmTaskEditor) then
            frmTaskEditor := TfrmTaskEditor.Create(Application);
        frmTaskEditor.Designer := ADesigner;
        frmTaskEditor.Component := Component;
        frmTaskEditor.Show;
    finally

    end;
end;

{$IFDEF FPC}
{ TLazarusHook }

var Hook: TLazarusHook;

procedure TLazarusHook.Init;
begin
    GlobalDesignHook.AddHandlerPersistentDeleting(OnPersistentDeleting);
    GlobalDesignHook.AddHandlerPersistentDeleted(OnPersistentDeleted);
    GlobalDesignHook.AddHandlerModified(OnModified);
    GlobalDesignHook.AddHandlerPersistentAdded(OnPersistentAdded);
    GlobalDesignHook.AddHandlerComponentRenamed(OnComponentRenamed);
end;

procedure TLazarusHook.OnPersistentAdded(APersistent: TPersistent; Select: boolean);
begin
    if APersistent is TTask then begin
        if Assigned(frmTaskEditor)and(frmTaskEditor.Component = (APersistent as TTask).Parent) then begin
            Select := true;
            frmTaskEditor.OnModified(Self);
        end;
    end;
end;

procedure TLazarusHook.OnComponentRenamed(AComponent: TComponent);
begin
    if AComponent is TTask then begin
        if Assigned(frmTaskEditor)and(frmTaskEditor.Component = AComponent) then begin
            frmTaskEditor.OnModified(Self);
        end;
    end;
end;

procedure TLazarusHook.OnPersistentDeleting(APersistent: TPersistent);
var t: TWCThread;
begin
    if APersistent is TWCThread then begin
        t := APersistent as TWCThread;
        if Assigned(frmTaskEditor) then begin
            frmTaskEditor.Close;
            frmTaskEditor := nil;
        end;
        while t.Tasks.Count > 0 do begin
            t.task[0].free;
        end;
        if GlobalDesignHook <> nil then
            GlobalDesignHook.PersistentDeleted;
    end else if APersistent is TTask then begin
        FNeedUpdate := Assigned(frmTaskEditor);
        if GlobalDesignHook <> nil then
            GlobalDesignHook.PersistentDeleted;
    end;
end;

procedure TLazarusHook.OnPersistentDeleted;
begin
    if (FNeedUpdate) and Assigned(frmTaskEditor) then begin
        frmTaskEditor.OnModified(Self);
    end;
    FNeedUpdate := false;
end;

{$ifdef ver16}
procedure TLazarusHook.OnModified(Sender: TObject; PropName: ShortString);
{$else}
procedure TLazarusHook.OnModified(Sender: TObject);
{$EndIf}
begin
    if Assigned(frmTaskEditor) then begin
        frmTaskEditor.OnModified(Self);
    end;
end;

procedure TLazarusHook.Done;
begin
    GlobalDesignHook.RemoveHandlerPersistentAdded(OnPersistentAdded);
    GlobalDesignHook.RemoveHandlerPersistentDeleting(OnPersistentDeleting);
    GlobalDesignHook.RemoveHandlerPersistentDeleted(OnPersistentDeleted);
    GlobalDesignHook.RemoveHandlerModified(OnModified);
    GlobalDesignHook.RemoveHandlerComponentRenamed(OnComponentRenamed);
end;
{$ENDIF}

procedure Register;
begin
    RegisterClass(TTask);
    RegisterNoIcon([TTask]);
    RegisterComponents('System', [TWCThread]);
    {$IFDEF FPC}
    Hook := TLazarusHook.Create;
    Hook.Init;
    {$ELSE}
    {$ENDIF}
    RegisterPropertyEditor(TypeInfo(TOwnedList), TWCThread, 'Tasks', TTasksEditor);
    RegisterComponentEditor(TWCThread, TWCThreadEditor);
end;

{ TTWCThreadEditor }

procedure TWCThreadEditor.ExecuteVerb(Index: Integer);
var task: TTask;
begin
    case index of
        0: begin
            ShowTasksEditor(Designer, Component as TWCThread);
        end;
        1: begin
            {$IFDEF FPC}
            task := FormEditingHook.CreateComponent(Component as TWCThread, TTask, 'wcthread, wthread', 0, 0, 0, 0, true) as TTask;
            {$ELSE}
            task := Designer.CreateComponent(TTask, Component as TWCThread, 0, 0, 0, 0) as TTask;
            {$ENDIF}
            task.Parent := Component as TWCThread;
            {$IFDEF FPC}
            if Assigned(Designer) then
                Designer.PropertyEditorHook.PersistentAdded(task, True);
            {$ElSE}
            Designer.Modified;
            {$ENDIF}
        end;
    end;
end;

function TWCThreadEditor.GetVerb(Index: Integer): string;
begin
    case index of
        0: Result := 'Edit Tasks...';
        1: Result := 'Add Task';
    end;
end;

function TWCThreadEditor.GetVerbCount: Integer;
begin
    Result := 2;
end;

{ TfrmTaskEditor }

procedure TfrmTaskEditor.actAddExecute(Sender: TObject);
var task: TTask;
    item: TListItem;
begin
    {$IFDEF FPC}
    task := FormEditingHook.CreateComponent(FComponent, TTask, 'wcthread, wthread', 0, 0, 0, 0, true) as TTask;
    {$ELSE}
    task := Designer.CreateComponent(TTask, FComponent, 0, 0, 0, 0) as TTask;
    {$ENDIF}
    task.Parent := FComponent;
    lvTasks.Items.BeginUpdate;
    item := lvTasks.Items.Add;
    item.Caption := task.name;
    item.Data := task;
    lvTasks.Selected := item;
    lvTasks.Items.EndUpdate;
    {$IFDEF FPC}
    if Assigned(Designer) then
        Designer.PropertyEditorHook.PersistentAdded(task, True);
    {$ELSE}
    Modified;
    {$ENDIF}
end;

procedure TfrmTaskEditor.actDeleteExecute(Sender: TObject);
var task: TTask;
    i: integer;
begin
    i := lvTasks.Items.IndexOf(lvTasks.Selected);
    task := SelectedTask;
    lvTasks.Items.Delete(i);
    FComponent.Tasks.Remove(task);
    task.Free;
    {$IFDEF FPC}
    if Assigned(Designer) then
        Designer.PropertyEditorHook.PersistentDeleted;
    {$ELSE}
    //lvTasks.DeleteSelected;
    {$ENDIF}
    Modified;
end;

procedure TfrmTaskEditor.ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
begin
    actDelete.Enabled := Assigned(lvTasks.Selected) and (lvTasks.SelCount = 1);
    Handled := true;
end;

procedure TfrmTaskEditor.FormDestroy(Sender: TObject);
begin
    {$IFDEF FPC}
    Designer := nil;
    {$ENDIF}
end;

procedure TfrmTaskEditor.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
    AAction := caFree;
    frmTaskEditor := nil;
end;

function TfrmTaskEditor.GetSelectedTask: TTask;
begin
    if Assigned(lvTasks.Selected) then
        result := TObject(lvTasks.Selected.Data) as TTask
    else
        result := nil;
end;

{$IFDEF FPC}
procedure TfrmTaskEditor.SetDesigner(AValue: TComponentEditorDesigner);
begin
    if FDesigner=AValue then Exit;
    FDesigner:=AValue;
end;
{$ENDIF}

{$IFDEF FPC}
{$ELSE}
procedure TfrmTaskEditor.ItemsModified(const Designer: IDesigner);
begin
    UpdateList;
end;
{$ENDIF}

procedure TfrmTaskEditor.lvTasksDblClick(Sender: TObject);
begin
    if Assigned(SelectedTask) then
{$IFDEF FPC}
        Designer.InvokeComponentEditor(SelectedTask);
{$ELSE}
        Designer.Edit(SelectedTask);
{$ENDIF}
    Close;
end;

procedure TfrmTaskEditor.lvTasksEdited(Sender: TObject; Item: TListItem; var S: string);
begin
    if Assigned(SelectedTask) then begin
        SelectedTask.Name := S;
        Modified;
    end;
end;

procedure TfrmTaskEditor.lvTasksSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
    if Selected then
        SetSelection;
end;

procedure TfrmTaskEditor.SetComponent(const Value: TWCThread);
begin
    FComponent := Value;
    {$IFDEF FPC}
    //FDesigner := FindRootDesigner(FComponent);
    {$ENDIF}
    UpdateList;
end;

{$IFDEF FPC}
function TfrmTaskEditor.GetDesigner: TComponentEditorDesigner;
begin
    result := FDesigner;
end;
{$ENDIF}

procedure TfrmTaskEditor.SetSelection;
begin
    if FSelectionError then Exit;
    try
        if Assigned(SelectedTask) then begin
            {$IFDEF FPC}
            GlobalDesignHook.SelectOnlyThis(SelectedTask);
            {$ELSE}
            Designer.SelectComponent(SelectedTask);
            {$ENDIF}
        end;
    except
        FSelectionError := True;
        Application.HandleException(ExceptObject);
        frmTaskEditor.Free;
        frmTaskEditor := nil;
    end;
end;

procedure TfrmTaskEditor.Modified;
begin
    if Assigned(Designer) then
        Designer.Modified;
end;

procedure TfrmTaskEditor.OnModified(Sender: TObject);
begin
    UpdateList;
end;

procedure TfrmTaskEditor.UpdateList;
var s, i: integer;
begin
    s := lvTasks.ItemIndex;
    try
        lvTasks.Items.BeginUpdate;
        lvTasks.Clear;
        for I := 0 to FComponent.Tasks.Count-1 do
            lvTasks.AddItem(FComponent.Task[i].Name, FComponent.Task[i]);
        if (s >= 0) and (s < lvTasks.Items.Count) then
            lvTasks.ItemIndex := s;
    finally
        lvTasks.Items.EndUpdate;
    end;
end;

{ TTasksEditor }

procedure TTasksEditor.Edit;
var list: TOwnedList;
{$IFDEF FPC}
    Temp: TPersistent;
    IDesigner: TIDesigner;
    Designer: TComponentEditorDesigner;
    AComponent: TComponent;
{$ENDIF}
begin
    list := TObject(PtrInt(GetOrdValue)) as TOwnedList;
    {$IFDEF FPC}
    Designer := nil;
    Temp := list.Owner;
    if Temp is TComponent then begin
        AComponent := TComponent(Temp);
        IDesigner := FindRootDesigner(AComponent);
        if IDesigner is TComponentEditorDesigner then
            Designer := IDesigner as TComponentEditorDesigner;
    end;
    ShowTasksEditor(Designer, list.Owner);
    {$ELSE}
    ShowTasksEditor(Designer, list.Owner);
    {$ENDIF}
end;

function TTasksEditor.GetAttributes: TPropertyAttributes;
begin
    Result := [paDialog];
end;

function TTasksEditor.GetValue: string;
begin
    Result := '(Tasks)';
end;

initialization
{$IFDEF FPC}
    //Hook := TLazarusHook.Create;
    //Hook.Init;
{$ENDIF}

finalization
{$IFDEF FPC}
    //Hook.Done;
    //Hook.Free;
{$ENDIF}

end.
