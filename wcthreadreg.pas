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
    LResources,
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
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure lvTasksEdited(Sender: TObject; Item: TListItem; var S: string);
        procedure lvTasksSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
        procedure FormClose(Sender: TObject; var AAction: TCloseAction);
        procedure lvTasksDblClick(Sender: TObject);
    private
        FOldCaption: string;
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
        procedure UpdateCaption;
        {$ifdef FPC}
        procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
        procedure OnComponentRenamed(AComponent: TComponent);
        procedure OnPersistentDeleting(APersistent: TPersistent);
        {$endif}
    protected
        procedure UpdateList;
        procedure SetSelection;
        procedure Modified;
    public
        {$IFDEF FPC}
        property Designer: TComponentEditorDesigner read FDesigner write SetDesigner;
        {$ELSE}
        procedure ItemInserted(const ADesigner: IDesigner; Item: TPersistent); override;
        procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
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
{$R wcthread.dcr}
{$ENDIF}

type

    {$IFDEF FPC}
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

procedure Register;
begin
    RegisterClass(TTask);
    RegisterNoIcon([TTask]);
    RegisterComponents('System', [TWCThread]);
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
    Component.Tasks.Remove(task);
    {$IFDEF FPC}
    //if Assigned(Designer) then
    //    Designer.PropertyEditorHook.PersistentDeleted{$ifdef ver22}(task){$endif};
    {$ELSE}
    //lvTasks.DeleteSelected;
    {$ENDIF}
    task.Free;
    Modified;
end;

procedure TfrmTaskEditor.ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
begin
    actAdd.Enabled := Assigned(Component);
    actDelete.Enabled := actAdd.Enabled and Assigned(lvTasks.Selected) and (lvTasks.SelCount = 1);
    Handled := true;
end;

procedure TfrmTaskEditor.FormCreate(Sender: TObject);
begin
    {$ifdef FPC}
    if Assigned(GlobalDesignHook) then begin
        GlobalDesignHook.AddHandlerPersistentDeleting(OnPersistentDeleting);
        GlobalDesignHook.AddHandlerPersistentAdded(OnPersistentAdded);
        GlobalDesignHook.AddHandlerComponentRenamed(OnComponentRenamed);
    end;
    {$endif}
end;

procedure TfrmTaskEditor.FormDestroy(Sender: TObject);
begin
    {$IFDEF FPC}
    Designer := nil;
    if Assigned(GlobalDesignHook) then begin
        GlobalDesignHook.RemoveAllHandlersForObject(Self);
    end;
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

procedure TfrmTaskEditor.UpdateCaption;
begin
    if FOldCaption = '' then FOldCaption := Caption;
    if Assigned(Component) then
        Caption := Format('%s = %s', [FOldCaption, Component.Name])
    else
        Caption := FOldCaption;
end;

{$IFDEF FPC}

procedure TfrmTaskEditor.OnPersistentAdded(APersistent: TPersistent; Select: boolean);
begin
     if Assigned(Component) and (APersistent is TTask) and (Component.Tasks.IndexOf(APersistent) >= 0) then begin
        UpdateList;
     end;
end;

procedure TfrmTaskEditor.OnComponentRenamed(AComponent: TComponent);
begin
    if Assigned(Component) then begin
        if AComponent = Component then begin
            UpdateCaption;
        end else if (AComponent is TTask) and (Component.Tasks.IndexOf(AComponent) >= 0) then begin
            lvTasks.Items[Component.Tasks.IndexOf(AComponent)].Caption := (AComponent as TTask).Name;
        end;
    end;
end;

procedure TfrmTaskEditor.OnPersistentDeleting(APersistent: TPersistent);
begin
    if Assigned(Component) then begin
        if APersistent = Component then begin
            Component := nil;
        end else if (APersistent is TTask) and (Component.Tasks.IndexOf(APersistent) >= 0) then begin
            lvTasks.Items.Delete(Component.Tasks.IndexOf(APersistent));
        end;
    end;
end;

procedure TfrmTaskEditor.SetDesigner(AValue: TComponentEditorDesigner);
begin
    if FDesigner=AValue then Exit;
    FDesigner:=AValue;
end;

{$ELSE}

procedure TfrmTaskEditor.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin
    if Assigned(Component) then begin
        if Item = Component then begin
            Component := nil;
        end else if (Item is TTask) and (Component.Tasks.IndexOf(Item) >= 0) then begin
            lvTasks.Items.Delete(Component.Tasks.IndexOf(Item));
        end;
    end;
end;

procedure TfrmTaskEditor.ItemInserted(const ADesigner: IDesigner; Item: TPersistent);
begin
     if Assigned(Component) and (Item is TTask) and (Component.Tasks.IndexOf(Item) >= 0) then begin
        UpdateList;
     end;
end;

procedure TfrmTaskEditor.ItemsModified(const Designer: IDesigner);
begin
    UpdateList;
    UpdateCaption;
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
        {$IFDEF FPC}
        if Assigned(Designer) then
            Designer.PropertyEditorHook.ComponentRenamed(SelectedTask);
        {$ELSE}
        //lvTasks.DeleteSelected;
        {$ENDIF}
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
    UpdateCaption;
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

procedure TfrmTaskEditor.UpdateList;
var s, i: integer;
begin
    s := lvTasks.ItemIndex;
    try
        lvTasks.Items.BeginUpdate;
        lvTasks.Clear;
        if Assigned(Component) then begin
            for I := 0 to Component.Tasks.Count-1 do
                lvTasks.AddItem(Component.Task[i].Name, Component.Task[i]);
            if (s >= 0) and (s < lvTasks.Items.Count) then
                lvTasks.ItemIndex := s;
        end;
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
    Result := [paDialog, paReadOnly];
end;

function TTasksEditor.GetValue: string;
begin
    Result := '(Tasks)';
end;

initialization
{$IFDEF FPC}
    {$I wcthread.lrs}
{$ENDIF}

finalization

end.
