{**********************************************}
{                                              }
{              Eduardo Rodrigues               }
{                 11/09/2019                   }
{                                              }
{**********************************************}
unit FormConfig;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.IOUtils,
  System.Win.Registry,
  System.IniFiles,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.TreeView,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation,
  FMX.Layouts;

type
  TfrmConfig = class(TForm)
    tvINI: TTreeView;
    pnlTop: TPanel;
    btnAdicionar: TButton;
    edtVCL: TEdit;
    edtFMX: TEdit;
    lbEqual: TLabel;
    btnRemover: TButton;
    Panel1: TPanel;
    edtINI: TEdit;
    btnAbrir: TButton;
    btnSalvar: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnAdicionarClick(Sender: TObject);
    procedure btnRemoverClick(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure btnAbrirClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvINIClick(Sender: TObject);
  end;

implementation

uses
  FormMain;

{$R *.fmx}

procedure TfrmConfig.btnAbrirClick(Sender: TObject);
var
  Dlg: TOpenDialog;
  RegFile: TRegistryIniFile;
begin
  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.FileName := TPath.GetFileName(edtINI.Text);
    Dlg.InitialDir := TPath.GetDirectoryName(edtINI.Text);
    Dlg.DefaultExt := '.ini';
    Dlg.Filter := 'INI Files|*.ini|All Files|*.*';
    if Dlg.Execute then
    begin
      RegFile := TRegistryIniFile.Create(RegKey);
      try
        RegFile.WriteString('Files', 'inifile', Dlg.FileName);
      finally
        FreeAndNil(RegFile);
      end;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TfrmConfig.btnAdicionarClick(Sender: TObject);
var
  tvSec: TTreeViewItem;
begin
  if not Assigned(tvINI.Selected) then Exit;

  tvSec := TTreeViewItem.Create(tvINI);
  tvSec.Text := edtVCL.Text + '=' + edtFMX.Text;
  if tvINI.Selected.Level = 1
  then tvINI.Selected.AddObject(tvSec)
  else tvINI.Selected.ParentItem.AddObject(tvSec);
end;

procedure TfrmConfig.btnRemoverClick(Sender: TObject);
begin
  if not Assigned(tvINI.Selected) then Exit;

  if tvINI.Selected.Level = 1 then
    tvINI.RemoveObject(tvINI.Selected)
  else
    tvINI.Selected.ParentItem.RemoveObject(tvINI.Selected);
end;

procedure TfrmConfig.btnSalvarClick(Sender: TObject);
var
  RegFile: TRegistryIniFile;
  Ini: TIniFile;
  IniFileName, Key, Value: String;
  I, J: Integer;
begin
  tvINI.Sorted := True;
  RegFile := TRegistryIniFile.Create(RegKey);
  try
    IniFileName := RegFile.ReadString('Files', 'Inifile', '');
    if FileExists(IniFileName) then
    begin
      DeleteFile(ChangeFileExt(IniFileName, '.bkp'));
      RenameFile(IniFileName, ChangeFileExt(IniFileName, '.bkp'));
    end;

    Ini := TIniFile.Create(IniFileName);
    try
      for I := 0 to tvINI.Count - 1 do
      begin
        if (tvINI.Items[I].Count = 0) or tvINI.Items[I].Text.Trim.IsEmpty then
          Continue;
        for J := 0 to tvINI.Items[I].Count - 1 do
        begin
          Key   := Copy(tvINI.Items[I].Items[J].Text, 1, Pos('=', tvINI.Items[I].Items[J].Text) - 1);
          Value := Copy(tvINI.Items[I].Items[J].Text, Pos('=', tvINI.Items[I].Items[J].Text) + 1, MaxInt);
          Ini.WriteString(tvINI.Items[I].Text, Key, Value);
        end;
      end;
    finally
      FreeAndNil(Ini);
    end;
  finally
    FreeAndNil(RegFile);
  end;
end;

procedure TfrmConfig.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TfrmConfig.FormCreate(Sender: TObject);
var
  RegFile: TRegistryIniFile;
  Ini: TIniFile;
  IniFileName, ClassName, Item: String;
  IniSections, IniValues: TStringList;
  tvObj, tvSec: TTreeViewItem;
begin
  RegFile := TRegistryIniFile.Create('DFMtoFMXConvertor');
  try
    IniFileName := RegFile.ReadString('Files', 'Inifile', '');
  finally
    FreeAndNil(RegFile);
  end;
  edtINI.Text := IniFileName;

  Ini := TIniFile.Create(IniFileName);
  try
    IniSections := TStringList.Create;
    try
      Ini.ReadSections(IniSections);
      for ClassName in IniSections do
      begin
        tvObj := TTreeViewItem.Create(tvINI);
        tvObj.Text := ClassName;
        IniValues := TStringList.Create;
        try
          Ini.ReadSectionValues(ClassName, IniValues);
          for Item in IniValues do
          begin
            tvSec := TTreeViewItem.Create(tvObj);
            tvSec.Text := Item;
            tvObj.AddObject(tvSec);
          end;
        finally
          FreeAndNil(IniValues);
        end;
        tvINI.AddObject(tvObj);
      end;
    finally
      FreeAndNil(IniSections);
    end;
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TfrmConfig.tvINIClick(Sender: TObject);
var
  PosEqual: Integer;
begin
  if not Assigned(tvINI.Selected) then Exit;

  PosEqual := Pos('=', tvINI.Selected.Text);
  if PosEqual > 0 then
  begin
    edtVCL.Text := Copy(tvINI.Selected.Text, 1, PosEqual - 1);
    edtFMX.Text := Copy(tvINI.Selected.Text, PosEqual + 1, MaxInt);
  end;
end;


end.
