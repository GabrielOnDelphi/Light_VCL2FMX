{**********************************************}
{                                              }
{              Eduardo Rodrigues               }
{                 11/09/2019                   }
{                                              }
{**********************************************}
unit FormConfig;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.TreeView,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Edit;

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
  System.Win.Registry,
  System.IniFiles,
  System.StrUtils,
  FormMain;

{$R *.fmx}

procedure TfrmConfig.btnAbrirClick(Sender: TObject);
var
  Dlg: TOpenDialog;
  RegFile: TRegistryIniFile;
begin
  Dlg := TOpenDialog.Create(Self);
  try
    Dlg.FileName := ExtractFileName(edtINI.Text);
    Dlg.InitialDir := ExtractFilePath(edtINI.Text);
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
  tvSec.Text := edtVCL.Text +'='+ edtFMX.Text;
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
  sIniFile: String;
  sKey: String;
  sValue: String;
  I: Integer;
  J: Integer;
begin
  tvINI.Sorted := True;
  RegFile := TRegistryIniFile.Create(RegKey);
  try
    sIniFile := RegFile.ReadString('Files', 'Inifile', EmptyStr);
    DeleteFile(ChangeFileExt(sIniFile, '.bkp'));
    System.SysUtils.RenameFile(sIniFile, ChangeFileExt(sIniFile, '.bkp'));
    Ini := TIniFile.Create(sIniFile);

      try
        for I := 0 to Pred(tvINI.Count) do
        begin
          if (tvINI.Items[I].Count = 0) or tvINI.Items[I].Text.Trim.IsEmpty then
            Continue;
          for J := 0 to Pred(tvINI.Items[I].Count) do
          begin
            sKey := Copy(tvINI.Items[I].Items[J].Text, 1, Pred(Pos('=', tvINI.Items[I].Items[J].Text)));
            sValue := Copy(tvINI.Items[I].Items[J].Text, Succ(Pos('=', tvINI.Items[I].Items[J].Text)));
            Ini.WriteString(tvINI.Items[I].Text, sKey, sValue);
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
  sIniFile: String;
  IniObjectTranslations: TStringList;
  IniSectionValues: TStringList;
  sClass: String;
  sItem: String;
  tvObj: TTreeViewItem;
  tvSec: TTreeViewItem;
begin
  RegFile := TRegistryIniFile.Create('DFMtoFMXConvertor');  //don't put it to   RegKey !
  try
    sIniFile := RegFile.ReadString('Files', 'Inifile', EmptyStr)
  finally
    FreeAndNil(RegFile);
  end;
  edtINI.Text := sIniFile;
  Ini := TIniFile.Create(sIniFile);
  try
    IniObjectTranslations := TStringList.Create;
    try
      Ini.ReadSections(IniObjectTranslations);
      for sClass in IniObjectTranslations do
      begin
        tvObj := TTreeViewItem.Create(tvINI);
        tvObj.Text := sClass;
        IniSectionValues := TStringList.Create;
        try
          Ini.ReadSectionValues(sClass, IniSectionValues);
          for sItem in IniSectionValues do
          begin
            tvSec := TTreeViewItem.Create(tvObj);
            tvSec.Text := sItem;
            tvObj.AddObject(tvSec);
          end;
        finally
          FreeAndNil(IniSectionValues);
        end;
        tvINI.AddObject(tvObj);
      end;
    finally
      FreeAndNil(IniObjectTranslations);
    end;
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TfrmConfig.tvINIClick(Sender: TObject);
begin
  if not Assigned(tvINI.Selected) then
    Exit;

  edtVCL.Text := Copy(tvINI.Selected.Text, 1, Pred(Pos('=', tvINI.Selected.Text)));
  edtFMX.Text := Copy(tvINI.Selected.Text, Succ(Pos('=', tvINI.Selected.Text)));
end;

end.
