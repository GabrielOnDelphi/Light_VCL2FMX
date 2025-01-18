unit FormMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Win.Registry,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Memo,
  Parser,
  FMX.StdCtrls,
  FMX.ScrollBox,
  FMX.Controls.Presentation,
  FMX.Objects, FMX.Memo.Types, FMX.Menus;

type
  TfrmMain = class(TForm)
    mmOutput: TMemo;
    mmInput: TMemo;
    dlgOpen: TOpenDialog;
    dlgSalvar: TSaveDialog;
    Layout1: TLayout;
    Layout2: TLayout;
    btnConfig: TButton;
    BtnOpenFile: TButton;
    BtnProcess: TButton;
    BtnSaveFMX: TButton;
    procedure BtnOpenFileClick(Sender: TObject);
    procedure BtnProcessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnSaveFMXClick(Sender: TObject);
    procedure btnConfigClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    DFMObj: TDfmToFmxObject;
    ConfigFile: String;
    FInPasFileName: String;
    InputDFM: String;
    OutputPas: String;
    FOutFmxFileName: String;
    function GetRegFile: TRegistryIniFile;
    Procedure RegIniLoad;
    Procedure RegIniSave;
    Procedure UpdateForm;
    function MyMessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn): Integer;
  end;

var
  frmMain: TfrmMain;

IMPLEMENTATION
{$R *.fmx}

USES
  Utils,
  FormConfig,
  FMX.DialogService;



procedure TfrmMain.FormCreate(Sender: TObject);
begin
  RegIniLoad;
  if NOT FileExists(ConfigFile)
  then ConfigFile := 'ConversionConfig.ini';
  UpdateForm;
end;


procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RegIniSave;
end;



procedure TfrmMain.btnConfigClick(Sender: TObject);
begin
  TFrmConfig.Create(Self).ShowModal;
  RegIniLoad;
end;


procedure TfrmMain.BtnOpenFileClick(Sender: TObject);
begin
  BtnProcess.Enabled := False;
  FreeAndNil(DFMObj);
  if InputDFM <> ''
  then dlgOpen.InitialDir := ExtractFileDir(InputDFM);
  if dlgOpen.Execute then
  begin
    FInPasFileName := ChangeFileExt(dlgOpen.FileName, '.pas');
    InputDFM := ChangeFileExt(FInPasFileName, '.dfm');
    if FileExists(InputDFM) then
    begin
      if TDfmToFmxObject.DFMIsTextBased(InputDFM) then
      begin
        mmInput.Lines.Clear;
        mmInput.Lines.LoadFromFile(InputDFM);
        BtnProcess.Enabled := True;
      end
      else
        raise Exception.Create('Incompatible dfm file:' + InputDFM);
    end;
  end;
  UpdateForm;
end;


procedure TfrmMain.BtnProcessClick(Sender: TObject);
var
  Data: String;
  Stm: TStringStream;
begin
  if mmInput.Text <> '' then
  begin
    FreeAndNil(DFMObj);
    Data := mmInput.Text;
    Stm := TStringStream.Create;
    Stm.LoadFromFile(InputDFM);
    Stm.Seek(0,soFromBeginning);
    try
      Data := Trim(ReadLineFrmStream(Stm));
      if Pos('object', Data) = 1
      then DFMObj := TDfmToFmxObject.Create(Data, Stm, 0);
    finally
      Stm.Free;
    end;
  end;

  DFMObj.LiveBindings;

  DFMObj.LoadInfileDefs(ConfigFile);
  mmOutput.Text := '';
  mmOutput.Text := DFMObj.FMXFile;
  BtnProcess.Enabled := False;
  UpdateForm;
end;


function TfrmMain.MyMessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn): Integer;
var mr: TModalResult;
begin
  mr := mrNone;

  TDialogService.MessageDialog(
    AMessage,
    ADialogType,
    AButtons,
    ADefaultButton,
    0,
    procedure (const AResult: TModalResult)
    begin
      mr := AResult
    end
  );

  while mr = mrNone do // wait for modal result
    Application.ProcessMessages;

  Result := mr;
end;


procedure TfrmMain.BtnSaveFMXClick(Sender: TObject);
begin
  if DFMObj = nil then
    UpdateForm
  else
  begin
    OutputPas := ExtractFilePath(OutputPas) + ChangeFileExt(ExtractFileName(InputDFM), 'FMX.pas');

    if not OutputPas.IsEmpty then
    begin
      dlgSalvar.InitialDir := ExtractFileDir(OutputPas);
      dlgSalvar.FileName   := ExtractFileName(ChangeFileExt(OutputPas, '.fmx'));
    end;

    if dlgSalvar.Execute then
    begin
      OutputPas := ChangeFileExt(dlgSalvar.FileName, '.pas');
      FOutFmxFileName := ChangeFileExt(OutputPas, '.fmx');

      if FileExists(FOutFmxFileName) or FileExists(OutputPas) then
        if myMessageDialog('Replace Existing Files: '+ FOutFmxFileName +' e/ou '+ OutputPas,
          TMsgDlgType.mtWarning,
          [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel],
          TMsgDlgBtn.mbOK) = mrOk then
        begin
          DeleteFile(FOutFmxFileName);
          DeleteFile(OutputPas);
        end;

      if FileExists(FOutFmxFileName) then
        raise Exception.Create(FOutFmxFileName + 'Já existe');

      DFMObj.WriteFMXToFile(FOutFmxFileName);

      if FileExists(OutputPas) then
        raise Exception.Create(OutputPas + 'Já existe');

      DFMObj.WritePasToFile(OutputPas, FInPasFileName);
    end;
  end;
end;


function TfrmMain.GetRegFile: TRegistryIniFile;
begin
  Result:= TRegistryIniFile.Create('DFMtoFMXConvertor');
end;







procedure TfrmMain.RegIniLoad;
Var
  RegFile: TRegistryIniFile;
begin
  RegFile := GetRegFile;
  try
    ConfigFile:= RegFile.ReadString('Files', 'ConfigFile',   '');
    InputDFM  := RegFile.ReadString('Files', 'InputDfm',  '');
    OutputPas := RegFile.ReadString('Files', 'OutputPas', '');

    if InputDFM <> ''
    then FInPasFileName := ChangeFileExt(InputDFM, '.pas');

    if OutputPas <> '' then
    begin
      FOutFmxFileName := ChangeFileExt(OutputPas, '.fmx');
      dlgOpen.InitialDir := ExtractFileDir(OutputPas);
    end;

    if FileExists(InputDFM)
    AND TDfmToFmxObject.DFMIsTextBased(InputDFM) then
    begin
      mmInput.Lines.Clear;
      mmInput.Lines.LoadFromFile(InputDFM);
    end;
  finally
    RegFile.Free;
  end;
end;


procedure TfrmMain.RegIniSave;
Var
  RegFile: TRegistryIniFile;
begin
  RegFile := GetRegFile;
  try
    RegFile.WriteString('Files', 'ConfigFile', ConfigFile);
    RegFile.WriteString('Files', 'InputDFm', InputDFM);
    RegFile.WriteString('Files', 'OutputPas', OutputPas);
  finally
    RegFile.Free;
  end;
end;


procedure TfrmMain.UpdateForm;
begin
  BtnSaveFMX.Visible := DFMObj <> nil;
end;


end.
