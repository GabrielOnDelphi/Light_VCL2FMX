program Light_VCL2FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  CvtrObj in 'CvtrObj.pas',
  FormMain in 'FormMain.pas' {frmMain},
  PatchLib in 'PatchLib.pas',
  FormConfig in 'FormConfig.pas' {frmConfig},
  Image in 'Image.pas',
  ImageList in 'ImageList.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := False;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
