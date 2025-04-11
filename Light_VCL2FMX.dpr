program Light_VCL2FMX;

uses
  FastMM4,
  System.StartUpCopy,
  FMX.Forms,
  LightFMX.AppData,
  Parser in 'Parser.pas',
  FormMain in 'FormMain.pas' {frmMain},
  Utils in 'Utils.pas',
  FormConfig in 'FormConfig.pas' {frmConfig},
  ParseImage in 'ParseImage.pas',
  ParseImageList in 'ParseImageList.pas',
  ccINIFile in 'C:\Projects\LightSaber\ccINIFile.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := FALSE;

  AppData:= TAppData.Create('Light Vcl2Fmx');
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE, asFull);       // Main form
  AppData.Run;
end.



