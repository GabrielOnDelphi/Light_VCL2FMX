program Light_VCL2FMX;

uses
  FastMM4,
  System.StartUpCopy,
  FMX.Forms,
  cbAppDataFmx,
  Parser in 'Parser.pas',
  FormMain in 'FormMain.pas' {frmMain},
  Utils in 'Utils.pas',
  FormConfig in 'FormConfig.pas' {frmConfig},
  ParseImage in 'ParseImage.pas',
  ParseImageList in 'ParseImageList.pas';

{$R *.res}

begin
 {
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
}
  ReportMemoryLeaksOnShutdown := FALSE;

  AppData:= TAppData.Create('Vcl2Fmx');
  AppData.CreateMainForm(TfrmMain, frmMain, False, True);       // Main form
  Application.Run;
end.



