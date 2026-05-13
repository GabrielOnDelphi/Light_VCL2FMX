{**********************************************}
{                                              }
{              Eduardo Rodrigues               }
{                 18/09/2019                   }
{                                              }
{**********************************************}
unit ParseImage;

interface

uses
  Vcl.Graphics,
  FMX.Objects;

type
  TGraphicAccess = class(Vcl.Graphics.TGraphic)
  end;

function ProcessImage(sData, APad: String): String;

function ImageDFMtoFMX(sData: String): String;

function ImageToHex(Image:FMX.Objects.TImage; LineLen:integer): String;

implementation

uses
  System.Classes,
  System.SysUtils,
  Vcl.Imaging.Jpeg,
  Vcl.Imaging.GIFImg,
  Vcl.Imaging.PngImage;

var
  FPad: string;

function ProcessImage(sData, APad: String): String;
begin
  FPad := APad;
  Result :=    APad +'  MultiResBitmap = <'+
  sLineBreak + APad +'    item '+
  sLineBreak + APad +'      PNG = { '+ ImageDFMtoFMX(sData) +'} '+
  sLineBreak + APad +'    end>';
end;

function ImageDFMtoFMX(sData: String): String;
var
  Linput: String;
  Loutput: TMemoryStream;
  LclsName: ShortString;
  Lgraphic: TGraphic;
  img1: FMX.Objects.TImage;
  stream: TMemoryStream;
begin
  // Remove the surrounding braces of the DFM hex blob
  sData := StringReplace(sData, '{', EmptyStr, []);
  sData := StringReplace(sData, '}', EmptyStr, []);

  Linput  := sData;
  Loutput := TMemoryStream.Create;
  try
    // Decode HEX into memory. The DFM Picture.Data layout is:
    //   <short-string class name><graphic payload>
    Loutput.Size := Length(Linput) div 2;
    HexToBin(PChar(Linput), Loutput.Memory^, Loutput.Size);
    LclsName := PShortString(Loutput.Memory)^;

    // Build the FMX target image
    img1 := FMX.Objects.TImage.Create(nil);
    try
      // Instantiate the matching VCL TGraphic descendant for the class name we just read
      Lgraphic := TGraphicClass(FindClass(UTF8ToString(LclsName))).Create;
      try
        // Read the graphic payload past the short-string class name header
        Loutput.Position := 1 + Length(LclsName);
        TGraphicAccess(Lgraphic).ReadData(Loutput);

        // Round-trip VCL → FMX via a memory stream. We can use the VCL TGraphic directly;
        // the intermediate Vcl.ExtCtrls.TImage from the original fork was unnecessary and leaked.
        stream := TMemoryStream.Create;
        try
          Lgraphic.SaveToStream(stream);
          stream.Position := 0;
          img1.Bitmap.LoadFromStream(stream);
        finally
          FreeAndNil(stream);
        end;

        Result := ImageToHex(img1, 64);
      finally
        FreeAndNil(Lgraphic);
      end;
    finally
      FreeAndNil(img1);
    end;
  finally
    FreeAndNil(Loutput);
  end;
end;

function ImageToHex(Image:FMX.Objects.TImage; LineLen:integer): String;
var
  ms:TMemoryStream;
  s:String;
  t:Ansistring;
begin
  ms := TMemoryStream.Create;
  try
    image.Bitmap.SaveToStream(ms);
    SetLength(t, ms.Size * 2);
    BinToHex(ms.Memory^, Pansichar(t), ms.Size);
    repeat
      s := Copy(String(t), 1, LineLen);
      Result := Result + sLineBreak + FPad +'        '+ s;
      Delete(t, 1, LineLen);
    until t = '';
  finally
    FreeAndNil(ms);
  end;
end;

initialization
  System.Classes.RegisterClass(TMetafile);
  System.Classes.RegisterClass(TIcon);
  System.Classes.RegisterClass(TBitmap);
  System.Classes.RegisterClass(TWICImage);
  System.Classes.RegisterClass(TJpegImage);
  System.Classes.RegisterClass(TGifImage);
  System.Classes.RegisterClass(TPngImage);

end.
