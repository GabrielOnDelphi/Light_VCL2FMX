procedure TParser.UpdateUsesStringList(UsesList: TStrings);
var
  I, Idx: Integer;
  UnitName: String;
begin
  // Replace existing units based on IniReplaceValues
  for I := UsesList.Count - 1 downto 0 do
  begin
    UnitName := Trim(UsesList[I]);
    Idx := IniReplaceValues.IndexOfName(UnitName);
    if Idx >= 0 then
    begin
      // Replace with the mapped unit, or remove if mapped to empty
      if IniReplaceValues.ValueFromIndex[Idx].IsEmpty then
        UsesList.Delete(I)
      else
        UsesList[I] := IniReplaceValues.ValueFromIndex[Idx];
    end
    else if UnitName.StartsWith('Vcl.', True) then
    begin
      // Remove VCL units not explicitly replaced
      UsesList.Delete(I);
    end;
  end;

  // Remove any remaining empty entries
  for I := UsesList.Count - 1 downto 0 do
    if Trim(UsesList[I]).IsEmpty then
      UsesList.Delete(I);

  // Add new units from IniIncludeValues, avoiding duplicates
  if Assigned(IniIncludeValues) then
    for I := 0 to IniIncludeValues.Count - 1 do
      if (not IniIncludeValues[I].IsEmpty) and 
         (UsesList.IndexOf(IniIncludeValues[I]) < 0) and 
         (IniIncludeValues[I] <> 'Empty Include') then
        UsesList.Add(IniIncludeValues[I]);

  // Handle owned objects recursively
  if Assigned(OwnedObjs) then
    for I := 0 to OwnedObjs.Count - 1 do
      if OwnedObjs[I] is TParser then
        TParser(OwnedObjs[I]).UpdateUsesStringList(UsesList);
end;