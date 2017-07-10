uses LCLVersion;

{$IF (lcl_fullversion>=1060000) and (lcl_fullversion<1070000)}
{$define ver16}
{$ENDIF}
{$If (lcl_fullversion>=1070000) and (lcl_fullversion<1080000)}
{$define ver17}
{$ENDIF}
{$If (lcl_fullversion>=1080000) and (lcl_fullversion<1090000)}
{$define ver18}
{$ENDIF}
{$If (lcl_fullversion>=1090000) and (lcl_fullversion<2000000)}
{$define ver19}
{$ENDIF}

