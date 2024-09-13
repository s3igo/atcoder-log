final: prev: {
  time = prev.time.overrideAttrs (oldAttrs: {
    # Rename the binary from 'time' to 'gtime'
    postInstall =
      oldAttrs.postInstall or ""
      + ''
        mv $out/bin/time $out/bin/gtime
      '';
  });
}
