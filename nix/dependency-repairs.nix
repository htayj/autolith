# The JSON is a checked projection of the readable source manifest. Its full
# records are compared with that manifest by the finite integration fixture.
{ pkgs, src }:
let
  lib = pkgs.lib;
  root = "${src}/patches/dependencies";
  data = builtins.fromJSON (builtins.readFile "${root}/build-manifest.json");
  identity = builtins.hashFile "sha256" "${root}/manifest.sexp";
  guard = provider: direction:
    let
      records = data.providers.${provider}.files;
      sums = pkgs.writeText "${provider}-${direction}-${identity}.sha256"
        (lib.concatMapStrings (record: "${record.${direction}}  ${record.path}\n") records);
      members = pkgs.writeText "${provider}-members-${identity}"
        (lib.concatMapStrings (record: "${record.path}\n") records);
    in ''
      # No link may substitute for any authenticated upstream input.
      test -z "$(${pkgs.findutils}/bin/find . -type l -print -quit)"
      ${pkgs.findutils}/bin/find . -type f -printf '%P\n' | LC_ALL=C ${pkgs.coreutils}/bin/sort > "$TMPDIR/autolith-${provider}-members"
      ${pkgs.diffutils}/bin/diff -u ${members} "$TMPDIR/autolith-${provider}-members"
      ${pkgs.coreutils}/bin/sha256sum --check ${sums}
    '';
  protocolSums = pkgs.writeText "cffi-protocol-${identity}.sha256"
    (lib.concatMapStrings (record: "${record.input}  ${record.path}\n") data.protocolFiles);
  protocolGuard = ''
    ${lib.concatMapStrings (record: ''
      test "$(${pkgs.coreutils}/bin/readlink -f ${lib.escapeShellArg record.path})" = "$PWD/${record.path}"
    '') data.protocolFiles}
    ${pkgs.coreutils}/bin/sha256sum --check ${protocolSums}
  '';
  overrideCffi = old: {
    prePatch = protocolGuard + "\n" + (old.prePatch or "");
    postPatch = (old.postPatch or "") + "\n" + protocolGuard;
    passthru = (old.passthru or {}) // { autolithDependencyRepair = identity; };
  };
  override = provider: old:
    let
      record = data.providers.${provider};
      patch = "${root}/${record.patch}";
    in assert builtins.hashFile "sha256" patch == record.patchSha256; {
      # Retain the upstream source, patch list, flags and both hooks.
      prePatch = guard provider "input" + "\n" + (old.prePatch or "");
      postPatch = (old.postPatch or "") + "\n" + ''
        ${pkgs.patch}/bin/patch --batch --forward --fuzz=0 --no-backup-if-mismatch \
          --reject-file=- -p1 -i ${patch}
      '' + "\n" + guard provider "output";
      passthru = (old.passthru or {}) // {
        autolithDependencyRepair = identity;
      };
    };
in assert identity == data.manifestSha256; {
  inherit identity override overrideCffi;
}
