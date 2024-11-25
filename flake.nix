{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      formatter.${system} = pkgs.nixpkgs-fmt;
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          python3Packages.black
          (python3.withPackages (ps: [
            ps.beautifulsoup4
            ps.docopt
            ps.jinja2
          ]))
        ];
      };
    };
}
