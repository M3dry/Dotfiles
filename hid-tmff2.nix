{ stdenv, lib, fetchFromGitHub, kernel, kmod }:

stdenv.mkDerivation rec {
	name = "hid-tmff2-${kernel.version}";
	version = "0.8";

	src = fetchFromGitHub {
		owner = "Kimplul";
		repo = "hid-tmff2";
		rev = "bb4817e4c999f137cae02d9c739580e43b644972";
		hash = "sha256-fUXBE0lG7bNwJahKLtSYPb5zTTSJvErj5RKbwjyBMHM=";
		fetchSubmodules = true;
	};

	nativeBuildInputs = kernel.moduleBuildDependencies ++ [ kmod ];

	makeFlags = kernel.makeFlags ++ [
		"KDIR=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
	];

	installFlags = [
		"INSTALL_MOD_PATH=${placeholder "out"}"
	];

	meta = with lib; {
		description = "A linux kernel module for Thrustmaster T300RS and T248";
		homepage = "https://github.com/Kimplul/hid-tmff2";
		license = licenses.gpl3;
		maintainers = [ maintainers.rayslash ];
		platforms = platforms.linux;
	};
}
