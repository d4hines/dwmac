// swift-tools-version: 6.2
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "DwmacPackage",
    // Runtime support for parameterized protocol types is only available in macOS 13.0.0 or newer
    // And it specifies deploymentTarget for CLI
    // (Lean's runtime, linked by CDwmacCore, is built for macOS 14)
    platforms: [.macOS(.v14)],
    // Products define the executables and libraries a package produces, making them visible to other packages.
    products: [
        .executable(name: "dwmac", targets: ["Cli"]),
        // Don't use this build for release, use xcode instead
        .executable(name: "DwmacApp", targets: ["DwmacApp"]),
        // We only need to expose this as a product for xcode
        .library(name: "AppBundle", targets: ["AppBundle"]),
    ],
    dependencies: [
        .package(url: "https://github.com/InerziaSoft/ISSoundAdditions.git", exact: "2.0.1"),
        .package(url: "https://github.com/LebJe/TOMLKit.git", exact: "0.5.5"),
        .package(url: "https://github.com/apple/swift-collections.git", exact: "1.3.0"),
        .package(url: "https://github.com/soffes/HotKey.git", exact: "0.2.1"),
    ],
    // Targets are the basic building blocks of a package, defining a module or a test suite.
    // Targets can depend on other targets in this package and products from dependencies.
    targets: [
        // Exposes the private _AXUIElementGetWindow function to swift
        .target(
            name: "PrivateApi",
            path: "Sources/PrivateApi",
            publicHeadersPath: "include",
        ),
        // Formally verified algorithms: Lean (Lean/DwmacCore) compiled to C, behind a plain C API. generated/ is
        // filled from Lean's build by overlays/dwmac (dev.sh, package.nix), which also pass Lean's include path and
        // runtime libraries
        .target(
            name: "CDwmacCore",
            path: "Sources/CDwmacCore",
            publicHeadersPath: "include",
            cSettings: [
                // Lean's generated code relies on wrapping integer overflow (leanc --print-cflags)
                .unsafeFlags(["-fwrapv", "-Wno-unused-parameter", "-Wno-unused-label", "-Wno-unused-but-set-variable"]),
            ],
        ),
        .target(
            name: "Common",
            dependencies: [
                .product(name: "Collections", package: "swift-collections"),
            ],
        ),
        .target(
            name: "AppBundle",
            dependencies: [
                .product(name: "Collections", package: "swift-collections"),
                .product(name: "HotKey", package: "HotKey"),
                .product(name: "ISSoundAdditions", package: "ISSoundAdditions"),
                .product(name: "TOMLKit", package: "TOMLKit"),
                .target(name: "CDwmacCore"),
                .target(name: "Common"),
                .target(name: "PrivateApi"),
            ],
            swiftSettings: [
                .enableUpcomingFeature("NonisolatedNonsendingByDefault"),
            ],
        ),
        .executableTarget(
            name: "DwmacApp",
            dependencies: [
                .target(name: "AppBundle"),
            ],
        ),
        .executableTarget(
            name: "Cli",
            dependencies: [
                .target(name: "Common"),
                .target(name: "CDwmacCore"), // Lean parses the command line (help and errors work without the server)
            ],
        ),
        .testTarget(
            name: "AppBundleTests",
            dependencies: [
                .target(name: "AppBundle"),
            ],
            path: "test/AppBundleTests",
        ),
    ],
)
