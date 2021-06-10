# Introduction
This document contains development notes about the `mimedb` library.

# Versioning
The following `mimedb` versions are available:
- `0.y.z` unstable versions.
- `x.y.z` stable versions: `mimedb` will maintain reasonable backward
  compatibility, deprecating features before removing them.
- Experimental untagged versions.

Developers who use unstable or experimental versions are responsible for
updating their application when `mimedb` is modified. Note that
unstable versions can be modified without backward compatibility at any
time.

# Supported
The library implement a subset of the [version
0.21](https://specifications.freedesktop.org/shared-mime-info-spec/shared-mime-info-spec-0.21.html)
of the [Shared MIME Info
Specification](https://freedesktop.org/wiki/Specifications/shared-mime-info-spec/).

List of the not supported features:
- Weight on the mimetype extension
- Real glob maching on the mimetype extension
- Match by magic byte
