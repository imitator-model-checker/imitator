#!/bin/sh
#
# Build the internal odoc API reference and make the HTML root open directly
# on the IMITATOR package page.
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$repo_root"

dune build @doc-private _build/default/_doc/_html/IMITATOR/index.html

html_dir="_build/default/_doc/_html"
index="$html_dir/index.html"
package_page="IMITATOR/index.html"
bundle_dir="doc/api/html"

tmp=$(mktemp "$index.XXXXXX")
trap 'rm -f "$tmp"' EXIT

cat > "$tmp" <<EOF
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>IMITATOR API reference</title>
    <meta charset="utf-8"/>
    <meta http-equiv="refresh" content="0; url=$package_page"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <link rel="canonical" href="$package_page"/>
    <script>window.location.replace("$package_page");</script>
  </head>
  <body>
    <p><a href="$package_page">Open the IMITATOR API reference</a>.</p>
  </body>
</html>
EOF

mv "$tmp" "$index"
trap - EXIT

rm -rf "$bundle_dir"
mkdir -p "$bundle_dir"
cp -R "$html_dir/." "$bundle_dir/"

echo "Bundled API reference in $bundle_dir"
