# IMITATOR documentation

IMITATOR ships three different kinds of documentation, each in its own folder.

| Folder              | Audience            | Format                | Output            |
| ------------------- | ------------------- | --------------------- | ----------------- |
| `user-guide/`       | End users           | LaTeX                 | PDF               |
| `developer-manual/` | Contributors        | LaTeX + Markdown      | PDF (+ Markdown)  |
| `api/`              | Contributors        | odoc (from code)      | HTML              |

Shared LaTeX assets used by *both* manuals (preamble, title page,
bibliography, images) live in `shared/`, and the build scripts live in
`scripts/`.

```
doc/
├── user-guide/         end-user manual (IMITATOR-user-manual.tex) + examples
├── developer-manual/   developer manual (IMITATOR-developer-manual.tex + .md) + UML diagrams
├── api/                odoc landing page for the generated API reference
├── shared/             LaTeX files and images shared by both manuals
└── scripts/            build scripts
```

## User guide and developer manual (PDF)

These are LaTeX documents. Building them requires a TeX distribution with
`pdflatex` and `biber`; the developer manual additionally needs `m4` and
Graphviz (`dot`) to generate its UML class diagrams.

```sh
doc/scripts/build-user-manual.sh        # -> doc/user-guide/IMITATOR-user-manual.pdf
doc/scripts/build-developer-manual.sh   # -> doc/developer-manual/IMITATOR-developer-manual.pdf
doc/scripts/build-all.sh                # both
```

The scripts make the `shared/` assets reachable automatically (via
`TEXINPUTS`/`BIBINPUTS`), so the manuals can be built from anywhere.

## API reference (HTML)

The API reference is generated from the documentation comments in the source
code with [odoc](https://ocaml.github.io/odoc/). From the repository root:

```sh
opam install odoc
doc/scripts/build-api-reference.sh
```

Then open `api/html/index.html`; it redirects directly to the IMITATOR package
page. The script builds the `@doc-private` alias (rather than `@doc`) because
IMITATOR's libraries are private, then copies the generated HTML from
`_build/default/_doc/_html/` into `api/html/`. See `api/index.mld` for the
landing-page text.
