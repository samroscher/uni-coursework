# ML and DL with Python — Coursework

Coursework for the LMU Munich course *Machine Learning and Deep Learning with Python* (Summer 2026).

## Setup

This project was set up using Miniconda and uses a conda environment for reproducibility.

**Create the environment:**

```bash
conda env create -f environment.yml
conda activate lecture_python
```
You can find an `environment.yml` and a `requirements.txt` in the folder. These contain all packages that are needed during the course.

---

### 1. Install Python

Three common routes:

- **python.org** — plain Python interpreter, only `pip` for packages
- **Miniconda** — minimal installer, brings `conda` + `pip` (recommended)
- **Anaconda** — full distribution, bundles Python + many pre-installed data science packages + Spyder

Miniconda is the sweet spot: lightweight, includes both package managers, avoids bloat.

### 2. Package management: `pip` vs `conda`

Python package management is a bit of a mess — there are two coexisting systems.

| | `pip` | `conda` |
|---|---|---|
| Source | PyPI (Python Package Index) | Anaconda repository |
| Format | Source distribution (needs compiler) or wheel | Binary (no compiler needed) |
| Language scope | Python only | Cross-language (C libs, R, etc.) |
| Virtual envs | No built-in support | Built-in |
| Dependency resolution | Recursive, serial loop | SAT solver |
| Package count | ~150k on PyPI | ~1.5k in Anaconda repo |

Wheels are smaller, install faster than source distributions, and require no compiler.

### 3. IDE / Editor choices

- **Spyder** — good introductory IDE, ships with Anaconda
- **VSCode** — lightweight editor, huge plugin ecosystem, great GitHub integration
- **PyCharm** — full-featured IDE, best for large-scale Python projects (Pro version for webdev)

### 4. Python via the console

Start an interactive Python session from the terminal:

```bash
python
```

You get a `>>>` prompt and can execute Python line by line. Exit with `exit()` or `Ctrl+D`.

Useful for quick one-liners:

```bash
python -c "print('Hello World')"
```

### 5. Jupyter Notebooks / Lab

A browser-based web application for writing Python code. Files have the `.ipynb` extension. Useful for exploratory analyses and early project stages where frequent feedback is needed.

- **Code cells** — executable Python (run with `Ctrl+Enter`)
- **Markdown cells** — formatted text, headers, equations, images
- Cloud variant: **Google Colab** runs notebooks without local setup

### 6. Virtual environments

**Scenario:** Project A needs `pkg_xy` v1.2.0, Project B needs v2.1.3. Solution: virtual environments — isolated Python + package stacks, one per project.

Virtual environments allow you to:

- Run different Python versions on the same machine (2.x, 3.x, 3.y, etc.)
- Maintain different package versions per project

**Basic commands (conda):**

```bash
conda create --name some_name python=3.11
conda activate some_name
conda deactivate
conda env list                                # list all envs
conda env remove --name some_name             # delete env
```

Envs live in a central location (e.g. `~/miniconda3/envs/`), **not** inside the project folder — so no `.gitignore` entry is needed for them.

### 7. Package management in practice

**conda:**

```bash
conda install pandas
conda install pandas==1.3.5
conda update pandas
conda remove pandas
conda list
```

**pip:**

```bash
pip install pandas
pip install pandas==1.3.5
pip install pandas --upgrade
pip uninstall pandas
pip freeze
```

**conda version specifiers:**

| Meaning | Example |
|---|---|
| Exact | `conda install pandas==1.3.5` |
| Fuzzy (1.3.0, 1.3.1, ...) | `conda install pandas=1.3` |
| Greater or equal | `conda install "pandas>=1.3.5"` |
| OR (either version) | `conda install "pandas==1.3.4\|1.3.5"` |
| AND (range) | `conda install "pandas>=1.3.3,<1.4"` |

**Rule:** uninstall with whichever tool installed the package. Check the `Channel` column in `conda list`.

### 8. Reproducibility

Isolated envs work fine locally, but sharing code across machines / platforms requires standardized env specifications:

- `environment.yml` — conda
- `requirements.txt` — pip

**Full conda export** (includes every package, with build hashes):

```bash
conda env export > environment.yml
```

Notes about the full export:

1. Includes all auto-installed dependencies, not just what you explicitly installed
2. Cryptic build strings appear after each version (e.g. `py37hfca59bb_0`)
3. pip-installed packages are also captured, under a `pip:` block
4. The `prefix:` line at the bottom is machine-specific but is **ignored on import** — you can delete it after export

**Reduced conda export** (only explicitly installed packages via conda):

```bash
conda env export --from-history > environment.yml
```

Produces a much cleaner, portable file with only the packages you asked for — not their dependencies, not build hashes.

**pip export:**

```bash
pip freeze > requirements.txt
```

Installable via `pip install -r requirements.txt` — but requiring users to run both conda and pip commands is cumbersome.

**The two reproducibility files compared:**

| | `pip freeze` → `requirements.txt` | `conda env export --from-history` → `environment.yml` |
|---|---|---|
| What is listed? | All Python packages in the env | Only packages explicitly installed by the user via conda |
| Sub-dependencies? | Yes (exhaustive) | No (minimal) |
| conda-installed packages | Shown with local `@ file:///...` path, not portable | Shown with name + version, portable |

**Mixed conda + pip:** `pip freeze` includes conda-installed packages with ugly local paths. Filter them out:

```bash
pip freeze | grep -v "@ file://" > requirements.txt
```

**Best practice:** hand-write the `environment.yml` so that both conda and pip packages are explicit, and the file is a single source of truth:

```yaml
name: my_env
channels:
  - defaults
dependencies:
  - python=3.7.11
  - numpy=1.21.2
  - pandas=1.3.4
  - scikit-learn=1.0.1
  - pip=21.2.4
  - pip:
    # regular pip packages
    - matplotlib==2.0.0
    # or reference a whole requirements file
    - -r requirements.txt
```

Recreate anywhere with:

```bash
conda env create -f environment.yml
```

**Alternative (export-then-clean workflow):** if you already have a working env and want to capture its state:

1. `conda env export --from-history > environment.yml` — check that the Python version is pinned
2. `pip freeze > requirements.txt`
3. Clean `requirements.txt` — remove `@ file:///...` lines
4. In the `environment.yml`, reference the cleaned pip file via `- -r requirements.txt` inside the `pip:` block

---

## Exercises — Interpreter Setup

Hands-on checks to confirm your Python installation works:

**1. Verify the Python path binding:**

```bash
python -c "print('Hello World')"
```

**2. Check the installed Python version:**

```bash
python --version
```

**3. Check which interpreter is currently active:**

```bash
which python
```

**4. List other installed Python versions on the system** (on Linux/macOS, Python is usually in `/usr/bin`):

```bash
ls /usr/bin/python*
```

## Exercises — Environment Setup

**1. Create a project directory:**

```bash
mkdir example
cd example
```

**2. Create and activate a conda env with a specific Python version:**

```bash
conda create --name example python=3.11
conda activate example
```

**3. Check installed packages:**

```bash
pip freeze
```

**4. Install a package:**

```bash
pip install pandas
```

**5. Save installed packages to `requirements.txt`:**

```bash
pip freeze > requirements.txt
```

**6. Add a package to `requirements.txt`:**

```bash
echo "numpy" >> requirements.txt
```

**7. Update the environment from the modified file:**

```bash
pip install -r requirements.txt
```

**8. Remove a package** (note: pip does *not* remove orphan dependencies automatically):

```bash
pip uninstall numpy
```

**9. Export the conda environment history to `environment.yml`:**

```bash
conda env export --from-history > environment.yml
```

**10. Recreate an environment from the yml file:**

```bash
conda env create -f environment.yml
```

---
