# -*- coding: utf-8 -*-
#
# This file is execfile()d with the current directory set to its containing dir.

import sys, os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), ".ext")))

# -- General configuration -----------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be extensions
# coming with Sphinx (named 'sphinx.ext.*') or your custom ones.
extensions = ["sphinx.ext.todo", "make_domain", "emacs_domain"]
templates_path = ['.templates']

source_suffix = '.txt'
master_doc = 'index'

project = u'Taskfile: A Tasklist Compiler'
copyright = u'2012, Sam Kleinman'

version = '0.1'
release = ''

exclude_patterns = []

pygments_style = 'sphinx'
add_function_parentheses = False

# -- Options for HTML output ---------------------------------------------------

html_theme = 'cyborg'
html_theme_path = ['themes']

html_title = 'Taskfile, a Tasklist Compiler'
html_short_title = 'Taskfile'

#html_logo = None
#html_favicon = None

html_static_path = ['source/.static']
html_use_smartypants = True

# If false, no module index is generated.
#html_domain_indices = True
# If false, no index is generated.
html_use_index = True
html_split_index = False
html_show_sourcelink = True
html_show_sphinx = False
html_show_copyright = True

htmlhelp_basename = 'cyborg-institute'

# -- Options for LaTeX output --------------------------------------------------

latex_paper_size = 'letter'
latex_font_size = '10pt'

latex_documents = [
# (source start file, target name, title, author, documentclass [howto/manual]).
  ('index', 'taskfile.tex', u'Taskfile: A Tasklist Compiler',
   u'Sam Kleinman', 'howto'),
]

#latex_logo = None

# -- Options for manual page output --------------------------------------------

man_pages = [
# (source start file, name, description, authors, manual section).
  ('index', 'Taskfile', u'Taskfile: A Tasklist Compiler', [u'Sam Kleinman'], 1)
]

# -- Options for Epub output ---------------------------------------------------

# Bibliographic Dublin Core info.
epub_title = u'The Cyborg Institute'
epub_author = u'Sam Kleinman'
epub_publisher = u'Sam Kleinman'
epub_copyright = u'2011, Sam Kleinman'

# The depth of the table of contents in toc.ncx.
#epub_tocdepth = 3
# Allow duplicate toc entries.
#epub_tocdup = True

# Example configuration for intersphinx: refer to the Python standard library.
# intersphinx_mapping = {'http://docs.python.org/': None}
