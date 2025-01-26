# Injection into conf.py for Ukrainian

author = u'Ada Ukraine + AdaCore'

exclude_patterns += [
    'about.rst',
    '**/README.md'
]

extensions += [
    'ablog',
    "myst_parser",
]


blog_authors = {
        'max': ("Максим Резник", "https://github.com/reznikmm")
}

blog_default_language = 'uk'

blog_languages = {
    'uk': ('Україньска', None),
}

blog_baseurl = "https://ada-ukraine.github.io/"
blog_post_pattern = ["posts/*.md"]
blog_title = "Про Аду українською"

copyright = u'2024 – 2025, Ada Ukraine. All rights reserved. CC BY 4.0 License'

html_logo = "img/logo_ukr.svg"

html_sidebars = {
   '**': [
          'navbar-logo.html',
          'icon-links.html',
          'search-button-field.html',
          'sbt-sidebar-nav.html',
          'ablog/postcard.html', 'ablog/recentposts.html',
          'ablog/tagcloud.html', 'ablog/categories.html',
          'ablog/archives.html',
         ]
}

html_theme = "sphinx_book_theme"

html_theme_options = {
    "icon_links": [
        {
            "name": "Atom feed",
            "url": "/blog/atom.xml",  # required
            "icon": "fa-solid fa-rss",
            "type": "fontawesome",
        }
    ],
    "path_to_docs": "content",
    "repository_branch": "ukr",
    "repository_url": "https://github.com/ada-ukraine/ada-ukraine.github.io",
    "use_edit_page_button": True,
    "use_fullscreen_button": False,
    "use_repository_button": True,
}

html_title = "ada-ukraine.github.io"

language = 'uk'

myst_update_mathjax = False # ablog docs sets this

# master_doc = 'index_ukr'
