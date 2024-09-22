# Injection into conf.py for Ukrainian

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

blog_post_pattern = ["posts/*.md"]

copyright = u'2023 – 2024, Ada Ukraine. All rights reserved. CC BY 4.0 License'

html_logo = "img/logo_ukr.svg"

html_sidebars = {
   'index': [
          'navbar-nav',
          'ablog/postcard.html', 'ablog/recentposts.html',
          'ablog/tagcloud.html', 'ablog/categories.html',
          'ablog/archives.html',
         ],
   '**': [
          'sidebar-nav-bs',
          'ablog/postcard.html', 'ablog/recentposts.html',
          'ablog/tagcloud.html', 'ablog/categories.html',
          'ablog/archives.html',
         ]
}

html_theme = "pydata_sphinx_theme"

html_theme_options = {
}

language = 'uk'

myst_update_mathjax = False # ablog docs sets this

# master_doc = 'index_ukr'
