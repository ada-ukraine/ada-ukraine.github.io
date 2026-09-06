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
blog_post_pattern = ["posts/*.md", "posts/podcast/*.md"]
blog_title = "Про Аду українською"

copyright = u'2024 – 2025, Ada Ukraine. All rights reserved. CC BY 4.0 License'

html_logo = "img/logo_ukr.svg"

# Custom templates for the Ukrainian fork live in a separate directory
# from '_templates' (which publish-latest.yml wipes before the production
# build, since that directory holds AdaCore-only files like analytics.html).
templates_path.append('_templates_ukr')

# Same idea for static assets: keep ukr-specific files out of the shared
# 'img' static path.
html_static_path.append('_static_ukr')
html_css_files.append('ukr-ablog-sidebar.css')

html_sidebars = {
   '**': [
          'sidebar/brand.html',
          'sidebar/social-links.html',
          'sidebar/search.html',
          'sidebar/scroll-start.html',
          'sidebar/navigation.html',
          'ablog/postcard.html', 'ablog/recentposts.html',
          'ablog/tagcloud.html', 'ablog/categories.html',
          'ablog/archives.html',
          'sidebar/scroll-end.html',
         ]
}

# html_theme is already set to "furo" by the base conf.py (upstream default);
# no need to re-set it here.

# Update (not replace!) html_theme_options: the base conf.py already sets
# furo's light/dark color variables, sidebar_hide_name, top_of_page_buttons,
# etc. Overwriting the dict here would silently drop all of that.
html_theme_options.update({
    "source_repository": "https://github.com/ada-ukraine/ada-ukraine.github.io",
    "source_branch": "ukr",
    "source_directory": "content/",
    "footer_icons": [
        {
            "name": "Telegram",
            "url": "https://t.me/ada_in_ukraine",
            "html": '<span class="fa-brands fa-telegram"></span>',
            "class": "",
        },
        {
            "name": "Atom feed",
            "url": "/blog/atom.xml",
            "html": '<span class="fa-solid fa-rss"></span>',
            "class": "",
        },
    ],
})

html_title = "ada-ukraine.github.io"

language = 'uk'

myst_update_mathjax = False # ablog docs sets this

# master_doc = 'index_ukr'
