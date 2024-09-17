# Injection into conf.py for Ukrainian

exclude_patterns += [
    'about.rst',
    './index.rst',
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

html_logo = "img/logo_ukr.svg"

language = 'uk'

myst_update_mathjax = False # ablog docs sets this


# master_doc = 'index_ukr'
