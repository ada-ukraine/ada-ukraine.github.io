.. intro-to-ada documentation master file, created by
   sphinx-quickstart on Mon Feb 19 11:39:35 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

:prev_state: False
:next_state: False

.. _Intro_Ada_Course_Index:

Введення в Ada
==============

.. include:: ../global.txt

.. only:: no_hidden_books

    .. warning::

        This version of the website contains UNPUBLISHED contents.
        Please do not share it externally!

.. only:: builder_epub

    Release |release|

    |today|

.. only:: builder_latex or builder_epub

    .. container:: content-copyright

        Copyright © 2018 |ndash| 2022, AdaCore

        Цю книгу опубліковано за ліцензією CC BY-SA, що означає,
        що ви можете копіювати, розповсюджувати, форматувати,
        трансформувати та доповнювати для будь-яких цілей,
        навіть комерційних, за умови, що ви вказуєте належне авторство,
        надаєте посилання на ліцензії та вказуєте, чи були внесені зміни.
        Якщо ви доповнюєте, трансформуєте або використовуєте матеріал,
        ви повинні поширювати результат за тією ж ліцензією, що й оригінал.
        Ви можете знайти деталі ліцензії
        `на цій сорінці <http://creativecommons.org/licenses/by-sa/4.0>`_

        .. image:: ../../images/ccheart_black.png
            :width: 108pt

.. container:: content-description

    Цей курс навчить вас основам мови програмування Ada та призначений для тих,
    хто вже має базові знання про техніку програмування. Ви дізнаєтеся, як застосувати
    ці прийоми в Ada.

    Цей матеріал був написаний Raphaël Amiard та Gustavo A. Hoffmann, рецензент: Richard Kenner.

    .. note::

        У прикладах коду в цьому курсі використовується обмеження на 50 стовпців,
        що значно покращує читабельність коду на пристроях із невеликим розміром екрана.
        Це обмеження, однак, призводить до незвичайного стилю кодування. Наприклад замість
        виклику :ada:`Put_Line` в одну строку ми ваємо наступне:

        .. code-block:: ada

            Put_Line
              (" is in the northeast quadrant");

        або так:

        .. code-block:: ada

             Put_Line ("  (X => "
                       & Integer'Image (P.X)
                       & ")");

        Зверніть увагу, що типовий код Ada використовує обмеження щонайменше в 79 стовпців.
        Тому, будь ласка, не сприймайте стиль кодування з цього курсу як посилання!

    .. only:: builder_latex or builder_epub

        .. note::

            Кожен приклад коду з цієї книги має пов’язані "метадані блоку коду",
            які містять назву "проекту" і хеш-значення MD5. Ця інформація
            використовується для визначення окремого прикладу коду.

            Ви можете знайти всі приклади коду в zip-файлі, який Ви можете
            `завантажити з наступного сайту <https://learn.adacore.com/zip/learning-ada_code.zip>`_.
            Структура каталогів у файлі zip базується на метаданих блоку коду.
            Наприклад, якщо ви шукаєте приклад коду з цими метаданими:

            - Project: Courses.Intro_To_Ada.Imperative_Language.Greet

            - MD5: cba89a34b87c9dfa71533d982d05e6ab

            Ви знайтидете його в цій директорії:

            :file:`projects/Courses/Intro_To_Ada/Imperative_Language/Greet/cba89a34b87c9dfa71533d982d05e6ab/`

            Щоб використати цей приклад коду, просто виконайте наступні дії:

            1. Розархівуйте zip файл;
            2. Зайдіть в необхідну директорію;
            3. Запустіть GNAT Studio в цій директорії;
            4. Побудуйте (чи скомпілюйте) проект;
            5. Запустіть застосунок (якщо в проекті є стартова (головна) процедура).


.. only:: builder_html

    .. container:: ebook-download

        .. raw:: html

            <a class="ebook-download-button" href="/pdf_books/courses/intro-to-ada-ua.pdf">
                Download PDF
            </a>

            <a class="ebook-download-button" href="/epub_books/courses/intro-to-ada-ua.epub">
                Download EPUB
            </a>

.. toctree::
    :maxdepth: 4
    :caption: Зміст:

    Вступ <chapters/introduction>
    Імперативна мова <chapters/imperative_language>
