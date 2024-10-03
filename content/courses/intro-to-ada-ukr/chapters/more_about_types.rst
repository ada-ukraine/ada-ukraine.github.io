Більше про типи
===============

.. _Intro_Ada_Aggregates:

.. include:: ../../global.txt

Агрегати
--------

До тепер ми багато говорили про агрегати та бачили низку прикладів.
Тепер ми ще раз розглянемо цю функцію але більш детально.

По суті, агрегат Ada є літеральним значенням для композитного типу.
Це дуже потужна нотація, яка допомагає вам у багатьох випадках уникнути
написання процедурного коду для ініціалізації ваших структур даних.

Основне правило під час написання агрегатів полягає в тому, що
*кожний компонент* массиву або запису має бути визначений, навіть ті
компоненти, які мають значення за замовчуванням.

Це означає, що наступний код неправильний:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Incorrect_Aggregate
    :class: ada-expect-compile-error

    package Incorrect is
       type Point is record
          X, Y : Integer := 0;
       end record;

       Origin : Point := (X => 0);
    end Incorrect;

Є кілька можливостей, які можна використовувати, щоб зробити ініціалізацію зручнішою:

- Щоб вказати значення за замовчуванням для компонента, ви можете використовувати
  нотацію :ada:`<>`.

- Ви можете використовувати символ :ada:`|`, щоб надати кільком компонентам
  однакове значення.

- Ви можете використовувати :ada:`others` для посилань на кожен компонент,
  який ще не заданий, за умови, що всі ці поля мають однаковий тип.

- Ви можете використовувати позначення діапазону :ada:`..` для посилання,
  щоб вказати безперервну послідовність індексів у масиві.

Однак зауважте, що як тільки ви використали іменований доступ, усі наступні
компоненти також повинні бути визначені за іменами.

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Points

    package Points is
       type Point is record
          X, Y : Integer := 0;
       end record;

       type Point_Array is
         array (Positive range <>) of Point;

       --  використовуємо значення
       --  за замовчуванням
       Origin   : Point := (X | Y => <>);

       --  так само, за замовчуванням
       Origin_2 : Point := (others => <>);

       Points_1 : Point_Array := ((1, 2), (3, 4));
       Points_2 : Point_Array := (1       => (1, 2),
                                  2       => (3, 4),
                                  3 .. 20 => <>);
    end Points;

Омоніми та кваліфікуючі вирази
-------------------------------

Ada має загальну концепцію визначення імен, яку ми бачили раніше в розділі
про :ref:`перечислимі типи <Intro_Ada_Enum_Types>`.

Візьмемо простий приклад: в Ada можливо мати функції, які мають однакові
назви, але різні типи або кількість параметрів.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading
    :class: ada-syntax-only

    package Pkg is
       function F (A : Integer) return Integer;
       function F (A : Character) return Integer;
    end Pkg;

Це поширена концепція в мовах програмування, яка називається
:wikipedia:`Перевантаження функції <Function_overloading>`.

Одним із нових аспектів можливостей Ada є здатність розрізняти імена
на основі типу результату функції.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading
    :class: ada-syntax-only

    package Pkg is
       type SSID is new Integer;

       function Convert (Self : SSID)
                         return Integer;
       function Convert (Self : SSID)
                         return String;
    end Pkg;

    with Ada.Text_IO; use Ada.Text_IO;
    with Pkg;         use Pkg;

    procedure Main is
       S : String := Convert (123_145_299);
       --            ^ Дозволено, буде вибрана
       --              відповідна Convert
    begin
       Put_Line (S);
    end Main;

.. attention::
    Зауважте, що розрізняти імена на основі типу дозволено як для функцій,
    так і для літералів перечислимих типів, тому ви можете мати
    кілька літералів з однаковою назвою. Семантично літерал
    перечислимого типу розглядається як функція, яка не має параметрів.

.. _Intro_Ada_Qualified_Expressions:

Однак іноді неоднозначність робить неможливим визначити, до якого імені іде посилання.
Ось де кваліфікуючий вираз стає корисним.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading_Error
    :class: ada-syntax-only, ada-expect-compile-error

    package Pkg is
       type SSID is new Integer;

       function Convert (Self : SSID)
                         return Integer;
       function Convert (Self : SSID)
                         return String;
       function Convert (Self : Integer)
                         return String;
    end Pkg;

    with Ada.Text_IO; use Ada.Text_IO;
    with Pkg;         use Pkg;

    procedure Main is
       S : String := Convert (123_145_299);
       --            ^ Невірно, неможливо
       --              визначити яку викликати

       S2 : String := Convert (SSID'(123_145_299));
       --                     ^ Ми явно вказали 
       --                       що параметер має
       --                       тип SSID.

       --  Також ми можемо використати
       --  тимчасову змінну для цього

       I : SSID := 123_145_299;

       S3 : String := Convert (I);
    begin
       Put_Line (S);
    end Main;

Синтаксично ціллю кваліфікуючого виразу може бути або будь-який вираз у дужках,
або агрегат:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Qual_Expr

    package Qual_Expr is
       type Point is record
          A, B : Integer;
       end record;

       P : Point := Point'(12, 15);

       A : Integer := Integer'(12);
    end Qual_Expr;

Приклад ілюструє, що кваліфікуючі вирази є зручним (і іноді необхідним) способом
для програміста зробити тип виразу явним, звичайно для компілятора, але також і
для інших програмістів.

.. attention::
    Хоча вони виглядають і відчуваються схожими, перетворення типів і
    кваліфікуючі вирази *не* однакові.

    Кваліфікуючий вираз визначає точний тип, до якого буде розв’язано цільовий
    вираз, тоді як перетворення типу намагатиметься перетворити ціль і видасть
    помилку під час виконання, якщо цільове значення не може бути таким чином
    перетворено.

    Зауважте, що Ви можете використовувати кваліфікуючий вираз для перетворення
    з одного підтипу в інший, створюючи виключення, якщо обмеження порушується.

    .. code-block:: ada

        X : Integer := Natural'(1);


Тип символа
-----------

Як зазначалося раніше, кожен перечислимий тип є відмінним і несумісним
з будь-яким іншим перечислимим типом. Проте ми не згадували раніше про те,
що символьні літерали дозволені як літерали перерахування. Це означає,
що на додаток до суворо типізованих типів символів мови також дозволені
типи символів, визначені користувачем:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Character_Example
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Character_Example is
       type My_Char is ('a', 'b', 'c');
       --  Наш власний перечислимий тип,
       --  з трьома можливими значеннями.

       C : Character;
       --  ^ Вбудований тип символа
       --    (перечислимий тип)

       M : My_Char;
    begin
       C := '?';
       --   ^ Літерал-символ
       --     (перечислимий літерал)

       M := 'a';

       C := 65;
       --   ^ Не вірно: 65 не є
       --     значенням символа

       C := Character'Val (65);
       --  Присвоїти символ який
       --  має позицію 65 в
       --  перечисленні (який є 'A')

       M := C;
       --   ^ Не вірно: C є значенням Сharacter,
       --     а M має тип My_Char

       M := 'd';
       --   ^ Не вірно: 'd' не є літералом
       --     для типу My_Char
    end Character_Example;

У цьому прикладі ми використовуємо символи для декларації :ada:`My_Char`
