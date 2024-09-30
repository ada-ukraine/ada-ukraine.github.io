Модульне програмування
======================

.. include:: ../../global.txt

Поки що наші приклади були простими автономними підпрограмами. Ada корисна в
цьому відношенні, оскільки вона дозволяє використовувати декларативні блоки.
Таким чином ми змогли оголосити наші типи та змінні в тілах основних
підпрограм.

Однак легко побачити, що це не буде масштабуватися для реальних програм.
Нам потрібен кращий спосіб структурувати наші програми в модулі та окремі
одиниці.

Ada заохочує поділ програм на кілька пакетів і підпакетів, надаючи багато
інструментів програмісту, який шукає як ідеально організовати код.

Пакети
------

Ось приклад оголошення пакета в Ada:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Week

    package Week is

       Mon : constant String := "Monday";
       Tue : constant String := "Tuesday";
       Wed : constant String := "Wednesday";
       Thu : constant String := "Thursday";
       Fri : constant String := "Friday";
       Sat : constant String := "Saturday";
       Sun : constant String := "Sunday";

    end Week;

І ось як ви його використовати:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Week

    with Ada.Text_IO; use Ada.Text_IO;
    with Week;
    --  Посилання на пакет Week і
    --  додавання залежності Main від Week

    procedure Main is
    begin
       Put_Line ("First day of the week is "
                 & Week.Mon);
    end Main;

Пакети дозволяють зробити Ваш код модульним, розділяючи ваші програми на
семантично значущі одиниці. Крім того, відокремлення специфікації пакета
від його тіла (що ми побачимо нижче) може зменшити час компіляції.

Хоча ключове слово :ada:`with` вказує на залежність, ви можете бачити в
прикладі вище, що вам все одно потрібно додавати назву пакету як префікс до
посилань на сутності з пакету Week. Якби ми викорастали також :ada:`use Week`,
тоді такий префікс не був би потрібним.

Для доступу до сутностей із пакета використовується нотація з крапками
:ada:`A.B`, яка є такою ж нотацією, як і для доступу до полів запису.

Ключове слово :ada:`with` може з’являтися *тільки* перед блоком компіляції
(тобто перед зарезервованим словом, таким як :ada:`procedure` або :ada:`package`,
яке позначає початок блоку). Пізніше це неможливо. Це правило потрібне лише
з методологічних міркувань: людина, яка читає Ваш код, повинна мати змогу одразу
бачити, від яких пакетів залежить код.

.. admonition:: В інших мовах

    Пакети виглядають схожими на файли заголовків у C/C++, але семантично
    сильно від них відрізняються.

    - Перша і найважливіша відмінність полягає в тому, що пакети є механізмом
      на рівні мови. Це на відміну від файлу заголовка :c:`#include`\'d, який
      є функціональністю препроцесора C.

    - Негайним наслідком є ​​те, що конструкція :ada:`with` є механізмом
      семантичного включення, а не механізмом включення тексту коду. Отже,
      коли ви використовуєте :ada:`with`, ви говорите компілятору "Я залежу
      від цієї семантичної одиниці", а не "включаю цей текст на це місце".

    - Таким чином, сам пакет не змінюється залежно від того, де він був
      включений. Порівняйте це з C/C++, де значення включеного коду залежить
      від контексту, у якому з’являється :c:`#include`.

      Це дозволяє компіляції/повторній компіляції бути більш ефективними. Це
      також дозволяє таким інструментам, як IDE, мати правильну інформацію
      про семантику програми. У свою чергу, це дозволяє покращити інструменти
      в цілому та код, який краще аналізується навіть людьми.

    Важливою перевагою Ada :ada:`with` порівняно з :c:`#include` є відсутність
    стану. Порядок речень :ada:`with` і :ada:`use` не має значення та може бути
    змінений без побічних ефектів.

.. admonition:: В інструментах GNAT

    Стандарт мови Ada не вимагає жодних особливих відносин між файлами коду
    та пакетами; наприклад, теоретично ви можете помістити весь свій код в
    один файл або використовувати власні угоди про іменування файлів. Однак
    на практиці реалізація матиме певні правила. З GNAT кожна одиниця компіляції
    верхнього рівня повинна мати окремий файл. У наведеному вище прикладі пакет
    :ada:`Week` буде у файлі ``.ads`` (для специфікації Ada), а процедура
    :ada:`Main` буде у файлі ``.adb``. (для тіла Ada).

.. _Intro_Ada_Use_Clause:

Використання пакетів
--------------------

Як ми бачили вище, ключове слово :ada:`with` вказує на залежність від іншого
пакета. Однак кожне посилання на сутність, що надходить із пакета :ada:`Week`,
має мати префікс повної назви пакета. Можна зробити кожну сутність пакета
видимою безпосередньо в поточній області за допомогою ключового слова :ada:`use`.

Фактично, ми використовували :ada:`use` майже з початку цього курсу.

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Week

    with Ada.Text_IO; use Ada.Text_IO;
    --                ^ Робимо кожну сутність
    --                  з пакету Ada.Text_IO
    --                  безпосередньо видимою.
    with Week;

    procedure Main is
       use Week;
       --  Робимо кожну сутність з пакету Week
       --  Week безпосередньо видимою.
    begin
       Put_Line ("First day of the week is " & Mon);
    end Main;

Як ви можете бачити в прикладі вище:

- :ada:`Put_Line` — це підпрограма, яка походить з пакету
  :ada:`Ada.Text_IO`. Ми можемо посилатися на неї безпосередньо,
  тому що у нас є :ada:`use`\d у верхній частині блоку :ada:`Main`.

- На відміну від :ada:`with`, :ada:`use` можна розмістити або перед
  блоком компіляції, або в будь-якому декларативному блоці. В останньому
  випадку :ada:`use` матиме ефект у межах цього блоку.

Реалізація пакету
-----------------

У наведеному вище простому прикладі пакет :ada:`Week` містить лише декларації,
а не реалізації. Це не помилка: у специфікації пакета, яка проілюстрована вище,
ви не можете розміщати код реалізації. Вони мають бути в реалізації пакету.

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Operations

    package Operations is

       --  Декларація
       function Increment_By
         (I    : Integer;
          Incr : Integer := 0) return Integer;

       function Get_Increment_Value return Integer;

    end Operations;

    package body Operations is

       Last_Increment : Integer := 1;

       --  Реалізація
       function Increment_By
         (I    : Integer;
          Incr : Integer := 0) return Integer is
       begin
          if Incr /= 0 then
             Last_Increment := Incr;
          end if;

          return I + Last_Increment;
       end Increment_By;

       function Get_Increment_Value return Integer is
       begin
          return Last_Increment;
       end Get_Increment_Value;

    end Operations;

Тут ми бачимо, що реалізація функції :ada:`Increment_By` має бути також
декларована і в реалізації пакету (тілі пакету). За збігом обставин, це
дозволяє нам помістити змінну :ada:`Last_Increment` в тіло, і зробити
їх недоступною для користувача пакета :ada:`Operations`, забезпечуючи
першу форму інкапсуляції.

Це працює, оскільки сутності, оголошені в тілі, видимі *лише* в тілі.

Наступний приклад показує, як :ada:`Last_Increment` використовується:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Operations

    with Ada.Text_IO; use Ada.Text_IO;
    with Operations;

    procedure Main is
       use Operations;

       I : Integer := 0;
       R : Integer;

       procedure Display_Update_Values is
          Incr : constant Integer :=
                   Get_Increment_Value;
       begin
          Put_Line (Integer'Image (I)
                    & " incremented by "
                    & Integer'Image (Incr)
                    & " is "
                    & Integer'Image (R));
          I := R;
       end Display_Update_Values;
    begin
       R := Increment_By (I);
       Display_Update_Values;
       R := Increment_By (I);
       Display_Update_Values;

       R := Increment_By (I, 5);
       Display_Update_Values;
       R := Increment_By (I);
       Display_Update_Values;

       R := Increment_By (I, 10);
       Display_Update_Values;
       R := Increment_By (I);
       Display_Update_Values;
    end Main;

.. _Intro_Ada_Child_Packages:

Дочірні пакети
--------------

З пакетів можна створити ієрархію. Ми досягаємо цього за допомогою
дочірніх пакетів, які розширюють функціональні можливості батьківського
пакета. Одним із прикладів дочірнього пакету, який ми використовували досі,
є пакет :ada:`Ada.Text_IO`. Тут батьківський пакет називається :ada:`Ada`,
а дочірній — :ada:`Text_IO`. У попередніх прикладах ми використовували
процедуру :ada:`Put_Line` з дочірнього пакету :ada:`Text_IO`.

.. admonition:: Важливо

    Ada також підтримує вкладені пакети. Однак, оскільки вони можуть бути
    складнішими у використанні, рекомендується замість них використовувати
    дочірні пакети. Вкладені пакети будуть розглянуті в поглибленому курсі.

Давайте почнемо обговорення дочірніх пакетів, взявши наш попередній
пакет :ada:`Week`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    package Week is

       Mon : constant String := "Monday";
       Tue : constant String := "Tuesday";
       Wed : constant String := "Wednesday";
       Thu : constant String := "Thursday";
       Fri : constant String := "Friday";
       Sat : constant String := "Saturday";
       Sun : constant String := "Sunday";

    end Week;

Якщо ми хочемо створити дочірній пакет для :ada:`Week`, ми можемо написати:

.. code:: ada no_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages
    :class: ada-syntax-only

    package Week.Child is

       function Get_First_Of_Week return String;

    end Week.Child;

Тут :ada:`Week` — батьківський пакет, а :ada:`Child` — дочірній пакет.
Це відповідна реалізація пакета :ada:`Week.Child`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    package body Week.Child is

       function Get_First_Of_Week return String is
       begin
          return Mon;
       end Get_First_Of_Week;

    end Week.Child;

У реалізації функції :ada:`Get_First_Of_Week` ми можемо використовувати :ada:`Mon`
безпосередньо, навіть якщо він був оголошений у батьківському пакеті :ada:`Week`.
Ми не пишемо тут :ada:`with Week`, оскільки всі елементи зі специфікації пакета
:ada:`Week` |mdash| наприклад :ada:`Mon`, :ada:`Tue` тощо |mdash| видимі в 
дочірньому пакеті :ada:`Week.Child`.

Тепер, коли ми завершили реалізацію пакета :ada:`Week.Child`, ми можемо використовувати
елементи цього дочірнього пакета в підпрограмі, просто написавши :ada:`with Week.Child`.
Так само, якщо ми хочемо використовувати ці елементи напряму, ми додатково пишемо
:ada:`use Week.Child`. Наприклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    with Ada.Text_IO; use Ada.Text_IO;
    with Week.Child;  use Week.Child;

    procedure Main is
    begin
       Put_Line ("First day of the week is "
                 & Get_First_Of_Week);
    end Main;

Дочірній пакет дочірнього пакета
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Поки що ми бачили дворівневу ієрархію пакетів. Але ієрархія, яку ми потенційно можемо
створити, не обмежується цим. Наприклад, ми могли б розширити ієрархію попереднього
прикладу, оголосивши пакет :ada:`Week.Child.Grandchild`. У цьому випадку
:ada:`Week.Child` буде батьківським пакетом для :ada:`Grandchild`.
Розглянемо цю реалізацію:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    package Week.Child.Grandchild is

       function Get_Second_Of_Week return String;

    end Week.Child.Grandchild;

    package body Week.Child.Grandchild is

       function Get_Second_Of_Week return String is
       begin
          return Tue;
       end Get_Second_Of_Week;

    end Week.Child.Grandchild;

Ми можемо використовувати цей новий пакет :ada:`Grandchild` у нашій тестовій
програмі так само, як і раніше: адаптувати :ada:`with` та :ada:`use`, а також
виклик функції. Ось оновлений код:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    with Ada.Text_IO; use Ada.Text_IO;

    with Week.Child.Grandchild;
    use  Week.Child.Grandchild;

    procedure Main is
    begin
       Put_Line ("Second day of the week is "
                 & Get_Second_Of_Week);
    end Main;

Знову ж таки, це не межа для ієрархії пакетів. Ми могли б продовжити
розширення ієрархії попереднього прикладу, реалізувавши пакет
:ada:`Week.Child.Grandchild.Grand_grandchild`.

Багато дочірніх пакетів
~~~~~~~~~~~~~~~~~~~~~~~

Поки що ми бачили один дочірній пакет у батьківського пакета. Однак батьківський
пакет також може мати кілька дочірніх елементів. Ми могли б розширити наведений
вище приклад і реалізувати пакет :ada:`Week.Child_2`. Наприклад:

.. code:: ada no_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages
    :class: ada-syntax-only

    package Week.Child_2 is

       function Get_Last_Of_Week return String;

    end Week.Child_2;

Тут :ada:`Week` все ще є батьківським пакетом пакета :ada:`Child`, але він також
є батьківським пакетом :ada:`Child_2`. Таким же чином :ada:`Child_2`, очевидно,
є одним із дочірніх пакетів :ada:`Week`.

Це відповідне тіло пакета :ada:`Week.Child_2`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    package body Week.Child_2 is

       function Get_Last_Of_Week return String is
       begin
          return Sun;
       end Get_Last_Of_Week;

    end Week.Child_2;

Тепер ми можемо посилатися на обидва дочірні пакети в нашому тестовому прикладі:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Child_Packages

    with Ada.Text_IO;  use Ada.Text_IO;
    with Week.Child;   use Week.Child;
    with Week.Child_2; use Week.Child_2;

    procedure Main is
    begin
       Put_Line ("First day of the week is "
                 & Get_First_Of_Week);
       Put_Line ("Last day of the week is "
                 & Get_Last_Of_Week);
    end Main;

Видимість
~~~~~~~~~

У попередньому розділі ми бачили, що елементи, оголошені в специфікації батьківського
пакета, видимі в дочірньому пакеті. Однак це не стосується елементів, оголошених у
реалізації батьківського пакета.

Давайте розглянемо пакет :ada:`Book` і його дочірній - :ada:`Additional_Operations`:

.. code:: ada no_button project=Courses.Intro_To_Ada.Modular_Programming.Visibility
    :class: ada-syntax-only

    package Book is

       Title : constant String :=
         "Visible for my children";

       function Get_Title return String;

       function Get_Author return String;

    end Book;

    package Book.Additional_Operations is

       function Get_Extended_Title return String;

       function Get_Extended_Author return String;

    end Book.Additional_Operations;

Це реалізація обох пакетів:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Visibility

    package body Book is

       Author : constant String :=
         "Author not visible for my children";

       function Get_Title return String is
       begin
          return Title;
       end Get_Title;

       function Get_Author return String is
       begin
          return Author;
       end Get_Author;

    end Book;

    package body Book.Additional_Operations is

       function Get_Extended_Title return String is
       begin
          return "Book Title: " & Title;
       end Get_Extended_Title;

       function Get_Extended_Author return String is
       begin
          --  Строка "Author" декларована в
          --  реалізації пакету Book не видима
          --  тут. Тому ми не можемо написати:
          --
          --  return "Book Author: " & Author;

          return "Book Author: Unknown";
       end Get_Extended_Author;

    end Book.Additional_Operations;

У реалізації :ada:`Get_Extended_Title` ми використовуємо константу
:ada:`Title` з батьківського пакета :ada:`Book`. Однак, як зазначено
в коментарях до функції :ada:`Get_Extended_Author`, строка :ada:`Author`
|mdash| яку ми оголосили в реалізації пакету :ada:`Book` |mdash|
не видимий в пакеті :ada:`Book.Additional_Operations`. Тому ми не можемо
використовувати його для реалізації функції :ada:`Get_Extended_Author`.

Однак ми можемо використати функцію :ada:`Get_Author` з :ada:`Book` у
реалізації функції :ada:`Get_Extended_Author`, щоб отримати цей рядок.
Так само ми можемо використати цю стратегію для реалізації функції
:ada:`Get_Extended_Title`. Це адаптований код:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Modular_Programming.Visibility

    package body Book.Additional_Operations is

       function Get_Extended_Title return String is
       begin
          return "Book Title: " & Get_Title;
       end Get_Extended_Title;

       function Get_Extended_Author return String is
       begin
          return "Book Author: " & Get_Author;
       end Get_Extended_Author;

    end Book.Additional_Operations;

Це простий тестовий код для пакетів вище:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Visibility

    with Ada.Text_IO; use Ada.Text_IO;

    with Book.Additional_Operations;
    use  Book.Additional_Operations;

    procedure Main is
    begin
       Put_Line (Get_Extended_Title);
       Put_Line (Get_Extended_Author);
    end Main;

Декларуючи елементи в реалізації пакету, ми можемо реалізувати інкапсуляцію
в Ada. Ці елементи будуть видимі лише в самій реалізації пакету, але ніде
більше. Однак це не єдиний спосіб досягти інкапсуляції в Ada: ми обговоримо
інші підходи в розділі :doc:`./privacy`.

.. _Intro_Ada_Package_Renaming:

Перенайменування
----------------

Раніше ми згадували, що :ref:`підпрограми можна перейменувати <Intro_Ada_Subprogram_Renaming>`.
Ми також можемо перейменовувати і пакети. Знову ж таки, для цього ми використовуємо ключове
слово :ada:`renames`. У наступному прикладі пакет :ada:`Ada.Text_IO` змінюється на :ada:`TIO`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Rename_Text_IO

    with Ada.Text_IO;

    procedure Main is
       package TIO renames Ada.Text_IO;
    begin
       TIO.Put_Line ("Hello");
    end Main;

Ми можемо використовувати перейменування, щоб покращити читабельність нашого коду,
використовуючи коротші імена пакетів. У прикладі вище ми пишемо :ada:`TIO.Put_Line`
замість довшої версії (:ada:`Ada.Text_IO.Put_Line`). Цей підхід особливо корисний,
коли ми не використовуємо :ada:`use` для пакетів і хочемо уникнути того, щоб код
став надто багатослівним.

Зауважте, що ми також можемо перейменовувати підпрограми та об’єкти всередині пакетів.
Наприклад, ми могли просто перейменувати процедуру :ada:`Put_Line` як у прикладі нижче:

.. code:: ada run_button project=Courses.Intro_To_Ada.Modular_Programming.Rename_Put_Line

    with Ada.Text_IO;

    procedure Main is
       procedure Say (Something : String)
         renames Ada.Text_IO.Put_Line;
    begin
       Say ("Hello");
    end Main;

В цьому прикладі ми переіменували процедуру :ada:`Put_Line` в :ada:`Say`.
