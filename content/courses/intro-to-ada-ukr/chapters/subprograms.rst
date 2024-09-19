Підпрограми
===========

.. _Intro_Ada_Subprograms:

.. include:: ../../global.txt

Підпрограми
-----------

Досі ми використовували процедури, здебільшого, щоб мати головну підпрограму
для виконання. Процедури є одним із видів *підпрограм*.

В Ada є два типи підпрограм: *функції* та *процедури*. Різниця між ними полягає
в тому, що функція повертає результат, а процедура – ​​ні.

У наступному прикладі показано специфікацію та реалізацію функції:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Increment

    function Increment (I : Integer) return Integer;
    --  Оголошення (не реалізація) функції з
    --  одним параметром, яка повертає цілочисленне
    --  значення

    function Increment (I : Integer) return Integer is
       --  Тут функція реалізовується
    begin
        return I + 1;
    end Increment;

Підпрограми в Ada, зазвичай, мають параметри. Одне важливе синтаксичне зауваження
полягає в тому, що підпрограма, яка не має параметрів, взагалі не має розділу
для параметрів, наприклад:

.. code-block:: ada

   procedure Proc;

   function Func return Integer;

Зверніть увагу на відсутність дужок.

Ось ще один приклад підпрограми:

.. code:: ada no_button project=Courses.Intro_To_Ada.Subprograms.Increment_By
    :class: ada-syntax-only

    function Increment_By
      (I    : Integer := 0;
       Incr : Integer := 1) return Integer;
    --                ^ Значення параметрів
    --                  за замовчуванням

У цьому прикладі ми бачимо, що параметри можуть мати значення за замовчуванням.
Під час виклику підпрограми ви можете не вказувати параметри, якщо вони мають такі
значення. На відміну від C/C++, виклик підпрограми без параметрів не містить дужок.

Це реалізація цієї функції:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Increment_By

    function Increment_By
      (I    : Integer := 0;
       Incr : Integer := 1) return Integer is
    begin
       return I + Incr;
    end Increment_By;

.. admonition:: В інструментах GNAT

   Стандарт Ada не визначає, у якому файлі має зберігатися специфікація або
   реалізація підпрограми. Іншими словами, стандарт не вимагає певної структури
   файлів або певних розширень імен файлів. Наприклад, ми могли б зберегти і
   специфікацію, і реалізацію функції :ada:`Increment` у файлі під назвою
   :file:`increment.txt` (ми навіть могли б зберігати весь код застосунку
   в одному файлі). З точки зору стандарту, це було б цілком прийнятно.

   Проте для інструментів GNAT потрібна така схема іменування файлів:

   - файли з розширенням `.ads` містять специфікацію, а

   - файли з розширенням `.adb` містять реалізацію.

   Таким чином, для інструментів GNAT специфікація функції :ada:`Increment` має
   зберігатися у файлі :file:`increment.ads`, тоді як її реалізація має зберігатися
   у файлі :file:`increment.adb`. Це правило завжди стосується пакетів, які ми
   обговоримо :doc:`пізніше <./modular_programming>` (однак зауважте, що це правило
   можна обійти). Для отримання додаткової інформації ви можете звернутися до курсу
   :doc:`Вступ до інструментів GNAT </courses/GNAT_Toolchain_Intro/index>` або до
   `Посібник користувача GPRbuild
   <https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug.html>`_.


Виклик підпрограм
~~~~~~~~~~~~~~~~~

Ми можемо визвати нашу підпрограму таким чином:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Increment_By
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Increment_By;

    procedure Show_Increment is
       A, B, C : Integer;
    begin
       C := Increment_By;
       --              ^ Виклик без параметрів
       --                значення I є 0
       --                та Incr є 1

       Put_Line ("Using defaults for Increment_By is "
                 & Integer'Image (C));

       A := 10;
       B := 3;
       C := Increment_By (A, B);
       --                 ^ Виклик з параметрами

       Put_Line ("Increment of "
                 & Integer'Image (A)
                 & " with "
                 & Integer'Image (B)
                 & " is "
                 & Integer'Image (C));

       A := 20;
       B := 5;
       C := Increment_By (I    => A,
                          Incr => B);
        --                ^ Виклик з іменуванням
        --                  параметрів

       Put_Line ("Increment of "
                 & Integer'Image (A)
                 & " with "
                 & Integer'Image (B)
                 & " is "
                 & Integer'Image (C));
    end Show_Increment;

Ada дозволяє вам передавати параметри за іменами, незалежно від того, чи мають
вони значення за замовчуванням чи ні. Проте є деякі правила:

- Не іменовані параметри ідуть в порядку об'явлення.

- Після іменованих параметрів не можуть ідти порядкові.

Зазвичай, параметри іменують при виклику, якщо відповідні параметри підпрограми
мають значення за замовчуванням. Однак також цілком прийнятно іменувати кожен
параметр, якщо це робить код зрозумілішим.

Вкладені підпрограми
~~~~~~~~~~~~~~~~~~~~

Як коротко згадувалося раніше, Ada дозволяє вам оголошувати одну підпрограму
всередині іншої.

Це корисно з двох причин:

- Це дозволяє зробити ваші програми структурованішими. Якщо вам потрібна
  підпрограма тільки як «помічник» для іншої підпрограми, то принцип
  локалізації вказує на те, що допоміжну підпрограму слід зробити вкладеною.

- Це дозволяє вам легко надавати доступ та контролювати його, оскільки
  вкладені підпрограми мають доступ до параметрів, а також будь-яких
  локальних змінних, оголошених до вкладеної підпрограми.

У попередньому прикладі ми можемо перемістити дубльований код
(виклик :ada:`Put_Line`) до окремої процедури. Ось версія з вкладеною
процедурою :ada:`Display_Result`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Increment_By
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Increment_By;

    procedure Show_Increment is
       A, B, C : Integer;

       procedure Display_Result is
       begin
          Put_Line ("Increment of "
                    & Integer'Image (A)
                    & " with "
                    & Integer'Image (B)
                    & " is "
                    & Integer'Image (C));
       end Display_Result;

    begin
       A := 10;
       B := 3;
       C := Increment_By (A, B);
       Display_Result;
       A := 20;
       B := 5;
       C := Increment_By (A, B);
       Display_Result;
    end Show_Increment;

Виклик функцій
~~~~~~~~~~~~~~

Важливою особливістю викликів функцій в Ada є те, що зезультат, що повертається
під час виклику, не можна ігнорувати; тобто виклик функції не можна використовувати
як оператор.

Якщо ви хочете викликати функцію і не потребуєте її результату, вам усе одно
потрібно буде явно зберегти її в локальній змінній.

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Quadruple
    :class: ada-expect-compile-error

    function Quadruple (I : Integer)
                        return Integer is

        function Double (I : Integer)
                         return Integer is
        begin
           return I * 2;
        end Double;

       Res : Integer := Double (Double (I));
       --               ^ Виклик Double
    begin
       Double (I);
       --  ПОМИЛКА: не можна викликати функцію
       --           "Double" як оператор

       return Res;
    end Quadruple;

.. admonition:: В інструментах GNAT

    У GNAT, коли всі попередження активовані, стає ще важче ігнорувати
    результат функції, оскільки невикористані змінні будуть позначені.
    Наприклад, цей код буде невірним:

    .. code-block:: ada

        function Read_Int
           (Stream :     Network_Stream;
            Result : out Integer) return Boolean;

        procedure Main is
            Stream : Network_Stream := Get_Stream;
            My_Int : Integer;

            -- Попередження в рядку нижче:
            --    Значення В не використовується
            B : Boolean := Read_Int (Stream, My_Int);
        begin
           null;
        end Main;

    Тоді у вас є два варіанти, щоб вимкнути це попередження:

    - Або додайте до змінної :ada:`pragma Unreferenced`, наприклад:

    .. code-block:: ada

        B : Boolean := Read_Int (Stream, My_Int);
        pragma Unreferenced (B);

    - Або використайте одне з імен :ada:`discard` :ada:`dummy` :ada:`ignore`
      :ada:`junk` :ada:`unused` (незалежно від регістру) як ім'я змінної.

    Будь який з варіанів означає що Ви свідомо і в "ручну" вимикаєте таку
    перевірку для цього конкретного місця (гарантуючи його корректність).


.. _Intro_Ada_Parameter_Modes:

Режими доступу до параметрів
----------------------------

Поки що ми бачили, що Ada є мовою, орієнтованою на безпеку. Є багато способів
це реалізувати. Розглянемо два важливі моменти:

- Ada змушує програміста вказати якомога більше про очікувану поведінку
  програми, щоб компілятор міг попередити або відхилити, якщо є
  невідповідність.

- Ada надає різноманітні методи для досягнення одноманітності та гнучкості
  посилань і динамічного керування пам’яттю, але без недоліків таких як
  витік пам’яті та завислі посилання.

Режими параметрів — це можливість, яка допомагає досягти двох цілей проектування,
наведених вище. Параметр підпрограми можна обмежити за допомогою режиму доступу,
який є одним із наведених:

+---------------+---------------------------------------------+
| :ada:`in`     | Тільки для зчитування, зміна не можлива     |
+---------------+---------------------------------------------+
| :ada:`out`    | Запис можливий, значення не існує до запису |
+---------------+---------------------------------------------+
| :ada:`in out` | Зчитування та запис                         |
+---------------+---------------------------------------------+

Режимом за замовчуванням (коли нічого не вказано) для параметрів
є :ada:`in`; досі більшість прикладів використовували параметри
в режимі доступу :ada:`in`.

.. admonition:: Історично

    Функції та процедури спочатку мали більше відмінностей.
    До Ada 2012 функції могли мати лише параметри :ada:`in`.

Виклики підпрограм
------------------

Параметри In
~~~~~~~~~~~~

Перший режим для параметрів - це той, який ми неявно використовували досі.
Параметри, передані за допомогою цього режиму, не можна змінювати, тому
наступна програма викличе помилку:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Swap
    :class: ada-expect-compile-error

    procedure Swap (A, B : Integer) is
       Tmp : Integer;
    begin
       Tmp := A;

       --  ПОМИЛКА: зміна "in" параметра
       --         заборонена
       A := B;

       --  ПОМИЛКА: зміна "in" параметра
       --         заборонена
       B := Tmp;
    end Swap;

Той факт, що :ada:`in` є режимом за замовчуванням, дуже важливий.
Це означає, що параметр не буде змінено, якщо ви явно не вкажете
режим, у якому це дозволено.

Параметри In out
~~~~~~~~~~~~~~~~

Розглянемо наступний приклад з використанням :ada:`in out` режиму доступу:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.In_Out_Params
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure In_Out_Params is
       procedure Swap (A, B : in out Integer) is
          Tmp : Integer;
       begin
          Tmp := A;
          A   := B;
          B   := Tmp;
       end Swap;

       A : Integer := 12;
       B : Integer := 44;
    begin
        Swap (A, B);

        --  Виведе на екран 44
        Put_Line (Integer'Image (A));
    end In_Out_Params;

Параметр :ada:`in out` дозволено читати та записувати, тому у наведеному
вище прикладі ми бачимо, що значення :ada:`A` змінено після виклику :ada:`Swap` .

.. attention::

    Хоча параметри :ada:`in out` виглядають дещо як посилання в C++ або звичайні
    параметри в Java, які передаються за посиланням, стандарт мови Ada не вимагає
    передачі таких параметрів «за посиланням», за винятком певних категорій типів,
    як буде пояснено пізніше.

    Загалом, краще розглядати режими ширше, ніж семантику за значенням і за
    посиланням. Для компілятора це означає, що масив, переданий як параметр
    :ada:`in`, може бути переданий за посиланням, оскільки це більш ефективно
    (що нічого не змінює для користувача, оскільки параметр не можна змінити).
    Однак параметр дискретного типу завжди передаватиметься через копію,
    незалежно від його режиму (який більш ефективний у більшості архітектур).

Параметри Out
~~~~~~~~~~~~~

Режим :ada:`out` застосовується, коли підпрограмі потрібно записати параметр,
який може бути неініціалізованим у точці виклику. Читання значення параметра
:ada:`out` дозволено, але це слід робити лише після того, як сама підпрограма
призначить йому значення. Такі параметри подібні до результатів, що повертаються
функціями. Коли підпрограма завершується, фактичне значення буде тим, що у
параметра було в точці повернення.

.. admonition:: В інших мовах

    Ada не має конструкції яка дозволяє повертати кілька значень із функції
    (за винятком використання типу - запису/record). Отже, спосіб повернути
    кілька значень із підпрограми — це використовувати параметри :ada:`out`.

Наприклад, підпрограми, які читають цілі числа з мережі, може мати одну з
наступних специфікацій:

.. code-block:: ada

    procedure Read_Int
       (Stream  :     Network_Stream;
        Success : out Boolean;
        Result  : out Integer);

    function Read_Int
       (Stream :     Network_Stream;
        Result : out Integer) return Boolean;

Зчитування змінної з таким режимом доступу перед записом у неї в ідеалі
має викликати помилку, яка, як правило, спричинить або неефективні перевірки
під час виконання, або складні правила під час компіляції. Отже, з точки зору
користувача такий параметр діє як неініціалізована змінна, коли підпрограма
викликається.

.. admonition:: В інструментах GNAT

    GNAT виявить прості випадки неправильного використання таких параметрів.
    Наприклад, компілятор видасть попередження для наступної процедури:

    .. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Out_Params

        procedure Outp is
           procedure Foo (A : out Integer) is
              B : Integer := A;
              --             ^ Попередження про доступ
              --               до неініціалізованої A
           begin
              A := B;
           end Foo;
        begin
           null;
        end Outp;

Завчасна декларація підпрограм
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Як ми бачили раніше, підпрограму можна декларувати окремо від реалізації.
Це можливо загалом і може бути корисним, якщо вам потрібно, щоб підпрограми
були взаємно рекурсивними (викликають одна-одну), як у прикладі нижче:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Mutually_Recursive_Subprograms
    :class: ada-run

    procedure Mutually_Recursive_Subprograms is
        procedure Compute_A (V : Natural);
        --  Декларація Compute_A

        procedure Compute_B (V : Natural) is
        begin
           if V > 5 then
              Compute_A (V - 1);
              --  Виклик Compute_A
           end if;
        end Compute_B;

        procedure Compute_A (V : Natural) is
        begin
           if V > 2 then
              Compute_B (V - 1);
              --  Виклик Compute_B
           end if;
        end Compute_A;
    begin
       Compute_A (15);
    end Mutually_Recursive_Subprograms;

.. _Intro_Ada_Subprogram_Renaming:

Перенайменування
----------------

Підпрограми можна перейменувати за допомогою ключового слова :ada:`renames`
і вказання нової назви для неї:

.. code-block:: ada

    procedure New_Proc renames Original_Proc;

Це може бути корисно, наприклад, для покращення читабельності вашої програми,
коли ви використовуєте код із зовнішніх джерел, який не можна змінити у вашій
системі. Давайте розглянемо приклад:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Proc_Renaming
   :class: nosyntax-check

    procedure A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed
      (A_Message : String);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed
      (A_Message : String) is
    begin
       Put_Line (A_Message);
    end A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

Як випливає з формулювання в назві процедури вище, ми не можемо змінити її назву.
Однак ми можемо перейменувати його на щось на зразок :ada:`Show` у нашій програмі
та використати це коротше ім’я. Зауважте, що ми також повинні оголосити всі параметри
оригінавльної підпрограми. Ми також можемо перейменувати їх. Наприклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Proc_Renaming
   :class: nosyntax-check

    with A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

    procedure Show_Renaming is

       procedure Show (S : String) renames
         A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

    begin
       Show ("Hello World!");
    end Show_Renaming;

Зауважте, що оригінальна назва
(:ada:`A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed`)
все ще може бути використана після оголошення процедури :ada:`Show`.

Ми також можемо перейменувати підпрограми зі стандартної бібліотеки.
Наприклад, ми можемо перейменувати :ada:`Integer'Image` на :ada:`Img`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Integer_Image_Renaming

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Image_Renaming is

       function Img (I : Integer) return String
         renames Integer'Image;

    begin
       Put_Line (Img (2));
       Put_Line (Img (3));
    end Show_Image_Renaming;

Перейменування також дозволяє нам вводити значення за замовчуванням для параметрів,
які були недоступні в оригінальній декларації. Наприклад, ми можемо вказати
:ada:`"Hello World!"` як значення за замовчуванням для параметра :ada:`String`
процедури :ada:`Show`:

.. code-block:: ada

    with A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

    procedure Show_Renaming_Defaults is

       procedure Show (S : String := "Hello World!")
         renames
           A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

    begin
       Show;
    end Show_Renaming_Defaults;
