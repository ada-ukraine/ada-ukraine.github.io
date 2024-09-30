Суворо типізована мова
======================

.. include:: ../../global.txt

Ada — це суворо типізована мова. Цікаво, що це відповідає сучасним трендам:
сильна статична типізація стає все більш популярною в розробці мов програмування
завдяки таким факторам, як розвиток статично типізованого функціонального
програмування, великий поштовх дослідницького співтовариства в області типізації
та багато практичних мов з системами суворого типізування.

.. _Intro_Ada_What_Is_A_Type:

Що такє тип?
------------

У статично типізованих мовах тип — це переважно (але не тільки) конструкція
*часу компіляції*. Це конструкція для забезпечення інваріантів щодо поведінки
програми. Інваріанти — це незмінні властивості, які зберігаються для всіх
змінних даного типу. Їх застосування гарантує, наприклад, що змінні типу даних
ніколи не матимуть недійсних значень.

Тип використовується для міркування про *об’єкти*, якими керує програма
(об’єкт — це змінна чи константа). Мета полягає в тому, щоб класифікувати
об’єкти за тим, що ви можете виконати з ними (тобто за дозволеними операціями),
і таким чином ви можете міркувати про правильність значень об’єктів.

.. _Intro_Ada_Integers:

Цілочислені типи
----------------

Приємною особливістю Ada є те, що ви можете визначати власні цілочислені типи на
основі вимог вашої програми (тобто діапазону значень, який має сенс). Насправді
механізм визначення, який надає Ada, формує семантичну основу для попередньо
визначених цілочисельних типів. У цьому відношенні немає «магічного» вбудованого
типу, який відрізняється від більшості мов і, можливо, дуже елегантний.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Integer_Type_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Integer_Type_Example is
       --  Декларуємо знаковий цілочислений
       --  тип і зазначаємо межі
       type My_Int is range -1 .. 20;
       --                         ^ Верхня межа
       --                   ^ Нижня межа

       --  Як і змінні, типи декларуються
       --  лише в декларативній частині.
    begin
       for I in My_Int loop
          Put_Line (My_Int'Image (I));
          --              ^ Атрибут 'Image
          --                перетворює значення
          --                на строку.
       end loop;
    end Integer_Type_Example;

Цей приклад ілюструє декларацію цілочисельного типу зі знаком і кілька
речей, які ми можемо з ними робити.

Кожна декларація типу в Ada починається з ключового слова :ada:`type`
(за винятком :ref:`task types <Intro_Ada_Task_Types>`). Після назви ми можемо побачити
діапазон, який дуже схожий на діапазони, які ми використовуємо в циклах :ada:`for`,
який визначає нижню та верхню межу типу. Кожне ціле число у цьому діапазоні є дійсним
значенням для типу.

.. admonition:: цілочислені типи Ada

   В Ada цілочисельний тип визначається не в термінах його
   машинного представлення, а радше в його діапазоні. Потім
   компілятор вибере найбільш відповідне представлення.

У наведеному вище прикладі слід звернути увагу на вираз :ada:`My_Int'Image (I)`.
Нотація :ada:`Name'Attribute (необов'язкові параметри)` використовується для того,
що в Ada називається атрибутом. Атрибут — це вбудована операція над типом,
значенням або іншою сутністю програми. Доступ до нього здійснюється за допомогою
символу :ada:`''` (апостроф ASCII).

Ada має декілька типів, доступних як «вбудовані»; :ada: `Integer` є одним із них.
Ось як можна визначити :ada:`Integer` для типового процесора:

.. code-block:: ada

    type Integer is
      range -(2 ** 31) .. +(2 ** 31 - 1);

:ada:`**` — оператор експоненти, що означає, що перше дійсне значення
для :ada:`Integer` дорівнює -2\ :sup:`31`, а останнє дійсне
значення — 2\ :sup:`31 ` - 1.

Ada не вимагає діапазону для вбудованого типу :ada:`Integer`. Реалізація
для 16-бітної платформи, ймовірно, вибере діапазон від -2\ :sup:`15`
до 2\ :sup:`15` - 1.


Семартика операцій
~~~~~~~~~~~~~~~~~~

На відміну від деяких інших мов, Ada вимагає, щоб операції над цілими числами
перевірялися на переповнення.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Overflow_Check
    :class: ada-run-expect-failure

    procedure Main is
       A : Integer := Integer'Last;
       B : Integer;
    begin
       B := A + 5;
       --  Ця операція призведе до переповненя, тому
       --  призведе до виникнення виключення під
       --  час виконання
    end Main;

Є два рівні перевірки переповнення:

* Переповнення на архітектурному рівні, коли результат операції перевищує
  максимальне значення (або менше мінімального значення), яке може бути
  представлене в сховищі, зарезервованому для об’єкта типу, і

* Переповнення на рівні типу, коли результат операції виходить за межі
  діапазону, визначеного для типу.

Здебільшого з міркувань ефективності, хоча переповнення на архітектурному рівні
завжди призводить до виключення, переповнення на рівні типу перевірятиметься лише
на певних етапах, як-от присвоєння:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Overflow_Check_2

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type My_Int is range 1 .. 20;
       A : My_Int := 12;
       B : My_Int := 15;
       M : My_Int := (A + B) / 2;
       --  Виключення небуде,
       --  результат в межах
    begin
       for I in 1 .. M loop
          Put_Line ("Hello, World!");
       end loop;
       --  Тіло циклу виконається 13 раз
    end Main;

Переповнення на рівні типу перевірятиметься лише в певних точках виконання.
Результатом, як ми бачимо вище, є те, що у вас може бути операція, яка
переповнюється під час проміжного обчислення, але виключення не створюється,
оскільки кінцевий результат не виходить за заявлені межі.

.. _Intro_Ada_Unsigned_Types:

Беззнакові типи
---------------

Ada також має беззнакові цілочисленні типи. На мові Ada вони називаються
*модульними* типами. Причина такої назви пов’язана з їхньою поведінкою у
разі переповнення: вони просто «обертаються», ніби була застосована операція
за модулем.

Для модульних типів машинного розміру, наприклад, модуль 2\ :sup:`32`, імітує
найпоширенішу поведінку реалізації беззнакових типів. Однак перевагою Ada є те,
що модуль є більш загальним:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Unsigned_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Mod_Int is mod 2 ** 5;
       --              ^ Межі 0 .. 31

       A : constant Mod_Int := 20;
       B : constant Mod_Int := 15;

       M : constant Mod_Int := A + B;
       --  Немає переповнення,
       --  M = (20 + 15) mod 32 = 3
    begin
       for I in 1 .. M loop
          Put_Line ("Hello, World!");
       end loop;
    end Main;

На відміну від C/C++, оскільки ця поведінка обертання гарантується специфікацією Ada,
ви можете покластися на неї для реалізації портативного коду. Крім того, можливість
використовувати обертання в довільних межах дуже корисна
|mdash| модуль не обов’язково має бути ступенем 2 |mdash| для реалізації певних
алгоритмів і структур даних, таких як :wikipedia:`кільцеві буфери <Circular_buffer>`.

.. _Intro_Ada_Enum_Types:

Перечислення
------------

Перечислимі типи є ще однією перевагою системи типів Ади. На відміну від перечислення C,
вони *не* цілі числа, і кожен новий перечислимий тип несумісний з іншими такими типами.
Перечислимі типи є частиною більшого сімейства дискретних типів, що робить їх придатними
для використання в певних ситуаціях, які ми опишемо пізніше, але один контекст, який ми
вже бачили, це оператор case.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Enumeration_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Enumeration_Example is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);
       --  Перечислимий тип
    begin
       for I in Days loop
          case I is
             when Saturday .. Sunday =>
                Put_Line ("Week end!");

             when Monday .. Friday =>
                Put_Line ("Hello on "
                          & Days'Image (I));
                --  Атрибут 'Image працює з
                --  перечислимими типами теж
          end case;
       end loop;
    end Enumeration_Example;

Перечислимі типи достатньо потужні, тому, на відміну від більшості мов,
вони використовуються для визначення стандартного логічного типу:

.. code-block:: ada

    type Boolean is (False, True);

Як згадувалося раніше, кожен «вбудований» тип в Ada визначається
загальнодоступними для користувача засобами.

Типи з плаваючою комою
----------------------

Базові властивості
~~~~~~~~~~~~~~~~~~

Як і більшість мов, Ada підтримує типи з плаваючою комою.
Найпоширенішим типом числа з плаваючою комою є :ada:`Float`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Floating_Point_Demo

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Demo is
       A : constant Float := 2.5;
    begin
       Put_Line ("The value of A is "
                 & Float'Image (A));
    end Floating_Point_Demo;

Програма відображатиме :ada:`2,5` як значення :ada:`A`.

Мова Ada не вказує точність (кількість десяткових цифр у мантисі) для Float;
на типовій 32-розрядній платформі точність буде 6.

Доступні всі загальні операції, які можна очікувати для типів із плаваючою комою,
включаючи абсолютне значення та піднесення до степеня. Наприклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Floating_Point_Operations

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Operations is
       A : Float := 2.5;
    begin
       A := abs (A - 4.5);
       Put_Line ("The value of A is "
                 & Float'Image (A));

       A := A ** 2 + 1.0;
       Put_Line ("The value of A is "
                 & Float'Image (A));
    end Floating_Point_Operations;

Значення :ada:`A` становить :ada:`2.0` після першої операції та
:ada:`5.0` після другої операції.

На додаток до :ada:`Float`, реалізація Ada може пропонувати типи даних з вищою
точністю, такі як :ada:`Long_Float` і :ada:`Long_Long_Float`. Як і :ada:`Float`,
стандарт не вказує точність цих типів: він лише гарантує, що тип :ada:`Long_Float`,
наприклад, має принаймні точність :ada:`Float`. Щоб гарантувати дотримання певної
вимоги до точності, ми можемо визначити власні типи з плаваючою комою, як ми
побачимо в наступному розділі.

Точність типів з плаваючою комою
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ada дозволяє користувачеві вказати точність для типу з плаваючою комою, виражену
десятковими цифрами. Тоді операції над цими користувацькими типами матимуть
принаймні вказану точність. Синтаксис простого оголошення типу з плаваючою комою:

.. code-block:: ada

    type T is digits <number_of_decimal_digits>;

Компілятор вибере представлення з плаваючою комою, яке підтримує необхідну точність.
Наприклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Custom_Floating_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Custom_Floating_Types is
       type T3  is digits 3;
       type T15 is digits 15;
       type T18 is digits 18;
    begin
       Put_Line ("T3  requires "
                 & Integer'Image (T3'Size)
                 & " bits");
       Put_Line ("T15 requires "
                 & Integer'Image (T15'Size)
                 & " bits");
       Put_Line ("T18 requires "
                 & Integer'Image (T18'Size)
                 & " bits");
    end Custom_Floating_Types;

У цьому прикладі атрибут :ada:`'Size` використовується для отримання кількості
бітів, які використовуються для вказаного типу даних. Як ми бачимо, запустивши
цей приклад, компілятор виділяє 32 біти для :ada:`T3`, 64 біти для :ada:`T15` і
128 бітів для :ada:`T18`. Це включає як мантису, так і експоненту.

Кількість цифр, указана в типі даних, також використовується у форматуванні під
час відображення змінних із плаваючою комою. Наприклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Display_Custom_Floating_Types

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Display_Custom_Floating_Types is
       type T3  is digits 3;
       type T18 is digits 18;

       C1 : constant := 1.0e-4;

       A : constant T3  := 1.0 + C1;
       B : constant T18 := 1.0 + C1;
    begin
       Put_Line ("The value of A is "
                 & T3'Image (A));
       Put_Line ("The value of B is "
                 & T18'Image (B));
    end Display_Custom_Floating_Types;

Як і очікувалося, програма відображатиме змінні відповідно до заданої
точності (1,00E+00 і 1,00010000000000000E+00).

Межі типів з плаваючою комою
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Окрім точності, для типу з плаваючою комою також можна вказати діапазон.
Синтаксис подібний до того, який використовується для цілочисельних типів
даних |mdash| використовуючи ключове слово :ada:`range`. Цей простий приклад
створює новий тип із плаваючою комою на основі типу :ada:`Float`, для
нормалізованого діапазону між :ada:`-1.0` і :ada:`1.0`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Floating_Point_Range

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Range is
       type T_Norm  is new Float range -1.0 .. 1.0;
       A  : T_Norm;
    begin
       A := 1.0;
       Put_Line ("The value of A is "
                 & T_Norm'Image (A));
    end Floating_Point_Range;

Програма відповідає за те, щоб змінні цього типу залишалися в цьому діапазоні;
інакше генерується виключення. У наступному прикладі виключення :ada:`Constraint_Error`
виникає під час призначення :ada:`2.0` змінній :ada:`A`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Floating_Point_Range_Exception
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Range_Exception is
       type T_Norm  is new Float range -1.0 .. 1.0;
       A  : T_Norm;
    begin
       A := 2.0;
       Put_Line ("The value of A is "
                 & T_Norm'Image (A));
    end Floating_Point_Range_Exception;

Діапазони також можна вказати для користувацьких типів з плаваючою комою. Наприклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Custom_Range_Types

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    procedure Custom_Range_Types is
       type T6_Inv_Trig  is
         digits 6 range -Pi / 2.0 .. Pi / 2.0;
    begin
       null;
    end Custom_Range_Types;

У цьому прикладі ми визначаємо тип під назвою :ada:`T6_Inv_Trig`, який має діапазон
від -π / 2 до π / 2 з мінімальною точністю 6 цифр.
(:ada:`Pi` визначено у стандартному пакеті :ada:`Ada.Numerics`.)

Сувора типізація
----------------

Як зазначалося раніше, Ada є суворо типізованою. В результаті різні типи однієї сім'ї
несумісні один з одним; значення одного типу не може бути присвоєно змінній іншого типу.
Наприклад:


.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Imperial_Metric_Error
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Illegal_Example is
       --  Декларація двух різних типів
       --  з плаваючою комою
       type Meters is new Float;
       type Miles  is new Float;

       Dist_Imperial : Miles;

       --  Декларація константи
       Dist_Metric : constant Meters := 1000.0;
    begin
       --  Не вірно: типи різні
       Dist_Imperial := Dist_Metric * 621.371e-6;
       Put_Line (Miles'Image (Dist_Imperial));
    end Illegal_Example;

.. _Intro_Ada_Type_Conversion:

Наслідком цих правил є те, що в загальному випадку вираз «змішаного режиму»,
наприклад :ada:`2 * 3.0`, викличе помилку компіляції. У таких мовах, як C
або Python, такі вирази стають дійсними шляхом неявних перетворень. В Ada такі
перетворення мають бути явними:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Imperial_Metric

    with Ada.Text_IO; use Ada.Text_IO;
    procedure Conv is
       type Meters is new Float;
       type Miles is new Float;
       Dist_Imperial : Miles;
       Dist_Metric : constant Meters := 1000.0;
    begin
       Dist_Imperial :=
         Miles (Dist_Metric) * 621.371e-6;
       --  ^^^^^^^^^^^^^^^^^
       --    Перетворення типу з Meters до Miles
       --    Тепер код корректний

       Put_Line (Miles'Image (Dist_Imperial));
    end Conv;

Звичайно, ми, ймовірно, не хочемо писати код перетворення кожного разу, коли ми
перетворюємо метри в милі. Ідіоматичний спосіб Ada в цьому випадку полягав би
в тому, щоб ввести функції перетворення разом із типами.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Imperial_Metric_Func

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Conv is
       type Meters is new Float;
       type Miles  is new Float;

       --  Декларація функції, як процедура
       --  але повертає значення.
       function To_Miles (M : Meters) return Miles is
       --                             ^ тип результату
       begin
          return Miles (M) * 621.371e-6;
       end To_Miles;

       Dist_Imperial : Miles;
       Dist_Metric   : constant Meters := 1000.0;
    begin
       Dist_Imperial := To_Miles (Dist_Metric);
       Put_Line (Miles'Image (Dist_Imperial));
    end Conv;

Якщо ви пишете багато коду з перетворюваннями, необхідність явного надання таких
перетворень спочатку може здатися болючою. Однак такий підхід має переваги.
Примітно, що ви можете покластися на відсутність неявних перетворень, що,
у свою чергу, запобігатиме деяким неявним помилкам.

.. admonition:: В інших мовах

    У C, наприклад, правила для неявних перетворень не завжди можуть
    бути цілком очевидними. Однак в Ada код завжди буде робити саме те,
    що він має робити. Наприклад:

    .. code-block:: c

        int a = 3, b = 2;
        float f = a / b;

    Цей код компілюватиметься добре, але результат :c:`f` буде 1,0 замість
    1,5, тому що компілятор згенерує цілочисельне ділення (три поділене на
    два), що призводить до одиниці. Розробник програмного забезпечення
    повинен знати про таке перетворення даних і використовувати відповідне
    приведення:

    .. code-block:: c

        int a = 3, b = 2;
        float f = (float)a / b;

    У виправленому прикладі компілятор перетворить обидві змінні на
    відповідне представлення з плаваючою комою перед виконанням ділення.
    Це дасть очікуваний результат.

    Цей приклад дуже простий, і досвідчені розробники C, ймовірно,
    помітять і виправлять його, перш ніж він створить більші проблеми.
    Однак у більш складних програмах, де оголошення типу не завжди видно
    |mdash| напр. при посиланні на елементи :c:`struct` |mdash| ця ситуація
    не завжди може бути очевидною та швидко призвести до дефектів програмного
    забезпечення, які може бути важче знайти.

    Компілятор Ada, навпаки, завжди відхиляє код, який поєднує змінні з
    плаваючою крапкою та цілі числа без явного перетворення. Наступний
    код Ada, заснований на помилковому прикладі в C, не компілюється:

    .. code:: ada compile_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Implicit_Cast
        :class: ada-expect-compile-error

        procedure Main is
           A : Integer := 3;
           B : Integer := 2;
           F : Float;
        begin
           F := A / B;
        end Main;

    Відповідний рядок потрібно змінити на :ada:`F := Float (A) / Float (B);`,
    щоб його прийняв компілятор.

  Ви можете використовувати сувору типізацію Ada, щоб допомогти застосувати
  інваріанти у вашому коді, як у прикладі вище: оскільки милі та метри є двома
  різними типами, ви не можете помилково перетворити екземпляр одного на
  екземпляр іншого.

Похідні типи
------------

В Ada ви можете створювати нові типи на основі існуючих. Це дуже корисно: ви
отримуєте тип, який має ті самі властивості, що й деякий існуючий тип, але
який розглядається як окремий тип в інтересах надійної типізації.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Derived_Types
    :class: ada-expect-compile-error

    procedure Main is
       --  Тип: ідентифікаційний номер
       --  несумісний з Integer.
       type Social_Security_Number is new Integer
         range 0 .. 999_99_9999;
       --      ^ Оскільки номер має максимум
       --        9 цифр, та не може бути
       --        негативним, ми накладаємо
       --        такі обмеження для типу.

       SSN : Social_Security_Number :=
         555_55_5555;
       --   ^ Ви можете додавати ʼ_ʼ для
       --     форматування будь якого числа.

       I   : Integer;

       --  Значення -1 нижче викличе виключення
       --  під час виконання і попередження
       --  під час компіляції з GNAT.
       Invalid : Social_Security_Number := -1;
    begin
       --  Невірно, різні типи:
       I := SSN;

       --  Також невірно:
       SSN := I;

       --  Вірно, з приведенням типів:
       I := Integer (SSN);

       --  Також вірно:
       SSN := Social_Security_Number (I);
    end Main;

Тип :ada:`Social_Security` називається *похідним типом*;
його *батьківським типом* є :ada:`Integer`.

Як показано в цьому прикладі, ви можете уточнити дійсний діапазон під час
визначення похідного скалярного типу (наприклад, ціле, з плаваючою комою
та перерахування).

Синтаксис для перечислень використовує синтаксис :ada:`range <range>`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Days

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       type Weekend_Days is new
         Days range Saturday .. Sunday;
       --  Новий тип, де тільки Saturday і Sunday
       --  є дійсними.
    begin
       null;
    end Greet;

Підтипи
-------

Як ми бачили, типи можуть використовуватися в Ada для встановлення обмежень
на допустимий діапазон значень. Однак іноді ми хочемо накласти обмеження на
деякі значення, залишаючись у межах одного типу. Ось тут і вступають у гру
підтипи. Підтип не створює новий тип.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Days_Subtype

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       --  Декларація підтипу
       subtype Weekend_Days is
         Days range Saturday .. Sunday;
       --     ^ Обмеження на підтип

       M : Days := Sunday;

       S : Weekend_Days := M;
       --  Немає помилки, Days та Weekend_Days
       --  є одного типу.
    begin
       for I in Days loop
          case I is
             --  Як і тип, підтип може
             --  бути використаний як діапазон
             when Weekend_Days =>
                Put_Line ("Week end!");
             when others =>
                Put_Line ("Hello on "
                          & Days'Image (I));
          end case;
       end loop;
    end Greet;

Кілька підтипів попередньо визначені в стандартному пакеті Ada та
автоматично доступні для вас:

.. code-block:: ada

    subtype Natural  is Integer range 0 .. Integer'Last;
    subtype Positive is Integer range 1 .. Integer'Last;

Хоча підтипи одного типу статично сумісні один з одним, обмеження
застосовуються під час виконання: якщо ви порушите обмеження підтипу,
буде згенеровано виключення.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Days_Subtype_Error
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       subtype Weekend_Days is
         Days range Saturday .. Sunday;

       Day     : Days := Saturday;
       Weekend : Weekend_Days;
    begin
       Weekend := Day;
       --         ^ Вірно: той самий тип, підтип
       --           обмеження не порушені
       Weekend := Monday;
       --         ^ Невірне значення для підтипу
       --           виключення під час виконання
    end Greet;

.. _Intro_Ada_Subtype_Aliases:

Підтипи як псевдоніми типів
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Раніше ми бачили, що можемо створювати нові типи, оголосивши, наприклад,
:ada:`type Miles is new Float`. Ми також можемо створити псевдоніми типів,
які генеруватимуть альтернативні імена |mdash| *псевдоніми* |mdash| для
відомих типів. Зауважте, що псевдоніми типів іноді називають *синонімами типу*.

Ми досягаємо цього в Ada, використовуючи підтипи без нових обмежень. Однак
у цьому випадку ми не отримуємо всіх переваг перевірки типу Ada. Давайте
перепишемо приклад, використовуючи псевдоніми типів:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Undetected_Imperial_Metric_Error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Undetected_Imperial_Metric_Error is
       --  Декларуємо два псевдоніми
       subtype Meters is Float;
       subtype Miles is Float;

       Dist_Imperial : Miles;

       --  Декларація константи
       Dist_Metric : constant Meters := 100.0;
    begin
       --  Приведення типів не потрібні
       Dist_Imperial := (Dist_Metric * 1609.0)
                          / 1000.0;

       --  Невірно по лозіці але не виявиться
       --  під час компіляції:
       Dist_Imperial := Dist_Metric;

       Put_Line (Miles'Image (Dist_Imperial));
    end Undetected_Imperial_Metric_Error;

У прикладі вище той факт, що і :ada:`Meters`, і :ada:`Miles` є підтипами
:ada:`Float`, дозволяє нам змішувати змінні обох типів без приведення типів.
Однак це може призвести до різного роду помилок у программі, яких ми хотіли
б уникнути, як ми бачимо в невиявленій помилці, виділеній у коді вище.
У цьому прикладі помилка в присвоєнні значення в метрах змінній, призначеній
для зберігання значень у милях, залишається непоміченою, тому що і :ada:`Meters`,
і :ada:`Miles` є підтипами :ada:`Float`. Тому рекомендується використовувати
жорстку типізацію |mdash| через :ada:`type X is new Y` |mdash| для таких випадків,
як наведений вище.

Однак є багато ситуацій, коли псевдоніми типів корисні. Наприклад, у програмі,
яка використовує типи з плаваючою комою в кількох контекстах, ми можемо
використовувати псевдоніми типів, щоб вказати додаткове значення типів або
уникнути довгих імен змінних. Наприклад, замість того, щоб писати:

.. code-block:: ada

    Paid_Amount, Due_Amount : Float;

Ми можемо написати:

.. code-block:: ada

    subtype Amount is Float;

    Paid, Due : Amount;

.. admonition:: В інших мовах

    У C, наприклад, ми можемо використовувати декларацію :c:`typedef` для
    створення псевдоніма типу. Наприклад:

    .. code-block:: c

        typedef float meters;

    Це відповідає декларації, яку ми бачили вище з використанням підтипів.
    Інші мови програмування включають цю концепцію подібним чином.
    Наприклад:

        - C++: ``using meters = float;``
        - Swift: ``typealias Meters = Double``
        - Kotlin: ``typealias Meters = Double``
        - Haskell: ``type Meters = Float``

Зауважте, однак, що підтипи в Ada відповідають псевдонімам типів тоді
і тільки тоді, коли вони не мають нових обмежень. Таким чином, якщо ми
додамо нове обмеження до оголошення підтипу, у нас більше не буде псевдоніма
типу. Наприклад, таке оголошення *не* можна вважати псевдонімом типу :ada:`Float`:

.. code-block:: ada

    subtype Meters is Float range 0.0 .. 1_000_000.0;

Розглянемо інший приклад:

.. code-block:: ada

    subtype Degree_Celsius is Float;

    subtype Liquid_Water_Temperature is
      Degree_Celsius range 0.0 .. 100.0;

    subtype Running_Water_Temperature is
      Liquid_Water_Temperature;

У цьому прикладі :ada:`Liquid_Water_Temperature` не є псевдонімом
:ada:`Degree_Celsius`, оскільки він додає нове обмеження, яке не було
частиною оголошення :ada:`Degree_Celsius`. Однак у нас є псевдоніми
двох типів:

- :ada:`Degree_Celsius` є псевдонімом :ada:`Float`;
- :ada:`Running_Water_Temperature` є псевдонімом
  :ada:`Liquid_Water_Temperature`, навіть якщо
  :ada:`Liquid_Water_Temperature` сама по собі
  має обмежений діапазон.
