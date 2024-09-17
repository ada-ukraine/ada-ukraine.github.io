Імперативна мова
================

.. include:: ../../global.txt

Ada це багатопарадигмальна мова з підтримкою об’єктно-орієнтованого програмування
та деяких елементів функціонального програмування, але її ядром є проста, узгоджена
процедурна/імперативна мова, схожа на C або Pascal.

.. admonition:: В інших мовах

    Одна важлива відмінність між Ada та такою мовою, як C, полягає в тому,
    що оператори та вирази дуже чітко розрізняються. В Ada, якщо ви спробуєте
    використати вираз, де потрібен оператор, ваша програма не зможе скомпілюватися.
    Це правило підтримує корисний стилістичний принцип: вирази призначенні для
    отримання значень а не побічних ефектів. Це також може запобігти деяким помилкам,
    таким як помилкове використання оператора рівності :ada:`=` замість операції
    присвоювання :ada:`:=` де потрібне саме присвоєння.

Hello world
-----------

Ось дуже проста імперативна програма Ada:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet

    with Ada.Text_IO;

    procedure Greet is
    begin
       --  Вивести "Hello, World!" на екран
       Ada.Text_IO.Put_Line ("Hello, World!");
    end Greet;

яка може знаходитися в файлі :file:`greet.adb`.

.. only:: builder_html

    Якщо ви скомпілюєте цей файл за допомогою компілятора GNAT і запустите
    на виконання, ви отримаєте очікуваний результат.

    .. code-block:: sh

        $ gprbuild greet.adb
        using project file [...]_default.gpr
        Compile
           [Ada]          greet.adb
        Bind
           [gprbind]      greet.bexch
           [Ada]          greet.ali
        Link
           [link]         greet.adb

         $ ./greet
        Hello, World!
         $

У наведеній вище програмі є кілька цікавих речей:

-  Підпрограма в Ada може бути або процедурою, або функцією. Процедура,
   як показано вище, не повертає значення під час виклику.

-  :ada:`with` використовується для посилання на зовнішні модулі, які
   потрібні процедурі. Це схоже на ``import`` у інших мовах або
   приблизно схоже на :c:`#include` у C та C++.
   Пізніше ми побачимо, як вони працюють у деталях. Тут нам потрібен
   модуль стандартної бібліотеки, пакет :ada:`Ada.Text_IO`, який містить
   процедуру друку тексту на екрані: :ada:`Put_Line`.

-  :ada:`Greet` це процедура та основна точка входу для нашої першої
   програми. На відміну від C або C++, її можна назвати як завгодно.
   Точку входу визначить побудовник. У нашому простому прикладі
   :program:`gprbuild`, побудовник GNAT, використовуватиме файл, який
   Ви передали як параметр.

-  :ada:`Put_Line` це процедура, як і :ada:`Greet`, за винятком того,
   що вона оголошена в модулі :ada:`Ada.Text_IO`.
   Це еквівалент С :c:`printf`.

-  Коментарі починаються з :ada:`--` і йдуть до кінця рядка. Синтаксису
   багаторядкового коментаря немає, тобто неможливо почати коментар в
   одному рядку з продовженням в наступному рядку. Єдиний спосіб
   створити кілька рядків коментарів в Ada, це використовувати :ada:`--`
   для кожного рядка. Наприклад:

.. code-block:: ada

    --  Ми починаємо коментар в цьому рядку
    --  і продовжуємо в наступному

.. admonition:: В інших мовах

    Процедури подібні до функцій у C або C++, які повертають :c:`void`. 
    Пізніше ми побачимо, як оголошувати функції в Ada.

Ось мінімалістичний варіант прикладу "Hello, World":

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_2

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
    begin
       --  Вивести "Hello, World!" на екран
       Put_Line ("Hello, World!");
    end Greet;

Ця версія використовує ключове слово :ada:`use`, яке має
форму :ada:`use` *ім'я-пакету*. Як видно з виклику
:ada:`Put_Line`, ефект полягає в тому, що на сутності з
названого пакета можна посилатися безпосередньо, без
необхідності вказувати *ім'я-пакету.* попереду.


.. _Intro_Ada_If_Statement:

Імперативна мова - If/Then/Else
----------------------------------

У цьому розділі описується оператор :ada:`if` і представлено кілька інших
фундаментальних можливостей мови, включаючи цілочисельний ввід/вивід,
оголошення змінних і режими доступу до параметрів підпрограм.

Оператор :ada:`if` не дивує за формою та функціями:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Positive

    with Ada.Text_IO;         use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Positive is
       N : Integer;
    begin
       --  Виводимо запит на екран
       Put ("Enter an integer value: ");

       --  Зчитуємо цілочисленне значення
       Get (N);

       if N > 0 then
          --  Виводимо число
          Put (N);
          Put_Line (" is a positive number");
       end if;
    end Check_Positive;

Оператор :ada:`if` складається як мінімум із зарезервованого слова :ada:`if`,
умови (яка має бути логічним значенням), зарезервованого слова :ada:`then` і
непорожньої послідовності операторів (частина :ada:`then`) яка виконується,
якщо умова оцінюється як Істина і завершальне :ada:`end if`.

У цьому прикладі оголошується цілочисленна змінна N, запитується у користувача
ціле число, перевіряється, чи значення є додатним, і, якщо так, відображається
ціле значення, за яким слідує рядок "це додатне число". Якщо значення відємне,
процедура не відображає жодних результатів.

Тип Integer є попередньо визначеним типом зі знаком, і його діапазон залежить
від архітектури комп’ютера. На типових сучасних процесорах ціле число має
32-розрядний знак.

Приклад ілюструє деякі базові функції для цілочисельного введення-виведення.
Відповідні підпрограми знаходяться у пакеті :ada:`Ada.Integer_Text_IO` і включають
процедуру :ada:`Get` (яка читає число з клавіатури) і процедуру :ada:`Put`
(яка відображає ціле число).

Далі невеликі модифікації прикладу, які ілюструють оператор :ada:`if` із частиною :ada:`else`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Positive_2

    with Ada.Text_IO;         use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Positive is
       N : Integer;
    begin
       --  Виводимо запит на екран
       Put ("Enter an integer value: ");

       --  Зчитуємо цілочисленне значення
       Get (N);

       --  Виводимо число
       Put (N);

       if N > 0 then
          Put_Line (" is a positive number");
       else
          Put_Line (" is not a positive number");
       end if;
    end Check_Positive;

У цьому прикладі, якщо вхідне значення є відємним, програма відображає
значення, за яким слідує рядок "не є додатним числом".

Останній варіант ілюструє оператор :ada:`if` з декількома :ada:`elsif`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Direction

    with Ada.Text_IO;         use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Direction is
       N : Integer;
    begin
       Put ("Enter an integer value: ");
       Get (N);
       Put (N);

       if N = 0 or N = 360 then
          Put_Line (" is due north");
       elsif N in 1 .. 89 then
          Put_Line (" is in the northeast quadrant");
       elsif N = 90 then
          Put_Line (" is due east");
       elsif N in 91 .. 179 then
          Put_Line (" is in the southeast quadrant");
       elsif N = 180 then
          Put_Line (" is due south");
       elsif N in 181 .. 269 then
          Put_Line (" is in the southwest quadrant");
       elsif N = 270 then
          Put_Line (" is due west");
       elsif N in 271 .. 359 then
          Put_Line (" is in the northwest quadrant");
       else
          Put_Line (" is not in the range 0..360");
       end if;
    end Check_Direction;

У цьому прикладі очікується, що користувач введе ціле число від
0 до 360 включно, і відображається, якому квадранту чи осі
відповідає значення. Оператор :ada:`in` перевіряє, чи знаходиться
скалярне значення в заданому діапазоні, і повертає логічний результат.
Ефект від програми має бути зрозумілим; пізніше ми побачимо
альтернативний і ефективніший стиль для досягнення того самого ефекту
за допомогою оператора :ada:`case`.

Ключове слово :ada:`elsif` відрізняється від C або C++, де замість нього
використовуються вкладені блоки :ada:`else .. if`. І ще одна відмінність
полягає в наявності :ada:`end if`, що дозволяє уникнути проблеми,
відомої як «висячий else».


.. _Intro_Ada_Loop_Statement:

Імперативна мова - Цикли
------------------------

Ada має три способи визначення циклів. Вони відрізняються від циклів
C / Java / Javascript, проте більш простим синтаксисом і семантикою
відповідно до філософії Ada.

Цикли For
~~~~~~~~~

Першим типом циклу є цикл :ada:`for`, який дозволяє виконувати ітерацію
в дискретному діапазоні.

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_5a

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_5a is
    begin
       for I in 1 .. 5 loop
          --  Виклик процедури Put_Line
          Put_Line ("Hello, World!"
                      & Integer'Image (I));
          --        ^ Procedure parameter
       end loop;
    end Greet_5a;

.. only:: builder_html

    Виконання дає такий результат:

    .. code-block:: sh

       Hello, World! 1
       Hello, World! 2
       Hello, World! 3
       Hello, World! 4
       Hello, World! 5

Кілька речей, на які варто звернути увагу:

-  :ada:`1 .. 5` це дискретний діапазон від :ada:`1` до :ada:`5` включно.

-  Параметр циклу :ada:`I` (назва довільна) у тілі циклу має значення
   в цьому діапазоні.

-  :ada:`I` є локальним для циклу, тому ви не можете посилатися на :ada:`I`
   поза циклом.

-  Хоча значення :ada:`I` збільшується на кожній ітерації, з точки зору коду
   всередині циклу воно є постійним. Змінювати його неможна. Спроба змінити
   його значення призведе до помилки компіляції.

.. _Intro_Ada_Image_Attribute:

-  :ada:`Integer'Image` це функція, яка приймає ціле число та перетворює його
   на :ada:`String`. Це приклад мовної конструкції, відомої як *атрибут*,
   позначений синтаксисом :ada:`'`, який буде розглянуто більш детально пізніше.

-  Символ :ada:`&` є оператором конкатенації для строкових значень

-  :ada:`end loop` позначає кінець циклу

«Крок» циклу обмежений 1 (напрямок вперед) і -1 (назад). Щоб виконати ітерацію
в діапазоні назад, використовуйте ключове слово :ada:`reverse`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_5a_Reverse

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_5a_Reverse is
    begin
       for I in reverse 1 .. 5 loop
          Put_Line ("Hello, World!"
                    & Integer'Image (I));
       end loop;
    end Greet_5a_Reverse;

.. only:: builder_html

    Виконання дає такий результат:

    .. code-block:: sh

       Hello, World! 5
       Hello, World! 4
       Hello, World! 3
       Hello, World! 2
       Hello, World! 1

Межі циклу :ada:`for` можуть бути обчислені під час виконання; вони
обчислюються один раз перед виконанням тіла циклу. Якщо значення
верхньої межі менше значення нижньої, то цикл не виконується взагалі.
Це також стосується циклів :ada:`reverse`. Таким чином, у наступному
прикладі виводу на екран не буде:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_No_Op

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_No_Op is
    begin
       for I in reverse 5 .. 1 loop
          Put_Line ("Hello, World!"
                    & Integer'Image (I));
       end loop;
    end Greet_No_Op;

Докладніше про цикл :ada:`for` буде підніше.

.. _Intro_Ada_Bare_Loops:

Безумовний цикл
~~~~~~~~~~~~~~~

Найпростішим циклом в Ada є безумовний цикл, який є основою для інших
типів циклів Ada.

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_5b

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_5b is
       --  Оголошуємо змінну:
       I : Integer := 1;
       --  ^ Тип
       --             ^ Начальне значення
    begin
       loop
          Put_Line ("Hello, World!"
                    & Integer'Image (I));

          --  Вихід із циклу:
          exit when I = 5;
          --        ^ Умова

          --  Присвоєння:
          I := I + 1;
          --  Конструкції як в С 'I++' немає
       end loop;
    end Greet_5b;

Цей приклад дає той самий ефект, що й :ada:`Greet_5a`, показаний раніше.

Він ілюструє кілька концепцій:

-  Ми оголосили змінну з назвою :ada:`I` між :ada:`is` і :ada:`begin`.
   Це являє собою *декларативний блок*. Ada чітко відокремлює декларативну
   область від суто коду підпрограми. Оголошення може відбуватися в
   декларативній області, але не допускається посеред коду.

-  Цикл починається ключовим словом :ada:`loop` і, як і будь-який тип
   циклу, завершується комбінацією ключових слів :ada:`end loop`. Сам по собі
   це нескінченний цикл. Ви можете вийти з цього за допомогою оператора :ada:`exit`

-  Синтаксис присвоєння: :ada:`:=`, а рівності — :ada:`=`. Немає способу
   сплутати їх, оскільки, як зазначалося раніше, в Ada твердження та вирази
   є різними, а вирази не є дійсними твердженнями.


Цикл While
~~~~~~~~~~

Останній вид циклу в Ada — це цикл :ada:`while`.

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_5c

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_5c is
       I : Integer := 1;
    begin
       --  Умова має бути логічним виразом
       --  (не численним).
       --  Оператор "<=" вертає результат
       --  порівняння
       while I <= 5 loop
          Put_Line ("Hello, World!"
                    & Integer'Image (I));

          I := I + 1;
       end loop;
    end Greet_5c;

Умова оцінюється перед кожною ітерацією. Якщо результат хибний,
то цикл припиняється.

Ця програма робить то самє, що й попередні.

.. admonition:: В інших мовах

    Зауважте, що Ada має іншу семантику, ніж мови на основі C щодо умови в
    циклі while. В Ada умова має бути логічним значенням, інакше компілятор
    відхилить програму; умова не є цілим числом, яке розглядається як
    :ada:`True` або :ada:`False` залежно від того, відмінне воно від нуля
    чи ні.


.. _Intro_Ada_Case_Statement:

Імперативна мова - Case
-----------------------

Оператор Ada :ada:`case` схожий на оператор C і C++ :c:`switch`, але з
деякими важливими відмінностями.

Ось приклад, варіація програми, яка була показана раніше з
оператором :ada:`if`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Direction_2

    with Ada.Text_IO;         use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Direction is
       N : Integer;
    begin
       loop
          Put ("Enter an integer value: ");
          Get (N);
          Put (N);

          case N is
             when 0 | 360 =>
                Put_Line
                  (" is due north");
             when 1 .. 89 =>
                Put_Line
                  (" is in the northeast quadrant");
             when 90 =>
                Put_Line
                  (" is due east");
             when 91 .. 179 =>
                Put_Line
                  (" is in the southeast quadrant");
             when 180 =>
                Put_Line
                  (" is due south");
             when 181 .. 269 =>
                Put_Line
                  (" is in the southwest quadrant");
             when 270 =>
                Put_Line
                  (" is due west");
             when 271 .. 359 =>
                Put_Line
                  (" is in the northwest quadrant");
             when others =>
                Put_Line
                  (" Au revoir");
                exit;
          end case;
       end loop;
    end Check_Direction;

Ця програма постійно запитує ціле число, а потім, якщо значення знаходиться
в діапазоні :ada:`0 .. 360`, відображає відповідний квадрант або вісь. Якщо
значення виходить за межі цього діапазону, цикл (і програма) припиняється
після виведення прощального повідомлення.

Ефект оператора case подібний до оператора if у попередньому прикладі, але
оператор case може бути ефективнішим, оскільки він не передбачає багаторазових
перевірок діапазону.

Важливі моменти щодо конструкції case:

-  Вираз для case (тут змінна :ada:`N`) має бути дискретного типу, тобто
   або цілого типу, або типу перерахування. Дискретні типи будуть розглянуті
   більш детально пізніше :ref:`дискретні типи <Intro_Ada_What_Is_A_Type>`.

-  Кожне можливе значення виразу case має бути охоплено одною з гілок
   оператора case. Це буде перевірено під час компіляції.

-  Гілка може охоплювати одне значення, наприклад :ada:`0`; діапазон значень,
   наприклад :ada:`1 .. 89`; або будь-яка комбінація значень (розділених символом `|`).

-  Як особливий випадок, необов’язкова кінцева гілка може вказати :ada:`others`,
   яка охоплює всі інші значення, не включені в попередні гілки.

-  Виконання складається з оцінки виразу case, а потім передачі керування коду в
   унікальній гілці, яка охоплює це значення.

-  Коли виконання коду у вибраній гілці завершено, керування передається коду
   після :ada:`end case`. На відміну від C, виконання не переходить до наступної
   гілки. Отже, Ada не потребує (і не має) оператора :c:`break`.


Імперативна мова - Декларативні блоки
-------------------------------------

Як згадувалося раніше, Ada проводить чіткий синтаксичний розподіл між деклараціями,
які вводять імена для сутностей, які використовуватимуться в програмі, та операторами,
які виконують обробку. Області в програмі, де можуть з’являтися декларації, називаються
декларативними блоками.

У будь-якій підпрограмі розділ між :ada:`is` і :ada:`begin` є декларативним блоком.
Ви можете оголосити там змінні, константи, типи, внутрішні підпрограми та інші сутності.

Ми коротко згадували про оголошення змінних у попередньому підрозділі. Давайте
розглянемо простий приклад, де ми оголошуємо цілочисельну змінну :ada:`X` у декларативному
блоці та виконуємо її ініціалізацію та модифікацію:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Variable_Declaration

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       X : Integer;
    begin
       X := 0;
       Put_Line ("The initial value of X is "
                 & Integer'Image (X));

       Put_Line ("Performing operation on X...");
       X := X + 1;

       Put_Line ("The value of X now is "
                 & Integer'Image (X));
    end Main;

Давайте розглянемо ще один приклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Nested_Procedure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       procedure Nested is
       begin
          Put_Line ("Hello World");
       end Nested;
    begin
       Nested;
       --  Викликали процедуру Nested
    end Main;

.. _Intro_Ada_Block_Statement:

Оголошення неможливі посеред операторів. Якщо вам потрібно оголосити локальну змінну серед
операторів, ви можете оголосити новий декларативний блок за допомогою оператора :ada:`declare`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_6

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
    begin
       loop
          Put_Line ("Please enter your name: ");

          declare
             Name : String := Get_Line;
             --               ^ Виклик функції
             --                 Get_Line
          begin
             exit when Name = "";
             Put_Line ("Hi " & Name & "!");
          end;

          --  Змінної Name більше не існує
       end loop;

      Put_Line ("Bye!");
    end Greet;

.. attention::

    Функція :ada:`Get_Line` дозволяє вам отримувати дані від користувача та отримати
    строку як результат. Це більш-менш еквівалентно функції C:c:`scanf`.

    Вона повертає :ada:`String`, який, як ми побачимо пізніше, є
    :ref:`Необмеженим масивом <Intro_Ada_Unconstrained_Array_Types>`. Наразі ми просто
    зауважимо, що якщо ви бажаєте оголосити змінну :ada:`String` і не знаєте її розмір
    заздалегідь, вам потрібно ініціалізувати змінну під час її оголошення.

Імперативна мова - умовні вирази
--------------------------------

Ada 2012 додала аналог виразу для умовних операторів
(:ada:`if` і :ada:`case`).

If вираз
~~~~~~~~

Ось альтернативна версія прикладу, який ми бачили раніше;
оператор :ada:`if` було замінено виразом :ada:`if`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Positive

    with Ada.Text_IO;         use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Positive is
       N : Integer;
    begin
       Put ("Enter an integer value: ");
       Get (N);
       Put (N);

       declare
          S : constant String :=
            (if N > 0
               then " is a positive number"
               else " is not a positive number");
       begin
          Put_Line (S);
       end;
    end Check_Positive;

Вираз :ada:`if` використовує один із двох строк залежно від N і 
присвоює це значення локальній змінній S.

Вирази Ada :ada:`if` подібні до операторів :ada:`if`. Однак є кілька
відмінностей, які пов’язані з тим, що це вираз:

-  Обидва варіанти мають бути одного типу

-  Він *має* бути в дужках, якщо навколишній вираз
   їх ще не містить

-  Гілка :ada:`else` є обов’язковою, якщо вираз після :ada:`then` не є
   логічним типом. У цьому випадку гілка :ada:`else` є необов’язковою
   і, якщо її немає, за замовчуванням використовується :ada:`else True`.

Ось інший приклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Even_Odd

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       for I in 1 .. 10 loop
          Put_Line (if I mod 2 = 0
                      then "Even"
                      else "Odd");
       end loop;
    end Main;

Ця програма виводить 10 рядків, чергуючи «Непарні» та «Парні».


Case вираз
~~~~~~~~~~

Подібно до виразів :ada:`if`, Ada також має вирази :ada:`case`.
Вони працюють так, як Ви очікуєте.

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Case_Expression

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       for I in 1 .. 10 loop
          Put_Line
            (case I is
             when 1 | 3 | 5 | 7 | 9  => "Odd",
             when 2 | 4 | 6 | 8 | 10 => "Even");
       end loop;
    end Main;

Ця програма робить то самє, що й попередній приклад.

Синтаксис відрізняється від оператора :ada:`case`, в даному
випадку варіанти розділені комами.
